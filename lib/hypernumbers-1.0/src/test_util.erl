%%% @private
-module(test_util).

-export([expected/2,
         equal_to_digit/3,
         test_state/1,
         make_float/1,
         conv_for_post/1,
         conv_from_get/1,
         cmp/2,
         hnpost/3,
         hnget/2,
         float_cmp/3,
         stripfileref/1,
         rc_to_a1/2]).

-include("excel_errors.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(FILEDIR, "/../../tests/excel_files/").
-define(EXCEL_IMPORT_FLOAT_PRECISION, 9).
-define(DEFAULT,1000000).

%% @doc Convert row and column pair to A1-style ref
%% @spec rc_to_a1(Row :: integer(), Col :: integer()) -> string()
rc_to_a1(Row, Col) ->
    tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1).

test_state(State)->
    receive
        {msg, Pid, _Suite, Ref} ->
            Pid ! read_from_excel_data(State, Ref),
            test_state(State);
        die ->
            ok
    end.

read_from_excel_data(State, {Sheet, Row, Col})->
    
    Key = { {sheet,Sheet}, {row_index,Row}, {col_index,Col} },
    {value, Result} = lists:keysearch(Key, 1, State),

    case element(2, Result) of
        {value, number,Number}          -> {number, Number};
        {string, String}                -> {string, String};
        {formula, Formula}              -> {formula, Formula};
        {value, boolean, Boolean}       -> {boolean, Boolean};
        {value, error, Error}           -> {error, Error};
        {value, date, {datetime, D, T}} -> {date, {D, T}}
    end.

equal_to_digit(F1, F2, DigitIdx) ->
    % force any rogue integers to floats  
    F1a=float(F1),
    F2a=float(F2),
    [As0,Bs0]=io_lib:fwrite("~.*f~.*f",[DigitIdx+1,F1a-erlang:trunc(F1a), 
                                        DigitIdx+1,F2a-erlang:trunc(F2a)]),
    As=string:substr(As0,1,DigitIdx+2), 
    Bs=string:substr(Bs0,1,DigitIdx+2),
    As==Bs.

float_cmp(0.0,0.0,_)          -> true;
float_cmp(0.0,Expres,Digit)   -> (Expres < math:pow(0.1, Digit));
float_cmp(Res, Expres, Digit) -> (abs(Res - Expres)/Res) < math:pow(0.1, Digit).

excel_equal(X, X) ->
    true;
excel_equal({date,F1}, {number,Number})->
    {datetime, D, T} = muin_date:excel_win_to_gregorian(Number),
    F1 == {D, T}; 
excel_equal({date, F1}, {string, F2}) ->
    dh_date:format("Y/m/d h:i:s", F1) == F2;
excel_equal({number, F1}, {number, F2}) ->
    equal_to_digit(F1, F2, ?EXCEL_IMPORT_FLOAT_PRECISION);
excel_equal({formula, Formula1}, {formula, Formula2}) ->
    Formula2 == transform_formula(Formula1);
excel_equal({error,Error1},{number,ErrorVal}) ->
    Error1 == make_err_val(ErrorVal);
excel_equal({number,Num},{string,Str})->
    Num == tconv:to_num(Str);
excel_equal(_X, _Y) ->
    false.

transform_formula(Formula) ->
    % if row address, strip the column bounds (=$A169:$IV169) becomes (=169:169)
    Tmp = case re:run(Formula, "\\$A[0-9]+:\\$IV[0-9]+") of
              {match, _} -> re:replace(Formula, "\\$A|\\$IV", "", [{return, list}, global]);
              _          -> Formula
          end,
    % fix-up the fact that we have changed the name of the function Error.Type 
    % to ErrorType
    % Ugly bodge
    Tmp2 = re:replace(Tmp, "ERROR.TYPE", "ERRORTYPE", [{return, list}, global]),
    stripfileref(Tmp2).

%% Nasty function to convert 
%% stuff'C:\\cygwin\\stuff\\[e_gnumeric_bitwise.xls]Name'!stuff
%% to 
%% ../e_gnumeric_bitwise/Name!stuff"
stripfileref(Str) ->
    case string:str(Str,"'C:\\") of
        0 -> Str;
        X -> 
            Pre = string:sub_string(Str, 1, X-1),
            Post = string:sub_string(Str, X+1),
            File = "../" ++ string:sub_string(Post, string:chr(Post, 91)+1), 
            Pos = string:chr(File,$'),%'       
            Content = string:sub_string(File,1,Pos-1),
            Rest    = string:sub_string(File,Pos+1),
            S1 = re:replace(Content,"\\]","/", [{return, list}, global]),
            S2 = re:replace(S1,".xls","", [{return, list}, global]),    
            Pre ++ S2 ++ Rest
    end.

make_err_val(?ErrDiv0Int)  -> "#DIV/0!";
make_err_val(?ErrNAInt)    -> "#N/A";
make_err_val(?ErrNameInt)  -> "#NAME?";
make_err_val(?ErrNullInt)  -> "#NULL!";
make_err_val(?ErrNumInt)   -> "#NUM!";
make_err_val(?ErrRefInt)   -> "#REF!";
make_err_val(?ErrValueInt) -> "#VALUE!";
make_err_val(X)            -> X.

expected(Got, Expected) ->
    case excel_equal(Got, Expected) of
        true  -> {test, ok};
        false -> exit({fail, expected, Expected, got, Got})
    end.

make_float(List) ->
    Return = try
                list_to_float(List)
              catch
                exit:_Reason   -> "not float";
                error:_Message -> "not float";
                throw:_Term     -> "not float"
            end,
    case Return of
      "not float" -> make_float2(List);
      _           -> Return
    end.

make_float2(List)->
    try
      list_to_integer(List)*1.0
    catch
      exit:_Reason   -> "not float";
      error:_Message -> "not float";
      throw:_Term    -> "not float"
    end.

-define(HNSERVER, "http://127.0.0.1:9000").

hnget(Path, Ref) ->
    Url = string:to_lower(?HNSERVER ++ Path ++ Ref),
    io:format("In hnget Url is ~p~n", [Url]),
    {ok, {{_V, _Code, _R}, _H, Body}} = http:request(get, {Url, []}, [], []),
    Body.
  
hnpost(Path, Ref, Postdata) ->
    Url = string:to_lower(?HNSERVER ++ Path ++ Ref),
    Postreq = "{\"formula\":\"" ++ Postdata ++ "\"}",
    Return = http:request(post,
                          {Url, [], "application/json", Postreq},
                          [{timeout, 5000}],
                          []),
    handle_return(Return, Ref).

handle_return({error, timeout}, _Ref) ->
    bits:log("in test_util:handle_return TIMEOUT!");
handle_return({ok, {{_V, 200, _R}, _H, _Body}}, _Ref) ->
    ok;
handle_return({ok, {{_V, Code, _R}, _H, Body}}, Ref) ->
    io:format("in test_util:handle_return HTTP POST error (~s)~n-code:~p~n-body:~p~n",
	      [Ref, Code, Body]).

cmp(A,A) -> true;
cmp(G,E) ->
    E2 = case E of
             true   -> true;
             false  -> false;
             _Other -> case lists:member(E, ['#NULL!', '#DIV/0!', '#VALUE!',
                                             '#REF!', '#NAME?', '#NUM!',
                                             '#N/A']) of
                           true    -> E;
                           _Other2 -> case tconv:to_num(E) of
                                          N when is_number(N) -> N;
                                          {error, nan}        -> E
                                      end
                       end
         end,
    G2 = conv_from_get(G),
    if
        is_float(G2) andalso is_float(E2) ->
            float_cmp(G2, E2, 5);
        is_integer(G2) andalso is_float(E2) ->
            % sometimes large integers appear as (exponented) floats from excel...
            float_cmp(G2 * 1.0, E2, 5);
        true -> E2 == G2
    end.

conv_from_get(X) when is_float(X) -> X;
conv_from_get(X) when is_integer(X) -> X;
conv_from_get(true)  -> true;
conv_from_get(false) -> false;
conv_from_get("true")  -> true;
conv_from_get("false") -> false;
conv_from_get("TRUE")  -> true;
conv_from_get("FALSE") -> false;
conv_from_get(X)       ->
    % need to try and convert to a date
    case muin_date:from_rfc1123_string(X) of
        {datetime, D, T} -> dh_date:format("Y/M/D H:m:S", {D, T});
        _                  -> conv_from_get2(X)
    end.

conv_from_get2(X) ->
    case lists:member(X, ["#NULL!", "#DIV/0!", "#VALUE!",
                          "#REF!", "#NAME?", "#NUM!", "#N/A"]) of
        true -> % error value
            list_to_atom(X);
        false ->
            case tconv:to_num(X) of
                N when is_number(N) -> N; % number
                {error, nan}        -> X % string
            end
    end.

%% @TODO: Some of these conversion need to be done inside the reader itself.
conv_for_post(Val) ->
    case Val of
        {_, boolean, true}        -> "true";
        {_, boolean, false}       -> "false";
        {_, date, {datetime,D,T}} -> dh_date:format("Y/M/D H:m:S", {D,T});
        {_, number, N}            -> tconv:to_s(N);
        {_, error, E}             -> E;
        {string, X}               -> X;
        {formula, F}              -> F
    end.

