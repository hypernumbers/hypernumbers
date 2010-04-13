%%% @private
-module(test_util).

-export([expected/2,
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
-define(EXCEL_IMPORT_MAX_ABS_ERROR, (1.0e-9)).
-define(EXCEL_IMPORT_MAX_REL_ERROR, (1.0e-5)).
-define(DEFAULT,1000000).

%% @doc Convert row and column pair to A1-style ref
%% @spec rc_to_a1(Row :: integer(), Col :: integer()) -> string()
rc_to_a1(Row, Col) ->
    tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1).

test_state(State)->
    
    {Cells, Ranges} = lists:partition(
                        fun({{_Sheet,_Row,_Col},_Val})       -> true;
                           ({{_Sheet,_R1,_C1,_R2,_C2},_Val}) -> false
                        end, State),
    
    test_state(Cells, Ranges).

test_state(Cells, Ranges) ->
    receive
        {msg, Pid, _Suite, Ref} ->
            Pid ! read_from_excel_data(Cells, Ranges, Ref),
            test_state(Cells, Ranges);
        die ->
            ok
    end.

read_from_excel_data(Cells, Ranges, {Sheet, Row, Col})->

    Key = { {sheet,Sheet}, {row_index,Row}, {col_index,Col} },
    
    Res = case lists:keysearch(Key, 1, Cells) of
              false ->
                  % If the cell is not found, search if the cell is
                  % contained within any array formula and return that
                  Tmp = [Val || {{_Sheet,{_,R1},{_,C1},{_,R2},{_,C2}}, Val}
                                    <- Ranges,
                                R1 =< Row, R2 >= Row, C1 =< Col, C2 >= Col ],
                  case Tmp of
                      [X] -> X;
                      _   -> not_found
                  end;
              {value, Tmp} ->
                  element(2, Tmp)
          end,
    
    case Res of 
        {value, number,Number}          -> {number, Number};
        {string, String}                -> {string, String};
        {formula, Formula}              -> {formula, Formula};
        {value, boolean, Boolean}       -> {boolean, Boolean};
        {value, error, Error}           -> {error, Error};
        {value, date, {datetime, D, T}} -> {date, {D, T}};
        _ -> not_found
    end.

-spec float_cmp(float(), float(), any()) -> boolean().
float_cmp(F1, F2, _Digit) when abs(F1 - F2) < ?EXCEL_IMPORT_MAX_ABS_ERROR ->
    true;
float_cmp(F1, F2, _Digit) ->
    RelError = if (abs(F1) > abs(F2)) ->
                       abs((F1 - F2) / F1);
                  true ->
                       abs((F1 - F2) / F2)
               end,
    (RelError =< ?EXCEL_IMPORT_MAX_REL_ERROR).


excel_equal(X, X) ->
    true;
excel_equal({string, X}, {formula, X}) ->
    true;
excel_equal({string, "'"++X}, {formula, X}) ->
    true;
excel_equal({date,F1}, {number,Number})->
    {datetime, D, T} = muin_date:excel_win_to_gregorian(Number),
    F1 == {D, T}; 
excel_equal({string, "'"++F1}, {string, F1}) ->
    true;
%% Cant check because goes pre gregorian, fuzzy check
excel_equal({date, {{1900, 1, 1}, _T}}, {string, "1899/12/31"++_}) ->
    true;
%% Date was set on leap year that didnt exist, autofix
excel_equal({date, {{1900, 3, 1}, _T}}, {string, "1900/02/28"++_}) ->
    true;
%% January / February in 1900 are offset by 1 day in excel
excel_equal({date, {{1900, M, _D}, _T}=F1}, {string, F2})
  when M =:= 1 orelse M =:= 2 ->
    NewTime = calendar:datetime_to_gregorian_seconds(F1) - 86400,
    NDate   = calendar:gregorian_seconds_to_datetime(NewTime),
    dh_date:format("Y/m/d H:i:s", NDate) == F2;
excel_equal({date, F1}, {string, F2}) ->
    dh_date:format("Y/m/d H:i:s", F1) == F2;
excel_equal({number, 0}, {number, 0.0}) ->
    true;
excel_equal({number, F1}, {number, F2}) ->
    erlang:abs(1 - F1 / F2) < 0.0000001;
excel_equal({formula, Formula1}, {formula, Formula2}) ->
    transform_expected(Formula2) == transform_got(Formula1);
excel_equal({error,Error1},{number,ErrorVal}) ->
    Error1 == make_err_val(ErrorVal);
excel_equal({number,Num},{string,Str})->
    Num == tconv:to_num(Str);
excel_equal(_X, _Y) ->
    false.

transform_expected(Formula) ->
    Opt = [{return, list}, global],
    
    Tmp2 = re:replace(Formula, "ERRORTYPE", "ERROR.TYPE", Opt),
    Tmp3 = re:replace(Tmp2, "FINDB\\(", "FIND\\(", Opt),
    Tmp4 = re:replace(Tmp3, "LEFTB\\(", "LEFT\\(", Opt),
    Tmp5 = re:replace(Tmp4, "LENB\\(", "LEN\\(", Opt),
    Tmp6 = re:replace(Tmp5, "MIDB\\(", "MID\\(", Opt),
    Tmp7 = re:replace(Tmp6, "RIGHTB\\(", "RIGHT\\(", Opt),
    Tmp8 = re:replace(Tmp7, "SEARCHB\\(", "SEARCH\\(", Opt),


    % Change sheet!A1 to ../sheet/a1
    Tmp9 = re:replace(Tmp8, "(([a-z0-9]+)!([A-Z0-9]+))","../\\2/\\3", Opt),
    Tmp9.

transform_got(Formula2) ->
    Formula = re:replace(Formula2, " / ", "/", [{return, list}, global]),
    % if row address, strip the column bounds (=$A169:$IV169) becomes (=169:169)
    Tmp = case re:run(Formula, "\\$A[0-9]+:\\$IV[0-9]+") of
              {match, _} -> re:replace(Formula, "\\$A|\\$IV", "",
                                       [{return, list}, global]);
              _          -> Formula
          end,

    % change ../bob to bob, by itself
    Tmp1 = re:replace(Tmp, "\.\./([a-z]+)$","\\1", %"
                      [{return, list}, global]),
    % change ../bob to bob when a parameter
    Tmp2 = re:replace(Tmp1, "\.\./([a-z]+(?:,|\\)))","\\1",
                      [{return, list}, global]),
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
                throw:_Term    -> "not float"
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

-define(HNSERVER, "http://hypernumbers.dev:9000").
hnget(Path, Cell) ->
    Url = string:to_lower(?HNSERVER ++ Path ++ Cell),
    Ref = hn_util:parse_url(Url),
    case hn_db_api:read_attributes(Ref,["rawvalue"]) of
        [{_Ref, {"rawvalue", Val}}] when is_atom(Val) ->
            atom_to_list(Val);
        [{_Ref, {"rawvalue", {datetime, D, T}}}] ->
            dh_date:format("Y/m/d H:i:s",{D,T});
        [{_Ref, {"rawvalue", {errval, Val}}}] ->
            atom_to_list(Val);
        [{_Ref, {"rawvalue", Val}}] ->
            Val;
        _Else ->
            ""
    end.
%     if is_list(Body) -> string:strip(Body, both, $");
%        true            -> Body
% end.
  
hnpost(Path, Ref, Postdata) ->
    Url = string:to_lower(?HNSERVER ++ Path ++ Ref),
    Postreq = "{\"formula\":\"" ++ Postdata ++ "\"}",
    Return = httpc:request(post,
                          {Url, [{"Accept", "application/json"}],
                           "application/json", Postreq},
                          [{timeout, 5000}],
                          []),
    handle_return(Return, Ref).

handle_return({error, timeout}, _Ref) ->
    ok;
handle_return({ok, {{_V, 200, _R}, _H, _Body}}, _Ref) ->
    ok;
handle_return({ok, {{_V, Code, _R}, _H, Body}}, Ref) ->
    io:format("in test_util:handle_return HTTP POST error (~s)~n-code:~p~n-body:~p~n",
	      [Ref, Code, Body]).

cmp(A,A) ->
    true;
cmp(G, E) ->
    
    G2 = conv_from_get(G),
    E2 = conv_for_no_reason(E),
    
    E2 == G2
        orelse cmp_nums(G2, E2)
        orelse cmp_date(G2, E2).

cmp_nums(N1, N2) when is_number(N1), is_number(N2) ->
    float_cmp(N1, N2, 5);
cmp_nums(_, _) ->
    false.

cmp_date({{1,1,1}, {H,M,S}}, N2) ->
    Secs = ((H * 3600) + (M * 60) + S) / 86400,
    float_cmp(Secs, N2, 5);
cmp_date({Date, Time}, N2) ->
    dh_date:format("Y/M/D H:m:S", {Date, Time}) == N2;
cmp_date(_, _) ->
    false.

conv_for_no_reason(E) ->
    case E of
        true   -> true;
        false  -> false;
        _Other ->
            case lists:member(E, ['#NULL!', '#DIV/0!', '#VALUE!',
                                  '#REF!', '#NAME?', '#NUM!',
                                  '#N/A']) of
                true    -> E;
                _Other2 ->
                    case tconv:to_num(E) of
                        N when is_number(N) -> N;
                        {error, nan}        -> E
                    end
            end
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
        {datetime, D, T} -> {D, T};
        _                -> conv_from_get2(X)
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

