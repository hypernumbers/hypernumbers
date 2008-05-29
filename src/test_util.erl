-module(test_util).

-export([
         expected/2,
         expected2/2,
         readxls/1,
         read_excel_file/1,
         equal_to_digit/3,
         excel_equal/2,
         wait/0,
         wait/1,
         test_state/1,
         make_float/1,

         conv_for_post/1,
         conv_from_get/1,
         cmp/2,
         hnpost/3,
         hnget/2,
         float_cmp/3,
         stripfileref/1
	]).

-include("excel_errors.hrl").

-define(FILEDIR, "../../../../excel_files/").
-define(EXCEL_IMPORT_FLOAT_PRECISION, 9).
-define(DEFAULT,1000000).


%% Nasty function to convert 
%% stuff'C:\\cygwin\\stuff\\[e_gnumeric_bitwise.xls]Name'!stuff
%% to 
%% ../e_gnumeric_bitwise/Name!stuff"
stripfileref(Str) ->
    case string:str(Str,"'C:\\") of
    0 -> Str;
    X -> 
        Pre = string:sub_string(Str,1,X-1),
        Post = string:sub_string(Str,X+1),
        File = "../"++string:sub_string(Post,string:chr(Post,$[)+1),
        Pos = string:chr(File,$'),%'       
        Content = string:sub_string(File,1,Pos-1),
        Rest    = string:sub_string(File,Pos+1),
        {ok,S1,_Count} = regexp:gsub(Content,"\\]","/"),
        {ok,S2,_Count} = regexp:gsub(S1,".xls",""),
        
        Pre ++ S2 ++ Rest
    end.
        

test_state(State)->
  receive
    {msg,Pid,_Suite,{Sheet,Row,Col}} -> Pid ! read_from_excel_data(State,{Sheet,Row,Col});
    {die}                            -> exit(die);
    Other                            -> io:format("message means nothing in test_util:state ~p~n",
                                           [Other])
  end,
  test_state(State).
  
read_from_excel_data(State,{Sheet,Row,Col})->
  Key={{sheet,Sheet},{row_index,Row},{col_index,Col}},
  Return=lists:keysearch(Key, 1, State),
  {value, Result2}=Return,
  El=element(2, Result2),
  case El of
      {value, number, Number} -> {number,Number};
      {string,String}         -> {string,String};
      {formula,Formula}       -> {formula,Formula};
      {value,boolean,Boolean} -> {boolean,Boolean};
      {value,error,Error}     -> {error, Error};
      Other                   -> io:format("(in generatetest.rb - fix me Other is ~p~n",[Other])
end.

equal_to_digit(F1,F2,DigitIdx) ->
  %% force any rogue integers to floats  
  F1a=float(F1),
  F2a=float(F2),
  [As0,Bs0]=io_lib:fwrite("~.*f~.*f",[DigitIdx+1,F1a-erlang:trunc(F1a), 
                                       DigitIdx+1,F2a-erlang:trunc(F2a)]),
    As=string:substr(As0,1,DigitIdx+2), 
    Bs=string:substr(Bs0,1,DigitIdx+2),
    As==Bs.

float_cmp(Res, Expres, Digit) ->
    abs(Res - Expres) < math:pow(0.1, Digit).
    

excel_equal("-2146826281","#DIV/0!") -> true;
excel_equal("-2146826246","#N/A")    -> true;
excel_equal("-2146826259","#NAME?")  -> true;
excel_equal("-2146826288","#NULL!")  -> true;
excel_equal("-2146826252","#NUM!")   -> true;
excel_equal("-2146826265","#REF!")   -> true;
excel_equal("-2146826273","#VALUE!") -> true;
%% Checks that two Excel values are equal.
excel_equal(String1,String2) when is_list(String1), is_list(String2) ->
  %% fix-up the fact that we have changed the name of the function Error.Type to ErrorType
    Return=regexp:gsub(String2,"ERROR.TYPE","ERRORTYPE"),
    io:format("Return is ~p~n",[Return]),
    {ok,String2a,_}=Return,
    R2 = stripfileref(String2a),
    io:format("String1 is ~p and R2 is ~p~n",[String1,R2]),
    Result = case String1 of
            R2 -> true;
            _        -> false
      end,
    %% if the strings aren't the same try and make numbers of them and compare then
    case Result of
      true  -> true;
      false ->
        String1f=make_float(String1),
        String2f=make_float(String2),
        case {String1f,String2f} of
          {"not float","not float"} -> false;
          {"not float",_}           -> false;
          {_          ,"not float"} -> false;
          _                          -> equal_to_digit(String1f,String2f,
                                            ?EXCEL_IMPORT_FLOAT_PRECISION)
        end
    end.
        
excel_equal2({number, F1}, {number, F2}) ->
    equal_to_digit(F1, F2, ?EXCEL_IMPORT_FLOAT_PRECISION);
excel_equal2({formula, Fla1}, {formula, Fla2}) ->
  %% fix-up the fact that we have changed the name of the function Error.Type to ErrorType
  %% Ugly bodge
    Return=regexp:gsub(Fla2,"ERROR.TYPE","ERRORTYPE"),
    io:format("Return is ~p~n",[Return]),
    {ok,Fla2a,_}=Return,
    R2 = stripfileref(Fla2a),
    io:format("Fla1 is  ~p~nR2 is ~p~n",[Fla1,R2]),
    case Fla1 of
      R2 -> true;
      _     -> false
    end;
excel_equal2({boolean,Boolean1},{boolean,Boolean2}) ->
    case Boolean1 of
        Boolean2 -> true;
        _        -> false
    end;
excel_equal2({error,Error1},{number,ErrorVal}) ->
    Error2=make_err_val(ErrorVal),
    case Error1 of
        Error2 -> true;
        _      -> false
    end;
excel_equal2({error,Error1},{error,Error2}) ->
    case Error1 of
        Error2 -> true;
        _      -> false
    end;
excel_equal2({string,String1},{string,String2}) ->
    case String1 of
        String2 -> true;
        _       -> false
    end.

make_err_val(?ErrDiv0Int)  -> "#DIV/0!";
make_err_val(?ErrNAInt)    -> "#N/A";
make_err_val(?ErrNameInt)  -> "#NAME?";
make_err_val(?ErrNullInt)  -> "#NULL!";
make_err_val(?ErrNumInt)   -> "#NUM!";
make_err_val(?ErrRefInt)   -> "#REF!";
make_err_val(?ErrValueInt) -> "#VALUE!";
make_err_val(X)            -> X.

expected(Expected, Got) ->
    Expected2 = yaws_api:url_encode(Expected),
    Got2 = yaws_api:url_encode(Got),
    case Got2 of
        Expected2 ->
            io:format("SUCCESS~nExpected : ~p~nGot     : ~p~n",
                      [Expected2,Got2]),
            {test, ok};
        _Else ->
            exit({"E:", Expected2, "G:", Got2})
    end.

expected2(Expected, Got) ->
    Result = excel_equal2(Expected, Got),
    case Result of
        true ->
            io:format("<b style=\"color:green\">SUCCESS</b>~nExpected: ~p~nGot: ~p~n",
                      [Expected, Got]),
            {test, ok};
        false ->
            io:format("<b style=\"color:red\">FAIL</b>~nExpected: ~p~nGot: ~p~n",
                      [Expected, Got]),
            exit({fail, expected, Expected, got, Got})
    end.

readxls(Filename) ->
    filefilters:read(excel, Filename, fun extract_cell_info/1).

read_excel_file(Filename) ->
    c:pwd(),
    io:format("in test_util:read_excel_file Filename is ~p~n",[Filename]),
    readxls(Filename).

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
      throw:_Term     -> "not float"
    end.


%% Default operation wait time, used for the test suites.
wait()  -> internal_wait(?DEFAULT).

wait(N) -> internal_wait(?DEFAULT * N).

-define(HNSERVER, "http://127.0.0.1:9000").

hnget(Path, Ref) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    {ok, {{_V, Code, _R}, _H, Body}} = http:request(get, {Url, []}, [], []),
    io:format("Code for ~p~p is ~p.~nBody is: ~p~n~n", [Path, Ref, Code, Body]),
    Body.
  
hnpost(Path, Ref, Postdata) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    Postreq = "<create><formula><![CDATA[" ++ Postdata ++ "]]></formula></create>",
    io:format("Posting ~p to ~s...~n", [Postdata, Path ++ Ref]),
    Return = http:request(post,
                          {Url, [], "text/xml", Postreq},
                          [{timeout, 5000}],
                          []),
    handle_return(Return).

handle_return({error, timeout}) ->
    bits:log("TIMEOUT!"),
    io:format("<b style=\"color:red;font-size:21px\">TIMEOUT</b>~n");
handle_return({ok, {{_V, 200, _R}, _H, Body}}) ->
    io:format("OK.~n");
handle_return({ok, {{_V, Code, _R}, _H, Body}}) ->
    io:format("HTTP POST error, code:~n~pbody:~n~p~n", [Code, Body]).

cmp(G, E) ->
    Val = conv_from_get(G),
    if is_float(Val) andalso is_float(E) ->
            float_cmp(Val, E, 5);
       true ->
            Val == E
    end.

conv_from_get(Val) ->
    case Val of
        [34 | Tl] -> % String
            hslists:init(Tl);
        X = [35 | _] -> % Starts with # -> error value.
            list_to_atom(X);
        "TRUE" ->
            true;
        "FALSE" ->
            false;
        "true" ->
            true;
        "false" ->
            false;
        _ ->
            tconv:to_num(Val)
    end.

%% TODO: Some of these conversion need to be done inside the reader itself.
conv_for_post(Val) ->
    case Val of
        {_, boolean, true} -> "true";
        {_, boolean, false} -> "false";
        {_, number, N}     -> tconv:to_s(N);
        {_, error, E}      -> E;
        {string, X}        -> "\"" ++ X ++ "\"";
        {formula, F}       -> F
    end.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% Gets the list of xls table names and their ETS table ids, grabs the cell table
%% and extracts cell information from it (sheet, row, col, contents).
extract_cell_info(Tables) ->
    {value, {cell,Tid}} = lists:keysearch(cell, 1, Tables),
    Cells = ets:foldl(fun(X, Acc) -> [X | Acc] end,
                      [], Tid),
    Res = lists:map(fun({Index, [_, Body]}) -> {Index, Body} end,
                    Cells),
    Res.

internal_wait(0) ->
    ok;
internal_wait(N) ->
    internal_wait(N-1).
