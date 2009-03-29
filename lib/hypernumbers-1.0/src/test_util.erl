%%% @private
-module(test_util).

-export([expected/2,
         expected2/2,
         expected3/2,
         expected4/2,
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
         stripfileref/1,
         transform_reader_output/1,
         import_xls/1,
         load_perf_tests/0,
         rc_to_a1/2]).

-include("excel_errors.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(FILEDIR, "/../../tests/excel_files/").
-define(EXCEL_IMPORT_FLOAT_PRECISION, 9).
-define(DEFAULT,1000000).

load_perf_tests()->
    import_xls("w_recalc_perf_tests_1"),
    import_xls("w_recalc_perf_tests_2"),
    import_xls("w_recalc_perf_tests_3").    

%% what a crap sandwich this whole thing is

read_reader_record({{{sheet, SheetName}, {row_index, Row}, {col_index, Col}}, Val}) ->
    {SheetName, {Row, Col}, Val};
read_reader_record({{{sheet, SheetName}, {firstrow, Fr}, {firstcol, Fc}, {lastrow, Lr}, {lastcol, Lc}}, Fla}) ->
    {SheetName, {{Fr, Fc}, {Lr, Lc}}, Fla}.

%% @doc Convert row and column pair to A1-style ref
%% @spec rc_to_a1(Row :: integer(), Col :: integer()) -> string()
rc_to_a1(Row, Col) ->
    tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1).

import_xls(Name) ->
    hypernumbers_app:clean_start(),

    P = code:lib_dir(hypernumbers),
    [_Hn, _Lib, _DD, _Ebin | Rest] = lists:reverse(string:tokens(P, "/")),
    File = string:join(lists:reverse(Rest), "/") ++
           "/tests/excel_files/Win_Excel07_As_97/" ++
           Name ++ ".xls",
    File2 = case os:type() of
                {win32, nt} -> File;
                _           -> "/" ++ File
            end,
    io:format("in test_util:import_xls File is ~p~n",[File2]),
    io:format("- for some reason Names and Formats are not being used!~n"),
    {Celldata, _Names, _Formats, CSS} = readxls(File2),
    F = fun(X, {Ls, Fs}) ->
                {SheetName, Target, V} = read_reader_record(X),
                Sheet = excel_util:esc_tab_name(SheetName),
                Postdata = conv_for_post(V),
                Path = "/" ++ Name ++ "/" ++ Sheet ++ "/",
                Ref = case Target of
                          {{Fr, Fc}, {Lr, Lc}} -> {rc_to_a1(Fr, Fc), rc_to_a1(Lr, Lc)};
                          {Row, Col}           -> rc_to_a1(Row, Col)
                      end,
                Datatpl = {Path, Ref, Postdata},
                case Postdata of
                    [$=|_] -> {Ls, [Datatpl|Fs]};
                    _      -> {[Datatpl|Ls], Fs}
                end                
        end,
    {Lits, Flas} = lists:foldl(F,{[], []}, Celldata),
    
    Dopost = fun({Path, Ref, Postdata}) when is_list(Ref) -> % single cell
                     Url = string:to_lower("http://127.0.0.1:9000" ++ Path ++ Ref),
                     {ok, RefRec} = hn_util:parse_url(Url),
                     Postdata2 = fix_integers(Postdata),
                     % ok = hn_main:set_cell(RefRec, Postdata2);
                     {RefX, {Key, Val}} = hn_util:ref_to_refX(RefRec, Postdata2),
                     {ok, ok} = hn_db_api:write_attributes(RefX, [{"formula", Val}]);
                ({Path, {Tl, Br}, Postdata}) -> % array formula
                     Url = string:to_lower("http://127.0.0.1:9000" ++ Path ++ Tl ++ ":" ++ Br),
                     {ok, RefRec} = hn_util:parse_url(Url),
                     hn_main:formula_to_range(Postdata, RefRec)
             end,
    
    ?INFO("Start Posting: ~p", [Name]),
    % gen_server:cast(dirty_cell,  {setstate, passive}),
    lists:foreach(Dopost, Lits),
    lists:foreach(Dopost, Flas),
    ?INFO("Start Recalculating: ~p", [Name]),
    % _Return1=gen_server:cast(dirty_cell, {setstate, active}),
    % _Return2=gen_server:call(dirty_cell, flush, infinity),

    % Now fire in the CSS and formats
    WriteCSS = fun(X) ->
                       {{{sheet, SheetName}, {row_index, Row}, {col_index, Col}}, CSSItem} = X,
                       Sheet = excel_util:esc_tab_name(SheetName),
                       Path = "/" ++ Name ++ "/" ++ Sheet ++ "/",
                       Ref = rc_to_a1(Row,Col),
                       Url = string:to_lower("http://127.0.0.1:9000" ++ Path ++ Ref),
                       {ok, RefRec} = hn_util:parse_url(Url),
                       #ref{path = Path2} = RefRec,
                       Addr = #ref{site = "http://127.0.0.1:9000",
                                   path = Path2, ref = {page, "/"}},
                       hn_db:write_style_IMPORT(Addr, CSSItem)
               end,
    lists:foreach(WriteCSS, CSS),
    loop(),
    ?INFO("End Import: ~p", [Name]),
    ok.

% flatpack(List) -> flatpack(List, []).

% flatpack([H | []], Acc) -> lists:concat(lists:reverse([H | Acc]));
% flatpack([H | T], Acc)  -> flatpack(T, [" ", H | Acc]).

fix_integers(X) ->
    case make_float(X) of
        "not float" -> X;
        X2          -> if
                           (X2-round(X2)) == 0.0 -> integer_to_list(round(X2));
                           true                  -> X
                       end
    end.

loop()->
    case mnesia:table_info(dirty_cell,size) of
        0 -> ok;
        _ -> timer:sleep(250),
              loop()
    end.

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
    case Return of
        {value, Result2} ->
            El=element(2, Result2),
            case El of
                {value,number,Number}       -> {number,Number};
                {string,String}             -> {string,String};
                {formula,Formula}           -> {formula,Formula};
                {value,boolean,Boolean}     -> {boolean,Boolean};
                {value,error,Error}         -> {error, Error};
                {value,date,{datetime,D,T}} -> {date,{D,T}};
                Other                       -> 
                    io:format("(in test_util:read_from_excel_date "++
                              " fix generatetest.rb - Other is ~p~n",
                              [Other])
            end;
        _Other2 -> {fail, data_not_read}
    end.

equal_to_digit(F1,F2,DigitIdx) ->
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

excel_equal("-2146826281","#DIV/0!") -> true;
excel_equal("-2146826246","#N/A")    -> true;
excel_equal("-2146826259","#NAME?")  -> true;
excel_equal("-2146826288","#NULL!")  -> true;
excel_equal("-2146826252","#NUM!")   -> true;
excel_equal("-2146826265","#REF!")   -> true;
excel_equal("-2146826273","#VALUE!") -> true;

%% Checks that two Excel values are equal.
excel_equal(String1,String2) when is_list(String1), is_list(String2) ->
    % fix-up the fact that we have changed the name of the function Error.Type to ErrorType
    Return=regexp:gsub(String2,"ERROR.TYPE","ERRORTYPE"),
    {ok,String2a,_}=Return,
    R2 = stripfileref(String2a),
    Result = case String1 of
            R2 -> true;
            _        -> false
      end,
    % if the strings aren't the same try and make numbers of them and compare then
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

eq(X,Y) ->
    if
        X == Y -> true;
        true   -> false
    end.

excel_equal2({date,F1},{number,Number})->
    {datetime,D,T}=muin_date:excel_win_to_gregorian(Number),
    F2={D,T},
    eq(F1,F2);
excel_equal2({date, F1}, {string, F2}) ->
    F1String=make_date_string(F1),
    eq(F1String,F2);
excel_equal2({number, F1}, {number, F2}) ->
    equal_to_digit(F1, F2, ?EXCEL_IMPORT_FLOAT_PRECISION);
excel_equal2({formula, PreFla1}, {formula, Fla2}) ->
    % if row address, strip the column bounds (=$A169:$IV169) becomes (=169:169)
    Fla1 = case regexp:match(PreFla1,"\\$A[0-9]+:\\$IV[0-9]+") of
    {match,_,_} ->
        {ok,Str,_Count} = regexp:gsub(PreFla1,"\\$A|\\$IV",""),
        Str;
    _ -> PreFla1
    end,
    % fix-up the fact that we have changed the name of the function Error.Type 
    % to ErrorType
    % Ugly bodge    
    {ok,Fla2a,_} = regexp:gsub(Fla2,"ERROR.TYPE","ERRORTYPE"),
    R2 = stripfileref(Fla2a),
    eq(Fla1,R2);
excel_equal2({boolean,Boolean1},{boolean,Boolean2}) ->
    eq(Boolean1,Boolean2);
excel_equal2({error,Error1},{number,ErrorVal}) ->
    Error2=make_err_val(ErrorVal),
    eq(Error1,Error2);
excel_equal2({error,Error1},{error,Error2}) ->
    eq(Error1,Error2);
excel_equal2({string,String1},{string,String2}) ->
    eq(String1,String2);
excel_equal2({number,Num},{string,Str})->
    eq(Num,tconv:to_num(Str)).

make_date_string({Days,Time}) ->
     make_day_string(Days)++" "++make_time_string(Time).

make_day_string({Year,Month,Day}) ->
    integer_to_list(Day)++"/"++pad(integer_to_list(Month))++"/"++pad(integer_to_list(Year)).

make_time_string({Hour,Minute,Second})->
    pad(integer_to_list(Hour))++":"++pad(integer_to_list(Minute))++":"++pad(integer_to_list(Second)).

pad(X) when is_list(X) ->
    case length(X) of
	1 -> "0"++X;
	_ -> X
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
    io:format(" in test_util:expected Expected is ~p and Got is ~p~n",[Expected,Got]),
    Expected2 = yaws_api:url_encode(Expected),
    Got2 = yaws_api:url_encode(Got),
    case Got2 of
        Expected2 ->
            io:format("SUCCESS~nExpected : ~p~nGot      : ~p~n",
                      [Expected2,Got2]),
            {test, ok};
        _Else ->
            exit({"E:", Expected2, "G:", Got2})
    end.

expected2(Expected, Got) ->
    io:format(" in test_util:expected2 Expected is ~p and Got is ~p~n",[Expected,Got]),
    Result = excel_equal2(Expected, Got),
    case Result of
        true ->
            io:format("<b style=\"color:green\">SUCCESS</b>~nExpected: ~p~nGot: ~p~n",
                      [Expected, Got]),
            {test, ok};
        false ->
            io:format("<b style=\"color:red\">FAIL</b>~nExpected: ~w~nGot: ~w~n",
                      [Expected, Got]),
            exit({fail, expected, Expected, got, Got})
    end.

expected3(Expected, Got) ->
    case Got of
        Expected ->
            io:format("SUCCESS~nExpected : ~p~nGot     : ~p~n",
                      [Expected,Got]),
            {test, ok};
        _Else ->
            exit({"E:", Expected, "G:", Got})
    end.

expected4(Expected, Got) ->
    case Got of
        Expected ->
            io:format("SUCCESS~nExpected : ~p~nGot      : ~p~n",
                      [Expected,Got]),
            {test, ok};
        _Else ->
            exit({"E:", Expected, "G:", Got})
    end.

%% @doc Read XLS file.
%% @TODO: this should be part of the reader - clients should not need to
%% decipher the collection of ETS tables dumped on them.
readxls(Fn) ->
    filefilters:read(excel, Fn, fun decipher_ets_tables/1).

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
      throw:_Term    -> "not float"
    end.


%% Default operation wait time, used for the test suites.
wait()  -> internal_wait(?DEFAULT).

wait(N) -> internal_wait(?DEFAULT * N).

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
        {datetime, D, T} -> make_string(D, T);
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

make_string({Y, M, D}, {H, Mn, S}) -> integer_to_list(Y)++"/"++
                                           pad(integer_to_list(M))++"/"++
                                           pad(integer_to_list(D))++" "++
                                           pad(integer_to_list(H))++":"++
                                           pad(integer_to_list(Mn))++":"++
                                           pad(integer_to_list(S)).

%% @TODO: Some of these conversion need to be done inside the reader itself.
conv_for_post(Val) ->
    case Val of
        {_, boolean, true}        -> "true";
        {_, boolean, false}       -> "false";
        {_, date, {datetime,D,T}} -> make_date_string({D,T});
        {_, number, N}            -> tconv:to_s(N);
        {_, error, E}             -> E;
        {string, X}               -> X;
        {formula, F}              -> F
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @doc Transforms reader's output to a nice regular structure:
%% * Row and column indexes are 1-based.
%% * Each entry is {{sheet, Sheet}, {row, Row}, {col, Col}, {Type, Value}}.
%% * Some type tags are changed: boolean -> bool, error -> errval.
transform_reader_output(O) ->
    Mktypeval = fun(error, Value) ->
                        {errval, list_to_atom(Value)};
                   (boolean, Value) ->
                        {bool, Value};
                   (Type, Value) ->
                        {Type, Value}
                end,
    
    Mkrec = fun(Sheet, Row, Col, Type, Value) ->
                    {{sheet, Sheet}, {row, Row + 1}, {col, Col + 1},
                     Mktypeval(Type, Value)}
            end,
    
    lists:foldl(fun({{{sheet, Sheet}, {row_index, Row}, {col_index, Col}},
                     {Type, Value}}, Acc) ->
                        R = Mkrec(Sheet, Row, Col, Type, Value),
                        [R | Acc];
                   ({{{sheet, Sheet}, {row_index, Row}, {col_index, Col}},
                     {value, Type, Value}}, Acc) ->
                        R = Mkrec(Sheet, Row, Col, Type, Value),
                        [R | Acc]
                end,
                [], O).

%% Input: list of {key, id} pairs where key is an ETS table holding info
%% about some part of the XLS file.
%% @TODO: formats, names, styles.
decipher_ets_tables(Tids) ->
    %
    % First get the formulae/string data
    % 
    % Grab information about single cell values.
    {value, {cell, Tid1}} = lists:keysearch(cell, 1, Tids),
    CellRecs = ets:foldl(fun(X, Acc) -> [X|Acc] end, [], Tid1),
    CellInfo = lists:map(fun({Index, [_, Body]}) -> {Index, Body} end, CellRecs),
    % Grab information about array formulas.
    {value, {array_formulae, Tid2}} = lists:keysearch(array_formulae, 1, Tids),
    AFRecs = ets:foldl(fun(X, Acc) -> [X|Acc] end, [], Tid2),
    Celldata = CellInfo ++ AFRecs,

    %
    % Now get the Names information
    % 
    {value, {names, Tid3}} = lists:keysearch(names, 1, Tids),
    Names = ets:foldl(fun(X, Acc) -> [X | Acc] end, [], Tid3),

    %
    % Now get the Format information
    % 
    {value, {formats, Tid4}} = lists:keysearch(formats, 1, Tids),
    Formats = ets:foldl(fun(X, Acc) -> [X | Acc] end, [], Tid4),
    
    %
    % Now get the CSS information
    % 
    {value, {css, Tid5}} = lists:keysearch(css, 1, Tids),
    CSS = ets:foldl(fun(X, Acc) -> [X | Acc] end, [], Tid5),

    {Celldata, Names, Formats, CSS}.

internal_wait(0) ->
    ok;
internal_wait(N) ->
    internal_wait(N-1).
