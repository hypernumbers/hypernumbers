%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_ping_SUITE).
-compile(export_all).
-include("ct.hrl").
-import(lists, [foreach/2, map/2]).

-define(print_error_or_return(Res, Testcase),
        case Res of
            true ->
                {test, ok};
            false ->
                io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~nCONV:~n    ~p~n~n",
                          [E, G, conv_from_get(G)]),
                exit("FAIL: Mismatch in ~p in ~p~n", 
                     [Testcase, "b_ping_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_ping" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Ping_Sheet1/", "A1", "This works with Pong and Pang to test off-file references").
?test(sheet1_A2, "/Ping_Sheet1/", "A2", "Simple ref to Pong Sheet 1").
?test(sheet1_B2, "/Ping_Sheet1/", "B2", "from Pong Sheet 1").
?test(sheet1_A3, "/Ping_Sheet1/", "A3", "Simple ref to Pong Sheet 2").
?test(sheet1_B3, "/Ping_Sheet1/", "B3", "from Pong Sheet 2").
?test(sheet1_A4, "/Ping_Sheet1/", "A4", "Simple ref to Pong Sheet 3").
?test(sheet1_B4, "/Ping_Sheet1/", "B4", "from Pong Sheet 3").
?test(sheet1_A5, "/Ping_Sheet1/", "A5", "Refer to Function on Pong Sheet 1").
?test(sheet1_B5, "/Ping_Sheet1/", "B5", 15.0).
?test(sheet1_A6, "/Ping_Sheet1/", "A6", "Simple ref to Pang Sheet 1").
?test(sheet1_B6, "/Ping_Sheet1/", "B6", "from Pang Sheet 1").
?test(sheet1_A7, "/Ping_Sheet1/", "A7", "Simple ref to Pang Sheet 2").
?test(sheet1_B7, "/Ping_Sheet1/", "B7", "from Pang Sheet 2").
?test(sheet1_A8, "/Ping_Sheet1/", "A8", "Simple ref to Pang Sheet 3").
?test(sheet1_B8, "/Ping_Sheet1/", "B8", "from Pang Sheet 3").
?test(sheet1_A9, "/Ping_Sheet1/", "A9", "Refer to Function on Pang Sheet 1").
?test(sheet1_B9, "/Ping_Sheet1/", "B9", 6.0).
?test(sheet1_A10, "/Ping_Sheet1/", "A10", "Simple ref to another sheet in this file").
?test(sheet1_B10, "/Ping_Sheet1/", "B10", "from Sheet 2 in Ping").
?test(sheet1_A11, "/Ping_Sheet1/", "A11", "Refer to function to another sheet in this file").
?test(sheet1_B11, "/Ping_Sheet1/", "B11", 10.0).
?test(sheet1_A12, "/Ping_Sheet1/", "A12", "3d Ref on this file").
?test(sheet1_B12, "/Ping_Sheet1/", "B12", 77.0).
?test(sheet2_A1, "/Ping_Sheet2/", "A1", "from Sheet 2 in Ping").
?test(sheet2_A2, "/Ping_Sheet2/", "A2", 1.0).
?test(sheet2_A3, "/Ping_Sheet2/", "A3", 2.0).
?test(sheet2_A4, "/Ping_Sheet2/", "A4", 3.0).
?test(sheet2_A5, "/Ping_Sheet2/", "A5", 4.0).
?test(sheet2_A8, "/Ping_Sheet2/", "A8", 33.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_ping.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_ping" ++ "/" ++ Sheetname ++ "/",
                Ref = tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1),
                hnpost(Path, Ref, Postdata)
        end,
    foreach(Postcell, Celldata),
    Config.

end_per_suite(_Config) ->
    production_boot:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        sheet1_A1,
        sheet1_A2,
        sheet1_B2,
        sheet1_A3,
        sheet1_B3,
        sheet1_A4,
        sheet1_B4,
        sheet1_A5,
        sheet1_B5,
        sheet1_A6,
        sheet1_B6,
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet1_A9,
        sheet1_B9,
        sheet1_A10,
        sheet1_B10,
        sheet1_A11,
        sheet1_B11,
        sheet1_A12,
        sheet1_B12,
        sheet2_A1,
        sheet2_A2,
        sheet2_A3,
        sheet2_A4,
        sheet2_A5,
        sheet2_A8
    ].

-define(HNSERVER, "http://127.0.0.1:9000").

hnget(Path, Ref) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    {ok, {{_V, Code, _R}, _H, Body}} = http:request(get, {Url, []}, [], []),
    io:format("Code for ~p~p is ~p.~nBody is: ~p~n~n", [Path, Ref, Code, Body]),
    Body.
  
hnpost(Path, Ref, Postdata) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    Postreq = "<create><formula>" ++ Postdata ++ "</formula></create>",
    Return = http:request(post,
                          {Url, [], "text/xml", Postreq},
                          [], []),
    {ok, {{_V, Code, _R}, _H, Body}} = Return,
    io:format("Posted ~p to ~p~p.~nResponse code: ~p. Response body: ~p.~n~n", 
              [Postdata, Path, Ref, Code, Body]),
    Return.

cmp(G, E) ->
    Val = conv_from_get(G),
    Val == E.

conv_from_get(Val) ->
    case Val of
        [34 | Tl] -> % String
            hslists:init(Tl);
        [39 | Tl] -> % Atom, i.e. an error value.
            list_to_atom(hslists:init(Tl));
        "TRUE" ->
            true;
        "FALSE" ->
            false;
        _ ->
            tconv:to_num(Val)
    end.

conv_for_post(Val) ->
    case Val of
        {_, boolean, true} -> "true";
        {_, boolean, fase} -> "false";
        {_, number, N}     -> tconv:to_s(N);
        {_, error, E}      -> E;
        {string, X}        -> "\"" ++ X ++ "\"";
        {formula, F}       -> F
    end.
