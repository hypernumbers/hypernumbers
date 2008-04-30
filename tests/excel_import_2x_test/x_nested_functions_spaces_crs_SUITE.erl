%% This file is generated; DO NOT EDIT MANUALLY.

-module(x_nested_functions_spaces_crs_SUITE).
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
                     [Testcase, "x_nested_functions_spaces_crs_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "x_nested_functions_spaces_crs" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This spreadsheet test nested function calls, spaces and formula formatting").
?test(sheet1_A2, "/Sheet1/", "A2", "Argument with operators").
?test(sheet1_B2, "/Sheet1/", "B2", 3.0).
?test(sheet1_A3, "/Sheet1/", "A3", "Argument with operators and spaces").
?test(sheet1_B3, "/Sheet1/", "B3", 3.0).
?test(sheet1_A4, "/Sheet1/", "A4", "Argument with operators, spaces and CR").
?test(sheet1_B4, "/Sheet1/", "B4", 3.0).
?test(sheet1_A6, "/Sheet1/", "A6", "Argument with operators").
?test(sheet1_B6, "/Sheet1/", "B6", 22.0).
?test(sheet1_A7, "/Sheet1/", "A7", "Argument with operators and spaces").
?test(sheet1_B7, "/Sheet1/", "B7", 22.0).
?test(sheet1_A8, "/Sheet1/", "A8", "Argument with operators, spaces and CR").
?test(sheet1_B8, "/Sheet1/", "B8", 22.0).
?test(sheet1_A10, "/Sheet1/", "A10", "Single nested functions").
?test(sheet1_B10, "/Sheet1/", "B10", -2.0).
?test(sheet1_A11, "/Sheet1/", "A11", "Single nested functions and spaces").
?test(sheet1_B11, "/Sheet1/", "B11", -2.0).
?test(sheet1_A12, "/Sheet1/", "A12", "Single nested functions, spaces and CR").
?test(sheet1_B12, "/Sheet1/", "B12", -2.0).
?test(sheet1_A14, "/Sheet1/", "A14", "Multiply nested functions").
?test(sheet1_B14, "/Sheet1/", "B14", 15.0).
?test(sheet1_A15, "/Sheet1/", "A15", "Multiply nested functions and spaces").
?test(sheet1_B15, "/Sheet1/", "B15", 15.0).
?test(sheet1_A16, "/Sheet1/", "A16", "Multiply nested functions, spaces and CR").
?test(sheet1_B16, "/Sheet1/", "B16", 15.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "x_nested_functions_spaces_crs.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "x_nested_functions_spaces_crs" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A6,
        sheet1_B6,
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet1_A10,
        sheet1_B10,
        sheet1_A11,
        sheet1_B11,
        sheet1_A12,
        sheet1_B12,
        sheet1_A14,
        sheet1_B14,
        sheet1_A15,
        sheet1_B15,
        sheet1_A16,
        sheet1_B16
    ].

-define(HNSERVER, "http://127.0.0.1:9000").

hnget(Path, Ref) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    {ok, {{V, Code, R}, H, Body}} = http:request(get, {Url, []}, [], []),
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
