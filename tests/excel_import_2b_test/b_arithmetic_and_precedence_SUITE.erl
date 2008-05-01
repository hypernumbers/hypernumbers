%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_arithmetic_and_precedence_SUITE).
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
                     [Testcase, "b_arithmetic_and_precedence_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_arithmetic_and_precedence" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This spreadsheet tests basic arithmetic operations").
?test(sheet1_A3, "/Sheet1/", "A3", "Simple Arithmetic").
?test(sheet1_A4, "/Sheet1/", "A4", "Formula").
?test(sheet1_B4, "/Sheet1/", "B4", "Answer").
?test(sheet1_A5, "/Sheet1/", "A5", 3.0).
?test(sheet1_B5, "/Sheet1/", "B5", 3.0).
?test(sheet1_A6, "/Sheet1/", "A6", -1.0).
?test(sheet1_B6, "/Sheet1/", "B6", -1.0).
?test(sheet1_A7, "/Sheet1/", "A7", 2.0).
?test(sheet1_B7, "/Sheet1/", "B7", 2.0).
?test(sheet1_A8, "/Sheet1/", "A8", 0.5).
?test(sheet1_B8, "/Sheet1/", "B8", 0.5).
?test(sheet1_A9, "/Sheet1/", "A9", '#DIV/0!').
?test(sheet1_B9, "/Sheet1/", "B9", '#DIV/0!').
?test(sheet1_A10, "/Sheet1/", "A10", 4.0).
?test(sheet1_B10, "/Sheet1/", "B10", 4.0).
?test(sheet1_A12, "/Sheet1/", "A12", "Precedence and brackets").
?test(sheet1_A13, "/Sheet1/", "A13", "Formula").
?test(sheet1_B13, "/Sheet1/", "B13", "Answer").
?test(sheet1_A14, "/Sheet1/", "A14", 4.99992855509831).
?test(sheet1_B14, "/Sheet1/", "B14", 4.99992855509831).
?test(sheet1_A15, "/Sheet1/", "A15", 11.0).
?test(sheet1_B15, "/Sheet1/", "B15", 11.0).
?test(sheet1_A16, "/Sheet1/", "A16", 4.99992855509831).
?test(sheet1_B16, "/Sheet1/", "B16", 4.99992855509831).
?test(sheet1_A17, "/Sheet1/", "A17", -12.6).
?test(sheet1_B17, "/Sheet1/", "B17", -12.6).
?test(sheet1_A18, "/Sheet1/", "A18", 15625.0).
?test(sheet1_B18, "/Sheet1/", "B18", 15625.0).
?test(sheet1_A19, "/Sheet1/", "A19", 5.0).
?test(sheet1_B19, "/Sheet1/", "B19", 5.0).
?test(sheet1_A20, "/Sheet1/", "A20", 4.0).
?test(sheet1_B20, "/Sheet1/", "B20", 4.0).
?test(sheet1_A21, "/Sheet1/", "A21", 10.0).
?test(sheet1_B21, "/Sheet1/", "B21", 10.0).
?test(sheet1_A22, "/Sheet1/", "A22", 10.0).
?test(sheet1_B22, "/Sheet1/", "B22", 10.0).
?test(sheet1_A23, "/Sheet1/", "A23", 32.0).
?test(sheet1_B23, "/Sheet1/", "B23", 32.0).
?test(sheet1_A24, "/Sheet1/", "A24", 1.0).
?test(sheet1_B24, "/Sheet1/", "B24", 1.0).
?test(sheet1_A25, "/Sheet1/", "A25", 7.0).
?test(sheet1_B25, "/Sheet1/", "B25", 7.0).
?test(sheet1_A26, "/Sheet1/", "A26", 9.0).
?test(sheet1_B26, "/Sheet1/", "B26", 9.0).
?test(sheet1_A27, "/Sheet1/", "A27", 3.0).
?test(sheet1_B27, "/Sheet1/", "B27", 3.0).
?test(sheet1_A28, "/Sheet1/", "A28", 2224.9999285551).
?test(sheet1_B28, "/Sheet1/", "B28", 4.99992855509831).
?test(sheet1_A29, "/Sheet1/", "A29", 3.0).
?test(sheet1_B29, "/Sheet1/", "B29", 3.0).
?test(sheet1_A30, "/Sheet1/", "A30", 109.0).
?test(sheet1_B30, "/Sheet1/", "B30", -1.0).
?test(sheet1_A31, "/Sheet1/", "A31", 112.0).
?test(sheet1_B31, "/Sheet1/", "B31", 2.0).
?test(sheet1_A32, "/Sheet1/", "A32", 97.0).
?test(sheet1_B32, "/Sheet1/", "B32", -13.0).
?test(sheet1_A33, "/Sheet1/", "A33", 111.4).
?test(sheet1_B33, "/Sheet1/", "B33", 1.4).
?test(sheet1_A34, "/Sheet1/", "A34", -187391.0).
?test(sheet1_B34, "/Sheet1/", "B34", -187501.0).
?test(sheet1_A35, "/Sheet1/", "A35", 117608.060801556).
?test(sheet1_B35, "/Sheet1/", "B35", 117608.060801556).
?test(sheet1_A36, "/Sheet1/", "A36", 0.03).
?test(sheet1_B36, "/Sheet1/", "B36", 0.03).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_arithmetic_and_precedence.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_arithmetic_and_precedence" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A3,
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
        sheet1_A12,
        sheet1_A13,
        sheet1_B13,
        sheet1_A14,
        sheet1_B14,
        sheet1_A15,
        sheet1_B15,
        sheet1_A16,
        sheet1_B16,
        sheet1_A17,
        sheet1_B17,
        sheet1_A18,
        sheet1_B18,
        sheet1_A19,
        sheet1_B19,
        sheet1_A20,
        sheet1_B20,
        sheet1_A21,
        sheet1_B21,
        sheet1_A22,
        sheet1_B22,
        sheet1_A23,
        sheet1_B23,
        sheet1_A24,
        sheet1_B24,
        sheet1_A25,
        sheet1_B25,
        sheet1_A26,
        sheet1_B26,
        sheet1_A27,
        sheet1_B27,
        sheet1_A28,
        sheet1_B28,
        sheet1_A29,
        sheet1_B29,
        sheet1_A30,
        sheet1_B30,
        sheet1_A31,
        sheet1_B31,
        sheet1_A32,
        sheet1_B32,
        sheet1_A33,
        sheet1_B33,
        sheet1_A34,
        sheet1_B34,
        sheet1_A35,
        sheet1_B35,
        sheet1_A36,
        sheet1_B36
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
