%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_array_formulae_SUITE).
-compile(export_all).
-include("ct.hrl").
-import(lists, [foreach/2, map/2]).
-import(test_util, [conv_for_post/1, conv_from_get/1, cmp/2, hnpost/3, hnget/2, readxls/1]).

-define(print_error_or_return(Res, Testcase),
        case Res of
            true ->
                {test, ok};
            false ->
                io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~nCONV:~n    ~p~n~n",
                          [E, G, conv_from_get(G)]),
                exit("FAIL: Mismatch in ~p in ~p~n", 
                     [Testcase, "b_array_formulae_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_array_formulae" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "Array Formulae").
?test(sheet1_A2, "/Sheet1/", "A2", 1.0).
?test(sheet1_B2, "/Sheet1/", "B2", 2.0).
?test(sheet1_C2, "/Sheet1/", "C2", 2.0).
?test(sheet1_A3, "/Sheet1/", "A3", 3.0).
?test(sheet1_B3, "/Sheet1/", "B3", 4.0).
?test(sheet1_C3, "/Sheet1/", "C3", 12.0).
?test(sheet1_A5, "/Sheet1/", "A5", "Functions with arrays").
?test(sheet1_A6, "/Sheet1/", "A6", "1D Sum").
?test(sheet1_B6, "/Sheet1/", "B6", 6.0).
?test(sheet1_A7, "/Sheet1/", "A7", "2D Sum").
?test(sheet1_B7, "/Sheet1/", "B7", 45.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_array_formulae.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_array_formulae" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_C2,
        sheet1_A3,
        sheet1_B3,
        sheet1_C3,
        sheet1_A5,
        sheet1_A6,
        sheet1_B6,
        sheet1_A7,
        sheet1_B7
    ].