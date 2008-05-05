%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_nested_functions_SUITE).
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
                     [Testcase, "b_nested_functions_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_nested_functions" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This spreadsheet test nested function calls").
?test(sheet1_A3, "/Sheet1/", "A3", "Nested funs except SUM with 1 arg or CHOOSE").
?test(sheet1_A4, "/Sheet1/", "A4", "Argument with operators").
?test(sheet1_B4, "/Sheet1/", "B4", 11.0).
?test(sheet1_A5, "/Sheet1/", "A5", "Single nested functions").
?test(sheet1_B5, "/Sheet1/", "B5", 3.74165738677394).
?test(sheet1_A6, "/Sheet1/", "A6", "Multiply nested functions").
?test(sheet1_B6, "/Sheet1/", "B6", 1.46081299152751).
?test(sheet1_A7, "/Sheet1/", "A7", "Multiply nested functions").
?test(sheet1_B7, "/Sheet1/", "B7", 4.5).
?test(sheet1_A8, "/Sheet1/", "A8", "Multiply nested functions").
?test(sheet1_B8, "/Sheet1/", "B8", 2.23205080756888).
?test(sheet1_A9, "/Sheet1/", "A9", "Multiply nested functions").
?test(sheet1_B9, "/Sheet1/", "B9", 5.5).
?test(sheet1_A10, "/Sheet1/", "A10", "Multiply nested functions").
?test(sheet1_B10, "/Sheet1/", "B10", 2.64575131106459).
?test(sheet1_A11, "/Sheet1/", "A11", "Multiply nested functions").
?test(sheet1_B11, "/Sheet1/", "B11", 1.73205080756888).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_nested_functions.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_nested_functions" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A11,
        sheet1_B11
    ].