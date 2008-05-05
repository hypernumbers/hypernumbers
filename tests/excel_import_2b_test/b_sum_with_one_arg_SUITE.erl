%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_sum_with_one_arg_SUITE).
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
                     [Testcase, "b_sum_with_one_arg_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_sum_with_one_arg" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "Special Test For Sum With 1 Parameter").
?test(sheet1_A2, "/Sheet1/", "A2", "See Section 3.10 of the excelfileformat.v1.40.pdf").
?test(sheet1_A3, "/Sheet1/", "A3", "When sum has a single parameter is is converted into an attribute token").
?test(sheet1_A4, "/Sheet1/", "A4", 1.0).
?test(sheet1_A5, "/Sheet1/", "A5", 4.0).
?test(sheet1_B5, "/Sheet1/", "B5", 4.0).
?test(sheet1_A6, "/Sheet1/", "A6", 1.0).
?test(sheet1_A7, "/Sheet1/", "A7", 10.0).
?test(sheet1_A8, "/Sheet1/", "A8", 3.0).
?test(sheet1_A9, "/Sheet1/", "A9", 3.0).
?test(sheet1_B9, "/Sheet1/", "B9", 1.0).
?test(sheet1_C9, "/Sheet1/", "C9", 2.0).
?test(sheet1_A10, "/Sheet1/", "A10", 20.0).
?test(sheet1_B10, "/Sheet1/", "B10", 1.0).
?test(sheet1_C10, "/Sheet1/", "C10", 2.0).
?test(sheet1_D10, "/Sheet1/", "D10", 3.0).
?test(sheet1_E10, "/Sheet1/", "E10", 4.0).
?test(sheet1_F10, "/Sheet1/", "F10", 5.0).
?test(sheet1_G10, "/Sheet1/", "G10", 6.0).
?test(sheet1_H10, "/Sheet1/", "H10", 7.0).
?test(sheet1_I10, "/Sheet1/", "I10", 8.0).
?test(sheet1_A11, "/Sheet1/", "A11", 21.0).
?test(sheet1_B11, "/Sheet1/", "B11", 1.0).
?test(sheet1_C11, "/Sheet1/", "C11", 2.0).
?test(sheet1_D11, "/Sheet1/", "D11", 3.0).
?test(sheet1_E11, "/Sheet1/", "E11", 4.0).
?test(sheet1_F11, "/Sheet1/", "F11", 5.0).
?test(sheet1_G11, "/Sheet1/", "G11", 6.0).
?test(sheet1_H11, "/Sheet1/", "H11", 7.0).
?test(sheet1_I11, "/Sheet1/", "I11", 8.0).
?test(sheet1_A12, "/Sheet1/", "A12", 'NULL!').
?test(sheet1_B12, "/Sheet1/", "B12", 1.0).
?test(sheet1_C12, "/Sheet1/", "C12", 2.0).
?test(sheet1_D12, "/Sheet1/", "D12", 3.0).
?test(sheet1_E12, "/Sheet1/", "E12", 4.0).
?test(sheet1_F12, "/Sheet1/", "F12", 5.0).
?test(sheet1_G12, "/Sheet1/", "G12", 6.0).
?test(sheet1_H12, "/Sheet1/", "H12", 7.0).
?test(sheet1_I12, "/Sheet1/", "I12", 8.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_sum_with_one_arg.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_sum_with_one_arg" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A3,
        sheet1_A4,
        sheet1_A5,
        sheet1_B5,
        sheet1_A6,
        sheet1_A7,
        sheet1_A8,
        sheet1_A9,
        sheet1_B9,
        sheet1_C9,
        sheet1_A10,
        sheet1_B10,
        sheet1_C10,
        sheet1_D10,
        sheet1_E10,
        sheet1_F10,
        sheet1_G10,
        sheet1_H10,
        sheet1_I10,
        sheet1_A11,
        sheet1_B11,
        sheet1_C11,
        sheet1_D11,
        sheet1_E11,
        sheet1_F11,
        sheet1_G11,
        sheet1_H11,
        sheet1_I11,
        sheet1_A12,
        sheet1_B12,
        sheet1_C12,
        sheet1_D12,
        sheet1_E12,
        sheet1_F12,
        sheet1_G12,
        sheet1_H12,
        sheet1_I12
    ].