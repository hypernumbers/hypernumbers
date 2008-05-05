%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_just_numbers_SUITE).
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
                     [Testcase, "b_just_numbers_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_just_numbers" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", 0.0).
?test(sheet1_C1, "/Sheet1/", "C1", 1.1).
?test(sheet1_G1, "/Sheet1/", "G1", 0.0).
?test(sheet1_A2, "/Sheet1/", "A2", 1.0).
?test(sheet1_C2, "/Sheet1/", "C2", 0.12).
?test(sheet1_E2, "/Sheet1/", "E2", 0.123).
?test(sheet1_G2, "/Sheet1/", "G2", 256.0).
?test(sheet1_A3, "/Sheet1/", "A3", 2.0).
?test(sheet1_C3, "/Sheet1/", "C3", -0.99).
?test(sheet1_E3, "/Sheet1/", "E3", 123.123456789012).
?test(sheet1_G3, "/Sheet1/", "G3", 65535.0).
?test(sheet1_A4, "/Sheet1/", "A4", 3.0).
?test(sheet1_G4, "/Sheet1/", "G4", 16777216.0).
?test(sheet1_A5, "/Sheet1/", "A5", 4.0).
?test(sheet1_G5, "/Sheet1/", "G5", 4294967296.0).
?test(sheet1_A6, "/Sheet1/", "A6", 5.0).
?test(sheet1_G6, "/Sheet1/", "G6", 4294967297.0).
?test(sheet1_A7, "/Sheet1/", "A7", 6.0).
?test(sheet1_G7, "/Sheet1/", "G7", 1.84467440737095e+19).
?test(sheet1_A8, "/Sheet1/", "A8", 7.0).
?test(sheet1_A9, "/Sheet1/", "A9", 8.0).
?test(sheet1_A10, "/Sheet1/", "A10", 9.0).
?test(sheet1_A11, "/Sheet1/", "A11", 10.0).
?test(sheet1_C11, "/Sheet1/", "C11", 777.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_just_numbers.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_just_numbers" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_C1,
        sheet1_G1,
        sheet1_A2,
        sheet1_C2,
        sheet1_E2,
        sheet1_G2,
        sheet1_A3,
        sheet1_C3,
        sheet1_E3,
        sheet1_G3,
        sheet1_A4,
        sheet1_G4,
        sheet1_A5,
        sheet1_G5,
        sheet1_A6,
        sheet1_G6,
        sheet1_A7,
        sheet1_G7,
        sheet1_A8,
        sheet1_A9,
        sheet1_A10,
        sheet1_A11,
        sheet1_C11
    ].