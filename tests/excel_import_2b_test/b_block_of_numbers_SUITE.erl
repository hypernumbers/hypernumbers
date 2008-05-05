%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_block_of_numbers_SUITE).
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
                     [Testcase, "b_block_of_numbers_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_block_of_numbers" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", 1.0).
?test(sheet1_B1, "/Sheet1/", "B1", 2.0).
?test(sheet1_C1, "/Sheet1/", "C1", 3.0).
?test(sheet1_D1, "/Sheet1/", "D1", 4.0).
?test(sheet1_E1, "/Sheet1/", "E1", 5.0).
?test(sheet1_F1, "/Sheet1/", "F1", 6.0).
?test(sheet1_G1, "/Sheet1/", "G1", 7.0).
?test(sheet1_H1, "/Sheet1/", "H1", 8.0).
?test(sheet1_I1, "/Sheet1/", "I1", 9.0).
?test(sheet1_A2, "/Sheet1/", "A2", 11.0).
?test(sheet1_B2, "/Sheet1/", "B2", 22.0).
?test(sheet1_C2, "/Sheet1/", "C2", 33.0).
?test(sheet1_D2, "/Sheet1/", "D2", 44.0).
?test(sheet1_E2, "/Sheet1/", "E2", 55.0).
?test(sheet1_F2, "/Sheet1/", "F2", 66.0).
?test(sheet1_G2, "/Sheet1/", "G2", 77.0).
?test(sheet1_H2, "/Sheet1/", "H2", 88.0).
?test(sheet1_I2, "/Sheet1/", "I2", 99.0).
?test(sheet1_A3, "/Sheet1/", "A3", 111.0).
?test(sheet1_B3, "/Sheet1/", "B3", 222.0).
?test(sheet1_C3, "/Sheet1/", "C3", 333.0).
?test(sheet1_D3, "/Sheet1/", "D3", 444.0).
?test(sheet1_E3, "/Sheet1/", "E3", 555.0).
?test(sheet1_F3, "/Sheet1/", "F3", 666.0).
?test(sheet1_G3, "/Sheet1/", "G3", 777.0).
?test(sheet1_H3, "/Sheet1/", "H3", 888.0).
?test(sheet1_I3, "/Sheet1/", "I3", 999.0).
?test(sheet1_A4, "/Sheet1/", "A4", 1111.0).
?test(sheet1_B4, "/Sheet1/", "B4", 2222.0).
?test(sheet1_C4, "/Sheet1/", "C4", 3333.0).
?test(sheet1_D4, "/Sheet1/", "D4", 4444.0).
?test(sheet1_E4, "/Sheet1/", "E4", 5555.0).
?test(sheet1_F4, "/Sheet1/", "F4", 6666.0).
?test(sheet1_G4, "/Sheet1/", "G4", 7777.0).
?test(sheet1_H4, "/Sheet1/", "H4", 8888.0).
?test(sheet1_I4, "/Sheet1/", "I4", 9999.0).
?test(sheet1_A5, "/Sheet1/", "A5", 123456789.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_block_of_numbers.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_block_of_numbers" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_B1,
        sheet1_C1,
        sheet1_D1,
        sheet1_E1,
        sheet1_F1,
        sheet1_G1,
        sheet1_H1,
        sheet1_I1,
        sheet1_A2,
        sheet1_B2,
        sheet1_C2,
        sheet1_D2,
        sheet1_E2,
        sheet1_F2,
        sheet1_G2,
        sheet1_H2,
        sheet1_I2,
        sheet1_A3,
        sheet1_B3,
        sheet1_C3,
        sheet1_D3,
        sheet1_E3,
        sheet1_F3,
        sheet1_G3,
        sheet1_H3,
        sheet1_I3,
        sheet1_A4,
        sheet1_B4,
        sheet1_C4,
        sheet1_D4,
        sheet1_E4,
        sheet1_F4,
        sheet1_G4,
        sheet1_H4,
        sheet1_I4,
        sheet1_A5
    ].