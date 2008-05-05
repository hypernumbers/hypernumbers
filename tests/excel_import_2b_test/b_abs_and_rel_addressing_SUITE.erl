%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_abs_and_rel_addressing_SUITE).
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
                     [Testcase, "b_abs_and_rel_addressing_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_abs_and_rel_addressing" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_B4, "/Sheet1/", "B4", 11.0).
?test(sheet1_C4, "/Sheet1/", "C4", 11.0).
?test(sheet1_B5, "/Sheet1/", "B5", 22.0).
?test(sheet1_C5, "/Sheet1/", "C5", 22.0).
?test(sheet1_B6, "/Sheet1/", "B6", 33.0).
?test(sheet1_C6, "/Sheet1/", "C6", 33.0).
?test(sheet1_B7, "/Sheet1/", "B7", 44.0).
?test(sheet1_C7, "/Sheet1/", "C7", 44.0).
?test(sheet1_B10, "/Sheet1/", "B10", 60.5).
?test(sheet1_C10, "/Sheet1/", "C10", 55.0).
?test(sheet1_D10, "/Sheet1/", "D10", 111.0).
?test(sheet1_B11, "/Sheet1/", "B11", 71.5).
?test(sheet1_C11, "/Sheet1/", "C11", 66.0).
?test(sheet1_D11, "/Sheet1/", "D11", 222.0).
?test(sheet1_B12, "/Sheet1/", "B12", 82.5).
?test(sheet1_C12, "/Sheet1/", "C12", 77.0).
?test(sheet1_D12, "/Sheet1/", "D12", 333.0).
?test(sheet1_B13, "/Sheet1/", "B13", 49.5).
?test(sheet1_C13, "/Sheet1/", "C13", 88.0).
?test(sheet1_D13, "/Sheet1/", "D13", 444.0).
?test(sheet1_B14, "/Sheet1/", "B14", 16.5).
?test(sheet1_C14, "/Sheet1/", "C14", 11.0).
?test(sheet1_D14, "/Sheet1/", "D14", 44.0).
?test(sheet1_B15, "/Sheet1/", "B15", 46.2).
?test(sheet1_C15, "/Sheet1/", "C15", 22.0).
?test(sheet1_D15, "/Sheet1/", "D15", 55.0).
?test(sheet1_B16, "/Sheet1/", "B16", 230.6).
?test(sheet1_C16, "/Sheet1/", "C16", 33.0).
?test(sheet1_D16, "/Sheet1/", "D16", 66.0).
?test(sheet1_B17, "/Sheet1/", "B17", 943.5).
?test(sheet1_C17, "/Sheet1/", "C17", 999.0).
?test(sheet1_D17, "/Sheet1/", "D17", 777.0).
?test(sheet1_B18, "/Sheet1/", "B18", 832.5).
?test(sheet1_C18, "/Sheet1/", "C18", 888.0).
?test(sheet1_D18, "/Sheet1/", "D18", 888.0).
?test(sheet1_B19, "/Sheet1/", "B19", 777.0).
?test(sheet1_C19, "/Sheet1/", "C19", 777.0).
?test(sheet1_D19, "/Sheet1/", "D19", 999.0).
?test(sheet1_B22, "/Sheet1/", "B22", 11.0).
?test(sheet1_C22, "/Sheet1/", "C22", 11.0).
?test(sheet1_D22, "/Sheet1/", "D22", 0.0).
?test(sheet1_B23, "/Sheet1/", "B23", 22.0).
?test(sheet1_C23, "/Sheet1/", "C23", 22.0).
?test(sheet1_D23, "/Sheet1/", "D23", 22.0).
?test(sheet1_B24, "/Sheet1/", "B24", 22.0).
?test(sheet1_C24, "/Sheet1/", "C24", 22.0).
?test(sheet1_D24, "/Sheet1/", "D24", 0.0).
?test(sheet1_B25, "/Sheet1/", "B25", 22.0).
?test(sheet1_C25, "/Sheet1/", "C25", 22.0).
?test(sheet1_D25, "/Sheet1/", "D25", 22.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_abs_and_rel_addressing.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_abs_and_rel_addressing" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_B4,
        sheet1_C4,
        sheet1_B5,
        sheet1_C5,
        sheet1_B6,
        sheet1_C6,
        sheet1_B7,
        sheet1_C7,
        sheet1_B10,
        sheet1_C10,
        sheet1_D10,
        sheet1_B11,
        sheet1_C11,
        sheet1_D11,
        sheet1_B12,
        sheet1_C12,
        sheet1_D12,
        sheet1_B13,
        sheet1_C13,
        sheet1_D13,
        sheet1_B14,
        sheet1_C14,
        sheet1_D14,
        sheet1_B15,
        sheet1_C15,
        sheet1_D15,
        sheet1_B16,
        sheet1_C16,
        sheet1_D16,
        sheet1_B17,
        sheet1_C17,
        sheet1_D17,
        sheet1_B18,
        sheet1_C18,
        sheet1_D18,
        sheet1_B19,
        sheet1_C19,
        sheet1_D19,
        sheet1_B22,
        sheet1_C22,
        sheet1_D22,
        sheet1_B23,
        sheet1_C23,
        sheet1_D23,
        sheet1_B24,
        sheet1_C24,
        sheet1_D24,
        sheet1_B25,
        sheet1_C25,
        sheet1_D25
    ].