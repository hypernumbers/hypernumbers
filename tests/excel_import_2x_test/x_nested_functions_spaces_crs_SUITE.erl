%% This file is generated; DO NOT EDIT MANUALLY.

-module(x_nested_functions_spaces_crs_SUITE).
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
                     [Testcase, "x_nested_functions_spaces_crs_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "x_nested_functions_spaces_crs" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_B2, "/Sheet1/", "B2", 3.0).
?test(sheet1_B3, "/Sheet1/", "B3", 3.0).
?test(sheet1_B4, "/Sheet1/", "B4", 3.0).
?test(sheet1_B6, "/Sheet1/", "B6", 22.0).
?test(sheet1_B7, "/Sheet1/", "B7", 22.0).
?test(sheet1_B8, "/Sheet1/", "B8", 22.0).
?test(sheet1_B10, "/Sheet1/", "B10", -2.0).
?test(sheet1_B11, "/Sheet1/", "B11", -2.0).
?test(sheet1_B12, "/Sheet1/", "B12", -2.0).
?test(sheet1_B14, "/Sheet1/", "B14", 15.0).
?test(sheet1_B15, "/Sheet1/", "B15", 15.0).
?test(sheet1_B16, "/Sheet1/", "B16", 15.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
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
        sheet1_B2,
        sheet1_B3,
        sheet1_B4,
        sheet1_B6,
        sheet1_B7,
        sheet1_B8,
        sheet1_B10,
        sheet1_B11,
        sheet1_B12,
        sheet1_B14,
        sheet1_B15,
        sheet1_B16
    ].