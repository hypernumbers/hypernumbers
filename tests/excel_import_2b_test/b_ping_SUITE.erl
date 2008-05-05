%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_ping_SUITE).
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
                     [Testcase, "b_ping_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_ping" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_B2, "/Ping_Sheet1/", "B2", "from Pong Sheet 1").
?test(sheet1_B3, "/Ping_Sheet1/", "B3", "from Pong Sheet 2").
?test(sheet1_B4, "/Ping_Sheet1/", "B4", "from Pong Sheet 3").
?test(sheet1_B5, "/Ping_Sheet1/", "B5", 15.0).
?test(sheet1_B6, "/Ping_Sheet1/", "B6", "from Pang Sheet 1").
?test(sheet1_B7, "/Ping_Sheet1/", "B7", "from Pang Sheet 2").
?test(sheet1_B8, "/Ping_Sheet1/", "B8", "from Pang Sheet 3").
?test(sheet1_B9, "/Ping_Sheet1/", "B9", 6.0).
?test(sheet1_B10, "/Ping_Sheet1/", "B10", "from Sheet 2 in Ping").
?test(sheet1_B11, "/Ping_Sheet1/", "B11", 10.0).
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
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
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
        sheet1_B2,
        sheet1_B3,
        sheet1_B4,
        sheet1_B5,
        sheet1_B6,
        sheet1_B7,
        sheet1_B8,
        sheet1_B9,
        sheet1_B10,
        sheet1_B11,
        sheet1_B12,
        sheet2_A1,
        sheet2_A2,
        sheet2_A3,
        sheet2_A4,
        sheet2_A5,
        sheet2_A8
    ].