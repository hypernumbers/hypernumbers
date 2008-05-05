%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_three_dee_ref_SUITE).
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
                     [Testcase, "b_three_dee_ref_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_three_dee_ref" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Tom/", "A1", "This Spreadsheet tests 3D references").
?test(sheet1_A2, "/Tom/", "A2", "Simple Ref").
?test(sheet1_B2, "/Tom/", "B2", 444.0).
?test(sheet1_A3, "/Tom/", "A3", "Simple Range").
?test(sheet1_B3, "/Tom/", "B3", 10.0).
?test(sheet1_A4, "/Tom/", "A4", "3D Range").
?test(sheet1_B4, "/Tom/", "B4", 999.0).
?test(sheet1_A6, "/Tom/", "A6", "And now some errors - the sheet bob has been deleted").
?test(sheet1_A7, "/Tom/", "A7", "Simple Ref").
?test(sheet1_B7, "/Tom/", "B7", '#REF!').
?test(sheet1_A8, "/Tom/", "A8", "Simple Range").
?test(sheet1_B8, "/Tom/", "B8", '#REF!').
?test(sheet2_A1, "/Dick/", "A1", 444.0).
?test(sheet2_A2, "/Dick/", "A2", 1.0).
?test(sheet2_B2, "/Dick/", "B2", 2.0).
?test(sheet2_C2, "/Dick/", "C2", 3.0).
?test(sheet2_D2, "/Dick/", "D2", 4.0).
?test(sheet3_A1, "/Harry/", "A1", 555.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_three_dee_ref.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_three_dee_ref" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet2_A1,
        sheet2_A2,
        sheet2_B2,
        sheet2_C2,
        sheet2_D2,
        sheet3_A1
    ].