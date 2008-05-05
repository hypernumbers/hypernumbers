%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_information_SUITE).
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
                     [Testcase, "d_gnumeric_information_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_information" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "INFORMATION FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 18.0).
?test(sheet1_B8, "/Sheet1/", "B8", 18.0).
?test(sheet1_A10, "/Sheet1/", "A10", "Test data").
?test(sheet1_A12, "/Sheet1/", "A12", 0.0225).
?test(sheet1_B12, "/Sheet1/", "B12", 1.0).
?test(sheet1_A13, "/Sheet1/", "A13", "1999/11/11 00:00:00").
?test(sheet1_B13, "/Sheet1/", "B13", "koe").
?test(sheet1_A14, "/Sheet1/", "A14", "232323").
?test(sheet1_A16, "/Sheet1/", "A16", 22.0).
?test(sheet1_A18, "/Sheet1/", "A18", 12.8).
?test(sheet1_A19, "/Sheet1/", "A19", '#DIV/0!').
?test(sheet1_A20, "/Sheet1/", "A20", '#NUM!').
?test(sheet1_A21, "/Sheet1/", "A21", '#VALUE!').
?test(sheet1_A22, "/Sheet1/", "A22", '#N/A').
?test(sheet1_A25, "/Sheet1/", "A25", "Function").
?test(sheet1_B25, "/Sheet1/", "B25", "1st test").
?test(sheet1_C25, "/Sheet1/", "C25", "Correct").
?test(sheet1_D25, "/Sheet1/", "D25", "2nd test").
?test(sheet1_E25, "/Sheet1/", "E25", "Correct").
?test(sheet1_F25, "/Sheet1/", "F25", "3rd test").
?test(sheet1_G25, "/Sheet1/", "G25", "Correct").
?test(sheet1_H25, "/Sheet1/", "H25", "Status").
?test(sheet1_I25, "/Sheet1/", "I25", "Status message").
?test(sheet1_A26, "/Sheet1/", "A26", "CELL").
?test(sheet1_B26, "/Sheet1/", "B26", "P2").
?test(sheet1_C26, "/Sheet1/", "C26", "P2").
?test(sheet1_D26, "/Sheet1/", "D26", "D2").
?test(sheet1_E26, "/Sheet1/", "E26", "D2").
?test(sheet1_F26, "/Sheet1/", "F26", "C0-").
?test(sheet1_G26, "/Sheet1/", "G26", "C0-").
?test(sheet1_H26, "/Sheet1/", "H26", 1.0).
?test(sheet1_I26, "/Sheet1/", "I26", "Ok.").
?test(sheet1_A27, "/Sheet1/", "A27", "COUNTBLANK").
?test(sheet1_B27, "/Sheet1/", "B27", 2.0).
?test(sheet1_C27, "/Sheet1/", "C27", 2.0).
?test(sheet1_D27, "/Sheet1/", "D27", 2.0).
?test(sheet1_E27, "/Sheet1/", "E27", 2.0).
?test(sheet1_F27, "/Sheet1/", "F27", 0.0).
?test(sheet1_G27, "/Sheet1/", "G27", 0.0).
?test(sheet1_H27, "/Sheet1/", "H27", 1.0).
?test(sheet1_I27, "/Sheet1/", "I27", "Ok.").
?test(sheet1_A28, "/Sheet1/", "A28", "ERROR.TYPE").
?test(sheet1_B28, "/Sheet1/", "B28", 2.0).
?test(sheet1_C28, "/Sheet1/", "C28", 2.0).
?test(sheet1_D28, "/Sheet1/", "D28", 6.0).
?test(sheet1_E28, "/Sheet1/", "E28", 6.0).
?test(sheet1_F28, "/Sheet1/", "F28", 3.0).
?test(sheet1_G28, "/Sheet1/", "G28", 3.0).
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_A29, "/Sheet1/", "A29", "INFO").
?test(sheet1_B29, "/Sheet1/", "B29", "pcdos").
?test(sheet1_C29, "/Sheet1/", "C29", "pcdos").
?test(sheet1_D29, "/Sheet1/", "D29", "12.0").
?test(sheet1_E29, "/Sheet1/", "E29", "9.0").
?test(sheet1_F29, "/Sheet1/", "F29", "Windows (32-bit) NT 5.01").
?test(sheet1_G29, "/Sheet1/", "G29", "Windows (32-bit) 4.00").
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_A30, "/Sheet1/", "A30", "ISBLANK").
?test(sheet1_B30, "/Sheet1/", "B30", false).
?test(sheet1_C30, "/Sheet1/", "C30", false).
?test(sheet1_D30, "/Sheet1/", "D30", true).
?test(sheet1_E30, "/Sheet1/", "E30", true).
?test(sheet1_F30, "/Sheet1/", "F30", false).
?test(sheet1_G30, "/Sheet1/", "G30", false).
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_A31, "/Sheet1/", "A31", "ISERR").
?test(sheet1_B31, "/Sheet1/", "B31", false).
?test(sheet1_C31, "/Sheet1/", "C31", false).
?test(sheet1_D31, "/Sheet1/", "D31", true).
?test(sheet1_E31, "/Sheet1/", "E31", true).
?test(sheet1_F31, "/Sheet1/", "F31", false).
?test(sheet1_G31, "/Sheet1/", "G31", false).
?test(sheet1_H31, "/Sheet1/", "H31", 1.0).
?test(sheet1_I31, "/Sheet1/", "I31", "Ok.").
?test(sheet1_A32, "/Sheet1/", "A32", "ISERROR").
?test(sheet1_B32, "/Sheet1/", "B32", true).
?test(sheet1_C32, "/Sheet1/", "C32", true).
?test(sheet1_D32, "/Sheet1/", "D32", true).
?test(sheet1_E32, "/Sheet1/", "E32", true).
?test(sheet1_F32, "/Sheet1/", "F32", false).
?test(sheet1_G32, "/Sheet1/", "G32", false).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "ISEVEN").
?test(sheet1_B33, "/Sheet1/", "B33", false).
?test(sheet1_C33, "/Sheet1/", "C33", false).
?test(sheet1_D33, "/Sheet1/", "D33", true).
?test(sheet1_E33, "/Sheet1/", "E33", true).
?test(sheet1_F33, "/Sheet1/", "F33", true).
?test(sheet1_G33, "/Sheet1/", "G33", true).
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "ISLOGICAL").
?test(sheet1_B34, "/Sheet1/", "B34", false).
?test(sheet1_C34, "/Sheet1/", "C34", false).
?test(sheet1_D34, "/Sheet1/", "D34", false).
?test(sheet1_E34, "/Sheet1/", "E34", false).
?test(sheet1_F34, "/Sheet1/", "F34", true).
?test(sheet1_G34, "/Sheet1/", "G34", true).
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "ISNA").
?test(sheet1_B35, "/Sheet1/", "B35", false).
?test(sheet1_C35, "/Sheet1/", "C35", false).
?test(sheet1_D35, "/Sheet1/", "D35", false).
?test(sheet1_E35, "/Sheet1/", "E35", false).
?test(sheet1_F35, "/Sheet1/", "F35", true).
?test(sheet1_G35, "/Sheet1/", "G35", true).
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_A36, "/Sheet1/", "A36", "ISNONTEXT").
?test(sheet1_B36, "/Sheet1/", "B36", false).
?test(sheet1_C36, "/Sheet1/", "C36", false).
?test(sheet1_D36, "/Sheet1/", "D36", true).
?test(sheet1_E36, "/Sheet1/", "E36", true).
?test(sheet1_F36, "/Sheet1/", "F36", true).
?test(sheet1_G36, "/Sheet1/", "G36", true).
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_A37, "/Sheet1/", "A37", "ISNUMBER").
?test(sheet1_B37, "/Sheet1/", "B37", true).
?test(sheet1_C37, "/Sheet1/", "C37", true).
?test(sheet1_D37, "/Sheet1/", "D37", true).
?test(sheet1_E37, "/Sheet1/", "E37", true).
?test(sheet1_F37, "/Sheet1/", "F37", false).
?test(sheet1_G37, "/Sheet1/", "G37", false).
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_A38, "/Sheet1/", "A38", "ISODD").
?test(sheet1_B38, "/Sheet1/", "B38", true).
?test(sheet1_C38, "/Sheet1/", "C38", true).
?test(sheet1_D38, "/Sheet1/", "D38", false).
?test(sheet1_E38, "/Sheet1/", "E38", false).
?test(sheet1_F38, "/Sheet1/", "F38", true).
?test(sheet1_G38, "/Sheet1/", "G38", true).
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_A39, "/Sheet1/", "A39", "ISREF").
?test(sheet1_B39, "/Sheet1/", "B39", false).
?test(sheet1_C39, "/Sheet1/", "C39", false).
?test(sheet1_D39, "/Sheet1/", "D39", true).
?test(sheet1_E39, "/Sheet1/", "E39", true).
?test(sheet1_F39, "/Sheet1/", "F39", true).
?test(sheet1_G39, "/Sheet1/", "G39", true).
?test(sheet1_H39, "/Sheet1/", "H39", 1.0).
?test(sheet1_I39, "/Sheet1/", "I39", "Ok.").
?test(sheet1_A40, "/Sheet1/", "A40", "ISTEXT").
?test(sheet1_B40, "/Sheet1/", "B40", true).
?test(sheet1_C40, "/Sheet1/", "C40", true).
?test(sheet1_D40, "/Sheet1/", "D40", false).
?test(sheet1_E40, "/Sheet1/", "E40", false).
?test(sheet1_F40, "/Sheet1/", "F40", false).
?test(sheet1_G40, "/Sheet1/", "G40", false).
?test(sheet1_H40, "/Sheet1/", "H40", 1.0).
?test(sheet1_I40, "/Sheet1/", "I40", "Ok.").
?test(sheet1_A41, "/Sheet1/", "A41", "N").
?test(sheet1_B41, "/Sheet1/", "B41", 12.8).
?test(sheet1_C41, "/Sheet1/", "C41", 12.8).
?test(sheet1_D41, "/Sheet1/", "D41", 0.0).
?test(sheet1_E41, "/Sheet1/", "E41", 0.0).
?test(sheet1_F41, "/Sheet1/", "F41", 1.0).
?test(sheet1_G41, "/Sheet1/", "G41", 1.0).
?test(sheet1_H41, "/Sheet1/", "H41", 1.0).
?test(sheet1_I41, "/Sheet1/", "I41", "Ok.").
?test(sheet1_A42, "/Sheet1/", "A42", "NA").
?test(sheet1_B42, "/Sheet1/", "B42", '#N/A').
?test(sheet1_C42, "/Sheet1/", "C42", 0.0).
?test(sheet1_D42, "/Sheet1/", "D42", 0.0).
?test(sheet1_E42, "/Sheet1/", "E42", 0.0).
?test(sheet1_F42, "/Sheet1/", "F42", 0.0).
?test(sheet1_G42, "/Sheet1/", "G42", 0.0).
?test(sheet1_H42, "/Sheet1/", "H42", 1.0).
?test(sheet1_I42, "/Sheet1/", "I42", "Ok.").
?test(sheet1_A43, "/Sheet1/", "A43", "TYPE").
?test(sheet1_B43, "/Sheet1/", "B43", 16.0).
?test(sheet1_C43, "/Sheet1/", "C43", 16.0).
?test(sheet1_D43, "/Sheet1/", "D43", 2.0).
?test(sheet1_E43, "/Sheet1/", "E43", 2.0).
?test(sheet1_F43, "/Sheet1/", "F43", 1.0).
?test(sheet1_G43, "/Sheet1/", "G43", 1.0).
?test(sheet1_H43, "/Sheet1/", "H43", 1.0).
?test(sheet1_I43, "/Sheet1/", "I43", "Ok.").
?test(sheet1_A44, "/Sheet1/", "A44", "Total").
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_information.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_information" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet1_A10,
        sheet1_A12,
        sheet1_B12,
        sheet1_A13,
        sheet1_B13,
        sheet1_A14,
        sheet1_A16,
        sheet1_A18,
        sheet1_A19,
        sheet1_A20,
        sheet1_A21,
        sheet1_A22,
        sheet1_A25,
        sheet1_B25,
        sheet1_C25,
        sheet1_D25,
        sheet1_E25,
        sheet1_F25,
        sheet1_G25,
        sheet1_H25,
        sheet1_I25,
        sheet1_A26,
        sheet1_B26,
        sheet1_C26,
        sheet1_D26,
        sheet1_E26,
        sheet1_F26,
        sheet1_G26,
        sheet1_H26,
        sheet1_I26,
        sheet1_A27,
        sheet1_B27,
        sheet1_C27,
        sheet1_D27,
        sheet1_E27,
        sheet1_F27,
        sheet1_G27,
        sheet1_H27,
        sheet1_I27,
        sheet1_A28,
        sheet1_B28,
        sheet1_C28,
        sheet1_D28,
        sheet1_E28,
        sheet1_F28,
        sheet1_G28,
        sheet1_H28,
        sheet1_I28,
        sheet1_A29,
        sheet1_B29,
        sheet1_C29,
        sheet1_D29,
        sheet1_E29,
        sheet1_F29,
        sheet1_G29,
        sheet1_H29,
        sheet1_I29,
        sheet1_A30,
        sheet1_B30,
        sheet1_C30,
        sheet1_D30,
        sheet1_E30,
        sheet1_F30,
        sheet1_G30,
        sheet1_H30,
        sheet1_I30,
        sheet1_A31,
        sheet1_B31,
        sheet1_C31,
        sheet1_D31,
        sheet1_E31,
        sheet1_F31,
        sheet1_G31,
        sheet1_H31,
        sheet1_I31,
        sheet1_A32,
        sheet1_B32,
        sheet1_C32,
        sheet1_D32,
        sheet1_E32,
        sheet1_F32,
        sheet1_G32,
        sheet1_H32,
        sheet1_I32,
        sheet1_A33,
        sheet1_B33,
        sheet1_C33,
        sheet1_D33,
        sheet1_E33,
        sheet1_F33,
        sheet1_G33,
        sheet1_H33,
        sheet1_I33,
        sheet1_A34,
        sheet1_B34,
        sheet1_C34,
        sheet1_D34,
        sheet1_E34,
        sheet1_F34,
        sheet1_G34,
        sheet1_H34,
        sheet1_I34,
        sheet1_A35,
        sheet1_B35,
        sheet1_C35,
        sheet1_D35,
        sheet1_E35,
        sheet1_F35,
        sheet1_G35,
        sheet1_H35,
        sheet1_I35,
        sheet1_A36,
        sheet1_B36,
        sheet1_C36,
        sheet1_D36,
        sheet1_E36,
        sheet1_F36,
        sheet1_G36,
        sheet1_H36,
        sheet1_I36,
        sheet1_A37,
        sheet1_B37,
        sheet1_C37,
        sheet1_D37,
        sheet1_E37,
        sheet1_F37,
        sheet1_G37,
        sheet1_H37,
        sheet1_I37,
        sheet1_A38,
        sheet1_B38,
        sheet1_C38,
        sheet1_D38,
        sheet1_E38,
        sheet1_F38,
        sheet1_G38,
        sheet1_H38,
        sheet1_I38,
        sheet1_A39,
        sheet1_B39,
        sheet1_C39,
        sheet1_D39,
        sheet1_E39,
        sheet1_F39,
        sheet1_G39,
        sheet1_H39,
        sheet1_I39,
        sheet1_A40,
        sheet1_B40,
        sheet1_C40,
        sheet1_D40,
        sheet1_E40,
        sheet1_F40,
        sheet1_G40,
        sheet1_H40,
        sheet1_I40,
        sheet1_A41,
        sheet1_B41,
        sheet1_C41,
        sheet1_D41,
        sheet1_E41,
        sheet1_F41,
        sheet1_G41,
        sheet1_H41,
        sheet1_I41,
        sheet1_A42,
        sheet1_B42,
        sheet1_C42,
        sheet1_D42,
        sheet1_E42,
        sheet1_F42,
        sheet1_G42,
        sheet1_H42,
        sheet1_I42,
        sheet1_A43,
        sheet1_B43,
        sheet1_C43,
        sheet1_D43,
        sheet1_E43,
        sheet1_F43,
        sheet1_G43,
        sheet1_H43,
        sheet1_I43,
        sheet1_A44
    ].