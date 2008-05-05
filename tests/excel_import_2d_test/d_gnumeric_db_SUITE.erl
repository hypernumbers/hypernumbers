%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_db_SUITE).
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
                     [Testcase, "d_gnumeric_db_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_db" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "DATABASE FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_C3, "/Sheet1/", "C3", "Accuracy Limit").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_C4, "/Sheet1/", "C4", 0.001).
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 13.0).
?test(sheet1_B8, "/Sheet1/", "B8", 13.0).
?test(sheet1_A10, "/Sheet1/", "A10", "Test data").
?test(sheet1_A11, "/Sheet1/", "A11", "Age").
?test(sheet1_B11, "/Sheet1/", "B11", "Salary").
?test(sheet1_C11, "/Sheet1/", "C11", "Sex").
?test(sheet1_D11, "/Sheet1/", "D11", "Age").
?test(sheet1_E11, "/Sheet1/", "E11", "Salary").
?test(sheet1_G11, "/Sheet1/", "G11", "John").
?test(sheet1_H11, "/Sheet1/", "H11", "Jos").
?test(sheet1_I11, "/Sheet1/", "I11", "John").
?test(sheet1_A12, "/Sheet1/", "A12", ">30").
?test(sheet1_B12, "/Sheet1/", "B12", ">34000").
?test(sheet1_C12, "/Sheet1/", "C12", "male").
?test(sheet1_D12, "/Sheet1/", "D12", 22.0).
?test(sheet1_E12, "/Sheet1/", "E12", ">40000").
?test(sheet1_G12, "/Sheet1/", "G12", 23324.0).
?test(sheet1_H12, "/Sheet1/", "H12", 45345.0).
?test(sheet1_I12, "/Sheet1/", "I12", 27234.0).
?test(sheet1_A13, "/Sheet1/", "A13", 23.0).
?test(sheet1_D13, "/Sheet1/", "D13", 33.0).
?test(sheet1_G13, "/Sheet1/", "G13", 24433.0).
?test(sheet1_H13, "/Sheet1/", "H13", 34443.0).
?test(sheet1_I13, "/Sheet1/", "I13", 31323.0).
?test(sheet1_A15, "/Sheet1/", "A15", "Name").
?test(sheet1_B15, "/Sheet1/", "B15", "Age").
?test(sheet1_C15, "/Sheet1/", "C15", "Salary").
?test(sheet1_D15, "/Sheet1/", "D15", "Sex").
?test(sheet1_G15, "/Sheet1/", "G15", "John").
?test(sheet1_H15, "/Sheet1/", "H15", "Jos").
?test(sheet1_I15, "/Sheet1/", "I15", "John2").
?test(sheet1_A16, "/Sheet1/", "A16", "Bill").
?test(sheet1_B16, "/Sheet1/", "B16", 23.0).
?test(sheet1_C16, "/Sheet1/", "C16", 23532.0).
?test(sheet1_D16, "/Sheet1/", "D16", "male").
?test(sheet1_F16, "/Sheet1/", "F16", "Total").
?test(sheet1_G16, "/Sheet1/", "G16", 47757.0).
?test(sheet1_H16, "/Sheet1/", "H16", 79788.0).
?test(sheet1_I16, "/Sheet1/", "I16", 58557.0).
?test(sheet1_A17, "/Sheet1/", "A17", "Jack").
?test(sheet1_B17, "/Sheet1/", "B17", 32.0).
?test(sheet1_C17, "/Sheet1/", "C17", 34212.0).
?test(sheet1_D17, "/Sheet1/", "D17", "male").
?test(sheet1_A18, "/Sheet1/", "A18", "John").
?test(sheet1_B18, "/Sheet1/", "B18", 52.0).
?test(sheet1_C18, "/Sheet1/", "C18", 43213.0).
?test(sheet1_D18, "/Sheet1/", "D18", "male").
?test(sheet1_A19, "/Sheet1/", "A19", "Kathy").
?test(sheet1_B19, "/Sheet1/", "B19", 34.0).
?test(sheet1_C19, "/Sheet1/", "C19", 32332.0).
?test(sheet1_D19, "/Sheet1/", "D19", "female").
?test(sheet1_A20, "/Sheet1/", "A20", "Lisa").
?test(sheet1_B20, "/Sheet1/", "B20", 33.0).
?test(sheet1_C20, "/Sheet1/", "C20", 37324.0).
?test(sheet1_D20, "/Sheet1/", "D20", "female").
?test(sheet1_A21, "/Sheet1/", "A21", "Sean").
?test(sheet1_B21, "/Sheet1/", "B21", 43.0).
?test(sheet1_C21, "/Sheet1/", "C21", "not available").
?test(sheet1_D21, "/Sheet1/", "D21", "male").
?test(sheet1_A22, "/Sheet1/", "A22", "Tina").
?test(sheet1_B22, "/Sheet1/", "B22", 22.0).
?test(sheet1_C22, "/Sheet1/", "C22", 24322.0).
?test(sheet1_D22, "/Sheet1/", "D22", "female").
?test(sheet1_A31, "/Sheet1/", "A31", "Function").
?test(sheet1_B31, "/Sheet1/", "B31", "1st test").
?test(sheet1_C31, "/Sheet1/", "C31", "Correct").
?test(sheet1_D31, "/Sheet1/", "D31", "2nd test").
?test(sheet1_E31, "/Sheet1/", "E31", "Correct").
?test(sheet1_F31, "/Sheet1/", "F31", "3rd test").
?test(sheet1_G31, "/Sheet1/", "G31", "Correct").
?test(sheet1_H31, "/Sheet1/", "H31", "Status").
?test(sheet1_I31, "/Sheet1/", "I31", "Status message").
?test(sheet1_A32, "/Sheet1/", "A32", "DAVERAGE").
?test(sheet1_B32, "/Sheet1/", "B32", 39.0).
?test(sheet1_C32, "/Sheet1/", "C32", 39.0).
?test(sheet1_D32, "/Sheet1/", "D32", 34122.6).
?test(sheet1_E32, "/Sheet1/", "E32", 34122.6).
?test(sheet1_F32, "/Sheet1/", "F32", 35.6666666666667).
?test(sheet1_G32, "/Sheet1/", "G32", 35.6666666666667).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "DCOUNT").
?test(sheet1_B33, "/Sheet1/", "B33", 2.0).
?test(sheet1_C33, "/Sheet1/", "C33", 2.0).
?test(sheet1_D33, "/Sheet1/", "D33", 5.0).
?test(sheet1_E33, "/Sheet1/", "E33", 5.0).
?test(sheet1_F33, "/Sheet1/", "F33", 4.0).
?test(sheet1_G33, "/Sheet1/", "G33", 4.0).
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "DCOUNTA").
?test(sheet1_B34, "/Sheet1/", "B34", 6.0).
?test(sheet1_C34, "/Sheet1/", "C34", 6.0).
?test(sheet1_D34, "/Sheet1/", "D34", 6.0).
?test(sheet1_E34, "/Sheet1/", "E34", 6.0).
?test(sheet1_F34, "/Sheet1/", "F34", 4.0).
?test(sheet1_G34, "/Sheet1/", "G34", 4.0).
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "DGET").
?test(sheet1_B35, "/Sheet1/", "B35", 24322.0).
?test(sheet1_C35, "/Sheet1/", "C35", 24322.0).
?test(sheet1_D35, "/Sheet1/", "D35", 52.0).
?test(sheet1_E35, "/Sheet1/", "E35", 52.0).
?test(sheet1_F35, "/Sheet1/", "F35", "John").
?test(sheet1_G35, "/Sheet1/", "G35", "John").
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_A36, "/Sheet1/", "A36", "DMAX").
?test(sheet1_B36, "/Sheet1/", "B36", 43213.0).
?test(sheet1_C36, "/Sheet1/", "C36", 43213.0).
?test(sheet1_D36, "/Sheet1/", "D36", 43213.0).
?test(sheet1_E36, "/Sheet1/", "E36", 43213.0).
?test(sheet1_F36, "/Sheet1/", "F36", 52.0).
?test(sheet1_G36, "/Sheet1/", "G36", 52.0).
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_A37, "/Sheet1/", "A37", "DMIN").
?test(sheet1_B37, "/Sheet1/", "B37", 23532.0).
?test(sheet1_C37, "/Sheet1/", "C37", 23532.0).
?test(sheet1_D37, "/Sheet1/", "D37", 23532.0).
?test(sheet1_E37, "/Sheet1/", "E37", 23532.0).
?test(sheet1_F37, "/Sheet1/", "F37", 23.0).
?test(sheet1_G37, "/Sheet1/", "G37", 23.0).
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_A38, "/Sheet1/", "A38", "DPRODUCT").
?test(sheet1_B38, "/Sheet1/", "B38", 1846470912.0).
?test(sheet1_C38, "/Sheet1/", "C38", 1846470912.0).
?test(sheet1_D38, "/Sheet1/", "D38", 80281344.0).
?test(sheet1_E38, "/Sheet1/", "E38", 80281344.0).
?test(sheet1_F38, "/Sheet1/", "F38", 1645696.0).
?test(sheet1_G38, "/Sheet1/", "G38", 1645696.0).
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_A39, "/Sheet1/", "A39", "DSTDEV").
?test(sheet1_B39, "/Sheet1/", "B39", 10.0282933077701).
?test(sheet1_C39, "/Sheet1/", "C39", 10.0282933077701).
?test(sheet1_D39, "/Sheet1/", "D39", 7215.59739730536).
?test(sheet1_E39, "/Sheet1/", "E39", 7215.59739730536).
?test(sheet1_F39, "/Sheet1/", "F39", 12.6622799421484).
?test(sheet1_G39, "/Sheet1/", "G39", 12.6622799421484).
?test(sheet1_H39, "/Sheet1/", "H39", 1.0).
?test(sheet1_I39, "/Sheet1/", "I39", "Ok.").
?test(sheet1_A40, "/Sheet1/", "A40", "DSTDEVP").
?test(sheet1_B40, "/Sheet1/", "B40", 9.15453742990631).
?test(sheet1_C40, "/Sheet1/", "C40", 9.15453742990631).
?test(sheet1_D40, "/Sheet1/", "D40", 6453.82651145814).
?test(sheet1_E40, "/Sheet1/", "E40", 6453.82651145814).
?test(sheet1_F40, "/Sheet1/", "F40", 10.9658560997307).
?test(sheet1_G40, "/Sheet1/", "G40", 10.9658560997307).
?test(sheet1_H40, "/Sheet1/", "H40", 1.0).
?test(sheet1_I40, "/Sheet1/", "I40", "Ok.").
?test(sheet1_A41, "/Sheet1/", "A41", "DSUM").
?test(sheet1_B41, "/Sheet1/", "B41", 217.0).
?test(sheet1_C41, "/Sheet1/", "C41", 217.0).
?test(sheet1_D41, "/Sheet1/", "D41", 170613.0).
?test(sheet1_E41, "/Sheet1/", "E41", 170613.0).
?test(sheet1_F41, "/Sheet1/", "F41", 150.0).
?test(sheet1_G41, "/Sheet1/", "G41", 150.0).
?test(sheet1_H41, "/Sheet1/", "H41", 1.0).
?test(sheet1_I41, "/Sheet1/", "I41", "Ok.").
?test(sheet1_A42, "/Sheet1/", "A42", "DVAR").
?test(sheet1_B42, "/Sheet1/", "B42", 100.566666666667).
?test(sheet1_C42, "/Sheet1/", "C42", 100.566666666667).
?test(sheet1_D42, "/Sheet1/", "D42", 52064845.8).
?test(sheet1_E42, "/Sheet1/", "E42", 52064845.8).
?test(sheet1_F42, "/Sheet1/", "F42", 160.333333333333).
?test(sheet1_G42, "/Sheet1/", "G42", 160.333333333333).
?test(sheet1_H42, "/Sheet1/", "H42", 1.0).
?test(sheet1_I42, "/Sheet1/", "I42", "Ok.").
?test(sheet1_A43, "/Sheet1/", "A43", "DVARP").
?test(sheet1_B43, "/Sheet1/", "B43", 83.8055555555556).
?test(sheet1_C43, "/Sheet1/", "C43", 83.8055555555556).
?test(sheet1_D43, "/Sheet1/", "D43", 41651876.64).
?test(sheet1_E43, "/Sheet1/", "E43", 41651876.64).
?test(sheet1_F43, "/Sheet1/", "F43", 120.25).
?test(sheet1_G43, "/Sheet1/", "G43", 120.25).
?test(sheet1_H43, "/Sheet1/", "H43", 1.0).
?test(sheet1_I43, "/Sheet1/", "I43", "Ok.").
?test(sheet1_A44, "/Sheet1/", "A44", "GETPIVOTDATA").
?test(sheet1_B44, "/Sheet1/", "B44", 47757.0).
?test(sheet1_C44, "/Sheet1/", "C44", 47757.0).
?test(sheet1_D44, "/Sheet1/", "D44", 79788.0).
?test(sheet1_E44, "/Sheet1/", "E44", 79788.0).
?test(sheet1_F44, "/Sheet1/", "F44", 76446.0).
?test(sheet1_G44, "/Sheet1/", "G44", 76446.0).
?test(sheet1_H44, "/Sheet1/", "H44", 1.0).
?test(sheet1_I44, "/Sheet1/", "I44", "Ok.").
?test(sheet1_A45, "/Sheet1/", "A45", "Total").
?test(sheet1_H45, "/Sheet1/", "H45", true).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_db.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_db" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_C3,
        sheet1_A4,
        sheet1_C4,
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet1_A10,
        sheet1_A11,
        sheet1_B11,
        sheet1_C11,
        sheet1_D11,
        sheet1_E11,
        sheet1_G11,
        sheet1_H11,
        sheet1_I11,
        sheet1_A12,
        sheet1_B12,
        sheet1_C12,
        sheet1_D12,
        sheet1_E12,
        sheet1_G12,
        sheet1_H12,
        sheet1_I12,
        sheet1_A13,
        sheet1_D13,
        sheet1_G13,
        sheet1_H13,
        sheet1_I13,
        sheet1_A15,
        sheet1_B15,
        sheet1_C15,
        sheet1_D15,
        sheet1_G15,
        sheet1_H15,
        sheet1_I15,
        sheet1_A16,
        sheet1_B16,
        sheet1_C16,
        sheet1_D16,
        sheet1_F16,
        sheet1_G16,
        sheet1_H16,
        sheet1_I16,
        sheet1_A17,
        sheet1_B17,
        sheet1_C17,
        sheet1_D17,
        sheet1_A18,
        sheet1_B18,
        sheet1_C18,
        sheet1_D18,
        sheet1_A19,
        sheet1_B19,
        sheet1_C19,
        sheet1_D19,
        sheet1_A20,
        sheet1_B20,
        sheet1_C20,
        sheet1_D20,
        sheet1_A21,
        sheet1_B21,
        sheet1_C21,
        sheet1_D21,
        sheet1_A22,
        sheet1_B22,
        sheet1_C22,
        sheet1_D22,
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
        sheet1_A44,
        sheet1_B44,
        sheet1_C44,
        sheet1_D44,
        sheet1_E44,
        sheet1_F44,
        sheet1_G44,
        sheet1_H44,
        sheet1_I44,
        sheet1_A45,
        sheet1_H45
    ].