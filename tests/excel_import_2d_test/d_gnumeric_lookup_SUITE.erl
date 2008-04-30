%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_lookup_SUITE).
-compile(export_all).
-include("ct.hrl").
-import(lists, [foreach/2, map/2]).

-define(print_error_or_return(Res, Testcase),
        case Res of
            true ->
                {test, ok};
            false ->
                io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~nCONV:~n    ~p~n~n",
                          [E, G, conv_from_get(G)]),
                exit("FAIL: Mismatch in ~p in ~p~n", 
                     [Testcase, "d_gnumeric_lookup_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_lookup" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "LOOKUP AND REFERENCE FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 16.0).
?test(sheet1_B8, "/Sheet1/", "B8", 16.0).
?test(sheet1_A10, "/Sheet1/", "A10", "Test data").
?test(sheet1_F10, "/Sheet1/", "F10", "f10").
?test(sheet1_G10, "/Sheet1/", "G10", "g10").
?test(sheet1_H10, "/Sheet1/", "H10", "h10").
?test(sheet1_B11, "/Sheet1/", "B11", 1.0).
?test(sheet1_D11, "/Sheet1/", "D11", 2.0).
?test(sheet1_F11, "/Sheet1/", "F11", "f11").
?test(sheet1_G11, "/Sheet1/", "G11", "g11").
?test(sheet1_H11, "/Sheet1/", "H11", "h11").
?test(sheet1_A12, "/Sheet1/", "A12", "B15").
?test(sheet1_B12, "/Sheet1/", "B12", 2.0).
?test(sheet1_D12, "/Sheet1/", "D12", 3.0).
?test(sheet1_F12, "/Sheet1/", "F12", "f12").
?test(sheet1_G12, "/Sheet1/", "G12", "g12").
?test(sheet1_H12, "/Sheet1/", "H12", "h12").
?test(sheet1_A13, "/Sheet1/", "A13", "B17").
?test(sheet1_B13, "/Sheet1/", "B13", 3.0).
?test(sheet1_D13, "/Sheet1/", "D13", 5.0).
?test(sheet1_A14, "/Sheet1/", "A14", "B20").
?test(sheet1_B14, "/Sheet1/", "B14", 4.0).
?test(sheet1_D14, "/Sheet1/", "D14", 2.0).
?test(sheet1_F14, "/Sheet1/", "F14", 3.131).
?test(sheet1_G14, "/Sheet1/", "G14", "black").
?test(sheet1_B15, "/Sheet1/", "B15", 5.0).
?test(sheet1_D15, "/Sheet1/", "D15", 4.0).
?test(sheet1_F15, "/Sheet1/", "F15", 3.233).
?test(sheet1_G15, "/Sheet1/", "G15", "red").
?test(sheet1_B16, "/Sheet1/", "B16", 6.0).
?test(sheet1_D16, "/Sheet1/", "D16", 6.0).
?test(sheet1_F16, "/Sheet1/", "F16", 4.721).
?test(sheet1_G16, "/Sheet1/", "G16", "green").
?test(sheet1_B17, "/Sheet1/", "B17", 7.0).
?test(sheet1_D17, "/Sheet1/", "D17", 3.0).
?test(sheet1_F17, "/Sheet1/", "F17", 4.923).
?test(sheet1_G17, "/Sheet1/", "G17", "yellow").
?test(sheet1_B18, "/Sheet1/", "B18", 8.0).
?test(sheet1_D18, "/Sheet1/", "D18", 2.0).
?test(sheet1_F18, "/Sheet1/", "F18", 5.212).
?test(sheet1_G18, "/Sheet1/", "G18", "violet").
?test(sheet1_B19, "/Sheet1/", "B19", 9.0).
?test(sheet1_D19, "/Sheet1/", "D19", 5.0).
?test(sheet1_F19, "/Sheet1/", "F19", 6.213).
?test(sheet1_G19, "/Sheet1/", "G19", "brown").
?test(sheet1_B20, "/Sheet1/", "B20", 10.0).
?test(sheet1_D20, "/Sheet1/", "D20", 1.0).
?test(sheet1_A21, "/Sheet1/", "A21", "x").
?test(sheet1_B21, "/Sheet1/", "B21", "y").
?test(sheet1_C21, "/Sheet1/", "C21", "z").
?test(sheet1_A22, "/Sheet1/", "A22", 4.2).
?test(sheet1_B22, "/Sheet1/", "B22", 5.2).
?test(sheet1_C22, "/Sheet1/", "C22", 7.1).
?test(sheet1_A23, "/Sheet1/", "A23", 2.3).
?test(sheet1_B23, "/Sheet1/", "B23", 4.9).
?test(sheet1_C23, "/Sheet1/", "C23", 6.3).
?test(sheet1_A24, "/Sheet1/", "A24", 3.4).
?test(sheet1_B24, "/Sheet1/", "B24", 5.9).
?test(sheet1_C24, "/Sheet1/", "C24", 7.0).
?test(sheet1_A25, "/Sheet1/", "A25", 0.4).
?test(sheet1_B25, "/Sheet1/", "B25", 7.2).
?test(sheet1_C25, "/Sheet1/", "C25", 2.2).
?test(sheet1_A27, "/Sheet1/", "A27", "Function").
?test(sheet1_B27, "/Sheet1/", "B27", "1st test").
?test(sheet1_C27, "/Sheet1/", "C27", "Correct").
?test(sheet1_D27, "/Sheet1/", "D27", "2nd test").
?test(sheet1_E27, "/Sheet1/", "E27", "Correct").
?test(sheet1_F27, "/Sheet1/", "F27", "3rd test").
?test(sheet1_G27, "/Sheet1/", "G27", "Correct").
?test(sheet1_H27, "/Sheet1/", "H27", "Status").
?test(sheet1_I27, "/Sheet1/", "I27", "Status message").
?test(sheet1_A28, "/Sheet1/", "A28", "ADDRESS").
?test(sheet1_B28, "/Sheet1/", "B28", "$C$2").
?test(sheet1_C28, "/Sheet1/", "C28", "$C$2").
?test(sheet1_D28, "/Sheet1/", "D28", "C$2").
?test(sheet1_E28, "/Sheet1/", "E28", "C$2").
?test(sheet1_F28, "/Sheet1/", "F28", "'EXCEL SHEET'!R2C3").
?test(sheet1_G28, "/Sheet1/", "G28", "'EXCEL SHEET'!R2C3").
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_A29, "/Sheet1/", "A29", "AREAS").
?test(sheet1_B29, "/Sheet1/", "B29", 1.0).
?test(sheet1_C29, "/Sheet1/", "C29", 1.0).
?test(sheet1_D29, "/Sheet1/", "D29", 2.0).
?test(sheet1_E29, "/Sheet1/", "E29", 2.0).
?test(sheet1_F29, "/Sheet1/", "F29", 4.0).
?test(sheet1_G29, "/Sheet1/", "G29", 4.0).
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_A30, "/Sheet1/", "A30", "CHOOSE").
?test(sheet1_B30, "/Sheet1/", "B30", 4.0).
?test(sheet1_C30, "/Sheet1/", "C30", 4.0).
?test(sheet1_D30, "/Sheet1/", "D30", 7.0).
?test(sheet1_E30, "/Sheet1/", "E30", 7.0).
?test(sheet1_F30, "/Sheet1/", "F30", 2.0).
?test(sheet1_G30, "/Sheet1/", "G30", 2.0).
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_A31, "/Sheet1/", "A31", "COLUMN").
?test(sheet1_B31, "/Sheet1/", "B31", 2.0).
?test(sheet1_C31, "/Sheet1/", "C31", 2.0).
?test(sheet1_D31, "/Sheet1/", "D31", 4.0).
?test(sheet1_E31, "/Sheet1/", "E31", 4.0).
?test(sheet1_F31, "/Sheet1/", "F31", 1.0).
?test(sheet1_G31, "/Sheet1/", "G31", 1.0).
?test(sheet1_H31, "/Sheet1/", "H31", 1.0).
?test(sheet1_I31, "/Sheet1/", "I31", "Ok.").
?test(sheet1_J31, "/Sheet1/", "J31", 1.0).
?test(sheet1_K31, "/Sheet1/", "K31", 2.0).
?test(sheet1_A32, "/Sheet1/", "A32", "COLUMNS").
?test(sheet1_B32, "/Sheet1/", "B32", 1.0).
?test(sheet1_C32, "/Sheet1/", "C32", 1.0).
?test(sheet1_D32, "/Sheet1/", "D32", 5.0).
?test(sheet1_E32, "/Sheet1/", "E32", 5.0).
?test(sheet1_F32, "/Sheet1/", "F32", 4.0).
?test(sheet1_G32, "/Sheet1/", "G32", 4.0).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "HLOOKUP").
?test(sheet1_B33, "/Sheet1/", "B33", 4.2).
?test(sheet1_C33, "/Sheet1/", "C33", 4.2).
?test(sheet1_D33, "/Sheet1/", "D33", 6.3).
?test(sheet1_E33, "/Sheet1/", "E33", 6.3).
?test(sheet1_F33, "/Sheet1/", "F33", "y").
?test(sheet1_G33, "/Sheet1/", "G33", "y").
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "HYPERLINK").
?test(sheet1_B34, "/Sheet1/", "B34", "finfuns.xls").
?test(sheet1_C34, "/Sheet1/", "C34", "finfuns.xls").
?test(sheet1_D34, "/Sheet1/", "D34", "Finfuns").
?test(sheet1_E34, "/Sheet1/", "E34", "Finfuns").
?test(sheet1_F34, "/Sheet1/", "F34", 1.0).
?test(sheet1_G34, "/Sheet1/", "G34", 1.0).
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "INDEX").
?test(sheet1_B35, "/Sheet1/", "B35", 4.0).
?test(sheet1_C35, "/Sheet1/", "C35", 4.0).
?test(sheet1_D35, "/Sheet1/", "D35", 42.0).
?test(sheet1_E35, "/Sheet1/", "E35", 42.0).
?test(sheet1_F35, "/Sheet1/", "F35", 5.0).
?test(sheet1_G35, "/Sheet1/", "G35", 5.0).
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_A36, "/Sheet1/", "A36", "INDIRECT").
?test(sheet1_B36, "/Sheet1/", "B36", 5.0).
?test(sheet1_C36, "/Sheet1/", "C36", 5.0).
?test(sheet1_D36, "/Sheet1/", "D36", "B17").
?test(sheet1_E36, "/Sheet1/", "E36", "B17").
?test(sheet1_F36, "/Sheet1/", "F36", 10.0).
?test(sheet1_G36, "/Sheet1/", "G36", 10.0).
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_A37, "/Sheet1/", "A37", "LOOKUP").
?test(sheet1_B37, "/Sheet1/", "B37", "red").
?test(sheet1_C37, "/Sheet1/", "C37", "red").
?test(sheet1_D37, "/Sheet1/", "D37", "yellow").
?test(sheet1_E37, "/Sheet1/", "E37", "yellow").
?test(sheet1_F37, "/Sheet1/", "F37", "brown").
?test(sheet1_G37, "/Sheet1/", "G37", "brown").
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_A38, "/Sheet1/", "A38", "MATCH").
?test(sheet1_B38, "/Sheet1/", "B38", 9.0).
?test(sheet1_C38, "/Sheet1/", "C38", 9.0).
?test(sheet1_D38, "/Sheet1/", "D38", 3.0).
?test(sheet1_E38, "/Sheet1/", "E38", 3.0).
?test(sheet1_F38, "/Sheet1/", "F38", 10.0).
?test(sheet1_G38, "/Sheet1/", "G38", 10.0).
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_A39, "/Sheet1/", "A39", "OFFSET").
?test(sheet1_B39, "/Sheet1/", "B39", "f10").
?test(sheet1_C39, "/Sheet1/", "C39", "f10").
?test(sheet1_D39, "/Sheet1/", "D39", "g12").
?test(sheet1_E39, "/Sheet1/", "E39", "g12").
?test(sheet1_F39, "/Sheet1/", "F39", 1.0).
?test(sheet1_G39, "/Sheet1/", "G39", 1.0).
?test(sheet1_H39, "/Sheet1/", "H39", 1.0).
?test(sheet1_I39, "/Sheet1/", "I39", "Ok.").
?test(sheet1_J39, "/Sheet1/", "J39", "f10").
?test(sheet1_K39, "/Sheet1/", "K39", "g10").
?test(sheet1_A40, "/Sheet1/", "A40", "ROW").
?test(sheet1_B40, "/Sheet1/", "B40", 40.0).
?test(sheet1_C40, "/Sheet1/", "C40", 40.0).
?test(sheet1_D40, "/Sheet1/", "D40", 35.0).
?test(sheet1_E40, "/Sheet1/", "E40", 35.0).
?test(sheet1_F40, "/Sheet1/", "F40", 1.0).
?test(sheet1_G40, "/Sheet1/", "G40", 1.0).
?test(sheet1_H40, "/Sheet1/", "H40", 1.0).
?test(sheet1_I40, "/Sheet1/", "I40", "Ok.").
?test(sheet1_J40, "/Sheet1/", "J40", 3.0).
?test(sheet1_K40, "/Sheet1/", "K40", 3.0).
?test(sheet1_A41, "/Sheet1/", "A41", "ROWS").
?test(sheet1_B41, "/Sheet1/", "B41", 5.0).
?test(sheet1_C41, "/Sheet1/", "C41", 5.0).
?test(sheet1_D41, "/Sheet1/", "D41", 3.0).
?test(sheet1_E41, "/Sheet1/", "E41", 3.0).
?test(sheet1_F41, "/Sheet1/", "F41", 3.0).
?test(sheet1_G41, "/Sheet1/", "G41", 3.0).
?test(sheet1_H41, "/Sheet1/", "H41", 1.0).
?test(sheet1_I41, "/Sheet1/", "I41", "Ok.").
?test(sheet1_A42, "/Sheet1/", "A42", "TRANSPOSE").
?test(sheet1_B42, "/Sheet1/", "B42", "f12").
?test(sheet1_C42, "/Sheet1/", "C42", "f12").
?test(sheet1_D42, "/Sheet1/", "D42", 1.0).
?test(sheet1_E42, "/Sheet1/", "E42", 1.0).
?test(sheet1_F42, "/Sheet1/", "F42", 1.0).
?test(sheet1_G42, "/Sheet1/", "G42", 1.0).
?test(sheet1_H42, "/Sheet1/", "H42", 1.0).
?test(sheet1_I42, "/Sheet1/", "I42", "Ok.").
?test(sheet1_J42, "/Sheet1/", "J42", "f10").
?test(sheet1_K42, "/Sheet1/", "K42", "f11").
?test(sheet1_L42, "/Sheet1/", "L42", "f12").
?test(sheet1_A43, "/Sheet1/", "A43", "VLOOKUP").
?test(sheet1_B43, "/Sheet1/", "B43", 7.2).
?test(sheet1_C43, "/Sheet1/", "C43", 7.2).
?test(sheet1_D43, "/Sheet1/", "D43", 4.9).
?test(sheet1_E43, "/Sheet1/", "E43", 4.9).
?test(sheet1_F43, "/Sheet1/", "F43", 2.3).
?test(sheet1_G43, "/Sheet1/", "G43", 2.3).
?test(sheet1_H43, "/Sheet1/", "H43", 1.0).
?test(sheet1_I43, "/Sheet1/", "I43", "Ok.").
?test(sheet1_J43, "/Sheet1/", "J43", "g10").
?test(sheet1_K43, "/Sheet1/", "K43", "g11").
?test(sheet1_L43, "/Sheet1/", "L43", "g12").
?test(sheet1_A44, "/Sheet1/", "A44", "Total").
?test(sheet1_J44, "/Sheet1/", "J44", "h10").
?test(sheet1_K44, "/Sheet1/", "K44", "h11").
?test(sheet1_L44, "/Sheet1/", "L44", "h12").
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_lookup.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_lookup" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_F10,
        sheet1_G10,
        sheet1_H10,
        sheet1_B11,
        sheet1_D11,
        sheet1_F11,
        sheet1_G11,
        sheet1_H11,
        sheet1_A12,
        sheet1_B12,
        sheet1_D12,
        sheet1_F12,
        sheet1_G12,
        sheet1_H12,
        sheet1_A13,
        sheet1_B13,
        sheet1_D13,
        sheet1_A14,
        sheet1_B14,
        sheet1_D14,
        sheet1_F14,
        sheet1_G14,
        sheet1_B15,
        sheet1_D15,
        sheet1_F15,
        sheet1_G15,
        sheet1_B16,
        sheet1_D16,
        sheet1_F16,
        sheet1_G16,
        sheet1_B17,
        sheet1_D17,
        sheet1_F17,
        sheet1_G17,
        sheet1_B18,
        sheet1_D18,
        sheet1_F18,
        sheet1_G18,
        sheet1_B19,
        sheet1_D19,
        sheet1_F19,
        sheet1_G19,
        sheet1_B20,
        sheet1_D20,
        sheet1_A21,
        sheet1_B21,
        sheet1_C21,
        sheet1_A22,
        sheet1_B22,
        sheet1_C22,
        sheet1_A23,
        sheet1_B23,
        sheet1_C23,
        sheet1_A24,
        sheet1_B24,
        sheet1_C24,
        sheet1_A25,
        sheet1_B25,
        sheet1_C25,
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
        sheet1_J31,
        sheet1_K31,
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
        sheet1_J39,
        sheet1_K39,
        sheet1_A40,
        sheet1_B40,
        sheet1_C40,
        sheet1_D40,
        sheet1_E40,
        sheet1_F40,
        sheet1_G40,
        sheet1_H40,
        sheet1_I40,
        sheet1_J40,
        sheet1_K40,
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
        sheet1_J42,
        sheet1_K42,
        sheet1_L42,
        sheet1_A43,
        sheet1_B43,
        sheet1_C43,
        sheet1_D43,
        sheet1_E43,
        sheet1_F43,
        sheet1_G43,
        sheet1_H43,
        sheet1_I43,
        sheet1_J43,
        sheet1_K43,
        sheet1_L43,
        sheet1_A44,
        sheet1_J44,
        sheet1_K44,
        sheet1_L44
    ].

-define(HNSERVER, "http://127.0.0.1:9000").

hnget(Path, Ref) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    {ok, {{V, Code, R}, H, Body}} = http:request(get, {Url, []}, [], []),
    io:format("Code for ~p~p is ~p.~nBody is: ~p~n~n", [Path, Ref, Code, Body]),
    Body.
  
hnpost(Path, Ref, Postdata) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    Postreq = "<create><formula>" ++ Postdata ++ "</formula></create>",
    Return = http:request(post,
                          {Url, [], "text/xml", Postreq},
                          [], []),
    {ok, {{_V, Code, _R}, _H, Body}} = Return,
    io:format("Posted ~p to ~p~p.~nResponse code: ~p. Response body: ~p.~n~n", 
              [Postdata, Path, Ref, Code, Body]),
    Return.

cmp(G, E) ->
    Val = conv_from_get(G),
    Val == E.

conv_from_get(Val) ->
    case Val of
        [34 | Tl] -> % String
            hslists:init(Tl);
        [39 | Tl] -> % Atom, i.e. an error value.
            list_to_atom(hslists:init(Tl));
        "TRUE" ->
            true;
        "FALSE" ->
            false;
        _ ->
            tconv:to_num(Val)
    end.

conv_for_post(Val) ->
    case Val of
        {_, boolean, true} -> "true";
        {_, boolean, fase} -> "false";
        {_, number, N}     -> tconv:to_s(N);
        {_, error, E}      -> E;
        {string, X}        -> "\"" ++ X ++ "\"";
        {formula, F}       -> F
    end.
