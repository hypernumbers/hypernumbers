%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_financial_SUITE).
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
                     [Testcase, "d_gnumeric_financial_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_financial" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "FINANCIAL FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_C3, "/Sheet1/", "C3", "Accuracy Limit").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_C4, "/Sheet1/", "C4", 0.0001).
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 53.0).
?test(sheet1_B8, "/Sheet1/", "B8", 53.0).
?test(sheet1_A10, "/Sheet1/", "A10", "Test data").
?test(sheet1_A11, "/Sheet1/", "A11", -70000.0).
?test(sheet1_A12, "/Sheet1/", "A12", 12000.0).
?test(sheet1_A13, "/Sheet1/", "A13", 15000.0).
?test(sheet1_A14, "/Sheet1/", "A14", 18000.0).
?test(sheet1_A15, "/Sheet1/", "A15", 21000.0).
?test(sheet1_A16, "/Sheet1/", "A16", 26000.0).
?test(sheet1_A20, "/Sheet1/", "A20", "Function").
?test(sheet1_B20, "/Sheet1/", "B20", "1st test").
?test(sheet1_C20, "/Sheet1/", "C20", "Correct").
?test(sheet1_D20, "/Sheet1/", "D20", "2nd test").
?test(sheet1_E20, "/Sheet1/", "E20", "Correct").
?test(sheet1_F20, "/Sheet1/", "F20", "3rd test").
?test(sheet1_G20, "/Sheet1/", "G20", "Correct").
?test(sheet1_H20, "/Sheet1/", "H20", "Status").
?test(sheet1_I20, "/Sheet1/", "I20", "Status message").
?test(sheet1_A21, "/Sheet1/", "A21", "ACCRINT").
?test(sheet1_B21, "/Sheet1/", "B21", 16.9444444444444).
?test(sheet1_C21, "/Sheet1/", "C21", 16.9444444444444).
?test(sheet1_D21, "/Sheet1/", "D21", 16.986301369863).
?test(sheet1_E21, "/Sheet1/", "E21", 16.986301369863).
?test(sheet1_F21, "/Sheet1/", "F21", 44.7777777777778).
?test(sheet1_G21, "/Sheet1/", "G21", 44.7777777777778).
?test(sheet1_H21, "/Sheet1/", "H21", 1.0).
?test(sheet1_I21, "/Sheet1/", "I21", "Ok.").
?test(sheet1_A22, "/Sheet1/", "A22", "ACCRINTM").
?test(sheet1_B22, "/Sheet1/", "B22", 24.6575342465753).
?test(sheet1_C22, "/Sheet1/", "C22", 24.6575342465753).
?test(sheet1_D22, "/Sheet1/", "D22", 20.5555555555556).
?test(sheet1_E22, "/Sheet1/", "E22", 20.5555555555556).
?test(sheet1_F22, "/Sheet1/", "F22", 29.1666666666667).
?test(sheet1_G22, "/Sheet1/", "G22", 29.1666666666667).
?test(sheet1_H22, "/Sheet1/", "H22", 1.0).
?test(sheet1_I22, "/Sheet1/", "I22", "Ok.").
?test(sheet1_A23, "/Sheet1/", "A23", "AMORDEGRC").
?test(sheet1_B23, "/Sheet1/", "B23", 733.0).
?test(sheet1_C23, "/Sheet1/", "C23", 733.0).
?test(sheet1_D23, "/Sheet1/", "D23", 476.0).
?test(sheet1_E23, "/Sheet1/", "E23", 476.0).
?test(sheet1_F23, "/Sheet1/", "F23", 310.0).
?test(sheet1_G23, "/Sheet1/", "G23", 310.0).
?test(sheet1_H23, "/Sheet1/", "H23", 1.0).
?test(sheet1_I23, "/Sheet1/", "I23", "Ok.").
?test(sheet1_A24, "/Sheet1/", "A24", "AMORLINC").
?test(sheet1_B24, "/Sheet1/", "B24", 360.0).
?test(sheet1_C24, "/Sheet1/", "C24", 360.0).
?test(sheet1_D24, "/Sheet1/", "D24", 1063.33333333333).
?test(sheet1_E24, "/Sheet1/", "E24", 1063.33333333333).
?test(sheet1_F24, "/Sheet1/", "F24", 312.0).
?test(sheet1_G24, "/Sheet1/", "G24", 312.0).
?test(sheet1_H24, "/Sheet1/", "H24", 1.0).
?test(sheet1_I24, "/Sheet1/", "I24", "Ok.").
?test(sheet1_A25, "/Sheet1/", "A25", "COUPDAYBS").
?test(sheet1_B25, "/Sheet1/", "B25", 71.0).
?test(sheet1_C25, "/Sheet1/", "C25", 71.0).
?test(sheet1_D25, "/Sheet1/", "D25", 70.0).
?test(sheet1_E25, "/Sheet1/", "E25", 70.0).
?test(sheet1_F25, "/Sheet1/", "F25", 71.0).
?test(sheet1_G25, "/Sheet1/", "G25", 71.0).
?test(sheet1_H25, "/Sheet1/", "H25", 1.0).
?test(sheet1_I25, "/Sheet1/", "I25", "Ok.").
?test(sheet1_A26, "/Sheet1/", "A26", "COUPDAYS").
?test(sheet1_B26, "/Sheet1/", "B26", 181.0).
?test(sheet1_C26, "/Sheet1/", "C26", 181.0).
?test(sheet1_D26, "/Sheet1/", "D26", 180.0).
?test(sheet1_E26, "/Sheet1/", "E26", 180.0).
?test(sheet1_F26, "/Sheet1/", "F26", 90.0).
?test(sheet1_G26, "/Sheet1/", "G26", 90.0).
?test(sheet1_H26, "/Sheet1/", "H26", 1.0).
?test(sheet1_I26, "/Sheet1/", "I26", "Ok.").
?test(sheet1_A27, "/Sheet1/", "A27", "COUPDAYSNC").
?test(sheet1_B27, "/Sheet1/", "B27", 110.0).
?test(sheet1_C27, "/Sheet1/", "C27", 110.0).
?test(sheet1_D27, "/Sheet1/", "D27", 21.0).
?test(sheet1_E27, "/Sheet1/", "E27", 21.0).
?test(sheet1_F27, "/Sheet1/", "F27", 294.0).
?test(sheet1_G27, "/Sheet1/", "G27", 294.0).
?test(sheet1_H27, "/Sheet1/", "H27", 1.0).
?test(sheet1_I27, "/Sheet1/", "I27", "Ok.").
?test(sheet1_A28, "/Sheet1/", "A28", "COUPNCD").
?test(sheet1_B28, "/Sheet1/", "B28", 35930.0).
?test(sheet1_C28, "/Sheet1/", "C28", 35930.0).
?test(sheet1_D28, "/Sheet1/", "D28", 35841.0).
?test(sheet1_E28, "/Sheet1/", "E28", 35841.0).
?test(sheet1_F28, "/Sheet1/", "F28", 36114.0).
?test(sheet1_G28, "/Sheet1/", "G28", 36114.0).
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_A29, "/Sheet1/", "A29", "COUPNUM").
?test(sheet1_B29, "/Sheet1/", "B29", 4.0).
?test(sheet1_C29, "/Sheet1/", "C29", 4.0).
?test(sheet1_D29, "/Sheet1/", "D29", 8.0).
?test(sheet1_E29, "/Sheet1/", "E29", 8.0).
?test(sheet1_F29, "/Sheet1/", "F29", 2.0).
?test(sheet1_G29, "/Sheet1/", "G29", 2.0).
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_A30, "/Sheet1/", "A30", "COUPPCD").
?test(sheet1_B30, "/Sheet1/", "B30", 35749.0).
?test(sheet1_C30, "/Sheet1/", "C30", 35749.0).
?test(sheet1_D30, "/Sheet1/", "D30", 35750.0).
?test(sheet1_E30, "/Sheet1/", "E30", 35750.0).
?test(sheet1_F30, "/Sheet1/", "F30", 35749.0).
?test(sheet1_G30, "/Sheet1/", "G30", 35749.0).
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_A31, "/Sheet1/", "A31", "CUMIPMT").
?test(sheet1_B31, "/Sheet1/", "B31", -11135.2321307508).
?test(sheet1_C31, "/Sheet1/", "C31", -11135.2321307508).
?test(sheet1_D31, "/Sheet1/", "D31", -11052.3395838718).
?test(sheet1_E31, "/Sheet1/", "E31", -11052.3395838718).
?test(sheet1_F31, "/Sheet1/", "F31", -12066.8831126228).
?test(sheet1_G31, "/Sheet1/", "G31", -12066.8831126228).
?test(sheet1_H31, "/Sheet1/", "H31", 1.0).
?test(sheet1_I31, "/Sheet1/", "I31", "Ok.").
?test(sheet1_A32, "/Sheet1/", "A32", "CUMPRINC").
?test(sheet1_B32, "/Sheet1/", "B32", -934.107123420868).
?test(sheet1_C32, "/Sheet1/", "C32", -934.107123420874).
?test(sheet1_D32, "/Sheet1/", "D32", -927.153472378032).
?test(sheet1_E32, "/Sheet1/", "E32", -927.153472378039).
?test(sheet1_F32, "/Sheet1/", "F32", -961.463598360312).
?test(sheet1_G32, "/Sheet1/", "G32", -961.463598360318).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "DB").
?test(sheet1_B33, "/Sheet1/", "B33", 186083.333333333).
?test(sheet1_C33, "/Sheet1/", "C33", 186083.333333333).
?test(sheet1_D33, "/Sheet1/", "D33", 259639.416666667).
?test(sheet1_E33, "/Sheet1/", "E33", 259639.416666667).
?test(sheet1_F33, "/Sheet1/", "F33", 123000.0).
?test(sheet1_G33, "/Sheet1/", "G33", 123000.0).
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "DDB").
?test(sheet1_B34, "/Sheet1/", "B34", 1.31506849315068).
?test(sheet1_C34, "/Sheet1/", "C34", 1.31506849315068).
?test(sheet1_D34, "/Sheet1/", "D34", 1.38811728395062).
?test(sheet1_E34, "/Sheet1/", "E34", 1.38811728395062).
?test(sheet1_F34, "/Sheet1/", "F34", 11.6859441328818).
?test(sheet1_G34, "/Sheet1/", "G34", 11.6859441328818).
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "DISC").
?test(sheet1_B35, "/Sheet1/", "B35", 0.0312875536480688).
?test(sheet1_C35, "/Sheet1/", "C35", 0.0312875536480688).
?test(sheet1_D35, "/Sheet1/", "D35", 0.031722103004292).
?test(sheet1_E35, "/Sheet1/", "E35", 0.031722103004292).
?test(sheet1_F35, "/Sheet1/", "F35", 0.0316738197424892).
?test(sheet1_G35, "/Sheet1/", "G35", 0.0316738197424892).
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_A36, "/Sheet1/", "A36", "DOLLARDE").
?test(sheet1_B36, "/Sheet1/", "B36", 1.125).
?test(sheet1_C36, "/Sheet1/", "C36", 1.125).
?test(sheet1_D36, "/Sheet1/", "D36", 1.16666666666667).
?test(sheet1_E36, "/Sheet1/", "E36", 1.16666666666667).
?test(sheet1_F36, "/Sheet1/", "F36", 2.42857142857143).
?test(sheet1_G36, "/Sheet1/", "G36", 2.42857142857143).
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_A37, "/Sheet1/", "A37", "DOLLARFR").
?test(sheet1_B37, "/Sheet1/", "B37", 1.02).
?test(sheet1_C37, "/Sheet1/", "C37", 1.02).
?test(sheet1_D37, "/Sheet1/", "D37", 1.075).
?test(sheet1_E37, "/Sheet1/", "E37", 1.075).
?test(sheet1_F37, "/Sheet1/", "F37", 1.036).
?test(sheet1_G37, "/Sheet1/", "G37", 1.036).
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_A38, "/Sheet1/", "A38", "DURATION").
?test(sheet1_B38, "/Sheet1/", "B38", 5.99377495554519).
?test(sheet1_C38, "/Sheet1/", "C38", 5.99377495554519).
?test(sheet1_D38, "/Sheet1/", "D38", 6.14906788677325).
?test(sheet1_E38, "/Sheet1/", "E38", 6.14906788677324).
?test(sheet1_F38, "/Sheet1/", "F38", 5.17013121142132).
?test(sheet1_G38, "/Sheet1/", "G38", 5.17013121142132).
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_A39, "/Sheet1/", "A39", "EFFECT").
?test(sheet1_B39, "/Sheet1/", "B39", 27.696487426555).
?test(sheet1_C39, "/Sheet1/", "C39", 27.6964874265551).
?test(sheet1_D39, "/Sheet1/", "D39", 46.0184984576).
?test(sheet1_E39, "/Sheet1/", "E39", 46.0184984576).
?test(sheet1_F39, "/Sheet1/", "F39", 41.4207474827766).
?test(sheet1_G39, "/Sheet1/", "G39", 41.4207474827766).
?test(sheet1_H39, "/Sheet1/", "H39", 1.0).
?test(sheet1_I39, "/Sheet1/", "I39", "Ok.").
?test(sheet1_A40, "/Sheet1/", "A40", "FV").
?test(sheet1_B40, "/Sheet1/", "B40", 731.153396824367).
?test(sheet1_C40, "/Sheet1/", "C40", 731.153396824367).
?test(sheet1_D40, "/Sheet1/", "D40", 730.130594183551).
?test(sheet1_E40, "/Sheet1/", "E40", 730.130594183551).
?test(sheet1_F40, "/Sheet1/", "F40", 568.139564999998).
?test(sheet1_G40, "/Sheet1/", "G40", 568.139564999998).
?test(sheet1_H40, "/Sheet1/", "H40", 1.0).
?test(sheet1_I40, "/Sheet1/", "I40", "Ok.").
?test(sheet1_A41, "/Sheet1/", "A41", "FVSCHEDULE").
?test(sheet1_B41, "/Sheet1/", "B41", 1.33089).
?test(sheet1_C41, "/Sheet1/", "C41", 1.33089).
?test(sheet1_D41, "/Sheet1/", "D41", 2.66178).
?test(sheet1_E41, "/Sheet1/", "E41", 2.66178).
?test(sheet1_F41, "/Sheet1/", "F41", 1.377732).
?test(sheet1_G41, "/Sheet1/", "G41", 1.377732).
?test(sheet1_H41, "/Sheet1/", "H41", 1.0).
?test(sheet1_I41, "/Sheet1/", "I41", "Ok.").
?test(sheet1_A42, "/Sheet1/", "A42", "INTRATE").
?test(sheet1_B42, "/Sheet1/", "B42", 0.0583280898876405).
?test(sheet1_C42, "/Sheet1/", "C42", 0.0583280898876405).
?test(sheet1_D42, "/Sheet1/", "D42", 0.059138202247191).
?test(sheet1_E42, "/Sheet1/", "E42", 0.059138202247191).
?test(sheet1_F42, "/Sheet1/", "F42", 36.9877752808989).
?test(sheet1_G42, "/Sheet1/", "G42", 36.9877752808989).
?test(sheet1_H42, "/Sheet1/", "H42", 1.0).
?test(sheet1_I42, "/Sheet1/", "I42", "Ok.").
?test(sheet1_A43, "/Sheet1/", "A43", "IPMT").
?test(sheet1_B43, "/Sheet1/", "B43", -800.0).
?test(sheet1_C43, "/Sheet1/", "C43", -800.0).
?test(sheet1_D43, "/Sheet1/", "D43", -794.383652785133).
?test(sheet1_E43, "/Sheet1/", "E43", -794.383652785133).
?test(sheet1_F43, "/Sheet1/", "F43", -582.259600990719).
?test(sheet1_G43, "/Sheet1/", "G43", -582.259600990719).
?test(sheet1_H43, "/Sheet1/", "H43", 1.0).
?test(sheet1_I43, "/Sheet1/", "I43", "Ok.").
?test(sheet1_A44, "/Sheet1/", "A44", "IRR").
?test(sheet1_B44, "/Sheet1/", "B44", 0.0866309480363425).
?test(sheet1_C44, "/Sheet1/", "C44", 0.0866309480363425).
?test(sheet1_D44, "/Sheet1/", "D44", -0.0212448482730205).
?test(sheet1_E44, "/Sheet1/", "E44", -0.0212448482730205).
?test(sheet1_F44, "/Sheet1/", "F44", 0.0866309480363425).
?test(sheet1_G44, "/Sheet1/", "G44", 0.0866309480363425).
?test(sheet1_H44, "/Sheet1/", "H44", 1.0).
?test(sheet1_I44, "/Sheet1/", "I44", "Ok.").
?test(sheet1_A45, "/Sheet1/", "A45", "ISPMT").
?test(sheet1_B45, "/Sheet1/", "B45", -777777.777777778).
?test(sheet1_C45, "/Sheet1/", "C45", -777777.777777778).
?test(sheet1_D45, "/Sheet1/", "D45", -755555.555555556).
?test(sheet1_E45, "/Sheet1/", "E45", -755555.555555556).
?test(sheet1_F45, "/Sheet1/", "F45", -66666.6666666667).
?test(sheet1_G45, "/Sheet1/", "G45", -66666.6666666667).
?test(sheet1_H45, "/Sheet1/", "H45", 1.0).
?test(sheet1_I45, "/Sheet1/", "I45", "Ok.").
?test(sheet1_A46, "/Sheet1/", "A46", "MDURATION").
?test(sheet1_B46, "/Sheet1/", "B46", 5.73566981391884).
?test(sheet1_C46, "/Sheet1/", "C46", 5.73566981391884).
?test(sheet1_D46, "/Sheet1/", "D46", 4.94749398222136).
?test(sheet1_E46, "/Sheet1/", "E46", 4.94749398222136).
?test(sheet1_F46, "/Sheet1/", "F46", 5.78450172048619).
?test(sheet1_G46, "/Sheet1/", "G46", 5.78450172048618).
?test(sheet1_H46, "/Sheet1/", "H46", 1.0).
?test(sheet1_I46, "/Sheet1/", "I46", "Ok.").
?test(sheet1_A47, "/Sheet1/", "A47", "MIRR").
?test(sheet1_B47, "/Sheet1/", "B47", 0.0986691073371502).
?test(sheet1_C47, "/Sheet1/", "C47", 0.0986691073371502).
?test(sheet1_D47, "/Sheet1/", "D47", 0.102307932029938).
?test(sheet1_E47, "/Sheet1/", "E47", 0.102307932029938).
?test(sheet1_F47, "/Sheet1/", "F47", 0.109627339393136).
?test(sheet1_G47, "/Sheet1/", "G47", 0.109627339393136).
?test(sheet1_H47, "/Sheet1/", "H47", 1.0).
?test(sheet1_I47, "/Sheet1/", "I47", "Ok.").
?test(sheet1_A48, "/Sheet1/", "A48", "NOMINAL").
?test(sheet1_B48, "/Sheet1/", "B48", 0.0521992942005811).
?test(sheet1_C48, "/Sheet1/", "C48", 0.0521992942005811).
?test(sheet1_D48, "/Sheet1/", "D48", 0.0511643234486265).
?test(sheet1_E48, "/Sheet1/", "E48", 0.0511643234486265).
?test(sheet1_F48, "/Sheet1/", "F48", 0.0532300000000001).
?test(sheet1_G48, "/Sheet1/", "G48", 0.0532300000000001).
?test(sheet1_H48, "/Sheet1/", "H48", 1.0).
?test(sheet1_I48, "/Sheet1/", "I48", "Ok.").
?test(sheet1_A49, "/Sheet1/", "A49", "NPER").
?test(sheet1_B49, "/Sheet1/", "B49", 15.2880146199901).
?test(sheet1_C49, "/Sheet1/", "C49", 15.2880146199901).
?test(sheet1_D49, "/Sheet1/", "D49", 15.6755722726758).
?test(sheet1_E49, "/Sheet1/", "E49", 15.6755722726758).
?test(sheet1_F49, "/Sheet1/", "F49", 14.843018249563).
?test(sheet1_G49, "/Sheet1/", "G49", 14.843018249563).
?test(sheet1_H49, "/Sheet1/", "H49", 1.0).
?test(sheet1_I49, "/Sheet1/", "I49", "Ok.").
?test(sheet1_A50, "/Sheet1/", "A50", "NPV").
?test(sheet1_B50, "/Sheet1/", "B50", 1287.92943111482).
?test(sheet1_C50, "/Sheet1/", "C50", 1287.92943111482).
?test(sheet1_D50, "/Sheet1/", "D50", -1046.03214993372).
?test(sheet1_E50, "/Sheet1/", "E50", -1046.03214993372).
?test(sheet1_F50, "/Sheet1/", "F50", 120.948640590551).
?test(sheet1_G50, "/Sheet1/", "G50", 120.948640590551).
?test(sheet1_H50, "/Sheet1/", "H50", 1.0).
?test(sheet1_I50, "/Sheet1/", "I50", "Ok.").
?test(sheet1_A51, "/Sheet1/", "A51", "ODDFPRICE").
?test(sheet1_B51, "/Sheet1/", "B51", 113.598506930947).
?test(sheet1_C51, "/Sheet1/", "C51", 113.598506930947).
?test(sheet1_D51, "/Sheet1/", "D51", 113.599205828238).
?test(sheet1_E51, "/Sheet1/", "E51", 113.599205828238).
?test(sheet1_F51, "/Sheet1/", "F51", 113.650021611091).
?test(sheet1_G51, "/Sheet1/", "G51", 113.650021611091).
?test(sheet1_H51, "/Sheet1/", "H51", 1.0).
?test(sheet1_I51, "/Sheet1/", "I51", "Ok.").
?test(sheet1_A52, "/Sheet1/", "A52", "ODDFYIELD").
?test(sheet1_B52, "/Sheet1/", "B52", 26.6623945461366).
?test(sheet1_C52, "/Sheet1/", "C52", 26.6623945461366).
?test(sheet1_D52, "/Sheet1/", "D52", 27.3846040787552).
?test(sheet1_E52, "/Sheet1/", "E52", 27.3846040787553).
?test(sheet1_F52, "/Sheet1/", "F52", 12.6150786877774).
?test(sheet1_G52, "/Sheet1/", "G52", 12.6150786877484).
?test(sheet1_H52, "/Sheet1/", "H52", 1.0).
?test(sheet1_I52, "/Sheet1/", "I52", "Ok.").
?test(sheet1_A53, "/Sheet1/", "A53", "ODDLPRICE").
?test(sheet1_B53, "/Sheet1/", "B53", 110.882869098244).
?test(sheet1_C53, "/Sheet1/", "C53", 110.882869098244).
?test(sheet1_D53, "/Sheet1/", "D53", 110.876447541458).
?test(sheet1_E53, "/Sheet1/", "E53", 110.876447541458).
?test(sheet1_F53, "/Sheet1/", "F53", 167.40887498637).
?test(sheet1_G53, "/Sheet1/", "G53", 167.40887498637).
?test(sheet1_H53, "/Sheet1/", "H53", 1.0).
?test(sheet1_I53, "/Sheet1/", "I53", "Ok.").
?test(sheet1_A54, "/Sheet1/", "A54", "ODDLYIELD").
?test(sheet1_B54, "/Sheet1/", "B54", 25.3736957556897).
?test(sheet1_C54, "/Sheet1/", "C54", 25.3736957556897).
?test(sheet1_D54, "/Sheet1/", "D54", 24.8953796585306).
?test(sheet1_E54, "/Sheet1/", "E54", 24.8953796585306).
?test(sheet1_F54, "/Sheet1/", "F54", 38.2841454497638).
?test(sheet1_G54, "/Sheet1/", "G54", 38.2841454497638).
?test(sheet1_H54, "/Sheet1/", "H54", 1.0).
?test(sheet1_I54, "/Sheet1/", "I54", "Ok.").
?test(sheet1_A55, "/Sheet1/", "A55", "PMT").
?test(sheet1_B55, "/Sheet1/", "B55", -0.712189201293568).
?test(sheet1_C55, "/Sheet1/", "C55", -0.712189201293568).
?test(sheet1_D55, "/Sheet1/", "D55", -1.42177480301833).
?test(sheet1_E55, "/Sheet1/", "E55", -1.42177480301833).
?test(sheet1_F55, "/Sheet1/", "F55", -0.537405054177032).
?test(sheet1_G55, "/Sheet1/", "G55", -0.537405054177032).
?test(sheet1_H55, "/Sheet1/", "H55", 1.0).
?test(sheet1_I55, "/Sheet1/", "I55", "Ok.").
?test(sheet1_A56, "/Sheet1/", "A56", "PPMT").
?test(sheet1_B56, "/Sheet1/", "B56", -39.3580099670867).
?test(sheet1_C56, "/Sheet1/", "C56", -39.3580099670867).
?test(sheet1_D56, "/Sheet1/", "D56", -41.719490565112).
?test(sheet1_E56, "/Sheet1/", "E56", -41.719490565112).
?test(sheet1_F56, "/Sheet1/", "F56", -100.880007092922).
?test(sheet1_G56, "/Sheet1/", "G56", -100.880007092922).
?test(sheet1_H56, "/Sheet1/", "H56", 1.0).
?test(sheet1_I56, "/Sheet1/", "I56", "Ok.").
?test(sheet1_A57, "/Sheet1/", "A57", "PRICE").
?test(sheet1_B57, "/Sheet1/", "B57", 95.042874399392).
?test(sheet1_C57, "/Sheet1/", "C57", 95.042874399392).
?test(sheet1_D57, "/Sheet1/", "D57", 95.0446242216248).
?test(sheet1_E57, "/Sheet1/", "E57", 95.0446242216248).
?test(sheet1_F57, "/Sheet1/", "F57", 95.0249306475994).
?test(sheet1_G57, "/Sheet1/", "G57", 95.0249306475995).
?test(sheet1_H57, "/Sheet1/", "H57", 1.0).
?test(sheet1_I57, "/Sheet1/", "I57", "Ok.").
?test(sheet1_A58, "/Sheet1/", "A58", "PRICEDISC").
?test(sheet1_B58, "/Sheet1/", "B58", 95.6395833333333).
?test(sheet1_C58, "/Sheet1/", "C58", 95.6395833333333).
?test(sheet1_D58, "/Sheet1/", "D58", 95.6993150684931).
?test(sheet1_E58, "/Sheet1/", "E58", 95.6993150684931).
?test(sheet1_F58, "/Sheet1/", "F58", 191.279166666667).
?test(sheet1_G58, "/Sheet1/", "G58", 191.279166666667).
?test(sheet1_H58, "/Sheet1/", "H58", 1.0).
?test(sheet1_I58, "/Sheet1/", "I58", "Ok.").
?test(sheet1_A59, "/Sheet1/", "A59", "PRICEMAT").
?test(sheet1_B59, "/Sheet1/", "B59", -0.155106652205351).
?test(sheet1_C59, "/Sheet1/", "C59", -0.155106652205351).
?test(sheet1_D59, "/Sheet1/", "D59", -0.116277800197569).
?test(sheet1_E59, "/Sheet1/", "E59", -0.116277800197569).
?test(sheet1_F59, "/Sheet1/", "F59", -0.83970614992722).
?test(sheet1_G59, "/Sheet1/", "G59", -0.83970614992722).
?test(sheet1_H59, "/Sheet1/", "H59", 1.0).
?test(sheet1_I59, "/Sheet1/", "I59", "Ok.").
?test(sheet1_A60, "/Sheet1/", "A60", "PV").
?test(sheet1_B60, "/Sheet1/", "B60", -6249.99994056535).
?test(sheet1_C60, "/Sheet1/", "C60", -6249.99994056535).
?test(sheet1_D60, "/Sheet1/", "D60", -6749.99993580906).
?test(sheet1_E60, "/Sheet1/", "E60", -6749.99993580906).
?test(sheet1_F60, "/Sheet1/", "F60", -1249.99998811878).
?test(sheet1_G60, "/Sheet1/", "G60", -1249.99998811878).
?test(sheet1_H60, "/Sheet1/", "H60", 1.0).
?test(sheet1_I60, "/Sheet1/", "I60", "Ok.").
?test(sheet1_A61, "/Sheet1/", "A61", "RATE").
?test(sheet1_B61, "/Sheet1/", "B61", 0.00770147248820137).
?test(sheet1_C61, "/Sheet1/", "C61", 0.00770147248820137).
?test(sheet1_D61, "/Sheet1/", "D61", -0.263371645959229).
?test(sheet1_E61, "/Sheet1/", "E61", -0.263371645959229).
?test(sheet1_F61, "/Sheet1/", "F61", -0.0192014817274456).
?test(sheet1_G61, "/Sheet1/", "G61", -0.0192014817274456).
?test(sheet1_H61, "/Sheet1/", "H61", 1.0).
?test(sheet1_I61, "/Sheet1/", "I61", "Ok.").
?test(sheet1_A62, "/Sheet1/", "A62", "RECEIVED").
?test(sheet1_B62, "/Sheet1/", "B62", 100951.326037731).
?test(sheet1_C62, "/Sheet1/", "C62", 100951.326037731).
?test(sheet1_D62, "/Sheet1/", "D62", 100938.171912917).
?test(sheet1_E62, "/Sheet1/", "E62", 100938.171912917).
?test(sheet1_F62, "/Sheet1/", "F62", 100909.587812364).
?test(sheet1_G62, "/Sheet1/", "G62", 100909.587812364).
?test(sheet1_H62, "/Sheet1/", "H62", 1.0).
?test(sheet1_I62, "/Sheet1/", "I62", "Ok.").
?test(sheet1_A63, "/Sheet1/", "A63", "SLN").
?test(sheet1_B63, "/Sheet1/", "B63", 2250.0).
?test(sheet1_C63, "/Sheet1/", "C63", 2250.0).
?test(sheet1_D63, "/Sheet1/", "D63", 3214.28571428571).
?test(sheet1_E63, "/Sheet1/", "E63", 3214.28571428571).
?test(sheet1_F63, "/Sheet1/", "F63", 2850.0).
?test(sheet1_G63, "/Sheet1/", "G63", 2850.0).
?test(sheet1_H63, "/Sheet1/", "H63", 1.0).
?test(sheet1_I63, "/Sheet1/", "I63", "Ok.").
?test(sheet1_A64, "/Sheet1/", "A64", "SYD").
?test(sheet1_B64, "/Sheet1/", "B64", 4090.90909090909).
?test(sheet1_C64, "/Sheet1/", "C64", 4090.90909090909).
?test(sheet1_D64, "/Sheet1/", "D64", 3681.81818181818).
?test(sheet1_E64, "/Sheet1/", "E64", 3681.81818181818).
?test(sheet1_F64, "/Sheet1/", "F64", 4500.0).
?test(sheet1_G64, "/Sheet1/", "G64", 4500.0).
?test(sheet1_H64, "/Sheet1/", "H64", 1.0).
?test(sheet1_I64, "/Sheet1/", "I64", "Ok.").
?test(sheet1_A65, "/Sheet1/", "A65", "TBILLEQ").
?test(sheet1_B65, "/Sheet1/", "B65", 0.0238287743121293).
?test(sheet1_C65, "/Sheet1/", "C65", 0.0238287743121293).
?test(sheet1_D65, "/Sheet1/", "D65", 0.0670630198529214).
?test(sheet1_E65, "/Sheet1/", "E65", 0.0670630198529214).
?test(sheet1_F65, "/Sheet1/", "F65", 0.241010113788246).
?test(sheet1_G65, "/Sheet1/", "G65", 0.241010113788246).
?test(sheet1_H65, "/Sheet1/", "H65", 1.0).
?test(sheet1_I65, "/Sheet1/", "I65", "Ok.").
?test(sheet1_A66, "/Sheet1/", "A66", "TBILLPRICE").
?test(sheet1_B66, "/Sheet1/", "B66", 98.45).
?test(sheet1_C66, "/Sheet1/", "C66", 98.45).
?test(sheet1_D66, "/Sheet1/", "D66", 96.7277777777778).
?test(sheet1_E66, "/Sheet1/", "E66", 96.7277777777778).
?test(sheet1_F66, "/Sheet1/", "F66", 97.7).
?test(sheet1_G66, "/Sheet1/", "G66", 97.7).
?test(sheet1_H66, "/Sheet1/", "H66", 1.0).
?test(sheet1_I66, "/Sheet1/", "I66", "Ok.").
?test(sheet1_A67, "/Sheet1/", "A67", "TBILLYIELD").
?test(sheet1_B67, "/Sheet1/", "B67", 6445.8064516129).
?test(sheet1_C67, "/Sheet1/", "C67", 6445.8064516129).
?test(sheet1_D67, "/Sheet1/", "D67", 3050.220713073).
?test(sheet1_E67, "/Sheet1/", "E67", 3050.220713073).
?test(sheet1_F67, "/Sheet1/", "F67", 4343.91304347826).
?test(sheet1_G67, "/Sheet1/", "G67", 4343.91304347826).
?test(sheet1_H67, "/Sheet1/", "H67", 1.0).
?test(sheet1_I67, "/Sheet1/", "I67", "Ok.").
?test(sheet1_A68, "/Sheet1/", "A68", "VDB").
?test(sheet1_B68, "/Sheet1/", "B68", 116.424).
?test(sheet1_C68, "/Sheet1/", "C68", 116.424).
?test(sheet1_D68, "/Sheet1/", "D68", 131.9472).
?test(sheet1_E68, "/Sheet1/", "E68", 131.9472).
?test(sheet1_F68, "/Sheet1/", "F68", 194.04).
?test(sheet1_G68, "/Sheet1/", "G68", 194.04).
?test(sheet1_H68, "/Sheet1/", "H68", 1.0).
?test(sheet1_I68, "/Sheet1/", "I68", "Ok.").
?test(sheet1_A69, "/Sheet1/", "A69", "XIRR").
?test(sheet1_B69, "/Sheet1/", "B69", 0.42845830321312).
?test(sheet1_C69, "/Sheet1/", "C69", 0.42845830321312).
?test(sheet1_D69, "/Sheet1/", "D69", 0.417076354026794).
?test(sheet1_E69, "/Sheet1/", "E69", 0.417076354026794).
?test(sheet1_F69, "/Sheet1/", "F69", 0.42300886631012).
?test(sheet1_G69, "/Sheet1/", "G69", 0.42300886631012).
?test(sheet1_H69, "/Sheet1/", "H69", 1.0).
?test(sheet1_I69, "/Sheet1/", "I69", "Ok.").
?test(sheet1_A70, "/Sheet1/", "A70", "XNPV").
?test(sheet1_B70, "/Sheet1/", "B70", 2083.79424177736).
?test(sheet1_C70, "/Sheet1/", "C70", 2083.79424177736).
?test(sheet1_D70, "/Sheet1/", "D70", 2676.53362441346).
?test(sheet1_E70, "/Sheet1/", "E70", 2676.53362441346).
?test(sheet1_F70, "/Sheet1/", "F70", 2054.22321426475).
?test(sheet1_G70, "/Sheet1/", "G70", 2054.22321426475).
?test(sheet1_H70, "/Sheet1/", "H70", 1.0).
?test(sheet1_I70, "/Sheet1/", "I70", "Ok.").
?test(sheet1_A71, "/Sheet1/", "A71", "YIELD").
?test(sheet1_B71, "/Sheet1/", "B71", 0.0650008983776999).
?test(sheet1_C71, "/Sheet1/", "C71", 0.0650008983776999).
?test(sheet1_D71, "/Sheet1/", "D71", 0.0649730031730504).
?test(sheet1_E71, "/Sheet1/", "E71", 0.0649730031730506).
?test(sheet1_F71, "/Sheet1/", "F71", 0.131403015235581).
?test(sheet1_G71, "/Sheet1/", "G71", 0.131403015235581).
?test(sheet1_H71, "/Sheet1/", "H71", 1.0).
?test(sheet1_I71, "/Sheet1/", "I71", "Ok.").
?test(sheet1_A72, "/Sheet1/", "A72", "YIELDDISC").
?test(sheet1_B72, "/Sheet1/", "B72", 195.845682792407).
?test(sheet1_C72, "/Sheet1/", "C72", 195.845682792407).
?test(sheet1_D72, "/Sheet1/", "D72", 198.686654116864).
?test(sheet1_E72, "/Sheet1/", "E72", 198.686654116864).
?test(sheet1_F72, "/Sheet1/", "F72", 391.804041641151).
?test(sheet1_G72, "/Sheet1/", "G72", 391.804041641151).
?test(sheet1_H72, "/Sheet1/", "H72", 1.0).
?test(sheet1_I72, "/Sheet1/", "I72", "Ok.").
?test(sheet1_A73, "/Sheet1/", "A73", "YIELDMAT").
?test(sheet1_B73, "/Sheet1/", "B73", 0.0566316480630335).
?test(sheet1_C73, "/Sheet1/", "C73", 0.0566316480630335).
?test(sheet1_D73, "/Sheet1/", "D73", -0.633163543124186).
?test(sheet1_E73, "/Sheet1/", "E73", -0.633163543124186).
?test(sheet1_F73, "/Sheet1/", "F73", 0.151151631477927).
?test(sheet1_G73, "/Sheet1/", "G73", 0.151151631477927).
?test(sheet1_H73, "/Sheet1/", "H73", 1.0).
?test(sheet1_I73, "/Sheet1/", "I73", "Ok.").
?test(sheet1_A74, "/Sheet1/", "A74", "Total").
?test(sheet1_H74, "/Sheet1/", "H74", true).
?test(sheet1_I74, "/Sheet1/", "I74", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_financial.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_financial" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A12,
        sheet1_A13,
        sheet1_A14,
        sheet1_A15,
        sheet1_A16,
        sheet1_A20,
        sheet1_B20,
        sheet1_C20,
        sheet1_D20,
        sheet1_E20,
        sheet1_F20,
        sheet1_G20,
        sheet1_H20,
        sheet1_I20,
        sheet1_A21,
        sheet1_B21,
        sheet1_C21,
        sheet1_D21,
        sheet1_E21,
        sheet1_F21,
        sheet1_G21,
        sheet1_H21,
        sheet1_I21,
        sheet1_A22,
        sheet1_B22,
        sheet1_C22,
        sheet1_D22,
        sheet1_E22,
        sheet1_F22,
        sheet1_G22,
        sheet1_H22,
        sheet1_I22,
        sheet1_A23,
        sheet1_B23,
        sheet1_C23,
        sheet1_D23,
        sheet1_E23,
        sheet1_F23,
        sheet1_G23,
        sheet1_H23,
        sheet1_I23,
        sheet1_A24,
        sheet1_B24,
        sheet1_C24,
        sheet1_D24,
        sheet1_E24,
        sheet1_F24,
        sheet1_G24,
        sheet1_H24,
        sheet1_I24,
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
        sheet1_B45,
        sheet1_C45,
        sheet1_D45,
        sheet1_E45,
        sheet1_F45,
        sheet1_G45,
        sheet1_H45,
        sheet1_I45,
        sheet1_A46,
        sheet1_B46,
        sheet1_C46,
        sheet1_D46,
        sheet1_E46,
        sheet1_F46,
        sheet1_G46,
        sheet1_H46,
        sheet1_I46,
        sheet1_A47,
        sheet1_B47,
        sheet1_C47,
        sheet1_D47,
        sheet1_E47,
        sheet1_F47,
        sheet1_G47,
        sheet1_H47,
        sheet1_I47,
        sheet1_A48,
        sheet1_B48,
        sheet1_C48,
        sheet1_D48,
        sheet1_E48,
        sheet1_F48,
        sheet1_G48,
        sheet1_H48,
        sheet1_I48,
        sheet1_A49,
        sheet1_B49,
        sheet1_C49,
        sheet1_D49,
        sheet1_E49,
        sheet1_F49,
        sheet1_G49,
        sheet1_H49,
        sheet1_I49,
        sheet1_A50,
        sheet1_B50,
        sheet1_C50,
        sheet1_D50,
        sheet1_E50,
        sheet1_F50,
        sheet1_G50,
        sheet1_H50,
        sheet1_I50,
        sheet1_A51,
        sheet1_B51,
        sheet1_C51,
        sheet1_D51,
        sheet1_E51,
        sheet1_F51,
        sheet1_G51,
        sheet1_H51,
        sheet1_I51,
        sheet1_A52,
        sheet1_B52,
        sheet1_C52,
        sheet1_D52,
        sheet1_E52,
        sheet1_F52,
        sheet1_G52,
        sheet1_H52,
        sheet1_I52,
        sheet1_A53,
        sheet1_B53,
        sheet1_C53,
        sheet1_D53,
        sheet1_E53,
        sheet1_F53,
        sheet1_G53,
        sheet1_H53,
        sheet1_I53,
        sheet1_A54,
        sheet1_B54,
        sheet1_C54,
        sheet1_D54,
        sheet1_E54,
        sheet1_F54,
        sheet1_G54,
        sheet1_H54,
        sheet1_I54,
        sheet1_A55,
        sheet1_B55,
        sheet1_C55,
        sheet1_D55,
        sheet1_E55,
        sheet1_F55,
        sheet1_G55,
        sheet1_H55,
        sheet1_I55,
        sheet1_A56,
        sheet1_B56,
        sheet1_C56,
        sheet1_D56,
        sheet1_E56,
        sheet1_F56,
        sheet1_G56,
        sheet1_H56,
        sheet1_I56,
        sheet1_A57,
        sheet1_B57,
        sheet1_C57,
        sheet1_D57,
        sheet1_E57,
        sheet1_F57,
        sheet1_G57,
        sheet1_H57,
        sheet1_I57,
        sheet1_A58,
        sheet1_B58,
        sheet1_C58,
        sheet1_D58,
        sheet1_E58,
        sheet1_F58,
        sheet1_G58,
        sheet1_H58,
        sheet1_I58,
        sheet1_A59,
        sheet1_B59,
        sheet1_C59,
        sheet1_D59,
        sheet1_E59,
        sheet1_F59,
        sheet1_G59,
        sheet1_H59,
        sheet1_I59,
        sheet1_A60,
        sheet1_B60,
        sheet1_C60,
        sheet1_D60,
        sheet1_E60,
        sheet1_F60,
        sheet1_G60,
        sheet1_H60,
        sheet1_I60,
        sheet1_A61,
        sheet1_B61,
        sheet1_C61,
        sheet1_D61,
        sheet1_E61,
        sheet1_F61,
        sheet1_G61,
        sheet1_H61,
        sheet1_I61,
        sheet1_A62,
        sheet1_B62,
        sheet1_C62,
        sheet1_D62,
        sheet1_E62,
        sheet1_F62,
        sheet1_G62,
        sheet1_H62,
        sheet1_I62,
        sheet1_A63,
        sheet1_B63,
        sheet1_C63,
        sheet1_D63,
        sheet1_E63,
        sheet1_F63,
        sheet1_G63,
        sheet1_H63,
        sheet1_I63,
        sheet1_A64,
        sheet1_B64,
        sheet1_C64,
        sheet1_D64,
        sheet1_E64,
        sheet1_F64,
        sheet1_G64,
        sheet1_H64,
        sheet1_I64,
        sheet1_A65,
        sheet1_B65,
        sheet1_C65,
        sheet1_D65,
        sheet1_E65,
        sheet1_F65,
        sheet1_G65,
        sheet1_H65,
        sheet1_I65,
        sheet1_A66,
        sheet1_B66,
        sheet1_C66,
        sheet1_D66,
        sheet1_E66,
        sheet1_F66,
        sheet1_G66,
        sheet1_H66,
        sheet1_I66,
        sheet1_A67,
        sheet1_B67,
        sheet1_C67,
        sheet1_D67,
        sheet1_E67,
        sheet1_F67,
        sheet1_G67,
        sheet1_H67,
        sheet1_I67,
        sheet1_A68,
        sheet1_B68,
        sheet1_C68,
        sheet1_D68,
        sheet1_E68,
        sheet1_F68,
        sheet1_G68,
        sheet1_H68,
        sheet1_I68,
        sheet1_A69,
        sheet1_B69,
        sheet1_C69,
        sheet1_D69,
        sheet1_E69,
        sheet1_F69,
        sheet1_G69,
        sheet1_H69,
        sheet1_I69,
        sheet1_A70,
        sheet1_B70,
        sheet1_C70,
        sheet1_D70,
        sheet1_E70,
        sheet1_F70,
        sheet1_G70,
        sheet1_H70,
        sheet1_I70,
        sheet1_A71,
        sheet1_B71,
        sheet1_C71,
        sheet1_D71,
        sheet1_E71,
        sheet1_F71,
        sheet1_G71,
        sheet1_H71,
        sheet1_I71,
        sheet1_A72,
        sheet1_B72,
        sheet1_C72,
        sheet1_D72,
        sheet1_E72,
        sheet1_F72,
        sheet1_G72,
        sheet1_H72,
        sheet1_I72,
        sheet1_A73,
        sheet1_B73,
        sheet1_C73,
        sheet1_D73,
        sheet1_E73,
        sheet1_F73,
        sheet1_G73,
        sheet1_H73,
        sheet1_I73,
        sheet1_A74,
        sheet1_H74,
        sheet1_I74
    ].

-define(HNSERVER, "http://127.0.0.1:9000").

hnget(Path, Ref) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    {ok, {{_V, Code, _R}, _H, Body}} = http:request(get, {Url, []}, [], []),
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
