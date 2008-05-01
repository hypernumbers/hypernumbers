%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_stats_SUITE).
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
                     [Testcase, "d_gnumeric_stats_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_stats" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "STATISTICAL FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_C3, "/Sheet1/", "C3", "Accuracy Limit").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_C4, "/Sheet1/", "C4", 0.001).
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 78.0).
?test(sheet1_B8, "/Sheet1/", "B8", 78.0).
?test(sheet1_A11, "/Sheet1/", "A11", "Test Data:").
?test(sheet1_A13, "/Sheet1/", "A13", "x").
?test(sheet1_B13, "/Sheet1/", "B13", "y").
?test(sheet1_C13, "/Sheet1/", "C13", "z").
?test(sheet1_D13, "/Sheet1/", "D13", "neg").
?test(sheet1_E13, "/Sheet1/", "E13", "pos&neg").
?test(sheet1_F13, "/Sheet1/", "F13", "[0..1]").
?test(sheet1_G13, "/Sheet1/", "G13", "manytypes").
?test(sheet1_H13, "/Sheet1/", "H13", "same").
?test(sheet1_I13, "/Sheet1/", "I13", "mode").
?test(sheet1_A14, "/Sheet1/", "A14", 33.7).
?test(sheet1_B14, "/Sheet1/", "B14", 77.4).
?test(sheet1_C14, "/Sheet1/", "C14", 11.5).
?test(sheet1_D14, "/Sheet1/", "D14", -2.0).
?test(sheet1_E14, "/Sheet1/", "E14", 33.9).
?test(sheet1_F14, "/Sheet1/", "F14", 0.123543).
?test(sheet1_G14, "/Sheet1/", "G14", -1.9).
?test(sheet1_H14, "/Sheet1/", "H14", 33.7).
?test(sheet1_I14, "/Sheet1/", "I14", 2.0).
?test(sheet1_A15, "/Sheet1/", "A15", 21.5).
?test(sheet1_B15, "/Sheet1/", "B15", 55.3).
?test(sheet1_C15, "/Sheet1/", "C15", 12.3).
?test(sheet1_D15, "/Sheet1/", "D15", -5.0).
?test(sheet1_E15, "/Sheet1/", "E15", 23.3).
?test(sheet1_F15, "/Sheet1/", "F15", 0.44312).
?test(sheet1_G15, "/Sheet1/", "G15", 0.0).
?test(sheet1_H15, "/Sheet1/", "H15", 21.5).
?test(sheet1_I15, "/Sheet1/", "I15", 2.0).
?test(sheet1_A16, "/Sheet1/", "A16", 17.9).
?test(sheet1_B16, "/Sheet1/", "B16", 44.0).
?test(sheet1_C16, "/Sheet1/", "C16", 66.3).
?test(sheet1_D16, "/Sheet1/", "D16", -1.0).
?test(sheet1_E16, "/Sheet1/", "E16", -1.3).
?test(sheet1_F16, "/Sheet1/", "F16", 0.32342).
?test(sheet1_G16, "/Sheet1/", "G16", "text").
?test(sheet1_H16, "/Sheet1/", "H16", 0.0).
?test(sheet1_I16, "/Sheet1/", "I16", 4.0).
?test(sheet1_A17, "/Sheet1/", "A17", 56.2).
?test(sheet1_B17, "/Sheet1/", "B17", 88.2).
?test(sheet1_C17, "/Sheet1/", "C17", 23.9).
?test(sheet1_D17, "/Sheet1/", "D17", -9.0).
?test(sheet1_E17, "/Sheet1/", "E17", 4.4).
?test(sheet1_F17, "/Sheet1/", "F17", 0.71645).
?test(sheet1_G17, "/Sheet1/", "G17", '#DIV/0!').
?test(sheet1_H17, "/Sheet1/", "H17", 56.2).
?test(sheet1_I17, "/Sheet1/", "I17", 5.0).
?test(sheet1_A18, "/Sheet1/", "A18", "text").
?test(sheet1_A20, "/Sheet1/", "A20", "Function").
?test(sheet1_B20, "/Sheet1/", "B20", "1st test").
?test(sheet1_C20, "/Sheet1/", "C20", "Correct").
?test(sheet1_D20, "/Sheet1/", "D20", "2nd test").
?test(sheet1_E20, "/Sheet1/", "E20", "Correct").
?test(sheet1_F20, "/Sheet1/", "F20", "3rd test").
?test(sheet1_G20, "/Sheet1/", "G20", "Correct").
?test(sheet1_H20, "/Sheet1/", "H20", "Status").
?test(sheet1_I20, "/Sheet1/", "I20", "Status message").
?test(sheet1_A21, "/Sheet1/", "A21", "AVEDEV").
?test(sheet1_B21, "/Sheet1/", "B21", 12.625).
?test(sheet1_C21, "/Sheet1/", "C21", 12.625).
?test(sheet1_D21, "/Sheet1/", "D21", 2.75).
?test(sheet1_E21, "/Sheet1/", "E21", 2.75).
?test(sheet1_F21, "/Sheet1/", "F21", 13.525).
?test(sheet1_G21, "/Sheet1/", "G21", 13.525).
?test(sheet1_H21, "/Sheet1/", "H21", 1.0).
?test(sheet1_I21, "/Sheet1/", "I21", "Ok.").
?test(sheet1_A22, "/Sheet1/", "A22", "AVERAGE").
?test(sheet1_B22, "/Sheet1/", "B22", 32.325).
?test(sheet1_C22, "/Sheet1/", "C22", 32.325).
?test(sheet1_D22, "/Sheet1/", "D22", -4.25).
?test(sheet1_E22, "/Sheet1/", "E22", -4.25).
?test(sheet1_F22, "/Sheet1/", "F22", 15.075).
?test(sheet1_G22, "/Sheet1/", "G22", 15.075).
?test(sheet1_H22, "/Sheet1/", "H22", 1.0).
?test(sheet1_I22, "/Sheet1/", "I22", "Ok.").
?test(sheet1_A23, "/Sheet1/", "A23", "AVERAGEA").
?test(sheet1_B23, "/Sheet1/", "B23", 25.86).
?test(sheet1_C23, "/Sheet1/", "C23", 25.86).
?test(sheet1_D23, "/Sheet1/", "D23", 66.225).
?test(sheet1_E23, "/Sheet1/", "E23", 66.225).
?test(sheet1_F23, "/Sheet1/", "F23", 28.5).
?test(sheet1_G23, "/Sheet1/", "G23", 28.5).
?test(sheet1_H23, "/Sheet1/", "H23", 1.0).
?test(sheet1_I23, "/Sheet1/", "I23", "Ok.").
?test(sheet1_J23, "/Sheet1/", "J23", 1.0).
?test(sheet1_K23, "/Sheet1/", "K23", 55.0).
?test(sheet1_L23, "/Sheet1/", "L23", 70.0).
?test(sheet1_A24, "/Sheet1/", "A24", "BETADIST").
?test(sheet1_B24, "/Sheet1/", "B24", 0.0731980800029772).
?test(sheet1_C24, "/Sheet1/", "C24", 0.0731980800029772).
?test(sheet1_D24, "/Sheet1/", "D24", 0.793835627027863).
?test(sheet1_E24, "/Sheet1/", "E24", 0.793835627027863).
?test(sheet1_F24, "/Sheet1/", "F24", 0.197214615861317).
?test(sheet1_G24, "/Sheet1/", "G24", 0.197214615861317).
?test(sheet1_H24, "/Sheet1/", "H24", 1.0).
?test(sheet1_I24, "/Sheet1/", "I24", "Ok.").
?test(sheet1_J24, "/Sheet1/", "J24", 1.0).
?test(sheet1_K24, "/Sheet1/", "K24", 77.0).
?test(sheet1_L24, "/Sheet1/", "L24", 79.0).
?test(sheet1_A25, "/Sheet1/", "A25", "BETAINV").
?test(sheet1_B25, "/Sheet1/", "B25", 0.607096672058105).
?test(sheet1_C25, "/Sheet1/", "C25", 0.607096672058105).
?test(sheet1_D25, "/Sheet1/", "D25", 0.939406156539917).
?test(sheet1_E25, "/Sheet1/", "E25", 0.939406156539917).
?test(sheet1_F25, "/Sheet1/", "F25", 0.991891264915466).
?test(sheet1_G25, "/Sheet1/", "G25", 0.991891264915466).
?test(sheet1_H25, "/Sheet1/", "H25", 1.0).
?test(sheet1_I25, "/Sheet1/", "I25", "Ok.").
?test(sheet1_J25, "/Sheet1/", "J25", 3.0).
?test(sheet1_K25, "/Sheet1/", "K25", 86.0).
?test(sheet1_L25, "/Sheet1/", "L25", 89.0).
?test(sheet1_A26, "/Sheet1/", "A26", "BINOMDIST").
?test(sheet1_B26, "/Sheet1/", "B26", 0.2048).
?test(sheet1_C26, "/Sheet1/", "C26", 0.2048).
?test(sheet1_D26, "/Sheet1/", "D26", 0.26272).
?test(sheet1_E26, "/Sheet1/", "E26", 0.26272).
?test(sheet1_F26, "/Sheet1/", "F26", 0.04).
?test(sheet1_G26, "/Sheet1/", "G26", 0.04).
?test(sheet1_H26, "/Sheet1/", "H26", 1.0).
?test(sheet1_I26, "/Sheet1/", "I26", "Ok.").
?test(sheet1_J26, "/Sheet1/", "J26", 3.0).
?test(sheet1_K26, "/Sheet1/", "K26", 85.0).
?test(sheet1_A27, "/Sheet1/", "A27", "CHIDIST").
?test(sheet1_B27, "/Sheet1/", "B27", 0.0706512130633769).
?test(sheet1_C27, "/Sheet1/", "C27", 0.0706512130633769).
?test(sheet1_D27, "/Sheet1/", "D27", 0.62340404135856).
?test(sheet1_E27, "/Sheet1/", "E27", 0.62340404135856).
?test(sheet1_F27, "/Sheet1/", "F27", 0.274878481559641).
?test(sheet1_G27, "/Sheet1/", "G27", 0.274878481559641).
?test(sheet1_H27, "/Sheet1/", "H27", 1.0).
?test(sheet1_I27, "/Sheet1/", "I27", "Ok.").
?test(sheet1_K27, "/Sheet1/", "K27", 89.0).
?test(sheet1_A28, "/Sheet1/", "A28", "CHIINV").
?test(sheet1_B28, "/Sheet1/", "B28", 1.56429300989689).
?test(sheet1_C28, "/Sheet1/", "C28", 1.56429437527805).
?test(sheet1_D28, "/Sheet1/", "D28", 21.6145605351543).
?test(sheet1_E28, "/Sheet1/", "E28", 21.6145624030162).
?test(sheet1_F28, "/Sheet1/", "F28", 69.8626454760192).
?test(sheet1_G28, "/Sheet1/", "G28", 69.8626587610037).
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_K28, "/Sheet1/", "K28", 94.0).
?test(sheet1_A29, "/Sheet1/", "A29", "CHITEST").
?test(sheet1_B29, "/Sheet1/", "B29", 0.000798272283588582).
?test(sheet1_C29, "/Sheet1/", "C29", 0.000798272283588582).
?test(sheet1_D29, "/Sheet1/", "D29", 0.708046772250793).
?test(sheet1_E29, "/Sheet1/", "E29", 0.708046772250793).
?test(sheet1_F29, "/Sheet1/", "F29", 0.0161433249858667).
?test(sheet1_G29, "/Sheet1/", "G29", 0.0161433249858667).
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_K29, "/Sheet1/", "K29", 97.0).
?test(sheet1_A30, "/Sheet1/", "A30", "CONFIDENCE").
?test(sheet1_B30, "/Sheet1/", "B30", 0.341185936186322).
?test(sheet1_C30, "/Sheet1/", "C30", 0.341185430974773).
?test(sheet1_D30, "/Sheet1/", "D30", 0.250590264135424).
?test(sheet1_E30, "/Sheet1/", "E30", 0.25059001226833).
?test(sheet1_F30, "/Sheet1/", "F30", 0.516280465228656).
?test(sheet1_G30, "/Sheet1/", "G30", 0.516279946317592).
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_K30, "/Sheet1/", "K30", 98.0).
?test(sheet1_A31, "/Sheet1/", "A31", "CORREL").
?test(sheet1_B31, "/Sheet1/", "B31", 0.935229477683998).
?test(sheet1_C31, "/Sheet1/", "C31", 0.935229477683998).
?test(sheet1_D31, "/Sheet1/", "D31", -0.61035938815341).
?test(sheet1_E31, "/Sheet1/", "E31", -0.61035938815341).
?test(sheet1_F31, "/Sheet1/", "F31", 0.425870490617365).
?test(sheet1_G31, "/Sheet1/", "G31", 0.425870490617365).
?test(sheet1_H31, "/Sheet1/", "H31", 1.0).
?test(sheet1_I31, "/Sheet1/", "I31", "Ok.").
?test(sheet1_A32, "/Sheet1/", "A32", "COUNT").
?test(sheet1_B32, "/Sheet1/", "B32", 4.0).
?test(sheet1_C32, "/Sheet1/", "C32", 4.0).
?test(sheet1_D32, "/Sheet1/", "D32", 2.0).
?test(sheet1_E32, "/Sheet1/", "E32", 2.0).
?test(sheet1_F32, "/Sheet1/", "F32", 4.0).
?test(sheet1_G32, "/Sheet1/", "G32", 4.0).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "COUNTA").
?test(sheet1_B33, "/Sheet1/", "B33", 4.0).
?test(sheet1_C33, "/Sheet1/", "C33", 4.0).
?test(sheet1_D33, "/Sheet1/", "D33", 4.0).
?test(sheet1_E33, "/Sheet1/", "E33", 4.0).
?test(sheet1_F33, "/Sheet1/", "F33", 4.0).
?test(sheet1_G33, "/Sheet1/", "G33", 4.0).
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "COVAR").
?test(sheet1_B34, "/Sheet1/", "B34", 244.719375).
?test(sheet1_C34, "/Sheet1/", "C34", 244.719375).
?test(sheet1_D34, "/Sheet1/", "D34", -238.545).
?test(sheet1_E34, "/Sheet1/", "E34", -238.545).
?test(sheet1_F34, "/Sheet1/", "F34", -35.81875).
?test(sheet1_G34, "/Sheet1/", "G34", -35.81875).
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "CRITBINOM").
?test(sheet1_B35, "/Sheet1/", "B35", 6.0).
?test(sheet1_C35, "/Sheet1/", "C35", 6.0).
?test(sheet1_D35, "/Sheet1/", "D35", 5.0).
?test(sheet1_E35, "/Sheet1/", "E35", 5.0).
?test(sheet1_F35, "/Sheet1/", "F35", 1.0).
?test(sheet1_G35, "/Sheet1/", "G35", 1.0).
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_A36, "/Sheet1/", "A36", "DEVSQ").
?test(sheet1_B36, "/Sheet1/", "B36", 897.1675).
?test(sheet1_C36, "/Sheet1/", "C36", 897.1675).
?test(sheet1_D36, "/Sheet1/", "D36", 1221.0875).
?test(sheet1_E36, "/Sheet1/", "E36", 1221.0875).
?test(sheet1_F36, "/Sheet1/", "F36", 38.75).
?test(sheet1_G36, "/Sheet1/", "G36", 38.75).
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_A37, "/Sheet1/", "A37", "EXPONDIST").
?test(sheet1_B37, "/Sheet1/", "B37", 0.00134185051161005).
?test(sheet1_C37, "/Sheet1/", "C37", 0.00134185051161005).
?test(sheet1_D37, "/Sheet1/", "D37", 0.864664716763387).
?test(sheet1_E37, "/Sheet1/", "E37", 0.864664716763387).
?test(sheet1_F37, "/Sheet1/", "F37", 0.632120558828558).
?test(sheet1_G37, "/Sheet1/", "G37", 0.632120558828558).
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_A38, "/Sheet1/", "A38", "FDIST").
?test(sheet1_B38, "/Sheet1/", "B38", 0.232511319136467).
?test(sheet1_C38, "/Sheet1/", "C38", 0.232511319136467).
?test(sheet1_D38, "/Sheet1/", "D38", 0.179200000013568).
?test(sheet1_E38, "/Sheet1/", "E38", 0.179200000013568).
?test(sheet1_F38, "/Sheet1/", "F38", 0.319176083204613).
?test(sheet1_G38, "/Sheet1/", "G38", 0.319176083204613).
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_A39, "/Sheet1/", "A39", "FINV").
?test(sheet1_B39, "/Sheet1/", "B39", 2.47213595501605).
?test(sheet1_C39, "/Sheet1/", "C39", 2.47213449711126).
?test(sheet1_D39, "/Sheet1/", "D39", 0.402801416280597).
?test(sheet1_E39, "/Sheet1/", "E39", 0.402801347831883).
?test(sheet1_F39, "/Sheet1/", "F39", 1.87016969439757).
?test(sheet1_G39, "/Sheet1/", "G39", 1.87016979680266).
?test(sheet1_H39, "/Sheet1/", "H39", 1.0).
?test(sheet1_I39, "/Sheet1/", "I39", "Ok.").
?test(sheet1_A40, "/Sheet1/", "A40", "FISHER").
?test(sheet1_B40, "/Sheet1/", "B40", 0.345074338781844).
?test(sheet1_C40, "/Sheet1/", "C40", 0.345074338781844).
?test(sheet1_D40, "/Sheet1/", "D40", 0.459896681212679).
?test(sheet1_E40, "/Sheet1/", "E40", 0.459896681212679).
?test(sheet1_F40, "/Sheet1/", "F40", 0.21317134656486).
?test(sheet1_G40, "/Sheet1/", "G40", 0.21317134656486).
?test(sheet1_H40, "/Sheet1/", "H40", 1.0).
?test(sheet1_I40, "/Sheet1/", "I40", "Ok.").
?test(sheet1_A41, "/Sheet1/", "A41", "FISHERINV").
?test(sheet1_B41, "/Sheet1/", "B41", 0.964027580075817).
?test(sheet1_C41, "/Sheet1/", "C41", 0.964027580075817).
?test(sheet1_D41, "/Sheet1/", "D41", 0.935409070603099).
?test(sheet1_E41, "/Sheet1/", "E41", 0.935409070603099).
?test(sheet1_F41, "/Sheet1/", "F41", 0.230767512782364).
?test(sheet1_G41, "/Sheet1/", "G41", 0.230767512782364).
?test(sheet1_H41, "/Sheet1/", "H41", 1.0).
?test(sheet1_I41, "/Sheet1/", "I41", "Ok.").
?test(sheet1_A42, "/Sheet1/", "A42", "FORECAST").
?test(sheet1_B42, "/Sheet1/", "B42", -11.1441473276895).
?test(sheet1_C42, "/Sheet1/", "C42", -11.1441473276894).
?test(sheet1_D42, "/Sheet1/", "D42", 78.3820419298105).
?test(sheet1_E42, "/Sheet1/", "E42", 78.3820419298105).
?test(sheet1_F42, "/Sheet1/", "F42", 17.6199126628827).
?test(sheet1_G42, "/Sheet1/", "G42", 17.6199126628827).
?test(sheet1_H42, "/Sheet1/", "H42", 1.0).
?test(sheet1_I42, "/Sheet1/", "I42", "Ok.").
?test(sheet1_A43, "/Sheet1/", "A43", "FREQUENCY").
?test(sheet1_B43, "/Sheet1/", "B43", true).
?test(sheet1_C43, "/Sheet1/", "C43", true).
?test(sheet1_D43, "/Sheet1/", "D43", 3.0).
?test(sheet1_E43, "/Sheet1/", "E43", 3.0).
?test(sheet1_F43, "/Sheet1/", "F43", 3.0).
?test(sheet1_G43, "/Sheet1/", "G43", 3.0).
?test(sheet1_H43, "/Sheet1/", "H43", 1.0).
?test(sheet1_I43, "/Sheet1/", "I43", "Ok.").
?test(sheet1_A44, "/Sheet1/", "A44", "FTEST").
?test(sheet1_B44, "/Sheet1/", "B44", 0.806059847971795).
?test(sheet1_C44, "/Sheet1/", "C44", 0.806059847971795).
?test(sheet1_D44, "/Sheet1/", "D44", 0.694718513168786).
?test(sheet1_E44, "/Sheet1/", "E44", 0.694718513168786).
?test(sheet1_F44, "/Sheet1/", "F44", 0.00883670381469979).
?test(sheet1_G44, "/Sheet1/", "G44", 0.00883670381469979).
?test(sheet1_H44, "/Sheet1/", "H44", 1.0).
?test(sheet1_I44, "/Sheet1/", "I44", "Ok.").
?test(sheet1_A45, "/Sheet1/", "A45", "GAMMADIST").
?test(sheet1_B45, "/Sheet1/", "B45", 0.0796145900705827).
?test(sheet1_C45, "/Sheet1/", "C45", 0.0796145900705827).
?test(sheet1_D45, "/Sheet1/", "D45", 0.0446249191350084).
?test(sheet1_E45, "/Sheet1/", "E45", 0.0446249191350084).
?test(sheet1_F45, "/Sheet1/", "F45", 0.329679953473777).
?test(sheet1_G45, "/Sheet1/", "G45", 0.329679953473777).
?test(sheet1_H45, "/Sheet1/", "H45", 1.0).
?test(sheet1_I45, "/Sheet1/", "I45", "Ok.").
?test(sheet1_A46, "/Sheet1/", "A46", "GAMMAINV").
?test(sheet1_B46, "/Sheet1/", "B46", 4.82909398084613).
?test(sheet1_C46, "/Sheet1/", "C46", 4.82909399579512).
?test(sheet1_D46, "/Sheet1/", "D46", 8.89624072801235).
?test(sheet1_E46, "/Sheet1/", "E46", 8.89624061528593).
?test(sheet1_F46, "/Sheet1/", "F46", 1.46967597010066).
?test(sheet1_G46, "/Sheet1/", "G46", 1.46967522596242).
?test(sheet1_H46, "/Sheet1/", "H46", 1.0).
?test(sheet1_I46, "/Sheet1/", "I46", "Ok.").
?test(sheet1_J46, "/Sheet1/", "J46", 38.5244112479739).
?test(sheet1_L46, "/Sheet1/", "L46", 7.54506298064603).
?test(sheet1_N46, "/Sheet1/", "N46", 1.76194202553705).
?test(sheet1_A47, "/Sheet1/", "A47", "GAMMALN").
?test(sheet1_B47, "/Sheet1/", "B47", 48.471181351627).
?test(sheet1_C47, "/Sheet1/", "C47", 48.471181351627).
?test(sheet1_D47, "/Sheet1/", "D47", 15.1044125728633).
?test(sheet1_E47, "/Sheet1/", "E47", 15.1044125728633).
?test(sheet1_F47, "/Sheet1/", "F47", 304.686856765559).
?test(sheet1_G47, "/Sheet1/", "G47", 304.686856765559).
?test(sheet1_H47, "/Sheet1/", "H47", 1.0).
?test(sheet1_I47, "/Sheet1/", "I47", "Ok.").
?test(sheet1_J47, "/Sheet1/", "J47", 22.2986724713983).
?test(sheet1_L47, "/Sheet1/", "L47", 7.69588550459077).
?test(sheet1_N47, "/Sheet1/", "N47", 1.83275357168507).
?test(sheet1_A48, "/Sheet1/", "A48", "GEOMEAN").
?test(sheet1_B48, "/Sheet1/", "B48", 29.2189379961055).
?test(sheet1_C48, "/Sheet1/", "C48", 29.2189379961055).
?test(sheet1_D48, "/Sheet1/", "D48", 63.8405821195347).
?test(sheet1_E48, "/Sheet1/", "E48", 63.8405821195347).
?test(sheet1_F48, "/Sheet1/", "F48", 21.7584821075804).
?test(sheet1_G48, "/Sheet1/", "G48", 21.7584821075804).
?test(sheet1_H48, "/Sheet1/", "H48", 1.0).
?test(sheet1_I48, "/Sheet1/", "I48", "Ok.").
?test(sheet1_J48, "/Sheet1/", "J48", 16.8602629092744).
?test(sheet1_L48, "/Sheet1/", "L48", 29.2732051533918).
?test(sheet1_N48, "/Sheet1/", "N48", 26.1935965207531).
?test(sheet1_A49, "/Sheet1/", "A49", "GROWTH").
?test(sheet1_B49, "/Sheet1/", "B49", 128.007716542169).
?test(sheet1_C49, "/Sheet1/", "C49", 128.007716542169).
?test(sheet1_D49, "/Sheet1/", "D49", 54.7682327121101).
?test(sheet1_E49, "/Sheet1/", "E49", 54.7682327121102).
?test(sheet1_F49, "/Sheet1/", "F49", 33.0334418978221).
?test(sheet1_G49, "/Sheet1/", "G49", 33.0334418978223).
?test(sheet1_H49, "/Sheet1/", "H49", 1.0).
?test(sheet1_I49, "/Sheet1/", "I49", "Ok.").
?test(sheet1_J49, "/Sheet1/", "J49", 50.3243699135227).
?test(sheet1_L49, "/Sheet1/", "L49", 10.2540790734815).
?test(sheet1_N49, "/Sheet1/", "N49", 3.24514977984692).
?test(sheet1_A50, "/Sheet1/", "A50", "HARMEAN").
?test(sheet1_B50, "/Sheet1/", "B50", 26.6942980226078).
?test(sheet1_C50, "/Sheet1/", "C50", 26.6942980226078).
?test(sheet1_D50, "/Sheet1/", "D50", 61.4739418097052).
?test(sheet1_E50, "/Sheet1/", "E50", 61.4739418097052).
?test(sheet1_F50, "/Sheet1/", "F50", 17.7634647523052).
?test(sheet1_G50, "/Sheet1/", "G50", 17.7634647523052).
?test(sheet1_H50, "/Sheet1/", "H50", 1.0).
?test(sheet1_I50, "/Sheet1/", "I50", "Ok.").
?test(sheet1_A51, "/Sheet1/", "A51", "HYPGEOMDIST").
?test(sheet1_B51, "/Sheet1/", "B51", 0.466666666666667).
?test(sheet1_C51, "/Sheet1/", "C51", 0.466666666666667).
?test(sheet1_D51, "/Sheet1/", "D51", 0.095959595959596).
?test(sheet1_E51, "/Sheet1/", "E51", 0.095959595959596).
?test(sheet1_F51, "/Sheet1/", "F51", 0.000398644250409643).
?test(sheet1_G51, "/Sheet1/", "G51", 0.000398644250409643).
?test(sheet1_H51, "/Sheet1/", "H51", 1.0).
?test(sheet1_I51, "/Sheet1/", "I51", "Ok.").
?test(sheet1_A52, "/Sheet1/", "A52", "INTERCEPT").
?test(sheet1_B52, "/Sheet1/", "B52", -20.7638756436373).
?test(sheet1_C52, "/Sheet1/", "C52", -20.7638756436373).
?test(sheet1_D52, "/Sheet1/", "D52", 79.8122821568471).
?test(sheet1_E52, "/Sheet1/", "E52", 79.8122821568471).
?test(sheet1_F52, "/Sheet1/", "F52", 41.5077419354839).
?test(sheet1_G52, "/Sheet1/", "G52", 41.5077419354839).
?test(sheet1_H52, "/Sheet1/", "H52", 1.0).
?test(sheet1_I52, "/Sheet1/", "I52", "Ok.").
?test(sheet1_A53, "/Sheet1/", "A53", "KURT").
?test(sheet1_B53, "/Sheet1/", "B53", 0.735770169517057).
?test(sheet1_C53, "/Sheet1/", "C53", 0.735770169517057).
?test(sheet1_D53, "/Sheet1/", "D53", -3.29874094799308).
?test(sheet1_E53, "/Sheet1/", "E53", -3.29874094799308).
?test(sheet1_F53, "/Sheet1/", "F53", 2.93453009300567).
?test(sheet1_G53, "/Sheet1/", "G53", 2.93453009300567).
?test(sheet1_H53, "/Sheet1/", "H53", 1.0).
?test(sheet1_I53, "/Sheet1/", "I53", "Ok.").
?test(sheet1_A54, "/Sheet1/", "A54", "LARGE").
?test(sheet1_B54, "/Sheet1/", "B54", 33.7).
?test(sheet1_C54, "/Sheet1/", "C54", 33.7).
?test(sheet1_D54, "/Sheet1/", "D54", 44.0).
?test(sheet1_E54, "/Sheet1/", "E54", 44.0).
?test(sheet1_F54, "/Sheet1/", "F54", 23.9).
?test(sheet1_G54, "/Sheet1/", "G54", 23.9).
?test(sheet1_H54, "/Sheet1/", "H54", 1.0).
?test(sheet1_I54, "/Sheet1/", "I54", "Ok.").
?test(sheet1_A55, "/Sheet1/", "A55", "LINEST").
?test(sheet1_B55, "/Sheet1/", "B55", -19.9622316173083).
?test(sheet1_C55, "/Sheet1/", "C55", -19.9622316173082).
?test(sheet1_D55, "/Sheet1/", "D55", 916.4461894099).
?test(sheet1_E55, "/Sheet1/", "E55", 916.4461894099).
?test(sheet1_F55, "/Sheet1/", "F55", 0.0).
?test(sheet1_G55, "/Sheet1/", "G55", 0.0).
?test(sheet1_H55, "/Sheet1/", "H55", 1.0).
?test(sheet1_I55, "/Sheet1/", "I55", "Ok.").
?test(sheet1_J55, "/Sheet1/", "J55", 0.80164402632899).
?test(sheet1_K55, "/Sheet1/", "K55", -20.7638756436373).
?test(sheet1_A56, "/Sheet1/", "A56", "LOGEST").
?test(sheet1_B56, "/Sheet1/", "B56", 6.70179257788771).
?test(sheet1_C56, "/Sheet1/", "C56", 6.70179257788775).
?test(sheet1_D56, "/Sheet1/", "D56", 53.5366321082835).
?test(sheet1_E56, "/Sheet1/", "E56", 53.5366321082835).
?test(sheet1_F56, "/Sheet1/", "F56", 2.05048674088668).
?test(sheet1_G56, "/Sheet1/", "G56", 2.05048674088668).
?test(sheet1_H56, "/Sheet1/", "H56", 1.0).
?test(sheet1_I56, "/Sheet1/", "I56", "Ok.").
?test(sheet1_J56, "/Sheet1/", "J56", 1.02504908124737).
?test(sheet1_K56, "/Sheet1/", "K56", 5.67674349664034).
?test(sheet1_M56, "/Sheet1/", "M56", 1.05048674088668).
?test(sheet1_N56, "/Sheet1/", "N56", 1.0).
?test(sheet1_A57, "/Sheet1/", "A57", "LOGINV").
?test(sheet1_B57, "/Sheet1/", "B57", 7.38905609893065).
?test(sheet1_C57, "/Sheet1/", "C57", 7.38905609893065).
?test(sheet1_D57, "/Sheet1/", "D57", 96.8711380483649).
?test(sheet1_E57, "/Sheet1/", "E57", 96.870872360914).
?test(sheet1_F57, "/Sheet1/", "F57", 0.704706704912013).
?test(sheet1_G57, "/Sheet1/", "G57", 0.704705602979732).
?test(sheet1_H57, "/Sheet1/", "H57", 1.0).
?test(sheet1_I57, "/Sheet1/", "I57", "Ok.").
?test(sheet1_A58, "/Sheet1/", "A58", "LOGNORMDIST").
?test(sheet1_B58, "/Sheet1/", "B58", 0.519662338497517).
?test(sheet1_C58, "/Sheet1/", "C58", 0.519662405713752).
?test(sheet1_D58, "/Sheet1/", "D58", 0.550709973373502).
?test(sheet1_E58, "/Sheet1/", "E58", 0.55071001170712).
?test(sheet1_F58, "/Sheet1/", "F58", 0.464498836040377).
?test(sheet1_G58, "/Sheet1/", "G58", 0.464498771942327).
?test(sheet1_H58, "/Sheet1/", "H58", 1.0).
?test(sheet1_I58, "/Sheet1/", "I58", "Ok.").
?test(sheet1_J58, "/Sheet1/", "J58", 1.02504908124737).
?test(sheet1_K58, "/Sheet1/", "K58", 5.67674349664034).
?test(sheet1_M58, "/Sheet1/", "M58", 0.80164402632899).
?test(sheet1_N58, "/Sheet1/", "N58", -20.7638756436373).
?test(sheet1_A59, "/Sheet1/", "A59", "MAX").
?test(sheet1_B59, "/Sheet1/", "B59", 56.2).
?test(sheet1_C59, "/Sheet1/", "C59", 56.2).
?test(sheet1_D59, "/Sheet1/", "D59", -1.0).
?test(sheet1_E59, "/Sheet1/", "E59", -1.0).
?test(sheet1_F59, "/Sheet1/", "F59", 33.9).
?test(sheet1_G59, "/Sheet1/", "G59", 33.9).
?test(sheet1_H59, "/Sheet1/", "H59", 1.0).
?test(sheet1_I59, "/Sheet1/", "I59", "Ok.").
?test(sheet1_J59, "/Sheet1/", "J59", 0.00378604268351555).
?test(sheet1_K59, "/Sheet1/", "K59", 0.25930999921225).
?test(sheet1_M59, "/Sheet1/", "M59", 0.21458695185756).
?test(sheet1_N59, "/Sheet1/", "N59", 14.6972834087211).
?test(sheet1_A60, "/Sheet1/", "A60", "MAXA").
?test(sheet1_B60, "/Sheet1/", "B60", 56.2).
?test(sheet1_C60, "/Sheet1/", "C60", 56.2).
?test(sheet1_D60, "/Sheet1/", "D60", 88.2).
?test(sheet1_E60, "/Sheet1/", "E60", 88.2).
?test(sheet1_F60, "/Sheet1/", "F60", 66.3).
?test(sheet1_G60, "/Sheet1/", "G60", 66.3).
?test(sheet1_H60, "/Sheet1/", "H60", 1.0).
?test(sheet1_I60, "/Sheet1/", "I60", "Ok.").
?test(sheet1_J60, "/Sheet1/", "J60", 0.955259032584279).
?test(sheet1_K60, "/Sheet1/", "K60", 0.132299711943532).
?test(sheet1_M60, "/Sheet1/", "M60", 0.874654175929083).
?test(sheet1_N60, "/Sheet1/", "N60", 7.49853984510133).
?test(sheet1_A61, "/Sheet1/", "A61", "MEDIAN").
?test(sheet1_B61, "/Sheet1/", "B61", 27.6).
?test(sheet1_C61, "/Sheet1/", "C61", 27.6).
?test(sheet1_D61, "/Sheet1/", "D61", 66.35).
?test(sheet1_E61, "/Sheet1/", "E61", 66.35).
?test(sheet1_F61, "/Sheet1/", "F61", 18.1).
?test(sheet1_G61, "/Sheet1/", "G61", 18.1).
?test(sheet1_H61, "/Sheet1/", "H61", 1.0).
?test(sheet1_I61, "/Sheet1/", "I61", "Ok.").
?test(sheet1_J61, "/Sheet1/", "J61", 42.7017602774779).
?test(sheet1_K61, "/Sheet1/", "K61", 2.0).
?test(sheet1_M61, "/Sheet1/", "M61", 13.9558566455988).
?test(sheet1_N61, "/Sheet1/", "N61", 2.0).
?test(sheet1_A62, "/Sheet1/", "A62", "MIN").
?test(sheet1_B62, "/Sheet1/", "B62", 17.9).
?test(sheet1_C62, "/Sheet1/", "C62", 17.9).
?test(sheet1_D62, "/Sheet1/", "D62", -9.0).
?test(sheet1_E62, "/Sheet1/", "E62", -9.0).
?test(sheet1_F62, "/Sheet1/", "F62", -1.3).
?test(sheet1_G62, "/Sheet1/", "G62", -1.3).
?test(sheet1_H62, "/Sheet1/", "H62", 1.0).
?test(sheet1_I62, "/Sheet1/", "I62", "Ok.").
?test(sheet1_J62, "/Sheet1/", "J62", 0.747418038933591).
?test(sheet1_K62, "/Sheet1/", "K62", 0.035006427560683).
?test(sheet1_M62, "/Sheet1/", "M62", 784.711300382856).
?test(sheet1_N62, "/Sheet1/", "N62", 112.456199617145).
?test(sheet1_A63, "/Sheet1/", "A63", "MINA").
?test(sheet1_B63, "/Sheet1/", "B63", 0.0).
?test(sheet1_C63, "/Sheet1/", "C63", 0.0).
?test(sheet1_D63, "/Sheet1/", "D63", 44.0).
?test(sheet1_E63, "/Sheet1/", "E63", 44.0).
?test(sheet1_F63, "/Sheet1/", "F63", 11.5).
?test(sheet1_G63, "/Sheet1/", "G63", 11.5).
?test(sheet1_H63, "/Sheet1/", "H63", 1.0).
?test(sheet1_I63, "/Sheet1/", "I63", "Ok.").
?test(sheet1_A64, "/Sheet1/", "A64", "MODE").
?test(sheet1_B64, "/Sheet1/", "B64", 2.0).
?test(sheet1_C64, "/Sheet1/", "C64", 2.0).
?test(sheet1_D64, "/Sheet1/", "D64", 2.0).
?test(sheet1_E64, "/Sheet1/", "E64", 2.0).
?test(sheet1_F64, "/Sheet1/", "F64", 2.0).
?test(sheet1_G64, "/Sheet1/", "G64", 2.0).
?test(sheet1_H64, "/Sheet1/", "H64", 1.0).
?test(sheet1_I64, "/Sheet1/", "I64", "Ok.").
?test(sheet1_A65, "/Sheet1/", "A65", "NEGBINOMDIST").
?test(sheet1_B65, "/Sheet1/", "B65", 0.15287262890625).
?test(sheet1_C65, "/Sheet1/", "C65", 0.15287262890625).
?test(sheet1_D65, "/Sheet1/", "D65", 0.01171875).
?test(sheet1_E65, "/Sheet1/", "E65", 0.01171875).
?test(sheet1_F65, "/Sheet1/", "F65", 0.000670425887915264).
?test(sheet1_G65, "/Sheet1/", "G65", 0.000670425887915264).
?test(sheet1_H65, "/Sheet1/", "H65", 1.0).
?test(sheet1_I65, "/Sheet1/", "I65", "Ok.").
?test(sheet1_A66, "/Sheet1/", "A66", "NORMDIST").
?test(sheet1_B66, "/Sheet1/", "B66", 0.17603266338215).
?test(sheet1_C66, "/Sheet1/", "C66", 0.17603266338215).
?test(sheet1_D66, "/Sheet1/", "D66", 0.691462461274013).
?test(sheet1_E66, "/Sheet1/", "E66", 0.691462467364291).
?test(sheet1_F66, "/Sheet1/", "F66", 0.422327973135432).
?test(sheet1_G66, "/Sheet1/", "G66", 0.422327973135432).
?test(sheet1_H66, "/Sheet1/", "H66", 1.0).
?test(sheet1_I66, "/Sheet1/", "I66", "Ok.").
?test(sheet1_A67, "/Sheet1/", "A67", "NORMINV").
?test(sheet1_B67, "/Sheet1/", "B67", 4.11890768852026).
?test(sheet1_C67, "/Sheet1/", "C67", 4.11890665013925).
?test(sheet1_D67, "/Sheet1/", "D67", 2.52230630162957).
?test(sheet1_E67, "/Sheet1/", "E67", 2.52230757824145).
?test(sheet1_F67, "/Sheet1/", "F67", 1.5066942062716).
?test(sheet1_G67, "/Sheet1/", "G67", 1.50669314077822).
?test(sheet1_H67, "/Sheet1/", "H67", 1.0).
?test(sheet1_I67, "/Sheet1/", "I67", "Ok.").
?test(sheet1_A68, "/Sheet1/", "A68", "NORMSDIST").
?test(sheet1_B68, "/Sheet1/", "B68", 0.977249868051821).
?test(sheet1_C68, "/Sheet1/", "C68", 0.977249937963813).
?test(sheet1_D68, "/Sheet1/", "D68", 0.884930329778292).
?test(sheet1_E68, "/Sheet1/", "E68", 0.884930268282292).
?test(sheet1_F68, "/Sheet1/", "F68", 0.590954115142006).
?test(sheet1_G68, "/Sheet1/", "G68", 0.590954073198176).
?test(sheet1_H68, "/Sheet1/", "H68", 1.0).
?test(sheet1_I68, "/Sheet1/", "I68", "Ok.").
?test(sheet1_A69, "/Sheet1/", "A69", "NORMSINV").
?test(sheet1_B69, "/Sheet1/", "B69", -0.841621233572914).
?test(sheet1_C69, "/Sheet1/", "C69", -0.841621385916369).
?test(sheet1_D69, "/Sheet1/", "D69", -0.412463129441405).
?test(sheet1_E69, "/Sheet1/", "E69", -0.412462668464286).
?test(sheet1_F69, "/Sheet1/", "F69", 0.772193214188685).
?test(sheet1_G69, "/Sheet1/", "G69", 0.772192834119778).
?test(sheet1_H69, "/Sheet1/", "H69", 1.0).
?test(sheet1_I69, "/Sheet1/", "I69", "Ok.").
?test(sheet1_A70, "/Sheet1/", "A70", "PEARSON").
?test(sheet1_B70, "/Sheet1/", "B70", 0.935229477683998).
?test(sheet1_C70, "/Sheet1/", "C70", 0.935229477683997).
?test(sheet1_D70, "/Sheet1/", "D70", -0.61035938815341).
?test(sheet1_E70, "/Sheet1/", "E70", -0.61035938815341).
?test(sheet1_F70, "/Sheet1/", "F70", 0.425870490617365).
?test(sheet1_G70, "/Sheet1/", "G70", 0.425870490617365).
?test(sheet1_H70, "/Sheet1/", "H70", 1.0).
?test(sheet1_I70, "/Sheet1/", "I70", "Ok.").
?test(sheet1_A71, "/Sheet1/", "A71", "PERCENTILE").
?test(sheet1_B71, "/Sheet1/", "B71", 17.9).
?test(sheet1_C71, "/Sheet1/", "C71", 17.9).
?test(sheet1_D71, "/Sheet1/", "D71", 66.3).
?test(sheet1_E71, "/Sheet1/", "E71", 66.3).
?test(sheet1_F71, "/Sheet1/", "F71", 88.2).
?test(sheet1_G71, "/Sheet1/", "G71", 88.2).
?test(sheet1_H71, "/Sheet1/", "H71", 1.0).
?test(sheet1_I71, "/Sheet1/", "I71", "Ok.").
?test(sheet1_A72, "/Sheet1/", "A72", "PERCENTRANK").
?test(sheet1_B72, "/Sheet1/", "B72", 0.333).
?test(sheet1_C72, "/Sheet1/", "C72", 0.333).
?test(sheet1_D72, "/Sheet1/", "D72", 0.597701).
?test(sheet1_E72, "/Sheet1/", "E72", 0.597701).
?test(sheet1_F72, "/Sheet1/", "F72", 0.63).
?test(sheet1_G72, "/Sheet1/", "G72", 0.63).
?test(sheet1_H72, "/Sheet1/", "H72", 1.0).
?test(sheet1_I72, "/Sheet1/", "I72", "Ok.").
?test(sheet1_A73, "/Sheet1/", "A73", "PERMUT").
?test(sheet1_B73, "/Sheet1/", "B73", 210.0).
?test(sheet1_C73, "/Sheet1/", "C73", 210.0).
?test(sheet1_D73, "/Sheet1/", "D73", 95040.0).
?test(sheet1_E73, "/Sheet1/", "E73", 95040.0).
?test(sheet1_F73, "/Sheet1/", "F73", 72.0).
?test(sheet1_G73, "/Sheet1/", "G73", 72.0).
?test(sheet1_H73, "/Sheet1/", "H73", 1.0).
?test(sheet1_I73, "/Sheet1/", "I73", "Ok.").
?test(sheet1_A74, "/Sheet1/", "A74", "POISSON").
?test(sheet1_B74, "/Sheet1/", "B74", 0.0892350783599904).
?test(sheet1_C74, "/Sheet1/", "C74", 0.0892350783599889).
?test(sheet1_D74, "/Sheet1/", "D74", 0.15120388277665).
?test(sheet1_E74, "/Sheet1/", "E74", 0.151203882776648).
?test(sheet1_F74, "/Sheet1/", "F74", 0.06196880441666).
?test(sheet1_G74, "/Sheet1/", "G74", 0.061968804416659).
?test(sheet1_H74, "/Sheet1/", "H74", 1.0).
?test(sheet1_I74, "/Sheet1/", "I74", "Ok.").
?test(sheet1_A75, "/Sheet1/", "A75", "PROB").
?test(sheet1_B75, "/Sheet1/", "B75", 0.2).
?test(sheet1_C75, "/Sheet1/", "C75", 0.2).
?test(sheet1_D75, "/Sheet1/", "D75", 0.3).
?test(sheet1_E75, "/Sheet1/", "E75", 0.3).
?test(sheet1_F75, "/Sheet1/", "F75", 0.1).
?test(sheet1_G75, "/Sheet1/", "G75", 0.1).
?test(sheet1_H75, "/Sheet1/", "H75", 1.0).
?test(sheet1_I75, "/Sheet1/", "I75", "Ok.").
?test(sheet1_A76, "/Sheet1/", "A76", "QUARTILE").
?test(sheet1_B76, "/Sheet1/", "B76", 20.6).
?test(sheet1_C76, "/Sheet1/", "C76", 20.6).
?test(sheet1_D76, "/Sheet1/", "D76", 27.6).
?test(sheet1_E76, "/Sheet1/", "E76", 27.6).
?test(sheet1_F76, "/Sheet1/", "F76", 39.325).
?test(sheet1_G76, "/Sheet1/", "G76", 39.325).
?test(sheet1_H76, "/Sheet1/", "H76", 1.0).
?test(sheet1_I76, "/Sheet1/", "I76", "Ok.").
?test(sheet1_A77, "/Sheet1/", "A77", "RANK").
?test(sheet1_B77, "/Sheet1/", "B77", 3.0).
?test(sheet1_C77, "/Sheet1/", "C77", 3.0).
?test(sheet1_D77, "/Sheet1/", "D77", 2.0).
?test(sheet1_E77, "/Sheet1/", "E77", 2.0).
?test(sheet1_F77, "/Sheet1/", "F77", 3.0).
?test(sheet1_G77, "/Sheet1/", "G77", 3.0).
?test(sheet1_H77, "/Sheet1/", "H77", 1.0).
?test(sheet1_I77, "/Sheet1/", "I77", "Ok.").
?test(sheet1_A78, "/Sheet1/", "A78", "RSQ").
?test(sheet1_B78, "/Sheet1/", "B78", 0.874654175929083).
?test(sheet1_C78, "/Sheet1/", "C78", 0.874654175929082).
?test(sheet1_D78, "/Sheet1/", "D78", 0.372538582707005).
?test(sheet1_E78, "/Sheet1/", "E78", 0.372538582707005).
?test(sheet1_F78, "/Sheet1/", "F78", 0.181365674778676).
?test(sheet1_G78, "/Sheet1/", "G78", 0.181365674778676).
?test(sheet1_H78, "/Sheet1/", "H78", 1.0).
?test(sheet1_I78, "/Sheet1/", "I78", "Ok.").
?test(sheet1_A79, "/Sheet1/", "A79", "SKEW").
?test(sheet1_B79, "/Sheet1/", "B79", 1.20421846201566).
?test(sheet1_C79, "/Sheet1/", "C79", 1.20421846201566).
?test(sheet1_D79, "/Sheet1/", "D79", -0.0223037310213973).
?test(sheet1_E79, "/Sheet1/", "E79", -0.0223037310213973).
?test(sheet1_F79, "/Sheet1/", "F79", 1.73122368386485).
?test(sheet1_G79, "/Sheet1/", "G79", 1.73122368386485).
?test(sheet1_H79, "/Sheet1/", "H79", 1.0).
?test(sheet1_I79, "/Sheet1/", "I79", "Ok.").
?test(sheet1_A80, "/Sheet1/", "A80", "SLOPE").
?test(sheet1_B80, "/Sheet1/", "B80", 0.80164402632899).
?test(sheet1_C80, "/Sheet1/", "C80", 0.801644026328989).
?test(sheet1_D80, "/Sheet1/", "D80", -0.476746742345511).
?test(sheet1_E80, "/Sheet1/", "E80", -0.476746742345511).
?test(sheet1_F80, "/Sheet1/", "F80", 3.06064516129032).
?test(sheet1_G80, "/Sheet1/", "G80", 3.06064516129032).
?test(sheet1_H80, "/Sheet1/", "H80", 1.0).
?test(sheet1_I80, "/Sheet1/", "I80", "Ok.").
?test(sheet1_A81, "/Sheet1/", "A81", "SMALL").
?test(sheet1_B81, "/Sheet1/", "B81", 21.5).
?test(sheet1_C81, "/Sheet1/", "C81", 21.5).
?test(sheet1_D81, "/Sheet1/", "D81", 77.4).
?test(sheet1_E81, "/Sheet1/", "E81", 77.4).
?test(sheet1_F81, "/Sheet1/", "F81", 11.5).
?test(sheet1_G81, "/Sheet1/", "G81", 11.5).
?test(sheet1_H81, "/Sheet1/", "H81", 1.0).
?test(sheet1_I81, "/Sheet1/", "I81", "Ok.").
?test(sheet1_A82, "/Sheet1/", "A82", "STANDARDIZE").
?test(sheet1_B82, "/Sheet1/", "B82", 0.25).
?test(sheet1_C82, "/Sheet1/", "C82", 0.25).
?test(sheet1_D82, "/Sheet1/", "D82", 0.5).
?test(sheet1_E82, "/Sheet1/", "E82", 0.5).
?test(sheet1_F82, "/Sheet1/", "F82", -0.928571428571429).
?test(sheet1_G82, "/Sheet1/", "G82", -0.928571428571429).
?test(sheet1_H82, "/Sheet1/", "H82", 1.0).
?test(sheet1_I82, "/Sheet1/", "I82", "Ok.").
?test(sheet1_A83, "/Sheet1/", "A83", "STDEV").
?test(sheet1_B83, "/Sheet1/", "B83", 17.2932308529474).
?test(sheet1_C83, "/Sheet1/", "C83", 17.2932308529474).
?test(sheet1_D83, "/Sheet1/", "D83", 20.1749638578776).
?test(sheet1_E83, "/Sheet1/", "E83", 20.1749638578776).
?test(sheet1_F83, "/Sheet1/", "F83", 25.8291824622203).
?test(sheet1_G83, "/Sheet1/", "G83", 25.8291824622203).
?test(sheet1_H83, "/Sheet1/", "H83", 1.0).
?test(sheet1_I83, "/Sheet1/", "I83", "Ok.").
?test(sheet1_A84, "/Sheet1/", "A84", "STDEVA").
?test(sheet1_B84, "/Sheet1/", "B84", 20.815210784424).
?test(sheet1_C84, "/Sheet1/", "C84", 20.815210784424).
?test(sheet1_D84, "/Sheet1/", "D84", 20.1749638578776).
?test(sheet1_E84, "/Sheet1/", "E84", 20.1749638578776).
?test(sheet1_F84, "/Sheet1/", "F84", 25.8291824622203).
?test(sheet1_G84, "/Sheet1/", "G84", 25.8291824622203).
?test(sheet1_H84, "/Sheet1/", "H84", 1.0).
?test(sheet1_I84, "/Sheet1/", "I84", "Ok.").
?test(sheet1_A85, "/Sheet1/", "A85", "STDEVP").
?test(sheet1_B85, "/Sheet1/", "B85", 14.9763772321613).
?test(sheet1_C85, "/Sheet1/", "C85", 14.9763772321613).
?test(sheet1_D85, "/Sheet1/", "D85", 17.4720312213549).
?test(sheet1_E85, "/Sheet1/", "E85", 17.4720312213549).
?test(sheet1_F85, "/Sheet1/", "F85", 22.3687281712662).
?test(sheet1_G85, "/Sheet1/", "G85", 22.3687281712662).
?test(sheet1_H85, "/Sheet1/", "H85", 1.0).
?test(sheet1_I85, "/Sheet1/", "I85", "Ok.").
?test(sheet1_A86, "/Sheet1/", "A86", "STDEVPA").
?test(sheet1_B86, "/Sheet1/", "B86", 18.6176905119835).
?test(sheet1_C86, "/Sheet1/", "C86", 18.6176905119835).
?test(sheet1_D86, "/Sheet1/", "D86", 17.4720312213549).
?test(sheet1_E86, "/Sheet1/", "E86", 17.4720312213549).
?test(sheet1_F86, "/Sheet1/", "F86", 22.3687281712662).
?test(sheet1_G86, "/Sheet1/", "G86", 22.3687281712662).
?test(sheet1_H86, "/Sheet1/", "H86", 1.0).
?test(sheet1_I86, "/Sheet1/", "I86", "Ok.").
?test(sheet1_A87, "/Sheet1/", "A87", "STEYX").
?test(sheet1_B87, "/Sheet1/", "B87", 7.49853984510133).
?test(sheet1_C87, "/Sheet1/", "C87", 7.49853984510135).
?test(sheet1_D87, "/Sheet1/", "D87", 19.5727526601237).
?test(sheet1_E87, "/Sheet1/", "E87", 19.5727526601237).
?test(sheet1_F87, "/Sheet1/", "F87", 28.6220848635365).
?test(sheet1_G87, "/Sheet1/", "G87", 28.6220848635365).
?test(sheet1_H87, "/Sheet1/", "H87", 1.0).
?test(sheet1_I87, "/Sheet1/", "I87", "Ok.").
?test(sheet1_A88, "/Sheet1/", "A88", "TDIST").
?test(sheet1_B88, "/Sheet1/", "B88", 0.0509697393237112).
?test(sheet1_C88, "/Sheet1/", "C88", 0.0509697393237112).
?test(sheet1_D88, "/Sheet1/", "D88", 0.101939478647422).
?test(sheet1_E88, "/Sheet1/", "E88", 0.101939478647422).
?test(sheet1_F88, "/Sheet1/", "F88", 0.00603817952612437).
?test(sheet1_G88, "/Sheet1/", "G88", 0.00603817952612437).
?test(sheet1_H88, "/Sheet1/", "H88", 1.0).
?test(sheet1_I88, "/Sheet1/", "I88", "Ok.").
?test(sheet1_A89, "/Sheet1/", "A89", "TINV").
?test(sheet1_B89, "/Sheet1/", "B89", 0.85299845376438).
?test(sheet1_C89, "/Sheet1/", "C89", 0.852998027767171).
?test(sheet1_D89, "/Sheet1/", "D89", 1.13294871549948).
?test(sheet1_E89, "/Sheet1/", "E89", 1.13294845505152).
?test(sheet1_F89, "/Sheet1/", "F89", 0.334354486109928).
?test(sheet1_G89, "/Sheet1/", "G89", 0.33435412660765).
?test(sheet1_H89, "/Sheet1/", "H89", 1.0).
?test(sheet1_I89, "/Sheet1/", "I89", "Ok.").
?test(sheet1_A90, "/Sheet1/", "A90", "TREND").
?test(sheet1_B90, "/Sheet1/", "B90", 129.3).
?test(sheet1_C90, "/Sheet1/", "C90", 129.3).
?test(sheet1_D90, "/Sheet1/", "D90", 8.3319164269555).
?test(sheet1_E90, "/Sheet1/", "E90", 8.33191642695589).
?test(sheet1_F90, "/Sheet1/", "F90", 57.9703987776652).
?test(sheet1_G90, "/Sheet1/", "G90", 57.9703987776652).
?test(sheet1_H90, "/Sheet1/", "H90", 1.0).
?test(sheet1_I90, "/Sheet1/", "I90", "Ok.").
?test(sheet1_J90, "/Sheet1/", "J90", 41.2833719942265).
?test(sheet1_L90, "/Sheet1/", "L90", -11.5449693408539).
?test(sheet1_A91, "/Sheet1/", "A91", "TRIMMEAN").
?test(sheet1_B91, "/Sheet1/", "B91", 32.325).
?test(sheet1_C91, "/Sheet1/", "C91", 32.325).
?test(sheet1_D91, "/Sheet1/", "D91", 27.6).
?test(sheet1_E91, "/Sheet1/", "E91", 27.6).
?test(sheet1_F91, "/Sheet1/", "F91", 66.35).
?test(sheet1_G91, "/Sheet1/", "G91", 66.35).
?test(sheet1_H91, "/Sheet1/", "H91", 1.0).
?test(sheet1_I91, "/Sheet1/", "I91", "Ok.").
?test(sheet1_J91, "/Sheet1/", "J91", 23.5670390123558).
?test(sheet1_L91, "/Sheet1/", "L91", -10.9036541197908).
?test(sheet1_A92, "/Sheet1/", "A92", "TTEST").
?test(sheet1_B92, "/Sheet1/", "B92", 0.00132855343538823).
?test(sheet1_C92, "/Sheet1/", "C92", 0.00132855343538823).
?test(sheet1_D92, "/Sheet1/", "D92", 0.0609260013873942).
?test(sheet1_E92, "/Sheet1/", "E92", 0.0609260013873942).
?test(sheet1_F92, "/Sheet1/", "F92", 0.0442962528979641).
?test(sheet1_G92, "/Sheet1/", "G92", 0.0442962528979641).
?test(sheet1_H92, "/Sheet1/", "H92", 1.0).
?test(sheet1_I92, "/Sheet1/", "I92", "Ok.").
?test(sheet1_J92, "/Sheet1/", "J92", 14.5084615148382).
?test(sheet1_L92, "/Sheet1/", "L92", 32.3851233019747).
?test(sheet1_A93, "/Sheet1/", "A93", "VAR").
?test(sheet1_B93, "/Sheet1/", "B93", 299.055833333334).
?test(sheet1_C93, "/Sheet1/", "C93", 299.055833333334).
?test(sheet1_D93, "/Sheet1/", "D93", 407.029166666667).
?test(sheet1_E93, "/Sheet1/", "E93", 407.029166666667).
?test(sheet1_F93, "/Sheet1/", "F93", 667.146666666667).
?test(sheet1_G93, "/Sheet1/", "G93", 667.146666666667).
?test(sheet1_H93, "/Sheet1/", "H93", 1.0).
?test(sheet1_I93, "/Sheet1/", "I93", "Ok.").
?test(sheet1_J93, "/Sheet1/", "J93", 49.9411274785796).
?test(sheet1_L93, "/Sheet1/", "L93", -1.60458341437448).
?test(sheet1_A94, "/Sheet1/", "A94", "VARA").
?test(sheet1_B94, "/Sheet1/", "B94", 433.273).
?test(sheet1_C94, "/Sheet1/", "C94", 433.273).
?test(sheet1_D94, "/Sheet1/", "D94", 407.029166666667).
?test(sheet1_E94, "/Sheet1/", "E94", 407.029166666667).
?test(sheet1_F94, "/Sheet1/", "F94", 667.146666666667).
?test(sheet1_G94, "/Sheet1/", "G94", 667.146666666667).
?test(sheet1_H94, "/Sheet1/", "H94", 1.0).
?test(sheet1_I94, "/Sheet1/", "I94", "Ok.").
?test(sheet1_A95, "/Sheet1/", "A95", "VARP").
?test(sheet1_B95, "/Sheet1/", "B95", 224.291875).
?test(sheet1_C95, "/Sheet1/", "C95", 224.291875).
?test(sheet1_D95, "/Sheet1/", "D95", 305.271875).
?test(sheet1_E95, "/Sheet1/", "E95", 305.271875).
?test(sheet1_F95, "/Sheet1/", "F95", 500.36).
?test(sheet1_G95, "/Sheet1/", "G95", 500.36).
?test(sheet1_H95, "/Sheet1/", "H95", 1.0).
?test(sheet1_I95, "/Sheet1/", "I95", "Ok.").
?test(sheet1_K95, "/Sheet1/", "K95", 5.84789110476447).
?test(sheet1_A96, "/Sheet1/", "A96", "VARPA").
?test(sheet1_B96, "/Sheet1/", "B96", 346.6184).
?test(sheet1_C96, "/Sheet1/", "C96", 346.6184).
?test(sheet1_D96, "/Sheet1/", "D96", 305.271875).
?test(sheet1_E96, "/Sheet1/", "E96", 305.271875).
?test(sheet1_F96, "/Sheet1/", "F96", 538.702222222222).
?test(sheet1_G96, "/Sheet1/", "G96", 538.702222222222).
?test(sheet1_H96, "/Sheet1/", "H96", 1.0).
?test(sheet1_I96, "/Sheet1/", "I96", "Ok.").
?test(sheet1_K96, "/Sheet1/", "K96", 6.25470092074809).
?test(sheet1_A97, "/Sheet1/", "A97", "WEIBULL").
?test(sheet1_B97, "/Sheet1/", "B97", 0.213668559274096).
?test(sheet1_C97, "/Sheet1/", "C97", 0.213668559274096).
?test(sheet1_D97, "/Sheet1/", "D97", 0.430217175269077).
?test(sheet1_E97, "/Sheet1/", "E97", 0.430217175269077).
?test(sheet1_F97, "/Sheet1/", "F97", 0.117503097415405).
?test(sheet1_G97, "/Sheet1/", "G97", 0.117503097415405).
?test(sheet1_H97, "/Sheet1/", "H97", 1.0).
?test(sheet1_I97, "/Sheet1/", "I97", "Ok.").
?test(sheet1_K97, "/Sheet1/", "K97", 33.7143634996421).
?test(sheet1_A98, "/Sheet1/", "A98", "ZTEST").
?test(sheet1_B98, "/Sheet1/", "B98", 0.000347555260994281).
?test(sheet1_C98, "/Sheet1/", "C98", 0.000347607424298069).
?test(sheet1_D98, "/Sheet1/", "D98", 0.0166116482224574).
?test(sheet1_E98, "/Sheet1/", "E98", 0.0166115886532073).
?test(sheet1_F98, "/Sheet1/", "F98", 0.16195860992195).
?test(sheet1_G98, "/Sheet1/", "G98", 0.161958610416791).
?test(sheet1_H98, "/Sheet1/", "H98", 1.0).
?test(sheet1_I98, "/Sheet1/", "I98", "Ok.").
?test(sheet1_K98, "/Sheet1/", "K98", 12.1534432525105).
?test(sheet1_A99, "/Sheet1/", "A99", "Total").
?test(sheet1_H99, "/Sheet1/", "H99", true).
?test(sheet1_I99, "/Sheet1/", "I99", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_stats.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_stats" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A11,
        sheet1_A13,
        sheet1_B13,
        sheet1_C13,
        sheet1_D13,
        sheet1_E13,
        sheet1_F13,
        sheet1_G13,
        sheet1_H13,
        sheet1_I13,
        sheet1_A14,
        sheet1_B14,
        sheet1_C14,
        sheet1_D14,
        sheet1_E14,
        sheet1_F14,
        sheet1_G14,
        sheet1_H14,
        sheet1_I14,
        sheet1_A15,
        sheet1_B15,
        sheet1_C15,
        sheet1_D15,
        sheet1_E15,
        sheet1_F15,
        sheet1_G15,
        sheet1_H15,
        sheet1_I15,
        sheet1_A16,
        sheet1_B16,
        sheet1_C16,
        sheet1_D16,
        sheet1_E16,
        sheet1_F16,
        sheet1_G16,
        sheet1_H16,
        sheet1_I16,
        sheet1_A17,
        sheet1_B17,
        sheet1_C17,
        sheet1_D17,
        sheet1_E17,
        sheet1_F17,
        sheet1_G17,
        sheet1_H17,
        sheet1_I17,
        sheet1_A18,
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
        sheet1_J23,
        sheet1_K23,
        sheet1_L23,
        sheet1_A24,
        sheet1_B24,
        sheet1_C24,
        sheet1_D24,
        sheet1_E24,
        sheet1_F24,
        sheet1_G24,
        sheet1_H24,
        sheet1_I24,
        sheet1_J24,
        sheet1_K24,
        sheet1_L24,
        sheet1_A25,
        sheet1_B25,
        sheet1_C25,
        sheet1_D25,
        sheet1_E25,
        sheet1_F25,
        sheet1_G25,
        sheet1_H25,
        sheet1_I25,
        sheet1_J25,
        sheet1_K25,
        sheet1_L25,
        sheet1_A26,
        sheet1_B26,
        sheet1_C26,
        sheet1_D26,
        sheet1_E26,
        sheet1_F26,
        sheet1_G26,
        sheet1_H26,
        sheet1_I26,
        sheet1_J26,
        sheet1_K26,
        sheet1_A27,
        sheet1_B27,
        sheet1_C27,
        sheet1_D27,
        sheet1_E27,
        sheet1_F27,
        sheet1_G27,
        sheet1_H27,
        sheet1_I27,
        sheet1_K27,
        sheet1_A28,
        sheet1_B28,
        sheet1_C28,
        sheet1_D28,
        sheet1_E28,
        sheet1_F28,
        sheet1_G28,
        sheet1_H28,
        sheet1_I28,
        sheet1_K28,
        sheet1_A29,
        sheet1_B29,
        sheet1_C29,
        sheet1_D29,
        sheet1_E29,
        sheet1_F29,
        sheet1_G29,
        sheet1_H29,
        sheet1_I29,
        sheet1_K29,
        sheet1_A30,
        sheet1_B30,
        sheet1_C30,
        sheet1_D30,
        sheet1_E30,
        sheet1_F30,
        sheet1_G30,
        sheet1_H30,
        sheet1_I30,
        sheet1_K30,
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
        sheet1_J46,
        sheet1_L46,
        sheet1_N46,
        sheet1_A47,
        sheet1_B47,
        sheet1_C47,
        sheet1_D47,
        sheet1_E47,
        sheet1_F47,
        sheet1_G47,
        sheet1_H47,
        sheet1_I47,
        sheet1_J47,
        sheet1_L47,
        sheet1_N47,
        sheet1_A48,
        sheet1_B48,
        sheet1_C48,
        sheet1_D48,
        sheet1_E48,
        sheet1_F48,
        sheet1_G48,
        sheet1_H48,
        sheet1_I48,
        sheet1_J48,
        sheet1_L48,
        sheet1_N48,
        sheet1_A49,
        sheet1_B49,
        sheet1_C49,
        sheet1_D49,
        sheet1_E49,
        sheet1_F49,
        sheet1_G49,
        sheet1_H49,
        sheet1_I49,
        sheet1_J49,
        sheet1_L49,
        sheet1_N49,
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
        sheet1_J55,
        sheet1_K55,
        sheet1_A56,
        sheet1_B56,
        sheet1_C56,
        sheet1_D56,
        sheet1_E56,
        sheet1_F56,
        sheet1_G56,
        sheet1_H56,
        sheet1_I56,
        sheet1_J56,
        sheet1_K56,
        sheet1_M56,
        sheet1_N56,
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
        sheet1_J58,
        sheet1_K58,
        sheet1_M58,
        sheet1_N58,
        sheet1_A59,
        sheet1_B59,
        sheet1_C59,
        sheet1_D59,
        sheet1_E59,
        sheet1_F59,
        sheet1_G59,
        sheet1_H59,
        sheet1_I59,
        sheet1_J59,
        sheet1_K59,
        sheet1_M59,
        sheet1_N59,
        sheet1_A60,
        sheet1_B60,
        sheet1_C60,
        sheet1_D60,
        sheet1_E60,
        sheet1_F60,
        sheet1_G60,
        sheet1_H60,
        sheet1_I60,
        sheet1_J60,
        sheet1_K60,
        sheet1_M60,
        sheet1_N60,
        sheet1_A61,
        sheet1_B61,
        sheet1_C61,
        sheet1_D61,
        sheet1_E61,
        sheet1_F61,
        sheet1_G61,
        sheet1_H61,
        sheet1_I61,
        sheet1_J61,
        sheet1_K61,
        sheet1_M61,
        sheet1_N61,
        sheet1_A62,
        sheet1_B62,
        sheet1_C62,
        sheet1_D62,
        sheet1_E62,
        sheet1_F62,
        sheet1_G62,
        sheet1_H62,
        sheet1_I62,
        sheet1_J62,
        sheet1_K62,
        sheet1_M62,
        sheet1_N62,
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
        sheet1_B74,
        sheet1_C74,
        sheet1_D74,
        sheet1_E74,
        sheet1_F74,
        sheet1_G74,
        sheet1_H74,
        sheet1_I74,
        sheet1_A75,
        sheet1_B75,
        sheet1_C75,
        sheet1_D75,
        sheet1_E75,
        sheet1_F75,
        sheet1_G75,
        sheet1_H75,
        sheet1_I75,
        sheet1_A76,
        sheet1_B76,
        sheet1_C76,
        sheet1_D76,
        sheet1_E76,
        sheet1_F76,
        sheet1_G76,
        sheet1_H76,
        sheet1_I76,
        sheet1_A77,
        sheet1_B77,
        sheet1_C77,
        sheet1_D77,
        sheet1_E77,
        sheet1_F77,
        sheet1_G77,
        sheet1_H77,
        sheet1_I77,
        sheet1_A78,
        sheet1_B78,
        sheet1_C78,
        sheet1_D78,
        sheet1_E78,
        sheet1_F78,
        sheet1_G78,
        sheet1_H78,
        sheet1_I78,
        sheet1_A79,
        sheet1_B79,
        sheet1_C79,
        sheet1_D79,
        sheet1_E79,
        sheet1_F79,
        sheet1_G79,
        sheet1_H79,
        sheet1_I79,
        sheet1_A80,
        sheet1_B80,
        sheet1_C80,
        sheet1_D80,
        sheet1_E80,
        sheet1_F80,
        sheet1_G80,
        sheet1_H80,
        sheet1_I80,
        sheet1_A81,
        sheet1_B81,
        sheet1_C81,
        sheet1_D81,
        sheet1_E81,
        sheet1_F81,
        sheet1_G81,
        sheet1_H81,
        sheet1_I81,
        sheet1_A82,
        sheet1_B82,
        sheet1_C82,
        sheet1_D82,
        sheet1_E82,
        sheet1_F82,
        sheet1_G82,
        sheet1_H82,
        sheet1_I82,
        sheet1_A83,
        sheet1_B83,
        sheet1_C83,
        sheet1_D83,
        sheet1_E83,
        sheet1_F83,
        sheet1_G83,
        sheet1_H83,
        sheet1_I83,
        sheet1_A84,
        sheet1_B84,
        sheet1_C84,
        sheet1_D84,
        sheet1_E84,
        sheet1_F84,
        sheet1_G84,
        sheet1_H84,
        sheet1_I84,
        sheet1_A85,
        sheet1_B85,
        sheet1_C85,
        sheet1_D85,
        sheet1_E85,
        sheet1_F85,
        sheet1_G85,
        sheet1_H85,
        sheet1_I85,
        sheet1_A86,
        sheet1_B86,
        sheet1_C86,
        sheet1_D86,
        sheet1_E86,
        sheet1_F86,
        sheet1_G86,
        sheet1_H86,
        sheet1_I86,
        sheet1_A87,
        sheet1_B87,
        sheet1_C87,
        sheet1_D87,
        sheet1_E87,
        sheet1_F87,
        sheet1_G87,
        sheet1_H87,
        sheet1_I87,
        sheet1_A88,
        sheet1_B88,
        sheet1_C88,
        sheet1_D88,
        sheet1_E88,
        sheet1_F88,
        sheet1_G88,
        sheet1_H88,
        sheet1_I88,
        sheet1_A89,
        sheet1_B89,
        sheet1_C89,
        sheet1_D89,
        sheet1_E89,
        sheet1_F89,
        sheet1_G89,
        sheet1_H89,
        sheet1_I89,
        sheet1_A90,
        sheet1_B90,
        sheet1_C90,
        sheet1_D90,
        sheet1_E90,
        sheet1_F90,
        sheet1_G90,
        sheet1_H90,
        sheet1_I90,
        sheet1_J90,
        sheet1_L90,
        sheet1_A91,
        sheet1_B91,
        sheet1_C91,
        sheet1_D91,
        sheet1_E91,
        sheet1_F91,
        sheet1_G91,
        sheet1_H91,
        sheet1_I91,
        sheet1_J91,
        sheet1_L91,
        sheet1_A92,
        sheet1_B92,
        sheet1_C92,
        sheet1_D92,
        sheet1_E92,
        sheet1_F92,
        sheet1_G92,
        sheet1_H92,
        sheet1_I92,
        sheet1_J92,
        sheet1_L92,
        sheet1_A93,
        sheet1_B93,
        sheet1_C93,
        sheet1_D93,
        sheet1_E93,
        sheet1_F93,
        sheet1_G93,
        sheet1_H93,
        sheet1_I93,
        sheet1_J93,
        sheet1_L93,
        sheet1_A94,
        sheet1_B94,
        sheet1_C94,
        sheet1_D94,
        sheet1_E94,
        sheet1_F94,
        sheet1_G94,
        sheet1_H94,
        sheet1_I94,
        sheet1_A95,
        sheet1_B95,
        sheet1_C95,
        sheet1_D95,
        sheet1_E95,
        sheet1_F95,
        sheet1_G95,
        sheet1_H95,
        sheet1_I95,
        sheet1_K95,
        sheet1_A96,
        sheet1_B96,
        sheet1_C96,
        sheet1_D96,
        sheet1_E96,
        sheet1_F96,
        sheet1_G96,
        sheet1_H96,
        sheet1_I96,
        sheet1_K96,
        sheet1_A97,
        sheet1_B97,
        sheet1_C97,
        sheet1_D97,
        sheet1_E97,
        sheet1_F97,
        sheet1_G97,
        sheet1_H97,
        sheet1_I97,
        sheet1_K97,
        sheet1_A98,
        sheet1_B98,
        sheet1_C98,
        sheet1_D98,
        sheet1_E98,
        sheet1_F98,
        sheet1_G98,
        sheet1_H98,
        sheet1_I98,
        sheet1_K98,
        sheet1_A99,
        sheet1_H99,
        sheet1_I99
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
