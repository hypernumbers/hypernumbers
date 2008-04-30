%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_maths_SUITE).
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
                     [Testcase, "d_gnumeric_maths_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_maths" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "MATH AND TRIGONOMETRY FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_C3, "/Sheet1/", "C3", "Accuracy Limit").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_C4, "/Sheet1/", "C4", 1.0e-05).
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 60.0).
?test(sheet1_B8, "/Sheet1/", "B8", 60.0).
?test(sheet1_A11, "/Sheet1/", "A11", "Test Data:").
?test(sheet1_A13, "/Sheet1/", "A13", "x").
?test(sheet1_B13, "/Sheet1/", "B13", "y").
?test(sheet1_C13, "/Sheet1/", "C13", "z").
?test(sheet1_D13, "/Sheet1/", "D13", "neg").
?test(sheet1_E13, "/Sheet1/", "E13", "pos&neg").
?test(sheet1_F13, "/Sheet1/", "F13", "[0..1]").
?test(sheet1_G13, "/Sheet1/", "G13", "manytypes").
?test(sheet1_H13, "/Sheet1/", "H13", "criteria").
?test(sheet1_I13, "/Sheet1/", "I13", "mode").
?test(sheet1_A14, "/Sheet1/", "A14", 33.7).
?test(sheet1_B14, "/Sheet1/", "B14", 77.4).
?test(sheet1_C14, "/Sheet1/", "C14", 11.5).
?test(sheet1_D14, "/Sheet1/", "D14", -2.0).
?test(sheet1_E14, "/Sheet1/", "E14", 33.9).
?test(sheet1_F14, "/Sheet1/", "F14", 0.123543).
?test(sheet1_G14, "/Sheet1/", "G14", -1.9).
?test(sheet1_H14, "/Sheet1/", "H14", ">22").
?test(sheet1_I14, "/Sheet1/", "I14", 2.0).
?test(sheet1_A15, "/Sheet1/", "A15", 21.5).
?test(sheet1_B15, "/Sheet1/", "B15", 55.3).
?test(sheet1_C15, "/Sheet1/", "C15", 12.3).
?test(sheet1_D15, "/Sheet1/", "D15", -5.0).
?test(sheet1_E15, "/Sheet1/", "E15", 23.3).
?test(sheet1_F15, "/Sheet1/", "F15", 0.44312).
?test(sheet1_G15, "/Sheet1/", "G15", 0.0).
?test(sheet1_H15, "/Sheet1/", "H15", ">35").
?test(sheet1_I15, "/Sheet1/", "I15", 2.0).
?test(sheet1_A16, "/Sheet1/", "A16", 17.9).
?test(sheet1_B16, "/Sheet1/", "B16", 44.0).
?test(sheet1_C16, "/Sheet1/", "C16", 66.3).
?test(sheet1_D16, "/Sheet1/", "D16", -1.0).
?test(sheet1_E16, "/Sheet1/", "E16", -1.3).
?test(sheet1_F16, "/Sheet1/", "F16", 0.32342).
?test(sheet1_G16, "/Sheet1/", "G16", "text").
?test(sheet1_H16, "/Sheet1/", "H16", "<60").
?test(sheet1_I16, "/Sheet1/", "I16", 4.0).
?test(sheet1_A17, "/Sheet1/", "A17", 56.2).
?test(sheet1_B17, "/Sheet1/", "B17", 88.2).
?test(sheet1_C17, "/Sheet1/", "C17", 23.9).
?test(sheet1_D17, "/Sheet1/", "D17", -9.0).
?test(sheet1_E17, "/Sheet1/", "E17", 4.4).
?test(sheet1_F17, "/Sheet1/", "F17", 0.71645).
?test(sheet1_G17, "/Sheet1/", "G17", '#DIV/0!').
?test(sheet1_I17, "/Sheet1/", "I17", 5.0).
?test(sheet1_A20, "/Sheet1/", "A20", "Function").
?test(sheet1_B20, "/Sheet1/", "B20", "1st test").
?test(sheet1_C20, "/Sheet1/", "C20", "Correct").
?test(sheet1_D20, "/Sheet1/", "D20", "2nd test").
?test(sheet1_E20, "/Sheet1/", "E20", "Correct").
?test(sheet1_F20, "/Sheet1/", "F20", "3rd test").
?test(sheet1_G20, "/Sheet1/", "G20", "Correct").
?test(sheet1_H20, "/Sheet1/", "H20", "Status").
?test(sheet1_I20, "/Sheet1/", "I20", "Status message").
?test(sheet1_A21, "/Sheet1/", "A21", "ABS").
?test(sheet1_B21, "/Sheet1/", "B21", 12.0).
?test(sheet1_C21, "/Sheet1/", "C21", 12.0).
?test(sheet1_D21, "/Sheet1/", "D21", 0.0).
?test(sheet1_E21, "/Sheet1/", "E21", 0.0).
?test(sheet1_F21, "/Sheet1/", "F21", 1.0).
?test(sheet1_G21, "/Sheet1/", "G21", 1.0).
?test(sheet1_H21, "/Sheet1/", "H21", 1.0).
?test(sheet1_I21, "/Sheet1/", "I21", "Ok.").
?test(sheet1_A22, "/Sheet1/", "A22", "ACOS").
?test(sheet1_B22, "/Sheet1/", "B22", 0.643501108793284).
?test(sheet1_C22, "/Sheet1/", "C22", 0.643501108793284).
?test(sheet1_D22, "/Sheet1/", "D22", 2.0943951023932).
?test(sheet1_E22, "/Sheet1/", "E22", 2.0943951023932).
?test(sheet1_F22, "/Sheet1/", "F22", 0.0).
?test(sheet1_G22, "/Sheet1/", "G22", 0.0).
?test(sheet1_H22, "/Sheet1/", "H22", 1.0).
?test(sheet1_I22, "/Sheet1/", "I22", "Ok.").
?test(sheet1_A23, "/Sheet1/", "A23", "ACOSH").
?test(sheet1_B23, "/Sheet1/", "B23", 0.0).
?test(sheet1_C23, "/Sheet1/", "C23", 0.0).
?test(sheet1_D23, "/Sheet1/", "D23", 2.29243166956118).
?test(sheet1_E23, "/Sheet1/", "E23", 2.29243166956118).
?test(sheet1_F23, "/Sheet1/", "F23", 4.43067504534955).
?test(sheet1_G23, "/Sheet1/", "G23", 4.43067504534955).
?test(sheet1_H23, "/Sheet1/", "H23", 1.0).
?test(sheet1_I23, "/Sheet1/", "I23", "Ok.").
?test(sheet1_A24, "/Sheet1/", "A24", "ASIN").
?test(sheet1_B24, "/Sheet1/", "B24", 0.927295218001612).
?test(sheet1_C24, "/Sheet1/", "C24", 0.927295218001612).
?test(sheet1_D24, "/Sheet1/", "D24", -0.523598775598299).
?test(sheet1_E24, "/Sheet1/", "E24", -0.523598775598299).
?test(sheet1_F24, "/Sheet1/", "F24", 1.5707963267949).
?test(sheet1_G24, "/Sheet1/", "G24", 1.5707963267949).
?test(sheet1_H24, "/Sheet1/", "H24", 1.0).
?test(sheet1_I24, "/Sheet1/", "I24", "Ok.").
?test(sheet1_A25, "/Sheet1/", "A25", "ASINH").
?test(sheet1_B25, "/Sheet1/", "B25", 0.881373587019543).
?test(sheet1_C25, "/Sheet1/", "C25", 0.881373587019543).
?test(sheet1_D25, "/Sheet1/", "D25", 2.31243834127275).
?test(sheet1_E25, "/Sheet1/", "E25", 2.31243834127275).
?test(sheet1_F25, "/Sheet1/", "F25", 4.43095849208054).
?test(sheet1_G25, "/Sheet1/", "G25", 4.43095849208054).
?test(sheet1_H25, "/Sheet1/", "H25", 1.0).
?test(sheet1_I25, "/Sheet1/", "I25", "Ok.").
?test(sheet1_A26, "/Sheet1/", "A26", "ATAN").
?test(sheet1_B26, "/Sheet1/", "B26", 0.674740942223553).
?test(sheet1_C26, "/Sheet1/", "C26", 0.674740942223553).
?test(sheet1_D26, "/Sheet1/", "D26", -0.463647609000806).
?test(sheet1_E26, "/Sheet1/", "E26", -0.463647609000806).
?test(sheet1_F26, "/Sheet1/", "F26", 0.785398163397448).
?test(sheet1_G26, "/Sheet1/", "G26", 0.785398163397448).
?test(sheet1_H26, "/Sheet1/", "H26", 1.0).
?test(sheet1_I26, "/Sheet1/", "I26", "Ok.").
?test(sheet1_A27, "/Sheet1/", "A27", "ATAN2").
?test(sheet1_B27, "/Sheet1/", "B27", 0.785398163397448).
?test(sheet1_C27, "/Sheet1/", "C27", 0.785398163397448).
?test(sheet1_D27, "/Sheet1/", "D27", -0.785398163397448).
?test(sheet1_E27, "/Sheet1/", "E27", -0.785398163397448).
?test(sheet1_F27, "/Sheet1/", "F27", -1.81577498992176).
?test(sheet1_G27, "/Sheet1/", "G27", -1.81577498992176).
?test(sheet1_H27, "/Sheet1/", "H27", 1.0).
?test(sheet1_I27, "/Sheet1/", "I27", "Ok.").
?test(sheet1_A28, "/Sheet1/", "A28", "ATANH").
?test(sheet1_B28, "/Sheet1/", "B28", 0.549306144334055).
?test(sheet1_C28, "/Sheet1/", "C28", 0.549306144334055).
?test(sheet1_D28, "/Sheet1/", "D28", 0.100335347731076).
?test(sheet1_E28, "/Sheet1/", "E28", 0.100335347731076).
?test(sheet1_F28, "/Sheet1/", "F28", -0.100335347731076).
?test(sheet1_G28, "/Sheet1/", "G28", -0.100335347731076).
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_A29, "/Sheet1/", "A29", "CEILING").
?test(sheet1_B29, "/Sheet1/", "B29", 214.0).
?test(sheet1_C29, "/Sheet1/", "C29", 214.0).
?test(sheet1_D29, "/Sheet1/", "D29", 12.0).
?test(sheet1_E29, "/Sheet1/", "E29", 12.0).
?test(sheet1_F29, "/Sheet1/", "F29", 10.35).
?test(sheet1_G29, "/Sheet1/", "G29", 10.35).
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_A30, "/Sheet1/", "A30", "COMBIN").
?test(sheet1_B30, "/Sheet1/", "B30", 21.0).
?test(sheet1_C30, "/Sheet1/", "C30", 21.0).
?test(sheet1_D30, "/Sheet1/", "D30", 6188.0).
?test(sheet1_E30, "/Sheet1/", "E30", 6188.0).
?test(sheet1_F30, "/Sheet1/", "F30", 45.0).
?test(sheet1_G30, "/Sheet1/", "G30", 45.0).
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_A31, "/Sheet1/", "A31", "COS").
?test(sheet1_B31, "/Sheet1/", "B31", 0.83422336050651).
?test(sheet1_C31, "/Sheet1/", "C31", 0.83422336050651).
?test(sheet1_D31, "/Sheet1/", "D31", 0.942754665528346).
?test(sheet1_E31, "/Sheet1/", "E31", 0.942754665528346).
?test(sheet1_F31, "/Sheet1/", "F31", 0.991168529845107).
?test(sheet1_G31, "/Sheet1/", "G31", 0.991168529845107).
?test(sheet1_H31, "/Sheet1/", "H31", 1.0).
?test(sheet1_I31, "/Sheet1/", "I31", "Ok.").
?test(sheet1_J31, "/Sheet1/", "J31", 2.2).
?test(sheet1_K31, "/Sheet1/", "K31", 4.2).
?test(sheet1_L31, "/Sheet1/", "L31", 6.3).
?test(sheet1_A32, "/Sheet1/", "A32", "COSH").
?test(sheet1_B32, "/Sheet1/", "B32", 81377.3957125741).
?test(sheet1_C32, "/Sheet1/", "C32", 81377.3957125741).
?test(sheet1_D32, "/Sheet1/", "D32", 1.54308063481524).
?test(sheet1_E32, "/Sheet1/", "E32", 1.54308063481524).
?test(sheet1_F32, "/Sheet1/", "F32", 10.0676619957778).
?test(sheet1_G32, "/Sheet1/", "G32", 10.0676619957778).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_J32, "/Sheet1/", "J32", 1.6).
?test(sheet1_K32, "/Sheet1/", "K32", 2.4).
?test(sheet1_L32, "/Sheet1/", "L32", 3.0).
?test(sheet1_A33, "/Sheet1/", "A33", "COUNTIF").
?test(sheet1_B33, "/Sheet1/", "B33", 2.0).
?test(sheet1_C33, "/Sheet1/", "C33", 2.0).
?test(sheet1_D33, "/Sheet1/", "D33", 4.0).
?test(sheet1_E33, "/Sheet1/", "E33", 4.0).
?test(sheet1_F33, "/Sheet1/", "F33", 3.0).
?test(sheet1_G33, "/Sheet1/", "G33", 3.0).
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_J33, "/Sheet1/", "J33", 3.1).
?test(sheet1_K33, "/Sheet1/", "K33", 2.1).
?test(sheet1_L33, "/Sheet1/", "L33", 0.2).
?test(sheet1_A34, "/Sheet1/", "A34", "DEGREES").
?test(sheet1_B34, "/Sheet1/", "B34", 131.780292880089).
?test(sheet1_C34, "/Sheet1/", "C34", 131.780292880089).
?test(sheet1_D34, "/Sheet1/", "D34", 18.9076072393172).
?test(sheet1_E34, "/Sheet1/", "E34", 18.9076072393172).
?test(sheet1_F34, "/Sheet1/", "F34", -183.346494441863).
?test(sheet1_G34, "/Sheet1/", "G34", -183.346494441863).
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "EVEN").
?test(sheet1_B35, "/Sheet1/", "B35", 4.0).
?test(sheet1_C35, "/Sheet1/", "C35", 4.0).
?test(sheet1_D35, "/Sheet1/", "D35", 4.0).
?test(sheet1_E35, "/Sheet1/", "E35", 4.0).
?test(sheet1_F35, "/Sheet1/", "F35", 0.0).
?test(sheet1_G35, "/Sheet1/", "G35", 0.0).
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_A36, "/Sheet1/", "A36", "EXP").
?test(sheet1_B36, "/Sheet1/", "B36", 20.0855369231877).
?test(sheet1_C36, "/Sheet1/", "C36", 20.0855369231877).
?test(sheet1_D36, "/Sheet1/", "D36", 1.22140275816017).
?test(sheet1_E36, "/Sheet1/", "E36", 1.22140275816017).
?test(sheet1_F36, "/Sheet1/", "F36", 4.6588861451034e-15).
?test(sheet1_G36, "/Sheet1/", "G36", 4.6588861451034e-15).
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_J36, "/Sheet1/", "J36", 1.3).
?test(sheet1_K36, "/Sheet1/", "K36", 2.4).
?test(sheet1_L36, "/Sheet1/", "L36", 5.2).
?test(sheet1_A37, "/Sheet1/", "A37", "FACT").
?test(sheet1_B37, "/Sheet1/", "B37", 5040.0).
?test(sheet1_C37, "/Sheet1/", "C37", 5040.0).
?test(sheet1_D37, "/Sheet1/", "D37", 87178291200.0).
?test(sheet1_E37, "/Sheet1/", "E37", 87178291200.0).
?test(sheet1_F37, "/Sheet1/", "F37", 1.0).
?test(sheet1_G37, "/Sheet1/", "G37", 1.0).
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_J37, "/Sheet1/", "J37", 2.5).
?test(sheet1_K37, "/Sheet1/", "K37", 3.5).
?test(sheet1_L37, "/Sheet1/", "L37", 2.5).
?test(sheet1_A38, "/Sheet1/", "A38", "FACTDOUBLE").
?test(sheet1_B38, "/Sheet1/", "B38", 8.0).
?test(sheet1_C38, "/Sheet1/", "C38", 8.0).
?test(sheet1_D38, "/Sheet1/", "D38", 645120.0).
?test(sheet1_E38, "/Sheet1/", "E38", 645120.0).
?test(sheet1_F38, "/Sheet1/", "F38", 4.71440074852053e+27).
?test(sheet1_G38, "/Sheet1/", "G38", 4.71440074852053e+27).
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_J38, "/Sheet1/", "J38", 3.8).
?test(sheet1_K38, "/Sheet1/", "K38", 1.9).
?test(sheet1_L38, "/Sheet1/", "L38", 4.2).
?test(sheet1_A39, "/Sheet1/", "A39", "FLOOR").
?test(sheet1_B39, "/Sheet1/", "B39", 233.92).
?test(sheet1_C39, "/Sheet1/", "C39", 233.92).
?test(sheet1_D39, "/Sheet1/", "D39", 234.0).
?test(sheet1_E39, "/Sheet1/", "E39", 234.0).
?test(sheet1_F39, "/Sheet1/", "F39", 233.8).
?test(sheet1_G39, "/Sheet1/", "G39", 233.8).
?test(sheet1_H39, "/Sheet1/", "H39", 1.0).
?test(sheet1_I39, "/Sheet1/", "I39", "Ok.").
?test(sheet1_A40, "/Sheet1/", "A40", "GCD").
?test(sheet1_B40, "/Sheet1/", "B40", 2310.0).
?test(sheet1_C40, "/Sheet1/", "C40", 2310.0).
?test(sheet1_D40, "/Sheet1/", "D40", 252.0).
?test(sheet1_E40, "/Sheet1/", "E40", 252.0).
?test(sheet1_F40, "/Sheet1/", "F40", 231.0).
?test(sheet1_G40, "/Sheet1/", "G40", 231.0).
?test(sheet1_H40, "/Sheet1/", "H40", 1.0).
?test(sheet1_I40, "/Sheet1/", "I40", "Ok.").
?test(sheet1_J40, "/Sheet1/", "J40", -0.293294030950626).
?test(sheet1_K40, "/Sheet1/", "K40", 0.00589535740604272).
?test(sheet1_L40, "/Sheet1/", "L40", 0.359616801768607).
?test(sheet1_A41, "/Sheet1/", "A41", "INT").
?test(sheet1_B41, "/Sheet1/", "B41", 7.0).
?test(sheet1_C41, "/Sheet1/", "C41", 7.0).
?test(sheet1_D41, "/Sheet1/", "D41", -8.0).
?test(sheet1_E41, "/Sheet1/", "E41", -8.0).
?test(sheet1_F41, "/Sheet1/", "F41", 0.0).
?test(sheet1_G41, "/Sheet1/", "G41", 0.0).
?test(sheet1_H41, "/Sheet1/", "H41", 1.0).
?test(sheet1_I41, "/Sheet1/", "I41", "Ok.").
?test(sheet1_J41, "/Sheet1/", "J41", 0.0294767870302137).
?test(sheet1_K41, "/Sheet1/", "K41", 0.421518054532056).
?test(sheet1_L41, "/Sheet1/", "L41", -0.287398673544584).
?test(sheet1_A42, "/Sheet1/", "A42", "LCM").
?test(sheet1_B42, "/Sheet1/", "B42", 1563379.0).
?test(sheet1_C42, "/Sheet1/", "C42", 1563379.0).
?test(sheet1_D42, "/Sheet1/", "D42", 23776.0).
?test(sheet1_E42, "/Sheet1/", "E42", 23776.0).
?test(sheet1_F42, "/Sheet1/", "F42", 5830.0).
?test(sheet1_G42, "/Sheet1/", "G42", 5830.0).
?test(sheet1_H42, "/Sheet1/", "H42", 1.0).
?test(sheet1_I42, "/Sheet1/", "I42", "Ok.").
?test(sheet1_J42, "/Sheet1/", "J42", 0.252026529108327).
?test(sheet1_K42, "/Sheet1/", "K42", -0.196020633750921).
?test(sheet1_L42, "/Sheet1/", "L42", 0.0427413411938099).
?test(sheet1_A43, "/Sheet1/", "A43", "LN").
?test(sheet1_B43, "/Sheet1/", "B43", 1.09861228866811).
?test(sheet1_C43, "/Sheet1/", "C43", 1.09861228866811).
?test(sheet1_D43, "/Sheet1/", "D43", 0.0).
?test(sheet1_E43, "/Sheet1/", "E43", 0.0).
?test(sheet1_F43, "/Sheet1/", "F43", 3.49650756146648).
?test(sheet1_G43, "/Sheet1/", "G43", 3.49650756146648).
?test(sheet1_H43, "/Sheet1/", "H43", 1.0).
?test(sheet1_I43, "/Sheet1/", "I43", "Ok.").
?test(sheet1_A44, "/Sheet1/", "A44", "LOG").
?test(sheet1_B44, "/Sheet1/", "B44", 0.477121254719662).
?test(sheet1_C44, "/Sheet1/", "C44", 0.477121254719662).
?test(sheet1_D44, "/Sheet1/", "D44", 0.792481250360578).
?test(sheet1_E44, "/Sheet1/", "E44", 0.792481250360578).
?test(sheet1_F44, "/Sheet1/", "F44", 2.96146999977742).
?test(sheet1_G44, "/Sheet1/", "G44", 2.96146999977742).
?test(sheet1_H44, "/Sheet1/", "H44", 1.0).
?test(sheet1_I44, "/Sheet1/", "I44", "Ok.").
?test(sheet1_J44, "/Sheet1/", "J44", -0.293294030950626).
?test(sheet1_K44, "/Sheet1/", "K44", 0.00589535740604272).
?test(sheet1_L44, "/Sheet1/", "L44", 0.359616801768607).
?test(sheet1_A45, "/Sheet1/", "A45", "LOG10").
?test(sheet1_B45, "/Sheet1/", "B45", 0.845098040014257).
?test(sheet1_C45, "/Sheet1/", "C45", 0.845098040014257).
?test(sheet1_D45, "/Sheet1/", "D45", 1.34242268082221).
?test(sheet1_E45, "/Sheet1/", "E45", 1.34242268082221).
?test(sheet1_F45, "/Sheet1/", "F45", 1.53147891704226).
?test(sheet1_G45, "/Sheet1/", "G45", 1.53147891704226).
?test(sheet1_H45, "/Sheet1/", "H45", 1.0).
?test(sheet1_I45, "/Sheet1/", "I45", "Ok.").
?test(sheet1_J45, "/Sheet1/", "J45", 0.0294767870302137).
?test(sheet1_K45, "/Sheet1/", "K45", 0.421518054532056).
?test(sheet1_L45, "/Sheet1/", "L45", -0.287398673544584).
?test(sheet1_A46, "/Sheet1/", "A46", "MDETERM").
?test(sheet1_B46, "/Sheet1/", "B46", -2.0).
?test(sheet1_C46, "/Sheet1/", "C46", -2.0).
?test(sheet1_D46, "/Sheet1/", "D46", 283.0).
?test(sheet1_E46, "/Sheet1/", "E46", 283.0).
?test(sheet1_F46, "/Sheet1/", "F46", -1745.0).
?test(sheet1_G46, "/Sheet1/", "G46", -1745.0).
?test(sheet1_H46, "/Sheet1/", "H46", 1.0).
?test(sheet1_I46, "/Sheet1/", "I46", "Ok.").
?test(sheet1_J46, "/Sheet1/", "J46", 0.252026529108327).
?test(sheet1_K46, "/Sheet1/", "K46", -0.196020633750921).
?test(sheet1_L46, "/Sheet1/", "L46", 0.0427413411938099).
?test(sheet1_A47, "/Sheet1/", "A47", "MINVERSE").
?test(sheet1_B47, "/Sheet1/", "B47", true).
?test(sheet1_C47, "/Sheet1/", "C47", true).
?test(sheet1_D47, "/Sheet1/", "D47", 1.0).
?test(sheet1_E47, "/Sheet1/", "E47", 1.0).
?test(sheet1_F47, "/Sheet1/", "F47", 1.0).
?test(sheet1_G47, "/Sheet1/", "G47", 1.0).
?test(sheet1_H47, "/Sheet1/", "H47", 1.0).
?test(sheet1_I47, "/Sheet1/", "I47", "Ok.").
?test(sheet1_A48, "/Sheet1/", "A48", "MMULT").
?test(sheet1_B48, "/Sheet1/", "B48", true).
?test(sheet1_C48, "/Sheet1/", "C48", true).
?test(sheet1_D48, "/Sheet1/", "D48", 1.0).
?test(sheet1_E48, "/Sheet1/", "E48", 1.0).
?test(sheet1_F48, "/Sheet1/", "F48", 1.0).
?test(sheet1_G48, "/Sheet1/", "G48", 1.0).
?test(sheet1_H48, "/Sheet1/", "H48", 1.0).
?test(sheet1_I48, "/Sheet1/", "I48", "Ok.").
?test(sheet1_J48, "/Sheet1/", "J48", 37.3).
?test(sheet1_K48, "/Sheet1/", "K48", 31.95).
?test(sheet1_L48, "/Sheet1/", "L48", 48.4).
?test(sheet1_A49, "/Sheet1/", "A49", "MOD").
?test(sheet1_B49, "/Sheet1/", "B49", 8.0).
?test(sheet1_C49, "/Sheet1/", "C49", 8.0).
?test(sheet1_D49, "/Sheet1/", "D49", 1.0).
?test(sheet1_E49, "/Sheet1/", "E49", 1.0).
?test(sheet1_F49, "/Sheet1/", "F49", 2.0).
?test(sheet1_G49, "/Sheet1/", "G49", 2.0).
?test(sheet1_H49, "/Sheet1/", "H49", 1.0).
?test(sheet1_I49, "/Sheet1/", "I49", "Ok.").
?test(sheet1_J49, "/Sheet1/", "J49", 19.48).
?test(sheet1_K49, "/Sheet1/", "K49", 17.94).
?test(sheet1_L49, "/Sheet1/", "L49", 26.92).
?test(sheet1_A50, "/Sheet1/", "A50", "MROUND").
?test(sheet1_B50, "/Sheet1/", "B50", 23.4).
?test(sheet1_C50, "/Sheet1/", "C50", 23.4).
?test(sheet1_D50, "/Sheet1/", "D50", 23.34).
?test(sheet1_E50, "/Sheet1/", "E50", 23.34).
?test(sheet1_F50, "/Sheet1/", "F50", 24.0).
?test(sheet1_G50, "/Sheet1/", "G50", 24.0).
?test(sheet1_H50, "/Sheet1/", "H50", 1.0).
?test(sheet1_I50, "/Sheet1/", "I50", "Ok.").
?test(sheet1_J50, "/Sheet1/", "J50", 10.04).
?test(sheet1_K50, "/Sheet1/", "K50", 15.17).
?test(sheet1_L50, "/Sheet1/", "L50", 22.21).
?test(sheet1_A51, "/Sheet1/", "A51", "MULTINOMIAL").
?test(sheet1_B51, "/Sheet1/", "B51", 10.0).
?test(sheet1_C51, "/Sheet1/", "C51", 10.0).
?test(sheet1_D51, "/Sheet1/", "D51", 1260.0).
?test(sheet1_E51, "/Sheet1/", "E51", 1260.0).
?test(sheet1_F51, "/Sheet1/", "F51", 2522520.0).
?test(sheet1_G51, "/Sheet1/", "G51", 2522520.0).
?test(sheet1_H51, "/Sheet1/", "H51", 1.0).
?test(sheet1_I51, "/Sheet1/", "I51", "Ok.").
?test(sheet1_A52, "/Sheet1/", "A52", "ODD").
?test(sheet1_B52, "/Sheet1/", "B52", 3.0).
?test(sheet1_C52, "/Sheet1/", "C52", 3.0).
?test(sheet1_D52, "/Sheet1/", "D52", 5.0).
?test(sheet1_E52, "/Sheet1/", "E52", 5.0).
?test(sheet1_F52, "/Sheet1/", "F52", 5.0).
?test(sheet1_G52, "/Sheet1/", "G52", 5.0).
?test(sheet1_H52, "/Sheet1/", "H52", 1.0).
?test(sheet1_I52, "/Sheet1/", "I52", "Ok.").
?test(sheet1_J52, "/Sheet1/", "J52", 37.3).
?test(sheet1_K52, "/Sheet1/", "K52", 31.95).
?test(sheet1_L52, "/Sheet1/", "L52", 48.4).
?test(sheet1_A53, "/Sheet1/", "A53", "PI").
?test(sheet1_B53, "/Sheet1/", "B53", 3.14159265358979).
?test(sheet1_C53, "/Sheet1/", "C53", 3.14159265358979).
?test(sheet1_D53, "/Sheet1/", "D53", 3.14159265358979).
?test(sheet1_E53, "/Sheet1/", "E53", 3.14159265358979).
?test(sheet1_F53, "/Sheet1/", "F53", 3.14159265358979).
?test(sheet1_G53, "/Sheet1/", "G53", 3.14159265358979).
?test(sheet1_H53, "/Sheet1/", "H53", 1.0).
?test(sheet1_I53, "/Sheet1/", "I53", "Ok.").
?test(sheet1_J53, "/Sheet1/", "J53", 19.48).
?test(sheet1_K53, "/Sheet1/", "K53", 17.94).
?test(sheet1_L53, "/Sheet1/", "L53", 26.92).
?test(sheet1_A54, "/Sheet1/", "A54", "POWER").
?test(sheet1_B54, "/Sheet1/", "B54", 32.0).
?test(sheet1_C54, "/Sheet1/", "C54", 32.0).
?test(sheet1_D54, "/Sheet1/", "D54", 0.03125).
?test(sheet1_E54, "/Sheet1/", "E54", 0.03125).
?test(sheet1_F54, "/Sheet1/", "F54", 0.707106781186547).
?test(sheet1_G54, "/Sheet1/", "G54", 0.707106781186547).
?test(sheet1_H54, "/Sheet1/", "H54", 1.0).
?test(sheet1_I54, "/Sheet1/", "I54", "Ok.").
?test(sheet1_J54, "/Sheet1/", "J54", 10.04).
?test(sheet1_K54, "/Sheet1/", "K54", 15.17).
?test(sheet1_L54, "/Sheet1/", "L54", 22.21).
?test(sheet1_A55, "/Sheet1/", "A55", "PRODUCT").
?test(sheet1_B55, "/Sheet1/", "B55", 10.0).
?test(sheet1_C55, "/Sheet1/", "C55", 10.0).
?test(sheet1_D55, "/Sheet1/", "D55", 90.0).
?test(sheet1_E55, "/Sheet1/", "E55", 90.0).
?test(sheet1_F55, "/Sheet1/", "F55", 270.0).
?test(sheet1_G55, "/Sheet1/", "G55", 270.0).
?test(sheet1_H55, "/Sheet1/", "H55", 1.0).
?test(sheet1_I55, "/Sheet1/", "I55", "Ok.").
?test(sheet1_A56, "/Sheet1/", "A56", "QUOTIENT").
?test(sheet1_B56, "/Sheet1/", "B56", 0.0).
?test(sheet1_C56, "/Sheet1/", "C56", 0.0).
?test(sheet1_D56, "/Sheet1/", "D56", 4.0).
?test(sheet1_E56, "/Sheet1/", "E56", 4.0).
?test(sheet1_F56, "/Sheet1/", "F56", 4.0).
?test(sheet1_G56, "/Sheet1/", "G56", 4.0).
?test(sheet1_H56, "/Sheet1/", "H56", 1.0).
?test(sheet1_I56, "/Sheet1/", "I56", "Ok.").
?test(sheet1_A57, "/Sheet1/", "A57", "RADIANS").
?test(sheet1_B57, "/Sheet1/", "B57", 0.401425727958696).
?test(sheet1_C57, "/Sheet1/", "C57", 0.401425727958696).
?test(sheet1_D57, "/Sheet1/", "D57", -0.401425727958696).
?test(sheet1_E57, "/Sheet1/", "E57", -0.401425727958696).
?test(sheet1_F57, "/Sheet1/", "F57", 11.8158790360016).
?test(sheet1_G57, "/Sheet1/", "G57", 11.8158790360016).
?test(sheet1_H57, "/Sheet1/", "H57", 1.0).
?test(sheet1_I57, "/Sheet1/", "I57", "Ok.").
?test(sheet1_A58, "/Sheet1/", "A58", "RAND").
?test(sheet1_B58, "/Sheet1/", "B58", 0.0).
?test(sheet1_C58, "/Sheet1/", "C58", 0.0).
?test(sheet1_D58, "/Sheet1/", "D58", 0.0).
?test(sheet1_E58, "/Sheet1/", "E58", 0.0).
?test(sheet1_F58, "/Sheet1/", "F58", 0.0).
?test(sheet1_G58, "/Sheet1/", "G58", 0.0).
?test(sheet1_H58, "/Sheet1/", "H58", 1.0).
?test(sheet1_I58, "/Sheet1/", "I58", "Ok.").
?test(sheet1_A59, "/Sheet1/", "A59", "RANDBETWEEN").
?test(sheet1_B59, "/Sheet1/", "B59", 0.0).
?test(sheet1_C59, "/Sheet1/", "C59", 0.0).
?test(sheet1_D59, "/Sheet1/", "D59", 0.0).
?test(sheet1_E59, "/Sheet1/", "E59", 0.0).
?test(sheet1_F59, "/Sheet1/", "F59", 0.0).
?test(sheet1_G59, "/Sheet1/", "G59", 0.0).
?test(sheet1_H59, "/Sheet1/", "H59", 1.0).
?test(sheet1_I59, "/Sheet1/", "I59", "Ok.").
?test(sheet1_A60, "/Sheet1/", "A60", "ROMAN").
?test(sheet1_B60, "/Sheet1/", "B60", "CDXCIX").
?test(sheet1_C60, "/Sheet1/", "C60", "CDXCIX").
?test(sheet1_D60, "/Sheet1/", "D60", "XDIX").
?test(sheet1_E60, "/Sheet1/", "E60", "XDIX").
?test(sheet1_F60, "/Sheet1/", "F60", "ID").
?test(sheet1_G60, "/Sheet1/", "G60", "ID").
?test(sheet1_H60, "/Sheet1/", "H60", 1.0).
?test(sheet1_I60, "/Sheet1/", "I60", "Ok.").
?test(sheet1_A61, "/Sheet1/", "A61", "ROUND").
?test(sheet1_B61, "/Sheet1/", "B61", 4.63).
?test(sheet1_C61, "/Sheet1/", "C61", 4.63).
?test(sheet1_D61, "/Sheet1/", "D61", 20.0).
?test(sheet1_E61, "/Sheet1/", "E61", 20.0).
?test(sheet1_F61, "/Sheet1/", "F61", 25.0).
?test(sheet1_G61, "/Sheet1/", "G61", 25.0).
?test(sheet1_H61, "/Sheet1/", "H61", 1.0).
?test(sheet1_I61, "/Sheet1/", "I61", "Ok.").
?test(sheet1_A62, "/Sheet1/", "A62", "ROUNDDOWN").
?test(sheet1_B62, "/Sheet1/", "B62", 3.24).
?test(sheet1_C62, "/Sheet1/", "C62", 3.24).
?test(sheet1_D62, "/Sheet1/", "D62", 3.0).
?test(sheet1_E62, "/Sheet1/", "E62", 3.0).
?test(sheet1_F62, "/Sheet1/", "F62", 10.0).
?test(sheet1_G62, "/Sheet1/", "G62", 10.0).
?test(sheet1_H62, "/Sheet1/", "H62", 1.0).
?test(sheet1_I62, "/Sheet1/", "I62", "Ok.").
?test(sheet1_A63, "/Sheet1/", "A63", "ROUNDUP").
?test(sheet1_B63, "/Sheet1/", "B63", 3.25).
?test(sheet1_C63, "/Sheet1/", "C63", 3.25).
?test(sheet1_D63, "/Sheet1/", "D63", 4.0).
?test(sheet1_E63, "/Sheet1/", "E63", 4.0).
?test(sheet1_F63, "/Sheet1/", "F63", 20.0).
?test(sheet1_G63, "/Sheet1/", "G63", 20.0).
?test(sheet1_H63, "/Sheet1/", "H63", 1.0).
?test(sheet1_I63, "/Sheet1/", "I63", "Ok.").
?test(sheet1_A64, "/Sheet1/", "A64", "SERIESSUM").
?test(sheet1_B64, "/Sheet1/", "B64", 21994149.7753329).
?test(sheet1_C64, "/Sheet1/", "C64", 21994149.7753329).
?test(sheet1_D64, "/Sheet1/", "D64", 431530.184668572).
?test(sheet1_E64, "/Sheet1/", "E64", 431530.184668571).
?test(sheet1_F64, "/Sheet1/", "F64", -42587.985786784).
?test(sheet1_G64, "/Sheet1/", "G64", -42587.985786784).
?test(sheet1_H64, "/Sheet1/", "H64", 1.0).
?test(sheet1_I64, "/Sheet1/", "I64", "Ok.").
?test(sheet1_A65, "/Sheet1/", "A65", "SIGN").
?test(sheet1_B65, "/Sheet1/", "B65", 1.0).
?test(sheet1_C65, "/Sheet1/", "C65", 1.0).
?test(sheet1_D65, "/Sheet1/", "D65", -1.0).
?test(sheet1_E65, "/Sheet1/", "E65", -1.0).
?test(sheet1_F65, "/Sheet1/", "F65", 0.0).
?test(sheet1_G65, "/Sheet1/", "G65", 0.0).
?test(sheet1_H65, "/Sheet1/", "H65", 1.0).
?test(sheet1_I65, "/Sheet1/", "I65", "Ok.").
?test(sheet1_A66, "/Sheet1/", "A66", "SIN").
?test(sheet1_B66, "/Sheet1/", "B66", 0.0).
?test(sheet1_C66, "/Sheet1/", "C66", 0.0).
?test(sheet1_D66, "/Sheet1/", "D66", 0.909297426825682).
?test(sheet1_E66, "/Sheet1/", "E66", 0.909297426825682).
?test(sheet1_F66, "/Sheet1/", "F66", 0.756802495307928).
?test(sheet1_G66, "/Sheet1/", "G66", 0.756802495307928).
?test(sheet1_H66, "/Sheet1/", "H66", 1.0).
?test(sheet1_I66, "/Sheet1/", "I66", "Ok.").
?test(sheet1_A67, "/Sheet1/", "A67", "SINH").
?test(sheet1_B67, "/Sheet1/", "B67", 3.62686040784702).
?test(sheet1_C67, "/Sheet1/", "C67", 3.62686040784702).
?test(sheet1_D67, "/Sheet1/", "D67", -10.0178749274099).
?test(sheet1_E67, "/Sheet1/", "E67", -10.0178749274099).
?test(sheet1_F67, "/Sheet1/", "F67", 201.713157370279).
?test(sheet1_G67, "/Sheet1/", "G67", 201.713157370279).
?test(sheet1_H67, "/Sheet1/", "H67", 1.0).
?test(sheet1_I67, "/Sheet1/", "I67", "Ok.").
?test(sheet1_A68, "/Sheet1/", "A68", "SQRT").
?test(sheet1_B68, "/Sheet1/", "B68", 1.73205080756888).
?test(sheet1_C68, "/Sheet1/", "C68", 1.73205080756888).
?test(sheet1_D68, "/Sheet1/", "D68", 2.44948974278318).
?test(sheet1_E68, "/Sheet1/", "E68", 2.44948974278318).
?test(sheet1_F68, "/Sheet1/", "F68", 9.9498743710662).
?test(sheet1_G68, "/Sheet1/", "G68", 9.9498743710662).
?test(sheet1_H68, "/Sheet1/", "H68", 1.0).
?test(sheet1_I68, "/Sheet1/", "I68", "Ok.").
?test(sheet1_A69, "/Sheet1/", "A69", "SQRTPI").
?test(sheet1_B69, "/Sheet1/", "B69", 2.506628274631).
?test(sheet1_C69, "/Sheet1/", "C69", 2.506628274631).
?test(sheet1_D69, "/Sheet1/", "D69", 1.77245385090552).
?test(sheet1_E69, "/Sheet1/", "E69", 1.77245385090552).
?test(sheet1_F69, "/Sheet1/", "F69", 4.68947209983475).
?test(sheet1_G69, "/Sheet1/", "G69", 4.68947209983475).
?test(sheet1_H69, "/Sheet1/", "H69", 1.0).
?test(sheet1_I69, "/Sheet1/", "I69", "Ok.").
?test(sheet1_A70, "/Sheet1/", "A70", "SUBTOTAL").
?test(sheet1_B70, "/Sheet1/", "B70", 4.0).
?test(sheet1_C70, "/Sheet1/", "C70", 4.0).
?test(sheet1_D70, "/Sheet1/", "D70", 25.8291824622203).
?test(sheet1_E70, "/Sheet1/", "E70", 25.8291824622203).
?test(sheet1_F70, "/Sheet1/", "F70", 60.3).
?test(sheet1_G70, "/Sheet1/", "G70", 60.3).
?test(sheet1_H70, "/Sheet1/", "H70", 1.0).
?test(sheet1_I70, "/Sheet1/", "I70", "Ok.").
?test(sheet1_A71, "/Sheet1/", "A71", "SUM").
?test(sheet1_B71, "/Sheet1/", "B71", 6.0).
?test(sheet1_C71, "/Sheet1/", "C71", 6.0).
?test(sheet1_D71, "/Sheet1/", "D71", 14.0).
?test(sheet1_E71, "/Sheet1/", "E71", 14.0).
?test(sheet1_F71, "/Sheet1/", "F71", 129.3).
?test(sheet1_G71, "/Sheet1/", "G71", 129.3).
?test(sheet1_H71, "/Sheet1/", "H71", 1.0).
?test(sheet1_I71, "/Sheet1/", "I71", "Ok.").
?test(sheet1_A72, "/Sheet1/", "A72", "SUMIF").
?test(sheet1_B72, "/Sheet1/", "B72", 89.9).
?test(sheet1_C72, "/Sheet1/", "C72", 89.9).
?test(sheet1_D72, "/Sheet1/", "D72", 264.9).
?test(sheet1_E72, "/Sheet1/", "E72", 264.9).
?test(sheet1_F72, "/Sheet1/", "F72", 23.9).
?test(sheet1_G72, "/Sheet1/", "G72", 23.9).
?test(sheet1_H72, "/Sheet1/", "H72", 1.0).
?test(sheet1_I72, "/Sheet1/", "I72", "Ok.").
?test(sheet1_A73, "/Sheet1/", "A73", "SUMPRODUCT").
?test(sheet1_B73, "/Sheet1/", "B73", 9541.77).
?test(sheet1_C73, "/Sheet1/", "C73", 9541.77).
?test(sheet1_D73, "/Sheet1/", "D73", 1912.85).
?test(sheet1_E73, "/Sheet1/", "E73", 1912.85).
?test(sheet1_F73, "/Sheet1/", "F73", 1978.8781342).
?test(sheet1_G73, "/Sheet1/", "G73", 1978.8781342).
?test(sheet1_H73, "/Sheet1/", "H73", 1.0).
?test(sheet1_I73, "/Sheet1/", "I73", "Ok.").
?test(sheet1_A74, "/Sheet1/", "A74", "SUMSQ").
?test(sheet1_B74, "/Sheet1/", "B74", 151.0).
?test(sheet1_C74, "/Sheet1/", "C74", 151.0).
?test(sheet1_D74, "/Sheet1/", "D74", 23840.88).
?test(sheet1_E74, "/Sheet1/", "E74", 23840.88).
?test(sheet1_F74, "/Sheet1/", "F74", 20588.24).
?test(sheet1_G74, "/Sheet1/", "G74", 20588.24).
?test(sheet1_H74, "/Sheet1/", "H74", 1.0).
?test(sheet1_I74, "/Sheet1/", "I74", "Ok.").
?test(sheet1_A75, "/Sheet1/", "A75", "SUMX2MY2").
?test(sheet1_B75, "/Sheet1/", "B75", -13687.3).
?test(sheet1_C75, "/Sheet1/", "C75", -13687.3).
?test(sheet1_D75, "/Sheet1/", "D75", 5139.44).
?test(sheet1_E75, "/Sheet1/", "E75", 5139.44).
?test(sheet1_F75, "/Sheet1/", "F75", 1712.32048069385).
?test(sheet1_G75, "/Sheet1/", "G75", 1712.32048069385).
?test(sheet1_H75, "/Sheet1/", "H75", 1.0).
?test(sheet1_I75, "/Sheet1/", "I75", "Ok.").
?test(sheet1_A76, "/Sheet1/", "A76", "SUMX2PY2").
?test(sheet1_B76, "/Sheet1/", "B76", 23840.88).
?test(sheet1_C76, "/Sheet1/", "C76", 23840.88).
?test(sheet1_D76, "/Sheet1/", "D76", 5361.44).
?test(sheet1_E76, "/Sheet1/", "E76", 5361.44).
?test(sheet1_F76, "/Sheet1/", "F76", 1713.97951930615).
?test(sheet1_G76, "/Sheet1/", "G76", 1713.97951930615).
?test(sheet1_H76, "/Sheet1/", "H76", 1.0).
?test(sheet1_I76, "/Sheet1/", "I76", "Ok.").
?test(sheet1_A77, "/Sheet1/", "A77", "SUMXMY2").
?test(sheet1_B77, "/Sheet1/", "B77", 4757.34).
?test(sheet1_C77, "/Sheet1/", "C77", 4757.34).
?test(sheet1_D77, "/Sheet1/", "D77", 6093.24).
?test(sheet1_E77, "/Sheet1/", "E77", 6093.24).
?test(sheet1_F77, "/Sheet1/", "F77", 1679.49004390615).
?test(sheet1_G77, "/Sheet1/", "G77", 1679.49004390615).
?test(sheet1_H77, "/Sheet1/", "H77", 1.0).
?test(sheet1_I77, "/Sheet1/", "I77", "Ok.").
?test(sheet1_A78, "/Sheet1/", "A78", "TAN").
?test(sheet1_B78, "/Sheet1/", "B78", -0.142546543074278).
?test(sheet1_C78, "/Sheet1/", "C78", -0.142546543074278).
?test(sheet1_D78, "/Sheet1/", "D78", -0.00885165604168446).
?test(sheet1_E78, "/Sheet1/", "E78", -0.00885165604168446).
?test(sheet1_F78, "/Sheet1/", "F78", -32.2685757759344).
?test(sheet1_G78, "/Sheet1/", "G78", -32.2685757759344).
?test(sheet1_H78, "/Sheet1/", "H78", 1.0).
?test(sheet1_I78, "/Sheet1/", "I78", "Ok.").
?test(sheet1_A79, "/Sheet1/", "A79", "TANH").
?test(sheet1_B79, "/Sheet1/", "B79", 0.964027580075817).
?test(sheet1_C79, "/Sheet1/", "C79", 0.964027580075817).
?test(sheet1_D79, "/Sheet1/", "D79", -1.0).
?test(sheet1_E79, "/Sheet1/", "E79", -1.0).
?test(sheet1_F79, "/Sheet1/", "F79", 0.964027580075817).
?test(sheet1_G79, "/Sheet1/", "G79", 0.964027580075817).
?test(sheet1_H79, "/Sheet1/", "H79", 1.0).
?test(sheet1_I79, "/Sheet1/", "I79", "Ok.").
?test(sheet1_A80, "/Sheet1/", "A80", "TRUNC").
?test(sheet1_B80, "/Sheet1/", "B80", 7.0).
?test(sheet1_C80, "/Sheet1/", "C80", 7.0).
?test(sheet1_D80, "/Sheet1/", "D80", 2.32).
?test(sheet1_E80, "/Sheet1/", "E80", 2.32).
?test(sheet1_F80, "/Sheet1/", "F80", 200.0).
?test(sheet1_G80, "/Sheet1/", "G80", 200.0).
?test(sheet1_H80, "/Sheet1/", "H80", 1.0).
?test(sheet1_I80, "/Sheet1/", "I80", "Ok.").
?test(sheet1_A81, "/Sheet1/", "A81", "Total").
?test(sheet1_H81, "/Sheet1/", "H81", true).
?test(sheet1_I81, "/Sheet1/", "I81", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_maths.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_maths" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_I17,
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
        sheet1_J31,
        sheet1_K31,
        sheet1_L31,
        sheet1_A32,
        sheet1_B32,
        sheet1_C32,
        sheet1_D32,
        sheet1_E32,
        sheet1_F32,
        sheet1_G32,
        sheet1_H32,
        sheet1_I32,
        sheet1_J32,
        sheet1_K32,
        sheet1_L32,
        sheet1_A33,
        sheet1_B33,
        sheet1_C33,
        sheet1_D33,
        sheet1_E33,
        sheet1_F33,
        sheet1_G33,
        sheet1_H33,
        sheet1_I33,
        sheet1_J33,
        sheet1_K33,
        sheet1_L33,
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
        sheet1_J36,
        sheet1_K36,
        sheet1_L36,
        sheet1_A37,
        sheet1_B37,
        sheet1_C37,
        sheet1_D37,
        sheet1_E37,
        sheet1_F37,
        sheet1_G37,
        sheet1_H37,
        sheet1_I37,
        sheet1_J37,
        sheet1_K37,
        sheet1_L37,
        sheet1_A38,
        sheet1_B38,
        sheet1_C38,
        sheet1_D38,
        sheet1_E38,
        sheet1_F38,
        sheet1_G38,
        sheet1_H38,
        sheet1_I38,
        sheet1_J38,
        sheet1_K38,
        sheet1_L38,
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
        sheet1_J40,
        sheet1_K40,
        sheet1_L40,
        sheet1_A41,
        sheet1_B41,
        sheet1_C41,
        sheet1_D41,
        sheet1_E41,
        sheet1_F41,
        sheet1_G41,
        sheet1_H41,
        sheet1_I41,
        sheet1_J41,
        sheet1_K41,
        sheet1_L41,
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
        sheet1_A44,
        sheet1_B44,
        sheet1_C44,
        sheet1_D44,
        sheet1_E44,
        sheet1_F44,
        sheet1_G44,
        sheet1_H44,
        sheet1_I44,
        sheet1_J44,
        sheet1_K44,
        sheet1_L44,
        sheet1_A45,
        sheet1_B45,
        sheet1_C45,
        sheet1_D45,
        sheet1_E45,
        sheet1_F45,
        sheet1_G45,
        sheet1_H45,
        sheet1_I45,
        sheet1_J45,
        sheet1_K45,
        sheet1_L45,
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
        sheet1_K46,
        sheet1_L46,
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
        sheet1_J48,
        sheet1_K48,
        sheet1_L48,
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
        sheet1_K49,
        sheet1_L49,
        sheet1_A50,
        sheet1_B50,
        sheet1_C50,
        sheet1_D50,
        sheet1_E50,
        sheet1_F50,
        sheet1_G50,
        sheet1_H50,
        sheet1_I50,
        sheet1_J50,
        sheet1_K50,
        sheet1_L50,
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
        sheet1_J52,
        sheet1_K52,
        sheet1_L52,
        sheet1_A53,
        sheet1_B53,
        sheet1_C53,
        sheet1_D53,
        sheet1_E53,
        sheet1_F53,
        sheet1_G53,
        sheet1_H53,
        sheet1_I53,
        sheet1_J53,
        sheet1_K53,
        sheet1_L53,
        sheet1_A54,
        sheet1_B54,
        sheet1_C54,
        sheet1_D54,
        sheet1_E54,
        sheet1_F54,
        sheet1_G54,
        sheet1_H54,
        sheet1_I54,
        sheet1_J54,
        sheet1_K54,
        sheet1_L54,
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
        sheet1_H81,
        sheet1_I81
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
