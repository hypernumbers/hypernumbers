%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_bitwise_bitrshift_SUITE).
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
                     [Testcase, "e_gnumeric_bitwise_bitrshift_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_bitwise_bitrshift" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Bitrshift/", "A1", "bitrshift(A,B)").
?test(sheet1_B1, "/Bitrshift/", "B1", "B").
?test(sheet1_C1, "/Bitrshift/", "C1", "errors").
?test(sheet1_D1, "/Bitrshift/", "D1", "errors").
?test(sheet1_E1, "/Bitrshift/", "E1", "errors").
?test(sheet1_F1, "/Bitrshift/", "F1", "errors").
?test(sheet1_G1, "/Bitrshift/", "G1", "errors").
?test(sheet1_H1, "/Bitrshift/", "H1", "errors").
?test(sheet1_I1, "/Bitrshift/", "I1", "String").
?test(sheet1_J1, "/Bitrshift/", "J1", "String Number").
?test(sheet1_K1, "/Bitrshift/", "K1", "String number Leading space").
?test(sheet1_L1, "/Bitrshift/", "L1", "Integer").
?test(sheet1_M1, "/Bitrshift/", "M1", "Float").
?test(sheet1_N1, "/Bitrshift/", "N1", "Blank").
?test(sheet1_O1, "/Bitrshift/", "O1", "Logical").
?test(sheet1_P1, "/Bitrshift/", "P1", "Logical").
?test(sheet1_Q1, "/Bitrshift/", "Q1", "Range Row").
?test(sheet1_R1, "/Bitrshift/", "R1", "Range Row").
?test(sheet1_S1, "/Bitrshift/", "S1", "Range Area").
?test(sheet1_T1, "/Bitrshift/", "T1", "Range Area").
?test(sheet1_U1, "/Bitrshift/", "U1", "Range Colunm").
?test(sheet1_V1, "/Bitrshift/", "V1", "Range Colunm").
?test(sheet1_A2, "/Bitrshift/", "A2", "A").
?test(sheet1_C2, "/Bitrshift/", "C2", '#DIV/0!').
?test(sheet1_D2, "/Bitrshift/", "D2", '#VALUE!').
?test(sheet1_E2, "/Bitrshift/", "E2", '#REF!').
?test(sheet1_F2, "/Bitrshift/", "F2", '#NAME?').
?test(sheet1_G2, "/Bitrshift/", "G2", '#NUM!').
?test(sheet1_H2, "/Bitrshift/", "H2", '#N/A').
?test(sheet1_I2, "/Bitrshift/", "I2", "Phillip").
?test(sheet1_J2, "/Bitrshift/", "J2", "13").
?test(sheet1_K2, "/Bitrshift/", "K2", " 24").
?test(sheet1_L2, "/Bitrshift/", "L2", "1968/03/23 00:00:00").
?test(sheet1_M2, "/Bitrshift/", "M2", 3.14159265358979).
?test(sheet1_O2, "/Bitrshift/", "O2", true).
?test(sheet1_P2, "/Bitrshift/", "P2", false).
?test(sheet1_Q2, "/Bitrshift/", "Q2", "X3:Y3").
?test(sheet1_R2, "/Bitrshift/", "R2", "X3:AA3").
?test(sheet1_S2, "/Bitrshift/", "S2", "X3:Y4").
?test(sheet1_T2, "/Bitrshift/", "T2", "X3:AA6").
?test(sheet1_U2, "/Bitrshift/", "U2", "X3:X4").
?test(sheet1_V2, "/Bitrshift/", "V2", "X3:X6").
?test(sheet1_A3, "/Bitrshift/", "A3", "errors").
?test(sheet1_B3, "/Bitrshift/", "B3", '#DIV/0!').
?test(sheet1_C3, "/Bitrshift/", "C3", '#NAME?').
?test(sheet1_D3, "/Bitrshift/", "D3", '#NAME?').
?test(sheet1_E3, "/Bitrshift/", "E3", '#NAME?').
?test(sheet1_F3, "/Bitrshift/", "F3", '#NAME?').
?test(sheet1_G3, "/Bitrshift/", "G3", '#NAME?').
?test(sheet1_H3, "/Bitrshift/", "H3", '#NAME?').
?test(sheet1_I3, "/Bitrshift/", "I3", '#NAME?').
?test(sheet1_J3, "/Bitrshift/", "J3", '#NAME?').
?test(sheet1_K3, "/Bitrshift/", "K3", '#NAME?').
?test(sheet1_L3, "/Bitrshift/", "L3", '#NAME?').
?test(sheet1_M3, "/Bitrshift/", "M3", '#NAME?').
?test(sheet1_N3, "/Bitrshift/", "N3", '#NAME?').
?test(sheet1_O3, "/Bitrshift/", "O3", '#NAME?').
?test(sheet1_P3, "/Bitrshift/", "P3", '#NAME?').
?test(sheet1_Q3, "/Bitrshift/", "Q3", '#NAME?').
?test(sheet1_R3, "/Bitrshift/", "R3", '#NAME?').
?test(sheet1_S3, "/Bitrshift/", "S3", '#NAME?').
?test(sheet1_T3, "/Bitrshift/", "T3", '#NAME?').
?test(sheet1_U3, "/Bitrshift/", "U3", '#NAME?').
?test(sheet1_V3, "/Bitrshift/", "V3", '#NAME?').
?test(sheet1_X3, "/Bitrshift/", "X3", 7.0).
?test(sheet1_Y3, "/Bitrshift/", "Y3", 5.0).
?test(sheet1_Z3, "/Bitrshift/", "Z3", 3.0).
?test(sheet1_AA3, "/Bitrshift/", "AA3", 1.0).
?test(sheet1_A4, "/Bitrshift/", "A4", "errors").
?test(sheet1_B4, "/Bitrshift/", "B4", '#VALUE!').
?test(sheet1_C4, "/Bitrshift/", "C4", '#NAME?').
?test(sheet1_D4, "/Bitrshift/", "D4", '#NAME?').
?test(sheet1_E4, "/Bitrshift/", "E4", '#NAME?').
?test(sheet1_F4, "/Bitrshift/", "F4", '#NAME?').
?test(sheet1_G4, "/Bitrshift/", "G4", '#NAME?').
?test(sheet1_H4, "/Bitrshift/", "H4", '#NAME?').
?test(sheet1_I4, "/Bitrshift/", "I4", '#NAME?').
?test(sheet1_J4, "/Bitrshift/", "J4", '#NAME?').
?test(sheet1_K4, "/Bitrshift/", "K4", '#NAME?').
?test(sheet1_L4, "/Bitrshift/", "L4", '#NAME?').
?test(sheet1_M4, "/Bitrshift/", "M4", '#NAME?').
?test(sheet1_N4, "/Bitrshift/", "N4", '#NAME?').
?test(sheet1_O4, "/Bitrshift/", "O4", '#NAME?').
?test(sheet1_P4, "/Bitrshift/", "P4", '#NAME?').
?test(sheet1_Q4, "/Bitrshift/", "Q4", '#NAME?').
?test(sheet1_R4, "/Bitrshift/", "R4", '#NAME?').
?test(sheet1_S4, "/Bitrshift/", "S4", '#NAME?').
?test(sheet1_T4, "/Bitrshift/", "T4", '#NAME?').
?test(sheet1_U4, "/Bitrshift/", "U4", '#NAME?').
?test(sheet1_V4, "/Bitrshift/", "V4", '#NAME?').
?test(sheet1_X4, "/Bitrshift/", "X4", 8.0).
?test(sheet1_Y4, "/Bitrshift/", "Y4", 9.0).
?test(sheet1_Z4, "/Bitrshift/", "Z4", 10.0).
?test(sheet1_AA4, "/Bitrshift/", "AA4", 11.0).
?test(sheet1_A5, "/Bitrshift/", "A5", "errors").
?test(sheet1_B5, "/Bitrshift/", "B5", '#REF!').
?test(sheet1_C5, "/Bitrshift/", "C5", '#NAME?').
?test(sheet1_D5, "/Bitrshift/", "D5", '#NAME?').
?test(sheet1_E5, "/Bitrshift/", "E5", '#NAME?').
?test(sheet1_F5, "/Bitrshift/", "F5", '#NAME?').
?test(sheet1_G5, "/Bitrshift/", "G5", '#NAME?').
?test(sheet1_H5, "/Bitrshift/", "H5", '#NAME?').
?test(sheet1_I5, "/Bitrshift/", "I5", '#NAME?').
?test(sheet1_J5, "/Bitrshift/", "J5", '#NAME?').
?test(sheet1_K5, "/Bitrshift/", "K5", '#NAME?').
?test(sheet1_L5, "/Bitrshift/", "L5", '#NAME?').
?test(sheet1_M5, "/Bitrshift/", "M5", '#NAME?').
?test(sheet1_N5, "/Bitrshift/", "N5", '#NAME?').
?test(sheet1_O5, "/Bitrshift/", "O5", '#NAME?').
?test(sheet1_P5, "/Bitrshift/", "P5", '#NAME?').
?test(sheet1_Q5, "/Bitrshift/", "Q5", '#NAME?').
?test(sheet1_R5, "/Bitrshift/", "R5", '#NAME?').
?test(sheet1_S5, "/Bitrshift/", "S5", '#NAME?').
?test(sheet1_T5, "/Bitrshift/", "T5", '#NAME?').
?test(sheet1_U5, "/Bitrshift/", "U5", '#NAME?').
?test(sheet1_V5, "/Bitrshift/", "V5", '#NAME?').
?test(sheet1_X5, "/Bitrshift/", "X5", 9.0).
?test(sheet1_Y5, "/Bitrshift/", "Y5", 13.0).
?test(sheet1_Z5, "/Bitrshift/", "Z5", 17.0).
?test(sheet1_AA5, "/Bitrshift/", "AA5", 21.0).
?test(sheet1_A6, "/Bitrshift/", "A6", "errors").
?test(sheet1_B6, "/Bitrshift/", "B6", '#NAME?').
?test(sheet1_C6, "/Bitrshift/", "C6", '#NAME?').
?test(sheet1_D6, "/Bitrshift/", "D6", '#NAME?').
?test(sheet1_E6, "/Bitrshift/", "E6", '#NAME?').
?test(sheet1_F6, "/Bitrshift/", "F6", '#NAME?').
?test(sheet1_G6, "/Bitrshift/", "G6", '#NAME?').
?test(sheet1_H6, "/Bitrshift/", "H6", '#NAME?').
?test(sheet1_I6, "/Bitrshift/", "I6", '#NAME?').
?test(sheet1_J6, "/Bitrshift/", "J6", '#NAME?').
?test(sheet1_K6, "/Bitrshift/", "K6", '#NAME?').
?test(sheet1_L6, "/Bitrshift/", "L6", '#NAME?').
?test(sheet1_M6, "/Bitrshift/", "M6", '#NAME?').
?test(sheet1_N6, "/Bitrshift/", "N6", '#NAME?').
?test(sheet1_O6, "/Bitrshift/", "O6", '#NAME?').
?test(sheet1_P6, "/Bitrshift/", "P6", '#NAME?').
?test(sheet1_Q6, "/Bitrshift/", "Q6", '#NAME?').
?test(sheet1_R6, "/Bitrshift/", "R6", '#NAME?').
?test(sheet1_S6, "/Bitrshift/", "S6", '#NAME?').
?test(sheet1_T6, "/Bitrshift/", "T6", '#NAME?').
?test(sheet1_U6, "/Bitrshift/", "U6", '#NAME?').
?test(sheet1_V6, "/Bitrshift/", "V6", '#NAME?').
?test(sheet1_X6, "/Bitrshift/", "X6", 10.0).
?test(sheet1_Y6, "/Bitrshift/", "Y6", 17.0).
?test(sheet1_Z6, "/Bitrshift/", "Z6", 24.0).
?test(sheet1_AA6, "/Bitrshift/", "AA6", 31.0).
?test(sheet1_A7, "/Bitrshift/", "A7", "errors").
?test(sheet1_B7, "/Bitrshift/", "B7", '#NUM!').
?test(sheet1_C7, "/Bitrshift/", "C7", '#NAME?').
?test(sheet1_D7, "/Bitrshift/", "D7", '#NAME?').
?test(sheet1_E7, "/Bitrshift/", "E7", '#NAME?').
?test(sheet1_F7, "/Bitrshift/", "F7", '#NAME?').
?test(sheet1_G7, "/Bitrshift/", "G7", '#NAME?').
?test(sheet1_H7, "/Bitrshift/", "H7", '#NAME?').
?test(sheet1_I7, "/Bitrshift/", "I7", '#NAME?').
?test(sheet1_J7, "/Bitrshift/", "J7", '#NAME?').
?test(sheet1_K7, "/Bitrshift/", "K7", '#NAME?').
?test(sheet1_L7, "/Bitrshift/", "L7", '#NAME?').
?test(sheet1_M7, "/Bitrshift/", "M7", '#NAME?').
?test(sheet1_N7, "/Bitrshift/", "N7", '#NAME?').
?test(sheet1_O7, "/Bitrshift/", "O7", '#NAME?').
?test(sheet1_P7, "/Bitrshift/", "P7", '#NAME?').
?test(sheet1_Q7, "/Bitrshift/", "Q7", '#NAME?').
?test(sheet1_R7, "/Bitrshift/", "R7", '#NAME?').
?test(sheet1_S7, "/Bitrshift/", "S7", '#NAME?').
?test(sheet1_T7, "/Bitrshift/", "T7", '#NAME?').
?test(sheet1_U7, "/Bitrshift/", "U7", '#NAME?').
?test(sheet1_V7, "/Bitrshift/", "V7", '#NAME?').
?test(sheet1_A8, "/Bitrshift/", "A8", "errors").
?test(sheet1_B8, "/Bitrshift/", "B8", '#N/A').
?test(sheet1_C8, "/Bitrshift/", "C8", '#NAME?').
?test(sheet1_D8, "/Bitrshift/", "D8", '#NAME?').
?test(sheet1_E8, "/Bitrshift/", "E8", '#NAME?').
?test(sheet1_F8, "/Bitrshift/", "F8", '#NAME?').
?test(sheet1_G8, "/Bitrshift/", "G8", '#NAME?').
?test(sheet1_H8, "/Bitrshift/", "H8", '#NAME?').
?test(sheet1_I8, "/Bitrshift/", "I8", '#NAME?').
?test(sheet1_J8, "/Bitrshift/", "J8", '#NAME?').
?test(sheet1_K8, "/Bitrshift/", "K8", '#NAME?').
?test(sheet1_L8, "/Bitrshift/", "L8", '#NAME?').
?test(sheet1_M8, "/Bitrshift/", "M8", '#NAME?').
?test(sheet1_N8, "/Bitrshift/", "N8", '#NAME?').
?test(sheet1_O8, "/Bitrshift/", "O8", '#NAME?').
?test(sheet1_P8, "/Bitrshift/", "P8", '#NAME?').
?test(sheet1_Q8, "/Bitrshift/", "Q8", '#NAME?').
?test(sheet1_R8, "/Bitrshift/", "R8", '#NAME?').
?test(sheet1_S8, "/Bitrshift/", "S8", '#NAME?').
?test(sheet1_T8, "/Bitrshift/", "T8", '#NAME?').
?test(sheet1_U8, "/Bitrshift/", "U8", '#NAME?').
?test(sheet1_V8, "/Bitrshift/", "V8", '#NAME?').
?test(sheet1_A9, "/Bitrshift/", "A9", "String").
?test(sheet1_B9, "/Bitrshift/", "B9", "Phillip").
?test(sheet1_C9, "/Bitrshift/", "C9", '#NAME?').
?test(sheet1_D9, "/Bitrshift/", "D9", '#NAME?').
?test(sheet1_E9, "/Bitrshift/", "E9", '#NAME?').
?test(sheet1_F9, "/Bitrshift/", "F9", '#NAME?').
?test(sheet1_G9, "/Bitrshift/", "G9", '#NAME?').
?test(sheet1_H9, "/Bitrshift/", "H9", '#NAME?').
?test(sheet1_I9, "/Bitrshift/", "I9", '#NAME?').
?test(sheet1_J9, "/Bitrshift/", "J9", '#NAME?').
?test(sheet1_K9, "/Bitrshift/", "K9", '#NAME?').
?test(sheet1_L9, "/Bitrshift/", "L9", '#NAME?').
?test(sheet1_M9, "/Bitrshift/", "M9", '#NAME?').
?test(sheet1_N9, "/Bitrshift/", "N9", '#NAME?').
?test(sheet1_O9, "/Bitrshift/", "O9", '#NAME?').
?test(sheet1_P9, "/Bitrshift/", "P9", '#NAME?').
?test(sheet1_Q9, "/Bitrshift/", "Q9", '#NAME?').
?test(sheet1_R9, "/Bitrshift/", "R9", '#NAME?').
?test(sheet1_S9, "/Bitrshift/", "S9", '#NAME?').
?test(sheet1_T9, "/Bitrshift/", "T9", '#NAME?').
?test(sheet1_U9, "/Bitrshift/", "U9", '#NAME?').
?test(sheet1_V9, "/Bitrshift/", "V9", '#NAME?').
?test(sheet1_A10, "/Bitrshift/", "A10", "String Number").
?test(sheet1_B10, "/Bitrshift/", "B10", "12").
?test(sheet1_C10, "/Bitrshift/", "C10", '#NAME?').
?test(sheet1_D10, "/Bitrshift/", "D10", '#NAME?').
?test(sheet1_E10, "/Bitrshift/", "E10", '#NAME?').
?test(sheet1_F10, "/Bitrshift/", "F10", '#NAME?').
?test(sheet1_G10, "/Bitrshift/", "G10", '#NAME?').
?test(sheet1_H10, "/Bitrshift/", "H10", '#NAME?').
?test(sheet1_I10, "/Bitrshift/", "I10", '#NAME?').
?test(sheet1_J10, "/Bitrshift/", "J10", '#NAME?').
?test(sheet1_K10, "/Bitrshift/", "K10", '#NAME?').
?test(sheet1_L10, "/Bitrshift/", "L10", '#NAME?').
?test(sheet1_M10, "/Bitrshift/", "M10", '#NAME?').
?test(sheet1_N10, "/Bitrshift/", "N10", '#NAME?').
?test(sheet1_O10, "/Bitrshift/", "O10", '#NAME?').
?test(sheet1_P10, "/Bitrshift/", "P10", '#NAME?').
?test(sheet1_Q10, "/Bitrshift/", "Q10", '#NAME?').
?test(sheet1_R10, "/Bitrshift/", "R10", '#NAME?').
?test(sheet1_S10, "/Bitrshift/", "S10", '#NAME?').
?test(sheet1_T10, "/Bitrshift/", "T10", '#NAME?').
?test(sheet1_U10, "/Bitrshift/", "U10", '#NAME?').
?test(sheet1_V10, "/Bitrshift/", "V10", '#NAME?').
?test(sheet1_A11, "/Bitrshift/", "A11", "String Number Leading space").
?test(sheet1_B11, "/Bitrshift/", "B11", " 23").
?test(sheet1_C11, "/Bitrshift/", "C11", '#NAME?').
?test(sheet1_D11, "/Bitrshift/", "D11", '#NAME?').
?test(sheet1_E11, "/Bitrshift/", "E11", '#NAME?').
?test(sheet1_F11, "/Bitrshift/", "F11", '#NAME?').
?test(sheet1_G11, "/Bitrshift/", "G11", '#NAME?').
?test(sheet1_H11, "/Bitrshift/", "H11", '#NAME?').
?test(sheet1_I11, "/Bitrshift/", "I11", '#NAME?').
?test(sheet1_J11, "/Bitrshift/", "J11", '#NAME?').
?test(sheet1_K11, "/Bitrshift/", "K11", '#NAME?').
?test(sheet1_L11, "/Bitrshift/", "L11", '#NAME?').
?test(sheet1_M11, "/Bitrshift/", "M11", '#NAME?').
?test(sheet1_N11, "/Bitrshift/", "N11", '#NAME?').
?test(sheet1_O11, "/Bitrshift/", "O11", '#NAME?').
?test(sheet1_P11, "/Bitrshift/", "P11", '#NAME?').
?test(sheet1_Q11, "/Bitrshift/", "Q11", '#NAME?').
?test(sheet1_R11, "/Bitrshift/", "R11", '#NAME?').
?test(sheet1_S11, "/Bitrshift/", "S11", '#NAME?').
?test(sheet1_T11, "/Bitrshift/", "T11", '#NAME?').
?test(sheet1_U11, "/Bitrshift/", "U11", '#NAME?').
?test(sheet1_V11, "/Bitrshift/", "V11", '#NAME?').
?test(sheet1_A12, "/Bitrshift/", "A12", "Interger").
?test(sheet1_B12, "/Bitrshift/", "B12", "1968/03/23 00:00:00").
?test(sheet1_C12, "/Bitrshift/", "C12", '#NAME?').
?test(sheet1_D12, "/Bitrshift/", "D12", '#NAME?').
?test(sheet1_E12, "/Bitrshift/", "E12", '#NAME?').
?test(sheet1_F12, "/Bitrshift/", "F12", '#NAME?').
?test(sheet1_G12, "/Bitrshift/", "G12", '#NAME?').
?test(sheet1_H12, "/Bitrshift/", "H12", '#NAME?').
?test(sheet1_I12, "/Bitrshift/", "I12", '#NAME?').
?test(sheet1_J12, "/Bitrshift/", "J12", '#NAME?').
?test(sheet1_K12, "/Bitrshift/", "K12", '#NAME?').
?test(sheet1_L12, "/Bitrshift/", "L12", '#NAME?').
?test(sheet1_M12, "/Bitrshift/", "M12", '#NAME?').
?test(sheet1_N12, "/Bitrshift/", "N12", '#NAME?').
?test(sheet1_O12, "/Bitrshift/", "O12", '#NAME?').
?test(sheet1_P12, "/Bitrshift/", "P12", '#NAME?').
?test(sheet1_Q12, "/Bitrshift/", "Q12", '#NAME?').
?test(sheet1_R12, "/Bitrshift/", "R12", '#NAME?').
?test(sheet1_S12, "/Bitrshift/", "S12", '#NAME?').
?test(sheet1_T12, "/Bitrshift/", "T12", '#NAME?').
?test(sheet1_U12, "/Bitrshift/", "U12", '#NAME?').
?test(sheet1_V12, "/Bitrshift/", "V12", '#NAME?').
?test(sheet1_A13, "/Bitrshift/", "A13", "Float").
?test(sheet1_B13, "/Bitrshift/", "B13", 3.14159265358979).
?test(sheet1_C13, "/Bitrshift/", "C13", '#NAME?').
?test(sheet1_D13, "/Bitrshift/", "D13", '#NAME?').
?test(sheet1_E13, "/Bitrshift/", "E13", '#NAME?').
?test(sheet1_F13, "/Bitrshift/", "F13", '#NAME?').
?test(sheet1_G13, "/Bitrshift/", "G13", '#NAME?').
?test(sheet1_H13, "/Bitrshift/", "H13", '#NAME?').
?test(sheet1_I13, "/Bitrshift/", "I13", '#NAME?').
?test(sheet1_J13, "/Bitrshift/", "J13", '#NAME?').
?test(sheet1_K13, "/Bitrshift/", "K13", '#NAME?').
?test(sheet1_L13, "/Bitrshift/", "L13", '#NAME?').
?test(sheet1_M13, "/Bitrshift/", "M13", '#NAME?').
?test(sheet1_N13, "/Bitrshift/", "N13", '#NAME?').
?test(sheet1_O13, "/Bitrshift/", "O13", '#NAME?').
?test(sheet1_P13, "/Bitrshift/", "P13", '#NAME?').
?test(sheet1_Q13, "/Bitrshift/", "Q13", '#NAME?').
?test(sheet1_R13, "/Bitrshift/", "R13", '#NAME?').
?test(sheet1_S13, "/Bitrshift/", "S13", '#NAME?').
?test(sheet1_T13, "/Bitrshift/", "T13", '#NAME?').
?test(sheet1_U13, "/Bitrshift/", "U13", '#NAME?').
?test(sheet1_V13, "/Bitrshift/", "V13", '#NAME?').
?test(sheet1_A14, "/Bitrshift/", "A14", "Blank").
?test(sheet1_C14, "/Bitrshift/", "C14", '#NAME?').
?test(sheet1_D14, "/Bitrshift/", "D14", '#NAME?').
?test(sheet1_E14, "/Bitrshift/", "E14", '#NAME?').
?test(sheet1_F14, "/Bitrshift/", "F14", '#NAME?').
?test(sheet1_G14, "/Bitrshift/", "G14", '#NAME?').
?test(sheet1_H14, "/Bitrshift/", "H14", '#NAME?').
?test(sheet1_I14, "/Bitrshift/", "I14", '#NAME?').
?test(sheet1_J14, "/Bitrshift/", "J14", '#NAME?').
?test(sheet1_K14, "/Bitrshift/", "K14", '#NAME?').
?test(sheet1_L14, "/Bitrshift/", "L14", '#NAME?').
?test(sheet1_M14, "/Bitrshift/", "M14", '#NAME?').
?test(sheet1_N14, "/Bitrshift/", "N14", '#NAME?').
?test(sheet1_O14, "/Bitrshift/", "O14", '#NAME?').
?test(sheet1_P14, "/Bitrshift/", "P14", '#NAME?').
?test(sheet1_Q14, "/Bitrshift/", "Q14", '#NAME?').
?test(sheet1_R14, "/Bitrshift/", "R14", '#NAME?').
?test(sheet1_S14, "/Bitrshift/", "S14", '#NAME?').
?test(sheet1_T14, "/Bitrshift/", "T14", '#NAME?').
?test(sheet1_U14, "/Bitrshift/", "U14", '#NAME?').
?test(sheet1_V14, "/Bitrshift/", "V14", '#NAME?').
?test(sheet1_A15, "/Bitrshift/", "A15", "Logical").
?test(sheet1_B15, "/Bitrshift/", "B15", true).
?test(sheet1_C15, "/Bitrshift/", "C15", '#NAME?').
?test(sheet1_D15, "/Bitrshift/", "D15", '#NAME?').
?test(sheet1_E15, "/Bitrshift/", "E15", '#NAME?').
?test(sheet1_F15, "/Bitrshift/", "F15", '#NAME?').
?test(sheet1_G15, "/Bitrshift/", "G15", '#NAME?').
?test(sheet1_H15, "/Bitrshift/", "H15", '#NAME?').
?test(sheet1_I15, "/Bitrshift/", "I15", '#NAME?').
?test(sheet1_J15, "/Bitrshift/", "J15", '#NAME?').
?test(sheet1_K15, "/Bitrshift/", "K15", '#NAME?').
?test(sheet1_L15, "/Bitrshift/", "L15", '#NAME?').
?test(sheet1_M15, "/Bitrshift/", "M15", '#NAME?').
?test(sheet1_N15, "/Bitrshift/", "N15", '#NAME?').
?test(sheet1_O15, "/Bitrshift/", "O15", '#NAME?').
?test(sheet1_P15, "/Bitrshift/", "P15", '#NAME?').
?test(sheet1_Q15, "/Bitrshift/", "Q15", '#NAME?').
?test(sheet1_R15, "/Bitrshift/", "R15", '#NAME?').
?test(sheet1_S15, "/Bitrshift/", "S15", '#NAME?').
?test(sheet1_T15, "/Bitrshift/", "T15", '#NAME?').
?test(sheet1_U15, "/Bitrshift/", "U15", '#NAME?').
?test(sheet1_V15, "/Bitrshift/", "V15", '#NAME?').
?test(sheet1_A16, "/Bitrshift/", "A16", "Logical").
?test(sheet1_B16, "/Bitrshift/", "B16", false).
?test(sheet1_C16, "/Bitrshift/", "C16", '#NAME?').
?test(sheet1_D16, "/Bitrshift/", "D16", '#NAME?').
?test(sheet1_E16, "/Bitrshift/", "E16", '#NAME?').
?test(sheet1_F16, "/Bitrshift/", "F16", '#NAME?').
?test(sheet1_G16, "/Bitrshift/", "G16", '#NAME?').
?test(sheet1_H16, "/Bitrshift/", "H16", '#NAME?').
?test(sheet1_I16, "/Bitrshift/", "I16", '#NAME?').
?test(sheet1_J16, "/Bitrshift/", "J16", '#NAME?').
?test(sheet1_K16, "/Bitrshift/", "K16", '#NAME?').
?test(sheet1_L16, "/Bitrshift/", "L16", '#NAME?').
?test(sheet1_M16, "/Bitrshift/", "M16", '#NAME?').
?test(sheet1_N16, "/Bitrshift/", "N16", '#NAME?').
?test(sheet1_O16, "/Bitrshift/", "O16", '#NAME?').
?test(sheet1_P16, "/Bitrshift/", "P16", '#NAME?').
?test(sheet1_Q16, "/Bitrshift/", "Q16", '#NAME?').
?test(sheet1_R16, "/Bitrshift/", "R16", '#NAME?').
?test(sheet1_S16, "/Bitrshift/", "S16", '#NAME?').
?test(sheet1_T16, "/Bitrshift/", "T16", '#NAME?').
?test(sheet1_U16, "/Bitrshift/", "U16", '#NAME?').
?test(sheet1_V16, "/Bitrshift/", "V16", '#NAME?').
?test(sheet1_A17, "/Bitrshift/", "A17", "Range Row").
?test(sheet1_B17, "/Bitrshift/", "B17", "X3:Y3").
?test(sheet1_C17, "/Bitrshift/", "C17", '#NAME?').
?test(sheet1_D17, "/Bitrshift/", "D17", '#NAME?').
?test(sheet1_E17, "/Bitrshift/", "E17", '#NAME?').
?test(sheet1_F17, "/Bitrshift/", "F17", '#NAME?').
?test(sheet1_G17, "/Bitrshift/", "G17", '#NAME?').
?test(sheet1_H17, "/Bitrshift/", "H17", '#NAME?').
?test(sheet1_I17, "/Bitrshift/", "I17", '#NAME?').
?test(sheet1_J17, "/Bitrshift/", "J17", '#NAME?').
?test(sheet1_K17, "/Bitrshift/", "K17", '#NAME?').
?test(sheet1_L17, "/Bitrshift/", "L17", '#NAME?').
?test(sheet1_M17, "/Bitrshift/", "M17", '#NAME?').
?test(sheet1_N17, "/Bitrshift/", "N17", '#NAME?').
?test(sheet1_O17, "/Bitrshift/", "O17", '#NAME?').
?test(sheet1_P17, "/Bitrshift/", "P17", '#NAME?').
?test(sheet1_Q17, "/Bitrshift/", "Q17", '#NAME?').
?test(sheet1_R17, "/Bitrshift/", "R17", '#NAME?').
?test(sheet1_S17, "/Bitrshift/", "S17", '#NAME?').
?test(sheet1_T17, "/Bitrshift/", "T17", '#NAME?').
?test(sheet1_U17, "/Bitrshift/", "U17", '#NAME?').
?test(sheet1_V17, "/Bitrshift/", "V17", '#NAME?').
?test(sheet1_A18, "/Bitrshift/", "A18", "Range Row").
?test(sheet1_B18, "/Bitrshift/", "B18", "X3:AA3").
?test(sheet1_C18, "/Bitrshift/", "C18", '#NAME?').
?test(sheet1_D18, "/Bitrshift/", "D18", '#NAME?').
?test(sheet1_E18, "/Bitrshift/", "E18", '#NAME?').
?test(sheet1_F18, "/Bitrshift/", "F18", '#NAME?').
?test(sheet1_G18, "/Bitrshift/", "G18", '#NAME?').
?test(sheet1_H18, "/Bitrshift/", "H18", '#NAME?').
?test(sheet1_I18, "/Bitrshift/", "I18", '#NAME?').
?test(sheet1_J18, "/Bitrshift/", "J18", '#NAME?').
?test(sheet1_K18, "/Bitrshift/", "K18", '#NAME?').
?test(sheet1_L18, "/Bitrshift/", "L18", '#NAME?').
?test(sheet1_M18, "/Bitrshift/", "M18", '#NAME?').
?test(sheet1_N18, "/Bitrshift/", "N18", '#NAME?').
?test(sheet1_O18, "/Bitrshift/", "O18", '#NAME?').
?test(sheet1_P18, "/Bitrshift/", "P18", '#NAME?').
?test(sheet1_Q18, "/Bitrshift/", "Q18", '#NAME?').
?test(sheet1_R18, "/Bitrshift/", "R18", '#NAME?').
?test(sheet1_S18, "/Bitrshift/", "S18", '#NAME?').
?test(sheet1_T18, "/Bitrshift/", "T18", '#NAME?').
?test(sheet1_U18, "/Bitrshift/", "U18", '#NAME?').
?test(sheet1_V18, "/Bitrshift/", "V18", '#NAME?').
?test(sheet1_A19, "/Bitrshift/", "A19", "Range Area").
?test(sheet1_B19, "/Bitrshift/", "B19", "X3:Y4").
?test(sheet1_C19, "/Bitrshift/", "C19", '#NAME?').
?test(sheet1_D19, "/Bitrshift/", "D19", '#NAME?').
?test(sheet1_E19, "/Bitrshift/", "E19", '#NAME?').
?test(sheet1_F19, "/Bitrshift/", "F19", '#NAME?').
?test(sheet1_G19, "/Bitrshift/", "G19", '#NAME?').
?test(sheet1_H19, "/Bitrshift/", "H19", '#NAME?').
?test(sheet1_I19, "/Bitrshift/", "I19", '#NAME?').
?test(sheet1_J19, "/Bitrshift/", "J19", '#NAME?').
?test(sheet1_K19, "/Bitrshift/", "K19", '#NAME?').
?test(sheet1_L19, "/Bitrshift/", "L19", '#NAME?').
?test(sheet1_M19, "/Bitrshift/", "M19", '#NAME?').
?test(sheet1_N19, "/Bitrshift/", "N19", '#NAME?').
?test(sheet1_O19, "/Bitrshift/", "O19", '#NAME?').
?test(sheet1_P19, "/Bitrshift/", "P19", '#NAME?').
?test(sheet1_Q19, "/Bitrshift/", "Q19", '#NAME?').
?test(sheet1_R19, "/Bitrshift/", "R19", '#NAME?').
?test(sheet1_S19, "/Bitrshift/", "S19", '#NAME?').
?test(sheet1_T19, "/Bitrshift/", "T19", '#NAME?').
?test(sheet1_U19, "/Bitrshift/", "U19", '#NAME?').
?test(sheet1_V19, "/Bitrshift/", "V19", '#NAME?').
?test(sheet1_A20, "/Bitrshift/", "A20", "Range Area").
?test(sheet1_B20, "/Bitrshift/", "B20", "X3:AA6").
?test(sheet1_C20, "/Bitrshift/", "C20", '#NAME?').
?test(sheet1_D20, "/Bitrshift/", "D20", '#NAME?').
?test(sheet1_E20, "/Bitrshift/", "E20", '#NAME?').
?test(sheet1_F20, "/Bitrshift/", "F20", '#NAME?').
?test(sheet1_G20, "/Bitrshift/", "G20", '#NAME?').
?test(sheet1_H20, "/Bitrshift/", "H20", '#NAME?').
?test(sheet1_I20, "/Bitrshift/", "I20", '#NAME?').
?test(sheet1_J20, "/Bitrshift/", "J20", '#NAME?').
?test(sheet1_K20, "/Bitrshift/", "K20", '#NAME?').
?test(sheet1_L20, "/Bitrshift/", "L20", '#NAME?').
?test(sheet1_M20, "/Bitrshift/", "M20", '#NAME?').
?test(sheet1_N20, "/Bitrshift/", "N20", '#NAME?').
?test(sheet1_O20, "/Bitrshift/", "O20", '#NAME?').
?test(sheet1_P20, "/Bitrshift/", "P20", '#NAME?').
?test(sheet1_Q20, "/Bitrshift/", "Q20", '#NAME?').
?test(sheet1_R20, "/Bitrshift/", "R20", '#NAME?').
?test(sheet1_S20, "/Bitrshift/", "S20", '#NAME?').
?test(sheet1_T20, "/Bitrshift/", "T20", '#NAME?').
?test(sheet1_U20, "/Bitrshift/", "U20", '#NAME?').
?test(sheet1_V20, "/Bitrshift/", "V20", '#NAME?').
?test(sheet1_A21, "/Bitrshift/", "A21", "Range Colunm").
?test(sheet1_B21, "/Bitrshift/", "B21", "X3:X4").
?test(sheet1_C21, "/Bitrshift/", "C21", '#NAME?').
?test(sheet1_D21, "/Bitrshift/", "D21", '#NAME?').
?test(sheet1_E21, "/Bitrshift/", "E21", '#NAME?').
?test(sheet1_F21, "/Bitrshift/", "F21", '#NAME?').
?test(sheet1_G21, "/Bitrshift/", "G21", '#NAME?').
?test(sheet1_H21, "/Bitrshift/", "H21", '#NAME?').
?test(sheet1_I21, "/Bitrshift/", "I21", '#NAME?').
?test(sheet1_J21, "/Bitrshift/", "J21", '#NAME?').
?test(sheet1_K21, "/Bitrshift/", "K21", '#NAME?').
?test(sheet1_L21, "/Bitrshift/", "L21", '#NAME?').
?test(sheet1_M21, "/Bitrshift/", "M21", '#NAME?').
?test(sheet1_N21, "/Bitrshift/", "N21", '#NAME?').
?test(sheet1_O21, "/Bitrshift/", "O21", '#NAME?').
?test(sheet1_P21, "/Bitrshift/", "P21", '#NAME?').
?test(sheet1_Q21, "/Bitrshift/", "Q21", '#NAME?').
?test(sheet1_R21, "/Bitrshift/", "R21", '#NAME?').
?test(sheet1_S21, "/Bitrshift/", "S21", '#NAME?').
?test(sheet1_T21, "/Bitrshift/", "T21", '#NAME?').
?test(sheet1_U21, "/Bitrshift/", "U21", '#NAME?').
?test(sheet1_V21, "/Bitrshift/", "V21", '#NAME?').
?test(sheet1_A22, "/Bitrshift/", "A22", "Range Colunm").
?test(sheet1_B22, "/Bitrshift/", "B22", "X3:X6").
?test(sheet1_C22, "/Bitrshift/", "C22", '#NAME?').
?test(sheet1_D22, "/Bitrshift/", "D22", '#NAME?').
?test(sheet1_E22, "/Bitrshift/", "E22", '#NAME?').
?test(sheet1_F22, "/Bitrshift/", "F22", '#NAME?').
?test(sheet1_G22, "/Bitrshift/", "G22", '#NAME?').
?test(sheet1_H22, "/Bitrshift/", "H22", '#NAME?').
?test(sheet1_I22, "/Bitrshift/", "I22", '#NAME?').
?test(sheet1_J22, "/Bitrshift/", "J22", '#NAME?').
?test(sheet1_K22, "/Bitrshift/", "K22", '#NAME?').
?test(sheet1_L22, "/Bitrshift/", "L22", '#NAME?').
?test(sheet1_M22, "/Bitrshift/", "M22", '#NAME?').
?test(sheet1_N22, "/Bitrshift/", "N22", '#NAME?').
?test(sheet1_O22, "/Bitrshift/", "O22", '#NAME?').
?test(sheet1_P22, "/Bitrshift/", "P22", '#NAME?').
?test(sheet1_Q22, "/Bitrshift/", "Q22", '#NAME?').
?test(sheet1_R22, "/Bitrshift/", "R22", '#NAME?').
?test(sheet1_S22, "/Bitrshift/", "S22", '#NAME?').
?test(sheet1_T22, "/Bitrshift/", "T22", '#NAME?').
?test(sheet1_U22, "/Bitrshift/", "U22", '#NAME?').
?test(sheet1_V22, "/Bitrshift/", "V22", '#NAME?').
?test(sheet1_A25, "/Bitrshift/", "A25", "bitrshift(A,B)").
?test(sheet1_B25, "/Bitrshift/", "B25", "B").
?test(sheet1_C25, "/Bitrshift/", "C25", "errors").
?test(sheet1_D25, "/Bitrshift/", "D25", "errors").
?test(sheet1_E25, "/Bitrshift/", "E25", "errors").
?test(sheet1_F25, "/Bitrshift/", "F25", "errors").
?test(sheet1_G25, "/Bitrshift/", "G25", "errors").
?test(sheet1_H25, "/Bitrshift/", "H25", "errors").
?test(sheet1_I25, "/Bitrshift/", "I25", "String").
?test(sheet1_J25, "/Bitrshift/", "J25", "String Number").
?test(sheet1_K25, "/Bitrshift/", "K25", "String number Leading space").
?test(sheet1_L25, "/Bitrshift/", "L25", "Integer").
?test(sheet1_M25, "/Bitrshift/", "M25", "Float").
?test(sheet1_N25, "/Bitrshift/", "N25", "Blank").
?test(sheet1_O25, "/Bitrshift/", "O25", "Logical").
?test(sheet1_P25, "/Bitrshift/", "P25", "Logical").
?test(sheet1_Q25, "/Bitrshift/", "Q25", "Range Row").
?test(sheet1_R25, "/Bitrshift/", "R25", "Range Row").
?test(sheet1_S25, "/Bitrshift/", "S25", "Range Area").
?test(sheet1_T25, "/Bitrshift/", "T25", "Range Area").
?test(sheet1_U25, "/Bitrshift/", "U25", "Range Colunm").
?test(sheet1_V25, "/Bitrshift/", "V25", "Range Colunm").
?test(sheet1_A26, "/Bitrshift/", "A26", "A").
?test(sheet1_C26, "/Bitrshift/", "C26", '#DIV/0!').
?test(sheet1_D26, "/Bitrshift/", "D26", '#VALUE!').
?test(sheet1_E26, "/Bitrshift/", "E26", '#REF!').
?test(sheet1_F26, "/Bitrshift/", "F26", '#NAME?').
?test(sheet1_G26, "/Bitrshift/", "G26", '#NUM!').
?test(sheet1_H26, "/Bitrshift/", "H26", '#N/A').
?test(sheet1_I26, "/Bitrshift/", "I26", "Phillip").
?test(sheet1_J26, "/Bitrshift/", "J26", "13").
?test(sheet1_K26, "/Bitrshift/", "K26", " 24").
?test(sheet1_L26, "/Bitrshift/", "L26", "1968/03/23 00:00:00").
?test(sheet1_M26, "/Bitrshift/", "M26", 3.14159265358979).
?test(sheet1_O26, "/Bitrshift/", "O26", true).
?test(sheet1_P26, "/Bitrshift/", "P26", false).
?test(sheet1_Q26, "/Bitrshift/", "Q26", "X3:Y3").
?test(sheet1_R26, "/Bitrshift/", "R26", "X3:AA3").
?test(sheet1_S26, "/Bitrshift/", "S26", "X3:Y4").
?test(sheet1_T26, "/Bitrshift/", "T26", "X3:AA6").
?test(sheet1_U26, "/Bitrshift/", "U26", "X3:X4").
?test(sheet1_V26, "/Bitrshift/", "V26", "X3:X6").
?test(sheet1_A27, "/Bitrshift/", "A27", "errors").
?test(sheet1_B27, "/Bitrshift/", "B27", '#DIV/0!').
?test(sheet1_C27, "/Bitrshift/", "C27", '#DIV/0!').
?test(sheet1_D27, "/Bitrshift/", "D27", '#DIV/0!').
?test(sheet1_E27, "/Bitrshift/", "E27", '#DIV/0!').
?test(sheet1_F27, "/Bitrshift/", "F27", '#DIV/0!').
?test(sheet1_G27, "/Bitrshift/", "G27", '#DIV/0!').
?test(sheet1_H27, "/Bitrshift/", "H27", '#DIV/0!').
?test(sheet1_I27, "/Bitrshift/", "I27", '#DIV/0!').
?test(sheet1_J27, "/Bitrshift/", "J27", '#DIV/0!').
?test(sheet1_K27, "/Bitrshift/", "K27", '#DIV/0!').
?test(sheet1_L27, "/Bitrshift/", "L27", '#DIV/0!').
?test(sheet1_M27, "/Bitrshift/", "M27", '#DIV/0!').
?test(sheet1_N27, "/Bitrshift/", "N27", '#DIV/0!').
?test(sheet1_O27, "/Bitrshift/", "O27", '#DIV/0!').
?test(sheet1_P27, "/Bitrshift/", "P27", '#DIV/0!').
?test(sheet1_Q27, "/Bitrshift/", "Q27", '#DIV/0!').
?test(sheet1_R27, "/Bitrshift/", "R27", '#DIV/0!').
?test(sheet1_S27, "/Bitrshift/", "S27", '#DIV/0!').
?test(sheet1_T27, "/Bitrshift/", "T27", '#DIV/0!').
?test(sheet1_U27, "/Bitrshift/", "U27", '#DIV/0!').
?test(sheet1_V27, "/Bitrshift/", "V27", '#DIV/0!').
?test(sheet1_A28, "/Bitrshift/", "A28", "errors").
?test(sheet1_B28, "/Bitrshift/", "B28", '#VALUE!').
?test(sheet1_C28, "/Bitrshift/", "C28", '#VALUE!').
?test(sheet1_D28, "/Bitrshift/", "D28", '#VALUE!').
?test(sheet1_E28, "/Bitrshift/", "E28", '#VALUE!').
?test(sheet1_F28, "/Bitrshift/", "F28", '#VALUE!').
?test(sheet1_G28, "/Bitrshift/", "G28", '#VALUE!').
?test(sheet1_H28, "/Bitrshift/", "H28", '#VALUE!').
?test(sheet1_I28, "/Bitrshift/", "I28", '#VALUE!').
?test(sheet1_J28, "/Bitrshift/", "J28", '#VALUE!').
?test(sheet1_K28, "/Bitrshift/", "K28", '#VALUE!').
?test(sheet1_L28, "/Bitrshift/", "L28", '#VALUE!').
?test(sheet1_M28, "/Bitrshift/", "M28", '#VALUE!').
?test(sheet1_N28, "/Bitrshift/", "N28", '#VALUE!').
?test(sheet1_O28, "/Bitrshift/", "O28", '#VALUE!').
?test(sheet1_P28, "/Bitrshift/", "P28", '#VALUE!').
?test(sheet1_Q28, "/Bitrshift/", "Q28", '#VALUE!').
?test(sheet1_R28, "/Bitrshift/", "R28", '#VALUE!').
?test(sheet1_S28, "/Bitrshift/", "S28", '#VALUE!').
?test(sheet1_T28, "/Bitrshift/", "T28", '#VALUE!').
?test(sheet1_U28, "/Bitrshift/", "U28", '#VALUE!').
?test(sheet1_V28, "/Bitrshift/", "V28", '#VALUE!').
?test(sheet1_A29, "/Bitrshift/", "A29", "errors").
?test(sheet1_B29, "/Bitrshift/", "B29", '#REF!').
?test(sheet1_C29, "/Bitrshift/", "C29", '#REF!').
?test(sheet1_D29, "/Bitrshift/", "D29", '#REF!').
?test(sheet1_E29, "/Bitrshift/", "E29", '#REF!').
?test(sheet1_F29, "/Bitrshift/", "F29", '#REF!').
?test(sheet1_G29, "/Bitrshift/", "G29", '#REF!').
?test(sheet1_H29, "/Bitrshift/", "H29", '#REF!').
?test(sheet1_I29, "/Bitrshift/", "I29", '#REF!').
?test(sheet1_J29, "/Bitrshift/", "J29", '#REF!').
?test(sheet1_K29, "/Bitrshift/", "K29", '#REF!').
?test(sheet1_L29, "/Bitrshift/", "L29", '#REF!').
?test(sheet1_M29, "/Bitrshift/", "M29", '#REF!').
?test(sheet1_N29, "/Bitrshift/", "N29", '#REF!').
?test(sheet1_O29, "/Bitrshift/", "O29", '#REF!').
?test(sheet1_P29, "/Bitrshift/", "P29", '#REF!').
?test(sheet1_Q29, "/Bitrshift/", "Q29", '#REF!').
?test(sheet1_R29, "/Bitrshift/", "R29", '#REF!').
?test(sheet1_S29, "/Bitrshift/", "S29", '#REF!').
?test(sheet1_T29, "/Bitrshift/", "T29", '#REF!').
?test(sheet1_U29, "/Bitrshift/", "U29", '#REF!').
?test(sheet1_V29, "/Bitrshift/", "V29", '#REF!').
?test(sheet1_A30, "/Bitrshift/", "A30", "errors").
?test(sheet1_B30, "/Bitrshift/", "B30", '#NAME?').
?test(sheet1_C30, "/Bitrshift/", "C30", '#NAME?').
?test(sheet1_D30, "/Bitrshift/", "D30", '#NAME?').
?test(sheet1_E30, "/Bitrshift/", "E30", '#NAME?').
?test(sheet1_F30, "/Bitrshift/", "F30", '#NAME?').
?test(sheet1_G30, "/Bitrshift/", "G30", '#NAME?').
?test(sheet1_H30, "/Bitrshift/", "H30", '#NAME?').
?test(sheet1_I30, "/Bitrshift/", "I30", '#NAME?').
?test(sheet1_J30, "/Bitrshift/", "J30", '#NAME?').
?test(sheet1_K30, "/Bitrshift/", "K30", '#NAME?').
?test(sheet1_L30, "/Bitrshift/", "L30", '#NAME?').
?test(sheet1_M30, "/Bitrshift/", "M30", '#NAME?').
?test(sheet1_N30, "/Bitrshift/", "N30", '#NAME?').
?test(sheet1_O30, "/Bitrshift/", "O30", '#NAME?').
?test(sheet1_P30, "/Bitrshift/", "P30", '#NAME?').
?test(sheet1_Q30, "/Bitrshift/", "Q30", '#NAME?').
?test(sheet1_R30, "/Bitrshift/", "R30", '#NAME?').
?test(sheet1_S30, "/Bitrshift/", "S30", '#NAME?').
?test(sheet1_T30, "/Bitrshift/", "T30", '#NAME?').
?test(sheet1_U30, "/Bitrshift/", "U30", '#NAME?').
?test(sheet1_V30, "/Bitrshift/", "V30", '#NAME?').
?test(sheet1_A31, "/Bitrshift/", "A31", "errors").
?test(sheet1_B31, "/Bitrshift/", "B31", '#NUM!').
?test(sheet1_C31, "/Bitrshift/", "C31", '#NUM!').
?test(sheet1_D31, "/Bitrshift/", "D31", '#NUM!').
?test(sheet1_E31, "/Bitrshift/", "E31", '#NUM!').
?test(sheet1_F31, "/Bitrshift/", "F31", '#NUM!').
?test(sheet1_G31, "/Bitrshift/", "G31", '#NUM!').
?test(sheet1_H31, "/Bitrshift/", "H31", '#NUM!').
?test(sheet1_I31, "/Bitrshift/", "I31", '#NUM!').
?test(sheet1_J31, "/Bitrshift/", "J31", '#NUM!').
?test(sheet1_K31, "/Bitrshift/", "K31", '#NUM!').
?test(sheet1_L31, "/Bitrshift/", "L31", '#NUM!').
?test(sheet1_M31, "/Bitrshift/", "M31", '#NUM!').
?test(sheet1_N31, "/Bitrshift/", "N31", '#NUM!').
?test(sheet1_O31, "/Bitrshift/", "O31", '#NUM!').
?test(sheet1_P31, "/Bitrshift/", "P31", '#NUM!').
?test(sheet1_Q31, "/Bitrshift/", "Q31", '#NUM!').
?test(sheet1_R31, "/Bitrshift/", "R31", '#NUM!').
?test(sheet1_S31, "/Bitrshift/", "S31", '#NUM!').
?test(sheet1_T31, "/Bitrshift/", "T31", '#NUM!').
?test(sheet1_U31, "/Bitrshift/", "U31", '#NUM!').
?test(sheet1_V31, "/Bitrshift/", "V31", '#NUM!').
?test(sheet1_A32, "/Bitrshift/", "A32", "errors").
?test(sheet1_B32, "/Bitrshift/", "B32", '#N/A').
?test(sheet1_C32, "/Bitrshift/", "C32", '#N/A').
?test(sheet1_D32, "/Bitrshift/", "D32", '#N/A').
?test(sheet1_E32, "/Bitrshift/", "E32", '#N/A').
?test(sheet1_F32, "/Bitrshift/", "F32", '#N/A').
?test(sheet1_G32, "/Bitrshift/", "G32", '#N/A').
?test(sheet1_H32, "/Bitrshift/", "H32", '#N/A').
?test(sheet1_I32, "/Bitrshift/", "I32", '#N/A').
?test(sheet1_J32, "/Bitrshift/", "J32", '#N/A').
?test(sheet1_K32, "/Bitrshift/", "K32", '#N/A').
?test(sheet1_L32, "/Bitrshift/", "L32", '#N/A').
?test(sheet1_M32, "/Bitrshift/", "M32", '#N/A').
?test(sheet1_N32, "/Bitrshift/", "N32", '#N/A').
?test(sheet1_O32, "/Bitrshift/", "O32", '#N/A').
?test(sheet1_P32, "/Bitrshift/", "P32", '#N/A').
?test(sheet1_Q32, "/Bitrshift/", "Q32", '#N/A').
?test(sheet1_R32, "/Bitrshift/", "R32", '#N/A').
?test(sheet1_S32, "/Bitrshift/", "S32", '#N/A').
?test(sheet1_T32, "/Bitrshift/", "T32", '#N/A').
?test(sheet1_U32, "/Bitrshift/", "U32", '#N/A').
?test(sheet1_V32, "/Bitrshift/", "V32", '#N/A').
?test(sheet1_A33, "/Bitrshift/", "A33", "String").
?test(sheet1_B33, "/Bitrshift/", "B33", "Phillip").
?test(sheet1_C33, "/Bitrshift/", "C33", '#VALUE!').
?test(sheet1_D33, "/Bitrshift/", "D33", '#VALUE!').
?test(sheet1_E33, "/Bitrshift/", "E33", '#VALUE!').
?test(sheet1_F33, "/Bitrshift/", "F33", '#VALUE!').
?test(sheet1_G33, "/Bitrshift/", "G33", '#VALUE!').
?test(sheet1_H33, "/Bitrshift/", "H33", '#VALUE!').
?test(sheet1_I33, "/Bitrshift/", "I33", '#VALUE!').
?test(sheet1_J33, "/Bitrshift/", "J33", '#VALUE!').
?test(sheet1_K33, "/Bitrshift/", "K33", '#VALUE!').
?test(sheet1_L33, "/Bitrshift/", "L33", '#VALUE!').
?test(sheet1_M33, "/Bitrshift/", "M33", '#VALUE!').
?test(sheet1_N33, "/Bitrshift/", "N33", '#VALUE!').
?test(sheet1_O33, "/Bitrshift/", "O33", '#VALUE!').
?test(sheet1_P33, "/Bitrshift/", "P33", '#VALUE!').
?test(sheet1_Q33, "/Bitrshift/", "Q33", '#VALUE!').
?test(sheet1_R33, "/Bitrshift/", "R33", '#VALUE!').
?test(sheet1_S33, "/Bitrshift/", "S33", '#VALUE!').
?test(sheet1_T33, "/Bitrshift/", "T33", '#VALUE!').
?test(sheet1_U33, "/Bitrshift/", "U33", '#VALUE!').
?test(sheet1_V33, "/Bitrshift/", "V33", '#VALUE!').
?test(sheet1_A34, "/Bitrshift/", "A34", "String Number").
?test(sheet1_B34, "/Bitrshift/", "B34", "12").
?test(sheet1_C34, "/Bitrshift/", "C34", '#DIV/0!').
?test(sheet1_D34, "/Bitrshift/", "D34", '#VALUE!').
?test(sheet1_E34, "/Bitrshift/", "E34", '#REF!').
?test(sheet1_F34, "/Bitrshift/", "F34", '#NAME?').
?test(sheet1_G34, "/Bitrshift/", "G34", '#NUM!').
?test(sheet1_H34, "/Bitrshift/", "H34", '#N/A').
?test(sheet1_I34, "/Bitrshift/", "I34", '#VALUE!').
?test(sheet1_J34, "/Bitrshift/", "J34", 0.0).
?test(sheet1_K34, "/Bitrshift/", "K34", 0.0).
?test(sheet1_L34, "/Bitrshift/", "L34", 0.0).
?test(sheet1_M34, "/Bitrshift/", "M34", 1.0).
?test(sheet1_N34, "/Bitrshift/", "N34", 12.0).
?test(sheet1_O34, "/Bitrshift/", "O34", 6.0).
?test(sheet1_P34, "/Bitrshift/", "P34", 12.0).
?test(sheet1_Q34, "/Bitrshift/", "Q34", '#VALUE!').
?test(sheet1_R34, "/Bitrshift/", "R34", '#VALUE!').
?test(sheet1_S34, "/Bitrshift/", "S34", '#VALUE!').
?test(sheet1_T34, "/Bitrshift/", "T34", '#VALUE!').
?test(sheet1_U34, "/Bitrshift/", "U34", '#VALUE!').
?test(sheet1_V34, "/Bitrshift/", "V34", '#VALUE!').
?test(sheet1_A35, "/Bitrshift/", "A35", "String Number Leading space").
?test(sheet1_B35, "/Bitrshift/", "B35", " 23").
?test(sheet1_C35, "/Bitrshift/", "C35", '#DIV/0!').
?test(sheet1_D35, "/Bitrshift/", "D35", '#VALUE!').
?test(sheet1_E35, "/Bitrshift/", "E35", '#REF!').
?test(sheet1_F35, "/Bitrshift/", "F35", '#NAME?').
?test(sheet1_G35, "/Bitrshift/", "G35", '#NUM!').
?test(sheet1_H35, "/Bitrshift/", "H35", '#N/A').
?test(sheet1_I35, "/Bitrshift/", "I35", '#VALUE!').
?test(sheet1_J35, "/Bitrshift/", "J35", 0.0).
?test(sheet1_K35, "/Bitrshift/", "K35", 0.0).
?test(sheet1_L35, "/Bitrshift/", "L35", 0.0).
?test(sheet1_M35, "/Bitrshift/", "M35", 2.0).
?test(sheet1_N35, "/Bitrshift/", "N35", 23.0).
?test(sheet1_O35, "/Bitrshift/", "O35", 11.0).
?test(sheet1_P35, "/Bitrshift/", "P35", 23.0).
?test(sheet1_Q35, "/Bitrshift/", "Q35", '#VALUE!').
?test(sheet1_R35, "/Bitrshift/", "R35", '#VALUE!').
?test(sheet1_S35, "/Bitrshift/", "S35", '#VALUE!').
?test(sheet1_T35, "/Bitrshift/", "T35", '#VALUE!').
?test(sheet1_U35, "/Bitrshift/", "U35", '#VALUE!').
?test(sheet1_V35, "/Bitrshift/", "V35", '#VALUE!').
?test(sheet1_A36, "/Bitrshift/", "A36", "Interger").
?test(sheet1_B36, "/Bitrshift/", "B36", "1968/03/23 00:00:00").
?test(sheet1_C36, "/Bitrshift/", "C36", '#DIV/0!').
?test(sheet1_D36, "/Bitrshift/", "D36", '#VALUE!').
?test(sheet1_E36, "/Bitrshift/", "E36", '#REF!').
?test(sheet1_F36, "/Bitrshift/", "F36", '#NAME?').
?test(sheet1_G36, "/Bitrshift/", "G36", '#NUM!').
?test(sheet1_H36, "/Bitrshift/", "H36", '#N/A').
?test(sheet1_I36, "/Bitrshift/", "I36", '#VALUE!').
?test(sheet1_J36, "/Bitrshift/", "J36", 3.0).
?test(sheet1_K36, "/Bitrshift/", "K36", 0.0).
?test(sheet1_L36, "/Bitrshift/", "L36", 0.0).
?test(sheet1_M36, "/Bitrshift/", "M36", 3115.0).
?test(sheet1_N36, "/Bitrshift/", "N36", 24920.0).
?test(sheet1_O36, "/Bitrshift/", "O36", 12460.0).
?test(sheet1_P36, "/Bitrshift/", "P36", 24920.0).
?test(sheet1_Q36, "/Bitrshift/", "Q36", '#VALUE!').
?test(sheet1_R36, "/Bitrshift/", "R36", '#VALUE!').
?test(sheet1_S36, "/Bitrshift/", "S36", '#VALUE!').
?test(sheet1_T36, "/Bitrshift/", "T36", '#VALUE!').
?test(sheet1_U36, "/Bitrshift/", "U36", '#VALUE!').
?test(sheet1_V36, "/Bitrshift/", "V36", '#VALUE!').
?test(sheet1_A37, "/Bitrshift/", "A37", "Float").
?test(sheet1_B37, "/Bitrshift/", "B37", 3.14159265358979).
?test(sheet1_C37, "/Bitrshift/", "C37", '#DIV/0!').
?test(sheet1_D37, "/Bitrshift/", "D37", '#VALUE!').
?test(sheet1_E37, "/Bitrshift/", "E37", '#REF!').
?test(sheet1_F37, "/Bitrshift/", "F37", '#NAME?').
?test(sheet1_G37, "/Bitrshift/", "G37", '#NUM!').
?test(sheet1_H37, "/Bitrshift/", "H37", '#N/A').
?test(sheet1_I37, "/Bitrshift/", "I37", '#VALUE!').
?test(sheet1_J37, "/Bitrshift/", "J37", 0.0).
?test(sheet1_K37, "/Bitrshift/", "K37", 0.0).
?test(sheet1_L37, "/Bitrshift/", "L37", 0.0).
?test(sheet1_M37, "/Bitrshift/", "M37", 0.0).
?test(sheet1_N37, "/Bitrshift/", "N37", 3.0).
?test(sheet1_O37, "/Bitrshift/", "O37", 1.0).
?test(sheet1_P37, "/Bitrshift/", "P37", 3.0).
?test(sheet1_Q37, "/Bitrshift/", "Q37", '#VALUE!').
?test(sheet1_R37, "/Bitrshift/", "R37", '#VALUE!').
?test(sheet1_S37, "/Bitrshift/", "S37", '#VALUE!').
?test(sheet1_T37, "/Bitrshift/", "T37", '#VALUE!').
?test(sheet1_U37, "/Bitrshift/", "U37", '#VALUE!').
?test(sheet1_V37, "/Bitrshift/", "V37", '#VALUE!').
?test(sheet1_A38, "/Bitrshift/", "A38", "Blank").
?test(sheet1_C38, "/Bitrshift/", "C38", '#DIV/0!').
?test(sheet1_D38, "/Bitrshift/", "D38", '#VALUE!').
?test(sheet1_E38, "/Bitrshift/", "E38", '#REF!').
?test(sheet1_F38, "/Bitrshift/", "F38", '#NAME?').
?test(sheet1_G38, "/Bitrshift/", "G38", '#NUM!').
?test(sheet1_H38, "/Bitrshift/", "H38", '#N/A').
?test(sheet1_I38, "/Bitrshift/", "I38", '#VALUE!').
?test(sheet1_J38, "/Bitrshift/", "J38", 0.0).
?test(sheet1_K38, "/Bitrshift/", "K38", 0.0).
?test(sheet1_L38, "/Bitrshift/", "L38", 0.0).
?test(sheet1_M38, "/Bitrshift/", "M38", 0.0).
?test(sheet1_N38, "/Bitrshift/", "N38", 0.0).
?test(sheet1_O38, "/Bitrshift/", "O38", 0.0).
?test(sheet1_P38, "/Bitrshift/", "P38", 0.0).
?test(sheet1_Q38, "/Bitrshift/", "Q38", '#VALUE!').
?test(sheet1_R38, "/Bitrshift/", "R38", '#VALUE!').
?test(sheet1_S38, "/Bitrshift/", "S38", '#VALUE!').
?test(sheet1_T38, "/Bitrshift/", "T38", '#VALUE!').
?test(sheet1_U38, "/Bitrshift/", "U38", '#VALUE!').
?test(sheet1_V38, "/Bitrshift/", "V38", '#VALUE!').
?test(sheet1_A39, "/Bitrshift/", "A39", "Logical").
?test(sheet1_B39, "/Bitrshift/", "B39", true).
?test(sheet1_C39, "/Bitrshift/", "C39", '#DIV/0!').
?test(sheet1_D39, "/Bitrshift/", "D39", '#VALUE!').
?test(sheet1_E39, "/Bitrshift/", "E39", '#REF!').
?test(sheet1_F39, "/Bitrshift/", "F39", '#NAME?').
?test(sheet1_G39, "/Bitrshift/", "G39", '#NUM!').
?test(sheet1_H39, "/Bitrshift/", "H39", '#N/A').
?test(sheet1_I39, "/Bitrshift/", "I39", '#VALUE!').
?test(sheet1_J39, "/Bitrshift/", "J39", 0.0).
?test(sheet1_K39, "/Bitrshift/", "K39", 0.0).
?test(sheet1_L39, "/Bitrshift/", "L39", 0.0).
?test(sheet1_M39, "/Bitrshift/", "M39", 0.0).
?test(sheet1_N39, "/Bitrshift/", "N39", 1.0).
?test(sheet1_O39, "/Bitrshift/", "O39", 0.0).
?test(sheet1_P39, "/Bitrshift/", "P39", 1.0).
?test(sheet1_Q39, "/Bitrshift/", "Q39", '#VALUE!').
?test(sheet1_R39, "/Bitrshift/", "R39", '#VALUE!').
?test(sheet1_S39, "/Bitrshift/", "S39", '#VALUE!').
?test(sheet1_T39, "/Bitrshift/", "T39", '#VALUE!').
?test(sheet1_U39, "/Bitrshift/", "U39", '#VALUE!').
?test(sheet1_V39, "/Bitrshift/", "V39", '#VALUE!').
?test(sheet1_A40, "/Bitrshift/", "A40", "Logical").
?test(sheet1_B40, "/Bitrshift/", "B40", false).
?test(sheet1_C40, "/Bitrshift/", "C40", '#DIV/0!').
?test(sheet1_D40, "/Bitrshift/", "D40", '#VALUE!').
?test(sheet1_E40, "/Bitrshift/", "E40", '#REF!').
?test(sheet1_F40, "/Bitrshift/", "F40", '#NAME?').
?test(sheet1_G40, "/Bitrshift/", "G40", '#NUM!').
?test(sheet1_H40, "/Bitrshift/", "H40", '#N/A').
?test(sheet1_I40, "/Bitrshift/", "I40", '#VALUE!').
?test(sheet1_J40, "/Bitrshift/", "J40", 0.0).
?test(sheet1_K40, "/Bitrshift/", "K40", 0.0).
?test(sheet1_L40, "/Bitrshift/", "L40", 0.0).
?test(sheet1_M40, "/Bitrshift/", "M40", 0.0).
?test(sheet1_N40, "/Bitrshift/", "N40", 0.0).
?test(sheet1_O40, "/Bitrshift/", "O40", 0.0).
?test(sheet1_P40, "/Bitrshift/", "P40", 0.0).
?test(sheet1_Q40, "/Bitrshift/", "Q40", '#VALUE!').
?test(sheet1_R40, "/Bitrshift/", "R40", '#VALUE!').
?test(sheet1_S40, "/Bitrshift/", "S40", '#VALUE!').
?test(sheet1_T40, "/Bitrshift/", "T40", '#VALUE!').
?test(sheet1_U40, "/Bitrshift/", "U40", '#VALUE!').
?test(sheet1_V40, "/Bitrshift/", "V40", '#VALUE!').
?test(sheet1_A41, "/Bitrshift/", "A41", "Range Row").
?test(sheet1_B41, "/Bitrshift/", "B41", "X3:Y3").
?test(sheet1_C41, "/Bitrshift/", "C41", '#VALUE!').
?test(sheet1_D41, "/Bitrshift/", "D41", '#VALUE!').
?test(sheet1_E41, "/Bitrshift/", "E41", '#VALUE!').
?test(sheet1_F41, "/Bitrshift/", "F41", '#VALUE!').
?test(sheet1_G41, "/Bitrshift/", "G41", '#VALUE!').
?test(sheet1_H41, "/Bitrshift/", "H41", '#VALUE!').
?test(sheet1_I41, "/Bitrshift/", "I41", '#VALUE!').
?test(sheet1_J41, "/Bitrshift/", "J41", '#VALUE!').
?test(sheet1_K41, "/Bitrshift/", "K41", '#VALUE!').
?test(sheet1_L41, "/Bitrshift/", "L41", '#VALUE!').
?test(sheet1_M41, "/Bitrshift/", "M41", '#VALUE!').
?test(sheet1_N41, "/Bitrshift/", "N41", '#VALUE!').
?test(sheet1_O41, "/Bitrshift/", "O41", '#VALUE!').
?test(sheet1_P41, "/Bitrshift/", "P41", '#VALUE!').
?test(sheet1_Q41, "/Bitrshift/", "Q41", '#VALUE!').
?test(sheet1_R41, "/Bitrshift/", "R41", '#VALUE!').
?test(sheet1_S41, "/Bitrshift/", "S41", '#VALUE!').
?test(sheet1_T41, "/Bitrshift/", "T41", '#VALUE!').
?test(sheet1_U41, "/Bitrshift/", "U41", '#VALUE!').
?test(sheet1_V41, "/Bitrshift/", "V41", '#VALUE!').
?test(sheet1_A42, "/Bitrshift/", "A42", "Range Row").
?test(sheet1_B42, "/Bitrshift/", "B42", "X3:AA3").
?test(sheet1_C42, "/Bitrshift/", "C42", '#VALUE!').
?test(sheet1_D42, "/Bitrshift/", "D42", '#VALUE!').
?test(sheet1_E42, "/Bitrshift/", "E42", '#VALUE!').
?test(sheet1_F42, "/Bitrshift/", "F42", '#VALUE!').
?test(sheet1_G42, "/Bitrshift/", "G42", '#VALUE!').
?test(sheet1_H42, "/Bitrshift/", "H42", '#VALUE!').
?test(sheet1_I42, "/Bitrshift/", "I42", '#VALUE!').
?test(sheet1_J42, "/Bitrshift/", "J42", '#VALUE!').
?test(sheet1_K42, "/Bitrshift/", "K42", '#VALUE!').
?test(sheet1_L42, "/Bitrshift/", "L42", '#VALUE!').
?test(sheet1_M42, "/Bitrshift/", "M42", '#VALUE!').
?test(sheet1_N42, "/Bitrshift/", "N42", '#VALUE!').
?test(sheet1_O42, "/Bitrshift/", "O42", '#VALUE!').
?test(sheet1_P42, "/Bitrshift/", "P42", '#VALUE!').
?test(sheet1_Q42, "/Bitrshift/", "Q42", '#VALUE!').
?test(sheet1_R42, "/Bitrshift/", "R42", '#VALUE!').
?test(sheet1_S42, "/Bitrshift/", "S42", '#VALUE!').
?test(sheet1_T42, "/Bitrshift/", "T42", '#VALUE!').
?test(sheet1_U42, "/Bitrshift/", "U42", '#VALUE!').
?test(sheet1_V42, "/Bitrshift/", "V42", '#VALUE!').
?test(sheet1_A43, "/Bitrshift/", "A43", "Range Area").
?test(sheet1_B43, "/Bitrshift/", "B43", "X3:Y4").
?test(sheet1_C43, "/Bitrshift/", "C43", '#VALUE!').
?test(sheet1_D43, "/Bitrshift/", "D43", '#VALUE!').
?test(sheet1_E43, "/Bitrshift/", "E43", '#VALUE!').
?test(sheet1_F43, "/Bitrshift/", "F43", '#VALUE!').
?test(sheet1_G43, "/Bitrshift/", "G43", '#VALUE!').
?test(sheet1_H43, "/Bitrshift/", "H43", '#VALUE!').
?test(sheet1_I43, "/Bitrshift/", "I43", '#VALUE!').
?test(sheet1_J43, "/Bitrshift/", "J43", '#VALUE!').
?test(sheet1_K43, "/Bitrshift/", "K43", '#VALUE!').
?test(sheet1_L43, "/Bitrshift/", "L43", '#VALUE!').
?test(sheet1_M43, "/Bitrshift/", "M43", '#VALUE!').
?test(sheet1_N43, "/Bitrshift/", "N43", '#VALUE!').
?test(sheet1_O43, "/Bitrshift/", "O43", '#VALUE!').
?test(sheet1_P43, "/Bitrshift/", "P43", '#VALUE!').
?test(sheet1_Q43, "/Bitrshift/", "Q43", '#VALUE!').
?test(sheet1_R43, "/Bitrshift/", "R43", '#VALUE!').
?test(sheet1_S43, "/Bitrshift/", "S43", '#VALUE!').
?test(sheet1_T43, "/Bitrshift/", "T43", '#VALUE!').
?test(sheet1_U43, "/Bitrshift/", "U43", '#VALUE!').
?test(sheet1_V43, "/Bitrshift/", "V43", '#VALUE!').
?test(sheet1_A44, "/Bitrshift/", "A44", "Range Area").
?test(sheet1_B44, "/Bitrshift/", "B44", "X3:AA6").
?test(sheet1_C44, "/Bitrshift/", "C44", '#VALUE!').
?test(sheet1_D44, "/Bitrshift/", "D44", '#VALUE!').
?test(sheet1_E44, "/Bitrshift/", "E44", '#VALUE!').
?test(sheet1_F44, "/Bitrshift/", "F44", '#VALUE!').
?test(sheet1_G44, "/Bitrshift/", "G44", '#VALUE!').
?test(sheet1_H44, "/Bitrshift/", "H44", '#VALUE!').
?test(sheet1_I44, "/Bitrshift/", "I44", '#VALUE!').
?test(sheet1_J44, "/Bitrshift/", "J44", '#VALUE!').
?test(sheet1_K44, "/Bitrshift/", "K44", '#VALUE!').
?test(sheet1_L44, "/Bitrshift/", "L44", '#VALUE!').
?test(sheet1_M44, "/Bitrshift/", "M44", '#VALUE!').
?test(sheet1_N44, "/Bitrshift/", "N44", '#VALUE!').
?test(sheet1_O44, "/Bitrshift/", "O44", '#VALUE!').
?test(sheet1_P44, "/Bitrshift/", "P44", '#VALUE!').
?test(sheet1_Q44, "/Bitrshift/", "Q44", '#VALUE!').
?test(sheet1_R44, "/Bitrshift/", "R44", '#VALUE!').
?test(sheet1_S44, "/Bitrshift/", "S44", '#VALUE!').
?test(sheet1_T44, "/Bitrshift/", "T44", '#VALUE!').
?test(sheet1_U44, "/Bitrshift/", "U44", '#VALUE!').
?test(sheet1_V44, "/Bitrshift/", "V44", '#VALUE!').
?test(sheet1_A45, "/Bitrshift/", "A45", "Range Colunm").
?test(sheet1_B45, "/Bitrshift/", "B45", "X3:X4").
?test(sheet1_C45, "/Bitrshift/", "C45", '#VALUE!').
?test(sheet1_D45, "/Bitrshift/", "D45", '#VALUE!').
?test(sheet1_E45, "/Bitrshift/", "E45", '#VALUE!').
?test(sheet1_F45, "/Bitrshift/", "F45", '#VALUE!').
?test(sheet1_G45, "/Bitrshift/", "G45", '#VALUE!').
?test(sheet1_H45, "/Bitrshift/", "H45", '#VALUE!').
?test(sheet1_I45, "/Bitrshift/", "I45", '#VALUE!').
?test(sheet1_J45, "/Bitrshift/", "J45", '#VALUE!').
?test(sheet1_K45, "/Bitrshift/", "K45", '#VALUE!').
?test(sheet1_L45, "/Bitrshift/", "L45", '#VALUE!').
?test(sheet1_M45, "/Bitrshift/", "M45", '#VALUE!').
?test(sheet1_N45, "/Bitrshift/", "N45", '#VALUE!').
?test(sheet1_O45, "/Bitrshift/", "O45", '#VALUE!').
?test(sheet1_P45, "/Bitrshift/", "P45", '#VALUE!').
?test(sheet1_Q45, "/Bitrshift/", "Q45", '#VALUE!').
?test(sheet1_R45, "/Bitrshift/", "R45", '#VALUE!').
?test(sheet1_S45, "/Bitrshift/", "S45", '#VALUE!').
?test(sheet1_T45, "/Bitrshift/", "T45", '#VALUE!').
?test(sheet1_U45, "/Bitrshift/", "U45", '#VALUE!').
?test(sheet1_V45, "/Bitrshift/", "V45", '#VALUE!').
?test(sheet1_A46, "/Bitrshift/", "A46", "Range Colunm").
?test(sheet1_B46, "/Bitrshift/", "B46", "X3:X6").
?test(sheet1_C46, "/Bitrshift/", "C46", '#VALUE!').
?test(sheet1_D46, "/Bitrshift/", "D46", '#VALUE!').
?test(sheet1_E46, "/Bitrshift/", "E46", '#VALUE!').
?test(sheet1_F46, "/Bitrshift/", "F46", '#VALUE!').
?test(sheet1_G46, "/Bitrshift/", "G46", '#VALUE!').
?test(sheet1_H46, "/Bitrshift/", "H46", '#VALUE!').
?test(sheet1_I46, "/Bitrshift/", "I46", '#VALUE!').
?test(sheet1_J46, "/Bitrshift/", "J46", '#VALUE!').
?test(sheet1_K46, "/Bitrshift/", "K46", '#VALUE!').
?test(sheet1_L46, "/Bitrshift/", "L46", '#VALUE!').
?test(sheet1_M46, "/Bitrshift/", "M46", '#VALUE!').
?test(sheet1_N46, "/Bitrshift/", "N46", '#VALUE!').
?test(sheet1_O46, "/Bitrshift/", "O46", '#VALUE!').
?test(sheet1_P46, "/Bitrshift/", "P46", '#VALUE!').
?test(sheet1_Q46, "/Bitrshift/", "Q46", '#VALUE!').
?test(sheet1_R46, "/Bitrshift/", "R46", '#VALUE!').
?test(sheet1_S46, "/Bitrshift/", "S46", '#VALUE!').
?test(sheet1_T46, "/Bitrshift/", "T46", '#VALUE!').
?test(sheet1_U46, "/Bitrshift/", "U46", '#VALUE!').
?test(sheet1_V46, "/Bitrshift/", "V46", '#VALUE!').
?test(sheet1_A49, "/Bitrshift/", "A49", 320.0).
?test(sheet1_C49, "/Bitrshift/", "C49", 0.0).
?test(sheet1_D49, "/Bitrshift/", "D49", 0.0).
?test(sheet1_E49, "/Bitrshift/", "E49", 0.0).
?test(sheet1_F49, "/Bitrshift/", "F49", 0.0).
?test(sheet1_G49, "/Bitrshift/", "G49", 0.0).
?test(sheet1_H49, "/Bitrshift/", "H49", 0.0).
?test(sheet1_I49, "/Bitrshift/", "I49", 0.0).
?test(sheet1_J49, "/Bitrshift/", "J49", 0.0).
?test(sheet1_K49, "/Bitrshift/", "K49", 0.0).
?test(sheet1_L49, "/Bitrshift/", "L49", 0.0).
?test(sheet1_M49, "/Bitrshift/", "M49", 0.0).
?test(sheet1_N49, "/Bitrshift/", "N49", 0.0).
?test(sheet1_O49, "/Bitrshift/", "O49", 0.0).
?test(sheet1_P49, "/Bitrshift/", "P49", 0.0).
?test(sheet1_Q49, "/Bitrshift/", "Q49", 0.0).
?test(sheet1_R49, "/Bitrshift/", "R49", 0.0).
?test(sheet1_S49, "/Bitrshift/", "S49", 0.0).
?test(sheet1_T49, "/Bitrshift/", "T49", 0.0).
?test(sheet1_U49, "/Bitrshift/", "U49", 0.0).
?test(sheet1_V49, "/Bitrshift/", "V49", 0.0).
?test(sheet1_A50, "/Bitrshift/", "A50", 7.0).
?test(sheet1_C50, "/Bitrshift/", "C50", 0.0).
?test(sheet1_D50, "/Bitrshift/", "D50", 0.0).
?test(sheet1_E50, "/Bitrshift/", "E50", 0.0).
?test(sheet1_F50, "/Bitrshift/", "F50", 0.0).
?test(sheet1_G50, "/Bitrshift/", "G50", 0.0).
?test(sheet1_H50, "/Bitrshift/", "H50", 0.0).
?test(sheet1_I50, "/Bitrshift/", "I50", 0.0).
?test(sheet1_J50, "/Bitrshift/", "J50", 0.0).
?test(sheet1_K50, "/Bitrshift/", "K50", 0.0).
?test(sheet1_L50, "/Bitrshift/", "L50", 0.0).
?test(sheet1_M50, "/Bitrshift/", "M50", 0.0).
?test(sheet1_N50, "/Bitrshift/", "N50", 0.0).
?test(sheet1_O50, "/Bitrshift/", "O50", 0.0).
?test(sheet1_P50, "/Bitrshift/", "P50", 0.0).
?test(sheet1_Q50, "/Bitrshift/", "Q50", 0.0).
?test(sheet1_R50, "/Bitrshift/", "R50", 0.0).
?test(sheet1_S50, "/Bitrshift/", "S50", 0.0).
?test(sheet1_T50, "/Bitrshift/", "T50", 0.0).
?test(sheet1_U50, "/Bitrshift/", "U50", 0.0).
?test(sheet1_V50, "/Bitrshift/", "V50", 0.0).
?test(sheet1_C51, "/Bitrshift/", "C51", 0.0).
?test(sheet1_D51, "/Bitrshift/", "D51", 0.0).
?test(sheet1_E51, "/Bitrshift/", "E51", 0.0).
?test(sheet1_F51, "/Bitrshift/", "F51", 0.0).
?test(sheet1_G51, "/Bitrshift/", "G51", 0.0).
?test(sheet1_H51, "/Bitrshift/", "H51", 0.0).
?test(sheet1_I51, "/Bitrshift/", "I51", 0.0).
?test(sheet1_J51, "/Bitrshift/", "J51", 0.0).
?test(sheet1_K51, "/Bitrshift/", "K51", 0.0).
?test(sheet1_L51, "/Bitrshift/", "L51", 0.0).
?test(sheet1_M51, "/Bitrshift/", "M51", 0.0).
?test(sheet1_N51, "/Bitrshift/", "N51", 0.0).
?test(sheet1_O51, "/Bitrshift/", "O51", 0.0).
?test(sheet1_P51, "/Bitrshift/", "P51", 0.0).
?test(sheet1_Q51, "/Bitrshift/", "Q51", 0.0).
?test(sheet1_R51, "/Bitrshift/", "R51", 0.0).
?test(sheet1_S51, "/Bitrshift/", "S51", 0.0).
?test(sheet1_T51, "/Bitrshift/", "T51", 0.0).
?test(sheet1_U51, "/Bitrshift/", "U51", 0.0).
?test(sheet1_V51, "/Bitrshift/", "V51", 0.0).
?test(sheet1_C52, "/Bitrshift/", "C52", 0.0).
?test(sheet1_D52, "/Bitrshift/", "D52", 0.0).
?test(sheet1_E52, "/Bitrshift/", "E52", 0.0).
?test(sheet1_F52, "/Bitrshift/", "F52", 1.0).
?test(sheet1_G52, "/Bitrshift/", "G52", 0.0).
?test(sheet1_H52, "/Bitrshift/", "H52", 0.0).
?test(sheet1_I52, "/Bitrshift/", "I52", 0.0).
?test(sheet1_J52, "/Bitrshift/", "J52", 0.0).
?test(sheet1_K52, "/Bitrshift/", "K52", 0.0).
?test(sheet1_L52, "/Bitrshift/", "L52", 0.0).
?test(sheet1_M52, "/Bitrshift/", "M52", 0.0).
?test(sheet1_N52, "/Bitrshift/", "N52", 0.0).
?test(sheet1_O52, "/Bitrshift/", "O52", 0.0).
?test(sheet1_P52, "/Bitrshift/", "P52", 0.0).
?test(sheet1_Q52, "/Bitrshift/", "Q52", 0.0).
?test(sheet1_R52, "/Bitrshift/", "R52", 0.0).
?test(sheet1_S52, "/Bitrshift/", "S52", 0.0).
?test(sheet1_T52, "/Bitrshift/", "T52", 0.0).
?test(sheet1_U52, "/Bitrshift/", "U52", 0.0).
?test(sheet1_V52, "/Bitrshift/", "V52", 0.0).
?test(sheet1_C53, "/Bitrshift/", "C53", 0.0).
?test(sheet1_D53, "/Bitrshift/", "D53", 0.0).
?test(sheet1_E53, "/Bitrshift/", "E53", 0.0).
?test(sheet1_F53, "/Bitrshift/", "F53", 1.0).
?test(sheet1_G53, "/Bitrshift/", "G53", 0.0).
?test(sheet1_H53, "/Bitrshift/", "H53", 0.0).
?test(sheet1_I53, "/Bitrshift/", "I53", 0.0).
?test(sheet1_J53, "/Bitrshift/", "J53", 0.0).
?test(sheet1_K53, "/Bitrshift/", "K53", 0.0).
?test(sheet1_L53, "/Bitrshift/", "L53", 0.0).
?test(sheet1_M53, "/Bitrshift/", "M53", 0.0).
?test(sheet1_N53, "/Bitrshift/", "N53", 0.0).
?test(sheet1_O53, "/Bitrshift/", "O53", 0.0).
?test(sheet1_P53, "/Bitrshift/", "P53", 0.0).
?test(sheet1_Q53, "/Bitrshift/", "Q53", 0.0).
?test(sheet1_R53, "/Bitrshift/", "R53", 0.0).
?test(sheet1_S53, "/Bitrshift/", "S53", 0.0).
?test(sheet1_T53, "/Bitrshift/", "T53", 0.0).
?test(sheet1_U53, "/Bitrshift/", "U53", 0.0).
?test(sheet1_V53, "/Bitrshift/", "V53", 0.0).
?test(sheet1_C54, "/Bitrshift/", "C54", 0.0).
?test(sheet1_D54, "/Bitrshift/", "D54", 0.0).
?test(sheet1_E54, "/Bitrshift/", "E54", 0.0).
?test(sheet1_F54, "/Bitrshift/", "F54", 1.0).
?test(sheet1_G54, "/Bitrshift/", "G54", 0.0).
?test(sheet1_H54, "/Bitrshift/", "H54", 0.0).
?test(sheet1_I54, "/Bitrshift/", "I54", 0.0).
?test(sheet1_J54, "/Bitrshift/", "J54", 0.0).
?test(sheet1_K54, "/Bitrshift/", "K54", 0.0).
?test(sheet1_L54, "/Bitrshift/", "L54", 0.0).
?test(sheet1_M54, "/Bitrshift/", "M54", 0.0).
?test(sheet1_N54, "/Bitrshift/", "N54", 0.0).
?test(sheet1_O54, "/Bitrshift/", "O54", 0.0).
?test(sheet1_P54, "/Bitrshift/", "P54", 0.0).
?test(sheet1_Q54, "/Bitrshift/", "Q54", 0.0).
?test(sheet1_R54, "/Bitrshift/", "R54", 0.0).
?test(sheet1_S54, "/Bitrshift/", "S54", 0.0).
?test(sheet1_T54, "/Bitrshift/", "T54", 0.0).
?test(sheet1_U54, "/Bitrshift/", "U54", 0.0).
?test(sheet1_V54, "/Bitrshift/", "V54", 0.0).
?test(sheet1_C55, "/Bitrshift/", "C55", 0.0).
?test(sheet1_D55, "/Bitrshift/", "D55", 0.0).
?test(sheet1_E55, "/Bitrshift/", "E55", 0.0).
?test(sheet1_F55, "/Bitrshift/", "F55", 1.0).
?test(sheet1_G55, "/Bitrshift/", "G55", 0.0).
?test(sheet1_H55, "/Bitrshift/", "H55", 0.0).
?test(sheet1_I55, "/Bitrshift/", "I55", 0.0).
?test(sheet1_J55, "/Bitrshift/", "J55", 0.0).
?test(sheet1_K55, "/Bitrshift/", "K55", 0.0).
?test(sheet1_L55, "/Bitrshift/", "L55", 0.0).
?test(sheet1_M55, "/Bitrshift/", "M55", 0.0).
?test(sheet1_N55, "/Bitrshift/", "N55", 0.0).
?test(sheet1_O55, "/Bitrshift/", "O55", 0.0).
?test(sheet1_P55, "/Bitrshift/", "P55", 0.0).
?test(sheet1_Q55, "/Bitrshift/", "Q55", 0.0).
?test(sheet1_R55, "/Bitrshift/", "R55", 0.0).
?test(sheet1_S55, "/Bitrshift/", "S55", 0.0).
?test(sheet1_T55, "/Bitrshift/", "T55", 0.0).
?test(sheet1_U55, "/Bitrshift/", "U55", 0.0).
?test(sheet1_V55, "/Bitrshift/", "V55", 0.0).
?test(sheet1_C56, "/Bitrshift/", "C56", 0.0).
?test(sheet1_D56, "/Bitrshift/", "D56", 0.0).
?test(sheet1_E56, "/Bitrshift/", "E56", 0.0).
?test(sheet1_F56, "/Bitrshift/", "F56", 1.0).
?test(sheet1_G56, "/Bitrshift/", "G56", 0.0).
?test(sheet1_H56, "/Bitrshift/", "H56", 0.0).
?test(sheet1_I56, "/Bitrshift/", "I56", 0.0).
?test(sheet1_J56, "/Bitrshift/", "J56", 0.0).
?test(sheet1_K56, "/Bitrshift/", "K56", 0.0).
?test(sheet1_L56, "/Bitrshift/", "L56", 0.0).
?test(sheet1_M56, "/Bitrshift/", "M56", 0.0).
?test(sheet1_N56, "/Bitrshift/", "N56", 0.0).
?test(sheet1_O56, "/Bitrshift/", "O56", 0.0).
?test(sheet1_P56, "/Bitrshift/", "P56", 0.0).
?test(sheet1_Q56, "/Bitrshift/", "Q56", 0.0).
?test(sheet1_R56, "/Bitrshift/", "R56", 0.0).
?test(sheet1_S56, "/Bitrshift/", "S56", 0.0).
?test(sheet1_T56, "/Bitrshift/", "T56", 0.0).
?test(sheet1_U56, "/Bitrshift/", "U56", 0.0).
?test(sheet1_V56, "/Bitrshift/", "V56", 0.0).
?test(sheet1_C57, "/Bitrshift/", "C57", 0.0).
?test(sheet1_D57, "/Bitrshift/", "D57", 0.0).
?test(sheet1_E57, "/Bitrshift/", "E57", 0.0).
?test(sheet1_F57, "/Bitrshift/", "F57", 1.0).
?test(sheet1_G57, "/Bitrshift/", "G57", 0.0).
?test(sheet1_H57, "/Bitrshift/", "H57", 0.0).
?test(sheet1_I57, "/Bitrshift/", "I57", 0.0).
?test(sheet1_J57, "/Bitrshift/", "J57", 0.0).
?test(sheet1_K57, "/Bitrshift/", "K57", 0.0).
?test(sheet1_L57, "/Bitrshift/", "L57", 0.0).
?test(sheet1_M57, "/Bitrshift/", "M57", 0.0).
?test(sheet1_N57, "/Bitrshift/", "N57", 0.0).
?test(sheet1_O57, "/Bitrshift/", "O57", 0.0).
?test(sheet1_P57, "/Bitrshift/", "P57", 0.0).
?test(sheet1_Q57, "/Bitrshift/", "Q57", 0.0).
?test(sheet1_R57, "/Bitrshift/", "R57", 0.0).
?test(sheet1_S57, "/Bitrshift/", "S57", 0.0).
?test(sheet1_T57, "/Bitrshift/", "T57", 0.0).
?test(sheet1_U57, "/Bitrshift/", "U57", 0.0).
?test(sheet1_V57, "/Bitrshift/", "V57", 0.0).
?test(sheet1_C58, "/Bitrshift/", "C58", 0.0).
?test(sheet1_D58, "/Bitrshift/", "D58", 0.0).
?test(sheet1_E58, "/Bitrshift/", "E58", 0.0).
?test(sheet1_F58, "/Bitrshift/", "F58", 1.0).
?test(sheet1_G58, "/Bitrshift/", "G58", 0.0).
?test(sheet1_H58, "/Bitrshift/", "H58", 0.0).
?test(sheet1_I58, "/Bitrshift/", "I58", 0.0).
?test(sheet1_J58, "/Bitrshift/", "J58", 0.0).
?test(sheet1_K58, "/Bitrshift/", "K58", 0.0).
?test(sheet1_L58, "/Bitrshift/", "L58", 0.0).
?test(sheet1_M58, "/Bitrshift/", "M58", 0.0).
?test(sheet1_N58, "/Bitrshift/", "N58", 0.0).
?test(sheet1_O58, "/Bitrshift/", "O58", 0.0).
?test(sheet1_P58, "/Bitrshift/", "P58", 0.0).
?test(sheet1_Q58, "/Bitrshift/", "Q58", 0.0).
?test(sheet1_R58, "/Bitrshift/", "R58", 0.0).
?test(sheet1_S58, "/Bitrshift/", "S58", 0.0).
?test(sheet1_T58, "/Bitrshift/", "T58", 0.0).
?test(sheet1_U58, "/Bitrshift/", "U58", 0.0).
?test(sheet1_V58, "/Bitrshift/", "V58", 0.0).
?test(sheet1_C59, "/Bitrshift/", "C59", 0.0).
?test(sheet1_D59, "/Bitrshift/", "D59", 0.0).
?test(sheet1_E59, "/Bitrshift/", "E59", 0.0).
?test(sheet1_F59, "/Bitrshift/", "F59", 0.0).
?test(sheet1_G59, "/Bitrshift/", "G59", 0.0).
?test(sheet1_H59, "/Bitrshift/", "H59", 0.0).
?test(sheet1_I59, "/Bitrshift/", "I59", 0.0).
?test(sheet1_J59, "/Bitrshift/", "J59", 0.0).
?test(sheet1_K59, "/Bitrshift/", "K59", 0.0).
?test(sheet1_L59, "/Bitrshift/", "L59", 0.0).
?test(sheet1_M59, "/Bitrshift/", "M59", 0.0).
?test(sheet1_N59, "/Bitrshift/", "N59", 0.0).
?test(sheet1_O59, "/Bitrshift/", "O59", 0.0).
?test(sheet1_P59, "/Bitrshift/", "P59", 0.0).
?test(sheet1_Q59, "/Bitrshift/", "Q59", 0.0).
?test(sheet1_R59, "/Bitrshift/", "R59", 0.0).
?test(sheet1_S59, "/Bitrshift/", "S59", 0.0).
?test(sheet1_T59, "/Bitrshift/", "T59", 0.0).
?test(sheet1_U59, "/Bitrshift/", "U59", 0.0).
?test(sheet1_V59, "/Bitrshift/", "V59", 0.0).
?test(sheet1_C60, "/Bitrshift/", "C60", 0.0).
?test(sheet1_D60, "/Bitrshift/", "D60", 0.0).
?test(sheet1_E60, "/Bitrshift/", "E60", 0.0).
?test(sheet1_F60, "/Bitrshift/", "F60", 0.0).
?test(sheet1_G60, "/Bitrshift/", "G60", 0.0).
?test(sheet1_H60, "/Bitrshift/", "H60", 0.0).
?test(sheet1_I60, "/Bitrshift/", "I60", 0.0).
?test(sheet1_J60, "/Bitrshift/", "J60", 0.0).
?test(sheet1_K60, "/Bitrshift/", "K60", 0.0).
?test(sheet1_L60, "/Bitrshift/", "L60", 0.0).
?test(sheet1_M60, "/Bitrshift/", "M60", 0.0).
?test(sheet1_N60, "/Bitrshift/", "N60", 0.0).
?test(sheet1_O60, "/Bitrshift/", "O60", 0.0).
?test(sheet1_P60, "/Bitrshift/", "P60", 0.0).
?test(sheet1_Q60, "/Bitrshift/", "Q60", 0.0).
?test(sheet1_R60, "/Bitrshift/", "R60", 0.0).
?test(sheet1_S60, "/Bitrshift/", "S60", 0.0).
?test(sheet1_T60, "/Bitrshift/", "T60", 0.0).
?test(sheet1_U60, "/Bitrshift/", "U60", 0.0).
?test(sheet1_V60, "/Bitrshift/", "V60", 0.0).
?test(sheet1_C61, "/Bitrshift/", "C61", 0.0).
?test(sheet1_D61, "/Bitrshift/", "D61", 0.0).
?test(sheet1_E61, "/Bitrshift/", "E61", 0.0).
?test(sheet1_F61, "/Bitrshift/", "F61", 0.0).
?test(sheet1_G61, "/Bitrshift/", "G61", 0.0).
?test(sheet1_H61, "/Bitrshift/", "H61", 0.0).
?test(sheet1_I61, "/Bitrshift/", "I61", 0.0).
?test(sheet1_J61, "/Bitrshift/", "J61", 0.0).
?test(sheet1_K61, "/Bitrshift/", "K61", 0.0).
?test(sheet1_L61, "/Bitrshift/", "L61", 0.0).
?test(sheet1_M61, "/Bitrshift/", "M61", 0.0).
?test(sheet1_N61, "/Bitrshift/", "N61", 0.0).
?test(sheet1_O61, "/Bitrshift/", "O61", 0.0).
?test(sheet1_P61, "/Bitrshift/", "P61", 0.0).
?test(sheet1_Q61, "/Bitrshift/", "Q61", 0.0).
?test(sheet1_R61, "/Bitrshift/", "R61", 0.0).
?test(sheet1_S61, "/Bitrshift/", "S61", 0.0).
?test(sheet1_T61, "/Bitrshift/", "T61", 0.0).
?test(sheet1_U61, "/Bitrshift/", "U61", 0.0).
?test(sheet1_V61, "/Bitrshift/", "V61", 0.0).
?test(sheet1_C62, "/Bitrshift/", "C62", 0.0).
?test(sheet1_D62, "/Bitrshift/", "D62", 0.0).
?test(sheet1_E62, "/Bitrshift/", "E62", 0.0).
?test(sheet1_F62, "/Bitrshift/", "F62", 0.0).
?test(sheet1_G62, "/Bitrshift/", "G62", 0.0).
?test(sheet1_H62, "/Bitrshift/", "H62", 0.0).
?test(sheet1_I62, "/Bitrshift/", "I62", 0.0).
?test(sheet1_J62, "/Bitrshift/", "J62", 0.0).
?test(sheet1_K62, "/Bitrshift/", "K62", 0.0).
?test(sheet1_L62, "/Bitrshift/", "L62", 0.0).
?test(sheet1_M62, "/Bitrshift/", "M62", 0.0).
?test(sheet1_N62, "/Bitrshift/", "N62", 0.0).
?test(sheet1_O62, "/Bitrshift/", "O62", 0.0).
?test(sheet1_P62, "/Bitrshift/", "P62", 0.0).
?test(sheet1_Q62, "/Bitrshift/", "Q62", 0.0).
?test(sheet1_R62, "/Bitrshift/", "R62", 0.0).
?test(sheet1_S62, "/Bitrshift/", "S62", 0.0).
?test(sheet1_T62, "/Bitrshift/", "T62", 0.0).
?test(sheet1_U62, "/Bitrshift/", "U62", 0.0).
?test(sheet1_V62, "/Bitrshift/", "V62", 0.0).
?test(sheet1_C63, "/Bitrshift/", "C63", 0.0).
?test(sheet1_D63, "/Bitrshift/", "D63", 0.0).
?test(sheet1_E63, "/Bitrshift/", "E63", 0.0).
?test(sheet1_F63, "/Bitrshift/", "F63", 0.0).
?test(sheet1_G63, "/Bitrshift/", "G63", 0.0).
?test(sheet1_H63, "/Bitrshift/", "H63", 0.0).
?test(sheet1_I63, "/Bitrshift/", "I63", 0.0).
?test(sheet1_J63, "/Bitrshift/", "J63", 0.0).
?test(sheet1_K63, "/Bitrshift/", "K63", 0.0).
?test(sheet1_L63, "/Bitrshift/", "L63", 0.0).
?test(sheet1_M63, "/Bitrshift/", "M63", 0.0).
?test(sheet1_N63, "/Bitrshift/", "N63", 0.0).
?test(sheet1_O63, "/Bitrshift/", "O63", 0.0).
?test(sheet1_P63, "/Bitrshift/", "P63", 0.0).
?test(sheet1_Q63, "/Bitrshift/", "Q63", 0.0).
?test(sheet1_R63, "/Bitrshift/", "R63", 0.0).
?test(sheet1_S63, "/Bitrshift/", "S63", 0.0).
?test(sheet1_T63, "/Bitrshift/", "T63", 0.0).
?test(sheet1_U63, "/Bitrshift/", "U63", 0.0).
?test(sheet1_V63, "/Bitrshift/", "V63", 0.0).
?test(sheet1_C64, "/Bitrshift/", "C64", 0.0).
?test(sheet1_D64, "/Bitrshift/", "D64", 0.0).
?test(sheet1_E64, "/Bitrshift/", "E64", 0.0).
?test(sheet1_F64, "/Bitrshift/", "F64", 0.0).
?test(sheet1_G64, "/Bitrshift/", "G64", 0.0).
?test(sheet1_H64, "/Bitrshift/", "H64", 0.0).
?test(sheet1_I64, "/Bitrshift/", "I64", 0.0).
?test(sheet1_J64, "/Bitrshift/", "J64", 0.0).
?test(sheet1_K64, "/Bitrshift/", "K64", 0.0).
?test(sheet1_L64, "/Bitrshift/", "L64", 0.0).
?test(sheet1_M64, "/Bitrshift/", "M64", 0.0).
?test(sheet1_N64, "/Bitrshift/", "N64", 0.0).
?test(sheet1_O64, "/Bitrshift/", "O64", 0.0).
?test(sheet1_P64, "/Bitrshift/", "P64", 0.0).
?test(sheet1_Q64, "/Bitrshift/", "Q64", 0.0).
?test(sheet1_R64, "/Bitrshift/", "R64", 0.0).
?test(sheet1_S64, "/Bitrshift/", "S64", 0.0).
?test(sheet1_T64, "/Bitrshift/", "T64", 0.0).
?test(sheet1_U64, "/Bitrshift/", "U64", 0.0).
?test(sheet1_V64, "/Bitrshift/", "V64", 0.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_bitwise_bitrshift.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_bitwise_bitrshift" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_B1,
        sheet1_C1,
        sheet1_D1,
        sheet1_E1,
        sheet1_F1,
        sheet1_G1,
        sheet1_H1,
        sheet1_I1,
        sheet1_J1,
        sheet1_K1,
        sheet1_L1,
        sheet1_M1,
        sheet1_N1,
        sheet1_O1,
        sheet1_P1,
        sheet1_Q1,
        sheet1_R1,
        sheet1_S1,
        sheet1_T1,
        sheet1_U1,
        sheet1_V1,
        sheet1_A2,
        sheet1_C2,
        sheet1_D2,
        sheet1_E2,
        sheet1_F2,
        sheet1_G2,
        sheet1_H2,
        sheet1_I2,
        sheet1_J2,
        sheet1_K2,
        sheet1_L2,
        sheet1_M2,
        sheet1_O2,
        sheet1_P2,
        sheet1_Q2,
        sheet1_R2,
        sheet1_S2,
        sheet1_T2,
        sheet1_U2,
        sheet1_V2,
        sheet1_A3,
        sheet1_B3,
        sheet1_C3,
        sheet1_D3,
        sheet1_E3,
        sheet1_F3,
        sheet1_G3,
        sheet1_H3,
        sheet1_I3,
        sheet1_J3,
        sheet1_K3,
        sheet1_L3,
        sheet1_M3,
        sheet1_N3,
        sheet1_O3,
        sheet1_P3,
        sheet1_Q3,
        sheet1_R3,
        sheet1_S3,
        sheet1_T3,
        sheet1_U3,
        sheet1_V3,
        sheet1_X3,
        sheet1_Y3,
        sheet1_Z3,
        sheet1_AA3,
        sheet1_A4,
        sheet1_B4,
        sheet1_C4,
        sheet1_D4,
        sheet1_E4,
        sheet1_F4,
        sheet1_G4,
        sheet1_H4,
        sheet1_I4,
        sheet1_J4,
        sheet1_K4,
        sheet1_L4,
        sheet1_M4,
        sheet1_N4,
        sheet1_O4,
        sheet1_P4,
        sheet1_Q4,
        sheet1_R4,
        sheet1_S4,
        sheet1_T4,
        sheet1_U4,
        sheet1_V4,
        sheet1_X4,
        sheet1_Y4,
        sheet1_Z4,
        sheet1_AA4,
        sheet1_A5,
        sheet1_B5,
        sheet1_C5,
        sheet1_D5,
        sheet1_E5,
        sheet1_F5,
        sheet1_G5,
        sheet1_H5,
        sheet1_I5,
        sheet1_J5,
        sheet1_K5,
        sheet1_L5,
        sheet1_M5,
        sheet1_N5,
        sheet1_O5,
        sheet1_P5,
        sheet1_Q5,
        sheet1_R5,
        sheet1_S5,
        sheet1_T5,
        sheet1_U5,
        sheet1_V5,
        sheet1_X5,
        sheet1_Y5,
        sheet1_Z5,
        sheet1_AA5,
        sheet1_A6,
        sheet1_B6,
        sheet1_C6,
        sheet1_D6,
        sheet1_E6,
        sheet1_F6,
        sheet1_G6,
        sheet1_H6,
        sheet1_I6,
        sheet1_J6,
        sheet1_K6,
        sheet1_L6,
        sheet1_M6,
        sheet1_N6,
        sheet1_O6,
        sheet1_P6,
        sheet1_Q6,
        sheet1_R6,
        sheet1_S6,
        sheet1_T6,
        sheet1_U6,
        sheet1_V6,
        sheet1_X6,
        sheet1_Y6,
        sheet1_Z6,
        sheet1_AA6,
        sheet1_A7,
        sheet1_B7,
        sheet1_C7,
        sheet1_D7,
        sheet1_E7,
        sheet1_F7,
        sheet1_G7,
        sheet1_H7,
        sheet1_I7,
        sheet1_J7,
        sheet1_K7,
        sheet1_L7,
        sheet1_M7,
        sheet1_N7,
        sheet1_O7,
        sheet1_P7,
        sheet1_Q7,
        sheet1_R7,
        sheet1_S7,
        sheet1_T7,
        sheet1_U7,
        sheet1_V7,
        sheet1_A8,
        sheet1_B8,
        sheet1_C8,
        sheet1_D8,
        sheet1_E8,
        sheet1_F8,
        sheet1_G8,
        sheet1_H8,
        sheet1_I8,
        sheet1_J8,
        sheet1_K8,
        sheet1_L8,
        sheet1_M8,
        sheet1_N8,
        sheet1_O8,
        sheet1_P8,
        sheet1_Q8,
        sheet1_R8,
        sheet1_S8,
        sheet1_T8,
        sheet1_U8,
        sheet1_V8,
        sheet1_A9,
        sheet1_B9,
        sheet1_C9,
        sheet1_D9,
        sheet1_E9,
        sheet1_F9,
        sheet1_G9,
        sheet1_H9,
        sheet1_I9,
        sheet1_J9,
        sheet1_K9,
        sheet1_L9,
        sheet1_M9,
        sheet1_N9,
        sheet1_O9,
        sheet1_P9,
        sheet1_Q9,
        sheet1_R9,
        sheet1_S9,
        sheet1_T9,
        sheet1_U9,
        sheet1_V9,
        sheet1_A10,
        sheet1_B10,
        sheet1_C10,
        sheet1_D10,
        sheet1_E10,
        sheet1_F10,
        sheet1_G10,
        sheet1_H10,
        sheet1_I10,
        sheet1_J10,
        sheet1_K10,
        sheet1_L10,
        sheet1_M10,
        sheet1_N10,
        sheet1_O10,
        sheet1_P10,
        sheet1_Q10,
        sheet1_R10,
        sheet1_S10,
        sheet1_T10,
        sheet1_U10,
        sheet1_V10,
        sheet1_A11,
        sheet1_B11,
        sheet1_C11,
        sheet1_D11,
        sheet1_E11,
        sheet1_F11,
        sheet1_G11,
        sheet1_H11,
        sheet1_I11,
        sheet1_J11,
        sheet1_K11,
        sheet1_L11,
        sheet1_M11,
        sheet1_N11,
        sheet1_O11,
        sheet1_P11,
        sheet1_Q11,
        sheet1_R11,
        sheet1_S11,
        sheet1_T11,
        sheet1_U11,
        sheet1_V11,
        sheet1_A12,
        sheet1_B12,
        sheet1_C12,
        sheet1_D12,
        sheet1_E12,
        sheet1_F12,
        sheet1_G12,
        sheet1_H12,
        sheet1_I12,
        sheet1_J12,
        sheet1_K12,
        sheet1_L12,
        sheet1_M12,
        sheet1_N12,
        sheet1_O12,
        sheet1_P12,
        sheet1_Q12,
        sheet1_R12,
        sheet1_S12,
        sheet1_T12,
        sheet1_U12,
        sheet1_V12,
        sheet1_A13,
        sheet1_B13,
        sheet1_C13,
        sheet1_D13,
        sheet1_E13,
        sheet1_F13,
        sheet1_G13,
        sheet1_H13,
        sheet1_I13,
        sheet1_J13,
        sheet1_K13,
        sheet1_L13,
        sheet1_M13,
        sheet1_N13,
        sheet1_O13,
        sheet1_P13,
        sheet1_Q13,
        sheet1_R13,
        sheet1_S13,
        sheet1_T13,
        sheet1_U13,
        sheet1_V13,
        sheet1_A14,
        sheet1_C14,
        sheet1_D14,
        sheet1_E14,
        sheet1_F14,
        sheet1_G14,
        sheet1_H14,
        sheet1_I14,
        sheet1_J14,
        sheet1_K14,
        sheet1_L14,
        sheet1_M14,
        sheet1_N14,
        sheet1_O14,
        sheet1_P14,
        sheet1_Q14,
        sheet1_R14,
        sheet1_S14,
        sheet1_T14,
        sheet1_U14,
        sheet1_V14,
        sheet1_A15,
        sheet1_B15,
        sheet1_C15,
        sheet1_D15,
        sheet1_E15,
        sheet1_F15,
        sheet1_G15,
        sheet1_H15,
        sheet1_I15,
        sheet1_J15,
        sheet1_K15,
        sheet1_L15,
        sheet1_M15,
        sheet1_N15,
        sheet1_O15,
        sheet1_P15,
        sheet1_Q15,
        sheet1_R15,
        sheet1_S15,
        sheet1_T15,
        sheet1_U15,
        sheet1_V15,
        sheet1_A16,
        sheet1_B16,
        sheet1_C16,
        sheet1_D16,
        sheet1_E16,
        sheet1_F16,
        sheet1_G16,
        sheet1_H16,
        sheet1_I16,
        sheet1_J16,
        sheet1_K16,
        sheet1_L16,
        sheet1_M16,
        sheet1_N16,
        sheet1_O16,
        sheet1_P16,
        sheet1_Q16,
        sheet1_R16,
        sheet1_S16,
        sheet1_T16,
        sheet1_U16,
        sheet1_V16,
        sheet1_A17,
        sheet1_B17,
        sheet1_C17,
        sheet1_D17,
        sheet1_E17,
        sheet1_F17,
        sheet1_G17,
        sheet1_H17,
        sheet1_I17,
        sheet1_J17,
        sheet1_K17,
        sheet1_L17,
        sheet1_M17,
        sheet1_N17,
        sheet1_O17,
        sheet1_P17,
        sheet1_Q17,
        sheet1_R17,
        sheet1_S17,
        sheet1_T17,
        sheet1_U17,
        sheet1_V17,
        sheet1_A18,
        sheet1_B18,
        sheet1_C18,
        sheet1_D18,
        sheet1_E18,
        sheet1_F18,
        sheet1_G18,
        sheet1_H18,
        sheet1_I18,
        sheet1_J18,
        sheet1_K18,
        sheet1_L18,
        sheet1_M18,
        sheet1_N18,
        sheet1_O18,
        sheet1_P18,
        sheet1_Q18,
        sheet1_R18,
        sheet1_S18,
        sheet1_T18,
        sheet1_U18,
        sheet1_V18,
        sheet1_A19,
        sheet1_B19,
        sheet1_C19,
        sheet1_D19,
        sheet1_E19,
        sheet1_F19,
        sheet1_G19,
        sheet1_H19,
        sheet1_I19,
        sheet1_J19,
        sheet1_K19,
        sheet1_L19,
        sheet1_M19,
        sheet1_N19,
        sheet1_O19,
        sheet1_P19,
        sheet1_Q19,
        sheet1_R19,
        sheet1_S19,
        sheet1_T19,
        sheet1_U19,
        sheet1_V19,
        sheet1_A20,
        sheet1_B20,
        sheet1_C20,
        sheet1_D20,
        sheet1_E20,
        sheet1_F20,
        sheet1_G20,
        sheet1_H20,
        sheet1_I20,
        sheet1_J20,
        sheet1_K20,
        sheet1_L20,
        sheet1_M20,
        sheet1_N20,
        sheet1_O20,
        sheet1_P20,
        sheet1_Q20,
        sheet1_R20,
        sheet1_S20,
        sheet1_T20,
        sheet1_U20,
        sheet1_V20,
        sheet1_A21,
        sheet1_B21,
        sheet1_C21,
        sheet1_D21,
        sheet1_E21,
        sheet1_F21,
        sheet1_G21,
        sheet1_H21,
        sheet1_I21,
        sheet1_J21,
        sheet1_K21,
        sheet1_L21,
        sheet1_M21,
        sheet1_N21,
        sheet1_O21,
        sheet1_P21,
        sheet1_Q21,
        sheet1_R21,
        sheet1_S21,
        sheet1_T21,
        sheet1_U21,
        sheet1_V21,
        sheet1_A22,
        sheet1_B22,
        sheet1_C22,
        sheet1_D22,
        sheet1_E22,
        sheet1_F22,
        sheet1_G22,
        sheet1_H22,
        sheet1_I22,
        sheet1_J22,
        sheet1_K22,
        sheet1_L22,
        sheet1_M22,
        sheet1_N22,
        sheet1_O22,
        sheet1_P22,
        sheet1_Q22,
        sheet1_R22,
        sheet1_S22,
        sheet1_T22,
        sheet1_U22,
        sheet1_V22,
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
        sheet1_M25,
        sheet1_N25,
        sheet1_O25,
        sheet1_P25,
        sheet1_Q25,
        sheet1_R25,
        sheet1_S25,
        sheet1_T25,
        sheet1_U25,
        sheet1_V25,
        sheet1_A26,
        sheet1_C26,
        sheet1_D26,
        sheet1_E26,
        sheet1_F26,
        sheet1_G26,
        sheet1_H26,
        sheet1_I26,
        sheet1_J26,
        sheet1_K26,
        sheet1_L26,
        sheet1_M26,
        sheet1_O26,
        sheet1_P26,
        sheet1_Q26,
        sheet1_R26,
        sheet1_S26,
        sheet1_T26,
        sheet1_U26,
        sheet1_V26,
        sheet1_A27,
        sheet1_B27,
        sheet1_C27,
        sheet1_D27,
        sheet1_E27,
        sheet1_F27,
        sheet1_G27,
        sheet1_H27,
        sheet1_I27,
        sheet1_J27,
        sheet1_K27,
        sheet1_L27,
        sheet1_M27,
        sheet1_N27,
        sheet1_O27,
        sheet1_P27,
        sheet1_Q27,
        sheet1_R27,
        sheet1_S27,
        sheet1_T27,
        sheet1_U27,
        sheet1_V27,
        sheet1_A28,
        sheet1_B28,
        sheet1_C28,
        sheet1_D28,
        sheet1_E28,
        sheet1_F28,
        sheet1_G28,
        sheet1_H28,
        sheet1_I28,
        sheet1_J28,
        sheet1_K28,
        sheet1_L28,
        sheet1_M28,
        sheet1_N28,
        sheet1_O28,
        sheet1_P28,
        sheet1_Q28,
        sheet1_R28,
        sheet1_S28,
        sheet1_T28,
        sheet1_U28,
        sheet1_V28,
        sheet1_A29,
        sheet1_B29,
        sheet1_C29,
        sheet1_D29,
        sheet1_E29,
        sheet1_F29,
        sheet1_G29,
        sheet1_H29,
        sheet1_I29,
        sheet1_J29,
        sheet1_K29,
        sheet1_L29,
        sheet1_M29,
        sheet1_N29,
        sheet1_O29,
        sheet1_P29,
        sheet1_Q29,
        sheet1_R29,
        sheet1_S29,
        sheet1_T29,
        sheet1_U29,
        sheet1_V29,
        sheet1_A30,
        sheet1_B30,
        sheet1_C30,
        sheet1_D30,
        sheet1_E30,
        sheet1_F30,
        sheet1_G30,
        sheet1_H30,
        sheet1_I30,
        sheet1_J30,
        sheet1_K30,
        sheet1_L30,
        sheet1_M30,
        sheet1_N30,
        sheet1_O30,
        sheet1_P30,
        sheet1_Q30,
        sheet1_R30,
        sheet1_S30,
        sheet1_T30,
        sheet1_U30,
        sheet1_V30,
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
        sheet1_M31,
        sheet1_N31,
        sheet1_O31,
        sheet1_P31,
        sheet1_Q31,
        sheet1_R31,
        sheet1_S31,
        sheet1_T31,
        sheet1_U31,
        sheet1_V31,
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
        sheet1_M32,
        sheet1_N32,
        sheet1_O32,
        sheet1_P32,
        sheet1_Q32,
        sheet1_R32,
        sheet1_S32,
        sheet1_T32,
        sheet1_U32,
        sheet1_V32,
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
        sheet1_M33,
        sheet1_N33,
        sheet1_O33,
        sheet1_P33,
        sheet1_Q33,
        sheet1_R33,
        sheet1_S33,
        sheet1_T33,
        sheet1_U33,
        sheet1_V33,
        sheet1_A34,
        sheet1_B34,
        sheet1_C34,
        sheet1_D34,
        sheet1_E34,
        sheet1_F34,
        sheet1_G34,
        sheet1_H34,
        sheet1_I34,
        sheet1_J34,
        sheet1_K34,
        sheet1_L34,
        sheet1_M34,
        sheet1_N34,
        sheet1_O34,
        sheet1_P34,
        sheet1_Q34,
        sheet1_R34,
        sheet1_S34,
        sheet1_T34,
        sheet1_U34,
        sheet1_V34,
        sheet1_A35,
        sheet1_B35,
        sheet1_C35,
        sheet1_D35,
        sheet1_E35,
        sheet1_F35,
        sheet1_G35,
        sheet1_H35,
        sheet1_I35,
        sheet1_J35,
        sheet1_K35,
        sheet1_L35,
        sheet1_M35,
        sheet1_N35,
        sheet1_O35,
        sheet1_P35,
        sheet1_Q35,
        sheet1_R35,
        sheet1_S35,
        sheet1_T35,
        sheet1_U35,
        sheet1_V35,
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
        sheet1_M36,
        sheet1_N36,
        sheet1_O36,
        sheet1_P36,
        sheet1_Q36,
        sheet1_R36,
        sheet1_S36,
        sheet1_T36,
        sheet1_U36,
        sheet1_V36,
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
        sheet1_M37,
        sheet1_N37,
        sheet1_O37,
        sheet1_P37,
        sheet1_Q37,
        sheet1_R37,
        sheet1_S37,
        sheet1_T37,
        sheet1_U37,
        sheet1_V37,
        sheet1_A38,
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
        sheet1_M38,
        sheet1_N38,
        sheet1_O38,
        sheet1_P38,
        sheet1_Q38,
        sheet1_R38,
        sheet1_S38,
        sheet1_T38,
        sheet1_U38,
        sheet1_V38,
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
        sheet1_L39,
        sheet1_M39,
        sheet1_N39,
        sheet1_O39,
        sheet1_P39,
        sheet1_Q39,
        sheet1_R39,
        sheet1_S39,
        sheet1_T39,
        sheet1_U39,
        sheet1_V39,
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
        sheet1_M40,
        sheet1_N40,
        sheet1_O40,
        sheet1_P40,
        sheet1_Q40,
        sheet1_R40,
        sheet1_S40,
        sheet1_T40,
        sheet1_U40,
        sheet1_V40,
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
        sheet1_M41,
        sheet1_N41,
        sheet1_O41,
        sheet1_P41,
        sheet1_Q41,
        sheet1_R41,
        sheet1_S41,
        sheet1_T41,
        sheet1_U41,
        sheet1_V41,
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
        sheet1_M42,
        sheet1_N42,
        sheet1_O42,
        sheet1_P42,
        sheet1_Q42,
        sheet1_R42,
        sheet1_S42,
        sheet1_T42,
        sheet1_U42,
        sheet1_V42,
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
        sheet1_M43,
        sheet1_N43,
        sheet1_O43,
        sheet1_P43,
        sheet1_Q43,
        sheet1_R43,
        sheet1_S43,
        sheet1_T43,
        sheet1_U43,
        sheet1_V43,
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
        sheet1_M44,
        sheet1_N44,
        sheet1_O44,
        sheet1_P44,
        sheet1_Q44,
        sheet1_R44,
        sheet1_S44,
        sheet1_T44,
        sheet1_U44,
        sheet1_V44,
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
        sheet1_M45,
        sheet1_N45,
        sheet1_O45,
        sheet1_P45,
        sheet1_Q45,
        sheet1_R45,
        sheet1_S45,
        sheet1_T45,
        sheet1_U45,
        sheet1_V45,
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
        sheet1_M46,
        sheet1_N46,
        sheet1_O46,
        sheet1_P46,
        sheet1_Q46,
        sheet1_R46,
        sheet1_S46,
        sheet1_T46,
        sheet1_U46,
        sheet1_V46,
        sheet1_A49,
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
        sheet1_M49,
        sheet1_N49,
        sheet1_O49,
        sheet1_P49,
        sheet1_Q49,
        sheet1_R49,
        sheet1_S49,
        sheet1_T49,
        sheet1_U49,
        sheet1_V49,
        sheet1_A50,
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
        sheet1_M50,
        sheet1_N50,
        sheet1_O50,
        sheet1_P50,
        sheet1_Q50,
        sheet1_R50,
        sheet1_S50,
        sheet1_T50,
        sheet1_U50,
        sheet1_V50,
        sheet1_C51,
        sheet1_D51,
        sheet1_E51,
        sheet1_F51,
        sheet1_G51,
        sheet1_H51,
        sheet1_I51,
        sheet1_J51,
        sheet1_K51,
        sheet1_L51,
        sheet1_M51,
        sheet1_N51,
        sheet1_O51,
        sheet1_P51,
        sheet1_Q51,
        sheet1_R51,
        sheet1_S51,
        sheet1_T51,
        sheet1_U51,
        sheet1_V51,
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
        sheet1_M52,
        sheet1_N52,
        sheet1_O52,
        sheet1_P52,
        sheet1_Q52,
        sheet1_R52,
        sheet1_S52,
        sheet1_T52,
        sheet1_U52,
        sheet1_V52,
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
        sheet1_M53,
        sheet1_N53,
        sheet1_O53,
        sheet1_P53,
        sheet1_Q53,
        sheet1_R53,
        sheet1_S53,
        sheet1_T53,
        sheet1_U53,
        sheet1_V53,
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
        sheet1_M54,
        sheet1_N54,
        sheet1_O54,
        sheet1_P54,
        sheet1_Q54,
        sheet1_R54,
        sheet1_S54,
        sheet1_T54,
        sheet1_U54,
        sheet1_V54,
        sheet1_C55,
        sheet1_D55,
        sheet1_E55,
        sheet1_F55,
        sheet1_G55,
        sheet1_H55,
        sheet1_I55,
        sheet1_J55,
        sheet1_K55,
        sheet1_L55,
        sheet1_M55,
        sheet1_N55,
        sheet1_O55,
        sheet1_P55,
        sheet1_Q55,
        sheet1_R55,
        sheet1_S55,
        sheet1_T55,
        sheet1_U55,
        sheet1_V55,
        sheet1_C56,
        sheet1_D56,
        sheet1_E56,
        sheet1_F56,
        sheet1_G56,
        sheet1_H56,
        sheet1_I56,
        sheet1_J56,
        sheet1_K56,
        sheet1_L56,
        sheet1_M56,
        sheet1_N56,
        sheet1_O56,
        sheet1_P56,
        sheet1_Q56,
        sheet1_R56,
        sheet1_S56,
        sheet1_T56,
        sheet1_U56,
        sheet1_V56,
        sheet1_C57,
        sheet1_D57,
        sheet1_E57,
        sheet1_F57,
        sheet1_G57,
        sheet1_H57,
        sheet1_I57,
        sheet1_J57,
        sheet1_K57,
        sheet1_L57,
        sheet1_M57,
        sheet1_N57,
        sheet1_O57,
        sheet1_P57,
        sheet1_Q57,
        sheet1_R57,
        sheet1_S57,
        sheet1_T57,
        sheet1_U57,
        sheet1_V57,
        sheet1_C58,
        sheet1_D58,
        sheet1_E58,
        sheet1_F58,
        sheet1_G58,
        sheet1_H58,
        sheet1_I58,
        sheet1_J58,
        sheet1_K58,
        sheet1_L58,
        sheet1_M58,
        sheet1_N58,
        sheet1_O58,
        sheet1_P58,
        sheet1_Q58,
        sheet1_R58,
        sheet1_S58,
        sheet1_T58,
        sheet1_U58,
        sheet1_V58,
        sheet1_C59,
        sheet1_D59,
        sheet1_E59,
        sheet1_F59,
        sheet1_G59,
        sheet1_H59,
        sheet1_I59,
        sheet1_J59,
        sheet1_K59,
        sheet1_L59,
        sheet1_M59,
        sheet1_N59,
        sheet1_O59,
        sheet1_P59,
        sheet1_Q59,
        sheet1_R59,
        sheet1_S59,
        sheet1_T59,
        sheet1_U59,
        sheet1_V59,
        sheet1_C60,
        sheet1_D60,
        sheet1_E60,
        sheet1_F60,
        sheet1_G60,
        sheet1_H60,
        sheet1_I60,
        sheet1_J60,
        sheet1_K60,
        sheet1_L60,
        sheet1_M60,
        sheet1_N60,
        sheet1_O60,
        sheet1_P60,
        sheet1_Q60,
        sheet1_R60,
        sheet1_S60,
        sheet1_T60,
        sheet1_U60,
        sheet1_V60,
        sheet1_C61,
        sheet1_D61,
        sheet1_E61,
        sheet1_F61,
        sheet1_G61,
        sheet1_H61,
        sheet1_I61,
        sheet1_J61,
        sheet1_K61,
        sheet1_L61,
        sheet1_M61,
        sheet1_N61,
        sheet1_O61,
        sheet1_P61,
        sheet1_Q61,
        sheet1_R61,
        sheet1_S61,
        sheet1_T61,
        sheet1_U61,
        sheet1_V61,
        sheet1_C62,
        sheet1_D62,
        sheet1_E62,
        sheet1_F62,
        sheet1_G62,
        sheet1_H62,
        sheet1_I62,
        sheet1_J62,
        sheet1_K62,
        sheet1_L62,
        sheet1_M62,
        sheet1_N62,
        sheet1_O62,
        sheet1_P62,
        sheet1_Q62,
        sheet1_R62,
        sheet1_S62,
        sheet1_T62,
        sheet1_U62,
        sheet1_V62,
        sheet1_C63,
        sheet1_D63,
        sheet1_E63,
        sheet1_F63,
        sheet1_G63,
        sheet1_H63,
        sheet1_I63,
        sheet1_J63,
        sheet1_K63,
        sheet1_L63,
        sheet1_M63,
        sheet1_N63,
        sheet1_O63,
        sheet1_P63,
        sheet1_Q63,
        sheet1_R63,
        sheet1_S63,
        sheet1_T63,
        sheet1_U63,
        sheet1_V63,
        sheet1_C64,
        sheet1_D64,
        sheet1_E64,
        sheet1_F64,
        sheet1_G64,
        sheet1_H64,
        sheet1_I64,
        sheet1_J64,
        sheet1_K64,
        sheet1_L64,
        sheet1_M64,
        sheet1_N64,
        sheet1_O64,
        sheet1_P64,
        sheet1_Q64,
        sheet1_R64,
        sheet1_S64,
        sheet1_T64,
        sheet1_U64,
        sheet1_V64
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
