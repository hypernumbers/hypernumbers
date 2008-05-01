%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_bitwise_bitor_SUITE).
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
                     [Testcase, "e_gnumeric_bitwise_bitor_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_bitwise_bitor" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Bitor/", "A1", "bitor(A,B)").
?test(sheet1_B1, "/Bitor/", "B1", "B").
?test(sheet1_C1, "/Bitor/", "C1", "errors").
?test(sheet1_D1, "/Bitor/", "D1", "errors").
?test(sheet1_E1, "/Bitor/", "E1", "errors").
?test(sheet1_F1, "/Bitor/", "F1", "errors").
?test(sheet1_G1, "/Bitor/", "G1", "errors").
?test(sheet1_H1, "/Bitor/", "H1", "errors").
?test(sheet1_I1, "/Bitor/", "I1", "String").
?test(sheet1_J1, "/Bitor/", "J1", "String Number").
?test(sheet1_K1, "/Bitor/", "K1", "String number Leading space").
?test(sheet1_L1, "/Bitor/", "L1", "Integer").
?test(sheet1_M1, "/Bitor/", "M1", "Float").
?test(sheet1_N1, "/Bitor/", "N1", "Blank").
?test(sheet1_O1, "/Bitor/", "O1", "Logical").
?test(sheet1_P1, "/Bitor/", "P1", "Logical").
?test(sheet1_Q1, "/Bitor/", "Q1", "Range Row").
?test(sheet1_R1, "/Bitor/", "R1", "Range Row").
?test(sheet1_S1, "/Bitor/", "S1", "Range Area").
?test(sheet1_T1, "/Bitor/", "T1", "Range Area").
?test(sheet1_U1, "/Bitor/", "U1", "Range Colunm").
?test(sheet1_V1, "/Bitor/", "V1", "Range Colunm").
?test(sheet1_A2, "/Bitor/", "A2", "A").
?test(sheet1_C2, "/Bitor/", "C2", '#DIV/0!').
?test(sheet1_D2, "/Bitor/", "D2", '#VALUE!').
?test(sheet1_E2, "/Bitor/", "E2", '#REF!').
?test(sheet1_F2, "/Bitor/", "F2", '#NAME?').
?test(sheet1_G2, "/Bitor/", "G2", '#NUM!').
?test(sheet1_H2, "/Bitor/", "H2", '#N/A').
?test(sheet1_I2, "/Bitor/", "I2", "Phillip").
?test(sheet1_J2, "/Bitor/", "J2", "13").
?test(sheet1_K2, "/Bitor/", "K2", " 24").
?test(sheet1_L2, "/Bitor/", "L2", "1968/03/23 00:00:00").
?test(sheet1_M2, "/Bitor/", "M2", 3.14159265358979).
?test(sheet1_O2, "/Bitor/", "O2", true).
?test(sheet1_P2, "/Bitor/", "P2", false).
?test(sheet1_Q2, "/Bitor/", "Q2", "X3:Y3").
?test(sheet1_R2, "/Bitor/", "R2", "X3:AA3").
?test(sheet1_S2, "/Bitor/", "S2", "X3:Y4").
?test(sheet1_T2, "/Bitor/", "T2", "X3:AA6").
?test(sheet1_U2, "/Bitor/", "U2", "X3:X4").
?test(sheet1_V2, "/Bitor/", "V2", "X3:X6").
?test(sheet1_A3, "/Bitor/", "A3", "errors").
?test(sheet1_B3, "/Bitor/", "B3", '#DIV/0!').
?test(sheet1_C3, "/Bitor/", "C3", '#NAME?').
?test(sheet1_D3, "/Bitor/", "D3", '#NAME?').
?test(sheet1_E3, "/Bitor/", "E3", '#NAME?').
?test(sheet1_F3, "/Bitor/", "F3", '#NAME?').
?test(sheet1_G3, "/Bitor/", "G3", '#NAME?').
?test(sheet1_H3, "/Bitor/", "H3", '#NAME?').
?test(sheet1_I3, "/Bitor/", "I3", '#NAME?').
?test(sheet1_J3, "/Bitor/", "J3", '#NAME?').
?test(sheet1_K3, "/Bitor/", "K3", '#NAME?').
?test(sheet1_L3, "/Bitor/", "L3", '#NAME?').
?test(sheet1_M3, "/Bitor/", "M3", '#NAME?').
?test(sheet1_N3, "/Bitor/", "N3", '#NAME?').
?test(sheet1_O3, "/Bitor/", "O3", '#NAME?').
?test(sheet1_P3, "/Bitor/", "P3", '#NAME?').
?test(sheet1_Q3, "/Bitor/", "Q3", '#NAME?').
?test(sheet1_R3, "/Bitor/", "R3", '#NAME?').
?test(sheet1_S3, "/Bitor/", "S3", '#NAME?').
?test(sheet1_T3, "/Bitor/", "T3", '#NAME?').
?test(sheet1_U3, "/Bitor/", "U3", '#NAME?').
?test(sheet1_V3, "/Bitor/", "V3", '#NAME?').
?test(sheet1_X3, "/Bitor/", "X3", 7.0).
?test(sheet1_Y3, "/Bitor/", "Y3", 5.0).
?test(sheet1_Z3, "/Bitor/", "Z3", 3.0).
?test(sheet1_AA3, "/Bitor/", "AA3", 1.0).
?test(sheet1_A4, "/Bitor/", "A4", "errors").
?test(sheet1_B4, "/Bitor/", "B4", '#VALUE!').
?test(sheet1_C4, "/Bitor/", "C4", '#NAME?').
?test(sheet1_D4, "/Bitor/", "D4", '#NAME?').
?test(sheet1_E4, "/Bitor/", "E4", '#NAME?').
?test(sheet1_F4, "/Bitor/", "F4", '#NAME?').
?test(sheet1_G4, "/Bitor/", "G4", '#NAME?').
?test(sheet1_H4, "/Bitor/", "H4", '#NAME?').
?test(sheet1_I4, "/Bitor/", "I4", '#NAME?').
?test(sheet1_J4, "/Bitor/", "J4", '#NAME?').
?test(sheet1_K4, "/Bitor/", "K4", '#NAME?').
?test(sheet1_L4, "/Bitor/", "L4", '#NAME?').
?test(sheet1_M4, "/Bitor/", "M4", '#NAME?').
?test(sheet1_N4, "/Bitor/", "N4", '#NAME?').
?test(sheet1_O4, "/Bitor/", "O4", '#NAME?').
?test(sheet1_P4, "/Bitor/", "P4", '#NAME?').
?test(sheet1_Q4, "/Bitor/", "Q4", '#NAME?').
?test(sheet1_R4, "/Bitor/", "R4", '#NAME?').
?test(sheet1_S4, "/Bitor/", "S4", '#NAME?').
?test(sheet1_T4, "/Bitor/", "T4", '#NAME?').
?test(sheet1_U4, "/Bitor/", "U4", '#NAME?').
?test(sheet1_V4, "/Bitor/", "V4", '#NAME?').
?test(sheet1_X4, "/Bitor/", "X4", 8.0).
?test(sheet1_Y4, "/Bitor/", "Y4", 9.0).
?test(sheet1_Z4, "/Bitor/", "Z4", 10.0).
?test(sheet1_AA4, "/Bitor/", "AA4", 11.0).
?test(sheet1_A5, "/Bitor/", "A5", "errors").
?test(sheet1_B5, "/Bitor/", "B5", '#REF!').
?test(sheet1_C5, "/Bitor/", "C5", '#NAME?').
?test(sheet1_D5, "/Bitor/", "D5", '#NAME?').
?test(sheet1_E5, "/Bitor/", "E5", '#NAME?').
?test(sheet1_F5, "/Bitor/", "F5", '#NAME?').
?test(sheet1_G5, "/Bitor/", "G5", '#NAME?').
?test(sheet1_H5, "/Bitor/", "H5", '#NAME?').
?test(sheet1_I5, "/Bitor/", "I5", '#NAME?').
?test(sheet1_J5, "/Bitor/", "J5", '#NAME?').
?test(sheet1_K5, "/Bitor/", "K5", '#NAME?').
?test(sheet1_L5, "/Bitor/", "L5", '#NAME?').
?test(sheet1_M5, "/Bitor/", "M5", '#NAME?').
?test(sheet1_N5, "/Bitor/", "N5", '#NAME?').
?test(sheet1_O5, "/Bitor/", "O5", '#NAME?').
?test(sheet1_P5, "/Bitor/", "P5", '#NAME?').
?test(sheet1_Q5, "/Bitor/", "Q5", '#NAME?').
?test(sheet1_R5, "/Bitor/", "R5", '#NAME?').
?test(sheet1_S5, "/Bitor/", "S5", '#NAME?').
?test(sheet1_T5, "/Bitor/", "T5", '#NAME?').
?test(sheet1_U5, "/Bitor/", "U5", '#NAME?').
?test(sheet1_V5, "/Bitor/", "V5", '#NAME?').
?test(sheet1_X5, "/Bitor/", "X5", 9.0).
?test(sheet1_Y5, "/Bitor/", "Y5", 13.0).
?test(sheet1_Z5, "/Bitor/", "Z5", 17.0).
?test(sheet1_AA5, "/Bitor/", "AA5", 21.0).
?test(sheet1_A6, "/Bitor/", "A6", "errors").
?test(sheet1_B6, "/Bitor/", "B6", '#NAME?').
?test(sheet1_C6, "/Bitor/", "C6", '#NAME?').
?test(sheet1_D6, "/Bitor/", "D6", '#NAME?').
?test(sheet1_E6, "/Bitor/", "E6", '#NAME?').
?test(sheet1_F6, "/Bitor/", "F6", '#NAME?').
?test(sheet1_G6, "/Bitor/", "G6", '#NAME?').
?test(sheet1_H6, "/Bitor/", "H6", '#NAME?').
?test(sheet1_I6, "/Bitor/", "I6", '#NAME?').
?test(sheet1_J6, "/Bitor/", "J6", '#NAME?').
?test(sheet1_K6, "/Bitor/", "K6", '#NAME?').
?test(sheet1_L6, "/Bitor/", "L6", '#NAME?').
?test(sheet1_M6, "/Bitor/", "M6", '#NAME?').
?test(sheet1_N6, "/Bitor/", "N6", '#NAME?').
?test(sheet1_O6, "/Bitor/", "O6", '#NAME?').
?test(sheet1_P6, "/Bitor/", "P6", '#NAME?').
?test(sheet1_Q6, "/Bitor/", "Q6", '#NAME?').
?test(sheet1_R6, "/Bitor/", "R6", '#NAME?').
?test(sheet1_S6, "/Bitor/", "S6", '#NAME?').
?test(sheet1_T6, "/Bitor/", "T6", '#NAME?').
?test(sheet1_U6, "/Bitor/", "U6", '#NAME?').
?test(sheet1_V6, "/Bitor/", "V6", '#NAME?').
?test(sheet1_X6, "/Bitor/", "X6", 10.0).
?test(sheet1_Y6, "/Bitor/", "Y6", 17.0).
?test(sheet1_Z6, "/Bitor/", "Z6", 24.0).
?test(sheet1_AA6, "/Bitor/", "AA6", 31.0).
?test(sheet1_A7, "/Bitor/", "A7", "errors").
?test(sheet1_B7, "/Bitor/", "B7", '#NUM!').
?test(sheet1_C7, "/Bitor/", "C7", '#NAME?').
?test(sheet1_D7, "/Bitor/", "D7", '#NAME?').
?test(sheet1_E7, "/Bitor/", "E7", '#NAME?').
?test(sheet1_F7, "/Bitor/", "F7", '#NAME?').
?test(sheet1_G7, "/Bitor/", "G7", '#NAME?').
?test(sheet1_H7, "/Bitor/", "H7", '#NAME?').
?test(sheet1_I7, "/Bitor/", "I7", '#NAME?').
?test(sheet1_J7, "/Bitor/", "J7", '#NAME?').
?test(sheet1_K7, "/Bitor/", "K7", '#NAME?').
?test(sheet1_L7, "/Bitor/", "L7", '#NAME?').
?test(sheet1_M7, "/Bitor/", "M7", '#NAME?').
?test(sheet1_N7, "/Bitor/", "N7", '#NAME?').
?test(sheet1_O7, "/Bitor/", "O7", '#NAME?').
?test(sheet1_P7, "/Bitor/", "P7", '#NAME?').
?test(sheet1_Q7, "/Bitor/", "Q7", '#NAME?').
?test(sheet1_R7, "/Bitor/", "R7", '#NAME?').
?test(sheet1_S7, "/Bitor/", "S7", '#NAME?').
?test(sheet1_T7, "/Bitor/", "T7", '#NAME?').
?test(sheet1_U7, "/Bitor/", "U7", '#NAME?').
?test(sheet1_V7, "/Bitor/", "V7", '#NAME?').
?test(sheet1_A8, "/Bitor/", "A8", "errors").
?test(sheet1_B8, "/Bitor/", "B8", '#N/A').
?test(sheet1_C8, "/Bitor/", "C8", '#NAME?').
?test(sheet1_D8, "/Bitor/", "D8", '#NAME?').
?test(sheet1_E8, "/Bitor/", "E8", '#NAME?').
?test(sheet1_F8, "/Bitor/", "F8", '#NAME?').
?test(sheet1_G8, "/Bitor/", "G8", '#NAME?').
?test(sheet1_H8, "/Bitor/", "H8", '#NAME?').
?test(sheet1_I8, "/Bitor/", "I8", '#NAME?').
?test(sheet1_J8, "/Bitor/", "J8", '#NAME?').
?test(sheet1_K8, "/Bitor/", "K8", '#NAME?').
?test(sheet1_L8, "/Bitor/", "L8", '#NAME?').
?test(sheet1_M8, "/Bitor/", "M8", '#NAME?').
?test(sheet1_N8, "/Bitor/", "N8", '#NAME?').
?test(sheet1_O8, "/Bitor/", "O8", '#NAME?').
?test(sheet1_P8, "/Bitor/", "P8", '#NAME?').
?test(sheet1_Q8, "/Bitor/", "Q8", '#NAME?').
?test(sheet1_R8, "/Bitor/", "R8", '#NAME?').
?test(sheet1_S8, "/Bitor/", "S8", '#NAME?').
?test(sheet1_T8, "/Bitor/", "T8", '#NAME?').
?test(sheet1_U8, "/Bitor/", "U8", '#NAME?').
?test(sheet1_V8, "/Bitor/", "V8", '#NAME?').
?test(sheet1_A9, "/Bitor/", "A9", "String").
?test(sheet1_B9, "/Bitor/", "B9", "Phillip").
?test(sheet1_C9, "/Bitor/", "C9", '#NAME?').
?test(sheet1_D9, "/Bitor/", "D9", '#NAME?').
?test(sheet1_E9, "/Bitor/", "E9", '#NAME?').
?test(sheet1_F9, "/Bitor/", "F9", '#NAME?').
?test(sheet1_G9, "/Bitor/", "G9", '#NAME?').
?test(sheet1_H9, "/Bitor/", "H9", '#NAME?').
?test(sheet1_I9, "/Bitor/", "I9", '#NAME?').
?test(sheet1_J9, "/Bitor/", "J9", '#NAME?').
?test(sheet1_K9, "/Bitor/", "K9", '#NAME?').
?test(sheet1_L9, "/Bitor/", "L9", '#NAME?').
?test(sheet1_M9, "/Bitor/", "M9", '#NAME?').
?test(sheet1_N9, "/Bitor/", "N9", '#NAME?').
?test(sheet1_O9, "/Bitor/", "O9", '#NAME?').
?test(sheet1_P9, "/Bitor/", "P9", '#NAME?').
?test(sheet1_Q9, "/Bitor/", "Q9", '#NAME?').
?test(sheet1_R9, "/Bitor/", "R9", '#NAME?').
?test(sheet1_S9, "/Bitor/", "S9", '#NAME?').
?test(sheet1_T9, "/Bitor/", "T9", '#NAME?').
?test(sheet1_U9, "/Bitor/", "U9", '#NAME?').
?test(sheet1_V9, "/Bitor/", "V9", '#NAME?').
?test(sheet1_A10, "/Bitor/", "A10", "String Number").
?test(sheet1_B10, "/Bitor/", "B10", "12").
?test(sheet1_C10, "/Bitor/", "C10", '#NAME?').
?test(sheet1_D10, "/Bitor/", "D10", '#NAME?').
?test(sheet1_E10, "/Bitor/", "E10", '#NAME?').
?test(sheet1_F10, "/Bitor/", "F10", '#NAME?').
?test(sheet1_G10, "/Bitor/", "G10", '#NAME?').
?test(sheet1_H10, "/Bitor/", "H10", '#NAME?').
?test(sheet1_I10, "/Bitor/", "I10", '#NAME?').
?test(sheet1_J10, "/Bitor/", "J10", '#NAME?').
?test(sheet1_K10, "/Bitor/", "K10", '#NAME?').
?test(sheet1_L10, "/Bitor/", "L10", '#NAME?').
?test(sheet1_M10, "/Bitor/", "M10", '#NAME?').
?test(sheet1_N10, "/Bitor/", "N10", '#NAME?').
?test(sheet1_O10, "/Bitor/", "O10", '#NAME?').
?test(sheet1_P10, "/Bitor/", "P10", '#NAME?').
?test(sheet1_Q10, "/Bitor/", "Q10", '#NAME?').
?test(sheet1_R10, "/Bitor/", "R10", '#NAME?').
?test(sheet1_S10, "/Bitor/", "S10", '#NAME?').
?test(sheet1_T10, "/Bitor/", "T10", '#NAME?').
?test(sheet1_U10, "/Bitor/", "U10", '#NAME?').
?test(sheet1_V10, "/Bitor/", "V10", '#NAME?').
?test(sheet1_A11, "/Bitor/", "A11", "String Number Leading space").
?test(sheet1_B11, "/Bitor/", "B11", " 23").
?test(sheet1_C11, "/Bitor/", "C11", '#NAME?').
?test(sheet1_D11, "/Bitor/", "D11", '#NAME?').
?test(sheet1_E11, "/Bitor/", "E11", '#NAME?').
?test(sheet1_F11, "/Bitor/", "F11", '#NAME?').
?test(sheet1_G11, "/Bitor/", "G11", '#NAME?').
?test(sheet1_H11, "/Bitor/", "H11", '#NAME?').
?test(sheet1_I11, "/Bitor/", "I11", '#NAME?').
?test(sheet1_J11, "/Bitor/", "J11", '#NAME?').
?test(sheet1_K11, "/Bitor/", "K11", '#NAME?').
?test(sheet1_L11, "/Bitor/", "L11", '#NAME?').
?test(sheet1_M11, "/Bitor/", "M11", '#NAME?').
?test(sheet1_N11, "/Bitor/", "N11", '#NAME?').
?test(sheet1_O11, "/Bitor/", "O11", '#NAME?').
?test(sheet1_P11, "/Bitor/", "P11", '#NAME?').
?test(sheet1_Q11, "/Bitor/", "Q11", '#NAME?').
?test(sheet1_R11, "/Bitor/", "R11", '#NAME?').
?test(sheet1_S11, "/Bitor/", "S11", '#NAME?').
?test(sheet1_T11, "/Bitor/", "T11", '#NAME?').
?test(sheet1_U11, "/Bitor/", "U11", '#NAME?').
?test(sheet1_V11, "/Bitor/", "V11", '#NAME?').
?test(sheet1_A12, "/Bitor/", "A12", "Interger").
?test(sheet1_B12, "/Bitor/", "B12", "1968/03/23 00:00:00").
?test(sheet1_C12, "/Bitor/", "C12", '#NAME?').
?test(sheet1_D12, "/Bitor/", "D12", '#NAME?').
?test(sheet1_E12, "/Bitor/", "E12", '#NAME?').
?test(sheet1_F12, "/Bitor/", "F12", '#NAME?').
?test(sheet1_G12, "/Bitor/", "G12", '#NAME?').
?test(sheet1_H12, "/Bitor/", "H12", '#NAME?').
?test(sheet1_I12, "/Bitor/", "I12", '#NAME?').
?test(sheet1_J12, "/Bitor/", "J12", '#NAME?').
?test(sheet1_K12, "/Bitor/", "K12", '#NAME?').
?test(sheet1_L12, "/Bitor/", "L12", '#NAME?').
?test(sheet1_M12, "/Bitor/", "M12", '#NAME?').
?test(sheet1_N12, "/Bitor/", "N12", '#NAME?').
?test(sheet1_O12, "/Bitor/", "O12", '#NAME?').
?test(sheet1_P12, "/Bitor/", "P12", '#NAME?').
?test(sheet1_Q12, "/Bitor/", "Q12", '#NAME?').
?test(sheet1_R12, "/Bitor/", "R12", '#NAME?').
?test(sheet1_S12, "/Bitor/", "S12", '#NAME?').
?test(sheet1_T12, "/Bitor/", "T12", '#NAME?').
?test(sheet1_U12, "/Bitor/", "U12", '#NAME?').
?test(sheet1_V12, "/Bitor/", "V12", '#NAME?').
?test(sheet1_A13, "/Bitor/", "A13", "Float").
?test(sheet1_B13, "/Bitor/", "B13", 3.14159265358979).
?test(sheet1_C13, "/Bitor/", "C13", '#NAME?').
?test(sheet1_D13, "/Bitor/", "D13", '#NAME?').
?test(sheet1_E13, "/Bitor/", "E13", '#NAME?').
?test(sheet1_F13, "/Bitor/", "F13", '#NAME?').
?test(sheet1_G13, "/Bitor/", "G13", '#NAME?').
?test(sheet1_H13, "/Bitor/", "H13", '#NAME?').
?test(sheet1_I13, "/Bitor/", "I13", '#NAME?').
?test(sheet1_J13, "/Bitor/", "J13", '#NAME?').
?test(sheet1_K13, "/Bitor/", "K13", '#NAME?').
?test(sheet1_L13, "/Bitor/", "L13", '#NAME?').
?test(sheet1_M13, "/Bitor/", "M13", '#NAME?').
?test(sheet1_N13, "/Bitor/", "N13", '#NAME?').
?test(sheet1_O13, "/Bitor/", "O13", '#NAME?').
?test(sheet1_P13, "/Bitor/", "P13", '#NAME?').
?test(sheet1_Q13, "/Bitor/", "Q13", '#NAME?').
?test(sheet1_R13, "/Bitor/", "R13", '#NAME?').
?test(sheet1_S13, "/Bitor/", "S13", '#NAME?').
?test(sheet1_T13, "/Bitor/", "T13", '#NAME?').
?test(sheet1_U13, "/Bitor/", "U13", '#NAME?').
?test(sheet1_V13, "/Bitor/", "V13", '#NAME?').
?test(sheet1_A14, "/Bitor/", "A14", "Blank").
?test(sheet1_C14, "/Bitor/", "C14", '#NAME?').
?test(sheet1_D14, "/Bitor/", "D14", '#NAME?').
?test(sheet1_E14, "/Bitor/", "E14", '#NAME?').
?test(sheet1_F14, "/Bitor/", "F14", '#NAME?').
?test(sheet1_G14, "/Bitor/", "G14", '#NAME?').
?test(sheet1_H14, "/Bitor/", "H14", '#NAME?').
?test(sheet1_I14, "/Bitor/", "I14", '#NAME?').
?test(sheet1_J14, "/Bitor/", "J14", '#NAME?').
?test(sheet1_K14, "/Bitor/", "K14", '#NAME?').
?test(sheet1_L14, "/Bitor/", "L14", '#NAME?').
?test(sheet1_M14, "/Bitor/", "M14", '#NAME?').
?test(sheet1_N14, "/Bitor/", "N14", '#NAME?').
?test(sheet1_O14, "/Bitor/", "O14", '#NAME?').
?test(sheet1_P14, "/Bitor/", "P14", '#NAME?').
?test(sheet1_Q14, "/Bitor/", "Q14", '#NAME?').
?test(sheet1_R14, "/Bitor/", "R14", '#NAME?').
?test(sheet1_S14, "/Bitor/", "S14", '#NAME?').
?test(sheet1_T14, "/Bitor/", "T14", '#NAME?').
?test(sheet1_U14, "/Bitor/", "U14", '#NAME?').
?test(sheet1_V14, "/Bitor/", "V14", '#NAME?').
?test(sheet1_A15, "/Bitor/", "A15", "Logical").
?test(sheet1_B15, "/Bitor/", "B15", true).
?test(sheet1_C15, "/Bitor/", "C15", '#NAME?').
?test(sheet1_D15, "/Bitor/", "D15", '#NAME?').
?test(sheet1_E15, "/Bitor/", "E15", '#NAME?').
?test(sheet1_F15, "/Bitor/", "F15", '#NAME?').
?test(sheet1_G15, "/Bitor/", "G15", '#NAME?').
?test(sheet1_H15, "/Bitor/", "H15", '#NAME?').
?test(sheet1_I15, "/Bitor/", "I15", '#NAME?').
?test(sheet1_J15, "/Bitor/", "J15", '#NAME?').
?test(sheet1_K15, "/Bitor/", "K15", '#NAME?').
?test(sheet1_L15, "/Bitor/", "L15", '#NAME?').
?test(sheet1_M15, "/Bitor/", "M15", '#NAME?').
?test(sheet1_N15, "/Bitor/", "N15", '#NAME?').
?test(sheet1_O15, "/Bitor/", "O15", '#NAME?').
?test(sheet1_P15, "/Bitor/", "P15", '#NAME?').
?test(sheet1_Q15, "/Bitor/", "Q15", '#NAME?').
?test(sheet1_R15, "/Bitor/", "R15", '#NAME?').
?test(sheet1_S15, "/Bitor/", "S15", '#NAME?').
?test(sheet1_T15, "/Bitor/", "T15", '#NAME?').
?test(sheet1_U15, "/Bitor/", "U15", '#NAME?').
?test(sheet1_V15, "/Bitor/", "V15", '#NAME?').
?test(sheet1_A16, "/Bitor/", "A16", "Logical").
?test(sheet1_B16, "/Bitor/", "B16", false).
?test(sheet1_C16, "/Bitor/", "C16", '#NAME?').
?test(sheet1_D16, "/Bitor/", "D16", '#NAME?').
?test(sheet1_E16, "/Bitor/", "E16", '#NAME?').
?test(sheet1_F16, "/Bitor/", "F16", '#NAME?').
?test(sheet1_G16, "/Bitor/", "G16", '#NAME?').
?test(sheet1_H16, "/Bitor/", "H16", '#NAME?').
?test(sheet1_I16, "/Bitor/", "I16", '#NAME?').
?test(sheet1_J16, "/Bitor/", "J16", '#NAME?').
?test(sheet1_K16, "/Bitor/", "K16", '#NAME?').
?test(sheet1_L16, "/Bitor/", "L16", '#NAME?').
?test(sheet1_M16, "/Bitor/", "M16", '#NAME?').
?test(sheet1_N16, "/Bitor/", "N16", '#NAME?').
?test(sheet1_O16, "/Bitor/", "O16", '#NAME?').
?test(sheet1_P16, "/Bitor/", "P16", '#NAME?').
?test(sheet1_Q16, "/Bitor/", "Q16", '#NAME?').
?test(sheet1_R16, "/Bitor/", "R16", '#NAME?').
?test(sheet1_S16, "/Bitor/", "S16", '#NAME?').
?test(sheet1_T16, "/Bitor/", "T16", '#NAME?').
?test(sheet1_U16, "/Bitor/", "U16", '#NAME?').
?test(sheet1_V16, "/Bitor/", "V16", '#NAME?').
?test(sheet1_A17, "/Bitor/", "A17", "Range Row").
?test(sheet1_B17, "/Bitor/", "B17", "X3:Y3").
?test(sheet1_C17, "/Bitor/", "C17", '#NAME?').
?test(sheet1_D17, "/Bitor/", "D17", '#NAME?').
?test(sheet1_E17, "/Bitor/", "E17", '#NAME?').
?test(sheet1_F17, "/Bitor/", "F17", '#NAME?').
?test(sheet1_G17, "/Bitor/", "G17", '#NAME?').
?test(sheet1_H17, "/Bitor/", "H17", '#NAME?').
?test(sheet1_I17, "/Bitor/", "I17", '#NAME?').
?test(sheet1_J17, "/Bitor/", "J17", '#NAME?').
?test(sheet1_K17, "/Bitor/", "K17", '#NAME?').
?test(sheet1_L17, "/Bitor/", "L17", '#NAME?').
?test(sheet1_M17, "/Bitor/", "M17", '#NAME?').
?test(sheet1_N17, "/Bitor/", "N17", '#NAME?').
?test(sheet1_O17, "/Bitor/", "O17", '#NAME?').
?test(sheet1_P17, "/Bitor/", "P17", '#NAME?').
?test(sheet1_Q17, "/Bitor/", "Q17", '#NAME?').
?test(sheet1_R17, "/Bitor/", "R17", '#NAME?').
?test(sheet1_S17, "/Bitor/", "S17", '#NAME?').
?test(sheet1_T17, "/Bitor/", "T17", '#NAME?').
?test(sheet1_U17, "/Bitor/", "U17", '#NAME?').
?test(sheet1_V17, "/Bitor/", "V17", '#NAME?').
?test(sheet1_A18, "/Bitor/", "A18", "Range Row").
?test(sheet1_B18, "/Bitor/", "B18", "X3:AA3").
?test(sheet1_C18, "/Bitor/", "C18", '#NAME?').
?test(sheet1_D18, "/Bitor/", "D18", '#NAME?').
?test(sheet1_E18, "/Bitor/", "E18", '#NAME?').
?test(sheet1_F18, "/Bitor/", "F18", '#NAME?').
?test(sheet1_G18, "/Bitor/", "G18", '#NAME?').
?test(sheet1_H18, "/Bitor/", "H18", '#NAME?').
?test(sheet1_I18, "/Bitor/", "I18", '#NAME?').
?test(sheet1_J18, "/Bitor/", "J18", '#NAME?').
?test(sheet1_K18, "/Bitor/", "K18", '#NAME?').
?test(sheet1_L18, "/Bitor/", "L18", '#NAME?').
?test(sheet1_M18, "/Bitor/", "M18", '#NAME?').
?test(sheet1_N18, "/Bitor/", "N18", '#NAME?').
?test(sheet1_O18, "/Bitor/", "O18", '#NAME?').
?test(sheet1_P18, "/Bitor/", "P18", '#NAME?').
?test(sheet1_Q18, "/Bitor/", "Q18", '#NAME?').
?test(sheet1_R18, "/Bitor/", "R18", '#NAME?').
?test(sheet1_S18, "/Bitor/", "S18", '#NAME?').
?test(sheet1_T18, "/Bitor/", "T18", '#NAME?').
?test(sheet1_U18, "/Bitor/", "U18", '#NAME?').
?test(sheet1_V18, "/Bitor/", "V18", '#NAME?').
?test(sheet1_A19, "/Bitor/", "A19", "Range Area").
?test(sheet1_B19, "/Bitor/", "B19", "X3:Y4").
?test(sheet1_C19, "/Bitor/", "C19", '#NAME?').
?test(sheet1_D19, "/Bitor/", "D19", '#NAME?').
?test(sheet1_E19, "/Bitor/", "E19", '#NAME?').
?test(sheet1_F19, "/Bitor/", "F19", '#NAME?').
?test(sheet1_G19, "/Bitor/", "G19", '#NAME?').
?test(sheet1_H19, "/Bitor/", "H19", '#NAME?').
?test(sheet1_I19, "/Bitor/", "I19", '#NAME?').
?test(sheet1_J19, "/Bitor/", "J19", '#NAME?').
?test(sheet1_K19, "/Bitor/", "K19", '#NAME?').
?test(sheet1_L19, "/Bitor/", "L19", '#NAME?').
?test(sheet1_M19, "/Bitor/", "M19", '#NAME?').
?test(sheet1_N19, "/Bitor/", "N19", '#NAME?').
?test(sheet1_O19, "/Bitor/", "O19", '#NAME?').
?test(sheet1_P19, "/Bitor/", "P19", '#NAME?').
?test(sheet1_Q19, "/Bitor/", "Q19", '#NAME?').
?test(sheet1_R19, "/Bitor/", "R19", '#NAME?').
?test(sheet1_S19, "/Bitor/", "S19", '#NAME?').
?test(sheet1_T19, "/Bitor/", "T19", '#NAME?').
?test(sheet1_U19, "/Bitor/", "U19", '#NAME?').
?test(sheet1_V19, "/Bitor/", "V19", '#NAME?').
?test(sheet1_A20, "/Bitor/", "A20", "Range Area").
?test(sheet1_B20, "/Bitor/", "B20", "X3:AA6").
?test(sheet1_C20, "/Bitor/", "C20", '#NAME?').
?test(sheet1_D20, "/Bitor/", "D20", '#NAME?').
?test(sheet1_E20, "/Bitor/", "E20", '#NAME?').
?test(sheet1_F20, "/Bitor/", "F20", '#NAME?').
?test(sheet1_G20, "/Bitor/", "G20", '#NAME?').
?test(sheet1_H20, "/Bitor/", "H20", '#NAME?').
?test(sheet1_I20, "/Bitor/", "I20", '#NAME?').
?test(sheet1_J20, "/Bitor/", "J20", '#NAME?').
?test(sheet1_K20, "/Bitor/", "K20", '#NAME?').
?test(sheet1_L20, "/Bitor/", "L20", '#NAME?').
?test(sheet1_M20, "/Bitor/", "M20", '#NAME?').
?test(sheet1_N20, "/Bitor/", "N20", '#NAME?').
?test(sheet1_O20, "/Bitor/", "O20", '#NAME?').
?test(sheet1_P20, "/Bitor/", "P20", '#NAME?').
?test(sheet1_Q20, "/Bitor/", "Q20", '#NAME?').
?test(sheet1_R20, "/Bitor/", "R20", '#NAME?').
?test(sheet1_S20, "/Bitor/", "S20", '#NAME?').
?test(sheet1_T20, "/Bitor/", "T20", '#NAME?').
?test(sheet1_U20, "/Bitor/", "U20", '#NAME?').
?test(sheet1_V20, "/Bitor/", "V20", '#NAME?').
?test(sheet1_A21, "/Bitor/", "A21", "Range Colunm").
?test(sheet1_B21, "/Bitor/", "B21", "X3:X4").
?test(sheet1_C21, "/Bitor/", "C21", '#NAME?').
?test(sheet1_D21, "/Bitor/", "D21", '#NAME?').
?test(sheet1_E21, "/Bitor/", "E21", '#NAME?').
?test(sheet1_F21, "/Bitor/", "F21", '#NAME?').
?test(sheet1_G21, "/Bitor/", "G21", '#NAME?').
?test(sheet1_H21, "/Bitor/", "H21", '#NAME?').
?test(sheet1_I21, "/Bitor/", "I21", '#NAME?').
?test(sheet1_J21, "/Bitor/", "J21", '#NAME?').
?test(sheet1_K21, "/Bitor/", "K21", '#NAME?').
?test(sheet1_L21, "/Bitor/", "L21", '#NAME?').
?test(sheet1_M21, "/Bitor/", "M21", '#NAME?').
?test(sheet1_N21, "/Bitor/", "N21", '#NAME?').
?test(sheet1_O21, "/Bitor/", "O21", '#NAME?').
?test(sheet1_P21, "/Bitor/", "P21", '#NAME?').
?test(sheet1_Q21, "/Bitor/", "Q21", '#NAME?').
?test(sheet1_R21, "/Bitor/", "R21", '#NAME?').
?test(sheet1_S21, "/Bitor/", "S21", '#NAME?').
?test(sheet1_T21, "/Bitor/", "T21", '#NAME?').
?test(sheet1_U21, "/Bitor/", "U21", '#NAME?').
?test(sheet1_V21, "/Bitor/", "V21", '#NAME?').
?test(sheet1_A22, "/Bitor/", "A22", "Range Colunm").
?test(sheet1_B22, "/Bitor/", "B22", "X3:X6").
?test(sheet1_C22, "/Bitor/", "C22", '#NAME?').
?test(sheet1_D22, "/Bitor/", "D22", '#NAME?').
?test(sheet1_E22, "/Bitor/", "E22", '#NAME?').
?test(sheet1_F22, "/Bitor/", "F22", '#NAME?').
?test(sheet1_G22, "/Bitor/", "G22", '#NAME?').
?test(sheet1_H22, "/Bitor/", "H22", '#NAME?').
?test(sheet1_I22, "/Bitor/", "I22", '#NAME?').
?test(sheet1_J22, "/Bitor/", "J22", '#NAME?').
?test(sheet1_K22, "/Bitor/", "K22", '#NAME?').
?test(sheet1_L22, "/Bitor/", "L22", '#NAME?').
?test(sheet1_M22, "/Bitor/", "M22", '#NAME?').
?test(sheet1_N22, "/Bitor/", "N22", '#NAME?').
?test(sheet1_O22, "/Bitor/", "O22", '#NAME?').
?test(sheet1_P22, "/Bitor/", "P22", '#NAME?').
?test(sheet1_Q22, "/Bitor/", "Q22", '#NAME?').
?test(sheet1_R22, "/Bitor/", "R22", '#NAME?').
?test(sheet1_S22, "/Bitor/", "S22", '#NAME?').
?test(sheet1_T22, "/Bitor/", "T22", '#NAME?').
?test(sheet1_U22, "/Bitor/", "U22", '#NAME?').
?test(sheet1_V22, "/Bitor/", "V22", '#NAME?').
?test(sheet1_A25, "/Bitor/", "A25", "bitor(A,B)").
?test(sheet1_B25, "/Bitor/", "B25", "B").
?test(sheet1_C25, "/Bitor/", "C25", "errors").
?test(sheet1_D25, "/Bitor/", "D25", "errors").
?test(sheet1_E25, "/Bitor/", "E25", "errors").
?test(sheet1_F25, "/Bitor/", "F25", "errors").
?test(sheet1_G25, "/Bitor/", "G25", "errors").
?test(sheet1_H25, "/Bitor/", "H25", "errors").
?test(sheet1_I25, "/Bitor/", "I25", "String").
?test(sheet1_J25, "/Bitor/", "J25", "String Number").
?test(sheet1_K25, "/Bitor/", "K25", "String number Leading space").
?test(sheet1_L25, "/Bitor/", "L25", "Integer").
?test(sheet1_M25, "/Bitor/", "M25", "Float").
?test(sheet1_N25, "/Bitor/", "N25", "Blank").
?test(sheet1_O25, "/Bitor/", "O25", "Logical").
?test(sheet1_P25, "/Bitor/", "P25", "Logical").
?test(sheet1_Q25, "/Bitor/", "Q25", "Range Row").
?test(sheet1_R25, "/Bitor/", "R25", "Range Row").
?test(sheet1_S25, "/Bitor/", "S25", "Range Area").
?test(sheet1_T25, "/Bitor/", "T25", "Range Area").
?test(sheet1_U25, "/Bitor/", "U25", "Range Colunm").
?test(sheet1_V25, "/Bitor/", "V25", "Range Colunm").
?test(sheet1_A26, "/Bitor/", "A26", "A").
?test(sheet1_C26, "/Bitor/", "C26", '#DIV/0!').
?test(sheet1_D26, "/Bitor/", "D26", '#VALUE!').
?test(sheet1_E26, "/Bitor/", "E26", '#REF!').
?test(sheet1_F26, "/Bitor/", "F26", '#NAME?').
?test(sheet1_G26, "/Bitor/", "G26", '#NUM!').
?test(sheet1_H26, "/Bitor/", "H26", '#N/A').
?test(sheet1_I26, "/Bitor/", "I26", "Phillip").
?test(sheet1_J26, "/Bitor/", "J26", "13").
?test(sheet1_K26, "/Bitor/", "K26", " 24").
?test(sheet1_L26, "/Bitor/", "L26", "1968/03/23 00:00:00").
?test(sheet1_M26, "/Bitor/", "M26", 3.14159265358979).
?test(sheet1_O26, "/Bitor/", "O26", true).
?test(sheet1_P26, "/Bitor/", "P26", false).
?test(sheet1_Q26, "/Bitor/", "Q26", "X3:Y3").
?test(sheet1_R26, "/Bitor/", "R26", "X3:AA3").
?test(sheet1_S26, "/Bitor/", "S26", "X3:Y4").
?test(sheet1_T26, "/Bitor/", "T26", "X3:AA6").
?test(sheet1_U26, "/Bitor/", "U26", "X3:X4").
?test(sheet1_V26, "/Bitor/", "V26", "X3:X6").
?test(sheet1_A27, "/Bitor/", "A27", "errors").
?test(sheet1_B27, "/Bitor/", "B27", '#DIV/0!').
?test(sheet1_C27, "/Bitor/", "C27", '#DIV/0!').
?test(sheet1_D27, "/Bitor/", "D27", '#DIV/0!').
?test(sheet1_E27, "/Bitor/", "E27", '#DIV/0!').
?test(sheet1_F27, "/Bitor/", "F27", '#DIV/0!').
?test(sheet1_G27, "/Bitor/", "G27", '#DIV/0!').
?test(sheet1_H27, "/Bitor/", "H27", '#DIV/0!').
?test(sheet1_I27, "/Bitor/", "I27", '#DIV/0!').
?test(sheet1_J27, "/Bitor/", "J27", '#DIV/0!').
?test(sheet1_K27, "/Bitor/", "K27", '#DIV/0!').
?test(sheet1_L27, "/Bitor/", "L27", '#DIV/0!').
?test(sheet1_M27, "/Bitor/", "M27", '#DIV/0!').
?test(sheet1_N27, "/Bitor/", "N27", '#DIV/0!').
?test(sheet1_O27, "/Bitor/", "O27", '#DIV/0!').
?test(sheet1_P27, "/Bitor/", "P27", '#DIV/0!').
?test(sheet1_Q27, "/Bitor/", "Q27", '#DIV/0!').
?test(sheet1_R27, "/Bitor/", "R27", '#DIV/0!').
?test(sheet1_S27, "/Bitor/", "S27", '#DIV/0!').
?test(sheet1_T27, "/Bitor/", "T27", '#DIV/0!').
?test(sheet1_U27, "/Bitor/", "U27", '#DIV/0!').
?test(sheet1_V27, "/Bitor/", "V27", '#DIV/0!').
?test(sheet1_A28, "/Bitor/", "A28", "errors").
?test(sheet1_B28, "/Bitor/", "B28", '#VALUE!').
?test(sheet1_C28, "/Bitor/", "C28", '#VALUE!').
?test(sheet1_D28, "/Bitor/", "D28", '#VALUE!').
?test(sheet1_E28, "/Bitor/", "E28", '#VALUE!').
?test(sheet1_F28, "/Bitor/", "F28", '#VALUE!').
?test(sheet1_G28, "/Bitor/", "G28", '#VALUE!').
?test(sheet1_H28, "/Bitor/", "H28", '#VALUE!').
?test(sheet1_I28, "/Bitor/", "I28", '#VALUE!').
?test(sheet1_J28, "/Bitor/", "J28", '#VALUE!').
?test(sheet1_K28, "/Bitor/", "K28", '#VALUE!').
?test(sheet1_L28, "/Bitor/", "L28", '#VALUE!').
?test(sheet1_M28, "/Bitor/", "M28", '#VALUE!').
?test(sheet1_N28, "/Bitor/", "N28", '#VALUE!').
?test(sheet1_O28, "/Bitor/", "O28", '#VALUE!').
?test(sheet1_P28, "/Bitor/", "P28", '#VALUE!').
?test(sheet1_Q28, "/Bitor/", "Q28", '#VALUE!').
?test(sheet1_R28, "/Bitor/", "R28", '#VALUE!').
?test(sheet1_S28, "/Bitor/", "S28", '#VALUE!').
?test(sheet1_T28, "/Bitor/", "T28", '#VALUE!').
?test(sheet1_U28, "/Bitor/", "U28", '#VALUE!').
?test(sheet1_V28, "/Bitor/", "V28", '#VALUE!').
?test(sheet1_A29, "/Bitor/", "A29", "errors").
?test(sheet1_B29, "/Bitor/", "B29", '#REF!').
?test(sheet1_C29, "/Bitor/", "C29", '#REF!').
?test(sheet1_D29, "/Bitor/", "D29", '#REF!').
?test(sheet1_E29, "/Bitor/", "E29", '#REF!').
?test(sheet1_F29, "/Bitor/", "F29", '#REF!').
?test(sheet1_G29, "/Bitor/", "G29", '#REF!').
?test(sheet1_H29, "/Bitor/", "H29", '#REF!').
?test(sheet1_I29, "/Bitor/", "I29", '#REF!').
?test(sheet1_J29, "/Bitor/", "J29", '#REF!').
?test(sheet1_K29, "/Bitor/", "K29", '#REF!').
?test(sheet1_L29, "/Bitor/", "L29", '#REF!').
?test(sheet1_M29, "/Bitor/", "M29", '#REF!').
?test(sheet1_N29, "/Bitor/", "N29", '#REF!').
?test(sheet1_O29, "/Bitor/", "O29", '#REF!').
?test(sheet1_P29, "/Bitor/", "P29", '#REF!').
?test(sheet1_Q29, "/Bitor/", "Q29", '#REF!').
?test(sheet1_R29, "/Bitor/", "R29", '#REF!').
?test(sheet1_S29, "/Bitor/", "S29", '#REF!').
?test(sheet1_T29, "/Bitor/", "T29", '#REF!').
?test(sheet1_U29, "/Bitor/", "U29", '#REF!').
?test(sheet1_V29, "/Bitor/", "V29", '#REF!').
?test(sheet1_A30, "/Bitor/", "A30", "errors").
?test(sheet1_B30, "/Bitor/", "B30", '#NAME?').
?test(sheet1_C30, "/Bitor/", "C30", '#NAME?').
?test(sheet1_D30, "/Bitor/", "D30", '#NAME?').
?test(sheet1_E30, "/Bitor/", "E30", '#NAME?').
?test(sheet1_F30, "/Bitor/", "F30", '#NAME?').
?test(sheet1_G30, "/Bitor/", "G30", '#NAME?').
?test(sheet1_H30, "/Bitor/", "H30", '#NAME?').
?test(sheet1_I30, "/Bitor/", "I30", '#NAME?').
?test(sheet1_J30, "/Bitor/", "J30", '#NAME?').
?test(sheet1_K30, "/Bitor/", "K30", '#NAME?').
?test(sheet1_L30, "/Bitor/", "L30", '#NAME?').
?test(sheet1_M30, "/Bitor/", "M30", '#NAME?').
?test(sheet1_N30, "/Bitor/", "N30", '#NAME?').
?test(sheet1_O30, "/Bitor/", "O30", '#NAME?').
?test(sheet1_P30, "/Bitor/", "P30", '#NAME?').
?test(sheet1_Q30, "/Bitor/", "Q30", '#NAME?').
?test(sheet1_R30, "/Bitor/", "R30", '#NAME?').
?test(sheet1_S30, "/Bitor/", "S30", '#NAME?').
?test(sheet1_T30, "/Bitor/", "T30", '#NAME?').
?test(sheet1_U30, "/Bitor/", "U30", '#NAME?').
?test(sheet1_V30, "/Bitor/", "V30", '#NAME?').
?test(sheet1_A31, "/Bitor/", "A31", "errors").
?test(sheet1_B31, "/Bitor/", "B31", '#NUM!').
?test(sheet1_C31, "/Bitor/", "C31", '#NUM!').
?test(sheet1_D31, "/Bitor/", "D31", '#NUM!').
?test(sheet1_E31, "/Bitor/", "E31", '#NUM!').
?test(sheet1_F31, "/Bitor/", "F31", '#NUM!').
?test(sheet1_G31, "/Bitor/", "G31", '#NUM!').
?test(sheet1_H31, "/Bitor/", "H31", '#NUM!').
?test(sheet1_I31, "/Bitor/", "I31", '#NUM!').
?test(sheet1_J31, "/Bitor/", "J31", '#NUM!').
?test(sheet1_K31, "/Bitor/", "K31", '#NUM!').
?test(sheet1_L31, "/Bitor/", "L31", '#NUM!').
?test(sheet1_M31, "/Bitor/", "M31", '#NUM!').
?test(sheet1_N31, "/Bitor/", "N31", '#NUM!').
?test(sheet1_O31, "/Bitor/", "O31", '#NUM!').
?test(sheet1_P31, "/Bitor/", "P31", '#NUM!').
?test(sheet1_Q31, "/Bitor/", "Q31", '#NUM!').
?test(sheet1_R31, "/Bitor/", "R31", '#NUM!').
?test(sheet1_S31, "/Bitor/", "S31", '#NUM!').
?test(sheet1_T31, "/Bitor/", "T31", '#NUM!').
?test(sheet1_U31, "/Bitor/", "U31", '#NUM!').
?test(sheet1_V31, "/Bitor/", "V31", '#NUM!').
?test(sheet1_A32, "/Bitor/", "A32", "errors").
?test(sheet1_B32, "/Bitor/", "B32", '#N/A').
?test(sheet1_C32, "/Bitor/", "C32", '#N/A').
?test(sheet1_D32, "/Bitor/", "D32", '#N/A').
?test(sheet1_E32, "/Bitor/", "E32", '#N/A').
?test(sheet1_F32, "/Bitor/", "F32", '#N/A').
?test(sheet1_G32, "/Bitor/", "G32", '#N/A').
?test(sheet1_H32, "/Bitor/", "H32", '#N/A').
?test(sheet1_I32, "/Bitor/", "I32", '#N/A').
?test(sheet1_J32, "/Bitor/", "J32", '#N/A').
?test(sheet1_K32, "/Bitor/", "K32", '#N/A').
?test(sheet1_L32, "/Bitor/", "L32", '#N/A').
?test(sheet1_M32, "/Bitor/", "M32", '#N/A').
?test(sheet1_N32, "/Bitor/", "N32", '#N/A').
?test(sheet1_O32, "/Bitor/", "O32", '#N/A').
?test(sheet1_P32, "/Bitor/", "P32", '#N/A').
?test(sheet1_Q32, "/Bitor/", "Q32", '#N/A').
?test(sheet1_R32, "/Bitor/", "R32", '#N/A').
?test(sheet1_S32, "/Bitor/", "S32", '#N/A').
?test(sheet1_T32, "/Bitor/", "T32", '#N/A').
?test(sheet1_U32, "/Bitor/", "U32", '#N/A').
?test(sheet1_V32, "/Bitor/", "V32", '#N/A').
?test(sheet1_A33, "/Bitor/", "A33", "String").
?test(sheet1_B33, "/Bitor/", "B33", "Phillip").
?test(sheet1_C33, "/Bitor/", "C33", '#VALUE!').
?test(sheet1_D33, "/Bitor/", "D33", '#VALUE!').
?test(sheet1_E33, "/Bitor/", "E33", '#VALUE!').
?test(sheet1_F33, "/Bitor/", "F33", '#VALUE!').
?test(sheet1_G33, "/Bitor/", "G33", '#VALUE!').
?test(sheet1_H33, "/Bitor/", "H33", '#VALUE!').
?test(sheet1_I33, "/Bitor/", "I33", '#VALUE!').
?test(sheet1_J33, "/Bitor/", "J33", '#VALUE!').
?test(sheet1_K33, "/Bitor/", "K33", '#VALUE!').
?test(sheet1_L33, "/Bitor/", "L33", '#VALUE!').
?test(sheet1_M33, "/Bitor/", "M33", '#VALUE!').
?test(sheet1_N33, "/Bitor/", "N33", '#VALUE!').
?test(sheet1_O33, "/Bitor/", "O33", '#VALUE!').
?test(sheet1_P33, "/Bitor/", "P33", '#VALUE!').
?test(sheet1_Q33, "/Bitor/", "Q33", '#VALUE!').
?test(sheet1_R33, "/Bitor/", "R33", '#VALUE!').
?test(sheet1_S33, "/Bitor/", "S33", '#VALUE!').
?test(sheet1_T33, "/Bitor/", "T33", '#VALUE!').
?test(sheet1_U33, "/Bitor/", "U33", '#VALUE!').
?test(sheet1_V33, "/Bitor/", "V33", '#VALUE!').
?test(sheet1_A34, "/Bitor/", "A34", "String Number").
?test(sheet1_B34, "/Bitor/", "B34", "12").
?test(sheet1_C34, "/Bitor/", "C34", '#DIV/0!').
?test(sheet1_D34, "/Bitor/", "D34", '#VALUE!').
?test(sheet1_E34, "/Bitor/", "E34", '#REF!').
?test(sheet1_F34, "/Bitor/", "F34", '#NAME?').
?test(sheet1_G34, "/Bitor/", "G34", '#NUM!').
?test(sheet1_H34, "/Bitor/", "H34", '#N/A').
?test(sheet1_I34, "/Bitor/", "I34", '#VALUE!').
?test(sheet1_J34, "/Bitor/", "J34", 13.0).
?test(sheet1_K34, "/Bitor/", "K34", 28.0).
?test(sheet1_L34, "/Bitor/", "L34", 24924.0).
?test(sheet1_M34, "/Bitor/", "M34", 15.0).
?test(sheet1_N34, "/Bitor/", "N34", 12.0).
?test(sheet1_O34, "/Bitor/", "O34", 13.0).
?test(sheet1_P34, "/Bitor/", "P34", 12.0).
?test(sheet1_Q34, "/Bitor/", "Q34", '#VALUE!').
?test(sheet1_R34, "/Bitor/", "R34", '#VALUE!').
?test(sheet1_S34, "/Bitor/", "S34", '#VALUE!').
?test(sheet1_T34, "/Bitor/", "T34", '#VALUE!').
?test(sheet1_U34, "/Bitor/", "U34", '#VALUE!').
?test(sheet1_V34, "/Bitor/", "V34", '#VALUE!').
?test(sheet1_A35, "/Bitor/", "A35", "String Number Leading space").
?test(sheet1_B35, "/Bitor/", "B35", " 23").
?test(sheet1_C35, "/Bitor/", "C35", '#DIV/0!').
?test(sheet1_D35, "/Bitor/", "D35", '#VALUE!').
?test(sheet1_E35, "/Bitor/", "E35", '#REF!').
?test(sheet1_F35, "/Bitor/", "F35", '#NAME?').
?test(sheet1_G35, "/Bitor/", "G35", '#NUM!').
?test(sheet1_H35, "/Bitor/", "H35", '#N/A').
?test(sheet1_I35, "/Bitor/", "I35", '#VALUE!').
?test(sheet1_J35, "/Bitor/", "J35", 31.0).
?test(sheet1_K35, "/Bitor/", "K35", 31.0).
?test(sheet1_L35, "/Bitor/", "L35", 24927.0).
?test(sheet1_M35, "/Bitor/", "M35", 23.0).
?test(sheet1_N35, "/Bitor/", "N35", 23.0).
?test(sheet1_O35, "/Bitor/", "O35", 23.0).
?test(sheet1_P35, "/Bitor/", "P35", 23.0).
?test(sheet1_Q35, "/Bitor/", "Q35", '#VALUE!').
?test(sheet1_R35, "/Bitor/", "R35", '#VALUE!').
?test(sheet1_S35, "/Bitor/", "S35", '#VALUE!').
?test(sheet1_T35, "/Bitor/", "T35", '#VALUE!').
?test(sheet1_U35, "/Bitor/", "U35", '#VALUE!').
?test(sheet1_V35, "/Bitor/", "V35", '#VALUE!').
?test(sheet1_A36, "/Bitor/", "A36", "Interger").
?test(sheet1_B36, "/Bitor/", "B36", "1968/03/23 00:00:00").
?test(sheet1_C36, "/Bitor/", "C36", '#DIV/0!').
?test(sheet1_D36, "/Bitor/", "D36", '#VALUE!').
?test(sheet1_E36, "/Bitor/", "E36", '#REF!').
?test(sheet1_F36, "/Bitor/", "F36", '#NAME?').
?test(sheet1_G36, "/Bitor/", "G36", '#NUM!').
?test(sheet1_H36, "/Bitor/", "H36", '#N/A').
?test(sheet1_I36, "/Bitor/", "I36", '#VALUE!').
?test(sheet1_J36, "/Bitor/", "J36", 24925.0).
?test(sheet1_K36, "/Bitor/", "K36", 24920.0).
?test(sheet1_L36, "/Bitor/", "L36", 24920.0).
?test(sheet1_M36, "/Bitor/", "M36", 24923.0).
?test(sheet1_N36, "/Bitor/", "N36", 24920.0).
?test(sheet1_O36, "/Bitor/", "O36", 24921.0).
?test(sheet1_P36, "/Bitor/", "P36", 24920.0).
?test(sheet1_Q36, "/Bitor/", "Q36", '#VALUE!').
?test(sheet1_R36, "/Bitor/", "R36", '#VALUE!').
?test(sheet1_S36, "/Bitor/", "S36", '#VALUE!').
?test(sheet1_T36, "/Bitor/", "T36", '#VALUE!').
?test(sheet1_U36, "/Bitor/", "U36", '#VALUE!').
?test(sheet1_V36, "/Bitor/", "V36", '#VALUE!').
?test(sheet1_A37, "/Bitor/", "A37", "Float").
?test(sheet1_B37, "/Bitor/", "B37", 3.14159265358979).
?test(sheet1_C37, "/Bitor/", "C37", '#DIV/0!').
?test(sheet1_D37, "/Bitor/", "D37", '#VALUE!').
?test(sheet1_E37, "/Bitor/", "E37", '#REF!').
?test(sheet1_F37, "/Bitor/", "F37", '#NAME?').
?test(sheet1_G37, "/Bitor/", "G37", '#NUM!').
?test(sheet1_H37, "/Bitor/", "H37", '#N/A').
?test(sheet1_I37, "/Bitor/", "I37", '#VALUE!').
?test(sheet1_J37, "/Bitor/", "J37", 15.0).
?test(sheet1_K37, "/Bitor/", "K37", 27.0).
?test(sheet1_L37, "/Bitor/", "L37", 24923.0).
?test(sheet1_M37, "/Bitor/", "M37", 3.0).
?test(sheet1_N37, "/Bitor/", "N37", 3.0).
?test(sheet1_O37, "/Bitor/", "O37", 3.0).
?test(sheet1_P37, "/Bitor/", "P37", 3.0).
?test(sheet1_Q37, "/Bitor/", "Q37", '#VALUE!').
?test(sheet1_R37, "/Bitor/", "R37", '#VALUE!').
?test(sheet1_S37, "/Bitor/", "S37", '#VALUE!').
?test(sheet1_T37, "/Bitor/", "T37", '#VALUE!').
?test(sheet1_U37, "/Bitor/", "U37", '#VALUE!').
?test(sheet1_V37, "/Bitor/", "V37", '#VALUE!').
?test(sheet1_A38, "/Bitor/", "A38", "Blank").
?test(sheet1_C38, "/Bitor/", "C38", '#DIV/0!').
?test(sheet1_D38, "/Bitor/", "D38", '#VALUE!').
?test(sheet1_E38, "/Bitor/", "E38", '#REF!').
?test(sheet1_F38, "/Bitor/", "F38", '#NAME?').
?test(sheet1_G38, "/Bitor/", "G38", '#NUM!').
?test(sheet1_H38, "/Bitor/", "H38", '#N/A').
?test(sheet1_I38, "/Bitor/", "I38", '#VALUE!').
?test(sheet1_J38, "/Bitor/", "J38", 13.0).
?test(sheet1_K38, "/Bitor/", "K38", 24.0).
?test(sheet1_L38, "/Bitor/", "L38", 24920.0).
?test(sheet1_M38, "/Bitor/", "M38", 3.0).
?test(sheet1_N38, "/Bitor/", "N38", 0.0).
?test(sheet1_O38, "/Bitor/", "O38", 1.0).
?test(sheet1_P38, "/Bitor/", "P38", 0.0).
?test(sheet1_Q38, "/Bitor/", "Q38", '#VALUE!').
?test(sheet1_R38, "/Bitor/", "R38", '#VALUE!').
?test(sheet1_S38, "/Bitor/", "S38", '#VALUE!').
?test(sheet1_T38, "/Bitor/", "T38", '#VALUE!').
?test(sheet1_U38, "/Bitor/", "U38", '#VALUE!').
?test(sheet1_V38, "/Bitor/", "V38", '#VALUE!').
?test(sheet1_A39, "/Bitor/", "A39", "Logical").
?test(sheet1_B39, "/Bitor/", "B39", true).
?test(sheet1_C39, "/Bitor/", "C39", '#DIV/0!').
?test(sheet1_D39, "/Bitor/", "D39", '#VALUE!').
?test(sheet1_E39, "/Bitor/", "E39", '#REF!').
?test(sheet1_F39, "/Bitor/", "F39", '#NAME?').
?test(sheet1_G39, "/Bitor/", "G39", '#NUM!').
?test(sheet1_H39, "/Bitor/", "H39", '#N/A').
?test(sheet1_I39, "/Bitor/", "I39", '#VALUE!').
?test(sheet1_J39, "/Bitor/", "J39", 13.0).
?test(sheet1_K39, "/Bitor/", "K39", 25.0).
?test(sheet1_L39, "/Bitor/", "L39", 24921.0).
?test(sheet1_M39, "/Bitor/", "M39", 3.0).
?test(sheet1_N39, "/Bitor/", "N39", 1.0).
?test(sheet1_O39, "/Bitor/", "O39", 1.0).
?test(sheet1_P39, "/Bitor/", "P39", 1.0).
?test(sheet1_Q39, "/Bitor/", "Q39", '#VALUE!').
?test(sheet1_R39, "/Bitor/", "R39", '#VALUE!').
?test(sheet1_S39, "/Bitor/", "S39", '#VALUE!').
?test(sheet1_T39, "/Bitor/", "T39", '#VALUE!').
?test(sheet1_U39, "/Bitor/", "U39", '#VALUE!').
?test(sheet1_V39, "/Bitor/", "V39", '#VALUE!').
?test(sheet1_A40, "/Bitor/", "A40", "Logical").
?test(sheet1_B40, "/Bitor/", "B40", false).
?test(sheet1_C40, "/Bitor/", "C40", '#DIV/0!').
?test(sheet1_D40, "/Bitor/", "D40", '#VALUE!').
?test(sheet1_E40, "/Bitor/", "E40", '#REF!').
?test(sheet1_F40, "/Bitor/", "F40", '#NAME?').
?test(sheet1_G40, "/Bitor/", "G40", '#NUM!').
?test(sheet1_H40, "/Bitor/", "H40", '#N/A').
?test(sheet1_I40, "/Bitor/", "I40", '#VALUE!').
?test(sheet1_J40, "/Bitor/", "J40", 13.0).
?test(sheet1_K40, "/Bitor/", "K40", 24.0).
?test(sheet1_L40, "/Bitor/", "L40", 24920.0).
?test(sheet1_M40, "/Bitor/", "M40", 3.0).
?test(sheet1_N40, "/Bitor/", "N40", 0.0).
?test(sheet1_O40, "/Bitor/", "O40", 1.0).
?test(sheet1_P40, "/Bitor/", "P40", 0.0).
?test(sheet1_Q40, "/Bitor/", "Q40", '#VALUE!').
?test(sheet1_R40, "/Bitor/", "R40", '#VALUE!').
?test(sheet1_S40, "/Bitor/", "S40", '#VALUE!').
?test(sheet1_T40, "/Bitor/", "T40", '#VALUE!').
?test(sheet1_U40, "/Bitor/", "U40", '#VALUE!').
?test(sheet1_V40, "/Bitor/", "V40", '#VALUE!').
?test(sheet1_A41, "/Bitor/", "A41", "Range Row").
?test(sheet1_B41, "/Bitor/", "B41", "X3:Y3").
?test(sheet1_C41, "/Bitor/", "C41", '#VALUE!').
?test(sheet1_D41, "/Bitor/", "D41", '#VALUE!').
?test(sheet1_E41, "/Bitor/", "E41", '#VALUE!').
?test(sheet1_F41, "/Bitor/", "F41", '#VALUE!').
?test(sheet1_G41, "/Bitor/", "G41", '#VALUE!').
?test(sheet1_H41, "/Bitor/", "H41", '#VALUE!').
?test(sheet1_I41, "/Bitor/", "I41", '#VALUE!').
?test(sheet1_J41, "/Bitor/", "J41", '#VALUE!').
?test(sheet1_K41, "/Bitor/", "K41", '#VALUE!').
?test(sheet1_L41, "/Bitor/", "L41", '#VALUE!').
?test(sheet1_M41, "/Bitor/", "M41", '#VALUE!').
?test(sheet1_N41, "/Bitor/", "N41", '#VALUE!').
?test(sheet1_O41, "/Bitor/", "O41", '#VALUE!').
?test(sheet1_P41, "/Bitor/", "P41", '#VALUE!').
?test(sheet1_Q41, "/Bitor/", "Q41", '#VALUE!').
?test(sheet1_R41, "/Bitor/", "R41", '#VALUE!').
?test(sheet1_S41, "/Bitor/", "S41", '#VALUE!').
?test(sheet1_T41, "/Bitor/", "T41", '#VALUE!').
?test(sheet1_U41, "/Bitor/", "U41", '#VALUE!').
?test(sheet1_V41, "/Bitor/", "V41", '#VALUE!').
?test(sheet1_A42, "/Bitor/", "A42", "Range Row").
?test(sheet1_B42, "/Bitor/", "B42", "X3:AA3").
?test(sheet1_C42, "/Bitor/", "C42", '#VALUE!').
?test(sheet1_D42, "/Bitor/", "D42", '#VALUE!').
?test(sheet1_E42, "/Bitor/", "E42", '#VALUE!').
?test(sheet1_F42, "/Bitor/", "F42", '#VALUE!').
?test(sheet1_G42, "/Bitor/", "G42", '#VALUE!').
?test(sheet1_H42, "/Bitor/", "H42", '#VALUE!').
?test(sheet1_I42, "/Bitor/", "I42", '#VALUE!').
?test(sheet1_J42, "/Bitor/", "J42", '#VALUE!').
?test(sheet1_K42, "/Bitor/", "K42", '#VALUE!').
?test(sheet1_L42, "/Bitor/", "L42", '#VALUE!').
?test(sheet1_M42, "/Bitor/", "M42", '#VALUE!').
?test(sheet1_N42, "/Bitor/", "N42", '#VALUE!').
?test(sheet1_O42, "/Bitor/", "O42", '#VALUE!').
?test(sheet1_P42, "/Bitor/", "P42", '#VALUE!').
?test(sheet1_Q42, "/Bitor/", "Q42", '#VALUE!').
?test(sheet1_R42, "/Bitor/", "R42", '#VALUE!').
?test(sheet1_S42, "/Bitor/", "S42", '#VALUE!').
?test(sheet1_T42, "/Bitor/", "T42", '#VALUE!').
?test(sheet1_U42, "/Bitor/", "U42", '#VALUE!').
?test(sheet1_V42, "/Bitor/", "V42", '#VALUE!').
?test(sheet1_A43, "/Bitor/", "A43", "Range Area").
?test(sheet1_B43, "/Bitor/", "B43", "X3:Y4").
?test(sheet1_C43, "/Bitor/", "C43", '#VALUE!').
?test(sheet1_D43, "/Bitor/", "D43", '#VALUE!').
?test(sheet1_E43, "/Bitor/", "E43", '#VALUE!').
?test(sheet1_F43, "/Bitor/", "F43", '#VALUE!').
?test(sheet1_G43, "/Bitor/", "G43", '#VALUE!').
?test(sheet1_H43, "/Bitor/", "H43", '#VALUE!').
?test(sheet1_I43, "/Bitor/", "I43", '#VALUE!').
?test(sheet1_J43, "/Bitor/", "J43", '#VALUE!').
?test(sheet1_K43, "/Bitor/", "K43", '#VALUE!').
?test(sheet1_L43, "/Bitor/", "L43", '#VALUE!').
?test(sheet1_M43, "/Bitor/", "M43", '#VALUE!').
?test(sheet1_N43, "/Bitor/", "N43", '#VALUE!').
?test(sheet1_O43, "/Bitor/", "O43", '#VALUE!').
?test(sheet1_P43, "/Bitor/", "P43", '#VALUE!').
?test(sheet1_Q43, "/Bitor/", "Q43", '#VALUE!').
?test(sheet1_R43, "/Bitor/", "R43", '#VALUE!').
?test(sheet1_S43, "/Bitor/", "S43", '#VALUE!').
?test(sheet1_T43, "/Bitor/", "T43", '#VALUE!').
?test(sheet1_U43, "/Bitor/", "U43", '#VALUE!').
?test(sheet1_V43, "/Bitor/", "V43", '#VALUE!').
?test(sheet1_A44, "/Bitor/", "A44", "Range Area").
?test(sheet1_B44, "/Bitor/", "B44", "X3:AA6").
?test(sheet1_C44, "/Bitor/", "C44", '#VALUE!').
?test(sheet1_D44, "/Bitor/", "D44", '#VALUE!').
?test(sheet1_E44, "/Bitor/", "E44", '#VALUE!').
?test(sheet1_F44, "/Bitor/", "F44", '#VALUE!').
?test(sheet1_G44, "/Bitor/", "G44", '#VALUE!').
?test(sheet1_H44, "/Bitor/", "H44", '#VALUE!').
?test(sheet1_I44, "/Bitor/", "I44", '#VALUE!').
?test(sheet1_J44, "/Bitor/", "J44", '#VALUE!').
?test(sheet1_K44, "/Bitor/", "K44", '#VALUE!').
?test(sheet1_L44, "/Bitor/", "L44", '#VALUE!').
?test(sheet1_M44, "/Bitor/", "M44", '#VALUE!').
?test(sheet1_N44, "/Bitor/", "N44", '#VALUE!').
?test(sheet1_O44, "/Bitor/", "O44", '#VALUE!').
?test(sheet1_P44, "/Bitor/", "P44", '#VALUE!').
?test(sheet1_Q44, "/Bitor/", "Q44", '#VALUE!').
?test(sheet1_R44, "/Bitor/", "R44", '#VALUE!').
?test(sheet1_S44, "/Bitor/", "S44", '#VALUE!').
?test(sheet1_T44, "/Bitor/", "T44", '#VALUE!').
?test(sheet1_U44, "/Bitor/", "U44", '#VALUE!').
?test(sheet1_V44, "/Bitor/", "V44", '#VALUE!').
?test(sheet1_A45, "/Bitor/", "A45", "Range Colunm").
?test(sheet1_B45, "/Bitor/", "B45", "X3:X4").
?test(sheet1_C45, "/Bitor/", "C45", '#VALUE!').
?test(sheet1_D45, "/Bitor/", "D45", '#VALUE!').
?test(sheet1_E45, "/Bitor/", "E45", '#VALUE!').
?test(sheet1_F45, "/Bitor/", "F45", '#VALUE!').
?test(sheet1_G45, "/Bitor/", "G45", '#VALUE!').
?test(sheet1_H45, "/Bitor/", "H45", '#VALUE!').
?test(sheet1_I45, "/Bitor/", "I45", '#VALUE!').
?test(sheet1_J45, "/Bitor/", "J45", '#VALUE!').
?test(sheet1_K45, "/Bitor/", "K45", '#VALUE!').
?test(sheet1_L45, "/Bitor/", "L45", '#VALUE!').
?test(sheet1_M45, "/Bitor/", "M45", '#VALUE!').
?test(sheet1_N45, "/Bitor/", "N45", '#VALUE!').
?test(sheet1_O45, "/Bitor/", "O45", '#VALUE!').
?test(sheet1_P45, "/Bitor/", "P45", '#VALUE!').
?test(sheet1_Q45, "/Bitor/", "Q45", '#VALUE!').
?test(sheet1_R45, "/Bitor/", "R45", '#VALUE!').
?test(sheet1_S45, "/Bitor/", "S45", '#VALUE!').
?test(sheet1_T45, "/Bitor/", "T45", '#VALUE!').
?test(sheet1_U45, "/Bitor/", "U45", '#VALUE!').
?test(sheet1_V45, "/Bitor/", "V45", '#VALUE!').
?test(sheet1_A46, "/Bitor/", "A46", "Range Colunm").
?test(sheet1_B46, "/Bitor/", "B46", "X3:X6").
?test(sheet1_C46, "/Bitor/", "C46", '#VALUE!').
?test(sheet1_D46, "/Bitor/", "D46", '#VALUE!').
?test(sheet1_E46, "/Bitor/", "E46", '#VALUE!').
?test(sheet1_F46, "/Bitor/", "F46", '#VALUE!').
?test(sheet1_G46, "/Bitor/", "G46", '#VALUE!').
?test(sheet1_H46, "/Bitor/", "H46", '#VALUE!').
?test(sheet1_I46, "/Bitor/", "I46", '#VALUE!').
?test(sheet1_J46, "/Bitor/", "J46", '#VALUE!').
?test(sheet1_K46, "/Bitor/", "K46", '#VALUE!').
?test(sheet1_L46, "/Bitor/", "L46", '#VALUE!').
?test(sheet1_M46, "/Bitor/", "M46", '#VALUE!').
?test(sheet1_N46, "/Bitor/", "N46", '#VALUE!').
?test(sheet1_O46, "/Bitor/", "O46", '#VALUE!').
?test(sheet1_P46, "/Bitor/", "P46", '#VALUE!').
?test(sheet1_Q46, "/Bitor/", "Q46", '#VALUE!').
?test(sheet1_R46, "/Bitor/", "R46", '#VALUE!').
?test(sheet1_S46, "/Bitor/", "S46", '#VALUE!').
?test(sheet1_T46, "/Bitor/", "T46", '#VALUE!').
?test(sheet1_U46, "/Bitor/", "U46", '#VALUE!').
?test(sheet1_V46, "/Bitor/", "V46", '#VALUE!').
?test(sheet1_A49, "/Bitor/", "A49", 320.0).
?test(sheet1_C49, "/Bitor/", "C49", 0.0).
?test(sheet1_D49, "/Bitor/", "D49", 0.0).
?test(sheet1_E49, "/Bitor/", "E49", 0.0).
?test(sheet1_F49, "/Bitor/", "F49", 0.0).
?test(sheet1_G49, "/Bitor/", "G49", 0.0).
?test(sheet1_H49, "/Bitor/", "H49", 0.0).
?test(sheet1_I49, "/Bitor/", "I49", 0.0).
?test(sheet1_J49, "/Bitor/", "J49", 0.0).
?test(sheet1_K49, "/Bitor/", "K49", 0.0).
?test(sheet1_L49, "/Bitor/", "L49", 0.0).
?test(sheet1_M49, "/Bitor/", "M49", 0.0).
?test(sheet1_N49, "/Bitor/", "N49", 0.0).
?test(sheet1_O49, "/Bitor/", "O49", 0.0).
?test(sheet1_P49, "/Bitor/", "P49", 0.0).
?test(sheet1_Q49, "/Bitor/", "Q49", 0.0).
?test(sheet1_R49, "/Bitor/", "R49", 0.0).
?test(sheet1_S49, "/Bitor/", "S49", 0.0).
?test(sheet1_T49, "/Bitor/", "T49", 0.0).
?test(sheet1_U49, "/Bitor/", "U49", 0.0).
?test(sheet1_V49, "/Bitor/", "V49", 0.0).
?test(sheet1_A50, "/Bitor/", "A50", 7.0).
?test(sheet1_C50, "/Bitor/", "C50", 0.0).
?test(sheet1_D50, "/Bitor/", "D50", 0.0).
?test(sheet1_E50, "/Bitor/", "E50", 0.0).
?test(sheet1_F50, "/Bitor/", "F50", 0.0).
?test(sheet1_G50, "/Bitor/", "G50", 0.0).
?test(sheet1_H50, "/Bitor/", "H50", 0.0).
?test(sheet1_I50, "/Bitor/", "I50", 0.0).
?test(sheet1_J50, "/Bitor/", "J50", 0.0).
?test(sheet1_K50, "/Bitor/", "K50", 0.0).
?test(sheet1_L50, "/Bitor/", "L50", 0.0).
?test(sheet1_M50, "/Bitor/", "M50", 0.0).
?test(sheet1_N50, "/Bitor/", "N50", 0.0).
?test(sheet1_O50, "/Bitor/", "O50", 0.0).
?test(sheet1_P50, "/Bitor/", "P50", 0.0).
?test(sheet1_Q50, "/Bitor/", "Q50", 0.0).
?test(sheet1_R50, "/Bitor/", "R50", 0.0).
?test(sheet1_S50, "/Bitor/", "S50", 0.0).
?test(sheet1_T50, "/Bitor/", "T50", 0.0).
?test(sheet1_U50, "/Bitor/", "U50", 0.0).
?test(sheet1_V50, "/Bitor/", "V50", 0.0).
?test(sheet1_C51, "/Bitor/", "C51", 0.0).
?test(sheet1_D51, "/Bitor/", "D51", 0.0).
?test(sheet1_E51, "/Bitor/", "E51", 0.0).
?test(sheet1_F51, "/Bitor/", "F51", 0.0).
?test(sheet1_G51, "/Bitor/", "G51", 0.0).
?test(sheet1_H51, "/Bitor/", "H51", 0.0).
?test(sheet1_I51, "/Bitor/", "I51", 0.0).
?test(sheet1_J51, "/Bitor/", "J51", 0.0).
?test(sheet1_K51, "/Bitor/", "K51", 0.0).
?test(sheet1_L51, "/Bitor/", "L51", 0.0).
?test(sheet1_M51, "/Bitor/", "M51", 0.0).
?test(sheet1_N51, "/Bitor/", "N51", 0.0).
?test(sheet1_O51, "/Bitor/", "O51", 0.0).
?test(sheet1_P51, "/Bitor/", "P51", 0.0).
?test(sheet1_Q51, "/Bitor/", "Q51", 0.0).
?test(sheet1_R51, "/Bitor/", "R51", 0.0).
?test(sheet1_S51, "/Bitor/", "S51", 0.0).
?test(sheet1_T51, "/Bitor/", "T51", 0.0).
?test(sheet1_U51, "/Bitor/", "U51", 0.0).
?test(sheet1_V51, "/Bitor/", "V51", 0.0).
?test(sheet1_C52, "/Bitor/", "C52", 0.0).
?test(sheet1_D52, "/Bitor/", "D52", 0.0).
?test(sheet1_E52, "/Bitor/", "E52", 0.0).
?test(sheet1_F52, "/Bitor/", "F52", 1.0).
?test(sheet1_G52, "/Bitor/", "G52", 0.0).
?test(sheet1_H52, "/Bitor/", "H52", 0.0).
?test(sheet1_I52, "/Bitor/", "I52", 0.0).
?test(sheet1_J52, "/Bitor/", "J52", 0.0).
?test(sheet1_K52, "/Bitor/", "K52", 0.0).
?test(sheet1_L52, "/Bitor/", "L52", 0.0).
?test(sheet1_M52, "/Bitor/", "M52", 0.0).
?test(sheet1_N52, "/Bitor/", "N52", 0.0).
?test(sheet1_O52, "/Bitor/", "O52", 0.0).
?test(sheet1_P52, "/Bitor/", "P52", 0.0).
?test(sheet1_Q52, "/Bitor/", "Q52", 0.0).
?test(sheet1_R52, "/Bitor/", "R52", 0.0).
?test(sheet1_S52, "/Bitor/", "S52", 0.0).
?test(sheet1_T52, "/Bitor/", "T52", 0.0).
?test(sheet1_U52, "/Bitor/", "U52", 0.0).
?test(sheet1_V52, "/Bitor/", "V52", 0.0).
?test(sheet1_C53, "/Bitor/", "C53", 0.0).
?test(sheet1_D53, "/Bitor/", "D53", 0.0).
?test(sheet1_E53, "/Bitor/", "E53", 0.0).
?test(sheet1_F53, "/Bitor/", "F53", 1.0).
?test(sheet1_G53, "/Bitor/", "G53", 0.0).
?test(sheet1_H53, "/Bitor/", "H53", 0.0).
?test(sheet1_I53, "/Bitor/", "I53", 0.0).
?test(sheet1_J53, "/Bitor/", "J53", 0.0).
?test(sheet1_K53, "/Bitor/", "K53", 0.0).
?test(sheet1_L53, "/Bitor/", "L53", 0.0).
?test(sheet1_M53, "/Bitor/", "M53", 0.0).
?test(sheet1_N53, "/Bitor/", "N53", 0.0).
?test(sheet1_O53, "/Bitor/", "O53", 0.0).
?test(sheet1_P53, "/Bitor/", "P53", 0.0).
?test(sheet1_Q53, "/Bitor/", "Q53", 0.0).
?test(sheet1_R53, "/Bitor/", "R53", 0.0).
?test(sheet1_S53, "/Bitor/", "S53", 0.0).
?test(sheet1_T53, "/Bitor/", "T53", 0.0).
?test(sheet1_U53, "/Bitor/", "U53", 0.0).
?test(sheet1_V53, "/Bitor/", "V53", 0.0).
?test(sheet1_C54, "/Bitor/", "C54", 0.0).
?test(sheet1_D54, "/Bitor/", "D54", 0.0).
?test(sheet1_E54, "/Bitor/", "E54", 0.0).
?test(sheet1_F54, "/Bitor/", "F54", 1.0).
?test(sheet1_G54, "/Bitor/", "G54", 0.0).
?test(sheet1_H54, "/Bitor/", "H54", 0.0).
?test(sheet1_I54, "/Bitor/", "I54", 0.0).
?test(sheet1_J54, "/Bitor/", "J54", 0.0).
?test(sheet1_K54, "/Bitor/", "K54", 0.0).
?test(sheet1_L54, "/Bitor/", "L54", 0.0).
?test(sheet1_M54, "/Bitor/", "M54", 0.0).
?test(sheet1_N54, "/Bitor/", "N54", 0.0).
?test(sheet1_O54, "/Bitor/", "O54", 0.0).
?test(sheet1_P54, "/Bitor/", "P54", 0.0).
?test(sheet1_Q54, "/Bitor/", "Q54", 0.0).
?test(sheet1_R54, "/Bitor/", "R54", 0.0).
?test(sheet1_S54, "/Bitor/", "S54", 0.0).
?test(sheet1_T54, "/Bitor/", "T54", 0.0).
?test(sheet1_U54, "/Bitor/", "U54", 0.0).
?test(sheet1_V54, "/Bitor/", "V54", 0.0).
?test(sheet1_C55, "/Bitor/", "C55", 0.0).
?test(sheet1_D55, "/Bitor/", "D55", 0.0).
?test(sheet1_E55, "/Bitor/", "E55", 0.0).
?test(sheet1_F55, "/Bitor/", "F55", 1.0).
?test(sheet1_G55, "/Bitor/", "G55", 0.0).
?test(sheet1_H55, "/Bitor/", "H55", 0.0).
?test(sheet1_I55, "/Bitor/", "I55", 0.0).
?test(sheet1_J55, "/Bitor/", "J55", 0.0).
?test(sheet1_K55, "/Bitor/", "K55", 0.0).
?test(sheet1_L55, "/Bitor/", "L55", 0.0).
?test(sheet1_M55, "/Bitor/", "M55", 0.0).
?test(sheet1_N55, "/Bitor/", "N55", 0.0).
?test(sheet1_O55, "/Bitor/", "O55", 0.0).
?test(sheet1_P55, "/Bitor/", "P55", 0.0).
?test(sheet1_Q55, "/Bitor/", "Q55", 0.0).
?test(sheet1_R55, "/Bitor/", "R55", 0.0).
?test(sheet1_S55, "/Bitor/", "S55", 0.0).
?test(sheet1_T55, "/Bitor/", "T55", 0.0).
?test(sheet1_U55, "/Bitor/", "U55", 0.0).
?test(sheet1_V55, "/Bitor/", "V55", 0.0).
?test(sheet1_C56, "/Bitor/", "C56", 0.0).
?test(sheet1_D56, "/Bitor/", "D56", 0.0).
?test(sheet1_E56, "/Bitor/", "E56", 0.0).
?test(sheet1_F56, "/Bitor/", "F56", 1.0).
?test(sheet1_G56, "/Bitor/", "G56", 0.0).
?test(sheet1_H56, "/Bitor/", "H56", 0.0).
?test(sheet1_I56, "/Bitor/", "I56", 0.0).
?test(sheet1_J56, "/Bitor/", "J56", 0.0).
?test(sheet1_K56, "/Bitor/", "K56", 0.0).
?test(sheet1_L56, "/Bitor/", "L56", 0.0).
?test(sheet1_M56, "/Bitor/", "M56", 0.0).
?test(sheet1_N56, "/Bitor/", "N56", 0.0).
?test(sheet1_O56, "/Bitor/", "O56", 0.0).
?test(sheet1_P56, "/Bitor/", "P56", 0.0).
?test(sheet1_Q56, "/Bitor/", "Q56", 0.0).
?test(sheet1_R56, "/Bitor/", "R56", 0.0).
?test(sheet1_S56, "/Bitor/", "S56", 0.0).
?test(sheet1_T56, "/Bitor/", "T56", 0.0).
?test(sheet1_U56, "/Bitor/", "U56", 0.0).
?test(sheet1_V56, "/Bitor/", "V56", 0.0).
?test(sheet1_C57, "/Bitor/", "C57", 0.0).
?test(sheet1_D57, "/Bitor/", "D57", 0.0).
?test(sheet1_E57, "/Bitor/", "E57", 0.0).
?test(sheet1_F57, "/Bitor/", "F57", 1.0).
?test(sheet1_G57, "/Bitor/", "G57", 0.0).
?test(sheet1_H57, "/Bitor/", "H57", 0.0).
?test(sheet1_I57, "/Bitor/", "I57", 0.0).
?test(sheet1_J57, "/Bitor/", "J57", 0.0).
?test(sheet1_K57, "/Bitor/", "K57", 0.0).
?test(sheet1_L57, "/Bitor/", "L57", 0.0).
?test(sheet1_M57, "/Bitor/", "M57", 0.0).
?test(sheet1_N57, "/Bitor/", "N57", 0.0).
?test(sheet1_O57, "/Bitor/", "O57", 0.0).
?test(sheet1_P57, "/Bitor/", "P57", 0.0).
?test(sheet1_Q57, "/Bitor/", "Q57", 0.0).
?test(sheet1_R57, "/Bitor/", "R57", 0.0).
?test(sheet1_S57, "/Bitor/", "S57", 0.0).
?test(sheet1_T57, "/Bitor/", "T57", 0.0).
?test(sheet1_U57, "/Bitor/", "U57", 0.0).
?test(sheet1_V57, "/Bitor/", "V57", 0.0).
?test(sheet1_C58, "/Bitor/", "C58", 0.0).
?test(sheet1_D58, "/Bitor/", "D58", 0.0).
?test(sheet1_E58, "/Bitor/", "E58", 0.0).
?test(sheet1_F58, "/Bitor/", "F58", 1.0).
?test(sheet1_G58, "/Bitor/", "G58", 0.0).
?test(sheet1_H58, "/Bitor/", "H58", 0.0).
?test(sheet1_I58, "/Bitor/", "I58", 0.0).
?test(sheet1_J58, "/Bitor/", "J58", 0.0).
?test(sheet1_K58, "/Bitor/", "K58", 0.0).
?test(sheet1_L58, "/Bitor/", "L58", 0.0).
?test(sheet1_M58, "/Bitor/", "M58", 0.0).
?test(sheet1_N58, "/Bitor/", "N58", 0.0).
?test(sheet1_O58, "/Bitor/", "O58", 0.0).
?test(sheet1_P58, "/Bitor/", "P58", 0.0).
?test(sheet1_Q58, "/Bitor/", "Q58", 0.0).
?test(sheet1_R58, "/Bitor/", "R58", 0.0).
?test(sheet1_S58, "/Bitor/", "S58", 0.0).
?test(sheet1_T58, "/Bitor/", "T58", 0.0).
?test(sheet1_U58, "/Bitor/", "U58", 0.0).
?test(sheet1_V58, "/Bitor/", "V58", 0.0).
?test(sheet1_C59, "/Bitor/", "C59", 0.0).
?test(sheet1_D59, "/Bitor/", "D59", 0.0).
?test(sheet1_E59, "/Bitor/", "E59", 0.0).
?test(sheet1_F59, "/Bitor/", "F59", 0.0).
?test(sheet1_G59, "/Bitor/", "G59", 0.0).
?test(sheet1_H59, "/Bitor/", "H59", 0.0).
?test(sheet1_I59, "/Bitor/", "I59", 0.0).
?test(sheet1_J59, "/Bitor/", "J59", 0.0).
?test(sheet1_K59, "/Bitor/", "K59", 0.0).
?test(sheet1_L59, "/Bitor/", "L59", 0.0).
?test(sheet1_M59, "/Bitor/", "M59", 0.0).
?test(sheet1_N59, "/Bitor/", "N59", 0.0).
?test(sheet1_O59, "/Bitor/", "O59", 0.0).
?test(sheet1_P59, "/Bitor/", "P59", 0.0).
?test(sheet1_Q59, "/Bitor/", "Q59", 0.0).
?test(sheet1_R59, "/Bitor/", "R59", 0.0).
?test(sheet1_S59, "/Bitor/", "S59", 0.0).
?test(sheet1_T59, "/Bitor/", "T59", 0.0).
?test(sheet1_U59, "/Bitor/", "U59", 0.0).
?test(sheet1_V59, "/Bitor/", "V59", 0.0).
?test(sheet1_C60, "/Bitor/", "C60", 0.0).
?test(sheet1_D60, "/Bitor/", "D60", 0.0).
?test(sheet1_E60, "/Bitor/", "E60", 0.0).
?test(sheet1_F60, "/Bitor/", "F60", 0.0).
?test(sheet1_G60, "/Bitor/", "G60", 0.0).
?test(sheet1_H60, "/Bitor/", "H60", 0.0).
?test(sheet1_I60, "/Bitor/", "I60", 0.0).
?test(sheet1_J60, "/Bitor/", "J60", 0.0).
?test(sheet1_K60, "/Bitor/", "K60", 0.0).
?test(sheet1_L60, "/Bitor/", "L60", 0.0).
?test(sheet1_M60, "/Bitor/", "M60", 0.0).
?test(sheet1_N60, "/Bitor/", "N60", 0.0).
?test(sheet1_O60, "/Bitor/", "O60", 0.0).
?test(sheet1_P60, "/Bitor/", "P60", 0.0).
?test(sheet1_Q60, "/Bitor/", "Q60", 0.0).
?test(sheet1_R60, "/Bitor/", "R60", 0.0).
?test(sheet1_S60, "/Bitor/", "S60", 0.0).
?test(sheet1_T60, "/Bitor/", "T60", 0.0).
?test(sheet1_U60, "/Bitor/", "U60", 0.0).
?test(sheet1_V60, "/Bitor/", "V60", 0.0).
?test(sheet1_C61, "/Bitor/", "C61", 0.0).
?test(sheet1_D61, "/Bitor/", "D61", 0.0).
?test(sheet1_E61, "/Bitor/", "E61", 0.0).
?test(sheet1_F61, "/Bitor/", "F61", 0.0).
?test(sheet1_G61, "/Bitor/", "G61", 0.0).
?test(sheet1_H61, "/Bitor/", "H61", 0.0).
?test(sheet1_I61, "/Bitor/", "I61", 0.0).
?test(sheet1_J61, "/Bitor/", "J61", 0.0).
?test(sheet1_K61, "/Bitor/", "K61", 0.0).
?test(sheet1_L61, "/Bitor/", "L61", 0.0).
?test(sheet1_M61, "/Bitor/", "M61", 0.0).
?test(sheet1_N61, "/Bitor/", "N61", 0.0).
?test(sheet1_O61, "/Bitor/", "O61", 0.0).
?test(sheet1_P61, "/Bitor/", "P61", 0.0).
?test(sheet1_Q61, "/Bitor/", "Q61", 0.0).
?test(sheet1_R61, "/Bitor/", "R61", 0.0).
?test(sheet1_S61, "/Bitor/", "S61", 0.0).
?test(sheet1_T61, "/Bitor/", "T61", 0.0).
?test(sheet1_U61, "/Bitor/", "U61", 0.0).
?test(sheet1_V61, "/Bitor/", "V61", 0.0).
?test(sheet1_C62, "/Bitor/", "C62", 0.0).
?test(sheet1_D62, "/Bitor/", "D62", 0.0).
?test(sheet1_E62, "/Bitor/", "E62", 0.0).
?test(sheet1_F62, "/Bitor/", "F62", 0.0).
?test(sheet1_G62, "/Bitor/", "G62", 0.0).
?test(sheet1_H62, "/Bitor/", "H62", 0.0).
?test(sheet1_I62, "/Bitor/", "I62", 0.0).
?test(sheet1_J62, "/Bitor/", "J62", 0.0).
?test(sheet1_K62, "/Bitor/", "K62", 0.0).
?test(sheet1_L62, "/Bitor/", "L62", 0.0).
?test(sheet1_M62, "/Bitor/", "M62", 0.0).
?test(sheet1_N62, "/Bitor/", "N62", 0.0).
?test(sheet1_O62, "/Bitor/", "O62", 0.0).
?test(sheet1_P62, "/Bitor/", "P62", 0.0).
?test(sheet1_Q62, "/Bitor/", "Q62", 0.0).
?test(sheet1_R62, "/Bitor/", "R62", 0.0).
?test(sheet1_S62, "/Bitor/", "S62", 0.0).
?test(sheet1_T62, "/Bitor/", "T62", 0.0).
?test(sheet1_U62, "/Bitor/", "U62", 0.0).
?test(sheet1_V62, "/Bitor/", "V62", 0.0).
?test(sheet1_C63, "/Bitor/", "C63", 0.0).
?test(sheet1_D63, "/Bitor/", "D63", 0.0).
?test(sheet1_E63, "/Bitor/", "E63", 0.0).
?test(sheet1_F63, "/Bitor/", "F63", 0.0).
?test(sheet1_G63, "/Bitor/", "G63", 0.0).
?test(sheet1_H63, "/Bitor/", "H63", 0.0).
?test(sheet1_I63, "/Bitor/", "I63", 0.0).
?test(sheet1_J63, "/Bitor/", "J63", 0.0).
?test(sheet1_K63, "/Bitor/", "K63", 0.0).
?test(sheet1_L63, "/Bitor/", "L63", 0.0).
?test(sheet1_M63, "/Bitor/", "M63", 0.0).
?test(sheet1_N63, "/Bitor/", "N63", 0.0).
?test(sheet1_O63, "/Bitor/", "O63", 0.0).
?test(sheet1_P63, "/Bitor/", "P63", 0.0).
?test(sheet1_Q63, "/Bitor/", "Q63", 0.0).
?test(sheet1_R63, "/Bitor/", "R63", 0.0).
?test(sheet1_S63, "/Bitor/", "S63", 0.0).
?test(sheet1_T63, "/Bitor/", "T63", 0.0).
?test(sheet1_U63, "/Bitor/", "U63", 0.0).
?test(sheet1_V63, "/Bitor/", "V63", 0.0).
?test(sheet1_C64, "/Bitor/", "C64", 0.0).
?test(sheet1_D64, "/Bitor/", "D64", 0.0).
?test(sheet1_E64, "/Bitor/", "E64", 0.0).
?test(sheet1_F64, "/Bitor/", "F64", 0.0).
?test(sheet1_G64, "/Bitor/", "G64", 0.0).
?test(sheet1_H64, "/Bitor/", "H64", 0.0).
?test(sheet1_I64, "/Bitor/", "I64", 0.0).
?test(sheet1_J64, "/Bitor/", "J64", 0.0).
?test(sheet1_K64, "/Bitor/", "K64", 0.0).
?test(sheet1_L64, "/Bitor/", "L64", 0.0).
?test(sheet1_M64, "/Bitor/", "M64", 0.0).
?test(sheet1_N64, "/Bitor/", "N64", 0.0).
?test(sheet1_O64, "/Bitor/", "O64", 0.0).
?test(sheet1_P64, "/Bitor/", "P64", 0.0).
?test(sheet1_Q64, "/Bitor/", "Q64", 0.0).
?test(sheet1_R64, "/Bitor/", "R64", 0.0).
?test(sheet1_S64, "/Bitor/", "S64", 0.0).
?test(sheet1_T64, "/Bitor/", "T64", 0.0).
?test(sheet1_U64, "/Bitor/", "U64", 0.0).
?test(sheet1_V64, "/Bitor/", "V64", 0.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_bitwise_bitor.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_bitwise_bitor" ++ "/" ++ Sheetname ++ "/",
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
