%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_bitwise_bitxor_SUITE).
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
                     [Testcase, "e_gnumeric_bitwise_bitxor_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_bitwise_bitxor" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Bitxor/", "A1", "bitxor(A,B)").
?test(sheet1_B1, "/Bitxor/", "B1", "B").
?test(sheet1_C1, "/Bitxor/", "C1", "errors").
?test(sheet1_D1, "/Bitxor/", "D1", "errors").
?test(sheet1_E1, "/Bitxor/", "E1", "errors").
?test(sheet1_F1, "/Bitxor/", "F1", "errors").
?test(sheet1_G1, "/Bitxor/", "G1", "errors").
?test(sheet1_H1, "/Bitxor/", "H1", "errors").
?test(sheet1_I1, "/Bitxor/", "I1", "String").
?test(sheet1_J1, "/Bitxor/", "J1", "String Number").
?test(sheet1_K1, "/Bitxor/", "K1", "String number Leading space").
?test(sheet1_L1, "/Bitxor/", "L1", "Integer").
?test(sheet1_M1, "/Bitxor/", "M1", "Float").
?test(sheet1_N1, "/Bitxor/", "N1", "Blank").
?test(sheet1_O1, "/Bitxor/", "O1", "Logical").
?test(sheet1_P1, "/Bitxor/", "P1", "Logical").
?test(sheet1_Q1, "/Bitxor/", "Q1", "Range Row").
?test(sheet1_R1, "/Bitxor/", "R1", "Range Row").
?test(sheet1_S1, "/Bitxor/", "S1", "Range Area").
?test(sheet1_T1, "/Bitxor/", "T1", "Range Area").
?test(sheet1_U1, "/Bitxor/", "U1", "Range Colunm").
?test(sheet1_V1, "/Bitxor/", "V1", "Range Colunm").
?test(sheet1_A2, "/Bitxor/", "A2", "A").
?test(sheet1_C2, "/Bitxor/", "C2", '#DIV/0!').
?test(sheet1_D2, "/Bitxor/", "D2", '#VALUE!').
?test(sheet1_E2, "/Bitxor/", "E2", '#REF!').
?test(sheet1_F2, "/Bitxor/", "F2", '#NAME?').
?test(sheet1_G2, "/Bitxor/", "G2", '#NUM!').
?test(sheet1_H2, "/Bitxor/", "H2", '#N/A').
?test(sheet1_I2, "/Bitxor/", "I2", "Phillip").
?test(sheet1_J2, "/Bitxor/", "J2", "13").
?test(sheet1_K2, "/Bitxor/", "K2", " 24").
?test(sheet1_L2, "/Bitxor/", "L2", "1968/03/23 00:00:00").
?test(sheet1_M2, "/Bitxor/", "M2", 3.14159265358979).
?test(sheet1_O2, "/Bitxor/", "O2", true).
?test(sheet1_P2, "/Bitxor/", "P2", false).
?test(sheet1_Q2, "/Bitxor/", "Q2", "X3:Y3").
?test(sheet1_R2, "/Bitxor/", "R2", "X3:AA3").
?test(sheet1_S2, "/Bitxor/", "S2", "X3:Y4").
?test(sheet1_T2, "/Bitxor/", "T2", "X3:AA6").
?test(sheet1_U2, "/Bitxor/", "U2", "X3:X4").
?test(sheet1_V2, "/Bitxor/", "V2", "X3:X6").
?test(sheet1_A3, "/Bitxor/", "A3", "errors").
?test(sheet1_B3, "/Bitxor/", "B3", '#DIV/0!').
?test(sheet1_C3, "/Bitxor/", "C3", '#NAME?').
?test(sheet1_D3, "/Bitxor/", "D3", '#NAME?').
?test(sheet1_E3, "/Bitxor/", "E3", '#NAME?').
?test(sheet1_F3, "/Bitxor/", "F3", '#NAME?').
?test(sheet1_G3, "/Bitxor/", "G3", '#NAME?').
?test(sheet1_H3, "/Bitxor/", "H3", '#NAME?').
?test(sheet1_I3, "/Bitxor/", "I3", '#NAME?').
?test(sheet1_J3, "/Bitxor/", "J3", '#NAME?').
?test(sheet1_K3, "/Bitxor/", "K3", '#NAME?').
?test(sheet1_L3, "/Bitxor/", "L3", '#NAME?').
?test(sheet1_M3, "/Bitxor/", "M3", '#NAME?').
?test(sheet1_N3, "/Bitxor/", "N3", '#NAME?').
?test(sheet1_O3, "/Bitxor/", "O3", '#NAME?').
?test(sheet1_P3, "/Bitxor/", "P3", '#NAME?').
?test(sheet1_Q3, "/Bitxor/", "Q3", '#NAME?').
?test(sheet1_R3, "/Bitxor/", "R3", '#NAME?').
?test(sheet1_S3, "/Bitxor/", "S3", '#NAME?').
?test(sheet1_T3, "/Bitxor/", "T3", '#NAME?').
?test(sheet1_U3, "/Bitxor/", "U3", '#NAME?').
?test(sheet1_V3, "/Bitxor/", "V3", '#NAME?').
?test(sheet1_X3, "/Bitxor/", "X3", 7.0).
?test(sheet1_Y3, "/Bitxor/", "Y3", 5.0).
?test(sheet1_Z3, "/Bitxor/", "Z3", 3.0).
?test(sheet1_AA3, "/Bitxor/", "AA3", 1.0).
?test(sheet1_A4, "/Bitxor/", "A4", "errors").
?test(sheet1_B4, "/Bitxor/", "B4", '#VALUE!').
?test(sheet1_C4, "/Bitxor/", "C4", '#NAME?').
?test(sheet1_D4, "/Bitxor/", "D4", '#NAME?').
?test(sheet1_E4, "/Bitxor/", "E4", '#NAME?').
?test(sheet1_F4, "/Bitxor/", "F4", '#NAME?').
?test(sheet1_G4, "/Bitxor/", "G4", '#NAME?').
?test(sheet1_H4, "/Bitxor/", "H4", '#NAME?').
?test(sheet1_I4, "/Bitxor/", "I4", '#NAME?').
?test(sheet1_J4, "/Bitxor/", "J4", '#NAME?').
?test(sheet1_K4, "/Bitxor/", "K4", '#NAME?').
?test(sheet1_L4, "/Bitxor/", "L4", '#NAME?').
?test(sheet1_M4, "/Bitxor/", "M4", '#NAME?').
?test(sheet1_N4, "/Bitxor/", "N4", '#NAME?').
?test(sheet1_O4, "/Bitxor/", "O4", '#NAME?').
?test(sheet1_P4, "/Bitxor/", "P4", '#NAME?').
?test(sheet1_Q4, "/Bitxor/", "Q4", '#NAME?').
?test(sheet1_R4, "/Bitxor/", "R4", '#NAME?').
?test(sheet1_S4, "/Bitxor/", "S4", '#NAME?').
?test(sheet1_T4, "/Bitxor/", "T4", '#NAME?').
?test(sheet1_U4, "/Bitxor/", "U4", '#NAME?').
?test(sheet1_V4, "/Bitxor/", "V4", '#NAME?').
?test(sheet1_X4, "/Bitxor/", "X4", 8.0).
?test(sheet1_Y4, "/Bitxor/", "Y4", 9.0).
?test(sheet1_Z4, "/Bitxor/", "Z4", 10.0).
?test(sheet1_AA4, "/Bitxor/", "AA4", 11.0).
?test(sheet1_A5, "/Bitxor/", "A5", "errors").
?test(sheet1_B5, "/Bitxor/", "B5", '#REF!').
?test(sheet1_C5, "/Bitxor/", "C5", '#NAME?').
?test(sheet1_D5, "/Bitxor/", "D5", '#NAME?').
?test(sheet1_E5, "/Bitxor/", "E5", '#NAME?').
?test(sheet1_F5, "/Bitxor/", "F5", '#NAME?').
?test(sheet1_G5, "/Bitxor/", "G5", '#NAME?').
?test(sheet1_H5, "/Bitxor/", "H5", '#NAME?').
?test(sheet1_I5, "/Bitxor/", "I5", '#NAME?').
?test(sheet1_J5, "/Bitxor/", "J5", '#NAME?').
?test(sheet1_K5, "/Bitxor/", "K5", '#NAME?').
?test(sheet1_L5, "/Bitxor/", "L5", '#NAME?').
?test(sheet1_M5, "/Bitxor/", "M5", '#NAME?').
?test(sheet1_N5, "/Bitxor/", "N5", '#NAME?').
?test(sheet1_O5, "/Bitxor/", "O5", '#NAME?').
?test(sheet1_P5, "/Bitxor/", "P5", '#NAME?').
?test(sheet1_Q5, "/Bitxor/", "Q5", '#NAME?').
?test(sheet1_R5, "/Bitxor/", "R5", '#NAME?').
?test(sheet1_S5, "/Bitxor/", "S5", '#NAME?').
?test(sheet1_T5, "/Bitxor/", "T5", '#NAME?').
?test(sheet1_U5, "/Bitxor/", "U5", '#NAME?').
?test(sheet1_V5, "/Bitxor/", "V5", '#NAME?').
?test(sheet1_X5, "/Bitxor/", "X5", 9.0).
?test(sheet1_Y5, "/Bitxor/", "Y5", 13.0).
?test(sheet1_Z5, "/Bitxor/", "Z5", 17.0).
?test(sheet1_AA5, "/Bitxor/", "AA5", 21.0).
?test(sheet1_A6, "/Bitxor/", "A6", "errors").
?test(sheet1_B6, "/Bitxor/", "B6", '#NAME?').
?test(sheet1_C6, "/Bitxor/", "C6", '#NAME?').
?test(sheet1_D6, "/Bitxor/", "D6", '#NAME?').
?test(sheet1_E6, "/Bitxor/", "E6", '#NAME?').
?test(sheet1_F6, "/Bitxor/", "F6", '#NAME?').
?test(sheet1_G6, "/Bitxor/", "G6", '#NAME?').
?test(sheet1_H6, "/Bitxor/", "H6", '#NAME?').
?test(sheet1_I6, "/Bitxor/", "I6", '#NAME?').
?test(sheet1_J6, "/Bitxor/", "J6", '#NAME?').
?test(sheet1_K6, "/Bitxor/", "K6", '#NAME?').
?test(sheet1_L6, "/Bitxor/", "L6", '#NAME?').
?test(sheet1_M6, "/Bitxor/", "M6", '#NAME?').
?test(sheet1_N6, "/Bitxor/", "N6", '#NAME?').
?test(sheet1_O6, "/Bitxor/", "O6", '#NAME?').
?test(sheet1_P6, "/Bitxor/", "P6", '#NAME?').
?test(sheet1_Q6, "/Bitxor/", "Q6", '#NAME?').
?test(sheet1_R6, "/Bitxor/", "R6", '#NAME?').
?test(sheet1_S6, "/Bitxor/", "S6", '#NAME?').
?test(sheet1_T6, "/Bitxor/", "T6", '#NAME?').
?test(sheet1_U6, "/Bitxor/", "U6", '#NAME?').
?test(sheet1_V6, "/Bitxor/", "V6", '#NAME?').
?test(sheet1_X6, "/Bitxor/", "X6", 10.0).
?test(sheet1_Y6, "/Bitxor/", "Y6", 17.0).
?test(sheet1_Z6, "/Bitxor/", "Z6", 24.0).
?test(sheet1_AA6, "/Bitxor/", "AA6", 31.0).
?test(sheet1_A7, "/Bitxor/", "A7", "errors").
?test(sheet1_B7, "/Bitxor/", "B7", '#NUM!').
?test(sheet1_C7, "/Bitxor/", "C7", '#NAME?').
?test(sheet1_D7, "/Bitxor/", "D7", '#NAME?').
?test(sheet1_E7, "/Bitxor/", "E7", '#NAME?').
?test(sheet1_F7, "/Bitxor/", "F7", '#NAME?').
?test(sheet1_G7, "/Bitxor/", "G7", '#NAME?').
?test(sheet1_H7, "/Bitxor/", "H7", '#NAME?').
?test(sheet1_I7, "/Bitxor/", "I7", '#NAME?').
?test(sheet1_J7, "/Bitxor/", "J7", '#NAME?').
?test(sheet1_K7, "/Bitxor/", "K7", '#NAME?').
?test(sheet1_L7, "/Bitxor/", "L7", '#NAME?').
?test(sheet1_M7, "/Bitxor/", "M7", '#NAME?').
?test(sheet1_N7, "/Bitxor/", "N7", '#NAME?').
?test(sheet1_O7, "/Bitxor/", "O7", '#NAME?').
?test(sheet1_P7, "/Bitxor/", "P7", '#NAME?').
?test(sheet1_Q7, "/Bitxor/", "Q7", '#NAME?').
?test(sheet1_R7, "/Bitxor/", "R7", '#NAME?').
?test(sheet1_S7, "/Bitxor/", "S7", '#NAME?').
?test(sheet1_T7, "/Bitxor/", "T7", '#NAME?').
?test(sheet1_U7, "/Bitxor/", "U7", '#NAME?').
?test(sheet1_V7, "/Bitxor/", "V7", '#NAME?').
?test(sheet1_A8, "/Bitxor/", "A8", "errors").
?test(sheet1_B8, "/Bitxor/", "B8", '#N/A').
?test(sheet1_C8, "/Bitxor/", "C8", '#NAME?').
?test(sheet1_D8, "/Bitxor/", "D8", '#NAME?').
?test(sheet1_E8, "/Bitxor/", "E8", '#NAME?').
?test(sheet1_F8, "/Bitxor/", "F8", '#NAME?').
?test(sheet1_G8, "/Bitxor/", "G8", '#NAME?').
?test(sheet1_H8, "/Bitxor/", "H8", '#NAME?').
?test(sheet1_I8, "/Bitxor/", "I8", '#NAME?').
?test(sheet1_J8, "/Bitxor/", "J8", '#NAME?').
?test(sheet1_K8, "/Bitxor/", "K8", '#NAME?').
?test(sheet1_L8, "/Bitxor/", "L8", '#NAME?').
?test(sheet1_M8, "/Bitxor/", "M8", '#NAME?').
?test(sheet1_N8, "/Bitxor/", "N8", '#NAME?').
?test(sheet1_O8, "/Bitxor/", "O8", '#NAME?').
?test(sheet1_P8, "/Bitxor/", "P8", '#NAME?').
?test(sheet1_Q8, "/Bitxor/", "Q8", '#NAME?').
?test(sheet1_R8, "/Bitxor/", "R8", '#NAME?').
?test(sheet1_S8, "/Bitxor/", "S8", '#NAME?').
?test(sheet1_T8, "/Bitxor/", "T8", '#NAME?').
?test(sheet1_U8, "/Bitxor/", "U8", '#NAME?').
?test(sheet1_V8, "/Bitxor/", "V8", '#NAME?').
?test(sheet1_A9, "/Bitxor/", "A9", "String").
?test(sheet1_B9, "/Bitxor/", "B9", "Phillip").
?test(sheet1_C9, "/Bitxor/", "C9", '#NAME?').
?test(sheet1_D9, "/Bitxor/", "D9", '#NAME?').
?test(sheet1_E9, "/Bitxor/", "E9", '#NAME?').
?test(sheet1_F9, "/Bitxor/", "F9", '#NAME?').
?test(sheet1_G9, "/Bitxor/", "G9", '#NAME?').
?test(sheet1_H9, "/Bitxor/", "H9", '#NAME?').
?test(sheet1_I9, "/Bitxor/", "I9", '#NAME?').
?test(sheet1_J9, "/Bitxor/", "J9", '#NAME?').
?test(sheet1_K9, "/Bitxor/", "K9", '#NAME?').
?test(sheet1_L9, "/Bitxor/", "L9", '#NAME?').
?test(sheet1_M9, "/Bitxor/", "M9", '#NAME?').
?test(sheet1_N9, "/Bitxor/", "N9", '#NAME?').
?test(sheet1_O9, "/Bitxor/", "O9", '#NAME?').
?test(sheet1_P9, "/Bitxor/", "P9", '#NAME?').
?test(sheet1_Q9, "/Bitxor/", "Q9", '#NAME?').
?test(sheet1_R9, "/Bitxor/", "R9", '#NAME?').
?test(sheet1_S9, "/Bitxor/", "S9", '#NAME?').
?test(sheet1_T9, "/Bitxor/", "T9", '#NAME?').
?test(sheet1_U9, "/Bitxor/", "U9", '#NAME?').
?test(sheet1_V9, "/Bitxor/", "V9", '#NAME?').
?test(sheet1_A10, "/Bitxor/", "A10", "String Number").
?test(sheet1_B10, "/Bitxor/", "B10", "12").
?test(sheet1_C10, "/Bitxor/", "C10", '#NAME?').
?test(sheet1_D10, "/Bitxor/", "D10", '#NAME?').
?test(sheet1_E10, "/Bitxor/", "E10", '#NAME?').
?test(sheet1_F10, "/Bitxor/", "F10", '#NAME?').
?test(sheet1_G10, "/Bitxor/", "G10", '#NAME?').
?test(sheet1_H10, "/Bitxor/", "H10", '#NAME?').
?test(sheet1_I10, "/Bitxor/", "I10", '#NAME?').
?test(sheet1_J10, "/Bitxor/", "J10", '#NAME?').
?test(sheet1_K10, "/Bitxor/", "K10", '#NAME?').
?test(sheet1_L10, "/Bitxor/", "L10", '#NAME?').
?test(sheet1_M10, "/Bitxor/", "M10", '#NAME?').
?test(sheet1_N10, "/Bitxor/", "N10", '#NAME?').
?test(sheet1_O10, "/Bitxor/", "O10", '#NAME?').
?test(sheet1_P10, "/Bitxor/", "P10", '#NAME?').
?test(sheet1_Q10, "/Bitxor/", "Q10", '#NAME?').
?test(sheet1_R10, "/Bitxor/", "R10", '#NAME?').
?test(sheet1_S10, "/Bitxor/", "S10", '#NAME?').
?test(sheet1_T10, "/Bitxor/", "T10", '#NAME?').
?test(sheet1_U10, "/Bitxor/", "U10", '#NAME?').
?test(sheet1_V10, "/Bitxor/", "V10", '#NAME?').
?test(sheet1_A11, "/Bitxor/", "A11", "String Number Leading space").
?test(sheet1_B11, "/Bitxor/", "B11", " 23").
?test(sheet1_C11, "/Bitxor/", "C11", '#NAME?').
?test(sheet1_D11, "/Bitxor/", "D11", '#NAME?').
?test(sheet1_E11, "/Bitxor/", "E11", '#NAME?').
?test(sheet1_F11, "/Bitxor/", "F11", '#NAME?').
?test(sheet1_G11, "/Bitxor/", "G11", '#NAME?').
?test(sheet1_H11, "/Bitxor/", "H11", '#NAME?').
?test(sheet1_I11, "/Bitxor/", "I11", '#NAME?').
?test(sheet1_J11, "/Bitxor/", "J11", '#NAME?').
?test(sheet1_K11, "/Bitxor/", "K11", '#NAME?').
?test(sheet1_L11, "/Bitxor/", "L11", '#NAME?').
?test(sheet1_M11, "/Bitxor/", "M11", '#NAME?').
?test(sheet1_N11, "/Bitxor/", "N11", '#NAME?').
?test(sheet1_O11, "/Bitxor/", "O11", '#NAME?').
?test(sheet1_P11, "/Bitxor/", "P11", '#NAME?').
?test(sheet1_Q11, "/Bitxor/", "Q11", '#NAME?').
?test(sheet1_R11, "/Bitxor/", "R11", '#NAME?').
?test(sheet1_S11, "/Bitxor/", "S11", '#NAME?').
?test(sheet1_T11, "/Bitxor/", "T11", '#NAME?').
?test(sheet1_U11, "/Bitxor/", "U11", '#NAME?').
?test(sheet1_V11, "/Bitxor/", "V11", '#NAME?').
?test(sheet1_A12, "/Bitxor/", "A12", "Interger").
?test(sheet1_B12, "/Bitxor/", "B12", "1968/03/23 00:00:00").
?test(sheet1_C12, "/Bitxor/", "C12", '#NAME?').
?test(sheet1_D12, "/Bitxor/", "D12", '#NAME?').
?test(sheet1_E12, "/Bitxor/", "E12", '#NAME?').
?test(sheet1_F12, "/Bitxor/", "F12", '#NAME?').
?test(sheet1_G12, "/Bitxor/", "G12", '#NAME?').
?test(sheet1_H12, "/Bitxor/", "H12", '#NAME?').
?test(sheet1_I12, "/Bitxor/", "I12", '#NAME?').
?test(sheet1_J12, "/Bitxor/", "J12", '#NAME?').
?test(sheet1_K12, "/Bitxor/", "K12", '#NAME?').
?test(sheet1_L12, "/Bitxor/", "L12", '#NAME?').
?test(sheet1_M12, "/Bitxor/", "M12", '#NAME?').
?test(sheet1_N12, "/Bitxor/", "N12", '#NAME?').
?test(sheet1_O12, "/Bitxor/", "O12", '#NAME?').
?test(sheet1_P12, "/Bitxor/", "P12", '#NAME?').
?test(sheet1_Q12, "/Bitxor/", "Q12", '#NAME?').
?test(sheet1_R12, "/Bitxor/", "R12", '#NAME?').
?test(sheet1_S12, "/Bitxor/", "S12", '#NAME?').
?test(sheet1_T12, "/Bitxor/", "T12", '#NAME?').
?test(sheet1_U12, "/Bitxor/", "U12", '#NAME?').
?test(sheet1_V12, "/Bitxor/", "V12", '#NAME?').
?test(sheet1_A13, "/Bitxor/", "A13", "Float").
?test(sheet1_B13, "/Bitxor/", "B13", 3.14159265358979).
?test(sheet1_C13, "/Bitxor/", "C13", '#NAME?').
?test(sheet1_D13, "/Bitxor/", "D13", '#NAME?').
?test(sheet1_E13, "/Bitxor/", "E13", '#NAME?').
?test(sheet1_F13, "/Bitxor/", "F13", '#NAME?').
?test(sheet1_G13, "/Bitxor/", "G13", '#NAME?').
?test(sheet1_H13, "/Bitxor/", "H13", '#NAME?').
?test(sheet1_I13, "/Bitxor/", "I13", '#NAME?').
?test(sheet1_J13, "/Bitxor/", "J13", '#NAME?').
?test(sheet1_K13, "/Bitxor/", "K13", '#NAME?').
?test(sheet1_L13, "/Bitxor/", "L13", '#NAME?').
?test(sheet1_M13, "/Bitxor/", "M13", '#NAME?').
?test(sheet1_N13, "/Bitxor/", "N13", '#NAME?').
?test(sheet1_O13, "/Bitxor/", "O13", '#NAME?').
?test(sheet1_P13, "/Bitxor/", "P13", '#NAME?').
?test(sheet1_Q13, "/Bitxor/", "Q13", '#NAME?').
?test(sheet1_R13, "/Bitxor/", "R13", '#NAME?').
?test(sheet1_S13, "/Bitxor/", "S13", '#NAME?').
?test(sheet1_T13, "/Bitxor/", "T13", '#NAME?').
?test(sheet1_U13, "/Bitxor/", "U13", '#NAME?').
?test(sheet1_V13, "/Bitxor/", "V13", '#NAME?').
?test(sheet1_A14, "/Bitxor/", "A14", "Blank").
?test(sheet1_C14, "/Bitxor/", "C14", '#NAME?').
?test(sheet1_D14, "/Bitxor/", "D14", '#NAME?').
?test(sheet1_E14, "/Bitxor/", "E14", '#NAME?').
?test(sheet1_F14, "/Bitxor/", "F14", '#NAME?').
?test(sheet1_G14, "/Bitxor/", "G14", '#NAME?').
?test(sheet1_H14, "/Bitxor/", "H14", '#NAME?').
?test(sheet1_I14, "/Bitxor/", "I14", '#NAME?').
?test(sheet1_J14, "/Bitxor/", "J14", '#NAME?').
?test(sheet1_K14, "/Bitxor/", "K14", '#NAME?').
?test(sheet1_L14, "/Bitxor/", "L14", '#NAME?').
?test(sheet1_M14, "/Bitxor/", "M14", '#NAME?').
?test(sheet1_N14, "/Bitxor/", "N14", '#NAME?').
?test(sheet1_O14, "/Bitxor/", "O14", '#NAME?').
?test(sheet1_P14, "/Bitxor/", "P14", '#NAME?').
?test(sheet1_Q14, "/Bitxor/", "Q14", '#NAME?').
?test(sheet1_R14, "/Bitxor/", "R14", '#NAME?').
?test(sheet1_S14, "/Bitxor/", "S14", '#NAME?').
?test(sheet1_T14, "/Bitxor/", "T14", '#NAME?').
?test(sheet1_U14, "/Bitxor/", "U14", '#NAME?').
?test(sheet1_V14, "/Bitxor/", "V14", '#NAME?').
?test(sheet1_A15, "/Bitxor/", "A15", "Logical").
?test(sheet1_B15, "/Bitxor/", "B15", true).
?test(sheet1_C15, "/Bitxor/", "C15", '#NAME?').
?test(sheet1_D15, "/Bitxor/", "D15", '#NAME?').
?test(sheet1_E15, "/Bitxor/", "E15", '#NAME?').
?test(sheet1_F15, "/Bitxor/", "F15", '#NAME?').
?test(sheet1_G15, "/Bitxor/", "G15", '#NAME?').
?test(sheet1_H15, "/Bitxor/", "H15", '#NAME?').
?test(sheet1_I15, "/Bitxor/", "I15", '#NAME?').
?test(sheet1_J15, "/Bitxor/", "J15", '#NAME?').
?test(sheet1_K15, "/Bitxor/", "K15", '#NAME?').
?test(sheet1_L15, "/Bitxor/", "L15", '#NAME?').
?test(sheet1_M15, "/Bitxor/", "M15", '#NAME?').
?test(sheet1_N15, "/Bitxor/", "N15", '#NAME?').
?test(sheet1_O15, "/Bitxor/", "O15", '#NAME?').
?test(sheet1_P15, "/Bitxor/", "P15", '#NAME?').
?test(sheet1_Q15, "/Bitxor/", "Q15", '#NAME?').
?test(sheet1_R15, "/Bitxor/", "R15", '#NAME?').
?test(sheet1_S15, "/Bitxor/", "S15", '#NAME?').
?test(sheet1_T15, "/Bitxor/", "T15", '#NAME?').
?test(sheet1_U15, "/Bitxor/", "U15", '#NAME?').
?test(sheet1_V15, "/Bitxor/", "V15", '#NAME?').
?test(sheet1_A16, "/Bitxor/", "A16", "Logical").
?test(sheet1_B16, "/Bitxor/", "B16", false).
?test(sheet1_C16, "/Bitxor/", "C16", '#NAME?').
?test(sheet1_D16, "/Bitxor/", "D16", '#NAME?').
?test(sheet1_E16, "/Bitxor/", "E16", '#NAME?').
?test(sheet1_F16, "/Bitxor/", "F16", '#NAME?').
?test(sheet1_G16, "/Bitxor/", "G16", '#NAME?').
?test(sheet1_H16, "/Bitxor/", "H16", '#NAME?').
?test(sheet1_I16, "/Bitxor/", "I16", '#NAME?').
?test(sheet1_J16, "/Bitxor/", "J16", '#NAME?').
?test(sheet1_K16, "/Bitxor/", "K16", '#NAME?').
?test(sheet1_L16, "/Bitxor/", "L16", '#NAME?').
?test(sheet1_M16, "/Bitxor/", "M16", '#NAME?').
?test(sheet1_N16, "/Bitxor/", "N16", '#NAME?').
?test(sheet1_O16, "/Bitxor/", "O16", '#NAME?').
?test(sheet1_P16, "/Bitxor/", "P16", '#NAME?').
?test(sheet1_Q16, "/Bitxor/", "Q16", '#NAME?').
?test(sheet1_R16, "/Bitxor/", "R16", '#NAME?').
?test(sheet1_S16, "/Bitxor/", "S16", '#NAME?').
?test(sheet1_T16, "/Bitxor/", "T16", '#NAME?').
?test(sheet1_U16, "/Bitxor/", "U16", '#NAME?').
?test(sheet1_V16, "/Bitxor/", "V16", '#NAME?').
?test(sheet1_A17, "/Bitxor/", "A17", "Range Row").
?test(sheet1_B17, "/Bitxor/", "B17", "X3:Y3").
?test(sheet1_C17, "/Bitxor/", "C17", '#NAME?').
?test(sheet1_D17, "/Bitxor/", "D17", '#NAME?').
?test(sheet1_E17, "/Bitxor/", "E17", '#NAME?').
?test(sheet1_F17, "/Bitxor/", "F17", '#NAME?').
?test(sheet1_G17, "/Bitxor/", "G17", '#NAME?').
?test(sheet1_H17, "/Bitxor/", "H17", '#NAME?').
?test(sheet1_I17, "/Bitxor/", "I17", '#NAME?').
?test(sheet1_J17, "/Bitxor/", "J17", '#NAME?').
?test(sheet1_K17, "/Bitxor/", "K17", '#NAME?').
?test(sheet1_L17, "/Bitxor/", "L17", '#NAME?').
?test(sheet1_M17, "/Bitxor/", "M17", '#NAME?').
?test(sheet1_N17, "/Bitxor/", "N17", '#NAME?').
?test(sheet1_O17, "/Bitxor/", "O17", '#NAME?').
?test(sheet1_P17, "/Bitxor/", "P17", '#NAME?').
?test(sheet1_Q17, "/Bitxor/", "Q17", '#NAME?').
?test(sheet1_R17, "/Bitxor/", "R17", '#NAME?').
?test(sheet1_S17, "/Bitxor/", "S17", '#NAME?').
?test(sheet1_T17, "/Bitxor/", "T17", '#NAME?').
?test(sheet1_U17, "/Bitxor/", "U17", '#NAME?').
?test(sheet1_V17, "/Bitxor/", "V17", '#NAME?').
?test(sheet1_A18, "/Bitxor/", "A18", "Range Row").
?test(sheet1_B18, "/Bitxor/", "B18", "X3:AA3").
?test(sheet1_C18, "/Bitxor/", "C18", '#NAME?').
?test(sheet1_D18, "/Bitxor/", "D18", '#NAME?').
?test(sheet1_E18, "/Bitxor/", "E18", '#NAME?').
?test(sheet1_F18, "/Bitxor/", "F18", '#NAME?').
?test(sheet1_G18, "/Bitxor/", "G18", '#NAME?').
?test(sheet1_H18, "/Bitxor/", "H18", '#NAME?').
?test(sheet1_I18, "/Bitxor/", "I18", '#NAME?').
?test(sheet1_J18, "/Bitxor/", "J18", '#NAME?').
?test(sheet1_K18, "/Bitxor/", "K18", '#NAME?').
?test(sheet1_L18, "/Bitxor/", "L18", '#NAME?').
?test(sheet1_M18, "/Bitxor/", "M18", '#NAME?').
?test(sheet1_N18, "/Bitxor/", "N18", '#NAME?').
?test(sheet1_O18, "/Bitxor/", "O18", '#NAME?').
?test(sheet1_P18, "/Bitxor/", "P18", '#NAME?').
?test(sheet1_Q18, "/Bitxor/", "Q18", '#NAME?').
?test(sheet1_R18, "/Bitxor/", "R18", '#NAME?').
?test(sheet1_S18, "/Bitxor/", "S18", '#NAME?').
?test(sheet1_T18, "/Bitxor/", "T18", '#NAME?').
?test(sheet1_U18, "/Bitxor/", "U18", '#NAME?').
?test(sheet1_V18, "/Bitxor/", "V18", '#NAME?').
?test(sheet1_A19, "/Bitxor/", "A19", "Range Area").
?test(sheet1_B19, "/Bitxor/", "B19", "X3:Y4").
?test(sheet1_C19, "/Bitxor/", "C19", '#NAME?').
?test(sheet1_D19, "/Bitxor/", "D19", '#NAME?').
?test(sheet1_E19, "/Bitxor/", "E19", '#NAME?').
?test(sheet1_F19, "/Bitxor/", "F19", '#NAME?').
?test(sheet1_G19, "/Bitxor/", "G19", '#NAME?').
?test(sheet1_H19, "/Bitxor/", "H19", '#NAME?').
?test(sheet1_I19, "/Bitxor/", "I19", '#NAME?').
?test(sheet1_J19, "/Bitxor/", "J19", '#NAME?').
?test(sheet1_K19, "/Bitxor/", "K19", '#NAME?').
?test(sheet1_L19, "/Bitxor/", "L19", '#NAME?').
?test(sheet1_M19, "/Bitxor/", "M19", '#NAME?').
?test(sheet1_N19, "/Bitxor/", "N19", '#NAME?').
?test(sheet1_O19, "/Bitxor/", "O19", '#NAME?').
?test(sheet1_P19, "/Bitxor/", "P19", '#NAME?').
?test(sheet1_Q19, "/Bitxor/", "Q19", '#NAME?').
?test(sheet1_R19, "/Bitxor/", "R19", '#NAME?').
?test(sheet1_S19, "/Bitxor/", "S19", '#NAME?').
?test(sheet1_T19, "/Bitxor/", "T19", '#NAME?').
?test(sheet1_U19, "/Bitxor/", "U19", '#NAME?').
?test(sheet1_V19, "/Bitxor/", "V19", '#NAME?').
?test(sheet1_A20, "/Bitxor/", "A20", "Range Area").
?test(sheet1_B20, "/Bitxor/", "B20", "X3:AA6").
?test(sheet1_C20, "/Bitxor/", "C20", '#NAME?').
?test(sheet1_D20, "/Bitxor/", "D20", '#NAME?').
?test(sheet1_E20, "/Bitxor/", "E20", '#NAME?').
?test(sheet1_F20, "/Bitxor/", "F20", '#NAME?').
?test(sheet1_G20, "/Bitxor/", "G20", '#NAME?').
?test(sheet1_H20, "/Bitxor/", "H20", '#NAME?').
?test(sheet1_I20, "/Bitxor/", "I20", '#NAME?').
?test(sheet1_J20, "/Bitxor/", "J20", '#NAME?').
?test(sheet1_K20, "/Bitxor/", "K20", '#NAME?').
?test(sheet1_L20, "/Bitxor/", "L20", '#NAME?').
?test(sheet1_M20, "/Bitxor/", "M20", '#NAME?').
?test(sheet1_N20, "/Bitxor/", "N20", '#NAME?').
?test(sheet1_O20, "/Bitxor/", "O20", '#NAME?').
?test(sheet1_P20, "/Bitxor/", "P20", '#NAME?').
?test(sheet1_Q20, "/Bitxor/", "Q20", '#NAME?').
?test(sheet1_R20, "/Bitxor/", "R20", '#NAME?').
?test(sheet1_S20, "/Bitxor/", "S20", '#NAME?').
?test(sheet1_T20, "/Bitxor/", "T20", '#NAME?').
?test(sheet1_U20, "/Bitxor/", "U20", '#NAME?').
?test(sheet1_V20, "/Bitxor/", "V20", '#NAME?').
?test(sheet1_A21, "/Bitxor/", "A21", "Range Colunm").
?test(sheet1_B21, "/Bitxor/", "B21", "X3:X4").
?test(sheet1_C21, "/Bitxor/", "C21", '#NAME?').
?test(sheet1_D21, "/Bitxor/", "D21", '#NAME?').
?test(sheet1_E21, "/Bitxor/", "E21", '#NAME?').
?test(sheet1_F21, "/Bitxor/", "F21", '#NAME?').
?test(sheet1_G21, "/Bitxor/", "G21", '#NAME?').
?test(sheet1_H21, "/Bitxor/", "H21", '#NAME?').
?test(sheet1_I21, "/Bitxor/", "I21", '#NAME?').
?test(sheet1_J21, "/Bitxor/", "J21", '#NAME?').
?test(sheet1_K21, "/Bitxor/", "K21", '#NAME?').
?test(sheet1_L21, "/Bitxor/", "L21", '#NAME?').
?test(sheet1_M21, "/Bitxor/", "M21", '#NAME?').
?test(sheet1_N21, "/Bitxor/", "N21", '#NAME?').
?test(sheet1_O21, "/Bitxor/", "O21", '#NAME?').
?test(sheet1_P21, "/Bitxor/", "P21", '#NAME?').
?test(sheet1_Q21, "/Bitxor/", "Q21", '#NAME?').
?test(sheet1_R21, "/Bitxor/", "R21", '#NAME?').
?test(sheet1_S21, "/Bitxor/", "S21", '#NAME?').
?test(sheet1_T21, "/Bitxor/", "T21", '#NAME?').
?test(sheet1_U21, "/Bitxor/", "U21", '#NAME?').
?test(sheet1_V21, "/Bitxor/", "V21", '#NAME?').
?test(sheet1_A22, "/Bitxor/", "A22", "Range Colunm").
?test(sheet1_B22, "/Bitxor/", "B22", "X3:X6").
?test(sheet1_C22, "/Bitxor/", "C22", '#NAME?').
?test(sheet1_D22, "/Bitxor/", "D22", '#NAME?').
?test(sheet1_E22, "/Bitxor/", "E22", '#NAME?').
?test(sheet1_F22, "/Bitxor/", "F22", '#NAME?').
?test(sheet1_G22, "/Bitxor/", "G22", '#NAME?').
?test(sheet1_H22, "/Bitxor/", "H22", '#NAME?').
?test(sheet1_I22, "/Bitxor/", "I22", '#NAME?').
?test(sheet1_J22, "/Bitxor/", "J22", '#NAME?').
?test(sheet1_K22, "/Bitxor/", "K22", '#NAME?').
?test(sheet1_L22, "/Bitxor/", "L22", '#NAME?').
?test(sheet1_M22, "/Bitxor/", "M22", '#NAME?').
?test(sheet1_N22, "/Bitxor/", "N22", '#NAME?').
?test(sheet1_O22, "/Bitxor/", "O22", '#NAME?').
?test(sheet1_P22, "/Bitxor/", "P22", '#NAME?').
?test(sheet1_Q22, "/Bitxor/", "Q22", '#NAME?').
?test(sheet1_R22, "/Bitxor/", "R22", '#NAME?').
?test(sheet1_S22, "/Bitxor/", "S22", '#NAME?').
?test(sheet1_T22, "/Bitxor/", "T22", '#NAME?').
?test(sheet1_U22, "/Bitxor/", "U22", '#NAME?').
?test(sheet1_V22, "/Bitxor/", "V22", '#NAME?').
?test(sheet1_A25, "/Bitxor/", "A25", "bitxor(A,B)").
?test(sheet1_B25, "/Bitxor/", "B25", "B").
?test(sheet1_C25, "/Bitxor/", "C25", "errors").
?test(sheet1_D25, "/Bitxor/", "D25", "errors").
?test(sheet1_E25, "/Bitxor/", "E25", "errors").
?test(sheet1_F25, "/Bitxor/", "F25", "errors").
?test(sheet1_G25, "/Bitxor/", "G25", "errors").
?test(sheet1_H25, "/Bitxor/", "H25", "errors").
?test(sheet1_I25, "/Bitxor/", "I25", "String").
?test(sheet1_J25, "/Bitxor/", "J25", "String Number").
?test(sheet1_K25, "/Bitxor/", "K25", "String number Leading space").
?test(sheet1_L25, "/Bitxor/", "L25", "Integer").
?test(sheet1_M25, "/Bitxor/", "M25", "Float").
?test(sheet1_N25, "/Bitxor/", "N25", "Blank").
?test(sheet1_O25, "/Bitxor/", "O25", "Logical").
?test(sheet1_P25, "/Bitxor/", "P25", "Logical").
?test(sheet1_Q25, "/Bitxor/", "Q25", "Range Row").
?test(sheet1_R25, "/Bitxor/", "R25", "Range Row").
?test(sheet1_S25, "/Bitxor/", "S25", "Range Area").
?test(sheet1_T25, "/Bitxor/", "T25", "Range Area").
?test(sheet1_U25, "/Bitxor/", "U25", "Range Colunm").
?test(sheet1_V25, "/Bitxor/", "V25", "Range Colunm").
?test(sheet1_A26, "/Bitxor/", "A26", "A").
?test(sheet1_C26, "/Bitxor/", "C26", '#DIV/0!').
?test(sheet1_D26, "/Bitxor/", "D26", '#VALUE!').
?test(sheet1_E26, "/Bitxor/", "E26", '#REF!').
?test(sheet1_F26, "/Bitxor/", "F26", '#NAME?').
?test(sheet1_G26, "/Bitxor/", "G26", '#NUM!').
?test(sheet1_H26, "/Bitxor/", "H26", '#N/A').
?test(sheet1_I26, "/Bitxor/", "I26", "Phillip").
?test(sheet1_J26, "/Bitxor/", "J26", "13").
?test(sheet1_K26, "/Bitxor/", "K26", " 24").
?test(sheet1_L26, "/Bitxor/", "L26", "1968/03/23 00:00:00").
?test(sheet1_M26, "/Bitxor/", "M26", 3.14159265358979).
?test(sheet1_O26, "/Bitxor/", "O26", true).
?test(sheet1_P26, "/Bitxor/", "P26", false).
?test(sheet1_Q26, "/Bitxor/", "Q26", "X3:Y3").
?test(sheet1_R26, "/Bitxor/", "R26", "X3:AA3").
?test(sheet1_S26, "/Bitxor/", "S26", "X3:Y4").
?test(sheet1_T26, "/Bitxor/", "T26", "X3:AA6").
?test(sheet1_U26, "/Bitxor/", "U26", "X3:X4").
?test(sheet1_V26, "/Bitxor/", "V26", "X3:X6").
?test(sheet1_A27, "/Bitxor/", "A27", "errors").
?test(sheet1_B27, "/Bitxor/", "B27", '#DIV/0!').
?test(sheet1_C27, "/Bitxor/", "C27", '#DIV/0!').
?test(sheet1_D27, "/Bitxor/", "D27", '#DIV/0!').
?test(sheet1_E27, "/Bitxor/", "E27", '#DIV/0!').
?test(sheet1_F27, "/Bitxor/", "F27", '#DIV/0!').
?test(sheet1_G27, "/Bitxor/", "G27", '#DIV/0!').
?test(sheet1_H27, "/Bitxor/", "H27", '#DIV/0!').
?test(sheet1_I27, "/Bitxor/", "I27", '#DIV/0!').
?test(sheet1_J27, "/Bitxor/", "J27", '#DIV/0!').
?test(sheet1_K27, "/Bitxor/", "K27", '#DIV/0!').
?test(sheet1_L27, "/Bitxor/", "L27", '#DIV/0!').
?test(sheet1_M27, "/Bitxor/", "M27", '#DIV/0!').
?test(sheet1_N27, "/Bitxor/", "N27", '#DIV/0!').
?test(sheet1_O27, "/Bitxor/", "O27", '#DIV/0!').
?test(sheet1_P27, "/Bitxor/", "P27", '#DIV/0!').
?test(sheet1_Q27, "/Bitxor/", "Q27", '#DIV/0!').
?test(sheet1_R27, "/Bitxor/", "R27", '#DIV/0!').
?test(sheet1_S27, "/Bitxor/", "S27", '#DIV/0!').
?test(sheet1_T27, "/Bitxor/", "T27", '#DIV/0!').
?test(sheet1_U27, "/Bitxor/", "U27", '#DIV/0!').
?test(sheet1_V27, "/Bitxor/", "V27", '#DIV/0!').
?test(sheet1_A28, "/Bitxor/", "A28", "errors").
?test(sheet1_B28, "/Bitxor/", "B28", '#VALUE!').
?test(sheet1_C28, "/Bitxor/", "C28", '#VALUE!').
?test(sheet1_D28, "/Bitxor/", "D28", '#VALUE!').
?test(sheet1_E28, "/Bitxor/", "E28", '#VALUE!').
?test(sheet1_F28, "/Bitxor/", "F28", '#VALUE!').
?test(sheet1_G28, "/Bitxor/", "G28", '#VALUE!').
?test(sheet1_H28, "/Bitxor/", "H28", '#VALUE!').
?test(sheet1_I28, "/Bitxor/", "I28", '#VALUE!').
?test(sheet1_J28, "/Bitxor/", "J28", '#VALUE!').
?test(sheet1_K28, "/Bitxor/", "K28", '#VALUE!').
?test(sheet1_L28, "/Bitxor/", "L28", '#VALUE!').
?test(sheet1_M28, "/Bitxor/", "M28", '#VALUE!').
?test(sheet1_N28, "/Bitxor/", "N28", '#VALUE!').
?test(sheet1_O28, "/Bitxor/", "O28", '#VALUE!').
?test(sheet1_P28, "/Bitxor/", "P28", '#VALUE!').
?test(sheet1_Q28, "/Bitxor/", "Q28", '#VALUE!').
?test(sheet1_R28, "/Bitxor/", "R28", '#VALUE!').
?test(sheet1_S28, "/Bitxor/", "S28", '#VALUE!').
?test(sheet1_T28, "/Bitxor/", "T28", '#VALUE!').
?test(sheet1_U28, "/Bitxor/", "U28", '#VALUE!').
?test(sheet1_V28, "/Bitxor/", "V28", '#VALUE!').
?test(sheet1_A29, "/Bitxor/", "A29", "errors").
?test(sheet1_B29, "/Bitxor/", "B29", '#REF!').
?test(sheet1_C29, "/Bitxor/", "C29", '#REF!').
?test(sheet1_D29, "/Bitxor/", "D29", '#REF!').
?test(sheet1_E29, "/Bitxor/", "E29", '#REF!').
?test(sheet1_F29, "/Bitxor/", "F29", '#REF!').
?test(sheet1_G29, "/Bitxor/", "G29", '#REF!').
?test(sheet1_H29, "/Bitxor/", "H29", '#REF!').
?test(sheet1_I29, "/Bitxor/", "I29", '#REF!').
?test(sheet1_J29, "/Bitxor/", "J29", '#REF!').
?test(sheet1_K29, "/Bitxor/", "K29", '#REF!').
?test(sheet1_L29, "/Bitxor/", "L29", '#REF!').
?test(sheet1_M29, "/Bitxor/", "M29", '#REF!').
?test(sheet1_N29, "/Bitxor/", "N29", '#REF!').
?test(sheet1_O29, "/Bitxor/", "O29", '#REF!').
?test(sheet1_P29, "/Bitxor/", "P29", '#REF!').
?test(sheet1_Q29, "/Bitxor/", "Q29", '#REF!').
?test(sheet1_R29, "/Bitxor/", "R29", '#REF!').
?test(sheet1_S29, "/Bitxor/", "S29", '#REF!').
?test(sheet1_T29, "/Bitxor/", "T29", '#REF!').
?test(sheet1_U29, "/Bitxor/", "U29", '#REF!').
?test(sheet1_V29, "/Bitxor/", "V29", '#REF!').
?test(sheet1_A30, "/Bitxor/", "A30", "errors").
?test(sheet1_B30, "/Bitxor/", "B30", '#NAME?').
?test(sheet1_C30, "/Bitxor/", "C30", '#NAME?').
?test(sheet1_D30, "/Bitxor/", "D30", '#NAME?').
?test(sheet1_E30, "/Bitxor/", "E30", '#NAME?').
?test(sheet1_F30, "/Bitxor/", "F30", '#NAME?').
?test(sheet1_G30, "/Bitxor/", "G30", '#NAME?').
?test(sheet1_H30, "/Bitxor/", "H30", '#NAME?').
?test(sheet1_I30, "/Bitxor/", "I30", '#NAME?').
?test(sheet1_J30, "/Bitxor/", "J30", '#NAME?').
?test(sheet1_K30, "/Bitxor/", "K30", '#NAME?').
?test(sheet1_L30, "/Bitxor/", "L30", '#NAME?').
?test(sheet1_M30, "/Bitxor/", "M30", '#NAME?').
?test(sheet1_N30, "/Bitxor/", "N30", '#NAME?').
?test(sheet1_O30, "/Bitxor/", "O30", '#NAME?').
?test(sheet1_P30, "/Bitxor/", "P30", '#NAME?').
?test(sheet1_Q30, "/Bitxor/", "Q30", '#NAME?').
?test(sheet1_R30, "/Bitxor/", "R30", '#NAME?').
?test(sheet1_S30, "/Bitxor/", "S30", '#NAME?').
?test(sheet1_T30, "/Bitxor/", "T30", '#NAME?').
?test(sheet1_U30, "/Bitxor/", "U30", '#NAME?').
?test(sheet1_V30, "/Bitxor/", "V30", '#NAME?').
?test(sheet1_A31, "/Bitxor/", "A31", "errors").
?test(sheet1_B31, "/Bitxor/", "B31", '#NUM!').
?test(sheet1_C31, "/Bitxor/", "C31", '#NUM!').
?test(sheet1_D31, "/Bitxor/", "D31", '#NUM!').
?test(sheet1_E31, "/Bitxor/", "E31", '#NUM!').
?test(sheet1_F31, "/Bitxor/", "F31", '#NUM!').
?test(sheet1_G31, "/Bitxor/", "G31", '#NUM!').
?test(sheet1_H31, "/Bitxor/", "H31", '#NUM!').
?test(sheet1_I31, "/Bitxor/", "I31", '#NUM!').
?test(sheet1_J31, "/Bitxor/", "J31", '#NUM!').
?test(sheet1_K31, "/Bitxor/", "K31", '#NUM!').
?test(sheet1_L31, "/Bitxor/", "L31", '#NUM!').
?test(sheet1_M31, "/Bitxor/", "M31", '#NUM!').
?test(sheet1_N31, "/Bitxor/", "N31", '#NUM!').
?test(sheet1_O31, "/Bitxor/", "O31", '#NUM!').
?test(sheet1_P31, "/Bitxor/", "P31", '#NUM!').
?test(sheet1_Q31, "/Bitxor/", "Q31", '#NUM!').
?test(sheet1_R31, "/Bitxor/", "R31", '#NUM!').
?test(sheet1_S31, "/Bitxor/", "S31", '#NUM!').
?test(sheet1_T31, "/Bitxor/", "T31", '#NUM!').
?test(sheet1_U31, "/Bitxor/", "U31", '#NUM!').
?test(sheet1_V31, "/Bitxor/", "V31", '#NUM!').
?test(sheet1_A32, "/Bitxor/", "A32", "errors").
?test(sheet1_B32, "/Bitxor/", "B32", '#N/A').
?test(sheet1_C32, "/Bitxor/", "C32", '#N/A').
?test(sheet1_D32, "/Bitxor/", "D32", '#N/A').
?test(sheet1_E32, "/Bitxor/", "E32", '#N/A').
?test(sheet1_F32, "/Bitxor/", "F32", '#N/A').
?test(sheet1_G32, "/Bitxor/", "G32", '#N/A').
?test(sheet1_H32, "/Bitxor/", "H32", '#N/A').
?test(sheet1_I32, "/Bitxor/", "I32", '#N/A').
?test(sheet1_J32, "/Bitxor/", "J32", '#N/A').
?test(sheet1_K32, "/Bitxor/", "K32", '#N/A').
?test(sheet1_L32, "/Bitxor/", "L32", '#N/A').
?test(sheet1_M32, "/Bitxor/", "M32", '#N/A').
?test(sheet1_N32, "/Bitxor/", "N32", '#N/A').
?test(sheet1_O32, "/Bitxor/", "O32", '#N/A').
?test(sheet1_P32, "/Bitxor/", "P32", '#N/A').
?test(sheet1_Q32, "/Bitxor/", "Q32", '#N/A').
?test(sheet1_R32, "/Bitxor/", "R32", '#N/A').
?test(sheet1_S32, "/Bitxor/", "S32", '#N/A').
?test(sheet1_T32, "/Bitxor/", "T32", '#N/A').
?test(sheet1_U32, "/Bitxor/", "U32", '#N/A').
?test(sheet1_V32, "/Bitxor/", "V32", '#N/A').
?test(sheet1_A33, "/Bitxor/", "A33", "String").
?test(sheet1_B33, "/Bitxor/", "B33", "Phillip").
?test(sheet1_C33, "/Bitxor/", "C33", '#VALUE!').
?test(sheet1_D33, "/Bitxor/", "D33", '#VALUE!').
?test(sheet1_E33, "/Bitxor/", "E33", '#VALUE!').
?test(sheet1_F33, "/Bitxor/", "F33", '#VALUE!').
?test(sheet1_G33, "/Bitxor/", "G33", '#VALUE!').
?test(sheet1_H33, "/Bitxor/", "H33", '#VALUE!').
?test(sheet1_I33, "/Bitxor/", "I33", '#VALUE!').
?test(sheet1_J33, "/Bitxor/", "J33", '#VALUE!').
?test(sheet1_K33, "/Bitxor/", "K33", '#VALUE!').
?test(sheet1_L33, "/Bitxor/", "L33", '#VALUE!').
?test(sheet1_M33, "/Bitxor/", "M33", '#VALUE!').
?test(sheet1_N33, "/Bitxor/", "N33", '#VALUE!').
?test(sheet1_O33, "/Bitxor/", "O33", '#VALUE!').
?test(sheet1_P33, "/Bitxor/", "P33", '#VALUE!').
?test(sheet1_Q33, "/Bitxor/", "Q33", '#VALUE!').
?test(sheet1_R33, "/Bitxor/", "R33", '#VALUE!').
?test(sheet1_S33, "/Bitxor/", "S33", '#VALUE!').
?test(sheet1_T33, "/Bitxor/", "T33", '#VALUE!').
?test(sheet1_U33, "/Bitxor/", "U33", '#VALUE!').
?test(sheet1_V33, "/Bitxor/", "V33", '#VALUE!').
?test(sheet1_A34, "/Bitxor/", "A34", "String Number").
?test(sheet1_B34, "/Bitxor/", "B34", "12").
?test(sheet1_C34, "/Bitxor/", "C34", '#DIV/0!').
?test(sheet1_D34, "/Bitxor/", "D34", '#VALUE!').
?test(sheet1_E34, "/Bitxor/", "E34", '#REF!').
?test(sheet1_F34, "/Bitxor/", "F34", '#NAME?').
?test(sheet1_G34, "/Bitxor/", "G34", '#NUM!').
?test(sheet1_H34, "/Bitxor/", "H34", '#N/A').
?test(sheet1_I34, "/Bitxor/", "I34", '#VALUE!').
?test(sheet1_J34, "/Bitxor/", "J34", 1.0).
?test(sheet1_K34, "/Bitxor/", "K34", 20.0).
?test(sheet1_L34, "/Bitxor/", "L34", 24916.0).
?test(sheet1_M34, "/Bitxor/", "M34", 15.0).
?test(sheet1_N34, "/Bitxor/", "N34", 12.0).
?test(sheet1_O34, "/Bitxor/", "O34", 13.0).
?test(sheet1_P34, "/Bitxor/", "P34", 12.0).
?test(sheet1_Q34, "/Bitxor/", "Q34", '#VALUE!').
?test(sheet1_R34, "/Bitxor/", "R34", '#VALUE!').
?test(sheet1_S34, "/Bitxor/", "S34", '#VALUE!').
?test(sheet1_T34, "/Bitxor/", "T34", '#VALUE!').
?test(sheet1_U34, "/Bitxor/", "U34", '#VALUE!').
?test(sheet1_V34, "/Bitxor/", "V34", '#VALUE!').
?test(sheet1_A35, "/Bitxor/", "A35", "String Number Leading space").
?test(sheet1_B35, "/Bitxor/", "B35", " 23").
?test(sheet1_C35, "/Bitxor/", "C35", '#DIV/0!').
?test(sheet1_D35, "/Bitxor/", "D35", '#VALUE!').
?test(sheet1_E35, "/Bitxor/", "E35", '#REF!').
?test(sheet1_F35, "/Bitxor/", "F35", '#NAME?').
?test(sheet1_G35, "/Bitxor/", "G35", '#NUM!').
?test(sheet1_H35, "/Bitxor/", "H35", '#N/A').
?test(sheet1_I35, "/Bitxor/", "I35", '#VALUE!').
?test(sheet1_J35, "/Bitxor/", "J35", 26.0).
?test(sheet1_K35, "/Bitxor/", "K35", 15.0).
?test(sheet1_L35, "/Bitxor/", "L35", 24911.0).
?test(sheet1_M35, "/Bitxor/", "M35", 20.0).
?test(sheet1_N35, "/Bitxor/", "N35", 23.0).
?test(sheet1_O35, "/Bitxor/", "O35", 22.0).
?test(sheet1_P35, "/Bitxor/", "P35", 23.0).
?test(sheet1_Q35, "/Bitxor/", "Q35", '#VALUE!').
?test(sheet1_R35, "/Bitxor/", "R35", '#VALUE!').
?test(sheet1_S35, "/Bitxor/", "S35", '#VALUE!').
?test(sheet1_T35, "/Bitxor/", "T35", '#VALUE!').
?test(sheet1_U35, "/Bitxor/", "U35", '#VALUE!').
?test(sheet1_V35, "/Bitxor/", "V35", '#VALUE!').
?test(sheet1_A36, "/Bitxor/", "A36", "Interger").
?test(sheet1_B36, "/Bitxor/", "B36", "1968/03/23 00:00:00").
?test(sheet1_C36, "/Bitxor/", "C36", '#DIV/0!').
?test(sheet1_D36, "/Bitxor/", "D36", '#VALUE!').
?test(sheet1_E36, "/Bitxor/", "E36", '#REF!').
?test(sheet1_F36, "/Bitxor/", "F36", '#NAME?').
?test(sheet1_G36, "/Bitxor/", "G36", '#NUM!').
?test(sheet1_H36, "/Bitxor/", "H36", '#N/A').
?test(sheet1_I36, "/Bitxor/", "I36", '#VALUE!').
?test(sheet1_J36, "/Bitxor/", "J36", 24917.0).
?test(sheet1_K36, "/Bitxor/", "K36", 24896.0).
?test(sheet1_L36, "/Bitxor/", "L36", 0.0).
?test(sheet1_M36, "/Bitxor/", "M36", 24923.0).
?test(sheet1_N36, "/Bitxor/", "N36", 24920.0).
?test(sheet1_O36, "/Bitxor/", "O36", 24921.0).
?test(sheet1_P36, "/Bitxor/", "P36", 24920.0).
?test(sheet1_Q36, "/Bitxor/", "Q36", '#VALUE!').
?test(sheet1_R36, "/Bitxor/", "R36", '#VALUE!').
?test(sheet1_S36, "/Bitxor/", "S36", '#VALUE!').
?test(sheet1_T36, "/Bitxor/", "T36", '#VALUE!').
?test(sheet1_U36, "/Bitxor/", "U36", '#VALUE!').
?test(sheet1_V36, "/Bitxor/", "V36", '#VALUE!').
?test(sheet1_A37, "/Bitxor/", "A37", "Float").
?test(sheet1_B37, "/Bitxor/", "B37", 3.14159265358979).
?test(sheet1_C37, "/Bitxor/", "C37", '#DIV/0!').
?test(sheet1_D37, "/Bitxor/", "D37", '#VALUE!').
?test(sheet1_E37, "/Bitxor/", "E37", '#REF!').
?test(sheet1_F37, "/Bitxor/", "F37", '#NAME?').
?test(sheet1_G37, "/Bitxor/", "G37", '#NUM!').
?test(sheet1_H37, "/Bitxor/", "H37", '#N/A').
?test(sheet1_I37, "/Bitxor/", "I37", '#VALUE!').
?test(sheet1_J37, "/Bitxor/", "J37", 14.0).
?test(sheet1_K37, "/Bitxor/", "K37", 27.0).
?test(sheet1_L37, "/Bitxor/", "L37", 24923.0).
?test(sheet1_M37, "/Bitxor/", "M37", 0.0).
?test(sheet1_N37, "/Bitxor/", "N37", 3.0).
?test(sheet1_O37, "/Bitxor/", "O37", 2.0).
?test(sheet1_P37, "/Bitxor/", "P37", 3.0).
?test(sheet1_Q37, "/Bitxor/", "Q37", '#VALUE!').
?test(sheet1_R37, "/Bitxor/", "R37", '#VALUE!').
?test(sheet1_S37, "/Bitxor/", "S37", '#VALUE!').
?test(sheet1_T37, "/Bitxor/", "T37", '#VALUE!').
?test(sheet1_U37, "/Bitxor/", "U37", '#VALUE!').
?test(sheet1_V37, "/Bitxor/", "V37", '#VALUE!').
?test(sheet1_A38, "/Bitxor/", "A38", "Blank").
?test(sheet1_C38, "/Bitxor/", "C38", '#DIV/0!').
?test(sheet1_D38, "/Bitxor/", "D38", '#VALUE!').
?test(sheet1_E38, "/Bitxor/", "E38", '#REF!').
?test(sheet1_F38, "/Bitxor/", "F38", '#NAME?').
?test(sheet1_G38, "/Bitxor/", "G38", '#NUM!').
?test(sheet1_H38, "/Bitxor/", "H38", '#N/A').
?test(sheet1_I38, "/Bitxor/", "I38", '#VALUE!').
?test(sheet1_J38, "/Bitxor/", "J38", 13.0).
?test(sheet1_K38, "/Bitxor/", "K38", 24.0).
?test(sheet1_L38, "/Bitxor/", "L38", 24920.0).
?test(sheet1_M38, "/Bitxor/", "M38", 3.0).
?test(sheet1_N38, "/Bitxor/", "N38", 0.0).
?test(sheet1_O38, "/Bitxor/", "O38", 1.0).
?test(sheet1_P38, "/Bitxor/", "P38", 0.0).
?test(sheet1_Q38, "/Bitxor/", "Q38", '#VALUE!').
?test(sheet1_R38, "/Bitxor/", "R38", '#VALUE!').
?test(sheet1_S38, "/Bitxor/", "S38", '#VALUE!').
?test(sheet1_T38, "/Bitxor/", "T38", '#VALUE!').
?test(sheet1_U38, "/Bitxor/", "U38", '#VALUE!').
?test(sheet1_V38, "/Bitxor/", "V38", '#VALUE!').
?test(sheet1_A39, "/Bitxor/", "A39", "Logical").
?test(sheet1_B39, "/Bitxor/", "B39", true).
?test(sheet1_C39, "/Bitxor/", "C39", '#DIV/0!').
?test(sheet1_D39, "/Bitxor/", "D39", '#VALUE!').
?test(sheet1_E39, "/Bitxor/", "E39", '#REF!').
?test(sheet1_F39, "/Bitxor/", "F39", '#NAME?').
?test(sheet1_G39, "/Bitxor/", "G39", '#NUM!').
?test(sheet1_H39, "/Bitxor/", "H39", '#N/A').
?test(sheet1_I39, "/Bitxor/", "I39", '#VALUE!').
?test(sheet1_J39, "/Bitxor/", "J39", 12.0).
?test(sheet1_K39, "/Bitxor/", "K39", 25.0).
?test(sheet1_L39, "/Bitxor/", "L39", 24921.0).
?test(sheet1_M39, "/Bitxor/", "M39", 2.0).
?test(sheet1_N39, "/Bitxor/", "N39", 1.0).
?test(sheet1_O39, "/Bitxor/", "O39", 0.0).
?test(sheet1_P39, "/Bitxor/", "P39", 1.0).
?test(sheet1_Q39, "/Bitxor/", "Q39", '#VALUE!').
?test(sheet1_R39, "/Bitxor/", "R39", '#VALUE!').
?test(sheet1_S39, "/Bitxor/", "S39", '#VALUE!').
?test(sheet1_T39, "/Bitxor/", "T39", '#VALUE!').
?test(sheet1_U39, "/Bitxor/", "U39", '#VALUE!').
?test(sheet1_V39, "/Bitxor/", "V39", '#VALUE!').
?test(sheet1_A40, "/Bitxor/", "A40", "Logical").
?test(sheet1_B40, "/Bitxor/", "B40", false).
?test(sheet1_C40, "/Bitxor/", "C40", '#DIV/0!').
?test(sheet1_D40, "/Bitxor/", "D40", '#VALUE!').
?test(sheet1_E40, "/Bitxor/", "E40", '#REF!').
?test(sheet1_F40, "/Bitxor/", "F40", '#NAME?').
?test(sheet1_G40, "/Bitxor/", "G40", '#NUM!').
?test(sheet1_H40, "/Bitxor/", "H40", '#N/A').
?test(sheet1_I40, "/Bitxor/", "I40", '#VALUE!').
?test(sheet1_J40, "/Bitxor/", "J40", 13.0).
?test(sheet1_K40, "/Bitxor/", "K40", 24.0).
?test(sheet1_L40, "/Bitxor/", "L40", 24920.0).
?test(sheet1_M40, "/Bitxor/", "M40", 3.0).
?test(sheet1_N40, "/Bitxor/", "N40", 0.0).
?test(sheet1_O40, "/Bitxor/", "O40", 1.0).
?test(sheet1_P40, "/Bitxor/", "P40", 0.0).
?test(sheet1_Q40, "/Bitxor/", "Q40", '#VALUE!').
?test(sheet1_R40, "/Bitxor/", "R40", '#VALUE!').
?test(sheet1_S40, "/Bitxor/", "S40", '#VALUE!').
?test(sheet1_T40, "/Bitxor/", "T40", '#VALUE!').
?test(sheet1_U40, "/Bitxor/", "U40", '#VALUE!').
?test(sheet1_V40, "/Bitxor/", "V40", '#VALUE!').
?test(sheet1_A41, "/Bitxor/", "A41", "Range Row").
?test(sheet1_B41, "/Bitxor/", "B41", "X3:Y3").
?test(sheet1_C41, "/Bitxor/", "C41", '#VALUE!').
?test(sheet1_D41, "/Bitxor/", "D41", '#VALUE!').
?test(sheet1_E41, "/Bitxor/", "E41", '#VALUE!').
?test(sheet1_F41, "/Bitxor/", "F41", '#VALUE!').
?test(sheet1_G41, "/Bitxor/", "G41", '#VALUE!').
?test(sheet1_H41, "/Bitxor/", "H41", '#VALUE!').
?test(sheet1_I41, "/Bitxor/", "I41", '#VALUE!').
?test(sheet1_J41, "/Bitxor/", "J41", '#VALUE!').
?test(sheet1_K41, "/Bitxor/", "K41", '#VALUE!').
?test(sheet1_L41, "/Bitxor/", "L41", '#VALUE!').
?test(sheet1_M41, "/Bitxor/", "M41", '#VALUE!').
?test(sheet1_N41, "/Bitxor/", "N41", '#VALUE!').
?test(sheet1_O41, "/Bitxor/", "O41", '#VALUE!').
?test(sheet1_P41, "/Bitxor/", "P41", '#VALUE!').
?test(sheet1_Q41, "/Bitxor/", "Q41", '#VALUE!').
?test(sheet1_R41, "/Bitxor/", "R41", '#VALUE!').
?test(sheet1_S41, "/Bitxor/", "S41", '#VALUE!').
?test(sheet1_T41, "/Bitxor/", "T41", '#VALUE!').
?test(sheet1_U41, "/Bitxor/", "U41", '#VALUE!').
?test(sheet1_V41, "/Bitxor/", "V41", '#VALUE!').
?test(sheet1_A42, "/Bitxor/", "A42", "Range Row").
?test(sheet1_B42, "/Bitxor/", "B42", "X3:AA3").
?test(sheet1_C42, "/Bitxor/", "C42", '#VALUE!').
?test(sheet1_D42, "/Bitxor/", "D42", '#VALUE!').
?test(sheet1_E42, "/Bitxor/", "E42", '#VALUE!').
?test(sheet1_F42, "/Bitxor/", "F42", '#VALUE!').
?test(sheet1_G42, "/Bitxor/", "G42", '#VALUE!').
?test(sheet1_H42, "/Bitxor/", "H42", '#VALUE!').
?test(sheet1_I42, "/Bitxor/", "I42", '#VALUE!').
?test(sheet1_J42, "/Bitxor/", "J42", '#VALUE!').
?test(sheet1_K42, "/Bitxor/", "K42", '#VALUE!').
?test(sheet1_L42, "/Bitxor/", "L42", '#VALUE!').
?test(sheet1_M42, "/Bitxor/", "M42", '#VALUE!').
?test(sheet1_N42, "/Bitxor/", "N42", '#VALUE!').
?test(sheet1_O42, "/Bitxor/", "O42", '#VALUE!').
?test(sheet1_P42, "/Bitxor/", "P42", '#VALUE!').
?test(sheet1_Q42, "/Bitxor/", "Q42", '#VALUE!').
?test(sheet1_R42, "/Bitxor/", "R42", '#VALUE!').
?test(sheet1_S42, "/Bitxor/", "S42", '#VALUE!').
?test(sheet1_T42, "/Bitxor/", "T42", '#VALUE!').
?test(sheet1_U42, "/Bitxor/", "U42", '#VALUE!').
?test(sheet1_V42, "/Bitxor/", "V42", '#VALUE!').
?test(sheet1_A43, "/Bitxor/", "A43", "Range Area").
?test(sheet1_B43, "/Bitxor/", "B43", "X3:Y4").
?test(sheet1_C43, "/Bitxor/", "C43", '#VALUE!').
?test(sheet1_D43, "/Bitxor/", "D43", '#VALUE!').
?test(sheet1_E43, "/Bitxor/", "E43", '#VALUE!').
?test(sheet1_F43, "/Bitxor/", "F43", '#VALUE!').
?test(sheet1_G43, "/Bitxor/", "G43", '#VALUE!').
?test(sheet1_H43, "/Bitxor/", "H43", '#VALUE!').
?test(sheet1_I43, "/Bitxor/", "I43", '#VALUE!').
?test(sheet1_J43, "/Bitxor/", "J43", '#VALUE!').
?test(sheet1_K43, "/Bitxor/", "K43", '#VALUE!').
?test(sheet1_L43, "/Bitxor/", "L43", '#VALUE!').
?test(sheet1_M43, "/Bitxor/", "M43", '#VALUE!').
?test(sheet1_N43, "/Bitxor/", "N43", '#VALUE!').
?test(sheet1_O43, "/Bitxor/", "O43", '#VALUE!').
?test(sheet1_P43, "/Bitxor/", "P43", '#VALUE!').
?test(sheet1_Q43, "/Bitxor/", "Q43", '#VALUE!').
?test(sheet1_R43, "/Bitxor/", "R43", '#VALUE!').
?test(sheet1_S43, "/Bitxor/", "S43", '#VALUE!').
?test(sheet1_T43, "/Bitxor/", "T43", '#VALUE!').
?test(sheet1_U43, "/Bitxor/", "U43", '#VALUE!').
?test(sheet1_V43, "/Bitxor/", "V43", '#VALUE!').
?test(sheet1_A44, "/Bitxor/", "A44", "Range Area").
?test(sheet1_B44, "/Bitxor/", "B44", "X3:AA6").
?test(sheet1_C44, "/Bitxor/", "C44", '#VALUE!').
?test(sheet1_D44, "/Bitxor/", "D44", '#VALUE!').
?test(sheet1_E44, "/Bitxor/", "E44", '#VALUE!').
?test(sheet1_F44, "/Bitxor/", "F44", '#VALUE!').
?test(sheet1_G44, "/Bitxor/", "G44", '#VALUE!').
?test(sheet1_H44, "/Bitxor/", "H44", '#VALUE!').
?test(sheet1_I44, "/Bitxor/", "I44", '#VALUE!').
?test(sheet1_J44, "/Bitxor/", "J44", '#VALUE!').
?test(sheet1_K44, "/Bitxor/", "K44", '#VALUE!').
?test(sheet1_L44, "/Bitxor/", "L44", '#VALUE!').
?test(sheet1_M44, "/Bitxor/", "M44", '#VALUE!').
?test(sheet1_N44, "/Bitxor/", "N44", '#VALUE!').
?test(sheet1_O44, "/Bitxor/", "O44", '#VALUE!').
?test(sheet1_P44, "/Bitxor/", "P44", '#VALUE!').
?test(sheet1_Q44, "/Bitxor/", "Q44", '#VALUE!').
?test(sheet1_R44, "/Bitxor/", "R44", '#VALUE!').
?test(sheet1_S44, "/Bitxor/", "S44", '#VALUE!').
?test(sheet1_T44, "/Bitxor/", "T44", '#VALUE!').
?test(sheet1_U44, "/Bitxor/", "U44", '#VALUE!').
?test(sheet1_V44, "/Bitxor/", "V44", '#VALUE!').
?test(sheet1_A45, "/Bitxor/", "A45", "Range Colunm").
?test(sheet1_B45, "/Bitxor/", "B45", "X3:X4").
?test(sheet1_C45, "/Bitxor/", "C45", '#VALUE!').
?test(sheet1_D45, "/Bitxor/", "D45", '#VALUE!').
?test(sheet1_E45, "/Bitxor/", "E45", '#VALUE!').
?test(sheet1_F45, "/Bitxor/", "F45", '#VALUE!').
?test(sheet1_G45, "/Bitxor/", "G45", '#VALUE!').
?test(sheet1_H45, "/Bitxor/", "H45", '#VALUE!').
?test(sheet1_I45, "/Bitxor/", "I45", '#VALUE!').
?test(sheet1_J45, "/Bitxor/", "J45", '#VALUE!').
?test(sheet1_K45, "/Bitxor/", "K45", '#VALUE!').
?test(sheet1_L45, "/Bitxor/", "L45", '#VALUE!').
?test(sheet1_M45, "/Bitxor/", "M45", '#VALUE!').
?test(sheet1_N45, "/Bitxor/", "N45", '#VALUE!').
?test(sheet1_O45, "/Bitxor/", "O45", '#VALUE!').
?test(sheet1_P45, "/Bitxor/", "P45", '#VALUE!').
?test(sheet1_Q45, "/Bitxor/", "Q45", '#VALUE!').
?test(sheet1_R45, "/Bitxor/", "R45", '#VALUE!').
?test(sheet1_S45, "/Bitxor/", "S45", '#VALUE!').
?test(sheet1_T45, "/Bitxor/", "T45", '#VALUE!').
?test(sheet1_U45, "/Bitxor/", "U45", '#VALUE!').
?test(sheet1_V45, "/Bitxor/", "V45", '#VALUE!').
?test(sheet1_A46, "/Bitxor/", "A46", "Range Colunm").
?test(sheet1_B46, "/Bitxor/", "B46", "X3:X6").
?test(sheet1_C46, "/Bitxor/", "C46", '#VALUE!').
?test(sheet1_D46, "/Bitxor/", "D46", '#VALUE!').
?test(sheet1_E46, "/Bitxor/", "E46", '#VALUE!').
?test(sheet1_F46, "/Bitxor/", "F46", '#VALUE!').
?test(sheet1_G46, "/Bitxor/", "G46", '#VALUE!').
?test(sheet1_H46, "/Bitxor/", "H46", '#VALUE!').
?test(sheet1_I46, "/Bitxor/", "I46", '#VALUE!').
?test(sheet1_J46, "/Bitxor/", "J46", '#VALUE!').
?test(sheet1_K46, "/Bitxor/", "K46", '#VALUE!').
?test(sheet1_L46, "/Bitxor/", "L46", '#VALUE!').
?test(sheet1_M46, "/Bitxor/", "M46", '#VALUE!').
?test(sheet1_N46, "/Bitxor/", "N46", '#VALUE!').
?test(sheet1_O46, "/Bitxor/", "O46", '#VALUE!').
?test(sheet1_P46, "/Bitxor/", "P46", '#VALUE!').
?test(sheet1_Q46, "/Bitxor/", "Q46", '#VALUE!').
?test(sheet1_R46, "/Bitxor/", "R46", '#VALUE!').
?test(sheet1_S46, "/Bitxor/", "S46", '#VALUE!').
?test(sheet1_T46, "/Bitxor/", "T46", '#VALUE!').
?test(sheet1_U46, "/Bitxor/", "U46", '#VALUE!').
?test(sheet1_V46, "/Bitxor/", "V46", '#VALUE!').
?test(sheet1_A49, "/Bitxor/", "A49", 320.0).
?test(sheet1_C49, "/Bitxor/", "C49", 0.0).
?test(sheet1_D49, "/Bitxor/", "D49", 0.0).
?test(sheet1_E49, "/Bitxor/", "E49", 0.0).
?test(sheet1_F49, "/Bitxor/", "F49", 0.0).
?test(sheet1_G49, "/Bitxor/", "G49", 0.0).
?test(sheet1_H49, "/Bitxor/", "H49", 0.0).
?test(sheet1_I49, "/Bitxor/", "I49", 0.0).
?test(sheet1_J49, "/Bitxor/", "J49", 0.0).
?test(sheet1_K49, "/Bitxor/", "K49", 0.0).
?test(sheet1_L49, "/Bitxor/", "L49", 0.0).
?test(sheet1_M49, "/Bitxor/", "M49", 0.0).
?test(sheet1_N49, "/Bitxor/", "N49", 0.0).
?test(sheet1_O49, "/Bitxor/", "O49", 0.0).
?test(sheet1_P49, "/Bitxor/", "P49", 0.0).
?test(sheet1_Q49, "/Bitxor/", "Q49", 0.0).
?test(sheet1_R49, "/Bitxor/", "R49", 0.0).
?test(sheet1_S49, "/Bitxor/", "S49", 0.0).
?test(sheet1_T49, "/Bitxor/", "T49", 0.0).
?test(sheet1_U49, "/Bitxor/", "U49", 0.0).
?test(sheet1_V49, "/Bitxor/", "V49", 0.0).
?test(sheet1_A50, "/Bitxor/", "A50", 7.0).
?test(sheet1_C50, "/Bitxor/", "C50", 0.0).
?test(sheet1_D50, "/Bitxor/", "D50", 0.0).
?test(sheet1_E50, "/Bitxor/", "E50", 0.0).
?test(sheet1_F50, "/Bitxor/", "F50", 0.0).
?test(sheet1_G50, "/Bitxor/", "G50", 0.0).
?test(sheet1_H50, "/Bitxor/", "H50", 0.0).
?test(sheet1_I50, "/Bitxor/", "I50", 0.0).
?test(sheet1_J50, "/Bitxor/", "J50", 0.0).
?test(sheet1_K50, "/Bitxor/", "K50", 0.0).
?test(sheet1_L50, "/Bitxor/", "L50", 0.0).
?test(sheet1_M50, "/Bitxor/", "M50", 0.0).
?test(sheet1_N50, "/Bitxor/", "N50", 0.0).
?test(sheet1_O50, "/Bitxor/", "O50", 0.0).
?test(sheet1_P50, "/Bitxor/", "P50", 0.0).
?test(sheet1_Q50, "/Bitxor/", "Q50", 0.0).
?test(sheet1_R50, "/Bitxor/", "R50", 0.0).
?test(sheet1_S50, "/Bitxor/", "S50", 0.0).
?test(sheet1_T50, "/Bitxor/", "T50", 0.0).
?test(sheet1_U50, "/Bitxor/", "U50", 0.0).
?test(sheet1_V50, "/Bitxor/", "V50", 0.0).
?test(sheet1_C51, "/Bitxor/", "C51", 0.0).
?test(sheet1_D51, "/Bitxor/", "D51", 0.0).
?test(sheet1_E51, "/Bitxor/", "E51", 0.0).
?test(sheet1_F51, "/Bitxor/", "F51", 0.0).
?test(sheet1_G51, "/Bitxor/", "G51", 0.0).
?test(sheet1_H51, "/Bitxor/", "H51", 0.0).
?test(sheet1_I51, "/Bitxor/", "I51", 0.0).
?test(sheet1_J51, "/Bitxor/", "J51", 0.0).
?test(sheet1_K51, "/Bitxor/", "K51", 0.0).
?test(sheet1_L51, "/Bitxor/", "L51", 0.0).
?test(sheet1_M51, "/Bitxor/", "M51", 0.0).
?test(sheet1_N51, "/Bitxor/", "N51", 0.0).
?test(sheet1_O51, "/Bitxor/", "O51", 0.0).
?test(sheet1_P51, "/Bitxor/", "P51", 0.0).
?test(sheet1_Q51, "/Bitxor/", "Q51", 0.0).
?test(sheet1_R51, "/Bitxor/", "R51", 0.0).
?test(sheet1_S51, "/Bitxor/", "S51", 0.0).
?test(sheet1_T51, "/Bitxor/", "T51", 0.0).
?test(sheet1_U51, "/Bitxor/", "U51", 0.0).
?test(sheet1_V51, "/Bitxor/", "V51", 0.0).
?test(sheet1_C52, "/Bitxor/", "C52", 0.0).
?test(sheet1_D52, "/Bitxor/", "D52", 0.0).
?test(sheet1_E52, "/Bitxor/", "E52", 0.0).
?test(sheet1_F52, "/Bitxor/", "F52", 1.0).
?test(sheet1_G52, "/Bitxor/", "G52", 0.0).
?test(sheet1_H52, "/Bitxor/", "H52", 0.0).
?test(sheet1_I52, "/Bitxor/", "I52", 0.0).
?test(sheet1_J52, "/Bitxor/", "J52", 0.0).
?test(sheet1_K52, "/Bitxor/", "K52", 0.0).
?test(sheet1_L52, "/Bitxor/", "L52", 0.0).
?test(sheet1_M52, "/Bitxor/", "M52", 0.0).
?test(sheet1_N52, "/Bitxor/", "N52", 0.0).
?test(sheet1_O52, "/Bitxor/", "O52", 0.0).
?test(sheet1_P52, "/Bitxor/", "P52", 0.0).
?test(sheet1_Q52, "/Bitxor/", "Q52", 0.0).
?test(sheet1_R52, "/Bitxor/", "R52", 0.0).
?test(sheet1_S52, "/Bitxor/", "S52", 0.0).
?test(sheet1_T52, "/Bitxor/", "T52", 0.0).
?test(sheet1_U52, "/Bitxor/", "U52", 0.0).
?test(sheet1_V52, "/Bitxor/", "V52", 0.0).
?test(sheet1_C53, "/Bitxor/", "C53", 0.0).
?test(sheet1_D53, "/Bitxor/", "D53", 0.0).
?test(sheet1_E53, "/Bitxor/", "E53", 0.0).
?test(sheet1_F53, "/Bitxor/", "F53", 1.0).
?test(sheet1_G53, "/Bitxor/", "G53", 0.0).
?test(sheet1_H53, "/Bitxor/", "H53", 0.0).
?test(sheet1_I53, "/Bitxor/", "I53", 0.0).
?test(sheet1_J53, "/Bitxor/", "J53", 0.0).
?test(sheet1_K53, "/Bitxor/", "K53", 0.0).
?test(sheet1_L53, "/Bitxor/", "L53", 0.0).
?test(sheet1_M53, "/Bitxor/", "M53", 0.0).
?test(sheet1_N53, "/Bitxor/", "N53", 0.0).
?test(sheet1_O53, "/Bitxor/", "O53", 0.0).
?test(sheet1_P53, "/Bitxor/", "P53", 0.0).
?test(sheet1_Q53, "/Bitxor/", "Q53", 0.0).
?test(sheet1_R53, "/Bitxor/", "R53", 0.0).
?test(sheet1_S53, "/Bitxor/", "S53", 0.0).
?test(sheet1_T53, "/Bitxor/", "T53", 0.0).
?test(sheet1_U53, "/Bitxor/", "U53", 0.0).
?test(sheet1_V53, "/Bitxor/", "V53", 0.0).
?test(sheet1_C54, "/Bitxor/", "C54", 0.0).
?test(sheet1_D54, "/Bitxor/", "D54", 0.0).
?test(sheet1_E54, "/Bitxor/", "E54", 0.0).
?test(sheet1_F54, "/Bitxor/", "F54", 1.0).
?test(sheet1_G54, "/Bitxor/", "G54", 0.0).
?test(sheet1_H54, "/Bitxor/", "H54", 0.0).
?test(sheet1_I54, "/Bitxor/", "I54", 0.0).
?test(sheet1_J54, "/Bitxor/", "J54", 0.0).
?test(sheet1_K54, "/Bitxor/", "K54", 0.0).
?test(sheet1_L54, "/Bitxor/", "L54", 0.0).
?test(sheet1_M54, "/Bitxor/", "M54", 0.0).
?test(sheet1_N54, "/Bitxor/", "N54", 0.0).
?test(sheet1_O54, "/Bitxor/", "O54", 0.0).
?test(sheet1_P54, "/Bitxor/", "P54", 0.0).
?test(sheet1_Q54, "/Bitxor/", "Q54", 0.0).
?test(sheet1_R54, "/Bitxor/", "R54", 0.0).
?test(sheet1_S54, "/Bitxor/", "S54", 0.0).
?test(sheet1_T54, "/Bitxor/", "T54", 0.0).
?test(sheet1_U54, "/Bitxor/", "U54", 0.0).
?test(sheet1_V54, "/Bitxor/", "V54", 0.0).
?test(sheet1_C55, "/Bitxor/", "C55", 0.0).
?test(sheet1_D55, "/Bitxor/", "D55", 0.0).
?test(sheet1_E55, "/Bitxor/", "E55", 0.0).
?test(sheet1_F55, "/Bitxor/", "F55", 1.0).
?test(sheet1_G55, "/Bitxor/", "G55", 0.0).
?test(sheet1_H55, "/Bitxor/", "H55", 0.0).
?test(sheet1_I55, "/Bitxor/", "I55", 0.0).
?test(sheet1_J55, "/Bitxor/", "J55", 0.0).
?test(sheet1_K55, "/Bitxor/", "K55", 0.0).
?test(sheet1_L55, "/Bitxor/", "L55", 0.0).
?test(sheet1_M55, "/Bitxor/", "M55", 0.0).
?test(sheet1_N55, "/Bitxor/", "N55", 0.0).
?test(sheet1_O55, "/Bitxor/", "O55", 0.0).
?test(sheet1_P55, "/Bitxor/", "P55", 0.0).
?test(sheet1_Q55, "/Bitxor/", "Q55", 0.0).
?test(sheet1_R55, "/Bitxor/", "R55", 0.0).
?test(sheet1_S55, "/Bitxor/", "S55", 0.0).
?test(sheet1_T55, "/Bitxor/", "T55", 0.0).
?test(sheet1_U55, "/Bitxor/", "U55", 0.0).
?test(sheet1_V55, "/Bitxor/", "V55", 0.0).
?test(sheet1_C56, "/Bitxor/", "C56", 0.0).
?test(sheet1_D56, "/Bitxor/", "D56", 0.0).
?test(sheet1_E56, "/Bitxor/", "E56", 0.0).
?test(sheet1_F56, "/Bitxor/", "F56", 1.0).
?test(sheet1_G56, "/Bitxor/", "G56", 0.0).
?test(sheet1_H56, "/Bitxor/", "H56", 0.0).
?test(sheet1_I56, "/Bitxor/", "I56", 0.0).
?test(sheet1_J56, "/Bitxor/", "J56", 0.0).
?test(sheet1_K56, "/Bitxor/", "K56", 0.0).
?test(sheet1_L56, "/Bitxor/", "L56", 0.0).
?test(sheet1_M56, "/Bitxor/", "M56", 0.0).
?test(sheet1_N56, "/Bitxor/", "N56", 0.0).
?test(sheet1_O56, "/Bitxor/", "O56", 0.0).
?test(sheet1_P56, "/Bitxor/", "P56", 0.0).
?test(sheet1_Q56, "/Bitxor/", "Q56", 0.0).
?test(sheet1_R56, "/Bitxor/", "R56", 0.0).
?test(sheet1_S56, "/Bitxor/", "S56", 0.0).
?test(sheet1_T56, "/Bitxor/", "T56", 0.0).
?test(sheet1_U56, "/Bitxor/", "U56", 0.0).
?test(sheet1_V56, "/Bitxor/", "V56", 0.0).
?test(sheet1_C57, "/Bitxor/", "C57", 0.0).
?test(sheet1_D57, "/Bitxor/", "D57", 0.0).
?test(sheet1_E57, "/Bitxor/", "E57", 0.0).
?test(sheet1_F57, "/Bitxor/", "F57", 1.0).
?test(sheet1_G57, "/Bitxor/", "G57", 0.0).
?test(sheet1_H57, "/Bitxor/", "H57", 0.0).
?test(sheet1_I57, "/Bitxor/", "I57", 0.0).
?test(sheet1_J57, "/Bitxor/", "J57", 0.0).
?test(sheet1_K57, "/Bitxor/", "K57", 0.0).
?test(sheet1_L57, "/Bitxor/", "L57", 0.0).
?test(sheet1_M57, "/Bitxor/", "M57", 0.0).
?test(sheet1_N57, "/Bitxor/", "N57", 0.0).
?test(sheet1_O57, "/Bitxor/", "O57", 0.0).
?test(sheet1_P57, "/Bitxor/", "P57", 0.0).
?test(sheet1_Q57, "/Bitxor/", "Q57", 0.0).
?test(sheet1_R57, "/Bitxor/", "R57", 0.0).
?test(sheet1_S57, "/Bitxor/", "S57", 0.0).
?test(sheet1_T57, "/Bitxor/", "T57", 0.0).
?test(sheet1_U57, "/Bitxor/", "U57", 0.0).
?test(sheet1_V57, "/Bitxor/", "V57", 0.0).
?test(sheet1_C58, "/Bitxor/", "C58", 0.0).
?test(sheet1_D58, "/Bitxor/", "D58", 0.0).
?test(sheet1_E58, "/Bitxor/", "E58", 0.0).
?test(sheet1_F58, "/Bitxor/", "F58", 1.0).
?test(sheet1_G58, "/Bitxor/", "G58", 0.0).
?test(sheet1_H58, "/Bitxor/", "H58", 0.0).
?test(sheet1_I58, "/Bitxor/", "I58", 0.0).
?test(sheet1_J58, "/Bitxor/", "J58", 0.0).
?test(sheet1_K58, "/Bitxor/", "K58", 0.0).
?test(sheet1_L58, "/Bitxor/", "L58", 0.0).
?test(sheet1_M58, "/Bitxor/", "M58", 0.0).
?test(sheet1_N58, "/Bitxor/", "N58", 0.0).
?test(sheet1_O58, "/Bitxor/", "O58", 0.0).
?test(sheet1_P58, "/Bitxor/", "P58", 0.0).
?test(sheet1_Q58, "/Bitxor/", "Q58", 0.0).
?test(sheet1_R58, "/Bitxor/", "R58", 0.0).
?test(sheet1_S58, "/Bitxor/", "S58", 0.0).
?test(sheet1_T58, "/Bitxor/", "T58", 0.0).
?test(sheet1_U58, "/Bitxor/", "U58", 0.0).
?test(sheet1_V58, "/Bitxor/", "V58", 0.0).
?test(sheet1_C59, "/Bitxor/", "C59", 0.0).
?test(sheet1_D59, "/Bitxor/", "D59", 0.0).
?test(sheet1_E59, "/Bitxor/", "E59", 0.0).
?test(sheet1_F59, "/Bitxor/", "F59", 0.0).
?test(sheet1_G59, "/Bitxor/", "G59", 0.0).
?test(sheet1_H59, "/Bitxor/", "H59", 0.0).
?test(sheet1_I59, "/Bitxor/", "I59", 0.0).
?test(sheet1_J59, "/Bitxor/", "J59", 0.0).
?test(sheet1_K59, "/Bitxor/", "K59", 0.0).
?test(sheet1_L59, "/Bitxor/", "L59", 0.0).
?test(sheet1_M59, "/Bitxor/", "M59", 0.0).
?test(sheet1_N59, "/Bitxor/", "N59", 0.0).
?test(sheet1_O59, "/Bitxor/", "O59", 0.0).
?test(sheet1_P59, "/Bitxor/", "P59", 0.0).
?test(sheet1_Q59, "/Bitxor/", "Q59", 0.0).
?test(sheet1_R59, "/Bitxor/", "R59", 0.0).
?test(sheet1_S59, "/Bitxor/", "S59", 0.0).
?test(sheet1_T59, "/Bitxor/", "T59", 0.0).
?test(sheet1_U59, "/Bitxor/", "U59", 0.0).
?test(sheet1_V59, "/Bitxor/", "V59", 0.0).
?test(sheet1_C60, "/Bitxor/", "C60", 0.0).
?test(sheet1_D60, "/Bitxor/", "D60", 0.0).
?test(sheet1_E60, "/Bitxor/", "E60", 0.0).
?test(sheet1_F60, "/Bitxor/", "F60", 0.0).
?test(sheet1_G60, "/Bitxor/", "G60", 0.0).
?test(sheet1_H60, "/Bitxor/", "H60", 0.0).
?test(sheet1_I60, "/Bitxor/", "I60", 0.0).
?test(sheet1_J60, "/Bitxor/", "J60", 0.0).
?test(sheet1_K60, "/Bitxor/", "K60", 0.0).
?test(sheet1_L60, "/Bitxor/", "L60", 0.0).
?test(sheet1_M60, "/Bitxor/", "M60", 0.0).
?test(sheet1_N60, "/Bitxor/", "N60", 0.0).
?test(sheet1_O60, "/Bitxor/", "O60", 0.0).
?test(sheet1_P60, "/Bitxor/", "P60", 0.0).
?test(sheet1_Q60, "/Bitxor/", "Q60", 0.0).
?test(sheet1_R60, "/Bitxor/", "R60", 0.0).
?test(sheet1_S60, "/Bitxor/", "S60", 0.0).
?test(sheet1_T60, "/Bitxor/", "T60", 0.0).
?test(sheet1_U60, "/Bitxor/", "U60", 0.0).
?test(sheet1_V60, "/Bitxor/", "V60", 0.0).
?test(sheet1_C61, "/Bitxor/", "C61", 0.0).
?test(sheet1_D61, "/Bitxor/", "D61", 0.0).
?test(sheet1_E61, "/Bitxor/", "E61", 0.0).
?test(sheet1_F61, "/Bitxor/", "F61", 0.0).
?test(sheet1_G61, "/Bitxor/", "G61", 0.0).
?test(sheet1_H61, "/Bitxor/", "H61", 0.0).
?test(sheet1_I61, "/Bitxor/", "I61", 0.0).
?test(sheet1_J61, "/Bitxor/", "J61", 0.0).
?test(sheet1_K61, "/Bitxor/", "K61", 0.0).
?test(sheet1_L61, "/Bitxor/", "L61", 0.0).
?test(sheet1_M61, "/Bitxor/", "M61", 0.0).
?test(sheet1_N61, "/Bitxor/", "N61", 0.0).
?test(sheet1_O61, "/Bitxor/", "O61", 0.0).
?test(sheet1_P61, "/Bitxor/", "P61", 0.0).
?test(sheet1_Q61, "/Bitxor/", "Q61", 0.0).
?test(sheet1_R61, "/Bitxor/", "R61", 0.0).
?test(sheet1_S61, "/Bitxor/", "S61", 0.0).
?test(sheet1_T61, "/Bitxor/", "T61", 0.0).
?test(sheet1_U61, "/Bitxor/", "U61", 0.0).
?test(sheet1_V61, "/Bitxor/", "V61", 0.0).
?test(sheet1_C62, "/Bitxor/", "C62", 0.0).
?test(sheet1_D62, "/Bitxor/", "D62", 0.0).
?test(sheet1_E62, "/Bitxor/", "E62", 0.0).
?test(sheet1_F62, "/Bitxor/", "F62", 0.0).
?test(sheet1_G62, "/Bitxor/", "G62", 0.0).
?test(sheet1_H62, "/Bitxor/", "H62", 0.0).
?test(sheet1_I62, "/Bitxor/", "I62", 0.0).
?test(sheet1_J62, "/Bitxor/", "J62", 0.0).
?test(sheet1_K62, "/Bitxor/", "K62", 0.0).
?test(sheet1_L62, "/Bitxor/", "L62", 0.0).
?test(sheet1_M62, "/Bitxor/", "M62", 0.0).
?test(sheet1_N62, "/Bitxor/", "N62", 0.0).
?test(sheet1_O62, "/Bitxor/", "O62", 0.0).
?test(sheet1_P62, "/Bitxor/", "P62", 0.0).
?test(sheet1_Q62, "/Bitxor/", "Q62", 0.0).
?test(sheet1_R62, "/Bitxor/", "R62", 0.0).
?test(sheet1_S62, "/Bitxor/", "S62", 0.0).
?test(sheet1_T62, "/Bitxor/", "T62", 0.0).
?test(sheet1_U62, "/Bitxor/", "U62", 0.0).
?test(sheet1_V62, "/Bitxor/", "V62", 0.0).
?test(sheet1_C63, "/Bitxor/", "C63", 0.0).
?test(sheet1_D63, "/Bitxor/", "D63", 0.0).
?test(sheet1_E63, "/Bitxor/", "E63", 0.0).
?test(sheet1_F63, "/Bitxor/", "F63", 0.0).
?test(sheet1_G63, "/Bitxor/", "G63", 0.0).
?test(sheet1_H63, "/Bitxor/", "H63", 0.0).
?test(sheet1_I63, "/Bitxor/", "I63", 0.0).
?test(sheet1_J63, "/Bitxor/", "J63", 0.0).
?test(sheet1_K63, "/Bitxor/", "K63", 0.0).
?test(sheet1_L63, "/Bitxor/", "L63", 0.0).
?test(sheet1_M63, "/Bitxor/", "M63", 0.0).
?test(sheet1_N63, "/Bitxor/", "N63", 0.0).
?test(sheet1_O63, "/Bitxor/", "O63", 0.0).
?test(sheet1_P63, "/Bitxor/", "P63", 0.0).
?test(sheet1_Q63, "/Bitxor/", "Q63", 0.0).
?test(sheet1_R63, "/Bitxor/", "R63", 0.0).
?test(sheet1_S63, "/Bitxor/", "S63", 0.0).
?test(sheet1_T63, "/Bitxor/", "T63", 0.0).
?test(sheet1_U63, "/Bitxor/", "U63", 0.0).
?test(sheet1_V63, "/Bitxor/", "V63", 0.0).
?test(sheet1_C64, "/Bitxor/", "C64", 0.0).
?test(sheet1_D64, "/Bitxor/", "D64", 0.0).
?test(sheet1_E64, "/Bitxor/", "E64", 0.0).
?test(sheet1_F64, "/Bitxor/", "F64", 0.0).
?test(sheet1_G64, "/Bitxor/", "G64", 0.0).
?test(sheet1_H64, "/Bitxor/", "H64", 0.0).
?test(sheet1_I64, "/Bitxor/", "I64", 0.0).
?test(sheet1_J64, "/Bitxor/", "J64", 0.0).
?test(sheet1_K64, "/Bitxor/", "K64", 0.0).
?test(sheet1_L64, "/Bitxor/", "L64", 0.0).
?test(sheet1_M64, "/Bitxor/", "M64", 0.0).
?test(sheet1_N64, "/Bitxor/", "N64", 0.0).
?test(sheet1_O64, "/Bitxor/", "O64", 0.0).
?test(sheet1_P64, "/Bitxor/", "P64", 0.0).
?test(sheet1_Q64, "/Bitxor/", "Q64", 0.0).
?test(sheet1_R64, "/Bitxor/", "R64", 0.0).
?test(sheet1_S64, "/Bitxor/", "S64", 0.0).
?test(sheet1_T64, "/Bitxor/", "T64", 0.0).
?test(sheet1_U64, "/Bitxor/", "U64", 0.0).
?test(sheet1_V64, "/Bitxor/", "V64", 0.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_bitwise_bitxor.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_bitwise_bitxor" ++ "/" ++ Sheetname ++ "/",
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
