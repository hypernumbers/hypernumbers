%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_bitwise_bitand_SUITE).
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
                     [Testcase, "e_gnumeric_bitwise_bitand_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_bitwise_bitand" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Bitand/", "A1", "bitand(A,B)").
?test(sheet1_B1, "/Bitand/", "B1", "B").
?test(sheet1_C1, "/Bitand/", "C1", "errors").
?test(sheet1_D1, "/Bitand/", "D1", "errors").
?test(sheet1_E1, "/Bitand/", "E1", "errors").
?test(sheet1_F1, "/Bitand/", "F1", "errors").
?test(sheet1_G1, "/Bitand/", "G1", "errors").
?test(sheet1_H1, "/Bitand/", "H1", "errors").
?test(sheet1_I1, "/Bitand/", "I1", "String").
?test(sheet1_J1, "/Bitand/", "J1", "String Number").
?test(sheet1_K1, "/Bitand/", "K1", "String number Leading space").
?test(sheet1_L1, "/Bitand/", "L1", "Integer").
?test(sheet1_M1, "/Bitand/", "M1", "Float").
?test(sheet1_N1, "/Bitand/", "N1", "Blank").
?test(sheet1_O1, "/Bitand/", "O1", "Logical").
?test(sheet1_P1, "/Bitand/", "P1", "Logical").
?test(sheet1_Q1, "/Bitand/", "Q1", "Range Row").
?test(sheet1_R1, "/Bitand/", "R1", "Range Row").
?test(sheet1_S1, "/Bitand/", "S1", "Range Area").
?test(sheet1_T1, "/Bitand/", "T1", "Range Area").
?test(sheet1_U1, "/Bitand/", "U1", "Range Colunm").
?test(sheet1_V1, "/Bitand/", "V1", "Range Colunm").
?test(sheet1_A2, "/Bitand/", "A2", "A").
?test(sheet1_C2, "/Bitand/", "C2", '#DIV/0!').
?test(sheet1_D2, "/Bitand/", "D2", '#VALUE!').
?test(sheet1_E2, "/Bitand/", "E2", '#REF!').
?test(sheet1_F2, "/Bitand/", "F2", '#NAME?').
?test(sheet1_G2, "/Bitand/", "G2", '#NUM!').
?test(sheet1_H2, "/Bitand/", "H2", '#N/A').
?test(sheet1_I2, "/Bitand/", "I2", "Phillip").
?test(sheet1_J2, "/Bitand/", "J2", "13").
?test(sheet1_K2, "/Bitand/", "K2", " 24").
?test(sheet1_L2, "/Bitand/", "L2", "1968/03/23 00:00:00").
?test(sheet1_M2, "/Bitand/", "M2", 3.14159265358979).
?test(sheet1_O2, "/Bitand/", "O2", true).
?test(sheet1_P2, "/Bitand/", "P2", false).
?test(sheet1_Q2, "/Bitand/", "Q2", "X3:Y3").
?test(sheet1_R2, "/Bitand/", "R2", "X3:AA3").
?test(sheet1_S2, "/Bitand/", "S2", "X3:Y4").
?test(sheet1_T2, "/Bitand/", "T2", "X3:AA6").
?test(sheet1_U2, "/Bitand/", "U2", "X3:X4").
?test(sheet1_V2, "/Bitand/", "V2", "X3:X6").
?test(sheet1_A3, "/Bitand/", "A3", "errors").
?test(sheet1_B3, "/Bitand/", "B3", '#DIV/0!').
?test(sheet1_C3, "/Bitand/", "C3", '#NAME?').
?test(sheet1_D3, "/Bitand/", "D3", '#NAME?').
?test(sheet1_E3, "/Bitand/", "E3", '#NAME?').
?test(sheet1_F3, "/Bitand/", "F3", '#NAME?').
?test(sheet1_G3, "/Bitand/", "G3", '#NAME?').
?test(sheet1_H3, "/Bitand/", "H3", '#NAME?').
?test(sheet1_I3, "/Bitand/", "I3", '#NAME?').
?test(sheet1_J3, "/Bitand/", "J3", '#NAME?').
?test(sheet1_K3, "/Bitand/", "K3", '#NAME?').
?test(sheet1_L3, "/Bitand/", "L3", '#NAME?').
?test(sheet1_M3, "/Bitand/", "M3", '#NAME?').
?test(sheet1_N3, "/Bitand/", "N3", '#NAME?').
?test(sheet1_O3, "/Bitand/", "O3", '#NAME?').
?test(sheet1_P3, "/Bitand/", "P3", '#NAME?').
?test(sheet1_Q3, "/Bitand/", "Q3", '#NAME?').
?test(sheet1_R3, "/Bitand/", "R3", '#NAME?').
?test(sheet1_S3, "/Bitand/", "S3", '#NAME?').
?test(sheet1_T3, "/Bitand/", "T3", '#NAME?').
?test(sheet1_U3, "/Bitand/", "U3", '#NAME?').
?test(sheet1_V3, "/Bitand/", "V3", '#NAME?').
?test(sheet1_X3, "/Bitand/", "X3", 7.0).
?test(sheet1_Y3, "/Bitand/", "Y3", 5.0).
?test(sheet1_Z3, "/Bitand/", "Z3", 3.0).
?test(sheet1_AA3, "/Bitand/", "AA3", 1.0).
?test(sheet1_A4, "/Bitand/", "A4", "errors").
?test(sheet1_B4, "/Bitand/", "B4", '#VALUE!').
?test(sheet1_C4, "/Bitand/", "C4", '#NAME?').
?test(sheet1_D4, "/Bitand/", "D4", '#NAME?').
?test(sheet1_E4, "/Bitand/", "E4", '#NAME?').
?test(sheet1_F4, "/Bitand/", "F4", '#NAME?').
?test(sheet1_G4, "/Bitand/", "G4", '#NAME?').
?test(sheet1_H4, "/Bitand/", "H4", '#NAME?').
?test(sheet1_I4, "/Bitand/", "I4", '#NAME?').
?test(sheet1_J4, "/Bitand/", "J4", '#NAME?').
?test(sheet1_K4, "/Bitand/", "K4", '#NAME?').
?test(sheet1_L4, "/Bitand/", "L4", '#NAME?').
?test(sheet1_M4, "/Bitand/", "M4", '#NAME?').
?test(sheet1_N4, "/Bitand/", "N4", '#NAME?').
?test(sheet1_O4, "/Bitand/", "O4", '#NAME?').
?test(sheet1_P4, "/Bitand/", "P4", '#NAME?').
?test(sheet1_Q4, "/Bitand/", "Q4", '#NAME?').
?test(sheet1_R4, "/Bitand/", "R4", '#NAME?').
?test(sheet1_S4, "/Bitand/", "S4", '#NAME?').
?test(sheet1_T4, "/Bitand/", "T4", '#NAME?').
?test(sheet1_U4, "/Bitand/", "U4", '#NAME?').
?test(sheet1_V4, "/Bitand/", "V4", '#NAME?').
?test(sheet1_X4, "/Bitand/", "X4", 8.0).
?test(sheet1_Y4, "/Bitand/", "Y4", 9.0).
?test(sheet1_Z4, "/Bitand/", "Z4", 10.0).
?test(sheet1_AA4, "/Bitand/", "AA4", 11.0).
?test(sheet1_A5, "/Bitand/", "A5", "errors").
?test(sheet1_B5, "/Bitand/", "B5", '#REF!').
?test(sheet1_C5, "/Bitand/", "C5", '#NAME?').
?test(sheet1_D5, "/Bitand/", "D5", '#NAME?').
?test(sheet1_E5, "/Bitand/", "E5", '#NAME?').
?test(sheet1_F5, "/Bitand/", "F5", '#NAME?').
?test(sheet1_G5, "/Bitand/", "G5", '#NAME?').
?test(sheet1_H5, "/Bitand/", "H5", '#NAME?').
?test(sheet1_I5, "/Bitand/", "I5", '#NAME?').
?test(sheet1_J5, "/Bitand/", "J5", '#NAME?').
?test(sheet1_K5, "/Bitand/", "K5", '#NAME?').
?test(sheet1_L5, "/Bitand/", "L5", '#NAME?').
?test(sheet1_M5, "/Bitand/", "M5", '#NAME?').
?test(sheet1_N5, "/Bitand/", "N5", '#NAME?').
?test(sheet1_O5, "/Bitand/", "O5", '#NAME?').
?test(sheet1_P5, "/Bitand/", "P5", '#NAME?').
?test(sheet1_Q5, "/Bitand/", "Q5", '#NAME?').
?test(sheet1_R5, "/Bitand/", "R5", '#NAME?').
?test(sheet1_S5, "/Bitand/", "S5", '#NAME?').
?test(sheet1_T5, "/Bitand/", "T5", '#NAME?').
?test(sheet1_U5, "/Bitand/", "U5", '#NAME?').
?test(sheet1_V5, "/Bitand/", "V5", '#NAME?').
?test(sheet1_X5, "/Bitand/", "X5", 9.0).
?test(sheet1_Y5, "/Bitand/", "Y5", 13.0).
?test(sheet1_Z5, "/Bitand/", "Z5", 17.0).
?test(sheet1_AA5, "/Bitand/", "AA5", 21.0).
?test(sheet1_A6, "/Bitand/", "A6", "errors").
?test(sheet1_B6, "/Bitand/", "B6", '#NAME?').
?test(sheet1_C6, "/Bitand/", "C6", '#NAME?').
?test(sheet1_D6, "/Bitand/", "D6", '#NAME?').
?test(sheet1_E6, "/Bitand/", "E6", '#NAME?').
?test(sheet1_F6, "/Bitand/", "F6", '#NAME?').
?test(sheet1_G6, "/Bitand/", "G6", '#NAME?').
?test(sheet1_H6, "/Bitand/", "H6", '#NAME?').
?test(sheet1_I6, "/Bitand/", "I6", '#NAME?').
?test(sheet1_J6, "/Bitand/", "J6", '#NAME?').
?test(sheet1_K6, "/Bitand/", "K6", '#NAME?').
?test(sheet1_L6, "/Bitand/", "L6", '#NAME?').
?test(sheet1_M6, "/Bitand/", "M6", '#NAME?').
?test(sheet1_N6, "/Bitand/", "N6", '#NAME?').
?test(sheet1_O6, "/Bitand/", "O6", '#NAME?').
?test(sheet1_P6, "/Bitand/", "P6", '#NAME?').
?test(sheet1_Q6, "/Bitand/", "Q6", '#NAME?').
?test(sheet1_R6, "/Bitand/", "R6", '#NAME?').
?test(sheet1_S6, "/Bitand/", "S6", '#NAME?').
?test(sheet1_T6, "/Bitand/", "T6", '#NAME?').
?test(sheet1_U6, "/Bitand/", "U6", '#NAME?').
?test(sheet1_V6, "/Bitand/", "V6", '#NAME?').
?test(sheet1_X6, "/Bitand/", "X6", 10.0).
?test(sheet1_Y6, "/Bitand/", "Y6", 17.0).
?test(sheet1_Z6, "/Bitand/", "Z6", 24.0).
?test(sheet1_AA6, "/Bitand/", "AA6", 31.0).
?test(sheet1_A7, "/Bitand/", "A7", "errors").
?test(sheet1_B7, "/Bitand/", "B7", '#NUM!').
?test(sheet1_C7, "/Bitand/", "C7", '#NAME?').
?test(sheet1_D7, "/Bitand/", "D7", '#NAME?').
?test(sheet1_E7, "/Bitand/", "E7", '#NAME?').
?test(sheet1_F7, "/Bitand/", "F7", '#NAME?').
?test(sheet1_G7, "/Bitand/", "G7", '#NAME?').
?test(sheet1_H7, "/Bitand/", "H7", '#NAME?').
?test(sheet1_I7, "/Bitand/", "I7", '#NAME?').
?test(sheet1_J7, "/Bitand/", "J7", '#NAME?').
?test(sheet1_K7, "/Bitand/", "K7", '#NAME?').
?test(sheet1_L7, "/Bitand/", "L7", '#NAME?').
?test(sheet1_M7, "/Bitand/", "M7", '#NAME?').
?test(sheet1_N7, "/Bitand/", "N7", '#NAME?').
?test(sheet1_O7, "/Bitand/", "O7", '#NAME?').
?test(sheet1_P7, "/Bitand/", "P7", '#NAME?').
?test(sheet1_Q7, "/Bitand/", "Q7", '#NAME?').
?test(sheet1_R7, "/Bitand/", "R7", '#NAME?').
?test(sheet1_S7, "/Bitand/", "S7", '#NAME?').
?test(sheet1_T7, "/Bitand/", "T7", '#NAME?').
?test(sheet1_U7, "/Bitand/", "U7", '#NAME?').
?test(sheet1_V7, "/Bitand/", "V7", '#NAME?').
?test(sheet1_A8, "/Bitand/", "A8", "errors").
?test(sheet1_B8, "/Bitand/", "B8", '#N/A').
?test(sheet1_C8, "/Bitand/", "C8", '#NAME?').
?test(sheet1_D8, "/Bitand/", "D8", '#NAME?').
?test(sheet1_E8, "/Bitand/", "E8", '#NAME?').
?test(sheet1_F8, "/Bitand/", "F8", '#NAME?').
?test(sheet1_G8, "/Bitand/", "G8", '#NAME?').
?test(sheet1_H8, "/Bitand/", "H8", '#NAME?').
?test(sheet1_I8, "/Bitand/", "I8", '#NAME?').
?test(sheet1_J8, "/Bitand/", "J8", '#NAME?').
?test(sheet1_K8, "/Bitand/", "K8", '#NAME?').
?test(sheet1_L8, "/Bitand/", "L8", '#NAME?').
?test(sheet1_M8, "/Bitand/", "M8", '#NAME?').
?test(sheet1_N8, "/Bitand/", "N8", '#NAME?').
?test(sheet1_O8, "/Bitand/", "O8", '#NAME?').
?test(sheet1_P8, "/Bitand/", "P8", '#NAME?').
?test(sheet1_Q8, "/Bitand/", "Q8", '#NAME?').
?test(sheet1_R8, "/Bitand/", "R8", '#NAME?').
?test(sheet1_S8, "/Bitand/", "S8", '#NAME?').
?test(sheet1_T8, "/Bitand/", "T8", '#NAME?').
?test(sheet1_U8, "/Bitand/", "U8", '#NAME?').
?test(sheet1_V8, "/Bitand/", "V8", '#NAME?').
?test(sheet1_A9, "/Bitand/", "A9", "String").
?test(sheet1_B9, "/Bitand/", "B9", "Phillip").
?test(sheet1_C9, "/Bitand/", "C9", '#NAME?').
?test(sheet1_D9, "/Bitand/", "D9", '#NAME?').
?test(sheet1_E9, "/Bitand/", "E9", '#NAME?').
?test(sheet1_F9, "/Bitand/", "F9", '#NAME?').
?test(sheet1_G9, "/Bitand/", "G9", '#NAME?').
?test(sheet1_H9, "/Bitand/", "H9", '#NAME?').
?test(sheet1_I9, "/Bitand/", "I9", '#NAME?').
?test(sheet1_J9, "/Bitand/", "J9", '#NAME?').
?test(sheet1_K9, "/Bitand/", "K9", '#NAME?').
?test(sheet1_L9, "/Bitand/", "L9", '#NAME?').
?test(sheet1_M9, "/Bitand/", "M9", '#NAME?').
?test(sheet1_N9, "/Bitand/", "N9", '#NAME?').
?test(sheet1_O9, "/Bitand/", "O9", '#NAME?').
?test(sheet1_P9, "/Bitand/", "P9", '#NAME?').
?test(sheet1_Q9, "/Bitand/", "Q9", '#NAME?').
?test(sheet1_R9, "/Bitand/", "R9", '#NAME?').
?test(sheet1_S9, "/Bitand/", "S9", '#NAME?').
?test(sheet1_T9, "/Bitand/", "T9", '#NAME?').
?test(sheet1_U9, "/Bitand/", "U9", '#NAME?').
?test(sheet1_V9, "/Bitand/", "V9", '#NAME?').
?test(sheet1_A10, "/Bitand/", "A10", "String Number").
?test(sheet1_B10, "/Bitand/", "B10", "12").
?test(sheet1_C10, "/Bitand/", "C10", '#NAME?').
?test(sheet1_D10, "/Bitand/", "D10", '#NAME?').
?test(sheet1_E10, "/Bitand/", "E10", '#NAME?').
?test(sheet1_F10, "/Bitand/", "F10", '#NAME?').
?test(sheet1_G10, "/Bitand/", "G10", '#NAME?').
?test(sheet1_H10, "/Bitand/", "H10", '#NAME?').
?test(sheet1_I10, "/Bitand/", "I10", '#NAME?').
?test(sheet1_J10, "/Bitand/", "J10", '#NAME?').
?test(sheet1_K10, "/Bitand/", "K10", '#NAME?').
?test(sheet1_L10, "/Bitand/", "L10", '#NAME?').
?test(sheet1_M10, "/Bitand/", "M10", '#NAME?').
?test(sheet1_N10, "/Bitand/", "N10", '#NAME?').
?test(sheet1_O10, "/Bitand/", "O10", '#NAME?').
?test(sheet1_P10, "/Bitand/", "P10", '#NAME?').
?test(sheet1_Q10, "/Bitand/", "Q10", '#NAME?').
?test(sheet1_R10, "/Bitand/", "R10", '#NAME?').
?test(sheet1_S10, "/Bitand/", "S10", '#NAME?').
?test(sheet1_T10, "/Bitand/", "T10", '#NAME?').
?test(sheet1_U10, "/Bitand/", "U10", '#NAME?').
?test(sheet1_V10, "/Bitand/", "V10", '#NAME?').
?test(sheet1_A11, "/Bitand/", "A11", "String Number Leading space").
?test(sheet1_B11, "/Bitand/", "B11", " 23").
?test(sheet1_C11, "/Bitand/", "C11", '#NAME?').
?test(sheet1_D11, "/Bitand/", "D11", '#NAME?').
?test(sheet1_E11, "/Bitand/", "E11", '#NAME?').
?test(sheet1_F11, "/Bitand/", "F11", '#NAME?').
?test(sheet1_G11, "/Bitand/", "G11", '#NAME?').
?test(sheet1_H11, "/Bitand/", "H11", '#NAME?').
?test(sheet1_I11, "/Bitand/", "I11", '#NAME?').
?test(sheet1_J11, "/Bitand/", "J11", '#NAME?').
?test(sheet1_K11, "/Bitand/", "K11", '#NAME?').
?test(sheet1_L11, "/Bitand/", "L11", '#NAME?').
?test(sheet1_M11, "/Bitand/", "M11", '#NAME?').
?test(sheet1_N11, "/Bitand/", "N11", '#NAME?').
?test(sheet1_O11, "/Bitand/", "O11", '#NAME?').
?test(sheet1_P11, "/Bitand/", "P11", '#NAME?').
?test(sheet1_Q11, "/Bitand/", "Q11", '#NAME?').
?test(sheet1_R11, "/Bitand/", "R11", '#NAME?').
?test(sheet1_S11, "/Bitand/", "S11", '#NAME?').
?test(sheet1_T11, "/Bitand/", "T11", '#NAME?').
?test(sheet1_U11, "/Bitand/", "U11", '#NAME?').
?test(sheet1_V11, "/Bitand/", "V11", '#NAME?').
?test(sheet1_A12, "/Bitand/", "A12", "Interger").
?test(sheet1_B12, "/Bitand/", "B12", "1968/03/23 00:00:00").
?test(sheet1_C12, "/Bitand/", "C12", '#NAME?').
?test(sheet1_D12, "/Bitand/", "D12", '#NAME?').
?test(sheet1_E12, "/Bitand/", "E12", '#NAME?').
?test(sheet1_F12, "/Bitand/", "F12", '#NAME?').
?test(sheet1_G12, "/Bitand/", "G12", '#NAME?').
?test(sheet1_H12, "/Bitand/", "H12", '#NAME?').
?test(sheet1_I12, "/Bitand/", "I12", '#NAME?').
?test(sheet1_J12, "/Bitand/", "J12", '#NAME?').
?test(sheet1_K12, "/Bitand/", "K12", '#NAME?').
?test(sheet1_L12, "/Bitand/", "L12", '#NAME?').
?test(sheet1_M12, "/Bitand/", "M12", '#NAME?').
?test(sheet1_N12, "/Bitand/", "N12", '#NAME?').
?test(sheet1_O12, "/Bitand/", "O12", '#NAME?').
?test(sheet1_P12, "/Bitand/", "P12", '#NAME?').
?test(sheet1_Q12, "/Bitand/", "Q12", '#NAME?').
?test(sheet1_R12, "/Bitand/", "R12", '#NAME?').
?test(sheet1_S12, "/Bitand/", "S12", '#NAME?').
?test(sheet1_T12, "/Bitand/", "T12", '#NAME?').
?test(sheet1_U12, "/Bitand/", "U12", '#NAME?').
?test(sheet1_V12, "/Bitand/", "V12", '#NAME?').
?test(sheet1_A13, "/Bitand/", "A13", "Float").
?test(sheet1_B13, "/Bitand/", "B13", 3.14159265358979).
?test(sheet1_C13, "/Bitand/", "C13", '#NAME?').
?test(sheet1_D13, "/Bitand/", "D13", '#NAME?').
?test(sheet1_E13, "/Bitand/", "E13", '#NAME?').
?test(sheet1_F13, "/Bitand/", "F13", '#NAME?').
?test(sheet1_G13, "/Bitand/", "G13", '#NAME?').
?test(sheet1_H13, "/Bitand/", "H13", '#NAME?').
?test(sheet1_I13, "/Bitand/", "I13", '#NAME?').
?test(sheet1_J13, "/Bitand/", "J13", '#NAME?').
?test(sheet1_K13, "/Bitand/", "K13", '#NAME?').
?test(sheet1_L13, "/Bitand/", "L13", '#NAME?').
?test(sheet1_M13, "/Bitand/", "M13", '#NAME?').
?test(sheet1_N13, "/Bitand/", "N13", '#NAME?').
?test(sheet1_O13, "/Bitand/", "O13", '#NAME?').
?test(sheet1_P13, "/Bitand/", "P13", '#NAME?').
?test(sheet1_Q13, "/Bitand/", "Q13", '#NAME?').
?test(sheet1_R13, "/Bitand/", "R13", '#NAME?').
?test(sheet1_S13, "/Bitand/", "S13", '#NAME?').
?test(sheet1_T13, "/Bitand/", "T13", '#NAME?').
?test(sheet1_U13, "/Bitand/", "U13", '#NAME?').
?test(sheet1_V13, "/Bitand/", "V13", '#NAME?').
?test(sheet1_A14, "/Bitand/", "A14", "Blank").
?test(sheet1_C14, "/Bitand/", "C14", '#NAME?').
?test(sheet1_D14, "/Bitand/", "D14", '#NAME?').
?test(sheet1_E14, "/Bitand/", "E14", '#NAME?').
?test(sheet1_F14, "/Bitand/", "F14", '#NAME?').
?test(sheet1_G14, "/Bitand/", "G14", '#NAME?').
?test(sheet1_H14, "/Bitand/", "H14", '#NAME?').
?test(sheet1_I14, "/Bitand/", "I14", '#NAME?').
?test(sheet1_J14, "/Bitand/", "J14", '#NAME?').
?test(sheet1_K14, "/Bitand/", "K14", '#NAME?').
?test(sheet1_L14, "/Bitand/", "L14", '#NAME?').
?test(sheet1_M14, "/Bitand/", "M14", '#NAME?').
?test(sheet1_N14, "/Bitand/", "N14", '#NAME?').
?test(sheet1_O14, "/Bitand/", "O14", '#NAME?').
?test(sheet1_P14, "/Bitand/", "P14", '#NAME?').
?test(sheet1_Q14, "/Bitand/", "Q14", '#NAME?').
?test(sheet1_R14, "/Bitand/", "R14", '#NAME?').
?test(sheet1_S14, "/Bitand/", "S14", '#NAME?').
?test(sheet1_T14, "/Bitand/", "T14", '#NAME?').
?test(sheet1_U14, "/Bitand/", "U14", '#NAME?').
?test(sheet1_V14, "/Bitand/", "V14", '#NAME?').
?test(sheet1_A15, "/Bitand/", "A15", "Logical").
?test(sheet1_B15, "/Bitand/", "B15", true).
?test(sheet1_C15, "/Bitand/", "C15", '#NAME?').
?test(sheet1_D15, "/Bitand/", "D15", '#NAME?').
?test(sheet1_E15, "/Bitand/", "E15", '#NAME?').
?test(sheet1_F15, "/Bitand/", "F15", '#NAME?').
?test(sheet1_G15, "/Bitand/", "G15", '#NAME?').
?test(sheet1_H15, "/Bitand/", "H15", '#NAME?').
?test(sheet1_I15, "/Bitand/", "I15", '#NAME?').
?test(sheet1_J15, "/Bitand/", "J15", '#NAME?').
?test(sheet1_K15, "/Bitand/", "K15", '#NAME?').
?test(sheet1_L15, "/Bitand/", "L15", '#NAME?').
?test(sheet1_M15, "/Bitand/", "M15", '#NAME?').
?test(sheet1_N15, "/Bitand/", "N15", '#NAME?').
?test(sheet1_O15, "/Bitand/", "O15", '#NAME?').
?test(sheet1_P15, "/Bitand/", "P15", '#NAME?').
?test(sheet1_Q15, "/Bitand/", "Q15", '#NAME?').
?test(sheet1_R15, "/Bitand/", "R15", '#NAME?').
?test(sheet1_S15, "/Bitand/", "S15", '#NAME?').
?test(sheet1_T15, "/Bitand/", "T15", '#NAME?').
?test(sheet1_U15, "/Bitand/", "U15", '#NAME?').
?test(sheet1_V15, "/Bitand/", "V15", '#NAME?').
?test(sheet1_A16, "/Bitand/", "A16", "Logical").
?test(sheet1_B16, "/Bitand/", "B16", false).
?test(sheet1_C16, "/Bitand/", "C16", '#NAME?').
?test(sheet1_D16, "/Bitand/", "D16", '#NAME?').
?test(sheet1_E16, "/Bitand/", "E16", '#NAME?').
?test(sheet1_F16, "/Bitand/", "F16", '#NAME?').
?test(sheet1_G16, "/Bitand/", "G16", '#NAME?').
?test(sheet1_H16, "/Bitand/", "H16", '#NAME?').
?test(sheet1_I16, "/Bitand/", "I16", '#NAME?').
?test(sheet1_J16, "/Bitand/", "J16", '#NAME?').
?test(sheet1_K16, "/Bitand/", "K16", '#NAME?').
?test(sheet1_L16, "/Bitand/", "L16", '#NAME?').
?test(sheet1_M16, "/Bitand/", "M16", '#NAME?').
?test(sheet1_N16, "/Bitand/", "N16", '#NAME?').
?test(sheet1_O16, "/Bitand/", "O16", '#NAME?').
?test(sheet1_P16, "/Bitand/", "P16", '#NAME?').
?test(sheet1_Q16, "/Bitand/", "Q16", '#NAME?').
?test(sheet1_R16, "/Bitand/", "R16", '#NAME?').
?test(sheet1_S16, "/Bitand/", "S16", '#NAME?').
?test(sheet1_T16, "/Bitand/", "T16", '#NAME?').
?test(sheet1_U16, "/Bitand/", "U16", '#NAME?').
?test(sheet1_V16, "/Bitand/", "V16", '#NAME?').
?test(sheet1_A17, "/Bitand/", "A17", "Range Row").
?test(sheet1_B17, "/Bitand/", "B17", "X3:Y3").
?test(sheet1_C17, "/Bitand/", "C17", '#NAME?').
?test(sheet1_D17, "/Bitand/", "D17", '#NAME?').
?test(sheet1_E17, "/Bitand/", "E17", '#NAME?').
?test(sheet1_F17, "/Bitand/", "F17", '#NAME?').
?test(sheet1_G17, "/Bitand/", "G17", '#NAME?').
?test(sheet1_H17, "/Bitand/", "H17", '#NAME?').
?test(sheet1_I17, "/Bitand/", "I17", '#NAME?').
?test(sheet1_J17, "/Bitand/", "J17", '#NAME?').
?test(sheet1_K17, "/Bitand/", "K17", '#NAME?').
?test(sheet1_L17, "/Bitand/", "L17", '#NAME?').
?test(sheet1_M17, "/Bitand/", "M17", '#NAME?').
?test(sheet1_N17, "/Bitand/", "N17", '#NAME?').
?test(sheet1_O17, "/Bitand/", "O17", '#NAME?').
?test(sheet1_P17, "/Bitand/", "P17", '#NAME?').
?test(sheet1_Q17, "/Bitand/", "Q17", '#NAME?').
?test(sheet1_R17, "/Bitand/", "R17", '#NAME?').
?test(sheet1_S17, "/Bitand/", "S17", '#NAME?').
?test(sheet1_T17, "/Bitand/", "T17", '#NAME?').
?test(sheet1_U17, "/Bitand/", "U17", '#NAME?').
?test(sheet1_V17, "/Bitand/", "V17", '#NAME?').
?test(sheet1_A18, "/Bitand/", "A18", "Range Row").
?test(sheet1_B18, "/Bitand/", "B18", "X3:AA3").
?test(sheet1_C18, "/Bitand/", "C18", '#NAME?').
?test(sheet1_D18, "/Bitand/", "D18", '#NAME?').
?test(sheet1_E18, "/Bitand/", "E18", '#NAME?').
?test(sheet1_F18, "/Bitand/", "F18", '#NAME?').
?test(sheet1_G18, "/Bitand/", "G18", '#NAME?').
?test(sheet1_H18, "/Bitand/", "H18", '#NAME?').
?test(sheet1_I18, "/Bitand/", "I18", '#NAME?').
?test(sheet1_J18, "/Bitand/", "J18", '#NAME?').
?test(sheet1_K18, "/Bitand/", "K18", '#NAME?').
?test(sheet1_L18, "/Bitand/", "L18", '#NAME?').
?test(sheet1_M18, "/Bitand/", "M18", '#NAME?').
?test(sheet1_N18, "/Bitand/", "N18", '#NAME?').
?test(sheet1_O18, "/Bitand/", "O18", '#NAME?').
?test(sheet1_P18, "/Bitand/", "P18", '#NAME?').
?test(sheet1_Q18, "/Bitand/", "Q18", '#NAME?').
?test(sheet1_R18, "/Bitand/", "R18", '#NAME?').
?test(sheet1_S18, "/Bitand/", "S18", '#NAME?').
?test(sheet1_T18, "/Bitand/", "T18", '#NAME?').
?test(sheet1_U18, "/Bitand/", "U18", '#NAME?').
?test(sheet1_V18, "/Bitand/", "V18", '#NAME?').
?test(sheet1_A19, "/Bitand/", "A19", "Range Area").
?test(sheet1_B19, "/Bitand/", "B19", "X3:Y4").
?test(sheet1_C19, "/Bitand/", "C19", '#NAME?').
?test(sheet1_D19, "/Bitand/", "D19", '#NAME?').
?test(sheet1_E19, "/Bitand/", "E19", '#NAME?').
?test(sheet1_F19, "/Bitand/", "F19", '#NAME?').
?test(sheet1_G19, "/Bitand/", "G19", '#NAME?').
?test(sheet1_H19, "/Bitand/", "H19", '#NAME?').
?test(sheet1_I19, "/Bitand/", "I19", '#NAME?').
?test(sheet1_J19, "/Bitand/", "J19", '#NAME?').
?test(sheet1_K19, "/Bitand/", "K19", '#NAME?').
?test(sheet1_L19, "/Bitand/", "L19", '#NAME?').
?test(sheet1_M19, "/Bitand/", "M19", '#NAME?').
?test(sheet1_N19, "/Bitand/", "N19", '#NAME?').
?test(sheet1_O19, "/Bitand/", "O19", '#NAME?').
?test(sheet1_P19, "/Bitand/", "P19", '#NAME?').
?test(sheet1_Q19, "/Bitand/", "Q19", '#NAME?').
?test(sheet1_R19, "/Bitand/", "R19", '#NAME?').
?test(sheet1_S19, "/Bitand/", "S19", '#NAME?').
?test(sheet1_T19, "/Bitand/", "T19", '#NAME?').
?test(sheet1_U19, "/Bitand/", "U19", '#NAME?').
?test(sheet1_V19, "/Bitand/", "V19", '#NAME?').
?test(sheet1_A20, "/Bitand/", "A20", "Range Area").
?test(sheet1_B20, "/Bitand/", "B20", "X3:AA6").
?test(sheet1_C20, "/Bitand/", "C20", '#NAME?').
?test(sheet1_D20, "/Bitand/", "D20", '#NAME?').
?test(sheet1_E20, "/Bitand/", "E20", '#NAME?').
?test(sheet1_F20, "/Bitand/", "F20", '#NAME?').
?test(sheet1_G20, "/Bitand/", "G20", '#NAME?').
?test(sheet1_H20, "/Bitand/", "H20", '#NAME?').
?test(sheet1_I20, "/Bitand/", "I20", '#NAME?').
?test(sheet1_J20, "/Bitand/", "J20", '#NAME?').
?test(sheet1_K20, "/Bitand/", "K20", '#NAME?').
?test(sheet1_L20, "/Bitand/", "L20", '#NAME?').
?test(sheet1_M20, "/Bitand/", "M20", '#NAME?').
?test(sheet1_N20, "/Bitand/", "N20", '#NAME?').
?test(sheet1_O20, "/Bitand/", "O20", '#NAME?').
?test(sheet1_P20, "/Bitand/", "P20", '#NAME?').
?test(sheet1_Q20, "/Bitand/", "Q20", '#NAME?').
?test(sheet1_R20, "/Bitand/", "R20", '#NAME?').
?test(sheet1_S20, "/Bitand/", "S20", '#NAME?').
?test(sheet1_T20, "/Bitand/", "T20", '#NAME?').
?test(sheet1_U20, "/Bitand/", "U20", '#NAME?').
?test(sheet1_V20, "/Bitand/", "V20", '#NAME?').
?test(sheet1_A21, "/Bitand/", "A21", "Range Colunm").
?test(sheet1_B21, "/Bitand/", "B21", "X3:X4").
?test(sheet1_C21, "/Bitand/", "C21", '#NAME?').
?test(sheet1_D21, "/Bitand/", "D21", '#NAME?').
?test(sheet1_E21, "/Bitand/", "E21", '#NAME?').
?test(sheet1_F21, "/Bitand/", "F21", '#NAME?').
?test(sheet1_G21, "/Bitand/", "G21", '#NAME?').
?test(sheet1_H21, "/Bitand/", "H21", '#NAME?').
?test(sheet1_I21, "/Bitand/", "I21", '#NAME?').
?test(sheet1_J21, "/Bitand/", "J21", '#NAME?').
?test(sheet1_K21, "/Bitand/", "K21", '#NAME?').
?test(sheet1_L21, "/Bitand/", "L21", '#NAME?').
?test(sheet1_M21, "/Bitand/", "M21", '#NAME?').
?test(sheet1_N21, "/Bitand/", "N21", '#NAME?').
?test(sheet1_O21, "/Bitand/", "O21", '#NAME?').
?test(sheet1_P21, "/Bitand/", "P21", '#NAME?').
?test(sheet1_Q21, "/Bitand/", "Q21", '#NAME?').
?test(sheet1_R21, "/Bitand/", "R21", '#NAME?').
?test(sheet1_S21, "/Bitand/", "S21", '#NAME?').
?test(sheet1_T21, "/Bitand/", "T21", '#NAME?').
?test(sheet1_U21, "/Bitand/", "U21", '#NAME?').
?test(sheet1_V21, "/Bitand/", "V21", '#NAME?').
?test(sheet1_A22, "/Bitand/", "A22", "Range Colunm").
?test(sheet1_B22, "/Bitand/", "B22", "X3:X6").
?test(sheet1_C22, "/Bitand/", "C22", '#NAME?').
?test(sheet1_D22, "/Bitand/", "D22", '#NAME?').
?test(sheet1_E22, "/Bitand/", "E22", '#NAME?').
?test(sheet1_F22, "/Bitand/", "F22", '#NAME?').
?test(sheet1_G22, "/Bitand/", "G22", '#NAME?').
?test(sheet1_H22, "/Bitand/", "H22", '#NAME?').
?test(sheet1_I22, "/Bitand/", "I22", '#NAME?').
?test(sheet1_J22, "/Bitand/", "J22", '#NAME?').
?test(sheet1_K22, "/Bitand/", "K22", '#NAME?').
?test(sheet1_L22, "/Bitand/", "L22", '#NAME?').
?test(sheet1_M22, "/Bitand/", "M22", '#NAME?').
?test(sheet1_N22, "/Bitand/", "N22", '#NAME?').
?test(sheet1_O22, "/Bitand/", "O22", '#NAME?').
?test(sheet1_P22, "/Bitand/", "P22", '#NAME?').
?test(sheet1_Q22, "/Bitand/", "Q22", '#NAME?').
?test(sheet1_R22, "/Bitand/", "R22", '#NAME?').
?test(sheet1_S22, "/Bitand/", "S22", '#NAME?').
?test(sheet1_T22, "/Bitand/", "T22", '#NAME?').
?test(sheet1_U22, "/Bitand/", "U22", '#NAME?').
?test(sheet1_V22, "/Bitand/", "V22", '#NAME?').
?test(sheet1_A25, "/Bitand/", "A25", "bitand(A,B)").
?test(sheet1_B25, "/Bitand/", "B25", "B").
?test(sheet1_C25, "/Bitand/", "C25", "errors").
?test(sheet1_D25, "/Bitand/", "D25", "errors").
?test(sheet1_E25, "/Bitand/", "E25", "errors").
?test(sheet1_F25, "/Bitand/", "F25", "errors").
?test(sheet1_G25, "/Bitand/", "G25", "errors").
?test(sheet1_H25, "/Bitand/", "H25", "errors").
?test(sheet1_I25, "/Bitand/", "I25", "String").
?test(sheet1_J25, "/Bitand/", "J25", "String Number").
?test(sheet1_K25, "/Bitand/", "K25", "String number Leading space").
?test(sheet1_L25, "/Bitand/", "L25", "Integer").
?test(sheet1_M25, "/Bitand/", "M25", "Float").
?test(sheet1_N25, "/Bitand/", "N25", "Blank").
?test(sheet1_O25, "/Bitand/", "O25", "Logical").
?test(sheet1_P25, "/Bitand/", "P25", "Logical").
?test(sheet1_Q25, "/Bitand/", "Q25", "Range Row").
?test(sheet1_R25, "/Bitand/", "R25", "Range Row").
?test(sheet1_S25, "/Bitand/", "S25", "Range Area").
?test(sheet1_T25, "/Bitand/", "T25", "Range Area").
?test(sheet1_U25, "/Bitand/", "U25", "Range Colunm").
?test(sheet1_V25, "/Bitand/", "V25", "Range Colunm").
?test(sheet1_A26, "/Bitand/", "A26", "A").
?test(sheet1_C26, "/Bitand/", "C26", '#DIV/0!').
?test(sheet1_D26, "/Bitand/", "D26", '#VALUE!').
?test(sheet1_E26, "/Bitand/", "E26", '#REF!').
?test(sheet1_F26, "/Bitand/", "F26", '#NAME?').
?test(sheet1_G26, "/Bitand/", "G26", '#NUM!').
?test(sheet1_H26, "/Bitand/", "H26", '#N/A').
?test(sheet1_I26, "/Bitand/", "I26", "Phillip").
?test(sheet1_J26, "/Bitand/", "J26", "13").
?test(sheet1_K26, "/Bitand/", "K26", " 24").
?test(sheet1_L26, "/Bitand/", "L26", "1968/03/23 00:00:00").
?test(sheet1_M26, "/Bitand/", "M26", 3.14159265358979).
?test(sheet1_O26, "/Bitand/", "O26", true).
?test(sheet1_P26, "/Bitand/", "P26", false).
?test(sheet1_Q26, "/Bitand/", "Q26", "X3:Y3").
?test(sheet1_R26, "/Bitand/", "R26", "X3:AA3").
?test(sheet1_S26, "/Bitand/", "S26", "X3:Y4").
?test(sheet1_T26, "/Bitand/", "T26", "X3:AA6").
?test(sheet1_U26, "/Bitand/", "U26", "X3:X4").
?test(sheet1_V26, "/Bitand/", "V26", "X3:X6").
?test(sheet1_A27, "/Bitand/", "A27", "errors").
?test(sheet1_B27, "/Bitand/", "B27", '#DIV/0!').
?test(sheet1_C27, "/Bitand/", "C27", '#DIV/0!').
?test(sheet1_D27, "/Bitand/", "D27", '#DIV/0!').
?test(sheet1_E27, "/Bitand/", "E27", '#DIV/0!').
?test(sheet1_F27, "/Bitand/", "F27", '#DIV/0!').
?test(sheet1_G27, "/Bitand/", "G27", '#DIV/0!').
?test(sheet1_H27, "/Bitand/", "H27", '#DIV/0!').
?test(sheet1_I27, "/Bitand/", "I27", '#DIV/0!').
?test(sheet1_J27, "/Bitand/", "J27", '#DIV/0!').
?test(sheet1_K27, "/Bitand/", "K27", '#DIV/0!').
?test(sheet1_L27, "/Bitand/", "L27", '#DIV/0!').
?test(sheet1_M27, "/Bitand/", "M27", '#DIV/0!').
?test(sheet1_N27, "/Bitand/", "N27", '#DIV/0!').
?test(sheet1_O27, "/Bitand/", "O27", '#DIV/0!').
?test(sheet1_P27, "/Bitand/", "P27", '#DIV/0!').
?test(sheet1_Q27, "/Bitand/", "Q27", '#DIV/0!').
?test(sheet1_R27, "/Bitand/", "R27", '#DIV/0!').
?test(sheet1_S27, "/Bitand/", "S27", '#DIV/0!').
?test(sheet1_T27, "/Bitand/", "T27", '#DIV/0!').
?test(sheet1_U27, "/Bitand/", "U27", '#DIV/0!').
?test(sheet1_V27, "/Bitand/", "V27", '#DIV/0!').
?test(sheet1_A28, "/Bitand/", "A28", "errors").
?test(sheet1_B28, "/Bitand/", "B28", '#VALUE!').
?test(sheet1_C28, "/Bitand/", "C28", '#VALUE!').
?test(sheet1_D28, "/Bitand/", "D28", '#VALUE!').
?test(sheet1_E28, "/Bitand/", "E28", '#VALUE!').
?test(sheet1_F28, "/Bitand/", "F28", '#VALUE!').
?test(sheet1_G28, "/Bitand/", "G28", '#VALUE!').
?test(sheet1_H28, "/Bitand/", "H28", '#VALUE!').
?test(sheet1_I28, "/Bitand/", "I28", '#VALUE!').
?test(sheet1_J28, "/Bitand/", "J28", '#VALUE!').
?test(sheet1_K28, "/Bitand/", "K28", '#VALUE!').
?test(sheet1_L28, "/Bitand/", "L28", '#VALUE!').
?test(sheet1_M28, "/Bitand/", "M28", '#VALUE!').
?test(sheet1_N28, "/Bitand/", "N28", '#VALUE!').
?test(sheet1_O28, "/Bitand/", "O28", '#VALUE!').
?test(sheet1_P28, "/Bitand/", "P28", '#VALUE!').
?test(sheet1_Q28, "/Bitand/", "Q28", '#VALUE!').
?test(sheet1_R28, "/Bitand/", "R28", '#VALUE!').
?test(sheet1_S28, "/Bitand/", "S28", '#VALUE!').
?test(sheet1_T28, "/Bitand/", "T28", '#VALUE!').
?test(sheet1_U28, "/Bitand/", "U28", '#VALUE!').
?test(sheet1_V28, "/Bitand/", "V28", '#VALUE!').
?test(sheet1_A29, "/Bitand/", "A29", "errors").
?test(sheet1_B29, "/Bitand/", "B29", '#REF!').
?test(sheet1_C29, "/Bitand/", "C29", '#REF!').
?test(sheet1_D29, "/Bitand/", "D29", '#REF!').
?test(sheet1_E29, "/Bitand/", "E29", '#REF!').
?test(sheet1_F29, "/Bitand/", "F29", '#REF!').
?test(sheet1_G29, "/Bitand/", "G29", '#REF!').
?test(sheet1_H29, "/Bitand/", "H29", '#REF!').
?test(sheet1_I29, "/Bitand/", "I29", '#REF!').
?test(sheet1_J29, "/Bitand/", "J29", '#REF!').
?test(sheet1_K29, "/Bitand/", "K29", '#REF!').
?test(sheet1_L29, "/Bitand/", "L29", '#REF!').
?test(sheet1_M29, "/Bitand/", "M29", '#REF!').
?test(sheet1_N29, "/Bitand/", "N29", '#REF!').
?test(sheet1_O29, "/Bitand/", "O29", '#REF!').
?test(sheet1_P29, "/Bitand/", "P29", '#REF!').
?test(sheet1_Q29, "/Bitand/", "Q29", '#REF!').
?test(sheet1_R29, "/Bitand/", "R29", '#REF!').
?test(sheet1_S29, "/Bitand/", "S29", '#REF!').
?test(sheet1_T29, "/Bitand/", "T29", '#REF!').
?test(sheet1_U29, "/Bitand/", "U29", '#REF!').
?test(sheet1_V29, "/Bitand/", "V29", '#REF!').
?test(sheet1_A30, "/Bitand/", "A30", "errors").
?test(sheet1_B30, "/Bitand/", "B30", '#NAME?').
?test(sheet1_C30, "/Bitand/", "C30", '#NAME?').
?test(sheet1_D30, "/Bitand/", "D30", '#NAME?').
?test(sheet1_E30, "/Bitand/", "E30", '#NAME?').
?test(sheet1_F30, "/Bitand/", "F30", '#NAME?').
?test(sheet1_G30, "/Bitand/", "G30", '#NAME?').
?test(sheet1_H30, "/Bitand/", "H30", '#NAME?').
?test(sheet1_I30, "/Bitand/", "I30", '#NAME?').
?test(sheet1_J30, "/Bitand/", "J30", '#NAME?').
?test(sheet1_K30, "/Bitand/", "K30", '#NAME?').
?test(sheet1_L30, "/Bitand/", "L30", '#NAME?').
?test(sheet1_M30, "/Bitand/", "M30", '#NAME?').
?test(sheet1_N30, "/Bitand/", "N30", '#NAME?').
?test(sheet1_O30, "/Bitand/", "O30", '#NAME?').
?test(sheet1_P30, "/Bitand/", "P30", '#NAME?').
?test(sheet1_Q30, "/Bitand/", "Q30", '#NAME?').
?test(sheet1_R30, "/Bitand/", "R30", '#NAME?').
?test(sheet1_S30, "/Bitand/", "S30", '#NAME?').
?test(sheet1_T30, "/Bitand/", "T30", '#NAME?').
?test(sheet1_U30, "/Bitand/", "U30", '#NAME?').
?test(sheet1_V30, "/Bitand/", "V30", '#NAME?').
?test(sheet1_A31, "/Bitand/", "A31", "errors").
?test(sheet1_B31, "/Bitand/", "B31", '#NUM!').
?test(sheet1_C31, "/Bitand/", "C31", '#NUM!').
?test(sheet1_D31, "/Bitand/", "D31", '#NUM!').
?test(sheet1_E31, "/Bitand/", "E31", '#NUM!').
?test(sheet1_F31, "/Bitand/", "F31", '#NUM!').
?test(sheet1_G31, "/Bitand/", "G31", '#NUM!').
?test(sheet1_H31, "/Bitand/", "H31", '#NUM!').
?test(sheet1_I31, "/Bitand/", "I31", '#NUM!').
?test(sheet1_J31, "/Bitand/", "J31", '#NUM!').
?test(sheet1_K31, "/Bitand/", "K31", '#NUM!').
?test(sheet1_L31, "/Bitand/", "L31", '#NUM!').
?test(sheet1_M31, "/Bitand/", "M31", '#NUM!').
?test(sheet1_N31, "/Bitand/", "N31", '#NUM!').
?test(sheet1_O31, "/Bitand/", "O31", '#NUM!').
?test(sheet1_P31, "/Bitand/", "P31", '#NUM!').
?test(sheet1_Q31, "/Bitand/", "Q31", '#NUM!').
?test(sheet1_R31, "/Bitand/", "R31", '#NUM!').
?test(sheet1_S31, "/Bitand/", "S31", '#NUM!').
?test(sheet1_T31, "/Bitand/", "T31", '#NUM!').
?test(sheet1_U31, "/Bitand/", "U31", '#NUM!').
?test(sheet1_V31, "/Bitand/", "V31", '#NUM!').
?test(sheet1_A32, "/Bitand/", "A32", "errors").
?test(sheet1_B32, "/Bitand/", "B32", '#N/A').
?test(sheet1_C32, "/Bitand/", "C32", '#N/A').
?test(sheet1_D32, "/Bitand/", "D32", '#N/A').
?test(sheet1_E32, "/Bitand/", "E32", '#N/A').
?test(sheet1_F32, "/Bitand/", "F32", '#N/A').
?test(sheet1_G32, "/Bitand/", "G32", '#N/A').
?test(sheet1_H32, "/Bitand/", "H32", '#N/A').
?test(sheet1_I32, "/Bitand/", "I32", '#N/A').
?test(sheet1_J32, "/Bitand/", "J32", '#N/A').
?test(sheet1_K32, "/Bitand/", "K32", '#N/A').
?test(sheet1_L32, "/Bitand/", "L32", '#N/A').
?test(sheet1_M32, "/Bitand/", "M32", '#N/A').
?test(sheet1_N32, "/Bitand/", "N32", '#N/A').
?test(sheet1_O32, "/Bitand/", "O32", '#N/A').
?test(sheet1_P32, "/Bitand/", "P32", '#N/A').
?test(sheet1_Q32, "/Bitand/", "Q32", '#N/A').
?test(sheet1_R32, "/Bitand/", "R32", '#N/A').
?test(sheet1_S32, "/Bitand/", "S32", '#N/A').
?test(sheet1_T32, "/Bitand/", "T32", '#N/A').
?test(sheet1_U32, "/Bitand/", "U32", '#N/A').
?test(sheet1_V32, "/Bitand/", "V32", '#N/A').
?test(sheet1_A33, "/Bitand/", "A33", "String").
?test(sheet1_B33, "/Bitand/", "B33", "Phillip").
?test(sheet1_C33, "/Bitand/", "C33", '#VALUE!').
?test(sheet1_D33, "/Bitand/", "D33", '#VALUE!').
?test(sheet1_E33, "/Bitand/", "E33", '#VALUE!').
?test(sheet1_F33, "/Bitand/", "F33", '#VALUE!').
?test(sheet1_G33, "/Bitand/", "G33", '#VALUE!').
?test(sheet1_H33, "/Bitand/", "H33", '#VALUE!').
?test(sheet1_I33, "/Bitand/", "I33", '#VALUE!').
?test(sheet1_J33, "/Bitand/", "J33", '#VALUE!').
?test(sheet1_K33, "/Bitand/", "K33", '#VALUE!').
?test(sheet1_L33, "/Bitand/", "L33", '#VALUE!').
?test(sheet1_M33, "/Bitand/", "M33", '#VALUE!').
?test(sheet1_N33, "/Bitand/", "N33", '#VALUE!').
?test(sheet1_O33, "/Bitand/", "O33", '#VALUE!').
?test(sheet1_P33, "/Bitand/", "P33", '#VALUE!').
?test(sheet1_Q33, "/Bitand/", "Q33", '#VALUE!').
?test(sheet1_R33, "/Bitand/", "R33", '#VALUE!').
?test(sheet1_S33, "/Bitand/", "S33", '#VALUE!').
?test(sheet1_T33, "/Bitand/", "T33", '#VALUE!').
?test(sheet1_U33, "/Bitand/", "U33", '#VALUE!').
?test(sheet1_V33, "/Bitand/", "V33", '#VALUE!').
?test(sheet1_A34, "/Bitand/", "A34", "String Number").
?test(sheet1_B34, "/Bitand/", "B34", "12").
?test(sheet1_C34, "/Bitand/", "C34", '#DIV/0!').
?test(sheet1_D34, "/Bitand/", "D34", '#VALUE!').
?test(sheet1_E34, "/Bitand/", "E34", '#REF!').
?test(sheet1_F34, "/Bitand/", "F34", '#NAME?').
?test(sheet1_G34, "/Bitand/", "G34", '#NUM!').
?test(sheet1_H34, "/Bitand/", "H34", '#N/A').
?test(sheet1_I34, "/Bitand/", "I34", '#VALUE!').
?test(sheet1_J34, "/Bitand/", "J34", 12.0).
?test(sheet1_K34, "/Bitand/", "K34", 8.0).
?test(sheet1_L34, "/Bitand/", "L34", 8.0).
?test(sheet1_M34, "/Bitand/", "M34", 0.0).
?test(sheet1_N34, "/Bitand/", "N34", 0.0).
?test(sheet1_O34, "/Bitand/", "O34", 0.0).
?test(sheet1_P34, "/Bitand/", "P34", 0.0).
?test(sheet1_Q34, "/Bitand/", "Q34", '#VALUE!').
?test(sheet1_R34, "/Bitand/", "R34", '#VALUE!').
?test(sheet1_S34, "/Bitand/", "S34", '#VALUE!').
?test(sheet1_T34, "/Bitand/", "T34", '#VALUE!').
?test(sheet1_U34, "/Bitand/", "U34", '#VALUE!').
?test(sheet1_V34, "/Bitand/", "V34", '#VALUE!').
?test(sheet1_A35, "/Bitand/", "A35", "String Number Leading space").
?test(sheet1_B35, "/Bitand/", "B35", " 23").
?test(sheet1_C35, "/Bitand/", "C35", '#DIV/0!').
?test(sheet1_D35, "/Bitand/", "D35", '#VALUE!').
?test(sheet1_E35, "/Bitand/", "E35", '#REF!').
?test(sheet1_F35, "/Bitand/", "F35", '#NAME?').
?test(sheet1_G35, "/Bitand/", "G35", '#NUM!').
?test(sheet1_H35, "/Bitand/", "H35", '#N/A').
?test(sheet1_I35, "/Bitand/", "I35", '#VALUE!').
?test(sheet1_J35, "/Bitand/", "J35", 5.0).
?test(sheet1_K35, "/Bitand/", "K35", 16.0).
?test(sheet1_L35, "/Bitand/", "L35", 16.0).
?test(sheet1_M35, "/Bitand/", "M35", 3.0).
?test(sheet1_N35, "/Bitand/", "N35", 0.0).
?test(sheet1_O35, "/Bitand/", "O35", 1.0).
?test(sheet1_P35, "/Bitand/", "P35", 0.0).
?test(sheet1_Q35, "/Bitand/", "Q35", '#VALUE!').
?test(sheet1_R35, "/Bitand/", "R35", '#VALUE!').
?test(sheet1_S35, "/Bitand/", "S35", '#VALUE!').
?test(sheet1_T35, "/Bitand/", "T35", '#VALUE!').
?test(sheet1_U35, "/Bitand/", "U35", '#VALUE!').
?test(sheet1_V35, "/Bitand/", "V35", '#VALUE!').
?test(sheet1_A36, "/Bitand/", "A36", "Interger").
?test(sheet1_B36, "/Bitand/", "B36", "1968/03/23 00:00:00").
?test(sheet1_C36, "/Bitand/", "C36", '#DIV/0!').
?test(sheet1_D36, "/Bitand/", "D36", '#VALUE!').
?test(sheet1_E36, "/Bitand/", "E36", '#REF!').
?test(sheet1_F36, "/Bitand/", "F36", '#NAME?').
?test(sheet1_G36, "/Bitand/", "G36", '#NUM!').
?test(sheet1_H36, "/Bitand/", "H36", '#N/A').
?test(sheet1_I36, "/Bitand/", "I36", '#VALUE!').
?test(sheet1_J36, "/Bitand/", "J36", 8.0).
?test(sheet1_K36, "/Bitand/", "K36", 24.0).
?test(sheet1_L36, "/Bitand/", "L36", 24920.0).
?test(sheet1_M36, "/Bitand/", "M36", 0.0).
?test(sheet1_N36, "/Bitand/", "N36", 0.0).
?test(sheet1_O36, "/Bitand/", "O36", 0.0).
?test(sheet1_P36, "/Bitand/", "P36", 0.0).
?test(sheet1_Q36, "/Bitand/", "Q36", '#VALUE!').
?test(sheet1_R36, "/Bitand/", "R36", '#VALUE!').
?test(sheet1_S36, "/Bitand/", "S36", '#VALUE!').
?test(sheet1_T36, "/Bitand/", "T36", '#VALUE!').
?test(sheet1_U36, "/Bitand/", "U36", '#VALUE!').
?test(sheet1_V36, "/Bitand/", "V36", '#VALUE!').
?test(sheet1_A37, "/Bitand/", "A37", "Float").
?test(sheet1_B37, "/Bitand/", "B37", 3.14159265358979).
?test(sheet1_C37, "/Bitand/", "C37", '#DIV/0!').
?test(sheet1_D37, "/Bitand/", "D37", '#VALUE!').
?test(sheet1_E37, "/Bitand/", "E37", '#REF!').
?test(sheet1_F37, "/Bitand/", "F37", '#NAME?').
?test(sheet1_G37, "/Bitand/", "G37", '#NUM!').
?test(sheet1_H37, "/Bitand/", "H37", '#N/A').
?test(sheet1_I37, "/Bitand/", "I37", '#VALUE!').
?test(sheet1_J37, "/Bitand/", "J37", 1.0).
?test(sheet1_K37, "/Bitand/", "K37", 0.0).
?test(sheet1_L37, "/Bitand/", "L37", 0.0).
?test(sheet1_M37, "/Bitand/", "M37", 3.0).
?test(sheet1_N37, "/Bitand/", "N37", 0.0).
?test(sheet1_O37, "/Bitand/", "O37", 1.0).
?test(sheet1_P37, "/Bitand/", "P37", 0.0).
?test(sheet1_Q37, "/Bitand/", "Q37", '#VALUE!').
?test(sheet1_R37, "/Bitand/", "R37", '#VALUE!').
?test(sheet1_S37, "/Bitand/", "S37", '#VALUE!').
?test(sheet1_T37, "/Bitand/", "T37", '#VALUE!').
?test(sheet1_U37, "/Bitand/", "U37", '#VALUE!').
?test(sheet1_V37, "/Bitand/", "V37", '#VALUE!').
?test(sheet1_A38, "/Bitand/", "A38", "Blank").
?test(sheet1_C38, "/Bitand/", "C38", '#DIV/0!').
?test(sheet1_D38, "/Bitand/", "D38", '#VALUE!').
?test(sheet1_E38, "/Bitand/", "E38", '#REF!').
?test(sheet1_F38, "/Bitand/", "F38", '#NAME?').
?test(sheet1_G38, "/Bitand/", "G38", '#NUM!').
?test(sheet1_H38, "/Bitand/", "H38", '#N/A').
?test(sheet1_I38, "/Bitand/", "I38", '#VALUE!').
?test(sheet1_J38, "/Bitand/", "J38", 0.0).
?test(sheet1_K38, "/Bitand/", "K38", 0.0).
?test(sheet1_L38, "/Bitand/", "L38", 0.0).
?test(sheet1_M38, "/Bitand/", "M38", 0.0).
?test(sheet1_N38, "/Bitand/", "N38", 0.0).
?test(sheet1_O38, "/Bitand/", "O38", 0.0).
?test(sheet1_P38, "/Bitand/", "P38", 0.0).
?test(sheet1_Q38, "/Bitand/", "Q38", '#VALUE!').
?test(sheet1_R38, "/Bitand/", "R38", '#VALUE!').
?test(sheet1_S38, "/Bitand/", "S38", '#VALUE!').
?test(sheet1_T38, "/Bitand/", "T38", '#VALUE!').
?test(sheet1_U38, "/Bitand/", "U38", '#VALUE!').
?test(sheet1_V38, "/Bitand/", "V38", '#VALUE!').
?test(sheet1_A39, "/Bitand/", "A39", "Logical").
?test(sheet1_B39, "/Bitand/", "B39", true).
?test(sheet1_C39, "/Bitand/", "C39", '#DIV/0!').
?test(sheet1_D39, "/Bitand/", "D39", '#VALUE!').
?test(sheet1_E39, "/Bitand/", "E39", '#REF!').
?test(sheet1_F39, "/Bitand/", "F39", '#NAME?').
?test(sheet1_G39, "/Bitand/", "G39", '#NUM!').
?test(sheet1_H39, "/Bitand/", "H39", '#N/A').
?test(sheet1_I39, "/Bitand/", "I39", '#VALUE!').
?test(sheet1_J39, "/Bitand/", "J39", 1.0).
?test(sheet1_K39, "/Bitand/", "K39", 0.0).
?test(sheet1_L39, "/Bitand/", "L39", 0.0).
?test(sheet1_M39, "/Bitand/", "M39", 1.0).
?test(sheet1_N39, "/Bitand/", "N39", 0.0).
?test(sheet1_O39, "/Bitand/", "O39", 1.0).
?test(sheet1_P39, "/Bitand/", "P39", 0.0).
?test(sheet1_Q39, "/Bitand/", "Q39", '#VALUE!').
?test(sheet1_R39, "/Bitand/", "R39", '#VALUE!').
?test(sheet1_S39, "/Bitand/", "S39", '#VALUE!').
?test(sheet1_T39, "/Bitand/", "T39", '#VALUE!').
?test(sheet1_U39, "/Bitand/", "U39", '#VALUE!').
?test(sheet1_V39, "/Bitand/", "V39", '#VALUE!').
?test(sheet1_A40, "/Bitand/", "A40", "Logical").
?test(sheet1_B40, "/Bitand/", "B40", false).
?test(sheet1_C40, "/Bitand/", "C40", '#DIV/0!').
?test(sheet1_D40, "/Bitand/", "D40", '#VALUE!').
?test(sheet1_E40, "/Bitand/", "E40", '#REF!').
?test(sheet1_F40, "/Bitand/", "F40", '#NAME?').
?test(sheet1_G40, "/Bitand/", "G40", '#NUM!').
?test(sheet1_H40, "/Bitand/", "H40", '#N/A').
?test(sheet1_I40, "/Bitand/", "I40", '#VALUE!').
?test(sheet1_J40, "/Bitand/", "J40", 0.0).
?test(sheet1_K40, "/Bitand/", "K40", 0.0).
?test(sheet1_L40, "/Bitand/", "L40", 0.0).
?test(sheet1_M40, "/Bitand/", "M40", 0.0).
?test(sheet1_N40, "/Bitand/", "N40", 0.0).
?test(sheet1_O40, "/Bitand/", "O40", 0.0).
?test(sheet1_P40, "/Bitand/", "P40", 0.0).
?test(sheet1_Q40, "/Bitand/", "Q40", '#VALUE!').
?test(sheet1_R40, "/Bitand/", "R40", '#VALUE!').
?test(sheet1_S40, "/Bitand/", "S40", '#VALUE!').
?test(sheet1_T40, "/Bitand/", "T40", '#VALUE!').
?test(sheet1_U40, "/Bitand/", "U40", '#VALUE!').
?test(sheet1_V40, "/Bitand/", "V40", '#VALUE!').
?test(sheet1_A41, "/Bitand/", "A41", "Range Row").
?test(sheet1_B41, "/Bitand/", "B41", "X3:Y3").
?test(sheet1_C41, "/Bitand/", "C41", '#VALUE!').
?test(sheet1_D41, "/Bitand/", "D41", '#VALUE!').
?test(sheet1_E41, "/Bitand/", "E41", '#VALUE!').
?test(sheet1_F41, "/Bitand/", "F41", '#VALUE!').
?test(sheet1_G41, "/Bitand/", "G41", '#VALUE!').
?test(sheet1_H41, "/Bitand/", "H41", '#VALUE!').
?test(sheet1_I41, "/Bitand/", "I41", '#VALUE!').
?test(sheet1_J41, "/Bitand/", "J41", '#VALUE!').
?test(sheet1_K41, "/Bitand/", "K41", '#VALUE!').
?test(sheet1_L41, "/Bitand/", "L41", '#VALUE!').
?test(sheet1_M41, "/Bitand/", "M41", '#VALUE!').
?test(sheet1_N41, "/Bitand/", "N41", '#VALUE!').
?test(sheet1_O41, "/Bitand/", "O41", '#VALUE!').
?test(sheet1_P41, "/Bitand/", "P41", '#VALUE!').
?test(sheet1_Q41, "/Bitand/", "Q41", '#VALUE!').
?test(sheet1_R41, "/Bitand/", "R41", '#VALUE!').
?test(sheet1_S41, "/Bitand/", "S41", '#VALUE!').
?test(sheet1_T41, "/Bitand/", "T41", '#VALUE!').
?test(sheet1_U41, "/Bitand/", "U41", '#VALUE!').
?test(sheet1_V41, "/Bitand/", "V41", '#VALUE!').
?test(sheet1_A42, "/Bitand/", "A42", "Range Row").
?test(sheet1_B42, "/Bitand/", "B42", "X3:AA3").
?test(sheet1_C42, "/Bitand/", "C42", '#VALUE!').
?test(sheet1_D42, "/Bitand/", "D42", '#VALUE!').
?test(sheet1_E42, "/Bitand/", "E42", '#VALUE!').
?test(sheet1_F42, "/Bitand/", "F42", '#VALUE!').
?test(sheet1_G42, "/Bitand/", "G42", '#VALUE!').
?test(sheet1_H42, "/Bitand/", "H42", '#VALUE!').
?test(sheet1_I42, "/Bitand/", "I42", '#VALUE!').
?test(sheet1_J42, "/Bitand/", "J42", '#VALUE!').
?test(sheet1_K42, "/Bitand/", "K42", '#VALUE!').
?test(sheet1_L42, "/Bitand/", "L42", '#VALUE!').
?test(sheet1_M42, "/Bitand/", "M42", '#VALUE!').
?test(sheet1_N42, "/Bitand/", "N42", '#VALUE!').
?test(sheet1_O42, "/Bitand/", "O42", '#VALUE!').
?test(sheet1_P42, "/Bitand/", "P42", '#VALUE!').
?test(sheet1_Q42, "/Bitand/", "Q42", '#VALUE!').
?test(sheet1_R42, "/Bitand/", "R42", '#VALUE!').
?test(sheet1_S42, "/Bitand/", "S42", '#VALUE!').
?test(sheet1_T42, "/Bitand/", "T42", '#VALUE!').
?test(sheet1_U42, "/Bitand/", "U42", '#VALUE!').
?test(sheet1_V42, "/Bitand/", "V42", '#VALUE!').
?test(sheet1_A43, "/Bitand/", "A43", "Range Area").
?test(sheet1_B43, "/Bitand/", "B43", "X3:Y4").
?test(sheet1_C43, "/Bitand/", "C43", '#VALUE!').
?test(sheet1_D43, "/Bitand/", "D43", '#VALUE!').
?test(sheet1_E43, "/Bitand/", "E43", '#VALUE!').
?test(sheet1_F43, "/Bitand/", "F43", '#VALUE!').
?test(sheet1_G43, "/Bitand/", "G43", '#VALUE!').
?test(sheet1_H43, "/Bitand/", "H43", '#VALUE!').
?test(sheet1_I43, "/Bitand/", "I43", '#VALUE!').
?test(sheet1_J43, "/Bitand/", "J43", '#VALUE!').
?test(sheet1_K43, "/Bitand/", "K43", '#VALUE!').
?test(sheet1_L43, "/Bitand/", "L43", '#VALUE!').
?test(sheet1_M43, "/Bitand/", "M43", '#VALUE!').
?test(sheet1_N43, "/Bitand/", "N43", '#VALUE!').
?test(sheet1_O43, "/Bitand/", "O43", '#VALUE!').
?test(sheet1_P43, "/Bitand/", "P43", '#VALUE!').
?test(sheet1_Q43, "/Bitand/", "Q43", '#VALUE!').
?test(sheet1_R43, "/Bitand/", "R43", '#VALUE!').
?test(sheet1_S43, "/Bitand/", "S43", '#VALUE!').
?test(sheet1_T43, "/Bitand/", "T43", '#VALUE!').
?test(sheet1_U43, "/Bitand/", "U43", '#VALUE!').
?test(sheet1_V43, "/Bitand/", "V43", '#VALUE!').
?test(sheet1_A44, "/Bitand/", "A44", "Range Area").
?test(sheet1_B44, "/Bitand/", "B44", "X3:AA6").
?test(sheet1_C44, "/Bitand/", "C44", '#VALUE!').
?test(sheet1_D44, "/Bitand/", "D44", '#VALUE!').
?test(sheet1_E44, "/Bitand/", "E44", '#VALUE!').
?test(sheet1_F44, "/Bitand/", "F44", '#VALUE!').
?test(sheet1_G44, "/Bitand/", "G44", '#VALUE!').
?test(sheet1_H44, "/Bitand/", "H44", '#VALUE!').
?test(sheet1_I44, "/Bitand/", "I44", '#VALUE!').
?test(sheet1_J44, "/Bitand/", "J44", '#VALUE!').
?test(sheet1_K44, "/Bitand/", "K44", '#VALUE!').
?test(sheet1_L44, "/Bitand/", "L44", '#VALUE!').
?test(sheet1_M44, "/Bitand/", "M44", '#VALUE!').
?test(sheet1_N44, "/Bitand/", "N44", '#VALUE!').
?test(sheet1_O44, "/Bitand/", "O44", '#VALUE!').
?test(sheet1_P44, "/Bitand/", "P44", '#VALUE!').
?test(sheet1_Q44, "/Bitand/", "Q44", '#VALUE!').
?test(sheet1_R44, "/Bitand/", "R44", '#VALUE!').
?test(sheet1_S44, "/Bitand/", "S44", '#VALUE!').
?test(sheet1_T44, "/Bitand/", "T44", '#VALUE!').
?test(sheet1_U44, "/Bitand/", "U44", '#VALUE!').
?test(sheet1_V44, "/Bitand/", "V44", '#VALUE!').
?test(sheet1_A45, "/Bitand/", "A45", "Range Colunm").
?test(sheet1_B45, "/Bitand/", "B45", "X3:X4").
?test(sheet1_C45, "/Bitand/", "C45", '#VALUE!').
?test(sheet1_D45, "/Bitand/", "D45", '#VALUE!').
?test(sheet1_E45, "/Bitand/", "E45", '#VALUE!').
?test(sheet1_F45, "/Bitand/", "F45", '#VALUE!').
?test(sheet1_G45, "/Bitand/", "G45", '#VALUE!').
?test(sheet1_H45, "/Bitand/", "H45", '#VALUE!').
?test(sheet1_I45, "/Bitand/", "I45", '#VALUE!').
?test(sheet1_J45, "/Bitand/", "J45", '#VALUE!').
?test(sheet1_K45, "/Bitand/", "K45", '#VALUE!').
?test(sheet1_L45, "/Bitand/", "L45", '#VALUE!').
?test(sheet1_M45, "/Bitand/", "M45", '#VALUE!').
?test(sheet1_N45, "/Bitand/", "N45", '#VALUE!').
?test(sheet1_O45, "/Bitand/", "O45", '#VALUE!').
?test(sheet1_P45, "/Bitand/", "P45", '#VALUE!').
?test(sheet1_Q45, "/Bitand/", "Q45", '#VALUE!').
?test(sheet1_R45, "/Bitand/", "R45", '#VALUE!').
?test(sheet1_S45, "/Bitand/", "S45", '#VALUE!').
?test(sheet1_T45, "/Bitand/", "T45", '#VALUE!').
?test(sheet1_U45, "/Bitand/", "U45", '#VALUE!').
?test(sheet1_V45, "/Bitand/", "V45", '#VALUE!').
?test(sheet1_A46, "/Bitand/", "A46", "Range Colunm").
?test(sheet1_B46, "/Bitand/", "B46", "X3:X6").
?test(sheet1_C46, "/Bitand/", "C46", '#VALUE!').
?test(sheet1_D46, "/Bitand/", "D46", '#VALUE!').
?test(sheet1_E46, "/Bitand/", "E46", '#VALUE!').
?test(sheet1_F46, "/Bitand/", "F46", '#VALUE!').
?test(sheet1_G46, "/Bitand/", "G46", '#VALUE!').
?test(sheet1_H46, "/Bitand/", "H46", '#VALUE!').
?test(sheet1_I46, "/Bitand/", "I46", '#VALUE!').
?test(sheet1_J46, "/Bitand/", "J46", '#VALUE!').
?test(sheet1_K46, "/Bitand/", "K46", '#VALUE!').
?test(sheet1_L46, "/Bitand/", "L46", '#VALUE!').
?test(sheet1_M46, "/Bitand/", "M46", '#VALUE!').
?test(sheet1_N46, "/Bitand/", "N46", '#VALUE!').
?test(sheet1_O46, "/Bitand/", "O46", '#VALUE!').
?test(sheet1_P46, "/Bitand/", "P46", '#VALUE!').
?test(sheet1_Q46, "/Bitand/", "Q46", '#VALUE!').
?test(sheet1_R46, "/Bitand/", "R46", '#VALUE!').
?test(sheet1_S46, "/Bitand/", "S46", '#VALUE!').
?test(sheet1_T46, "/Bitand/", "T46", '#VALUE!').
?test(sheet1_U46, "/Bitand/", "U46", '#VALUE!').
?test(sheet1_V46, "/Bitand/", "V46", '#VALUE!').
?test(sheet1_A49, "/Bitand/", "A49", 320.0).
?test(sheet1_C49, "/Bitand/", "C49", 0.0).
?test(sheet1_D49, "/Bitand/", "D49", 0.0).
?test(sheet1_E49, "/Bitand/", "E49", 0.0).
?test(sheet1_F49, "/Bitand/", "F49", 0.0).
?test(sheet1_G49, "/Bitand/", "G49", 0.0).
?test(sheet1_H49, "/Bitand/", "H49", 0.0).
?test(sheet1_I49, "/Bitand/", "I49", 0.0).
?test(sheet1_J49, "/Bitand/", "J49", 0.0).
?test(sheet1_K49, "/Bitand/", "K49", 0.0).
?test(sheet1_L49, "/Bitand/", "L49", 0.0).
?test(sheet1_M49, "/Bitand/", "M49", 0.0).
?test(sheet1_N49, "/Bitand/", "N49", 0.0).
?test(sheet1_O49, "/Bitand/", "O49", 0.0).
?test(sheet1_P49, "/Bitand/", "P49", 0.0).
?test(sheet1_Q49, "/Bitand/", "Q49", 0.0).
?test(sheet1_R49, "/Bitand/", "R49", 0.0).
?test(sheet1_S49, "/Bitand/", "S49", 0.0).
?test(sheet1_T49, "/Bitand/", "T49", 0.0).
?test(sheet1_U49, "/Bitand/", "U49", 0.0).
?test(sheet1_V49, "/Bitand/", "V49", 0.0).
?test(sheet1_A50, "/Bitand/", "A50", 7.0).
?test(sheet1_C50, "/Bitand/", "C50", 0.0).
?test(sheet1_D50, "/Bitand/", "D50", 0.0).
?test(sheet1_E50, "/Bitand/", "E50", 0.0).
?test(sheet1_F50, "/Bitand/", "F50", 0.0).
?test(sheet1_G50, "/Bitand/", "G50", 0.0).
?test(sheet1_H50, "/Bitand/", "H50", 0.0).
?test(sheet1_I50, "/Bitand/", "I50", 0.0).
?test(sheet1_J50, "/Bitand/", "J50", 0.0).
?test(sheet1_K50, "/Bitand/", "K50", 0.0).
?test(sheet1_L50, "/Bitand/", "L50", 0.0).
?test(sheet1_M50, "/Bitand/", "M50", 0.0).
?test(sheet1_N50, "/Bitand/", "N50", 0.0).
?test(sheet1_O50, "/Bitand/", "O50", 0.0).
?test(sheet1_P50, "/Bitand/", "P50", 0.0).
?test(sheet1_Q50, "/Bitand/", "Q50", 0.0).
?test(sheet1_R50, "/Bitand/", "R50", 0.0).
?test(sheet1_S50, "/Bitand/", "S50", 0.0).
?test(sheet1_T50, "/Bitand/", "T50", 0.0).
?test(sheet1_U50, "/Bitand/", "U50", 0.0).
?test(sheet1_V50, "/Bitand/", "V50", 0.0).
?test(sheet1_C51, "/Bitand/", "C51", 0.0).
?test(sheet1_D51, "/Bitand/", "D51", 0.0).
?test(sheet1_E51, "/Bitand/", "E51", 0.0).
?test(sheet1_F51, "/Bitand/", "F51", 0.0).
?test(sheet1_G51, "/Bitand/", "G51", 0.0).
?test(sheet1_H51, "/Bitand/", "H51", 0.0).
?test(sheet1_I51, "/Bitand/", "I51", 0.0).
?test(sheet1_J51, "/Bitand/", "J51", 0.0).
?test(sheet1_K51, "/Bitand/", "K51", 0.0).
?test(sheet1_L51, "/Bitand/", "L51", 0.0).
?test(sheet1_M51, "/Bitand/", "M51", 0.0).
?test(sheet1_N51, "/Bitand/", "N51", 0.0).
?test(sheet1_O51, "/Bitand/", "O51", 0.0).
?test(sheet1_P51, "/Bitand/", "P51", 0.0).
?test(sheet1_Q51, "/Bitand/", "Q51", 0.0).
?test(sheet1_R51, "/Bitand/", "R51", 0.0).
?test(sheet1_S51, "/Bitand/", "S51", 0.0).
?test(sheet1_T51, "/Bitand/", "T51", 0.0).
?test(sheet1_U51, "/Bitand/", "U51", 0.0).
?test(sheet1_V51, "/Bitand/", "V51", 0.0).
?test(sheet1_C52, "/Bitand/", "C52", 0.0).
?test(sheet1_D52, "/Bitand/", "D52", 0.0).
?test(sheet1_E52, "/Bitand/", "E52", 0.0).
?test(sheet1_F52, "/Bitand/", "F52", 1.0).
?test(sheet1_G52, "/Bitand/", "G52", 0.0).
?test(sheet1_H52, "/Bitand/", "H52", 0.0).
?test(sheet1_I52, "/Bitand/", "I52", 0.0).
?test(sheet1_J52, "/Bitand/", "J52", 0.0).
?test(sheet1_K52, "/Bitand/", "K52", 0.0).
?test(sheet1_L52, "/Bitand/", "L52", 0.0).
?test(sheet1_M52, "/Bitand/", "M52", 0.0).
?test(sheet1_N52, "/Bitand/", "N52", 0.0).
?test(sheet1_O52, "/Bitand/", "O52", 0.0).
?test(sheet1_P52, "/Bitand/", "P52", 0.0).
?test(sheet1_Q52, "/Bitand/", "Q52", 0.0).
?test(sheet1_R52, "/Bitand/", "R52", 0.0).
?test(sheet1_S52, "/Bitand/", "S52", 0.0).
?test(sheet1_T52, "/Bitand/", "T52", 0.0).
?test(sheet1_U52, "/Bitand/", "U52", 0.0).
?test(sheet1_V52, "/Bitand/", "V52", 0.0).
?test(sheet1_C53, "/Bitand/", "C53", 0.0).
?test(sheet1_D53, "/Bitand/", "D53", 0.0).
?test(sheet1_E53, "/Bitand/", "E53", 0.0).
?test(sheet1_F53, "/Bitand/", "F53", 1.0).
?test(sheet1_G53, "/Bitand/", "G53", 0.0).
?test(sheet1_H53, "/Bitand/", "H53", 0.0).
?test(sheet1_I53, "/Bitand/", "I53", 0.0).
?test(sheet1_J53, "/Bitand/", "J53", 0.0).
?test(sheet1_K53, "/Bitand/", "K53", 0.0).
?test(sheet1_L53, "/Bitand/", "L53", 0.0).
?test(sheet1_M53, "/Bitand/", "M53", 0.0).
?test(sheet1_N53, "/Bitand/", "N53", 0.0).
?test(sheet1_O53, "/Bitand/", "O53", 0.0).
?test(sheet1_P53, "/Bitand/", "P53", 0.0).
?test(sheet1_Q53, "/Bitand/", "Q53", 0.0).
?test(sheet1_R53, "/Bitand/", "R53", 0.0).
?test(sheet1_S53, "/Bitand/", "S53", 0.0).
?test(sheet1_T53, "/Bitand/", "T53", 0.0).
?test(sheet1_U53, "/Bitand/", "U53", 0.0).
?test(sheet1_V53, "/Bitand/", "V53", 0.0).
?test(sheet1_C54, "/Bitand/", "C54", 0.0).
?test(sheet1_D54, "/Bitand/", "D54", 0.0).
?test(sheet1_E54, "/Bitand/", "E54", 0.0).
?test(sheet1_F54, "/Bitand/", "F54", 1.0).
?test(sheet1_G54, "/Bitand/", "G54", 0.0).
?test(sheet1_H54, "/Bitand/", "H54", 0.0).
?test(sheet1_I54, "/Bitand/", "I54", 0.0).
?test(sheet1_J54, "/Bitand/", "J54", 0.0).
?test(sheet1_K54, "/Bitand/", "K54", 0.0).
?test(sheet1_L54, "/Bitand/", "L54", 0.0).
?test(sheet1_M54, "/Bitand/", "M54", 0.0).
?test(sheet1_N54, "/Bitand/", "N54", 0.0).
?test(sheet1_O54, "/Bitand/", "O54", 0.0).
?test(sheet1_P54, "/Bitand/", "P54", 0.0).
?test(sheet1_Q54, "/Bitand/", "Q54", 0.0).
?test(sheet1_R54, "/Bitand/", "R54", 0.0).
?test(sheet1_S54, "/Bitand/", "S54", 0.0).
?test(sheet1_T54, "/Bitand/", "T54", 0.0).
?test(sheet1_U54, "/Bitand/", "U54", 0.0).
?test(sheet1_V54, "/Bitand/", "V54", 0.0).
?test(sheet1_C55, "/Bitand/", "C55", 0.0).
?test(sheet1_D55, "/Bitand/", "D55", 0.0).
?test(sheet1_E55, "/Bitand/", "E55", 0.0).
?test(sheet1_F55, "/Bitand/", "F55", 1.0).
?test(sheet1_G55, "/Bitand/", "G55", 0.0).
?test(sheet1_H55, "/Bitand/", "H55", 0.0).
?test(sheet1_I55, "/Bitand/", "I55", 0.0).
?test(sheet1_J55, "/Bitand/", "J55", 0.0).
?test(sheet1_K55, "/Bitand/", "K55", 0.0).
?test(sheet1_L55, "/Bitand/", "L55", 0.0).
?test(sheet1_M55, "/Bitand/", "M55", 0.0).
?test(sheet1_N55, "/Bitand/", "N55", 0.0).
?test(sheet1_O55, "/Bitand/", "O55", 0.0).
?test(sheet1_P55, "/Bitand/", "P55", 0.0).
?test(sheet1_Q55, "/Bitand/", "Q55", 0.0).
?test(sheet1_R55, "/Bitand/", "R55", 0.0).
?test(sheet1_S55, "/Bitand/", "S55", 0.0).
?test(sheet1_T55, "/Bitand/", "T55", 0.0).
?test(sheet1_U55, "/Bitand/", "U55", 0.0).
?test(sheet1_V55, "/Bitand/", "V55", 0.0).
?test(sheet1_C56, "/Bitand/", "C56", 0.0).
?test(sheet1_D56, "/Bitand/", "D56", 0.0).
?test(sheet1_E56, "/Bitand/", "E56", 0.0).
?test(sheet1_F56, "/Bitand/", "F56", 1.0).
?test(sheet1_G56, "/Bitand/", "G56", 0.0).
?test(sheet1_H56, "/Bitand/", "H56", 0.0).
?test(sheet1_I56, "/Bitand/", "I56", 0.0).
?test(sheet1_J56, "/Bitand/", "J56", 0.0).
?test(sheet1_K56, "/Bitand/", "K56", 0.0).
?test(sheet1_L56, "/Bitand/", "L56", 0.0).
?test(sheet1_M56, "/Bitand/", "M56", 0.0).
?test(sheet1_N56, "/Bitand/", "N56", 0.0).
?test(sheet1_O56, "/Bitand/", "O56", 0.0).
?test(sheet1_P56, "/Bitand/", "P56", 0.0).
?test(sheet1_Q56, "/Bitand/", "Q56", 0.0).
?test(sheet1_R56, "/Bitand/", "R56", 0.0).
?test(sheet1_S56, "/Bitand/", "S56", 0.0).
?test(sheet1_T56, "/Bitand/", "T56", 0.0).
?test(sheet1_U56, "/Bitand/", "U56", 0.0).
?test(sheet1_V56, "/Bitand/", "V56", 0.0).
?test(sheet1_C57, "/Bitand/", "C57", 0.0).
?test(sheet1_D57, "/Bitand/", "D57", 0.0).
?test(sheet1_E57, "/Bitand/", "E57", 0.0).
?test(sheet1_F57, "/Bitand/", "F57", 1.0).
?test(sheet1_G57, "/Bitand/", "G57", 0.0).
?test(sheet1_H57, "/Bitand/", "H57", 0.0).
?test(sheet1_I57, "/Bitand/", "I57", 0.0).
?test(sheet1_J57, "/Bitand/", "J57", 0.0).
?test(sheet1_K57, "/Bitand/", "K57", 0.0).
?test(sheet1_L57, "/Bitand/", "L57", 0.0).
?test(sheet1_M57, "/Bitand/", "M57", 0.0).
?test(sheet1_N57, "/Bitand/", "N57", 0.0).
?test(sheet1_O57, "/Bitand/", "O57", 0.0).
?test(sheet1_P57, "/Bitand/", "P57", 0.0).
?test(sheet1_Q57, "/Bitand/", "Q57", 0.0).
?test(sheet1_R57, "/Bitand/", "R57", 0.0).
?test(sheet1_S57, "/Bitand/", "S57", 0.0).
?test(sheet1_T57, "/Bitand/", "T57", 0.0).
?test(sheet1_U57, "/Bitand/", "U57", 0.0).
?test(sheet1_V57, "/Bitand/", "V57", 0.0).
?test(sheet1_C58, "/Bitand/", "C58", 0.0).
?test(sheet1_D58, "/Bitand/", "D58", 0.0).
?test(sheet1_E58, "/Bitand/", "E58", 0.0).
?test(sheet1_F58, "/Bitand/", "F58", 1.0).
?test(sheet1_G58, "/Bitand/", "G58", 0.0).
?test(sheet1_H58, "/Bitand/", "H58", 0.0).
?test(sheet1_I58, "/Bitand/", "I58", 0.0).
?test(sheet1_J58, "/Bitand/", "J58", 0.0).
?test(sheet1_K58, "/Bitand/", "K58", 0.0).
?test(sheet1_L58, "/Bitand/", "L58", 0.0).
?test(sheet1_M58, "/Bitand/", "M58", 0.0).
?test(sheet1_N58, "/Bitand/", "N58", 0.0).
?test(sheet1_O58, "/Bitand/", "O58", 0.0).
?test(sheet1_P58, "/Bitand/", "P58", 0.0).
?test(sheet1_Q58, "/Bitand/", "Q58", 0.0).
?test(sheet1_R58, "/Bitand/", "R58", 0.0).
?test(sheet1_S58, "/Bitand/", "S58", 0.0).
?test(sheet1_T58, "/Bitand/", "T58", 0.0).
?test(sheet1_U58, "/Bitand/", "U58", 0.0).
?test(sheet1_V58, "/Bitand/", "V58", 0.0).
?test(sheet1_C59, "/Bitand/", "C59", 0.0).
?test(sheet1_D59, "/Bitand/", "D59", 0.0).
?test(sheet1_E59, "/Bitand/", "E59", 0.0).
?test(sheet1_F59, "/Bitand/", "F59", 0.0).
?test(sheet1_G59, "/Bitand/", "G59", 0.0).
?test(sheet1_H59, "/Bitand/", "H59", 0.0).
?test(sheet1_I59, "/Bitand/", "I59", 0.0).
?test(sheet1_J59, "/Bitand/", "J59", 0.0).
?test(sheet1_K59, "/Bitand/", "K59", 0.0).
?test(sheet1_L59, "/Bitand/", "L59", 0.0).
?test(sheet1_M59, "/Bitand/", "M59", 0.0).
?test(sheet1_N59, "/Bitand/", "N59", 0.0).
?test(sheet1_O59, "/Bitand/", "O59", 0.0).
?test(sheet1_P59, "/Bitand/", "P59", 0.0).
?test(sheet1_Q59, "/Bitand/", "Q59", 0.0).
?test(sheet1_R59, "/Bitand/", "R59", 0.0).
?test(sheet1_S59, "/Bitand/", "S59", 0.0).
?test(sheet1_T59, "/Bitand/", "T59", 0.0).
?test(sheet1_U59, "/Bitand/", "U59", 0.0).
?test(sheet1_V59, "/Bitand/", "V59", 0.0).
?test(sheet1_C60, "/Bitand/", "C60", 0.0).
?test(sheet1_D60, "/Bitand/", "D60", 0.0).
?test(sheet1_E60, "/Bitand/", "E60", 0.0).
?test(sheet1_F60, "/Bitand/", "F60", 0.0).
?test(sheet1_G60, "/Bitand/", "G60", 0.0).
?test(sheet1_H60, "/Bitand/", "H60", 0.0).
?test(sheet1_I60, "/Bitand/", "I60", 0.0).
?test(sheet1_J60, "/Bitand/", "J60", 0.0).
?test(sheet1_K60, "/Bitand/", "K60", 0.0).
?test(sheet1_L60, "/Bitand/", "L60", 0.0).
?test(sheet1_M60, "/Bitand/", "M60", 0.0).
?test(sheet1_N60, "/Bitand/", "N60", 0.0).
?test(sheet1_O60, "/Bitand/", "O60", 0.0).
?test(sheet1_P60, "/Bitand/", "P60", 0.0).
?test(sheet1_Q60, "/Bitand/", "Q60", 0.0).
?test(sheet1_R60, "/Bitand/", "R60", 0.0).
?test(sheet1_S60, "/Bitand/", "S60", 0.0).
?test(sheet1_T60, "/Bitand/", "T60", 0.0).
?test(sheet1_U60, "/Bitand/", "U60", 0.0).
?test(sheet1_V60, "/Bitand/", "V60", 0.0).
?test(sheet1_C61, "/Bitand/", "C61", 0.0).
?test(sheet1_D61, "/Bitand/", "D61", 0.0).
?test(sheet1_E61, "/Bitand/", "E61", 0.0).
?test(sheet1_F61, "/Bitand/", "F61", 0.0).
?test(sheet1_G61, "/Bitand/", "G61", 0.0).
?test(sheet1_H61, "/Bitand/", "H61", 0.0).
?test(sheet1_I61, "/Bitand/", "I61", 0.0).
?test(sheet1_J61, "/Bitand/", "J61", 0.0).
?test(sheet1_K61, "/Bitand/", "K61", 0.0).
?test(sheet1_L61, "/Bitand/", "L61", 0.0).
?test(sheet1_M61, "/Bitand/", "M61", 0.0).
?test(sheet1_N61, "/Bitand/", "N61", 0.0).
?test(sheet1_O61, "/Bitand/", "O61", 0.0).
?test(sheet1_P61, "/Bitand/", "P61", 0.0).
?test(sheet1_Q61, "/Bitand/", "Q61", 0.0).
?test(sheet1_R61, "/Bitand/", "R61", 0.0).
?test(sheet1_S61, "/Bitand/", "S61", 0.0).
?test(sheet1_T61, "/Bitand/", "T61", 0.0).
?test(sheet1_U61, "/Bitand/", "U61", 0.0).
?test(sheet1_V61, "/Bitand/", "V61", 0.0).
?test(sheet1_C62, "/Bitand/", "C62", 0.0).
?test(sheet1_D62, "/Bitand/", "D62", 0.0).
?test(sheet1_E62, "/Bitand/", "E62", 0.0).
?test(sheet1_F62, "/Bitand/", "F62", 0.0).
?test(sheet1_G62, "/Bitand/", "G62", 0.0).
?test(sheet1_H62, "/Bitand/", "H62", 0.0).
?test(sheet1_I62, "/Bitand/", "I62", 0.0).
?test(sheet1_J62, "/Bitand/", "J62", 0.0).
?test(sheet1_K62, "/Bitand/", "K62", 0.0).
?test(sheet1_L62, "/Bitand/", "L62", 0.0).
?test(sheet1_M62, "/Bitand/", "M62", 0.0).
?test(sheet1_N62, "/Bitand/", "N62", 0.0).
?test(sheet1_O62, "/Bitand/", "O62", 0.0).
?test(sheet1_P62, "/Bitand/", "P62", 0.0).
?test(sheet1_Q62, "/Bitand/", "Q62", 0.0).
?test(sheet1_R62, "/Bitand/", "R62", 0.0).
?test(sheet1_S62, "/Bitand/", "S62", 0.0).
?test(sheet1_T62, "/Bitand/", "T62", 0.0).
?test(sheet1_U62, "/Bitand/", "U62", 0.0).
?test(sheet1_V62, "/Bitand/", "V62", 0.0).
?test(sheet1_C63, "/Bitand/", "C63", 0.0).
?test(sheet1_D63, "/Bitand/", "D63", 0.0).
?test(sheet1_E63, "/Bitand/", "E63", 0.0).
?test(sheet1_F63, "/Bitand/", "F63", 0.0).
?test(sheet1_G63, "/Bitand/", "G63", 0.0).
?test(sheet1_H63, "/Bitand/", "H63", 0.0).
?test(sheet1_I63, "/Bitand/", "I63", 0.0).
?test(sheet1_J63, "/Bitand/", "J63", 0.0).
?test(sheet1_K63, "/Bitand/", "K63", 0.0).
?test(sheet1_L63, "/Bitand/", "L63", 0.0).
?test(sheet1_M63, "/Bitand/", "M63", 0.0).
?test(sheet1_N63, "/Bitand/", "N63", 0.0).
?test(sheet1_O63, "/Bitand/", "O63", 0.0).
?test(sheet1_P63, "/Bitand/", "P63", 0.0).
?test(sheet1_Q63, "/Bitand/", "Q63", 0.0).
?test(sheet1_R63, "/Bitand/", "R63", 0.0).
?test(sheet1_S63, "/Bitand/", "S63", 0.0).
?test(sheet1_T63, "/Bitand/", "T63", 0.0).
?test(sheet1_U63, "/Bitand/", "U63", 0.0).
?test(sheet1_V63, "/Bitand/", "V63", 0.0).
?test(sheet1_C64, "/Bitand/", "C64", 0.0).
?test(sheet1_D64, "/Bitand/", "D64", 0.0).
?test(sheet1_E64, "/Bitand/", "E64", 0.0).
?test(sheet1_F64, "/Bitand/", "F64", 0.0).
?test(sheet1_G64, "/Bitand/", "G64", 0.0).
?test(sheet1_H64, "/Bitand/", "H64", 0.0).
?test(sheet1_I64, "/Bitand/", "I64", 0.0).
?test(sheet1_J64, "/Bitand/", "J64", 0.0).
?test(sheet1_K64, "/Bitand/", "K64", 0.0).
?test(sheet1_L64, "/Bitand/", "L64", 0.0).
?test(sheet1_M64, "/Bitand/", "M64", 0.0).
?test(sheet1_N64, "/Bitand/", "N64", 0.0).
?test(sheet1_O64, "/Bitand/", "O64", 0.0).
?test(sheet1_P64, "/Bitand/", "P64", 0.0).
?test(sheet1_Q64, "/Bitand/", "Q64", 0.0).
?test(sheet1_R64, "/Bitand/", "R64", 0.0).
?test(sheet1_S64, "/Bitand/", "S64", 0.0).
?test(sheet1_T64, "/Bitand/", "T64", 0.0).
?test(sheet1_U64, "/Bitand/", "U64", 0.0).
?test(sheet1_V64, "/Bitand/", "V64", 0.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_bitwise_bitand.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_bitwise_bitand" ++ "/" ++ Sheetname ++ "/",
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
