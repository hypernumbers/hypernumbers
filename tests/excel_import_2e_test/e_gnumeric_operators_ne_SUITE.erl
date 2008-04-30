%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_operators_ne_SUITE).
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
                     [Testcase, "e_gnumeric_operators_ne_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_operators_ne" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/NOT_EQUAL/", "A1", "<>").
?test(sheet1_B1, "/NOT_EQUAL/", "B1", "B").
?test(sheet1_C1, "/NOT_EQUAL/", "C1", "Blank").
?test(sheet1_D1, "/NOT_EQUAL/", "D1", "Boolean").
?test(sheet1_E1, "/NOT_EQUAL/", "E1", "Boolean").
?test(sheet1_F1, "/NOT_EQUAL/", "F1", "Error").
?test(sheet1_G1, "/NOT_EQUAL/", "G1", "Error").
?test(sheet1_H1, "/NOT_EQUAL/", "H1", "Error").
?test(sheet1_I1, "/NOT_EQUAL/", "I1", "Error").
?test(sheet1_J1, "/NOT_EQUAL/", "J1", "Error").
?test(sheet1_K1, "/NOT_EQUAL/", "K1", "Error").
?test(sheet1_L1, "/NOT_EQUAL/", "L1", "Error").
?test(sheet1_M1, "/NOT_EQUAL/", "M1", "String").
?test(sheet1_N1, "/NOT_EQUAL/", "N1", "String").
?test(sheet1_O1, "/NOT_EQUAL/", "O1", "String").
?test(sheet1_P1, "/NOT_EQUAL/", "P1", "Str Num").
?test(sheet1_Q1, "/NOT_EQUAL/", "Q1", "Str Num").
?test(sheet1_R1, "/NOT_EQUAL/", "R1", "Integer").
?test(sheet1_S1, "/NOT_EQUAL/", "S1", "Integer").
?test(sheet1_T1, "/NOT_EQUAL/", "T1", "Zero").
?test(sheet1_U1, "/NOT_EQUAL/", "U1", "Float").
?test(sheet1_V1, "/NOT_EQUAL/", "V1", "Float").
?test(sheet1_A2, "/NOT_EQUAL/", "A2", "A").
?test(sheet1_D2, "/NOT_EQUAL/", "D2", true).
?test(sheet1_E2, "/NOT_EQUAL/", "E2", false).
?test(sheet1_F2, "/NOT_EQUAL/", "F2", '#DIV/0!').
?test(sheet1_G2, "/NOT_EQUAL/", "G2", '#N/A').
?test(sheet1_H2, "/NOT_EQUAL/", "H2", '#NAME?').
?test(sheet1_I2, "/NOT_EQUAL/", "I2", 'NULL!').
?test(sheet1_J2, "/NOT_EQUAL/", "J2", '#NUM!').
?test(sheet1_K2, "/NOT_EQUAL/", "K2", '#REF!').
?test(sheet1_L2, "/NOT_EQUAL/", "L2", '#VALUE!').
?test(sheet1_M2, "/NOT_EQUAL/", "M2", "Liz").
?test(sheet1_N2, "/NOT_EQUAL/", "N2", "Doug").
?test(sheet1_O2, "/NOT_EQUAL/", "O2", "Bob").
?test(sheet1_P2, "/NOT_EQUAL/", "P2", "2.7").
?test(sheet1_Q2, "/NOT_EQUAL/", "Q2", "3.54").
?test(sheet1_R2, "/NOT_EQUAL/", "R2", "1999/02/01 00:00:00").
?test(sheet1_S2, "/NOT_EQUAL/", "S2", "1999/02/02 00:00:00").
?test(sheet1_T2, "/NOT_EQUAL/", "T2", 0.0).
?test(sheet1_U2, "/NOT_EQUAL/", "U2", 3.1415).
?test(sheet1_V2, "/NOT_EQUAL/", "V2", 36193.2).
?test(sheet1_A3, "/NOT_EQUAL/", "A3", "Blank").
?test(sheet1_C3, "/NOT_EQUAL/", "C3", false).
?test(sheet1_D3, "/NOT_EQUAL/", "D3", true).
?test(sheet1_E3, "/NOT_EQUAL/", "E3", false).
?test(sheet1_F3, "/NOT_EQUAL/", "F3", '#DIV/0!').
?test(sheet1_G3, "/NOT_EQUAL/", "G3", '#N/A').
?test(sheet1_H3, "/NOT_EQUAL/", "H3", '#NAME?').
?test(sheet1_I3, "/NOT_EQUAL/", "I3", 'NULL!').
?test(sheet1_J3, "/NOT_EQUAL/", "J3", '#NUM!').
?test(sheet1_K3, "/NOT_EQUAL/", "K3", '#REF!').
?test(sheet1_L3, "/NOT_EQUAL/", "L3", '#VALUE!').
?test(sheet1_M3, "/NOT_EQUAL/", "M3", true).
?test(sheet1_N3, "/NOT_EQUAL/", "N3", true).
?test(sheet1_O3, "/NOT_EQUAL/", "O3", true).
?test(sheet1_P3, "/NOT_EQUAL/", "P3", true).
?test(sheet1_Q3, "/NOT_EQUAL/", "Q3", true).
?test(sheet1_R3, "/NOT_EQUAL/", "R3", true).
?test(sheet1_S3, "/NOT_EQUAL/", "S3", true).
?test(sheet1_T3, "/NOT_EQUAL/", "T3", false).
?test(sheet1_U3, "/NOT_EQUAL/", "U3", true).
?test(sheet1_V3, "/NOT_EQUAL/", "V3", true).
?test(sheet1_A4, "/NOT_EQUAL/", "A4", "Boolean").
?test(sheet1_B4, "/NOT_EQUAL/", "B4", true).
?test(sheet1_C4, "/NOT_EQUAL/", "C4", true).
?test(sheet1_D4, "/NOT_EQUAL/", "D4", false).
?test(sheet1_E4, "/NOT_EQUAL/", "E4", true).
?test(sheet1_F4, "/NOT_EQUAL/", "F4", '#DIV/0!').
?test(sheet1_G4, "/NOT_EQUAL/", "G4", '#N/A').
?test(sheet1_H4, "/NOT_EQUAL/", "H4", '#NAME?').
?test(sheet1_I4, "/NOT_EQUAL/", "I4", 'NULL!').
?test(sheet1_J4, "/NOT_EQUAL/", "J4", '#NUM!').
?test(sheet1_K4, "/NOT_EQUAL/", "K4", '#REF!').
?test(sheet1_L4, "/NOT_EQUAL/", "L4", '#VALUE!').
?test(sheet1_M4, "/NOT_EQUAL/", "M4", true).
?test(sheet1_N4, "/NOT_EQUAL/", "N4", true).
?test(sheet1_O4, "/NOT_EQUAL/", "O4", true).
?test(sheet1_P4, "/NOT_EQUAL/", "P4", true).
?test(sheet1_Q4, "/NOT_EQUAL/", "Q4", true).
?test(sheet1_R4, "/NOT_EQUAL/", "R4", true).
?test(sheet1_S4, "/NOT_EQUAL/", "S4", true).
?test(sheet1_T4, "/NOT_EQUAL/", "T4", true).
?test(sheet1_U4, "/NOT_EQUAL/", "U4", true).
?test(sheet1_V4, "/NOT_EQUAL/", "V4", true).
?test(sheet1_A5, "/NOT_EQUAL/", "A5", "Boolean").
?test(sheet1_B5, "/NOT_EQUAL/", "B5", false).
?test(sheet1_C5, "/NOT_EQUAL/", "C5", false).
?test(sheet1_D5, "/NOT_EQUAL/", "D5", true).
?test(sheet1_E5, "/NOT_EQUAL/", "E5", false).
?test(sheet1_F5, "/NOT_EQUAL/", "F5", '#DIV/0!').
?test(sheet1_G5, "/NOT_EQUAL/", "G5", '#N/A').
?test(sheet1_H5, "/NOT_EQUAL/", "H5", '#NAME?').
?test(sheet1_I5, "/NOT_EQUAL/", "I5", 'NULL!').
?test(sheet1_J5, "/NOT_EQUAL/", "J5", '#NUM!').
?test(sheet1_K5, "/NOT_EQUAL/", "K5", '#REF!').
?test(sheet1_L5, "/NOT_EQUAL/", "L5", '#VALUE!').
?test(sheet1_M5, "/NOT_EQUAL/", "M5", true).
?test(sheet1_N5, "/NOT_EQUAL/", "N5", true).
?test(sheet1_O5, "/NOT_EQUAL/", "O5", true).
?test(sheet1_P5, "/NOT_EQUAL/", "P5", true).
?test(sheet1_Q5, "/NOT_EQUAL/", "Q5", true).
?test(sheet1_R5, "/NOT_EQUAL/", "R5", true).
?test(sheet1_S5, "/NOT_EQUAL/", "S5", true).
?test(sheet1_T5, "/NOT_EQUAL/", "T5", true).
?test(sheet1_U5, "/NOT_EQUAL/", "U5", true).
?test(sheet1_V5, "/NOT_EQUAL/", "V5", true).
?test(sheet1_A6, "/NOT_EQUAL/", "A6", "Error").
?test(sheet1_B6, "/NOT_EQUAL/", "B6", '#DIV/0!').
?test(sheet1_C6, "/NOT_EQUAL/", "C6", '#DIV/0!').
?test(sheet1_D6, "/NOT_EQUAL/", "D6", '#DIV/0!').
?test(sheet1_E6, "/NOT_EQUAL/", "E6", '#DIV/0!').
?test(sheet1_F6, "/NOT_EQUAL/", "F6", '#DIV/0!').
?test(sheet1_G6, "/NOT_EQUAL/", "G6", '#DIV/0!').
?test(sheet1_H6, "/NOT_EQUAL/", "H6", '#DIV/0!').
?test(sheet1_I6, "/NOT_EQUAL/", "I6", '#DIV/0!').
?test(sheet1_J6, "/NOT_EQUAL/", "J6", '#DIV/0!').
?test(sheet1_K6, "/NOT_EQUAL/", "K6", '#DIV/0!').
?test(sheet1_L6, "/NOT_EQUAL/", "L6", '#DIV/0!').
?test(sheet1_M6, "/NOT_EQUAL/", "M6", '#DIV/0!').
?test(sheet1_N6, "/NOT_EQUAL/", "N6", '#DIV/0!').
?test(sheet1_O6, "/NOT_EQUAL/", "O6", '#DIV/0!').
?test(sheet1_P6, "/NOT_EQUAL/", "P6", '#DIV/0!').
?test(sheet1_Q6, "/NOT_EQUAL/", "Q6", '#DIV/0!').
?test(sheet1_R6, "/NOT_EQUAL/", "R6", '#DIV/0!').
?test(sheet1_S6, "/NOT_EQUAL/", "S6", '#DIV/0!').
?test(sheet1_T6, "/NOT_EQUAL/", "T6", '#DIV/0!').
?test(sheet1_U6, "/NOT_EQUAL/", "U6", '#DIV/0!').
?test(sheet1_V6, "/NOT_EQUAL/", "V6", '#DIV/0!').
?test(sheet1_A7, "/NOT_EQUAL/", "A7", "Error").
?test(sheet1_B7, "/NOT_EQUAL/", "B7", '#N/A').
?test(sheet1_C7, "/NOT_EQUAL/", "C7", '#N/A').
?test(sheet1_D7, "/NOT_EQUAL/", "D7", '#N/A').
?test(sheet1_E7, "/NOT_EQUAL/", "E7", '#N/A').
?test(sheet1_F7, "/NOT_EQUAL/", "F7", '#N/A').
?test(sheet1_G7, "/NOT_EQUAL/", "G7", '#N/A').
?test(sheet1_H7, "/NOT_EQUAL/", "H7", '#N/A').
?test(sheet1_I7, "/NOT_EQUAL/", "I7", '#N/A').
?test(sheet1_J7, "/NOT_EQUAL/", "J7", '#N/A').
?test(sheet1_K7, "/NOT_EQUAL/", "K7", '#N/A').
?test(sheet1_L7, "/NOT_EQUAL/", "L7", '#N/A').
?test(sheet1_M7, "/NOT_EQUAL/", "M7", '#N/A').
?test(sheet1_N7, "/NOT_EQUAL/", "N7", '#N/A').
?test(sheet1_O7, "/NOT_EQUAL/", "O7", '#N/A').
?test(sheet1_P7, "/NOT_EQUAL/", "P7", '#N/A').
?test(sheet1_Q7, "/NOT_EQUAL/", "Q7", '#N/A').
?test(sheet1_R7, "/NOT_EQUAL/", "R7", '#N/A').
?test(sheet1_S7, "/NOT_EQUAL/", "S7", '#N/A').
?test(sheet1_T7, "/NOT_EQUAL/", "T7", '#N/A').
?test(sheet1_U7, "/NOT_EQUAL/", "U7", '#N/A').
?test(sheet1_V7, "/NOT_EQUAL/", "V7", '#N/A').
?test(sheet1_A8, "/NOT_EQUAL/", "A8", "Error").
?test(sheet1_B8, "/NOT_EQUAL/", "B8", '#NAME?').
?test(sheet1_C8, "/NOT_EQUAL/", "C8", '#NAME?').
?test(sheet1_D8, "/NOT_EQUAL/", "D8", '#NAME?').
?test(sheet1_E8, "/NOT_EQUAL/", "E8", '#NAME?').
?test(sheet1_F8, "/NOT_EQUAL/", "F8", '#NAME?').
?test(sheet1_G8, "/NOT_EQUAL/", "G8", '#NAME?').
?test(sheet1_H8, "/NOT_EQUAL/", "H8", '#NAME?').
?test(sheet1_I8, "/NOT_EQUAL/", "I8", '#NAME?').
?test(sheet1_J8, "/NOT_EQUAL/", "J8", '#NAME?').
?test(sheet1_K8, "/NOT_EQUAL/", "K8", '#NAME?').
?test(sheet1_L8, "/NOT_EQUAL/", "L8", '#NAME?').
?test(sheet1_M8, "/NOT_EQUAL/", "M8", '#NAME?').
?test(sheet1_N8, "/NOT_EQUAL/", "N8", '#NAME?').
?test(sheet1_O8, "/NOT_EQUAL/", "O8", '#NAME?').
?test(sheet1_P8, "/NOT_EQUAL/", "P8", '#NAME?').
?test(sheet1_Q8, "/NOT_EQUAL/", "Q8", '#NAME?').
?test(sheet1_R8, "/NOT_EQUAL/", "R8", '#NAME?').
?test(sheet1_S8, "/NOT_EQUAL/", "S8", '#NAME?').
?test(sheet1_T8, "/NOT_EQUAL/", "T8", '#NAME?').
?test(sheet1_U8, "/NOT_EQUAL/", "U8", '#NAME?').
?test(sheet1_V8, "/NOT_EQUAL/", "V8", '#NAME?').
?test(sheet1_A9, "/NOT_EQUAL/", "A9", "Error").
?test(sheet1_B9, "/NOT_EQUAL/", "B9", 'NULL!').
?test(sheet1_C9, "/NOT_EQUAL/", "C9", 'NULL!').
?test(sheet1_D9, "/NOT_EQUAL/", "D9", 'NULL!').
?test(sheet1_E9, "/NOT_EQUAL/", "E9", 'NULL!').
?test(sheet1_F9, "/NOT_EQUAL/", "F9", 'NULL!').
?test(sheet1_G9, "/NOT_EQUAL/", "G9", 'NULL!').
?test(sheet1_H9, "/NOT_EQUAL/", "H9", 'NULL!').
?test(sheet1_I9, "/NOT_EQUAL/", "I9", 'NULL!').
?test(sheet1_J9, "/NOT_EQUAL/", "J9", 'NULL!').
?test(sheet1_K9, "/NOT_EQUAL/", "K9", 'NULL!').
?test(sheet1_L9, "/NOT_EQUAL/", "L9", 'NULL!').
?test(sheet1_M9, "/NOT_EQUAL/", "M9", 'NULL!').
?test(sheet1_N9, "/NOT_EQUAL/", "N9", 'NULL!').
?test(sheet1_O9, "/NOT_EQUAL/", "O9", 'NULL!').
?test(sheet1_P9, "/NOT_EQUAL/", "P9", 'NULL!').
?test(sheet1_Q9, "/NOT_EQUAL/", "Q9", 'NULL!').
?test(sheet1_R9, "/NOT_EQUAL/", "R9", 'NULL!').
?test(sheet1_S9, "/NOT_EQUAL/", "S9", 'NULL!').
?test(sheet1_T9, "/NOT_EQUAL/", "T9", 'NULL!').
?test(sheet1_U9, "/NOT_EQUAL/", "U9", 'NULL!').
?test(sheet1_V9, "/NOT_EQUAL/", "V9", 'NULL!').
?test(sheet1_A10, "/NOT_EQUAL/", "A10", "Error").
?test(sheet1_B10, "/NOT_EQUAL/", "B10", '#NUM!').
?test(sheet1_C10, "/NOT_EQUAL/", "C10", '#NUM!').
?test(sheet1_D10, "/NOT_EQUAL/", "D10", '#NUM!').
?test(sheet1_E10, "/NOT_EQUAL/", "E10", '#NUM!').
?test(sheet1_F10, "/NOT_EQUAL/", "F10", '#NUM!').
?test(sheet1_G10, "/NOT_EQUAL/", "G10", '#NUM!').
?test(sheet1_H10, "/NOT_EQUAL/", "H10", '#NUM!').
?test(sheet1_I10, "/NOT_EQUAL/", "I10", '#NUM!').
?test(sheet1_J10, "/NOT_EQUAL/", "J10", '#NUM!').
?test(sheet1_K10, "/NOT_EQUAL/", "K10", '#NUM!').
?test(sheet1_L10, "/NOT_EQUAL/", "L10", '#NUM!').
?test(sheet1_M10, "/NOT_EQUAL/", "M10", '#NUM!').
?test(sheet1_N10, "/NOT_EQUAL/", "N10", '#NUM!').
?test(sheet1_O10, "/NOT_EQUAL/", "O10", '#NUM!').
?test(sheet1_P10, "/NOT_EQUAL/", "P10", '#NUM!').
?test(sheet1_Q10, "/NOT_EQUAL/", "Q10", '#NUM!').
?test(sheet1_R10, "/NOT_EQUAL/", "R10", '#NUM!').
?test(sheet1_S10, "/NOT_EQUAL/", "S10", '#NUM!').
?test(sheet1_T10, "/NOT_EQUAL/", "T10", '#NUM!').
?test(sheet1_U10, "/NOT_EQUAL/", "U10", '#NUM!').
?test(sheet1_V10, "/NOT_EQUAL/", "V10", '#NUM!').
?test(sheet1_A11, "/NOT_EQUAL/", "A11", "Error").
?test(sheet1_B11, "/NOT_EQUAL/", "B11", '#REF!').
?test(sheet1_C11, "/NOT_EQUAL/", "C11", '#REF!').
?test(sheet1_D11, "/NOT_EQUAL/", "D11", '#REF!').
?test(sheet1_E11, "/NOT_EQUAL/", "E11", '#REF!').
?test(sheet1_F11, "/NOT_EQUAL/", "F11", '#REF!').
?test(sheet1_G11, "/NOT_EQUAL/", "G11", '#REF!').
?test(sheet1_H11, "/NOT_EQUAL/", "H11", '#REF!').
?test(sheet1_I11, "/NOT_EQUAL/", "I11", '#REF!').
?test(sheet1_J11, "/NOT_EQUAL/", "J11", '#REF!').
?test(sheet1_K11, "/NOT_EQUAL/", "K11", '#REF!').
?test(sheet1_L11, "/NOT_EQUAL/", "L11", '#REF!').
?test(sheet1_M11, "/NOT_EQUAL/", "M11", '#REF!').
?test(sheet1_N11, "/NOT_EQUAL/", "N11", '#REF!').
?test(sheet1_O11, "/NOT_EQUAL/", "O11", '#REF!').
?test(sheet1_P11, "/NOT_EQUAL/", "P11", '#REF!').
?test(sheet1_Q11, "/NOT_EQUAL/", "Q11", '#REF!').
?test(sheet1_R11, "/NOT_EQUAL/", "R11", '#REF!').
?test(sheet1_S11, "/NOT_EQUAL/", "S11", '#REF!').
?test(sheet1_T11, "/NOT_EQUAL/", "T11", '#REF!').
?test(sheet1_U11, "/NOT_EQUAL/", "U11", '#REF!').
?test(sheet1_V11, "/NOT_EQUAL/", "V11", '#REF!').
?test(sheet1_A12, "/NOT_EQUAL/", "A12", "Error").
?test(sheet1_B12, "/NOT_EQUAL/", "B12", '#VALUE!').
?test(sheet1_C12, "/NOT_EQUAL/", "C12", '#VALUE!').
?test(sheet1_D12, "/NOT_EQUAL/", "D12", '#VALUE!').
?test(sheet1_E12, "/NOT_EQUAL/", "E12", '#VALUE!').
?test(sheet1_F12, "/NOT_EQUAL/", "F12", '#VALUE!').
?test(sheet1_G12, "/NOT_EQUAL/", "G12", '#VALUE!').
?test(sheet1_H12, "/NOT_EQUAL/", "H12", '#VALUE!').
?test(sheet1_I12, "/NOT_EQUAL/", "I12", '#VALUE!').
?test(sheet1_J12, "/NOT_EQUAL/", "J12", '#VALUE!').
?test(sheet1_K12, "/NOT_EQUAL/", "K12", '#VALUE!').
?test(sheet1_L12, "/NOT_EQUAL/", "L12", '#VALUE!').
?test(sheet1_M12, "/NOT_EQUAL/", "M12", '#VALUE!').
?test(sheet1_N12, "/NOT_EQUAL/", "N12", '#VALUE!').
?test(sheet1_O12, "/NOT_EQUAL/", "O12", '#VALUE!').
?test(sheet1_P12, "/NOT_EQUAL/", "P12", '#VALUE!').
?test(sheet1_Q12, "/NOT_EQUAL/", "Q12", '#VALUE!').
?test(sheet1_R12, "/NOT_EQUAL/", "R12", '#VALUE!').
?test(sheet1_S12, "/NOT_EQUAL/", "S12", '#VALUE!').
?test(sheet1_T12, "/NOT_EQUAL/", "T12", '#VALUE!').
?test(sheet1_U12, "/NOT_EQUAL/", "U12", '#VALUE!').
?test(sheet1_V12, "/NOT_EQUAL/", "V12", '#VALUE!').
?test(sheet1_A13, "/NOT_EQUAL/", "A13", "String").
?test(sheet1_B13, "/NOT_EQUAL/", "B13", "Liz").
?test(sheet1_C13, "/NOT_EQUAL/", "C13", true).
?test(sheet1_D13, "/NOT_EQUAL/", "D13", true).
?test(sheet1_E13, "/NOT_EQUAL/", "E13", true).
?test(sheet1_F13, "/NOT_EQUAL/", "F13", '#DIV/0!').
?test(sheet1_G13, "/NOT_EQUAL/", "G13", '#N/A').
?test(sheet1_H13, "/NOT_EQUAL/", "H13", '#NAME?').
?test(sheet1_I13, "/NOT_EQUAL/", "I13", 'NULL!').
?test(sheet1_J13, "/NOT_EQUAL/", "J13", '#NUM!').
?test(sheet1_K13, "/NOT_EQUAL/", "K13", '#REF!').
?test(sheet1_L13, "/NOT_EQUAL/", "L13", '#VALUE!').
?test(sheet1_M13, "/NOT_EQUAL/", "M13", false).
?test(sheet1_N13, "/NOT_EQUAL/", "N13", true).
?test(sheet1_O13, "/NOT_EQUAL/", "O13", true).
?test(sheet1_P13, "/NOT_EQUAL/", "P13", true).
?test(sheet1_Q13, "/NOT_EQUAL/", "Q13", true).
?test(sheet1_R13, "/NOT_EQUAL/", "R13", true).
?test(sheet1_S13, "/NOT_EQUAL/", "S13", true).
?test(sheet1_T13, "/NOT_EQUAL/", "T13", true).
?test(sheet1_U13, "/NOT_EQUAL/", "U13", true).
?test(sheet1_V13, "/NOT_EQUAL/", "V13", true).
?test(sheet1_A14, "/NOT_EQUAL/", "A14", "String").
?test(sheet1_B14, "/NOT_EQUAL/", "B14", "Doug").
?test(sheet1_C14, "/NOT_EQUAL/", "C14", true).
?test(sheet1_D14, "/NOT_EQUAL/", "D14", true).
?test(sheet1_E14, "/NOT_EQUAL/", "E14", true).
?test(sheet1_F14, "/NOT_EQUAL/", "F14", '#DIV/0!').
?test(sheet1_G14, "/NOT_EQUAL/", "G14", '#N/A').
?test(sheet1_H14, "/NOT_EQUAL/", "H14", '#NAME?').
?test(sheet1_I14, "/NOT_EQUAL/", "I14", 'NULL!').
?test(sheet1_J14, "/NOT_EQUAL/", "J14", '#NUM!').
?test(sheet1_K14, "/NOT_EQUAL/", "K14", '#REF!').
?test(sheet1_L14, "/NOT_EQUAL/", "L14", '#VALUE!').
?test(sheet1_M14, "/NOT_EQUAL/", "M14", true).
?test(sheet1_N14, "/NOT_EQUAL/", "N14", false).
?test(sheet1_O14, "/NOT_EQUAL/", "O14", true).
?test(sheet1_P14, "/NOT_EQUAL/", "P14", true).
?test(sheet1_Q14, "/NOT_EQUAL/", "Q14", true).
?test(sheet1_R14, "/NOT_EQUAL/", "R14", true).
?test(sheet1_S14, "/NOT_EQUAL/", "S14", true).
?test(sheet1_T14, "/NOT_EQUAL/", "T14", true).
?test(sheet1_U14, "/NOT_EQUAL/", "U14", true).
?test(sheet1_V14, "/NOT_EQUAL/", "V14", true).
?test(sheet1_A15, "/NOT_EQUAL/", "A15", "String").
?test(sheet1_B15, "/NOT_EQUAL/", "B15", "Bob").
?test(sheet1_C15, "/NOT_EQUAL/", "C15", true).
?test(sheet1_D15, "/NOT_EQUAL/", "D15", true).
?test(sheet1_E15, "/NOT_EQUAL/", "E15", true).
?test(sheet1_F15, "/NOT_EQUAL/", "F15", '#DIV/0!').
?test(sheet1_G15, "/NOT_EQUAL/", "G15", '#N/A').
?test(sheet1_H15, "/NOT_EQUAL/", "H15", '#NAME?').
?test(sheet1_I15, "/NOT_EQUAL/", "I15", 'NULL!').
?test(sheet1_J15, "/NOT_EQUAL/", "J15", '#NUM!').
?test(sheet1_K15, "/NOT_EQUAL/", "K15", '#REF!').
?test(sheet1_L15, "/NOT_EQUAL/", "L15", '#VALUE!').
?test(sheet1_M15, "/NOT_EQUAL/", "M15", true).
?test(sheet1_N15, "/NOT_EQUAL/", "N15", true).
?test(sheet1_O15, "/NOT_EQUAL/", "O15", false).
?test(sheet1_P15, "/NOT_EQUAL/", "P15", true).
?test(sheet1_Q15, "/NOT_EQUAL/", "Q15", true).
?test(sheet1_R15, "/NOT_EQUAL/", "R15", true).
?test(sheet1_S15, "/NOT_EQUAL/", "S15", true).
?test(sheet1_T15, "/NOT_EQUAL/", "T15", true).
?test(sheet1_U15, "/NOT_EQUAL/", "U15", true).
?test(sheet1_V15, "/NOT_EQUAL/", "V15", true).
?test(sheet1_A16, "/NOT_EQUAL/", "A16", "Str Num").
?test(sheet1_B16, "/NOT_EQUAL/", "B16", "2.7").
?test(sheet1_C16, "/NOT_EQUAL/", "C16", true).
?test(sheet1_D16, "/NOT_EQUAL/", "D16", true).
?test(sheet1_E16, "/NOT_EQUAL/", "E16", true).
?test(sheet1_F16, "/NOT_EQUAL/", "F16", '#DIV/0!').
?test(sheet1_G16, "/NOT_EQUAL/", "G16", '#N/A').
?test(sheet1_H16, "/NOT_EQUAL/", "H16", '#NAME?').
?test(sheet1_I16, "/NOT_EQUAL/", "I16", 'NULL!').
?test(sheet1_J16, "/NOT_EQUAL/", "J16", '#NUM!').
?test(sheet1_K16, "/NOT_EQUAL/", "K16", '#REF!').
?test(sheet1_L16, "/NOT_EQUAL/", "L16", '#VALUE!').
?test(sheet1_M16, "/NOT_EQUAL/", "M16", true).
?test(sheet1_N16, "/NOT_EQUAL/", "N16", true).
?test(sheet1_O16, "/NOT_EQUAL/", "O16", true).
?test(sheet1_P16, "/NOT_EQUAL/", "P16", false).
?test(sheet1_Q16, "/NOT_EQUAL/", "Q16", true).
?test(sheet1_R16, "/NOT_EQUAL/", "R16", true).
?test(sheet1_S16, "/NOT_EQUAL/", "S16", true).
?test(sheet1_T16, "/NOT_EQUAL/", "T16", true).
?test(sheet1_U16, "/NOT_EQUAL/", "U16", true).
?test(sheet1_V16, "/NOT_EQUAL/", "V16", true).
?test(sheet1_A17, "/NOT_EQUAL/", "A17", "Str Num").
?test(sheet1_B17, "/NOT_EQUAL/", "B17", "3.54").
?test(sheet1_C17, "/NOT_EQUAL/", "C17", true).
?test(sheet1_D17, "/NOT_EQUAL/", "D17", true).
?test(sheet1_E17, "/NOT_EQUAL/", "E17", true).
?test(sheet1_F17, "/NOT_EQUAL/", "F17", '#DIV/0!').
?test(sheet1_G17, "/NOT_EQUAL/", "G17", '#N/A').
?test(sheet1_H17, "/NOT_EQUAL/", "H17", '#NAME?').
?test(sheet1_I17, "/NOT_EQUAL/", "I17", 'NULL!').
?test(sheet1_J17, "/NOT_EQUAL/", "J17", '#NUM!').
?test(sheet1_K17, "/NOT_EQUAL/", "K17", '#REF!').
?test(sheet1_L17, "/NOT_EQUAL/", "L17", '#VALUE!').
?test(sheet1_M17, "/NOT_EQUAL/", "M17", true).
?test(sheet1_N17, "/NOT_EQUAL/", "N17", true).
?test(sheet1_O17, "/NOT_EQUAL/", "O17", true).
?test(sheet1_P17, "/NOT_EQUAL/", "P17", true).
?test(sheet1_Q17, "/NOT_EQUAL/", "Q17", false).
?test(sheet1_R17, "/NOT_EQUAL/", "R17", true).
?test(sheet1_S17, "/NOT_EQUAL/", "S17", true).
?test(sheet1_T17, "/NOT_EQUAL/", "T17", true).
?test(sheet1_U17, "/NOT_EQUAL/", "U17", true).
?test(sheet1_V17, "/NOT_EQUAL/", "V17", true).
?test(sheet1_A18, "/NOT_EQUAL/", "A18", "Integer").
?test(sheet1_B18, "/NOT_EQUAL/", "B18", "1999/02/01 00:00:00").
?test(sheet1_C18, "/NOT_EQUAL/", "C18", true).
?test(sheet1_D18, "/NOT_EQUAL/", "D18", true).
?test(sheet1_E18, "/NOT_EQUAL/", "E18", true).
?test(sheet1_F18, "/NOT_EQUAL/", "F18", '#DIV/0!').
?test(sheet1_G18, "/NOT_EQUAL/", "G18", '#N/A').
?test(sheet1_H18, "/NOT_EQUAL/", "H18", '#NAME?').
?test(sheet1_I18, "/NOT_EQUAL/", "I18", 'NULL!').
?test(sheet1_J18, "/NOT_EQUAL/", "J18", '#NUM!').
?test(sheet1_K18, "/NOT_EQUAL/", "K18", '#REF!').
?test(sheet1_L18, "/NOT_EQUAL/", "L18", '#VALUE!').
?test(sheet1_M18, "/NOT_EQUAL/", "M18", true).
?test(sheet1_N18, "/NOT_EQUAL/", "N18", true).
?test(sheet1_O18, "/NOT_EQUAL/", "O18", true).
?test(sheet1_P18, "/NOT_EQUAL/", "P18", true).
?test(sheet1_Q18, "/NOT_EQUAL/", "Q18", true).
?test(sheet1_R18, "/NOT_EQUAL/", "R18", false).
?test(sheet1_S18, "/NOT_EQUAL/", "S18", true).
?test(sheet1_T18, "/NOT_EQUAL/", "T18", true).
?test(sheet1_U18, "/NOT_EQUAL/", "U18", true).
?test(sheet1_V18, "/NOT_EQUAL/", "V18", true).
?test(sheet1_A19, "/NOT_EQUAL/", "A19", "Integer").
?test(sheet1_B19, "/NOT_EQUAL/", "B19", "1999/02/02 00:00:00").
?test(sheet1_C19, "/NOT_EQUAL/", "C19", true).
?test(sheet1_D19, "/NOT_EQUAL/", "D19", true).
?test(sheet1_E19, "/NOT_EQUAL/", "E19", true).
?test(sheet1_F19, "/NOT_EQUAL/", "F19", '#DIV/0!').
?test(sheet1_G19, "/NOT_EQUAL/", "G19", '#N/A').
?test(sheet1_H19, "/NOT_EQUAL/", "H19", '#NAME?').
?test(sheet1_I19, "/NOT_EQUAL/", "I19", 'NULL!').
?test(sheet1_J19, "/NOT_EQUAL/", "J19", '#NUM!').
?test(sheet1_K19, "/NOT_EQUAL/", "K19", '#REF!').
?test(sheet1_L19, "/NOT_EQUAL/", "L19", '#VALUE!').
?test(sheet1_M19, "/NOT_EQUAL/", "M19", true).
?test(sheet1_N19, "/NOT_EQUAL/", "N19", true).
?test(sheet1_O19, "/NOT_EQUAL/", "O19", true).
?test(sheet1_P19, "/NOT_EQUAL/", "P19", true).
?test(sheet1_Q19, "/NOT_EQUAL/", "Q19", true).
?test(sheet1_R19, "/NOT_EQUAL/", "R19", true).
?test(sheet1_S19, "/NOT_EQUAL/", "S19", false).
?test(sheet1_T19, "/NOT_EQUAL/", "T19", true).
?test(sheet1_U19, "/NOT_EQUAL/", "U19", true).
?test(sheet1_V19, "/NOT_EQUAL/", "V19", true).
?test(sheet1_A20, "/NOT_EQUAL/", "A20", "Zero").
?test(sheet1_B20, "/NOT_EQUAL/", "B20", 0.0).
?test(sheet1_C20, "/NOT_EQUAL/", "C20", false).
?test(sheet1_D20, "/NOT_EQUAL/", "D20", true).
?test(sheet1_E20, "/NOT_EQUAL/", "E20", true).
?test(sheet1_F20, "/NOT_EQUAL/", "F20", '#DIV/0!').
?test(sheet1_G20, "/NOT_EQUAL/", "G20", '#N/A').
?test(sheet1_H20, "/NOT_EQUAL/", "H20", '#NAME?').
?test(sheet1_I20, "/NOT_EQUAL/", "I20", 'NULL!').
?test(sheet1_J20, "/NOT_EQUAL/", "J20", '#NUM!').
?test(sheet1_K20, "/NOT_EQUAL/", "K20", '#REF!').
?test(sheet1_L20, "/NOT_EQUAL/", "L20", '#VALUE!').
?test(sheet1_M20, "/NOT_EQUAL/", "M20", true).
?test(sheet1_N20, "/NOT_EQUAL/", "N20", true).
?test(sheet1_O20, "/NOT_EQUAL/", "O20", true).
?test(sheet1_P20, "/NOT_EQUAL/", "P20", true).
?test(sheet1_Q20, "/NOT_EQUAL/", "Q20", true).
?test(sheet1_R20, "/NOT_EQUAL/", "R20", true).
?test(sheet1_S20, "/NOT_EQUAL/", "S20", true).
?test(sheet1_T20, "/NOT_EQUAL/", "T20", false).
?test(sheet1_U20, "/NOT_EQUAL/", "U20", true).
?test(sheet1_V20, "/NOT_EQUAL/", "V20", true).
?test(sheet1_A21, "/NOT_EQUAL/", "A21", "Float").
?test(sheet1_B21, "/NOT_EQUAL/", "B21", 3.1415).
?test(sheet1_C21, "/NOT_EQUAL/", "C21", true).
?test(sheet1_D21, "/NOT_EQUAL/", "D21", true).
?test(sheet1_E21, "/NOT_EQUAL/", "E21", true).
?test(sheet1_F21, "/NOT_EQUAL/", "F21", '#DIV/0!').
?test(sheet1_G21, "/NOT_EQUAL/", "G21", '#N/A').
?test(sheet1_H21, "/NOT_EQUAL/", "H21", '#NAME?').
?test(sheet1_I21, "/NOT_EQUAL/", "I21", 'NULL!').
?test(sheet1_J21, "/NOT_EQUAL/", "J21", '#NUM!').
?test(sheet1_K21, "/NOT_EQUAL/", "K21", '#REF!').
?test(sheet1_L21, "/NOT_EQUAL/", "L21", '#VALUE!').
?test(sheet1_M21, "/NOT_EQUAL/", "M21", true).
?test(sheet1_N21, "/NOT_EQUAL/", "N21", true).
?test(sheet1_O21, "/NOT_EQUAL/", "O21", true).
?test(sheet1_P21, "/NOT_EQUAL/", "P21", true).
?test(sheet1_Q21, "/NOT_EQUAL/", "Q21", true).
?test(sheet1_R21, "/NOT_EQUAL/", "R21", true).
?test(sheet1_S21, "/NOT_EQUAL/", "S21", true).
?test(sheet1_T21, "/NOT_EQUAL/", "T21", true).
?test(sheet1_U21, "/NOT_EQUAL/", "U21", false).
?test(sheet1_V21, "/NOT_EQUAL/", "V21", true).
?test(sheet1_A22, "/NOT_EQUAL/", "A22", "Float").
?test(sheet1_B22, "/NOT_EQUAL/", "B22", 36193.2).
?test(sheet1_C22, "/NOT_EQUAL/", "C22", true).
?test(sheet1_D22, "/NOT_EQUAL/", "D22", true).
?test(sheet1_E22, "/NOT_EQUAL/", "E22", true).
?test(sheet1_F22, "/NOT_EQUAL/", "F22", '#DIV/0!').
?test(sheet1_G22, "/NOT_EQUAL/", "G22", '#N/A').
?test(sheet1_H22, "/NOT_EQUAL/", "H22", '#NAME?').
?test(sheet1_I22, "/NOT_EQUAL/", "I22", 'NULL!').
?test(sheet1_J22, "/NOT_EQUAL/", "J22", '#NUM!').
?test(sheet1_K22, "/NOT_EQUAL/", "K22", '#REF!').
?test(sheet1_L22, "/NOT_EQUAL/", "L22", '#VALUE!').
?test(sheet1_M22, "/NOT_EQUAL/", "M22", true).
?test(sheet1_N22, "/NOT_EQUAL/", "N22", true).
?test(sheet1_O22, "/NOT_EQUAL/", "O22", true).
?test(sheet1_P22, "/NOT_EQUAL/", "P22", true).
?test(sheet1_Q22, "/NOT_EQUAL/", "Q22", true).
?test(sheet1_R22, "/NOT_EQUAL/", "R22", true).
?test(sheet1_S22, "/NOT_EQUAL/", "S22", true).
?test(sheet1_T22, "/NOT_EQUAL/", "T22", true).
?test(sheet1_U22, "/NOT_EQUAL/", "U22", true).
?test(sheet1_V22, "/NOT_EQUAL/", "V22", false).
?test(sheet1_A25, "/NOT_EQUAL/", "A25", "Blank").
?test(sheet1_C25, "/NOT_EQUAL/", "C25", false).
?test(sheet1_D25, "/NOT_EQUAL/", "D25", true).
?test(sheet1_E25, "/NOT_EQUAL/", "E25", false).
?test(sheet1_F25, "/NOT_EQUAL/", "F25", '#DIV/0!').
?test(sheet1_G25, "/NOT_EQUAL/", "G25", '#N/A').
?test(sheet1_H25, "/NOT_EQUAL/", "H25", '#NAME?').
?test(sheet1_I25, "/NOT_EQUAL/", "I25", 'NULL!').
?test(sheet1_J25, "/NOT_EQUAL/", "J25", '#NUM!').
?test(sheet1_K25, "/NOT_EQUAL/", "K25", '#REF!').
?test(sheet1_L25, "/NOT_EQUAL/", "L25", '#VALUE!').
?test(sheet1_M25, "/NOT_EQUAL/", "M25", true).
?test(sheet1_N25, "/NOT_EQUAL/", "N25", true).
?test(sheet1_O25, "/NOT_EQUAL/", "O25", true).
?test(sheet1_P25, "/NOT_EQUAL/", "P25", true).
?test(sheet1_Q25, "/NOT_EQUAL/", "Q25", true).
?test(sheet1_R25, "/NOT_EQUAL/", "R25", true).
?test(sheet1_S25, "/NOT_EQUAL/", "S25", true).
?test(sheet1_T25, "/NOT_EQUAL/", "T25", false).
?test(sheet1_U25, "/NOT_EQUAL/", "U25", true).
?test(sheet1_V25, "/NOT_EQUAL/", "V25", true).
?test(sheet1_A26, "/NOT_EQUAL/", "A26", "Boolean").
?test(sheet1_C26, "/NOT_EQUAL/", "C26", true).
?test(sheet1_D26, "/NOT_EQUAL/", "D26", false).
?test(sheet1_E26, "/NOT_EQUAL/", "E26", true).
?test(sheet1_F26, "/NOT_EQUAL/", "F26", '#DIV/0!').
?test(sheet1_G26, "/NOT_EQUAL/", "G26", '#N/A').
?test(sheet1_H26, "/NOT_EQUAL/", "H26", '#NAME?').
?test(sheet1_I26, "/NOT_EQUAL/", "I26", 'NULL!').
?test(sheet1_J26, "/NOT_EQUAL/", "J26", '#NUM!').
?test(sheet1_K26, "/NOT_EQUAL/", "K26", '#REF!').
?test(sheet1_L26, "/NOT_EQUAL/", "L26", '#VALUE!').
?test(sheet1_M26, "/NOT_EQUAL/", "M26", true).
?test(sheet1_N26, "/NOT_EQUAL/", "N26", true).
?test(sheet1_O26, "/NOT_EQUAL/", "O26", true).
?test(sheet1_P26, "/NOT_EQUAL/", "P26", true).
?test(sheet1_Q26, "/NOT_EQUAL/", "Q26", true).
?test(sheet1_R26, "/NOT_EQUAL/", "R26", true).
?test(sheet1_S26, "/NOT_EQUAL/", "S26", true).
?test(sheet1_T26, "/NOT_EQUAL/", "T26", true).
?test(sheet1_U26, "/NOT_EQUAL/", "U26", true).
?test(sheet1_V26, "/NOT_EQUAL/", "V26", true).
?test(sheet1_A27, "/NOT_EQUAL/", "A27", "Boolean").
?test(sheet1_C27, "/NOT_EQUAL/", "C27", false).
?test(sheet1_D27, "/NOT_EQUAL/", "D27", true).
?test(sheet1_E27, "/NOT_EQUAL/", "E27", false).
?test(sheet1_F27, "/NOT_EQUAL/", "F27", '#DIV/0!').
?test(sheet1_G27, "/NOT_EQUAL/", "G27", '#N/A').
?test(sheet1_H27, "/NOT_EQUAL/", "H27", '#NAME?').
?test(sheet1_I27, "/NOT_EQUAL/", "I27", 'NULL!').
?test(sheet1_J27, "/NOT_EQUAL/", "J27", '#NUM!').
?test(sheet1_K27, "/NOT_EQUAL/", "K27", '#REF!').
?test(sheet1_L27, "/NOT_EQUAL/", "L27", '#VALUE!').
?test(sheet1_M27, "/NOT_EQUAL/", "M27", true).
?test(sheet1_N27, "/NOT_EQUAL/", "N27", true).
?test(sheet1_O27, "/NOT_EQUAL/", "O27", true).
?test(sheet1_P27, "/NOT_EQUAL/", "P27", true).
?test(sheet1_Q27, "/NOT_EQUAL/", "Q27", true).
?test(sheet1_R27, "/NOT_EQUAL/", "R27", true).
?test(sheet1_S27, "/NOT_EQUAL/", "S27", true).
?test(sheet1_T27, "/NOT_EQUAL/", "T27", true).
?test(sheet1_U27, "/NOT_EQUAL/", "U27", true).
?test(sheet1_V27, "/NOT_EQUAL/", "V27", true).
?test(sheet1_A28, "/NOT_EQUAL/", "A28", "Error").
?test(sheet1_C28, "/NOT_EQUAL/", "C28", '#DIV/0!').
?test(sheet1_D28, "/NOT_EQUAL/", "D28", '#DIV/0!').
?test(sheet1_E28, "/NOT_EQUAL/", "E28", '#DIV/0!').
?test(sheet1_F28, "/NOT_EQUAL/", "F28", '#DIV/0!').
?test(sheet1_G28, "/NOT_EQUAL/", "G28", '#DIV/0!').
?test(sheet1_H28, "/NOT_EQUAL/", "H28", '#DIV/0!').
?test(sheet1_I28, "/NOT_EQUAL/", "I28", '#DIV/0!').
?test(sheet1_J28, "/NOT_EQUAL/", "J28", '#DIV/0!').
?test(sheet1_K28, "/NOT_EQUAL/", "K28", '#DIV/0!').
?test(sheet1_L28, "/NOT_EQUAL/", "L28", '#DIV/0!').
?test(sheet1_M28, "/NOT_EQUAL/", "M28", '#DIV/0!').
?test(sheet1_N28, "/NOT_EQUAL/", "N28", '#DIV/0!').
?test(sheet1_O28, "/NOT_EQUAL/", "O28", '#DIV/0!').
?test(sheet1_P28, "/NOT_EQUAL/", "P28", '#DIV/0!').
?test(sheet1_Q28, "/NOT_EQUAL/", "Q28", '#DIV/0!').
?test(sheet1_R28, "/NOT_EQUAL/", "R28", '#DIV/0!').
?test(sheet1_S28, "/NOT_EQUAL/", "S28", '#DIV/0!').
?test(sheet1_T28, "/NOT_EQUAL/", "T28", '#DIV/0!').
?test(sheet1_U28, "/NOT_EQUAL/", "U28", '#DIV/0!').
?test(sheet1_V28, "/NOT_EQUAL/", "V28", '#DIV/0!').
?test(sheet1_A29, "/NOT_EQUAL/", "A29", "Error").
?test(sheet1_C29, "/NOT_EQUAL/", "C29", '#N/A').
?test(sheet1_D29, "/NOT_EQUAL/", "D29", '#N/A').
?test(sheet1_E29, "/NOT_EQUAL/", "E29", '#N/A').
?test(sheet1_F29, "/NOT_EQUAL/", "F29", '#N/A').
?test(sheet1_G29, "/NOT_EQUAL/", "G29", '#N/A').
?test(sheet1_H29, "/NOT_EQUAL/", "H29", '#N/A').
?test(sheet1_I29, "/NOT_EQUAL/", "I29", '#N/A').
?test(sheet1_J29, "/NOT_EQUAL/", "J29", '#N/A').
?test(sheet1_K29, "/NOT_EQUAL/", "K29", '#N/A').
?test(sheet1_L29, "/NOT_EQUAL/", "L29", '#N/A').
?test(sheet1_M29, "/NOT_EQUAL/", "M29", '#N/A').
?test(sheet1_N29, "/NOT_EQUAL/", "N29", '#N/A').
?test(sheet1_O29, "/NOT_EQUAL/", "O29", '#N/A').
?test(sheet1_P29, "/NOT_EQUAL/", "P29", '#N/A').
?test(sheet1_Q29, "/NOT_EQUAL/", "Q29", '#N/A').
?test(sheet1_R29, "/NOT_EQUAL/", "R29", '#N/A').
?test(sheet1_S29, "/NOT_EQUAL/", "S29", '#N/A').
?test(sheet1_T29, "/NOT_EQUAL/", "T29", '#N/A').
?test(sheet1_U29, "/NOT_EQUAL/", "U29", '#N/A').
?test(sheet1_V29, "/NOT_EQUAL/", "V29", '#N/A').
?test(sheet1_A30, "/NOT_EQUAL/", "A30", "Error").
?test(sheet1_C30, "/NOT_EQUAL/", "C30", '#NAME?').
?test(sheet1_D30, "/NOT_EQUAL/", "D30", '#NAME?').
?test(sheet1_E30, "/NOT_EQUAL/", "E30", '#NAME?').
?test(sheet1_F30, "/NOT_EQUAL/", "F30", '#NAME?').
?test(sheet1_G30, "/NOT_EQUAL/", "G30", '#NAME?').
?test(sheet1_H30, "/NOT_EQUAL/", "H30", '#NAME?').
?test(sheet1_I30, "/NOT_EQUAL/", "I30", '#NAME?').
?test(sheet1_J30, "/NOT_EQUAL/", "J30", '#NAME?').
?test(sheet1_K30, "/NOT_EQUAL/", "K30", '#NAME?').
?test(sheet1_L30, "/NOT_EQUAL/", "L30", '#NAME?').
?test(sheet1_M30, "/NOT_EQUAL/", "M30", '#NAME?').
?test(sheet1_N30, "/NOT_EQUAL/", "N30", '#NAME?').
?test(sheet1_O30, "/NOT_EQUAL/", "O30", '#NAME?').
?test(sheet1_P30, "/NOT_EQUAL/", "P30", '#NAME?').
?test(sheet1_Q30, "/NOT_EQUAL/", "Q30", '#NAME?').
?test(sheet1_R30, "/NOT_EQUAL/", "R30", '#NAME?').
?test(sheet1_S30, "/NOT_EQUAL/", "S30", '#NAME?').
?test(sheet1_T30, "/NOT_EQUAL/", "T30", '#NAME?').
?test(sheet1_U30, "/NOT_EQUAL/", "U30", '#NAME?').
?test(sheet1_V30, "/NOT_EQUAL/", "V30", '#NAME?').
?test(sheet1_A31, "/NOT_EQUAL/", "A31", "Error").
?test(sheet1_C31, "/NOT_EQUAL/", "C31", 'NULL!').
?test(sheet1_D31, "/NOT_EQUAL/", "D31", 'NULL!').
?test(sheet1_E31, "/NOT_EQUAL/", "E31", 'NULL!').
?test(sheet1_F31, "/NOT_EQUAL/", "F31", 'NULL!').
?test(sheet1_G31, "/NOT_EQUAL/", "G31", 'NULL!').
?test(sheet1_H31, "/NOT_EQUAL/", "H31", 'NULL!').
?test(sheet1_I31, "/NOT_EQUAL/", "I31", 'NULL!').
?test(sheet1_J31, "/NOT_EQUAL/", "J31", 'NULL!').
?test(sheet1_K31, "/NOT_EQUAL/", "K31", 'NULL!').
?test(sheet1_L31, "/NOT_EQUAL/", "L31", 'NULL!').
?test(sheet1_M31, "/NOT_EQUAL/", "M31", 'NULL!').
?test(sheet1_N31, "/NOT_EQUAL/", "N31", 'NULL!').
?test(sheet1_O31, "/NOT_EQUAL/", "O31", 'NULL!').
?test(sheet1_P31, "/NOT_EQUAL/", "P31", 'NULL!').
?test(sheet1_Q31, "/NOT_EQUAL/", "Q31", 'NULL!').
?test(sheet1_R31, "/NOT_EQUAL/", "R31", 'NULL!').
?test(sheet1_S31, "/NOT_EQUAL/", "S31", 'NULL!').
?test(sheet1_T31, "/NOT_EQUAL/", "T31", 'NULL!').
?test(sheet1_U31, "/NOT_EQUAL/", "U31", 'NULL!').
?test(sheet1_V31, "/NOT_EQUAL/", "V31", 'NULL!').
?test(sheet1_A32, "/NOT_EQUAL/", "A32", "Error").
?test(sheet1_C32, "/NOT_EQUAL/", "C32", '#NUM!').
?test(sheet1_D32, "/NOT_EQUAL/", "D32", '#NUM!').
?test(sheet1_E32, "/NOT_EQUAL/", "E32", '#NUM!').
?test(sheet1_F32, "/NOT_EQUAL/", "F32", '#NUM!').
?test(sheet1_G32, "/NOT_EQUAL/", "G32", '#NUM!').
?test(sheet1_H32, "/NOT_EQUAL/", "H32", '#NUM!').
?test(sheet1_I32, "/NOT_EQUAL/", "I32", '#NUM!').
?test(sheet1_J32, "/NOT_EQUAL/", "J32", '#NUM!').
?test(sheet1_K32, "/NOT_EQUAL/", "K32", '#NUM!').
?test(sheet1_L32, "/NOT_EQUAL/", "L32", '#NUM!').
?test(sheet1_M32, "/NOT_EQUAL/", "M32", '#NUM!').
?test(sheet1_N32, "/NOT_EQUAL/", "N32", '#NUM!').
?test(sheet1_O32, "/NOT_EQUAL/", "O32", '#NUM!').
?test(sheet1_P32, "/NOT_EQUAL/", "P32", '#NUM!').
?test(sheet1_Q32, "/NOT_EQUAL/", "Q32", '#NUM!').
?test(sheet1_R32, "/NOT_EQUAL/", "R32", '#NUM!').
?test(sheet1_S32, "/NOT_EQUAL/", "S32", '#NUM!').
?test(sheet1_T32, "/NOT_EQUAL/", "T32", '#NUM!').
?test(sheet1_U32, "/NOT_EQUAL/", "U32", '#NUM!').
?test(sheet1_V32, "/NOT_EQUAL/", "V32", '#NUM!').
?test(sheet1_A33, "/NOT_EQUAL/", "A33", "Error").
?test(sheet1_C33, "/NOT_EQUAL/", "C33", '#REF!').
?test(sheet1_D33, "/NOT_EQUAL/", "D33", '#REF!').
?test(sheet1_E33, "/NOT_EQUAL/", "E33", '#REF!').
?test(sheet1_F33, "/NOT_EQUAL/", "F33", '#REF!').
?test(sheet1_G33, "/NOT_EQUAL/", "G33", '#REF!').
?test(sheet1_H33, "/NOT_EQUAL/", "H33", '#REF!').
?test(sheet1_I33, "/NOT_EQUAL/", "I33", '#REF!').
?test(sheet1_J33, "/NOT_EQUAL/", "J33", '#REF!').
?test(sheet1_K33, "/NOT_EQUAL/", "K33", '#REF!').
?test(sheet1_L33, "/NOT_EQUAL/", "L33", '#REF!').
?test(sheet1_M33, "/NOT_EQUAL/", "M33", '#REF!').
?test(sheet1_N33, "/NOT_EQUAL/", "N33", '#REF!').
?test(sheet1_O33, "/NOT_EQUAL/", "O33", '#REF!').
?test(sheet1_P33, "/NOT_EQUAL/", "P33", '#REF!').
?test(sheet1_Q33, "/NOT_EQUAL/", "Q33", '#REF!').
?test(sheet1_R33, "/NOT_EQUAL/", "R33", '#REF!').
?test(sheet1_S33, "/NOT_EQUAL/", "S33", '#REF!').
?test(sheet1_T33, "/NOT_EQUAL/", "T33", '#REF!').
?test(sheet1_U33, "/NOT_EQUAL/", "U33", '#REF!').
?test(sheet1_V33, "/NOT_EQUAL/", "V33", '#REF!').
?test(sheet1_A34, "/NOT_EQUAL/", "A34", "Error").
?test(sheet1_C34, "/NOT_EQUAL/", "C34", '#VALUE!').
?test(sheet1_D34, "/NOT_EQUAL/", "D34", '#VALUE!').
?test(sheet1_E34, "/NOT_EQUAL/", "E34", '#VALUE!').
?test(sheet1_F34, "/NOT_EQUAL/", "F34", '#VALUE!').
?test(sheet1_G34, "/NOT_EQUAL/", "G34", '#VALUE!').
?test(sheet1_H34, "/NOT_EQUAL/", "H34", '#VALUE!').
?test(sheet1_I34, "/NOT_EQUAL/", "I34", '#VALUE!').
?test(sheet1_J34, "/NOT_EQUAL/", "J34", '#VALUE!').
?test(sheet1_K34, "/NOT_EQUAL/", "K34", '#VALUE!').
?test(sheet1_L34, "/NOT_EQUAL/", "L34", '#VALUE!').
?test(sheet1_M34, "/NOT_EQUAL/", "M34", '#VALUE!').
?test(sheet1_N34, "/NOT_EQUAL/", "N34", '#VALUE!').
?test(sheet1_O34, "/NOT_EQUAL/", "O34", '#VALUE!').
?test(sheet1_P34, "/NOT_EQUAL/", "P34", '#VALUE!').
?test(sheet1_Q34, "/NOT_EQUAL/", "Q34", '#VALUE!').
?test(sheet1_R34, "/NOT_EQUAL/", "R34", '#VALUE!').
?test(sheet1_S34, "/NOT_EQUAL/", "S34", '#VALUE!').
?test(sheet1_T34, "/NOT_EQUAL/", "T34", '#VALUE!').
?test(sheet1_U34, "/NOT_EQUAL/", "U34", '#VALUE!').
?test(sheet1_V34, "/NOT_EQUAL/", "V34", '#VALUE!').
?test(sheet1_A35, "/NOT_EQUAL/", "A35", "String").
?test(sheet1_C35, "/NOT_EQUAL/", "C35", true).
?test(sheet1_D35, "/NOT_EQUAL/", "D35", true).
?test(sheet1_E35, "/NOT_EQUAL/", "E35", true).
?test(sheet1_F35, "/NOT_EQUAL/", "F35", '#DIV/0!').
?test(sheet1_G35, "/NOT_EQUAL/", "G35", '#N/A').
?test(sheet1_H35, "/NOT_EQUAL/", "H35", '#NAME?').
?test(sheet1_I35, "/NOT_EQUAL/", "I35", 'NULL!').
?test(sheet1_J35, "/NOT_EQUAL/", "J35", '#NUM!').
?test(sheet1_K35, "/NOT_EQUAL/", "K35", '#REF!').
?test(sheet1_L35, "/NOT_EQUAL/", "L35", '#VALUE!').
?test(sheet1_M35, "/NOT_EQUAL/", "M35", false).
?test(sheet1_N35, "/NOT_EQUAL/", "N35", true).
?test(sheet1_O35, "/NOT_EQUAL/", "O35", true).
?test(sheet1_P35, "/NOT_EQUAL/", "P35", true).
?test(sheet1_Q35, "/NOT_EQUAL/", "Q35", true).
?test(sheet1_R35, "/NOT_EQUAL/", "R35", true).
?test(sheet1_S35, "/NOT_EQUAL/", "S35", true).
?test(sheet1_T35, "/NOT_EQUAL/", "T35", true).
?test(sheet1_U35, "/NOT_EQUAL/", "U35", true).
?test(sheet1_V35, "/NOT_EQUAL/", "V35", true).
?test(sheet1_A36, "/NOT_EQUAL/", "A36", "String").
?test(sheet1_C36, "/NOT_EQUAL/", "C36", true).
?test(sheet1_D36, "/NOT_EQUAL/", "D36", true).
?test(sheet1_E36, "/NOT_EQUAL/", "E36", true).
?test(sheet1_F36, "/NOT_EQUAL/", "F36", '#DIV/0!').
?test(sheet1_G36, "/NOT_EQUAL/", "G36", '#N/A').
?test(sheet1_H36, "/NOT_EQUAL/", "H36", '#NAME?').
?test(sheet1_I36, "/NOT_EQUAL/", "I36", 'NULL!').
?test(sheet1_J36, "/NOT_EQUAL/", "J36", '#NUM!').
?test(sheet1_K36, "/NOT_EQUAL/", "K36", '#REF!').
?test(sheet1_L36, "/NOT_EQUAL/", "L36", '#VALUE!').
?test(sheet1_M36, "/NOT_EQUAL/", "M36", true).
?test(sheet1_N36, "/NOT_EQUAL/", "N36", false).
?test(sheet1_O36, "/NOT_EQUAL/", "O36", true).
?test(sheet1_P36, "/NOT_EQUAL/", "P36", true).
?test(sheet1_Q36, "/NOT_EQUAL/", "Q36", true).
?test(sheet1_R36, "/NOT_EQUAL/", "R36", true).
?test(sheet1_S36, "/NOT_EQUAL/", "S36", true).
?test(sheet1_T36, "/NOT_EQUAL/", "T36", true).
?test(sheet1_U36, "/NOT_EQUAL/", "U36", true).
?test(sheet1_V36, "/NOT_EQUAL/", "V36", true).
?test(sheet1_A37, "/NOT_EQUAL/", "A37", "String").
?test(sheet1_C37, "/NOT_EQUAL/", "C37", true).
?test(sheet1_D37, "/NOT_EQUAL/", "D37", true).
?test(sheet1_E37, "/NOT_EQUAL/", "E37", true).
?test(sheet1_F37, "/NOT_EQUAL/", "F37", '#DIV/0!').
?test(sheet1_G37, "/NOT_EQUAL/", "G37", '#N/A').
?test(sheet1_H37, "/NOT_EQUAL/", "H37", '#NAME?').
?test(sheet1_I37, "/NOT_EQUAL/", "I37", 'NULL!').
?test(sheet1_J37, "/NOT_EQUAL/", "J37", '#NUM!').
?test(sheet1_K37, "/NOT_EQUAL/", "K37", '#REF!').
?test(sheet1_L37, "/NOT_EQUAL/", "L37", '#VALUE!').
?test(sheet1_M37, "/NOT_EQUAL/", "M37", true).
?test(sheet1_N37, "/NOT_EQUAL/", "N37", true).
?test(sheet1_O37, "/NOT_EQUAL/", "O37", false).
?test(sheet1_P37, "/NOT_EQUAL/", "P37", true).
?test(sheet1_Q37, "/NOT_EQUAL/", "Q37", true).
?test(sheet1_R37, "/NOT_EQUAL/", "R37", true).
?test(sheet1_S37, "/NOT_EQUAL/", "S37", true).
?test(sheet1_T37, "/NOT_EQUAL/", "T37", true).
?test(sheet1_U37, "/NOT_EQUAL/", "U37", true).
?test(sheet1_V37, "/NOT_EQUAL/", "V37", true).
?test(sheet1_A38, "/NOT_EQUAL/", "A38", "Str Num").
?test(sheet1_C38, "/NOT_EQUAL/", "C38", true).
?test(sheet1_D38, "/NOT_EQUAL/", "D38", true).
?test(sheet1_E38, "/NOT_EQUAL/", "E38", true).
?test(sheet1_F38, "/NOT_EQUAL/", "F38", '#DIV/0!').
?test(sheet1_G38, "/NOT_EQUAL/", "G38", '#N/A').
?test(sheet1_H38, "/NOT_EQUAL/", "H38", '#NAME?').
?test(sheet1_I38, "/NOT_EQUAL/", "I38", 'NULL!').
?test(sheet1_J38, "/NOT_EQUAL/", "J38", '#NUM!').
?test(sheet1_K38, "/NOT_EQUAL/", "K38", '#REF!').
?test(sheet1_L38, "/NOT_EQUAL/", "L38", '#VALUE!').
?test(sheet1_M38, "/NOT_EQUAL/", "M38", true).
?test(sheet1_N38, "/NOT_EQUAL/", "N38", true).
?test(sheet1_O38, "/NOT_EQUAL/", "O38", true).
?test(sheet1_P38, "/NOT_EQUAL/", "P38", false).
?test(sheet1_Q38, "/NOT_EQUAL/", "Q38", true).
?test(sheet1_R38, "/NOT_EQUAL/", "R38", true).
?test(sheet1_S38, "/NOT_EQUAL/", "S38", true).
?test(sheet1_T38, "/NOT_EQUAL/", "T38", true).
?test(sheet1_U38, "/NOT_EQUAL/", "U38", true).
?test(sheet1_V38, "/NOT_EQUAL/", "V38", true).
?test(sheet1_A39, "/NOT_EQUAL/", "A39", "Str Num").
?test(sheet1_C39, "/NOT_EQUAL/", "C39", true).
?test(sheet1_D39, "/NOT_EQUAL/", "D39", true).
?test(sheet1_E39, "/NOT_EQUAL/", "E39", true).
?test(sheet1_F39, "/NOT_EQUAL/", "F39", '#DIV/0!').
?test(sheet1_G39, "/NOT_EQUAL/", "G39", '#N/A').
?test(sheet1_H39, "/NOT_EQUAL/", "H39", '#NAME?').
?test(sheet1_I39, "/NOT_EQUAL/", "I39", 'NULL!').
?test(sheet1_J39, "/NOT_EQUAL/", "J39", '#NUM!').
?test(sheet1_K39, "/NOT_EQUAL/", "K39", '#REF!').
?test(sheet1_L39, "/NOT_EQUAL/", "L39", '#VALUE!').
?test(sheet1_M39, "/NOT_EQUAL/", "M39", true).
?test(sheet1_N39, "/NOT_EQUAL/", "N39", true).
?test(sheet1_O39, "/NOT_EQUAL/", "O39", true).
?test(sheet1_P39, "/NOT_EQUAL/", "P39", true).
?test(sheet1_Q39, "/NOT_EQUAL/", "Q39", false).
?test(sheet1_R39, "/NOT_EQUAL/", "R39", true).
?test(sheet1_S39, "/NOT_EQUAL/", "S39", true).
?test(sheet1_T39, "/NOT_EQUAL/", "T39", true).
?test(sheet1_U39, "/NOT_EQUAL/", "U39", true).
?test(sheet1_V39, "/NOT_EQUAL/", "V39", true).
?test(sheet1_A40, "/NOT_EQUAL/", "A40", "Integer").
?test(sheet1_C40, "/NOT_EQUAL/", "C40", true).
?test(sheet1_D40, "/NOT_EQUAL/", "D40", true).
?test(sheet1_E40, "/NOT_EQUAL/", "E40", true).
?test(sheet1_F40, "/NOT_EQUAL/", "F40", '#DIV/0!').
?test(sheet1_G40, "/NOT_EQUAL/", "G40", '#N/A').
?test(sheet1_H40, "/NOT_EQUAL/", "H40", '#NAME?').
?test(sheet1_I40, "/NOT_EQUAL/", "I40", 'NULL!').
?test(sheet1_J40, "/NOT_EQUAL/", "J40", '#NUM!').
?test(sheet1_K40, "/NOT_EQUAL/", "K40", '#REF!').
?test(sheet1_L40, "/NOT_EQUAL/", "L40", '#VALUE!').
?test(sheet1_M40, "/NOT_EQUAL/", "M40", true).
?test(sheet1_N40, "/NOT_EQUAL/", "N40", true).
?test(sheet1_O40, "/NOT_EQUAL/", "O40", true).
?test(sheet1_P40, "/NOT_EQUAL/", "P40", true).
?test(sheet1_Q40, "/NOT_EQUAL/", "Q40", true).
?test(sheet1_R40, "/NOT_EQUAL/", "R40", false).
?test(sheet1_S40, "/NOT_EQUAL/", "S40", true).
?test(sheet1_T40, "/NOT_EQUAL/", "T40", true).
?test(sheet1_U40, "/NOT_EQUAL/", "U40", true).
?test(sheet1_V40, "/NOT_EQUAL/", "V40", true).
?test(sheet1_A41, "/NOT_EQUAL/", "A41", "Integer").
?test(sheet1_C41, "/NOT_EQUAL/", "C41", true).
?test(sheet1_D41, "/NOT_EQUAL/", "D41", true).
?test(sheet1_E41, "/NOT_EQUAL/", "E41", true).
?test(sheet1_F41, "/NOT_EQUAL/", "F41", '#DIV/0!').
?test(sheet1_G41, "/NOT_EQUAL/", "G41", '#N/A').
?test(sheet1_H41, "/NOT_EQUAL/", "H41", '#NAME?').
?test(sheet1_I41, "/NOT_EQUAL/", "I41", 'NULL!').
?test(sheet1_J41, "/NOT_EQUAL/", "J41", '#NUM!').
?test(sheet1_K41, "/NOT_EQUAL/", "K41", '#REF!').
?test(sheet1_L41, "/NOT_EQUAL/", "L41", '#VALUE!').
?test(sheet1_M41, "/NOT_EQUAL/", "M41", true).
?test(sheet1_N41, "/NOT_EQUAL/", "N41", true).
?test(sheet1_O41, "/NOT_EQUAL/", "O41", true).
?test(sheet1_P41, "/NOT_EQUAL/", "P41", true).
?test(sheet1_Q41, "/NOT_EQUAL/", "Q41", true).
?test(sheet1_R41, "/NOT_EQUAL/", "R41", true).
?test(sheet1_S41, "/NOT_EQUAL/", "S41", false).
?test(sheet1_T41, "/NOT_EQUAL/", "T41", true).
?test(sheet1_U41, "/NOT_EQUAL/", "U41", true).
?test(sheet1_V41, "/NOT_EQUAL/", "V41", true).
?test(sheet1_A42, "/NOT_EQUAL/", "A42", "Zero").
?test(sheet1_C42, "/NOT_EQUAL/", "C42", false).
?test(sheet1_D42, "/NOT_EQUAL/", "D42", true).
?test(sheet1_E42, "/NOT_EQUAL/", "E42", true).
?test(sheet1_F42, "/NOT_EQUAL/", "F42", '#DIV/0!').
?test(sheet1_G42, "/NOT_EQUAL/", "G42", '#N/A').
?test(sheet1_H42, "/NOT_EQUAL/", "H42", '#NAME?').
?test(sheet1_I42, "/NOT_EQUAL/", "I42", 'NULL!').
?test(sheet1_J42, "/NOT_EQUAL/", "J42", '#NUM!').
?test(sheet1_K42, "/NOT_EQUAL/", "K42", '#REF!').
?test(sheet1_L42, "/NOT_EQUAL/", "L42", '#VALUE!').
?test(sheet1_M42, "/NOT_EQUAL/", "M42", true).
?test(sheet1_N42, "/NOT_EQUAL/", "N42", true).
?test(sheet1_O42, "/NOT_EQUAL/", "O42", true).
?test(sheet1_P42, "/NOT_EQUAL/", "P42", true).
?test(sheet1_Q42, "/NOT_EQUAL/", "Q42", true).
?test(sheet1_R42, "/NOT_EQUAL/", "R42", true).
?test(sheet1_S42, "/NOT_EQUAL/", "S42", true).
?test(sheet1_T42, "/NOT_EQUAL/", "T42", false).
?test(sheet1_U42, "/NOT_EQUAL/", "U42", true).
?test(sheet1_V42, "/NOT_EQUAL/", "V42", true).
?test(sheet1_A43, "/NOT_EQUAL/", "A43", "Float").
?test(sheet1_C43, "/NOT_EQUAL/", "C43", true).
?test(sheet1_D43, "/NOT_EQUAL/", "D43", true).
?test(sheet1_E43, "/NOT_EQUAL/", "E43", true).
?test(sheet1_F43, "/NOT_EQUAL/", "F43", '#DIV/0!').
?test(sheet1_G43, "/NOT_EQUAL/", "G43", '#N/A').
?test(sheet1_H43, "/NOT_EQUAL/", "H43", '#NAME?').
?test(sheet1_I43, "/NOT_EQUAL/", "I43", 'NULL!').
?test(sheet1_J43, "/NOT_EQUAL/", "J43", '#NUM!').
?test(sheet1_K43, "/NOT_EQUAL/", "K43", '#REF!').
?test(sheet1_L43, "/NOT_EQUAL/", "L43", '#VALUE!').
?test(sheet1_M43, "/NOT_EQUAL/", "M43", true).
?test(sheet1_N43, "/NOT_EQUAL/", "N43", true).
?test(sheet1_O43, "/NOT_EQUAL/", "O43", true).
?test(sheet1_P43, "/NOT_EQUAL/", "P43", true).
?test(sheet1_Q43, "/NOT_EQUAL/", "Q43", true).
?test(sheet1_R43, "/NOT_EQUAL/", "R43", true).
?test(sheet1_S43, "/NOT_EQUAL/", "S43", true).
?test(sheet1_T43, "/NOT_EQUAL/", "T43", true).
?test(sheet1_U43, "/NOT_EQUAL/", "U43", false).
?test(sheet1_V43, "/NOT_EQUAL/", "V43", true).
?test(sheet1_A44, "/NOT_EQUAL/", "A44", "Float").
?test(sheet1_C44, "/NOT_EQUAL/", "C44", true).
?test(sheet1_D44, "/NOT_EQUAL/", "D44", true).
?test(sheet1_E44, "/NOT_EQUAL/", "E44", true).
?test(sheet1_F44, "/NOT_EQUAL/", "F44", '#DIV/0!').
?test(sheet1_G44, "/NOT_EQUAL/", "G44", '#N/A').
?test(sheet1_H44, "/NOT_EQUAL/", "H44", '#NAME?').
?test(sheet1_I44, "/NOT_EQUAL/", "I44", 'NULL!').
?test(sheet1_J44, "/NOT_EQUAL/", "J44", '#NUM!').
?test(sheet1_K44, "/NOT_EQUAL/", "K44", '#REF!').
?test(sheet1_L44, "/NOT_EQUAL/", "L44", '#VALUE!').
?test(sheet1_M44, "/NOT_EQUAL/", "M44", true).
?test(sheet1_N44, "/NOT_EQUAL/", "N44", true).
?test(sheet1_O44, "/NOT_EQUAL/", "O44", true).
?test(sheet1_P44, "/NOT_EQUAL/", "P44", true).
?test(sheet1_Q44, "/NOT_EQUAL/", "Q44", true).
?test(sheet1_R44, "/NOT_EQUAL/", "R44", true).
?test(sheet1_S44, "/NOT_EQUAL/", "S44", true).
?test(sheet1_T44, "/NOT_EQUAL/", "T44", true).
?test(sheet1_U44, "/NOT_EQUAL/", "U44", true).
?test(sheet1_V44, "/NOT_EQUAL/", "V44", false).
?test(sheet1_A47, "/NOT_EQUAL/", "A47", 400.0).
?test(sheet1_C47, "/NOT_EQUAL/", "C47", 1.0).
?test(sheet1_D47, "/NOT_EQUAL/", "D47", 1.0).
?test(sheet1_E47, "/NOT_EQUAL/", "E47", 1.0).
?test(sheet1_F47, "/NOT_EQUAL/", "F47", 1.0).
?test(sheet1_G47, "/NOT_EQUAL/", "G47", 1.0).
?test(sheet1_H47, "/NOT_EQUAL/", "H47", 1.0).
?test(sheet1_I47, "/NOT_EQUAL/", "I47", 1.0).
?test(sheet1_J47, "/NOT_EQUAL/", "J47", 1.0).
?test(sheet1_K47, "/NOT_EQUAL/", "K47", 1.0).
?test(sheet1_L47, "/NOT_EQUAL/", "L47", 1.0).
?test(sheet1_M47, "/NOT_EQUAL/", "M47", 1.0).
?test(sheet1_N47, "/NOT_EQUAL/", "N47", 1.0).
?test(sheet1_O47, "/NOT_EQUAL/", "O47", 1.0).
?test(sheet1_P47, "/NOT_EQUAL/", "P47", 1.0).
?test(sheet1_Q47, "/NOT_EQUAL/", "Q47", 1.0).
?test(sheet1_R47, "/NOT_EQUAL/", "R47", 1.0).
?test(sheet1_S47, "/NOT_EQUAL/", "S47", 1.0).
?test(sheet1_T47, "/NOT_EQUAL/", "T47", 1.0).
?test(sheet1_U47, "/NOT_EQUAL/", "U47", 1.0).
?test(sheet1_V47, "/NOT_EQUAL/", "V47", 1.0).
?test(sheet1_A48, "/NOT_EQUAL/", "A48", "Success").
?test(sheet1_C48, "/NOT_EQUAL/", "C48", 1.0).
?test(sheet1_D48, "/NOT_EQUAL/", "D48", 1.0).
?test(sheet1_E48, "/NOT_EQUAL/", "E48", 1.0).
?test(sheet1_F48, "/NOT_EQUAL/", "F48", 1.0).
?test(sheet1_G48, "/NOT_EQUAL/", "G48", 1.0).
?test(sheet1_H48, "/NOT_EQUAL/", "H48", 1.0).
?test(sheet1_I48, "/NOT_EQUAL/", "I48", 1.0).
?test(sheet1_J48, "/NOT_EQUAL/", "J48", 1.0).
?test(sheet1_K48, "/NOT_EQUAL/", "K48", 1.0).
?test(sheet1_L48, "/NOT_EQUAL/", "L48", 1.0).
?test(sheet1_M48, "/NOT_EQUAL/", "M48", 1.0).
?test(sheet1_N48, "/NOT_EQUAL/", "N48", 1.0).
?test(sheet1_O48, "/NOT_EQUAL/", "O48", 1.0).
?test(sheet1_P48, "/NOT_EQUAL/", "P48", 1.0).
?test(sheet1_Q48, "/NOT_EQUAL/", "Q48", 1.0).
?test(sheet1_R48, "/NOT_EQUAL/", "R48", 1.0).
?test(sheet1_S48, "/NOT_EQUAL/", "S48", 1.0).
?test(sheet1_T48, "/NOT_EQUAL/", "T48", 1.0).
?test(sheet1_U48, "/NOT_EQUAL/", "U48", 1.0).
?test(sheet1_V48, "/NOT_EQUAL/", "V48", 1.0).
?test(sheet1_C49, "/NOT_EQUAL/", "C49", 1.0).
?test(sheet1_D49, "/NOT_EQUAL/", "D49", 1.0).
?test(sheet1_E49, "/NOT_EQUAL/", "E49", 1.0).
?test(sheet1_F49, "/NOT_EQUAL/", "F49", 1.0).
?test(sheet1_G49, "/NOT_EQUAL/", "G49", 1.0).
?test(sheet1_H49, "/NOT_EQUAL/", "H49", 1.0).
?test(sheet1_I49, "/NOT_EQUAL/", "I49", 1.0).
?test(sheet1_J49, "/NOT_EQUAL/", "J49", 1.0).
?test(sheet1_K49, "/NOT_EQUAL/", "K49", 1.0).
?test(sheet1_L49, "/NOT_EQUAL/", "L49", 1.0).
?test(sheet1_M49, "/NOT_EQUAL/", "M49", 1.0).
?test(sheet1_N49, "/NOT_EQUAL/", "N49", 1.0).
?test(sheet1_O49, "/NOT_EQUAL/", "O49", 1.0).
?test(sheet1_P49, "/NOT_EQUAL/", "P49", 1.0).
?test(sheet1_Q49, "/NOT_EQUAL/", "Q49", 1.0).
?test(sheet1_R49, "/NOT_EQUAL/", "R49", 1.0).
?test(sheet1_S49, "/NOT_EQUAL/", "S49", 1.0).
?test(sheet1_T49, "/NOT_EQUAL/", "T49", 1.0).
?test(sheet1_U49, "/NOT_EQUAL/", "U49", 1.0).
?test(sheet1_V49, "/NOT_EQUAL/", "V49", 1.0).
?test(sheet1_C50, "/NOT_EQUAL/", "C50", 1.0).
?test(sheet1_D50, "/NOT_EQUAL/", "D50", 1.0).
?test(sheet1_E50, "/NOT_EQUAL/", "E50", 1.0).
?test(sheet1_F50, "/NOT_EQUAL/", "F50", 1.0).
?test(sheet1_G50, "/NOT_EQUAL/", "G50", 1.0).
?test(sheet1_H50, "/NOT_EQUAL/", "H50", 1.0).
?test(sheet1_I50, "/NOT_EQUAL/", "I50", 1.0).
?test(sheet1_J50, "/NOT_EQUAL/", "J50", 1.0).
?test(sheet1_K50, "/NOT_EQUAL/", "K50", 1.0).
?test(sheet1_L50, "/NOT_EQUAL/", "L50", 1.0).
?test(sheet1_M50, "/NOT_EQUAL/", "M50", 1.0).
?test(sheet1_N50, "/NOT_EQUAL/", "N50", 1.0).
?test(sheet1_O50, "/NOT_EQUAL/", "O50", 1.0).
?test(sheet1_P50, "/NOT_EQUAL/", "P50", 1.0).
?test(sheet1_Q50, "/NOT_EQUAL/", "Q50", 1.0).
?test(sheet1_R50, "/NOT_EQUAL/", "R50", 1.0).
?test(sheet1_S50, "/NOT_EQUAL/", "S50", 1.0).
?test(sheet1_T50, "/NOT_EQUAL/", "T50", 1.0).
?test(sheet1_U50, "/NOT_EQUAL/", "U50", 1.0).
?test(sheet1_V50, "/NOT_EQUAL/", "V50", 1.0).
?test(sheet1_C51, "/NOT_EQUAL/", "C51", 1.0).
?test(sheet1_D51, "/NOT_EQUAL/", "D51", 1.0).
?test(sheet1_E51, "/NOT_EQUAL/", "E51", 1.0).
?test(sheet1_F51, "/NOT_EQUAL/", "F51", 1.0).
?test(sheet1_G51, "/NOT_EQUAL/", "G51", 1.0).
?test(sheet1_H51, "/NOT_EQUAL/", "H51", 1.0).
?test(sheet1_I51, "/NOT_EQUAL/", "I51", 1.0).
?test(sheet1_J51, "/NOT_EQUAL/", "J51", 1.0).
?test(sheet1_K51, "/NOT_EQUAL/", "K51", 1.0).
?test(sheet1_L51, "/NOT_EQUAL/", "L51", 1.0).
?test(sheet1_M51, "/NOT_EQUAL/", "M51", 1.0).
?test(sheet1_N51, "/NOT_EQUAL/", "N51", 1.0).
?test(sheet1_O51, "/NOT_EQUAL/", "O51", 1.0).
?test(sheet1_P51, "/NOT_EQUAL/", "P51", 1.0).
?test(sheet1_Q51, "/NOT_EQUAL/", "Q51", 1.0).
?test(sheet1_R51, "/NOT_EQUAL/", "R51", 1.0).
?test(sheet1_S51, "/NOT_EQUAL/", "S51", 1.0).
?test(sheet1_T51, "/NOT_EQUAL/", "T51", 1.0).
?test(sheet1_U51, "/NOT_EQUAL/", "U51", 1.0).
?test(sheet1_V51, "/NOT_EQUAL/", "V51", 1.0).
?test(sheet1_C52, "/NOT_EQUAL/", "C52", 1.0).
?test(sheet1_D52, "/NOT_EQUAL/", "D52", 1.0).
?test(sheet1_E52, "/NOT_EQUAL/", "E52", 1.0).
?test(sheet1_F52, "/NOT_EQUAL/", "F52", 1.0).
?test(sheet1_G52, "/NOT_EQUAL/", "G52", 1.0).
?test(sheet1_H52, "/NOT_EQUAL/", "H52", 1.0).
?test(sheet1_I52, "/NOT_EQUAL/", "I52", 1.0).
?test(sheet1_J52, "/NOT_EQUAL/", "J52", 1.0).
?test(sheet1_K52, "/NOT_EQUAL/", "K52", 1.0).
?test(sheet1_L52, "/NOT_EQUAL/", "L52", 1.0).
?test(sheet1_M52, "/NOT_EQUAL/", "M52", 1.0).
?test(sheet1_N52, "/NOT_EQUAL/", "N52", 1.0).
?test(sheet1_O52, "/NOT_EQUAL/", "O52", 1.0).
?test(sheet1_P52, "/NOT_EQUAL/", "P52", 1.0).
?test(sheet1_Q52, "/NOT_EQUAL/", "Q52", 1.0).
?test(sheet1_R52, "/NOT_EQUAL/", "R52", 1.0).
?test(sheet1_S52, "/NOT_EQUAL/", "S52", 1.0).
?test(sheet1_T52, "/NOT_EQUAL/", "T52", 1.0).
?test(sheet1_U52, "/NOT_EQUAL/", "U52", 1.0).
?test(sheet1_V52, "/NOT_EQUAL/", "V52", 1.0).
?test(sheet1_C53, "/NOT_EQUAL/", "C53", 1.0).
?test(sheet1_D53, "/NOT_EQUAL/", "D53", 1.0).
?test(sheet1_E53, "/NOT_EQUAL/", "E53", 1.0).
?test(sheet1_F53, "/NOT_EQUAL/", "F53", 1.0).
?test(sheet1_G53, "/NOT_EQUAL/", "G53", 1.0).
?test(sheet1_H53, "/NOT_EQUAL/", "H53", 1.0).
?test(sheet1_I53, "/NOT_EQUAL/", "I53", 1.0).
?test(sheet1_J53, "/NOT_EQUAL/", "J53", 1.0).
?test(sheet1_K53, "/NOT_EQUAL/", "K53", 1.0).
?test(sheet1_L53, "/NOT_EQUAL/", "L53", 1.0).
?test(sheet1_M53, "/NOT_EQUAL/", "M53", 1.0).
?test(sheet1_N53, "/NOT_EQUAL/", "N53", 1.0).
?test(sheet1_O53, "/NOT_EQUAL/", "O53", 1.0).
?test(sheet1_P53, "/NOT_EQUAL/", "P53", 1.0).
?test(sheet1_Q53, "/NOT_EQUAL/", "Q53", 1.0).
?test(sheet1_R53, "/NOT_EQUAL/", "R53", 1.0).
?test(sheet1_S53, "/NOT_EQUAL/", "S53", 1.0).
?test(sheet1_T53, "/NOT_EQUAL/", "T53", 1.0).
?test(sheet1_U53, "/NOT_EQUAL/", "U53", 1.0).
?test(sheet1_V53, "/NOT_EQUAL/", "V53", 1.0).
?test(sheet1_C54, "/NOT_EQUAL/", "C54", 1.0).
?test(sheet1_D54, "/NOT_EQUAL/", "D54", 1.0).
?test(sheet1_E54, "/NOT_EQUAL/", "E54", 1.0).
?test(sheet1_F54, "/NOT_EQUAL/", "F54", 1.0).
?test(sheet1_G54, "/NOT_EQUAL/", "G54", 1.0).
?test(sheet1_H54, "/NOT_EQUAL/", "H54", 1.0).
?test(sheet1_I54, "/NOT_EQUAL/", "I54", 1.0).
?test(sheet1_J54, "/NOT_EQUAL/", "J54", 1.0).
?test(sheet1_K54, "/NOT_EQUAL/", "K54", 1.0).
?test(sheet1_L54, "/NOT_EQUAL/", "L54", 1.0).
?test(sheet1_M54, "/NOT_EQUAL/", "M54", 1.0).
?test(sheet1_N54, "/NOT_EQUAL/", "N54", 1.0).
?test(sheet1_O54, "/NOT_EQUAL/", "O54", 1.0).
?test(sheet1_P54, "/NOT_EQUAL/", "P54", 1.0).
?test(sheet1_Q54, "/NOT_EQUAL/", "Q54", 1.0).
?test(sheet1_R54, "/NOT_EQUAL/", "R54", 1.0).
?test(sheet1_S54, "/NOT_EQUAL/", "S54", 1.0).
?test(sheet1_T54, "/NOT_EQUAL/", "T54", 1.0).
?test(sheet1_U54, "/NOT_EQUAL/", "U54", 1.0).
?test(sheet1_V54, "/NOT_EQUAL/", "V54", 1.0).
?test(sheet1_C55, "/NOT_EQUAL/", "C55", 1.0).
?test(sheet1_D55, "/NOT_EQUAL/", "D55", 1.0).
?test(sheet1_E55, "/NOT_EQUAL/", "E55", 1.0).
?test(sheet1_F55, "/NOT_EQUAL/", "F55", 1.0).
?test(sheet1_G55, "/NOT_EQUAL/", "G55", 1.0).
?test(sheet1_H55, "/NOT_EQUAL/", "H55", 1.0).
?test(sheet1_I55, "/NOT_EQUAL/", "I55", 1.0).
?test(sheet1_J55, "/NOT_EQUAL/", "J55", 1.0).
?test(sheet1_K55, "/NOT_EQUAL/", "K55", 1.0).
?test(sheet1_L55, "/NOT_EQUAL/", "L55", 1.0).
?test(sheet1_M55, "/NOT_EQUAL/", "M55", 1.0).
?test(sheet1_N55, "/NOT_EQUAL/", "N55", 1.0).
?test(sheet1_O55, "/NOT_EQUAL/", "O55", 1.0).
?test(sheet1_P55, "/NOT_EQUAL/", "P55", 1.0).
?test(sheet1_Q55, "/NOT_EQUAL/", "Q55", 1.0).
?test(sheet1_R55, "/NOT_EQUAL/", "R55", 1.0).
?test(sheet1_S55, "/NOT_EQUAL/", "S55", 1.0).
?test(sheet1_T55, "/NOT_EQUAL/", "T55", 1.0).
?test(sheet1_U55, "/NOT_EQUAL/", "U55", 1.0).
?test(sheet1_V55, "/NOT_EQUAL/", "V55", 1.0).
?test(sheet1_C56, "/NOT_EQUAL/", "C56", 1.0).
?test(sheet1_D56, "/NOT_EQUAL/", "D56", 1.0).
?test(sheet1_E56, "/NOT_EQUAL/", "E56", 1.0).
?test(sheet1_F56, "/NOT_EQUAL/", "F56", 1.0).
?test(sheet1_G56, "/NOT_EQUAL/", "G56", 1.0).
?test(sheet1_H56, "/NOT_EQUAL/", "H56", 1.0).
?test(sheet1_I56, "/NOT_EQUAL/", "I56", 1.0).
?test(sheet1_J56, "/NOT_EQUAL/", "J56", 1.0).
?test(sheet1_K56, "/NOT_EQUAL/", "K56", 1.0).
?test(sheet1_L56, "/NOT_EQUAL/", "L56", 1.0).
?test(sheet1_M56, "/NOT_EQUAL/", "M56", 1.0).
?test(sheet1_N56, "/NOT_EQUAL/", "N56", 1.0).
?test(sheet1_O56, "/NOT_EQUAL/", "O56", 1.0).
?test(sheet1_P56, "/NOT_EQUAL/", "P56", 1.0).
?test(sheet1_Q56, "/NOT_EQUAL/", "Q56", 1.0).
?test(sheet1_R56, "/NOT_EQUAL/", "R56", 1.0).
?test(sheet1_S56, "/NOT_EQUAL/", "S56", 1.0).
?test(sheet1_T56, "/NOT_EQUAL/", "T56", 1.0).
?test(sheet1_U56, "/NOT_EQUAL/", "U56", 1.0).
?test(sheet1_V56, "/NOT_EQUAL/", "V56", 1.0).
?test(sheet1_C57, "/NOT_EQUAL/", "C57", 1.0).
?test(sheet1_D57, "/NOT_EQUAL/", "D57", 1.0).
?test(sheet1_E57, "/NOT_EQUAL/", "E57", 1.0).
?test(sheet1_F57, "/NOT_EQUAL/", "F57", 1.0).
?test(sheet1_G57, "/NOT_EQUAL/", "G57", 1.0).
?test(sheet1_H57, "/NOT_EQUAL/", "H57", 1.0).
?test(sheet1_I57, "/NOT_EQUAL/", "I57", 1.0).
?test(sheet1_J57, "/NOT_EQUAL/", "J57", 1.0).
?test(sheet1_K57, "/NOT_EQUAL/", "K57", 1.0).
?test(sheet1_L57, "/NOT_EQUAL/", "L57", 1.0).
?test(sheet1_M57, "/NOT_EQUAL/", "M57", 1.0).
?test(sheet1_N57, "/NOT_EQUAL/", "N57", 1.0).
?test(sheet1_O57, "/NOT_EQUAL/", "O57", 1.0).
?test(sheet1_P57, "/NOT_EQUAL/", "P57", 1.0).
?test(sheet1_Q57, "/NOT_EQUAL/", "Q57", 1.0).
?test(sheet1_R57, "/NOT_EQUAL/", "R57", 1.0).
?test(sheet1_S57, "/NOT_EQUAL/", "S57", 1.0).
?test(sheet1_T57, "/NOT_EQUAL/", "T57", 1.0).
?test(sheet1_U57, "/NOT_EQUAL/", "U57", 1.0).
?test(sheet1_V57, "/NOT_EQUAL/", "V57", 1.0).
?test(sheet1_C58, "/NOT_EQUAL/", "C58", 1.0).
?test(sheet1_D58, "/NOT_EQUAL/", "D58", 1.0).
?test(sheet1_E58, "/NOT_EQUAL/", "E58", 1.0).
?test(sheet1_F58, "/NOT_EQUAL/", "F58", 1.0).
?test(sheet1_G58, "/NOT_EQUAL/", "G58", 1.0).
?test(sheet1_H58, "/NOT_EQUAL/", "H58", 1.0).
?test(sheet1_I58, "/NOT_EQUAL/", "I58", 1.0).
?test(sheet1_J58, "/NOT_EQUAL/", "J58", 1.0).
?test(sheet1_K58, "/NOT_EQUAL/", "K58", 1.0).
?test(sheet1_L58, "/NOT_EQUAL/", "L58", 1.0).
?test(sheet1_M58, "/NOT_EQUAL/", "M58", 1.0).
?test(sheet1_N58, "/NOT_EQUAL/", "N58", 1.0).
?test(sheet1_O58, "/NOT_EQUAL/", "O58", 1.0).
?test(sheet1_P58, "/NOT_EQUAL/", "P58", 1.0).
?test(sheet1_Q58, "/NOT_EQUAL/", "Q58", 1.0).
?test(sheet1_R58, "/NOT_EQUAL/", "R58", 1.0).
?test(sheet1_S58, "/NOT_EQUAL/", "S58", 1.0).
?test(sheet1_T58, "/NOT_EQUAL/", "T58", 1.0).
?test(sheet1_U58, "/NOT_EQUAL/", "U58", 1.0).
?test(sheet1_V58, "/NOT_EQUAL/", "V58", 1.0).
?test(sheet1_C59, "/NOT_EQUAL/", "C59", 1.0).
?test(sheet1_D59, "/NOT_EQUAL/", "D59", 1.0).
?test(sheet1_E59, "/NOT_EQUAL/", "E59", 1.0).
?test(sheet1_F59, "/NOT_EQUAL/", "F59", 1.0).
?test(sheet1_G59, "/NOT_EQUAL/", "G59", 1.0).
?test(sheet1_H59, "/NOT_EQUAL/", "H59", 1.0).
?test(sheet1_I59, "/NOT_EQUAL/", "I59", 1.0).
?test(sheet1_J59, "/NOT_EQUAL/", "J59", 1.0).
?test(sheet1_K59, "/NOT_EQUAL/", "K59", 1.0).
?test(sheet1_L59, "/NOT_EQUAL/", "L59", 1.0).
?test(sheet1_M59, "/NOT_EQUAL/", "M59", 1.0).
?test(sheet1_N59, "/NOT_EQUAL/", "N59", 1.0).
?test(sheet1_O59, "/NOT_EQUAL/", "O59", 1.0).
?test(sheet1_P59, "/NOT_EQUAL/", "P59", 1.0).
?test(sheet1_Q59, "/NOT_EQUAL/", "Q59", 1.0).
?test(sheet1_R59, "/NOT_EQUAL/", "R59", 1.0).
?test(sheet1_S59, "/NOT_EQUAL/", "S59", 1.0).
?test(sheet1_T59, "/NOT_EQUAL/", "T59", 1.0).
?test(sheet1_U59, "/NOT_EQUAL/", "U59", 1.0).
?test(sheet1_V59, "/NOT_EQUAL/", "V59", 1.0).
?test(sheet1_C60, "/NOT_EQUAL/", "C60", 1.0).
?test(sheet1_D60, "/NOT_EQUAL/", "D60", 1.0).
?test(sheet1_E60, "/NOT_EQUAL/", "E60", 1.0).
?test(sheet1_F60, "/NOT_EQUAL/", "F60", 1.0).
?test(sheet1_G60, "/NOT_EQUAL/", "G60", 1.0).
?test(sheet1_H60, "/NOT_EQUAL/", "H60", 1.0).
?test(sheet1_I60, "/NOT_EQUAL/", "I60", 1.0).
?test(sheet1_J60, "/NOT_EQUAL/", "J60", 1.0).
?test(sheet1_K60, "/NOT_EQUAL/", "K60", 1.0).
?test(sheet1_L60, "/NOT_EQUAL/", "L60", 1.0).
?test(sheet1_M60, "/NOT_EQUAL/", "M60", 1.0).
?test(sheet1_N60, "/NOT_EQUAL/", "N60", 1.0).
?test(sheet1_O60, "/NOT_EQUAL/", "O60", 1.0).
?test(sheet1_P60, "/NOT_EQUAL/", "P60", 1.0).
?test(sheet1_Q60, "/NOT_EQUAL/", "Q60", 1.0).
?test(sheet1_R60, "/NOT_EQUAL/", "R60", 1.0).
?test(sheet1_S60, "/NOT_EQUAL/", "S60", 1.0).
?test(sheet1_T60, "/NOT_EQUAL/", "T60", 1.0).
?test(sheet1_U60, "/NOT_EQUAL/", "U60", 1.0).
?test(sheet1_V60, "/NOT_EQUAL/", "V60", 1.0).
?test(sheet1_C61, "/NOT_EQUAL/", "C61", 1.0).
?test(sheet1_D61, "/NOT_EQUAL/", "D61", 1.0).
?test(sheet1_E61, "/NOT_EQUAL/", "E61", 1.0).
?test(sheet1_F61, "/NOT_EQUAL/", "F61", 1.0).
?test(sheet1_G61, "/NOT_EQUAL/", "G61", 1.0).
?test(sheet1_H61, "/NOT_EQUAL/", "H61", 1.0).
?test(sheet1_I61, "/NOT_EQUAL/", "I61", 1.0).
?test(sheet1_J61, "/NOT_EQUAL/", "J61", 1.0).
?test(sheet1_K61, "/NOT_EQUAL/", "K61", 1.0).
?test(sheet1_L61, "/NOT_EQUAL/", "L61", 1.0).
?test(sheet1_M61, "/NOT_EQUAL/", "M61", 1.0).
?test(sheet1_N61, "/NOT_EQUAL/", "N61", 1.0).
?test(sheet1_O61, "/NOT_EQUAL/", "O61", 1.0).
?test(sheet1_P61, "/NOT_EQUAL/", "P61", 1.0).
?test(sheet1_Q61, "/NOT_EQUAL/", "Q61", 1.0).
?test(sheet1_R61, "/NOT_EQUAL/", "R61", 1.0).
?test(sheet1_S61, "/NOT_EQUAL/", "S61", 1.0).
?test(sheet1_T61, "/NOT_EQUAL/", "T61", 1.0).
?test(sheet1_U61, "/NOT_EQUAL/", "U61", 1.0).
?test(sheet1_V61, "/NOT_EQUAL/", "V61", 1.0).
?test(sheet1_C62, "/NOT_EQUAL/", "C62", 1.0).
?test(sheet1_D62, "/NOT_EQUAL/", "D62", 1.0).
?test(sheet1_E62, "/NOT_EQUAL/", "E62", 1.0).
?test(sheet1_F62, "/NOT_EQUAL/", "F62", 1.0).
?test(sheet1_G62, "/NOT_EQUAL/", "G62", 1.0).
?test(sheet1_H62, "/NOT_EQUAL/", "H62", 1.0).
?test(sheet1_I62, "/NOT_EQUAL/", "I62", 1.0).
?test(sheet1_J62, "/NOT_EQUAL/", "J62", 1.0).
?test(sheet1_K62, "/NOT_EQUAL/", "K62", 1.0).
?test(sheet1_L62, "/NOT_EQUAL/", "L62", 1.0).
?test(sheet1_M62, "/NOT_EQUAL/", "M62", 1.0).
?test(sheet1_N62, "/NOT_EQUAL/", "N62", 1.0).
?test(sheet1_O62, "/NOT_EQUAL/", "O62", 1.0).
?test(sheet1_P62, "/NOT_EQUAL/", "P62", 1.0).
?test(sheet1_Q62, "/NOT_EQUAL/", "Q62", 1.0).
?test(sheet1_R62, "/NOT_EQUAL/", "R62", 1.0).
?test(sheet1_S62, "/NOT_EQUAL/", "S62", 1.0).
?test(sheet1_T62, "/NOT_EQUAL/", "T62", 1.0).
?test(sheet1_U62, "/NOT_EQUAL/", "U62", 1.0).
?test(sheet1_V62, "/NOT_EQUAL/", "V62", 1.0).
?test(sheet1_C63, "/NOT_EQUAL/", "C63", 1.0).
?test(sheet1_D63, "/NOT_EQUAL/", "D63", 1.0).
?test(sheet1_E63, "/NOT_EQUAL/", "E63", 1.0).
?test(sheet1_F63, "/NOT_EQUAL/", "F63", 1.0).
?test(sheet1_G63, "/NOT_EQUAL/", "G63", 1.0).
?test(sheet1_H63, "/NOT_EQUAL/", "H63", 1.0).
?test(sheet1_I63, "/NOT_EQUAL/", "I63", 1.0).
?test(sheet1_J63, "/NOT_EQUAL/", "J63", 1.0).
?test(sheet1_K63, "/NOT_EQUAL/", "K63", 1.0).
?test(sheet1_L63, "/NOT_EQUAL/", "L63", 1.0).
?test(sheet1_M63, "/NOT_EQUAL/", "M63", 1.0).
?test(sheet1_N63, "/NOT_EQUAL/", "N63", 1.0).
?test(sheet1_O63, "/NOT_EQUAL/", "O63", 1.0).
?test(sheet1_P63, "/NOT_EQUAL/", "P63", 1.0).
?test(sheet1_Q63, "/NOT_EQUAL/", "Q63", 1.0).
?test(sheet1_R63, "/NOT_EQUAL/", "R63", 1.0).
?test(sheet1_S63, "/NOT_EQUAL/", "S63", 1.0).
?test(sheet1_T63, "/NOT_EQUAL/", "T63", 1.0).
?test(sheet1_U63, "/NOT_EQUAL/", "U63", 1.0).
?test(sheet1_V63, "/NOT_EQUAL/", "V63", 1.0).
?test(sheet1_C64, "/NOT_EQUAL/", "C64", 1.0).
?test(sheet1_D64, "/NOT_EQUAL/", "D64", 1.0).
?test(sheet1_E64, "/NOT_EQUAL/", "E64", 1.0).
?test(sheet1_F64, "/NOT_EQUAL/", "F64", 1.0).
?test(sheet1_G64, "/NOT_EQUAL/", "G64", 1.0).
?test(sheet1_H64, "/NOT_EQUAL/", "H64", 1.0).
?test(sheet1_I64, "/NOT_EQUAL/", "I64", 1.0).
?test(sheet1_J64, "/NOT_EQUAL/", "J64", 1.0).
?test(sheet1_K64, "/NOT_EQUAL/", "K64", 1.0).
?test(sheet1_L64, "/NOT_EQUAL/", "L64", 1.0).
?test(sheet1_M64, "/NOT_EQUAL/", "M64", 1.0).
?test(sheet1_N64, "/NOT_EQUAL/", "N64", 1.0).
?test(sheet1_O64, "/NOT_EQUAL/", "O64", 1.0).
?test(sheet1_P64, "/NOT_EQUAL/", "P64", 1.0).
?test(sheet1_Q64, "/NOT_EQUAL/", "Q64", 1.0).
?test(sheet1_R64, "/NOT_EQUAL/", "R64", 1.0).
?test(sheet1_S64, "/NOT_EQUAL/", "S64", 1.0).
?test(sheet1_T64, "/NOT_EQUAL/", "T64", 1.0).
?test(sheet1_U64, "/NOT_EQUAL/", "U64", 1.0).
?test(sheet1_V64, "/NOT_EQUAL/", "V64", 1.0).
?test(sheet1_C65, "/NOT_EQUAL/", "C65", 1.0).
?test(sheet1_D65, "/NOT_EQUAL/", "D65", 1.0).
?test(sheet1_E65, "/NOT_EQUAL/", "E65", 1.0).
?test(sheet1_F65, "/NOT_EQUAL/", "F65", 1.0).
?test(sheet1_G65, "/NOT_EQUAL/", "G65", 1.0).
?test(sheet1_H65, "/NOT_EQUAL/", "H65", 1.0).
?test(sheet1_I65, "/NOT_EQUAL/", "I65", 1.0).
?test(sheet1_J65, "/NOT_EQUAL/", "J65", 1.0).
?test(sheet1_K65, "/NOT_EQUAL/", "K65", 1.0).
?test(sheet1_L65, "/NOT_EQUAL/", "L65", 1.0).
?test(sheet1_M65, "/NOT_EQUAL/", "M65", 1.0).
?test(sheet1_N65, "/NOT_EQUAL/", "N65", 1.0).
?test(sheet1_O65, "/NOT_EQUAL/", "O65", 1.0).
?test(sheet1_P65, "/NOT_EQUAL/", "P65", 1.0).
?test(sheet1_Q65, "/NOT_EQUAL/", "Q65", 1.0).
?test(sheet1_R65, "/NOT_EQUAL/", "R65", 1.0).
?test(sheet1_S65, "/NOT_EQUAL/", "S65", 1.0).
?test(sheet1_T65, "/NOT_EQUAL/", "T65", 1.0).
?test(sheet1_U65, "/NOT_EQUAL/", "U65", 1.0).
?test(sheet1_V65, "/NOT_EQUAL/", "V65", 1.0).
?test(sheet1_C66, "/NOT_EQUAL/", "C66", 1.0).
?test(sheet1_D66, "/NOT_EQUAL/", "D66", 1.0).
?test(sheet1_E66, "/NOT_EQUAL/", "E66", 1.0).
?test(sheet1_F66, "/NOT_EQUAL/", "F66", 1.0).
?test(sheet1_G66, "/NOT_EQUAL/", "G66", 1.0).
?test(sheet1_H66, "/NOT_EQUAL/", "H66", 1.0).
?test(sheet1_I66, "/NOT_EQUAL/", "I66", 1.0).
?test(sheet1_J66, "/NOT_EQUAL/", "J66", 1.0).
?test(sheet1_K66, "/NOT_EQUAL/", "K66", 1.0).
?test(sheet1_L66, "/NOT_EQUAL/", "L66", 1.0).
?test(sheet1_M66, "/NOT_EQUAL/", "M66", 1.0).
?test(sheet1_N66, "/NOT_EQUAL/", "N66", 1.0).
?test(sheet1_O66, "/NOT_EQUAL/", "O66", 1.0).
?test(sheet1_P66, "/NOT_EQUAL/", "P66", 1.0).
?test(sheet1_Q66, "/NOT_EQUAL/", "Q66", 1.0).
?test(sheet1_R66, "/NOT_EQUAL/", "R66", 1.0).
?test(sheet1_S66, "/NOT_EQUAL/", "S66", 1.0).
?test(sheet1_T66, "/NOT_EQUAL/", "T66", 1.0).
?test(sheet1_U66, "/NOT_EQUAL/", "U66", 1.0).
?test(sheet1_V66, "/NOT_EQUAL/", "V66", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_operators_ne.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_operators_ne" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_N2,
        sheet1_O2,
        sheet1_P2,
        sheet1_Q2,
        sheet1_R2,
        sheet1_S2,
        sheet1_T2,
        sheet1_U2,
        sheet1_V2,
        sheet1_A3,
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
        sheet1_B14,
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
        sheet1_N26,
        sheet1_O26,
        sheet1_P26,
        sheet1_Q26,
        sheet1_R26,
        sheet1_S26,
        sheet1_T26,
        sheet1_U26,
        sheet1_V26,
        sheet1_A27,
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
        sheet1_A47,
        sheet1_C47,
        sheet1_D47,
        sheet1_E47,
        sheet1_F47,
        sheet1_G47,
        sheet1_H47,
        sheet1_I47,
        sheet1_J47,
        sheet1_K47,
        sheet1_L47,
        sheet1_M47,
        sheet1_N47,
        sheet1_O47,
        sheet1_P47,
        sheet1_Q47,
        sheet1_R47,
        sheet1_S47,
        sheet1_T47,
        sheet1_U47,
        sheet1_V47,
        sheet1_A48,
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
        sheet1_M48,
        sheet1_N48,
        sheet1_O48,
        sheet1_P48,
        sheet1_Q48,
        sheet1_R48,
        sheet1_S48,
        sheet1_T48,
        sheet1_U48,
        sheet1_V48,
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
        sheet1_V64,
        sheet1_C65,
        sheet1_D65,
        sheet1_E65,
        sheet1_F65,
        sheet1_G65,
        sheet1_H65,
        sheet1_I65,
        sheet1_J65,
        sheet1_K65,
        sheet1_L65,
        sheet1_M65,
        sheet1_N65,
        sheet1_O65,
        sheet1_P65,
        sheet1_Q65,
        sheet1_R65,
        sheet1_S65,
        sheet1_T65,
        sheet1_U65,
        sheet1_V65,
        sheet1_C66,
        sheet1_D66,
        sheet1_E66,
        sheet1_F66,
        sheet1_G66,
        sheet1_H66,
        sheet1_I66,
        sheet1_J66,
        sheet1_K66,
        sheet1_L66,
        sheet1_M66,
        sheet1_N66,
        sheet1_O66,
        sheet1_P66,
        sheet1_Q66,
        sheet1_R66,
        sheet1_S66,
        sheet1_T66,
        sheet1_U66,
        sheet1_V66
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
