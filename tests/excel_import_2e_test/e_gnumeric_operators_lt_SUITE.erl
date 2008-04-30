%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_operators_lt_SUITE).
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
                     [Testcase, "e_gnumeric_operators_lt_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_operators_lt" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/LT/", "A1", "<").
?test(sheet1_B1, "/LT/", "B1", "B").
?test(sheet1_C1, "/LT/", "C1", "Blank").
?test(sheet1_D1, "/LT/", "D1", "Boolean").
?test(sheet1_E1, "/LT/", "E1", "Boolean").
?test(sheet1_F1, "/LT/", "F1", "Error").
?test(sheet1_G1, "/LT/", "G1", "Error").
?test(sheet1_H1, "/LT/", "H1", "Error").
?test(sheet1_I1, "/LT/", "I1", "Error").
?test(sheet1_J1, "/LT/", "J1", "Error").
?test(sheet1_K1, "/LT/", "K1", "Error").
?test(sheet1_L1, "/LT/", "L1", "Error").
?test(sheet1_M1, "/LT/", "M1", "String").
?test(sheet1_N1, "/LT/", "N1", "String").
?test(sheet1_O1, "/LT/", "O1", "String").
?test(sheet1_P1, "/LT/", "P1", "Str Num").
?test(sheet1_Q1, "/LT/", "Q1", "Str Num").
?test(sheet1_R1, "/LT/", "R1", "Integer").
?test(sheet1_S1, "/LT/", "S1", "Integer").
?test(sheet1_T1, "/LT/", "T1", "Zero").
?test(sheet1_U1, "/LT/", "U1", "Float").
?test(sheet1_V1, "/LT/", "V1", "Float").
?test(sheet1_A2, "/LT/", "A2", "A").
?test(sheet1_D2, "/LT/", "D2", true).
?test(sheet1_E2, "/LT/", "E2", false).
?test(sheet1_F2, "/LT/", "F2", '#DIV/0!').
?test(sheet1_G2, "/LT/", "G2", '#N/A').
?test(sheet1_H2, "/LT/", "H2", '#NAME?').
?test(sheet1_I2, "/LT/", "I2", 'NULL!').
?test(sheet1_J2, "/LT/", "J2", '#NUM!').
?test(sheet1_K2, "/LT/", "K2", '#REF!').
?test(sheet1_L2, "/LT/", "L2", '#VALUE!').
?test(sheet1_M2, "/LT/", "M2", "Liz").
?test(sheet1_N2, "/LT/", "N2", "Doug").
?test(sheet1_O2, "/LT/", "O2", "Bob").
?test(sheet1_P2, "/LT/", "P2", "2.7").
?test(sheet1_Q2, "/LT/", "Q2", "3.54").
?test(sheet1_R2, "/LT/", "R2", "1999/02/01 00:00:00").
?test(sheet1_S2, "/LT/", "S2", "1999/02/02 00:00:00").
?test(sheet1_T2, "/LT/", "T2", 0.0).
?test(sheet1_U2, "/LT/", "U2", 3.1415).
?test(sheet1_V2, "/LT/", "V2", 36193.2).
?test(sheet1_A3, "/LT/", "A3", "Blank").
?test(sheet1_C3, "/LT/", "C3", false).
?test(sheet1_D3, "/LT/", "D3", true).
?test(sheet1_E3, "/LT/", "E3", false).
?test(sheet1_F3, "/LT/", "F3", '#DIV/0!').
?test(sheet1_G3, "/LT/", "G3", '#N/A').
?test(sheet1_H3, "/LT/", "H3", '#NAME?').
?test(sheet1_I3, "/LT/", "I3", 'NULL!').
?test(sheet1_J3, "/LT/", "J3", '#NUM!').
?test(sheet1_K3, "/LT/", "K3", '#REF!').
?test(sheet1_L3, "/LT/", "L3", '#VALUE!').
?test(sheet1_M3, "/LT/", "M3", true).
?test(sheet1_N3, "/LT/", "N3", true).
?test(sheet1_O3, "/LT/", "O3", true).
?test(sheet1_P3, "/LT/", "P3", true).
?test(sheet1_Q3, "/LT/", "Q3", true).
?test(sheet1_R3, "/LT/", "R3", true).
?test(sheet1_S3, "/LT/", "S3", true).
?test(sheet1_T3, "/LT/", "T3", false).
?test(sheet1_U3, "/LT/", "U3", true).
?test(sheet1_V3, "/LT/", "V3", true).
?test(sheet1_A4, "/LT/", "A4", "Boolean").
?test(sheet1_B4, "/LT/", "B4", true).
?test(sheet1_C4, "/LT/", "C4", false).
?test(sheet1_D4, "/LT/", "D4", false).
?test(sheet1_E4, "/LT/", "E4", false).
?test(sheet1_F4, "/LT/", "F4", '#DIV/0!').
?test(sheet1_G4, "/LT/", "G4", '#N/A').
?test(sheet1_H4, "/LT/", "H4", '#NAME?').
?test(sheet1_I4, "/LT/", "I4", 'NULL!').
?test(sheet1_J4, "/LT/", "J4", '#NUM!').
?test(sheet1_K4, "/LT/", "K4", '#REF!').
?test(sheet1_L4, "/LT/", "L4", '#VALUE!').
?test(sheet1_M4, "/LT/", "M4", false).
?test(sheet1_N4, "/LT/", "N4", false).
?test(sheet1_O4, "/LT/", "O4", false).
?test(sheet1_P4, "/LT/", "P4", false).
?test(sheet1_Q4, "/LT/", "Q4", false).
?test(sheet1_R4, "/LT/", "R4", false).
?test(sheet1_S4, "/LT/", "S4", false).
?test(sheet1_T4, "/LT/", "T4", false).
?test(sheet1_U4, "/LT/", "U4", false).
?test(sheet1_V4, "/LT/", "V4", false).
?test(sheet1_A5, "/LT/", "A5", "Boolean").
?test(sheet1_B5, "/LT/", "B5", false).
?test(sheet1_C5, "/LT/", "C5", false).
?test(sheet1_D5, "/LT/", "D5", true).
?test(sheet1_E5, "/LT/", "E5", false).
?test(sheet1_F5, "/LT/", "F5", '#DIV/0!').
?test(sheet1_G5, "/LT/", "G5", '#N/A').
?test(sheet1_H5, "/LT/", "H5", '#NAME?').
?test(sheet1_I5, "/LT/", "I5", 'NULL!').
?test(sheet1_J5, "/LT/", "J5", '#NUM!').
?test(sheet1_K5, "/LT/", "K5", '#REF!').
?test(sheet1_L5, "/LT/", "L5", '#VALUE!').
?test(sheet1_M5, "/LT/", "M5", false).
?test(sheet1_N5, "/LT/", "N5", false).
?test(sheet1_O5, "/LT/", "O5", false).
?test(sheet1_P5, "/LT/", "P5", false).
?test(sheet1_Q5, "/LT/", "Q5", false).
?test(sheet1_R5, "/LT/", "R5", false).
?test(sheet1_S5, "/LT/", "S5", false).
?test(sheet1_T5, "/LT/", "T5", false).
?test(sheet1_U5, "/LT/", "U5", false).
?test(sheet1_V5, "/LT/", "V5", false).
?test(sheet1_A6, "/LT/", "A6", "Error").
?test(sheet1_B6, "/LT/", "B6", '#DIV/0!').
?test(sheet1_C6, "/LT/", "C6", '#DIV/0!').
?test(sheet1_D6, "/LT/", "D6", '#DIV/0!').
?test(sheet1_E6, "/LT/", "E6", '#DIV/0!').
?test(sheet1_F6, "/LT/", "F6", '#DIV/0!').
?test(sheet1_G6, "/LT/", "G6", '#DIV/0!').
?test(sheet1_H6, "/LT/", "H6", '#DIV/0!').
?test(sheet1_I6, "/LT/", "I6", '#DIV/0!').
?test(sheet1_J6, "/LT/", "J6", '#DIV/0!').
?test(sheet1_K6, "/LT/", "K6", '#DIV/0!').
?test(sheet1_L6, "/LT/", "L6", '#DIV/0!').
?test(sheet1_M6, "/LT/", "M6", '#DIV/0!').
?test(sheet1_N6, "/LT/", "N6", '#DIV/0!').
?test(sheet1_O6, "/LT/", "O6", '#DIV/0!').
?test(sheet1_P6, "/LT/", "P6", '#DIV/0!').
?test(sheet1_Q6, "/LT/", "Q6", '#DIV/0!').
?test(sheet1_R6, "/LT/", "R6", '#DIV/0!').
?test(sheet1_S6, "/LT/", "S6", '#DIV/0!').
?test(sheet1_T6, "/LT/", "T6", '#DIV/0!').
?test(sheet1_U6, "/LT/", "U6", '#DIV/0!').
?test(sheet1_V6, "/LT/", "V6", '#DIV/0!').
?test(sheet1_A7, "/LT/", "A7", "Error").
?test(sheet1_B7, "/LT/", "B7", '#N/A').
?test(sheet1_C7, "/LT/", "C7", '#N/A').
?test(sheet1_D7, "/LT/", "D7", '#N/A').
?test(sheet1_E7, "/LT/", "E7", '#N/A').
?test(sheet1_F7, "/LT/", "F7", '#N/A').
?test(sheet1_G7, "/LT/", "G7", '#N/A').
?test(sheet1_H7, "/LT/", "H7", '#N/A').
?test(sheet1_I7, "/LT/", "I7", '#N/A').
?test(sheet1_J7, "/LT/", "J7", '#N/A').
?test(sheet1_K7, "/LT/", "K7", '#N/A').
?test(sheet1_L7, "/LT/", "L7", '#N/A').
?test(sheet1_M7, "/LT/", "M7", '#N/A').
?test(sheet1_N7, "/LT/", "N7", '#N/A').
?test(sheet1_O7, "/LT/", "O7", '#N/A').
?test(sheet1_P7, "/LT/", "P7", '#N/A').
?test(sheet1_Q7, "/LT/", "Q7", '#N/A').
?test(sheet1_R7, "/LT/", "R7", '#N/A').
?test(sheet1_S7, "/LT/", "S7", '#N/A').
?test(sheet1_T7, "/LT/", "T7", '#N/A').
?test(sheet1_U7, "/LT/", "U7", '#N/A').
?test(sheet1_V7, "/LT/", "V7", '#N/A').
?test(sheet1_A8, "/LT/", "A8", "Error").
?test(sheet1_B8, "/LT/", "B8", '#NAME?').
?test(sheet1_C8, "/LT/", "C8", '#NAME?').
?test(sheet1_D8, "/LT/", "D8", '#NAME?').
?test(sheet1_E8, "/LT/", "E8", '#NAME?').
?test(sheet1_F8, "/LT/", "F8", '#NAME?').
?test(sheet1_G8, "/LT/", "G8", '#NAME?').
?test(sheet1_H8, "/LT/", "H8", '#NAME?').
?test(sheet1_I8, "/LT/", "I8", '#NAME?').
?test(sheet1_J8, "/LT/", "J8", '#NAME?').
?test(sheet1_K8, "/LT/", "K8", '#NAME?').
?test(sheet1_L8, "/LT/", "L8", '#NAME?').
?test(sheet1_M8, "/LT/", "M8", '#NAME?').
?test(sheet1_N8, "/LT/", "N8", '#NAME?').
?test(sheet1_O8, "/LT/", "O8", '#NAME?').
?test(sheet1_P8, "/LT/", "P8", '#NAME?').
?test(sheet1_Q8, "/LT/", "Q8", '#NAME?').
?test(sheet1_R8, "/LT/", "R8", '#NAME?').
?test(sheet1_S8, "/LT/", "S8", '#NAME?').
?test(sheet1_T8, "/LT/", "T8", '#NAME?').
?test(sheet1_U8, "/LT/", "U8", '#NAME?').
?test(sheet1_V8, "/LT/", "V8", '#NAME?').
?test(sheet1_A9, "/LT/", "A9", "Error").
?test(sheet1_B9, "/LT/", "B9", 'NULL!').
?test(sheet1_C9, "/LT/", "C9", 'NULL!').
?test(sheet1_D9, "/LT/", "D9", 'NULL!').
?test(sheet1_E9, "/LT/", "E9", 'NULL!').
?test(sheet1_F9, "/LT/", "F9", 'NULL!').
?test(sheet1_G9, "/LT/", "G9", 'NULL!').
?test(sheet1_H9, "/LT/", "H9", 'NULL!').
?test(sheet1_I9, "/LT/", "I9", 'NULL!').
?test(sheet1_J9, "/LT/", "J9", 'NULL!').
?test(sheet1_K9, "/LT/", "K9", 'NULL!').
?test(sheet1_L9, "/LT/", "L9", 'NULL!').
?test(sheet1_M9, "/LT/", "M9", 'NULL!').
?test(sheet1_N9, "/LT/", "N9", 'NULL!').
?test(sheet1_O9, "/LT/", "O9", 'NULL!').
?test(sheet1_P9, "/LT/", "P9", 'NULL!').
?test(sheet1_Q9, "/LT/", "Q9", 'NULL!').
?test(sheet1_R9, "/LT/", "R9", 'NULL!').
?test(sheet1_S9, "/LT/", "S9", 'NULL!').
?test(sheet1_T9, "/LT/", "T9", 'NULL!').
?test(sheet1_U9, "/LT/", "U9", 'NULL!').
?test(sheet1_V9, "/LT/", "V9", 'NULL!').
?test(sheet1_A10, "/LT/", "A10", "Error").
?test(sheet1_B10, "/LT/", "B10", '#NUM!').
?test(sheet1_C10, "/LT/", "C10", '#NUM!').
?test(sheet1_D10, "/LT/", "D10", '#NUM!').
?test(sheet1_E10, "/LT/", "E10", '#NUM!').
?test(sheet1_F10, "/LT/", "F10", '#NUM!').
?test(sheet1_G10, "/LT/", "G10", '#NUM!').
?test(sheet1_H10, "/LT/", "H10", '#NUM!').
?test(sheet1_I10, "/LT/", "I10", '#NUM!').
?test(sheet1_J10, "/LT/", "J10", '#NUM!').
?test(sheet1_K10, "/LT/", "K10", '#NUM!').
?test(sheet1_L10, "/LT/", "L10", '#NUM!').
?test(sheet1_M10, "/LT/", "M10", '#NUM!').
?test(sheet1_N10, "/LT/", "N10", '#NUM!').
?test(sheet1_O10, "/LT/", "O10", '#NUM!').
?test(sheet1_P10, "/LT/", "P10", '#NUM!').
?test(sheet1_Q10, "/LT/", "Q10", '#NUM!').
?test(sheet1_R10, "/LT/", "R10", '#NUM!').
?test(sheet1_S10, "/LT/", "S10", '#NUM!').
?test(sheet1_T10, "/LT/", "T10", '#NUM!').
?test(sheet1_U10, "/LT/", "U10", '#NUM!').
?test(sheet1_V10, "/LT/", "V10", '#NUM!').
?test(sheet1_A11, "/LT/", "A11", "Error").
?test(sheet1_B11, "/LT/", "B11", '#REF!').
?test(sheet1_C11, "/LT/", "C11", '#REF!').
?test(sheet1_D11, "/LT/", "D11", '#REF!').
?test(sheet1_E11, "/LT/", "E11", '#REF!').
?test(sheet1_F11, "/LT/", "F11", '#REF!').
?test(sheet1_G11, "/LT/", "G11", '#REF!').
?test(sheet1_H11, "/LT/", "H11", '#REF!').
?test(sheet1_I11, "/LT/", "I11", '#REF!').
?test(sheet1_J11, "/LT/", "J11", '#REF!').
?test(sheet1_K11, "/LT/", "K11", '#REF!').
?test(sheet1_L11, "/LT/", "L11", '#REF!').
?test(sheet1_M11, "/LT/", "M11", '#REF!').
?test(sheet1_N11, "/LT/", "N11", '#REF!').
?test(sheet1_O11, "/LT/", "O11", '#REF!').
?test(sheet1_P11, "/LT/", "P11", '#REF!').
?test(sheet1_Q11, "/LT/", "Q11", '#REF!').
?test(sheet1_R11, "/LT/", "R11", '#REF!').
?test(sheet1_S11, "/LT/", "S11", '#REF!').
?test(sheet1_T11, "/LT/", "T11", '#REF!').
?test(sheet1_U11, "/LT/", "U11", '#REF!').
?test(sheet1_V11, "/LT/", "V11", '#REF!').
?test(sheet1_A12, "/LT/", "A12", "Error").
?test(sheet1_B12, "/LT/", "B12", '#VALUE!').
?test(sheet1_C12, "/LT/", "C12", '#VALUE!').
?test(sheet1_D12, "/LT/", "D12", '#VALUE!').
?test(sheet1_E12, "/LT/", "E12", '#VALUE!').
?test(sheet1_F12, "/LT/", "F12", '#VALUE!').
?test(sheet1_G12, "/LT/", "G12", '#VALUE!').
?test(sheet1_H12, "/LT/", "H12", '#VALUE!').
?test(sheet1_I12, "/LT/", "I12", '#VALUE!').
?test(sheet1_J12, "/LT/", "J12", '#VALUE!').
?test(sheet1_K12, "/LT/", "K12", '#VALUE!').
?test(sheet1_L12, "/LT/", "L12", '#VALUE!').
?test(sheet1_M12, "/LT/", "M12", '#VALUE!').
?test(sheet1_N12, "/LT/", "N12", '#VALUE!').
?test(sheet1_O12, "/LT/", "O12", '#VALUE!').
?test(sheet1_P12, "/LT/", "P12", '#VALUE!').
?test(sheet1_Q12, "/LT/", "Q12", '#VALUE!').
?test(sheet1_R12, "/LT/", "R12", '#VALUE!').
?test(sheet1_S12, "/LT/", "S12", '#VALUE!').
?test(sheet1_T12, "/LT/", "T12", '#VALUE!').
?test(sheet1_U12, "/LT/", "U12", '#VALUE!').
?test(sheet1_V12, "/LT/", "V12", '#VALUE!').
?test(sheet1_A13, "/LT/", "A13", "String").
?test(sheet1_B13, "/LT/", "B13", "Liz").
?test(sheet1_C13, "/LT/", "C13", false).
?test(sheet1_D13, "/LT/", "D13", true).
?test(sheet1_E13, "/LT/", "E13", true).
?test(sheet1_F13, "/LT/", "F13", '#DIV/0!').
?test(sheet1_G13, "/LT/", "G13", '#N/A').
?test(sheet1_H13, "/LT/", "H13", '#NAME?').
?test(sheet1_I13, "/LT/", "I13", 'NULL!').
?test(sheet1_J13, "/LT/", "J13", '#NUM!').
?test(sheet1_K13, "/LT/", "K13", '#REF!').
?test(sheet1_L13, "/LT/", "L13", '#VALUE!').
?test(sheet1_M13, "/LT/", "M13", false).
?test(sheet1_N13, "/LT/", "N13", false).
?test(sheet1_O13, "/LT/", "O13", false).
?test(sheet1_P13, "/LT/", "P13", false).
?test(sheet1_Q13, "/LT/", "Q13", false).
?test(sheet1_R13, "/LT/", "R13", false).
?test(sheet1_S13, "/LT/", "S13", false).
?test(sheet1_T13, "/LT/", "T13", false).
?test(sheet1_U13, "/LT/", "U13", false).
?test(sheet1_V13, "/LT/", "V13", false).
?test(sheet1_A14, "/LT/", "A14", "String").
?test(sheet1_B14, "/LT/", "B14", "Doug").
?test(sheet1_C14, "/LT/", "C14", false).
?test(sheet1_D14, "/LT/", "D14", true).
?test(sheet1_E14, "/LT/", "E14", true).
?test(sheet1_F14, "/LT/", "F14", '#DIV/0!').
?test(sheet1_G14, "/LT/", "G14", '#N/A').
?test(sheet1_H14, "/LT/", "H14", '#NAME?').
?test(sheet1_I14, "/LT/", "I14", 'NULL!').
?test(sheet1_J14, "/LT/", "J14", '#NUM!').
?test(sheet1_K14, "/LT/", "K14", '#REF!').
?test(sheet1_L14, "/LT/", "L14", '#VALUE!').
?test(sheet1_M14, "/LT/", "M14", true).
?test(sheet1_N14, "/LT/", "N14", false).
?test(sheet1_O14, "/LT/", "O14", false).
?test(sheet1_P14, "/LT/", "P14", false).
?test(sheet1_Q14, "/LT/", "Q14", false).
?test(sheet1_R14, "/LT/", "R14", false).
?test(sheet1_S14, "/LT/", "S14", false).
?test(sheet1_T14, "/LT/", "T14", false).
?test(sheet1_U14, "/LT/", "U14", false).
?test(sheet1_V14, "/LT/", "V14", false).
?test(sheet1_A15, "/LT/", "A15", "String").
?test(sheet1_B15, "/LT/", "B15", "Bob").
?test(sheet1_C15, "/LT/", "C15", false).
?test(sheet1_D15, "/LT/", "D15", true).
?test(sheet1_E15, "/LT/", "E15", true).
?test(sheet1_F15, "/LT/", "F15", '#DIV/0!').
?test(sheet1_G15, "/LT/", "G15", '#N/A').
?test(sheet1_H15, "/LT/", "H15", '#NAME?').
?test(sheet1_I15, "/LT/", "I15", 'NULL!').
?test(sheet1_J15, "/LT/", "J15", '#NUM!').
?test(sheet1_K15, "/LT/", "K15", '#REF!').
?test(sheet1_L15, "/LT/", "L15", '#VALUE!').
?test(sheet1_M15, "/LT/", "M15", true).
?test(sheet1_N15, "/LT/", "N15", true).
?test(sheet1_O15, "/LT/", "O15", false).
?test(sheet1_P15, "/LT/", "P15", false).
?test(sheet1_Q15, "/LT/", "Q15", false).
?test(sheet1_R15, "/LT/", "R15", false).
?test(sheet1_S15, "/LT/", "S15", false).
?test(sheet1_T15, "/LT/", "T15", false).
?test(sheet1_U15, "/LT/", "U15", false).
?test(sheet1_V15, "/LT/", "V15", false).
?test(sheet1_A16, "/LT/", "A16", "Str Num").
?test(sheet1_B16, "/LT/", "B16", "2.7").
?test(sheet1_C16, "/LT/", "C16", false).
?test(sheet1_D16, "/LT/", "D16", true).
?test(sheet1_E16, "/LT/", "E16", true).
?test(sheet1_F16, "/LT/", "F16", '#DIV/0!').
?test(sheet1_G16, "/LT/", "G16", '#N/A').
?test(sheet1_H16, "/LT/", "H16", '#NAME?').
?test(sheet1_I16, "/LT/", "I16", 'NULL!').
?test(sheet1_J16, "/LT/", "J16", '#NUM!').
?test(sheet1_K16, "/LT/", "K16", '#REF!').
?test(sheet1_L16, "/LT/", "L16", '#VALUE!').
?test(sheet1_M16, "/LT/", "M16", true).
?test(sheet1_N16, "/LT/", "N16", true).
?test(sheet1_O16, "/LT/", "O16", true).
?test(sheet1_P16, "/LT/", "P16", false).
?test(sheet1_Q16, "/LT/", "Q16", true).
?test(sheet1_R16, "/LT/", "R16", false).
?test(sheet1_S16, "/LT/", "S16", false).
?test(sheet1_T16, "/LT/", "T16", false).
?test(sheet1_U16, "/LT/", "U16", false).
?test(sheet1_V16, "/LT/", "V16", false).
?test(sheet1_A17, "/LT/", "A17", "Str Num").
?test(sheet1_B17, "/LT/", "B17", "3.54").
?test(sheet1_C17, "/LT/", "C17", false).
?test(sheet1_D17, "/LT/", "D17", true).
?test(sheet1_E17, "/LT/", "E17", true).
?test(sheet1_F17, "/LT/", "F17", '#DIV/0!').
?test(sheet1_G17, "/LT/", "G17", '#N/A').
?test(sheet1_H17, "/LT/", "H17", '#NAME?').
?test(sheet1_I17, "/LT/", "I17", 'NULL!').
?test(sheet1_J17, "/LT/", "J17", '#NUM!').
?test(sheet1_K17, "/LT/", "K17", '#REF!').
?test(sheet1_L17, "/LT/", "L17", '#VALUE!').
?test(sheet1_M17, "/LT/", "M17", true).
?test(sheet1_N17, "/LT/", "N17", true).
?test(sheet1_O17, "/LT/", "O17", true).
?test(sheet1_P17, "/LT/", "P17", false).
?test(sheet1_Q17, "/LT/", "Q17", false).
?test(sheet1_R17, "/LT/", "R17", false).
?test(sheet1_S17, "/LT/", "S17", false).
?test(sheet1_T17, "/LT/", "T17", false).
?test(sheet1_U17, "/LT/", "U17", false).
?test(sheet1_V17, "/LT/", "V17", false).
?test(sheet1_A18, "/LT/", "A18", "Integer").
?test(sheet1_B18, "/LT/", "B18", "1999/02/01 00:00:00").
?test(sheet1_C18, "/LT/", "C18", false).
?test(sheet1_D18, "/LT/", "D18", true).
?test(sheet1_E18, "/LT/", "E18", true).
?test(sheet1_F18, "/LT/", "F18", '#DIV/0!').
?test(sheet1_G18, "/LT/", "G18", '#N/A').
?test(sheet1_H18, "/LT/", "H18", '#NAME?').
?test(sheet1_I18, "/LT/", "I18", 'NULL!').
?test(sheet1_J18, "/LT/", "J18", '#NUM!').
?test(sheet1_K18, "/LT/", "K18", '#REF!').
?test(sheet1_L18, "/LT/", "L18", '#VALUE!').
?test(sheet1_M18, "/LT/", "M18", true).
?test(sheet1_N18, "/LT/", "N18", true).
?test(sheet1_O18, "/LT/", "O18", true).
?test(sheet1_P18, "/LT/", "P18", true).
?test(sheet1_Q18, "/LT/", "Q18", true).
?test(sheet1_R18, "/LT/", "R18", false).
?test(sheet1_S18, "/LT/", "S18", true).
?test(sheet1_T18, "/LT/", "T18", false).
?test(sheet1_U18, "/LT/", "U18", false).
?test(sheet1_V18, "/LT/", "V18", true).
?test(sheet1_A19, "/LT/", "A19", "Integer").
?test(sheet1_B19, "/LT/", "B19", "1999/02/02 00:00:00").
?test(sheet1_C19, "/LT/", "C19", false).
?test(sheet1_D19, "/LT/", "D19", true).
?test(sheet1_E19, "/LT/", "E19", true).
?test(sheet1_F19, "/LT/", "F19", '#DIV/0!').
?test(sheet1_G19, "/LT/", "G19", '#N/A').
?test(sheet1_H19, "/LT/", "H19", '#NAME?').
?test(sheet1_I19, "/LT/", "I19", 'NULL!').
?test(sheet1_J19, "/LT/", "J19", '#NUM!').
?test(sheet1_K19, "/LT/", "K19", '#REF!').
?test(sheet1_L19, "/LT/", "L19", '#VALUE!').
?test(sheet1_M19, "/LT/", "M19", true).
?test(sheet1_N19, "/LT/", "N19", true).
?test(sheet1_O19, "/LT/", "O19", true).
?test(sheet1_P19, "/LT/", "P19", true).
?test(sheet1_Q19, "/LT/", "Q19", true).
?test(sheet1_R19, "/LT/", "R19", false).
?test(sheet1_S19, "/LT/", "S19", false).
?test(sheet1_T19, "/LT/", "T19", false).
?test(sheet1_U19, "/LT/", "U19", false).
?test(sheet1_V19, "/LT/", "V19", true).
?test(sheet1_A20, "/LT/", "A20", "Zero").
?test(sheet1_B20, "/LT/", "B20", 0.0).
?test(sheet1_C20, "/LT/", "C20", false).
?test(sheet1_D20, "/LT/", "D20", true).
?test(sheet1_E20, "/LT/", "E20", true).
?test(sheet1_F20, "/LT/", "F20", '#DIV/0!').
?test(sheet1_G20, "/LT/", "G20", '#N/A').
?test(sheet1_H20, "/LT/", "H20", '#NAME?').
?test(sheet1_I20, "/LT/", "I20", 'NULL!').
?test(sheet1_J20, "/LT/", "J20", '#NUM!').
?test(sheet1_K20, "/LT/", "K20", '#REF!').
?test(sheet1_L20, "/LT/", "L20", '#VALUE!').
?test(sheet1_M20, "/LT/", "M20", true).
?test(sheet1_N20, "/LT/", "N20", true).
?test(sheet1_O20, "/LT/", "O20", true).
?test(sheet1_P20, "/LT/", "P20", true).
?test(sheet1_Q20, "/LT/", "Q20", true).
?test(sheet1_R20, "/LT/", "R20", true).
?test(sheet1_S20, "/LT/", "S20", true).
?test(sheet1_T20, "/LT/", "T20", false).
?test(sheet1_U20, "/LT/", "U20", true).
?test(sheet1_V20, "/LT/", "V20", true).
?test(sheet1_A21, "/LT/", "A21", "Float").
?test(sheet1_B21, "/LT/", "B21", 3.1415).
?test(sheet1_C21, "/LT/", "C21", false).
?test(sheet1_D21, "/LT/", "D21", true).
?test(sheet1_E21, "/LT/", "E21", true).
?test(sheet1_F21, "/LT/", "F21", '#DIV/0!').
?test(sheet1_G21, "/LT/", "G21", '#N/A').
?test(sheet1_H21, "/LT/", "H21", '#NAME?').
?test(sheet1_I21, "/LT/", "I21", 'NULL!').
?test(sheet1_J21, "/LT/", "J21", '#NUM!').
?test(sheet1_K21, "/LT/", "K21", '#REF!').
?test(sheet1_L21, "/LT/", "L21", '#VALUE!').
?test(sheet1_M21, "/LT/", "M21", true).
?test(sheet1_N21, "/LT/", "N21", true).
?test(sheet1_O21, "/LT/", "O21", true).
?test(sheet1_P21, "/LT/", "P21", true).
?test(sheet1_Q21, "/LT/", "Q21", true).
?test(sheet1_R21, "/LT/", "R21", true).
?test(sheet1_S21, "/LT/", "S21", true).
?test(sheet1_T21, "/LT/", "T21", false).
?test(sheet1_U21, "/LT/", "U21", false).
?test(sheet1_V21, "/LT/", "V21", true).
?test(sheet1_A22, "/LT/", "A22", "Float").
?test(sheet1_B22, "/LT/", "B22", 36193.2).
?test(sheet1_C22, "/LT/", "C22", false).
?test(sheet1_D22, "/LT/", "D22", true).
?test(sheet1_E22, "/LT/", "E22", true).
?test(sheet1_F22, "/LT/", "F22", '#DIV/0!').
?test(sheet1_G22, "/LT/", "G22", '#N/A').
?test(sheet1_H22, "/LT/", "H22", '#NAME?').
?test(sheet1_I22, "/LT/", "I22", 'NULL!').
?test(sheet1_J22, "/LT/", "J22", '#NUM!').
?test(sheet1_K22, "/LT/", "K22", '#REF!').
?test(sheet1_L22, "/LT/", "L22", '#VALUE!').
?test(sheet1_M22, "/LT/", "M22", true).
?test(sheet1_N22, "/LT/", "N22", true).
?test(sheet1_O22, "/LT/", "O22", true).
?test(sheet1_P22, "/LT/", "P22", true).
?test(sheet1_Q22, "/LT/", "Q22", true).
?test(sheet1_R22, "/LT/", "R22", false).
?test(sheet1_S22, "/LT/", "S22", false).
?test(sheet1_T22, "/LT/", "T22", false).
?test(sheet1_U22, "/LT/", "U22", false).
?test(sheet1_V22, "/LT/", "V22", false).
?test(sheet1_A25, "/LT/", "A25", "Blank").
?test(sheet1_C25, "/LT/", "C25", false).
?test(sheet1_D25, "/LT/", "D25", true).
?test(sheet1_E25, "/LT/", "E25", false).
?test(sheet1_F25, "/LT/", "F25", '#DIV/0!').
?test(sheet1_G25, "/LT/", "G25", '#N/A').
?test(sheet1_H25, "/LT/", "H25", '#NAME?').
?test(sheet1_I25, "/LT/", "I25", 'NULL!').
?test(sheet1_J25, "/LT/", "J25", '#NUM!').
?test(sheet1_K25, "/LT/", "K25", '#REF!').
?test(sheet1_L25, "/LT/", "L25", '#VALUE!').
?test(sheet1_M25, "/LT/", "M25", true).
?test(sheet1_N25, "/LT/", "N25", true).
?test(sheet1_O25, "/LT/", "O25", true).
?test(sheet1_P25, "/LT/", "P25", true).
?test(sheet1_Q25, "/LT/", "Q25", true).
?test(sheet1_R25, "/LT/", "R25", true).
?test(sheet1_S25, "/LT/", "S25", true).
?test(sheet1_T25, "/LT/", "T25", false).
?test(sheet1_U25, "/LT/", "U25", true).
?test(sheet1_V25, "/LT/", "V25", true).
?test(sheet1_A26, "/LT/", "A26", "Boolean").
?test(sheet1_C26, "/LT/", "C26", false).
?test(sheet1_D26, "/LT/", "D26", false).
?test(sheet1_E26, "/LT/", "E26", false).
?test(sheet1_F26, "/LT/", "F26", '#DIV/0!').
?test(sheet1_G26, "/LT/", "G26", '#N/A').
?test(sheet1_H26, "/LT/", "H26", '#NAME?').
?test(sheet1_I26, "/LT/", "I26", 'NULL!').
?test(sheet1_J26, "/LT/", "J26", '#NUM!').
?test(sheet1_K26, "/LT/", "K26", '#REF!').
?test(sheet1_L26, "/LT/", "L26", '#VALUE!').
?test(sheet1_M26, "/LT/", "M26", false).
?test(sheet1_N26, "/LT/", "N26", false).
?test(sheet1_O26, "/LT/", "O26", false).
?test(sheet1_P26, "/LT/", "P26", false).
?test(sheet1_Q26, "/LT/", "Q26", false).
?test(sheet1_R26, "/LT/", "R26", false).
?test(sheet1_S26, "/LT/", "S26", false).
?test(sheet1_T26, "/LT/", "T26", false).
?test(sheet1_U26, "/LT/", "U26", false).
?test(sheet1_V26, "/LT/", "V26", false).
?test(sheet1_A27, "/LT/", "A27", "Boolean").
?test(sheet1_C27, "/LT/", "C27", false).
?test(sheet1_D27, "/LT/", "D27", true).
?test(sheet1_E27, "/LT/", "E27", false).
?test(sheet1_F27, "/LT/", "F27", '#DIV/0!').
?test(sheet1_G27, "/LT/", "G27", '#N/A').
?test(sheet1_H27, "/LT/", "H27", '#NAME?').
?test(sheet1_I27, "/LT/", "I27", 'NULL!').
?test(sheet1_J27, "/LT/", "J27", '#NUM!').
?test(sheet1_K27, "/LT/", "K27", '#REF!').
?test(sheet1_L27, "/LT/", "L27", '#VALUE!').
?test(sheet1_M27, "/LT/", "M27", false).
?test(sheet1_N27, "/LT/", "N27", false).
?test(sheet1_O27, "/LT/", "O27", false).
?test(sheet1_P27, "/LT/", "P27", false).
?test(sheet1_Q27, "/LT/", "Q27", false).
?test(sheet1_R27, "/LT/", "R27", false).
?test(sheet1_S27, "/LT/", "S27", false).
?test(sheet1_T27, "/LT/", "T27", false).
?test(sheet1_U27, "/LT/", "U27", false).
?test(sheet1_V27, "/LT/", "V27", false).
?test(sheet1_A28, "/LT/", "A28", "Error").
?test(sheet1_C28, "/LT/", "C28", '#DIV/0!').
?test(sheet1_D28, "/LT/", "D28", '#DIV/0!').
?test(sheet1_E28, "/LT/", "E28", '#DIV/0!').
?test(sheet1_F28, "/LT/", "F28", '#DIV/0!').
?test(sheet1_G28, "/LT/", "G28", '#DIV/0!').
?test(sheet1_H28, "/LT/", "H28", '#DIV/0!').
?test(sheet1_I28, "/LT/", "I28", '#DIV/0!').
?test(sheet1_J28, "/LT/", "J28", '#DIV/0!').
?test(sheet1_K28, "/LT/", "K28", '#DIV/0!').
?test(sheet1_L28, "/LT/", "L28", '#DIV/0!').
?test(sheet1_M28, "/LT/", "M28", '#DIV/0!').
?test(sheet1_N28, "/LT/", "N28", '#DIV/0!').
?test(sheet1_O28, "/LT/", "O28", '#DIV/0!').
?test(sheet1_P28, "/LT/", "P28", '#DIV/0!').
?test(sheet1_Q28, "/LT/", "Q28", '#DIV/0!').
?test(sheet1_R28, "/LT/", "R28", '#DIV/0!').
?test(sheet1_S28, "/LT/", "S28", '#DIV/0!').
?test(sheet1_T28, "/LT/", "T28", '#DIV/0!').
?test(sheet1_U28, "/LT/", "U28", '#DIV/0!').
?test(sheet1_V28, "/LT/", "V28", '#DIV/0!').
?test(sheet1_A29, "/LT/", "A29", "Error").
?test(sheet1_C29, "/LT/", "C29", '#N/A').
?test(sheet1_D29, "/LT/", "D29", '#N/A').
?test(sheet1_E29, "/LT/", "E29", '#N/A').
?test(sheet1_F29, "/LT/", "F29", '#N/A').
?test(sheet1_G29, "/LT/", "G29", '#N/A').
?test(sheet1_H29, "/LT/", "H29", '#N/A').
?test(sheet1_I29, "/LT/", "I29", '#N/A').
?test(sheet1_J29, "/LT/", "J29", '#N/A').
?test(sheet1_K29, "/LT/", "K29", '#N/A').
?test(sheet1_L29, "/LT/", "L29", '#N/A').
?test(sheet1_M29, "/LT/", "M29", '#N/A').
?test(sheet1_N29, "/LT/", "N29", '#N/A').
?test(sheet1_O29, "/LT/", "O29", '#N/A').
?test(sheet1_P29, "/LT/", "P29", '#N/A').
?test(sheet1_Q29, "/LT/", "Q29", '#N/A').
?test(sheet1_R29, "/LT/", "R29", '#N/A').
?test(sheet1_S29, "/LT/", "S29", '#N/A').
?test(sheet1_T29, "/LT/", "T29", '#N/A').
?test(sheet1_U29, "/LT/", "U29", '#N/A').
?test(sheet1_V29, "/LT/", "V29", '#N/A').
?test(sheet1_A30, "/LT/", "A30", "Error").
?test(sheet1_C30, "/LT/", "C30", '#NAME?').
?test(sheet1_D30, "/LT/", "D30", '#NAME?').
?test(sheet1_E30, "/LT/", "E30", '#NAME?').
?test(sheet1_F30, "/LT/", "F30", '#NAME?').
?test(sheet1_G30, "/LT/", "G30", '#NAME?').
?test(sheet1_H30, "/LT/", "H30", '#NAME?').
?test(sheet1_I30, "/LT/", "I30", '#NAME?').
?test(sheet1_J30, "/LT/", "J30", '#NAME?').
?test(sheet1_K30, "/LT/", "K30", '#NAME?').
?test(sheet1_L30, "/LT/", "L30", '#NAME?').
?test(sheet1_M30, "/LT/", "M30", '#NAME?').
?test(sheet1_N30, "/LT/", "N30", '#NAME?').
?test(sheet1_O30, "/LT/", "O30", '#NAME?').
?test(sheet1_P30, "/LT/", "P30", '#NAME?').
?test(sheet1_Q30, "/LT/", "Q30", '#NAME?').
?test(sheet1_R30, "/LT/", "R30", '#NAME?').
?test(sheet1_S30, "/LT/", "S30", '#NAME?').
?test(sheet1_T30, "/LT/", "T30", '#NAME?').
?test(sheet1_U30, "/LT/", "U30", '#NAME?').
?test(sheet1_V30, "/LT/", "V30", '#NAME?').
?test(sheet1_A31, "/LT/", "A31", "Error").
?test(sheet1_C31, "/LT/", "C31", 'NULL!').
?test(sheet1_D31, "/LT/", "D31", 'NULL!').
?test(sheet1_E31, "/LT/", "E31", 'NULL!').
?test(sheet1_F31, "/LT/", "F31", 'NULL!').
?test(sheet1_G31, "/LT/", "G31", 'NULL!').
?test(sheet1_H31, "/LT/", "H31", 'NULL!').
?test(sheet1_I31, "/LT/", "I31", 'NULL!').
?test(sheet1_J31, "/LT/", "J31", 'NULL!').
?test(sheet1_K31, "/LT/", "K31", 'NULL!').
?test(sheet1_L31, "/LT/", "L31", 'NULL!').
?test(sheet1_M31, "/LT/", "M31", 'NULL!').
?test(sheet1_N31, "/LT/", "N31", 'NULL!').
?test(sheet1_O31, "/LT/", "O31", 'NULL!').
?test(sheet1_P31, "/LT/", "P31", 'NULL!').
?test(sheet1_Q31, "/LT/", "Q31", 'NULL!').
?test(sheet1_R31, "/LT/", "R31", 'NULL!').
?test(sheet1_S31, "/LT/", "S31", 'NULL!').
?test(sheet1_T31, "/LT/", "T31", 'NULL!').
?test(sheet1_U31, "/LT/", "U31", 'NULL!').
?test(sheet1_V31, "/LT/", "V31", 'NULL!').
?test(sheet1_A32, "/LT/", "A32", "Error").
?test(sheet1_C32, "/LT/", "C32", '#NUM!').
?test(sheet1_D32, "/LT/", "D32", '#NUM!').
?test(sheet1_E32, "/LT/", "E32", '#NUM!').
?test(sheet1_F32, "/LT/", "F32", '#NUM!').
?test(sheet1_G32, "/LT/", "G32", '#NUM!').
?test(sheet1_H32, "/LT/", "H32", '#NUM!').
?test(sheet1_I32, "/LT/", "I32", '#NUM!').
?test(sheet1_J32, "/LT/", "J32", '#NUM!').
?test(sheet1_K32, "/LT/", "K32", '#NUM!').
?test(sheet1_L32, "/LT/", "L32", '#NUM!').
?test(sheet1_M32, "/LT/", "M32", '#NUM!').
?test(sheet1_N32, "/LT/", "N32", '#NUM!').
?test(sheet1_O32, "/LT/", "O32", '#NUM!').
?test(sheet1_P32, "/LT/", "P32", '#NUM!').
?test(sheet1_Q32, "/LT/", "Q32", '#NUM!').
?test(sheet1_R32, "/LT/", "R32", '#NUM!').
?test(sheet1_S32, "/LT/", "S32", '#NUM!').
?test(sheet1_T32, "/LT/", "T32", '#NUM!').
?test(sheet1_U32, "/LT/", "U32", '#NUM!').
?test(sheet1_V32, "/LT/", "V32", '#NUM!').
?test(sheet1_A33, "/LT/", "A33", "Error").
?test(sheet1_C33, "/LT/", "C33", '#REF!').
?test(sheet1_D33, "/LT/", "D33", '#REF!').
?test(sheet1_E33, "/LT/", "E33", '#REF!').
?test(sheet1_F33, "/LT/", "F33", '#REF!').
?test(sheet1_G33, "/LT/", "G33", '#REF!').
?test(sheet1_H33, "/LT/", "H33", '#REF!').
?test(sheet1_I33, "/LT/", "I33", '#REF!').
?test(sheet1_J33, "/LT/", "J33", '#REF!').
?test(sheet1_K33, "/LT/", "K33", '#REF!').
?test(sheet1_L33, "/LT/", "L33", '#REF!').
?test(sheet1_M33, "/LT/", "M33", '#REF!').
?test(sheet1_N33, "/LT/", "N33", '#REF!').
?test(sheet1_O33, "/LT/", "O33", '#REF!').
?test(sheet1_P33, "/LT/", "P33", '#REF!').
?test(sheet1_Q33, "/LT/", "Q33", '#REF!').
?test(sheet1_R33, "/LT/", "R33", '#REF!').
?test(sheet1_S33, "/LT/", "S33", '#REF!').
?test(sheet1_T33, "/LT/", "T33", '#REF!').
?test(sheet1_U33, "/LT/", "U33", '#REF!').
?test(sheet1_V33, "/LT/", "V33", '#REF!').
?test(sheet1_A34, "/LT/", "A34", "Error").
?test(sheet1_C34, "/LT/", "C34", '#VALUE!').
?test(sheet1_D34, "/LT/", "D34", '#VALUE!').
?test(sheet1_E34, "/LT/", "E34", '#VALUE!').
?test(sheet1_F34, "/LT/", "F34", '#VALUE!').
?test(sheet1_G34, "/LT/", "G34", '#VALUE!').
?test(sheet1_H34, "/LT/", "H34", '#VALUE!').
?test(sheet1_I34, "/LT/", "I34", '#VALUE!').
?test(sheet1_J34, "/LT/", "J34", '#VALUE!').
?test(sheet1_K34, "/LT/", "K34", '#VALUE!').
?test(sheet1_L34, "/LT/", "L34", '#VALUE!').
?test(sheet1_M34, "/LT/", "M34", '#VALUE!').
?test(sheet1_N34, "/LT/", "N34", '#VALUE!').
?test(sheet1_O34, "/LT/", "O34", '#VALUE!').
?test(sheet1_P34, "/LT/", "P34", '#VALUE!').
?test(sheet1_Q34, "/LT/", "Q34", '#VALUE!').
?test(sheet1_R34, "/LT/", "R34", '#VALUE!').
?test(sheet1_S34, "/LT/", "S34", '#VALUE!').
?test(sheet1_T34, "/LT/", "T34", '#VALUE!').
?test(sheet1_U34, "/LT/", "U34", '#VALUE!').
?test(sheet1_V34, "/LT/", "V34", '#VALUE!').
?test(sheet1_A35, "/LT/", "A35", "String").
?test(sheet1_C35, "/LT/", "C35", false).
?test(sheet1_D35, "/LT/", "D35", true).
?test(sheet1_E35, "/LT/", "E35", true).
?test(sheet1_F35, "/LT/", "F35", '#DIV/0!').
?test(sheet1_G35, "/LT/", "G35", '#N/A').
?test(sheet1_H35, "/LT/", "H35", '#NAME?').
?test(sheet1_I35, "/LT/", "I35", 'NULL!').
?test(sheet1_J35, "/LT/", "J35", '#NUM!').
?test(sheet1_K35, "/LT/", "K35", '#REF!').
?test(sheet1_L35, "/LT/", "L35", '#VALUE!').
?test(sheet1_M35, "/LT/", "M35", false).
?test(sheet1_N35, "/LT/", "N35", false).
?test(sheet1_O35, "/LT/", "O35", false).
?test(sheet1_P35, "/LT/", "P35", false).
?test(sheet1_Q35, "/LT/", "Q35", false).
?test(sheet1_R35, "/LT/", "R35", false).
?test(sheet1_S35, "/LT/", "S35", false).
?test(sheet1_T35, "/LT/", "T35", false).
?test(sheet1_U35, "/LT/", "U35", false).
?test(sheet1_V35, "/LT/", "V35", false).
?test(sheet1_A36, "/LT/", "A36", "String").
?test(sheet1_C36, "/LT/", "C36", false).
?test(sheet1_D36, "/LT/", "D36", true).
?test(sheet1_E36, "/LT/", "E36", true).
?test(sheet1_F36, "/LT/", "F36", '#DIV/0!').
?test(sheet1_G36, "/LT/", "G36", '#N/A').
?test(sheet1_H36, "/LT/", "H36", '#NAME?').
?test(sheet1_I36, "/LT/", "I36", 'NULL!').
?test(sheet1_J36, "/LT/", "J36", '#NUM!').
?test(sheet1_K36, "/LT/", "K36", '#REF!').
?test(sheet1_L36, "/LT/", "L36", '#VALUE!').
?test(sheet1_M36, "/LT/", "M36", true).
?test(sheet1_N36, "/LT/", "N36", false).
?test(sheet1_O36, "/LT/", "O36", false).
?test(sheet1_P36, "/LT/", "P36", false).
?test(sheet1_Q36, "/LT/", "Q36", false).
?test(sheet1_R36, "/LT/", "R36", false).
?test(sheet1_S36, "/LT/", "S36", false).
?test(sheet1_T36, "/LT/", "T36", false).
?test(sheet1_U36, "/LT/", "U36", false).
?test(sheet1_V36, "/LT/", "V36", false).
?test(sheet1_A37, "/LT/", "A37", "String").
?test(sheet1_C37, "/LT/", "C37", false).
?test(sheet1_D37, "/LT/", "D37", true).
?test(sheet1_E37, "/LT/", "E37", true).
?test(sheet1_F37, "/LT/", "F37", '#DIV/0!').
?test(sheet1_G37, "/LT/", "G37", '#N/A').
?test(sheet1_H37, "/LT/", "H37", '#NAME?').
?test(sheet1_I37, "/LT/", "I37", 'NULL!').
?test(sheet1_J37, "/LT/", "J37", '#NUM!').
?test(sheet1_K37, "/LT/", "K37", '#REF!').
?test(sheet1_L37, "/LT/", "L37", '#VALUE!').
?test(sheet1_M37, "/LT/", "M37", true).
?test(sheet1_N37, "/LT/", "N37", true).
?test(sheet1_O37, "/LT/", "O37", false).
?test(sheet1_P37, "/LT/", "P37", false).
?test(sheet1_Q37, "/LT/", "Q37", false).
?test(sheet1_R37, "/LT/", "R37", false).
?test(sheet1_S37, "/LT/", "S37", false).
?test(sheet1_T37, "/LT/", "T37", false).
?test(sheet1_U37, "/LT/", "U37", false).
?test(sheet1_V37, "/LT/", "V37", false).
?test(sheet1_A38, "/LT/", "A38", "Str Num").
?test(sheet1_C38, "/LT/", "C38", false).
?test(sheet1_D38, "/LT/", "D38", true).
?test(sheet1_E38, "/LT/", "E38", true).
?test(sheet1_F38, "/LT/", "F38", '#DIV/0!').
?test(sheet1_G38, "/LT/", "G38", '#N/A').
?test(sheet1_H38, "/LT/", "H38", '#NAME?').
?test(sheet1_I38, "/LT/", "I38", 'NULL!').
?test(sheet1_J38, "/LT/", "J38", '#NUM!').
?test(sheet1_K38, "/LT/", "K38", '#REF!').
?test(sheet1_L38, "/LT/", "L38", '#VALUE!').
?test(sheet1_M38, "/LT/", "M38", true).
?test(sheet1_N38, "/LT/", "N38", true).
?test(sheet1_O38, "/LT/", "O38", true).
?test(sheet1_P38, "/LT/", "P38", false).
?test(sheet1_Q38, "/LT/", "Q38", true).
?test(sheet1_R38, "/LT/", "R38", false).
?test(sheet1_S38, "/LT/", "S38", false).
?test(sheet1_T38, "/LT/", "T38", false).
?test(sheet1_U38, "/LT/", "U38", false).
?test(sheet1_V38, "/LT/", "V38", false).
?test(sheet1_A39, "/LT/", "A39", "Str Num").
?test(sheet1_C39, "/LT/", "C39", false).
?test(sheet1_D39, "/LT/", "D39", true).
?test(sheet1_E39, "/LT/", "E39", true).
?test(sheet1_F39, "/LT/", "F39", '#DIV/0!').
?test(sheet1_G39, "/LT/", "G39", '#N/A').
?test(sheet1_H39, "/LT/", "H39", '#NAME?').
?test(sheet1_I39, "/LT/", "I39", 'NULL!').
?test(sheet1_J39, "/LT/", "J39", '#NUM!').
?test(sheet1_K39, "/LT/", "K39", '#REF!').
?test(sheet1_L39, "/LT/", "L39", '#VALUE!').
?test(sheet1_M39, "/LT/", "M39", true).
?test(sheet1_N39, "/LT/", "N39", true).
?test(sheet1_O39, "/LT/", "O39", true).
?test(sheet1_P39, "/LT/", "P39", false).
?test(sheet1_Q39, "/LT/", "Q39", false).
?test(sheet1_R39, "/LT/", "R39", false).
?test(sheet1_S39, "/LT/", "S39", false).
?test(sheet1_T39, "/LT/", "T39", false).
?test(sheet1_U39, "/LT/", "U39", false).
?test(sheet1_V39, "/LT/", "V39", false).
?test(sheet1_A40, "/LT/", "A40", "Integer").
?test(sheet1_C40, "/LT/", "C40", false).
?test(sheet1_D40, "/LT/", "D40", true).
?test(sheet1_E40, "/LT/", "E40", true).
?test(sheet1_F40, "/LT/", "F40", '#DIV/0!').
?test(sheet1_G40, "/LT/", "G40", '#N/A').
?test(sheet1_H40, "/LT/", "H40", '#NAME?').
?test(sheet1_I40, "/LT/", "I40", 'NULL!').
?test(sheet1_J40, "/LT/", "J40", '#NUM!').
?test(sheet1_K40, "/LT/", "K40", '#REF!').
?test(sheet1_L40, "/LT/", "L40", '#VALUE!').
?test(sheet1_M40, "/LT/", "M40", true).
?test(sheet1_N40, "/LT/", "N40", true).
?test(sheet1_O40, "/LT/", "O40", true).
?test(sheet1_P40, "/LT/", "P40", true).
?test(sheet1_Q40, "/LT/", "Q40", true).
?test(sheet1_R40, "/LT/", "R40", false).
?test(sheet1_S40, "/LT/", "S40", true).
?test(sheet1_T40, "/LT/", "T40", false).
?test(sheet1_U40, "/LT/", "U40", false).
?test(sheet1_V40, "/LT/", "V40", true).
?test(sheet1_A41, "/LT/", "A41", "Integer").
?test(sheet1_C41, "/LT/", "C41", false).
?test(sheet1_D41, "/LT/", "D41", true).
?test(sheet1_E41, "/LT/", "E41", true).
?test(sheet1_F41, "/LT/", "F41", '#DIV/0!').
?test(sheet1_G41, "/LT/", "G41", '#N/A').
?test(sheet1_H41, "/LT/", "H41", '#NAME?').
?test(sheet1_I41, "/LT/", "I41", 'NULL!').
?test(sheet1_J41, "/LT/", "J41", '#NUM!').
?test(sheet1_K41, "/LT/", "K41", '#REF!').
?test(sheet1_L41, "/LT/", "L41", '#VALUE!').
?test(sheet1_M41, "/LT/", "M41", true).
?test(sheet1_N41, "/LT/", "N41", true).
?test(sheet1_O41, "/LT/", "O41", true).
?test(sheet1_P41, "/LT/", "P41", true).
?test(sheet1_Q41, "/LT/", "Q41", true).
?test(sheet1_R41, "/LT/", "R41", false).
?test(sheet1_S41, "/LT/", "S41", false).
?test(sheet1_T41, "/LT/", "T41", false).
?test(sheet1_U41, "/LT/", "U41", false).
?test(sheet1_V41, "/LT/", "V41", true).
?test(sheet1_A42, "/LT/", "A42", "Zero").
?test(sheet1_C42, "/LT/", "C42", false).
?test(sheet1_D42, "/LT/", "D42", true).
?test(sheet1_E42, "/LT/", "E42", true).
?test(sheet1_F42, "/LT/", "F42", '#DIV/0!').
?test(sheet1_G42, "/LT/", "G42", '#N/A').
?test(sheet1_H42, "/LT/", "H42", '#NAME?').
?test(sheet1_I42, "/LT/", "I42", 'NULL!').
?test(sheet1_J42, "/LT/", "J42", '#NUM!').
?test(sheet1_K42, "/LT/", "K42", '#REF!').
?test(sheet1_L42, "/LT/", "L42", '#VALUE!').
?test(sheet1_M42, "/LT/", "M42", true).
?test(sheet1_N42, "/LT/", "N42", true).
?test(sheet1_O42, "/LT/", "O42", true).
?test(sheet1_P42, "/LT/", "P42", true).
?test(sheet1_Q42, "/LT/", "Q42", true).
?test(sheet1_R42, "/LT/", "R42", true).
?test(sheet1_S42, "/LT/", "S42", true).
?test(sheet1_T42, "/LT/", "T42", false).
?test(sheet1_U42, "/LT/", "U42", true).
?test(sheet1_V42, "/LT/", "V42", true).
?test(sheet1_A43, "/LT/", "A43", "Float").
?test(sheet1_C43, "/LT/", "C43", false).
?test(sheet1_D43, "/LT/", "D43", true).
?test(sheet1_E43, "/LT/", "E43", true).
?test(sheet1_F43, "/LT/", "F43", '#DIV/0!').
?test(sheet1_G43, "/LT/", "G43", '#N/A').
?test(sheet1_H43, "/LT/", "H43", '#NAME?').
?test(sheet1_I43, "/LT/", "I43", 'NULL!').
?test(sheet1_J43, "/LT/", "J43", '#NUM!').
?test(sheet1_K43, "/LT/", "K43", '#REF!').
?test(sheet1_L43, "/LT/", "L43", '#VALUE!').
?test(sheet1_M43, "/LT/", "M43", true).
?test(sheet1_N43, "/LT/", "N43", true).
?test(sheet1_O43, "/LT/", "O43", true).
?test(sheet1_P43, "/LT/", "P43", true).
?test(sheet1_Q43, "/LT/", "Q43", true).
?test(sheet1_R43, "/LT/", "R43", true).
?test(sheet1_S43, "/LT/", "S43", true).
?test(sheet1_T43, "/LT/", "T43", false).
?test(sheet1_U43, "/LT/", "U43", false).
?test(sheet1_V43, "/LT/", "V43", true).
?test(sheet1_A44, "/LT/", "A44", "Float").
?test(sheet1_C44, "/LT/", "C44", false).
?test(sheet1_D44, "/LT/", "D44", true).
?test(sheet1_E44, "/LT/", "E44", true).
?test(sheet1_F44, "/LT/", "F44", '#DIV/0!').
?test(sheet1_G44, "/LT/", "G44", '#N/A').
?test(sheet1_H44, "/LT/", "H44", '#NAME?').
?test(sheet1_I44, "/LT/", "I44", 'NULL!').
?test(sheet1_J44, "/LT/", "J44", '#NUM!').
?test(sheet1_K44, "/LT/", "K44", '#REF!').
?test(sheet1_L44, "/LT/", "L44", '#VALUE!').
?test(sheet1_M44, "/LT/", "M44", true).
?test(sheet1_N44, "/LT/", "N44", true).
?test(sheet1_O44, "/LT/", "O44", true).
?test(sheet1_P44, "/LT/", "P44", true).
?test(sheet1_Q44, "/LT/", "Q44", true).
?test(sheet1_R44, "/LT/", "R44", false).
?test(sheet1_S44, "/LT/", "S44", false).
?test(sheet1_T44, "/LT/", "T44", false).
?test(sheet1_U44, "/LT/", "U44", false).
?test(sheet1_V44, "/LT/", "V44", false).
?test(sheet1_A47, "/LT/", "A47", 400.0).
?test(sheet1_C47, "/LT/", "C47", 1.0).
?test(sheet1_D47, "/LT/", "D47", 1.0).
?test(sheet1_E47, "/LT/", "E47", 1.0).
?test(sheet1_F47, "/LT/", "F47", 1.0).
?test(sheet1_G47, "/LT/", "G47", 1.0).
?test(sheet1_H47, "/LT/", "H47", 1.0).
?test(sheet1_I47, "/LT/", "I47", 1.0).
?test(sheet1_J47, "/LT/", "J47", 1.0).
?test(sheet1_K47, "/LT/", "K47", 1.0).
?test(sheet1_L47, "/LT/", "L47", 1.0).
?test(sheet1_M47, "/LT/", "M47", 1.0).
?test(sheet1_N47, "/LT/", "N47", 1.0).
?test(sheet1_O47, "/LT/", "O47", 1.0).
?test(sheet1_P47, "/LT/", "P47", 1.0).
?test(sheet1_Q47, "/LT/", "Q47", 1.0).
?test(sheet1_R47, "/LT/", "R47", 1.0).
?test(sheet1_S47, "/LT/", "S47", 1.0).
?test(sheet1_T47, "/LT/", "T47", 1.0).
?test(sheet1_U47, "/LT/", "U47", 1.0).
?test(sheet1_V47, "/LT/", "V47", 1.0).
?test(sheet1_A48, "/LT/", "A48", "Success").
?test(sheet1_C48, "/LT/", "C48", 1.0).
?test(sheet1_D48, "/LT/", "D48", 1.0).
?test(sheet1_E48, "/LT/", "E48", 1.0).
?test(sheet1_F48, "/LT/", "F48", 1.0).
?test(sheet1_G48, "/LT/", "G48", 1.0).
?test(sheet1_H48, "/LT/", "H48", 1.0).
?test(sheet1_I48, "/LT/", "I48", 1.0).
?test(sheet1_J48, "/LT/", "J48", 1.0).
?test(sheet1_K48, "/LT/", "K48", 1.0).
?test(sheet1_L48, "/LT/", "L48", 1.0).
?test(sheet1_M48, "/LT/", "M48", 1.0).
?test(sheet1_N48, "/LT/", "N48", 1.0).
?test(sheet1_O48, "/LT/", "O48", 1.0).
?test(sheet1_P48, "/LT/", "P48", 1.0).
?test(sheet1_Q48, "/LT/", "Q48", 1.0).
?test(sheet1_R48, "/LT/", "R48", 1.0).
?test(sheet1_S48, "/LT/", "S48", 1.0).
?test(sheet1_T48, "/LT/", "T48", 1.0).
?test(sheet1_U48, "/LT/", "U48", 1.0).
?test(sheet1_V48, "/LT/", "V48", 1.0).
?test(sheet1_C49, "/LT/", "C49", 1.0).
?test(sheet1_D49, "/LT/", "D49", 1.0).
?test(sheet1_E49, "/LT/", "E49", 1.0).
?test(sheet1_F49, "/LT/", "F49", 1.0).
?test(sheet1_G49, "/LT/", "G49", 1.0).
?test(sheet1_H49, "/LT/", "H49", 1.0).
?test(sheet1_I49, "/LT/", "I49", 1.0).
?test(sheet1_J49, "/LT/", "J49", 1.0).
?test(sheet1_K49, "/LT/", "K49", 1.0).
?test(sheet1_L49, "/LT/", "L49", 1.0).
?test(sheet1_M49, "/LT/", "M49", 1.0).
?test(sheet1_N49, "/LT/", "N49", 1.0).
?test(sheet1_O49, "/LT/", "O49", 1.0).
?test(sheet1_P49, "/LT/", "P49", 1.0).
?test(sheet1_Q49, "/LT/", "Q49", 1.0).
?test(sheet1_R49, "/LT/", "R49", 1.0).
?test(sheet1_S49, "/LT/", "S49", 1.0).
?test(sheet1_T49, "/LT/", "T49", 1.0).
?test(sheet1_U49, "/LT/", "U49", 1.0).
?test(sheet1_V49, "/LT/", "V49", 1.0).
?test(sheet1_C50, "/LT/", "C50", 1.0).
?test(sheet1_D50, "/LT/", "D50", 1.0).
?test(sheet1_E50, "/LT/", "E50", 1.0).
?test(sheet1_F50, "/LT/", "F50", 1.0).
?test(sheet1_G50, "/LT/", "G50", 1.0).
?test(sheet1_H50, "/LT/", "H50", 1.0).
?test(sheet1_I50, "/LT/", "I50", 1.0).
?test(sheet1_J50, "/LT/", "J50", 1.0).
?test(sheet1_K50, "/LT/", "K50", 1.0).
?test(sheet1_L50, "/LT/", "L50", 1.0).
?test(sheet1_M50, "/LT/", "M50", 1.0).
?test(sheet1_N50, "/LT/", "N50", 1.0).
?test(sheet1_O50, "/LT/", "O50", 1.0).
?test(sheet1_P50, "/LT/", "P50", 1.0).
?test(sheet1_Q50, "/LT/", "Q50", 1.0).
?test(sheet1_R50, "/LT/", "R50", 1.0).
?test(sheet1_S50, "/LT/", "S50", 1.0).
?test(sheet1_T50, "/LT/", "T50", 1.0).
?test(sheet1_U50, "/LT/", "U50", 1.0).
?test(sheet1_V50, "/LT/", "V50", 1.0).
?test(sheet1_C51, "/LT/", "C51", 1.0).
?test(sheet1_D51, "/LT/", "D51", 1.0).
?test(sheet1_E51, "/LT/", "E51", 1.0).
?test(sheet1_F51, "/LT/", "F51", 1.0).
?test(sheet1_G51, "/LT/", "G51", 1.0).
?test(sheet1_H51, "/LT/", "H51", 1.0).
?test(sheet1_I51, "/LT/", "I51", 1.0).
?test(sheet1_J51, "/LT/", "J51", 1.0).
?test(sheet1_K51, "/LT/", "K51", 1.0).
?test(sheet1_L51, "/LT/", "L51", 1.0).
?test(sheet1_M51, "/LT/", "M51", 1.0).
?test(sheet1_N51, "/LT/", "N51", 1.0).
?test(sheet1_O51, "/LT/", "O51", 1.0).
?test(sheet1_P51, "/LT/", "P51", 1.0).
?test(sheet1_Q51, "/LT/", "Q51", 1.0).
?test(sheet1_R51, "/LT/", "R51", 1.0).
?test(sheet1_S51, "/LT/", "S51", 1.0).
?test(sheet1_T51, "/LT/", "T51", 1.0).
?test(sheet1_U51, "/LT/", "U51", 1.0).
?test(sheet1_V51, "/LT/", "V51", 1.0).
?test(sheet1_C52, "/LT/", "C52", 1.0).
?test(sheet1_D52, "/LT/", "D52", 1.0).
?test(sheet1_E52, "/LT/", "E52", 1.0).
?test(sheet1_F52, "/LT/", "F52", 1.0).
?test(sheet1_G52, "/LT/", "G52", 1.0).
?test(sheet1_H52, "/LT/", "H52", 1.0).
?test(sheet1_I52, "/LT/", "I52", 1.0).
?test(sheet1_J52, "/LT/", "J52", 1.0).
?test(sheet1_K52, "/LT/", "K52", 1.0).
?test(sheet1_L52, "/LT/", "L52", 1.0).
?test(sheet1_M52, "/LT/", "M52", 1.0).
?test(sheet1_N52, "/LT/", "N52", 1.0).
?test(sheet1_O52, "/LT/", "O52", 1.0).
?test(sheet1_P52, "/LT/", "P52", 1.0).
?test(sheet1_Q52, "/LT/", "Q52", 1.0).
?test(sheet1_R52, "/LT/", "R52", 1.0).
?test(sheet1_S52, "/LT/", "S52", 1.0).
?test(sheet1_T52, "/LT/", "T52", 1.0).
?test(sheet1_U52, "/LT/", "U52", 1.0).
?test(sheet1_V52, "/LT/", "V52", 1.0).
?test(sheet1_C53, "/LT/", "C53", 1.0).
?test(sheet1_D53, "/LT/", "D53", 1.0).
?test(sheet1_E53, "/LT/", "E53", 1.0).
?test(sheet1_F53, "/LT/", "F53", 1.0).
?test(sheet1_G53, "/LT/", "G53", 1.0).
?test(sheet1_H53, "/LT/", "H53", 1.0).
?test(sheet1_I53, "/LT/", "I53", 1.0).
?test(sheet1_J53, "/LT/", "J53", 1.0).
?test(sheet1_K53, "/LT/", "K53", 1.0).
?test(sheet1_L53, "/LT/", "L53", 1.0).
?test(sheet1_M53, "/LT/", "M53", 1.0).
?test(sheet1_N53, "/LT/", "N53", 1.0).
?test(sheet1_O53, "/LT/", "O53", 1.0).
?test(sheet1_P53, "/LT/", "P53", 1.0).
?test(sheet1_Q53, "/LT/", "Q53", 1.0).
?test(sheet1_R53, "/LT/", "R53", 1.0).
?test(sheet1_S53, "/LT/", "S53", 1.0).
?test(sheet1_T53, "/LT/", "T53", 1.0).
?test(sheet1_U53, "/LT/", "U53", 1.0).
?test(sheet1_V53, "/LT/", "V53", 1.0).
?test(sheet1_C54, "/LT/", "C54", 1.0).
?test(sheet1_D54, "/LT/", "D54", 1.0).
?test(sheet1_E54, "/LT/", "E54", 1.0).
?test(sheet1_F54, "/LT/", "F54", 1.0).
?test(sheet1_G54, "/LT/", "G54", 1.0).
?test(sheet1_H54, "/LT/", "H54", 1.0).
?test(sheet1_I54, "/LT/", "I54", 1.0).
?test(sheet1_J54, "/LT/", "J54", 1.0).
?test(sheet1_K54, "/LT/", "K54", 1.0).
?test(sheet1_L54, "/LT/", "L54", 1.0).
?test(sheet1_M54, "/LT/", "M54", 1.0).
?test(sheet1_N54, "/LT/", "N54", 1.0).
?test(sheet1_O54, "/LT/", "O54", 1.0).
?test(sheet1_P54, "/LT/", "P54", 1.0).
?test(sheet1_Q54, "/LT/", "Q54", 1.0).
?test(sheet1_R54, "/LT/", "R54", 1.0).
?test(sheet1_S54, "/LT/", "S54", 1.0).
?test(sheet1_T54, "/LT/", "T54", 1.0).
?test(sheet1_U54, "/LT/", "U54", 1.0).
?test(sheet1_V54, "/LT/", "V54", 1.0).
?test(sheet1_C55, "/LT/", "C55", 1.0).
?test(sheet1_D55, "/LT/", "D55", 1.0).
?test(sheet1_E55, "/LT/", "E55", 1.0).
?test(sheet1_F55, "/LT/", "F55", 1.0).
?test(sheet1_G55, "/LT/", "G55", 1.0).
?test(sheet1_H55, "/LT/", "H55", 1.0).
?test(sheet1_I55, "/LT/", "I55", 1.0).
?test(sheet1_J55, "/LT/", "J55", 1.0).
?test(sheet1_K55, "/LT/", "K55", 1.0).
?test(sheet1_L55, "/LT/", "L55", 1.0).
?test(sheet1_M55, "/LT/", "M55", 1.0).
?test(sheet1_N55, "/LT/", "N55", 1.0).
?test(sheet1_O55, "/LT/", "O55", 1.0).
?test(sheet1_P55, "/LT/", "P55", 1.0).
?test(sheet1_Q55, "/LT/", "Q55", 1.0).
?test(sheet1_R55, "/LT/", "R55", 1.0).
?test(sheet1_S55, "/LT/", "S55", 1.0).
?test(sheet1_T55, "/LT/", "T55", 1.0).
?test(sheet1_U55, "/LT/", "U55", 1.0).
?test(sheet1_V55, "/LT/", "V55", 1.0).
?test(sheet1_C56, "/LT/", "C56", 1.0).
?test(sheet1_D56, "/LT/", "D56", 1.0).
?test(sheet1_E56, "/LT/", "E56", 1.0).
?test(sheet1_F56, "/LT/", "F56", 1.0).
?test(sheet1_G56, "/LT/", "G56", 1.0).
?test(sheet1_H56, "/LT/", "H56", 1.0).
?test(sheet1_I56, "/LT/", "I56", 1.0).
?test(sheet1_J56, "/LT/", "J56", 1.0).
?test(sheet1_K56, "/LT/", "K56", 1.0).
?test(sheet1_L56, "/LT/", "L56", 1.0).
?test(sheet1_M56, "/LT/", "M56", 1.0).
?test(sheet1_N56, "/LT/", "N56", 1.0).
?test(sheet1_O56, "/LT/", "O56", 1.0).
?test(sheet1_P56, "/LT/", "P56", 1.0).
?test(sheet1_Q56, "/LT/", "Q56", 1.0).
?test(sheet1_R56, "/LT/", "R56", 1.0).
?test(sheet1_S56, "/LT/", "S56", 1.0).
?test(sheet1_T56, "/LT/", "T56", 1.0).
?test(sheet1_U56, "/LT/", "U56", 1.0).
?test(sheet1_V56, "/LT/", "V56", 1.0).
?test(sheet1_C57, "/LT/", "C57", 1.0).
?test(sheet1_D57, "/LT/", "D57", 1.0).
?test(sheet1_E57, "/LT/", "E57", 1.0).
?test(sheet1_F57, "/LT/", "F57", 1.0).
?test(sheet1_G57, "/LT/", "G57", 1.0).
?test(sheet1_H57, "/LT/", "H57", 1.0).
?test(sheet1_I57, "/LT/", "I57", 1.0).
?test(sheet1_J57, "/LT/", "J57", 1.0).
?test(sheet1_K57, "/LT/", "K57", 1.0).
?test(sheet1_L57, "/LT/", "L57", 1.0).
?test(sheet1_M57, "/LT/", "M57", 1.0).
?test(sheet1_N57, "/LT/", "N57", 1.0).
?test(sheet1_O57, "/LT/", "O57", 1.0).
?test(sheet1_P57, "/LT/", "P57", 1.0).
?test(sheet1_Q57, "/LT/", "Q57", 1.0).
?test(sheet1_R57, "/LT/", "R57", 1.0).
?test(sheet1_S57, "/LT/", "S57", 1.0).
?test(sheet1_T57, "/LT/", "T57", 1.0).
?test(sheet1_U57, "/LT/", "U57", 1.0).
?test(sheet1_V57, "/LT/", "V57", 1.0).
?test(sheet1_C58, "/LT/", "C58", 1.0).
?test(sheet1_D58, "/LT/", "D58", 1.0).
?test(sheet1_E58, "/LT/", "E58", 1.0).
?test(sheet1_F58, "/LT/", "F58", 1.0).
?test(sheet1_G58, "/LT/", "G58", 1.0).
?test(sheet1_H58, "/LT/", "H58", 1.0).
?test(sheet1_I58, "/LT/", "I58", 1.0).
?test(sheet1_J58, "/LT/", "J58", 1.0).
?test(sheet1_K58, "/LT/", "K58", 1.0).
?test(sheet1_L58, "/LT/", "L58", 1.0).
?test(sheet1_M58, "/LT/", "M58", 1.0).
?test(sheet1_N58, "/LT/", "N58", 1.0).
?test(sheet1_O58, "/LT/", "O58", 1.0).
?test(sheet1_P58, "/LT/", "P58", 1.0).
?test(sheet1_Q58, "/LT/", "Q58", 1.0).
?test(sheet1_R58, "/LT/", "R58", 1.0).
?test(sheet1_S58, "/LT/", "S58", 1.0).
?test(sheet1_T58, "/LT/", "T58", 1.0).
?test(sheet1_U58, "/LT/", "U58", 1.0).
?test(sheet1_V58, "/LT/", "V58", 1.0).
?test(sheet1_C59, "/LT/", "C59", 1.0).
?test(sheet1_D59, "/LT/", "D59", 1.0).
?test(sheet1_E59, "/LT/", "E59", 1.0).
?test(sheet1_F59, "/LT/", "F59", 1.0).
?test(sheet1_G59, "/LT/", "G59", 1.0).
?test(sheet1_H59, "/LT/", "H59", 1.0).
?test(sheet1_I59, "/LT/", "I59", 1.0).
?test(sheet1_J59, "/LT/", "J59", 1.0).
?test(sheet1_K59, "/LT/", "K59", 1.0).
?test(sheet1_L59, "/LT/", "L59", 1.0).
?test(sheet1_M59, "/LT/", "M59", 1.0).
?test(sheet1_N59, "/LT/", "N59", 1.0).
?test(sheet1_O59, "/LT/", "O59", 1.0).
?test(sheet1_P59, "/LT/", "P59", 1.0).
?test(sheet1_Q59, "/LT/", "Q59", 1.0).
?test(sheet1_R59, "/LT/", "R59", 1.0).
?test(sheet1_S59, "/LT/", "S59", 1.0).
?test(sheet1_T59, "/LT/", "T59", 1.0).
?test(sheet1_U59, "/LT/", "U59", 1.0).
?test(sheet1_V59, "/LT/", "V59", 1.0).
?test(sheet1_C60, "/LT/", "C60", 1.0).
?test(sheet1_D60, "/LT/", "D60", 1.0).
?test(sheet1_E60, "/LT/", "E60", 1.0).
?test(sheet1_F60, "/LT/", "F60", 1.0).
?test(sheet1_G60, "/LT/", "G60", 1.0).
?test(sheet1_H60, "/LT/", "H60", 1.0).
?test(sheet1_I60, "/LT/", "I60", 1.0).
?test(sheet1_J60, "/LT/", "J60", 1.0).
?test(sheet1_K60, "/LT/", "K60", 1.0).
?test(sheet1_L60, "/LT/", "L60", 1.0).
?test(sheet1_M60, "/LT/", "M60", 1.0).
?test(sheet1_N60, "/LT/", "N60", 1.0).
?test(sheet1_O60, "/LT/", "O60", 1.0).
?test(sheet1_P60, "/LT/", "P60", 1.0).
?test(sheet1_Q60, "/LT/", "Q60", 1.0).
?test(sheet1_R60, "/LT/", "R60", 1.0).
?test(sheet1_S60, "/LT/", "S60", 1.0).
?test(sheet1_T60, "/LT/", "T60", 1.0).
?test(sheet1_U60, "/LT/", "U60", 1.0).
?test(sheet1_V60, "/LT/", "V60", 1.0).
?test(sheet1_C61, "/LT/", "C61", 1.0).
?test(sheet1_D61, "/LT/", "D61", 1.0).
?test(sheet1_E61, "/LT/", "E61", 1.0).
?test(sheet1_F61, "/LT/", "F61", 1.0).
?test(sheet1_G61, "/LT/", "G61", 1.0).
?test(sheet1_H61, "/LT/", "H61", 1.0).
?test(sheet1_I61, "/LT/", "I61", 1.0).
?test(sheet1_J61, "/LT/", "J61", 1.0).
?test(sheet1_K61, "/LT/", "K61", 1.0).
?test(sheet1_L61, "/LT/", "L61", 1.0).
?test(sheet1_M61, "/LT/", "M61", 1.0).
?test(sheet1_N61, "/LT/", "N61", 1.0).
?test(sheet1_O61, "/LT/", "O61", 1.0).
?test(sheet1_P61, "/LT/", "P61", 1.0).
?test(sheet1_Q61, "/LT/", "Q61", 1.0).
?test(sheet1_R61, "/LT/", "R61", 1.0).
?test(sheet1_S61, "/LT/", "S61", 1.0).
?test(sheet1_T61, "/LT/", "T61", 1.0).
?test(sheet1_U61, "/LT/", "U61", 1.0).
?test(sheet1_V61, "/LT/", "V61", 1.0).
?test(sheet1_C62, "/LT/", "C62", 1.0).
?test(sheet1_D62, "/LT/", "D62", 1.0).
?test(sheet1_E62, "/LT/", "E62", 1.0).
?test(sheet1_F62, "/LT/", "F62", 1.0).
?test(sheet1_G62, "/LT/", "G62", 1.0).
?test(sheet1_H62, "/LT/", "H62", 1.0).
?test(sheet1_I62, "/LT/", "I62", 1.0).
?test(sheet1_J62, "/LT/", "J62", 1.0).
?test(sheet1_K62, "/LT/", "K62", 1.0).
?test(sheet1_L62, "/LT/", "L62", 1.0).
?test(sheet1_M62, "/LT/", "M62", 1.0).
?test(sheet1_N62, "/LT/", "N62", 1.0).
?test(sheet1_O62, "/LT/", "O62", 1.0).
?test(sheet1_P62, "/LT/", "P62", 1.0).
?test(sheet1_Q62, "/LT/", "Q62", 1.0).
?test(sheet1_R62, "/LT/", "R62", 1.0).
?test(sheet1_S62, "/LT/", "S62", 1.0).
?test(sheet1_T62, "/LT/", "T62", 1.0).
?test(sheet1_U62, "/LT/", "U62", 1.0).
?test(sheet1_V62, "/LT/", "V62", 1.0).
?test(sheet1_C63, "/LT/", "C63", 1.0).
?test(sheet1_D63, "/LT/", "D63", 1.0).
?test(sheet1_E63, "/LT/", "E63", 1.0).
?test(sheet1_F63, "/LT/", "F63", 1.0).
?test(sheet1_G63, "/LT/", "G63", 1.0).
?test(sheet1_H63, "/LT/", "H63", 1.0).
?test(sheet1_I63, "/LT/", "I63", 1.0).
?test(sheet1_J63, "/LT/", "J63", 1.0).
?test(sheet1_K63, "/LT/", "K63", 1.0).
?test(sheet1_L63, "/LT/", "L63", 1.0).
?test(sheet1_M63, "/LT/", "M63", 1.0).
?test(sheet1_N63, "/LT/", "N63", 1.0).
?test(sheet1_O63, "/LT/", "O63", 1.0).
?test(sheet1_P63, "/LT/", "P63", 1.0).
?test(sheet1_Q63, "/LT/", "Q63", 1.0).
?test(sheet1_R63, "/LT/", "R63", 1.0).
?test(sheet1_S63, "/LT/", "S63", 1.0).
?test(sheet1_T63, "/LT/", "T63", 1.0).
?test(sheet1_U63, "/LT/", "U63", 1.0).
?test(sheet1_V63, "/LT/", "V63", 1.0).
?test(sheet1_C64, "/LT/", "C64", 1.0).
?test(sheet1_D64, "/LT/", "D64", 1.0).
?test(sheet1_E64, "/LT/", "E64", 1.0).
?test(sheet1_F64, "/LT/", "F64", 1.0).
?test(sheet1_G64, "/LT/", "G64", 1.0).
?test(sheet1_H64, "/LT/", "H64", 1.0).
?test(sheet1_I64, "/LT/", "I64", 1.0).
?test(sheet1_J64, "/LT/", "J64", 1.0).
?test(sheet1_K64, "/LT/", "K64", 1.0).
?test(sheet1_L64, "/LT/", "L64", 1.0).
?test(sheet1_M64, "/LT/", "M64", 1.0).
?test(sheet1_N64, "/LT/", "N64", 1.0).
?test(sheet1_O64, "/LT/", "O64", 1.0).
?test(sheet1_P64, "/LT/", "P64", 1.0).
?test(sheet1_Q64, "/LT/", "Q64", 1.0).
?test(sheet1_R64, "/LT/", "R64", 1.0).
?test(sheet1_S64, "/LT/", "S64", 1.0).
?test(sheet1_T64, "/LT/", "T64", 1.0).
?test(sheet1_U64, "/LT/", "U64", 1.0).
?test(sheet1_V64, "/LT/", "V64", 1.0).
?test(sheet1_C65, "/LT/", "C65", 1.0).
?test(sheet1_D65, "/LT/", "D65", 1.0).
?test(sheet1_E65, "/LT/", "E65", 1.0).
?test(sheet1_F65, "/LT/", "F65", 1.0).
?test(sheet1_G65, "/LT/", "G65", 1.0).
?test(sheet1_H65, "/LT/", "H65", 1.0).
?test(sheet1_I65, "/LT/", "I65", 1.0).
?test(sheet1_J65, "/LT/", "J65", 1.0).
?test(sheet1_K65, "/LT/", "K65", 1.0).
?test(sheet1_L65, "/LT/", "L65", 1.0).
?test(sheet1_M65, "/LT/", "M65", 1.0).
?test(sheet1_N65, "/LT/", "N65", 1.0).
?test(sheet1_O65, "/LT/", "O65", 1.0).
?test(sheet1_P65, "/LT/", "P65", 1.0).
?test(sheet1_Q65, "/LT/", "Q65", 1.0).
?test(sheet1_R65, "/LT/", "R65", 1.0).
?test(sheet1_S65, "/LT/", "S65", 1.0).
?test(sheet1_T65, "/LT/", "T65", 1.0).
?test(sheet1_U65, "/LT/", "U65", 1.0).
?test(sheet1_V65, "/LT/", "V65", 1.0).
?test(sheet1_C66, "/LT/", "C66", 1.0).
?test(sheet1_D66, "/LT/", "D66", 1.0).
?test(sheet1_E66, "/LT/", "E66", 1.0).
?test(sheet1_F66, "/LT/", "F66", 1.0).
?test(sheet1_G66, "/LT/", "G66", 1.0).
?test(sheet1_H66, "/LT/", "H66", 1.0).
?test(sheet1_I66, "/LT/", "I66", 1.0).
?test(sheet1_J66, "/LT/", "J66", 1.0).
?test(sheet1_K66, "/LT/", "K66", 1.0).
?test(sheet1_L66, "/LT/", "L66", 1.0).
?test(sheet1_M66, "/LT/", "M66", 1.0).
?test(sheet1_N66, "/LT/", "N66", 1.0).
?test(sheet1_O66, "/LT/", "O66", 1.0).
?test(sheet1_P66, "/LT/", "P66", 1.0).
?test(sheet1_Q66, "/LT/", "Q66", 1.0).
?test(sheet1_R66, "/LT/", "R66", 1.0).
?test(sheet1_S66, "/LT/", "S66", 1.0).
?test(sheet1_T66, "/LT/", "T66", 1.0).
?test(sheet1_U66, "/LT/", "U66", 1.0).
?test(sheet1_V66, "/LT/", "V66", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_operators_lt.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_operators_lt" ++ "/" ++ Sheetname ++ "/",
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
