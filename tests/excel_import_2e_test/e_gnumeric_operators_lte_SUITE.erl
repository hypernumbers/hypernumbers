%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_operators_lte_SUITE).
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
                     [Testcase, "e_gnumeric_operators_lte_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_operators_lte" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/LTE/", "A1", "<=").
?test(sheet1_B1, "/LTE/", "B1", "B").
?test(sheet1_C1, "/LTE/", "C1", "Blank").
?test(sheet1_D1, "/LTE/", "D1", "Boolean").
?test(sheet1_E1, "/LTE/", "E1", "Boolean").
?test(sheet1_F1, "/LTE/", "F1", "Error").
?test(sheet1_G1, "/LTE/", "G1", "Error").
?test(sheet1_H1, "/LTE/", "H1", "Error").
?test(sheet1_I1, "/LTE/", "I1", "Error").
?test(sheet1_J1, "/LTE/", "J1", "Error").
?test(sheet1_K1, "/LTE/", "K1", "Error").
?test(sheet1_L1, "/LTE/", "L1", "Error").
?test(sheet1_M1, "/LTE/", "M1", "String").
?test(sheet1_N1, "/LTE/", "N1", "String").
?test(sheet1_O1, "/LTE/", "O1", "String").
?test(sheet1_P1, "/LTE/", "P1", "Str Num").
?test(sheet1_Q1, "/LTE/", "Q1", "Str Num").
?test(sheet1_R1, "/LTE/", "R1", "Integer").
?test(sheet1_S1, "/LTE/", "S1", "Integer").
?test(sheet1_T1, "/LTE/", "T1", "Zero").
?test(sheet1_U1, "/LTE/", "U1", "Float").
?test(sheet1_V1, "/LTE/", "V1", "Float").
?test(sheet1_A2, "/LTE/", "A2", "A").
?test(sheet1_D2, "/LTE/", "D2", true).
?test(sheet1_E2, "/LTE/", "E2", false).
?test(sheet1_F2, "/LTE/", "F2", '#DIV/0!').
?test(sheet1_G2, "/LTE/", "G2", '#N/A').
?test(sheet1_H2, "/LTE/", "H2", '#NAME?').
?test(sheet1_I2, "/LTE/", "I2", 'NULL!').
?test(sheet1_J2, "/LTE/", "J2", '#NUM!').
?test(sheet1_K2, "/LTE/", "K2", '#REF!').
?test(sheet1_L2, "/LTE/", "L2", '#VALUE!').
?test(sheet1_M2, "/LTE/", "M2", "Liz").
?test(sheet1_N2, "/LTE/", "N2", "Doug").
?test(sheet1_O2, "/LTE/", "O2", "Bob").
?test(sheet1_P2, "/LTE/", "P2", "2.7").
?test(sheet1_Q2, "/LTE/", "Q2", "3.54").
?test(sheet1_R2, "/LTE/", "R2", "1999/02/01 00:00:00").
?test(sheet1_S2, "/LTE/", "S2", "1999/02/02 00:00:00").
?test(sheet1_T2, "/LTE/", "T2", 0.0).
?test(sheet1_U2, "/LTE/", "U2", 3.1415).
?test(sheet1_V2, "/LTE/", "V2", 36193.2).
?test(sheet1_A3, "/LTE/", "A3", "Blank").
?test(sheet1_C3, "/LTE/", "C3", true).
?test(sheet1_D3, "/LTE/", "D3", true).
?test(sheet1_E3, "/LTE/", "E3", true).
?test(sheet1_F3, "/LTE/", "F3", '#DIV/0!').
?test(sheet1_G3, "/LTE/", "G3", '#N/A').
?test(sheet1_H3, "/LTE/", "H3", '#NAME?').
?test(sheet1_I3, "/LTE/", "I3", 'NULL!').
?test(sheet1_J3, "/LTE/", "J3", '#NUM!').
?test(sheet1_K3, "/LTE/", "K3", '#REF!').
?test(sheet1_L3, "/LTE/", "L3", '#VALUE!').
?test(sheet1_M3, "/LTE/", "M3", true).
?test(sheet1_N3, "/LTE/", "N3", true).
?test(sheet1_O3, "/LTE/", "O3", true).
?test(sheet1_P3, "/LTE/", "P3", true).
?test(sheet1_Q3, "/LTE/", "Q3", true).
?test(sheet1_R3, "/LTE/", "R3", true).
?test(sheet1_S3, "/LTE/", "S3", true).
?test(sheet1_T3, "/LTE/", "T3", true).
?test(sheet1_U3, "/LTE/", "U3", true).
?test(sheet1_V3, "/LTE/", "V3", true).
?test(sheet1_A4, "/LTE/", "A4", "Boolean").
?test(sheet1_B4, "/LTE/", "B4", true).
?test(sheet1_C4, "/LTE/", "C4", false).
?test(sheet1_D4, "/LTE/", "D4", true).
?test(sheet1_E4, "/LTE/", "E4", false).
?test(sheet1_F4, "/LTE/", "F4", '#DIV/0!').
?test(sheet1_G4, "/LTE/", "G4", '#N/A').
?test(sheet1_H4, "/LTE/", "H4", '#NAME?').
?test(sheet1_I4, "/LTE/", "I4", 'NULL!').
?test(sheet1_J4, "/LTE/", "J4", '#NUM!').
?test(sheet1_K4, "/LTE/", "K4", '#REF!').
?test(sheet1_L4, "/LTE/", "L4", '#VALUE!').
?test(sheet1_M4, "/LTE/", "M4", false).
?test(sheet1_N4, "/LTE/", "N4", false).
?test(sheet1_O4, "/LTE/", "O4", false).
?test(sheet1_P4, "/LTE/", "P4", false).
?test(sheet1_Q4, "/LTE/", "Q4", false).
?test(sheet1_R4, "/LTE/", "R4", false).
?test(sheet1_S4, "/LTE/", "S4", false).
?test(sheet1_T4, "/LTE/", "T4", false).
?test(sheet1_U4, "/LTE/", "U4", false).
?test(sheet1_V4, "/LTE/", "V4", false).
?test(sheet1_A5, "/LTE/", "A5", "Boolean").
?test(sheet1_B5, "/LTE/", "B5", false).
?test(sheet1_C5, "/LTE/", "C5", true).
?test(sheet1_D5, "/LTE/", "D5", true).
?test(sheet1_E5, "/LTE/", "E5", true).
?test(sheet1_F5, "/LTE/", "F5", '#DIV/0!').
?test(sheet1_G5, "/LTE/", "G5", '#N/A').
?test(sheet1_H5, "/LTE/", "H5", '#NAME?').
?test(sheet1_I5, "/LTE/", "I5", 'NULL!').
?test(sheet1_J5, "/LTE/", "J5", '#NUM!').
?test(sheet1_K5, "/LTE/", "K5", '#REF!').
?test(sheet1_L5, "/LTE/", "L5", '#VALUE!').
?test(sheet1_M5, "/LTE/", "M5", false).
?test(sheet1_N5, "/LTE/", "N5", false).
?test(sheet1_O5, "/LTE/", "O5", false).
?test(sheet1_P5, "/LTE/", "P5", false).
?test(sheet1_Q5, "/LTE/", "Q5", false).
?test(sheet1_R5, "/LTE/", "R5", false).
?test(sheet1_S5, "/LTE/", "S5", false).
?test(sheet1_T5, "/LTE/", "T5", false).
?test(sheet1_U5, "/LTE/", "U5", false).
?test(sheet1_V5, "/LTE/", "V5", false).
?test(sheet1_A6, "/LTE/", "A6", "Error").
?test(sheet1_B6, "/LTE/", "B6", '#DIV/0!').
?test(sheet1_C6, "/LTE/", "C6", '#DIV/0!').
?test(sheet1_D6, "/LTE/", "D6", '#DIV/0!').
?test(sheet1_E6, "/LTE/", "E6", '#DIV/0!').
?test(sheet1_F6, "/LTE/", "F6", '#DIV/0!').
?test(sheet1_G6, "/LTE/", "G6", '#DIV/0!').
?test(sheet1_H6, "/LTE/", "H6", '#DIV/0!').
?test(sheet1_I6, "/LTE/", "I6", '#DIV/0!').
?test(sheet1_J6, "/LTE/", "J6", '#DIV/0!').
?test(sheet1_K6, "/LTE/", "K6", '#DIV/0!').
?test(sheet1_L6, "/LTE/", "L6", '#DIV/0!').
?test(sheet1_M6, "/LTE/", "M6", '#DIV/0!').
?test(sheet1_N6, "/LTE/", "N6", '#DIV/0!').
?test(sheet1_O6, "/LTE/", "O6", '#DIV/0!').
?test(sheet1_P6, "/LTE/", "P6", '#DIV/0!').
?test(sheet1_Q6, "/LTE/", "Q6", '#DIV/0!').
?test(sheet1_R6, "/LTE/", "R6", '#DIV/0!').
?test(sheet1_S6, "/LTE/", "S6", '#DIV/0!').
?test(sheet1_T6, "/LTE/", "T6", '#DIV/0!').
?test(sheet1_U6, "/LTE/", "U6", '#DIV/0!').
?test(sheet1_V6, "/LTE/", "V6", '#DIV/0!').
?test(sheet1_A7, "/LTE/", "A7", "Error").
?test(sheet1_B7, "/LTE/", "B7", '#N/A').
?test(sheet1_C7, "/LTE/", "C7", '#N/A').
?test(sheet1_D7, "/LTE/", "D7", '#N/A').
?test(sheet1_E7, "/LTE/", "E7", '#N/A').
?test(sheet1_F7, "/LTE/", "F7", '#N/A').
?test(sheet1_G7, "/LTE/", "G7", '#N/A').
?test(sheet1_H7, "/LTE/", "H7", '#N/A').
?test(sheet1_I7, "/LTE/", "I7", '#N/A').
?test(sheet1_J7, "/LTE/", "J7", '#N/A').
?test(sheet1_K7, "/LTE/", "K7", '#N/A').
?test(sheet1_L7, "/LTE/", "L7", '#N/A').
?test(sheet1_M7, "/LTE/", "M7", '#N/A').
?test(sheet1_N7, "/LTE/", "N7", '#N/A').
?test(sheet1_O7, "/LTE/", "O7", '#N/A').
?test(sheet1_P7, "/LTE/", "P7", '#N/A').
?test(sheet1_Q7, "/LTE/", "Q7", '#N/A').
?test(sheet1_R7, "/LTE/", "R7", '#N/A').
?test(sheet1_S7, "/LTE/", "S7", '#N/A').
?test(sheet1_T7, "/LTE/", "T7", '#N/A').
?test(sheet1_U7, "/LTE/", "U7", '#N/A').
?test(sheet1_V7, "/LTE/", "V7", '#N/A').
?test(sheet1_A8, "/LTE/", "A8", "Error").
?test(sheet1_B8, "/LTE/", "B8", '#NAME?').
?test(sheet1_C8, "/LTE/", "C8", '#NAME?').
?test(sheet1_D8, "/LTE/", "D8", '#NAME?').
?test(sheet1_E8, "/LTE/", "E8", '#NAME?').
?test(sheet1_F8, "/LTE/", "F8", '#NAME?').
?test(sheet1_G8, "/LTE/", "G8", '#NAME?').
?test(sheet1_H8, "/LTE/", "H8", '#NAME?').
?test(sheet1_I8, "/LTE/", "I8", '#NAME?').
?test(sheet1_J8, "/LTE/", "J8", '#NAME?').
?test(sheet1_K8, "/LTE/", "K8", '#NAME?').
?test(sheet1_L8, "/LTE/", "L8", '#NAME?').
?test(sheet1_M8, "/LTE/", "M8", '#NAME?').
?test(sheet1_N8, "/LTE/", "N8", '#NAME?').
?test(sheet1_O8, "/LTE/", "O8", '#NAME?').
?test(sheet1_P8, "/LTE/", "P8", '#NAME?').
?test(sheet1_Q8, "/LTE/", "Q8", '#NAME?').
?test(sheet1_R8, "/LTE/", "R8", '#NAME?').
?test(sheet1_S8, "/LTE/", "S8", '#NAME?').
?test(sheet1_T8, "/LTE/", "T8", '#NAME?').
?test(sheet1_U8, "/LTE/", "U8", '#NAME?').
?test(sheet1_V8, "/LTE/", "V8", '#NAME?').
?test(sheet1_A9, "/LTE/", "A9", "Error").
?test(sheet1_B9, "/LTE/", "B9", 'NULL!').
?test(sheet1_C9, "/LTE/", "C9", 'NULL!').
?test(sheet1_D9, "/LTE/", "D9", 'NULL!').
?test(sheet1_E9, "/LTE/", "E9", 'NULL!').
?test(sheet1_F9, "/LTE/", "F9", 'NULL!').
?test(sheet1_G9, "/LTE/", "G9", 'NULL!').
?test(sheet1_H9, "/LTE/", "H9", 'NULL!').
?test(sheet1_I9, "/LTE/", "I9", 'NULL!').
?test(sheet1_J9, "/LTE/", "J9", 'NULL!').
?test(sheet1_K9, "/LTE/", "K9", 'NULL!').
?test(sheet1_L9, "/LTE/", "L9", 'NULL!').
?test(sheet1_M9, "/LTE/", "M9", 'NULL!').
?test(sheet1_N9, "/LTE/", "N9", 'NULL!').
?test(sheet1_O9, "/LTE/", "O9", 'NULL!').
?test(sheet1_P9, "/LTE/", "P9", 'NULL!').
?test(sheet1_Q9, "/LTE/", "Q9", 'NULL!').
?test(sheet1_R9, "/LTE/", "R9", 'NULL!').
?test(sheet1_S9, "/LTE/", "S9", 'NULL!').
?test(sheet1_T9, "/LTE/", "T9", 'NULL!').
?test(sheet1_U9, "/LTE/", "U9", 'NULL!').
?test(sheet1_V9, "/LTE/", "V9", 'NULL!').
?test(sheet1_A10, "/LTE/", "A10", "Error").
?test(sheet1_B10, "/LTE/", "B10", '#NUM!').
?test(sheet1_C10, "/LTE/", "C10", '#NUM!').
?test(sheet1_D10, "/LTE/", "D10", '#NUM!').
?test(sheet1_E10, "/LTE/", "E10", '#NUM!').
?test(sheet1_F10, "/LTE/", "F10", '#NUM!').
?test(sheet1_G10, "/LTE/", "G10", '#NUM!').
?test(sheet1_H10, "/LTE/", "H10", '#NUM!').
?test(sheet1_I10, "/LTE/", "I10", '#NUM!').
?test(sheet1_J10, "/LTE/", "J10", '#NUM!').
?test(sheet1_K10, "/LTE/", "K10", '#NUM!').
?test(sheet1_L10, "/LTE/", "L10", '#NUM!').
?test(sheet1_M10, "/LTE/", "M10", '#NUM!').
?test(sheet1_N10, "/LTE/", "N10", '#NUM!').
?test(sheet1_O10, "/LTE/", "O10", '#NUM!').
?test(sheet1_P10, "/LTE/", "P10", '#NUM!').
?test(sheet1_Q10, "/LTE/", "Q10", '#NUM!').
?test(sheet1_R10, "/LTE/", "R10", '#NUM!').
?test(sheet1_S10, "/LTE/", "S10", '#NUM!').
?test(sheet1_T10, "/LTE/", "T10", '#NUM!').
?test(sheet1_U10, "/LTE/", "U10", '#NUM!').
?test(sheet1_V10, "/LTE/", "V10", '#NUM!').
?test(sheet1_A11, "/LTE/", "A11", "Error").
?test(sheet1_B11, "/LTE/", "B11", '#REF!').
?test(sheet1_C11, "/LTE/", "C11", '#REF!').
?test(sheet1_D11, "/LTE/", "D11", '#REF!').
?test(sheet1_E11, "/LTE/", "E11", '#REF!').
?test(sheet1_F11, "/LTE/", "F11", '#REF!').
?test(sheet1_G11, "/LTE/", "G11", '#REF!').
?test(sheet1_H11, "/LTE/", "H11", '#REF!').
?test(sheet1_I11, "/LTE/", "I11", '#REF!').
?test(sheet1_J11, "/LTE/", "J11", '#REF!').
?test(sheet1_K11, "/LTE/", "K11", '#REF!').
?test(sheet1_L11, "/LTE/", "L11", '#REF!').
?test(sheet1_M11, "/LTE/", "M11", '#REF!').
?test(sheet1_N11, "/LTE/", "N11", '#REF!').
?test(sheet1_O11, "/LTE/", "O11", '#REF!').
?test(sheet1_P11, "/LTE/", "P11", '#REF!').
?test(sheet1_Q11, "/LTE/", "Q11", '#REF!').
?test(sheet1_R11, "/LTE/", "R11", '#REF!').
?test(sheet1_S11, "/LTE/", "S11", '#REF!').
?test(sheet1_T11, "/LTE/", "T11", '#REF!').
?test(sheet1_U11, "/LTE/", "U11", '#REF!').
?test(sheet1_V11, "/LTE/", "V11", '#REF!').
?test(sheet1_A12, "/LTE/", "A12", "Error").
?test(sheet1_B12, "/LTE/", "B12", '#VALUE!').
?test(sheet1_C12, "/LTE/", "C12", '#VALUE!').
?test(sheet1_D12, "/LTE/", "D12", '#VALUE!').
?test(sheet1_E12, "/LTE/", "E12", '#VALUE!').
?test(sheet1_F12, "/LTE/", "F12", '#VALUE!').
?test(sheet1_G12, "/LTE/", "G12", '#VALUE!').
?test(sheet1_H12, "/LTE/", "H12", '#VALUE!').
?test(sheet1_I12, "/LTE/", "I12", '#VALUE!').
?test(sheet1_J12, "/LTE/", "J12", '#VALUE!').
?test(sheet1_K12, "/LTE/", "K12", '#VALUE!').
?test(sheet1_L12, "/LTE/", "L12", '#VALUE!').
?test(sheet1_M12, "/LTE/", "M12", '#VALUE!').
?test(sheet1_N12, "/LTE/", "N12", '#VALUE!').
?test(sheet1_O12, "/LTE/", "O12", '#VALUE!').
?test(sheet1_P12, "/LTE/", "P12", '#VALUE!').
?test(sheet1_Q12, "/LTE/", "Q12", '#VALUE!').
?test(sheet1_R12, "/LTE/", "R12", '#VALUE!').
?test(sheet1_S12, "/LTE/", "S12", '#VALUE!').
?test(sheet1_T12, "/LTE/", "T12", '#VALUE!').
?test(sheet1_U12, "/LTE/", "U12", '#VALUE!').
?test(sheet1_V12, "/LTE/", "V12", '#VALUE!').
?test(sheet1_A13, "/LTE/", "A13", "String").
?test(sheet1_B13, "/LTE/", "B13", "Liz").
?test(sheet1_C13, "/LTE/", "C13", false).
?test(sheet1_D13, "/LTE/", "D13", true).
?test(sheet1_E13, "/LTE/", "E13", true).
?test(sheet1_F13, "/LTE/", "F13", '#DIV/0!').
?test(sheet1_G13, "/LTE/", "G13", '#N/A').
?test(sheet1_H13, "/LTE/", "H13", '#NAME?').
?test(sheet1_I13, "/LTE/", "I13", 'NULL!').
?test(sheet1_J13, "/LTE/", "J13", '#NUM!').
?test(sheet1_K13, "/LTE/", "K13", '#REF!').
?test(sheet1_L13, "/LTE/", "L13", '#VALUE!').
?test(sheet1_M13, "/LTE/", "M13", true).
?test(sheet1_N13, "/LTE/", "N13", false).
?test(sheet1_O13, "/LTE/", "O13", false).
?test(sheet1_P13, "/LTE/", "P13", false).
?test(sheet1_Q13, "/LTE/", "Q13", false).
?test(sheet1_R13, "/LTE/", "R13", false).
?test(sheet1_S13, "/LTE/", "S13", false).
?test(sheet1_T13, "/LTE/", "T13", false).
?test(sheet1_U13, "/LTE/", "U13", false).
?test(sheet1_V13, "/LTE/", "V13", false).
?test(sheet1_A14, "/LTE/", "A14", "String").
?test(sheet1_B14, "/LTE/", "B14", "Doug").
?test(sheet1_C14, "/LTE/", "C14", false).
?test(sheet1_D14, "/LTE/", "D14", true).
?test(sheet1_E14, "/LTE/", "E14", true).
?test(sheet1_F14, "/LTE/", "F14", '#DIV/0!').
?test(sheet1_G14, "/LTE/", "G14", '#N/A').
?test(sheet1_H14, "/LTE/", "H14", '#NAME?').
?test(sheet1_I14, "/LTE/", "I14", 'NULL!').
?test(sheet1_J14, "/LTE/", "J14", '#NUM!').
?test(sheet1_K14, "/LTE/", "K14", '#REF!').
?test(sheet1_L14, "/LTE/", "L14", '#VALUE!').
?test(sheet1_M14, "/LTE/", "M14", true).
?test(sheet1_N14, "/LTE/", "N14", true).
?test(sheet1_O14, "/LTE/", "O14", false).
?test(sheet1_P14, "/LTE/", "P14", false).
?test(sheet1_Q14, "/LTE/", "Q14", false).
?test(sheet1_R14, "/LTE/", "R14", false).
?test(sheet1_S14, "/LTE/", "S14", false).
?test(sheet1_T14, "/LTE/", "T14", false).
?test(sheet1_U14, "/LTE/", "U14", false).
?test(sheet1_V14, "/LTE/", "V14", false).
?test(sheet1_A15, "/LTE/", "A15", "String").
?test(sheet1_B15, "/LTE/", "B15", "Bob").
?test(sheet1_C15, "/LTE/", "C15", false).
?test(sheet1_D15, "/LTE/", "D15", true).
?test(sheet1_E15, "/LTE/", "E15", true).
?test(sheet1_F15, "/LTE/", "F15", '#DIV/0!').
?test(sheet1_G15, "/LTE/", "G15", '#N/A').
?test(sheet1_H15, "/LTE/", "H15", '#NAME?').
?test(sheet1_I15, "/LTE/", "I15", 'NULL!').
?test(sheet1_J15, "/LTE/", "J15", '#NUM!').
?test(sheet1_K15, "/LTE/", "K15", '#REF!').
?test(sheet1_L15, "/LTE/", "L15", '#VALUE!').
?test(sheet1_M15, "/LTE/", "M15", true).
?test(sheet1_N15, "/LTE/", "N15", true).
?test(sheet1_O15, "/LTE/", "O15", true).
?test(sheet1_P15, "/LTE/", "P15", false).
?test(sheet1_Q15, "/LTE/", "Q15", false).
?test(sheet1_R15, "/LTE/", "R15", false).
?test(sheet1_S15, "/LTE/", "S15", false).
?test(sheet1_T15, "/LTE/", "T15", false).
?test(sheet1_U15, "/LTE/", "U15", false).
?test(sheet1_V15, "/LTE/", "V15", false).
?test(sheet1_A16, "/LTE/", "A16", "Str Num").
?test(sheet1_B16, "/LTE/", "B16", "2.7").
?test(sheet1_C16, "/LTE/", "C16", false).
?test(sheet1_D16, "/LTE/", "D16", true).
?test(sheet1_E16, "/LTE/", "E16", true).
?test(sheet1_F16, "/LTE/", "F16", '#DIV/0!').
?test(sheet1_G16, "/LTE/", "G16", '#N/A').
?test(sheet1_H16, "/LTE/", "H16", '#NAME?').
?test(sheet1_I16, "/LTE/", "I16", 'NULL!').
?test(sheet1_J16, "/LTE/", "J16", '#NUM!').
?test(sheet1_K16, "/LTE/", "K16", '#REF!').
?test(sheet1_L16, "/LTE/", "L16", '#VALUE!').
?test(sheet1_M16, "/LTE/", "M16", true).
?test(sheet1_N16, "/LTE/", "N16", true).
?test(sheet1_O16, "/LTE/", "O16", true).
?test(sheet1_P16, "/LTE/", "P16", true).
?test(sheet1_Q16, "/LTE/", "Q16", true).
?test(sheet1_R16, "/LTE/", "R16", false).
?test(sheet1_S16, "/LTE/", "S16", false).
?test(sheet1_T16, "/LTE/", "T16", false).
?test(sheet1_U16, "/LTE/", "U16", false).
?test(sheet1_V16, "/LTE/", "V16", false).
?test(sheet1_A17, "/LTE/", "A17", "Str Num").
?test(sheet1_B17, "/LTE/", "B17", "3.54").
?test(sheet1_C17, "/LTE/", "C17", false).
?test(sheet1_D17, "/LTE/", "D17", true).
?test(sheet1_E17, "/LTE/", "E17", true).
?test(sheet1_F17, "/LTE/", "F17", '#DIV/0!').
?test(sheet1_G17, "/LTE/", "G17", '#N/A').
?test(sheet1_H17, "/LTE/", "H17", '#NAME?').
?test(sheet1_I17, "/LTE/", "I17", 'NULL!').
?test(sheet1_J17, "/LTE/", "J17", '#NUM!').
?test(sheet1_K17, "/LTE/", "K17", '#REF!').
?test(sheet1_L17, "/LTE/", "L17", '#VALUE!').
?test(sheet1_M17, "/LTE/", "M17", true).
?test(sheet1_N17, "/LTE/", "N17", true).
?test(sheet1_O17, "/LTE/", "O17", true).
?test(sheet1_P17, "/LTE/", "P17", false).
?test(sheet1_Q17, "/LTE/", "Q17", true).
?test(sheet1_R17, "/LTE/", "R17", false).
?test(sheet1_S17, "/LTE/", "S17", false).
?test(sheet1_T17, "/LTE/", "T17", false).
?test(sheet1_U17, "/LTE/", "U17", false).
?test(sheet1_V17, "/LTE/", "V17", false).
?test(sheet1_A18, "/LTE/", "A18", "Integer").
?test(sheet1_B18, "/LTE/", "B18", "1999/02/01 00:00:00").
?test(sheet1_C18, "/LTE/", "C18", false).
?test(sheet1_D18, "/LTE/", "D18", true).
?test(sheet1_E18, "/LTE/", "E18", true).
?test(sheet1_F18, "/LTE/", "F18", '#DIV/0!').
?test(sheet1_G18, "/LTE/", "G18", '#N/A').
?test(sheet1_H18, "/LTE/", "H18", '#NAME?').
?test(sheet1_I18, "/LTE/", "I18", 'NULL!').
?test(sheet1_J18, "/LTE/", "J18", '#NUM!').
?test(sheet1_K18, "/LTE/", "K18", '#REF!').
?test(sheet1_L18, "/LTE/", "L18", '#VALUE!').
?test(sheet1_M18, "/LTE/", "M18", true).
?test(sheet1_N18, "/LTE/", "N18", true).
?test(sheet1_O18, "/LTE/", "O18", true).
?test(sheet1_P18, "/LTE/", "P18", true).
?test(sheet1_Q18, "/LTE/", "Q18", true).
?test(sheet1_R18, "/LTE/", "R18", true).
?test(sheet1_S18, "/LTE/", "S18", true).
?test(sheet1_T18, "/LTE/", "T18", false).
?test(sheet1_U18, "/LTE/", "U18", false).
?test(sheet1_V18, "/LTE/", "V18", true).
?test(sheet1_A19, "/LTE/", "A19", "Integer").
?test(sheet1_B19, "/LTE/", "B19", "1999/02/02 00:00:00").
?test(sheet1_C19, "/LTE/", "C19", false).
?test(sheet1_D19, "/LTE/", "D19", true).
?test(sheet1_E19, "/LTE/", "E19", true).
?test(sheet1_F19, "/LTE/", "F19", '#DIV/0!').
?test(sheet1_G19, "/LTE/", "G19", '#N/A').
?test(sheet1_H19, "/LTE/", "H19", '#NAME?').
?test(sheet1_I19, "/LTE/", "I19", 'NULL!').
?test(sheet1_J19, "/LTE/", "J19", '#NUM!').
?test(sheet1_K19, "/LTE/", "K19", '#REF!').
?test(sheet1_L19, "/LTE/", "L19", '#VALUE!').
?test(sheet1_M19, "/LTE/", "M19", true).
?test(sheet1_N19, "/LTE/", "N19", true).
?test(sheet1_O19, "/LTE/", "O19", true).
?test(sheet1_P19, "/LTE/", "P19", true).
?test(sheet1_Q19, "/LTE/", "Q19", true).
?test(sheet1_R19, "/LTE/", "R19", false).
?test(sheet1_S19, "/LTE/", "S19", true).
?test(sheet1_T19, "/LTE/", "T19", false).
?test(sheet1_U19, "/LTE/", "U19", false).
?test(sheet1_V19, "/LTE/", "V19", true).
?test(sheet1_A20, "/LTE/", "A20", "Zero").
?test(sheet1_B20, "/LTE/", "B20", 0.0).
?test(sheet1_C20, "/LTE/", "C20", true).
?test(sheet1_D20, "/LTE/", "D20", true).
?test(sheet1_E20, "/LTE/", "E20", true).
?test(sheet1_F20, "/LTE/", "F20", '#DIV/0!').
?test(sheet1_G20, "/LTE/", "G20", '#N/A').
?test(sheet1_H20, "/LTE/", "H20", '#NAME?').
?test(sheet1_I20, "/LTE/", "I20", 'NULL!').
?test(sheet1_J20, "/LTE/", "J20", '#NUM!').
?test(sheet1_K20, "/LTE/", "K20", '#REF!').
?test(sheet1_L20, "/LTE/", "L20", '#VALUE!').
?test(sheet1_M20, "/LTE/", "M20", true).
?test(sheet1_N20, "/LTE/", "N20", true).
?test(sheet1_O20, "/LTE/", "O20", true).
?test(sheet1_P20, "/LTE/", "P20", true).
?test(sheet1_Q20, "/LTE/", "Q20", true).
?test(sheet1_R20, "/LTE/", "R20", true).
?test(sheet1_S20, "/LTE/", "S20", true).
?test(sheet1_T20, "/LTE/", "T20", true).
?test(sheet1_U20, "/LTE/", "U20", true).
?test(sheet1_V20, "/LTE/", "V20", true).
?test(sheet1_A21, "/LTE/", "A21", "Float").
?test(sheet1_B21, "/LTE/", "B21", 3.1415).
?test(sheet1_C21, "/LTE/", "C21", false).
?test(sheet1_D21, "/LTE/", "D21", true).
?test(sheet1_E21, "/LTE/", "E21", true).
?test(sheet1_F21, "/LTE/", "F21", '#DIV/0!').
?test(sheet1_G21, "/LTE/", "G21", '#N/A').
?test(sheet1_H21, "/LTE/", "H21", '#NAME?').
?test(sheet1_I21, "/LTE/", "I21", 'NULL!').
?test(sheet1_J21, "/LTE/", "J21", '#NUM!').
?test(sheet1_K21, "/LTE/", "K21", '#REF!').
?test(sheet1_L21, "/LTE/", "L21", '#VALUE!').
?test(sheet1_M21, "/LTE/", "M21", true).
?test(sheet1_N21, "/LTE/", "N21", true).
?test(sheet1_O21, "/LTE/", "O21", true).
?test(sheet1_P21, "/LTE/", "P21", true).
?test(sheet1_Q21, "/LTE/", "Q21", true).
?test(sheet1_R21, "/LTE/", "R21", true).
?test(sheet1_S21, "/LTE/", "S21", true).
?test(sheet1_T21, "/LTE/", "T21", false).
?test(sheet1_U21, "/LTE/", "U21", true).
?test(sheet1_V21, "/LTE/", "V21", true).
?test(sheet1_A22, "/LTE/", "A22", "Float").
?test(sheet1_B22, "/LTE/", "B22", 36193.2).
?test(sheet1_C22, "/LTE/", "C22", false).
?test(sheet1_D22, "/LTE/", "D22", true).
?test(sheet1_E22, "/LTE/", "E22", true).
?test(sheet1_F22, "/LTE/", "F22", '#DIV/0!').
?test(sheet1_G22, "/LTE/", "G22", '#N/A').
?test(sheet1_H22, "/LTE/", "H22", '#NAME?').
?test(sheet1_I22, "/LTE/", "I22", 'NULL!').
?test(sheet1_J22, "/LTE/", "J22", '#NUM!').
?test(sheet1_K22, "/LTE/", "K22", '#REF!').
?test(sheet1_L22, "/LTE/", "L22", '#VALUE!').
?test(sheet1_M22, "/LTE/", "M22", true).
?test(sheet1_N22, "/LTE/", "N22", true).
?test(sheet1_O22, "/LTE/", "O22", true).
?test(sheet1_P22, "/LTE/", "P22", true).
?test(sheet1_Q22, "/LTE/", "Q22", true).
?test(sheet1_R22, "/LTE/", "R22", false).
?test(sheet1_S22, "/LTE/", "S22", false).
?test(sheet1_T22, "/LTE/", "T22", false).
?test(sheet1_U22, "/LTE/", "U22", false).
?test(sheet1_V22, "/LTE/", "V22", true).
?test(sheet1_A25, "/LTE/", "A25", "Blank").
?test(sheet1_C25, "/LTE/", "C25", true).
?test(sheet1_D25, "/LTE/", "D25", true).
?test(sheet1_E25, "/LTE/", "E25", true).
?test(sheet1_F25, "/LTE/", "F25", '#DIV/0!').
?test(sheet1_G25, "/LTE/", "G25", '#N/A').
?test(sheet1_H25, "/LTE/", "H25", '#NAME?').
?test(sheet1_I25, "/LTE/", "I25", 'NULL!').
?test(sheet1_J25, "/LTE/", "J25", '#NUM!').
?test(sheet1_K25, "/LTE/", "K25", '#REF!').
?test(sheet1_L25, "/LTE/", "L25", '#VALUE!').
?test(sheet1_M25, "/LTE/", "M25", true).
?test(sheet1_N25, "/LTE/", "N25", true).
?test(sheet1_O25, "/LTE/", "O25", true).
?test(sheet1_P25, "/LTE/", "P25", true).
?test(sheet1_Q25, "/LTE/", "Q25", true).
?test(sheet1_R25, "/LTE/", "R25", true).
?test(sheet1_S25, "/LTE/", "S25", true).
?test(sheet1_T25, "/LTE/", "T25", true).
?test(sheet1_U25, "/LTE/", "U25", true).
?test(sheet1_V25, "/LTE/", "V25", true).
?test(sheet1_A26, "/LTE/", "A26", "Boolean").
?test(sheet1_C26, "/LTE/", "C26", false).
?test(sheet1_D26, "/LTE/", "D26", true).
?test(sheet1_E26, "/LTE/", "E26", false).
?test(sheet1_F26, "/LTE/", "F26", '#DIV/0!').
?test(sheet1_G26, "/LTE/", "G26", '#N/A').
?test(sheet1_H26, "/LTE/", "H26", '#NAME?').
?test(sheet1_I26, "/LTE/", "I26", 'NULL!').
?test(sheet1_J26, "/LTE/", "J26", '#NUM!').
?test(sheet1_K26, "/LTE/", "K26", '#REF!').
?test(sheet1_L26, "/LTE/", "L26", '#VALUE!').
?test(sheet1_M26, "/LTE/", "M26", false).
?test(sheet1_N26, "/LTE/", "N26", false).
?test(sheet1_O26, "/LTE/", "O26", false).
?test(sheet1_P26, "/LTE/", "P26", false).
?test(sheet1_Q26, "/LTE/", "Q26", false).
?test(sheet1_R26, "/LTE/", "R26", false).
?test(sheet1_S26, "/LTE/", "S26", false).
?test(sheet1_T26, "/LTE/", "T26", false).
?test(sheet1_U26, "/LTE/", "U26", false).
?test(sheet1_V26, "/LTE/", "V26", false).
?test(sheet1_A27, "/LTE/", "A27", "Boolean").
?test(sheet1_C27, "/LTE/", "C27", true).
?test(sheet1_D27, "/LTE/", "D27", true).
?test(sheet1_E27, "/LTE/", "E27", true).
?test(sheet1_F27, "/LTE/", "F27", '#DIV/0!').
?test(sheet1_G27, "/LTE/", "G27", '#N/A').
?test(sheet1_H27, "/LTE/", "H27", '#NAME?').
?test(sheet1_I27, "/LTE/", "I27", 'NULL!').
?test(sheet1_J27, "/LTE/", "J27", '#NUM!').
?test(sheet1_K27, "/LTE/", "K27", '#REF!').
?test(sheet1_L27, "/LTE/", "L27", '#VALUE!').
?test(sheet1_M27, "/LTE/", "M27", false).
?test(sheet1_N27, "/LTE/", "N27", false).
?test(sheet1_O27, "/LTE/", "O27", false).
?test(sheet1_P27, "/LTE/", "P27", false).
?test(sheet1_Q27, "/LTE/", "Q27", false).
?test(sheet1_R27, "/LTE/", "R27", false).
?test(sheet1_S27, "/LTE/", "S27", false).
?test(sheet1_T27, "/LTE/", "T27", false).
?test(sheet1_U27, "/LTE/", "U27", false).
?test(sheet1_V27, "/LTE/", "V27", false).
?test(sheet1_A28, "/LTE/", "A28", "Error").
?test(sheet1_C28, "/LTE/", "C28", '#DIV/0!').
?test(sheet1_D28, "/LTE/", "D28", '#DIV/0!').
?test(sheet1_E28, "/LTE/", "E28", '#DIV/0!').
?test(sheet1_F28, "/LTE/", "F28", '#DIV/0!').
?test(sheet1_G28, "/LTE/", "G28", '#DIV/0!').
?test(sheet1_H28, "/LTE/", "H28", '#DIV/0!').
?test(sheet1_I28, "/LTE/", "I28", '#DIV/0!').
?test(sheet1_J28, "/LTE/", "J28", '#DIV/0!').
?test(sheet1_K28, "/LTE/", "K28", '#DIV/0!').
?test(sheet1_L28, "/LTE/", "L28", '#DIV/0!').
?test(sheet1_M28, "/LTE/", "M28", '#DIV/0!').
?test(sheet1_N28, "/LTE/", "N28", '#DIV/0!').
?test(sheet1_O28, "/LTE/", "O28", '#DIV/0!').
?test(sheet1_P28, "/LTE/", "P28", '#DIV/0!').
?test(sheet1_Q28, "/LTE/", "Q28", '#DIV/0!').
?test(sheet1_R28, "/LTE/", "R28", '#DIV/0!').
?test(sheet1_S28, "/LTE/", "S28", '#DIV/0!').
?test(sheet1_T28, "/LTE/", "T28", '#DIV/0!').
?test(sheet1_U28, "/LTE/", "U28", '#DIV/0!').
?test(sheet1_V28, "/LTE/", "V28", '#DIV/0!').
?test(sheet1_A29, "/LTE/", "A29", "Error").
?test(sheet1_C29, "/LTE/", "C29", '#N/A').
?test(sheet1_D29, "/LTE/", "D29", '#N/A').
?test(sheet1_E29, "/LTE/", "E29", '#N/A').
?test(sheet1_F29, "/LTE/", "F29", '#N/A').
?test(sheet1_G29, "/LTE/", "G29", '#N/A').
?test(sheet1_H29, "/LTE/", "H29", '#N/A').
?test(sheet1_I29, "/LTE/", "I29", '#N/A').
?test(sheet1_J29, "/LTE/", "J29", '#N/A').
?test(sheet1_K29, "/LTE/", "K29", '#N/A').
?test(sheet1_L29, "/LTE/", "L29", '#N/A').
?test(sheet1_M29, "/LTE/", "M29", '#N/A').
?test(sheet1_N29, "/LTE/", "N29", '#N/A').
?test(sheet1_O29, "/LTE/", "O29", '#N/A').
?test(sheet1_P29, "/LTE/", "P29", '#N/A').
?test(sheet1_Q29, "/LTE/", "Q29", '#N/A').
?test(sheet1_R29, "/LTE/", "R29", '#N/A').
?test(sheet1_S29, "/LTE/", "S29", '#N/A').
?test(sheet1_T29, "/LTE/", "T29", '#N/A').
?test(sheet1_U29, "/LTE/", "U29", '#N/A').
?test(sheet1_V29, "/LTE/", "V29", '#N/A').
?test(sheet1_A30, "/LTE/", "A30", "Error").
?test(sheet1_C30, "/LTE/", "C30", '#NAME?').
?test(sheet1_D30, "/LTE/", "D30", '#NAME?').
?test(sheet1_E30, "/LTE/", "E30", '#NAME?').
?test(sheet1_F30, "/LTE/", "F30", '#NAME?').
?test(sheet1_G30, "/LTE/", "G30", '#NAME?').
?test(sheet1_H30, "/LTE/", "H30", '#NAME?').
?test(sheet1_I30, "/LTE/", "I30", '#NAME?').
?test(sheet1_J30, "/LTE/", "J30", '#NAME?').
?test(sheet1_K30, "/LTE/", "K30", '#NAME?').
?test(sheet1_L30, "/LTE/", "L30", '#NAME?').
?test(sheet1_M30, "/LTE/", "M30", '#NAME?').
?test(sheet1_N30, "/LTE/", "N30", '#NAME?').
?test(sheet1_O30, "/LTE/", "O30", '#NAME?').
?test(sheet1_P30, "/LTE/", "P30", '#NAME?').
?test(sheet1_Q30, "/LTE/", "Q30", '#NAME?').
?test(sheet1_R30, "/LTE/", "R30", '#NAME?').
?test(sheet1_S30, "/LTE/", "S30", '#NAME?').
?test(sheet1_T30, "/LTE/", "T30", '#NAME?').
?test(sheet1_U30, "/LTE/", "U30", '#NAME?').
?test(sheet1_V30, "/LTE/", "V30", '#NAME?').
?test(sheet1_A31, "/LTE/", "A31", "Error").
?test(sheet1_C31, "/LTE/", "C31", 'NULL!').
?test(sheet1_D31, "/LTE/", "D31", 'NULL!').
?test(sheet1_E31, "/LTE/", "E31", 'NULL!').
?test(sheet1_F31, "/LTE/", "F31", 'NULL!').
?test(sheet1_G31, "/LTE/", "G31", 'NULL!').
?test(sheet1_H31, "/LTE/", "H31", 'NULL!').
?test(sheet1_I31, "/LTE/", "I31", 'NULL!').
?test(sheet1_J31, "/LTE/", "J31", 'NULL!').
?test(sheet1_K31, "/LTE/", "K31", 'NULL!').
?test(sheet1_L31, "/LTE/", "L31", 'NULL!').
?test(sheet1_M31, "/LTE/", "M31", 'NULL!').
?test(sheet1_N31, "/LTE/", "N31", 'NULL!').
?test(sheet1_O31, "/LTE/", "O31", 'NULL!').
?test(sheet1_P31, "/LTE/", "P31", 'NULL!').
?test(sheet1_Q31, "/LTE/", "Q31", 'NULL!').
?test(sheet1_R31, "/LTE/", "R31", 'NULL!').
?test(sheet1_S31, "/LTE/", "S31", 'NULL!').
?test(sheet1_T31, "/LTE/", "T31", 'NULL!').
?test(sheet1_U31, "/LTE/", "U31", 'NULL!').
?test(sheet1_V31, "/LTE/", "V31", 'NULL!').
?test(sheet1_A32, "/LTE/", "A32", "Error").
?test(sheet1_C32, "/LTE/", "C32", '#NUM!').
?test(sheet1_D32, "/LTE/", "D32", '#NUM!').
?test(sheet1_E32, "/LTE/", "E32", '#NUM!').
?test(sheet1_F32, "/LTE/", "F32", '#NUM!').
?test(sheet1_G32, "/LTE/", "G32", '#NUM!').
?test(sheet1_H32, "/LTE/", "H32", '#NUM!').
?test(sheet1_I32, "/LTE/", "I32", '#NUM!').
?test(sheet1_J32, "/LTE/", "J32", '#NUM!').
?test(sheet1_K32, "/LTE/", "K32", '#NUM!').
?test(sheet1_L32, "/LTE/", "L32", '#NUM!').
?test(sheet1_M32, "/LTE/", "M32", '#NUM!').
?test(sheet1_N32, "/LTE/", "N32", '#NUM!').
?test(sheet1_O32, "/LTE/", "O32", '#NUM!').
?test(sheet1_P32, "/LTE/", "P32", '#NUM!').
?test(sheet1_Q32, "/LTE/", "Q32", '#NUM!').
?test(sheet1_R32, "/LTE/", "R32", '#NUM!').
?test(sheet1_S32, "/LTE/", "S32", '#NUM!').
?test(sheet1_T32, "/LTE/", "T32", '#NUM!').
?test(sheet1_U32, "/LTE/", "U32", '#NUM!').
?test(sheet1_V32, "/LTE/", "V32", '#NUM!').
?test(sheet1_A33, "/LTE/", "A33", "Error").
?test(sheet1_C33, "/LTE/", "C33", '#REF!').
?test(sheet1_D33, "/LTE/", "D33", '#REF!').
?test(sheet1_E33, "/LTE/", "E33", '#REF!').
?test(sheet1_F33, "/LTE/", "F33", '#REF!').
?test(sheet1_G33, "/LTE/", "G33", '#REF!').
?test(sheet1_H33, "/LTE/", "H33", '#REF!').
?test(sheet1_I33, "/LTE/", "I33", '#REF!').
?test(sheet1_J33, "/LTE/", "J33", '#REF!').
?test(sheet1_K33, "/LTE/", "K33", '#REF!').
?test(sheet1_L33, "/LTE/", "L33", '#REF!').
?test(sheet1_M33, "/LTE/", "M33", '#REF!').
?test(sheet1_N33, "/LTE/", "N33", '#REF!').
?test(sheet1_O33, "/LTE/", "O33", '#REF!').
?test(sheet1_P33, "/LTE/", "P33", '#REF!').
?test(sheet1_Q33, "/LTE/", "Q33", '#REF!').
?test(sheet1_R33, "/LTE/", "R33", '#REF!').
?test(sheet1_S33, "/LTE/", "S33", '#REF!').
?test(sheet1_T33, "/LTE/", "T33", '#REF!').
?test(sheet1_U33, "/LTE/", "U33", '#REF!').
?test(sheet1_V33, "/LTE/", "V33", '#REF!').
?test(sheet1_A34, "/LTE/", "A34", "Error").
?test(sheet1_C34, "/LTE/", "C34", '#VALUE!').
?test(sheet1_D34, "/LTE/", "D34", '#VALUE!').
?test(sheet1_E34, "/LTE/", "E34", '#VALUE!').
?test(sheet1_F34, "/LTE/", "F34", '#VALUE!').
?test(sheet1_G34, "/LTE/", "G34", '#VALUE!').
?test(sheet1_H34, "/LTE/", "H34", '#VALUE!').
?test(sheet1_I34, "/LTE/", "I34", '#VALUE!').
?test(sheet1_J34, "/LTE/", "J34", '#VALUE!').
?test(sheet1_K34, "/LTE/", "K34", '#VALUE!').
?test(sheet1_L34, "/LTE/", "L34", '#VALUE!').
?test(sheet1_M34, "/LTE/", "M34", '#VALUE!').
?test(sheet1_N34, "/LTE/", "N34", '#VALUE!').
?test(sheet1_O34, "/LTE/", "O34", '#VALUE!').
?test(sheet1_P34, "/LTE/", "P34", '#VALUE!').
?test(sheet1_Q34, "/LTE/", "Q34", '#VALUE!').
?test(sheet1_R34, "/LTE/", "R34", '#VALUE!').
?test(sheet1_S34, "/LTE/", "S34", '#VALUE!').
?test(sheet1_T34, "/LTE/", "T34", '#VALUE!').
?test(sheet1_U34, "/LTE/", "U34", '#VALUE!').
?test(sheet1_V34, "/LTE/", "V34", '#VALUE!').
?test(sheet1_A35, "/LTE/", "A35", "String").
?test(sheet1_C35, "/LTE/", "C35", false).
?test(sheet1_D35, "/LTE/", "D35", true).
?test(sheet1_E35, "/LTE/", "E35", true).
?test(sheet1_F35, "/LTE/", "F35", '#DIV/0!').
?test(sheet1_G35, "/LTE/", "G35", '#N/A').
?test(sheet1_H35, "/LTE/", "H35", '#NAME?').
?test(sheet1_I35, "/LTE/", "I35", 'NULL!').
?test(sheet1_J35, "/LTE/", "J35", '#NUM!').
?test(sheet1_K35, "/LTE/", "K35", '#REF!').
?test(sheet1_L35, "/LTE/", "L35", '#VALUE!').
?test(sheet1_M35, "/LTE/", "M35", true).
?test(sheet1_N35, "/LTE/", "N35", false).
?test(sheet1_O35, "/LTE/", "O35", false).
?test(sheet1_P35, "/LTE/", "P35", false).
?test(sheet1_Q35, "/LTE/", "Q35", false).
?test(sheet1_R35, "/LTE/", "R35", false).
?test(sheet1_S35, "/LTE/", "S35", false).
?test(sheet1_T35, "/LTE/", "T35", false).
?test(sheet1_U35, "/LTE/", "U35", false).
?test(sheet1_V35, "/LTE/", "V35", false).
?test(sheet1_A36, "/LTE/", "A36", "String").
?test(sheet1_C36, "/LTE/", "C36", false).
?test(sheet1_D36, "/LTE/", "D36", true).
?test(sheet1_E36, "/LTE/", "E36", true).
?test(sheet1_F36, "/LTE/", "F36", '#DIV/0!').
?test(sheet1_G36, "/LTE/", "G36", '#N/A').
?test(sheet1_H36, "/LTE/", "H36", '#NAME?').
?test(sheet1_I36, "/LTE/", "I36", 'NULL!').
?test(sheet1_J36, "/LTE/", "J36", '#NUM!').
?test(sheet1_K36, "/LTE/", "K36", '#REF!').
?test(sheet1_L36, "/LTE/", "L36", '#VALUE!').
?test(sheet1_M36, "/LTE/", "M36", true).
?test(sheet1_N36, "/LTE/", "N36", true).
?test(sheet1_O36, "/LTE/", "O36", false).
?test(sheet1_P36, "/LTE/", "P36", false).
?test(sheet1_Q36, "/LTE/", "Q36", false).
?test(sheet1_R36, "/LTE/", "R36", false).
?test(sheet1_S36, "/LTE/", "S36", false).
?test(sheet1_T36, "/LTE/", "T36", false).
?test(sheet1_U36, "/LTE/", "U36", false).
?test(sheet1_V36, "/LTE/", "V36", false).
?test(sheet1_A37, "/LTE/", "A37", "String").
?test(sheet1_C37, "/LTE/", "C37", false).
?test(sheet1_D37, "/LTE/", "D37", true).
?test(sheet1_E37, "/LTE/", "E37", true).
?test(sheet1_F37, "/LTE/", "F37", '#DIV/0!').
?test(sheet1_G37, "/LTE/", "G37", '#N/A').
?test(sheet1_H37, "/LTE/", "H37", '#NAME?').
?test(sheet1_I37, "/LTE/", "I37", 'NULL!').
?test(sheet1_J37, "/LTE/", "J37", '#NUM!').
?test(sheet1_K37, "/LTE/", "K37", '#REF!').
?test(sheet1_L37, "/LTE/", "L37", '#VALUE!').
?test(sheet1_M37, "/LTE/", "M37", true).
?test(sheet1_N37, "/LTE/", "N37", true).
?test(sheet1_O37, "/LTE/", "O37", true).
?test(sheet1_P37, "/LTE/", "P37", false).
?test(sheet1_Q37, "/LTE/", "Q37", false).
?test(sheet1_R37, "/LTE/", "R37", false).
?test(sheet1_S37, "/LTE/", "S37", false).
?test(sheet1_T37, "/LTE/", "T37", false).
?test(sheet1_U37, "/LTE/", "U37", false).
?test(sheet1_V37, "/LTE/", "V37", false).
?test(sheet1_A38, "/LTE/", "A38", "Str Num").
?test(sheet1_C38, "/LTE/", "C38", false).
?test(sheet1_D38, "/LTE/", "D38", true).
?test(sheet1_E38, "/LTE/", "E38", true).
?test(sheet1_F38, "/LTE/", "F38", '#DIV/0!').
?test(sheet1_G38, "/LTE/", "G38", '#N/A').
?test(sheet1_H38, "/LTE/", "H38", '#NAME?').
?test(sheet1_I38, "/LTE/", "I38", 'NULL!').
?test(sheet1_J38, "/LTE/", "J38", '#NUM!').
?test(sheet1_K38, "/LTE/", "K38", '#REF!').
?test(sheet1_L38, "/LTE/", "L38", '#VALUE!').
?test(sheet1_M38, "/LTE/", "M38", true).
?test(sheet1_N38, "/LTE/", "N38", true).
?test(sheet1_O38, "/LTE/", "O38", true).
?test(sheet1_P38, "/LTE/", "P38", true).
?test(sheet1_Q38, "/LTE/", "Q38", true).
?test(sheet1_R38, "/LTE/", "R38", false).
?test(sheet1_S38, "/LTE/", "S38", false).
?test(sheet1_T38, "/LTE/", "T38", false).
?test(sheet1_U38, "/LTE/", "U38", false).
?test(sheet1_V38, "/LTE/", "V38", false).
?test(sheet1_A39, "/LTE/", "A39", "Str Num").
?test(sheet1_C39, "/LTE/", "C39", false).
?test(sheet1_D39, "/LTE/", "D39", true).
?test(sheet1_E39, "/LTE/", "E39", true).
?test(sheet1_F39, "/LTE/", "F39", '#DIV/0!').
?test(sheet1_G39, "/LTE/", "G39", '#N/A').
?test(sheet1_H39, "/LTE/", "H39", '#NAME?').
?test(sheet1_I39, "/LTE/", "I39", 'NULL!').
?test(sheet1_J39, "/LTE/", "J39", '#NUM!').
?test(sheet1_K39, "/LTE/", "K39", '#REF!').
?test(sheet1_L39, "/LTE/", "L39", '#VALUE!').
?test(sheet1_M39, "/LTE/", "M39", true).
?test(sheet1_N39, "/LTE/", "N39", true).
?test(sheet1_O39, "/LTE/", "O39", true).
?test(sheet1_P39, "/LTE/", "P39", false).
?test(sheet1_Q39, "/LTE/", "Q39", true).
?test(sheet1_R39, "/LTE/", "R39", false).
?test(sheet1_S39, "/LTE/", "S39", false).
?test(sheet1_T39, "/LTE/", "T39", false).
?test(sheet1_U39, "/LTE/", "U39", false).
?test(sheet1_V39, "/LTE/", "V39", false).
?test(sheet1_A40, "/LTE/", "A40", "Integer").
?test(sheet1_C40, "/LTE/", "C40", false).
?test(sheet1_D40, "/LTE/", "D40", true).
?test(sheet1_E40, "/LTE/", "E40", true).
?test(sheet1_F40, "/LTE/", "F40", '#DIV/0!').
?test(sheet1_G40, "/LTE/", "G40", '#N/A').
?test(sheet1_H40, "/LTE/", "H40", '#NAME?').
?test(sheet1_I40, "/LTE/", "I40", 'NULL!').
?test(sheet1_J40, "/LTE/", "J40", '#NUM!').
?test(sheet1_K40, "/LTE/", "K40", '#REF!').
?test(sheet1_L40, "/LTE/", "L40", '#VALUE!').
?test(sheet1_M40, "/LTE/", "M40", true).
?test(sheet1_N40, "/LTE/", "N40", true).
?test(sheet1_O40, "/LTE/", "O40", true).
?test(sheet1_P40, "/LTE/", "P40", true).
?test(sheet1_Q40, "/LTE/", "Q40", true).
?test(sheet1_R40, "/LTE/", "R40", true).
?test(sheet1_S40, "/LTE/", "S40", true).
?test(sheet1_T40, "/LTE/", "T40", false).
?test(sheet1_U40, "/LTE/", "U40", false).
?test(sheet1_V40, "/LTE/", "V40", true).
?test(sheet1_A41, "/LTE/", "A41", "Integer").
?test(sheet1_C41, "/LTE/", "C41", false).
?test(sheet1_D41, "/LTE/", "D41", true).
?test(sheet1_E41, "/LTE/", "E41", true).
?test(sheet1_F41, "/LTE/", "F41", '#DIV/0!').
?test(sheet1_G41, "/LTE/", "G41", '#N/A').
?test(sheet1_H41, "/LTE/", "H41", '#NAME?').
?test(sheet1_I41, "/LTE/", "I41", 'NULL!').
?test(sheet1_J41, "/LTE/", "J41", '#NUM!').
?test(sheet1_K41, "/LTE/", "K41", '#REF!').
?test(sheet1_L41, "/LTE/", "L41", '#VALUE!').
?test(sheet1_M41, "/LTE/", "M41", true).
?test(sheet1_N41, "/LTE/", "N41", true).
?test(sheet1_O41, "/LTE/", "O41", true).
?test(sheet1_P41, "/LTE/", "P41", true).
?test(sheet1_Q41, "/LTE/", "Q41", true).
?test(sheet1_R41, "/LTE/", "R41", false).
?test(sheet1_S41, "/LTE/", "S41", true).
?test(sheet1_T41, "/LTE/", "T41", false).
?test(sheet1_U41, "/LTE/", "U41", false).
?test(sheet1_V41, "/LTE/", "V41", true).
?test(sheet1_A42, "/LTE/", "A42", "Zero").
?test(sheet1_C42, "/LTE/", "C42", true).
?test(sheet1_D42, "/LTE/", "D42", true).
?test(sheet1_E42, "/LTE/", "E42", true).
?test(sheet1_F42, "/LTE/", "F42", '#DIV/0!').
?test(sheet1_G42, "/LTE/", "G42", '#N/A').
?test(sheet1_H42, "/LTE/", "H42", '#NAME?').
?test(sheet1_I42, "/LTE/", "I42", 'NULL!').
?test(sheet1_J42, "/LTE/", "J42", '#NUM!').
?test(sheet1_K42, "/LTE/", "K42", '#REF!').
?test(sheet1_L42, "/LTE/", "L42", '#VALUE!').
?test(sheet1_M42, "/LTE/", "M42", true).
?test(sheet1_N42, "/LTE/", "N42", true).
?test(sheet1_O42, "/LTE/", "O42", true).
?test(sheet1_P42, "/LTE/", "P42", true).
?test(sheet1_Q42, "/LTE/", "Q42", true).
?test(sheet1_R42, "/LTE/", "R42", true).
?test(sheet1_S42, "/LTE/", "S42", true).
?test(sheet1_T42, "/LTE/", "T42", true).
?test(sheet1_U42, "/LTE/", "U42", true).
?test(sheet1_V42, "/LTE/", "V42", true).
?test(sheet1_A43, "/LTE/", "A43", "Float").
?test(sheet1_C43, "/LTE/", "C43", false).
?test(sheet1_D43, "/LTE/", "D43", true).
?test(sheet1_E43, "/LTE/", "E43", true).
?test(sheet1_F43, "/LTE/", "F43", '#DIV/0!').
?test(sheet1_G43, "/LTE/", "G43", '#N/A').
?test(sheet1_H43, "/LTE/", "H43", '#NAME?').
?test(sheet1_I43, "/LTE/", "I43", 'NULL!').
?test(sheet1_J43, "/LTE/", "J43", '#NUM!').
?test(sheet1_K43, "/LTE/", "K43", '#REF!').
?test(sheet1_L43, "/LTE/", "L43", '#VALUE!').
?test(sheet1_M43, "/LTE/", "M43", true).
?test(sheet1_N43, "/LTE/", "N43", true).
?test(sheet1_O43, "/LTE/", "O43", true).
?test(sheet1_P43, "/LTE/", "P43", true).
?test(sheet1_Q43, "/LTE/", "Q43", true).
?test(sheet1_R43, "/LTE/", "R43", true).
?test(sheet1_S43, "/LTE/", "S43", true).
?test(sheet1_T43, "/LTE/", "T43", false).
?test(sheet1_U43, "/LTE/", "U43", true).
?test(sheet1_V43, "/LTE/", "V43", true).
?test(sheet1_A44, "/LTE/", "A44", "Float").
?test(sheet1_C44, "/LTE/", "C44", false).
?test(sheet1_D44, "/LTE/", "D44", true).
?test(sheet1_E44, "/LTE/", "E44", true).
?test(sheet1_F44, "/LTE/", "F44", '#DIV/0!').
?test(sheet1_G44, "/LTE/", "G44", '#N/A').
?test(sheet1_H44, "/LTE/", "H44", '#NAME?').
?test(sheet1_I44, "/LTE/", "I44", 'NULL!').
?test(sheet1_J44, "/LTE/", "J44", '#NUM!').
?test(sheet1_K44, "/LTE/", "K44", '#REF!').
?test(sheet1_L44, "/LTE/", "L44", '#VALUE!').
?test(sheet1_M44, "/LTE/", "M44", true).
?test(sheet1_N44, "/LTE/", "N44", true).
?test(sheet1_O44, "/LTE/", "O44", true).
?test(sheet1_P44, "/LTE/", "P44", true).
?test(sheet1_Q44, "/LTE/", "Q44", true).
?test(sheet1_R44, "/LTE/", "R44", false).
?test(sheet1_S44, "/LTE/", "S44", false).
?test(sheet1_T44, "/LTE/", "T44", false).
?test(sheet1_U44, "/LTE/", "U44", false).
?test(sheet1_V44, "/LTE/", "V44", true).
?test(sheet1_A47, "/LTE/", "A47", 400.0).
?test(sheet1_C47, "/LTE/", "C47", 1.0).
?test(sheet1_D47, "/LTE/", "D47", 1.0).
?test(sheet1_E47, "/LTE/", "E47", 1.0).
?test(sheet1_F47, "/LTE/", "F47", 1.0).
?test(sheet1_G47, "/LTE/", "G47", 1.0).
?test(sheet1_H47, "/LTE/", "H47", 1.0).
?test(sheet1_I47, "/LTE/", "I47", 1.0).
?test(sheet1_J47, "/LTE/", "J47", 1.0).
?test(sheet1_K47, "/LTE/", "K47", 1.0).
?test(sheet1_L47, "/LTE/", "L47", 1.0).
?test(sheet1_M47, "/LTE/", "M47", 1.0).
?test(sheet1_N47, "/LTE/", "N47", 1.0).
?test(sheet1_O47, "/LTE/", "O47", 1.0).
?test(sheet1_P47, "/LTE/", "P47", 1.0).
?test(sheet1_Q47, "/LTE/", "Q47", 1.0).
?test(sheet1_R47, "/LTE/", "R47", 1.0).
?test(sheet1_S47, "/LTE/", "S47", 1.0).
?test(sheet1_T47, "/LTE/", "T47", 1.0).
?test(sheet1_U47, "/LTE/", "U47", 1.0).
?test(sheet1_V47, "/LTE/", "V47", 1.0).
?test(sheet1_A48, "/LTE/", "A48", "Success").
?test(sheet1_C48, "/LTE/", "C48", 1.0).
?test(sheet1_D48, "/LTE/", "D48", 1.0).
?test(sheet1_E48, "/LTE/", "E48", 1.0).
?test(sheet1_F48, "/LTE/", "F48", 1.0).
?test(sheet1_G48, "/LTE/", "G48", 1.0).
?test(sheet1_H48, "/LTE/", "H48", 1.0).
?test(sheet1_I48, "/LTE/", "I48", 1.0).
?test(sheet1_J48, "/LTE/", "J48", 1.0).
?test(sheet1_K48, "/LTE/", "K48", 1.0).
?test(sheet1_L48, "/LTE/", "L48", 1.0).
?test(sheet1_M48, "/LTE/", "M48", 1.0).
?test(sheet1_N48, "/LTE/", "N48", 1.0).
?test(sheet1_O48, "/LTE/", "O48", 1.0).
?test(sheet1_P48, "/LTE/", "P48", 1.0).
?test(sheet1_Q48, "/LTE/", "Q48", 1.0).
?test(sheet1_R48, "/LTE/", "R48", 1.0).
?test(sheet1_S48, "/LTE/", "S48", 1.0).
?test(sheet1_T48, "/LTE/", "T48", 1.0).
?test(sheet1_U48, "/LTE/", "U48", 1.0).
?test(sheet1_V48, "/LTE/", "V48", 1.0).
?test(sheet1_C49, "/LTE/", "C49", 1.0).
?test(sheet1_D49, "/LTE/", "D49", 1.0).
?test(sheet1_E49, "/LTE/", "E49", 1.0).
?test(sheet1_F49, "/LTE/", "F49", 1.0).
?test(sheet1_G49, "/LTE/", "G49", 1.0).
?test(sheet1_H49, "/LTE/", "H49", 1.0).
?test(sheet1_I49, "/LTE/", "I49", 1.0).
?test(sheet1_J49, "/LTE/", "J49", 1.0).
?test(sheet1_K49, "/LTE/", "K49", 1.0).
?test(sheet1_L49, "/LTE/", "L49", 1.0).
?test(sheet1_M49, "/LTE/", "M49", 1.0).
?test(sheet1_N49, "/LTE/", "N49", 1.0).
?test(sheet1_O49, "/LTE/", "O49", 1.0).
?test(sheet1_P49, "/LTE/", "P49", 1.0).
?test(sheet1_Q49, "/LTE/", "Q49", 1.0).
?test(sheet1_R49, "/LTE/", "R49", 1.0).
?test(sheet1_S49, "/LTE/", "S49", 1.0).
?test(sheet1_T49, "/LTE/", "T49", 1.0).
?test(sheet1_U49, "/LTE/", "U49", 1.0).
?test(sheet1_V49, "/LTE/", "V49", 1.0).
?test(sheet1_C50, "/LTE/", "C50", 1.0).
?test(sheet1_D50, "/LTE/", "D50", 1.0).
?test(sheet1_E50, "/LTE/", "E50", 1.0).
?test(sheet1_F50, "/LTE/", "F50", 1.0).
?test(sheet1_G50, "/LTE/", "G50", 1.0).
?test(sheet1_H50, "/LTE/", "H50", 1.0).
?test(sheet1_I50, "/LTE/", "I50", 1.0).
?test(sheet1_J50, "/LTE/", "J50", 1.0).
?test(sheet1_K50, "/LTE/", "K50", 1.0).
?test(sheet1_L50, "/LTE/", "L50", 1.0).
?test(sheet1_M50, "/LTE/", "M50", 1.0).
?test(sheet1_N50, "/LTE/", "N50", 1.0).
?test(sheet1_O50, "/LTE/", "O50", 1.0).
?test(sheet1_P50, "/LTE/", "P50", 1.0).
?test(sheet1_Q50, "/LTE/", "Q50", 1.0).
?test(sheet1_R50, "/LTE/", "R50", 1.0).
?test(sheet1_S50, "/LTE/", "S50", 1.0).
?test(sheet1_T50, "/LTE/", "T50", 1.0).
?test(sheet1_U50, "/LTE/", "U50", 1.0).
?test(sheet1_V50, "/LTE/", "V50", 1.0).
?test(sheet1_C51, "/LTE/", "C51", 1.0).
?test(sheet1_D51, "/LTE/", "D51", 1.0).
?test(sheet1_E51, "/LTE/", "E51", 1.0).
?test(sheet1_F51, "/LTE/", "F51", 1.0).
?test(sheet1_G51, "/LTE/", "G51", 1.0).
?test(sheet1_H51, "/LTE/", "H51", 1.0).
?test(sheet1_I51, "/LTE/", "I51", 1.0).
?test(sheet1_J51, "/LTE/", "J51", 1.0).
?test(sheet1_K51, "/LTE/", "K51", 1.0).
?test(sheet1_L51, "/LTE/", "L51", 1.0).
?test(sheet1_M51, "/LTE/", "M51", 1.0).
?test(sheet1_N51, "/LTE/", "N51", 1.0).
?test(sheet1_O51, "/LTE/", "O51", 1.0).
?test(sheet1_P51, "/LTE/", "P51", 1.0).
?test(sheet1_Q51, "/LTE/", "Q51", 1.0).
?test(sheet1_R51, "/LTE/", "R51", 1.0).
?test(sheet1_S51, "/LTE/", "S51", 1.0).
?test(sheet1_T51, "/LTE/", "T51", 1.0).
?test(sheet1_U51, "/LTE/", "U51", 1.0).
?test(sheet1_V51, "/LTE/", "V51", 1.0).
?test(sheet1_C52, "/LTE/", "C52", 1.0).
?test(sheet1_D52, "/LTE/", "D52", 1.0).
?test(sheet1_E52, "/LTE/", "E52", 1.0).
?test(sheet1_F52, "/LTE/", "F52", 1.0).
?test(sheet1_G52, "/LTE/", "G52", 1.0).
?test(sheet1_H52, "/LTE/", "H52", 1.0).
?test(sheet1_I52, "/LTE/", "I52", 1.0).
?test(sheet1_J52, "/LTE/", "J52", 1.0).
?test(sheet1_K52, "/LTE/", "K52", 1.0).
?test(sheet1_L52, "/LTE/", "L52", 1.0).
?test(sheet1_M52, "/LTE/", "M52", 1.0).
?test(sheet1_N52, "/LTE/", "N52", 1.0).
?test(sheet1_O52, "/LTE/", "O52", 1.0).
?test(sheet1_P52, "/LTE/", "P52", 1.0).
?test(sheet1_Q52, "/LTE/", "Q52", 1.0).
?test(sheet1_R52, "/LTE/", "R52", 1.0).
?test(sheet1_S52, "/LTE/", "S52", 1.0).
?test(sheet1_T52, "/LTE/", "T52", 1.0).
?test(sheet1_U52, "/LTE/", "U52", 1.0).
?test(sheet1_V52, "/LTE/", "V52", 1.0).
?test(sheet1_C53, "/LTE/", "C53", 1.0).
?test(sheet1_D53, "/LTE/", "D53", 1.0).
?test(sheet1_E53, "/LTE/", "E53", 1.0).
?test(sheet1_F53, "/LTE/", "F53", 1.0).
?test(sheet1_G53, "/LTE/", "G53", 1.0).
?test(sheet1_H53, "/LTE/", "H53", 1.0).
?test(sheet1_I53, "/LTE/", "I53", 1.0).
?test(sheet1_J53, "/LTE/", "J53", 1.0).
?test(sheet1_K53, "/LTE/", "K53", 1.0).
?test(sheet1_L53, "/LTE/", "L53", 1.0).
?test(sheet1_M53, "/LTE/", "M53", 1.0).
?test(sheet1_N53, "/LTE/", "N53", 1.0).
?test(sheet1_O53, "/LTE/", "O53", 1.0).
?test(sheet1_P53, "/LTE/", "P53", 1.0).
?test(sheet1_Q53, "/LTE/", "Q53", 1.0).
?test(sheet1_R53, "/LTE/", "R53", 1.0).
?test(sheet1_S53, "/LTE/", "S53", 1.0).
?test(sheet1_T53, "/LTE/", "T53", 1.0).
?test(sheet1_U53, "/LTE/", "U53", 1.0).
?test(sheet1_V53, "/LTE/", "V53", 1.0).
?test(sheet1_C54, "/LTE/", "C54", 1.0).
?test(sheet1_D54, "/LTE/", "D54", 1.0).
?test(sheet1_E54, "/LTE/", "E54", 1.0).
?test(sheet1_F54, "/LTE/", "F54", 1.0).
?test(sheet1_G54, "/LTE/", "G54", 1.0).
?test(sheet1_H54, "/LTE/", "H54", 1.0).
?test(sheet1_I54, "/LTE/", "I54", 1.0).
?test(sheet1_J54, "/LTE/", "J54", 1.0).
?test(sheet1_K54, "/LTE/", "K54", 1.0).
?test(sheet1_L54, "/LTE/", "L54", 1.0).
?test(sheet1_M54, "/LTE/", "M54", 1.0).
?test(sheet1_N54, "/LTE/", "N54", 1.0).
?test(sheet1_O54, "/LTE/", "O54", 1.0).
?test(sheet1_P54, "/LTE/", "P54", 1.0).
?test(sheet1_Q54, "/LTE/", "Q54", 1.0).
?test(sheet1_R54, "/LTE/", "R54", 1.0).
?test(sheet1_S54, "/LTE/", "S54", 1.0).
?test(sheet1_T54, "/LTE/", "T54", 1.0).
?test(sheet1_U54, "/LTE/", "U54", 1.0).
?test(sheet1_V54, "/LTE/", "V54", 1.0).
?test(sheet1_C55, "/LTE/", "C55", 1.0).
?test(sheet1_D55, "/LTE/", "D55", 1.0).
?test(sheet1_E55, "/LTE/", "E55", 1.0).
?test(sheet1_F55, "/LTE/", "F55", 1.0).
?test(sheet1_G55, "/LTE/", "G55", 1.0).
?test(sheet1_H55, "/LTE/", "H55", 1.0).
?test(sheet1_I55, "/LTE/", "I55", 1.0).
?test(sheet1_J55, "/LTE/", "J55", 1.0).
?test(sheet1_K55, "/LTE/", "K55", 1.0).
?test(sheet1_L55, "/LTE/", "L55", 1.0).
?test(sheet1_M55, "/LTE/", "M55", 1.0).
?test(sheet1_N55, "/LTE/", "N55", 1.0).
?test(sheet1_O55, "/LTE/", "O55", 1.0).
?test(sheet1_P55, "/LTE/", "P55", 1.0).
?test(sheet1_Q55, "/LTE/", "Q55", 1.0).
?test(sheet1_R55, "/LTE/", "R55", 1.0).
?test(sheet1_S55, "/LTE/", "S55", 1.0).
?test(sheet1_T55, "/LTE/", "T55", 1.0).
?test(sheet1_U55, "/LTE/", "U55", 1.0).
?test(sheet1_V55, "/LTE/", "V55", 1.0).
?test(sheet1_C56, "/LTE/", "C56", 1.0).
?test(sheet1_D56, "/LTE/", "D56", 1.0).
?test(sheet1_E56, "/LTE/", "E56", 1.0).
?test(sheet1_F56, "/LTE/", "F56", 1.0).
?test(sheet1_G56, "/LTE/", "G56", 1.0).
?test(sheet1_H56, "/LTE/", "H56", 1.0).
?test(sheet1_I56, "/LTE/", "I56", 1.0).
?test(sheet1_J56, "/LTE/", "J56", 1.0).
?test(sheet1_K56, "/LTE/", "K56", 1.0).
?test(sheet1_L56, "/LTE/", "L56", 1.0).
?test(sheet1_M56, "/LTE/", "M56", 1.0).
?test(sheet1_N56, "/LTE/", "N56", 1.0).
?test(sheet1_O56, "/LTE/", "O56", 1.0).
?test(sheet1_P56, "/LTE/", "P56", 1.0).
?test(sheet1_Q56, "/LTE/", "Q56", 1.0).
?test(sheet1_R56, "/LTE/", "R56", 1.0).
?test(sheet1_S56, "/LTE/", "S56", 1.0).
?test(sheet1_T56, "/LTE/", "T56", 1.0).
?test(sheet1_U56, "/LTE/", "U56", 1.0).
?test(sheet1_V56, "/LTE/", "V56", 1.0).
?test(sheet1_C57, "/LTE/", "C57", 1.0).
?test(sheet1_D57, "/LTE/", "D57", 1.0).
?test(sheet1_E57, "/LTE/", "E57", 1.0).
?test(sheet1_F57, "/LTE/", "F57", 1.0).
?test(sheet1_G57, "/LTE/", "G57", 1.0).
?test(sheet1_H57, "/LTE/", "H57", 1.0).
?test(sheet1_I57, "/LTE/", "I57", 1.0).
?test(sheet1_J57, "/LTE/", "J57", 1.0).
?test(sheet1_K57, "/LTE/", "K57", 1.0).
?test(sheet1_L57, "/LTE/", "L57", 1.0).
?test(sheet1_M57, "/LTE/", "M57", 1.0).
?test(sheet1_N57, "/LTE/", "N57", 1.0).
?test(sheet1_O57, "/LTE/", "O57", 1.0).
?test(sheet1_P57, "/LTE/", "P57", 1.0).
?test(sheet1_Q57, "/LTE/", "Q57", 1.0).
?test(sheet1_R57, "/LTE/", "R57", 1.0).
?test(sheet1_S57, "/LTE/", "S57", 1.0).
?test(sheet1_T57, "/LTE/", "T57", 1.0).
?test(sheet1_U57, "/LTE/", "U57", 1.0).
?test(sheet1_V57, "/LTE/", "V57", 1.0).
?test(sheet1_C58, "/LTE/", "C58", 1.0).
?test(sheet1_D58, "/LTE/", "D58", 1.0).
?test(sheet1_E58, "/LTE/", "E58", 1.0).
?test(sheet1_F58, "/LTE/", "F58", 1.0).
?test(sheet1_G58, "/LTE/", "G58", 1.0).
?test(sheet1_H58, "/LTE/", "H58", 1.0).
?test(sheet1_I58, "/LTE/", "I58", 1.0).
?test(sheet1_J58, "/LTE/", "J58", 1.0).
?test(sheet1_K58, "/LTE/", "K58", 1.0).
?test(sheet1_L58, "/LTE/", "L58", 1.0).
?test(sheet1_M58, "/LTE/", "M58", 1.0).
?test(sheet1_N58, "/LTE/", "N58", 1.0).
?test(sheet1_O58, "/LTE/", "O58", 1.0).
?test(sheet1_P58, "/LTE/", "P58", 1.0).
?test(sheet1_Q58, "/LTE/", "Q58", 1.0).
?test(sheet1_R58, "/LTE/", "R58", 1.0).
?test(sheet1_S58, "/LTE/", "S58", 1.0).
?test(sheet1_T58, "/LTE/", "T58", 1.0).
?test(sheet1_U58, "/LTE/", "U58", 1.0).
?test(sheet1_V58, "/LTE/", "V58", 1.0).
?test(sheet1_C59, "/LTE/", "C59", 1.0).
?test(sheet1_D59, "/LTE/", "D59", 1.0).
?test(sheet1_E59, "/LTE/", "E59", 1.0).
?test(sheet1_F59, "/LTE/", "F59", 1.0).
?test(sheet1_G59, "/LTE/", "G59", 1.0).
?test(sheet1_H59, "/LTE/", "H59", 1.0).
?test(sheet1_I59, "/LTE/", "I59", 1.0).
?test(sheet1_J59, "/LTE/", "J59", 1.0).
?test(sheet1_K59, "/LTE/", "K59", 1.0).
?test(sheet1_L59, "/LTE/", "L59", 1.0).
?test(sheet1_M59, "/LTE/", "M59", 1.0).
?test(sheet1_N59, "/LTE/", "N59", 1.0).
?test(sheet1_O59, "/LTE/", "O59", 1.0).
?test(sheet1_P59, "/LTE/", "P59", 1.0).
?test(sheet1_Q59, "/LTE/", "Q59", 1.0).
?test(sheet1_R59, "/LTE/", "R59", 1.0).
?test(sheet1_S59, "/LTE/", "S59", 1.0).
?test(sheet1_T59, "/LTE/", "T59", 1.0).
?test(sheet1_U59, "/LTE/", "U59", 1.0).
?test(sheet1_V59, "/LTE/", "V59", 1.0).
?test(sheet1_C60, "/LTE/", "C60", 1.0).
?test(sheet1_D60, "/LTE/", "D60", 1.0).
?test(sheet1_E60, "/LTE/", "E60", 1.0).
?test(sheet1_F60, "/LTE/", "F60", 1.0).
?test(sheet1_G60, "/LTE/", "G60", 1.0).
?test(sheet1_H60, "/LTE/", "H60", 1.0).
?test(sheet1_I60, "/LTE/", "I60", 1.0).
?test(sheet1_J60, "/LTE/", "J60", 1.0).
?test(sheet1_K60, "/LTE/", "K60", 1.0).
?test(sheet1_L60, "/LTE/", "L60", 1.0).
?test(sheet1_M60, "/LTE/", "M60", 1.0).
?test(sheet1_N60, "/LTE/", "N60", 1.0).
?test(sheet1_O60, "/LTE/", "O60", 1.0).
?test(sheet1_P60, "/LTE/", "P60", 1.0).
?test(sheet1_Q60, "/LTE/", "Q60", 1.0).
?test(sheet1_R60, "/LTE/", "R60", 1.0).
?test(sheet1_S60, "/LTE/", "S60", 1.0).
?test(sheet1_T60, "/LTE/", "T60", 1.0).
?test(sheet1_U60, "/LTE/", "U60", 1.0).
?test(sheet1_V60, "/LTE/", "V60", 1.0).
?test(sheet1_C61, "/LTE/", "C61", 1.0).
?test(sheet1_D61, "/LTE/", "D61", 1.0).
?test(sheet1_E61, "/LTE/", "E61", 1.0).
?test(sheet1_F61, "/LTE/", "F61", 1.0).
?test(sheet1_G61, "/LTE/", "G61", 1.0).
?test(sheet1_H61, "/LTE/", "H61", 1.0).
?test(sheet1_I61, "/LTE/", "I61", 1.0).
?test(sheet1_J61, "/LTE/", "J61", 1.0).
?test(sheet1_K61, "/LTE/", "K61", 1.0).
?test(sheet1_L61, "/LTE/", "L61", 1.0).
?test(sheet1_M61, "/LTE/", "M61", 1.0).
?test(sheet1_N61, "/LTE/", "N61", 1.0).
?test(sheet1_O61, "/LTE/", "O61", 1.0).
?test(sheet1_P61, "/LTE/", "P61", 1.0).
?test(sheet1_Q61, "/LTE/", "Q61", 1.0).
?test(sheet1_R61, "/LTE/", "R61", 1.0).
?test(sheet1_S61, "/LTE/", "S61", 1.0).
?test(sheet1_T61, "/LTE/", "T61", 1.0).
?test(sheet1_U61, "/LTE/", "U61", 1.0).
?test(sheet1_V61, "/LTE/", "V61", 1.0).
?test(sheet1_C62, "/LTE/", "C62", 1.0).
?test(sheet1_D62, "/LTE/", "D62", 1.0).
?test(sheet1_E62, "/LTE/", "E62", 1.0).
?test(sheet1_F62, "/LTE/", "F62", 1.0).
?test(sheet1_G62, "/LTE/", "G62", 1.0).
?test(sheet1_H62, "/LTE/", "H62", 1.0).
?test(sheet1_I62, "/LTE/", "I62", 1.0).
?test(sheet1_J62, "/LTE/", "J62", 1.0).
?test(sheet1_K62, "/LTE/", "K62", 1.0).
?test(sheet1_L62, "/LTE/", "L62", 1.0).
?test(sheet1_M62, "/LTE/", "M62", 1.0).
?test(sheet1_N62, "/LTE/", "N62", 1.0).
?test(sheet1_O62, "/LTE/", "O62", 1.0).
?test(sheet1_P62, "/LTE/", "P62", 1.0).
?test(sheet1_Q62, "/LTE/", "Q62", 1.0).
?test(sheet1_R62, "/LTE/", "R62", 1.0).
?test(sheet1_S62, "/LTE/", "S62", 1.0).
?test(sheet1_T62, "/LTE/", "T62", 1.0).
?test(sheet1_U62, "/LTE/", "U62", 1.0).
?test(sheet1_V62, "/LTE/", "V62", 1.0).
?test(sheet1_C63, "/LTE/", "C63", 1.0).
?test(sheet1_D63, "/LTE/", "D63", 1.0).
?test(sheet1_E63, "/LTE/", "E63", 1.0).
?test(sheet1_F63, "/LTE/", "F63", 1.0).
?test(sheet1_G63, "/LTE/", "G63", 1.0).
?test(sheet1_H63, "/LTE/", "H63", 1.0).
?test(sheet1_I63, "/LTE/", "I63", 1.0).
?test(sheet1_J63, "/LTE/", "J63", 1.0).
?test(sheet1_K63, "/LTE/", "K63", 1.0).
?test(sheet1_L63, "/LTE/", "L63", 1.0).
?test(sheet1_M63, "/LTE/", "M63", 1.0).
?test(sheet1_N63, "/LTE/", "N63", 1.0).
?test(sheet1_O63, "/LTE/", "O63", 1.0).
?test(sheet1_P63, "/LTE/", "P63", 1.0).
?test(sheet1_Q63, "/LTE/", "Q63", 1.0).
?test(sheet1_R63, "/LTE/", "R63", 1.0).
?test(sheet1_S63, "/LTE/", "S63", 1.0).
?test(sheet1_T63, "/LTE/", "T63", 1.0).
?test(sheet1_U63, "/LTE/", "U63", 1.0).
?test(sheet1_V63, "/LTE/", "V63", 1.0).
?test(sheet1_C64, "/LTE/", "C64", 1.0).
?test(sheet1_D64, "/LTE/", "D64", 1.0).
?test(sheet1_E64, "/LTE/", "E64", 1.0).
?test(sheet1_F64, "/LTE/", "F64", 1.0).
?test(sheet1_G64, "/LTE/", "G64", 1.0).
?test(sheet1_H64, "/LTE/", "H64", 1.0).
?test(sheet1_I64, "/LTE/", "I64", 1.0).
?test(sheet1_J64, "/LTE/", "J64", 1.0).
?test(sheet1_K64, "/LTE/", "K64", 1.0).
?test(sheet1_L64, "/LTE/", "L64", 1.0).
?test(sheet1_M64, "/LTE/", "M64", 1.0).
?test(sheet1_N64, "/LTE/", "N64", 1.0).
?test(sheet1_O64, "/LTE/", "O64", 1.0).
?test(sheet1_P64, "/LTE/", "P64", 1.0).
?test(sheet1_Q64, "/LTE/", "Q64", 1.0).
?test(sheet1_R64, "/LTE/", "R64", 1.0).
?test(sheet1_S64, "/LTE/", "S64", 1.0).
?test(sheet1_T64, "/LTE/", "T64", 1.0).
?test(sheet1_U64, "/LTE/", "U64", 1.0).
?test(sheet1_V64, "/LTE/", "V64", 1.0).
?test(sheet1_C65, "/LTE/", "C65", 1.0).
?test(sheet1_D65, "/LTE/", "D65", 1.0).
?test(sheet1_E65, "/LTE/", "E65", 1.0).
?test(sheet1_F65, "/LTE/", "F65", 1.0).
?test(sheet1_G65, "/LTE/", "G65", 1.0).
?test(sheet1_H65, "/LTE/", "H65", 1.0).
?test(sheet1_I65, "/LTE/", "I65", 1.0).
?test(sheet1_J65, "/LTE/", "J65", 1.0).
?test(sheet1_K65, "/LTE/", "K65", 1.0).
?test(sheet1_L65, "/LTE/", "L65", 1.0).
?test(sheet1_M65, "/LTE/", "M65", 1.0).
?test(sheet1_N65, "/LTE/", "N65", 1.0).
?test(sheet1_O65, "/LTE/", "O65", 1.0).
?test(sheet1_P65, "/LTE/", "P65", 1.0).
?test(sheet1_Q65, "/LTE/", "Q65", 1.0).
?test(sheet1_R65, "/LTE/", "R65", 1.0).
?test(sheet1_S65, "/LTE/", "S65", 1.0).
?test(sheet1_T65, "/LTE/", "T65", 1.0).
?test(sheet1_U65, "/LTE/", "U65", 1.0).
?test(sheet1_V65, "/LTE/", "V65", 1.0).
?test(sheet1_C66, "/LTE/", "C66", 1.0).
?test(sheet1_D66, "/LTE/", "D66", 1.0).
?test(sheet1_E66, "/LTE/", "E66", 1.0).
?test(sheet1_F66, "/LTE/", "F66", 1.0).
?test(sheet1_G66, "/LTE/", "G66", 1.0).
?test(sheet1_H66, "/LTE/", "H66", 1.0).
?test(sheet1_I66, "/LTE/", "I66", 1.0).
?test(sheet1_J66, "/LTE/", "J66", 1.0).
?test(sheet1_K66, "/LTE/", "K66", 1.0).
?test(sheet1_L66, "/LTE/", "L66", 1.0).
?test(sheet1_M66, "/LTE/", "M66", 1.0).
?test(sheet1_N66, "/LTE/", "N66", 1.0).
?test(sheet1_O66, "/LTE/", "O66", 1.0).
?test(sheet1_P66, "/LTE/", "P66", 1.0).
?test(sheet1_Q66, "/LTE/", "Q66", 1.0).
?test(sheet1_R66, "/LTE/", "R66", 1.0).
?test(sheet1_S66, "/LTE/", "S66", 1.0).
?test(sheet1_T66, "/LTE/", "T66", 1.0).
?test(sheet1_U66, "/LTE/", "U66", 1.0).
?test(sheet1_V66, "/LTE/", "V66", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_operators_lte.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_operators_lte" ++ "/" ++ Sheetname ++ "/",
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
