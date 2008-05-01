%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_operators_eq_SUITE).
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
                     [Testcase, "e_gnumeric_operators_eq_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_operators_eq" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/EQUAL/", "A1", "=").
?test(sheet1_B1, "/EQUAL/", "B1", "B").
?test(sheet1_C1, "/EQUAL/", "C1", "Blank").
?test(sheet1_D1, "/EQUAL/", "D1", "Boolean").
?test(sheet1_E1, "/EQUAL/", "E1", "Boolean").
?test(sheet1_F1, "/EQUAL/", "F1", "Error").
?test(sheet1_G1, "/EQUAL/", "G1", "Error").
?test(sheet1_H1, "/EQUAL/", "H1", "Error").
?test(sheet1_I1, "/EQUAL/", "I1", "Error").
?test(sheet1_J1, "/EQUAL/", "J1", "Error").
?test(sheet1_K1, "/EQUAL/", "K1", "Error").
?test(sheet1_L1, "/EQUAL/", "L1", "Error").
?test(sheet1_M1, "/EQUAL/", "M1", "String").
?test(sheet1_N1, "/EQUAL/", "N1", "String").
?test(sheet1_O1, "/EQUAL/", "O1", "String").
?test(sheet1_P1, "/EQUAL/", "P1", "Str Num").
?test(sheet1_Q1, "/EQUAL/", "Q1", "Str Num").
?test(sheet1_R1, "/EQUAL/", "R1", "Integer").
?test(sheet1_S1, "/EQUAL/", "S1", "Integer").
?test(sheet1_T1, "/EQUAL/", "T1", "Zero").
?test(sheet1_U1, "/EQUAL/", "U1", "Float").
?test(sheet1_V1, "/EQUAL/", "V1", "Float").
?test(sheet1_A2, "/EQUAL/", "A2", "A").
?test(sheet1_D2, "/EQUAL/", "D2", true).
?test(sheet1_E2, "/EQUAL/", "E2", false).
?test(sheet1_F2, "/EQUAL/", "F2", '#DIV/0!').
?test(sheet1_G2, "/EQUAL/", "G2", '#N/A').
?test(sheet1_H2, "/EQUAL/", "H2", '#NAME?').
?test(sheet1_I2, "/EQUAL/", "I2", 'NULL!').
?test(sheet1_J2, "/EQUAL/", "J2", '#NUM!').
?test(sheet1_K2, "/EQUAL/", "K2", '#REF!').
?test(sheet1_L2, "/EQUAL/", "L2", '#VALUE!').
?test(sheet1_M2, "/EQUAL/", "M2", "Liz").
?test(sheet1_N2, "/EQUAL/", "N2", "Doug").
?test(sheet1_O2, "/EQUAL/", "O2", "Bob").
?test(sheet1_P2, "/EQUAL/", "P2", "2.7").
?test(sheet1_Q2, "/EQUAL/", "Q2", "3.54").
?test(sheet1_R2, "/EQUAL/", "R2", "1999/02/01 00:00:00").
?test(sheet1_S2, "/EQUAL/", "S2", "1999/02/02 00:00:00").
?test(sheet1_T2, "/EQUAL/", "T2", 0.0).
?test(sheet1_U2, "/EQUAL/", "U2", 3.1415).
?test(sheet1_V2, "/EQUAL/", "V2", 36193.2).
?test(sheet1_A3, "/EQUAL/", "A3", "Blank").
?test(sheet1_C3, "/EQUAL/", "C3", true).
?test(sheet1_D3, "/EQUAL/", "D3", false).
?test(sheet1_E3, "/EQUAL/", "E3", true).
?test(sheet1_F3, "/EQUAL/", "F3", '#DIV/0!').
?test(sheet1_G3, "/EQUAL/", "G3", '#N/A').
?test(sheet1_H3, "/EQUAL/", "H3", '#NAME?').
?test(sheet1_I3, "/EQUAL/", "I3", 'NULL!').
?test(sheet1_J3, "/EQUAL/", "J3", '#NUM!').
?test(sheet1_K3, "/EQUAL/", "K3", '#REF!').
?test(sheet1_L3, "/EQUAL/", "L3", '#VALUE!').
?test(sheet1_M3, "/EQUAL/", "M3", false).
?test(sheet1_N3, "/EQUAL/", "N3", false).
?test(sheet1_O3, "/EQUAL/", "O3", false).
?test(sheet1_P3, "/EQUAL/", "P3", false).
?test(sheet1_Q3, "/EQUAL/", "Q3", false).
?test(sheet1_R3, "/EQUAL/", "R3", false).
?test(sheet1_S3, "/EQUAL/", "S3", false).
?test(sheet1_T3, "/EQUAL/", "T3", true).
?test(sheet1_U3, "/EQUAL/", "U3", false).
?test(sheet1_V3, "/EQUAL/", "V3", false).
?test(sheet1_A4, "/EQUAL/", "A4", "Boolean").
?test(sheet1_B4, "/EQUAL/", "B4", true).
?test(sheet1_C4, "/EQUAL/", "C4", false).
?test(sheet1_D4, "/EQUAL/", "D4", true).
?test(sheet1_E4, "/EQUAL/", "E4", false).
?test(sheet1_F4, "/EQUAL/", "F4", '#DIV/0!').
?test(sheet1_G4, "/EQUAL/", "G4", '#N/A').
?test(sheet1_H4, "/EQUAL/", "H4", '#NAME?').
?test(sheet1_I4, "/EQUAL/", "I4", 'NULL!').
?test(sheet1_J4, "/EQUAL/", "J4", '#NUM!').
?test(sheet1_K4, "/EQUAL/", "K4", '#REF!').
?test(sheet1_L4, "/EQUAL/", "L4", '#VALUE!').
?test(sheet1_M4, "/EQUAL/", "M4", false).
?test(sheet1_N4, "/EQUAL/", "N4", false).
?test(sheet1_O4, "/EQUAL/", "O4", false).
?test(sheet1_P4, "/EQUAL/", "P4", false).
?test(sheet1_Q4, "/EQUAL/", "Q4", false).
?test(sheet1_R4, "/EQUAL/", "R4", false).
?test(sheet1_S4, "/EQUAL/", "S4", false).
?test(sheet1_T4, "/EQUAL/", "T4", false).
?test(sheet1_U4, "/EQUAL/", "U4", false).
?test(sheet1_V4, "/EQUAL/", "V4", false).
?test(sheet1_A5, "/EQUAL/", "A5", "Boolean").
?test(sheet1_B5, "/EQUAL/", "B5", false).
?test(sheet1_C5, "/EQUAL/", "C5", true).
?test(sheet1_D5, "/EQUAL/", "D5", false).
?test(sheet1_E5, "/EQUAL/", "E5", true).
?test(sheet1_F5, "/EQUAL/", "F5", '#DIV/0!').
?test(sheet1_G5, "/EQUAL/", "G5", '#N/A').
?test(sheet1_H5, "/EQUAL/", "H5", '#NAME?').
?test(sheet1_I5, "/EQUAL/", "I5", 'NULL!').
?test(sheet1_J5, "/EQUAL/", "J5", '#NUM!').
?test(sheet1_K5, "/EQUAL/", "K5", '#REF!').
?test(sheet1_L5, "/EQUAL/", "L5", '#VALUE!').
?test(sheet1_M5, "/EQUAL/", "M5", false).
?test(sheet1_N5, "/EQUAL/", "N5", false).
?test(sheet1_O5, "/EQUAL/", "O5", false).
?test(sheet1_P5, "/EQUAL/", "P5", false).
?test(sheet1_Q5, "/EQUAL/", "Q5", false).
?test(sheet1_R5, "/EQUAL/", "R5", false).
?test(sheet1_S5, "/EQUAL/", "S5", false).
?test(sheet1_T5, "/EQUAL/", "T5", false).
?test(sheet1_U5, "/EQUAL/", "U5", false).
?test(sheet1_V5, "/EQUAL/", "V5", false).
?test(sheet1_A6, "/EQUAL/", "A6", "Error").
?test(sheet1_B6, "/EQUAL/", "B6", '#DIV/0!').
?test(sheet1_C6, "/EQUAL/", "C6", '#DIV/0!').
?test(sheet1_D6, "/EQUAL/", "D6", '#DIV/0!').
?test(sheet1_E6, "/EQUAL/", "E6", '#DIV/0!').
?test(sheet1_F6, "/EQUAL/", "F6", '#DIV/0!').
?test(sheet1_G6, "/EQUAL/", "G6", '#DIV/0!').
?test(sheet1_H6, "/EQUAL/", "H6", '#DIV/0!').
?test(sheet1_I6, "/EQUAL/", "I6", '#DIV/0!').
?test(sheet1_J6, "/EQUAL/", "J6", '#DIV/0!').
?test(sheet1_K6, "/EQUAL/", "K6", '#DIV/0!').
?test(sheet1_L6, "/EQUAL/", "L6", '#DIV/0!').
?test(sheet1_M6, "/EQUAL/", "M6", '#DIV/0!').
?test(sheet1_N6, "/EQUAL/", "N6", '#DIV/0!').
?test(sheet1_O6, "/EQUAL/", "O6", '#DIV/0!').
?test(sheet1_P6, "/EQUAL/", "P6", '#DIV/0!').
?test(sheet1_Q6, "/EQUAL/", "Q6", '#DIV/0!').
?test(sheet1_R6, "/EQUAL/", "R6", '#DIV/0!').
?test(sheet1_S6, "/EQUAL/", "S6", '#DIV/0!').
?test(sheet1_T6, "/EQUAL/", "T6", '#DIV/0!').
?test(sheet1_U6, "/EQUAL/", "U6", '#DIV/0!').
?test(sheet1_V6, "/EQUAL/", "V6", '#DIV/0!').
?test(sheet1_A7, "/EQUAL/", "A7", "Error").
?test(sheet1_B7, "/EQUAL/", "B7", '#N/A').
?test(sheet1_C7, "/EQUAL/", "C7", '#N/A').
?test(sheet1_D7, "/EQUAL/", "D7", '#N/A').
?test(sheet1_E7, "/EQUAL/", "E7", '#N/A').
?test(sheet1_F7, "/EQUAL/", "F7", '#N/A').
?test(sheet1_G7, "/EQUAL/", "G7", '#N/A').
?test(sheet1_H7, "/EQUAL/", "H7", '#N/A').
?test(sheet1_I7, "/EQUAL/", "I7", '#N/A').
?test(sheet1_J7, "/EQUAL/", "J7", '#N/A').
?test(sheet1_K7, "/EQUAL/", "K7", '#N/A').
?test(sheet1_L7, "/EQUAL/", "L7", '#N/A').
?test(sheet1_M7, "/EQUAL/", "M7", '#N/A').
?test(sheet1_N7, "/EQUAL/", "N7", '#N/A').
?test(sheet1_O7, "/EQUAL/", "O7", '#N/A').
?test(sheet1_P7, "/EQUAL/", "P7", '#N/A').
?test(sheet1_Q7, "/EQUAL/", "Q7", '#N/A').
?test(sheet1_R7, "/EQUAL/", "R7", '#N/A').
?test(sheet1_S7, "/EQUAL/", "S7", '#N/A').
?test(sheet1_T7, "/EQUAL/", "T7", '#N/A').
?test(sheet1_U7, "/EQUAL/", "U7", '#N/A').
?test(sheet1_V7, "/EQUAL/", "V7", '#N/A').
?test(sheet1_A8, "/EQUAL/", "A8", "Error").
?test(sheet1_B8, "/EQUAL/", "B8", '#NAME?').
?test(sheet1_C8, "/EQUAL/", "C8", '#NAME?').
?test(sheet1_D8, "/EQUAL/", "D8", '#NAME?').
?test(sheet1_E8, "/EQUAL/", "E8", '#NAME?').
?test(sheet1_F8, "/EQUAL/", "F8", '#NAME?').
?test(sheet1_G8, "/EQUAL/", "G8", '#NAME?').
?test(sheet1_H8, "/EQUAL/", "H8", '#NAME?').
?test(sheet1_I8, "/EQUAL/", "I8", '#NAME?').
?test(sheet1_J8, "/EQUAL/", "J8", '#NAME?').
?test(sheet1_K8, "/EQUAL/", "K8", '#NAME?').
?test(sheet1_L8, "/EQUAL/", "L8", '#NAME?').
?test(sheet1_M8, "/EQUAL/", "M8", '#NAME?').
?test(sheet1_N8, "/EQUAL/", "N8", '#NAME?').
?test(sheet1_O8, "/EQUAL/", "O8", '#NAME?').
?test(sheet1_P8, "/EQUAL/", "P8", '#NAME?').
?test(sheet1_Q8, "/EQUAL/", "Q8", '#NAME?').
?test(sheet1_R8, "/EQUAL/", "R8", '#NAME?').
?test(sheet1_S8, "/EQUAL/", "S8", '#NAME?').
?test(sheet1_T8, "/EQUAL/", "T8", '#NAME?').
?test(sheet1_U8, "/EQUAL/", "U8", '#NAME?').
?test(sheet1_V8, "/EQUAL/", "V8", '#NAME?').
?test(sheet1_A9, "/EQUAL/", "A9", "Error").
?test(sheet1_B9, "/EQUAL/", "B9", 'NULL!').
?test(sheet1_C9, "/EQUAL/", "C9", 'NULL!').
?test(sheet1_D9, "/EQUAL/", "D9", 'NULL!').
?test(sheet1_E9, "/EQUAL/", "E9", 'NULL!').
?test(sheet1_F9, "/EQUAL/", "F9", 'NULL!').
?test(sheet1_G9, "/EQUAL/", "G9", 'NULL!').
?test(sheet1_H9, "/EQUAL/", "H9", 'NULL!').
?test(sheet1_I9, "/EQUAL/", "I9", 'NULL!').
?test(sheet1_J9, "/EQUAL/", "J9", 'NULL!').
?test(sheet1_K9, "/EQUAL/", "K9", 'NULL!').
?test(sheet1_L9, "/EQUAL/", "L9", 'NULL!').
?test(sheet1_M9, "/EQUAL/", "M9", 'NULL!').
?test(sheet1_N9, "/EQUAL/", "N9", 'NULL!').
?test(sheet1_O9, "/EQUAL/", "O9", 'NULL!').
?test(sheet1_P9, "/EQUAL/", "P9", 'NULL!').
?test(sheet1_Q9, "/EQUAL/", "Q9", 'NULL!').
?test(sheet1_R9, "/EQUAL/", "R9", 'NULL!').
?test(sheet1_S9, "/EQUAL/", "S9", 'NULL!').
?test(sheet1_T9, "/EQUAL/", "T9", 'NULL!').
?test(sheet1_U9, "/EQUAL/", "U9", 'NULL!').
?test(sheet1_V9, "/EQUAL/", "V9", 'NULL!').
?test(sheet1_A10, "/EQUAL/", "A10", "Error").
?test(sheet1_B10, "/EQUAL/", "B10", '#NUM!').
?test(sheet1_C10, "/EQUAL/", "C10", '#NUM!').
?test(sheet1_D10, "/EQUAL/", "D10", '#NUM!').
?test(sheet1_E10, "/EQUAL/", "E10", '#NUM!').
?test(sheet1_F10, "/EQUAL/", "F10", '#NUM!').
?test(sheet1_G10, "/EQUAL/", "G10", '#NUM!').
?test(sheet1_H10, "/EQUAL/", "H10", '#NUM!').
?test(sheet1_I10, "/EQUAL/", "I10", '#NUM!').
?test(sheet1_J10, "/EQUAL/", "J10", '#NUM!').
?test(sheet1_K10, "/EQUAL/", "K10", '#NUM!').
?test(sheet1_L10, "/EQUAL/", "L10", '#NUM!').
?test(sheet1_M10, "/EQUAL/", "M10", '#NUM!').
?test(sheet1_N10, "/EQUAL/", "N10", '#NUM!').
?test(sheet1_O10, "/EQUAL/", "O10", '#NUM!').
?test(sheet1_P10, "/EQUAL/", "P10", '#NUM!').
?test(sheet1_Q10, "/EQUAL/", "Q10", '#NUM!').
?test(sheet1_R10, "/EQUAL/", "R10", '#NUM!').
?test(sheet1_S10, "/EQUAL/", "S10", '#NUM!').
?test(sheet1_T10, "/EQUAL/", "T10", '#NUM!').
?test(sheet1_U10, "/EQUAL/", "U10", '#NUM!').
?test(sheet1_V10, "/EQUAL/", "V10", '#NUM!').
?test(sheet1_A11, "/EQUAL/", "A11", "Error").
?test(sheet1_B11, "/EQUAL/", "B11", '#REF!').
?test(sheet1_C11, "/EQUAL/", "C11", '#REF!').
?test(sheet1_D11, "/EQUAL/", "D11", '#REF!').
?test(sheet1_E11, "/EQUAL/", "E11", '#REF!').
?test(sheet1_F11, "/EQUAL/", "F11", '#REF!').
?test(sheet1_G11, "/EQUAL/", "G11", '#REF!').
?test(sheet1_H11, "/EQUAL/", "H11", '#REF!').
?test(sheet1_I11, "/EQUAL/", "I11", '#REF!').
?test(sheet1_J11, "/EQUAL/", "J11", '#REF!').
?test(sheet1_K11, "/EQUAL/", "K11", '#REF!').
?test(sheet1_L11, "/EQUAL/", "L11", '#REF!').
?test(sheet1_M11, "/EQUAL/", "M11", '#REF!').
?test(sheet1_N11, "/EQUAL/", "N11", '#REF!').
?test(sheet1_O11, "/EQUAL/", "O11", '#REF!').
?test(sheet1_P11, "/EQUAL/", "P11", '#REF!').
?test(sheet1_Q11, "/EQUAL/", "Q11", '#REF!').
?test(sheet1_R11, "/EQUAL/", "R11", '#REF!').
?test(sheet1_S11, "/EQUAL/", "S11", '#REF!').
?test(sheet1_T11, "/EQUAL/", "T11", '#REF!').
?test(sheet1_U11, "/EQUAL/", "U11", '#REF!').
?test(sheet1_V11, "/EQUAL/", "V11", '#REF!').
?test(sheet1_A12, "/EQUAL/", "A12", "Error").
?test(sheet1_B12, "/EQUAL/", "B12", '#VALUE!').
?test(sheet1_C12, "/EQUAL/", "C12", '#VALUE!').
?test(sheet1_D12, "/EQUAL/", "D12", '#VALUE!').
?test(sheet1_E12, "/EQUAL/", "E12", '#VALUE!').
?test(sheet1_F12, "/EQUAL/", "F12", '#VALUE!').
?test(sheet1_G12, "/EQUAL/", "G12", '#VALUE!').
?test(sheet1_H12, "/EQUAL/", "H12", '#VALUE!').
?test(sheet1_I12, "/EQUAL/", "I12", '#VALUE!').
?test(sheet1_J12, "/EQUAL/", "J12", '#VALUE!').
?test(sheet1_K12, "/EQUAL/", "K12", '#VALUE!').
?test(sheet1_L12, "/EQUAL/", "L12", '#VALUE!').
?test(sheet1_M12, "/EQUAL/", "M12", '#VALUE!').
?test(sheet1_N12, "/EQUAL/", "N12", '#VALUE!').
?test(sheet1_O12, "/EQUAL/", "O12", '#VALUE!').
?test(sheet1_P12, "/EQUAL/", "P12", '#VALUE!').
?test(sheet1_Q12, "/EQUAL/", "Q12", '#VALUE!').
?test(sheet1_R12, "/EQUAL/", "R12", '#VALUE!').
?test(sheet1_S12, "/EQUAL/", "S12", '#VALUE!').
?test(sheet1_T12, "/EQUAL/", "T12", '#VALUE!').
?test(sheet1_U12, "/EQUAL/", "U12", '#VALUE!').
?test(sheet1_V12, "/EQUAL/", "V12", '#VALUE!').
?test(sheet1_A13, "/EQUAL/", "A13", "String").
?test(sheet1_B13, "/EQUAL/", "B13", "Liz").
?test(sheet1_C13, "/EQUAL/", "C13", false).
?test(sheet1_D13, "/EQUAL/", "D13", false).
?test(sheet1_E13, "/EQUAL/", "E13", false).
?test(sheet1_F13, "/EQUAL/", "F13", '#DIV/0!').
?test(sheet1_G13, "/EQUAL/", "G13", '#N/A').
?test(sheet1_H13, "/EQUAL/", "H13", '#NAME?').
?test(sheet1_I13, "/EQUAL/", "I13", 'NULL!').
?test(sheet1_J13, "/EQUAL/", "J13", '#NUM!').
?test(sheet1_K13, "/EQUAL/", "K13", '#REF!').
?test(sheet1_L13, "/EQUAL/", "L13", '#VALUE!').
?test(sheet1_M13, "/EQUAL/", "M13", true).
?test(sheet1_N13, "/EQUAL/", "N13", false).
?test(sheet1_O13, "/EQUAL/", "O13", false).
?test(sheet1_P13, "/EQUAL/", "P13", false).
?test(sheet1_Q13, "/EQUAL/", "Q13", false).
?test(sheet1_R13, "/EQUAL/", "R13", false).
?test(sheet1_S13, "/EQUAL/", "S13", false).
?test(sheet1_T13, "/EQUAL/", "T13", false).
?test(sheet1_U13, "/EQUAL/", "U13", false).
?test(sheet1_V13, "/EQUAL/", "V13", false).
?test(sheet1_A14, "/EQUAL/", "A14", "String").
?test(sheet1_B14, "/EQUAL/", "B14", "Doug").
?test(sheet1_C14, "/EQUAL/", "C14", false).
?test(sheet1_D14, "/EQUAL/", "D14", false).
?test(sheet1_E14, "/EQUAL/", "E14", false).
?test(sheet1_F14, "/EQUAL/", "F14", '#DIV/0!').
?test(sheet1_G14, "/EQUAL/", "G14", '#N/A').
?test(sheet1_H14, "/EQUAL/", "H14", '#NAME?').
?test(sheet1_I14, "/EQUAL/", "I14", 'NULL!').
?test(sheet1_J14, "/EQUAL/", "J14", '#NUM!').
?test(sheet1_K14, "/EQUAL/", "K14", '#REF!').
?test(sheet1_L14, "/EQUAL/", "L14", '#VALUE!').
?test(sheet1_M14, "/EQUAL/", "M14", false).
?test(sheet1_N14, "/EQUAL/", "N14", true).
?test(sheet1_O14, "/EQUAL/", "O14", false).
?test(sheet1_P14, "/EQUAL/", "P14", false).
?test(sheet1_Q14, "/EQUAL/", "Q14", false).
?test(sheet1_R14, "/EQUAL/", "R14", false).
?test(sheet1_S14, "/EQUAL/", "S14", false).
?test(sheet1_T14, "/EQUAL/", "T14", false).
?test(sheet1_U14, "/EQUAL/", "U14", false).
?test(sheet1_V14, "/EQUAL/", "V14", false).
?test(sheet1_A15, "/EQUAL/", "A15", "String").
?test(sheet1_B15, "/EQUAL/", "B15", "Bob").
?test(sheet1_C15, "/EQUAL/", "C15", false).
?test(sheet1_D15, "/EQUAL/", "D15", false).
?test(sheet1_E15, "/EQUAL/", "E15", false).
?test(sheet1_F15, "/EQUAL/", "F15", '#DIV/0!').
?test(sheet1_G15, "/EQUAL/", "G15", '#N/A').
?test(sheet1_H15, "/EQUAL/", "H15", '#NAME?').
?test(sheet1_I15, "/EQUAL/", "I15", 'NULL!').
?test(sheet1_J15, "/EQUAL/", "J15", '#NUM!').
?test(sheet1_K15, "/EQUAL/", "K15", '#REF!').
?test(sheet1_L15, "/EQUAL/", "L15", '#VALUE!').
?test(sheet1_M15, "/EQUAL/", "M15", false).
?test(sheet1_N15, "/EQUAL/", "N15", false).
?test(sheet1_O15, "/EQUAL/", "O15", true).
?test(sheet1_P15, "/EQUAL/", "P15", false).
?test(sheet1_Q15, "/EQUAL/", "Q15", false).
?test(sheet1_R15, "/EQUAL/", "R15", false).
?test(sheet1_S15, "/EQUAL/", "S15", false).
?test(sheet1_T15, "/EQUAL/", "T15", false).
?test(sheet1_U15, "/EQUAL/", "U15", false).
?test(sheet1_V15, "/EQUAL/", "V15", false).
?test(sheet1_A16, "/EQUAL/", "A16", "Str Num").
?test(sheet1_B16, "/EQUAL/", "B16", "2.7").
?test(sheet1_C16, "/EQUAL/", "C16", false).
?test(sheet1_D16, "/EQUAL/", "D16", false).
?test(sheet1_E16, "/EQUAL/", "E16", false).
?test(sheet1_F16, "/EQUAL/", "F16", '#DIV/0!').
?test(sheet1_G16, "/EQUAL/", "G16", '#N/A').
?test(sheet1_H16, "/EQUAL/", "H16", '#NAME?').
?test(sheet1_I16, "/EQUAL/", "I16", 'NULL!').
?test(sheet1_J16, "/EQUAL/", "J16", '#NUM!').
?test(sheet1_K16, "/EQUAL/", "K16", '#REF!').
?test(sheet1_L16, "/EQUAL/", "L16", '#VALUE!').
?test(sheet1_M16, "/EQUAL/", "M16", false).
?test(sheet1_N16, "/EQUAL/", "N16", false).
?test(sheet1_O16, "/EQUAL/", "O16", false).
?test(sheet1_P16, "/EQUAL/", "P16", true).
?test(sheet1_Q16, "/EQUAL/", "Q16", false).
?test(sheet1_R16, "/EQUAL/", "R16", false).
?test(sheet1_S16, "/EQUAL/", "S16", false).
?test(sheet1_T16, "/EQUAL/", "T16", false).
?test(sheet1_U16, "/EQUAL/", "U16", false).
?test(sheet1_V16, "/EQUAL/", "V16", false).
?test(sheet1_A17, "/EQUAL/", "A17", "Str Num").
?test(sheet1_B17, "/EQUAL/", "B17", "3.54").
?test(sheet1_C17, "/EQUAL/", "C17", false).
?test(sheet1_D17, "/EQUAL/", "D17", false).
?test(sheet1_E17, "/EQUAL/", "E17", false).
?test(sheet1_F17, "/EQUAL/", "F17", '#DIV/0!').
?test(sheet1_G17, "/EQUAL/", "G17", '#N/A').
?test(sheet1_H17, "/EQUAL/", "H17", '#NAME?').
?test(sheet1_I17, "/EQUAL/", "I17", 'NULL!').
?test(sheet1_J17, "/EQUAL/", "J17", '#NUM!').
?test(sheet1_K17, "/EQUAL/", "K17", '#REF!').
?test(sheet1_L17, "/EQUAL/", "L17", '#VALUE!').
?test(sheet1_M17, "/EQUAL/", "M17", false).
?test(sheet1_N17, "/EQUAL/", "N17", false).
?test(sheet1_O17, "/EQUAL/", "O17", false).
?test(sheet1_P17, "/EQUAL/", "P17", false).
?test(sheet1_Q17, "/EQUAL/", "Q17", true).
?test(sheet1_R17, "/EQUAL/", "R17", false).
?test(sheet1_S17, "/EQUAL/", "S17", false).
?test(sheet1_T17, "/EQUAL/", "T17", false).
?test(sheet1_U17, "/EQUAL/", "U17", false).
?test(sheet1_V17, "/EQUAL/", "V17", false).
?test(sheet1_A18, "/EQUAL/", "A18", "Integer").
?test(sheet1_B18, "/EQUAL/", "B18", "1999/02/01 00:00:00").
?test(sheet1_C18, "/EQUAL/", "C18", false).
?test(sheet1_D18, "/EQUAL/", "D18", false).
?test(sheet1_E18, "/EQUAL/", "E18", false).
?test(sheet1_F18, "/EQUAL/", "F18", '#DIV/0!').
?test(sheet1_G18, "/EQUAL/", "G18", '#N/A').
?test(sheet1_H18, "/EQUAL/", "H18", '#NAME?').
?test(sheet1_I18, "/EQUAL/", "I18", 'NULL!').
?test(sheet1_J18, "/EQUAL/", "J18", '#NUM!').
?test(sheet1_K18, "/EQUAL/", "K18", '#REF!').
?test(sheet1_L18, "/EQUAL/", "L18", '#VALUE!').
?test(sheet1_M18, "/EQUAL/", "M18", false).
?test(sheet1_N18, "/EQUAL/", "N18", false).
?test(sheet1_O18, "/EQUAL/", "O18", false).
?test(sheet1_P18, "/EQUAL/", "P18", false).
?test(sheet1_Q18, "/EQUAL/", "Q18", false).
?test(sheet1_R18, "/EQUAL/", "R18", true).
?test(sheet1_S18, "/EQUAL/", "S18", false).
?test(sheet1_T18, "/EQUAL/", "T18", false).
?test(sheet1_U18, "/EQUAL/", "U18", false).
?test(sheet1_V18, "/EQUAL/", "V18", false).
?test(sheet1_A19, "/EQUAL/", "A19", "Integer").
?test(sheet1_B19, "/EQUAL/", "B19", "1999/02/02 00:00:00").
?test(sheet1_C19, "/EQUAL/", "C19", false).
?test(sheet1_D19, "/EQUAL/", "D19", false).
?test(sheet1_E19, "/EQUAL/", "E19", false).
?test(sheet1_F19, "/EQUAL/", "F19", '#DIV/0!').
?test(sheet1_G19, "/EQUAL/", "G19", '#N/A').
?test(sheet1_H19, "/EQUAL/", "H19", '#NAME?').
?test(sheet1_I19, "/EQUAL/", "I19", 'NULL!').
?test(sheet1_J19, "/EQUAL/", "J19", '#NUM!').
?test(sheet1_K19, "/EQUAL/", "K19", '#REF!').
?test(sheet1_L19, "/EQUAL/", "L19", '#VALUE!').
?test(sheet1_M19, "/EQUAL/", "M19", false).
?test(sheet1_N19, "/EQUAL/", "N19", false).
?test(sheet1_O19, "/EQUAL/", "O19", false).
?test(sheet1_P19, "/EQUAL/", "P19", false).
?test(sheet1_Q19, "/EQUAL/", "Q19", false).
?test(sheet1_R19, "/EQUAL/", "R19", false).
?test(sheet1_S19, "/EQUAL/", "S19", true).
?test(sheet1_T19, "/EQUAL/", "T19", false).
?test(sheet1_U19, "/EQUAL/", "U19", false).
?test(sheet1_V19, "/EQUAL/", "V19", false).
?test(sheet1_A20, "/EQUAL/", "A20", "Zero").
?test(sheet1_B20, "/EQUAL/", "B20", 0.0).
?test(sheet1_C20, "/EQUAL/", "C20", true).
?test(sheet1_D20, "/EQUAL/", "D20", false).
?test(sheet1_E20, "/EQUAL/", "E20", false).
?test(sheet1_F20, "/EQUAL/", "F20", '#DIV/0!').
?test(sheet1_G20, "/EQUAL/", "G20", '#N/A').
?test(sheet1_H20, "/EQUAL/", "H20", '#NAME?').
?test(sheet1_I20, "/EQUAL/", "I20", 'NULL!').
?test(sheet1_J20, "/EQUAL/", "J20", '#NUM!').
?test(sheet1_K20, "/EQUAL/", "K20", '#REF!').
?test(sheet1_L20, "/EQUAL/", "L20", '#VALUE!').
?test(sheet1_M20, "/EQUAL/", "M20", false).
?test(sheet1_N20, "/EQUAL/", "N20", false).
?test(sheet1_O20, "/EQUAL/", "O20", false).
?test(sheet1_P20, "/EQUAL/", "P20", false).
?test(sheet1_Q20, "/EQUAL/", "Q20", false).
?test(sheet1_R20, "/EQUAL/", "R20", false).
?test(sheet1_S20, "/EQUAL/", "S20", false).
?test(sheet1_T20, "/EQUAL/", "T20", true).
?test(sheet1_U20, "/EQUAL/", "U20", false).
?test(sheet1_V20, "/EQUAL/", "V20", false).
?test(sheet1_A21, "/EQUAL/", "A21", "Float").
?test(sheet1_B21, "/EQUAL/", "B21", 3.1415).
?test(sheet1_C21, "/EQUAL/", "C21", false).
?test(sheet1_D21, "/EQUAL/", "D21", false).
?test(sheet1_E21, "/EQUAL/", "E21", false).
?test(sheet1_F21, "/EQUAL/", "F21", '#DIV/0!').
?test(sheet1_G21, "/EQUAL/", "G21", '#N/A').
?test(sheet1_H21, "/EQUAL/", "H21", '#NAME?').
?test(sheet1_I21, "/EQUAL/", "I21", 'NULL!').
?test(sheet1_J21, "/EQUAL/", "J21", '#NUM!').
?test(sheet1_K21, "/EQUAL/", "K21", '#REF!').
?test(sheet1_L21, "/EQUAL/", "L21", '#VALUE!').
?test(sheet1_M21, "/EQUAL/", "M21", false).
?test(sheet1_N21, "/EQUAL/", "N21", false).
?test(sheet1_O21, "/EQUAL/", "O21", false).
?test(sheet1_P21, "/EQUAL/", "P21", false).
?test(sheet1_Q21, "/EQUAL/", "Q21", false).
?test(sheet1_R21, "/EQUAL/", "R21", false).
?test(sheet1_S21, "/EQUAL/", "S21", false).
?test(sheet1_T21, "/EQUAL/", "T21", false).
?test(sheet1_U21, "/EQUAL/", "U21", true).
?test(sheet1_V21, "/EQUAL/", "V21", false).
?test(sheet1_A22, "/EQUAL/", "A22", "Float").
?test(sheet1_B22, "/EQUAL/", "B22", 36193.2).
?test(sheet1_C22, "/EQUAL/", "C22", false).
?test(sheet1_D22, "/EQUAL/", "D22", false).
?test(sheet1_E22, "/EQUAL/", "E22", false).
?test(sheet1_F22, "/EQUAL/", "F22", '#DIV/0!').
?test(sheet1_G22, "/EQUAL/", "G22", '#N/A').
?test(sheet1_H22, "/EQUAL/", "H22", '#NAME?').
?test(sheet1_I22, "/EQUAL/", "I22", 'NULL!').
?test(sheet1_J22, "/EQUAL/", "J22", '#NUM!').
?test(sheet1_K22, "/EQUAL/", "K22", '#REF!').
?test(sheet1_L22, "/EQUAL/", "L22", '#VALUE!').
?test(sheet1_M22, "/EQUAL/", "M22", false).
?test(sheet1_N22, "/EQUAL/", "N22", false).
?test(sheet1_O22, "/EQUAL/", "O22", false).
?test(sheet1_P22, "/EQUAL/", "P22", false).
?test(sheet1_Q22, "/EQUAL/", "Q22", false).
?test(sheet1_R22, "/EQUAL/", "R22", false).
?test(sheet1_S22, "/EQUAL/", "S22", false).
?test(sheet1_T22, "/EQUAL/", "T22", false).
?test(sheet1_U22, "/EQUAL/", "U22", false).
?test(sheet1_V22, "/EQUAL/", "V22", true).
?test(sheet1_A25, "/EQUAL/", "A25", "Blank").
?test(sheet1_C25, "/EQUAL/", "C25", true).
?test(sheet1_D25, "/EQUAL/", "D25", false).
?test(sheet1_E25, "/EQUAL/", "E25", true).
?test(sheet1_F25, "/EQUAL/", "F25", '#DIV/0!').
?test(sheet1_G25, "/EQUAL/", "G25", '#N/A').
?test(sheet1_H25, "/EQUAL/", "H25", '#NAME?').
?test(sheet1_I25, "/EQUAL/", "I25", 'NULL!').
?test(sheet1_J25, "/EQUAL/", "J25", '#NUM!').
?test(sheet1_K25, "/EQUAL/", "K25", '#REF!').
?test(sheet1_L25, "/EQUAL/", "L25", '#VALUE!').
?test(sheet1_M25, "/EQUAL/", "M25", false).
?test(sheet1_N25, "/EQUAL/", "N25", false).
?test(sheet1_O25, "/EQUAL/", "O25", false).
?test(sheet1_P25, "/EQUAL/", "P25", false).
?test(sheet1_Q25, "/EQUAL/", "Q25", false).
?test(sheet1_R25, "/EQUAL/", "R25", false).
?test(sheet1_S25, "/EQUAL/", "S25", false).
?test(sheet1_T25, "/EQUAL/", "T25", true).
?test(sheet1_U25, "/EQUAL/", "U25", false).
?test(sheet1_V25, "/EQUAL/", "V25", false).
?test(sheet1_A26, "/EQUAL/", "A26", "Boolean").
?test(sheet1_C26, "/EQUAL/", "C26", false).
?test(sheet1_D26, "/EQUAL/", "D26", true).
?test(sheet1_E26, "/EQUAL/", "E26", false).
?test(sheet1_F26, "/EQUAL/", "F26", '#DIV/0!').
?test(sheet1_G26, "/EQUAL/", "G26", '#N/A').
?test(sheet1_H26, "/EQUAL/", "H26", '#NAME?').
?test(sheet1_I26, "/EQUAL/", "I26", 'NULL!').
?test(sheet1_J26, "/EQUAL/", "J26", '#NUM!').
?test(sheet1_K26, "/EQUAL/", "K26", '#REF!').
?test(sheet1_L26, "/EQUAL/", "L26", '#VALUE!').
?test(sheet1_M26, "/EQUAL/", "M26", false).
?test(sheet1_N26, "/EQUAL/", "N26", false).
?test(sheet1_O26, "/EQUAL/", "O26", false).
?test(sheet1_P26, "/EQUAL/", "P26", false).
?test(sheet1_Q26, "/EQUAL/", "Q26", false).
?test(sheet1_R26, "/EQUAL/", "R26", false).
?test(sheet1_S26, "/EQUAL/", "S26", false).
?test(sheet1_T26, "/EQUAL/", "T26", false).
?test(sheet1_U26, "/EQUAL/", "U26", false).
?test(sheet1_V26, "/EQUAL/", "V26", false).
?test(sheet1_A27, "/EQUAL/", "A27", "Boolean").
?test(sheet1_C27, "/EQUAL/", "C27", true).
?test(sheet1_D27, "/EQUAL/", "D27", false).
?test(sheet1_E27, "/EQUAL/", "E27", true).
?test(sheet1_F27, "/EQUAL/", "F27", '#DIV/0!').
?test(sheet1_G27, "/EQUAL/", "G27", '#N/A').
?test(sheet1_H27, "/EQUAL/", "H27", '#NAME?').
?test(sheet1_I27, "/EQUAL/", "I27", 'NULL!').
?test(sheet1_J27, "/EQUAL/", "J27", '#NUM!').
?test(sheet1_K27, "/EQUAL/", "K27", '#REF!').
?test(sheet1_L27, "/EQUAL/", "L27", '#VALUE!').
?test(sheet1_M27, "/EQUAL/", "M27", false).
?test(sheet1_N27, "/EQUAL/", "N27", false).
?test(sheet1_O27, "/EQUAL/", "O27", false).
?test(sheet1_P27, "/EQUAL/", "P27", false).
?test(sheet1_Q27, "/EQUAL/", "Q27", false).
?test(sheet1_R27, "/EQUAL/", "R27", false).
?test(sheet1_S27, "/EQUAL/", "S27", false).
?test(sheet1_T27, "/EQUAL/", "T27", false).
?test(sheet1_U27, "/EQUAL/", "U27", false).
?test(sheet1_V27, "/EQUAL/", "V27", false).
?test(sheet1_A28, "/EQUAL/", "A28", "Error").
?test(sheet1_C28, "/EQUAL/", "C28", '#DIV/0!').
?test(sheet1_D28, "/EQUAL/", "D28", '#DIV/0!').
?test(sheet1_E28, "/EQUAL/", "E28", '#DIV/0!').
?test(sheet1_F28, "/EQUAL/", "F28", '#DIV/0!').
?test(sheet1_G28, "/EQUAL/", "G28", '#DIV/0!').
?test(sheet1_H28, "/EQUAL/", "H28", '#DIV/0!').
?test(sheet1_I28, "/EQUAL/", "I28", '#DIV/0!').
?test(sheet1_J28, "/EQUAL/", "J28", '#DIV/0!').
?test(sheet1_K28, "/EQUAL/", "K28", '#DIV/0!').
?test(sheet1_L28, "/EQUAL/", "L28", '#DIV/0!').
?test(sheet1_M28, "/EQUAL/", "M28", '#DIV/0!').
?test(sheet1_N28, "/EQUAL/", "N28", '#DIV/0!').
?test(sheet1_O28, "/EQUAL/", "O28", '#DIV/0!').
?test(sheet1_P28, "/EQUAL/", "P28", '#DIV/0!').
?test(sheet1_Q28, "/EQUAL/", "Q28", '#DIV/0!').
?test(sheet1_R28, "/EQUAL/", "R28", '#DIV/0!').
?test(sheet1_S28, "/EQUAL/", "S28", '#DIV/0!').
?test(sheet1_T28, "/EQUAL/", "T28", '#DIV/0!').
?test(sheet1_U28, "/EQUAL/", "U28", '#DIV/0!').
?test(sheet1_V28, "/EQUAL/", "V28", '#DIV/0!').
?test(sheet1_A29, "/EQUAL/", "A29", "Error").
?test(sheet1_C29, "/EQUAL/", "C29", '#N/A').
?test(sheet1_D29, "/EQUAL/", "D29", '#N/A').
?test(sheet1_E29, "/EQUAL/", "E29", '#N/A').
?test(sheet1_F29, "/EQUAL/", "F29", '#N/A').
?test(sheet1_G29, "/EQUAL/", "G29", '#N/A').
?test(sheet1_H29, "/EQUAL/", "H29", '#N/A').
?test(sheet1_I29, "/EQUAL/", "I29", '#N/A').
?test(sheet1_J29, "/EQUAL/", "J29", '#N/A').
?test(sheet1_K29, "/EQUAL/", "K29", '#N/A').
?test(sheet1_L29, "/EQUAL/", "L29", '#N/A').
?test(sheet1_M29, "/EQUAL/", "M29", '#N/A').
?test(sheet1_N29, "/EQUAL/", "N29", '#N/A').
?test(sheet1_O29, "/EQUAL/", "O29", '#N/A').
?test(sheet1_P29, "/EQUAL/", "P29", '#N/A').
?test(sheet1_Q29, "/EQUAL/", "Q29", '#N/A').
?test(sheet1_R29, "/EQUAL/", "R29", '#N/A').
?test(sheet1_S29, "/EQUAL/", "S29", '#N/A').
?test(sheet1_T29, "/EQUAL/", "T29", '#N/A').
?test(sheet1_U29, "/EQUAL/", "U29", '#N/A').
?test(sheet1_V29, "/EQUAL/", "V29", '#N/A').
?test(sheet1_A30, "/EQUAL/", "A30", "Error").
?test(sheet1_C30, "/EQUAL/", "C30", '#NAME?').
?test(sheet1_D30, "/EQUAL/", "D30", '#NAME?').
?test(sheet1_E30, "/EQUAL/", "E30", '#NAME?').
?test(sheet1_F30, "/EQUAL/", "F30", '#NAME?').
?test(sheet1_G30, "/EQUAL/", "G30", '#NAME?').
?test(sheet1_H30, "/EQUAL/", "H30", '#NAME?').
?test(sheet1_I30, "/EQUAL/", "I30", '#NAME?').
?test(sheet1_J30, "/EQUAL/", "J30", '#NAME?').
?test(sheet1_K30, "/EQUAL/", "K30", '#NAME?').
?test(sheet1_L30, "/EQUAL/", "L30", '#NAME?').
?test(sheet1_M30, "/EQUAL/", "M30", '#NAME?').
?test(sheet1_N30, "/EQUAL/", "N30", '#NAME?').
?test(sheet1_O30, "/EQUAL/", "O30", '#NAME?').
?test(sheet1_P30, "/EQUAL/", "P30", '#NAME?').
?test(sheet1_Q30, "/EQUAL/", "Q30", '#NAME?').
?test(sheet1_R30, "/EQUAL/", "R30", '#NAME?').
?test(sheet1_S30, "/EQUAL/", "S30", '#NAME?').
?test(sheet1_T30, "/EQUAL/", "T30", '#NAME?').
?test(sheet1_U30, "/EQUAL/", "U30", '#NAME?').
?test(sheet1_V30, "/EQUAL/", "V30", '#NAME?').
?test(sheet1_A31, "/EQUAL/", "A31", "Error").
?test(sheet1_C31, "/EQUAL/", "C31", 'NULL!').
?test(sheet1_D31, "/EQUAL/", "D31", 'NULL!').
?test(sheet1_E31, "/EQUAL/", "E31", 'NULL!').
?test(sheet1_F31, "/EQUAL/", "F31", 'NULL!').
?test(sheet1_G31, "/EQUAL/", "G31", 'NULL!').
?test(sheet1_H31, "/EQUAL/", "H31", 'NULL!').
?test(sheet1_I31, "/EQUAL/", "I31", 'NULL!').
?test(sheet1_J31, "/EQUAL/", "J31", 'NULL!').
?test(sheet1_K31, "/EQUAL/", "K31", 'NULL!').
?test(sheet1_L31, "/EQUAL/", "L31", 'NULL!').
?test(sheet1_M31, "/EQUAL/", "M31", 'NULL!').
?test(sheet1_N31, "/EQUAL/", "N31", 'NULL!').
?test(sheet1_O31, "/EQUAL/", "O31", 'NULL!').
?test(sheet1_P31, "/EQUAL/", "P31", 'NULL!').
?test(sheet1_Q31, "/EQUAL/", "Q31", 'NULL!').
?test(sheet1_R31, "/EQUAL/", "R31", 'NULL!').
?test(sheet1_S31, "/EQUAL/", "S31", 'NULL!').
?test(sheet1_T31, "/EQUAL/", "T31", 'NULL!').
?test(sheet1_U31, "/EQUAL/", "U31", 'NULL!').
?test(sheet1_V31, "/EQUAL/", "V31", 'NULL!').
?test(sheet1_A32, "/EQUAL/", "A32", "Error").
?test(sheet1_C32, "/EQUAL/", "C32", '#NUM!').
?test(sheet1_D32, "/EQUAL/", "D32", '#NUM!').
?test(sheet1_E32, "/EQUAL/", "E32", '#NUM!').
?test(sheet1_F32, "/EQUAL/", "F32", '#NUM!').
?test(sheet1_G32, "/EQUAL/", "G32", '#NUM!').
?test(sheet1_H32, "/EQUAL/", "H32", '#NUM!').
?test(sheet1_I32, "/EQUAL/", "I32", '#NUM!').
?test(sheet1_J32, "/EQUAL/", "J32", '#NUM!').
?test(sheet1_K32, "/EQUAL/", "K32", '#NUM!').
?test(sheet1_L32, "/EQUAL/", "L32", '#NUM!').
?test(sheet1_M32, "/EQUAL/", "M32", '#NUM!').
?test(sheet1_N32, "/EQUAL/", "N32", '#NUM!').
?test(sheet1_O32, "/EQUAL/", "O32", '#NUM!').
?test(sheet1_P32, "/EQUAL/", "P32", '#NUM!').
?test(sheet1_Q32, "/EQUAL/", "Q32", '#NUM!').
?test(sheet1_R32, "/EQUAL/", "R32", '#NUM!').
?test(sheet1_S32, "/EQUAL/", "S32", '#NUM!').
?test(sheet1_T32, "/EQUAL/", "T32", '#NUM!').
?test(sheet1_U32, "/EQUAL/", "U32", '#NUM!').
?test(sheet1_V32, "/EQUAL/", "V32", '#NUM!').
?test(sheet1_A33, "/EQUAL/", "A33", "Error").
?test(sheet1_C33, "/EQUAL/", "C33", '#REF!').
?test(sheet1_D33, "/EQUAL/", "D33", '#REF!').
?test(sheet1_E33, "/EQUAL/", "E33", '#REF!').
?test(sheet1_F33, "/EQUAL/", "F33", '#REF!').
?test(sheet1_G33, "/EQUAL/", "G33", '#REF!').
?test(sheet1_H33, "/EQUAL/", "H33", '#REF!').
?test(sheet1_I33, "/EQUAL/", "I33", '#REF!').
?test(sheet1_J33, "/EQUAL/", "J33", '#REF!').
?test(sheet1_K33, "/EQUAL/", "K33", '#REF!').
?test(sheet1_L33, "/EQUAL/", "L33", '#REF!').
?test(sheet1_M33, "/EQUAL/", "M33", '#REF!').
?test(sheet1_N33, "/EQUAL/", "N33", '#REF!').
?test(sheet1_O33, "/EQUAL/", "O33", '#REF!').
?test(sheet1_P33, "/EQUAL/", "P33", '#REF!').
?test(sheet1_Q33, "/EQUAL/", "Q33", '#REF!').
?test(sheet1_R33, "/EQUAL/", "R33", '#REF!').
?test(sheet1_S33, "/EQUAL/", "S33", '#REF!').
?test(sheet1_T33, "/EQUAL/", "T33", '#REF!').
?test(sheet1_U33, "/EQUAL/", "U33", '#REF!').
?test(sheet1_V33, "/EQUAL/", "V33", '#REF!').
?test(sheet1_A34, "/EQUAL/", "A34", "Error").
?test(sheet1_C34, "/EQUAL/", "C34", '#VALUE!').
?test(sheet1_D34, "/EQUAL/", "D34", '#VALUE!').
?test(sheet1_E34, "/EQUAL/", "E34", '#VALUE!').
?test(sheet1_F34, "/EQUAL/", "F34", '#VALUE!').
?test(sheet1_G34, "/EQUAL/", "G34", '#VALUE!').
?test(sheet1_H34, "/EQUAL/", "H34", '#VALUE!').
?test(sheet1_I34, "/EQUAL/", "I34", '#VALUE!').
?test(sheet1_J34, "/EQUAL/", "J34", '#VALUE!').
?test(sheet1_K34, "/EQUAL/", "K34", '#VALUE!').
?test(sheet1_L34, "/EQUAL/", "L34", '#VALUE!').
?test(sheet1_M34, "/EQUAL/", "M34", '#VALUE!').
?test(sheet1_N34, "/EQUAL/", "N34", '#VALUE!').
?test(sheet1_O34, "/EQUAL/", "O34", '#VALUE!').
?test(sheet1_P34, "/EQUAL/", "P34", '#VALUE!').
?test(sheet1_Q34, "/EQUAL/", "Q34", '#VALUE!').
?test(sheet1_R34, "/EQUAL/", "R34", '#VALUE!').
?test(sheet1_S34, "/EQUAL/", "S34", '#VALUE!').
?test(sheet1_T34, "/EQUAL/", "T34", '#VALUE!').
?test(sheet1_U34, "/EQUAL/", "U34", '#VALUE!').
?test(sheet1_V34, "/EQUAL/", "V34", '#VALUE!').
?test(sheet1_A35, "/EQUAL/", "A35", "String").
?test(sheet1_C35, "/EQUAL/", "C35", false).
?test(sheet1_D35, "/EQUAL/", "D35", false).
?test(sheet1_E35, "/EQUAL/", "E35", false).
?test(sheet1_F35, "/EQUAL/", "F35", '#DIV/0!').
?test(sheet1_G35, "/EQUAL/", "G35", '#N/A').
?test(sheet1_H35, "/EQUAL/", "H35", '#NAME?').
?test(sheet1_I35, "/EQUAL/", "I35", 'NULL!').
?test(sheet1_J35, "/EQUAL/", "J35", '#NUM!').
?test(sheet1_K35, "/EQUAL/", "K35", '#REF!').
?test(sheet1_L35, "/EQUAL/", "L35", '#VALUE!').
?test(sheet1_M35, "/EQUAL/", "M35", true).
?test(sheet1_N35, "/EQUAL/", "N35", false).
?test(sheet1_O35, "/EQUAL/", "O35", false).
?test(sheet1_P35, "/EQUAL/", "P35", false).
?test(sheet1_Q35, "/EQUAL/", "Q35", false).
?test(sheet1_R35, "/EQUAL/", "R35", false).
?test(sheet1_S35, "/EQUAL/", "S35", false).
?test(sheet1_T35, "/EQUAL/", "T35", false).
?test(sheet1_U35, "/EQUAL/", "U35", false).
?test(sheet1_V35, "/EQUAL/", "V35", false).
?test(sheet1_A36, "/EQUAL/", "A36", "String").
?test(sheet1_C36, "/EQUAL/", "C36", false).
?test(sheet1_D36, "/EQUAL/", "D36", false).
?test(sheet1_E36, "/EQUAL/", "E36", false).
?test(sheet1_F36, "/EQUAL/", "F36", '#DIV/0!').
?test(sheet1_G36, "/EQUAL/", "G36", '#N/A').
?test(sheet1_H36, "/EQUAL/", "H36", '#NAME?').
?test(sheet1_I36, "/EQUAL/", "I36", 'NULL!').
?test(sheet1_J36, "/EQUAL/", "J36", '#NUM!').
?test(sheet1_K36, "/EQUAL/", "K36", '#REF!').
?test(sheet1_L36, "/EQUAL/", "L36", '#VALUE!').
?test(sheet1_M36, "/EQUAL/", "M36", false).
?test(sheet1_N36, "/EQUAL/", "N36", true).
?test(sheet1_O36, "/EQUAL/", "O36", false).
?test(sheet1_P36, "/EQUAL/", "P36", false).
?test(sheet1_Q36, "/EQUAL/", "Q36", false).
?test(sheet1_R36, "/EQUAL/", "R36", false).
?test(sheet1_S36, "/EQUAL/", "S36", false).
?test(sheet1_T36, "/EQUAL/", "T36", false).
?test(sheet1_U36, "/EQUAL/", "U36", false).
?test(sheet1_V36, "/EQUAL/", "V36", false).
?test(sheet1_A37, "/EQUAL/", "A37", "String").
?test(sheet1_C37, "/EQUAL/", "C37", false).
?test(sheet1_D37, "/EQUAL/", "D37", false).
?test(sheet1_E37, "/EQUAL/", "E37", false).
?test(sheet1_F37, "/EQUAL/", "F37", '#DIV/0!').
?test(sheet1_G37, "/EQUAL/", "G37", '#N/A').
?test(sheet1_H37, "/EQUAL/", "H37", '#NAME?').
?test(sheet1_I37, "/EQUAL/", "I37", 'NULL!').
?test(sheet1_J37, "/EQUAL/", "J37", '#NUM!').
?test(sheet1_K37, "/EQUAL/", "K37", '#REF!').
?test(sheet1_L37, "/EQUAL/", "L37", '#VALUE!').
?test(sheet1_M37, "/EQUAL/", "M37", false).
?test(sheet1_N37, "/EQUAL/", "N37", false).
?test(sheet1_O37, "/EQUAL/", "O37", true).
?test(sheet1_P37, "/EQUAL/", "P37", false).
?test(sheet1_Q37, "/EQUAL/", "Q37", false).
?test(sheet1_R37, "/EQUAL/", "R37", false).
?test(sheet1_S37, "/EQUAL/", "S37", false).
?test(sheet1_T37, "/EQUAL/", "T37", false).
?test(sheet1_U37, "/EQUAL/", "U37", false).
?test(sheet1_V37, "/EQUAL/", "V37", false).
?test(sheet1_A38, "/EQUAL/", "A38", "Str Num").
?test(sheet1_C38, "/EQUAL/", "C38", false).
?test(sheet1_D38, "/EQUAL/", "D38", false).
?test(sheet1_E38, "/EQUAL/", "E38", false).
?test(sheet1_F38, "/EQUAL/", "F38", '#DIV/0!').
?test(sheet1_G38, "/EQUAL/", "G38", '#N/A').
?test(sheet1_H38, "/EQUAL/", "H38", '#NAME?').
?test(sheet1_I38, "/EQUAL/", "I38", 'NULL!').
?test(sheet1_J38, "/EQUAL/", "J38", '#NUM!').
?test(sheet1_K38, "/EQUAL/", "K38", '#REF!').
?test(sheet1_L38, "/EQUAL/", "L38", '#VALUE!').
?test(sheet1_M38, "/EQUAL/", "M38", false).
?test(sheet1_N38, "/EQUAL/", "N38", false).
?test(sheet1_O38, "/EQUAL/", "O38", false).
?test(sheet1_P38, "/EQUAL/", "P38", true).
?test(sheet1_Q38, "/EQUAL/", "Q38", false).
?test(sheet1_R38, "/EQUAL/", "R38", false).
?test(sheet1_S38, "/EQUAL/", "S38", false).
?test(sheet1_T38, "/EQUAL/", "T38", false).
?test(sheet1_U38, "/EQUAL/", "U38", false).
?test(sheet1_V38, "/EQUAL/", "V38", false).
?test(sheet1_A39, "/EQUAL/", "A39", "Str Num").
?test(sheet1_C39, "/EQUAL/", "C39", false).
?test(sheet1_D39, "/EQUAL/", "D39", false).
?test(sheet1_E39, "/EQUAL/", "E39", false).
?test(sheet1_F39, "/EQUAL/", "F39", '#DIV/0!').
?test(sheet1_G39, "/EQUAL/", "G39", '#N/A').
?test(sheet1_H39, "/EQUAL/", "H39", '#NAME?').
?test(sheet1_I39, "/EQUAL/", "I39", 'NULL!').
?test(sheet1_J39, "/EQUAL/", "J39", '#NUM!').
?test(sheet1_K39, "/EQUAL/", "K39", '#REF!').
?test(sheet1_L39, "/EQUAL/", "L39", '#VALUE!').
?test(sheet1_M39, "/EQUAL/", "M39", false).
?test(sheet1_N39, "/EQUAL/", "N39", false).
?test(sheet1_O39, "/EQUAL/", "O39", false).
?test(sheet1_P39, "/EQUAL/", "P39", false).
?test(sheet1_Q39, "/EQUAL/", "Q39", true).
?test(sheet1_R39, "/EQUAL/", "R39", false).
?test(sheet1_S39, "/EQUAL/", "S39", false).
?test(sheet1_T39, "/EQUAL/", "T39", false).
?test(sheet1_U39, "/EQUAL/", "U39", false).
?test(sheet1_V39, "/EQUAL/", "V39", false).
?test(sheet1_A40, "/EQUAL/", "A40", "Integer").
?test(sheet1_C40, "/EQUAL/", "C40", false).
?test(sheet1_D40, "/EQUAL/", "D40", false).
?test(sheet1_E40, "/EQUAL/", "E40", false).
?test(sheet1_F40, "/EQUAL/", "F40", '#DIV/0!').
?test(sheet1_G40, "/EQUAL/", "G40", '#N/A').
?test(sheet1_H40, "/EQUAL/", "H40", '#NAME?').
?test(sheet1_I40, "/EQUAL/", "I40", 'NULL!').
?test(sheet1_J40, "/EQUAL/", "J40", '#NUM!').
?test(sheet1_K40, "/EQUAL/", "K40", '#REF!').
?test(sheet1_L40, "/EQUAL/", "L40", '#VALUE!').
?test(sheet1_M40, "/EQUAL/", "M40", false).
?test(sheet1_N40, "/EQUAL/", "N40", false).
?test(sheet1_O40, "/EQUAL/", "O40", false).
?test(sheet1_P40, "/EQUAL/", "P40", false).
?test(sheet1_Q40, "/EQUAL/", "Q40", false).
?test(sheet1_R40, "/EQUAL/", "R40", true).
?test(sheet1_S40, "/EQUAL/", "S40", false).
?test(sheet1_T40, "/EQUAL/", "T40", false).
?test(sheet1_U40, "/EQUAL/", "U40", false).
?test(sheet1_V40, "/EQUAL/", "V40", false).
?test(sheet1_A41, "/EQUAL/", "A41", "Integer").
?test(sheet1_C41, "/EQUAL/", "C41", false).
?test(sheet1_D41, "/EQUAL/", "D41", false).
?test(sheet1_E41, "/EQUAL/", "E41", false).
?test(sheet1_F41, "/EQUAL/", "F41", '#DIV/0!').
?test(sheet1_G41, "/EQUAL/", "G41", '#N/A').
?test(sheet1_H41, "/EQUAL/", "H41", '#NAME?').
?test(sheet1_I41, "/EQUAL/", "I41", 'NULL!').
?test(sheet1_J41, "/EQUAL/", "J41", '#NUM!').
?test(sheet1_K41, "/EQUAL/", "K41", '#REF!').
?test(sheet1_L41, "/EQUAL/", "L41", '#VALUE!').
?test(sheet1_M41, "/EQUAL/", "M41", false).
?test(sheet1_N41, "/EQUAL/", "N41", false).
?test(sheet1_O41, "/EQUAL/", "O41", false).
?test(sheet1_P41, "/EQUAL/", "P41", false).
?test(sheet1_Q41, "/EQUAL/", "Q41", false).
?test(sheet1_R41, "/EQUAL/", "R41", false).
?test(sheet1_S41, "/EQUAL/", "S41", true).
?test(sheet1_T41, "/EQUAL/", "T41", false).
?test(sheet1_U41, "/EQUAL/", "U41", false).
?test(sheet1_V41, "/EQUAL/", "V41", false).
?test(sheet1_A42, "/EQUAL/", "A42", "Zero").
?test(sheet1_C42, "/EQUAL/", "C42", true).
?test(sheet1_D42, "/EQUAL/", "D42", false).
?test(sheet1_E42, "/EQUAL/", "E42", false).
?test(sheet1_F42, "/EQUAL/", "F42", '#DIV/0!').
?test(sheet1_G42, "/EQUAL/", "G42", '#N/A').
?test(sheet1_H42, "/EQUAL/", "H42", '#NAME?').
?test(sheet1_I42, "/EQUAL/", "I42", 'NULL!').
?test(sheet1_J42, "/EQUAL/", "J42", '#NUM!').
?test(sheet1_K42, "/EQUAL/", "K42", '#REF!').
?test(sheet1_L42, "/EQUAL/", "L42", '#VALUE!').
?test(sheet1_M42, "/EQUAL/", "M42", false).
?test(sheet1_N42, "/EQUAL/", "N42", false).
?test(sheet1_O42, "/EQUAL/", "O42", false).
?test(sheet1_P42, "/EQUAL/", "P42", false).
?test(sheet1_Q42, "/EQUAL/", "Q42", false).
?test(sheet1_R42, "/EQUAL/", "R42", false).
?test(sheet1_S42, "/EQUAL/", "S42", false).
?test(sheet1_T42, "/EQUAL/", "T42", true).
?test(sheet1_U42, "/EQUAL/", "U42", false).
?test(sheet1_V42, "/EQUAL/", "V42", false).
?test(sheet1_A43, "/EQUAL/", "A43", "Float").
?test(sheet1_C43, "/EQUAL/", "C43", false).
?test(sheet1_D43, "/EQUAL/", "D43", false).
?test(sheet1_E43, "/EQUAL/", "E43", false).
?test(sheet1_F43, "/EQUAL/", "F43", '#DIV/0!').
?test(sheet1_G43, "/EQUAL/", "G43", '#N/A').
?test(sheet1_H43, "/EQUAL/", "H43", '#NAME?').
?test(sheet1_I43, "/EQUAL/", "I43", 'NULL!').
?test(sheet1_J43, "/EQUAL/", "J43", '#NUM!').
?test(sheet1_K43, "/EQUAL/", "K43", '#REF!').
?test(sheet1_L43, "/EQUAL/", "L43", '#VALUE!').
?test(sheet1_M43, "/EQUAL/", "M43", false).
?test(sheet1_N43, "/EQUAL/", "N43", false).
?test(sheet1_O43, "/EQUAL/", "O43", false).
?test(sheet1_P43, "/EQUAL/", "P43", false).
?test(sheet1_Q43, "/EQUAL/", "Q43", false).
?test(sheet1_R43, "/EQUAL/", "R43", false).
?test(sheet1_S43, "/EQUAL/", "S43", false).
?test(sheet1_T43, "/EQUAL/", "T43", false).
?test(sheet1_U43, "/EQUAL/", "U43", true).
?test(sheet1_V43, "/EQUAL/", "V43", false).
?test(sheet1_A44, "/EQUAL/", "A44", "Float").
?test(sheet1_C44, "/EQUAL/", "C44", false).
?test(sheet1_D44, "/EQUAL/", "D44", false).
?test(sheet1_E44, "/EQUAL/", "E44", false).
?test(sheet1_F44, "/EQUAL/", "F44", '#DIV/0!').
?test(sheet1_G44, "/EQUAL/", "G44", '#N/A').
?test(sheet1_H44, "/EQUAL/", "H44", '#NAME?').
?test(sheet1_I44, "/EQUAL/", "I44", 'NULL!').
?test(sheet1_J44, "/EQUAL/", "J44", '#NUM!').
?test(sheet1_K44, "/EQUAL/", "K44", '#REF!').
?test(sheet1_L44, "/EQUAL/", "L44", '#VALUE!').
?test(sheet1_M44, "/EQUAL/", "M44", false).
?test(sheet1_N44, "/EQUAL/", "N44", false).
?test(sheet1_O44, "/EQUAL/", "O44", false).
?test(sheet1_P44, "/EQUAL/", "P44", false).
?test(sheet1_Q44, "/EQUAL/", "Q44", false).
?test(sheet1_R44, "/EQUAL/", "R44", false).
?test(sheet1_S44, "/EQUAL/", "S44", false).
?test(sheet1_T44, "/EQUAL/", "T44", false).
?test(sheet1_U44, "/EQUAL/", "U44", false).
?test(sheet1_V44, "/EQUAL/", "V44", true).
?test(sheet1_A47, "/EQUAL/", "A47", 400.0).
?test(sheet1_C47, "/EQUAL/", "C47", 1.0).
?test(sheet1_D47, "/EQUAL/", "D47", 1.0).
?test(sheet1_E47, "/EQUAL/", "E47", 1.0).
?test(sheet1_F47, "/EQUAL/", "F47", 1.0).
?test(sheet1_G47, "/EQUAL/", "G47", 1.0).
?test(sheet1_H47, "/EQUAL/", "H47", 1.0).
?test(sheet1_I47, "/EQUAL/", "I47", 1.0).
?test(sheet1_J47, "/EQUAL/", "J47", 1.0).
?test(sheet1_K47, "/EQUAL/", "K47", 1.0).
?test(sheet1_L47, "/EQUAL/", "L47", 1.0).
?test(sheet1_M47, "/EQUAL/", "M47", 1.0).
?test(sheet1_N47, "/EQUAL/", "N47", 1.0).
?test(sheet1_O47, "/EQUAL/", "O47", 1.0).
?test(sheet1_P47, "/EQUAL/", "P47", 1.0).
?test(sheet1_Q47, "/EQUAL/", "Q47", 1.0).
?test(sheet1_R47, "/EQUAL/", "R47", 1.0).
?test(sheet1_S47, "/EQUAL/", "S47", 1.0).
?test(sheet1_T47, "/EQUAL/", "T47", 1.0).
?test(sheet1_U47, "/EQUAL/", "U47", 1.0).
?test(sheet1_V47, "/EQUAL/", "V47", 1.0).
?test(sheet1_A48, "/EQUAL/", "A48", "Success").
?test(sheet1_C48, "/EQUAL/", "C48", 1.0).
?test(sheet1_D48, "/EQUAL/", "D48", 1.0).
?test(sheet1_E48, "/EQUAL/", "E48", 1.0).
?test(sheet1_F48, "/EQUAL/", "F48", 1.0).
?test(sheet1_G48, "/EQUAL/", "G48", 1.0).
?test(sheet1_H48, "/EQUAL/", "H48", 1.0).
?test(sheet1_I48, "/EQUAL/", "I48", 1.0).
?test(sheet1_J48, "/EQUAL/", "J48", 1.0).
?test(sheet1_K48, "/EQUAL/", "K48", 1.0).
?test(sheet1_L48, "/EQUAL/", "L48", 1.0).
?test(sheet1_M48, "/EQUAL/", "M48", 1.0).
?test(sheet1_N48, "/EQUAL/", "N48", 1.0).
?test(sheet1_O48, "/EQUAL/", "O48", 1.0).
?test(sheet1_P48, "/EQUAL/", "P48", 1.0).
?test(sheet1_Q48, "/EQUAL/", "Q48", 1.0).
?test(sheet1_R48, "/EQUAL/", "R48", 1.0).
?test(sheet1_S48, "/EQUAL/", "S48", 1.0).
?test(sheet1_T48, "/EQUAL/", "T48", 1.0).
?test(sheet1_U48, "/EQUAL/", "U48", 1.0).
?test(sheet1_V48, "/EQUAL/", "V48", 1.0).
?test(sheet1_C49, "/EQUAL/", "C49", 1.0).
?test(sheet1_D49, "/EQUAL/", "D49", 1.0).
?test(sheet1_E49, "/EQUAL/", "E49", 1.0).
?test(sheet1_F49, "/EQUAL/", "F49", 1.0).
?test(sheet1_G49, "/EQUAL/", "G49", 1.0).
?test(sheet1_H49, "/EQUAL/", "H49", 1.0).
?test(sheet1_I49, "/EQUAL/", "I49", 1.0).
?test(sheet1_J49, "/EQUAL/", "J49", 1.0).
?test(sheet1_K49, "/EQUAL/", "K49", 1.0).
?test(sheet1_L49, "/EQUAL/", "L49", 1.0).
?test(sheet1_M49, "/EQUAL/", "M49", 1.0).
?test(sheet1_N49, "/EQUAL/", "N49", 1.0).
?test(sheet1_O49, "/EQUAL/", "O49", 1.0).
?test(sheet1_P49, "/EQUAL/", "P49", 1.0).
?test(sheet1_Q49, "/EQUAL/", "Q49", 1.0).
?test(sheet1_R49, "/EQUAL/", "R49", 1.0).
?test(sheet1_S49, "/EQUAL/", "S49", 1.0).
?test(sheet1_T49, "/EQUAL/", "T49", 1.0).
?test(sheet1_U49, "/EQUAL/", "U49", 1.0).
?test(sheet1_V49, "/EQUAL/", "V49", 1.0).
?test(sheet1_C50, "/EQUAL/", "C50", 1.0).
?test(sheet1_D50, "/EQUAL/", "D50", 1.0).
?test(sheet1_E50, "/EQUAL/", "E50", 1.0).
?test(sheet1_F50, "/EQUAL/", "F50", 1.0).
?test(sheet1_G50, "/EQUAL/", "G50", 1.0).
?test(sheet1_H50, "/EQUAL/", "H50", 1.0).
?test(sheet1_I50, "/EQUAL/", "I50", 1.0).
?test(sheet1_J50, "/EQUAL/", "J50", 1.0).
?test(sheet1_K50, "/EQUAL/", "K50", 1.0).
?test(sheet1_L50, "/EQUAL/", "L50", 1.0).
?test(sheet1_M50, "/EQUAL/", "M50", 1.0).
?test(sheet1_N50, "/EQUAL/", "N50", 1.0).
?test(sheet1_O50, "/EQUAL/", "O50", 1.0).
?test(sheet1_P50, "/EQUAL/", "P50", 1.0).
?test(sheet1_Q50, "/EQUAL/", "Q50", 1.0).
?test(sheet1_R50, "/EQUAL/", "R50", 1.0).
?test(sheet1_S50, "/EQUAL/", "S50", 1.0).
?test(sheet1_T50, "/EQUAL/", "T50", 1.0).
?test(sheet1_U50, "/EQUAL/", "U50", 1.0).
?test(sheet1_V50, "/EQUAL/", "V50", 1.0).
?test(sheet1_C51, "/EQUAL/", "C51", 1.0).
?test(sheet1_D51, "/EQUAL/", "D51", 1.0).
?test(sheet1_E51, "/EQUAL/", "E51", 1.0).
?test(sheet1_F51, "/EQUAL/", "F51", 1.0).
?test(sheet1_G51, "/EQUAL/", "G51", 1.0).
?test(sheet1_H51, "/EQUAL/", "H51", 1.0).
?test(sheet1_I51, "/EQUAL/", "I51", 1.0).
?test(sheet1_J51, "/EQUAL/", "J51", 1.0).
?test(sheet1_K51, "/EQUAL/", "K51", 1.0).
?test(sheet1_L51, "/EQUAL/", "L51", 1.0).
?test(sheet1_M51, "/EQUAL/", "M51", 1.0).
?test(sheet1_N51, "/EQUAL/", "N51", 1.0).
?test(sheet1_O51, "/EQUAL/", "O51", 1.0).
?test(sheet1_P51, "/EQUAL/", "P51", 1.0).
?test(sheet1_Q51, "/EQUAL/", "Q51", 1.0).
?test(sheet1_R51, "/EQUAL/", "R51", 1.0).
?test(sheet1_S51, "/EQUAL/", "S51", 1.0).
?test(sheet1_T51, "/EQUAL/", "T51", 1.0).
?test(sheet1_U51, "/EQUAL/", "U51", 1.0).
?test(sheet1_V51, "/EQUAL/", "V51", 1.0).
?test(sheet1_C52, "/EQUAL/", "C52", 1.0).
?test(sheet1_D52, "/EQUAL/", "D52", 1.0).
?test(sheet1_E52, "/EQUAL/", "E52", 1.0).
?test(sheet1_F52, "/EQUAL/", "F52", 1.0).
?test(sheet1_G52, "/EQUAL/", "G52", 1.0).
?test(sheet1_H52, "/EQUAL/", "H52", 1.0).
?test(sheet1_I52, "/EQUAL/", "I52", 1.0).
?test(sheet1_J52, "/EQUAL/", "J52", 1.0).
?test(sheet1_K52, "/EQUAL/", "K52", 1.0).
?test(sheet1_L52, "/EQUAL/", "L52", 1.0).
?test(sheet1_M52, "/EQUAL/", "M52", 1.0).
?test(sheet1_N52, "/EQUAL/", "N52", 1.0).
?test(sheet1_O52, "/EQUAL/", "O52", 1.0).
?test(sheet1_P52, "/EQUAL/", "P52", 1.0).
?test(sheet1_Q52, "/EQUAL/", "Q52", 1.0).
?test(sheet1_R52, "/EQUAL/", "R52", 1.0).
?test(sheet1_S52, "/EQUAL/", "S52", 1.0).
?test(sheet1_T52, "/EQUAL/", "T52", 1.0).
?test(sheet1_U52, "/EQUAL/", "U52", 1.0).
?test(sheet1_V52, "/EQUAL/", "V52", 1.0).
?test(sheet1_C53, "/EQUAL/", "C53", 1.0).
?test(sheet1_D53, "/EQUAL/", "D53", 1.0).
?test(sheet1_E53, "/EQUAL/", "E53", 1.0).
?test(sheet1_F53, "/EQUAL/", "F53", 1.0).
?test(sheet1_G53, "/EQUAL/", "G53", 1.0).
?test(sheet1_H53, "/EQUAL/", "H53", 1.0).
?test(sheet1_I53, "/EQUAL/", "I53", 1.0).
?test(sheet1_J53, "/EQUAL/", "J53", 1.0).
?test(sheet1_K53, "/EQUAL/", "K53", 1.0).
?test(sheet1_L53, "/EQUAL/", "L53", 1.0).
?test(sheet1_M53, "/EQUAL/", "M53", 1.0).
?test(sheet1_N53, "/EQUAL/", "N53", 1.0).
?test(sheet1_O53, "/EQUAL/", "O53", 1.0).
?test(sheet1_P53, "/EQUAL/", "P53", 1.0).
?test(sheet1_Q53, "/EQUAL/", "Q53", 1.0).
?test(sheet1_R53, "/EQUAL/", "R53", 1.0).
?test(sheet1_S53, "/EQUAL/", "S53", 1.0).
?test(sheet1_T53, "/EQUAL/", "T53", 1.0).
?test(sheet1_U53, "/EQUAL/", "U53", 1.0).
?test(sheet1_V53, "/EQUAL/", "V53", 1.0).
?test(sheet1_C54, "/EQUAL/", "C54", 1.0).
?test(sheet1_D54, "/EQUAL/", "D54", 1.0).
?test(sheet1_E54, "/EQUAL/", "E54", 1.0).
?test(sheet1_F54, "/EQUAL/", "F54", 1.0).
?test(sheet1_G54, "/EQUAL/", "G54", 1.0).
?test(sheet1_H54, "/EQUAL/", "H54", 1.0).
?test(sheet1_I54, "/EQUAL/", "I54", 1.0).
?test(sheet1_J54, "/EQUAL/", "J54", 1.0).
?test(sheet1_K54, "/EQUAL/", "K54", 1.0).
?test(sheet1_L54, "/EQUAL/", "L54", 1.0).
?test(sheet1_M54, "/EQUAL/", "M54", 1.0).
?test(sheet1_N54, "/EQUAL/", "N54", 1.0).
?test(sheet1_O54, "/EQUAL/", "O54", 1.0).
?test(sheet1_P54, "/EQUAL/", "P54", 1.0).
?test(sheet1_Q54, "/EQUAL/", "Q54", 1.0).
?test(sheet1_R54, "/EQUAL/", "R54", 1.0).
?test(sheet1_S54, "/EQUAL/", "S54", 1.0).
?test(sheet1_T54, "/EQUAL/", "T54", 1.0).
?test(sheet1_U54, "/EQUAL/", "U54", 1.0).
?test(sheet1_V54, "/EQUAL/", "V54", 1.0).
?test(sheet1_C55, "/EQUAL/", "C55", 1.0).
?test(sheet1_D55, "/EQUAL/", "D55", 1.0).
?test(sheet1_E55, "/EQUAL/", "E55", 1.0).
?test(sheet1_F55, "/EQUAL/", "F55", 1.0).
?test(sheet1_G55, "/EQUAL/", "G55", 1.0).
?test(sheet1_H55, "/EQUAL/", "H55", 1.0).
?test(sheet1_I55, "/EQUAL/", "I55", 1.0).
?test(sheet1_J55, "/EQUAL/", "J55", 1.0).
?test(sheet1_K55, "/EQUAL/", "K55", 1.0).
?test(sheet1_L55, "/EQUAL/", "L55", 1.0).
?test(sheet1_M55, "/EQUAL/", "M55", 1.0).
?test(sheet1_N55, "/EQUAL/", "N55", 1.0).
?test(sheet1_O55, "/EQUAL/", "O55", 1.0).
?test(sheet1_P55, "/EQUAL/", "P55", 1.0).
?test(sheet1_Q55, "/EQUAL/", "Q55", 1.0).
?test(sheet1_R55, "/EQUAL/", "R55", 1.0).
?test(sheet1_S55, "/EQUAL/", "S55", 1.0).
?test(sheet1_T55, "/EQUAL/", "T55", 1.0).
?test(sheet1_U55, "/EQUAL/", "U55", 1.0).
?test(sheet1_V55, "/EQUAL/", "V55", 1.0).
?test(sheet1_C56, "/EQUAL/", "C56", 1.0).
?test(sheet1_D56, "/EQUAL/", "D56", 1.0).
?test(sheet1_E56, "/EQUAL/", "E56", 1.0).
?test(sheet1_F56, "/EQUAL/", "F56", 1.0).
?test(sheet1_G56, "/EQUAL/", "G56", 1.0).
?test(sheet1_H56, "/EQUAL/", "H56", 1.0).
?test(sheet1_I56, "/EQUAL/", "I56", 1.0).
?test(sheet1_J56, "/EQUAL/", "J56", 1.0).
?test(sheet1_K56, "/EQUAL/", "K56", 1.0).
?test(sheet1_L56, "/EQUAL/", "L56", 1.0).
?test(sheet1_M56, "/EQUAL/", "M56", 1.0).
?test(sheet1_N56, "/EQUAL/", "N56", 1.0).
?test(sheet1_O56, "/EQUAL/", "O56", 1.0).
?test(sheet1_P56, "/EQUAL/", "P56", 1.0).
?test(sheet1_Q56, "/EQUAL/", "Q56", 1.0).
?test(sheet1_R56, "/EQUAL/", "R56", 1.0).
?test(sheet1_S56, "/EQUAL/", "S56", 1.0).
?test(sheet1_T56, "/EQUAL/", "T56", 1.0).
?test(sheet1_U56, "/EQUAL/", "U56", 1.0).
?test(sheet1_V56, "/EQUAL/", "V56", 1.0).
?test(sheet1_C57, "/EQUAL/", "C57", 1.0).
?test(sheet1_D57, "/EQUAL/", "D57", 1.0).
?test(sheet1_E57, "/EQUAL/", "E57", 1.0).
?test(sheet1_F57, "/EQUAL/", "F57", 1.0).
?test(sheet1_G57, "/EQUAL/", "G57", 1.0).
?test(sheet1_H57, "/EQUAL/", "H57", 1.0).
?test(sheet1_I57, "/EQUAL/", "I57", 1.0).
?test(sheet1_J57, "/EQUAL/", "J57", 1.0).
?test(sheet1_K57, "/EQUAL/", "K57", 1.0).
?test(sheet1_L57, "/EQUAL/", "L57", 1.0).
?test(sheet1_M57, "/EQUAL/", "M57", 1.0).
?test(sheet1_N57, "/EQUAL/", "N57", 1.0).
?test(sheet1_O57, "/EQUAL/", "O57", 1.0).
?test(sheet1_P57, "/EQUAL/", "P57", 1.0).
?test(sheet1_Q57, "/EQUAL/", "Q57", 1.0).
?test(sheet1_R57, "/EQUAL/", "R57", 1.0).
?test(sheet1_S57, "/EQUAL/", "S57", 1.0).
?test(sheet1_T57, "/EQUAL/", "T57", 1.0).
?test(sheet1_U57, "/EQUAL/", "U57", 1.0).
?test(sheet1_V57, "/EQUAL/", "V57", 1.0).
?test(sheet1_C58, "/EQUAL/", "C58", 1.0).
?test(sheet1_D58, "/EQUAL/", "D58", 1.0).
?test(sheet1_E58, "/EQUAL/", "E58", 1.0).
?test(sheet1_F58, "/EQUAL/", "F58", 1.0).
?test(sheet1_G58, "/EQUAL/", "G58", 1.0).
?test(sheet1_H58, "/EQUAL/", "H58", 1.0).
?test(sheet1_I58, "/EQUAL/", "I58", 1.0).
?test(sheet1_J58, "/EQUAL/", "J58", 1.0).
?test(sheet1_K58, "/EQUAL/", "K58", 1.0).
?test(sheet1_L58, "/EQUAL/", "L58", 1.0).
?test(sheet1_M58, "/EQUAL/", "M58", 1.0).
?test(sheet1_N58, "/EQUAL/", "N58", 1.0).
?test(sheet1_O58, "/EQUAL/", "O58", 1.0).
?test(sheet1_P58, "/EQUAL/", "P58", 1.0).
?test(sheet1_Q58, "/EQUAL/", "Q58", 1.0).
?test(sheet1_R58, "/EQUAL/", "R58", 1.0).
?test(sheet1_S58, "/EQUAL/", "S58", 1.0).
?test(sheet1_T58, "/EQUAL/", "T58", 1.0).
?test(sheet1_U58, "/EQUAL/", "U58", 1.0).
?test(sheet1_V58, "/EQUAL/", "V58", 1.0).
?test(sheet1_C59, "/EQUAL/", "C59", 1.0).
?test(sheet1_D59, "/EQUAL/", "D59", 1.0).
?test(sheet1_E59, "/EQUAL/", "E59", 1.0).
?test(sheet1_F59, "/EQUAL/", "F59", 1.0).
?test(sheet1_G59, "/EQUAL/", "G59", 1.0).
?test(sheet1_H59, "/EQUAL/", "H59", 1.0).
?test(sheet1_I59, "/EQUAL/", "I59", 1.0).
?test(sheet1_J59, "/EQUAL/", "J59", 1.0).
?test(sheet1_K59, "/EQUAL/", "K59", 1.0).
?test(sheet1_L59, "/EQUAL/", "L59", 1.0).
?test(sheet1_M59, "/EQUAL/", "M59", 1.0).
?test(sheet1_N59, "/EQUAL/", "N59", 1.0).
?test(sheet1_O59, "/EQUAL/", "O59", 1.0).
?test(sheet1_P59, "/EQUAL/", "P59", 1.0).
?test(sheet1_Q59, "/EQUAL/", "Q59", 1.0).
?test(sheet1_R59, "/EQUAL/", "R59", 1.0).
?test(sheet1_S59, "/EQUAL/", "S59", 1.0).
?test(sheet1_T59, "/EQUAL/", "T59", 1.0).
?test(sheet1_U59, "/EQUAL/", "U59", 1.0).
?test(sheet1_V59, "/EQUAL/", "V59", 1.0).
?test(sheet1_C60, "/EQUAL/", "C60", 1.0).
?test(sheet1_D60, "/EQUAL/", "D60", 1.0).
?test(sheet1_E60, "/EQUAL/", "E60", 1.0).
?test(sheet1_F60, "/EQUAL/", "F60", 1.0).
?test(sheet1_G60, "/EQUAL/", "G60", 1.0).
?test(sheet1_H60, "/EQUAL/", "H60", 1.0).
?test(sheet1_I60, "/EQUAL/", "I60", 1.0).
?test(sheet1_J60, "/EQUAL/", "J60", 1.0).
?test(sheet1_K60, "/EQUAL/", "K60", 1.0).
?test(sheet1_L60, "/EQUAL/", "L60", 1.0).
?test(sheet1_M60, "/EQUAL/", "M60", 1.0).
?test(sheet1_N60, "/EQUAL/", "N60", 1.0).
?test(sheet1_O60, "/EQUAL/", "O60", 1.0).
?test(sheet1_P60, "/EQUAL/", "P60", 1.0).
?test(sheet1_Q60, "/EQUAL/", "Q60", 1.0).
?test(sheet1_R60, "/EQUAL/", "R60", 1.0).
?test(sheet1_S60, "/EQUAL/", "S60", 1.0).
?test(sheet1_T60, "/EQUAL/", "T60", 1.0).
?test(sheet1_U60, "/EQUAL/", "U60", 1.0).
?test(sheet1_V60, "/EQUAL/", "V60", 1.0).
?test(sheet1_C61, "/EQUAL/", "C61", 1.0).
?test(sheet1_D61, "/EQUAL/", "D61", 1.0).
?test(sheet1_E61, "/EQUAL/", "E61", 1.0).
?test(sheet1_F61, "/EQUAL/", "F61", 1.0).
?test(sheet1_G61, "/EQUAL/", "G61", 1.0).
?test(sheet1_H61, "/EQUAL/", "H61", 1.0).
?test(sheet1_I61, "/EQUAL/", "I61", 1.0).
?test(sheet1_J61, "/EQUAL/", "J61", 1.0).
?test(sheet1_K61, "/EQUAL/", "K61", 1.0).
?test(sheet1_L61, "/EQUAL/", "L61", 1.0).
?test(sheet1_M61, "/EQUAL/", "M61", 1.0).
?test(sheet1_N61, "/EQUAL/", "N61", 1.0).
?test(sheet1_O61, "/EQUAL/", "O61", 1.0).
?test(sheet1_P61, "/EQUAL/", "P61", 1.0).
?test(sheet1_Q61, "/EQUAL/", "Q61", 1.0).
?test(sheet1_R61, "/EQUAL/", "R61", 1.0).
?test(sheet1_S61, "/EQUAL/", "S61", 1.0).
?test(sheet1_T61, "/EQUAL/", "T61", 1.0).
?test(sheet1_U61, "/EQUAL/", "U61", 1.0).
?test(sheet1_V61, "/EQUAL/", "V61", 1.0).
?test(sheet1_C62, "/EQUAL/", "C62", 1.0).
?test(sheet1_D62, "/EQUAL/", "D62", 1.0).
?test(sheet1_E62, "/EQUAL/", "E62", 1.0).
?test(sheet1_F62, "/EQUAL/", "F62", 1.0).
?test(sheet1_G62, "/EQUAL/", "G62", 1.0).
?test(sheet1_H62, "/EQUAL/", "H62", 1.0).
?test(sheet1_I62, "/EQUAL/", "I62", 1.0).
?test(sheet1_J62, "/EQUAL/", "J62", 1.0).
?test(sheet1_K62, "/EQUAL/", "K62", 1.0).
?test(sheet1_L62, "/EQUAL/", "L62", 1.0).
?test(sheet1_M62, "/EQUAL/", "M62", 1.0).
?test(sheet1_N62, "/EQUAL/", "N62", 1.0).
?test(sheet1_O62, "/EQUAL/", "O62", 1.0).
?test(sheet1_P62, "/EQUAL/", "P62", 1.0).
?test(sheet1_Q62, "/EQUAL/", "Q62", 1.0).
?test(sheet1_R62, "/EQUAL/", "R62", 1.0).
?test(sheet1_S62, "/EQUAL/", "S62", 1.0).
?test(sheet1_T62, "/EQUAL/", "T62", 1.0).
?test(sheet1_U62, "/EQUAL/", "U62", 1.0).
?test(sheet1_V62, "/EQUAL/", "V62", 1.0).
?test(sheet1_C63, "/EQUAL/", "C63", 1.0).
?test(sheet1_D63, "/EQUAL/", "D63", 1.0).
?test(sheet1_E63, "/EQUAL/", "E63", 1.0).
?test(sheet1_F63, "/EQUAL/", "F63", 1.0).
?test(sheet1_G63, "/EQUAL/", "G63", 1.0).
?test(sheet1_H63, "/EQUAL/", "H63", 1.0).
?test(sheet1_I63, "/EQUAL/", "I63", 1.0).
?test(sheet1_J63, "/EQUAL/", "J63", 1.0).
?test(sheet1_K63, "/EQUAL/", "K63", 1.0).
?test(sheet1_L63, "/EQUAL/", "L63", 1.0).
?test(sheet1_M63, "/EQUAL/", "M63", 1.0).
?test(sheet1_N63, "/EQUAL/", "N63", 1.0).
?test(sheet1_O63, "/EQUAL/", "O63", 1.0).
?test(sheet1_P63, "/EQUAL/", "P63", 1.0).
?test(sheet1_Q63, "/EQUAL/", "Q63", 1.0).
?test(sheet1_R63, "/EQUAL/", "R63", 1.0).
?test(sheet1_S63, "/EQUAL/", "S63", 1.0).
?test(sheet1_T63, "/EQUAL/", "T63", 1.0).
?test(sheet1_U63, "/EQUAL/", "U63", 1.0).
?test(sheet1_V63, "/EQUAL/", "V63", 1.0).
?test(sheet1_C64, "/EQUAL/", "C64", 1.0).
?test(sheet1_D64, "/EQUAL/", "D64", 1.0).
?test(sheet1_E64, "/EQUAL/", "E64", 1.0).
?test(sheet1_F64, "/EQUAL/", "F64", 1.0).
?test(sheet1_G64, "/EQUAL/", "G64", 1.0).
?test(sheet1_H64, "/EQUAL/", "H64", 1.0).
?test(sheet1_I64, "/EQUAL/", "I64", 1.0).
?test(sheet1_J64, "/EQUAL/", "J64", 1.0).
?test(sheet1_K64, "/EQUAL/", "K64", 1.0).
?test(sheet1_L64, "/EQUAL/", "L64", 1.0).
?test(sheet1_M64, "/EQUAL/", "M64", 1.0).
?test(sheet1_N64, "/EQUAL/", "N64", 1.0).
?test(sheet1_O64, "/EQUAL/", "O64", 1.0).
?test(sheet1_P64, "/EQUAL/", "P64", 1.0).
?test(sheet1_Q64, "/EQUAL/", "Q64", 1.0).
?test(sheet1_R64, "/EQUAL/", "R64", 1.0).
?test(sheet1_S64, "/EQUAL/", "S64", 1.0).
?test(sheet1_T64, "/EQUAL/", "T64", 1.0).
?test(sheet1_U64, "/EQUAL/", "U64", 1.0).
?test(sheet1_V64, "/EQUAL/", "V64", 1.0).
?test(sheet1_C65, "/EQUAL/", "C65", 1.0).
?test(sheet1_D65, "/EQUAL/", "D65", 1.0).
?test(sheet1_E65, "/EQUAL/", "E65", 1.0).
?test(sheet1_F65, "/EQUAL/", "F65", 1.0).
?test(sheet1_G65, "/EQUAL/", "G65", 1.0).
?test(sheet1_H65, "/EQUAL/", "H65", 1.0).
?test(sheet1_I65, "/EQUAL/", "I65", 1.0).
?test(sheet1_J65, "/EQUAL/", "J65", 1.0).
?test(sheet1_K65, "/EQUAL/", "K65", 1.0).
?test(sheet1_L65, "/EQUAL/", "L65", 1.0).
?test(sheet1_M65, "/EQUAL/", "M65", 1.0).
?test(sheet1_N65, "/EQUAL/", "N65", 1.0).
?test(sheet1_O65, "/EQUAL/", "O65", 1.0).
?test(sheet1_P65, "/EQUAL/", "P65", 1.0).
?test(sheet1_Q65, "/EQUAL/", "Q65", 1.0).
?test(sheet1_R65, "/EQUAL/", "R65", 1.0).
?test(sheet1_S65, "/EQUAL/", "S65", 1.0).
?test(sheet1_T65, "/EQUAL/", "T65", 1.0).
?test(sheet1_U65, "/EQUAL/", "U65", 1.0).
?test(sheet1_V65, "/EQUAL/", "V65", 1.0).
?test(sheet1_C66, "/EQUAL/", "C66", 1.0).
?test(sheet1_D66, "/EQUAL/", "D66", 1.0).
?test(sheet1_E66, "/EQUAL/", "E66", 1.0).
?test(sheet1_F66, "/EQUAL/", "F66", 1.0).
?test(sheet1_G66, "/EQUAL/", "G66", 1.0).
?test(sheet1_H66, "/EQUAL/", "H66", 1.0).
?test(sheet1_I66, "/EQUAL/", "I66", 1.0).
?test(sheet1_J66, "/EQUAL/", "J66", 1.0).
?test(sheet1_K66, "/EQUAL/", "K66", 1.0).
?test(sheet1_L66, "/EQUAL/", "L66", 1.0).
?test(sheet1_M66, "/EQUAL/", "M66", 1.0).
?test(sheet1_N66, "/EQUAL/", "N66", 1.0).
?test(sheet1_O66, "/EQUAL/", "O66", 1.0).
?test(sheet1_P66, "/EQUAL/", "P66", 1.0).
?test(sheet1_Q66, "/EQUAL/", "Q66", 1.0).
?test(sheet1_R66, "/EQUAL/", "R66", 1.0).
?test(sheet1_S66, "/EQUAL/", "S66", 1.0).
?test(sheet1_T66, "/EQUAL/", "T66", 1.0).
?test(sheet1_U66, "/EQUAL/", "U66", 1.0).
?test(sheet1_V66, "/EQUAL/", "V66", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_operators_eq.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_operators_eq" ++ "/" ++ Sheetname ++ "/",
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
