%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_operators_gte_SUITE).
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
                     [Testcase, "e_gnumeric_operators_gte_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_operators_gte" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/GTE/", "A1", ">=").
?test(sheet1_B1, "/GTE/", "B1", "B").
?test(sheet1_C1, "/GTE/", "C1", "Blank").
?test(sheet1_D1, "/GTE/", "D1", "Boolean").
?test(sheet1_E1, "/GTE/", "E1", "Boolean").
?test(sheet1_F1, "/GTE/", "F1", "Error").
?test(sheet1_G1, "/GTE/", "G1", "Error").
?test(sheet1_H1, "/GTE/", "H1", "Error").
?test(sheet1_I1, "/GTE/", "I1", "Error").
?test(sheet1_J1, "/GTE/", "J1", "Error").
?test(sheet1_K1, "/GTE/", "K1", "Error").
?test(sheet1_L1, "/GTE/", "L1", "Error").
?test(sheet1_M1, "/GTE/", "M1", "String").
?test(sheet1_N1, "/GTE/", "N1", "String").
?test(sheet1_O1, "/GTE/", "O1", "String").
?test(sheet1_P1, "/GTE/", "P1", "Str Num").
?test(sheet1_Q1, "/GTE/", "Q1", "Str Num").
?test(sheet1_R1, "/GTE/", "R1", "Integer").
?test(sheet1_S1, "/GTE/", "S1", "Integer").
?test(sheet1_T1, "/GTE/", "T1", "Zero").
?test(sheet1_U1, "/GTE/", "U1", "Float").
?test(sheet1_V1, "/GTE/", "V1", "Float").
?test(sheet1_A2, "/GTE/", "A2", "A").
?test(sheet1_D2, "/GTE/", "D2", true).
?test(sheet1_E2, "/GTE/", "E2", false).
?test(sheet1_F2, "/GTE/", "F2", '#DIV/0!').
?test(sheet1_G2, "/GTE/", "G2", '#N/A').
?test(sheet1_H2, "/GTE/", "H2", '#NAME?').
?test(sheet1_I2, "/GTE/", "I2", 'NULL!').
?test(sheet1_J2, "/GTE/", "J2", '#NUM!').
?test(sheet1_K2, "/GTE/", "K2", '#REF!').
?test(sheet1_L2, "/GTE/", "L2", '#VALUE!').
?test(sheet1_M2, "/GTE/", "M2", "Liz").
?test(sheet1_N2, "/GTE/", "N2", "Doug").
?test(sheet1_O2, "/GTE/", "O2", "Bob").
?test(sheet1_P2, "/GTE/", "P2", "2.7").
?test(sheet1_Q2, "/GTE/", "Q2", "3.54").
?test(sheet1_R2, "/GTE/", "R2", "1999/02/01 00:00:00").
?test(sheet1_S2, "/GTE/", "S2", "1999/02/02 00:00:00").
?test(sheet1_T2, "/GTE/", "T2", 0.0).
?test(sheet1_U2, "/GTE/", "U2", 3.1415).
?test(sheet1_V2, "/GTE/", "V2", 36193.2).
?test(sheet1_A3, "/GTE/", "A3", "Blank").
?test(sheet1_C3, "/GTE/", "C3", true).
?test(sheet1_D3, "/GTE/", "D3", false).
?test(sheet1_E3, "/GTE/", "E3", true).
?test(sheet1_F3, "/GTE/", "F3", '#DIV/0!').
?test(sheet1_G3, "/GTE/", "G3", '#N/A').
?test(sheet1_H3, "/GTE/", "H3", '#NAME?').
?test(sheet1_I3, "/GTE/", "I3", 'NULL!').
?test(sheet1_J3, "/GTE/", "J3", '#NUM!').
?test(sheet1_K3, "/GTE/", "K3", '#REF!').
?test(sheet1_L3, "/GTE/", "L3", '#VALUE!').
?test(sheet1_M3, "/GTE/", "M3", false).
?test(sheet1_N3, "/GTE/", "N3", false).
?test(sheet1_O3, "/GTE/", "O3", false).
?test(sheet1_P3, "/GTE/", "P3", false).
?test(sheet1_Q3, "/GTE/", "Q3", false).
?test(sheet1_R3, "/GTE/", "R3", false).
?test(sheet1_S3, "/GTE/", "S3", false).
?test(sheet1_T3, "/GTE/", "T3", true).
?test(sheet1_U3, "/GTE/", "U3", false).
?test(sheet1_V3, "/GTE/", "V3", false).
?test(sheet1_A4, "/GTE/", "A4", "Boolean").
?test(sheet1_B4, "/GTE/", "B4", true).
?test(sheet1_C4, "/GTE/", "C4", true).
?test(sheet1_D4, "/GTE/", "D4", true).
?test(sheet1_E4, "/GTE/", "E4", true).
?test(sheet1_F4, "/GTE/", "F4", '#DIV/0!').
?test(sheet1_G4, "/GTE/", "G4", '#N/A').
?test(sheet1_H4, "/GTE/", "H4", '#NAME?').
?test(sheet1_I4, "/GTE/", "I4", 'NULL!').
?test(sheet1_J4, "/GTE/", "J4", '#NUM!').
?test(sheet1_K4, "/GTE/", "K4", '#REF!').
?test(sheet1_L4, "/GTE/", "L4", '#VALUE!').
?test(sheet1_M4, "/GTE/", "M4", true).
?test(sheet1_N4, "/GTE/", "N4", true).
?test(sheet1_O4, "/GTE/", "O4", true).
?test(sheet1_P4, "/GTE/", "P4", true).
?test(sheet1_Q4, "/GTE/", "Q4", true).
?test(sheet1_R4, "/GTE/", "R4", true).
?test(sheet1_S4, "/GTE/", "S4", true).
?test(sheet1_T4, "/GTE/", "T4", true).
?test(sheet1_U4, "/GTE/", "U4", true).
?test(sheet1_V4, "/GTE/", "V4", true).
?test(sheet1_A5, "/GTE/", "A5", "Boolean").
?test(sheet1_B5, "/GTE/", "B5", false).
?test(sheet1_C5, "/GTE/", "C5", true).
?test(sheet1_D5, "/GTE/", "D5", false).
?test(sheet1_E5, "/GTE/", "E5", true).
?test(sheet1_F5, "/GTE/", "F5", '#DIV/0!').
?test(sheet1_G5, "/GTE/", "G5", '#N/A').
?test(sheet1_H5, "/GTE/", "H5", '#NAME?').
?test(sheet1_I5, "/GTE/", "I5", 'NULL!').
?test(sheet1_J5, "/GTE/", "J5", '#NUM!').
?test(sheet1_K5, "/GTE/", "K5", '#REF!').
?test(sheet1_L5, "/GTE/", "L5", '#VALUE!').
?test(sheet1_M5, "/GTE/", "M5", true).
?test(sheet1_N5, "/GTE/", "N5", true).
?test(sheet1_O5, "/GTE/", "O5", true).
?test(sheet1_P5, "/GTE/", "P5", true).
?test(sheet1_Q5, "/GTE/", "Q5", true).
?test(sheet1_R5, "/GTE/", "R5", true).
?test(sheet1_S5, "/GTE/", "S5", true).
?test(sheet1_T5, "/GTE/", "T5", true).
?test(sheet1_U5, "/GTE/", "U5", true).
?test(sheet1_V5, "/GTE/", "V5", true).
?test(sheet1_A6, "/GTE/", "A6", "Error").
?test(sheet1_B6, "/GTE/", "B6", '#DIV/0!').
?test(sheet1_C6, "/GTE/", "C6", '#DIV/0!').
?test(sheet1_D6, "/GTE/", "D6", '#DIV/0!').
?test(sheet1_E6, "/GTE/", "E6", '#DIV/0!').
?test(sheet1_F6, "/GTE/", "F6", '#DIV/0!').
?test(sheet1_G6, "/GTE/", "G6", '#DIV/0!').
?test(sheet1_H6, "/GTE/", "H6", '#DIV/0!').
?test(sheet1_I6, "/GTE/", "I6", '#DIV/0!').
?test(sheet1_J6, "/GTE/", "J6", '#DIV/0!').
?test(sheet1_K6, "/GTE/", "K6", '#DIV/0!').
?test(sheet1_L6, "/GTE/", "L6", '#DIV/0!').
?test(sheet1_M6, "/GTE/", "M6", '#DIV/0!').
?test(sheet1_N6, "/GTE/", "N6", '#DIV/0!').
?test(sheet1_O6, "/GTE/", "O6", '#DIV/0!').
?test(sheet1_P6, "/GTE/", "P6", '#DIV/0!').
?test(sheet1_Q6, "/GTE/", "Q6", '#DIV/0!').
?test(sheet1_R6, "/GTE/", "R6", '#DIV/0!').
?test(sheet1_S6, "/GTE/", "S6", '#DIV/0!').
?test(sheet1_T6, "/GTE/", "T6", '#DIV/0!').
?test(sheet1_U6, "/GTE/", "U6", '#DIV/0!').
?test(sheet1_V6, "/GTE/", "V6", '#DIV/0!').
?test(sheet1_A7, "/GTE/", "A7", "Error").
?test(sheet1_B7, "/GTE/", "B7", '#N/A').
?test(sheet1_C7, "/GTE/", "C7", '#N/A').
?test(sheet1_D7, "/GTE/", "D7", '#N/A').
?test(sheet1_E7, "/GTE/", "E7", '#N/A').
?test(sheet1_F7, "/GTE/", "F7", '#N/A').
?test(sheet1_G7, "/GTE/", "G7", '#N/A').
?test(sheet1_H7, "/GTE/", "H7", '#N/A').
?test(sheet1_I7, "/GTE/", "I7", '#N/A').
?test(sheet1_J7, "/GTE/", "J7", '#N/A').
?test(sheet1_K7, "/GTE/", "K7", '#N/A').
?test(sheet1_L7, "/GTE/", "L7", '#N/A').
?test(sheet1_M7, "/GTE/", "M7", '#N/A').
?test(sheet1_N7, "/GTE/", "N7", '#N/A').
?test(sheet1_O7, "/GTE/", "O7", '#N/A').
?test(sheet1_P7, "/GTE/", "P7", '#N/A').
?test(sheet1_Q7, "/GTE/", "Q7", '#N/A').
?test(sheet1_R7, "/GTE/", "R7", '#N/A').
?test(sheet1_S7, "/GTE/", "S7", '#N/A').
?test(sheet1_T7, "/GTE/", "T7", '#N/A').
?test(sheet1_U7, "/GTE/", "U7", '#N/A').
?test(sheet1_V7, "/GTE/", "V7", '#N/A').
?test(sheet1_A8, "/GTE/", "A8", "Error").
?test(sheet1_B8, "/GTE/", "B8", '#NAME?').
?test(sheet1_C8, "/GTE/", "C8", '#NAME?').
?test(sheet1_D8, "/GTE/", "D8", '#NAME?').
?test(sheet1_E8, "/GTE/", "E8", '#NAME?').
?test(sheet1_F8, "/GTE/", "F8", '#NAME?').
?test(sheet1_G8, "/GTE/", "G8", '#NAME?').
?test(sheet1_H8, "/GTE/", "H8", '#NAME?').
?test(sheet1_I8, "/GTE/", "I8", '#NAME?').
?test(sheet1_J8, "/GTE/", "J8", '#NAME?').
?test(sheet1_K8, "/GTE/", "K8", '#NAME?').
?test(sheet1_L8, "/GTE/", "L8", '#NAME?').
?test(sheet1_M8, "/GTE/", "M8", '#NAME?').
?test(sheet1_N8, "/GTE/", "N8", '#NAME?').
?test(sheet1_O8, "/GTE/", "O8", '#NAME?').
?test(sheet1_P8, "/GTE/", "P8", '#NAME?').
?test(sheet1_Q8, "/GTE/", "Q8", '#NAME?').
?test(sheet1_R8, "/GTE/", "R8", '#NAME?').
?test(sheet1_S8, "/GTE/", "S8", '#NAME?').
?test(sheet1_T8, "/GTE/", "T8", '#NAME?').
?test(sheet1_U8, "/GTE/", "U8", '#NAME?').
?test(sheet1_V8, "/GTE/", "V8", '#NAME?').
?test(sheet1_A9, "/GTE/", "A9", "Error").
?test(sheet1_B9, "/GTE/", "B9", 'NULL!').
?test(sheet1_C9, "/GTE/", "C9", 'NULL!').
?test(sheet1_D9, "/GTE/", "D9", 'NULL!').
?test(sheet1_E9, "/GTE/", "E9", 'NULL!').
?test(sheet1_F9, "/GTE/", "F9", 'NULL!').
?test(sheet1_G9, "/GTE/", "G9", 'NULL!').
?test(sheet1_H9, "/GTE/", "H9", 'NULL!').
?test(sheet1_I9, "/GTE/", "I9", 'NULL!').
?test(sheet1_J9, "/GTE/", "J9", 'NULL!').
?test(sheet1_K9, "/GTE/", "K9", 'NULL!').
?test(sheet1_L9, "/GTE/", "L9", 'NULL!').
?test(sheet1_M9, "/GTE/", "M9", 'NULL!').
?test(sheet1_N9, "/GTE/", "N9", 'NULL!').
?test(sheet1_O9, "/GTE/", "O9", 'NULL!').
?test(sheet1_P9, "/GTE/", "P9", 'NULL!').
?test(sheet1_Q9, "/GTE/", "Q9", 'NULL!').
?test(sheet1_R9, "/GTE/", "R9", 'NULL!').
?test(sheet1_S9, "/GTE/", "S9", 'NULL!').
?test(sheet1_T9, "/GTE/", "T9", 'NULL!').
?test(sheet1_U9, "/GTE/", "U9", 'NULL!').
?test(sheet1_V9, "/GTE/", "V9", 'NULL!').
?test(sheet1_A10, "/GTE/", "A10", "Error").
?test(sheet1_B10, "/GTE/", "B10", '#NUM!').
?test(sheet1_C10, "/GTE/", "C10", '#NUM!').
?test(sheet1_D10, "/GTE/", "D10", '#NUM!').
?test(sheet1_E10, "/GTE/", "E10", '#NUM!').
?test(sheet1_F10, "/GTE/", "F10", '#NUM!').
?test(sheet1_G10, "/GTE/", "G10", '#NUM!').
?test(sheet1_H10, "/GTE/", "H10", '#NUM!').
?test(sheet1_I10, "/GTE/", "I10", '#NUM!').
?test(sheet1_J10, "/GTE/", "J10", '#NUM!').
?test(sheet1_K10, "/GTE/", "K10", '#NUM!').
?test(sheet1_L10, "/GTE/", "L10", '#NUM!').
?test(sheet1_M10, "/GTE/", "M10", '#NUM!').
?test(sheet1_N10, "/GTE/", "N10", '#NUM!').
?test(sheet1_O10, "/GTE/", "O10", '#NUM!').
?test(sheet1_P10, "/GTE/", "P10", '#NUM!').
?test(sheet1_Q10, "/GTE/", "Q10", '#NUM!').
?test(sheet1_R10, "/GTE/", "R10", '#NUM!').
?test(sheet1_S10, "/GTE/", "S10", '#NUM!').
?test(sheet1_T10, "/GTE/", "T10", '#NUM!').
?test(sheet1_U10, "/GTE/", "U10", '#NUM!').
?test(sheet1_V10, "/GTE/", "V10", '#NUM!').
?test(sheet1_A11, "/GTE/", "A11", "Error").
?test(sheet1_B11, "/GTE/", "B11", '#REF!').
?test(sheet1_C11, "/GTE/", "C11", '#REF!').
?test(sheet1_D11, "/GTE/", "D11", '#REF!').
?test(sheet1_E11, "/GTE/", "E11", '#REF!').
?test(sheet1_F11, "/GTE/", "F11", '#REF!').
?test(sheet1_G11, "/GTE/", "G11", '#REF!').
?test(sheet1_H11, "/GTE/", "H11", '#REF!').
?test(sheet1_I11, "/GTE/", "I11", '#REF!').
?test(sheet1_J11, "/GTE/", "J11", '#REF!').
?test(sheet1_K11, "/GTE/", "K11", '#REF!').
?test(sheet1_L11, "/GTE/", "L11", '#REF!').
?test(sheet1_M11, "/GTE/", "M11", '#REF!').
?test(sheet1_N11, "/GTE/", "N11", '#REF!').
?test(sheet1_O11, "/GTE/", "O11", '#REF!').
?test(sheet1_P11, "/GTE/", "P11", '#REF!').
?test(sheet1_Q11, "/GTE/", "Q11", '#REF!').
?test(sheet1_R11, "/GTE/", "R11", '#REF!').
?test(sheet1_S11, "/GTE/", "S11", '#REF!').
?test(sheet1_T11, "/GTE/", "T11", '#REF!').
?test(sheet1_U11, "/GTE/", "U11", '#REF!').
?test(sheet1_V11, "/GTE/", "V11", '#REF!').
?test(sheet1_A12, "/GTE/", "A12", "Error").
?test(sheet1_B12, "/GTE/", "B12", '#VALUE!').
?test(sheet1_C12, "/GTE/", "C12", '#VALUE!').
?test(sheet1_D12, "/GTE/", "D12", '#VALUE!').
?test(sheet1_E12, "/GTE/", "E12", '#VALUE!').
?test(sheet1_F12, "/GTE/", "F12", '#VALUE!').
?test(sheet1_G12, "/GTE/", "G12", '#VALUE!').
?test(sheet1_H12, "/GTE/", "H12", '#VALUE!').
?test(sheet1_I12, "/GTE/", "I12", '#VALUE!').
?test(sheet1_J12, "/GTE/", "J12", '#VALUE!').
?test(sheet1_K12, "/GTE/", "K12", '#VALUE!').
?test(sheet1_L12, "/GTE/", "L12", '#VALUE!').
?test(sheet1_M12, "/GTE/", "M12", '#VALUE!').
?test(sheet1_N12, "/GTE/", "N12", '#VALUE!').
?test(sheet1_O12, "/GTE/", "O12", '#VALUE!').
?test(sheet1_P12, "/GTE/", "P12", '#VALUE!').
?test(sheet1_Q12, "/GTE/", "Q12", '#VALUE!').
?test(sheet1_R12, "/GTE/", "R12", '#VALUE!').
?test(sheet1_S12, "/GTE/", "S12", '#VALUE!').
?test(sheet1_T12, "/GTE/", "T12", '#VALUE!').
?test(sheet1_U12, "/GTE/", "U12", '#VALUE!').
?test(sheet1_V12, "/GTE/", "V12", '#VALUE!').
?test(sheet1_A13, "/GTE/", "A13", "String").
?test(sheet1_B13, "/GTE/", "B13", "Liz").
?test(sheet1_C13, "/GTE/", "C13", true).
?test(sheet1_D13, "/GTE/", "D13", false).
?test(sheet1_E13, "/GTE/", "E13", false).
?test(sheet1_F13, "/GTE/", "F13", '#DIV/0!').
?test(sheet1_G13, "/GTE/", "G13", '#N/A').
?test(sheet1_H13, "/GTE/", "H13", '#NAME?').
?test(sheet1_I13, "/GTE/", "I13", 'NULL!').
?test(sheet1_J13, "/GTE/", "J13", '#NUM!').
?test(sheet1_K13, "/GTE/", "K13", '#REF!').
?test(sheet1_L13, "/GTE/", "L13", '#VALUE!').
?test(sheet1_M13, "/GTE/", "M13", true).
?test(sheet1_N13, "/GTE/", "N13", true).
?test(sheet1_O13, "/GTE/", "O13", true).
?test(sheet1_P13, "/GTE/", "P13", true).
?test(sheet1_Q13, "/GTE/", "Q13", true).
?test(sheet1_R13, "/GTE/", "R13", true).
?test(sheet1_S13, "/GTE/", "S13", true).
?test(sheet1_T13, "/GTE/", "T13", true).
?test(sheet1_U13, "/GTE/", "U13", true).
?test(sheet1_V13, "/GTE/", "V13", true).
?test(sheet1_A14, "/GTE/", "A14", "String").
?test(sheet1_B14, "/GTE/", "B14", "Doug").
?test(sheet1_C14, "/GTE/", "C14", true).
?test(sheet1_D14, "/GTE/", "D14", false).
?test(sheet1_E14, "/GTE/", "E14", false).
?test(sheet1_F14, "/GTE/", "F14", '#DIV/0!').
?test(sheet1_G14, "/GTE/", "G14", '#N/A').
?test(sheet1_H14, "/GTE/", "H14", '#NAME?').
?test(sheet1_I14, "/GTE/", "I14", 'NULL!').
?test(sheet1_J14, "/GTE/", "J14", '#NUM!').
?test(sheet1_K14, "/GTE/", "K14", '#REF!').
?test(sheet1_L14, "/GTE/", "L14", '#VALUE!').
?test(sheet1_M14, "/GTE/", "M14", false).
?test(sheet1_N14, "/GTE/", "N14", true).
?test(sheet1_O14, "/GTE/", "O14", true).
?test(sheet1_P14, "/GTE/", "P14", true).
?test(sheet1_Q14, "/GTE/", "Q14", true).
?test(sheet1_R14, "/GTE/", "R14", true).
?test(sheet1_S14, "/GTE/", "S14", true).
?test(sheet1_T14, "/GTE/", "T14", true).
?test(sheet1_U14, "/GTE/", "U14", true).
?test(sheet1_V14, "/GTE/", "V14", true).
?test(sheet1_A15, "/GTE/", "A15", "String").
?test(sheet1_B15, "/GTE/", "B15", "Bob").
?test(sheet1_C15, "/GTE/", "C15", true).
?test(sheet1_D15, "/GTE/", "D15", false).
?test(sheet1_E15, "/GTE/", "E15", false).
?test(sheet1_F15, "/GTE/", "F15", '#DIV/0!').
?test(sheet1_G15, "/GTE/", "G15", '#N/A').
?test(sheet1_H15, "/GTE/", "H15", '#NAME?').
?test(sheet1_I15, "/GTE/", "I15", 'NULL!').
?test(sheet1_J15, "/GTE/", "J15", '#NUM!').
?test(sheet1_K15, "/GTE/", "K15", '#REF!').
?test(sheet1_L15, "/GTE/", "L15", '#VALUE!').
?test(sheet1_M15, "/GTE/", "M15", false).
?test(sheet1_N15, "/GTE/", "N15", false).
?test(sheet1_O15, "/GTE/", "O15", true).
?test(sheet1_P15, "/GTE/", "P15", true).
?test(sheet1_Q15, "/GTE/", "Q15", true).
?test(sheet1_R15, "/GTE/", "R15", true).
?test(sheet1_S15, "/GTE/", "S15", true).
?test(sheet1_T15, "/GTE/", "T15", true).
?test(sheet1_U15, "/GTE/", "U15", true).
?test(sheet1_V15, "/GTE/", "V15", true).
?test(sheet1_A16, "/GTE/", "A16", "Str Num").
?test(sheet1_B16, "/GTE/", "B16", "2.7").
?test(sheet1_C16, "/GTE/", "C16", true).
?test(sheet1_D16, "/GTE/", "D16", false).
?test(sheet1_E16, "/GTE/", "E16", false).
?test(sheet1_F16, "/GTE/", "F16", '#DIV/0!').
?test(sheet1_G16, "/GTE/", "G16", '#N/A').
?test(sheet1_H16, "/GTE/", "H16", '#NAME?').
?test(sheet1_I16, "/GTE/", "I16", 'NULL!').
?test(sheet1_J16, "/GTE/", "J16", '#NUM!').
?test(sheet1_K16, "/GTE/", "K16", '#REF!').
?test(sheet1_L16, "/GTE/", "L16", '#VALUE!').
?test(sheet1_M16, "/GTE/", "M16", false).
?test(sheet1_N16, "/GTE/", "N16", false).
?test(sheet1_O16, "/GTE/", "O16", false).
?test(sheet1_P16, "/GTE/", "P16", true).
?test(sheet1_Q16, "/GTE/", "Q16", false).
?test(sheet1_R16, "/GTE/", "R16", true).
?test(sheet1_S16, "/GTE/", "S16", true).
?test(sheet1_T16, "/GTE/", "T16", true).
?test(sheet1_U16, "/GTE/", "U16", true).
?test(sheet1_V16, "/GTE/", "V16", true).
?test(sheet1_A17, "/GTE/", "A17", "Str Num").
?test(sheet1_B17, "/GTE/", "B17", "3.54").
?test(sheet1_C17, "/GTE/", "C17", true).
?test(sheet1_D17, "/GTE/", "D17", false).
?test(sheet1_E17, "/GTE/", "E17", false).
?test(sheet1_F17, "/GTE/", "F17", '#DIV/0!').
?test(sheet1_G17, "/GTE/", "G17", '#N/A').
?test(sheet1_H17, "/GTE/", "H17", '#NAME?').
?test(sheet1_I17, "/GTE/", "I17", 'NULL!').
?test(sheet1_J17, "/GTE/", "J17", '#NUM!').
?test(sheet1_K17, "/GTE/", "K17", '#REF!').
?test(sheet1_L17, "/GTE/", "L17", '#VALUE!').
?test(sheet1_M17, "/GTE/", "M17", false).
?test(sheet1_N17, "/GTE/", "N17", false).
?test(sheet1_O17, "/GTE/", "O17", false).
?test(sheet1_P17, "/GTE/", "P17", true).
?test(sheet1_Q17, "/GTE/", "Q17", true).
?test(sheet1_R17, "/GTE/", "R17", true).
?test(sheet1_S17, "/GTE/", "S17", true).
?test(sheet1_T17, "/GTE/", "T17", true).
?test(sheet1_U17, "/GTE/", "U17", true).
?test(sheet1_V17, "/GTE/", "V17", true).
?test(sheet1_A18, "/GTE/", "A18", "Integer").
?test(sheet1_B18, "/GTE/", "B18", "1999/02/01 00:00:00").
?test(sheet1_C18, "/GTE/", "C18", true).
?test(sheet1_D18, "/GTE/", "D18", false).
?test(sheet1_E18, "/GTE/", "E18", false).
?test(sheet1_F18, "/GTE/", "F18", '#DIV/0!').
?test(sheet1_G18, "/GTE/", "G18", '#N/A').
?test(sheet1_H18, "/GTE/", "H18", '#NAME?').
?test(sheet1_I18, "/GTE/", "I18", 'NULL!').
?test(sheet1_J18, "/GTE/", "J18", '#NUM!').
?test(sheet1_K18, "/GTE/", "K18", '#REF!').
?test(sheet1_L18, "/GTE/", "L18", '#VALUE!').
?test(sheet1_M18, "/GTE/", "M18", false).
?test(sheet1_N18, "/GTE/", "N18", false).
?test(sheet1_O18, "/GTE/", "O18", false).
?test(sheet1_P18, "/GTE/", "P18", false).
?test(sheet1_Q18, "/GTE/", "Q18", false).
?test(sheet1_R18, "/GTE/", "R18", true).
?test(sheet1_S18, "/GTE/", "S18", false).
?test(sheet1_T18, "/GTE/", "T18", true).
?test(sheet1_U18, "/GTE/", "U18", true).
?test(sheet1_V18, "/GTE/", "V18", false).
?test(sheet1_A19, "/GTE/", "A19", "Integer").
?test(sheet1_B19, "/GTE/", "B19", "1999/02/02 00:00:00").
?test(sheet1_C19, "/GTE/", "C19", true).
?test(sheet1_D19, "/GTE/", "D19", false).
?test(sheet1_E19, "/GTE/", "E19", false).
?test(sheet1_F19, "/GTE/", "F19", '#DIV/0!').
?test(sheet1_G19, "/GTE/", "G19", '#N/A').
?test(sheet1_H19, "/GTE/", "H19", '#NAME?').
?test(sheet1_I19, "/GTE/", "I19", 'NULL!').
?test(sheet1_J19, "/GTE/", "J19", '#NUM!').
?test(sheet1_K19, "/GTE/", "K19", '#REF!').
?test(sheet1_L19, "/GTE/", "L19", '#VALUE!').
?test(sheet1_M19, "/GTE/", "M19", false).
?test(sheet1_N19, "/GTE/", "N19", false).
?test(sheet1_O19, "/GTE/", "O19", false).
?test(sheet1_P19, "/GTE/", "P19", false).
?test(sheet1_Q19, "/GTE/", "Q19", false).
?test(sheet1_R19, "/GTE/", "R19", true).
?test(sheet1_S19, "/GTE/", "S19", true).
?test(sheet1_T19, "/GTE/", "T19", true).
?test(sheet1_U19, "/GTE/", "U19", true).
?test(sheet1_V19, "/GTE/", "V19", false).
?test(sheet1_A20, "/GTE/", "A20", "Zero").
?test(sheet1_B20, "/GTE/", "B20", 0.0).
?test(sheet1_C20, "/GTE/", "C20", true).
?test(sheet1_D20, "/GTE/", "D20", false).
?test(sheet1_E20, "/GTE/", "E20", false).
?test(sheet1_F20, "/GTE/", "F20", '#DIV/0!').
?test(sheet1_G20, "/GTE/", "G20", '#N/A').
?test(sheet1_H20, "/GTE/", "H20", '#NAME?').
?test(sheet1_I20, "/GTE/", "I20", 'NULL!').
?test(sheet1_J20, "/GTE/", "J20", '#NUM!').
?test(sheet1_K20, "/GTE/", "K20", '#REF!').
?test(sheet1_L20, "/GTE/", "L20", '#VALUE!').
?test(sheet1_M20, "/GTE/", "M20", false).
?test(sheet1_N20, "/GTE/", "N20", false).
?test(sheet1_O20, "/GTE/", "O20", false).
?test(sheet1_P20, "/GTE/", "P20", false).
?test(sheet1_Q20, "/GTE/", "Q20", false).
?test(sheet1_R20, "/GTE/", "R20", false).
?test(sheet1_S20, "/GTE/", "S20", false).
?test(sheet1_T20, "/GTE/", "T20", true).
?test(sheet1_U20, "/GTE/", "U20", false).
?test(sheet1_V20, "/GTE/", "V20", false).
?test(sheet1_A21, "/GTE/", "A21", "Float").
?test(sheet1_B21, "/GTE/", "B21", 3.1415).
?test(sheet1_C21, "/GTE/", "C21", true).
?test(sheet1_D21, "/GTE/", "D21", false).
?test(sheet1_E21, "/GTE/", "E21", false).
?test(sheet1_F21, "/GTE/", "F21", '#DIV/0!').
?test(sheet1_G21, "/GTE/", "G21", '#N/A').
?test(sheet1_H21, "/GTE/", "H21", '#NAME?').
?test(sheet1_I21, "/GTE/", "I21", 'NULL!').
?test(sheet1_J21, "/GTE/", "J21", '#NUM!').
?test(sheet1_K21, "/GTE/", "K21", '#REF!').
?test(sheet1_L21, "/GTE/", "L21", '#VALUE!').
?test(sheet1_M21, "/GTE/", "M21", false).
?test(sheet1_N21, "/GTE/", "N21", false).
?test(sheet1_O21, "/GTE/", "O21", false).
?test(sheet1_P21, "/GTE/", "P21", false).
?test(sheet1_Q21, "/GTE/", "Q21", false).
?test(sheet1_R21, "/GTE/", "R21", false).
?test(sheet1_S21, "/GTE/", "S21", false).
?test(sheet1_T21, "/GTE/", "T21", true).
?test(sheet1_U21, "/GTE/", "U21", true).
?test(sheet1_V21, "/GTE/", "V21", false).
?test(sheet1_A22, "/GTE/", "A22", "Float").
?test(sheet1_B22, "/GTE/", "B22", 36193.2).
?test(sheet1_C22, "/GTE/", "C22", true).
?test(sheet1_D22, "/GTE/", "D22", false).
?test(sheet1_E22, "/GTE/", "E22", false).
?test(sheet1_F22, "/GTE/", "F22", '#DIV/0!').
?test(sheet1_G22, "/GTE/", "G22", '#N/A').
?test(sheet1_H22, "/GTE/", "H22", '#NAME?').
?test(sheet1_I22, "/GTE/", "I22", 'NULL!').
?test(sheet1_J22, "/GTE/", "J22", '#NUM!').
?test(sheet1_K22, "/GTE/", "K22", '#REF!').
?test(sheet1_L22, "/GTE/", "L22", '#VALUE!').
?test(sheet1_M22, "/GTE/", "M22", false).
?test(sheet1_N22, "/GTE/", "N22", false).
?test(sheet1_O22, "/GTE/", "O22", false).
?test(sheet1_P22, "/GTE/", "P22", false).
?test(sheet1_Q22, "/GTE/", "Q22", false).
?test(sheet1_R22, "/GTE/", "R22", true).
?test(sheet1_S22, "/GTE/", "S22", true).
?test(sheet1_T22, "/GTE/", "T22", true).
?test(sheet1_U22, "/GTE/", "U22", true).
?test(sheet1_V22, "/GTE/", "V22", true).
?test(sheet1_A25, "/GTE/", "A25", "Blank").
?test(sheet1_C25, "/GTE/", "C25", true).
?test(sheet1_D25, "/GTE/", "D25", false).
?test(sheet1_E25, "/GTE/", "E25", true).
?test(sheet1_F25, "/GTE/", "F25", '#DIV/0!').
?test(sheet1_G25, "/GTE/", "G25", '#N/A').
?test(sheet1_H25, "/GTE/", "H25", '#NAME?').
?test(sheet1_I25, "/GTE/", "I25", 'NULL!').
?test(sheet1_J25, "/GTE/", "J25", '#NUM!').
?test(sheet1_K25, "/GTE/", "K25", '#REF!').
?test(sheet1_L25, "/GTE/", "L25", '#VALUE!').
?test(sheet1_M25, "/GTE/", "M25", false).
?test(sheet1_N25, "/GTE/", "N25", false).
?test(sheet1_O25, "/GTE/", "O25", false).
?test(sheet1_P25, "/GTE/", "P25", false).
?test(sheet1_Q25, "/GTE/", "Q25", false).
?test(sheet1_R25, "/GTE/", "R25", false).
?test(sheet1_S25, "/GTE/", "S25", false).
?test(sheet1_T25, "/GTE/", "T25", true).
?test(sheet1_U25, "/GTE/", "U25", false).
?test(sheet1_V25, "/GTE/", "V25", false).
?test(sheet1_A26, "/GTE/", "A26", "Boolean").
?test(sheet1_C26, "/GTE/", "C26", true).
?test(sheet1_D26, "/GTE/", "D26", true).
?test(sheet1_E26, "/GTE/", "E26", true).
?test(sheet1_F26, "/GTE/", "F26", '#DIV/0!').
?test(sheet1_G26, "/GTE/", "G26", '#N/A').
?test(sheet1_H26, "/GTE/", "H26", '#NAME?').
?test(sheet1_I26, "/GTE/", "I26", 'NULL!').
?test(sheet1_J26, "/GTE/", "J26", '#NUM!').
?test(sheet1_K26, "/GTE/", "K26", '#REF!').
?test(sheet1_L26, "/GTE/", "L26", '#VALUE!').
?test(sheet1_M26, "/GTE/", "M26", true).
?test(sheet1_N26, "/GTE/", "N26", true).
?test(sheet1_O26, "/GTE/", "O26", true).
?test(sheet1_P26, "/GTE/", "P26", true).
?test(sheet1_Q26, "/GTE/", "Q26", true).
?test(sheet1_R26, "/GTE/", "R26", true).
?test(sheet1_S26, "/GTE/", "S26", true).
?test(sheet1_T26, "/GTE/", "T26", true).
?test(sheet1_U26, "/GTE/", "U26", true).
?test(sheet1_V26, "/GTE/", "V26", true).
?test(sheet1_A27, "/GTE/", "A27", "Boolean").
?test(sheet1_C27, "/GTE/", "C27", true).
?test(sheet1_D27, "/GTE/", "D27", false).
?test(sheet1_E27, "/GTE/", "E27", true).
?test(sheet1_F27, "/GTE/", "F27", '#DIV/0!').
?test(sheet1_G27, "/GTE/", "G27", '#N/A').
?test(sheet1_H27, "/GTE/", "H27", '#NAME?').
?test(sheet1_I27, "/GTE/", "I27", 'NULL!').
?test(sheet1_J27, "/GTE/", "J27", '#NUM!').
?test(sheet1_K27, "/GTE/", "K27", '#REF!').
?test(sheet1_L27, "/GTE/", "L27", '#VALUE!').
?test(sheet1_M27, "/GTE/", "M27", true).
?test(sheet1_N27, "/GTE/", "N27", true).
?test(sheet1_O27, "/GTE/", "O27", true).
?test(sheet1_P27, "/GTE/", "P27", true).
?test(sheet1_Q27, "/GTE/", "Q27", true).
?test(sheet1_R27, "/GTE/", "R27", true).
?test(sheet1_S27, "/GTE/", "S27", true).
?test(sheet1_T27, "/GTE/", "T27", true).
?test(sheet1_U27, "/GTE/", "U27", true).
?test(sheet1_V27, "/GTE/", "V27", true).
?test(sheet1_A28, "/GTE/", "A28", "Error").
?test(sheet1_C28, "/GTE/", "C28", '#DIV/0!').
?test(sheet1_D28, "/GTE/", "D28", '#DIV/0!').
?test(sheet1_E28, "/GTE/", "E28", '#DIV/0!').
?test(sheet1_F28, "/GTE/", "F28", '#DIV/0!').
?test(sheet1_G28, "/GTE/", "G28", '#DIV/0!').
?test(sheet1_H28, "/GTE/", "H28", '#DIV/0!').
?test(sheet1_I28, "/GTE/", "I28", '#DIV/0!').
?test(sheet1_J28, "/GTE/", "J28", '#DIV/0!').
?test(sheet1_K28, "/GTE/", "K28", '#DIV/0!').
?test(sheet1_L28, "/GTE/", "L28", '#DIV/0!').
?test(sheet1_M28, "/GTE/", "M28", '#DIV/0!').
?test(sheet1_N28, "/GTE/", "N28", '#DIV/0!').
?test(sheet1_O28, "/GTE/", "O28", '#DIV/0!').
?test(sheet1_P28, "/GTE/", "P28", '#DIV/0!').
?test(sheet1_Q28, "/GTE/", "Q28", '#DIV/0!').
?test(sheet1_R28, "/GTE/", "R28", '#DIV/0!').
?test(sheet1_S28, "/GTE/", "S28", '#DIV/0!').
?test(sheet1_T28, "/GTE/", "T28", '#DIV/0!').
?test(sheet1_U28, "/GTE/", "U28", '#DIV/0!').
?test(sheet1_V28, "/GTE/", "V28", '#DIV/0!').
?test(sheet1_A29, "/GTE/", "A29", "Error").
?test(sheet1_C29, "/GTE/", "C29", '#N/A').
?test(sheet1_D29, "/GTE/", "D29", '#N/A').
?test(sheet1_E29, "/GTE/", "E29", '#N/A').
?test(sheet1_F29, "/GTE/", "F29", '#N/A').
?test(sheet1_G29, "/GTE/", "G29", '#N/A').
?test(sheet1_H29, "/GTE/", "H29", '#N/A').
?test(sheet1_I29, "/GTE/", "I29", '#N/A').
?test(sheet1_J29, "/GTE/", "J29", '#N/A').
?test(sheet1_K29, "/GTE/", "K29", '#N/A').
?test(sheet1_L29, "/GTE/", "L29", '#N/A').
?test(sheet1_M29, "/GTE/", "M29", '#N/A').
?test(sheet1_N29, "/GTE/", "N29", '#N/A').
?test(sheet1_O29, "/GTE/", "O29", '#N/A').
?test(sheet1_P29, "/GTE/", "P29", '#N/A').
?test(sheet1_Q29, "/GTE/", "Q29", '#N/A').
?test(sheet1_R29, "/GTE/", "R29", '#N/A').
?test(sheet1_S29, "/GTE/", "S29", '#N/A').
?test(sheet1_T29, "/GTE/", "T29", '#N/A').
?test(sheet1_U29, "/GTE/", "U29", '#N/A').
?test(sheet1_V29, "/GTE/", "V29", '#N/A').
?test(sheet1_A30, "/GTE/", "A30", "Error").
?test(sheet1_C30, "/GTE/", "C30", '#NAME?').
?test(sheet1_D30, "/GTE/", "D30", '#NAME?').
?test(sheet1_E30, "/GTE/", "E30", '#NAME?').
?test(sheet1_F30, "/GTE/", "F30", '#NAME?').
?test(sheet1_G30, "/GTE/", "G30", '#NAME?').
?test(sheet1_H30, "/GTE/", "H30", '#NAME?').
?test(sheet1_I30, "/GTE/", "I30", '#NAME?').
?test(sheet1_J30, "/GTE/", "J30", '#NAME?').
?test(sheet1_K30, "/GTE/", "K30", '#NAME?').
?test(sheet1_L30, "/GTE/", "L30", '#NAME?').
?test(sheet1_M30, "/GTE/", "M30", '#NAME?').
?test(sheet1_N30, "/GTE/", "N30", '#NAME?').
?test(sheet1_O30, "/GTE/", "O30", '#NAME?').
?test(sheet1_P30, "/GTE/", "P30", '#NAME?').
?test(sheet1_Q30, "/GTE/", "Q30", '#NAME?').
?test(sheet1_R30, "/GTE/", "R30", '#NAME?').
?test(sheet1_S30, "/GTE/", "S30", '#NAME?').
?test(sheet1_T30, "/GTE/", "T30", '#NAME?').
?test(sheet1_U30, "/GTE/", "U30", '#NAME?').
?test(sheet1_V30, "/GTE/", "V30", '#NAME?').
?test(sheet1_A31, "/GTE/", "A31", "Error").
?test(sheet1_C31, "/GTE/", "C31", 'NULL!').
?test(sheet1_D31, "/GTE/", "D31", 'NULL!').
?test(sheet1_E31, "/GTE/", "E31", 'NULL!').
?test(sheet1_F31, "/GTE/", "F31", 'NULL!').
?test(sheet1_G31, "/GTE/", "G31", 'NULL!').
?test(sheet1_H31, "/GTE/", "H31", 'NULL!').
?test(sheet1_I31, "/GTE/", "I31", 'NULL!').
?test(sheet1_J31, "/GTE/", "J31", 'NULL!').
?test(sheet1_K31, "/GTE/", "K31", 'NULL!').
?test(sheet1_L31, "/GTE/", "L31", 'NULL!').
?test(sheet1_M31, "/GTE/", "M31", 'NULL!').
?test(sheet1_N31, "/GTE/", "N31", 'NULL!').
?test(sheet1_O31, "/GTE/", "O31", 'NULL!').
?test(sheet1_P31, "/GTE/", "P31", 'NULL!').
?test(sheet1_Q31, "/GTE/", "Q31", 'NULL!').
?test(sheet1_R31, "/GTE/", "R31", 'NULL!').
?test(sheet1_S31, "/GTE/", "S31", 'NULL!').
?test(sheet1_T31, "/GTE/", "T31", 'NULL!').
?test(sheet1_U31, "/GTE/", "U31", 'NULL!').
?test(sheet1_V31, "/GTE/", "V31", 'NULL!').
?test(sheet1_A32, "/GTE/", "A32", "Error").
?test(sheet1_C32, "/GTE/", "C32", '#NUM!').
?test(sheet1_D32, "/GTE/", "D32", '#NUM!').
?test(sheet1_E32, "/GTE/", "E32", '#NUM!').
?test(sheet1_F32, "/GTE/", "F32", '#NUM!').
?test(sheet1_G32, "/GTE/", "G32", '#NUM!').
?test(sheet1_H32, "/GTE/", "H32", '#NUM!').
?test(sheet1_I32, "/GTE/", "I32", '#NUM!').
?test(sheet1_J32, "/GTE/", "J32", '#NUM!').
?test(sheet1_K32, "/GTE/", "K32", '#NUM!').
?test(sheet1_L32, "/GTE/", "L32", '#NUM!').
?test(sheet1_M32, "/GTE/", "M32", '#NUM!').
?test(sheet1_N32, "/GTE/", "N32", '#NUM!').
?test(sheet1_O32, "/GTE/", "O32", '#NUM!').
?test(sheet1_P32, "/GTE/", "P32", '#NUM!').
?test(sheet1_Q32, "/GTE/", "Q32", '#NUM!').
?test(sheet1_R32, "/GTE/", "R32", '#NUM!').
?test(sheet1_S32, "/GTE/", "S32", '#NUM!').
?test(sheet1_T32, "/GTE/", "T32", '#NUM!').
?test(sheet1_U32, "/GTE/", "U32", '#NUM!').
?test(sheet1_V32, "/GTE/", "V32", '#NUM!').
?test(sheet1_A33, "/GTE/", "A33", "Error").
?test(sheet1_C33, "/GTE/", "C33", '#REF!').
?test(sheet1_D33, "/GTE/", "D33", '#REF!').
?test(sheet1_E33, "/GTE/", "E33", '#REF!').
?test(sheet1_F33, "/GTE/", "F33", '#REF!').
?test(sheet1_G33, "/GTE/", "G33", '#REF!').
?test(sheet1_H33, "/GTE/", "H33", '#REF!').
?test(sheet1_I33, "/GTE/", "I33", '#REF!').
?test(sheet1_J33, "/GTE/", "J33", '#REF!').
?test(sheet1_K33, "/GTE/", "K33", '#REF!').
?test(sheet1_L33, "/GTE/", "L33", '#REF!').
?test(sheet1_M33, "/GTE/", "M33", '#REF!').
?test(sheet1_N33, "/GTE/", "N33", '#REF!').
?test(sheet1_O33, "/GTE/", "O33", '#REF!').
?test(sheet1_P33, "/GTE/", "P33", '#REF!').
?test(sheet1_Q33, "/GTE/", "Q33", '#REF!').
?test(sheet1_R33, "/GTE/", "R33", '#REF!').
?test(sheet1_S33, "/GTE/", "S33", '#REF!').
?test(sheet1_T33, "/GTE/", "T33", '#REF!').
?test(sheet1_U33, "/GTE/", "U33", '#REF!').
?test(sheet1_V33, "/GTE/", "V33", '#REF!').
?test(sheet1_A34, "/GTE/", "A34", "Error").
?test(sheet1_C34, "/GTE/", "C34", '#VALUE!').
?test(sheet1_D34, "/GTE/", "D34", '#VALUE!').
?test(sheet1_E34, "/GTE/", "E34", '#VALUE!').
?test(sheet1_F34, "/GTE/", "F34", '#VALUE!').
?test(sheet1_G34, "/GTE/", "G34", '#VALUE!').
?test(sheet1_H34, "/GTE/", "H34", '#VALUE!').
?test(sheet1_I34, "/GTE/", "I34", '#VALUE!').
?test(sheet1_J34, "/GTE/", "J34", '#VALUE!').
?test(sheet1_K34, "/GTE/", "K34", '#VALUE!').
?test(sheet1_L34, "/GTE/", "L34", '#VALUE!').
?test(sheet1_M34, "/GTE/", "M34", '#VALUE!').
?test(sheet1_N34, "/GTE/", "N34", '#VALUE!').
?test(sheet1_O34, "/GTE/", "O34", '#VALUE!').
?test(sheet1_P34, "/GTE/", "P34", '#VALUE!').
?test(sheet1_Q34, "/GTE/", "Q34", '#VALUE!').
?test(sheet1_R34, "/GTE/", "R34", '#VALUE!').
?test(sheet1_S34, "/GTE/", "S34", '#VALUE!').
?test(sheet1_T34, "/GTE/", "T34", '#VALUE!').
?test(sheet1_U34, "/GTE/", "U34", '#VALUE!').
?test(sheet1_V34, "/GTE/", "V34", '#VALUE!').
?test(sheet1_A35, "/GTE/", "A35", "String").
?test(sheet1_C35, "/GTE/", "C35", true).
?test(sheet1_D35, "/GTE/", "D35", false).
?test(sheet1_E35, "/GTE/", "E35", false).
?test(sheet1_F35, "/GTE/", "F35", '#DIV/0!').
?test(sheet1_G35, "/GTE/", "G35", '#N/A').
?test(sheet1_H35, "/GTE/", "H35", '#NAME?').
?test(sheet1_I35, "/GTE/", "I35", 'NULL!').
?test(sheet1_J35, "/GTE/", "J35", '#NUM!').
?test(sheet1_K35, "/GTE/", "K35", '#REF!').
?test(sheet1_L35, "/GTE/", "L35", '#VALUE!').
?test(sheet1_M35, "/GTE/", "M35", true).
?test(sheet1_N35, "/GTE/", "N35", true).
?test(sheet1_O35, "/GTE/", "O35", true).
?test(sheet1_P35, "/GTE/", "P35", true).
?test(sheet1_Q35, "/GTE/", "Q35", true).
?test(sheet1_R35, "/GTE/", "R35", true).
?test(sheet1_S35, "/GTE/", "S35", true).
?test(sheet1_T35, "/GTE/", "T35", true).
?test(sheet1_U35, "/GTE/", "U35", true).
?test(sheet1_V35, "/GTE/", "V35", true).
?test(sheet1_A36, "/GTE/", "A36", "String").
?test(sheet1_C36, "/GTE/", "C36", true).
?test(sheet1_D36, "/GTE/", "D36", false).
?test(sheet1_E36, "/GTE/", "E36", false).
?test(sheet1_F36, "/GTE/", "F36", '#DIV/0!').
?test(sheet1_G36, "/GTE/", "G36", '#N/A').
?test(sheet1_H36, "/GTE/", "H36", '#NAME?').
?test(sheet1_I36, "/GTE/", "I36", 'NULL!').
?test(sheet1_J36, "/GTE/", "J36", '#NUM!').
?test(sheet1_K36, "/GTE/", "K36", '#REF!').
?test(sheet1_L36, "/GTE/", "L36", '#VALUE!').
?test(sheet1_M36, "/GTE/", "M36", false).
?test(sheet1_N36, "/GTE/", "N36", true).
?test(sheet1_O36, "/GTE/", "O36", true).
?test(sheet1_P36, "/GTE/", "P36", true).
?test(sheet1_Q36, "/GTE/", "Q36", true).
?test(sheet1_R36, "/GTE/", "R36", true).
?test(sheet1_S36, "/GTE/", "S36", true).
?test(sheet1_T36, "/GTE/", "T36", true).
?test(sheet1_U36, "/GTE/", "U36", true).
?test(sheet1_V36, "/GTE/", "V36", true).
?test(sheet1_A37, "/GTE/", "A37", "String").
?test(sheet1_C37, "/GTE/", "C37", true).
?test(sheet1_D37, "/GTE/", "D37", false).
?test(sheet1_E37, "/GTE/", "E37", false).
?test(sheet1_F37, "/GTE/", "F37", '#DIV/0!').
?test(sheet1_G37, "/GTE/", "G37", '#N/A').
?test(sheet1_H37, "/GTE/", "H37", '#NAME?').
?test(sheet1_I37, "/GTE/", "I37", 'NULL!').
?test(sheet1_J37, "/GTE/", "J37", '#NUM!').
?test(sheet1_K37, "/GTE/", "K37", '#REF!').
?test(sheet1_L37, "/GTE/", "L37", '#VALUE!').
?test(sheet1_M37, "/GTE/", "M37", false).
?test(sheet1_N37, "/GTE/", "N37", false).
?test(sheet1_O37, "/GTE/", "O37", true).
?test(sheet1_P37, "/GTE/", "P37", true).
?test(sheet1_Q37, "/GTE/", "Q37", true).
?test(sheet1_R37, "/GTE/", "R37", true).
?test(sheet1_S37, "/GTE/", "S37", true).
?test(sheet1_T37, "/GTE/", "T37", true).
?test(sheet1_U37, "/GTE/", "U37", true).
?test(sheet1_V37, "/GTE/", "V37", true).
?test(sheet1_A38, "/GTE/", "A38", "Str Num").
?test(sheet1_C38, "/GTE/", "C38", true).
?test(sheet1_D38, "/GTE/", "D38", false).
?test(sheet1_E38, "/GTE/", "E38", false).
?test(sheet1_F38, "/GTE/", "F38", '#DIV/0!').
?test(sheet1_G38, "/GTE/", "G38", '#N/A').
?test(sheet1_H38, "/GTE/", "H38", '#NAME?').
?test(sheet1_I38, "/GTE/", "I38", 'NULL!').
?test(sheet1_J38, "/GTE/", "J38", '#NUM!').
?test(sheet1_K38, "/GTE/", "K38", '#REF!').
?test(sheet1_L38, "/GTE/", "L38", '#VALUE!').
?test(sheet1_M38, "/GTE/", "M38", false).
?test(sheet1_N38, "/GTE/", "N38", false).
?test(sheet1_O38, "/GTE/", "O38", false).
?test(sheet1_P38, "/GTE/", "P38", true).
?test(sheet1_Q38, "/GTE/", "Q38", false).
?test(sheet1_R38, "/GTE/", "R38", true).
?test(sheet1_S38, "/GTE/", "S38", true).
?test(sheet1_T38, "/GTE/", "T38", true).
?test(sheet1_U38, "/GTE/", "U38", true).
?test(sheet1_V38, "/GTE/", "V38", true).
?test(sheet1_A39, "/GTE/", "A39", "Str Num").
?test(sheet1_C39, "/GTE/", "C39", true).
?test(sheet1_D39, "/GTE/", "D39", false).
?test(sheet1_E39, "/GTE/", "E39", false).
?test(sheet1_F39, "/GTE/", "F39", '#DIV/0!').
?test(sheet1_G39, "/GTE/", "G39", '#N/A').
?test(sheet1_H39, "/GTE/", "H39", '#NAME?').
?test(sheet1_I39, "/GTE/", "I39", 'NULL!').
?test(sheet1_J39, "/GTE/", "J39", '#NUM!').
?test(sheet1_K39, "/GTE/", "K39", '#REF!').
?test(sheet1_L39, "/GTE/", "L39", '#VALUE!').
?test(sheet1_M39, "/GTE/", "M39", false).
?test(sheet1_N39, "/GTE/", "N39", false).
?test(sheet1_O39, "/GTE/", "O39", false).
?test(sheet1_P39, "/GTE/", "P39", true).
?test(sheet1_Q39, "/GTE/", "Q39", true).
?test(sheet1_R39, "/GTE/", "R39", true).
?test(sheet1_S39, "/GTE/", "S39", true).
?test(sheet1_T39, "/GTE/", "T39", true).
?test(sheet1_U39, "/GTE/", "U39", true).
?test(sheet1_V39, "/GTE/", "V39", true).
?test(sheet1_A40, "/GTE/", "A40", "Integer").
?test(sheet1_C40, "/GTE/", "C40", true).
?test(sheet1_D40, "/GTE/", "D40", false).
?test(sheet1_E40, "/GTE/", "E40", false).
?test(sheet1_F40, "/GTE/", "F40", '#DIV/0!').
?test(sheet1_G40, "/GTE/", "G40", '#N/A').
?test(sheet1_H40, "/GTE/", "H40", '#NAME?').
?test(sheet1_I40, "/GTE/", "I40", 'NULL!').
?test(sheet1_J40, "/GTE/", "J40", '#NUM!').
?test(sheet1_K40, "/GTE/", "K40", '#REF!').
?test(sheet1_L40, "/GTE/", "L40", '#VALUE!').
?test(sheet1_M40, "/GTE/", "M40", false).
?test(sheet1_N40, "/GTE/", "N40", false).
?test(sheet1_O40, "/GTE/", "O40", false).
?test(sheet1_P40, "/GTE/", "P40", false).
?test(sheet1_Q40, "/GTE/", "Q40", false).
?test(sheet1_R40, "/GTE/", "R40", true).
?test(sheet1_S40, "/GTE/", "S40", false).
?test(sheet1_T40, "/GTE/", "T40", true).
?test(sheet1_U40, "/GTE/", "U40", true).
?test(sheet1_V40, "/GTE/", "V40", false).
?test(sheet1_A41, "/GTE/", "A41", "Integer").
?test(sheet1_C41, "/GTE/", "C41", true).
?test(sheet1_D41, "/GTE/", "D41", false).
?test(sheet1_E41, "/GTE/", "E41", false).
?test(sheet1_F41, "/GTE/", "F41", '#DIV/0!').
?test(sheet1_G41, "/GTE/", "G41", '#N/A').
?test(sheet1_H41, "/GTE/", "H41", '#NAME?').
?test(sheet1_I41, "/GTE/", "I41", 'NULL!').
?test(sheet1_J41, "/GTE/", "J41", '#NUM!').
?test(sheet1_K41, "/GTE/", "K41", '#REF!').
?test(sheet1_L41, "/GTE/", "L41", '#VALUE!').
?test(sheet1_M41, "/GTE/", "M41", false).
?test(sheet1_N41, "/GTE/", "N41", false).
?test(sheet1_O41, "/GTE/", "O41", false).
?test(sheet1_P41, "/GTE/", "P41", false).
?test(sheet1_Q41, "/GTE/", "Q41", false).
?test(sheet1_R41, "/GTE/", "R41", true).
?test(sheet1_S41, "/GTE/", "S41", true).
?test(sheet1_T41, "/GTE/", "T41", true).
?test(sheet1_U41, "/GTE/", "U41", true).
?test(sheet1_V41, "/GTE/", "V41", false).
?test(sheet1_A42, "/GTE/", "A42", "Zero").
?test(sheet1_C42, "/GTE/", "C42", true).
?test(sheet1_D42, "/GTE/", "D42", false).
?test(sheet1_E42, "/GTE/", "E42", false).
?test(sheet1_F42, "/GTE/", "F42", '#DIV/0!').
?test(sheet1_G42, "/GTE/", "G42", '#N/A').
?test(sheet1_H42, "/GTE/", "H42", '#NAME?').
?test(sheet1_I42, "/GTE/", "I42", 'NULL!').
?test(sheet1_J42, "/GTE/", "J42", '#NUM!').
?test(sheet1_K42, "/GTE/", "K42", '#REF!').
?test(sheet1_L42, "/GTE/", "L42", '#VALUE!').
?test(sheet1_M42, "/GTE/", "M42", false).
?test(sheet1_N42, "/GTE/", "N42", false).
?test(sheet1_O42, "/GTE/", "O42", false).
?test(sheet1_P42, "/GTE/", "P42", false).
?test(sheet1_Q42, "/GTE/", "Q42", false).
?test(sheet1_R42, "/GTE/", "R42", false).
?test(sheet1_S42, "/GTE/", "S42", false).
?test(sheet1_T42, "/GTE/", "T42", true).
?test(sheet1_U42, "/GTE/", "U42", false).
?test(sheet1_V42, "/GTE/", "V42", false).
?test(sheet1_A43, "/GTE/", "A43", "Float").
?test(sheet1_C43, "/GTE/", "C43", true).
?test(sheet1_D43, "/GTE/", "D43", false).
?test(sheet1_E43, "/GTE/", "E43", false).
?test(sheet1_F43, "/GTE/", "F43", '#DIV/0!').
?test(sheet1_G43, "/GTE/", "G43", '#N/A').
?test(sheet1_H43, "/GTE/", "H43", '#NAME?').
?test(sheet1_I43, "/GTE/", "I43", 'NULL!').
?test(sheet1_J43, "/GTE/", "J43", '#NUM!').
?test(sheet1_K43, "/GTE/", "K43", '#REF!').
?test(sheet1_L43, "/GTE/", "L43", '#VALUE!').
?test(sheet1_M43, "/GTE/", "M43", false).
?test(sheet1_N43, "/GTE/", "N43", false).
?test(sheet1_O43, "/GTE/", "O43", false).
?test(sheet1_P43, "/GTE/", "P43", false).
?test(sheet1_Q43, "/GTE/", "Q43", false).
?test(sheet1_R43, "/GTE/", "R43", false).
?test(sheet1_S43, "/GTE/", "S43", false).
?test(sheet1_T43, "/GTE/", "T43", true).
?test(sheet1_U43, "/GTE/", "U43", true).
?test(sheet1_V43, "/GTE/", "V43", false).
?test(sheet1_A44, "/GTE/", "A44", "Float").
?test(sheet1_C44, "/GTE/", "C44", true).
?test(sheet1_D44, "/GTE/", "D44", false).
?test(sheet1_E44, "/GTE/", "E44", false).
?test(sheet1_F44, "/GTE/", "F44", '#DIV/0!').
?test(sheet1_G44, "/GTE/", "G44", '#N/A').
?test(sheet1_H44, "/GTE/", "H44", '#NAME?').
?test(sheet1_I44, "/GTE/", "I44", 'NULL!').
?test(sheet1_J44, "/GTE/", "J44", '#NUM!').
?test(sheet1_K44, "/GTE/", "K44", '#REF!').
?test(sheet1_L44, "/GTE/", "L44", '#VALUE!').
?test(sheet1_M44, "/GTE/", "M44", false).
?test(sheet1_N44, "/GTE/", "N44", false).
?test(sheet1_O44, "/GTE/", "O44", false).
?test(sheet1_P44, "/GTE/", "P44", false).
?test(sheet1_Q44, "/GTE/", "Q44", false).
?test(sheet1_R44, "/GTE/", "R44", true).
?test(sheet1_S44, "/GTE/", "S44", true).
?test(sheet1_T44, "/GTE/", "T44", true).
?test(sheet1_U44, "/GTE/", "U44", true).
?test(sheet1_V44, "/GTE/", "V44", true).
?test(sheet1_A47, "/GTE/", "A47", 400.0).
?test(sheet1_C47, "/GTE/", "C47", 1.0).
?test(sheet1_D47, "/GTE/", "D47", 1.0).
?test(sheet1_E47, "/GTE/", "E47", 1.0).
?test(sheet1_F47, "/GTE/", "F47", 1.0).
?test(sheet1_G47, "/GTE/", "G47", 1.0).
?test(sheet1_H47, "/GTE/", "H47", 1.0).
?test(sheet1_I47, "/GTE/", "I47", 1.0).
?test(sheet1_J47, "/GTE/", "J47", 1.0).
?test(sheet1_K47, "/GTE/", "K47", 1.0).
?test(sheet1_L47, "/GTE/", "L47", 1.0).
?test(sheet1_M47, "/GTE/", "M47", 1.0).
?test(sheet1_N47, "/GTE/", "N47", 1.0).
?test(sheet1_O47, "/GTE/", "O47", 1.0).
?test(sheet1_P47, "/GTE/", "P47", 1.0).
?test(sheet1_Q47, "/GTE/", "Q47", 1.0).
?test(sheet1_R47, "/GTE/", "R47", 1.0).
?test(sheet1_S47, "/GTE/", "S47", 1.0).
?test(sheet1_T47, "/GTE/", "T47", 1.0).
?test(sheet1_U47, "/GTE/", "U47", 1.0).
?test(sheet1_V47, "/GTE/", "V47", 1.0).
?test(sheet1_A48, "/GTE/", "A48", "Success").
?test(sheet1_C48, "/GTE/", "C48", 1.0).
?test(sheet1_D48, "/GTE/", "D48", 1.0).
?test(sheet1_E48, "/GTE/", "E48", 1.0).
?test(sheet1_F48, "/GTE/", "F48", 1.0).
?test(sheet1_G48, "/GTE/", "G48", 1.0).
?test(sheet1_H48, "/GTE/", "H48", 1.0).
?test(sheet1_I48, "/GTE/", "I48", 1.0).
?test(sheet1_J48, "/GTE/", "J48", 1.0).
?test(sheet1_K48, "/GTE/", "K48", 1.0).
?test(sheet1_L48, "/GTE/", "L48", 1.0).
?test(sheet1_M48, "/GTE/", "M48", 1.0).
?test(sheet1_N48, "/GTE/", "N48", 1.0).
?test(sheet1_O48, "/GTE/", "O48", 1.0).
?test(sheet1_P48, "/GTE/", "P48", 1.0).
?test(sheet1_Q48, "/GTE/", "Q48", 1.0).
?test(sheet1_R48, "/GTE/", "R48", 1.0).
?test(sheet1_S48, "/GTE/", "S48", 1.0).
?test(sheet1_T48, "/GTE/", "T48", 1.0).
?test(sheet1_U48, "/GTE/", "U48", 1.0).
?test(sheet1_V48, "/GTE/", "V48", 1.0).
?test(sheet1_C49, "/GTE/", "C49", 1.0).
?test(sheet1_D49, "/GTE/", "D49", 1.0).
?test(sheet1_E49, "/GTE/", "E49", 1.0).
?test(sheet1_F49, "/GTE/", "F49", 1.0).
?test(sheet1_G49, "/GTE/", "G49", 1.0).
?test(sheet1_H49, "/GTE/", "H49", 1.0).
?test(sheet1_I49, "/GTE/", "I49", 1.0).
?test(sheet1_J49, "/GTE/", "J49", 1.0).
?test(sheet1_K49, "/GTE/", "K49", 1.0).
?test(sheet1_L49, "/GTE/", "L49", 1.0).
?test(sheet1_M49, "/GTE/", "M49", 1.0).
?test(sheet1_N49, "/GTE/", "N49", 1.0).
?test(sheet1_O49, "/GTE/", "O49", 1.0).
?test(sheet1_P49, "/GTE/", "P49", 1.0).
?test(sheet1_Q49, "/GTE/", "Q49", 1.0).
?test(sheet1_R49, "/GTE/", "R49", 1.0).
?test(sheet1_S49, "/GTE/", "S49", 1.0).
?test(sheet1_T49, "/GTE/", "T49", 1.0).
?test(sheet1_U49, "/GTE/", "U49", 1.0).
?test(sheet1_V49, "/GTE/", "V49", 1.0).
?test(sheet1_C50, "/GTE/", "C50", 1.0).
?test(sheet1_D50, "/GTE/", "D50", 1.0).
?test(sheet1_E50, "/GTE/", "E50", 1.0).
?test(sheet1_F50, "/GTE/", "F50", 1.0).
?test(sheet1_G50, "/GTE/", "G50", 1.0).
?test(sheet1_H50, "/GTE/", "H50", 1.0).
?test(sheet1_I50, "/GTE/", "I50", 1.0).
?test(sheet1_J50, "/GTE/", "J50", 1.0).
?test(sheet1_K50, "/GTE/", "K50", 1.0).
?test(sheet1_L50, "/GTE/", "L50", 1.0).
?test(sheet1_M50, "/GTE/", "M50", 1.0).
?test(sheet1_N50, "/GTE/", "N50", 1.0).
?test(sheet1_O50, "/GTE/", "O50", 1.0).
?test(sheet1_P50, "/GTE/", "P50", 1.0).
?test(sheet1_Q50, "/GTE/", "Q50", 1.0).
?test(sheet1_R50, "/GTE/", "R50", 1.0).
?test(sheet1_S50, "/GTE/", "S50", 1.0).
?test(sheet1_T50, "/GTE/", "T50", 1.0).
?test(sheet1_U50, "/GTE/", "U50", 1.0).
?test(sheet1_V50, "/GTE/", "V50", 1.0).
?test(sheet1_C51, "/GTE/", "C51", 1.0).
?test(sheet1_D51, "/GTE/", "D51", 1.0).
?test(sheet1_E51, "/GTE/", "E51", 1.0).
?test(sheet1_F51, "/GTE/", "F51", 1.0).
?test(sheet1_G51, "/GTE/", "G51", 1.0).
?test(sheet1_H51, "/GTE/", "H51", 1.0).
?test(sheet1_I51, "/GTE/", "I51", 1.0).
?test(sheet1_J51, "/GTE/", "J51", 1.0).
?test(sheet1_K51, "/GTE/", "K51", 1.0).
?test(sheet1_L51, "/GTE/", "L51", 1.0).
?test(sheet1_M51, "/GTE/", "M51", 1.0).
?test(sheet1_N51, "/GTE/", "N51", 1.0).
?test(sheet1_O51, "/GTE/", "O51", 1.0).
?test(sheet1_P51, "/GTE/", "P51", 1.0).
?test(sheet1_Q51, "/GTE/", "Q51", 1.0).
?test(sheet1_R51, "/GTE/", "R51", 1.0).
?test(sheet1_S51, "/GTE/", "S51", 1.0).
?test(sheet1_T51, "/GTE/", "T51", 1.0).
?test(sheet1_U51, "/GTE/", "U51", 1.0).
?test(sheet1_V51, "/GTE/", "V51", 1.0).
?test(sheet1_C52, "/GTE/", "C52", 1.0).
?test(sheet1_D52, "/GTE/", "D52", 1.0).
?test(sheet1_E52, "/GTE/", "E52", 1.0).
?test(sheet1_F52, "/GTE/", "F52", 1.0).
?test(sheet1_G52, "/GTE/", "G52", 1.0).
?test(sheet1_H52, "/GTE/", "H52", 1.0).
?test(sheet1_I52, "/GTE/", "I52", 1.0).
?test(sheet1_J52, "/GTE/", "J52", 1.0).
?test(sheet1_K52, "/GTE/", "K52", 1.0).
?test(sheet1_L52, "/GTE/", "L52", 1.0).
?test(sheet1_M52, "/GTE/", "M52", 1.0).
?test(sheet1_N52, "/GTE/", "N52", 1.0).
?test(sheet1_O52, "/GTE/", "O52", 1.0).
?test(sheet1_P52, "/GTE/", "P52", 1.0).
?test(sheet1_Q52, "/GTE/", "Q52", 1.0).
?test(sheet1_R52, "/GTE/", "R52", 1.0).
?test(sheet1_S52, "/GTE/", "S52", 1.0).
?test(sheet1_T52, "/GTE/", "T52", 1.0).
?test(sheet1_U52, "/GTE/", "U52", 1.0).
?test(sheet1_V52, "/GTE/", "V52", 1.0).
?test(sheet1_C53, "/GTE/", "C53", 1.0).
?test(sheet1_D53, "/GTE/", "D53", 1.0).
?test(sheet1_E53, "/GTE/", "E53", 1.0).
?test(sheet1_F53, "/GTE/", "F53", 1.0).
?test(sheet1_G53, "/GTE/", "G53", 1.0).
?test(sheet1_H53, "/GTE/", "H53", 1.0).
?test(sheet1_I53, "/GTE/", "I53", 1.0).
?test(sheet1_J53, "/GTE/", "J53", 1.0).
?test(sheet1_K53, "/GTE/", "K53", 1.0).
?test(sheet1_L53, "/GTE/", "L53", 1.0).
?test(sheet1_M53, "/GTE/", "M53", 1.0).
?test(sheet1_N53, "/GTE/", "N53", 1.0).
?test(sheet1_O53, "/GTE/", "O53", 1.0).
?test(sheet1_P53, "/GTE/", "P53", 1.0).
?test(sheet1_Q53, "/GTE/", "Q53", 1.0).
?test(sheet1_R53, "/GTE/", "R53", 1.0).
?test(sheet1_S53, "/GTE/", "S53", 1.0).
?test(sheet1_T53, "/GTE/", "T53", 1.0).
?test(sheet1_U53, "/GTE/", "U53", 1.0).
?test(sheet1_V53, "/GTE/", "V53", 1.0).
?test(sheet1_C54, "/GTE/", "C54", 1.0).
?test(sheet1_D54, "/GTE/", "D54", 1.0).
?test(sheet1_E54, "/GTE/", "E54", 1.0).
?test(sheet1_F54, "/GTE/", "F54", 1.0).
?test(sheet1_G54, "/GTE/", "G54", 1.0).
?test(sheet1_H54, "/GTE/", "H54", 1.0).
?test(sheet1_I54, "/GTE/", "I54", 1.0).
?test(sheet1_J54, "/GTE/", "J54", 1.0).
?test(sheet1_K54, "/GTE/", "K54", 1.0).
?test(sheet1_L54, "/GTE/", "L54", 1.0).
?test(sheet1_M54, "/GTE/", "M54", 1.0).
?test(sheet1_N54, "/GTE/", "N54", 1.0).
?test(sheet1_O54, "/GTE/", "O54", 1.0).
?test(sheet1_P54, "/GTE/", "P54", 1.0).
?test(sheet1_Q54, "/GTE/", "Q54", 1.0).
?test(sheet1_R54, "/GTE/", "R54", 1.0).
?test(sheet1_S54, "/GTE/", "S54", 1.0).
?test(sheet1_T54, "/GTE/", "T54", 1.0).
?test(sheet1_U54, "/GTE/", "U54", 1.0).
?test(sheet1_V54, "/GTE/", "V54", 1.0).
?test(sheet1_C55, "/GTE/", "C55", 1.0).
?test(sheet1_D55, "/GTE/", "D55", 1.0).
?test(sheet1_E55, "/GTE/", "E55", 1.0).
?test(sheet1_F55, "/GTE/", "F55", 1.0).
?test(sheet1_G55, "/GTE/", "G55", 1.0).
?test(sheet1_H55, "/GTE/", "H55", 1.0).
?test(sheet1_I55, "/GTE/", "I55", 1.0).
?test(sheet1_J55, "/GTE/", "J55", 1.0).
?test(sheet1_K55, "/GTE/", "K55", 1.0).
?test(sheet1_L55, "/GTE/", "L55", 1.0).
?test(sheet1_M55, "/GTE/", "M55", 1.0).
?test(sheet1_N55, "/GTE/", "N55", 1.0).
?test(sheet1_O55, "/GTE/", "O55", 1.0).
?test(sheet1_P55, "/GTE/", "P55", 1.0).
?test(sheet1_Q55, "/GTE/", "Q55", 1.0).
?test(sheet1_R55, "/GTE/", "R55", 1.0).
?test(sheet1_S55, "/GTE/", "S55", 1.0).
?test(sheet1_T55, "/GTE/", "T55", 1.0).
?test(sheet1_U55, "/GTE/", "U55", 1.0).
?test(sheet1_V55, "/GTE/", "V55", 1.0).
?test(sheet1_C56, "/GTE/", "C56", 1.0).
?test(sheet1_D56, "/GTE/", "D56", 1.0).
?test(sheet1_E56, "/GTE/", "E56", 1.0).
?test(sheet1_F56, "/GTE/", "F56", 1.0).
?test(sheet1_G56, "/GTE/", "G56", 1.0).
?test(sheet1_H56, "/GTE/", "H56", 1.0).
?test(sheet1_I56, "/GTE/", "I56", 1.0).
?test(sheet1_J56, "/GTE/", "J56", 1.0).
?test(sheet1_K56, "/GTE/", "K56", 1.0).
?test(sheet1_L56, "/GTE/", "L56", 1.0).
?test(sheet1_M56, "/GTE/", "M56", 1.0).
?test(sheet1_N56, "/GTE/", "N56", 1.0).
?test(sheet1_O56, "/GTE/", "O56", 1.0).
?test(sheet1_P56, "/GTE/", "P56", 1.0).
?test(sheet1_Q56, "/GTE/", "Q56", 1.0).
?test(sheet1_R56, "/GTE/", "R56", 1.0).
?test(sheet1_S56, "/GTE/", "S56", 1.0).
?test(sheet1_T56, "/GTE/", "T56", 1.0).
?test(sheet1_U56, "/GTE/", "U56", 1.0).
?test(sheet1_V56, "/GTE/", "V56", 1.0).
?test(sheet1_C57, "/GTE/", "C57", 1.0).
?test(sheet1_D57, "/GTE/", "D57", 1.0).
?test(sheet1_E57, "/GTE/", "E57", 1.0).
?test(sheet1_F57, "/GTE/", "F57", 1.0).
?test(sheet1_G57, "/GTE/", "G57", 1.0).
?test(sheet1_H57, "/GTE/", "H57", 1.0).
?test(sheet1_I57, "/GTE/", "I57", 1.0).
?test(sheet1_J57, "/GTE/", "J57", 1.0).
?test(sheet1_K57, "/GTE/", "K57", 1.0).
?test(sheet1_L57, "/GTE/", "L57", 1.0).
?test(sheet1_M57, "/GTE/", "M57", 1.0).
?test(sheet1_N57, "/GTE/", "N57", 1.0).
?test(sheet1_O57, "/GTE/", "O57", 1.0).
?test(sheet1_P57, "/GTE/", "P57", 1.0).
?test(sheet1_Q57, "/GTE/", "Q57", 1.0).
?test(sheet1_R57, "/GTE/", "R57", 1.0).
?test(sheet1_S57, "/GTE/", "S57", 1.0).
?test(sheet1_T57, "/GTE/", "T57", 1.0).
?test(sheet1_U57, "/GTE/", "U57", 1.0).
?test(sheet1_V57, "/GTE/", "V57", 1.0).
?test(sheet1_C58, "/GTE/", "C58", 1.0).
?test(sheet1_D58, "/GTE/", "D58", 1.0).
?test(sheet1_E58, "/GTE/", "E58", 1.0).
?test(sheet1_F58, "/GTE/", "F58", 1.0).
?test(sheet1_G58, "/GTE/", "G58", 1.0).
?test(sheet1_H58, "/GTE/", "H58", 1.0).
?test(sheet1_I58, "/GTE/", "I58", 1.0).
?test(sheet1_J58, "/GTE/", "J58", 1.0).
?test(sheet1_K58, "/GTE/", "K58", 1.0).
?test(sheet1_L58, "/GTE/", "L58", 1.0).
?test(sheet1_M58, "/GTE/", "M58", 1.0).
?test(sheet1_N58, "/GTE/", "N58", 1.0).
?test(sheet1_O58, "/GTE/", "O58", 1.0).
?test(sheet1_P58, "/GTE/", "P58", 1.0).
?test(sheet1_Q58, "/GTE/", "Q58", 1.0).
?test(sheet1_R58, "/GTE/", "R58", 1.0).
?test(sheet1_S58, "/GTE/", "S58", 1.0).
?test(sheet1_T58, "/GTE/", "T58", 1.0).
?test(sheet1_U58, "/GTE/", "U58", 1.0).
?test(sheet1_V58, "/GTE/", "V58", 1.0).
?test(sheet1_C59, "/GTE/", "C59", 1.0).
?test(sheet1_D59, "/GTE/", "D59", 1.0).
?test(sheet1_E59, "/GTE/", "E59", 1.0).
?test(sheet1_F59, "/GTE/", "F59", 1.0).
?test(sheet1_G59, "/GTE/", "G59", 1.0).
?test(sheet1_H59, "/GTE/", "H59", 1.0).
?test(sheet1_I59, "/GTE/", "I59", 1.0).
?test(sheet1_J59, "/GTE/", "J59", 1.0).
?test(sheet1_K59, "/GTE/", "K59", 1.0).
?test(sheet1_L59, "/GTE/", "L59", 1.0).
?test(sheet1_M59, "/GTE/", "M59", 1.0).
?test(sheet1_N59, "/GTE/", "N59", 1.0).
?test(sheet1_O59, "/GTE/", "O59", 1.0).
?test(sheet1_P59, "/GTE/", "P59", 1.0).
?test(sheet1_Q59, "/GTE/", "Q59", 1.0).
?test(sheet1_R59, "/GTE/", "R59", 1.0).
?test(sheet1_S59, "/GTE/", "S59", 1.0).
?test(sheet1_T59, "/GTE/", "T59", 1.0).
?test(sheet1_U59, "/GTE/", "U59", 1.0).
?test(sheet1_V59, "/GTE/", "V59", 1.0).
?test(sheet1_C60, "/GTE/", "C60", 1.0).
?test(sheet1_D60, "/GTE/", "D60", 1.0).
?test(sheet1_E60, "/GTE/", "E60", 1.0).
?test(sheet1_F60, "/GTE/", "F60", 1.0).
?test(sheet1_G60, "/GTE/", "G60", 1.0).
?test(sheet1_H60, "/GTE/", "H60", 1.0).
?test(sheet1_I60, "/GTE/", "I60", 1.0).
?test(sheet1_J60, "/GTE/", "J60", 1.0).
?test(sheet1_K60, "/GTE/", "K60", 1.0).
?test(sheet1_L60, "/GTE/", "L60", 1.0).
?test(sheet1_M60, "/GTE/", "M60", 1.0).
?test(sheet1_N60, "/GTE/", "N60", 1.0).
?test(sheet1_O60, "/GTE/", "O60", 1.0).
?test(sheet1_P60, "/GTE/", "P60", 1.0).
?test(sheet1_Q60, "/GTE/", "Q60", 1.0).
?test(sheet1_R60, "/GTE/", "R60", 1.0).
?test(sheet1_S60, "/GTE/", "S60", 1.0).
?test(sheet1_T60, "/GTE/", "T60", 1.0).
?test(sheet1_U60, "/GTE/", "U60", 1.0).
?test(sheet1_V60, "/GTE/", "V60", 1.0).
?test(sheet1_C61, "/GTE/", "C61", 1.0).
?test(sheet1_D61, "/GTE/", "D61", 1.0).
?test(sheet1_E61, "/GTE/", "E61", 1.0).
?test(sheet1_F61, "/GTE/", "F61", 1.0).
?test(sheet1_G61, "/GTE/", "G61", 1.0).
?test(sheet1_H61, "/GTE/", "H61", 1.0).
?test(sheet1_I61, "/GTE/", "I61", 1.0).
?test(sheet1_J61, "/GTE/", "J61", 1.0).
?test(sheet1_K61, "/GTE/", "K61", 1.0).
?test(sheet1_L61, "/GTE/", "L61", 1.0).
?test(sheet1_M61, "/GTE/", "M61", 1.0).
?test(sheet1_N61, "/GTE/", "N61", 1.0).
?test(sheet1_O61, "/GTE/", "O61", 1.0).
?test(sheet1_P61, "/GTE/", "P61", 1.0).
?test(sheet1_Q61, "/GTE/", "Q61", 1.0).
?test(sheet1_R61, "/GTE/", "R61", 1.0).
?test(sheet1_S61, "/GTE/", "S61", 1.0).
?test(sheet1_T61, "/GTE/", "T61", 1.0).
?test(sheet1_U61, "/GTE/", "U61", 1.0).
?test(sheet1_V61, "/GTE/", "V61", 1.0).
?test(sheet1_C62, "/GTE/", "C62", 1.0).
?test(sheet1_D62, "/GTE/", "D62", 1.0).
?test(sheet1_E62, "/GTE/", "E62", 1.0).
?test(sheet1_F62, "/GTE/", "F62", 1.0).
?test(sheet1_G62, "/GTE/", "G62", 1.0).
?test(sheet1_H62, "/GTE/", "H62", 1.0).
?test(sheet1_I62, "/GTE/", "I62", 1.0).
?test(sheet1_J62, "/GTE/", "J62", 1.0).
?test(sheet1_K62, "/GTE/", "K62", 1.0).
?test(sheet1_L62, "/GTE/", "L62", 1.0).
?test(sheet1_M62, "/GTE/", "M62", 1.0).
?test(sheet1_N62, "/GTE/", "N62", 1.0).
?test(sheet1_O62, "/GTE/", "O62", 1.0).
?test(sheet1_P62, "/GTE/", "P62", 1.0).
?test(sheet1_Q62, "/GTE/", "Q62", 1.0).
?test(sheet1_R62, "/GTE/", "R62", 1.0).
?test(sheet1_S62, "/GTE/", "S62", 1.0).
?test(sheet1_T62, "/GTE/", "T62", 1.0).
?test(sheet1_U62, "/GTE/", "U62", 1.0).
?test(sheet1_V62, "/GTE/", "V62", 1.0).
?test(sheet1_C63, "/GTE/", "C63", 1.0).
?test(sheet1_D63, "/GTE/", "D63", 1.0).
?test(sheet1_E63, "/GTE/", "E63", 1.0).
?test(sheet1_F63, "/GTE/", "F63", 1.0).
?test(sheet1_G63, "/GTE/", "G63", 1.0).
?test(sheet1_H63, "/GTE/", "H63", 1.0).
?test(sheet1_I63, "/GTE/", "I63", 1.0).
?test(sheet1_J63, "/GTE/", "J63", 1.0).
?test(sheet1_K63, "/GTE/", "K63", 1.0).
?test(sheet1_L63, "/GTE/", "L63", 1.0).
?test(sheet1_M63, "/GTE/", "M63", 1.0).
?test(sheet1_N63, "/GTE/", "N63", 1.0).
?test(sheet1_O63, "/GTE/", "O63", 1.0).
?test(sheet1_P63, "/GTE/", "P63", 1.0).
?test(sheet1_Q63, "/GTE/", "Q63", 1.0).
?test(sheet1_R63, "/GTE/", "R63", 1.0).
?test(sheet1_S63, "/GTE/", "S63", 1.0).
?test(sheet1_T63, "/GTE/", "T63", 1.0).
?test(sheet1_U63, "/GTE/", "U63", 1.0).
?test(sheet1_V63, "/GTE/", "V63", 1.0).
?test(sheet1_C64, "/GTE/", "C64", 1.0).
?test(sheet1_D64, "/GTE/", "D64", 1.0).
?test(sheet1_E64, "/GTE/", "E64", 1.0).
?test(sheet1_F64, "/GTE/", "F64", 1.0).
?test(sheet1_G64, "/GTE/", "G64", 1.0).
?test(sheet1_H64, "/GTE/", "H64", 1.0).
?test(sheet1_I64, "/GTE/", "I64", 1.0).
?test(sheet1_J64, "/GTE/", "J64", 1.0).
?test(sheet1_K64, "/GTE/", "K64", 1.0).
?test(sheet1_L64, "/GTE/", "L64", 1.0).
?test(sheet1_M64, "/GTE/", "M64", 1.0).
?test(sheet1_N64, "/GTE/", "N64", 1.0).
?test(sheet1_O64, "/GTE/", "O64", 1.0).
?test(sheet1_P64, "/GTE/", "P64", 1.0).
?test(sheet1_Q64, "/GTE/", "Q64", 1.0).
?test(sheet1_R64, "/GTE/", "R64", 1.0).
?test(sheet1_S64, "/GTE/", "S64", 1.0).
?test(sheet1_T64, "/GTE/", "T64", 1.0).
?test(sheet1_U64, "/GTE/", "U64", 1.0).
?test(sheet1_V64, "/GTE/", "V64", 1.0).
?test(sheet1_C65, "/GTE/", "C65", 1.0).
?test(sheet1_D65, "/GTE/", "D65", 1.0).
?test(sheet1_E65, "/GTE/", "E65", 1.0).
?test(sheet1_F65, "/GTE/", "F65", 1.0).
?test(sheet1_G65, "/GTE/", "G65", 1.0).
?test(sheet1_H65, "/GTE/", "H65", 1.0).
?test(sheet1_I65, "/GTE/", "I65", 1.0).
?test(sheet1_J65, "/GTE/", "J65", 1.0).
?test(sheet1_K65, "/GTE/", "K65", 1.0).
?test(sheet1_L65, "/GTE/", "L65", 1.0).
?test(sheet1_M65, "/GTE/", "M65", 1.0).
?test(sheet1_N65, "/GTE/", "N65", 1.0).
?test(sheet1_O65, "/GTE/", "O65", 1.0).
?test(sheet1_P65, "/GTE/", "P65", 1.0).
?test(sheet1_Q65, "/GTE/", "Q65", 1.0).
?test(sheet1_R65, "/GTE/", "R65", 1.0).
?test(sheet1_S65, "/GTE/", "S65", 1.0).
?test(sheet1_T65, "/GTE/", "T65", 1.0).
?test(sheet1_U65, "/GTE/", "U65", 1.0).
?test(sheet1_V65, "/GTE/", "V65", 1.0).
?test(sheet1_C66, "/GTE/", "C66", 1.0).
?test(sheet1_D66, "/GTE/", "D66", 1.0).
?test(sheet1_E66, "/GTE/", "E66", 1.0).
?test(sheet1_F66, "/GTE/", "F66", 1.0).
?test(sheet1_G66, "/GTE/", "G66", 1.0).
?test(sheet1_H66, "/GTE/", "H66", 1.0).
?test(sheet1_I66, "/GTE/", "I66", 1.0).
?test(sheet1_J66, "/GTE/", "J66", 1.0).
?test(sheet1_K66, "/GTE/", "K66", 1.0).
?test(sheet1_L66, "/GTE/", "L66", 1.0).
?test(sheet1_M66, "/GTE/", "M66", 1.0).
?test(sheet1_N66, "/GTE/", "N66", 1.0).
?test(sheet1_O66, "/GTE/", "O66", 1.0).
?test(sheet1_P66, "/GTE/", "P66", 1.0).
?test(sheet1_Q66, "/GTE/", "Q66", 1.0).
?test(sheet1_R66, "/GTE/", "R66", 1.0).
?test(sheet1_S66, "/GTE/", "S66", 1.0).
?test(sheet1_T66, "/GTE/", "T66", 1.0).
?test(sheet1_U66, "/GTE/", "U66", 1.0).
?test(sheet1_V66, "/GTE/", "V66", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_operators_gte.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_operators_gte" ++ "/" ++ Sheetname ++ "/",
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
