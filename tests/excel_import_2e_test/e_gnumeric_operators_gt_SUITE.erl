%% This file is generated; DO NOT EDIT MANUALLY.

-module(e_gnumeric_operators_gt_SUITE).
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
                     [Testcase, "e_gnumeric_operators_gt_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "e_gnumeric_operators_gt" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/GT/", "A1", ">").
?test(sheet1_B1, "/GT/", "B1", "B").
?test(sheet1_C1, "/GT/", "C1", "Blank").
?test(sheet1_D1, "/GT/", "D1", "Boolean").
?test(sheet1_E1, "/GT/", "E1", "Boolean").
?test(sheet1_F1, "/GT/", "F1", "Error").
?test(sheet1_G1, "/GT/", "G1", "Error").
?test(sheet1_H1, "/GT/", "H1", "Error").
?test(sheet1_I1, "/GT/", "I1", "Error").
?test(sheet1_J1, "/GT/", "J1", "Error").
?test(sheet1_K1, "/GT/", "K1", "Error").
?test(sheet1_L1, "/GT/", "L1", "Error").
?test(sheet1_M1, "/GT/", "M1", "String").
?test(sheet1_N1, "/GT/", "N1", "String").
?test(sheet1_O1, "/GT/", "O1", "String").
?test(sheet1_P1, "/GT/", "P1", "Str Num").
?test(sheet1_Q1, "/GT/", "Q1", "Str Num").
?test(sheet1_R1, "/GT/", "R1", "Integer").
?test(sheet1_S1, "/GT/", "S1", "Integer").
?test(sheet1_T1, "/GT/", "T1", "Zero").
?test(sheet1_U1, "/GT/", "U1", "Float").
?test(sheet1_V1, "/GT/", "V1", "Float").
?test(sheet1_A2, "/GT/", "A2", "A").
?test(sheet1_D2, "/GT/", "D2", true).
?test(sheet1_E2, "/GT/", "E2", false).
?test(sheet1_F2, "/GT/", "F2", '#DIV/0!').
?test(sheet1_G2, "/GT/", "G2", '#N/A').
?test(sheet1_H2, "/GT/", "H2", '#NAME?').
?test(sheet1_I2, "/GT/", "I2", 'NULL!').
?test(sheet1_J2, "/GT/", "J2", '#NUM!').
?test(sheet1_K2, "/GT/", "K2", '#REF!').
?test(sheet1_L2, "/GT/", "L2", '#VALUE!').
?test(sheet1_M2, "/GT/", "M2", "Liz").
?test(sheet1_N2, "/GT/", "N2", "Doug").
?test(sheet1_O2, "/GT/", "O2", "Bob").
?test(sheet1_P2, "/GT/", "P2", "2.7").
?test(sheet1_Q2, "/GT/", "Q2", "3.54").
?test(sheet1_R2, "/GT/", "R2", "1999/02/01 00:00:00").
?test(sheet1_S2, "/GT/", "S2", "1999/02/02 00:00:00").
?test(sheet1_T2, "/GT/", "T2", 0.0).
?test(sheet1_U2, "/GT/", "U2", 3.1415).
?test(sheet1_V2, "/GT/", "V2", 36193.2).
?test(sheet1_A3, "/GT/", "A3", "Blank").
?test(sheet1_C3, "/GT/", "C3", false).
?test(sheet1_D3, "/GT/", "D3", false).
?test(sheet1_E3, "/GT/", "E3", false).
?test(sheet1_F3, "/GT/", "F3", '#DIV/0!').
?test(sheet1_G3, "/GT/", "G3", '#N/A').
?test(sheet1_H3, "/GT/", "H3", '#NAME?').
?test(sheet1_I3, "/GT/", "I3", 'NULL!').
?test(sheet1_J3, "/GT/", "J3", '#NUM!').
?test(sheet1_K3, "/GT/", "K3", '#REF!').
?test(sheet1_L3, "/GT/", "L3", '#VALUE!').
?test(sheet1_M3, "/GT/", "M3", false).
?test(sheet1_N3, "/GT/", "N3", false).
?test(sheet1_O3, "/GT/", "O3", false).
?test(sheet1_P3, "/GT/", "P3", false).
?test(sheet1_Q3, "/GT/", "Q3", false).
?test(sheet1_R3, "/GT/", "R3", false).
?test(sheet1_S3, "/GT/", "S3", false).
?test(sheet1_T3, "/GT/", "T3", false).
?test(sheet1_U3, "/GT/", "U3", false).
?test(sheet1_V3, "/GT/", "V3", false).
?test(sheet1_A4, "/GT/", "A4", "Boolean").
?test(sheet1_B4, "/GT/", "B4", true).
?test(sheet1_C4, "/GT/", "C4", true).
?test(sheet1_D4, "/GT/", "D4", false).
?test(sheet1_E4, "/GT/", "E4", true).
?test(sheet1_F4, "/GT/", "F4", '#DIV/0!').
?test(sheet1_G4, "/GT/", "G4", '#N/A').
?test(sheet1_H4, "/GT/", "H4", '#NAME?').
?test(sheet1_I4, "/GT/", "I4", 'NULL!').
?test(sheet1_J4, "/GT/", "J4", '#NUM!').
?test(sheet1_K4, "/GT/", "K4", '#REF!').
?test(sheet1_L4, "/GT/", "L4", '#VALUE!').
?test(sheet1_M4, "/GT/", "M4", true).
?test(sheet1_N4, "/GT/", "N4", true).
?test(sheet1_O4, "/GT/", "O4", true).
?test(sheet1_P4, "/GT/", "P4", true).
?test(sheet1_Q4, "/GT/", "Q4", true).
?test(sheet1_R4, "/GT/", "R4", true).
?test(sheet1_S4, "/GT/", "S4", true).
?test(sheet1_T4, "/GT/", "T4", true).
?test(sheet1_U4, "/GT/", "U4", true).
?test(sheet1_V4, "/GT/", "V4", true).
?test(sheet1_A5, "/GT/", "A5", "Boolean").
?test(sheet1_B5, "/GT/", "B5", false).
?test(sheet1_C5, "/GT/", "C5", false).
?test(sheet1_D5, "/GT/", "D5", false).
?test(sheet1_E5, "/GT/", "E5", false).
?test(sheet1_F5, "/GT/", "F5", '#DIV/0!').
?test(sheet1_G5, "/GT/", "G5", '#N/A').
?test(sheet1_H5, "/GT/", "H5", '#NAME?').
?test(sheet1_I5, "/GT/", "I5", 'NULL!').
?test(sheet1_J5, "/GT/", "J5", '#NUM!').
?test(sheet1_K5, "/GT/", "K5", '#REF!').
?test(sheet1_L5, "/GT/", "L5", '#VALUE!').
?test(sheet1_M5, "/GT/", "M5", true).
?test(sheet1_N5, "/GT/", "N5", true).
?test(sheet1_O5, "/GT/", "O5", true).
?test(sheet1_P5, "/GT/", "P5", true).
?test(sheet1_Q5, "/GT/", "Q5", true).
?test(sheet1_R5, "/GT/", "R5", true).
?test(sheet1_S5, "/GT/", "S5", true).
?test(sheet1_T5, "/GT/", "T5", true).
?test(sheet1_U5, "/GT/", "U5", true).
?test(sheet1_V5, "/GT/", "V5", true).
?test(sheet1_A6, "/GT/", "A6", "Error").
?test(sheet1_B6, "/GT/", "B6", '#DIV/0!').
?test(sheet1_C6, "/GT/", "C6", '#DIV/0!').
?test(sheet1_D6, "/GT/", "D6", '#DIV/0!').
?test(sheet1_E6, "/GT/", "E6", '#DIV/0!').
?test(sheet1_F6, "/GT/", "F6", '#DIV/0!').
?test(sheet1_G6, "/GT/", "G6", '#DIV/0!').
?test(sheet1_H6, "/GT/", "H6", '#DIV/0!').
?test(sheet1_I6, "/GT/", "I6", '#DIV/0!').
?test(sheet1_J6, "/GT/", "J6", '#DIV/0!').
?test(sheet1_K6, "/GT/", "K6", '#DIV/0!').
?test(sheet1_L6, "/GT/", "L6", '#DIV/0!').
?test(sheet1_M6, "/GT/", "M6", '#DIV/0!').
?test(sheet1_N6, "/GT/", "N6", '#DIV/0!').
?test(sheet1_O6, "/GT/", "O6", '#DIV/0!').
?test(sheet1_P6, "/GT/", "P6", '#DIV/0!').
?test(sheet1_Q6, "/GT/", "Q6", '#DIV/0!').
?test(sheet1_R6, "/GT/", "R6", '#DIV/0!').
?test(sheet1_S6, "/GT/", "S6", '#DIV/0!').
?test(sheet1_T6, "/GT/", "T6", '#DIV/0!').
?test(sheet1_U6, "/GT/", "U6", '#DIV/0!').
?test(sheet1_V6, "/GT/", "V6", '#DIV/0!').
?test(sheet1_A7, "/GT/", "A7", "Error").
?test(sheet1_B7, "/GT/", "B7", '#N/A').
?test(sheet1_C7, "/GT/", "C7", '#N/A').
?test(sheet1_D7, "/GT/", "D7", '#N/A').
?test(sheet1_E7, "/GT/", "E7", '#N/A').
?test(sheet1_F7, "/GT/", "F7", '#N/A').
?test(sheet1_G7, "/GT/", "G7", '#N/A').
?test(sheet1_H7, "/GT/", "H7", '#N/A').
?test(sheet1_I7, "/GT/", "I7", '#N/A').
?test(sheet1_J7, "/GT/", "J7", '#N/A').
?test(sheet1_K7, "/GT/", "K7", '#N/A').
?test(sheet1_L7, "/GT/", "L7", '#N/A').
?test(sheet1_M7, "/GT/", "M7", '#N/A').
?test(sheet1_N7, "/GT/", "N7", '#N/A').
?test(sheet1_O7, "/GT/", "O7", '#N/A').
?test(sheet1_P7, "/GT/", "P7", '#N/A').
?test(sheet1_Q7, "/GT/", "Q7", '#N/A').
?test(sheet1_R7, "/GT/", "R7", '#N/A').
?test(sheet1_S7, "/GT/", "S7", '#N/A').
?test(sheet1_T7, "/GT/", "T7", '#N/A').
?test(sheet1_U7, "/GT/", "U7", '#N/A').
?test(sheet1_V7, "/GT/", "V7", '#N/A').
?test(sheet1_A8, "/GT/", "A8", "Error").
?test(sheet1_B8, "/GT/", "B8", '#NAME?').
?test(sheet1_C8, "/GT/", "C8", '#NAME?').
?test(sheet1_D8, "/GT/", "D8", '#NAME?').
?test(sheet1_E8, "/GT/", "E8", '#NAME?').
?test(sheet1_F8, "/GT/", "F8", '#NAME?').
?test(sheet1_G8, "/GT/", "G8", '#NAME?').
?test(sheet1_H8, "/GT/", "H8", '#NAME?').
?test(sheet1_I8, "/GT/", "I8", '#NAME?').
?test(sheet1_J8, "/GT/", "J8", '#NAME?').
?test(sheet1_K8, "/GT/", "K8", '#NAME?').
?test(sheet1_L8, "/GT/", "L8", '#NAME?').
?test(sheet1_M8, "/GT/", "M8", '#NAME?').
?test(sheet1_N8, "/GT/", "N8", '#NAME?').
?test(sheet1_O8, "/GT/", "O8", '#NAME?').
?test(sheet1_P8, "/GT/", "P8", '#NAME?').
?test(sheet1_Q8, "/GT/", "Q8", '#NAME?').
?test(sheet1_R8, "/GT/", "R8", '#NAME?').
?test(sheet1_S8, "/GT/", "S8", '#NAME?').
?test(sheet1_T8, "/GT/", "T8", '#NAME?').
?test(sheet1_U8, "/GT/", "U8", '#NAME?').
?test(sheet1_V8, "/GT/", "V8", '#NAME?').
?test(sheet1_A9, "/GT/", "A9", "Error").
?test(sheet1_B9, "/GT/", "B9", 'NULL!').
?test(sheet1_C9, "/GT/", "C9", 'NULL!').
?test(sheet1_D9, "/GT/", "D9", 'NULL!').
?test(sheet1_E9, "/GT/", "E9", 'NULL!').
?test(sheet1_F9, "/GT/", "F9", 'NULL!').
?test(sheet1_G9, "/GT/", "G9", 'NULL!').
?test(sheet1_H9, "/GT/", "H9", 'NULL!').
?test(sheet1_I9, "/GT/", "I9", 'NULL!').
?test(sheet1_J9, "/GT/", "J9", 'NULL!').
?test(sheet1_K9, "/GT/", "K9", 'NULL!').
?test(sheet1_L9, "/GT/", "L9", 'NULL!').
?test(sheet1_M9, "/GT/", "M9", 'NULL!').
?test(sheet1_N9, "/GT/", "N9", 'NULL!').
?test(sheet1_O9, "/GT/", "O9", 'NULL!').
?test(sheet1_P9, "/GT/", "P9", 'NULL!').
?test(sheet1_Q9, "/GT/", "Q9", 'NULL!').
?test(sheet1_R9, "/GT/", "R9", 'NULL!').
?test(sheet1_S9, "/GT/", "S9", 'NULL!').
?test(sheet1_T9, "/GT/", "T9", 'NULL!').
?test(sheet1_U9, "/GT/", "U9", 'NULL!').
?test(sheet1_V9, "/GT/", "V9", 'NULL!').
?test(sheet1_A10, "/GT/", "A10", "Error").
?test(sheet1_B10, "/GT/", "B10", '#NUM!').
?test(sheet1_C10, "/GT/", "C10", '#NUM!').
?test(sheet1_D10, "/GT/", "D10", '#NUM!').
?test(sheet1_E10, "/GT/", "E10", '#NUM!').
?test(sheet1_F10, "/GT/", "F10", '#NUM!').
?test(sheet1_G10, "/GT/", "G10", '#NUM!').
?test(sheet1_H10, "/GT/", "H10", '#NUM!').
?test(sheet1_I10, "/GT/", "I10", '#NUM!').
?test(sheet1_J10, "/GT/", "J10", '#NUM!').
?test(sheet1_K10, "/GT/", "K10", '#NUM!').
?test(sheet1_L10, "/GT/", "L10", '#NUM!').
?test(sheet1_M10, "/GT/", "M10", '#NUM!').
?test(sheet1_N10, "/GT/", "N10", '#NUM!').
?test(sheet1_O10, "/GT/", "O10", '#NUM!').
?test(sheet1_P10, "/GT/", "P10", '#NUM!').
?test(sheet1_Q10, "/GT/", "Q10", '#NUM!').
?test(sheet1_R10, "/GT/", "R10", '#NUM!').
?test(sheet1_S10, "/GT/", "S10", '#NUM!').
?test(sheet1_T10, "/GT/", "T10", '#NUM!').
?test(sheet1_U10, "/GT/", "U10", '#NUM!').
?test(sheet1_V10, "/GT/", "V10", '#NUM!').
?test(sheet1_A11, "/GT/", "A11", "Error").
?test(sheet1_B11, "/GT/", "B11", '#REF!').
?test(sheet1_C11, "/GT/", "C11", '#REF!').
?test(sheet1_D11, "/GT/", "D11", '#REF!').
?test(sheet1_E11, "/GT/", "E11", '#REF!').
?test(sheet1_F11, "/GT/", "F11", '#REF!').
?test(sheet1_G11, "/GT/", "G11", '#REF!').
?test(sheet1_H11, "/GT/", "H11", '#REF!').
?test(sheet1_I11, "/GT/", "I11", '#REF!').
?test(sheet1_J11, "/GT/", "J11", '#REF!').
?test(sheet1_K11, "/GT/", "K11", '#REF!').
?test(sheet1_L11, "/GT/", "L11", '#REF!').
?test(sheet1_M11, "/GT/", "M11", '#REF!').
?test(sheet1_N11, "/GT/", "N11", '#REF!').
?test(sheet1_O11, "/GT/", "O11", '#REF!').
?test(sheet1_P11, "/GT/", "P11", '#REF!').
?test(sheet1_Q11, "/GT/", "Q11", '#REF!').
?test(sheet1_R11, "/GT/", "R11", '#REF!').
?test(sheet1_S11, "/GT/", "S11", '#REF!').
?test(sheet1_T11, "/GT/", "T11", '#REF!').
?test(sheet1_U11, "/GT/", "U11", '#REF!').
?test(sheet1_V11, "/GT/", "V11", '#REF!').
?test(sheet1_A12, "/GT/", "A12", "Error").
?test(sheet1_B12, "/GT/", "B12", '#VALUE!').
?test(sheet1_C12, "/GT/", "C12", '#VALUE!').
?test(sheet1_D12, "/GT/", "D12", '#VALUE!').
?test(sheet1_E12, "/GT/", "E12", '#VALUE!').
?test(sheet1_F12, "/GT/", "F12", '#VALUE!').
?test(sheet1_G12, "/GT/", "G12", '#VALUE!').
?test(sheet1_H12, "/GT/", "H12", '#VALUE!').
?test(sheet1_I12, "/GT/", "I12", '#VALUE!').
?test(sheet1_J12, "/GT/", "J12", '#VALUE!').
?test(sheet1_K12, "/GT/", "K12", '#VALUE!').
?test(sheet1_L12, "/GT/", "L12", '#VALUE!').
?test(sheet1_M12, "/GT/", "M12", '#VALUE!').
?test(sheet1_N12, "/GT/", "N12", '#VALUE!').
?test(sheet1_O12, "/GT/", "O12", '#VALUE!').
?test(sheet1_P12, "/GT/", "P12", '#VALUE!').
?test(sheet1_Q12, "/GT/", "Q12", '#VALUE!').
?test(sheet1_R12, "/GT/", "R12", '#VALUE!').
?test(sheet1_S12, "/GT/", "S12", '#VALUE!').
?test(sheet1_T12, "/GT/", "T12", '#VALUE!').
?test(sheet1_U12, "/GT/", "U12", '#VALUE!').
?test(sheet1_V12, "/GT/", "V12", '#VALUE!').
?test(sheet1_A13, "/GT/", "A13", "String").
?test(sheet1_B13, "/GT/", "B13", "Liz").
?test(sheet1_C13, "/GT/", "C13", true).
?test(sheet1_D13, "/GT/", "D13", false).
?test(sheet1_E13, "/GT/", "E13", false).
?test(sheet1_F13, "/GT/", "F13", '#DIV/0!').
?test(sheet1_G13, "/GT/", "G13", '#N/A').
?test(sheet1_H13, "/GT/", "H13", '#NAME?').
?test(sheet1_I13, "/GT/", "I13", 'NULL!').
?test(sheet1_J13, "/GT/", "J13", '#NUM!').
?test(sheet1_K13, "/GT/", "K13", '#REF!').
?test(sheet1_L13, "/GT/", "L13", '#VALUE!').
?test(sheet1_M13, "/GT/", "M13", false).
?test(sheet1_N13, "/GT/", "N13", true).
?test(sheet1_O13, "/GT/", "O13", true).
?test(sheet1_P13, "/GT/", "P13", true).
?test(sheet1_Q13, "/GT/", "Q13", true).
?test(sheet1_R13, "/GT/", "R13", true).
?test(sheet1_S13, "/GT/", "S13", true).
?test(sheet1_T13, "/GT/", "T13", true).
?test(sheet1_U13, "/GT/", "U13", true).
?test(sheet1_V13, "/GT/", "V13", true).
?test(sheet1_A14, "/GT/", "A14", "String").
?test(sheet1_B14, "/GT/", "B14", "Doug").
?test(sheet1_C14, "/GT/", "C14", true).
?test(sheet1_D14, "/GT/", "D14", false).
?test(sheet1_E14, "/GT/", "E14", false).
?test(sheet1_F14, "/GT/", "F14", '#DIV/0!').
?test(sheet1_G14, "/GT/", "G14", '#N/A').
?test(sheet1_H14, "/GT/", "H14", '#NAME?').
?test(sheet1_I14, "/GT/", "I14", 'NULL!').
?test(sheet1_J14, "/GT/", "J14", '#NUM!').
?test(sheet1_K14, "/GT/", "K14", '#REF!').
?test(sheet1_L14, "/GT/", "L14", '#VALUE!').
?test(sheet1_M14, "/GT/", "M14", false).
?test(sheet1_N14, "/GT/", "N14", false).
?test(sheet1_O14, "/GT/", "O14", true).
?test(sheet1_P14, "/GT/", "P14", true).
?test(sheet1_Q14, "/GT/", "Q14", true).
?test(sheet1_R14, "/GT/", "R14", true).
?test(sheet1_S14, "/GT/", "S14", true).
?test(sheet1_T14, "/GT/", "T14", true).
?test(sheet1_U14, "/GT/", "U14", true).
?test(sheet1_V14, "/GT/", "V14", true).
?test(sheet1_A15, "/GT/", "A15", "String").
?test(sheet1_B15, "/GT/", "B15", "Bob").
?test(sheet1_C15, "/GT/", "C15", true).
?test(sheet1_D15, "/GT/", "D15", false).
?test(sheet1_E15, "/GT/", "E15", false).
?test(sheet1_F15, "/GT/", "F15", '#DIV/0!').
?test(sheet1_G15, "/GT/", "G15", '#N/A').
?test(sheet1_H15, "/GT/", "H15", '#NAME?').
?test(sheet1_I15, "/GT/", "I15", 'NULL!').
?test(sheet1_J15, "/GT/", "J15", '#NUM!').
?test(sheet1_K15, "/GT/", "K15", '#REF!').
?test(sheet1_L15, "/GT/", "L15", '#VALUE!').
?test(sheet1_M15, "/GT/", "M15", false).
?test(sheet1_N15, "/GT/", "N15", false).
?test(sheet1_O15, "/GT/", "O15", false).
?test(sheet1_P15, "/GT/", "P15", true).
?test(sheet1_Q15, "/GT/", "Q15", true).
?test(sheet1_R15, "/GT/", "R15", true).
?test(sheet1_S15, "/GT/", "S15", true).
?test(sheet1_T15, "/GT/", "T15", true).
?test(sheet1_U15, "/GT/", "U15", true).
?test(sheet1_V15, "/GT/", "V15", true).
?test(sheet1_A16, "/GT/", "A16", "Str Num").
?test(sheet1_B16, "/GT/", "B16", "2.7").
?test(sheet1_C16, "/GT/", "C16", true).
?test(sheet1_D16, "/GT/", "D16", false).
?test(sheet1_E16, "/GT/", "E16", false).
?test(sheet1_F16, "/GT/", "F16", '#DIV/0!').
?test(sheet1_G16, "/GT/", "G16", '#N/A').
?test(sheet1_H16, "/GT/", "H16", '#NAME?').
?test(sheet1_I16, "/GT/", "I16", 'NULL!').
?test(sheet1_J16, "/GT/", "J16", '#NUM!').
?test(sheet1_K16, "/GT/", "K16", '#REF!').
?test(sheet1_L16, "/GT/", "L16", '#VALUE!').
?test(sheet1_M16, "/GT/", "M16", false).
?test(sheet1_N16, "/GT/", "N16", false).
?test(sheet1_O16, "/GT/", "O16", false).
?test(sheet1_P16, "/GT/", "P16", false).
?test(sheet1_Q16, "/GT/", "Q16", false).
?test(sheet1_R16, "/GT/", "R16", true).
?test(sheet1_S16, "/GT/", "S16", true).
?test(sheet1_T16, "/GT/", "T16", true).
?test(sheet1_U16, "/GT/", "U16", true).
?test(sheet1_V16, "/GT/", "V16", true).
?test(sheet1_A17, "/GT/", "A17", "Str Num").
?test(sheet1_B17, "/GT/", "B17", "3.54").
?test(sheet1_C17, "/GT/", "C17", true).
?test(sheet1_D17, "/GT/", "D17", false).
?test(sheet1_E17, "/GT/", "E17", false).
?test(sheet1_F17, "/GT/", "F17", '#DIV/0!').
?test(sheet1_G17, "/GT/", "G17", '#N/A').
?test(sheet1_H17, "/GT/", "H17", '#NAME?').
?test(sheet1_I17, "/GT/", "I17", 'NULL!').
?test(sheet1_J17, "/GT/", "J17", '#NUM!').
?test(sheet1_K17, "/GT/", "K17", '#REF!').
?test(sheet1_L17, "/GT/", "L17", '#VALUE!').
?test(sheet1_M17, "/GT/", "M17", false).
?test(sheet1_N17, "/GT/", "N17", false).
?test(sheet1_O17, "/GT/", "O17", false).
?test(sheet1_P17, "/GT/", "P17", true).
?test(sheet1_Q17, "/GT/", "Q17", false).
?test(sheet1_R17, "/GT/", "R17", true).
?test(sheet1_S17, "/GT/", "S17", true).
?test(sheet1_T17, "/GT/", "T17", true).
?test(sheet1_U17, "/GT/", "U17", true).
?test(sheet1_V17, "/GT/", "V17", true).
?test(sheet1_A18, "/GT/", "A18", "Integer").
?test(sheet1_B18, "/GT/", "B18", "1999/02/01 00:00:00").
?test(sheet1_C18, "/GT/", "C18", true).
?test(sheet1_D18, "/GT/", "D18", false).
?test(sheet1_E18, "/GT/", "E18", false).
?test(sheet1_F18, "/GT/", "F18", '#DIV/0!').
?test(sheet1_G18, "/GT/", "G18", '#N/A').
?test(sheet1_H18, "/GT/", "H18", '#NAME?').
?test(sheet1_I18, "/GT/", "I18", 'NULL!').
?test(sheet1_J18, "/GT/", "J18", '#NUM!').
?test(sheet1_K18, "/GT/", "K18", '#REF!').
?test(sheet1_L18, "/GT/", "L18", '#VALUE!').
?test(sheet1_M18, "/GT/", "M18", false).
?test(sheet1_N18, "/GT/", "N18", false).
?test(sheet1_O18, "/GT/", "O18", false).
?test(sheet1_P18, "/GT/", "P18", false).
?test(sheet1_Q18, "/GT/", "Q18", false).
?test(sheet1_R18, "/GT/", "R18", false).
?test(sheet1_S18, "/GT/", "S18", false).
?test(sheet1_T18, "/GT/", "T18", true).
?test(sheet1_U18, "/GT/", "U18", true).
?test(sheet1_V18, "/GT/", "V18", false).
?test(sheet1_A19, "/GT/", "A19", "Integer").
?test(sheet1_B19, "/GT/", "B19", "1999/02/02 00:00:00").
?test(sheet1_C19, "/GT/", "C19", true).
?test(sheet1_D19, "/GT/", "D19", false).
?test(sheet1_E19, "/GT/", "E19", false).
?test(sheet1_F19, "/GT/", "F19", '#DIV/0!').
?test(sheet1_G19, "/GT/", "G19", '#N/A').
?test(sheet1_H19, "/GT/", "H19", '#NAME?').
?test(sheet1_I19, "/GT/", "I19", 'NULL!').
?test(sheet1_J19, "/GT/", "J19", '#NUM!').
?test(sheet1_K19, "/GT/", "K19", '#REF!').
?test(sheet1_L19, "/GT/", "L19", '#VALUE!').
?test(sheet1_M19, "/GT/", "M19", false).
?test(sheet1_N19, "/GT/", "N19", false).
?test(sheet1_O19, "/GT/", "O19", false).
?test(sheet1_P19, "/GT/", "P19", false).
?test(sheet1_Q19, "/GT/", "Q19", false).
?test(sheet1_R19, "/GT/", "R19", true).
?test(sheet1_S19, "/GT/", "S19", false).
?test(sheet1_T19, "/GT/", "T19", true).
?test(sheet1_U19, "/GT/", "U19", true).
?test(sheet1_V19, "/GT/", "V19", false).
?test(sheet1_A20, "/GT/", "A20", "Zero").
?test(sheet1_B20, "/GT/", "B20", 0.0).
?test(sheet1_C20, "/GT/", "C20", false).
?test(sheet1_D20, "/GT/", "D20", false).
?test(sheet1_E20, "/GT/", "E20", false).
?test(sheet1_F20, "/GT/", "F20", '#DIV/0!').
?test(sheet1_G20, "/GT/", "G20", '#N/A').
?test(sheet1_H20, "/GT/", "H20", '#NAME?').
?test(sheet1_I20, "/GT/", "I20", 'NULL!').
?test(sheet1_J20, "/GT/", "J20", '#NUM!').
?test(sheet1_K20, "/GT/", "K20", '#REF!').
?test(sheet1_L20, "/GT/", "L20", '#VALUE!').
?test(sheet1_M20, "/GT/", "M20", false).
?test(sheet1_N20, "/GT/", "N20", false).
?test(sheet1_O20, "/GT/", "O20", false).
?test(sheet1_P20, "/GT/", "P20", false).
?test(sheet1_Q20, "/GT/", "Q20", false).
?test(sheet1_R20, "/GT/", "R20", false).
?test(sheet1_S20, "/GT/", "S20", false).
?test(sheet1_T20, "/GT/", "T20", false).
?test(sheet1_U20, "/GT/", "U20", false).
?test(sheet1_V20, "/GT/", "V20", false).
?test(sheet1_A21, "/GT/", "A21", "Float").
?test(sheet1_B21, "/GT/", "B21", 3.1415).
?test(sheet1_C21, "/GT/", "C21", true).
?test(sheet1_D21, "/GT/", "D21", false).
?test(sheet1_E21, "/GT/", "E21", false).
?test(sheet1_F21, "/GT/", "F21", '#DIV/0!').
?test(sheet1_G21, "/GT/", "G21", '#N/A').
?test(sheet1_H21, "/GT/", "H21", '#NAME?').
?test(sheet1_I21, "/GT/", "I21", 'NULL!').
?test(sheet1_J21, "/GT/", "J21", '#NUM!').
?test(sheet1_K21, "/GT/", "K21", '#REF!').
?test(sheet1_L21, "/GT/", "L21", '#VALUE!').
?test(sheet1_M21, "/GT/", "M21", false).
?test(sheet1_N21, "/GT/", "N21", false).
?test(sheet1_O21, "/GT/", "O21", false).
?test(sheet1_P21, "/GT/", "P21", false).
?test(sheet1_Q21, "/GT/", "Q21", false).
?test(sheet1_R21, "/GT/", "R21", false).
?test(sheet1_S21, "/GT/", "S21", false).
?test(sheet1_T21, "/GT/", "T21", true).
?test(sheet1_U21, "/GT/", "U21", false).
?test(sheet1_V21, "/GT/", "V21", false).
?test(sheet1_A22, "/GT/", "A22", "Float").
?test(sheet1_B22, "/GT/", "B22", 36193.2).
?test(sheet1_C22, "/GT/", "C22", true).
?test(sheet1_D22, "/GT/", "D22", false).
?test(sheet1_E22, "/GT/", "E22", false).
?test(sheet1_F22, "/GT/", "F22", '#DIV/0!').
?test(sheet1_G22, "/GT/", "G22", '#N/A').
?test(sheet1_H22, "/GT/", "H22", '#NAME?').
?test(sheet1_I22, "/GT/", "I22", 'NULL!').
?test(sheet1_J22, "/GT/", "J22", '#NUM!').
?test(sheet1_K22, "/GT/", "K22", '#REF!').
?test(sheet1_L22, "/GT/", "L22", '#VALUE!').
?test(sheet1_M22, "/GT/", "M22", false).
?test(sheet1_N22, "/GT/", "N22", false).
?test(sheet1_O22, "/GT/", "O22", false).
?test(sheet1_P22, "/GT/", "P22", false).
?test(sheet1_Q22, "/GT/", "Q22", false).
?test(sheet1_R22, "/GT/", "R22", true).
?test(sheet1_S22, "/GT/", "S22", true).
?test(sheet1_T22, "/GT/", "T22", true).
?test(sheet1_U22, "/GT/", "U22", true).
?test(sheet1_V22, "/GT/", "V22", false).
?test(sheet1_A25, "/GT/", "A25", "Blank").
?test(sheet1_C25, "/GT/", "C25", false).
?test(sheet1_D25, "/GT/", "D25", false).
?test(sheet1_E25, "/GT/", "E25", false).
?test(sheet1_F25, "/GT/", "F25", '#DIV/0!').
?test(sheet1_G25, "/GT/", "G25", '#N/A').
?test(sheet1_H25, "/GT/", "H25", '#NAME?').
?test(sheet1_I25, "/GT/", "I25", 'NULL!').
?test(sheet1_J25, "/GT/", "J25", '#NUM!').
?test(sheet1_K25, "/GT/", "K25", '#REF!').
?test(sheet1_L25, "/GT/", "L25", '#VALUE!').
?test(sheet1_M25, "/GT/", "M25", false).
?test(sheet1_N25, "/GT/", "N25", false).
?test(sheet1_O25, "/GT/", "O25", false).
?test(sheet1_P25, "/GT/", "P25", false).
?test(sheet1_Q25, "/GT/", "Q25", false).
?test(sheet1_R25, "/GT/", "R25", false).
?test(sheet1_S25, "/GT/", "S25", false).
?test(sheet1_T25, "/GT/", "T25", false).
?test(sheet1_U25, "/GT/", "U25", false).
?test(sheet1_V25, "/GT/", "V25", false).
?test(sheet1_A26, "/GT/", "A26", "Boolean").
?test(sheet1_C26, "/GT/", "C26", true).
?test(sheet1_D26, "/GT/", "D26", false).
?test(sheet1_E26, "/GT/", "E26", true).
?test(sheet1_F26, "/GT/", "F26", '#DIV/0!').
?test(sheet1_G26, "/GT/", "G26", '#N/A').
?test(sheet1_H26, "/GT/", "H26", '#NAME?').
?test(sheet1_I26, "/GT/", "I26", 'NULL!').
?test(sheet1_J26, "/GT/", "J26", '#NUM!').
?test(sheet1_K26, "/GT/", "K26", '#REF!').
?test(sheet1_L26, "/GT/", "L26", '#VALUE!').
?test(sheet1_M26, "/GT/", "M26", true).
?test(sheet1_N26, "/GT/", "N26", true).
?test(sheet1_O26, "/GT/", "O26", true).
?test(sheet1_P26, "/GT/", "P26", true).
?test(sheet1_Q26, "/GT/", "Q26", true).
?test(sheet1_R26, "/GT/", "R26", true).
?test(sheet1_S26, "/GT/", "S26", true).
?test(sheet1_T26, "/GT/", "T26", true).
?test(sheet1_U26, "/GT/", "U26", true).
?test(sheet1_V26, "/GT/", "V26", true).
?test(sheet1_A27, "/GT/", "A27", "Boolean").
?test(sheet1_C27, "/GT/", "C27", false).
?test(sheet1_D27, "/GT/", "D27", false).
?test(sheet1_E27, "/GT/", "E27", false).
?test(sheet1_F27, "/GT/", "F27", '#DIV/0!').
?test(sheet1_G27, "/GT/", "G27", '#N/A').
?test(sheet1_H27, "/GT/", "H27", '#NAME?').
?test(sheet1_I27, "/GT/", "I27", 'NULL!').
?test(sheet1_J27, "/GT/", "J27", '#NUM!').
?test(sheet1_K27, "/GT/", "K27", '#REF!').
?test(sheet1_L27, "/GT/", "L27", '#VALUE!').
?test(sheet1_M27, "/GT/", "M27", true).
?test(sheet1_N27, "/GT/", "N27", true).
?test(sheet1_O27, "/GT/", "O27", true).
?test(sheet1_P27, "/GT/", "P27", true).
?test(sheet1_Q27, "/GT/", "Q27", true).
?test(sheet1_R27, "/GT/", "R27", true).
?test(sheet1_S27, "/GT/", "S27", true).
?test(sheet1_T27, "/GT/", "T27", true).
?test(sheet1_U27, "/GT/", "U27", true).
?test(sheet1_V27, "/GT/", "V27", true).
?test(sheet1_A28, "/GT/", "A28", "Error").
?test(sheet1_C28, "/GT/", "C28", '#DIV/0!').
?test(sheet1_D28, "/GT/", "D28", '#DIV/0!').
?test(sheet1_E28, "/GT/", "E28", '#DIV/0!').
?test(sheet1_F28, "/GT/", "F28", '#DIV/0!').
?test(sheet1_G28, "/GT/", "G28", '#DIV/0!').
?test(sheet1_H28, "/GT/", "H28", '#DIV/0!').
?test(sheet1_I28, "/GT/", "I28", '#DIV/0!').
?test(sheet1_J28, "/GT/", "J28", '#DIV/0!').
?test(sheet1_K28, "/GT/", "K28", '#DIV/0!').
?test(sheet1_L28, "/GT/", "L28", '#DIV/0!').
?test(sheet1_M28, "/GT/", "M28", '#DIV/0!').
?test(sheet1_N28, "/GT/", "N28", '#DIV/0!').
?test(sheet1_O28, "/GT/", "O28", '#DIV/0!').
?test(sheet1_P28, "/GT/", "P28", '#DIV/0!').
?test(sheet1_Q28, "/GT/", "Q28", '#DIV/0!').
?test(sheet1_R28, "/GT/", "R28", '#DIV/0!').
?test(sheet1_S28, "/GT/", "S28", '#DIV/0!').
?test(sheet1_T28, "/GT/", "T28", '#DIV/0!').
?test(sheet1_U28, "/GT/", "U28", '#DIV/0!').
?test(sheet1_V28, "/GT/", "V28", '#DIV/0!').
?test(sheet1_A29, "/GT/", "A29", "Error").
?test(sheet1_C29, "/GT/", "C29", '#N/A').
?test(sheet1_D29, "/GT/", "D29", '#N/A').
?test(sheet1_E29, "/GT/", "E29", '#N/A').
?test(sheet1_F29, "/GT/", "F29", '#N/A').
?test(sheet1_G29, "/GT/", "G29", '#N/A').
?test(sheet1_H29, "/GT/", "H29", '#N/A').
?test(sheet1_I29, "/GT/", "I29", '#N/A').
?test(sheet1_J29, "/GT/", "J29", '#N/A').
?test(sheet1_K29, "/GT/", "K29", '#N/A').
?test(sheet1_L29, "/GT/", "L29", '#N/A').
?test(sheet1_M29, "/GT/", "M29", '#N/A').
?test(sheet1_N29, "/GT/", "N29", '#N/A').
?test(sheet1_O29, "/GT/", "O29", '#N/A').
?test(sheet1_P29, "/GT/", "P29", '#N/A').
?test(sheet1_Q29, "/GT/", "Q29", '#N/A').
?test(sheet1_R29, "/GT/", "R29", '#N/A').
?test(sheet1_S29, "/GT/", "S29", '#N/A').
?test(sheet1_T29, "/GT/", "T29", '#N/A').
?test(sheet1_U29, "/GT/", "U29", '#N/A').
?test(sheet1_V29, "/GT/", "V29", '#N/A').
?test(sheet1_A30, "/GT/", "A30", "Error").
?test(sheet1_C30, "/GT/", "C30", '#NAME?').
?test(sheet1_D30, "/GT/", "D30", '#NAME?').
?test(sheet1_E30, "/GT/", "E30", '#NAME?').
?test(sheet1_F30, "/GT/", "F30", '#NAME?').
?test(sheet1_G30, "/GT/", "G30", '#NAME?').
?test(sheet1_H30, "/GT/", "H30", '#NAME?').
?test(sheet1_I30, "/GT/", "I30", '#NAME?').
?test(sheet1_J30, "/GT/", "J30", '#NAME?').
?test(sheet1_K30, "/GT/", "K30", '#NAME?').
?test(sheet1_L30, "/GT/", "L30", '#NAME?').
?test(sheet1_M30, "/GT/", "M30", '#NAME?').
?test(sheet1_N30, "/GT/", "N30", '#NAME?').
?test(sheet1_O30, "/GT/", "O30", '#NAME?').
?test(sheet1_P30, "/GT/", "P30", '#NAME?').
?test(sheet1_Q30, "/GT/", "Q30", '#NAME?').
?test(sheet1_R30, "/GT/", "R30", '#NAME?').
?test(sheet1_S30, "/GT/", "S30", '#NAME?').
?test(sheet1_T30, "/GT/", "T30", '#NAME?').
?test(sheet1_U30, "/GT/", "U30", '#NAME?').
?test(sheet1_V30, "/GT/", "V30", '#NAME?').
?test(sheet1_A31, "/GT/", "A31", "Error").
?test(sheet1_C31, "/GT/", "C31", 'NULL!').
?test(sheet1_D31, "/GT/", "D31", 'NULL!').
?test(sheet1_E31, "/GT/", "E31", 'NULL!').
?test(sheet1_F31, "/GT/", "F31", 'NULL!').
?test(sheet1_G31, "/GT/", "G31", 'NULL!').
?test(sheet1_H31, "/GT/", "H31", 'NULL!').
?test(sheet1_I31, "/GT/", "I31", 'NULL!').
?test(sheet1_J31, "/GT/", "J31", 'NULL!').
?test(sheet1_K31, "/GT/", "K31", 'NULL!').
?test(sheet1_L31, "/GT/", "L31", 'NULL!').
?test(sheet1_M31, "/GT/", "M31", 'NULL!').
?test(sheet1_N31, "/GT/", "N31", 'NULL!').
?test(sheet1_O31, "/GT/", "O31", 'NULL!').
?test(sheet1_P31, "/GT/", "P31", 'NULL!').
?test(sheet1_Q31, "/GT/", "Q31", 'NULL!').
?test(sheet1_R31, "/GT/", "R31", 'NULL!').
?test(sheet1_S31, "/GT/", "S31", 'NULL!').
?test(sheet1_T31, "/GT/", "T31", 'NULL!').
?test(sheet1_U31, "/GT/", "U31", 'NULL!').
?test(sheet1_V31, "/GT/", "V31", 'NULL!').
?test(sheet1_A32, "/GT/", "A32", "Error").
?test(sheet1_C32, "/GT/", "C32", '#NUM!').
?test(sheet1_D32, "/GT/", "D32", '#NUM!').
?test(sheet1_E32, "/GT/", "E32", '#NUM!').
?test(sheet1_F32, "/GT/", "F32", '#NUM!').
?test(sheet1_G32, "/GT/", "G32", '#NUM!').
?test(sheet1_H32, "/GT/", "H32", '#NUM!').
?test(sheet1_I32, "/GT/", "I32", '#NUM!').
?test(sheet1_J32, "/GT/", "J32", '#NUM!').
?test(sheet1_K32, "/GT/", "K32", '#NUM!').
?test(sheet1_L32, "/GT/", "L32", '#NUM!').
?test(sheet1_M32, "/GT/", "M32", '#NUM!').
?test(sheet1_N32, "/GT/", "N32", '#NUM!').
?test(sheet1_O32, "/GT/", "O32", '#NUM!').
?test(sheet1_P32, "/GT/", "P32", '#NUM!').
?test(sheet1_Q32, "/GT/", "Q32", '#NUM!').
?test(sheet1_R32, "/GT/", "R32", '#NUM!').
?test(sheet1_S32, "/GT/", "S32", '#NUM!').
?test(sheet1_T32, "/GT/", "T32", '#NUM!').
?test(sheet1_U32, "/GT/", "U32", '#NUM!').
?test(sheet1_V32, "/GT/", "V32", '#NUM!').
?test(sheet1_A33, "/GT/", "A33", "Error").
?test(sheet1_C33, "/GT/", "C33", '#REF!').
?test(sheet1_D33, "/GT/", "D33", '#REF!').
?test(sheet1_E33, "/GT/", "E33", '#REF!').
?test(sheet1_F33, "/GT/", "F33", '#REF!').
?test(sheet1_G33, "/GT/", "G33", '#REF!').
?test(sheet1_H33, "/GT/", "H33", '#REF!').
?test(sheet1_I33, "/GT/", "I33", '#REF!').
?test(sheet1_J33, "/GT/", "J33", '#REF!').
?test(sheet1_K33, "/GT/", "K33", '#REF!').
?test(sheet1_L33, "/GT/", "L33", '#REF!').
?test(sheet1_M33, "/GT/", "M33", '#REF!').
?test(sheet1_N33, "/GT/", "N33", '#REF!').
?test(sheet1_O33, "/GT/", "O33", '#REF!').
?test(sheet1_P33, "/GT/", "P33", '#REF!').
?test(sheet1_Q33, "/GT/", "Q33", '#REF!').
?test(sheet1_R33, "/GT/", "R33", '#REF!').
?test(sheet1_S33, "/GT/", "S33", '#REF!').
?test(sheet1_T33, "/GT/", "T33", '#REF!').
?test(sheet1_U33, "/GT/", "U33", '#REF!').
?test(sheet1_V33, "/GT/", "V33", '#REF!').
?test(sheet1_A34, "/GT/", "A34", "Error").
?test(sheet1_C34, "/GT/", "C34", '#VALUE!').
?test(sheet1_D34, "/GT/", "D34", '#VALUE!').
?test(sheet1_E34, "/GT/", "E34", '#VALUE!').
?test(sheet1_F34, "/GT/", "F34", '#VALUE!').
?test(sheet1_G34, "/GT/", "G34", '#VALUE!').
?test(sheet1_H34, "/GT/", "H34", '#VALUE!').
?test(sheet1_I34, "/GT/", "I34", '#VALUE!').
?test(sheet1_J34, "/GT/", "J34", '#VALUE!').
?test(sheet1_K34, "/GT/", "K34", '#VALUE!').
?test(sheet1_L34, "/GT/", "L34", '#VALUE!').
?test(sheet1_M34, "/GT/", "M34", '#VALUE!').
?test(sheet1_N34, "/GT/", "N34", '#VALUE!').
?test(sheet1_O34, "/GT/", "O34", '#VALUE!').
?test(sheet1_P34, "/GT/", "P34", '#VALUE!').
?test(sheet1_Q34, "/GT/", "Q34", '#VALUE!').
?test(sheet1_R34, "/GT/", "R34", '#VALUE!').
?test(sheet1_S34, "/GT/", "S34", '#VALUE!').
?test(sheet1_T34, "/GT/", "T34", '#VALUE!').
?test(sheet1_U34, "/GT/", "U34", '#VALUE!').
?test(sheet1_V34, "/GT/", "V34", '#VALUE!').
?test(sheet1_A35, "/GT/", "A35", "String").
?test(sheet1_C35, "/GT/", "C35", true).
?test(sheet1_D35, "/GT/", "D35", false).
?test(sheet1_E35, "/GT/", "E35", false).
?test(sheet1_F35, "/GT/", "F35", '#DIV/0!').
?test(sheet1_G35, "/GT/", "G35", '#N/A').
?test(sheet1_H35, "/GT/", "H35", '#NAME?').
?test(sheet1_I35, "/GT/", "I35", 'NULL!').
?test(sheet1_J35, "/GT/", "J35", '#NUM!').
?test(sheet1_K35, "/GT/", "K35", '#REF!').
?test(sheet1_L35, "/GT/", "L35", '#VALUE!').
?test(sheet1_M35, "/GT/", "M35", false).
?test(sheet1_N35, "/GT/", "N35", true).
?test(sheet1_O35, "/GT/", "O35", true).
?test(sheet1_P35, "/GT/", "P35", true).
?test(sheet1_Q35, "/GT/", "Q35", true).
?test(sheet1_R35, "/GT/", "R35", true).
?test(sheet1_S35, "/GT/", "S35", true).
?test(sheet1_T35, "/GT/", "T35", true).
?test(sheet1_U35, "/GT/", "U35", true).
?test(sheet1_V35, "/GT/", "V35", true).
?test(sheet1_A36, "/GT/", "A36", "String").
?test(sheet1_C36, "/GT/", "C36", true).
?test(sheet1_D36, "/GT/", "D36", false).
?test(sheet1_E36, "/GT/", "E36", false).
?test(sheet1_F36, "/GT/", "F36", '#DIV/0!').
?test(sheet1_G36, "/GT/", "G36", '#N/A').
?test(sheet1_H36, "/GT/", "H36", '#NAME?').
?test(sheet1_I36, "/GT/", "I36", 'NULL!').
?test(sheet1_J36, "/GT/", "J36", '#NUM!').
?test(sheet1_K36, "/GT/", "K36", '#REF!').
?test(sheet1_L36, "/GT/", "L36", '#VALUE!').
?test(sheet1_M36, "/GT/", "M36", false).
?test(sheet1_N36, "/GT/", "N36", false).
?test(sheet1_O36, "/GT/", "O36", true).
?test(sheet1_P36, "/GT/", "P36", true).
?test(sheet1_Q36, "/GT/", "Q36", true).
?test(sheet1_R36, "/GT/", "R36", true).
?test(sheet1_S36, "/GT/", "S36", true).
?test(sheet1_T36, "/GT/", "T36", true).
?test(sheet1_U36, "/GT/", "U36", true).
?test(sheet1_V36, "/GT/", "V36", true).
?test(sheet1_A37, "/GT/", "A37", "String").
?test(sheet1_C37, "/GT/", "C37", true).
?test(sheet1_D37, "/GT/", "D37", false).
?test(sheet1_E37, "/GT/", "E37", false).
?test(sheet1_F37, "/GT/", "F37", '#DIV/0!').
?test(sheet1_G37, "/GT/", "G37", '#N/A').
?test(sheet1_H37, "/GT/", "H37", '#NAME?').
?test(sheet1_I37, "/GT/", "I37", 'NULL!').
?test(sheet1_J37, "/GT/", "J37", '#NUM!').
?test(sheet1_K37, "/GT/", "K37", '#REF!').
?test(sheet1_L37, "/GT/", "L37", '#VALUE!').
?test(sheet1_M37, "/GT/", "M37", false).
?test(sheet1_N37, "/GT/", "N37", false).
?test(sheet1_O37, "/GT/", "O37", false).
?test(sheet1_P37, "/GT/", "P37", true).
?test(sheet1_Q37, "/GT/", "Q37", true).
?test(sheet1_R37, "/GT/", "R37", true).
?test(sheet1_S37, "/GT/", "S37", true).
?test(sheet1_T37, "/GT/", "T37", true).
?test(sheet1_U37, "/GT/", "U37", true).
?test(sheet1_V37, "/GT/", "V37", true).
?test(sheet1_A38, "/GT/", "A38", "Str Num").
?test(sheet1_C38, "/GT/", "C38", true).
?test(sheet1_D38, "/GT/", "D38", false).
?test(sheet1_E38, "/GT/", "E38", false).
?test(sheet1_F38, "/GT/", "F38", '#DIV/0!').
?test(sheet1_G38, "/GT/", "G38", '#N/A').
?test(sheet1_H38, "/GT/", "H38", '#NAME?').
?test(sheet1_I38, "/GT/", "I38", 'NULL!').
?test(sheet1_J38, "/GT/", "J38", '#NUM!').
?test(sheet1_K38, "/GT/", "K38", '#REF!').
?test(sheet1_L38, "/GT/", "L38", '#VALUE!').
?test(sheet1_M38, "/GT/", "M38", false).
?test(sheet1_N38, "/GT/", "N38", false).
?test(sheet1_O38, "/GT/", "O38", false).
?test(sheet1_P38, "/GT/", "P38", false).
?test(sheet1_Q38, "/GT/", "Q38", false).
?test(sheet1_R38, "/GT/", "R38", true).
?test(sheet1_S38, "/GT/", "S38", true).
?test(sheet1_T38, "/GT/", "T38", true).
?test(sheet1_U38, "/GT/", "U38", true).
?test(sheet1_V38, "/GT/", "V38", true).
?test(sheet1_A39, "/GT/", "A39", "Str Num").
?test(sheet1_C39, "/GT/", "C39", true).
?test(sheet1_D39, "/GT/", "D39", false).
?test(sheet1_E39, "/GT/", "E39", false).
?test(sheet1_F39, "/GT/", "F39", '#DIV/0!').
?test(sheet1_G39, "/GT/", "G39", '#N/A').
?test(sheet1_H39, "/GT/", "H39", '#NAME?').
?test(sheet1_I39, "/GT/", "I39", 'NULL!').
?test(sheet1_J39, "/GT/", "J39", '#NUM!').
?test(sheet1_K39, "/GT/", "K39", '#REF!').
?test(sheet1_L39, "/GT/", "L39", '#VALUE!').
?test(sheet1_M39, "/GT/", "M39", false).
?test(sheet1_N39, "/GT/", "N39", false).
?test(sheet1_O39, "/GT/", "O39", false).
?test(sheet1_P39, "/GT/", "P39", true).
?test(sheet1_Q39, "/GT/", "Q39", false).
?test(sheet1_R39, "/GT/", "R39", true).
?test(sheet1_S39, "/GT/", "S39", true).
?test(sheet1_T39, "/GT/", "T39", true).
?test(sheet1_U39, "/GT/", "U39", true).
?test(sheet1_V39, "/GT/", "V39", true).
?test(sheet1_A40, "/GT/", "A40", "Integer").
?test(sheet1_C40, "/GT/", "C40", true).
?test(sheet1_D40, "/GT/", "D40", false).
?test(sheet1_E40, "/GT/", "E40", false).
?test(sheet1_F40, "/GT/", "F40", '#DIV/0!').
?test(sheet1_G40, "/GT/", "G40", '#N/A').
?test(sheet1_H40, "/GT/", "H40", '#NAME?').
?test(sheet1_I40, "/GT/", "I40", 'NULL!').
?test(sheet1_J40, "/GT/", "J40", '#NUM!').
?test(sheet1_K40, "/GT/", "K40", '#REF!').
?test(sheet1_L40, "/GT/", "L40", '#VALUE!').
?test(sheet1_M40, "/GT/", "M40", false).
?test(sheet1_N40, "/GT/", "N40", false).
?test(sheet1_O40, "/GT/", "O40", false).
?test(sheet1_P40, "/GT/", "P40", false).
?test(sheet1_Q40, "/GT/", "Q40", false).
?test(sheet1_R40, "/GT/", "R40", false).
?test(sheet1_S40, "/GT/", "S40", false).
?test(sheet1_T40, "/GT/", "T40", true).
?test(sheet1_U40, "/GT/", "U40", true).
?test(sheet1_V40, "/GT/", "V40", false).
?test(sheet1_A41, "/GT/", "A41", "Integer").
?test(sheet1_C41, "/GT/", "C41", true).
?test(sheet1_D41, "/GT/", "D41", false).
?test(sheet1_E41, "/GT/", "E41", false).
?test(sheet1_F41, "/GT/", "F41", '#DIV/0!').
?test(sheet1_G41, "/GT/", "G41", '#N/A').
?test(sheet1_H41, "/GT/", "H41", '#NAME?').
?test(sheet1_I41, "/GT/", "I41", 'NULL!').
?test(sheet1_J41, "/GT/", "J41", '#NUM!').
?test(sheet1_K41, "/GT/", "K41", '#REF!').
?test(sheet1_L41, "/GT/", "L41", '#VALUE!').
?test(sheet1_M41, "/GT/", "M41", false).
?test(sheet1_N41, "/GT/", "N41", false).
?test(sheet1_O41, "/GT/", "O41", false).
?test(sheet1_P41, "/GT/", "P41", false).
?test(sheet1_Q41, "/GT/", "Q41", false).
?test(sheet1_R41, "/GT/", "R41", true).
?test(sheet1_S41, "/GT/", "S41", false).
?test(sheet1_T41, "/GT/", "T41", true).
?test(sheet1_U41, "/GT/", "U41", true).
?test(sheet1_V41, "/GT/", "V41", false).
?test(sheet1_A42, "/GT/", "A42", "Zero").
?test(sheet1_C42, "/GT/", "C42", false).
?test(sheet1_D42, "/GT/", "D42", false).
?test(sheet1_E42, "/GT/", "E42", false).
?test(sheet1_F42, "/GT/", "F42", '#DIV/0!').
?test(sheet1_G42, "/GT/", "G42", '#N/A').
?test(sheet1_H42, "/GT/", "H42", '#NAME?').
?test(sheet1_I42, "/GT/", "I42", 'NULL!').
?test(sheet1_J42, "/GT/", "J42", '#NUM!').
?test(sheet1_K42, "/GT/", "K42", '#REF!').
?test(sheet1_L42, "/GT/", "L42", '#VALUE!').
?test(sheet1_M42, "/GT/", "M42", false).
?test(sheet1_N42, "/GT/", "N42", false).
?test(sheet1_O42, "/GT/", "O42", false).
?test(sheet1_P42, "/GT/", "P42", false).
?test(sheet1_Q42, "/GT/", "Q42", false).
?test(sheet1_R42, "/GT/", "R42", false).
?test(sheet1_S42, "/GT/", "S42", false).
?test(sheet1_T42, "/GT/", "T42", false).
?test(sheet1_U42, "/GT/", "U42", false).
?test(sheet1_V42, "/GT/", "V42", false).
?test(sheet1_A43, "/GT/", "A43", "Float").
?test(sheet1_C43, "/GT/", "C43", true).
?test(sheet1_D43, "/GT/", "D43", false).
?test(sheet1_E43, "/GT/", "E43", false).
?test(sheet1_F43, "/GT/", "F43", '#DIV/0!').
?test(sheet1_G43, "/GT/", "G43", '#N/A').
?test(sheet1_H43, "/GT/", "H43", '#NAME?').
?test(sheet1_I43, "/GT/", "I43", 'NULL!').
?test(sheet1_J43, "/GT/", "J43", '#NUM!').
?test(sheet1_K43, "/GT/", "K43", '#REF!').
?test(sheet1_L43, "/GT/", "L43", '#VALUE!').
?test(sheet1_M43, "/GT/", "M43", false).
?test(sheet1_N43, "/GT/", "N43", false).
?test(sheet1_O43, "/GT/", "O43", false).
?test(sheet1_P43, "/GT/", "P43", false).
?test(sheet1_Q43, "/GT/", "Q43", false).
?test(sheet1_R43, "/GT/", "R43", false).
?test(sheet1_S43, "/GT/", "S43", false).
?test(sheet1_T43, "/GT/", "T43", true).
?test(sheet1_U43, "/GT/", "U43", false).
?test(sheet1_V43, "/GT/", "V43", false).
?test(sheet1_A44, "/GT/", "A44", "Float").
?test(sheet1_C44, "/GT/", "C44", true).
?test(sheet1_D44, "/GT/", "D44", false).
?test(sheet1_E44, "/GT/", "E44", false).
?test(sheet1_F44, "/GT/", "F44", '#DIV/0!').
?test(sheet1_G44, "/GT/", "G44", '#N/A').
?test(sheet1_H44, "/GT/", "H44", '#NAME?').
?test(sheet1_I44, "/GT/", "I44", 'NULL!').
?test(sheet1_J44, "/GT/", "J44", '#NUM!').
?test(sheet1_K44, "/GT/", "K44", '#REF!').
?test(sheet1_L44, "/GT/", "L44", '#VALUE!').
?test(sheet1_M44, "/GT/", "M44", false).
?test(sheet1_N44, "/GT/", "N44", false).
?test(sheet1_O44, "/GT/", "O44", false).
?test(sheet1_P44, "/GT/", "P44", false).
?test(sheet1_Q44, "/GT/", "Q44", false).
?test(sheet1_R44, "/GT/", "R44", true).
?test(sheet1_S44, "/GT/", "S44", true).
?test(sheet1_T44, "/GT/", "T44", true).
?test(sheet1_U44, "/GT/", "U44", true).
?test(sheet1_V44, "/GT/", "V44", false).
?test(sheet1_A47, "/GT/", "A47", 400.0).
?test(sheet1_C47, "/GT/", "C47", 1.0).
?test(sheet1_D47, "/GT/", "D47", 1.0).
?test(sheet1_E47, "/GT/", "E47", 1.0).
?test(sheet1_F47, "/GT/", "F47", 1.0).
?test(sheet1_G47, "/GT/", "G47", 1.0).
?test(sheet1_H47, "/GT/", "H47", 1.0).
?test(sheet1_I47, "/GT/", "I47", 1.0).
?test(sheet1_J47, "/GT/", "J47", 1.0).
?test(sheet1_K47, "/GT/", "K47", 1.0).
?test(sheet1_L47, "/GT/", "L47", 1.0).
?test(sheet1_M47, "/GT/", "M47", 1.0).
?test(sheet1_N47, "/GT/", "N47", 1.0).
?test(sheet1_O47, "/GT/", "O47", 1.0).
?test(sheet1_P47, "/GT/", "P47", 1.0).
?test(sheet1_Q47, "/GT/", "Q47", 1.0).
?test(sheet1_R47, "/GT/", "R47", 1.0).
?test(sheet1_S47, "/GT/", "S47", 1.0).
?test(sheet1_T47, "/GT/", "T47", 1.0).
?test(sheet1_U47, "/GT/", "U47", 1.0).
?test(sheet1_V47, "/GT/", "V47", 1.0).
?test(sheet1_A48, "/GT/", "A48", "Success").
?test(sheet1_C48, "/GT/", "C48", 1.0).
?test(sheet1_D48, "/GT/", "D48", 1.0).
?test(sheet1_E48, "/GT/", "E48", 1.0).
?test(sheet1_F48, "/GT/", "F48", 1.0).
?test(sheet1_G48, "/GT/", "G48", 1.0).
?test(sheet1_H48, "/GT/", "H48", 1.0).
?test(sheet1_I48, "/GT/", "I48", 1.0).
?test(sheet1_J48, "/GT/", "J48", 1.0).
?test(sheet1_K48, "/GT/", "K48", 1.0).
?test(sheet1_L48, "/GT/", "L48", 1.0).
?test(sheet1_M48, "/GT/", "M48", 1.0).
?test(sheet1_N48, "/GT/", "N48", 1.0).
?test(sheet1_O48, "/GT/", "O48", 1.0).
?test(sheet1_P48, "/GT/", "P48", 1.0).
?test(sheet1_Q48, "/GT/", "Q48", 1.0).
?test(sheet1_R48, "/GT/", "R48", 1.0).
?test(sheet1_S48, "/GT/", "S48", 1.0).
?test(sheet1_T48, "/GT/", "T48", 1.0).
?test(sheet1_U48, "/GT/", "U48", 1.0).
?test(sheet1_V48, "/GT/", "V48", 1.0).
?test(sheet1_C49, "/GT/", "C49", 1.0).
?test(sheet1_D49, "/GT/", "D49", 1.0).
?test(sheet1_E49, "/GT/", "E49", 1.0).
?test(sheet1_F49, "/GT/", "F49", 1.0).
?test(sheet1_G49, "/GT/", "G49", 1.0).
?test(sheet1_H49, "/GT/", "H49", 1.0).
?test(sheet1_I49, "/GT/", "I49", 1.0).
?test(sheet1_J49, "/GT/", "J49", 1.0).
?test(sheet1_K49, "/GT/", "K49", 1.0).
?test(sheet1_L49, "/GT/", "L49", 1.0).
?test(sheet1_M49, "/GT/", "M49", 1.0).
?test(sheet1_N49, "/GT/", "N49", 1.0).
?test(sheet1_O49, "/GT/", "O49", 1.0).
?test(sheet1_P49, "/GT/", "P49", 1.0).
?test(sheet1_Q49, "/GT/", "Q49", 1.0).
?test(sheet1_R49, "/GT/", "R49", 1.0).
?test(sheet1_S49, "/GT/", "S49", 1.0).
?test(sheet1_T49, "/GT/", "T49", 1.0).
?test(sheet1_U49, "/GT/", "U49", 1.0).
?test(sheet1_V49, "/GT/", "V49", 1.0).
?test(sheet1_C50, "/GT/", "C50", 1.0).
?test(sheet1_D50, "/GT/", "D50", 1.0).
?test(sheet1_E50, "/GT/", "E50", 1.0).
?test(sheet1_F50, "/GT/", "F50", 1.0).
?test(sheet1_G50, "/GT/", "G50", 1.0).
?test(sheet1_H50, "/GT/", "H50", 1.0).
?test(sheet1_I50, "/GT/", "I50", 1.0).
?test(sheet1_J50, "/GT/", "J50", 1.0).
?test(sheet1_K50, "/GT/", "K50", 1.0).
?test(sheet1_L50, "/GT/", "L50", 1.0).
?test(sheet1_M50, "/GT/", "M50", 1.0).
?test(sheet1_N50, "/GT/", "N50", 1.0).
?test(sheet1_O50, "/GT/", "O50", 1.0).
?test(sheet1_P50, "/GT/", "P50", 1.0).
?test(sheet1_Q50, "/GT/", "Q50", 1.0).
?test(sheet1_R50, "/GT/", "R50", 1.0).
?test(sheet1_S50, "/GT/", "S50", 1.0).
?test(sheet1_T50, "/GT/", "T50", 1.0).
?test(sheet1_U50, "/GT/", "U50", 1.0).
?test(sheet1_V50, "/GT/", "V50", 1.0).
?test(sheet1_C51, "/GT/", "C51", 1.0).
?test(sheet1_D51, "/GT/", "D51", 1.0).
?test(sheet1_E51, "/GT/", "E51", 1.0).
?test(sheet1_F51, "/GT/", "F51", 1.0).
?test(sheet1_G51, "/GT/", "G51", 1.0).
?test(sheet1_H51, "/GT/", "H51", 1.0).
?test(sheet1_I51, "/GT/", "I51", 1.0).
?test(sheet1_J51, "/GT/", "J51", 1.0).
?test(sheet1_K51, "/GT/", "K51", 1.0).
?test(sheet1_L51, "/GT/", "L51", 1.0).
?test(sheet1_M51, "/GT/", "M51", 1.0).
?test(sheet1_N51, "/GT/", "N51", 1.0).
?test(sheet1_O51, "/GT/", "O51", 1.0).
?test(sheet1_P51, "/GT/", "P51", 1.0).
?test(sheet1_Q51, "/GT/", "Q51", 1.0).
?test(sheet1_R51, "/GT/", "R51", 1.0).
?test(sheet1_S51, "/GT/", "S51", 1.0).
?test(sheet1_T51, "/GT/", "T51", 1.0).
?test(sheet1_U51, "/GT/", "U51", 1.0).
?test(sheet1_V51, "/GT/", "V51", 1.0).
?test(sheet1_C52, "/GT/", "C52", 1.0).
?test(sheet1_D52, "/GT/", "D52", 1.0).
?test(sheet1_E52, "/GT/", "E52", 1.0).
?test(sheet1_F52, "/GT/", "F52", 1.0).
?test(sheet1_G52, "/GT/", "G52", 1.0).
?test(sheet1_H52, "/GT/", "H52", 1.0).
?test(sheet1_I52, "/GT/", "I52", 1.0).
?test(sheet1_J52, "/GT/", "J52", 1.0).
?test(sheet1_K52, "/GT/", "K52", 1.0).
?test(sheet1_L52, "/GT/", "L52", 1.0).
?test(sheet1_M52, "/GT/", "M52", 1.0).
?test(sheet1_N52, "/GT/", "N52", 1.0).
?test(sheet1_O52, "/GT/", "O52", 1.0).
?test(sheet1_P52, "/GT/", "P52", 1.0).
?test(sheet1_Q52, "/GT/", "Q52", 1.0).
?test(sheet1_R52, "/GT/", "R52", 1.0).
?test(sheet1_S52, "/GT/", "S52", 1.0).
?test(sheet1_T52, "/GT/", "T52", 1.0).
?test(sheet1_U52, "/GT/", "U52", 1.0).
?test(sheet1_V52, "/GT/", "V52", 1.0).
?test(sheet1_C53, "/GT/", "C53", 1.0).
?test(sheet1_D53, "/GT/", "D53", 1.0).
?test(sheet1_E53, "/GT/", "E53", 1.0).
?test(sheet1_F53, "/GT/", "F53", 1.0).
?test(sheet1_G53, "/GT/", "G53", 1.0).
?test(sheet1_H53, "/GT/", "H53", 1.0).
?test(sheet1_I53, "/GT/", "I53", 1.0).
?test(sheet1_J53, "/GT/", "J53", 1.0).
?test(sheet1_K53, "/GT/", "K53", 1.0).
?test(sheet1_L53, "/GT/", "L53", 1.0).
?test(sheet1_M53, "/GT/", "M53", 1.0).
?test(sheet1_N53, "/GT/", "N53", 1.0).
?test(sheet1_O53, "/GT/", "O53", 1.0).
?test(sheet1_P53, "/GT/", "P53", 1.0).
?test(sheet1_Q53, "/GT/", "Q53", 1.0).
?test(sheet1_R53, "/GT/", "R53", 1.0).
?test(sheet1_S53, "/GT/", "S53", 1.0).
?test(sheet1_T53, "/GT/", "T53", 1.0).
?test(sheet1_U53, "/GT/", "U53", 1.0).
?test(sheet1_V53, "/GT/", "V53", 1.0).
?test(sheet1_C54, "/GT/", "C54", 1.0).
?test(sheet1_D54, "/GT/", "D54", 1.0).
?test(sheet1_E54, "/GT/", "E54", 1.0).
?test(sheet1_F54, "/GT/", "F54", 1.0).
?test(sheet1_G54, "/GT/", "G54", 1.0).
?test(sheet1_H54, "/GT/", "H54", 1.0).
?test(sheet1_I54, "/GT/", "I54", 1.0).
?test(sheet1_J54, "/GT/", "J54", 1.0).
?test(sheet1_K54, "/GT/", "K54", 1.0).
?test(sheet1_L54, "/GT/", "L54", 1.0).
?test(sheet1_M54, "/GT/", "M54", 1.0).
?test(sheet1_N54, "/GT/", "N54", 1.0).
?test(sheet1_O54, "/GT/", "O54", 1.0).
?test(sheet1_P54, "/GT/", "P54", 1.0).
?test(sheet1_Q54, "/GT/", "Q54", 1.0).
?test(sheet1_R54, "/GT/", "R54", 1.0).
?test(sheet1_S54, "/GT/", "S54", 1.0).
?test(sheet1_T54, "/GT/", "T54", 1.0).
?test(sheet1_U54, "/GT/", "U54", 1.0).
?test(sheet1_V54, "/GT/", "V54", 1.0).
?test(sheet1_C55, "/GT/", "C55", 1.0).
?test(sheet1_D55, "/GT/", "D55", 1.0).
?test(sheet1_E55, "/GT/", "E55", 1.0).
?test(sheet1_F55, "/GT/", "F55", 1.0).
?test(sheet1_G55, "/GT/", "G55", 1.0).
?test(sheet1_H55, "/GT/", "H55", 1.0).
?test(sheet1_I55, "/GT/", "I55", 1.0).
?test(sheet1_J55, "/GT/", "J55", 1.0).
?test(sheet1_K55, "/GT/", "K55", 1.0).
?test(sheet1_L55, "/GT/", "L55", 1.0).
?test(sheet1_M55, "/GT/", "M55", 1.0).
?test(sheet1_N55, "/GT/", "N55", 1.0).
?test(sheet1_O55, "/GT/", "O55", 1.0).
?test(sheet1_P55, "/GT/", "P55", 1.0).
?test(sheet1_Q55, "/GT/", "Q55", 1.0).
?test(sheet1_R55, "/GT/", "R55", 1.0).
?test(sheet1_S55, "/GT/", "S55", 1.0).
?test(sheet1_T55, "/GT/", "T55", 1.0).
?test(sheet1_U55, "/GT/", "U55", 1.0).
?test(sheet1_V55, "/GT/", "V55", 1.0).
?test(sheet1_C56, "/GT/", "C56", 1.0).
?test(sheet1_D56, "/GT/", "D56", 1.0).
?test(sheet1_E56, "/GT/", "E56", 1.0).
?test(sheet1_F56, "/GT/", "F56", 1.0).
?test(sheet1_G56, "/GT/", "G56", 1.0).
?test(sheet1_H56, "/GT/", "H56", 1.0).
?test(sheet1_I56, "/GT/", "I56", 1.0).
?test(sheet1_J56, "/GT/", "J56", 1.0).
?test(sheet1_K56, "/GT/", "K56", 1.0).
?test(sheet1_L56, "/GT/", "L56", 1.0).
?test(sheet1_M56, "/GT/", "M56", 1.0).
?test(sheet1_N56, "/GT/", "N56", 1.0).
?test(sheet1_O56, "/GT/", "O56", 1.0).
?test(sheet1_P56, "/GT/", "P56", 1.0).
?test(sheet1_Q56, "/GT/", "Q56", 1.0).
?test(sheet1_R56, "/GT/", "R56", 1.0).
?test(sheet1_S56, "/GT/", "S56", 1.0).
?test(sheet1_T56, "/GT/", "T56", 1.0).
?test(sheet1_U56, "/GT/", "U56", 1.0).
?test(sheet1_V56, "/GT/", "V56", 1.0).
?test(sheet1_C57, "/GT/", "C57", 1.0).
?test(sheet1_D57, "/GT/", "D57", 1.0).
?test(sheet1_E57, "/GT/", "E57", 1.0).
?test(sheet1_F57, "/GT/", "F57", 1.0).
?test(sheet1_G57, "/GT/", "G57", 1.0).
?test(sheet1_H57, "/GT/", "H57", 1.0).
?test(sheet1_I57, "/GT/", "I57", 1.0).
?test(sheet1_J57, "/GT/", "J57", 1.0).
?test(sheet1_K57, "/GT/", "K57", 1.0).
?test(sheet1_L57, "/GT/", "L57", 1.0).
?test(sheet1_M57, "/GT/", "M57", 1.0).
?test(sheet1_N57, "/GT/", "N57", 1.0).
?test(sheet1_O57, "/GT/", "O57", 1.0).
?test(sheet1_P57, "/GT/", "P57", 1.0).
?test(sheet1_Q57, "/GT/", "Q57", 1.0).
?test(sheet1_R57, "/GT/", "R57", 1.0).
?test(sheet1_S57, "/GT/", "S57", 1.0).
?test(sheet1_T57, "/GT/", "T57", 1.0).
?test(sheet1_U57, "/GT/", "U57", 1.0).
?test(sheet1_V57, "/GT/", "V57", 1.0).
?test(sheet1_C58, "/GT/", "C58", 1.0).
?test(sheet1_D58, "/GT/", "D58", 1.0).
?test(sheet1_E58, "/GT/", "E58", 1.0).
?test(sheet1_F58, "/GT/", "F58", 1.0).
?test(sheet1_G58, "/GT/", "G58", 1.0).
?test(sheet1_H58, "/GT/", "H58", 1.0).
?test(sheet1_I58, "/GT/", "I58", 1.0).
?test(sheet1_J58, "/GT/", "J58", 1.0).
?test(sheet1_K58, "/GT/", "K58", 1.0).
?test(sheet1_L58, "/GT/", "L58", 1.0).
?test(sheet1_M58, "/GT/", "M58", 1.0).
?test(sheet1_N58, "/GT/", "N58", 1.0).
?test(sheet1_O58, "/GT/", "O58", 1.0).
?test(sheet1_P58, "/GT/", "P58", 1.0).
?test(sheet1_Q58, "/GT/", "Q58", 1.0).
?test(sheet1_R58, "/GT/", "R58", 1.0).
?test(sheet1_S58, "/GT/", "S58", 1.0).
?test(sheet1_T58, "/GT/", "T58", 1.0).
?test(sheet1_U58, "/GT/", "U58", 1.0).
?test(sheet1_V58, "/GT/", "V58", 1.0).
?test(sheet1_C59, "/GT/", "C59", 1.0).
?test(sheet1_D59, "/GT/", "D59", 1.0).
?test(sheet1_E59, "/GT/", "E59", 1.0).
?test(sheet1_F59, "/GT/", "F59", 1.0).
?test(sheet1_G59, "/GT/", "G59", 1.0).
?test(sheet1_H59, "/GT/", "H59", 1.0).
?test(sheet1_I59, "/GT/", "I59", 1.0).
?test(sheet1_J59, "/GT/", "J59", 1.0).
?test(sheet1_K59, "/GT/", "K59", 1.0).
?test(sheet1_L59, "/GT/", "L59", 1.0).
?test(sheet1_M59, "/GT/", "M59", 1.0).
?test(sheet1_N59, "/GT/", "N59", 1.0).
?test(sheet1_O59, "/GT/", "O59", 1.0).
?test(sheet1_P59, "/GT/", "P59", 1.0).
?test(sheet1_Q59, "/GT/", "Q59", 1.0).
?test(sheet1_R59, "/GT/", "R59", 1.0).
?test(sheet1_S59, "/GT/", "S59", 1.0).
?test(sheet1_T59, "/GT/", "T59", 1.0).
?test(sheet1_U59, "/GT/", "U59", 1.0).
?test(sheet1_V59, "/GT/", "V59", 1.0).
?test(sheet1_C60, "/GT/", "C60", 1.0).
?test(sheet1_D60, "/GT/", "D60", 1.0).
?test(sheet1_E60, "/GT/", "E60", 1.0).
?test(sheet1_F60, "/GT/", "F60", 1.0).
?test(sheet1_G60, "/GT/", "G60", 1.0).
?test(sheet1_H60, "/GT/", "H60", 1.0).
?test(sheet1_I60, "/GT/", "I60", 1.0).
?test(sheet1_J60, "/GT/", "J60", 1.0).
?test(sheet1_K60, "/GT/", "K60", 1.0).
?test(sheet1_L60, "/GT/", "L60", 1.0).
?test(sheet1_M60, "/GT/", "M60", 1.0).
?test(sheet1_N60, "/GT/", "N60", 1.0).
?test(sheet1_O60, "/GT/", "O60", 1.0).
?test(sheet1_P60, "/GT/", "P60", 1.0).
?test(sheet1_Q60, "/GT/", "Q60", 1.0).
?test(sheet1_R60, "/GT/", "R60", 1.0).
?test(sheet1_S60, "/GT/", "S60", 1.0).
?test(sheet1_T60, "/GT/", "T60", 1.0).
?test(sheet1_U60, "/GT/", "U60", 1.0).
?test(sheet1_V60, "/GT/", "V60", 1.0).
?test(sheet1_C61, "/GT/", "C61", 1.0).
?test(sheet1_D61, "/GT/", "D61", 1.0).
?test(sheet1_E61, "/GT/", "E61", 1.0).
?test(sheet1_F61, "/GT/", "F61", 1.0).
?test(sheet1_G61, "/GT/", "G61", 1.0).
?test(sheet1_H61, "/GT/", "H61", 1.0).
?test(sheet1_I61, "/GT/", "I61", 1.0).
?test(sheet1_J61, "/GT/", "J61", 1.0).
?test(sheet1_K61, "/GT/", "K61", 1.0).
?test(sheet1_L61, "/GT/", "L61", 1.0).
?test(sheet1_M61, "/GT/", "M61", 1.0).
?test(sheet1_N61, "/GT/", "N61", 1.0).
?test(sheet1_O61, "/GT/", "O61", 1.0).
?test(sheet1_P61, "/GT/", "P61", 1.0).
?test(sheet1_Q61, "/GT/", "Q61", 1.0).
?test(sheet1_R61, "/GT/", "R61", 1.0).
?test(sheet1_S61, "/GT/", "S61", 1.0).
?test(sheet1_T61, "/GT/", "T61", 1.0).
?test(sheet1_U61, "/GT/", "U61", 1.0).
?test(sheet1_V61, "/GT/", "V61", 1.0).
?test(sheet1_C62, "/GT/", "C62", 1.0).
?test(sheet1_D62, "/GT/", "D62", 1.0).
?test(sheet1_E62, "/GT/", "E62", 1.0).
?test(sheet1_F62, "/GT/", "F62", 1.0).
?test(sheet1_G62, "/GT/", "G62", 1.0).
?test(sheet1_H62, "/GT/", "H62", 1.0).
?test(sheet1_I62, "/GT/", "I62", 1.0).
?test(sheet1_J62, "/GT/", "J62", 1.0).
?test(sheet1_K62, "/GT/", "K62", 1.0).
?test(sheet1_L62, "/GT/", "L62", 1.0).
?test(sheet1_M62, "/GT/", "M62", 1.0).
?test(sheet1_N62, "/GT/", "N62", 1.0).
?test(sheet1_O62, "/GT/", "O62", 1.0).
?test(sheet1_P62, "/GT/", "P62", 1.0).
?test(sheet1_Q62, "/GT/", "Q62", 1.0).
?test(sheet1_R62, "/GT/", "R62", 1.0).
?test(sheet1_S62, "/GT/", "S62", 1.0).
?test(sheet1_T62, "/GT/", "T62", 1.0).
?test(sheet1_U62, "/GT/", "U62", 1.0).
?test(sheet1_V62, "/GT/", "V62", 1.0).
?test(sheet1_C63, "/GT/", "C63", 1.0).
?test(sheet1_D63, "/GT/", "D63", 1.0).
?test(sheet1_E63, "/GT/", "E63", 1.0).
?test(sheet1_F63, "/GT/", "F63", 1.0).
?test(sheet1_G63, "/GT/", "G63", 1.0).
?test(sheet1_H63, "/GT/", "H63", 1.0).
?test(sheet1_I63, "/GT/", "I63", 1.0).
?test(sheet1_J63, "/GT/", "J63", 1.0).
?test(sheet1_K63, "/GT/", "K63", 1.0).
?test(sheet1_L63, "/GT/", "L63", 1.0).
?test(sheet1_M63, "/GT/", "M63", 1.0).
?test(sheet1_N63, "/GT/", "N63", 1.0).
?test(sheet1_O63, "/GT/", "O63", 1.0).
?test(sheet1_P63, "/GT/", "P63", 1.0).
?test(sheet1_Q63, "/GT/", "Q63", 1.0).
?test(sheet1_R63, "/GT/", "R63", 1.0).
?test(sheet1_S63, "/GT/", "S63", 1.0).
?test(sheet1_T63, "/GT/", "T63", 1.0).
?test(sheet1_U63, "/GT/", "U63", 1.0).
?test(sheet1_V63, "/GT/", "V63", 1.0).
?test(sheet1_C64, "/GT/", "C64", 1.0).
?test(sheet1_D64, "/GT/", "D64", 1.0).
?test(sheet1_E64, "/GT/", "E64", 1.0).
?test(sheet1_F64, "/GT/", "F64", 1.0).
?test(sheet1_G64, "/GT/", "G64", 1.0).
?test(sheet1_H64, "/GT/", "H64", 1.0).
?test(sheet1_I64, "/GT/", "I64", 1.0).
?test(sheet1_J64, "/GT/", "J64", 1.0).
?test(sheet1_K64, "/GT/", "K64", 1.0).
?test(sheet1_L64, "/GT/", "L64", 1.0).
?test(sheet1_M64, "/GT/", "M64", 1.0).
?test(sheet1_N64, "/GT/", "N64", 1.0).
?test(sheet1_O64, "/GT/", "O64", 1.0).
?test(sheet1_P64, "/GT/", "P64", 1.0).
?test(sheet1_Q64, "/GT/", "Q64", 1.0).
?test(sheet1_R64, "/GT/", "R64", 1.0).
?test(sheet1_S64, "/GT/", "S64", 1.0).
?test(sheet1_T64, "/GT/", "T64", 1.0).
?test(sheet1_U64, "/GT/", "U64", 1.0).
?test(sheet1_V64, "/GT/", "V64", 1.0).
?test(sheet1_C65, "/GT/", "C65", 1.0).
?test(sheet1_D65, "/GT/", "D65", 1.0).
?test(sheet1_E65, "/GT/", "E65", 1.0).
?test(sheet1_F65, "/GT/", "F65", 1.0).
?test(sheet1_G65, "/GT/", "G65", 1.0).
?test(sheet1_H65, "/GT/", "H65", 1.0).
?test(sheet1_I65, "/GT/", "I65", 1.0).
?test(sheet1_J65, "/GT/", "J65", 1.0).
?test(sheet1_K65, "/GT/", "K65", 1.0).
?test(sheet1_L65, "/GT/", "L65", 1.0).
?test(sheet1_M65, "/GT/", "M65", 1.0).
?test(sheet1_N65, "/GT/", "N65", 1.0).
?test(sheet1_O65, "/GT/", "O65", 1.0).
?test(sheet1_P65, "/GT/", "P65", 1.0).
?test(sheet1_Q65, "/GT/", "Q65", 1.0).
?test(sheet1_R65, "/GT/", "R65", 1.0).
?test(sheet1_S65, "/GT/", "S65", 1.0).
?test(sheet1_T65, "/GT/", "T65", 1.0).
?test(sheet1_U65, "/GT/", "U65", 1.0).
?test(sheet1_V65, "/GT/", "V65", 1.0).
?test(sheet1_C66, "/GT/", "C66", 1.0).
?test(sheet1_D66, "/GT/", "D66", 1.0).
?test(sheet1_E66, "/GT/", "E66", 1.0).
?test(sheet1_F66, "/GT/", "F66", 1.0).
?test(sheet1_G66, "/GT/", "G66", 1.0).
?test(sheet1_H66, "/GT/", "H66", 1.0).
?test(sheet1_I66, "/GT/", "I66", 1.0).
?test(sheet1_J66, "/GT/", "J66", 1.0).
?test(sheet1_K66, "/GT/", "K66", 1.0).
?test(sheet1_L66, "/GT/", "L66", 1.0).
?test(sheet1_M66, "/GT/", "M66", 1.0).
?test(sheet1_N66, "/GT/", "N66", 1.0).
?test(sheet1_O66, "/GT/", "O66", 1.0).
?test(sheet1_P66, "/GT/", "P66", 1.0).
?test(sheet1_Q66, "/GT/", "Q66", 1.0).
?test(sheet1_R66, "/GT/", "R66", 1.0).
?test(sheet1_S66, "/GT/", "S66", 1.0).
?test(sheet1_T66, "/GT/", "T66", 1.0).
?test(sheet1_U66, "/GT/", "U66", 1.0).
?test(sheet1_V66, "/GT/", "V66", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "e_gnumeric_operators_gt.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "e_gnumeric_operators_gt" ++ "/" ++ Sheetname ++ "/",
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
