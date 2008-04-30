%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_engineering_SUITE).
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
                     [Testcase, "d_gnumeric_engineering_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_engineering" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "ENGINEERING FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_C3, "/Sheet1/", "C3", "Accuracy Limit").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_C4, "/Sheet1/", "C4", 0.0001).
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 38.0).
?test(sheet1_B8, "/Sheet1/", "B8", 38.0).
?test(sheet1_A12, "/Sheet1/", "A12", "Function").
?test(sheet1_B12, "/Sheet1/", "B12", "1st test").
?test(sheet1_C12, "/Sheet1/", "C12", "Correct").
?test(sheet1_D12, "/Sheet1/", "D12", "2nd test").
?test(sheet1_E12, "/Sheet1/", "E12", "Correct").
?test(sheet1_F12, "/Sheet1/", "F12", "3rd test").
?test(sheet1_G12, "/Sheet1/", "G12", "Correct").
?test(sheet1_H12, "/Sheet1/", "H12", "Status").
?test(sheet1_I12, "/Sheet1/", "I12", "Status message").
?test(sheet1_A13, "/Sheet1/", "A13", "BESSELI").
?test(sheet1_B13, "/Sheet1/", "B13", 0.00736737336693427).
?test(sheet1_C13, "/Sheet1/", "C13", 0.00736737336693427).
?test(sheet1_D13, "/Sheet1/", "D13", 0.0221684244039833).
?test(sheet1_E13, "/Sheet1/", "E13", 0.0221684244039833).
?test(sheet1_F13, "/Sheet1/", "F13", 52.3192931787635).
?test(sheet1_G13, "/Sheet1/", "G13", 52.3192931787635).
?test(sheet1_H13, "/Sheet1/", "H13", 1.0).
?test(sheet1_I13, "/Sheet1/", "I13", "Ok.").
?test(sheet1_A14, "/Sheet1/", "A14", "BESSELJ").
?test(sheet1_B14, "/Sheet1/", "B14", 0.0139740040278808).
?test(sheet1_C14, "/Sheet1/", "C14", 0.0139740040278808).
?test(sheet1_D14, "/Sheet1/", "D14", -0.291125241852537).
?test(sheet1_E14, "/Sheet1/", "E14", -0.291132206926025).
?test(sheet1_F14, "/Sheet1/", "F14", 0.430171471153396).
?test(sheet1_G14, "/Sheet1/", "G14", 0.430171471153396).
?test(sheet1_H14, "/Sheet1/", "H14", 1.0).
?test(sheet1_I14, "/Sheet1/", "I14", "Ok.").
?test(sheet1_A15, "/Sheet1/", "A15", "BESSELK").
?test(sheet1_B15, "/Sheet1/", "B15", 397.958801062385).
?test(sheet1_C15, "/Sheet1/", "C15", 397.958801062385).
?test(sheet1_D15, "/Sheet1/", "D15", 305.5380222).
?test(sheet1_E15, "/Sheet1/", "E15", 305.5380222).
?test(sheet1_F15, "/Sheet1/", "F15", 292.999196180751).
?test(sheet1_G15, "/Sheet1/", "G15", 292.999196180751).
?test(sheet1_H15, "/Sheet1/", "H15", 1.0).
?test(sheet1_I15, "/Sheet1/", "I15", "Ok.").
?test(sheet1_A16, "/Sheet1/", "A16", "BESSELY").
?test(sheet1_B16, "/Sheet1/", "B16", 0.21590359910699).
?test(sheet1_C16, "/Sheet1/", "C16", 0.21590359910699).
?test(sheet1_D16, "/Sheet1/", "D16", -0.955809329542292).
?test(sheet1_E16, "/Sheet1/", "E16", -0.955809329542292).
?test(sheet1_F16, "/Sheet1/", "F16", -2.29066097415494).
?test(sheet1_G16, "/Sheet1/", "G16", -2.29066097415494).
?test(sheet1_H16, "/Sheet1/", "H16", 1.0).
?test(sheet1_I16, "/Sheet1/", "I16", "Ok.").
?test(sheet1_A17, "/Sheet1/", "A17", "BIN2DEC").
?test(sheet1_B17, "/Sheet1/", "B17", 5.0).
?test(sheet1_C17, "/Sheet1/", "C17", 5.0).
?test(sheet1_D17, "/Sheet1/", "D17", 281.0).
?test(sheet1_E17, "/Sheet1/", "E17", 281.0).
?test(sheet1_F17, "/Sheet1/", "F17", -25.0).
?test(sheet1_G17, "/Sheet1/", "G17", -25.0).
?test(sheet1_H17, "/Sheet1/", "H17", 1.0).
?test(sheet1_I17, "/Sheet1/", "I17", "Ok.").
?test(sheet1_A18, "/Sheet1/", "A18", "BIN2HEX").
?test(sheet1_B18, "/Sheet1/", "B18", "27").
?test(sheet1_C18, "/Sheet1/", "C18", "27").
?test(sheet1_D18, "/Sheet1/", "D18", "BB").
?test(sheet1_E18, "/Sheet1/", "E18", "BB").
?test(sheet1_F18, "/Sheet1/", "F18", "16F").
?test(sheet1_G18, "/Sheet1/", "G18", "16F").
?test(sheet1_H18, "/Sheet1/", "H18", 1.0).
?test(sheet1_I18, "/Sheet1/", "I18", "Ok.").
?test(sheet1_A19, "/Sheet1/", "A19", "BIN2OCT").
?test(sheet1_B19, "/Sheet1/", "B19", "67").
?test(sheet1_C19, "/Sheet1/", "C19", "67").
?test(sheet1_D19, "/Sheet1/", "D19", "273").
?test(sheet1_E19, "/Sheet1/", "E19", "273").
?test(sheet1_F19, "/Sheet1/", "F19", "673").
?test(sheet1_G19, "/Sheet1/", "G19", "673").
?test(sheet1_H19, "/Sheet1/", "H19", 1.0).
?test(sheet1_I19, "/Sheet1/", "I19", "Ok.").
?test(sheet1_A20, "/Sheet1/", "A20", "COMPLEX").
?test(sheet1_B20, "/Sheet1/", "B20", "1-i").
?test(sheet1_C20, "/Sheet1/", "C20", "1-i").
?test(sheet1_D20, "/Sheet1/", "D20", "i").
?test(sheet1_E20, "/Sheet1/", "E20", "i").
?test(sheet1_F20, "/Sheet1/", "F20", "12-2i").
?test(sheet1_G20, "/Sheet1/", "G20", "12-2i").
?test(sheet1_H20, "/Sheet1/", "H20", 1.0).
?test(sheet1_I20, "/Sheet1/", "I20", "Ok.").
?test(sheet1_A21, "/Sheet1/", "A21", "CONVERT").
?test(sheet1_B21, "/Sheet1/", "B21", 1360.77692924643).
?test(sheet1_C21, "/Sheet1/", "C21", 1360.77692924643).
?test(sheet1_D21, "/Sheet1/", "D21", 228.346456692913).
?test(sheet1_E21, "/Sheet1/", "E21", 228.346456692913).
?test(sheet1_F21, "/Sheet1/", "F21", 33.0756792845004).
?test(sheet1_G21, "/Sheet1/", "G21", 33.0756792845004).
?test(sheet1_H21, "/Sheet1/", "H21", 1.0).
?test(sheet1_I21, "/Sheet1/", "I21", "Ok.").
?test(sheet1_A22, "/Sheet1/", "A22", "DEC2BIN").
?test(sheet1_B22, "/Sheet1/", "B22", "101010").
?test(sheet1_C22, "/Sheet1/", "C22", "101010").
?test(sheet1_D22, "/Sheet1/", "D22", "1111111110").
?test(sheet1_E22, "/Sheet1/", "E22", "1111111110").
?test(sheet1_F22, "/Sheet1/", "F22", "111").
?test(sheet1_G22, "/Sheet1/", "G22", "111").
?test(sheet1_H22, "/Sheet1/", "H22", 1.0).
?test(sheet1_I22, "/Sheet1/", "I22", "Ok.").
?test(sheet1_A23, "/Sheet1/", "A23", "DEC2HEX").
?test(sheet1_B23, "/Sheet1/", "B23", "2A").
?test(sheet1_C23, "/Sheet1/", "C23", "2A").
?test(sheet1_D23, "/Sheet1/", "D23", "FFFFFFFFFE").
?test(sheet1_E23, "/Sheet1/", "E23", "FFFFFFFFFE").
?test(sheet1_F23, "/Sheet1/", "F23", "C").
?test(sheet1_G23, "/Sheet1/", "G23", "C").
?test(sheet1_H23, "/Sheet1/", "H23", 1.0).
?test(sheet1_I23, "/Sheet1/", "I23", "Ok.").
?test(sheet1_A24, "/Sheet1/", "A24", "DEC2OCT").
?test(sheet1_B24, "/Sheet1/", "B24", "52").
?test(sheet1_C24, "/Sheet1/", "C24", "52").
?test(sheet1_D24, "/Sheet1/", "D24", "7777777776").
?test(sheet1_E24, "/Sheet1/", "E24", "7777777776").
?test(sheet1_F24, "/Sheet1/", "F24", "21015").
?test(sheet1_G24, "/Sheet1/", "G24", "21015").
?test(sheet1_H24, "/Sheet1/", "H24", 1.0).
?test(sheet1_I24, "/Sheet1/", "I24", "Ok.").
?test(sheet1_A25, "/Sheet1/", "A25", "DELTA").
?test(sheet1_B25, "/Sheet1/", "B25", 0.0).
?test(sheet1_C25, "/Sheet1/", "C25", 0.0).
?test(sheet1_D25, "/Sheet1/", "D25", 1.0).
?test(sheet1_E25, "/Sheet1/", "E25", 1.0).
?test(sheet1_F25, "/Sheet1/", "F25", 1.0).
?test(sheet1_G25, "/Sheet1/", "G25", 1.0).
?test(sheet1_H25, "/Sheet1/", "H25", 1.0).
?test(sheet1_I25, "/Sheet1/", "I25", "Ok.").
?test(sheet1_A26, "/Sheet1/", "A26", "ERF").
?test(sheet1_B26, "/Sheet1/", "B26", 0.428392351896386).
?test(sheet1_C26, "/Sheet1/", "C26", 0.428392351896386).
?test(sheet1_D26, "/Sheet1/", "D26", 0.222702589127257).
?test(sheet1_E26, "/Sheet1/", "E26", 0.222702589127258).
?test(sheet1_F26, "/Sheet1/", "F26", 0.796908112895256).
?test(sheet1_G26, "/Sheet1/", "G26", 0.796908112895256).
?test(sheet1_H26, "/Sheet1/", "H26", 1.0).
?test(sheet1_I26, "/Sheet1/", "I26", "Ok.").
?test(sheet1_A27, "/Sheet1/", "A27", "GESTEP").
?test(sheet1_B27, "/Sheet1/", "B27", 1.0).
?test(sheet1_C27, "/Sheet1/", "C27", 1.0).
?test(sheet1_D27, "/Sheet1/", "D27", 1.0).
?test(sheet1_E27, "/Sheet1/", "E27", 1.0).
?test(sheet1_F27, "/Sheet1/", "F27", 0.0).
?test(sheet1_G27, "/Sheet1/", "G27", 0.0).
?test(sheet1_H27, "/Sheet1/", "H27", 1.0).
?test(sheet1_I27, "/Sheet1/", "I27", "Ok.").
?test(sheet1_A28, "/Sheet1/", "A28", "HEX2BIN").
?test(sheet1_B28, "/Sheet1/", "B28", "101010").
?test(sheet1_C28, "/Sheet1/", "C28", "101010").
?test(sheet1_D28, "/Sheet1/", "D28", "11111111").
?test(sheet1_E28, "/Sheet1/", "E28", "11111111").
?test(sheet1_F28, "/Sheet1/", "F28", "11010").
?test(sheet1_G28, "/Sheet1/", "G28", "11010").
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_A29, "/Sheet1/", "A29", "HEX2DEC").
?test(sheet1_B29, "/Sheet1/", "B29", 42.0).
?test(sheet1_C29, "/Sheet1/", "C29", 42.0).
?test(sheet1_D29, "/Sheet1/", "D29", 4095.0).
?test(sheet1_E29, "/Sheet1/", "E29", 4095.0).
?test(sheet1_F29, "/Sheet1/", "F29", 16299.0).
?test(sheet1_G29, "/Sheet1/", "G29", 16299.0).
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_A30, "/Sheet1/", "A30", "HEX2OCT").
?test(sheet1_B30, "/Sheet1/", "B30", "52").
?test(sheet1_C30, "/Sheet1/", "C30", "52").
?test(sheet1_D30, "/Sheet1/", "D30", "37").
?test(sheet1_E30, "/Sheet1/", "E30", "37").
?test(sheet1_F30, "/Sheet1/", "F30", "177777").
?test(sheet1_G30, "/Sheet1/", "G30", "177777").
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_A31, "/Sheet1/", "A31", "IMABS").
?test(sheet1_B31, "/Sheet1/", "B31", 2.23606797749979).
?test(sheet1_C31, "/Sheet1/", "C31", 2.23606797749979).
?test(sheet1_D31, "/Sheet1/", "D31", 8.54400374531753).
?test(sheet1_E31, "/Sheet1/", "E31", 8.54400374531753).
?test(sheet1_F31, "/Sheet1/", "F31", 1.0).
?test(sheet1_G31, "/Sheet1/", "G31", 1.0).
?test(sheet1_H31, "/Sheet1/", "H31", 1.0).
?test(sheet1_I31, "/Sheet1/", "I31", "Ok.").
?test(sheet1_A32, "/Sheet1/", "A32", "IMAGINARY").
?test(sheet1_B32, "/Sheet1/", "B32", -1.0).
?test(sheet1_C32, "/Sheet1/", "C32", -1.0).
?test(sheet1_D32, "/Sheet1/", "D32", 1.0).
?test(sheet1_E32, "/Sheet1/", "E32", 1.0).
?test(sheet1_F32, "/Sheet1/", "F32", 21.0).
?test(sheet1_G32, "/Sheet1/", "G32", 21.0).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "IMARGUMENT").
?test(sheet1_B33, "/Sheet1/", "B33", -0.463647609000806).
?test(sheet1_C33, "/Sheet1/", "C33", -0.463647609000806).
?test(sheet1_D33, "/Sheet1/", "D33", 1.5707963267949).
?test(sheet1_E33, "/Sheet1/", "E33", 1.5707963267949).
?test(sheet1_F33, "/Sheet1/", "F33", -1.38257482149013).
?test(sheet1_G33, "/Sheet1/", "G33", -1.38257482149013).
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "IMCONJUGATE").
?test(sheet1_B34, "/Sheet1/", "B34", "1+j").
?test(sheet1_C34, "/Sheet1/", "C34", "1+j").
?test(sheet1_D34, "/Sheet1/", "D34", "4-2j").
?test(sheet1_E34, "/Sheet1/", "E34", "4-2j").
?test(sheet1_F34, "/Sheet1/", "F34", "5+2j").
?test(sheet1_G34, "/Sheet1/", "G34", "5+2j").
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "IMCOS").
?test(sheet1_B35, "/Sheet1/", "B35", "0.833730025131149-0.988897705762865j").
?test(sheet1_C35, "/Sheet1/", "C35", "0.833730025131149-0.988897705762865j").
?test(sheet1_D35, "/Sheet1/", "D35", "-3.72454550491532+0.511822569987385j").
?test(sheet1_E35, "/Sheet1/", "E35", "-3.72454550491532+0.511822569987385j").
?test(sheet1_F35, "/Sheet1/", "F35", "1149.26926544734-3885.12187972205j").
?test(sheet1_G35, "/Sheet1/", "G35", "1149.26926544734-3885.12187972205j").
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_J35, "/Sheet1/", "J35", 1.0).
?test(sheet1_K35, "/Sheet1/", "K35", 1.0).
?test(sheet1_L35, "/Sheet1/", "L35", 1.0).
?test(sheet1_A36, "/Sheet1/", "A36", "IMDIV").
?test(sheet1_B36, "/Sheet1/", "B36", "0.6-0.8j").
?test(sheet1_C36, "/Sheet1/", "C36", "0.6-0.8j").
?test(sheet1_D36, "/Sheet1/", "D36", "0.764705882352941-0.0588235294117647j").
?test(sheet1_E36, "/Sheet1/", "E36", "0.764705882352941-5.88235294117647E-002j").
?test(sheet1_F36, "/Sheet1/", "F36", "0.4-0.2j").
?test(sheet1_G36, "/Sheet1/", "G36", "0.4-0.2j").
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_J36, "/Sheet1/", "J36", 1.0).
?test(sheet1_K36, "/Sheet1/", "K36", 1.0).
?test(sheet1_L36, "/Sheet1/", "L36", 1.0).
?test(sheet1_A37, "/Sheet1/", "A37", "IMEXP").
?test(sheet1_B37, "/Sheet1/", "B37", "3.99232404844127-6.21767631236797j").
?test(sheet1_C37, "/Sheet1/", "C37", "3.99232404844127-6.21767631236797j").
?test(sheet1_D37, "/Sheet1/", "D37", "-22.7208474176192-49.6459573345806j").
?test(sheet1_E37, "/Sheet1/", "E37", "-22.7208474176192-49.6459573345806j").
?test(sheet1_F37, "/Sheet1/", "F37", "-3372.07274273649-7368.11336469684j").
?test(sheet1_G37, "/Sheet1/", "G37", "-3372.07274273649-7368.11336469684j").
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_J37, "/Sheet1/", "J37", 1.0).
?test(sheet1_K37, "/Sheet1/", "K37", 1.0).
?test(sheet1_L37, "/Sheet1/", "L37", 1.0).
?test(sheet1_A38, "/Sheet1/", "A38", "IMLN").
?test(sheet1_B38, "/Sheet1/", "B38", "1.15129254649702-0.321750554396642j").
?test(sheet1_C38, "/Sheet1/", "C38", "1.15129254649702-0.321750554396642j").
?test(sheet1_D38, "/Sheet1/", "D38", "1.99300834266824-0.304510883202113j").
?test(sheet1_E38, "/Sheet1/", "E38", "1.99300834266824-0.304510883202113j").
?test(sheet1_F38, "/Sheet1/", "F38", "2.27972016714096-1.2263959247521j").
?test(sheet1_G38, "/Sheet1/", "G38", "2.27972016714096-1.2263959247521j").
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_J38, "/Sheet1/", "J38", 1.0).
?test(sheet1_K38, "/Sheet1/", "K38", 1.0).
?test(sheet1_L38, "/Sheet1/", "L38", 1.0).
?test(sheet1_A39, "/Sheet1/", "A39", "IMLOG10").
?test(sheet1_B39, "/Sheet1/", "B39", "0.5-0.139734490323774j").
?test(sheet1_C39, "/Sheet1/", "C39", "0.5-0.139734490323774j").
?test(sheet1_D39, "/Sheet1/", "D39", "0.86555252560796-0.132247396254163j").
?test(sheet1_E39, "/Sheet1/", "E39", "0.86555252560796-0.132247396254163j").
?test(sheet1_F39, "/Sheet1/", "F39", "0.990069888872877-0.532616982748472j").
?test(sheet1_G39, "/Sheet1/", "G39", "0.990069888872877-0.532616982748472j").
?test(sheet1_H39, "/Sheet1/", "H39", 1.0).
?test(sheet1_I39, "/Sheet1/", "I39", "Ok.").
?test(sheet1_J39, "/Sheet1/", "J39", 1.0).
?test(sheet1_K39, "/Sheet1/", "K39", 1.0).
?test(sheet1_L39, "/Sheet1/", "L39", 1.0).
?test(sheet1_A40, "/Sheet1/", "A40", "IMLOG2").
?test(sheet1_B40, "/Sheet1/", "B40", "1.66096404757152-0.464187929267036j").
?test(sheet1_C40, "/Sheet1/", "C40", "1.66096404757152-0.464187929267036j").
?test(sheet1_D40, "/Sheet1/", "D40", "2.87530325263909-0.439316341126218j").
?test(sheet1_E40, "/Sheet1/", "E40", "2.87530325263909-0.439316341126218j").
?test(sheet1_F40, "/Sheet1/", "F40", "3.28894098000195-1.76931531894246j").
?test(sheet1_G40, "/Sheet1/", "G40", "3.28894098000195-1.76931531894246j").
?test(sheet1_H40, "/Sheet1/", "H40", 1.0).
?test(sheet1_I40, "/Sheet1/", "I40", "Ok.").
?test(sheet1_J40, "/Sheet1/", "J40", 1.0).
?test(sheet1_K40, "/Sheet1/", "K40", 1.0).
?test(sheet1_L40, "/Sheet1/", "L40", 1.0).
?test(sheet1_A41, "/Sheet1/", "A41", "IMPOWER").
?test(sheet1_B41, "/Sheet1/", "B41", "15-8j").
?test(sheet1_C41, "/Sheet1/", "C41", "15-8j").
?test(sheet1_D41, "/Sheet1/", "D41", "-4.23431261082551+0.670649233024723j").
?test(sheet1_E41, "/Sheet1/", "E41", "-4.23431261082551+0.670649233024723j").
?test(sheet1_F41, "/Sheet1/", "F41", "-169691027.479611-208555947.133373j").
?test(sheet1_G41, "/Sheet1/", "G41", "-169691027.47961-208555947.133372j").
?test(sheet1_H41, "/Sheet1/", "H41", 1.0).
?test(sheet1_I41, "/Sheet1/", "I41", "Ok.").
?test(sheet1_J41, "/Sheet1/", "J41", 1.0).
?test(sheet1_K41, "/Sheet1/", "K41", 1.0).
?test(sheet1_L41, "/Sheet1/", "L41", 1.0).
?test(sheet1_A42, "/Sheet1/", "A42", "IMPRODUCT").
?test(sheet1_B42, "/Sheet1/", "B42", "6-8j").
?test(sheet1_C42, "/Sheet1/", "C42", "6-8j").
?test(sheet1_D42, "/Sheet1/", "D42", "43-66j").
?test(sheet1_E42, "/Sheet1/", "E42", "43-66j").
?test(sheet1_F42, "/Sheet1/", "F42", "38-84j").
?test(sheet1_G42, "/Sheet1/", "G42", "38-84j").
?test(sheet1_H42, "/Sheet1/", "H42", 1.0).
?test(sheet1_I42, "/Sheet1/", "I42", "Ok.").
?test(sheet1_J42, "/Sheet1/", "J42", 1.0).
?test(sheet1_K42, "/Sheet1/", "K42", 1.0).
?test(sheet1_L42, "/Sheet1/", "L42", 1.0).
?test(sheet1_A43, "/Sheet1/", "A43", "IMREAL").
?test(sheet1_B43, "/Sheet1/", "B43", 132.0).
?test(sheet1_C43, "/Sheet1/", "C43", 132.0).
?test(sheet1_D43, "/Sheet1/", "D43", 0.0).
?test(sheet1_E43, "/Sheet1/", "E43", 0.0).
?test(sheet1_F43, "/Sheet1/", "F43", 33.0).
?test(sheet1_G43, "/Sheet1/", "G43", 33.0).
?test(sheet1_H43, "/Sheet1/", "H43", 1.0).
?test(sheet1_I43, "/Sheet1/", "I43", "Ok.").
?test(sheet1_A44, "/Sheet1/", "A44", "IMSIN").
?test(sheet1_B44, "/Sheet1/", "B44", "1.29845758141598+0.634963914784736j").
?test(sheet1_C44, "/Sheet1/", "C44", "1.29845758141598+0.634963914784736j").
?test(sheet1_D44, "/Sheet1/", "D44", "0.53092108624852+3.59056458998578j").
?test(sheet1_E44, "/Sheet1/", "E44", "0.53092108624852+3.59056458998578j").
?test(sheet1_F44, "/Sheet1/", "F44", "-3885.12199806271-1149.26923044064j").
?test(sheet1_G44, "/Sheet1/", "G44", "-3885.12199806271-1149.26923044064j").
?test(sheet1_H44, "/Sheet1/", "H44", 1.0).
?test(sheet1_I44, "/Sheet1/", "I44", "Ok.").
?test(sheet1_J44, "/Sheet1/", "J44", 1.0).
?test(sheet1_K44, "/Sheet1/", "K44", 1.0).
?test(sheet1_L44, "/Sheet1/", "L44", 1.0).
?test(sheet1_A45, "/Sheet1/", "A45", "IMSQRT").
?test(sheet1_B45, "/Sheet1/", "B45", "1.09868411346781+0.455089860562227j").
?test(sheet1_C45, "/Sheet1/", "C45", "1.09868411346781+0.455089860562227j").
?test(sheet1_D45, "/Sheet1/", "D45", "1.81735402102397-0.550250522700337j").
?test(sheet1_E45, "/Sheet1/", "E45", "1.81735402102397-0.550250522700337j").
?test(sheet1_F45, "/Sheet1/", "F45", "2.76546832751588-1.62721082545978j").
?test(sheet1_G45, "/Sheet1/", "G45", "2.76546832751588-1.62721082545978j").
?test(sheet1_H45, "/Sheet1/", "H45", 1.0).
?test(sheet1_I45, "/Sheet1/", "I45", "Ok.").
?test(sheet1_J45, "/Sheet1/", "J45", 1.0).
?test(sheet1_K45, "/Sheet1/", "K45", 1.0).
?test(sheet1_L45, "/Sheet1/", "L45", 1.0).
?test(sheet1_A46, "/Sheet1/", "A46", "IMSUB").
?test(sheet1_B46, "/Sheet1/", "B46", "1-2j").
?test(sheet1_C46, "/Sheet1/", "C46", "1-2j").
?test(sheet1_D46, "/Sheet1/", "D46", "12-20j").
?test(sheet1_E46, "/Sheet1/", "E46", "12-20j").
?test(sheet1_F46, "/Sheet1/", "F46", "-5+j").
?test(sheet1_G46, "/Sheet1/", "G46", "-5+j").
?test(sheet1_H46, "/Sheet1/", "H46", 1.0).
?test(sheet1_I46, "/Sheet1/", "I46", "Ok.").
?test(sheet1_J46, "/Sheet1/", "J46", 1.0).
?test(sheet1_K46, "/Sheet1/", "K46", 1.0).
?test(sheet1_L46, "/Sheet1/", "L46", 1.0).
?test(sheet1_A47, "/Sheet1/", "A47", "IMSUM").
?test(sheet1_B47, "/Sheet1/", "B47", "11-5j").
?test(sheet1_C47, "/Sheet1/", "C47", "11-5j").
?test(sheet1_D47, "/Sheet1/", "D47", "2+j").
?test(sheet1_E47, "/Sheet1/", "E47", "2+j").
?test(sheet1_F47, "/Sheet1/", "F47", "7-j").
?test(sheet1_G47, "/Sheet1/", "G47", "7-j").
?test(sheet1_H47, "/Sheet1/", "H47", 1.0).
?test(sheet1_I47, "/Sheet1/", "I47", "Ok.").
?test(sheet1_J47, "/Sheet1/", "J47", 1.0).
?test(sheet1_K47, "/Sheet1/", "K47", 1.0).
?test(sheet1_L47, "/Sheet1/", "L47", 1.0).
?test(sheet1_A48, "/Sheet1/", "A48", "OCT2BIN").
?test(sheet1_B48, "/Sheet1/", "B48", "10001011").
?test(sheet1_C48, "/Sheet1/", "C48", "10001011").
?test(sheet1_D48, "/Sheet1/", "D48", "1010100").
?test(sheet1_E48, "/Sheet1/", "E48", "1010100").
?test(sheet1_F48, "/Sheet1/", "F48", "11010").
?test(sheet1_G48, "/Sheet1/", "G48", "11010").
?test(sheet1_H48, "/Sheet1/", "H48", 1.0).
?test(sheet1_I48, "/Sheet1/", "I48", "Ok.").
?test(sheet1_A49, "/Sheet1/", "A49", "OCT2DEC").
?test(sheet1_B49, "/Sheet1/", "B49", 84.0).
?test(sheet1_C49, "/Sheet1/", "C49", 84.0).
?test(sheet1_D49, "/Sheet1/", "D49", 139.0).
?test(sheet1_E49, "/Sheet1/", "E49", 139.0).
?test(sheet1_F49, "/Sheet1/", "F49", 11.0).
?test(sheet1_G49, "/Sheet1/", "G49", 11.0).
?test(sheet1_H49, "/Sheet1/", "H49", 1.0).
?test(sheet1_I49, "/Sheet1/", "I49", "Ok.").
?test(sheet1_A50, "/Sheet1/", "A50", "OCT2HEX").
?test(sheet1_B50, "/Sheet1/", "B50", "5A").
?test(sheet1_C50, "/Sheet1/", "C50", "5A").
?test(sheet1_D50, "/Sheet1/", "D50", "3F").
?test(sheet1_E50, "/Sheet1/", "E50", "3F").
?test(sheet1_F50, "/Sheet1/", "F50", "8").
?test(sheet1_G50, "/Sheet1/", "G50", "8").
?test(sheet1_H50, "/Sheet1/", "H50", 1.0).
?test(sheet1_I50, "/Sheet1/", "I50", "Ok.").
?test(sheet1_A51, "/Sheet1/", "A51", "Total").
?test(sheet1_H51, "/Sheet1/", "H51", true).
?test(sheet1_I51, "/Sheet1/", "I51", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_engineering.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_engineering" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A3,
        sheet1_C3,
        sheet1_A4,
        sheet1_C4,
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet1_A12,
        sheet1_B12,
        sheet1_C12,
        sheet1_D12,
        sheet1_E12,
        sheet1_F12,
        sheet1_G12,
        sheet1_H12,
        sheet1_I12,
        sheet1_A13,
        sheet1_B13,
        sheet1_C13,
        sheet1_D13,
        sheet1_E13,
        sheet1_F13,
        sheet1_G13,
        sheet1_H13,
        sheet1_I13,
        sheet1_A14,
        sheet1_B14,
        sheet1_C14,
        sheet1_D14,
        sheet1_E14,
        sheet1_F14,
        sheet1_G14,
        sheet1_H14,
        sheet1_I14,
        sheet1_A15,
        sheet1_B15,
        sheet1_C15,
        sheet1_D15,
        sheet1_E15,
        sheet1_F15,
        sheet1_G15,
        sheet1_H15,
        sheet1_I15,
        sheet1_A16,
        sheet1_B16,
        sheet1_C16,
        sheet1_D16,
        sheet1_E16,
        sheet1_F16,
        sheet1_G16,
        sheet1_H16,
        sheet1_I16,
        sheet1_A17,
        sheet1_B17,
        sheet1_C17,
        sheet1_D17,
        sheet1_E17,
        sheet1_F17,
        sheet1_G17,
        sheet1_H17,
        sheet1_I17,
        sheet1_A18,
        sheet1_B18,
        sheet1_C18,
        sheet1_D18,
        sheet1_E18,
        sheet1_F18,
        sheet1_G18,
        sheet1_H18,
        sheet1_I18,
        sheet1_A19,
        sheet1_B19,
        sheet1_C19,
        sheet1_D19,
        sheet1_E19,
        sheet1_F19,
        sheet1_G19,
        sheet1_H19,
        sheet1_I19,
        sheet1_A20,
        sheet1_B20,
        sheet1_C20,
        sheet1_D20,
        sheet1_E20,
        sheet1_F20,
        sheet1_G20,
        sheet1_H20,
        sheet1_I20,
        sheet1_A21,
        sheet1_B21,
        sheet1_C21,
        sheet1_D21,
        sheet1_E21,
        sheet1_F21,
        sheet1_G21,
        sheet1_H21,
        sheet1_I21,
        sheet1_A22,
        sheet1_B22,
        sheet1_C22,
        sheet1_D22,
        sheet1_E22,
        sheet1_F22,
        sheet1_G22,
        sheet1_H22,
        sheet1_I22,
        sheet1_A23,
        sheet1_B23,
        sheet1_C23,
        sheet1_D23,
        sheet1_E23,
        sheet1_F23,
        sheet1_G23,
        sheet1_H23,
        sheet1_I23,
        sheet1_A24,
        sheet1_B24,
        sheet1_C24,
        sheet1_D24,
        sheet1_E24,
        sheet1_F24,
        sheet1_G24,
        sheet1_H24,
        sheet1_I24,
        sheet1_A25,
        sheet1_B25,
        sheet1_C25,
        sheet1_D25,
        sheet1_E25,
        sheet1_F25,
        sheet1_G25,
        sheet1_H25,
        sheet1_I25,
        sheet1_A26,
        sheet1_B26,
        sheet1_C26,
        sheet1_D26,
        sheet1_E26,
        sheet1_F26,
        sheet1_G26,
        sheet1_H26,
        sheet1_I26,
        sheet1_A27,
        sheet1_B27,
        sheet1_C27,
        sheet1_D27,
        sheet1_E27,
        sheet1_F27,
        sheet1_G27,
        sheet1_H27,
        sheet1_I27,
        sheet1_A28,
        sheet1_B28,
        sheet1_C28,
        sheet1_D28,
        sheet1_E28,
        sheet1_F28,
        sheet1_G28,
        sheet1_H28,
        sheet1_I28,
        sheet1_A29,
        sheet1_B29,
        sheet1_C29,
        sheet1_D29,
        sheet1_E29,
        sheet1_F29,
        sheet1_G29,
        sheet1_H29,
        sheet1_I29,
        sheet1_A30,
        sheet1_B30,
        sheet1_C30,
        sheet1_D30,
        sheet1_E30,
        sheet1_F30,
        sheet1_G30,
        sheet1_H30,
        sheet1_I30,
        sheet1_A31,
        sheet1_B31,
        sheet1_C31,
        sheet1_D31,
        sheet1_E31,
        sheet1_F31,
        sheet1_G31,
        sheet1_H31,
        sheet1_I31,
        sheet1_A32,
        sheet1_B32,
        sheet1_C32,
        sheet1_D32,
        sheet1_E32,
        sheet1_F32,
        sheet1_G32,
        sheet1_H32,
        sheet1_I32,
        sheet1_A33,
        sheet1_B33,
        sheet1_C33,
        sheet1_D33,
        sheet1_E33,
        sheet1_F33,
        sheet1_G33,
        sheet1_H33,
        sheet1_I33,
        sheet1_A34,
        sheet1_B34,
        sheet1_C34,
        sheet1_D34,
        sheet1_E34,
        sheet1_F34,
        sheet1_G34,
        sheet1_H34,
        sheet1_I34,
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
        sheet1_A38,
        sheet1_B38,
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
        sheet1_A43,
        sheet1_B43,
        sheet1_C43,
        sheet1_D43,
        sheet1_E43,
        sheet1_F43,
        sheet1_G43,
        sheet1_H43,
        sheet1_I43,
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
        sheet1_A47,
        sheet1_B47,
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
        sheet1_A48,
        sheet1_B48,
        sheet1_C48,
        sheet1_D48,
        sheet1_E48,
        sheet1_F48,
        sheet1_G48,
        sheet1_H48,
        sheet1_I48,
        sheet1_A49,
        sheet1_B49,
        sheet1_C49,
        sheet1_D49,
        sheet1_E49,
        sheet1_F49,
        sheet1_G49,
        sheet1_H49,
        sheet1_I49,
        sheet1_A50,
        sheet1_B50,
        sheet1_C50,
        sheet1_D50,
        sheet1_E50,
        sheet1_F50,
        sheet1_G50,
        sheet1_H50,
        sheet1_I50,
        sheet1_A51,
        sheet1_H51,
        sheet1_I51
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
