%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_text_SUITE).
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
                     [Testcase, "d_gnumeric_text_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_text" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "TEXT FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_A4, "/Sheet1/", "A4", '#VALUE!').
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", '#VALUE!').
?test(sheet1_B8, "/Sheet1/", "B8", 23.0).
?test(sheet1_A11, "/Sheet1/", "A11", "Function").
?test(sheet1_B11, "/Sheet1/", "B11", "1st test").
?test(sheet1_C11, "/Sheet1/", "C11", "Correct").
?test(sheet1_D11, "/Sheet1/", "D11", "2nd test").
?test(sheet1_E11, "/Sheet1/", "E11", "Correct").
?test(sheet1_F11, "/Sheet1/", "F11", "3rd test").
?test(sheet1_G11, "/Sheet1/", "G11", "Correct").
?test(sheet1_H11, "/Sheet1/", "H11", "Status").
?test(sheet1_I11, "/Sheet1/", "I11", "Status message").
?test(sheet1_A12, "/Sheet1/", "A12", "CHAR").
?test(sheet1_B12, "/Sheet1/", "B12", "A").
?test(sheet1_C12, "/Sheet1/", "C12", "A").
?test(sheet1_D12, "/Sheet1/", "D12", "a").
?test(sheet1_E12, "/Sheet1/", "E12", "a").
?test(sheet1_F12, "/Sheet1/", "F12", "&").
?test(sheet1_G12, "/Sheet1/", "G12", "&").
?test(sheet1_H12, "/Sheet1/", "H12", 1.0).
?test(sheet1_I12, "/Sheet1/", "I12", "Ok.").
?test(sheet1_A13, "/Sheet1/", "A13", "CLEAN").
?test(sheet1_B13, "/Sheet1/", "B13", "No cleaning..").
?test(sheet1_C13, "/Sheet1/", "C13", "No cleaning..").
?test(sheet1_D13, "/Sheet1/", "D13", "one").
?test(sheet1_E13, "/Sheet1/", "E13", "one").
?test(sheet1_F13, "/Sheet1/", "F13", "aaa").
?test(sheet1_G13, "/Sheet1/", "G13", "aaa").
?test(sheet1_H13, "/Sheet1/", "H13", 1.0).
?test(sheet1_I13, "/Sheet1/", "I13", "Ok.").
?test(sheet1_A14, "/Sheet1/", "A14", "CODE").
?test(sheet1_B14, "/Sheet1/", "B14", 65.0).
?test(sheet1_C14, "/Sheet1/", "C14", 65.0).
?test(sheet1_D14, "/Sheet1/", "D14", 65.0).
?test(sheet1_E14, "/Sheet1/", "E14", 65.0).
?test(sheet1_F14, "/Sheet1/", "F14", 97.0).
?test(sheet1_G14, "/Sheet1/", "G14", 97.0).
?test(sheet1_H14, "/Sheet1/", "H14", 1.0).
?test(sheet1_I14, "/Sheet1/", "I14", "Ok.").
?test(sheet1_A15, "/Sheet1/", "A15", "CONCATENATE").
?test(sheet1_B15, "/Sheet1/", "B15", "aabb").
?test(sheet1_C15, "/Sheet1/", "C15", "aabb").
?test(sheet1_D15, "/Sheet1/", "D15", "abc").
?test(sheet1_E15, "/Sheet1/", "E15", "abc").
?test(sheet1_F15, "/Sheet1/", "F15", "a2j").
?test(sheet1_G15, "/Sheet1/", "G15", "a2j").
?test(sheet1_H15, "/Sheet1/", "H15", 1.0).
?test(sheet1_I15, "/Sheet1/", "I15", "Ok.").
?test(sheet1_A16, "/Sheet1/", "A16", "DOLLAR").
?test(sheet1_B16, "/Sheet1/", "B16", "£12,345.00").
?test(sheet1_C16, "/Sheet1/", "C16", "$12,345.00").
?test(sheet1_D16, "/Sheet1/", "D16", "£23.3").
?test(sheet1_E16, "/Sheet1/", "E16", "$23.3").
?test(sheet1_F16, "/Sheet1/", "F16", "-£30").
?test(sheet1_G16, "/Sheet1/", "G16", "($30)").
?test(sheet1_H16, "/Sheet1/", "H16", 0.0).
?test(sheet1_I16, "/Sheet1/", "I16", "FAILED!!").
?test(sheet1_A17, "/Sheet1/", "A17", "EXACT").
?test(sheet1_B17, "/Sheet1/", "B17", true).
?test(sheet1_C17, "/Sheet1/", "C17", true).
?test(sheet1_D17, "/Sheet1/", "D17", false).
?test(sheet1_E17, "/Sheet1/", "E17", false).
?test(sheet1_F17, "/Sheet1/", "F17", false).
?test(sheet1_G17, "/Sheet1/", "G17", false).
?test(sheet1_H17, "/Sheet1/", "H17", 1.0).
?test(sheet1_I17, "/Sheet1/", "I17", "Ok.").
?test(sheet1_A18, "/Sheet1/", "A18", "FIND").
?test(sheet1_B18, "/Sheet1/", "B18", 1.0).
?test(sheet1_C18, "/Sheet1/", "C18", 1.0).
?test(sheet1_D18, "/Sheet1/", "D18", 2.0).
?test(sheet1_E18, "/Sheet1/", "E18", 2.0).
?test(sheet1_F18, "/Sheet1/", "F18", 4.0).
?test(sheet1_G18, "/Sheet1/", "G18", 4.0).
?test(sheet1_H18, "/Sheet1/", "H18", 1.0).
?test(sheet1_I18, "/Sheet1/", "I18", "Ok.").
?test(sheet1_A19, "/Sheet1/", "A19", "FIXED").
?test(sheet1_B19, "/Sheet1/", "B19", "1,234.57").
?test(sheet1_C19, "/Sheet1/", "C19", "1,234.57").
?test(sheet1_D19, "/Sheet1/", "D19", "1,234,570").
?test(sheet1_E19, "/Sheet1/", "E19", "1,234,570").
?test(sheet1_F19, "/Sheet1/", "F19", "-12,345.245").
?test(sheet1_G19, "/Sheet1/", "G19", "-12,345.245").
?test(sheet1_H19, "/Sheet1/", "H19", 1.0).
?test(sheet1_I19, "/Sheet1/", "I19", "Ok.").
?test(sheet1_A20, "/Sheet1/", "A20", "LEFT").
?test(sheet1_B20, "/Sheet1/", "B20", "Dir").
?test(sheet1_C20, "/Sheet1/", "C20", "Dir").
?test(sheet1_D20, "/Sheet1/", "D20", "ab").
?test(sheet1_E20, "/Sheet1/", "E20", "ab").
?test(sheet1_F20, "/Sheet1/", "F20", "t").
?test(sheet1_G20, "/Sheet1/", "G20", "t").
?test(sheet1_H20, "/Sheet1/", "H20", 1.0).
?test(sheet1_I20, "/Sheet1/", "I20", "Ok.").
?test(sheet1_A21, "/Sheet1/", "A21", "LEN").
?test(sheet1_B21, "/Sheet1/", "B21", 8.0).
?test(sheet1_C21, "/Sheet1/", "C21", 8.0).
?test(sheet1_D21, "/Sheet1/", "D21", 0.0).
?test(sheet1_E21, "/Sheet1/", "E21", 0.0).
?test(sheet1_F21, "/Sheet1/", "F21", 1.0).
?test(sheet1_G21, "/Sheet1/", "G21", 1.0).
?test(sheet1_H21, "/Sheet1/", "H21", 1.0).
?test(sheet1_I21, "/Sheet1/", "I21", "Ok.").
?test(sheet1_A22, "/Sheet1/", "A22", "LOWER").
?test(sheet1_B22, "/Sheet1/", "B22", "j. f. kennedy").
?test(sheet1_C22, "/Sheet1/", "C22", "j. f. kennedy").
?test(sheet1_D22, "/Sheet1/", "D22", "aabbaa").
?test(sheet1_E22, "/Sheet1/", "E22", "aabbaa").
?test(sheet1_F22, "/Sheet1/", "F22", "aa211aa").
?test(sheet1_G22, "/Sheet1/", "G22", "aa211aa").
?test(sheet1_H22, "/Sheet1/", "H22", 1.0).
?test(sheet1_I22, "/Sheet1/", "I22", "Ok.").
?test(sheet1_A23, "/Sheet1/", "A23", "MID").
?test(sheet1_B23, "/Sheet1/", "B23", "est").
?test(sheet1_C23, "/Sheet1/", "C23", "est").
?test(sheet1_D23, "/Sheet1/", "D23", "t").
?test(sheet1_E23, "/Sheet1/", "E23", "t").
?test(sheet1_F23, "/Sheet1/", "F23", "ing again").
?test(sheet1_G23, "/Sheet1/", "G23", "ing again").
?test(sheet1_H23, "/Sheet1/", "H23", 1.0).
?test(sheet1_I23, "/Sheet1/", "I23", "Ok.").
?test(sheet1_A24, "/Sheet1/", "A24", "PROPER").
?test(sheet1_B24, "/Sheet1/", "B24", "J. F. Kennedy").
?test(sheet1_C24, "/Sheet1/", "C24", "J. F. Kennedy").
?test(sheet1_D24, "/Sheet1/", "D24", "Aajj-Djaf Dkfj").
?test(sheet1_E24, "/Sheet1/", "E24", "Aajj-Djaf Dkfj").
?test(sheet1_F24, "/Sheet1/", "F24", "A").
?test(sheet1_G24, "/Sheet1/", "G24", "A").
?test(sheet1_H24, "/Sheet1/", "H24", 1.0).
?test(sheet1_I24, "/Sheet1/", "I24", "Ok.").
?test(sheet1_A25, "/Sheet1/", "A25", "REPLACE").
?test(sheet1_B25, "/Sheet1/", "B25", "t*****ing").
?test(sheet1_C25, "/Sheet1/", "C25", "t*****ing").
?test(sheet1_D25, "/Sheet1/", "D25", "bbsting").
?test(sheet1_E25, "/Sheet1/", "E25", "bbsting").
?test(sheet1_F25, "/Sheet1/", "F25", "tesaa").
?test(sheet1_G25, "/Sheet1/", "G25", "tesaa").
?test(sheet1_H25, "/Sheet1/", "H25", 1.0).
?test(sheet1_I25, "/Sheet1/", "I25", "Ok.").
?test(sheet1_A26, "/Sheet1/", "A26", "REPT").
?test(sheet1_B26, "/Sheet1/", "B26", "...").
?test(sheet1_C26, "/Sheet1/", "C26", "...").
?test(sheet1_D26, "/Sheet1/", "D26", "rwxrwxrwx").
?test(sheet1_E26, "/Sheet1/", "E26", "rwxrwxrwx").
?test(sheet1_F26, "/Sheet1/", "F26", "aaaaaaaaaaaaaaaaaaaaaa").
?test(sheet1_G26, "/Sheet1/", "G26", "aaaaaaaaaaaaaaaaaaaaaa").
?test(sheet1_H26, "/Sheet1/", "H26", 1.0).
?test(sheet1_I26, "/Sheet1/", "I26", "Ok.").
?test(sheet1_A27, "/Sheet1/", "A27", "RIGHT").
?test(sheet1_B27, "/Sheet1/", "B27", "g").
?test(sheet1_C27, "/Sheet1/", "C27", "g").
?test(sheet1_D27, "/Sheet1/", "D27", "ing").
?test(sheet1_E27, "/Sheet1/", "E27", "ing").
?test(sheet1_F27, "/Sheet1/", "F27", "ting").
?test(sheet1_G27, "/Sheet1/", "G27", "ting").
?test(sheet1_H27, "/Sheet1/", "H27", 1.0).
?test(sheet1_I27, "/Sheet1/", "I27", "Ok.").
?test(sheet1_A28, "/Sheet1/", "A28", "SEARCH").
?test(sheet1_B28, "/Sheet1/", "B28", 1.0).
?test(sheet1_C28, "/Sheet1/", "C28", 1.0).
?test(sheet1_D28, "/Sheet1/", "D28", 4.0).
?test(sheet1_E28, "/Sheet1/", "E28", 4.0).
?test(sheet1_F28, "/Sheet1/", "F28", 5.0).
?test(sheet1_G28, "/Sheet1/", "G28", 5.0).
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_A29, "/Sheet1/", "A29", "SUBSTITUTE").
?test(sheet1_B29, "/Sheet1/", "B29", "waiting").
?test(sheet1_C29, "/Sheet1/", "C29", "waiting").
?test(sheet1_D29, "/Sheet1/", "D29", "Revenue Total").
?test(sheet1_E29, "/Sheet1/", "E29", "Revenue Total").
?test(sheet1_F29, "/Sheet1/", "F29", "Revenue 1999").
?test(sheet1_G29, "/Sheet1/", "G29", "Revenue 1999").
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_A30, "/Sheet1/", "A30", "T").
?test(sheet1_B30, "/Sheet1/", "B30", "xx").
?test(sheet1_C30, "/Sheet1/", "C30", "xx").
?test(sheet1_D30, "/Sheet1/", "D30", "").
?test(sheet1_F30, "/Sheet1/", "F30", "").
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_A31, "/Sheet1/", "A31", "TEXT").
?test(sheet1_B31, "/Sheet1/", "B31", "$3.22").
?test(sheet1_C31, "/Sheet1/", "C31", "$3.22").
?test(sheet1_D31, "/Sheet1/", "D31", "4/15/1999").
?test(sheet1_E31, "/Sheet1/", "E31", "April, 15, 99").
?test(sheet1_F31, "/Sheet1/", "F31", "$23123").
?test(sheet1_G31, "/Sheet1/", "G31", "$23123").
?test(sheet1_H31, "/Sheet1/", "H31", 0.0).
?test(sheet1_I31, "/Sheet1/", "I31", "FAILED!!").
?test(sheet1_A32, "/Sheet1/", "A32", "TRIM").
?test(sheet1_B32, "/Sheet1/", "B32", "a ddd cc").
?test(sheet1_C32, "/Sheet1/", "C32", "a ddd cc").
?test(sheet1_D32, "/Sheet1/", "D32", "aaa").
?test(sheet1_E32, "/Sheet1/", "E32", "aaa").
?test(sheet1_F32, "/Sheet1/", "F32", "add cd e d").
?test(sheet1_G32, "/Sheet1/", "G32", "add cd e d").
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "UPPER").
?test(sheet1_B33, "/Sheet1/", "B33", "CANCEL").
?test(sheet1_C33, "/Sheet1/", "C33", "CANCEL").
?test(sheet1_D33, "/Sheet1/", "D33", "JJD22JJJJ").
?test(sheet1_E33, "/Sheet1/", "E33", "JJD22JJJJ").
?test(sheet1_F33, "/Sheet1/", "F33", "J").
?test(sheet1_G33, "/Sheet1/", "G33", "J").
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "VALUE").
?test(sheet1_B34, "/Sheet1/", "B34", '#VALUE!').
?test(sheet1_C34, "/Sheet1/", "C34", 1000.0).
?test(sheet1_D34, "/Sheet1/", "D34", '#VALUE!').
?test(sheet1_E34, "/Sheet1/", "E34", 223.234).
?test(sheet1_F34, "/Sheet1/", "F34", -324.324).
?test(sheet1_G34, "/Sheet1/", "G34", -324.324).
?test(sheet1_H34, "/Sheet1/", "H34", '#VALUE!').
?test(sheet1_I34, "/Sheet1/", "I34", '#VALUE!').
?test(sheet1_A35, "/Sheet1/", "A35", "Total").
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_text.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_text" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A4,
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet1_A11,
        sheet1_B11,
        sheet1_C11,
        sheet1_D11,
        sheet1_E11,
        sheet1_F11,
        sheet1_G11,
        sheet1_H11,
        sheet1_I11,
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
        sheet1_F30,
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
        sheet1_A35
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
