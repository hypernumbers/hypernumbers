%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_logical_SUITE).
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
                     [Testcase, "d_gnumeric_logical_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_logical" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "LOGICAL FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_A4, "/Sheet1/", "A4", "All ok.").
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", 6.0).
?test(sheet1_B8, "/Sheet1/", "B8", 6.0).
?test(sheet1_A11, "/Sheet1/", "A11", "Function").
?test(sheet1_B11, "/Sheet1/", "B11", "1st test").
?test(sheet1_C11, "/Sheet1/", "C11", "Correct").
?test(sheet1_D11, "/Sheet1/", "D11", "2nd test").
?test(sheet1_E11, "/Sheet1/", "E11", "Correct").
?test(sheet1_F11, "/Sheet1/", "F11", "3rd test").
?test(sheet1_G11, "/Sheet1/", "G11", "Correct").
?test(sheet1_H11, "/Sheet1/", "H11", "Status").
?test(sheet1_I11, "/Sheet1/", "I11", "Status message").
?test(sheet1_A12, "/Sheet1/", "A12", "AND").
?test(sheet1_B12, "/Sheet1/", "B12", true).
?test(sheet1_C12, "/Sheet1/", "C12", true).
?test(sheet1_D12, "/Sheet1/", "D12", true).
?test(sheet1_E12, "/Sheet1/", "E12", true).
?test(sheet1_F12, "/Sheet1/", "F12", false).
?test(sheet1_G12, "/Sheet1/", "G12", false).
?test(sheet1_H12, "/Sheet1/", "H12", 1.0).
?test(sheet1_I12, "/Sheet1/", "I12", "Ok.").
?test(sheet1_A13, "/Sheet1/", "A13", "FALSE").
?test(sheet1_B13, "/Sheet1/", "B13", false).
?test(sheet1_C13, "/Sheet1/", "C13", false).
?test(sheet1_D13, "/Sheet1/", "D13", false).
?test(sheet1_E13, "/Sheet1/", "E13", false).
?test(sheet1_F13, "/Sheet1/", "F13", false).
?test(sheet1_G13, "/Sheet1/", "G13", false).
?test(sheet1_H13, "/Sheet1/", "H13", 1.0).
?test(sheet1_I13, "/Sheet1/", "I13", "Ok.").
?test(sheet1_A14, "/Sheet1/", "A14", "IF").
?test(sheet1_B14, "/Sheet1/", "B14", "true").
?test(sheet1_C14, "/Sheet1/", "C14", "true").
?test(sheet1_D14, "/Sheet1/", "D14", "false").
?test(sheet1_E14, "/Sheet1/", "E14", "false").
?test(sheet1_F14, "/Sheet1/", "F14", "false").
?test(sheet1_G14, "/Sheet1/", "G14", "false").
?test(sheet1_H14, "/Sheet1/", "H14", 1.0).
?test(sheet1_I14, "/Sheet1/", "I14", "Ok.").
?test(sheet1_A15, "/Sheet1/", "A15", "NOT").
?test(sheet1_B15, "/Sheet1/", "B15", false).
?test(sheet1_C15, "/Sheet1/", "C15", false).
?test(sheet1_D15, "/Sheet1/", "D15", true).
?test(sheet1_E15, "/Sheet1/", "E15", true).
?test(sheet1_F15, "/Sheet1/", "F15", true).
?test(sheet1_G15, "/Sheet1/", "G15", true).
?test(sheet1_H15, "/Sheet1/", "H15", 1.0).
?test(sheet1_I15, "/Sheet1/", "I15", "Ok.").
?test(sheet1_A16, "/Sheet1/", "A16", "OR").
?test(sheet1_B16, "/Sheet1/", "B16", true).
?test(sheet1_C16, "/Sheet1/", "C16", true).
?test(sheet1_D16, "/Sheet1/", "D16", false).
?test(sheet1_E16, "/Sheet1/", "E16", false).
?test(sheet1_F16, "/Sheet1/", "F16", true).
?test(sheet1_G16, "/Sheet1/", "G16", true).
?test(sheet1_H16, "/Sheet1/", "H16", 1.0).
?test(sheet1_I16, "/Sheet1/", "I16", "Ok.").
?test(sheet1_A17, "/Sheet1/", "A17", "TRUE").
?test(sheet1_B17, "/Sheet1/", "B17", true).
?test(sheet1_C17, "/Sheet1/", "C17", true).
?test(sheet1_D17, "/Sheet1/", "D17", true).
?test(sheet1_E17, "/Sheet1/", "E17", true).
?test(sheet1_F17, "/Sheet1/", "F17", true).
?test(sheet1_G17, "/Sheet1/", "G17", true).
?test(sheet1_H17, "/Sheet1/", "H17", 1.0).
?test(sheet1_I17, "/Sheet1/", "I17", "Ok.").
?test(sheet1_A18, "/Sheet1/", "A18", "Total").
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_logical.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_logical" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A18
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
