%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_abs_and_rel_addressing_SUITE).
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
                     [Testcase, "b_abs_and_rel_addressing_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_abs_and_rel_addressing" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This spreadsheet tests absolute and relative addressing").
?test(sheet1_A3, "/Sheet1/", "A3", "Cells").
?test(sheet1_A4, "/Sheet1/", "A4", "Relative").
?test(sheet1_B4, "/Sheet1/", "B4", 11.0).
?test(sheet1_C4, "/Sheet1/", "C4", 11.0).
?test(sheet1_A5, "/Sheet1/", "A5", "Abs Col").
?test(sheet1_B5, "/Sheet1/", "B5", 22.0).
?test(sheet1_C5, "/Sheet1/", "C5", 22.0).
?test(sheet1_A6, "/Sheet1/", "A6", "Abs Row").
?test(sheet1_B6, "/Sheet1/", "B6", 33.0).
?test(sheet1_C6, "/Sheet1/", "C6", 33.0).
?test(sheet1_A7, "/Sheet1/", "A7", "Abs Row And Col").
?test(sheet1_B7, "/Sheet1/", "B7", 44.0).
?test(sheet1_C7, "/Sheet1/", "C7", 44.0).
?test(sheet1_A9, "/Sheet1/", "A9", "Ranges").
?test(sheet1_A10, "/Sheet1/", "A10", "Relative").
?test(sheet1_B10, "/Sheet1/", "B10", 60.5).
?test(sheet1_C10, "/Sheet1/", "C10", 55.0).
?test(sheet1_D10, "/Sheet1/", "D10", 111.0).
?test(sheet1_A11, "/Sheet1/", "A11", "First Abs Col").
?test(sheet1_B11, "/Sheet1/", "B11", 71.5).
?test(sheet1_C11, "/Sheet1/", "C11", 66.0).
?test(sheet1_D11, "/Sheet1/", "D11", 222.0).
?test(sheet1_A12, "/Sheet1/", "A12", "First Abs Row").
?test(sheet1_B12, "/Sheet1/", "B12", 82.5).
?test(sheet1_C12, "/Sheet1/", "C12", 77.0).
?test(sheet1_D12, "/Sheet1/", "D12", 333.0).
?test(sheet1_A13, "/Sheet1/", "A13", "First Abs Row And Col").
?test(sheet1_B13, "/Sheet1/", "B13", 49.5).
?test(sheet1_C13, "/Sheet1/", "C13", 88.0).
?test(sheet1_D13, "/Sheet1/", "D13", 444.0).
?test(sheet1_A14, "/Sheet1/", "A14", "Second Abs Col").
?test(sheet1_B14, "/Sheet1/", "B14", 16.5).
?test(sheet1_C14, "/Sheet1/", "C14", 11.0).
?test(sheet1_D14, "/Sheet1/", "D14", 44.0).
?test(sheet1_A15, "/Sheet1/", "A15", "Second Abs Row").
?test(sheet1_B15, "/Sheet1/", "B15", 46.2).
?test(sheet1_C15, "/Sheet1/", "C15", 22.0).
?test(sheet1_D15, "/Sheet1/", "D15", 55.0).
?test(sheet1_A16, "/Sheet1/", "A16", "Second Abs Row And Col").
?test(sheet1_B16, "/Sheet1/", "B16", 230.6).
?test(sheet1_C16, "/Sheet1/", "C16", 33.0).
?test(sheet1_D16, "/Sheet1/", "D16", 66.0).
?test(sheet1_A17, "/Sheet1/", "A17", "Mixed").
?test(sheet1_B17, "/Sheet1/", "B17", 943.5).
?test(sheet1_C17, "/Sheet1/", "C17", 999.0).
?test(sheet1_D17, "/Sheet1/", "D17", 777.0).
?test(sheet1_A18, "/Sheet1/", "A18", "Mixed").
?test(sheet1_B18, "/Sheet1/", "B18", 832.5).
?test(sheet1_C18, "/Sheet1/", "C18", 888.0).
?test(sheet1_D18, "/Sheet1/", "D18", 888.0).
?test(sheet1_A19, "/Sheet1/", "A19", "Mixed").
?test(sheet1_B19, "/Sheet1/", "B19", 777.0).
?test(sheet1_C19, "/Sheet1/", "C19", 777.0).
?test(sheet1_D19, "/Sheet1/", "D19", 999.0).
?test(sheet1_A21, "/Sheet1/", "A21", "Shared formulae").
?test(sheet1_A22, "/Sheet1/", "A22", "Relative").
?test(sheet1_B22, "/Sheet1/", "B22", 11.0).
?test(sheet1_C22, "/Sheet1/", "C22", 11.0).
?test(sheet1_D22, "/Sheet1/", "D22", 0.0).
?test(sheet1_E22, "/Sheet1/", "E22", 0.0).
?test(sheet1_F22, "/Sheet1/", "F22", 0.0).
?test(sheet1_G22, "/Sheet1/", "G22", 0.0).
?test(sheet1_H22, "/Sheet1/", "H22", 0.0).
?test(sheet1_I22, "/Sheet1/", "I22", 0.0).
?test(sheet1_J22, "/Sheet1/", "J22", 0.0).
?test(sheet1_K22, "/Sheet1/", "K22", 0.0).
?test(sheet1_L22, "/Sheet1/", "L22", 0.0).
?test(sheet1_A23, "/Sheet1/", "A23", "First Abs Col").
?test(sheet1_B23, "/Sheet1/", "B23", 22.0).
?test(sheet1_C23, "/Sheet1/", "C23", 22.0).
?test(sheet1_D23, "/Sheet1/", "D23", 22.0).
?test(sheet1_E23, "/Sheet1/", "E23", 22.0).
?test(sheet1_F23, "/Sheet1/", "F23", 22.0).
?test(sheet1_G23, "/Sheet1/", "G23", 22.0).
?test(sheet1_H23, "/Sheet1/", "H23", 22.0).
?test(sheet1_I23, "/Sheet1/", "I23", 22.0).
?test(sheet1_J23, "/Sheet1/", "J23", 22.0).
?test(sheet1_K23, "/Sheet1/", "K23", 22.0).
?test(sheet1_L23, "/Sheet1/", "L23", 22.0).
?test(sheet1_A24, "/Sheet1/", "A24", "First Abs Row").
?test(sheet1_B24, "/Sheet1/", "B24", 22.0).
?test(sheet1_C24, "/Sheet1/", "C24", 22.0).
?test(sheet1_D24, "/Sheet1/", "D24", 0.0).
?test(sheet1_E24, "/Sheet1/", "E24", 0.0).
?test(sheet1_F24, "/Sheet1/", "F24", 0.0).
?test(sheet1_G24, "/Sheet1/", "G24", 0.0).
?test(sheet1_H24, "/Sheet1/", "H24", 0.0).
?test(sheet1_I24, "/Sheet1/", "I24", 0.0).
?test(sheet1_J24, "/Sheet1/", "J24", 0.0).
?test(sheet1_K24, "/Sheet1/", "K24", 0.0).
?test(sheet1_L24, "/Sheet1/", "L24", 0.0).
?test(sheet1_A25, "/Sheet1/", "A25", "First Abs Row And Col").
?test(sheet1_B25, "/Sheet1/", "B25", 22.0).
?test(sheet1_C25, "/Sheet1/", "C25", 22.0).
?test(sheet1_D25, "/Sheet1/", "D25", 22.0).
?test(sheet1_E25, "/Sheet1/", "E25", 22.0).
?test(sheet1_F25, "/Sheet1/", "F25", 22.0).
?test(sheet1_G25, "/Sheet1/", "G25", 22.0).
?test(sheet1_H25, "/Sheet1/", "H25", 22.0).
?test(sheet1_I25, "/Sheet1/", "I25", 22.0).
?test(sheet1_J25, "/Sheet1/", "J25", 22.0).
?test(sheet1_K25, "/Sheet1/", "K25", 22.0).
?test(sheet1_L25, "/Sheet1/", "L25", 22.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_abs_and_rel_addressing.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_abs_and_rel_addressing" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_B4,
        sheet1_C4,
        sheet1_A5,
        sheet1_B5,
        sheet1_C5,
        sheet1_A6,
        sheet1_B6,
        sheet1_C6,
        sheet1_A7,
        sheet1_B7,
        sheet1_C7,
        sheet1_A9,
        sheet1_A10,
        sheet1_B10,
        sheet1_C10,
        sheet1_D10,
        sheet1_A11,
        sheet1_B11,
        sheet1_C11,
        sheet1_D11,
        sheet1_A12,
        sheet1_B12,
        sheet1_C12,
        sheet1_D12,
        sheet1_A13,
        sheet1_B13,
        sheet1_C13,
        sheet1_D13,
        sheet1_A14,
        sheet1_B14,
        sheet1_C14,
        sheet1_D14,
        sheet1_A15,
        sheet1_B15,
        sheet1_C15,
        sheet1_D15,
        sheet1_A16,
        sheet1_B16,
        sheet1_C16,
        sheet1_D16,
        sheet1_A17,
        sheet1_B17,
        sheet1_C17,
        sheet1_D17,
        sheet1_A18,
        sheet1_B18,
        sheet1_C18,
        sheet1_D18,
        sheet1_A19,
        sheet1_B19,
        sheet1_C19,
        sheet1_D19,
        sheet1_A21,
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
        sheet1_A23,
        sheet1_B23,
        sheet1_C23,
        sheet1_D23,
        sheet1_E23,
        sheet1_F23,
        sheet1_G23,
        sheet1_H23,
        sheet1_I23,
        sheet1_J23,
        sheet1_K23,
        sheet1_L23,
        sheet1_A24,
        sheet1_B24,
        sheet1_C24,
        sheet1_D24,
        sheet1_E24,
        sheet1_F24,
        sheet1_G24,
        sheet1_H24,
        sheet1_I24,
        sheet1_J24,
        sheet1_K24,
        sheet1_L24,
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
        sheet1_L25
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
