%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_shared_formulae_SUITE).
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
                     [Testcase, "b_shared_formulae_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_shared_formulae" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "Test Shared Formulae").
?test(sheet1_A2, "/Sheet1/", "A2", 1.0).
?test(sheet1_B2, "/Sheet1/", "B2", 2.0).
?test(sheet1_C2, "/Sheet1/", "C2", 1.5).
?test(sheet1_D2, "/Sheet1/", "D2", 1.75).
?test(sheet1_E2, "/Sheet1/", "E2", 1.625).
?test(sheet1_A3, "/Sheet1/", "A3", 2.0).
?test(sheet1_B3, "/Sheet1/", "B3", 3.0).
?test(sheet1_C3, "/Sheet1/", "C3", 2.5).
?test(sheet1_D3, "/Sheet1/", "D3", 2.75).
?test(sheet1_E3, "/Sheet1/", "E3", 2.625).
?test(sheet1_A4, "/Sheet1/", "A4", 3.0).
?test(sheet1_B4, "/Sheet1/", "B4", 4.0).
?test(sheet1_C4, "/Sheet1/", "C4", 3.5).
?test(sheet1_D4, "/Sheet1/", "D4", 3.75).
?test(sheet1_E4, "/Sheet1/", "E4", 3.625).
?test(sheet1_A7, "/Sheet1/", "A7", "Numbers->").
?test(sheet1_B7, "/Sheet1/", "B7", 10.0).
?test(sheet1_C7, "/Sheet1/", "C7", 9.0).
?test(sheet1_D7, "/Sheet1/", "D7", 8.0).
?test(sheet1_E7, "/Sheet1/", "E7", 7.0).
?test(sheet1_F7, "/Sheet1/", "F7", 6.0).
?test(sheet1_G7, "/Sheet1/", "G7", 5.0).
?test(sheet1_H7, "/Sheet1/", "H7", 4.0).
?test(sheet1_I7, "/Sheet1/", "I7", 3.0).
?test(sheet1_J7, "/Sheet1/", "J7", 2.0).
?test(sheet1_K7, "/Sheet1/", "K7", 1.0).
?test(sheet1_A8, "/Sheet1/", "A8", "Formulae ->").
?test(sheet1_B8, "/Sheet1/", "B8", 990.0).
?test(sheet1_C8, "/Sheet1/", "C8", 891.0).
?test(sheet1_D8, "/Sheet1/", "D8", 792.0).
?test(sheet1_E8, "/Sheet1/", "E8", 693.0).
?test(sheet1_F8, "/Sheet1/", "F8", 594.0).
?test(sheet1_G8, "/Sheet1/", "G8", 495.0).
?test(sheet1_H8, "/Sheet1/", "H8", 396.0).
?test(sheet1_I8, "/Sheet1/", "I8", 297.0).
?test(sheet1_J8, "/Sheet1/", "J8", 198.0).
?test(sheet1_K8, "/Sheet1/", "K8", 99.0).
?test(sheet1_B10, "/Sheet1/", "B10", "Numbers").
?test(sheet1_C10, "/Sheet1/", "C10", "Formulae").
?test(sheet1_B11, "/Sheet1/", "B11", 100.0).
?test(sheet1_C11, "/Sheet1/", "C11", 99900.0).
?test(sheet1_B12, "/Sheet1/", "B12", 90.0).
?test(sheet1_C12, "/Sheet1/", "C12", 89910.0).
?test(sheet1_B13, "/Sheet1/", "B13", 80.0).
?test(sheet1_C13, "/Sheet1/", "C13", 79920.0).
?test(sheet1_B14, "/Sheet1/", "B14", 70.0).
?test(sheet1_C14, "/Sheet1/", "C14", 69930.0).
?test(sheet1_B15, "/Sheet1/", "B15", 60.0).
?test(sheet1_C15, "/Sheet1/", "C15", 59940.0).
?test(sheet1_B16, "/Sheet1/", "B16", 50.0).
?test(sheet1_C16, "/Sheet1/", "C16", 49950.0).
?test(sheet1_B17, "/Sheet1/", "B17", 40.0).
?test(sheet1_C17, "/Sheet1/", "C17", 39960.0).
?test(sheet1_B18, "/Sheet1/", "B18", 30.0).
?test(sheet1_C18, "/Sheet1/", "C18", 29970.0).
?test(sheet1_B19, "/Sheet1/", "B19", 20.0).
?test(sheet1_C19, "/Sheet1/", "C19", 19980.0).
?test(sheet1_B20, "/Sheet1/", "B20", 10.0).
?test(sheet1_C20, "/Sheet1/", "C20", 9990.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_shared_formulae.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_shared_formulae" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A2,
        sheet1_B2,
        sheet1_C2,
        sheet1_D2,
        sheet1_E2,
        sheet1_A3,
        sheet1_B3,
        sheet1_C3,
        sheet1_D3,
        sheet1_E3,
        sheet1_A4,
        sheet1_B4,
        sheet1_C4,
        sheet1_D4,
        sheet1_E4,
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
        sheet1_B10,
        sheet1_C10,
        sheet1_B11,
        sheet1_C11,
        sheet1_B12,
        sheet1_C12,
        sheet1_B13,
        sheet1_C13,
        sheet1_B14,
        sheet1_C14,
        sheet1_B15,
        sheet1_C15,
        sheet1_B16,
        sheet1_C16,
        sheet1_B17,
        sheet1_C17,
        sheet1_B18,
        sheet1_C18,
        sheet1_B19,
        sheet1_C19,
        sheet1_B20,
        sheet1_C20
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
