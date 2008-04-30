%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_simple_arrays_and_ranges_SUITE).
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
                     [Testcase, "b_simple_arrays_and_ranges_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_simple_arrays_and_ranges" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_B1, "/Sheet1/", "B1", "Single Element Array I").
?test(sheet1_C1, "/Sheet1/", "C1", 1.0).
?test(sheet1_B2, "/Sheet1/", "B2", "Single Element Array 2").
?test(sheet1_C2, "/Sheet1/", "C2", "2").
?test(sheet1_B3, "/Sheet1/", "B3", "Single Element Array 3").
?test(sheet1_C3, "/Sheet1/", "C3", true).
?test(sheet1_B4, "/Sheet1/", "B4", "Single Element Array 4").
?test(sheet1_C4, "/Sheet1/", "C4", 'NULL!').
?test(sheet1_B5, "/Sheet1/", "B5", "Multiple Element Array").
?test(sheet1_C5, "/Sheet1/", "C5", 2.0).
?test(sheet1_B6, "/Sheet1/", "B6", "2D Array").
?test(sheet1_C6, "/Sheet1/", "C6", 33.0).
?test(sheet1_B7, "/Sheet1/", "B7", "3D Array").
?test(sheet1_C7, "/Sheet1/", "C7", 44.0).
?test(sheet1_A8, "/Sheet1/", "A8", "Bruk ->").
?test(sheet1_B8, "/Sheet1/", "B8", "Range").
?test(sheet1_C8, "/Sheet1/", "C8", 6.0).
?test(sheet1_D8, "/Sheet1/", "D8", 1.0).
?test(sheet1_E8, "/Sheet1/", "E8", 2.0).
?test(sheet1_F8, "/Sheet1/", "F8", 3.0).
?test(sheet1_A9, "/Sheet1/", "A9", "Bruk ->").
?test(sheet1_B9, "/Sheet1/", "B9", "Intersection").
?test(sheet1_C9, "/Sheet1/", "C9", 555.0).
?test(sheet1_D9, "/Sheet1/", "D9", 1.0).
?test(sheet1_E9, "/Sheet1/", "E9", 555.0).
?test(sheet1_F9, "/Sheet1/", "F9", 1.0).
?test(sheet1_G9, "/Sheet1/", "G9", 1.0).
?test(sheet1_B10, "/Sheet1/", "B10", "Union").
?test(sheet1_C10, "/Sheet1/", "C10", 2442.0).
?test(sheet1_D10, "/Sheet1/", "D10", 444.0).
?test(sheet1_E10, "/Sheet1/", "E10", 555.0).
?test(sheet1_F10, "/Sheet1/", "F10", 666.0).
?test(sheet1_G10, "/Sheet1/", "G10", 777.0).
?test(sheet1_B11, "/Sheet1/", "B11", "Complex I").
?test(sheet1_C11, "/Sheet1/", "C11", 34.0).
?test(sheet1_D11, "/Sheet1/", "D11", 9.0).
?test(sheet1_E11, "/Sheet1/", "E11", 8.0).
?test(sheet1_F11, "/Sheet1/", "F11", 7.0).
?test(sheet1_G11, "/Sheet1/", "G11", 6.0).
?test(sheet1_H11, "/Sheet1/", "H11", 5.0).
?test(sheet1_I11, "/Sheet1/", "I11", 4.0).
?test(sheet1_B12, "/Sheet1/", "B12", "Complex II").
?test(sheet1_C12, "/Sheet1/", "C12", 7.0).
?test(sheet1_D12, "/Sheet1/", "D12", 3.0).
?test(sheet1_E12, "/Sheet1/", "E12", 4.0).
?test(sheet1_F12, "/Sheet1/", "F12", 5.0).
?test(sheet1_G12, "/Sheet1/", "G12", 6.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_simple_arrays_and_ranges.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_simple_arrays_and_ranges" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_B1,
        sheet1_C1,
        sheet1_B2,
        sheet1_C2,
        sheet1_B3,
        sheet1_C3,
        sheet1_B4,
        sheet1_C4,
        sheet1_B5,
        sheet1_C5,
        sheet1_B6,
        sheet1_C6,
        sheet1_B7,
        sheet1_C7,
        sheet1_A8,
        sheet1_B8,
        sheet1_C8,
        sheet1_D8,
        sheet1_E8,
        sheet1_F8,
        sheet1_A9,
        sheet1_B9,
        sheet1_C9,
        sheet1_D9,
        sheet1_E9,
        sheet1_F9,
        sheet1_G9,
        sheet1_B10,
        sheet1_C10,
        sheet1_D10,
        sheet1_E10,
        sheet1_F10,
        sheet1_G10,
        sheet1_B11,
        sheet1_C11,
        sheet1_D11,
        sheet1_E11,
        sheet1_F11,
        sheet1_G11,
        sheet1_H11,
        sheet1_I11,
        sheet1_B12,
        sheet1_C12,
        sheet1_D12,
        sheet1_E12,
        sheet1_F12,
        sheet1_G12
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
