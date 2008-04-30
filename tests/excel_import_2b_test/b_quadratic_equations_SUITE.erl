%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_quadratic_equations_SUITE).
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
                     [Testcase, "b_quadratic_equations_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_quadratic_equations" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "Quadratic Equation Solver").
?test(sheet1_D1, "/Sheet1/", "D1", "").
?test(sheet1_A3, "/Sheet1/", "A3", "A:").
?test(sheet1_B3, "/Sheet1/", "B3", 3.5).
?test(sheet1_C3, "/Sheet1/", "C3", 1.0).
?test(sheet1_D3, "/Sheet1/", "D3", 1.0).
?test(sheet1_E3, "/Sheet1/", "E3", 12.0).
?test(sheet1_F3, "/Sheet1/", "F3", 890.9).
?test(sheet1_A4, "/Sheet1/", "A4", "B:").
?test(sheet1_B4, "/Sheet1/", "B4", 14.75).
?test(sheet1_C4, "/Sheet1/", "C4", 20.0).
?test(sheet1_D4, "/Sheet1/", "D4", 0.0).
?test(sheet1_E4, "/Sheet1/", "E4", 101.2).
?test(sheet1_F4, "/Sheet1/", "F4", 1011.45).
?test(sheet1_A5, "/Sheet1/", "A5", "C:").
?test(sheet1_B5, "/Sheet1/", "B5", 4.78).
?test(sheet1_C5, "/Sheet1/", "C5", 1.0).
?test(sheet1_D5, "/Sheet1/", "D5", 0.0).
?test(sheet1_E5, "/Sheet1/", "E5", 9.0).
?test(sheet1_F5, "/Sheet1/", "F5", 123.9).
?test(sheet1_A7, "/Sheet1/", "A7", "Answers:").
?test(sheet1_B7, "/Sheet1/", "B7", -0.353764194476439).
?test(sheet1_C7, "/Sheet1/", "C7", -0.0501256289338006).
?test(sheet1_D7, "/Sheet1/", "D7", 0.0).
?test(sheet1_E7, "/Sheet1/", "E7", -0.0898909545909816).
?test(sheet1_F7, "/Sheet1/", "F7", -0.139683370975035).
?test(sheet1_B8, "/Sheet1/", "B8", -3.86052151980928).
?test(sheet1_C8, "/Sheet1/", "C8", -19.9498743710662).
?test(sheet1_D8, "/Sheet1/", "D8", 0.0).
?test(sheet1_E8, "/Sheet1/", "E8", -8.34344237874235).
?test(sheet1_F8, "/Sheet1/", "F8", -0.995629234255631).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_quadratic_equations.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_quadratic_equations" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_D1,
        sheet1_A3,
        sheet1_B3,
        sheet1_C3,
        sheet1_D3,
        sheet1_E3,
        sheet1_F3,
        sheet1_A4,
        sheet1_B4,
        sheet1_C4,
        sheet1_D4,
        sheet1_E4,
        sheet1_F4,
        sheet1_A5,
        sheet1_B5,
        sheet1_C5,
        sheet1_D5,
        sheet1_E5,
        sheet1_F5,
        sheet1_A7,
        sheet1_B7,
        sheet1_C7,
        sheet1_D7,
        sheet1_E7,
        sheet1_F7,
        sheet1_B8,
        sheet1_C8,
        sheet1_D8,
        sheet1_E8,
        sheet1_F8
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
