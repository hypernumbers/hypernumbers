%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_floats_SUITE).
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
                     [Testcase, "b_floats_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_floats" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This test tests basic floats").
?test(sheet1_A2, "/Sheet1/", "A2", 3.0).
?test(sheet1_A3, "/Sheet1/", "A3", 3.3).
?test(sheet1_A4, "/Sheet1/", "A4", 0.3).
?test(sheet1_A5, "/Sheet1/", "A5", 0.03).
?test(sheet1_A6, "/Sheet1/", "A6", 0.0003).
?test(sheet1_A7, "/Sheet1/", "A7", 3.0e-05).
?test(sheet1_A8, "/Sheet1/", "A8", 3.0e-06).
?test(sheet1_A9, "/Sheet1/", "A9", 3.0e-07).
?test(sheet1_A10, "/Sheet1/", "A10", 3.0e-08).
?test(sheet1_A11, "/Sheet1/", "A11", 3.0e-09).
?test(sheet1_A12, "/Sheet1/", "A12", 3.0e-10).
?test(sheet1_A13, "/Sheet1/", "A13", 0.5).
?test(sheet1_B13, "/Sheet1/", "B13", "<- these are to determine what floats will reverse compile identically").
?test(sheet1_A14, "/Sheet1/", "A14", 0.05).
?test(sheet1_B14, "/Sheet1/", "B14", "<- these are to determine what floats will reverse compile identically").
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_floats.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_floats" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A3,
        sheet1_A4,
        sheet1_A5,
        sheet1_A6,
        sheet1_A7,
        sheet1_A8,
        sheet1_A9,
        sheet1_A10,
        sheet1_A11,
        sheet1_A12,
        sheet1_A13,
        sheet1_B13,
        sheet1_A14,
        sheet1_B14
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
