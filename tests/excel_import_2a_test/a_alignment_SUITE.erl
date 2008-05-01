%% This file is generated; DO NOT EDIT MANUALLY.

-module(a_alignment_SUITE).
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
                     [Testcase, "a_alignment_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "a_alignment" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This tests that the various test generated have got their columns and rows right with all fence post errors fixed for good measure").
?test(sheet1_A2, "/Sheet1/", "A2", "This is a cell A2 on Sheet1").
?test(sheet1_B2, "/Sheet1/", "B2", "This is cell B2 on Sheet1").
?test(sheet1_A3, "/Sheet1/", "A3", "This is cell A3 on Sheet1").
?test(sheet1_B3, "/Sheet1/", "B3", "This is cell B3 on Sheet1").
?test(sheet1_B4, "/Sheet1/", "B4", "This is cell B4 on Sheet1").
?test(sheet1_D6, "/Sheet1/", "D6", "This is cell D6 on Sheet1").
?test(sheet1_H12, "/Sheet1/", "H12", "This is cell H12 on Sheet1").
?test(sheet1_G13, "/Sheet1/", "G13", "This is cell G13 on Sheet1").
?test(sheet1_H13, "/Sheet1/", "H13", "This is cell H13 on Sheet1").
?test(sheet1_G26, "/Sheet1/", "G26", "This is cell G26 on Sheet1").
?test(sheet1_G27, "/Sheet1/", "G27", "This is the fencepost Cell G27 on Sheet1").
?test(sheet2_A1, "/Sheet2/", "A1", "This tests that the various test generates have got their columns and rows right with all fence post errors fixed for good measure").
?test(sheet2_A2, "/Sheet2/", "A2", "This is a cell A2 on Sheet2").
?test(sheet2_B2, "/Sheet2/", "B2", "This is cell B2 on Sheet2").
?test(sheet2_A3, "/Sheet2/", "A3", "This is cell A3 on Sheet2").
?test(sheet2_B3, "/Sheet2/", "B3", "This is cell B3 on Sheet2").
?test(sheet2_B4, "/Sheet2/", "B4", "This is cell B4 on Sheet2").
?test(sheet2_D6, "/Sheet2/", "D6", "This is cell D6 on Sheet2").
?test(sheet2_H12, "/Sheet2/", "H12", "This is cell H12 on Sheet2").
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "a_alignment.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "a_alignment" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A3,
        sheet1_B3,
        sheet1_B4,
        sheet1_D6,
        sheet1_H12,
        sheet1_G13,
        sheet1_H13,
        sheet1_G26,
        sheet1_G27,
        sheet2_A1,
        sheet2_A2,
        sheet2_B2,
        sheet2_A3,
        sheet2_B3,
        sheet2_B4,
        sheet2_D6,
        sheet2_H12
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
