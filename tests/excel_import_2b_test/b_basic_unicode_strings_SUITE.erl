%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_basic_unicode_strings_SUITE).
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
                     [Testcase, "b_basic_unicode_strings_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_basic_unicode_strings" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This just tests some Unicode strings").
?test(sheet1_A2, "/Sheet1/", "A2", "Some simple stuff").
?test(sheet1_B2, "/Sheet1/", "B2", "β").
?test(sheet1_B3, "/Sheet1/", "B3", "kfdks45678dkβsfjk").
?test(sheet1_A5, "/Sheet1/", "A5", "From the Word Insert Special Character dialog box").
?test(sheet1_A6, "/Sheet1/", "A6", "Latin1 Supplement").
?test(sheet1_B6, "/Sheet1/", "B6", "áâãäåæ").
?test(sheet1_A7, "/Sheet1/", "A7", "Latin Extended A").
?test(sheet1_B7, "/Sheet1/", "B7", "ĀāĂăĄą").
?test(sheet1_A8, "/Sheet1/", "A8", "Latin Extended B").
?test(sheet1_B8, "/Sheet1/", "B8", "ƒǺǻǼǽǾ").
?test(sheet1_A9, "/Sheet1/", "A9", "Spacing Modifying Letters").
?test(sheet1_B9, "/Sheet1/", "B9", "aˆbˇcˇdˉeˉf˘g˘h˙i˙").
?test(sheet1_A10, "/Sheet1/", "A10", "Combining Diacritical Marks").
?test(sheet1_B10, "/Sheet1/", "B10", "àb́ĉd̃̄ĕḟ̈̊g̋ȟ").
?test(sheet1_A11, "/Sheet1/", "A11", "Greek And Coptic").
?test(sheet1_B11, "/Sheet1/", "B11", "ʹ͵;΄΅Ά·ΈΑΒΓΔ").
?test(sheet1_A12, "/Sheet1/", "A12", "Cyrillic").
?test(sheet1_B12, "/Sheet1/", "B12", "ЀЁЉЊЋЌЍ").
?test(sheet1_A13, "/Sheet1/", "A13", "Latin Extended Additional").
?test(sheet1_B13, "/Sheet1/", "B13", "ẀẁẂẃẄẅỲỳ").
?test(sheet1_A14, "/Sheet1/", "A14", "General Punctuation").
?test(sheet1_B14, "/Sheet1/", "B14", "‐–—―‘’‚").
?test(sheet1_A15, "/Sheet1/", "A15", "Superscripts and Subscripts").
?test(sheet1_B15, "/Sheet1/", "B15", "⁰⁴⁵⁶₇₈₉₊₋₌₍₎").
?test(sheet1_A16, "/Sheet1/", "A16", "Currency Symbols").
?test(sheet1_B16, "/Sheet1/", "B16", "€").
?test(sheet1_A17, "/Sheet1/", "A17", "Letterlike Symbols").
?test(sheet1_B17, "/Sheet1/", "B17", "ℓ№℗℠™Ω").
?test(sheet1_A18, "/Sheet1/", "A18", "Number Forms").
?test(sheet1_B18, "/Sheet1/", "B18", "⅓⅔⅕⅖⅗⅘").
?test(sheet1_A19, "/Sheet1/", "A19", "Arrows").
?test(sheet1_B19, "/Sheet1/", "B19", "←↑→↓↔↕↖↗↘↙").
?test(sheet1_A20, "/Sheet1/", "A20", "Mathematical Operators").
?test(sheet1_B20, "/Sheet1/", "B20", "∂∆∏∑").
?test(sheet1_A21, "/Sheet1/", "A21", "Geometric Shapes").
?test(sheet1_B21, "/Sheet1/", "B21", "◊").
?test(sheet1_A22, "/Sheet1/", "A22", "Alphabetic Presentation Forms").
?test(sheet1_B22, "/Sheet1/", "B22", "ﬀﬁﬂﬃﬄ").
?test(sheet1_A24, "/Sheet1/", "A24", "The above in a function").
?test(sheet1_A25, "/Sheet1/", "A25", "Some simple stuff").
?test(sheet1_B25, "/Sheet1/", "B25", "β").
?test(sheet1_B26, "/Sheet1/", "B26", "kfdks45678dkβsfjk").
?test(sheet1_A27, "/Sheet1/", "A27", "Latin1 Supplement").
?test(sheet1_B27, "/Sheet1/", "B27", "áâãäåæ").
?test(sheet1_A28, "/Sheet1/", "A28", "Latin Extended A").
?test(sheet1_B28, "/Sheet1/", "B28", "ĀāĂăĄą").
?test(sheet1_A29, "/Sheet1/", "A29", "Latin Extended B").
?test(sheet1_B29, "/Sheet1/", "B29", "ƒǺǻǼǽǾ").
?test(sheet1_A30, "/Sheet1/", "A30", "Spacing Modifying Letters").
?test(sheet1_B30, "/Sheet1/", "B30", "aˆbˇcˇdˉeˉf˘g˘h˙i˙").
?test(sheet1_A31, "/Sheet1/", "A31", "Combining Diacritical Marks").
?test(sheet1_B31, "/Sheet1/", "B31", "àb́ĉd̃̄ĕḟ̈̊g̋ȟ").
?test(sheet1_A32, "/Sheet1/", "A32", "Greek And Coptic").
?test(sheet1_B32, "/Sheet1/", "B32", "ʹ͵;΄΅Ά·ΈΑΒΓΔ").
?test(sheet1_A33, "/Sheet1/", "A33", "Cyrillic").
?test(sheet1_B33, "/Sheet1/", "B33", "ЀЁЉЊЋЌЍ").
?test(sheet1_A34, "/Sheet1/", "A34", "Latin Extended Additional").
?test(sheet1_B34, "/Sheet1/", "B34", "ẀẁẂẃẄẅỲỳ").
?test(sheet1_A35, "/Sheet1/", "A35", "General Punctuation").
?test(sheet1_B35, "/Sheet1/", "B35", "‐–—―‘’‚").
?test(sheet1_A36, "/Sheet1/", "A36", "Superscripts and Subscripts").
?test(sheet1_B36, "/Sheet1/", "B36", "⁰⁴⁵⁶₇₈₉₊₋₌₍₎").
?test(sheet1_A37, "/Sheet1/", "A37", "Currency Symbols").
?test(sheet1_B37, "/Sheet1/", "B37", "€").
?test(sheet1_A38, "/Sheet1/", "A38", "Letterlike Symbols").
?test(sheet1_B38, "/Sheet1/", "B38", "ℓ№℗℠™Ω").
?test(sheet1_A39, "/Sheet1/", "A39", "Number Forms").
?test(sheet1_B39, "/Sheet1/", "B39", "⅓⅔⅕⅖⅗⅘").
?test(sheet1_A40, "/Sheet1/", "A40", "Arrows").
?test(sheet1_B40, "/Sheet1/", "B40", "←↑→↓↔↕↖↗↘↙").
?test(sheet1_A41, "/Sheet1/", "A41", "Mathematical Operators").
?test(sheet1_B41, "/Sheet1/", "B41", "∂∆∏∑").
?test(sheet1_A42, "/Sheet1/", "A42", "Geometric Shapes").
?test(sheet1_B42, "/Sheet1/", "B42", "◊").
?test(sheet1_A43, "/Sheet1/", "A43", "Alphabetic Presentation Forms").
?test(sheet1_B43, "/Sheet1/", "B43", "ﬀﬁﬂﬃﬄ").
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_basic_unicode_strings.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_basic_unicode_strings" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_B3,
        sheet1_A5,
        sheet1_A6,
        sheet1_B6,
        sheet1_A7,
        sheet1_B7,
        sheet1_A8,
        sheet1_B8,
        sheet1_A9,
        sheet1_B9,
        sheet1_A10,
        sheet1_B10,
        sheet1_A11,
        sheet1_B11,
        sheet1_A12,
        sheet1_B12,
        sheet1_A13,
        sheet1_B13,
        sheet1_A14,
        sheet1_B14,
        sheet1_A15,
        sheet1_B15,
        sheet1_A16,
        sheet1_B16,
        sheet1_A17,
        sheet1_B17,
        sheet1_A18,
        sheet1_B18,
        sheet1_A19,
        sheet1_B19,
        sheet1_A20,
        sheet1_B20,
        sheet1_A21,
        sheet1_B21,
        sheet1_A22,
        sheet1_B22,
        sheet1_A24,
        sheet1_A25,
        sheet1_B25,
        sheet1_B26,
        sheet1_A27,
        sheet1_B27,
        sheet1_A28,
        sheet1_B28,
        sheet1_A29,
        sheet1_B29,
        sheet1_A30,
        sheet1_B30,
        sheet1_A31,
        sheet1_B31,
        sheet1_A32,
        sheet1_B32,
        sheet1_A33,
        sheet1_B33,
        sheet1_A34,
        sheet1_B34,
        sheet1_A35,
        sheet1_B35,
        sheet1_A36,
        sheet1_B36,
        sheet1_A37,
        sheet1_B37,
        sheet1_A38,
        sheet1_B38,
        sheet1_A39,
        sheet1_B39,
        sheet1_A40,
        sheet1_B40,
        sheet1_A41,
        sheet1_B41,
        sheet1_A42,
        sheet1_B42,
        sheet1_A43,
        sheet1_B43
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
