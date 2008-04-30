%% This file is generated; DO NOT EDIT MANUALLY.

-module(d_gnumeric_date_and_time_SUITE).
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
                     [Testcase, "d_gnumeric_date_and_time_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "d_gnumeric_date_and_time" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "DATE/TIME FUNCTIONS").
?test(sheet1_A3, "/Sheet1/", "A3", "Test Status").
?test(sheet1_C3, "/Sheet1/", "C3", "Accuracy Limit").
?test(sheet1_A4, "/Sheet1/", "A4", '#VALUE!').
?test(sheet1_C4, "/Sheet1/", "C4", 1.0e-07).
?test(sheet1_A7, "/Sheet1/", "A7", "#Succeded").
?test(sheet1_B7, "/Sheet1/", "B7", "#Total").
?test(sheet1_A8, "/Sheet1/", "A8", '#VALUE!').
?test(sheet1_B8, "/Sheet1/", "B8", 18.0).
?test(sheet1_A11, "/Sheet1/", "A11", "Test Data:").
?test(sheet1_A13, "/Sheet1/", "A13", "some birthdays").
?test(sheet1_B13, "/Sheet1/", "B13", "appointments").
?test(sheet1_C13, "/Sheet1/", "C13", "times").
?test(sheet1_D13, "/Sheet1/", "D13", "past & future").
?test(sheet1_A14, "/Sheet1/", "A14", "1975/12/21 00:00:00").
?test(sheet1_B14, "/Sheet1/", "B14", "1999/06/10 09:00:00").
?test(sheet1_C14, "/Sheet1/", "C14", 0.597222222222222).
?test(sheet1_D14, "/Sheet1/", "D14", "1901/01/01 00:00:00").
?test(sheet1_A15, "/Sheet1/", "A15", "1983/06/14 00:00:00").
?test(sheet1_B15, "/Sheet1/", "B15", "1999/06/14 15:30:00").
?test(sheet1_C15, "/Sheet1/", "C15", 0.0).
?test(sheet1_D15, "/Sheet1/", "D15", "2000/01/01 00:00:00").
?test(sheet1_A16, "/Sheet1/", "A16", "1929/02/28 00:00:00").
?test(sheet1_B16, "/Sheet1/", "B16", "1999/06/15 16:50:17").
?test(sheet1_C16, "/Sheet1/", "C16", 0.5).
?test(sheet1_D16, "/Sheet1/", "D16", "2030/01/01 00:00:00").
?test(sheet1_A17, "/Sheet1/", "A17", "1998/09/02 00:00:00").
?test(sheet1_B17, "/Sheet1/", "B17", "2000/01/01 02:00:00").
?test(sheet1_C17, "/Sheet1/", "C17", 0.999988425925926).
?test(sheet1_D17, "/Sheet1/", "D17", "1899/12/31 00:00:00").
?test(sheet1_A20, "/Sheet1/", "A20", "Function").
?test(sheet1_B20, "/Sheet1/", "B20", "1st test").
?test(sheet1_C20, "/Sheet1/", "C20", "Correct").
?test(sheet1_D20, "/Sheet1/", "D20", "2nd test").
?test(sheet1_E20, "/Sheet1/", "E20", "Correct").
?test(sheet1_F20, "/Sheet1/", "F20", "3rd test").
?test(sheet1_G20, "/Sheet1/", "G20", "Correct").
?test(sheet1_H20, "/Sheet1/", "H20", "Status").
?test(sheet1_I20, "/Sheet1/", "I20", "Status message").
?test(sheet1_A21, "/Sheet1/", "A21", "DATE").
?test(sheet1_B21, "/Sheet1/", "B21", "1975/12/21 00:00:00").
?test(sheet1_C21, "/Sheet1/", "C21", "1975/12/21 00:00:00").
?test(sheet1_D21, "/Sheet1/", "D21", "1929/02/28 00:00:00").
?test(sheet1_E21, "/Sheet1/", "E21", "1929/02/28 00:00:00").
?test(sheet1_F21, "/Sheet1/", "F21", "1998/09/02 00:00:00").
?test(sheet1_G21, "/Sheet1/", "G21", "1998/09/02 00:00:00").
?test(sheet1_H21, "/Sheet1/", "H21", 1.0).
?test(sheet1_I21, "/Sheet1/", "I21", "Ok.").
?test(sheet1_A22, "/Sheet1/", "A22", "DATEVALUE").
?test(sheet1_B22, "/Sheet1/", "B22", 27749.0).
?test(sheet1_C22, "/Sheet1/", "C22", 27749.0).
?test(sheet1_D22, "/Sheet1/", "D22", 10652.0).
?test(sheet1_E22, "/Sheet1/", "E22", 10652.0).
?test(sheet1_F22, "/Sheet1/", "F22", 35854.0).
?test(sheet1_G22, "/Sheet1/", "G22", 35854.0).
?test(sheet1_H22, "/Sheet1/", "H22", 1.0).
?test(sheet1_I22, "/Sheet1/", "I22", "Ok.").
?test(sheet1_A23, "/Sheet1/", "A23", "DAY").
?test(sheet1_B23, "/Sheet1/", "B23", 21.0).
?test(sheet1_C23, "/Sheet1/", "C23", 21.0).
?test(sheet1_D23, "/Sheet1/", "D23", 10.0).
?test(sheet1_E23, "/Sheet1/", "E23", 10.0).
?test(sheet1_F23, "/Sheet1/", "F23", 1.0).
?test(sheet1_G23, "/Sheet1/", "G23", 1.0).
?test(sheet1_H23, "/Sheet1/", "H23", 1.0).
?test(sheet1_I23, "/Sheet1/", "I23", "Ok.").
?test(sheet1_A24, "/Sheet1/", "A24", "DAYS360").
?test(sheet1_B24, "/Sheet1/", "B24", 25022.0).
?test(sheet1_C24, "/Sheet1/", "C24", 25022.0).
?test(sheet1_D24, "/Sheet1/", "D24", 35640.0).
?test(sheet1_E24, "/Sheet1/", "E24", 35640.0).
?test(sheet1_F24, "/Sheet1/", "F24", 5.0).
?test(sheet1_G24, "/Sheet1/", "G24", 5.0).
?test(sheet1_H24, "/Sheet1/", "H24", 1.0).
?test(sheet1_I24, "/Sheet1/", "I24", "Ok.").
?test(sheet1_A25, "/Sheet1/", "A25", "EDATE").
?test(sheet1_B25, "/Sheet1/", "B25", '#VALUE!').
?test(sheet1_C25, "/Sheet1/", "C25", 35841.0).
?test(sheet1_D25, "/Sheet1/", "D25", '#VALUE!').
?test(sheet1_E25, "/Sheet1/", "E25", 35779.0).
?test(sheet1_F25, "/Sheet1/", "F25", '#VALUE!').
?test(sheet1_G25, "/Sheet1/", "G25", 35869.0).
?test(sheet1_H25, "/Sheet1/", "H25", '#VALUE!').
?test(sheet1_I25, "/Sheet1/", "I25", '#VALUE!').
?test(sheet1_A26, "/Sheet1/", "A26", "EOMONTH").
?test(sheet1_B26, "/Sheet1/", "B26", '#VALUE!').
?test(sheet1_C26, "/Sheet1/", "C26", 35854.0).
?test(sheet1_D26, "/Sheet1/", "D26", '#VALUE!').
?test(sheet1_E26, "/Sheet1/", "E26", 35795.0).
?test(sheet1_F26, "/Sheet1/", "F26", '#VALUE!').
?test(sheet1_G26, "/Sheet1/", "G26", 35885.0).
?test(sheet1_H26, "/Sheet1/", "H26", '#VALUE!').
?test(sheet1_I26, "/Sheet1/", "I26", '#VALUE!').
?test(sheet1_A27, "/Sheet1/", "A27", "HOUR").
?test(sheet1_B27, "/Sheet1/", "B27", 0.0).
?test(sheet1_C27, "/Sheet1/", "C27", 0.0).
?test(sheet1_D27, "/Sheet1/", "D27", 9.0).
?test(sheet1_E27, "/Sheet1/", "E27", 9.0).
?test(sheet1_F27, "/Sheet1/", "F27", 14.0).
?test(sheet1_G27, "/Sheet1/", "G27", 14.0).
?test(sheet1_H27, "/Sheet1/", "H27", 1.0).
?test(sheet1_I27, "/Sheet1/", "I27", "Ok.").
?test(sheet1_A28, "/Sheet1/", "A28", "MINUTE").
?test(sheet1_B28, "/Sheet1/", "B28", 0.0).
?test(sheet1_C28, "/Sheet1/", "C28", 0.0).
?test(sheet1_D28, "/Sheet1/", "D28", 50.0).
?test(sheet1_E28, "/Sheet1/", "E28", 50.0).
?test(sheet1_F28, "/Sheet1/", "F28", 59.0).
?test(sheet1_G28, "/Sheet1/", "G28", 59.0).
?test(sheet1_H28, "/Sheet1/", "H28", 1.0).
?test(sheet1_I28, "/Sheet1/", "I28", "Ok.").
?test(sheet1_A29, "/Sheet1/", "A29", "MONTH").
?test(sheet1_B29, "/Sheet1/", "B29", 6.0).
?test(sheet1_C29, "/Sheet1/", "C29", 6.0).
?test(sheet1_D29, "/Sheet1/", "D29", 1.0).
?test(sheet1_E29, "/Sheet1/", "E29", 1.0).
?test(sheet1_F29, "/Sheet1/", "F29", 1.0).
?test(sheet1_G29, "/Sheet1/", "G29", 1.0).
?test(sheet1_H29, "/Sheet1/", "H29", 1.0).
?test(sheet1_I29, "/Sheet1/", "I29", "Ok.").
?test(sheet1_A30, "/Sheet1/", "A30", "NETWORKDAYS").
?test(sheet1_B30, "/Sheet1/", "B30", 607.0).
?test(sheet1_C30, "/Sheet1/", "C30", 607.0).
?test(sheet1_D30, "/Sheet1/", "D30", 606.0).
?test(sheet1_E30, "/Sheet1/", "E30", 606.0).
?test(sheet1_F30, "/Sheet1/", "F30", 605.0).
?test(sheet1_G30, "/Sheet1/", "G30", 605.0).
?test(sheet1_H30, "/Sheet1/", "H30", 1.0).
?test(sheet1_I30, "/Sheet1/", "I30", "Ok.").
?test(sheet1_A31, "/Sheet1/", "A31", "NOW").
?test(sheet1_B31, "/Sheet1/", "B31", "2008/04/29 14:56:04").
?test(sheet1_H31, "/Sheet1/", "H31", 1.0).
?test(sheet1_I31, "/Sheet1/", "I31", "Ok.").
?test(sheet1_A32, "/Sheet1/", "A32", "SECOND").
?test(sheet1_B32, "/Sheet1/", "B32", 0.0).
?test(sheet1_C32, "/Sheet1/", "C32", 0.0).
?test(sheet1_D32, "/Sheet1/", "D32", 17.0).
?test(sheet1_E32, "/Sheet1/", "E32", 17.0).
?test(sheet1_F32, "/Sheet1/", "F32", 59.0).
?test(sheet1_G32, "/Sheet1/", "G32", 59.0).
?test(sheet1_H32, "/Sheet1/", "H32", 1.0).
?test(sheet1_I32, "/Sheet1/", "I32", "Ok.").
?test(sheet1_A33, "/Sheet1/", "A33", "TIME").
?test(sheet1_B33, "/Sheet1/", "B33", 0.0).
?test(sheet1_C33, "/Sheet1/", "C33", 0.0).
?test(sheet1_D33, "/Sheet1/", "D33", 0.597222222222222).
?test(sheet1_E33, "/Sheet1/", "E33", 0.597222222222222).
?test(sheet1_F33, "/Sheet1/", "F33", 0.999988425925926).
?test(sheet1_G33, "/Sheet1/", "G33", 0.999988425925926).
?test(sheet1_H33, "/Sheet1/", "H33", 1.0).
?test(sheet1_I33, "/Sheet1/", "I33", "Ok.").
?test(sheet1_A34, "/Sheet1/", "A34", "TIMEVALUE").
?test(sheet1_B34, "/Sheet1/", "B34", 0.0).
?test(sheet1_C34, "/Sheet1/", "C34", 0.0).
?test(sheet1_D34, "/Sheet1/", "D34", 0.597222222222222).
?test(sheet1_E34, "/Sheet1/", "E34", 0.597222222222222).
?test(sheet1_F34, "/Sheet1/", "F34", 0.999988425925926).
?test(sheet1_G34, "/Sheet1/", "G34", 0.999988425925926).
?test(sheet1_H34, "/Sheet1/", "H34", 1.0).
?test(sheet1_I34, "/Sheet1/", "I34", "Ok.").
?test(sheet1_A35, "/Sheet1/", "A35", "TODAY").
?test(sheet1_B35, "/Sheet1/", "B35", "2008/04/29 00:00:00").
?test(sheet1_H35, "/Sheet1/", "H35", 1.0).
?test(sheet1_I35, "/Sheet1/", "I35", "Ok.").
?test(sheet1_A36, "/Sheet1/", "A36", "WEEKDAY").
?test(sheet1_B36, "/Sheet1/", "B36", 1.0).
?test(sheet1_C36, "/Sheet1/", "C36", 1.0).
?test(sheet1_D36, "/Sheet1/", "D36", 5.0).
?test(sheet1_E36, "/Sheet1/", "E36", 5.0).
?test(sheet1_F36, "/Sheet1/", "F36", 3.0).
?test(sheet1_G36, "/Sheet1/", "G36", 3.0).
?test(sheet1_H36, "/Sheet1/", "H36", 1.0).
?test(sheet1_I36, "/Sheet1/", "I36", "Ok.").
?test(sheet1_A37, "/Sheet1/", "A37", "WORKDAY").
?test(sheet1_B37, "/Sheet1/", "B37", 36346.0).
?test(sheet1_C37, "/Sheet1/", "C37", 36346.0).
?test(sheet1_D37, "/Sheet1/", "D37", 36336.0).
?test(sheet1_E37, "/Sheet1/", "E37", 36336.0).
?test(sheet1_F37, "/Sheet1/", "F37", 36486.0).
?test(sheet1_G37, "/Sheet1/", "G37", 36486.0).
?test(sheet1_H37, "/Sheet1/", "H37", 1.0).
?test(sheet1_I37, "/Sheet1/", "I37", "Ok.").
?test(sheet1_A38, "/Sheet1/", "A38", "YEAR").
?test(sheet1_B38, "/Sheet1/", "B38", 1975.0).
?test(sheet1_C38, "/Sheet1/", "C38", 1975.0).
?test(sheet1_D38, "/Sheet1/", "D38", 2000.0).
?test(sheet1_E38, "/Sheet1/", "E38", 2000.0).
?test(sheet1_F38, "/Sheet1/", "F38", 1901.0).
?test(sheet1_G38, "/Sheet1/", "G38", 1901.0).
?test(sheet1_H38, "/Sheet1/", "H38", 1.0).
?test(sheet1_I38, "/Sheet1/", "I38", "Ok.").
?test(sheet1_A39, "/Sheet1/", "A39", "Total").
?test(sheet1_H39, "/Sheet1/", "H39", '#VALUE!').
?test(sheet1_I39, "/Sheet1/", "I39", '#VALUE!').
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "d_gnumeric_date_and_time.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "d_gnumeric_date_and_time" ++ "/" ++ Sheetname ++ "/",
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
        sheet1_A11,
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
        sheet1_H35,
        sheet1_I35,
        sheet1_A36,
        sheet1_B36,
        sheet1_C36,
        sheet1_D36,
        sheet1_E36,
        sheet1_F36,
        sheet1_G36,
        sheet1_H36,
        sheet1_I36,
        sheet1_A37,
        sheet1_B37,
        sheet1_C37,
        sheet1_D37,
        sheet1_E37,
        sheet1_F37,
        sheet1_G37,
        sheet1_H37,
        sheet1_I37,
        sheet1_A38,
        sheet1_B38,
        sheet1_C38,
        sheet1_D38,
        sheet1_E38,
        sheet1_F38,
        sheet1_G38,
        sheet1_H38,
        sheet1_I38,
        sheet1_A39,
        sheet1_H39,
        sheet1_I39
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
