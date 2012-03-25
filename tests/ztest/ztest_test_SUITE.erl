%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Test suite for z-tests
%%%
%%% @end
%%% Created :  24 Mar 2012 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(ztest_test_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(SITE, "http://tests.hypernumbers.dev:9000").
-define(SLEEP, 1000).

-include_lib("hypernumbers/include/spriki.hrl").
-include("ct.hrl").

%% Test server callback functions
%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case hn_setup:site_exists(?SITE) of
        true  -> ok = hn_setup:delete_site(?SITE);
        false -> ok
    end,
    hn_setup:site(?SITE, blank, []),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = hn_setup:delete_site(?SITE).

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Initiation before each test case
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%%
%% Returns a list of all test cases in this test suite
%%
%% @spec all() -> TestCases
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     test1,
     test2,
     test3,
     test4,
     test5,
     test6,
     test7,
     test8,
     test9,
     test10,
     test11,
     test12,
     test13
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
test1() ->
    [{doc, "Add a z-fn"}].

test1(Config) when is_list(Config) ->
    ok = load_vanilla_zformula("test1"),
    Got = read_raw("test1"),
    pass(0, Got).

test2() ->
    [{doc, "Add a value and then a z-fn"}].

test2(Config) when is_list(Config) ->
    Path = "test2",
    ok = load_values(Path, 1),
    ok = load_vanilla_zformula(Path),
    Got = read_raw(Path),
    pass(1, Got).

test3() ->
    [{doc, "Add a z-fn and then a value and wait for the recalc"}].

test3(Config) when is_list(Config) ->
    Path = "test3",
    ok = load_vanilla_zformula(Path),
    ok = load_values(Path, 1),
    Got = read_raw(Path),
    pass(1, Got).

test4() ->
    [{doc, "Force a recalc"}].

test4(Config) when is_list(Config) ->
    Path = "test4",
    ok = load_values(Path, 1),
    ok = load_vanilla_zformula(Path),
    ok = poke10(Path, 1),
    Got = read_raw(Path),
    pass(10, Got).

test5() ->
    [{doc, "Add lots of values and then force a recalc"}].

test5(Config) when is_list(Config) ->
    Path = "test5",
    ok = load_values(Path, 11),
    ok = load_vanilla_zformula(Path),
    ok = poke10(Path, 1),
    Got = read_raw(Path),
    pass(20, Got).

test6() ->
    [{doc, "Add lots of values and then force a recalc - "
      ++ "see if we can get a race condition"}].

test6(Config) when is_list(Config) ->
    Path = "test6",
    ok = load_values(Path, 110),
    ok = load_vanilla_zformula(Path),
    ok = poke10(Path, 1),
    ok = poke10(Path, 2),
    ok = poke10(Path, 3),
    ok = poke10(Path, 4),
    ok = poke10(Path, 5),
    ok = poke10(Path, 6),
    ok = poke10(Path, 7),
    ok = poke10(Path, 8),
    ok = poke10(Path, 9),
    ok = poke10(Path, 10),
    Got = read_raw(Path),
    pass(200, Got).

test7() ->
    [{doc, "Add a complex z-fn"}].

test7(Config) when is_list(Config) ->
    Path = "test7",
    ok = load_complex_zformula(Path),
    Got = read_raw(Path),
    pass(0, Got).

test8() ->
    [{doc, "Add a value and then a complex z-fn"}].

test8(Config) when is_list(Config) ->
    Path = "test8",
    ok = load_complex_values(Path, 1),
    ok = load_complex_zformula(Path),
    Got = read_raw(Path),
    pass(1, Got).

test9() ->
    [{doc, "Add a complex z-fn and then a value and wait for the recalc"}].

test9(Config) when is_list(Config) ->
    Path = "test9",
    ok = load_complex_zformula(Path),
    ok = load_complex_values(Path, 1),
    Got = read_raw(Path),
    pass(1, Got).

test10() ->
    [{doc, "Force a recalc"}].

test10(Config) when is_list(Config) ->
    Path = "test10",
    ok = load_values(Path, 1),
    ok = load_complex_zformula(Path),
    ok = dropcomplex(Path, 1),
    Got = read_raw(Path),
    pass(0, Got).

test11() ->
    [{doc, "Add lots of values and then force a recalc - "
      ++ "see if we can get a race condition"}].

test11(Config) when is_list(Config) ->
    Path = "test11",
    ok = load_complex_values(Path, 110),
    ok = load_complex_zformula(Path),
    ok = dropcomplex(Path, 1),
    ok = dropcomplex(Path, 2),
    ok = dropcomplex(Path, 3),
    ok = dropcomplex(Path, 4),
    ok = dropcomplex(Path, 5),
    ok = dropcomplex(Path, 6),
    ok = dropcomplex(Path, 7),
    ok = dropcomplex(Path, 8),
    ok = dropcomplex(Path, 9),
    ok = dropcomplex(Path, 10),
    Got = read_raw(Path),
    pass(100, Got).

test12() ->
    [{doc, "Add some values, get a z-value, then add another one"}].

test12(Config) when is_list(Config) ->
    Path = "test12",
    ok = load_values(Path, 99),
    ok = load_vanilla_zformula(Path),
    ok = add_page(Path),
    Got = read_raw(Path),
    pass(100, Got).

test13() ->
    [{doc, "Add some values, get a z-value, then add another one"}].

test13(Config) when is_list(Config) ->
    Path = "test13",
    ok = load_complex_values(Path, 99),
    ok = load_complex_zformula(Path),
    ok = add_complex_page(Path),
    Got = read_raw(Path),
    pass(100, Got).

%%% Internal fns
pass(Expected, Expected) -> io:format("Pass: Expected ~p Got ~p~n",
                                      [Expected, Expected]),
                            'test passed';
pass(Expected, Got)      -> io:format("Fail: Expected ~p Got ~p~n",
                                      [Expected, Got]),
                            exit("test failed").

load_vanilla_zformula(PageRoot) ->
    io:format("loading vanilla zformula for ~p~n", [PageRoot]),
    Formula = "=sum(/" ++ PageRoot ++ "/[true]/a1)",
    ZCell = #refX{site = ?SITE, path = [PageRoot], type = url,
                  obj = {cell, {1, 1}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", Formula}]}]).

load_complex_zformula(PageRoot) ->
    io:format("loading complex zformula for ~p~n", [PageRoot]),
    Formula = "=sum(/" ++ PageRoot ++ "/[a2>1]/a1)",
    ZCell = #refX{site = ?SITE, path = [PageRoot], type = url,
                  obj = {cell, {1, 1}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", Formula}]}]).

read_raw(PageRoot) ->
    timer:sleep(?SLEEP),
    ZCell = #refX{site = ?SITE, path = [PageRoot], type = url,
                  obj = {cell, {1, 1}}},
    [{_, ZCellVal}] = new_db_api:read_attribute(ZCell, "__rawvalue"),
    ZCellVal.

add_page(PageRoot) ->
    io:format("adding page on ~p~n", [PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, "added page"], type = url,
                  obj = {cell, {1, 1}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "1"}]}]).

add_complex_page(PageRoot) ->
    io:format("adding complex page on ~p~n", [PageRoot]),
    ZCell1 = #refX{site = ?SITE, path = [PageRoot, "added page"], type = url,
                   obj = {cell, {1, 1}}},
    ZCell2 = #refX{site = ?SITE, path = [PageRoot, "added page"], type = url,
                   obj = {cell, {1, 2}}},
   ok = new_db_api:write_attributes([{ZCell1, [{"formula", "1"}]}]),
   ok = new_db_api:write_attributes([{ZCell2, [{"formula", "999"}]}]).


load_values(_PageRoot, 0) -> ok;
load_values(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("loading value for page ~p on ~p~n", [N, PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {1, 1}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "1"}]}]),
    load_values(PageRoot, N - 1).

load_complex_values(_PageRoot, 0) -> ok;
load_complex_values(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("loading complex value for page ~p on ~p~n", [N, PageRoot]),
    ZCell1 = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {1, 1}}},
    ZCell2 = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {1, 2}}},
    ok = new_db_api:write_attributes([{ZCell1, [{"formula", "1"}]}]),
    ok = new_db_api:write_attributes([{ZCell2, [{"formula", "999"}]}]),
    load_complex_values(PageRoot, N - 1).

poke10(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("Poking the value 10 into ~p on ~p~n", [N, PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {1, 1}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "10"}]}]).

dropcomplex(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("Dropping the complex z-query into ~p on ~p~n", [N, PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {1, 2}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "-1"}]}]).

debug() ->
    init_per_suite([]),
    {ok, U} = passport:email_to_uid("test@hypernumbers.com"),
    ok = hn_groups:add_user(?SITE, "admin", U),
    test11([]).
