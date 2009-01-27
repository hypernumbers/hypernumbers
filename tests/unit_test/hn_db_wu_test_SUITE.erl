%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       A test suite for writing cell attributes
%%%
%%% @end
%%% Created : 26 Jan 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hn_db_wu_test_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../lib/hypernumbers-1.0/include/spriki.hrl").

-define(SITE1, "http://hypernumbers.dev").
-define(PATH1, ["one"]). % blah

% Ref 1
-define(Ref1,{cell, {1, 1}}).
-define(Val1, "1").
-define(Expected1, "1").

% Ref 2
-define(Ref2,{cell, {1, 2}}).
-define(Val2, "two").
-define(Expected2, "two").

% Ref 3
-define(Ref3,{cell, {1, 3}}).
-define(Val3, "'3").
-define(Expected3, "'3").

% Ref 4
-define(Ref4,{cell, {1, 4}}).
-define(Val4, "=2+2").
-define(Expected4, "=2+2").

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
    % First create a new database
    ok = hn_db:create(),
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
    ok.

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
    [test_case1,
    test_case2,
    test_case3,
    test_case4].

test(Ref, Val, Expected) ->
    Ref2 = #ref{site = ?SITE1, path = ?PATH1, ref = Ref, name = formula},
    % io:format("in test Ref2 is ~p~n", [Ref2]),
    Fun1 = fun() -> hn_db_wu:write_attr(Ref2, Val) end,
    mnesia:activity(transaction, Fun1),
    Fun2 = fun() -> hn_db_wu:read_cells(Ref2) end,
    [Got] = mnesia:activity(transaction, Fun2),
    #hn_item{val = GotValue} = Got,
    test2(Expected, GotValue).

test2(Expected, Expected) -> io:format("Success: ~p~n",[Expected]);
test2(Expected, Got)      -> io:format("Failure: Expected ~p Got ~p~n",
                                       [Expected, Got]),
                             exit('endless fail').

%% Test cases starts here.
%%--------------------------------------------------------------------
test_case1() ->
    [{doc, "Simple test of attributes"}].

test_case1(Config) when is_list(Config) ->
    test(?Ref1, ?Val1, ?Expected1),
    ok.

test_case2() ->
    [{doc, "Simple test of attributes"}].

test_case2(Config) when is_list(Config) ->
    test(?Ref2, ?Val2, ?Expected2),
    ok.

test_case3() ->
    [{doc, "Simple test of attributes"}].

test_case3(Config) when is_list(Config) ->
    test(?Ref3, ?Val3, ?Expected3),
    ok.

test_case4() ->
    [{doc, "Simple test of attributes"}].

test_case4(Config) when is_list(Config) ->
    test(?Ref4, ?Val4, ?Expected4),
    ok.
