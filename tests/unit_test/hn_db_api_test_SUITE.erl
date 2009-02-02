%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       A test suite for writing cell attributes
%%%
%%% @end
%%% Created : 26 Jan 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hn_db_api_test_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../lib/hypernumbers-1.0/include/spriki.hrl").

-define(SITE1, "http://hypernumbers.dev").
-define(SITE2, "http://hypernumbers.dev:9000").


%%
%% Test A
%% 
%% This checks that cell formula still works. It writes a formula
%% using write_attrs/2 and then checks that the hn_db_wu has set
%% local_cell_links correctly
%% 

-define(PATH_A, ["A"]).

-define(InitialiseA,[{{cell, {11, 199}}, "1"},
                     {{cell, {299, 23}}, "1"}]).
-define(Input1A, [{#refX{site = ?SITE1, path = ?PATH_A, obj = {column, 1}}, "C 1"},
                {#refX{site = ?SITE1, path = ?PATH_A, obj = {column, 99}}, "C 99"}]).
-define(Ref1A, {range, {1, 300, 300, 300}}).
-define(Expected1A, ["C 1", "C 99"].

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
    [
     test_case1A
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------

%%
%% Test A
%% 
test_case1A() ->
    [{doc, "Simple test of attributes"}].

test_case1A(Config) when is_list(Config) ->
    testA(?InitialiseA, ?Input1A, ?Ref1A, ?Expected1A),
    ok.

testA(Initialise, Input, Ref, Expected) ->
    write(Initialise, ?PATH_A),
    Return1 = hn_db_api:write_last(Input),
    RefX = #refX{site = ?SITE1, path = ?PATH_A, obj = Ref},
    Got = extract_values(hn_db_api:read(RefX)),
    io:format("in testA Got is ~p~n", [Got]),
    test2(lists:sort(Expected), lists:sort(Got)).


%%
%% Various comparison and helper functions
%% 

make_list(List, Path) -> make_list(List, Path, []).

make_list([], Path, Acc) -> Acc;
make_list([H | T], Path, Acc) ->
    Ref = #refX{site = ?SITE1, path = Path, obj = H},
    make_list(T, Path, [Ref | Acc]).

write([], _Path) -> ok;
write([{Ref, Val} | T], Path) ->
    Ref2 = #refX{site = ?SITE1, path = Path, obj = Ref},
    Fun1 = fun() -> hn_db_wu:write_attr(Ref2, {formula, Val}) end,
    mnesia:activity(transaction, Fun1),
    write(T, Path).

extract_values(List) -> extract_values2(List, []).

extract_values2([], Acc) -> lists:sort(Acc);
extract_values2([{_, {rawvalue, V}}| T], Acc) -> extract_values2(T, [V | Acc]);
extract_values2([{_, {_       , V}}| T], Acc) -> extract_values2(T, Acc).


extract_names(List) -> extract_names2(List, []).

extract_names2([], Acc)                -> lists:sort(Acc);
extract_names2([{_, {N, _}} | T], Acc) -> extract_names2(T, [N | Acc]).


test2(Expected, Expected) -> io:format("Success: ~p~n",[Expected]);
test2(Expected, Got)      -> io:format("Failure: Expected ~p~n"++
                                       "Got ~p~n",[Expected, Got]),
                             exit('endless fail').
