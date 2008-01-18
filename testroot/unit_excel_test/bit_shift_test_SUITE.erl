%%%-------------------------------------------------------------------
%%% File    : bit_shift_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : 
%%%
%%% Created : 27 Jul 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-------------------------------------------------------------------
-module(bit_shift_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    code:add_path("../../ebin"),
    production_boot:setup_paths(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initiation before each test case
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->

    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
all(doc) -> 
    ["Describe the main purpose of this suite"];

all(suite) -> 
    [bit_shift_test1, bit_shift_test2,
     bit_shift_test3, bit_shift_test4].

%% Test cases starts here.
%%--------------------------------------------------------------------
bit_shift_test1(doc) -> 
    ["Test a left shift by 2"];
bit_shift_test1(suite) -> 
    [];
bit_shift_test1(Config) when is_list(Config) -> 
    run_test(<<7,0,0>>,<<1,0,0>>).

bit_shift_test2(doc) -> 
    ["Test a left shift by 2"];
bit_shift_test2(suite) -> 
    [];
bit_shift_test2(Config) when is_list(Config) -> 
    run_test(<<7,8,0>>,<<1,2,0>>).

bit_shift_test3(doc) -> 
    ["Test a left shift by 2"];
bit_shift_test3(suite) -> 
    [];
bit_shift_test3(Config) when is_list(Config) -> 
    run_test(<<7,9,0>>,<<65,2,0>>).

bit_shift_test4(doc) -> 
    ["Test a left shift by 2"];
bit_shift_test4(suite) -> 
    [];
bit_shift_test4(Config) when is_list(Config) -> 
    run_test(<<7,9,9>>,<<"AB\b">>). % "AB\b" = <<65,66,02>>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Actually runs the test
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_test(Bin,Expected)->
    Got=excel_util:shift_left2_TESTING(Bin),
    test_util:expected(Expected,Got),
    ok.
