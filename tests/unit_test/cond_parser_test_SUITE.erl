%%%-----------------------------------------------------------------------------
%%% File        : num_parser_test_SUITE.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests the parser for the numerical format
%%%
%%%
%%% Created     : 10 Mar 2008 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(cond_parser_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

%% Tests 1 to 40 all come from the default custom formats in Excel
%% all tests furth of 40 are bespoke tests
%% all tests are generated from the Excel spreadsheet num_parser_master.xls
%% which is in Subversion alongside this test suite

%% Tests 1
-define(COND1,"A<3").
-define(OUTPUT1,{col,"A","<",3}).

%% Test 2
-define(COND2, "AA<3").
-define(OUTPUT2,{col,"AA","<",3}).

%% Test 3
-define(COND3, "3<3").
-define(OUTPUT3,{row,3,"<",3}).

%% Test 4
-define(COND4, "4<3").
-define(OUTPUT4,{row,4,"<",3}).

%% Test 5
-define(COND5, "3=3").
-define(OUTPUT5,{row,3,"=",3}).

%% Test 6
-define(COND6, "3=<3").
-define(OUTPUT6,{row,3,"=<",3}).

%% Test 7
-define(COND7, "99<=9").
-define(OUTPUT7,{row,99,"<=",9}).

%% Test 8
-define(COND8, "3=<3").
-define(OUTPUT8,{row,3,"=<",3}).

%% Test 9
-define(COND9, "3<=3").
-define(OUTPUT9,{row,3,"<=",3}).

%% Test 10
-define(COND10, "3>=3").
-define(OUTPUT10,{row,3,">=",3}).

%% Test 11
-define(COND11, "3=>3").
-define(OUTPUT11,{row,3,"=>",3}).


%% Test server callback functions
%%------------------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%------------------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
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
%%------------------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%------------------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%------------------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%------------------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%------------------------------------------------------------------------------
all() -> 
    [
     cond_parser_test1,
     cond_parser_test2,
     cond_parser_test3,
     cond_parser_test4,
     cond_parser_test5,
     cond_parser_test6,
     cond_parser_test7,
     cond_parser_test8,
     cond_parser_test9,
     cond_parser_test10,
     cond_parser_test11
    ].

%% Case executor
executor(Input,Output)->
    {Ok,Toks,_}=cond_lexer:string(Input),
    io:format("Toks are ~p~n",[Toks]),
    {ok,Got}=cond_parser:parse(Toks),
    test_util:expected4(Output,Got).

%% Test cases starts here.
%%------------------------------------------------------------------------------
cond_parser_test1() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test1(Config) when is_list(Config) -> 
    executor(?COND1,?OUTPUT1).

cond_parser_test2() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test2(suite) -> 
    [];

cond_parser_test2(Config) when is_list(Config) -> 
    executor(?COND2,?OUTPUT2).

cond_parser_test3() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test3(suite) -> 
    [];

cond_parser_test3(Config) when is_list(Config) -> 
    executor(?COND3,?OUTPUT3).


cond_parser_test4() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test4(suite) -> 
    [];

cond_parser_test4(Config) when is_list(Config) -> 
    executor(?COND4,?OUTPUT4).


cond_parser_test5() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test5(suite) -> 
    [];

cond_parser_test5(Config) when is_list(Config) -> 
    executor(?COND5,?OUTPUT5).


cond_parser_test6() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test6(suite) -> 
    [];

cond_parser_test6(Config) when is_list(Config) -> 
    executor(?COND6,?OUTPUT6).


cond_parser_test7() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test7(suite) -> 
    [];

cond_parser_test7(Config) when is_list(Config) -> 
    executor(?COND7,?OUTPUT7).


cond_parser_test8() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test8(suite) -> 
    [];

cond_parser_test8(Config) when is_list(Config) -> 
    executor(?COND8,?OUTPUT8).


cond_parser_test9() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test9(suite) -> 
    [];

cond_parser_test9(Config) when is_list(Config) -> 
    executor(?COND9,?OUTPUT9).


cond_parser_test10() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test10(suite) -> 
    [];

cond_parser_test10(Config) when is_list(Config) -> 
    executor(?COND10,?OUTPUT10).


cond_parser_test11() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_parser_test11(suite) -> 
    [];

cond_parser_test11(Config) when is_list(Config) -> 
    executor(?COND11,?OUTPUT11).

