%%%-----------------------------------------------------------------------------
%%% File        : cond_test_SUITE.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests the parser for the numerical format
%%%
%%% Created     : 13 September 2008 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(cond_lexer_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

%% Test 1
-define(INPUT1, "A<3").
-define(OUTPUT1,[{string,"A"},{condition,"<"},{integer,"3"}]).

%% Test 2
-define(INPUT2, "AA<3").
-define(OUTPUT2,[{string,"AA"},{condition,"<"},{integer,"3"}]).

%% Test 3
-define(INPUT3, "3<3").
-define(OUTPUT3,[{integer,"3"},{condition,"<"},{integer,"3"}]).

%% Test 4
-define(INPUT4, "3<3").
-define(OUTPUT4,[{integer,"3"},{condition,"<"},{integer,"3"}]).

%% Test 5
-define(INPUT5, "=").
-define(OUTPUT5,[{condition,"="}]).

%% Test 6
-define(INPUT6, "=<").
-define(OUTPUT6,[{condition,"=<"}]).

%% Test 7
-define(INPUT7, "<=").
-define(OUTPUT7,[{condition,"<="}]).

%% Test 8
-define(INPUT8, "=<").
-define(OUTPUT8,[{condition,"=<"}]).

%% Test 9
-define(INPUT9, "<=").
-define(OUTPUT9,[{condition,"<="}]).

%% Test 10
-define(INPUT10, ">=").
-define(OUTPUT10,[{condition,">="}]).

%% Test 11
-define(INPUT11, "=>").
-define(OUTPUT11,[{condition,"=>"}]).


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
    [cond_lexer_test1,
     cond_lexer_test2,
     cond_lexer_test3,
     cond_lexer_test4,
     cond_lexer_test5,
     cond_lexer_test6,
     cond_lexer_test7,
     cond_lexer_test8,
     cond_lexer_test9,
     cond_lexer_test10,
     cond_lexer_test11
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
cond_lexer_test1() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test1(suite) -> 
    [];

cond_lexer_test1(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT1),
    test_util:expected3(?OUTPUT1,Output).

cond_lexer_test2() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test2(suite) -> 
    [];

cond_lexer_test2(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT2),
    test_util:expected3(?OUTPUT2,Output).

cond_lexer_test3() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test3(suite) -> 
    [];

cond_lexer_test3(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT3),
    test_util:expected3(?OUTPUT3,Output).

cond_lexer_test4() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test4(suite) -> 
    [];

cond_lexer_test4(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT4),
    test_util:expected3(?OUTPUT4,Output).

cond_lexer_test5() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test5(suite) -> 
    [];

cond_lexer_test5(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT5),
    test_util:expected3(?OUTPUT5,Output).

cond_lexer_test6() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test6(suite) -> 
    [];

cond_lexer_test6(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT6),
    test_util:expected3(?OUTPUT6,Output).

cond_lexer_test7() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test7(suite) -> 
    [];

cond_lexer_test7(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT7),
    test_util:expected3(?OUTPUT7,Output).

cond_lexer_test8() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test8(suite) -> 
    [];

cond_lexer_test8(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT8),
    test_util:expected3(?OUTPUT8,Output).

cond_lexer_test9() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test9(suite) -> 
    [];

cond_lexer_test9(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT9),
    test_util:expected3(?OUTPUT9,Output).

cond_lexer_test10() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test10(suite) -> 
    [];

cond_lexer_test10(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT10),
    test_util:expected3(?OUTPUT10,Output).

cond_lexer_test11() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

cond_lexer_test11(suite) -> 
    [];

cond_lexer_test11(Config) when is_list(Config) -> 
    {ok,Output,_}=cond_lexer:string(?INPUT11),
    test_util:expected3(?OUTPUT11,Output).
