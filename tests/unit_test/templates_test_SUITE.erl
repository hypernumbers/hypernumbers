%%%-------------------------------------------------------------------
%%% File        : templates_test_SUITE.erl
%%% Author      : Gordon Guthrie gordon@hypernumbers.com
%%% Description : tests hn_templates.erl 
%%%
%%% Created     : 30 Aug 2008 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(templates_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("../../lib/hypernumbers-1.0/include/spriki.hrl").
-include("ct.hrl").

%% define a couple of items that define what site we are on
-define(SITE,"site").
-define(NAME,"name").
-define(USERNAME,"gordonguthrie").

%% now define the tests themselves
-define(INPUT1,"test1","/","index","null").
-define(NAME1,"test1").
-define(OUTPUT1,{template,"test1",[],"index","null"}).

-define(INPUT2,"test2","/test2/{auto,incr}/","index","null").
-define(NAME2,"test2").
-define(OUTPUT2,"/test2/1/").

-define(INPUT3,"test3","/test3/{auto,incr}/","index","null").
-define(NAME3,"test3").
-define(PAGES3,["test3","99"]).
-define(OUTPUT3,"/test3/100/").

%% These tests all are time related and are commented out in the test suite...

-define(INPUT4,"test4","/test4/{year,yyyy}/","index","null","null").
-define(NAME4,"test4").
-define(OUTPUT4,"/test4/2008/").

-define(INPUT5,"test5","/test5/{year,yy}/","index","null").
-define(NAME5,"test5").
-define(OUTPUT5,"/test5/08/").

-define(INPUT6,"test6","/test6/{month,mm}/","index","null").
-define(NAME6,"test6").
-define(OUTPUT6,"/test6/09/").

-define(INPUT7,"test7","/test7/{month,m}/","index","null").
-define(NAME7,"test7").
-define(OUTPUT7,"/test7/9/").

-define(INPUT8,"test8","/test8/{month,long}/","index","null").
-define(NAME8,"test8").
-define(OUTPUT8,"/test8/September/").

-define(INPUT9,"test9","/test9/{month,short}/","index","null").
-define(NAME9,"test9").
-define(OUTPUT9,"/test9/Sept/").

-define(INPUT10,"test10","/test10/{day,dd}/","index","null").
-define(NAME10,"test10").
-define(OUTPUT10,"/test10/01/").

-define(INPUT11,"test11","/test11/{day,d}/","index","null").
-define(NAME11,"test11").
-define(OUTPUT11,"/test11/1/").

-define(INPUT12,"test12","/test12/{weekday,dd}/","index","null").
-define(NAME12,"test12").
-define(OUTPUT12,"/test12/01/").

-define(INPUT13,"test13","/test13/{weekday,d}/","index","null").
-define(NAME13,"test13").
-define(OUTPUT13,"/test13/1/").

-define(INPUT14,"test14","/test14/{weekday,long}/","index","null").
-define(NAME14,"test14").
-define(OUTPUT14,"/test14/Monday/").

-define(INPUT15,"test15","/test15/{weekday,short}/","index","null").
-define(NAME15,"test15").
-define(OUTPUT15,"/test15/Mon/").

%% username based templates

-define(INPUT16,"test16","/test16/{username}/","index","null").
-define(NAME16,"test16").
-define(OUTPUT16,"/test16/gordonguthrie/").

%% now make it all work

-define(INPUT17,"test17","/test17/{username}/{auto,incr}/","index","null").
-define(NAME17,"test17").
-define(PAGES17,["test17","gordonguthrie","2"]).
-define(OUTPUT17,"/test17/gordonguthrie/3/").

-define(INPUT18,"test18","/test18/{auto,incr}/{username}/","index","null").
-define(NAME18,"test18").
-define(PAGES18,["test18","2","gordonguthrie"]).
-define(OUTPUT18,"/test18/3/gordonguthrie/").

-define(INPUT19,"test19","/test19//{username}/{year,long}/{auto,incr}/","index","null").
-define(NAME19,"test19").
-define(PAGES19,["test19","gordonguthrie","2008","999"]).
-define(OUTPUT19,"/test19/gordonguthrie/2008/1000/").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    code:add_patha("../../../../../lib/hypernumbers-1.0/ebin/"),
    code:add_patha("../../../../../lib/formula_engine-1.0/ebin/"),
    Paths=[?PAGES3,?PAGES17,?PAGES18,?PAGES19],
    Fun = fun(Path) ->
		  Addr=#ref{site=?SITE,path=Path,name=?NAME},
		  hn_db:write_item(Addr,dummy)
	  end,
    lists:map(Fun,Paths),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: sequences() -> Sequences
%%
%% Sequences = [{SeqName,TestCases}]
%% SeqName = atom()
%%   Name of a sequence.
%% TestCases = [atom()]
%%   List of test cases that are part of the sequence
%%
%% Description: Specifies test case sequences.
%%--------------------------------------------------------------------
sequences() -> 
    [].

%%--------------------------------------------------------------------
%% Function: all() -> TestCases | {skip,Reason}
%%
%% TestCases = [TestCase | {sequence,SeqName}]
%% TestCase = atom()
%%   Name of a test case.
%% SeqName = atom()
%%   Name of a test case sequence.
%% Reason = term()
%%   The reason for skipping all test cases.
%%
%% Description: Returns the list of test cases that are to be executed.
%%--------------------------------------------------------------------
all() -> 
    [test_1,
     test_2,
     test_3,
     %% test_4,
     %% test_5,
     %% test_6,
     %% test_7,
     %% test_8,
     %% test_9,
     %% test_10,
     %% test_11,
     %% test_12,
     %% test_13,
     %% test_14,
     %% test_15,
     test_16,
     test_17,
     test_18,
     test_19
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Test case info function - returns list of tuples to set
%%              properties for the test case.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list for the test case to be executed).
%%--------------------------------------------------------------------

test_1() -> "Check that the template reader and writer work". 

test_1(_Config) -> 
    {ok,ok}=hn_templates:write_def(?INPUT1),
    {ok,Template}=hn_db:read_template(?NAME1),
    test_util:expected3(?OUTPUT1,Template).

test_2() -> "test the auto-increment function".

test_2(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT2),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME2,?USERNAME),
    test_util:expected3(?OUTPUT2,Got).

test_3() -> "test the auto-increment function".

test_3(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT3),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME3,?USERNAME),
    test_util:expected3(?OUTPUT3,Got).

test_4() -> "test the time-based template function".

test_4(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT4),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME4,?USERNAME),
    test_util:expected3(?OUTPUT4,Got).

test_5() -> "test the time-based template function".

test_5(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT5),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME5,?USERNAME),
    test_util:expected3(?OUTPUT5,Got).

test_6() -> "test the time-based template function".

test_6(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT6),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME6,?USERNAME),
    test_util:expected3(?OUTPUT6,Got).

test_7() -> "test the time-based template function".

test_7(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT7),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME7,?USERNAME),
    test_util:expected3(?OUTPUT7,Got).

test_8() -> "test the time-based template function".

test_8(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT8),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME8,?USERNAME),
    test_util:expected3(?OUTPUT8,Got).

test_9() -> "test the time-based template function".

test_9(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT9),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME9,?USERNAME),
    test_util:expected3(?OUTPUT9,Got).

test_10() -> "test the time-based template function".

test_10(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT10),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME10,?USERNAME),
    test_util:expected3(?OUTPUT10,Got).

test_11() -> "test the time-based template function".

test_11(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT11),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME11,?USERNAME),
    test_util:expected3(?OUTPUT11,Got).

test_12() -> "test the time-based template function".

test_12(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT12),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME12,?USERNAME),
    test_util:expected3(?OUTPUT12,Got).

test_13() -> "test the time-based template function".

test_13(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT13),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME13,?USERNAME),
    test_util:expected3(?OUTPUT13,Got).

test_14() -> "test the time-based template function".

test_14(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT14),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME14,?USERNAME),
    test_util:expected3(?OUTPUT14,Got).

test_15() -> "test the time-based template function".

test_15(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT15),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME15,?USERNAME),
    test_util:expected3(?OUTPUT15,Got).

test_16() -> "test the username template function".

test_16(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT16),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME16,?USERNAME),
    test_util:expected3(?OUTPUT16,Got).

test_17() -> "test the username template function".

test_17(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT17),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME17,?USERNAME),
    test_util:expected3(?OUTPUT17,Got).

test_18() -> "test the username template function".

test_18(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT18),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME18,?USERNAME),
    test_util:expected3(?OUTPUT18,Got).

test_19() -> "test the username template function".

test_19(_Config) ->
    {ok,ok}=hn_templates:write_def(?INPUT19),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME19,?USERNAME),
    test_util:expected3(?OUTPUT19,Got).
