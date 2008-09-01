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

-include("../../include/spriki.hrl").
-include("ct.hrl").

%% define a couple of items that define what site we are on
-define(SITE,site).
-define(NAME,name).

%% now define the tests themselves
-define(INPUT1,"test1",false,"/","index").
-define(NAME1,"test1").
-define(OUTPUT1,{template,"test1",false,[],"index"}).

-define(INPUT2,"test2",false,"/test2/{auto,incr}","index").
-define(NAME2,"test2").
-define(OUTPUT2,"/test2/1/").

-define(INPUT3,"test3",false,"/test3/{auto,incr}","index").
-define(NAME3,"test3").
-define(PAGES3,["test3","99"]).
-define(OUTPUT3,"/test3/100/").

%% These tests all are time related and are commented out in the test suite...

-define(INPUT4,"test4",false,"/test4/{year,yyyy}","index").
-define(NAME4,"test4").
-define(OUTPUT4,"/test4/2008/").

-define(INPUT5,"test5",false,"/test5/{year,yy}","index").
-define(NAME5,"test5").
-define(OUTPUT5,"/test5/08/").

-define(INPUT6,"test6",false,"/test6/{month,mm}","index").
-define(NAME6,"test6").
-define(OUTPUT6,"/test6/09/").

-define(INPUT7,"test7",false,"/test7/{month,m}","index").
-define(NAME7,"test7").
-define(OUTPUT7,"/test7/9/").

-define(INPUT8,"test8",false,"/test8/{month,long}","index").
-define(NAME8,"test8").
-define(OUTPUT8,"/test8/September/").

-define(INPUT9,"test9",false,"/test9/{month,short}","index").
-define(NAME9,"test9").
-define(OUTPUT9,"/test9/Sept/").

-define(INPUT10,"test10",false,"/test10/{day,dd}","index").
-define(NAME10,"test10").
-define(OUTPUT10,"/test10/01/").

-define(INPUT11,"test11",false,"/test11/{day,d}","index").
-define(NAME11,"test11").
-define(OUTPUT11,"/test11/1/").

-define(INPUT12,"test12",false,"/test12/{weekday,dd}","index").
-define(NAME12,"test12").
-define(OUTPUT12,"/test12/01/").

-define(INPUT13,"test13",false,"/test13/{weekday,d}","index").
-define(NAME13,"test13").
-define(OUTPUT13,"/test13/1/").

-define(INPUT14,"test14",false,"/test14/{weekday,long}","index").
-define(NAME14,"test14").
-define(OUTPUT14,"/test14/Monday/").

-define(INPUT15,"test15",false,"/test15/{weekday,short}","index").
-define(NAME15,"test15").
-define(OUTPUT15,"/test15/Mon/").

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
    Paths=[?PAGES3],
    Fun = fun(Path) ->
		  Addr=#ref{site=?SITE,path=Path,name=?NAME},
		  io:format("In init_per_suite Addr is ~p~n",[Addr]),
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
     test_3
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
     %% test_15
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
    {atomic,ok}=hn_templates:write_def(?INPUT1),
    {ok,Template}=hn_db:read_template(?NAME1),
    test_util:expected3(?OUTPUT1,Template).

test_2() -> "test the auto-increment function".

test_2(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT2),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME2),
    test_util:expected3(?OUTPUT2,Got).

test_3() -> "test the auto-increment function".

test_3(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT3),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME3),
    test_util:expected3(?OUTPUT3,Got).

test_4() -> "test the auto-increment function".

test_4(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT4),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME4),
    test_util:expected3(?OUTPUT4,Got).

test_5() -> "test the auto-increment function".

test_5(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT5),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME5),
    test_util:expected3(?OUTPUT5,Got).

test_6() -> "test the auto-increment function".

test_6(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT6),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME6),
    test_util:expected3(?OUTPUT6,Got).

test_7() -> "test the auto-increment function".

test_7(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT7),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME7),
    test_util:expected3(?OUTPUT7,Got).

test_8() -> "test the auto-increment function".

test_8(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT8),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME8),
    test_util:expected3(?OUTPUT8,Got).

test_9() -> "test the auto-increment function".

test_9(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT9),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME9),
    test_util:expected3(?OUTPUT9,Got).

test_10() -> "test the auto-increment function".

test_10(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT10),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME10),
    test_util:expected3(?OUTPUT10,Got).

test_11() -> "test the auto-increment function".

test_11(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT11),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME11),
    test_util:expected3(?OUTPUT11,Got).

test_12() -> "test the auto-increment function".

test_12(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT12),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME12),
    test_util:expected3(?OUTPUT12,Got).

test_13() -> "test the auto-increment function".

test_13(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT13),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME13),
    test_util:expected3(?OUTPUT13,Got).

test_14() -> "test the auto-increment function".

test_14(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT14),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME14),
    test_util:expected3(?OUTPUT14,Got).

test_15() -> "test the auto-increment function".

test_15(_Config) ->
    {atomic,ok}=hn_templates:write_def(?INPUT15),
    Got=hn_templates:get_next(#ref{site=?SITE},?NAME15),
    test_util:expected3(?OUTPUT15,Got).
