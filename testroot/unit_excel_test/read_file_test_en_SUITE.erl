%%%-------------------------------------------------------------------
%%% File        : read_file_test_en_SUITE.erl
%%% Author      : Gordon Guthrie 
%%% Description : 
%%%
%%% Created     : 27 Jul 2007 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------
-module(read_file_test_en_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(TEMP_LOG_FILE,"/opt/SVN/spriki/trunk/testroot/unit_excel_test/scratch/log.txt").

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
    os:cmd("rm "++?TEMP_LOG_FILE),
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
    [
     parse_CRS_RK_common_rec_subs1, parse_CRS_RK_common_rec_subs2,
     parse_CRS_RK_common_rec_subs3, parse_CRS_RK_common_rec_subs4,
     parse_CRS_RK_common_rec_subs5, parse_CRS_RK_common_rec_subs6,
     parse_CRS_RK_common_rec_subs7, parse_CRS_RK_common_rec_subs8
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
parse_CRS_RK_common_rec_subs1(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs1(suite) -> 
    [];
parse_CRS_RK_common_rec_subs1(Config) when is_list(Config) -> 
    run_test(<<0,0,240,63>>, 1.0).      % hex 00 00 F0 3F


parse_CRS_RK_common_rec_subs2(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs2(suite) -> 
    [];
parse_CRS_RK_common_rec_subs2(Config) when is_list(Config) -> 
    run_test(<<1,0,240,63>>,0.01).     % hex 01 00 F0 3F


parse_CRS_RK_common_rec_subs3(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs3(suite) -> 
    [];
parse_CRS_RK_common_rec_subs3(Config) when is_list(Config) -> 
    run_test(<<70,86,75,0>>, 1234321).  % hex 46 56 4B 00


parse_CRS_RK_common_rec_subs4(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs4(suite) -> 
    [];
parse_CRS_RK_common_rec_subs4(Config) when is_list(Config) -> 
    run_test(<<71,86,75,0>>,12343.21). % hex 47 56 4B 00

parse_CRS_RK_common_rec_subs5(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs5(suite) -> 
    [];
parse_CRS_RK_common_rec_subs5(Config) when is_list(Config) -> 
    run_test(<<6,0,0,0>>,1).           % hex 06 00 00 00


parse_CRS_RK_common_rec_subs6(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs6(suite) -> 
    [];
parse_CRS_RK_common_rec_subs6(Config) when is_list(Config) -> 
    run_test(<<10,0,0,0>>,2).

parse_CRS_RK_common_rec_subs7(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs7(suite) -> 
    [];
parse_CRS_RK_common_rec_subs7(Config) when is_list(Config) -> 
    run_test(<<3,4,0,0>>,2.56).

parse_CRS_RK_common_rec_subs8(doc) -> 
    ["Test the RK common subscructure parsing as described in Section 2.5.3 "++
     "of excelfileformat.pdf V1.40"];
parse_CRS_RK_common_rec_subs8(suite) -> 
    [];
parse_CRS_RK_common_rec_subs8(Config) when is_list(Config) -> 
    run_test(<<2,4,0,0>>,256).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Actually runs the test
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_test(Args,Expected)->
    Got=excel_util:parse_CRS_RK_TESTING(Args,?TEMP_LOG_FILE),
    test_util:expected(Expected,Got),
    ok.
