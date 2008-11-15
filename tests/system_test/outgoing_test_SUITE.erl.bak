%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(outgoing_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

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
    code:add_path("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    Url = "http://127.0.0.1:9000",
    hn_util:post(Url++"/to1/a1?format=xml",
        "<post><action>create</action><value>=/mypage/a1</value></post>","text/xml"),
    hn_util:post(Url++"/to2/a1?format=xml",
        "<post><action>create</action><value>=/mypage/a2</value></post>","text/xml"),
    hn_util:post(Url++"/to3/a1?format=xml",
        "<post><action>create</action><value>=/mypage/a2</value></post>","text/xml"),
    Config.

%%------------------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    production_boot:stop(),
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
%        test_outgoing1,
%        test_outgoing2,
%        test_outgoing3,
%        test_outgoing4
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
test_outgoing1() ->    [{userdata,[{doc,"Description here"}]}].
test_outgoing1(Config) when is_list(Config) ->

    Rtn = "http://127.0.0.1:9000\n/to1/\n1\n1\nhttp://127.0.0.1:"
        ++"9000\n/mypage/\n1\n1",

    test_util:expected(lists:duplicate(3,Rtn),
        [get_data("mypage/a1",{xml}),get_data("mypage/a1",{list}),
         get_data("mypage/a1",{json})]).


test_outgoing2() ->    [{userdata,[{doc,"Description here"}]}].
test_outgoing2(Config) when is_list(Config) ->

    Rtn = "http://127.0.0.1:9000\n/to2/\n1\n1\nhttp://127.0.0.1:"
        ++"9000\n/mypage/\n2\n1\nhttp://127.0.0.1:9000\n/to3/\n1\n"
        ++"1\nhttp://127.0.0.1:9000\n/mypage/\n2\n1",

    test_util:expected(lists:duplicate(3,Rtn),
        [get_data("mypage/a2",{xml}),get_data("mypage/a2",{list}),
         get_data("mypage/a2",{json})]).

test_outgoing3() ->       [{userdata,[{doc,"Description here"}]}].
test_outgoing3(Config) when is_list(Config) ->

    Rtn = "http://127.0.0.1:9000\n/to3/\n1\n1\nhttp://127.0.0.1:9000\n/mypage"
        ++"/\n2\n1\nhttp://127.0.0.1:9000\n/to2/\n1\n1\nhttp://127.0.0.1:9000"
        ++"\n/mypage/\n2\n1\nhttp://127.0.0.1:9000\n/to1/\n1\n1\nhttp://127.0"
        ++".0.1:9000\n/mypage/\n1\n1",

    test_util:expected(lists:duplicate(3,Rtn),
        [get_data("mypage/",{xml}),get_data("mypage/",{list}),
         get_data("mypage/",{json})]).

test_outgoing4() ->       [{userdata,[{doc,"Description here"}]}].
test_outgoing4(Config) when is_list(Config) ->
    test_util:expected(lists:duplicate(3,"empty"),
        [get_data("mypage/a3",{xml}),get_data("mypage/a3",{list}),
         get_data("mypage/a3",{json})]).

get_data(Cell,Format) ->
    FormatStr = case Format of
        {list} -> "list"; {xml} -> "xml";
        {json} -> "json"; {json,nocallback} -> "json&nocollback" end,
    Url = "http://127.0.0.1:9000/"++Cell++"?format="++FormatStr++"&outgoing",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        http:request(get,{Url, []}, [], []),

    util2:flatten_data(Format,Body).

