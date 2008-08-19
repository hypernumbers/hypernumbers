%%%-----------------------------------------------------------------------------
%%% File        : util_test_SUITE.erl
%%% Author      : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : some tests for utility functions
%%%
%%%               MUCH MORE NEEDS TO BE DONE!
%%%
%%% Created     : 18 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(util_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

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
    [parse_url_test1,
     parse_url_test2,
     parse_url_test3,
     parse_url_test4,
     parse_url_test4a,
     parse_url_test4b,
     parse_url_test4c,
     parse_url_test5,
     parse_url_test6,
     parse_url_test7,
     parse_url_test8,
     parse_url_test9,
     parse_url_test10].

%% Test cases starts here.
%%------------------------------------------------------------------------------

%% Tests for valid URL's

parse_url_test1(Config) when is_list(Config) -> 
    URL="http://user:password@subbie.dom.tld:1234/some/path/to/valid/cell/aa44",
    Expected={valid,{cell,{"http://","user:password@","subbie.dom.tld:1234",
			   "/some/path/to/valid/cell/","aa44",[]}}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test2() -> 
    [{userdata,[{doc,"Tests the function parse_url"}]}].

parse_url_test2(Config) when is_list(Config) -> 
    URL="https://user:password@subbie.dom.tld:1234/some/path/to/valid/cell/aa44",
    Expected={valid,{cell,{"https://","user:password@","subbie.dom.tld:1234",
			   "/some/path/to/valid/cell/","aa44",[]}}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test3() -> 
    [{userdata,[{doc,"Tests the function parse_url"}]}].

parse_url_test3(Config) when is_list(Config) -> 
    URL="http://user:password@subbie.dom.tld/some/path/to/valid/cell/aa44",
    Expected={valid,{cell,{"http://","user:password@","subbie.dom.tld:80",
			   "/some/path/to/valid/cell/","aa44",[]}}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test4() -> 
    [{userdata,[{doc,"Tests the function parse_url"}]}].

parse_url_test4(Config) when is_list(Config) -> 
    URL="https://user:password@subbie.dom.tld/some/path/to/valid/cell/aa44",
    Expected={valid,{cell,{"https://","user:password@","subbie.dom.tld:443",
			   "/some/path/to/valid/cell/","aa44",[]}}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).

parse_url_test4a() -> 
    [{userdata,[{doc,"Tests the function parse_url"}]}].


parse_url_test4a(Config) when is_list(Config) -> 
    URL="https://user:password@subbie.dom.tld/some/path/to/valid/cell/aa44:b77",
    Expected={valid,{range,{"https://","user:password@","subbie.dom.tld:443",
			    "/some/path/to/valid/cell/","aa44:b77",[]}}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test4b() -> 
    [{userdata,[{doc,"Tests the function parse_url"}]}].

parse_url_test4b(Config) when is_list(Config) -> 
    URL="https://user:password@subbie.dom.tld/some/path/to/valid/cell/aa44:b77?decorated_up_the_ying_yang",
    Expected={valid,{range,{"https://","user:password@","subbie.dom.tld:443",
			    "/some/path/to/valid/cell/","aa44:b77",
			    "decorated_up_the_ying_yang"}}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).

parse_url_test4c() -> 
    [{userdata,[{doc,"Tests the function parse_url"}]}].

parse_url_test4c(Config) when is_list(Config) -> 
    URL="http://jeteasy.com:9000/xxx/a1?hypernumber",
    Expected={valid,{cell,{"http://","","jeteasy.com:9000","/xxx/",
			    "a1","hypernumber"}}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


%% Now through some invalid ones

parse_url_test5() -> 
    [{userdata,[{doc,"Tests the function parse_url - URL invalid wrong protocol"}]}].

parse_url_test5(Config) when is_list(Config) -> 
    URL="junk://user:password@subbie.dom.tld/some/path/to/valid/cell/aa44",
    Expected={invalid,{protocol,URL}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test6() -> 
    [{userdata,[{doc,"Tests the function parse_url - URL invalid duff port"}]}].

parse_url_test6(Config) when is_list(Config) -> 
    URL="http://user:password@subbie.dom.tld:abcd/some/path/to/valid/cell/aa44",
    Expected={invalid,{port,URL}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test7() -> 
    [{userdata,[{doc,"Tests the function parse_url - URL invalid duff port"}]}].

parse_url_test7(Config) when is_list(Config) -> 
    URL="http://user:password@subbie.dom.tld:70000/some/path/to/valid/cell/aa44",
    Expected={invalid,{port,URL}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test8() -> 
    [{userdata,[{doc,"Tests the function parse_url - URL to a page"}]}].

parse_url_test8(Config) when is_list(Config) -> 
    URL="http://user:password@subbie.dom.tld:1234/some/path/to/valid/cell/",
    Expected={invalid,{ref,URL}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test9() -> 
    [{userdata,[{doc,"Tests the function parse_url - URL to a mutant range"}]}].

parse_url_test9(Config) when is_list(Config) -> 
    URL="http://user:password@subbie.dom.tld:1234/some/path/to/valid/cell/a3-:33",
    Expected={invalid,{ref,URL}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).


parse_url_test10() -> 
    [{userdata,[{doc,"Tests the function parse_url - fire junk in the swine!"}]}].

parse_url_test10(Config) when is_list(Config) -> 
    URL="bob",
    Expected={invalid,{url,URL}},
    Got=hn_util:parse_url(URL),
    test_util:expected(Expected,Got).
