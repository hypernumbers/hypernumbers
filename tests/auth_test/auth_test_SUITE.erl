%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Test suite for authentication
%%%
%%% @end
%%% Created :  22 Mar 2012 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(auth_test_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(AUTH1, "http://auth.hypernumbers.dev:9000").
-define(AUTH2, "http://auth.hypernumbers.auth:9000").
-define(AUTH3, "http://hypernumbers.dev:9000").

-include("ct.hrl").

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
    {initial_view, []} = hn_setup:site("http://auth.hypernumbers.dev:9000",
                                       blank, ""),
    {initial_view, []} = hn_setup:site("http://auth.hypernumbers.auth:9000",
                                       blank, ""),
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
    ok = hn_setup:delete_site("http://auth.hypernumbers.dev:9000"),
    ok = hn_setup:delete_site("http://auth.hypernumbers.auth:9000"),
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
    application:set_env(hypernumbers, sync_url,
                        "http://hypernumbers.dev:9000"),
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
     test1a,
     test1b,
     test2a,
     test2b,
     test3a
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
% in the 1 cases the site being looked at is a subdomain of the sync_url site
test1a() -> [{doc, "Anonymous person comes to web site - gets redirect (1)"}].

test1a(Config) when is_list(Config) ->
    httpc:reset_cookies(),
    io:format("Cookies is ~p~n", [httpc:which_cookies()]),
    CHeaders = [{"Accept", "text/html"}],
    Ret = httpc:request(get, {?AUTH1, CHeaders}, [{autoredirect, false}], []),
    {ok, {{"HTTP/1.1", Code, _}, Options, _R}} = Ret,
    io:format("Code is ~p~n", [Code]),
    io:format("Options is ~p~n", [Options]),
    case Code of
        303 -> ok;
        N   -> exit({error, N})
    end.

test1b() -> [{doc, "Anonymous person comes to web site - "
             ++ "gets redirect - follows it (1)"}].

test1b(Config) when is_list(Config) ->
    httpc:reset_cookies(),
    io:format("Cookies is ~p~n", [httpc:which_cookies()]),
    CHeaders = [{"Accept", "text/html"}],
    Ret = httpc:request(get, {?AUTH1, CHeaders}, [{autoredirect, false}], []),
    {ok, {{"HTTP/1.1", Code, _}, Options, _R}} = Ret,
    io:format("Code is ~p~n", [Code]),
    io:format("Options is ~p~n", [Options]),
    case lists:keysearch("location", 1, Options) of
        false ->
            exit('no redirect');
        {value, {"location", URL}} ->
            Ret2 = httpc:request(get, {URL, CHeaders},
                                 [{autoredirect, false}], []),
            {ok, {{"HTTP/1.1", Code2, _}, Options2, _R2}} = Ret2,
            io:format("Code2 is ~p~n", [Code2]),
            io:format("Options2 is ~p~n", [Options2]),
            case lists:keysearch("location", 1, Options2) of
                false ->
                    exit('no 2nd redirect');
                {value, {"location", URL2}} ->
                    case URL2 of
                        ?AUTH1 ++ _Rest ->
                            ok;
                        _Other ->
                            exit('wrong return')
                    end
            end,
            ok
    end.

% in the 2 cases the site being looked at is a different domain to the sync_url
test2a() -> [{doc, "Anonymous person comes to web site - gets redirect (2)"}].

test2a(Config) when is_list(Config) ->
    httpc:reset_cookies(),
    io:format("Cookies is ~p~n", [httpc:which_cookies()]),
    CHeaders = [{"Accept", "text/html"}],
    Ret = httpc:request(get, {?AUTH2, CHeaders}, [{autoredirect, false}], []),
    {ok, {{"HTTP/1.1", Code, _}, Options, _R}} = Ret,
    io:format("Code is ~p~n", [Code]),
    io:format("Options is ~p~n", [Options]),
    case Code of
        303 -> ok;
        N   -> exit({error, N})
    end.

test2b() -> [{doc, "Anonymous person comes to web site - "
             ++ "gets redirect - follows it (2)"}].

test2b(Config) when is_list(Config) ->
    httpc:reset_cookies(),
    io:format("Cookies is ~p~n", [httpc:which_cookies()]),
    CHeaders = [{"Accept", "text/html"}],
    Ret = httpc:request(get, {?AUTH2, CHeaders}, [{autoredirect, false}], []),
    {ok, {{"HTTP/1.1", Code, _}, Options, _R}} = Ret,
    io:format("Code is ~p~n", [Code]),
    io:format("Options is ~p~n", [Options]),
    case lists:keysearch("location", 1, Options) of
        false ->
            exit('no redirect');
        {value, {"location", URL}} ->
            Ret2 = httpc:request(get, {URL, CHeaders},
                                 [{autoredirect, false}], []),
            {ok, {{"HTTP/1.1", Code2, _}, Options2, _R2}} = Ret2,
            io:format("Code2 is ~p~n", [Code2]),
            io:format("Options2 is ~p~n", [Options2]),
            case lists:keysearch("location", 1, Options2) of
                false ->
                    exit('no 2nd redirect');
                {value, {"location", URL2}} ->
                    case URL2 of
                        ?AUTH2 ++ _Rest ->
                            ok;
                        _Other ->
                            exit('wrong return')
                    end
            end,
            ok
    end.

% in this one the sync_url is the URL being queried so there is no redirect
test3a() -> [{doc, "Anonymous person comes to web site - gets redirect (3)"}].

test3a(Config) when is_list(Config) ->
    httpc:reset_cookies(),
    io:format("Cookies is ~p~n", [httpc:which_cookies()]),
    CHeaders = [{"Accept", "text/html"}],
    Ret = httpc:request(get, {?AUTH3, CHeaders}, [{autoredirect, false}], []),
    {ok, {{"HTTP/1.1", Code, _}, Options, _R}} = Ret,
    io:format("Code is ~p~n", [Code]),
    io:format("Options is ~p~n", [Options]),
    case Code of
        200 -> ok;
        N   -> exit({error, N})
    end.

