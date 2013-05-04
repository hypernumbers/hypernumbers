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
-define(USERADMIN, "admin@hypernumbers.com").
-define(USERUSER,  "user@hypernumbers.com").
-define(PASSWORD, "P455w0rd!").

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
    % life is easier if you ignore existing sites
    _ = hn_setup:site(?AUTH1, blank, ""),
    _ = hn_setup:site(?AUTH2, blank, ""),
    {ok, _, Uid1} = passport:get_or_create_user(?USERADMIN),
    {ok, _, Uid2} = passport:get_or_create_user(?USERUSER),
    ok = passport:validate_uid(Uid1),
    ok = passport:validate_uid(Uid2),
    ok = passport:set_password(Uid1, ?PASSWORD),
    ok = passport:set_password(Uid2, ?PASSWORD),
    ok = hn_groups:add_user(?AUTH1, "admin", ?USERADMIN),
    ok = hn_groups:add_user(?AUTH2, "admin", ?USERADMIN),
    ok = hn_groups:add_user(?AUTH3, "admin", ?USERADMIN),
    ok = hn_groups:add_user(?AUTH1, "user",  ?USERUSER),
    ok = hn_groups:add_user(?AUTH2, "user",  ?USERUSER),
    ok = hn_groups:add_user(?AUTH3, "user",  ?USERUSER),
    Groups1 = hn_groups:get_all_groups(?AUTH1),
    Groups2 = hn_groups:get_all_groups(?AUTH2),
    Groups3 = hn_groups:get_all_groups(?AUTH3),
    io:format("Groups on ~p is ~p~nGroups on ~p is ~p~nGroups on ~p is ~p~n",
              [?AUTH1, Groups1, ?AUTH2, Groups2, ?AUTH3, Groups3]),
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
     test1c,
     test1d,
     test1e,
     test2a,
     test2b,
     test2c,
     test2d,
     test2e,
     test3a, % yes there is a reason there isn't a test3b
     test3c,
     test3d,
     test3e
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
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH1]),
    {URL2, 303, Options2, Cookie2, _R2} = redirect(?AUTH1, CHeaders, []),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    {URL3, 303, Options3, Cookie3, _R3} = redirect(URL2, CHeaders, []),
    io:format("URL3 is ~p~nOptions3 is ~p~nCookie3 is ~p~n",
              [URL3, Options3, Cookie3]),
    {URL4, 303, Options4, Cookie4, _R4} = redirect(URL3, CHeaders, []),
    io:format("URL4 is ~p~nOptions4 is ~p~nCookie4 is ~p~n",
              [URL4, Options4, Cookie4]),
    {URL5, 200, Options5, Cookie5, _R5} = redirect(URL4, CHeaders, Cookie4),
    io:format("URL5 is ~p~nOptions5 is ~p~nCookie5 is ~p~n",
              [URL5, Options5, Cookie5]),
    ok.

test1c() -> [{doc, "Log in then get page (1)"}].

test1c(Config) when is_list(Config) ->

    SessionCookie = login(?USERADMIN, ?AUTH1, []),
    io:format("SessionCookie is ~p~n", [SessionCookie]),
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH1]),
    {URL2, 200, Options2, Cookie2, _R2} = redirect(?AUTH1, CHeaders, [SessionCookie]),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    ok.

test1d() -> [{doc, "Log in then get page (1)"}].

test1d(Config) when is_list(Config) ->

    SessionCookie = login(?USERUSER, ?AUTH1, []),
    io:format("SessionCookie is ~p~n", [SessionCookie]),
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH1]),
    {URL2, 200, Options2, Cookie2, R2} = redirect(?AUTH1, CHeaders, [SessionCookie]),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    ok.

test1e() -> [{doc, "Log in as one user, then login again as another"}].

test1e(Config) when is_list(Config) ->

    SessionCookie1 = login(?USERADMIN, ?AUTH1, []),
    io:format("SessionCookie1 is ~p~n", [SessionCookie1]),
    SessionCookie2 = login(?USERUSER, ?AUTH1, [SessionCookie1]),
    io:format("SessionCookie2 is ~p~n", [SessionCookie2]),
    ok.

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
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH2]),
    {URL2, 303, Options2, Cookie2, _R2} = redirect(?AUTH2, CHeaders, []),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    {URL3, 303, Options3, Cookie3, _R3} = redirect(URL2, CHeaders, []),
    io:format("URL3 is ~p~nOptions3 is ~p~nCookie3 is ~p~n",
              [URL3, Options3, Cookie3]),
    {URL4, 303, Options4, Cookie4, _R4} = redirect(URL3, CHeaders, Cookie3),
    io:format("URL4 is ~p~nOptions4 is ~p~nCookie4 is ~p~n",
              [URL4, Options4, Cookie4]),
    {URL5, 200, Options5, Cookie5, _R5} = redirect(URL4, CHeaders, Cookie4),
    io:format("URL5 is ~p~nOptions5 is ~p~nCookie5 is ~p~n",
              [URL5, Options5, Cookie5]),
    ok.

test2c() -> [{doc, "Log in then get page (1)"}].

test2c(Config) when is_list(Config) ->

    SessionCookie = login(?USERADMIN, ?AUTH2, []),
    io:format("SessionCookie is ~p~n", [SessionCookie]),
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH2]),
    {URL2, 200, Options2, Cookie2, _R2} = redirect(?AUTH2, CHeaders, [SessionCookie]),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    ok.

test2d() -> [{doc, "Log in then get page (1)"}].

test2d(Config) when is_list(Config) ->

    SessionCookie = login(?USERUSER, ?AUTH2, []),
    io:format("SessionCookie is ~p~n", [SessionCookie]),
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH2]),
    {URL2, 200, Options2, Cookie2, _R2} = redirect(?AUTH2, CHeaders,
                                                   [SessionCookie]),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    ok.

test2e() -> [{doc, "Log in as one user, then login again as another"}].

test2e(Config) when is_list(Config) ->

    SessionCookie1 = login(?USERADMIN, ?AUTH2, []),
    io:format("SessionCookie1 is ~p~n", [SessionCookie1]),
    SessionCookie2 = login(?USERUSER, ?AUTH2, [SessionCookie1]),
    io:format("SessionCookie2 is ~p~n", [SessionCookie2]),
    ok.

% in this one the sync_url is the URL being queried so there is no redirect
test3a() -> [{doc, "Anonymous person comes to web site - no redirect (3)"}].

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

% test1b and test2b test redirects - there are none if you are accessing the site
% that is the source of sync_url so no test3b

test3c() -> [{doc, "Log in then get page (1)"}].

test3c(Config) when is_list(Config) ->

    SessionCookie = login(?USERADMIN, ?AUTH3, []),
    io:format("SessionCookie is ~p~n", [SessionCookie]),
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH3]),
    {URL2, 200, Options2, Cookie2, _R2} = redirect(?AUTH3, CHeaders,
                                                   [SessionCookie]),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    ok.

test3d() -> [{doc, "Log in then get page (1)"}].

test3d(Config) when is_list(Config) ->

    SessionCookie = login(?USERUSER, ?AUTH3, []),
    io:format("SessionCookie is ~p~n", [SessionCookie]),
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [?AUTH3]),
    {URL2, 200, Options2, Cookie2, _R2} = redirect(?AUTH3, CHeaders,
                                                   [SessionCookie]),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    ok.

test3e() -> [{doc, "Log in as one user, then login again as another"}].

test3e(Config) when is_list(Config) ->

    SessionCookie1 = login(?USERADMIN, ?AUTH3, []),
    io:format("SessionCookie1 is ~p~n", [SessionCookie1]),
    SessionCookie2 = login(?USERUSER, ?AUTH3, [SessionCookie1]),
    io:format("SessionCookie2 is ~p~n", [SessionCookie2]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Helper Fns
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_right_cookie(["auth="++_R = H | _T]) -> H;
get_right_cookie([_H | T]) -> get_right_cookie(T).

login(User, Site, Cookies) ->
    io:format("Logging ~p into ~p~n", [User, Site]),
    URL = Site ++ "/_login/",
    CHeaders = lists:merge([{"Accept", "application/json"}], Cookies),
    Login = "{\"email\":\"" ++ User ++ "\", \"pass\":\"" ++ ?PASSWORD
        ++ "\",\"remember\":false}",
    io:format("Logging in with headers of ~p~n", [CHeaders]),
    case httpc:request(post, {URL, CHeaders, "application/json", Login}, [], []) of
        {ok, {{_, 200, _}, Headers, Resp}} ->
            io:format("Response to login is ~p~n", [Resp]),
            CookieList = proplists:get_all_values("set-cookie", Headers),
            io:format("In login CookieList is ~p~n", [CookieList]),
            {"set-cookie", get_right_cookie(CookieList)};
        Other ->
            io:format("Login error ~p~n", [Other]),
            error
    end.

redirect(URL, Headers, Cookies) ->
    ok = httpc:reset_cookies(),
    ok = httpc:set_options([{cookies, enabled}]),
    case Cookies of
        [] -> ok;
        _  -> io:format("Setting cookies ~p~n", [Cookies]),
              ok = httpc:store_cookies(Cookies, URL),
              io:format("Stored cookies are ~p~n", [httpc:which_cookies()])
    end,
    Ret = httpc:request(get, {URL, [Cookies | Headers]},
                            [{autoredirect, false}], []),
    {ok, {{"HTTP/1.1", Code, _}, Options, R}} = Ret,
    io:format("Options is ~p~n", [Options]),
    {URL3, Cookie3} = case lists:keysearch("location", 1, Options) of
                  {value, {"location", URL2}} ->
                              case lists:keysearch("set-cookie", 1, Options) of
                                  false            -> {URL2, []};
                                  {value, Cookie2} -> {URL2, [Cookie2]}
                      end;
                  _ ->
                      {[], []}
              end,
    {URL3, Code, Options, Cookie3, R}.
