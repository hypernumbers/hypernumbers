%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Test suite for authentication
%%%
%%% @end
%%% Created :  22 Mar 2012 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(provision_test_SUITE).

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
     test1a
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
% in the 1 cases the site being looked at is a subdomain of the sync_url site
test1a() -> [{doc, "Create a new website"}].

test1a(Config) when is_list(Config) ->

    SessionCookie1 = login(?USERADMIN, ?AUTH1, []),
    io:format("SessionCookie1 is ~p~n", [SessionCookie1]),
    {redirect, NewSite} = provision(?USERUSER, ?AUTH1, [SessionCookie1]),
    io:format("NewSite is ~p~n", [NewSite]),
    CHeaders = [{"Accept", "text/html"}],
    io:format("Visting ~p~n", [NewSite]),
    % we go to the new site to be logged in as the new user but with the
    % old cookie value
    {URL2, 200, Options2, Cookie2, R2} = redirect(NewSite, CHeaders,
                                                  [SessionCookie1]),
    io:format("URL2 is ~p~nOptions2 is ~p~nCookie2 is ~p~n",
              [URL2, Options2, Cookie2]),
    io:format("R2 is ~p~n", [R2]),
    % finally delete the new site to clear up
    ok = hn_setup:delete_site(NewSite),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Helper Fns
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_right_cookie(["auth="++_R = H | _T]) -> H;
get_right_cookie([_H | T]) -> get_right_cookie(T).

provision(User, Site, Cookies) ->
    io:format("Provisioning a site for ~p from ~p~n", [User, Site]),
    URL = Site ++ "/_hooks/",
    Provision = "{\"signup\":{\"email\":\"" ++ User
        ++ "\",\"sitetype\":\"blank\"}}",
    CHeaders = lists:merge([{"Accept", "application/json"}], Cookies),
    io:format("Provisioning with headers of ~p~n", [CHeaders]),
    case httpc:request(post, {URL, CHeaders, "application/json", Provision},
                       [], []) of
        {ok, {{_, 200, _}, Headers, Resp}} ->
            io:format("Response to provision is ~p~n- with headers of ~p~n",
                      [mochijson:decode(Resp), Headers]),
        {struct, List} = mochijson:decode(Resp),
        {value, {"url", NewSite}} = lists:keysearch("url", 1, List),
        {redirect, NewSite};
        Other ->
            io:format("Provisioning error ~p~n", [Other]),
            error
    end.

login(User, Site, Cookies) ->
    io:format("Logging ~p into ~p~n", [User, Site]),
    URL = Site ++ "/_login/",
    CHeaders = lists:merge([{"Accept", "application/json"}], Cookies),
    Login = "{\"email\":\"" ++ User ++ "\", \"pass\":\"" ++ ?PASSWORD
        ++ "\",\"remember\":\"false\"}",
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
