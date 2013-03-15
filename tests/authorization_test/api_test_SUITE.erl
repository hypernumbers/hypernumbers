%%%-----------------------------------------------------------------------------
%%% File        : webpage_test_suite.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests security for a webpage view
%%%
%%% Created     : 30 August 2010 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(api_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").

-define(test(Name, Path, Expected, Status),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Fn = case Status of
                        loggedinjson          ->
                            get_logged_in_json_TEST;
                        loggedinjsonnotadmin  ->
                            get_logged_in_json_notadmin_TEST;
                        loggedoutjson         ->
                            get_logged_out_json_TEST;
                        loggedinhtml          ->
                            get_logged_in_html_TEST;
                        loggedinhtmlnotadmin  ->
                            get_logged_in_notadmin_html_TEST;
                        loggedouthtml         ->
                            get_logged_out_html_TEST
                    end,
               Got = test:Fn(URL),
               case Expected of
                   Got    -> {test, ok};
                   _Other -> io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                       [Expected, Got]),
                             exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                                  ++ " of " ++ atom_to_list(?MODULE))
               end).

-define(tapi(Name, Path, Expected, Status, Accept),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Got = test:get_api_TEST(URL, Status, Accept),
               case Expected of
                   Got    -> {test, ok};
                   _Other -> io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                       [Expected, Got]),
                             exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                                  ++ " of " ++ atom_to_list(?MODULE))
               end).

%% callbacks
init_per_suite(Config)               -> Config.
end_per_suite(_Config)               -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

read_config(Key) ->
    Root                   = code:lib_dir(hypernumbers)++"/../../",
    {ok, [Config]}         = file:consult([Root, "/var/", "sys.config"]),
    {hypernumbers, HNConf} = lists:keyfind(hypernumbers, 1, Config),
    {Key, Val}             = lists:keyfind(Key, 1, HNConf),
    Val.

%% tests to run
all() ->
    [
     % special pages
     get_json_page,

    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
%% The special pages tests
?test(get_json_page, "/some/page/",  200, api).  % can't see that page
