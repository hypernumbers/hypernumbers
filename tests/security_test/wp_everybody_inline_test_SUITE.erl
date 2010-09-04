%%%-----------------------------------------------------------------------------
%%% File        : webpage_test_suite.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests security for a webpage view
%%%
%%% Created     : 30 August 2010 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(wp_everybody_inline_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").

-define(test(Name, Path, Input, Expected),
        Name(_Config) ->
               URL  = ?SITE++Path,
               Got = test:post_logged_out_TEST(URL, Input),
               case Expected of
                   Got    -> {test, ok};
                   _Other -> io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                       [Expected, Got]),
                             exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                                  ++ " of " ++ atom_to_list(?MODULE))
               end).

%% callbacks
init_per_suite(Config) ->
    Path = ["wp_test1"],
    Group = "admin", 
    View = "webpage",
    Node = list_to_atom(atom_to_list(read_config(nodename))
                        ++"@"++net_adm:localhost()),
    io:format("Node is ~p~n", [Node]),
    ok = rpc:call(Node, auth_srv, add_view,
             [?SITE, Path, [Group], View]),
    Ret2 = rpc:call(Node, auth_srv, dump_script, [?SITE]),
    io:format("Perms are ~p~n", [Ret2]),
    Config.
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
     wp_test1,
     wp_test2,
     wp_test3,
     wp_test4,
     wp_test5,
     wp_test6
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------ 
%% the passing test - the form is taken straight of the page being tested...
?test(wp_test1, "/test4/a1", "{\"postinline\":{\"formula\":\"bleh\"}}", 200).

?test(wp_test2, "/test4/a2", "{\"postinline\":{\"formula\":\"bleh\"}}", 403).

?test(wp_test3, "/test4/a1:a1", "{\"postinline\":{\"formula\":\"bleh\"}}", 404).

?test(wp_test4, "/test4/a:a", "{\"postinline\":{\"formula\":\"bleh\"}}", 404).

?test(wp_test5, "/test4/1:1", "{\"postinline\":{\"formula\":\"bleh\"}}", 404).

?test(wp_test6, "/test4/", "{\"postinline\":{\"formula\":\"bleh\"}}", 404).
