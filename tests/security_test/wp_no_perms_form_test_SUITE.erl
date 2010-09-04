%%%-----------------------------------------------------------------------------
%%% File        : webpage_test_suite.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests security for a webpage view
%%%
%%% Created     : 30 August 2010 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(wp_no_perms_form_test_SUITE).

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
     wp_test6,
     wp_test7,
     wp_test8
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------ 
%% the passing test - the form is taken straight of the page being tested...
?test(wp_test1, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 3:\",\"formula\":\"hey\"},{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Hip\"},{\"label\":\"Question 8:\",\"formula\":\"One for the money\"},{\"label\":\"Question 4:\",\"formula\":\"blah\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"}]}}", 401).

%% the failing tests - one field removed
?test(wp_test2, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Hip\"},{\"label\":\"Question 8:\",\"formula\":\"One for the money\"},{\"label\":\"Question 4:\",\"formula\":\"blah\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"}]}}", 401).

%% the failing tests - one field added
?test(wp_test3, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 3:\",\"formula\":\"hey\"},{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Hip\"},{\"label\":\"Question 8:\",\"formula\":\"One for the money\"},{\"label\":\"Question 4:\",\"formula\":\"blah\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"},{\"label\":\"new Question:\",\"formula\":\"howdy!\"}]}}", 401).

%% change value in drop down (values specified inline)
?test(wp_test4, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 3:\",\"formula\":\"changed on ya man!\"},{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Hip\"},{\"label\":\"Question 8:\",\"formula\":\"One for the money\"},{\"label\":\"Question 4:\",\"formula\":\"blah\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"}]}}", 401).

%% changed value in radio box (values specified inline)
?test(wp_test5, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 3:\",\"formula\":\"hey\"},{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Hip\"},{\"label\":\"Question 8:\",\"formula\":\"One for the money\"},{\"label\":\"Question 4:\",\"formula\":\"changed on ya man!\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"}]}}", 401).

%% change value in drop down (values specified in a lookup)
?test(wp_test6, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 3:\",\"formula\":\"hey\"},{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Changed here\"},{\"label\":\"Question 8:\",\"formula\":\"One for the money\"},{\"label\":\"Question 4:\",\"formula\":\"blah\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"}]}}", 401).

%% changed value in radio box (values specified a lookup)
?test(wp_test7, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 3:\",\"formula\":\"hey\"},{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Hip\"},{\"label\":\"Question 8:\",\"formula\":\"Changed here\"},{\"label\":\"Question 4:\",\"formula\":\"blah\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"}]}}", 401).

%% blank radio box
?test(wp_test8, "/test2/no_perms/", "{\"postform\":{\"results\":\"./replies/\",\"values\":[{\"label\":\"Question 3:\",\"formula\":\"hey\"},{\"label\":\"Question 11:\",\"formula\":\"\"},{\"label\":\"Question 7:\",\"formula\":\"Hip\"},{\"label\":\"Question 8:\",\"formula\":\"One for the money\"},{\"label\":\"Question 4:\",\"formula\":\"\"},{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 9:\",\"formula\":\"\"},{\"label\":\"Question 5:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 10:\",\"formula\":\"\"},{\"label\":\"Question 6:\",\"formula\":\"\"}]}}", 401).

