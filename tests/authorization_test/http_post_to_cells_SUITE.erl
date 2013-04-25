%%%-----------------------------------------------------------------------------
%%% File        : http_post_to_cells_suite.erl
%%% Author      : Gordon Guthrie <gordon@vixo.com>
%%% Description : tests the general post security
%%%
%%% Created     : 22nd April 2013 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(http_post_to_cells_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").

-define(test(Name, Path, Body, Expected, Status),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Fn = case Status of
                        loggedinjson          ->
                            post_logged_in_TEST;
                        loggedinjsonnotadmin  ->
                            post_logged_in_notadmin_TEST;
                        loggedoutjson         ->
                            post_logged_out_TEST
                    end,
               Got = test:Fn(URL, Body),
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
all2()->
    [
     post_range2c
    ].

all() ->
    [
     post_formula1a,
     post_formula1b,
     post_formula2a,
     post_formula2b,
     post_formula3a,
     post_formula3b,
     post_formula4a,
     post_formula4b,
     post_formula5a,
     post_formula5b,

     post_range1a,
     post_range1b,
     post_range2a,
     post_range2c,
     post_range2d,
     post_range2b,
     post_range3a,
     post_range3b,
     post_range4a,
     post_range4b,
     post_range5a,
     post_range5b,

     post_select1a,
     post_select1b,
     post_select2a,
     post_select2b,
     post_select3a,
     post_select3b,
     post_select4a,
     post_select4b,
     post_select5a,
     post_select5b,

     post_dynamic1a,
     post_dynamic1b,
     post_dynamic2a,
     post_dynamic2b,
     post_dynamic3a,
     post_dynamic3b,
     post_dynamic4a,
     post_dynamic4b,
     post_dynamic5a,
     post_dynamic5b,

     post_rich1a,
     post_rich1b,
     post_rich2a,
     post_rich2b,
     post_rich3a,
     post_rich3b,
     post_rich4a,
     post_rich4b,
     post_rich5a,
     post_rich5b,

     post_format1a,
     post_format1b,
     post_format2a,
     post_format2b,
     post_format3a,
     post_format3b,
     post_format4a,
     post_format4b,
     post_format5a,
     post_format5b,

     post_merge1a,
     post_merge1b,
     post_merge2a,
     post_merge2b,
     post_merge3a,
     post_merge3b,
     post_merge4a,
     post_merge4b,
     post_merge5a,
     post_merge5b,

     post_ghost1a,
     post_ghost1b,
     post_ghost2a,
     post_ghost2b,
     post_ghost3a,
     post_ghost3b,
     post_ghost4a,
     post_ghost4b,
     post_ghost5a,
     post_ghost5b

    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
% formula
-define(FORMULA, "{\"set\":{\"formula\":\"3333\"}}").
?test(post_formula1a, "/some/page/a1", ?FORMULA, 200, loggedinjson).
?test(post_formula1b, "/some/page/a1", ?FORMULA, 401, loggedoutjson).

?test(post_formula2a, "/some/page/a1:b5", ?FORMULA, 200, loggedinjson).
?test(post_formula2b, "/some/page/a1:b5", ?FORMULA, 401, loggedoutjson).

?test(post_formula3a, "/some/page/a:b", ?FORMULA, 401, loggedinjson).
?test(post_formula3b, "/some/page/a:b", ?FORMULA, 401, loggedoutjson).

?test(post_formula4a, "/some/page/1:2", ?FORMULA, 401, loggedinjson).
?test(post_formula4b, "/some/page/1:2", ?FORMULA, 401, loggedoutjson).

?test(post_formula5a, "/some/page/", ?FORMULA, 401, loggedinjson).
?test(post_formula5b, "/some/page/", ?FORMULA, 401, loggedoutjson).

-define(RANGE, "{\"set\":{\"formula\":[[\"A1\", \"B1\"],[\"A2\", \"B2\"],[\"A3\", \"B3\"]]}}").
?test(post_range1a, "/a/different/page/a1", ?RANGE, 401, loggedinjson).
?test(post_range1b, "/a/different/page/a1", ?RANGE, 401, loggedoutjson).

?test(post_range2a, "/a/different/page/a1:b3", ?RANGE, 200, loggedinjson).
?test(post_range2b, "/a/different/page/a1:b3", ?RANGE, 401, loggedoutjson).

?test(post_range2c, "/a/different/page/c7:e8", ?RANGE, 401, loggedinjson).
?test(post_range2d, "/a/different/page/c7:e8", ?RANGE, 401, loggedoutjson).

?test(post_range3a, "/a/different/page/a:b", ?RANGE, 401, loggedinjson).
?test(post_range3b, "/a/different/page/a:b", ?RANGE, 401, loggedoutjson).

?test(post_range4a, "/a/different/page/1:2", ?RANGE, 401, loggedinjson).
?test(post_range4b, "/a/different/page/1:2", ?RANGE, 401, loggedoutjson).

?test(post_range5a, "/a/different/page/", ?RANGE, 401, loggedinjson).
?test(post_range5b, "/a/different/page/", ?RANGE, 401, loggedoutjson).

% select inputs
-define(SELECT, "{\"set\":{\"input\":{\"select\":[\"1\",\"2\",\"3\"]}}}").
?test(post_select1a, "/some/page/a1", ?SELECT, 200, loggedinjson).
?test(post_select1b, "/some/page/a1", ?SELECT, 401, loggedoutjson).

?test(post_select2a, "/some/page/a1:b5", ?SELECT, 200, loggedinjson).
?test(post_select2b, "/some/page/a1:b5", ?SELECT, 401, loggedoutjson).

?test(post_select3a, "/some/page/a:b", ?SELECT, 401, loggedinjson).
?test(post_select3b, "/some/page/a:b", ?SELECT, 401, loggedoutjson).

?test(post_select4a, "/some/page/1:2", ?SELECT, 401, loggedinjson).
?test(post_select4b, "/some/page/1:2", ?SELECT, 401, loggedoutjson).

?test(post_select5a, "/some/page/", ?SELECT, 401, loggedinjson).
?test(post_select5b, "/some/page/", ?SELECT, 401, loggedoutjson).

-define(DYNAMIC, "{\"set\":{\"input\":{\"dynamic_select\":\"a1:a3\"}}}").
?test(post_dynamic1a, "/some/page/a1", ?DYNAMIC, 200, loggedinjson).
?test(post_dynamic1b, "/some/page/a1", ?DYNAMIC, 401, loggedoutjson).

?test(post_dynamic2a, "/some/page/a1:b5", ?DYNAMIC, 200, loggedinjson).
?test(post_dynamic2b, "/some/page/a1:b5", ?DYNAMIC, 401, loggedoutjson).

?test(post_dynamic3a, "/some/page/a:b", ?DYNAMIC, 401, loggedinjson).
?test(post_dynamic3b, "/some/page/a:b", ?DYNAMIC, 401, loggedoutjson).

?test(post_dynamic4a, "/some/page/1:2", ?DYNAMIC, 401, loggedinjson).
?test(post_dynamic4b, "/some/page/1:2", ?DYNAMIC, 401, loggedoutjson).

?test(post_dynamic5a, "/some/page/", ?DYNAMIC, 401, loggedinjson).
?test(post_dynamic5b, "/some/page/", ?DYNAMIC, 401, loggedoutjson).

-define(RICH, "{\"set\":{\"input\":\"inlinerich\"}}").
?test(post_rich1a, "/some/page/a1", ?RICH, 200, loggedinjson).
?test(post_rich1b, "/some/page/a1", ?RICH, 401, loggedoutjson).

?test(post_rich2a, "/some/page/a1:b5", ?RICH, 200, loggedinjson).
?test(post_rich2b, "/some/page/a1:b5", ?RICH, 401, loggedoutjson).

?test(post_rich3a, "/some/page/a:b", ?RICH, 401, loggedinjson).
?test(post_rich3b, "/some/page/a:b", ?RICH, 401, loggedoutjson).

?test(post_rich4a, "/some/page/1:2", ?RICH, 401, loggedinjson).
?test(post_rich4b, "/some/page/1:2", ?RICH, 401, loggedoutjson).

?test(post_rich5a, "/some/page/", ?RICH, 401, loggedinjson).
?test(post_rich5b, "/some/page/", ?RICH, 401, loggedoutjson).

-define(MAGICS, "["
        ++ "{\"border-right-style\":"
        ++ "{\"border-left-style\":"
        ++ "{\"border-top-style\":"
        ++ "{\"border-bottom-style\":"
        ++ "{\"border-right-color\":"
        ++ "{\"border-left-color\":"
        ++ "{\"border-top-color\":"
        ++ "{\"border-bottom-color\":"
        ++ "{\"border-right-width\":"
        ++ "{\"border-left-width\":"
        ++ "{\"border-top-width\":"
        ++ "{\"border-bottom-width\":"
        ++ "{color"
        ++ "{\"vertical-align\":"
        ++ "{\"background-color\":"
        ++ "{\"font-weight\":"
        ++ "{\"font-size\":"
        ++ "{\"font-family\":"
        ++ "{\"font-style\":"
        ++ "{\"font-stretch\": \"expanded\"}," % condensed | expanded
        ++ "{\"text-decoration\":"
        ++ "{\"text-shadow\": \"2px 2px 2px\"}"
        ++ "{\"text-align\":"
        ++ "{\"white-space\":").

-define(FORMAT, "{\"set\":{\"format\":\"#,0.0\"}}").
?test(post_format1a, "/some/page/a1", ?FORMAT, 200, loggedinjson).
?test(post_format1b, "/some/page/a1", ?FORMAT, 401, loggedoutjson).

?test(post_format2a, "/some/page/a1:b5", ?FORMAT, 200, loggedinjson).
?test(post_format2b, "/some/page/a1:b5", ?FORMAT, 401, loggedoutjson).

?test(post_format3a, "/some/page/a:b", ?FORMAT, 401, loggedinjson).
?test(post_format3b, "/some/page/a:b", ?FORMAT, 401, loggedoutjson).

?test(post_format4a, "/some/page/1:2", ?FORMAT, 401, loggedinjson).
?test(post_format4b, "/some/page/1:2", ?FORMAT, 401, loggedoutjson).

?test(post_format5a, "/some/page/", ?FORMAT, 401, loggedinjson).
?test(post_format5b, "/some/page/", ?FORMAT, 401, loggedoutjson).

-define(MERGE, "{\"set\":{\"merge\":{\"right\":1,\"down\":1}}}").
?test(post_merge1a, "/some/page/a1", ?MERGE, 200, loggedinjson).
?test(post_merge1b, "/some/page/a1", ?MERGE, 401, loggedoutjson).

?test(post_merge2a, "/some/page/a1:b5", ?MERGE, 200, loggedinjson).
?test(post_merge2b, "/some/page/a1:b5", ?MERGE, 401, loggedoutjson).

?test(post_merge3a, "/some/page/a:b", ?MERGE, 401, loggedinjson).
?test(post_merge3b, "/some/page/a:b", ?MERGE, 401, loggedoutjson).

?test(post_merge4a, "/some/page/1:2", ?MERGE, 401, loggedinjson).
?test(post_merge4b, "/some/page/1:2", ?MERGE, 401, loggedoutjson).

?test(post_merge5a, "/some/page/", ?MERGE, 401, loggedinjson).
?test(post_merge5b, "/some/page/", ?MERGE, 401, loggedoutjson).

-define(GHOST, "{\"set\":{\"ghost\":true}}").
?test(post_ghost1a, "/some/page/a1", ?GHOST, 200, loggedinjson).
?test(post_ghost1b, "/some/page/a1", ?GHOST, 401, loggedoutjson).

?test(post_ghost2a, "/some/page/a1:b5", ?GHOST, 200, loggedinjson).
?test(post_ghost2b, "/some/page/a1:b5", ?GHOST, 401, loggedoutjson).

?test(post_ghost3a, "/some/page/a:b", ?GHOST, 401, loggedinjson).
?test(post_ghost3b, "/some/page/a:b", ?GHOST, 401, loggedoutjson).

?test(post_ghost4a, "/some/page/1:2", ?GHOST, 401, loggedinjson).
?test(post_ghost4b, "/some/page/1:2", ?GHOST, 401, loggedoutjson).

?test(post_ghost5a, "/some/page/", ?GHOST, 401, loggedinjson).
?test(post_ghost5b, "/some/page/", ?GHOST, 401, loggedoutjson).
