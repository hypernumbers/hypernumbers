%%%-----------------------------------------------------------------------------
%%% File        : webpage_test_suite.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests security for a webpage view
%%%
%%% Created     : 30 August 2010 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(wp_logged_in_no_perms_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").

-define(test(Name, Path, Input, Expected),
        Name(_Config) ->
               URL  = ?SITE++Path,
               Got = test:post_login_TEST("test@hypernumbers.com",
                                          "i!am!secure",URL, Input),
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
     wp_test2a,
     wp_test2b,
     wp_test2c,
     wp_test3,
     wp_test4,
     wp_test5a,
     wp_test5b,
     wp_test6a,
     wp_test6b,
     wp_test7a,
     wp_test7b,
     wp_test8a,
     wp_test8b,
     wp_test9a,
     wp_test9b,
     wp_test10a,
     wp_test10b,
     wp_test11,
     wp_test12a,
     wp_test12b,
     wp_test12c,
     wp_test12d,
     wp_test13,
     wp_test14,
     wp_test15,
     wp_test16,
     wp_test17,
     wp_test18,
     wp_test19,
     wp_test20,
     wp_test21,
     wp_test22,
     wp_test23,
     wp_test24,
     wp_test25
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------

%ipost(Ref=#refX{site = S, path = P}, _Qry, 
%      Env=#env{body = [{"drag", {_, [{"range", Rng}]}}],
%               uid = Uid})
?test(wp_test1, "/A1:A1", "{\"drag\":{\"range\": \"A4:B9\"}}", 401).

%ipost(#refX{site=_Site, path=["_forgotten_password"]}=Ref, _Qry, Env)
?test(wp_test2a, "/_forgotten_password/", "{\"email\":\"a@b.com\"}", 200).

?test(wp_test2b, "/_forgotten_password/",
      "{\"email\": \"a@b.com\", \"site\": \"http://example.com\"}", 200).

?test(wp_test2c, "/_forgotten_password/",
      "{\"email\": \"a@b.com\", \"hash\": \"c\", \"newpwd\": \"d\"}", 200).

%ipost(#refX{site=Site, path=["_login"]}, Qry, E) ->
%    [{"email", Email0},{"pass", Pass}, {"remember", _R}] = ?SORT(E#env.body),
?test(wp_test3, "/_login/", "{\"email\":\"a@b.com\",\"pass\": \"hey\", \"remember\":\"false\"}", 200).

%ipost(_Ref, #qry{mark = []}, 
%      Env=#env{body = [{"set",{struct, [{"mark", _Msg}]}}]}) ->
?test(wp_test4, "/401path/", "{\"set\":{\"mark\":\"mark message\"}}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"insert", "before"}], uid = Uid})
%  when O == row orelse O == column ->
?test(wp_test5a, "/A:A", "{\"insert\":\"before\"}", 401).
?test(wp_test5b, "/1:1", "{\"insert\":\"before\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"insert", "after"}], uid = Uid})
%  when O == row orelse O == column ->
?test(wp_test6a, "/A:A", "{\"insert\":\"after\"}", 401).
?test(wp_test6b, "/1:1", "{\"insert\":\"after\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"insert", "before"}], uid = Uid})
%  when O == cell orelse O == range ->
?test(wp_test7a, "/A1", "{\"insert\":\"before\"}", 401).
?test(wp_test7b, "/A1:B2", "{\"insert\":\"before\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"insert", "after"}], uid = Uid})
%  when O == cell orelse O == range ->
?test(wp_test8a, "/A1", "{\"insert\":\"after\"}", 401).
?test(wp_test8b, "/A1:B2", "{\"insert\":\"after\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"insert", "before"}, {"displacement", D}],
%               uid = Uid})
%  when O == cell orelse O == range,
%       D == "horizontal" orelse D == "vertical" ->
?test(wp_test9a, "/A1", "{\"insert\":\"before\", \"displacement\": \"horizontal\"}", 401).
?test(wp_test9b, "/A1:B2", "{\"insert\":\"before\", \"displacement\": \"vertical\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"insert", "after"}, {"displacement", D}],
%               uid = Uid})
%  when O == cell orelse O == range,
%       D == "horizontal" orelse D == "vertical" ->
?test(wp_test10a, "/A1", "{\"insert\":\"after\", \"displacement\": \"horizontal\"}", 401).
?test(wp_test10b, "/A1:B2", "{\"insert\":\"after\", \"displacement\": \"vertical\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"delete", "all"}], uid = Uid}) 
%  when O == page ->
?test(wp_test11, "/401path/", "{\"delete\":\"all\"}", 401).

%ipost(Ref, _Qry, 
%      Env=#env{body=[{"delete", "all"}],
%               uid = Uid}) ->
?test(wp_test12a, "/401path/A1", "{\"delete\":\"all\"}", 401).
?test(wp_test12b, "/401path/A1:B1", "{\"delete\":\"all\"}", 401).
?test(wp_test12c, "/401path/A:A", "{\"delete\":\"all\"}", 401).
?test(wp_test12d, "/401path/1:1", "{\"delete\":\"all\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"delete", Direction}],
%               uid = Uid})
%  when O == cell orelse O == range,
%       Direction == "horizontal" orelse Direction == "vertical" ->
?test(wp_test13, "/401path/A1", "{\"delete\":\"horizonal\"}", 401).

%ipost(#refX{obj = {O, _}} = Ref, _Qry, 
%      Env=#env{body=[{"insert", Direction}],
%               uid = Uid})
%  when O == cell orelse O == range,
%       Direction == "horizontal" orelse Direction == "vertical" ->
?test(wp_test14, "/401path/A1", "{\"insert\":\"vertical\"}", 401).

%ipost(Ref, 
%      _Qry,
%      Env=#env{body=[{"copy", {struct, [{"src", Src}]}}],
%               uid = Uid}) ->
?test(wp_test15, "/401path/A1", "{\"copy\":{\"src\": \"b1\"}}", 401).

%ipost(Ref, 
%      _Qry,
%      Env=#env{body=[{"copystyle", {struct, [{"src", Src}]}}],
%               uid = Uid}) ->
?test(wp_test16, "/401path/A1", "{\"copystyle\":{\"src\": \"b1\"}}", 401).

%ipost(Ref, 
%      _Qry,
%      Env=#env{body=[{"copyvalue", {struct, [{"src", Src}]}}],
%               uid = Uid}) ->
?test(wp_test17, "/401path/A1", "{\"copyvalue\":{\"src\": \"b1\"}}", 401).

%ipost(#refX{obj = {range, _}} = Ref, _Qry, 
%      Env=#env{body=[{"borders", {struct, Attrs}}]}) ->
?test(wp_test18, "/401path/", "{\"borders\":{\"top-border\":\"none\"}}", 401).

%ipost(#refX{site = _Site, path=["_user"]}, _Qry, 
%      _Env=#env{body = [{"set", {struct, [{"language", _Lang}]}}], 
%               uid = _Uid}) ->
?test(wp_test19, "/401path/", "{\"set\":{\"language\":\"de\"}}", 401).

%ipost(Ref=#refX{path = P} = Ref, _Qry,
%      Env=#env{body = [{"postform", {struct, Vals}}], uid = PosterUid}) ->
?test(wp_test20, "/401path/", "{\"postform\":{\"bleh\":\"bloh\"}}", 401).

%ipost(Ref, _Qry, Env=#env{body = [{"set", {struct, Attr}}], uid = Uid}) ->
?test(wp_test21, "/401path/", "{\"set\":{\"bleh\":\"bloh\"}}", 401).

%ipost(Ref, _Qry, Env=#env{body = [{"clear", What}], uid = Uid}) 
%  when What == "contents"; What == "style"; What == "all" ->
?test(wp_test22, "/401path/", "{\"clear\":\"contents\"}", 401).

%ipost(Ref, _Qry, Env=#env{body = [{"clear", What}], uid = Uid}) ->
?test(wp_test23, "/401path/", "{\"clear\": \"arbritrary\"}", 401).

%ipost(#refX{site = Site, path = _P}, _Qry,
%      Env=#env{body = [{"admin", Json}], uid = Uid}) ->
?test(wp_test24, "/401path/", "{\"admin\":\"json\"}", 401).

%ipost(#refX{site=RootSite, path=["_hooks"]}, 
%      _Qry, Env=#env{body=Body, uid=PrevUid}) ->
%    [{"signup",{struct,[{"email",Email0} , {"sitetype", SiteType}]}}] = Body,
?test(wp_test25, "/_hooks/", "{\"signup\":{\"email\":\"a@b.com\",\"sitetype\":\"blank\"}}", 200).



