%%%-----------------------------------------------------------------------------
%%% File        : http_post_test_suite.erl
%%% Author      : Gordon Guthrie <gordon@vixo.com>
%%% Description : tests the general post security
%%%
%%% Created     : 31st March 2013 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(api_post_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").

-define(tapi(Name, Path, Body, Expected, Status),
        Name(_Config) ->
               io:format("running test ~p~n", [Name]),
               URL  = ?SITE ++ Path,
               io:format("URL is ~p~nBody is ~p~nStatus  is ~p~n",
                         [URL, Body, Status]),
               Got = test:post_api_TEST(URL, Body, Status),
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
    ].

all() ->
    [
     post_formula1a,
     post_formula1b,
     post_formula1c,
     post_formula1d,

     post_formula2a,
     post_formula2d,
     post_formula2c,
     post_formula2b,

     post_formula3a,
     post_formula3b,
     post_formula3c,
     post_formula3d,

     post_formula4a,
     post_formula4b,
     post_formula4c,
     post_formula4d,

     post_formula5a,
     post_formula5b,
     post_formula5c,
     post_formula5d,

     post_range1a,
     post_range1b,
     post_range1c,
     post_range1d,

     post_range2a,
     post_range2b,
     post_range2c,
     post_range2d,

     post_range2e,
     post_range2f,
     post_range2g,
     post_range2h,

     post_range3a,
     post_range3b,
     post_range3c,
     post_range3d,

     post_range4a,
     post_range4b,
     post_range4c,
     post_range4d,

     post_range5a,
     post_range5b,
     post_range5c,
     post_range5d,

     %% mark
     post_mark1a,
     post_mark2a,
     post_mark3a,
     post_mark4a,
     post_mark1b,
     post_mark2b,
     post_mark3b,
     post_mark4b,
     post_mark1c,
     post_mark2c,
     post_mark3c,
     post_mark4c,
     post_mark1d,
     post_mark2d,
     post_mark3d,
     post_mark4d,
     post_mark1e,
     post_mark2e,
     post_mark3e,
     post_mark4e,

						% jserr
     post_jserr1a,
     post_jserr2a,
     post_jserr3a,
     post_jserr4a,
     post_jserr1b,
     post_jserr2b,
     post_jserr3b,
     post_jserr4b,
     post_jserr1c,
     post_jserr2c,
     post_jserr3c,
     post_jserr4c,
     post_jserr1d,
     post_jserr2d,
     post_jserr3d,
     post_jserr4d,
     post_jserr1e,
     post_jserr2e,
     post_jserr3e,
     post_jserr4e,

						% loading templates
     post_loadtemplate1a,
     post_loadtemplate2a,
     post_loadtemplate3a,
     post_loadtemplate4a,
     post_loadtemplate1b,
     post_loadtemplate2b,
     post_loadtemplate3b,
     post_loadtemplate4b,
     post_loadtemplate1c,
     post_loadtemplate2c,
     post_loadtemplate3c,
     post_loadtemplate4c,
     post_loadtemplate1d,
     post_loadtemplate2d,
     post_loadtemplate3d,
     post_loadtemplate4d,
     post_loadtemplate1e,
     post_loadtemplate2e,
     post_loadtemplate3e,
     post_loadtemplate4e,

						% drag and drop
     post_drag_and_drop1a,
     post_drag_and_drop2a,
     post_drag_and_drop3a,
     post_drag_and_drop4a,
     post_drag_and_drop1b,
     post_drag_and_drop2b,
     post_drag_and_drop3b,
     post_drag_and_drop4b,
     post_drag_and_drop1c,
     post_drag_and_drop2c,
     post_drag_and_drop3c,
     post_drag_and_drop4c,
     post_drag_and_drop1d,
     post_drag_and_drop2d,
     post_drag_and_drop3d,
     post_drag_and_drop4d,
     post_drag_and_drop1e,
     post_drag_and_drop2e,
     post_drag_and_drop3e,
     post_drag_and_drop4e,

						% inserts
     post_insert1a,
     post_insert2a,
     post_insert3a,
     post_insert4a,
     post_insert1b,
     post_insert2b,
     post_insert3b,
     post_insert4b,
     post_insert1c,
     post_insert2c,
     post_insert3c,
     post_insert4c,
     post_insert1d,
     post_insert2d,
     post_insert3d,
     post_insert4d,
     post_insert1e,
     post_insert2e,
     post_insert3e,
     post_insert4e,
     post_insert1f,
     post_insert2f,
     post_insert3f,
     post_insert4f,
     post_insert1g,
     post_insert2g,
     post_insert3g,
     post_insert4g,
     post_insert1h,
     post_insert2h,
     post_insert3h,
     post_insert4h,
     post_insert1i,
     post_insert2i,
     post_insert3i,
     post_insert4i,
     post_insert1j,
     post_insert2j,
     post_insert3j,
     post_insert4j,
     post_insert1k,
     post_insert2k,
     post_insert3k,
     post_insert4k,
     post_insert1l,
     post_insert2l,
     post_insert3l,
     post_insert4l,
     post_insert1m,
     post_insert2m,
     post_insert3m,
     post_insert4m,
     post_insert1n,
     post_insert2n,
     post_insert3n,
     post_insert4n,
     post_insert1o,
     post_insert2o,
     post_insert3o,
     post_insert4o,
     post_insert1p,
     post_insert2p,
     post_insert3p,
     post_insert4p,

						% deletes
     post_delete1a,
     post_delete2a,
     post_delete3a,
     post_delete4a,
     post_delete1b,
     post_delete2b,
     post_delete3b,
     post_delete4b,
     post_delete1c,
     post_delete2c,
     post_delete3c,
     post_delete4c,
     post_delete1d,
     post_delete2d,
     post_delete3d,
     post_delete4d,
     post_delete1e,
     post_delete2e,
     post_delete3e,
     post_delete4e,

						% 3 kinds of copy
     post_copy1a,
     post_copy2a,
     post_copy3a,
     post_copy4a,
     post_copy1b,
     post_copy2b,
     post_copy3b,
     post_copy4b,
     post_copy1c,
     post_copy2c,
     post_copy3c,
     post_copy4c,
     post_copy1d,
     post_copy2d,
     post_copy3d,
     post_copy4d,
     post_copy1e,
     post_copy2e,
     post_copy3e,
     post_copy4e,
     post_copy1f,
     post_copy2f,
     post_copy3f,
     post_copy4f,
     post_copy1g,
     post_copy2g,
     post_copy3g,
     post_copy4g,
     post_copy1h,
     post_copy2h,
     post_copy3h,
     post_copy4h,
     post_copy1i,
     post_copy2i,
     post_copy3i,
     post_copy4i,
     post_copy1j,
     post_copy2j,
     post_copy3j,
     post_copy4j,
     post_copy1k,
     post_copy2k,
     post_copy3k,
     post_copy4k,
     post_copy1l,
     post_copy2l,
     post_copy3l,
     post_copy4l,
     post_copy1m,
     post_copy2m,
     post_copy3m,
     post_copy4m,
     post_copy1n,
     post_copy2n,
     post_copy3n,
     post_copy4n,
     post_copy1o,
     post_copy2o,
     post_copy3o,
     post_copy4o,
     post_copy1p,
     post_copy2p,
     post_copy3p,
     post_copy4p,
     post_copy1r,
     post_copy2r,
     post_copy3r,
     post_copy4r,
     post_copy1s,
     post_copy2s,
     post_copy3s,
     post_copy4s,
     post_copy1t,
     post_copy2t,
     post_copy3t,
     post_copy4t,

     post_copystyle1a,
     post_copystyle2a,
     post_copystyle3a,
     post_copystyle4a,
     post_copystyle1b,
     post_copystyle2b,
     post_copystyle3b,
     post_copystyle4b,
     post_copystyle1c,
     post_copystyle2c,
     post_copystyle3c,
     post_copystyle4c,
     post_copystyle1d,
     post_copystyle2d,
     post_copystyle3d,
     post_copystyle4d,
     post_copystyle1e,
     post_copystyle2e,
     post_copystyle3e,
     post_copystyle4e,
     post_copystyle1f,
     post_copystyle2f,
     post_copystyle3f,
     post_copystyle4f,
     post_copystyle1g,
     post_copystyle2g,
     post_copystyle3g,
     post_copystyle4g,
     post_copystyle1h,
     post_copystyle2h,
     post_copystyle3h,
     post_copystyle4h,
     post_copystyle1i,
     post_copystyle2i,
     post_copystyle3i,
     post_copystyle4i,
     post_copystyle1j,
     post_copystyle2j,
     post_copystyle3j,
     post_copystyle4j,
     post_copystyle1k,
     post_copystyle2k,
     post_copystyle3k,
     post_copystyle4k,
     post_copystyle1l,
     post_copystyle2l,
     post_copystyle3l,
     post_copystyle4l,
     post_copystyle1m,
     post_copystyle2m,
     post_copystyle3m,
     post_copystyle4m,
     post_copystyle1n,
     post_copystyle2n,
     post_copystyle3n,
     post_copystyle4n,
     post_copystyle1o,
     post_copystyle2o,
     post_copystyle3o,
     post_copystyle4o,
     post_copystyle1p,
     post_copystyle2p,
     post_copystyle3p,
     post_copystyle4p,
     post_copystyle1r,
     post_copystyle2r,
     post_copystyle3r,
     post_copystyle4r,
     post_copystyle1s,
     post_copystyle2s,
     post_copystyle3s,
     post_copystyle4s,
     post_copystyle1t,
     post_copystyle2t,
     post_copystyle3t,
     post_copystyle4t,

     post_copyvalue1a,
     post_copyvalue2a,
     post_copyvalue3a,
     post_copyvalue4a,
     post_copyvalue1b,
     post_copyvalue2b,
     post_copyvalue3b,
     post_copyvalue4b,
     post_copyvalue1c,
     post_copyvalue2c,
     post_copyvalue3c,
     post_copyvalue4c,
     post_copyvalue1d,
     post_copyvalue2d,
     post_copyvalue3d,
     post_copyvalue4d,
     post_copyvalue1e,
     post_copyvalue2e,
     post_copyvalue3e,
     post_copyvalue4e,
     post_copyvalue1f,
     post_copyvalue2f,
     post_copyvalue3f,
     post_copyvalue4f,
     post_copyvalue1g,
     post_copyvalue2g,
     post_copyvalue3g,
     post_copyvalue4g,
     post_copyvalue1h,
     post_copyvalue2h,
     post_copyvalue3h,
     post_copyvalue4h,
     post_copyvalue1i,
     post_copyvalue2i,
     post_copyvalue3i,
     post_copyvalue4i,
     post_copyvalue1j,
     post_copyvalue2j,
     post_copyvalue3j,
     post_copyvalue4j,
     post_copyvalue1k,
     post_copyvalue2k,
     post_copyvalue3k,
     post_copyvalue4k,
     post_copyvalue1l,
     post_copyvalue2l,
     post_copyvalue3l,
     post_copyvalue4l,
     post_copyvalue1m,
     post_copyvalue2m,
     post_copyvalue3m,
     post_copyvalue4m,
     post_copyvalue1n,
     post_copyvalue2n,
     post_copyvalue3n,
     post_copyvalue4n,
     post_copyvalue1o,
     post_copyvalue2o,
     post_copyvalue3o,
     post_copyvalue4o,
     post_copyvalue1p,
     post_copyvalue2p,
     post_copyvalue3p,
     post_copyvalue4p,
     post_copyvalue1r,
     post_copyvalue2r,
     post_copyvalue3r,
     post_copyvalue4r,
     post_copyvalue1s,
     post_copyvalue2s,
     post_copyvalue3s,
     post_copyvalue4s,
     post_copyvalue1t,
     post_copyvalue2t,
     post_copyvalue3t,
     post_copyvalue4t,

						% inline
     post_inline1a,
     post_inline2a,
     post_inline3a,
     post_inline4a,
     post_inline1b,
     post_inline2b,
     post_inline3b,
     post_inline4b,
     post_inline1c,
     post_inline2c,
     post_inline3c,
     post_inline4c,
     post_inline1d,
     post_inline2d,
     post_inline3d,
     post_inline4d,
     post_inline1e,
     post_inline2e,
     post_inline3e,
     post_inline4e,

						% inline
     post_inlineclear1a,
     post_inlineclear2a,
     post_inlineclear3a,
     post_inlineclear4a,
     post_inlineclear1b,
     post_inlineclear2b,
     post_inlineclear3b,
     post_inlineclear4b,
     post_inlineclear1c,
     post_inlineclear2c,
     post_inlineclear3c,
     post_inlineclear4c,
     post_inlineclear1d,
     post_inlineclear2d,
     post_inlineclear3d,
     post_inlineclear4d,
     post_inlineclear1e,
     post_inlineclear2e,
     post_inlineclear3e,
     post_inlineclear4e,

						% forms
     post_form1a,
     post_form2a,
     post_form3a,
     post_form4a,
     post_form1b,
     post_form2b,
     post_form3b,
     post_form4b,
     post_form1c,
     post_form2c,
     post_form3c,
     post_form4c,
     post_form1d,
     post_form2d,
     post_form3d,
     post_form4d,
     post_form1e,
     post_form2e,
     post_form3e,
     post_form4e,

						% webcontrols
     post_webcontrol1a,
     post_webcontrol2a,
     post_webcontrol3a,
     post_webcontrol4a,
     post_webcontrol1b,
     post_webcontrol2b,
     post_webcontrol3b,
     post_webcontrol4b,
     post_webcontrol1c,
     post_webcontrol2c,
     post_webcontrol3c,
     post_webcontrol4c,
     post_webcontrol1d,
     post_webcontrol2d,
     post_webcontrol3d,
     post_webcontrol4d,
     post_webcontrol1e,
     post_webcontrol2e,
     post_webcontrol3e,
     post_webcontrol4e,

						% reverts
     post_revert1a,
     post_revert2a,
     post_revert3a,
     post_revert4a,
     post_revert1b,
     post_revert2b,
     post_revert3b,
     post_revert4b,
     post_revert1c,
     post_revert2c,
     post_revert3c,
     post_revert4c,
     post_revert1d,
     post_revert2d,
     post_revert3d,
     post_revert4d,
     post_revert1e,
     post_revert2e,
     post_revert3e,
     post_revert4e,

     post_default_view1a,
     post_default_view2a,
     post_default_view3a,
     post_default_view4a,
     post_default_view1b,
     post_default_view2b,
     post_default_view3b,
     post_default_view4b,
     post_default_view1c,
     post_default_view2c,
     post_default_view3c,
     post_default_view4c,
     post_default_view1d,
     post_default_view2d,
     post_default_view3d,
     post_default_view4d,
     post_default_view1e,
     post_default_view2e,
     post_default_view3e,
     post_default_view4e,

     post_set_view1a,
     post_set_view2a,
     post_set_view3a,
     post_set_view4a,
     post_set_view1b,
     post_set_view2b,
     post_set_view3b,
     post_set_view4b,
     post_set_view1c,
     post_set_view2c,
     post_set_view3c,
     post_set_view4c,
     post_set_view1d,
     post_set_view2d,
     post_set_view3d,
     post_set_view4d,
     post_set_view1e,
     post_set_view2e,
     post_set_view3e,
     post_set_view4e,

     post_save_template1a,
     post_save_template2a,
     post_save_template3a,
     post_save_template4a,
     post_save_template1b,
     post_save_template2b,
     post_save_template3b,
     post_save_template4b,
     post_save_template1c,
     post_save_template2c,
     post_save_template3c,
     post_save_template4c,
     post_save_template1d,
     post_save_template2d,
     post_save_template3d,
     post_save_template4d,
     post_save_template1e,
     post_save_template2e,
     post_save_template3e,
     post_save_template4e,

     post_add_group1a,
     post_add_group2a,
     post_add_group3a,
     post_add_group4a,
     post_add_group1b,
     post_add_group2b,
     post_add_group3b,
     post_add_group4b,
     post_add_group1c,
     post_add_group2c,
     post_add_group3c,
     post_add_group4c,
     post_add_group1d,
     post_add_group2d,
     post_add_group3d,
     post_add_group4d,
     post_add_group1e,
     post_add_group2e,
     post_add_group3e,
     post_add_group4e,

     post_add_user1a,
     post_add_user2a,
     post_add_user3a,
     post_add_user4a,
     post_add_user1b,
     post_add_user2b,
     post_add_user3b,
     post_add_user4b,
     post_add_user1c,
     post_add_user2c,
     post_add_user3c,
     post_add_user4c,
     post_add_user1d,
     post_add_user2d,
     post_add_user3d,
     post_add_user4d,
     post_add_user1e,
     post_add_user2e,
     post_add_user3e,
     post_add_user4e,

     post_invite_user1a,
     post_invite_user2a,
     post_invite_user3a,
     post_invite_user4a,
     post_invite_user1b,
     post_invite_user2b,
     post_invite_user3b,
     post_invite_user4b,
     post_invite_user1c,
     post_invite_user2c,
     post_invite_user3c,
     post_invite_user4c,
     post_invite_user1d,
     post_invite_user2d,
     post_invite_user3d,
     post_invite_user4d,
     post_invite_user1e,
     post_invite_user2e,
     post_invite_user3e,
     post_invite_user4e,

     post_append1a,
     post_append2a,
     post_append3a,
     post_append4a,
     post_append1b,
     post_append2b,
     post_append3b,
     post_append4b,
     post_append1c,
     post_append2c,
     post_append3c,
     post_append4c,
     post_append1d,
     post_append2d,
     post_append3d,
     post_append4d,
     post_append1e,
     post_append2e,
     post_append3e,
     post_append4e

    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
-define(FORMULA, "{\"set\":{\"formula\":\"3333\"}}").
?tapi(post_formula1a, "/some/page/a1", ?FORMULA, 200, api_subdirs).
?tapi(post_formula1b, "/some/page/a1", ?FORMULA, 401, api).
?tapi(post_formula1c, "/some/page/a1", ?FORMULA, 200, api_admin).
?tapi(post_formula1d, "/some/page/a1", ?FORMULA, 401, api_appendonly).

?tapi(post_formula2a, "/some/page/a1:b5", ?FORMULA, 200, api_subdirs).
?tapi(post_formula2d, "/some/page/a1:b5", ?FORMULA, 401, api).
?tapi(post_formula2c, "/some/page/a1:b5", ?FORMULA, 200, api_admin).
?tapi(post_formula2b, "/some/page/a1:b5", ?FORMULA, 401, api_appendonly).

?tapi(post_formula3a, "/some/page/a:b", ?FORMULA, 401, api_subdirs).
?tapi(post_formula3b, "/some/page/a:b", ?FORMULA, 401, api).
?tapi(post_formula3c, "/some/page/a:b", ?FORMULA, 401, api_admin).
?tapi(post_formula3d, "/some/page/a:b", ?FORMULA, 401, api_appendonly).

?tapi(post_formula4a, "/some/page/1:2", ?FORMULA, 401, api_subdirs).
?tapi(post_formula4b, "/some/page/1:2", ?FORMULA, 401, api).
?tapi(post_formula4c, "/some/page/1:2", ?FORMULA, 401, api_admin).
?tapi(post_formula4d, "/some/page/1:2", ?FORMULA, 401, api_appendonly).

?tapi(post_formula5a, "/some/page/", ?FORMULA, 401, api_subdirs).
?tapi(post_formula5b, "/some/page/", ?FORMULA, 401, api).
?tapi(post_formula5c, "/some/page/", ?FORMULA, 401, api_admin).
?tapi(post_formula5d, "/some/page/", ?FORMULA, 401, api_appendonly).

-define(RANGE, "{\"set\":{\"formula\":[[\"A1\", \"B1\"],[\"A2\", \"B2\"],[\"A3\", \"B3\"]]}}").
?tapi(post_range1a, "/a/different/page/a1", ?RANGE, 401, api_subdirs).
?tapi(post_range1b, "/a/different/page/a1", ?RANGE, 401, api).
?tapi(post_range1c, "/a/different/page/a1", ?RANGE, 401, api_admin).
?tapi(post_range1d, "/a/different/page/a1", ?RANGE, 401, api_appendonly).

?tapi(post_range2a, "/a/different/page/a1:b3", ?RANGE, 200, api_subdirs).
?tapi(post_range2b, "/a/different/page/a1:b3", ?RANGE, 401, api).
?tapi(post_range2c, "/a/different/page/a1:b3", ?RANGE, 200, api_admin).
?tapi(post_range2d, "/a/different/page/a1:b3", ?RANGE, 401, api_appendonly).

?tapi(post_range2e, "/a/different/page/c7:e8", ?RANGE, 401, api_subdirs).
?tapi(post_range2f, "/a/different/page/c7:e8", ?RANGE, 401, api).
?tapi(post_range2g, "/a/different/page/c7:e8", ?RANGE, 401, api_admin).
?tapi(post_range2h, "/a/different/page/c7:e8", ?RANGE, 401, api_appendonly).

?tapi(post_range3a, "/a/different/page/a:b", ?RANGE, 401, api_subdirs).
?tapi(post_range3b, "/a/different/page/a:b", ?RANGE, 401, api).
?tapi(post_range3c, "/a/different/page/a:b", ?RANGE, 401, api_admin).
?tapi(post_range3d, "/a/different/page/a:b", ?RANGE, 401, api_appendonly).

?tapi(post_range4a, "/a/different/page/1:2", ?RANGE, 401, api_subdirs).
?tapi(post_range4b, "/a/different/page/1:2", ?RANGE, 401, api).
?tapi(post_range4c, "/a/different/page/1:2", ?RANGE, 401, api_admin).
?tapi(post_range4d, "/a/different/page/1:2", ?RANGE, 401, api_appendonly).

?tapi(post_range5a, "/a/different/page/", ?RANGE, 401, api_subdirs).
?tapi(post_range5b, "/a/different/page/", ?RANGE, 401, api).
?tapi(post_range5c, "/a/different/page/", ?RANGE, 401, api_admin).
?tapi(post_range5d, "/a/different/page/", ?RANGE, 401, api_appendonly).

						% mark
?tapi(post_mark1a, "/some/page/a1", "{\"mark\": \"leper face\"}", 401, api_subdirs).
?tapi(post_mark2a, "/some/page/a1", "{\"mark\": \"leper face\"}", 401, api).
?tapi(post_mark3a, "/some/page/a1", "{\"mark\": \"leper face\"}", 401, api_admin).
?tapi(post_mark4a, "/some/page/a1", "{\"mark\": \"leper face\"}", 401, api_appendonly).

?tapi(post_mark1b, "/some/page/a1:b5", "{\"mark\": \"leper face\"}", 401, api_subdirs).
?tapi(post_mark2b, "/some/page/a1:b5", "{\"mark\": \"leper face\"}", 401, api).
?tapi(post_mark3b, "/some/page/a1:b5", "{\"mark\": \"leper face\"}", 401, api_admin).
?tapi(post_mark4b, "/some/page/a1:b5", "{\"mark\": \"leper face\"}", 401, api_appendonly).

?tapi(post_mark1c, "/some/page/c:d", "{\"mark\": \"leper face\"}", 401, api_subdirs).
?tapi(post_mark2c, "/some/page/c:d", "{\"mark\": \"leper face\"}", 401, api).
?tapi(post_mark3c, "/some/page/c:d", "{\"mark\": \"leper face\"}", 401, api_admin).
?tapi(post_mark4c, "/some/page/c:d", "{\"mark\": \"leper face\"}", 401, api_appendonly).

?tapi(post_mark1d, "/some/page/10:11", "{\"mark\": \"leper face\"}", 401, api_subdirs).
?tapi(post_mark2d, "/some/page/10:11", "{\"mark\": \"leper face\"}", 401, api).
?tapi(post_mark3d, "/some/page/10:11", "{\"mark\": \"leper face\"}", 401, api_admin).
?tapi(post_mark4d, "/some/page/10:11", "{\"mark\": \"leper face\"}", 401, api_appendonly).

?tapi(post_mark1e, "/some/page/", "{\"mark\": \"leper face\"}", 401, api_subdirs).
?tapi(post_mark2e, "/some/page/", "{\"mark\": \"leper face\"}", 401, api).
?tapi(post_mark3e, "/some/page/", "{\"mark\": \"leper face\"}", 401, api_admin).
?tapi(post_mark4e, "/some/page/", "{\"mark\": \"leper face\"}", 401, api_appendonly).

-define(JSERR, "{
        \"jserr\": {
            \"msg\": \"Msg\",
            \"cookies\": \"cookie\",
            \"pageurl\": \"http://example.com\",
            \"lineno\": \"1234\",
            \"errorfileurl\": \"http://example.com/javascript.js\"
        }
}").
% jserror
?tapi(post_jserr1a, "/some/page/a1", ?JSERR, 401, api_subdirs).
?tapi(post_jserr2a, "/some/page/a1", ?JSERR, 401, api).
?tapi(post_jserr3a, "/some/page/a1", ?JSERR, 401, api_admin).
?tapi(post_jserr4a, "/some/page/a1", ?JSERR, 401, api_appendonly).

?tapi(post_jserr1b, "/some/page/a1:b2", ?JSERR, 401, api_subdirs).
?tapi(post_jserr2b, "/some/page/a1:b2", ?JSERR, 401, api).
?tapi(post_jserr3b, "/some/page/a1:b2", ?JSERR, 401, api_admin).
?tapi(post_jserr4b, "/some/page/a1:b2", ?JSERR, 401, api_appendonly).

?tapi(post_jserr1c, "/some/page/c:d", ?JSERR, 401, api_subdirs).
?tapi(post_jserr2c, "/some/page/c:d", ?JSERR, 401, api).
?tapi(post_jserr3c, "/some/page/c:d", ?JSERR, 401, api_admin).
?tapi(post_jserr4c, "/some/page/c:d", ?JSERR, 401, api_appendonly).

?tapi(post_jserr1d, "/some/page/10:11", ?JSERR, 401, api_subdirs).
?tapi(post_jserr2d, "/some/page/10:11", ?JSERR, 401, api).
?tapi(post_jserr3d, "/some/page/10:11", ?JSERR, 401, api_admin).
?tapi(post_jserr4d, "/some/page/10:11", ?JSERR, 401, api_appendonly).

?tapi(post_jserr1e, "/some/page/", ?JSERR, 401, api_subdirs).
?tapi(post_jserr2e, "/some/page/", ?JSERR, 401, api).
?tapi(post_jserr3e, "/some/page/", ?JSERR, 401, api_admin).
?tapi(post_jserr4e, "/some/page/", ?JSERR, 401, api_appendonly).

% load template
-define(LOADTEMPLATE, "{\"load_template\":{\"name\":\"blank\"}}").
?tapi(post_loadtemplate1a, "/some/page/a1", ?LOADTEMPLATE, 401, api_subdirs).
?tapi(post_loadtemplate2a, "/some/page/a1", ?LOADTEMPLATE, 401, api).
?tapi(post_loadtemplate3a, "/some/page/a1", ?LOADTEMPLATE, 401, api_admin).
?tapi(post_loadtemplate4a, "/some/page/a1", ?LOADTEMPLATE, 401, api_appendonly).

?tapi(post_loadtemplate1b, "/some/page/a1:b5", ?LOADTEMPLATE, 401, api_subdirs).
?tapi(post_loadtemplate2b, "/some/page/a1:b5", ?LOADTEMPLATE, 401, api).
?tapi(post_loadtemplate3b, "/some/page/a1:b5", ?LOADTEMPLATE, 401, api_admin).
?tapi(post_loadtemplate4b, "/some/page/a1:b5", ?LOADTEMPLATE, 401, api_appendonly).

?tapi(post_loadtemplate1c, "/some/page/c:d", ?LOADTEMPLATE, 401, api_subdirs).
?tapi(post_loadtemplate2c, "/some/page/c:d", ?LOADTEMPLATE, 401, api).
?tapi(post_loadtemplate3c, "/some/page/c:d", ?LOADTEMPLATE, 401, api_admin).
?tapi(post_loadtemplate4c, "/some/page/c:d", ?LOADTEMPLATE, 401, api_appendonly).

?tapi(post_loadtemplate1d, "/some/page/10:11", ?LOADTEMPLATE, 401, api_subdirs).
?tapi(post_loadtemplate2d, "/some/page/10:11", ?LOADTEMPLATE, 401, api).
?tapi(post_loadtemplate3d, "/some/page/10:11", ?LOADTEMPLATE, 401, api_admin).
?tapi(post_loadtemplate4d, "/some/page/10:11", ?LOADTEMPLATE, 401, api_appendonly).

?tapi(post_loadtemplate1e, "/some/page/", ?LOADTEMPLATE, 200, api_subdirs).
?tapi(post_loadtemplate2e, "/some/page/", ?LOADTEMPLATE, 401, api).
?tapi(post_loadtemplate3e, "/some/page/", ?LOADTEMPLATE, 200, api_admin).
?tapi(post_loadtemplate4e, "/some/page/", ?LOADTEMPLATE, 401, api_appendonly).

% drag and drop
-define(DRAG, "{\"drag\":{\"range\":\"A6:A6\"}}").
?tapi(post_drag_and_drop1a, "/some/page/a5", ?DRAG, 200, api_subdirs).
?tapi(post_drag_and_drop2a, "/some/page/a5", ?DRAG, 401, api).
?tapi(post_drag_and_drop3a, "/some/page/a5", ?DRAG, 200, api_admin).
?tapi(post_drag_and_drop4a, "/some/page/a5", ?DRAG, 401, api_appendonly).

?tapi(post_drag_and_drop1b, "/some/page/a5:b5", ?DRAG, 401, api_subdirs).
?tapi(post_drag_and_drop2b, "/some/page/a5:b5", ?DRAG, 401, api).
?tapi(post_drag_and_drop3b, "/some/page/a5:b5", ?DRAG, 401, api_admin).
?tapi(post_drag_and_drop4b, "/some/page/a5:b5", ?DRAG, 401, api_appendonly).

?tapi(post_drag_and_drop1c, "/some/page/c:d", ?DRAG, 401, api_subdirs).
?tapi(post_drag_and_drop2c, "/some/page/c:d", ?DRAG, 401, api).
?tapi(post_drag_and_drop3c, "/some/page/c:d", ?DRAG, 401, api_admin).
?tapi(post_drag_and_drop4c, "/some/page/c:d", ?DRAG, 401, api_appendonly).

?tapi(post_drag_and_drop1d, "/some/page/10:11", ?DRAG, 401, api_subdirs).
?tapi(post_drag_and_drop2d, "/some/page/10:11", ?DRAG, 401, api).
?tapi(post_drag_and_drop3d, "/some/page/10:11", ?DRAG, 401, api_admin).
?tapi(post_drag_and_drop4d, "/some/page/10:11", ?DRAG, 401, api_appendonly).

?tapi(post_drag_and_drop1e, "/some/page/", ?DRAG, 401, api_subdirs).
?tapi(post_drag_and_drop2e, "/some/page/", ?DRAG, 401, api).
?tapi(post_drag_and_drop3e, "/some/page/", ?DRAG, 401, api_admin).
?tapi(post_drag_and_drop4e, "/some/page/", ?DRAG, 401, api_appendonly).

% insert stuff
-define(INSERTa, "{\"insert\":\"after\"}").
-define(INSERTb, "{\"insert\":\"before\"}").
-define(INSERTc, "{\"insert\":\"horizontal\"}").
-define(INSERTd, "{\"insert\":\"vertical\"}").

% cells
?tapi(post_insert1a, "/some/page/a5", ?INSERTa, 200, api_subdirs).
?tapi(post_insert2a, "/some/page/a5", ?INSERTa, 401, api).
?tapi(post_insert3a, "/some/page/a5", ?INSERTa, 200, api_admin).
?tapi(post_insert4a, "/some/page/a5", ?INSERTa, 401, api_appendonly).

?tapi(post_insert1b, "/some/page/a5", ?INSERTb, 200, api_subdirs).
?tapi(post_insert2b, "/some/page/a5", ?INSERTb, 401, api).
?tapi(post_insert3b, "/some/page/a5", ?INSERTb, 200, api_admin).
?tapi(post_insert4b, "/some/page/a5", ?INSERTb, 401, api_appendonly).

?tapi(post_insert1c, "/some/page/a5", ?INSERTc, 200, api_subdirs).
?tapi(post_insert2c, "/some/page/a5", ?INSERTc, 401, api).
?tapi(post_insert3c, "/some/page/a5", ?INSERTc, 200, api_admin).
?tapi(post_insert4c, "/some/page/a5", ?INSERTc, 401, api_appendonly).

?tapi(post_insert1d, "/some/page/a5", ?INSERTd, 200, api_subdirs).
?tapi(post_insert2d, "/some/page/a5", ?INSERTd, 401, api).
?tapi(post_insert3d, "/some/page/a5", ?INSERTd, 200, api_admin).
?tapi(post_insert4d, "/some/page/a5", ?INSERTd, 401, api_appendonly).

% ranges
?tapi(post_insert1e, "/some/page/a5:b6", ?INSERTa, 200, api_subdirs).
?tapi(post_insert2e, "/some/page/a5:b6", ?INSERTa, 401, api).
?tapi(post_insert3e, "/some/page/a5:b6", ?INSERTa, 200, api_admin).
?tapi(post_insert4e, "/some/page/a5:b6", ?INSERTa, 401, api_appendonly).

?tapi(post_insert1f, "/some/page/a5:b6", ?INSERTb, 200, api_subdirs).
?tapi(post_insert2f, "/some/page/a5:b6", ?INSERTb, 401, api).
?tapi(post_insert3f, "/some/page/a5:b6", ?INSERTb, 200, api_admin).
?tapi(post_insert4f, "/some/page/a5:b6", ?INSERTb, 401, api_appendonly).

?tapi(post_insert1g, "/some/page/a5:b6", ?INSERTc, 200, api_subdirs).
?tapi(post_insert2g, "/some/page/a5:b6", ?INSERTc, 401, api).
?tapi(post_insert3g, "/some/page/a5:b6", ?INSERTc, 200, api_admin).
?tapi(post_insert4g, "/some/page/a5:b6", ?INSERTc, 401, api_appendonly).

?tapi(post_insert1h, "/some/page/a5:b6", ?INSERTd, 200, api_subdirs).
?tapi(post_insert2h, "/some/page/a5:b6", ?INSERTd, 401, api).
?tapi(post_insert3h, "/some/page/a5:b6", ?INSERTd, 200, api_admin).
?tapi(post_insert4h, "/some/page/a5:b6", ?INSERTd, 401, api_appendonly).

% columns
?tapi(post_insert1i, "/some/page/b:e", ?INSERTa, 200, api_subdirs).
?tapi(post_insert2i, "/some/page/b:e", ?INSERTa, 401, api).
?tapi(post_insert3i, "/some/page/b:e", ?INSERTa, 200, api_admin).
?tapi(post_insert4i, "/some/page/b:e", ?INSERTa, 401, api_appendonly).

?tapi(post_insert1j, "/some/page/b:e", ?INSERTb, 200, api_subdirs).
?tapi(post_insert2j, "/some/page/b:e", ?INSERTb, 401, api).
?tapi(post_insert3j, "/some/page/b:e", ?INSERTb, 200, api_admin).
?tapi(post_insert4j, "/some/page/b:e", ?INSERTb, 401, api_appendonly).

?tapi(post_insert1k, "/some/page/b:e", ?INSERTc, 401, api_subdirs).
?tapi(post_insert2k, "/some/page/b:e", ?INSERTc, 401, api).
?tapi(post_insert3k, "/some/page/b:e", ?INSERTc, 401, api_admin).
?tapi(post_insert4k, "/some/page/b:e", ?INSERTc, 401, api_appendonly).

?tapi(post_insert1l, "/some/page/b:e", ?INSERTd, 401, api_subdirs).
?tapi(post_insert2l, "/some/page/b:e", ?INSERTd, 401, api).
?tapi(post_insert3l, "/some/page/b:e", ?INSERTd, 401, api_admin).
?tapi(post_insert4l, "/some/page/b:e", ?INSERTd, 401, api_appendonly).

% rows
?tapi(post_insert1m, "/some/page/6:8", ?INSERTa, 200, api_subdirs).
?tapi(post_insert2m, "/some/page/6:8", ?INSERTa, 401, api).
?tapi(post_insert3m, "/some/page/6:8", ?INSERTa, 200, api_admin).
?tapi(post_insert4m, "/some/page/6:8", ?INSERTa, 401, api_appendonly).

?tapi(post_insert1n, "/some/page/6:8", ?INSERTb, 200, api_subdirs).
?tapi(post_insert2n, "/some/page/6:8", ?INSERTb, 401, api).
?tapi(post_insert3n, "/some/page/6:8", ?INSERTb, 200, api_admin).
?tapi(post_insert4n, "/some/page/6:8", ?INSERTb, 401, api_appendonly).

?tapi(post_insert1o, "/some/page/6:8", ?INSERTc, 401, api_subdirs).
?tapi(post_insert2o, "/some/page/6:8", ?INSERTc, 401, api).
?tapi(post_insert3o, "/some/page/6:8", ?INSERTc, 401, api_admin).
?tapi(post_insert4o, "/some/page/6:8", ?INSERTc, 401, api_appendonly).

?tapi(post_insert1p, "/some/page/6:8", ?INSERTd, 401, api_subdirs).
?tapi(post_insert2p, "/some/page/6:8", ?INSERTd, 401, api).
?tapi(post_insert3p, "/some/page/6:8", ?INSERTd, 401, api_admin).
?tapi(post_insert4p, "/some/page/6:8", ?INSERTd, 401, api_appendonly).

% try page inserts
?tapi(post_insert1q, "/some/page/", ?INSERTd, 401, api_subdirs).
?tapi(post_insert2q, "/some/page/", ?INSERTd, 401, api).
?tapi(post_insert3q, "/some/page/", ?INSERTd, 401, api_admin).
?tapi(post_insert4q, "/some/page/", ?INSERTd, 401, api_appendonly).

% delete stuff
-define(DELETEa, "{\"delete\":\"all\"}").
-define(DELETEb, "{\"delete\":\"horizontal\"}").
-define(DELETEc, "{\"delete\":\"vertical\"}").

% cells
?tapi(post_delete1a, "/some/page/a5", ?DELETEa, 200, api_subdirs).
?tapi(post_delete2a, "/some/page/a5", ?DELETEa, 401, api).
?tapi(post_delete3a, "/some/page/a5", ?DELETEa, 200, api_admin).
?tapi(post_delete4a, "/some/page/a5", ?DELETEa, 401, api_appendonly).

?tapi(post_delete1b, "/some/page/a5", ?DELETEb, 200, api_subdirs).
?tapi(post_delete2b, "/some/page/a5", ?DELETEb, 401, api).
?tapi(post_delete3b, "/some/page/a5", ?DELETEb, 200, api_admin).
?tapi(post_delete4b, "/some/page/a5", ?DELETEb, 401, api_appendonly).

?tapi(post_delete1c, "/some/page/a5", ?DELETEc, 200, api_subdirs).
?tapi(post_delete2c, "/some/page/a5", ?DELETEc, 401, api).
?tapi(post_delete3c, "/some/page/a5", ?DELETEc, 200, api_admin).
?tapi(post_delete4c, "/some/page/a5", ?DELETEc, 401, api_appendonly).

% ranges
?tapi(post_delete1d, "/some/page/a5:b6", ?DELETEa, 200, api_subdirs).
?tapi(post_delete2d, "/some/page/a5:b6", ?DELETEa, 401, api).
?tapi(post_delete3d, "/some/page/a5:b6", ?DELETEa, 200, api_admin).
?tapi(post_delete4d, "/some/page/a5:b6", ?DELETEa, 401, api_appendonly).

?tapi(post_delete1e, "/some/page/a5:b6", ?DELETEb, 200, api_subdirs).
?tapi(post_delete2e, "/some/page/a5:b6", ?DELETEb, 401, api).
?tapi(post_delete3e, "/some/page/a5:b6", ?DELETEb, 200, api_admin).
?tapi(post_delete4e, "/some/page/a5:b6", ?DELETEb, 401, api_appendonly).

?tapi(post_delete1f, "/some/page/a5:b6", ?DELETEc, 200, api_subdirs).
?tapi(post_delete2f, "/some/page/a5:b6", ?DELETEc, 401, api).
?tapi(post_delete3f, "/some/page/a5:b6", ?DELETEc, 200, api_admin).
?tapi(post_delete4f, "/some/page/a5:b6", ?DELETEc, 401, api_appendonly).

% columns
?tapi(post_delete1g, "/some/page/b:e", ?DELETEa, 200, api_subdirs).
?tapi(post_delete2g, "/some/page/b:e", ?DELETEa, 401, api).
?tapi(post_delete3g, "/some/page/b:e", ?DELETEa, 200, api_admin).
?tapi(post_delete4g, "/some/page/b:e", ?DELETEa, 401, api_appendonly).

?tapi(post_delete1h, "/some/page/b:e", ?DELETEb, 401, api_subdirs).
?tapi(post_delete2h, "/some/page/b:e", ?DELETEb, 401, api).
?tapi(post_delete3h, "/some/page/b:e", ?DELETEb, 401, api_admin).
?tapi(post_delete4h, "/some/page/b:e", ?DELETEb, 401, api_appendonly).

?tapi(post_delete1i, "/some/page/b:e", ?DELETEc, 401, api_subdirs).
?tapi(post_delete2i, "/some/page/b:e", ?DELETEc, 401, api).
?tapi(post_delete3i, "/some/page/b:e", ?DELETEc, 401, api_admin).
?tapi(post_delete4i, "/some/page/b:e", ?DELETEc, 401, api_appendonly).

% rows
?tapi(post_delete1m, "/some/page/6:8", ?DELETEa, 200, api_subdirs).
?tapi(post_delete2m, "/some/page/6:8", ?DELETEa, 401, api).
?tapi(post_delete3m, "/some/page/6:8", ?DELETEa, 200, api_admin).
?tapi(post_delete4m, "/some/page/6:8", ?DELETEa, 401, api_appendonly).

?tapi(post_delete1n, "/some/page/6:8", ?DELETEb, 401, api_subdirs).
?tapi(post_delete2n, "/some/page/6:8", ?DELETEb, 401, api).
?tapi(post_delete3n, "/some/page/6:8", ?DELETEb, 401, api_admin).
?tapi(post_delete4n, "/some/page/6:8", ?DELETEb, 401, api_appendonly).

?tapi(post_delete1o, "/some/page/6:8", ?DELETEc, 401, api_subdirs).
?tapi(post_delete2o, "/some/page/6:8", ?DELETEc, 401, api).
?tapi(post_delete3o, "/some/page/6:8", ?DELETEc, 401, api_admin).
?tapi(post_delete4o, "/some/page/6:8", ?DELETEc, 401, api_appendonly).

-define(COPYa, "{\"copy\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/A1:A1\"}}").
-define(COPYb, "{\"copy\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/A1:A2\"}}").
-define(COPYc, "{\"copy\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/g:g\"}}").
-define(COPYd, "{\"copy\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/10:10\"}}").
?tapi(post_copy1a, "/some/page/B4", ?COPYa, 200, api_subdirs).
?tapi(post_copy2a, "/some/page/B4", ?COPYa, 401, api).
?tapi(post_copy3a, "/some/page/B4", ?COPYa, 200, api_admin).
?tapi(post_copy4a, "/some/page/B4", ?COPYa, 401, api_appendonly).

?tapi(post_copy1b, "/some/page/B4:b5", ?COPYa, 200, api_subdirs).
?tapi(post_copy2b, "/some/page/B4:b5", ?COPYa, 401, api).
?tapi(post_copy3b, "/some/page/B4:b5", ?COPYa, 200, api_admin).
?tapi(post_copy4b, "/some/page/B4:b5", ?COPYa, 401, api_appendonly).

?tapi(post_copy1c, "/some/page/B:c", ?COPYa, 401, api_subdirs).
?tapi(post_copy2c, "/some/page/B:c", ?COPYa, 401, api).
?tapi(post_copy3c, "/some/page/B:c", ?COPYa, 401, api_admin).
?tapi(post_copy4c, "/some/page/B:c", ?COPYa, 401, api_appendonly).

?tapi(post_copy1d, "/some/page/3:4", ?COPYa, 401, api_subdirs).
?tapi(post_copy2d, "/some/page/3:4", ?COPYa, 401, api).
?tapi(post_copy3d, "/some/page/3:4", ?COPYa, 401, api_admin).
?tapi(post_copy4d, "/some/page/3:4", ?COPYa, 401, api_appendonly).

?tapi(post_copy1e, "/some/page/", ?COPYa, 401, api_subdirs).
?tapi(post_copy2e, "/some/page/", ?COPYa, 401, api).
?tapi(post_copy3e, "/some/page/", ?COPYa, 401, api_admin).
?tapi(post_copy4e, "/some/page/", ?COPYa, 401, api_appendonly).

?tapi(post_copy1f, "/some/page/B4", ?COPYb, 200, api_subdirs).
?tapi(post_copy2f, "/some/page/B4", ?COPYb, 401, api).
?tapi(post_copy3f, "/some/page/B4", ?COPYb, 200, api_admin).
?tapi(post_copy4f, "/some/page/B4", ?COPYb, 401, api_appendonly).

?tapi(post_copy1g, "/some/page/B4:b5", ?COPYb, 200, api_subdirs).
?tapi(post_copy2g, "/some/page/B4:b5", ?COPYb, 401, api).
?tapi(post_copy3g, "/some/page/B4:b5", ?COPYb, 200, api_admin).
?tapi(post_copy4g, "/some/page/B4:b5", ?COPYb, 401, api_appendonly).

?tapi(post_copy1h, "/some/page/B:c", ?COPYb, 401, api_subdirs).
?tapi(post_copy2h, "/some/page/B:c", ?COPYb, 401, api).
?tapi(post_copy3h, "/some/page/B:c", ?COPYb, 401, api_admin).
?tapi(post_copy4h, "/some/page/B:c", ?COPYb, 401, api_appendonly).

?tapi(post_copy1i, "/some/page/3:4", ?COPYb, 401, api_subdirs).
?tapi(post_copy2i, "/some/page/3:4", ?COPYb, 401, api).
?tapi(post_copy3i, "/some/page/3:4", ?COPYb, 401, api_admin).
?tapi(post_copy4i, "/some/page/3:4", ?COPYb, 401, api_appendonly).

?tapi(post_copy1j, "/some/page/", ?COPYb, 401, api_subdirs).
?tapi(post_copy2j, "/some/page/", ?COPYb, 401, api).
?tapi(post_copy3j, "/some/page/", ?COPYb, 401, api_admin).
?tapi(post_copy4j, "/some/page/", ?COPYb, 401, api_appendonly).

?tapi(post_copy1k, "/some/page/B4", ?COPYc, 401, api_subdirs).
?tapi(post_copy2k, "/some/page/B4", ?COPYc, 401, api).
?tapi(post_copy3k, "/some/page/B4", ?COPYc, 401, api_admin).
?tapi(post_copy4k, "/some/page/B4", ?COPYc, 401, api_appendonly).

?tapi(post_copy1l, "/some/page/B4:b5", ?COPYc, 401, api_subdirs).
?tapi(post_copy2l, "/some/page/B4:b5", ?COPYc, 401, api).
?tapi(post_copy3l, "/some/page/B4:b5", ?COPYc, 401, api_admin).
?tapi(post_copy4l, "/some/page/B4:b5", ?COPYc, 401, api_appendonly).

?tapi(post_copy1m, "/some/page/B:c", ?COPYc, 401, api_subdirs).
?tapi(post_copy2m, "/some/page/B:c", ?COPYc, 401, api).
?tapi(post_copy3m, "/some/page/B:c", ?COPYc, 401, api_admin).
?tapi(post_copy4m, "/some/page/B:c", ?COPYc, 401, api_appendonly).

?tapi(post_copy1n, "/some/page/3:4", ?COPYc, 401, api_subdirs).
?tapi(post_copy2n, "/some/page/3:4", ?COPYc, 401, api).
?tapi(post_copy3n, "/some/page/3:4", ?COPYc, 401, api_admin).
?tapi(post_copy4n, "/some/page/3:4", ?COPYc, 401, api_appendonly).

?tapi(post_copy1o, "/some/page/", ?COPYc, 401, api_subdirs).
?tapi(post_copy2o, "/some/page/", ?COPYc, 401, api).
?tapi(post_copy3o, "/some/page/", ?COPYc, 401, api_admin).
?tapi(post_copy4o, "/some/page/", ?COPYc, 401, api_appendonly).

?tapi(post_copy1p, "/some/page/B4", ?COPYd, 401, api_subdirs).
?tapi(post_copy2p, "/some/page/B4", ?COPYd, 401, api).
?tapi(post_copy3p, "/some/page/B4", ?COPYd, 401, api_admin).
?tapi(post_copy4p, "/some/page/B4", ?COPYd, 401, api_appendonly).

?tapi(post_copy1q, "/some/page/B4:b5", ?COPYd, 401, api_subdirs).
?tapi(post_copy2q, "/some/page/B4:b5", ?COPYd, 401, api).
?tapi(post_copy3q, "/some/page/B4:b5", ?COPYd, 401, api_admin).
?tapi(post_copy4q, "/some/page/B4:b5", ?COPYd, 401, api_appendonly).

?tapi(post_copy1r, "/some/page/B:c", ?COPYd, 401, api_subdirs).
?tapi(post_copy2r, "/some/page/B:c", ?COPYd, 401, api).
?tapi(post_copy3r, "/some/page/B:c", ?COPYd, 401, api_admin).
?tapi(post_copy4r, "/some/page/B:c", ?COPYd, 401, api_appendonly).

?tapi(post_copy1s, "/some/page/3:4", ?COPYd, 401, api_subdirs).
?tapi(post_copy2s, "/some/page/3:4", ?COPYd, 401, api).
?tapi(post_copy3s, "/some/page/3:4", ?COPYd, 401, api_admin).
?tapi(post_copy4s, "/some/page/3:4", ?COPYd, 401, api_appendonly).

?tapi(post_copy1t, "/some/page/", ?COPYd, 401, api_subdirs).
?tapi(post_copy2t, "/some/page/", ?COPYd, 401, api).
?tapi(post_copy3t, "/some/page/", ?COPYd, 401, api_admin).
?tapi(post_copy4t, "/some/page/", ?COPYd, 401, api_appendonly).

-define(COPYSTYLEa, "{\"copystyle\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/A1:A1\"}}").
-define(COPYSTYLEb, "{\"copystyle\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/A1:A2\"}}").
-define(COPYSTYLEc, "{\"copystyle\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/g:g\"}}").
-define(COPYSTYLEd, "{\"copystyle\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/10:10\"}}").
?tapi(post_copystyle1a, "/some/page/B4", ?COPYSTYLEa, 200, api_subdirs).
?tapi(post_copystyle2a, "/some/page/B4", ?COPYSTYLEa, 401, api).
?tapi(post_copystyle3a, "/some/page/B4", ?COPYSTYLEa, 200, api_admin).
?tapi(post_copystyle4a, "/some/page/B4", ?COPYSTYLEa, 401, api_appendonly).

?tapi(post_copystyle1b, "/some/page/B4:b5", ?COPYSTYLEa, 200, api_subdirs).
?tapi(post_copystyle2b, "/some/page/B4:b5", ?COPYSTYLEa, 401, api).
?tapi(post_copystyle3b, "/some/page/B4:b5", ?COPYSTYLEa, 200, api_admin).
?tapi(post_copystyle4b, "/some/page/B4:b5", ?COPYSTYLEa, 401, api_appendonly).

?tapi(post_copystyle1c, "/some/page/B:c", ?COPYSTYLEa, 401, api_subdirs).
?tapi(post_copystyle2c, "/some/page/B:c", ?COPYSTYLEa, 401, api).
?tapi(post_copystyle3c, "/some/page/B:c", ?COPYSTYLEa, 401, api_admin).
?tapi(post_copystyle4c, "/some/page/B:c", ?COPYSTYLEa, 401, api_appendonly).

?tapi(post_copystyle1d, "/some/page/3:4", ?COPYSTYLEa, 401, api_subdirs).
?tapi(post_copystyle2d, "/some/page/3:4", ?COPYSTYLEa, 401, api).
?tapi(post_copystyle3d, "/some/page/3:4", ?COPYSTYLEa, 401, api_admin).
?tapi(post_copystyle4d, "/some/page/3:4", ?COPYSTYLEa, 401, api_appendonly).

?tapi(post_copystyle1e, "/some/page/", ?COPYSTYLEa, 401, api_subdirs).
?tapi(post_copystyle2e, "/some/page/", ?COPYSTYLEa, 401, api).
?tapi(post_copystyle3e, "/some/page/", ?COPYSTYLEa, 401, api_admin).
?tapi(post_copystyle4e, "/some/page/", ?COPYSTYLEa, 401, api_appendonly).

?tapi(post_copystyle1f, "/some/page/B4", ?COPYSTYLEb, 200, api_subdirs).
?tapi(post_copystyle2f, "/some/page/B4", ?COPYSTYLEb, 401, api).
?tapi(post_copystyle3f, "/some/page/B4", ?COPYSTYLEb, 200, api_admin).
?tapi(post_copystyle4f, "/some/page/B4", ?COPYSTYLEb, 401, api_appendonly).

?tapi(post_copystyle1g, "/some/page/B4:b5", ?COPYSTYLEb, 200, api_subdirs).
?tapi(post_copystyle2g, "/some/page/B4:b5", ?COPYSTYLEb, 401, api).
?tapi(post_copystyle3g, "/some/page/B4:b5", ?COPYSTYLEb, 200, api_admin).
?tapi(post_copystyle4g, "/some/page/B4:b5", ?COPYSTYLEb, 401, api_appendonly).

?tapi(post_copystyle1h, "/some/page/B:c", ?COPYSTYLEb, 401, api_subdirs).
?tapi(post_copystyle2h, "/some/page/B:c", ?COPYSTYLEb, 401, api).
?tapi(post_copystyle3h, "/some/page/B:c", ?COPYSTYLEb, 401, api_admin).
?tapi(post_copystyle4h, "/some/page/B:c", ?COPYSTYLEb, 401, api_appendonly).

?tapi(post_copystyle1i, "/some/page/3:4", ?COPYSTYLEb, 401, api_subdirs).
?tapi(post_copystyle2i, "/some/page/3:4", ?COPYSTYLEb, 401, api).
?tapi(post_copystyle3i, "/some/page/3:4", ?COPYSTYLEb, 401, api_admin).
?tapi(post_copystyle4i, "/some/page/3:4", ?COPYSTYLEb, 401, api_appendonly).

?tapi(post_copystyle1j, "/some/page/", ?COPYSTYLEb, 401, api_subdirs).
?tapi(post_copystyle2j, "/some/page/", ?COPYSTYLEb, 401, api).
?tapi(post_copystyle3j, "/some/page/", ?COPYSTYLEb, 401, api_admin).
?tapi(post_copystyle4j, "/some/page/", ?COPYSTYLEb, 401, api_appendonly).

?tapi(post_copystyle1k, "/some/page/B4", ?COPYSTYLEc, 401, api_subdirs).
?tapi(post_copystyle2k, "/some/page/B4", ?COPYSTYLEc, 401, api).
?tapi(post_copystyle3k, "/some/page/B4", ?COPYSTYLEc, 401, api_admin).
?tapi(post_copystyle4k, "/some/page/B4", ?COPYSTYLEc, 401, api_appendonly).

?tapi(post_copystyle1l, "/some/page/B4:b5", ?COPYSTYLEc, 401, api_subdirs).
?tapi(post_copystyle2l, "/some/page/B4:b5", ?COPYSTYLEc, 401, api).
?tapi(post_copystyle3l, "/some/page/B4:b5", ?COPYSTYLEc, 401, api_admin).
?tapi(post_copystyle4l, "/some/page/B4:b5", ?COPYSTYLEc, 401, api_appendonly).

?tapi(post_copystyle1m, "/some/page/B:c", ?COPYSTYLEc, 401, api_subdirs).
?tapi(post_copystyle2m, "/some/page/B:c", ?COPYSTYLEc, 401, api).
?tapi(post_copystyle3m, "/some/page/B:c", ?COPYSTYLEc, 401, api_admin).
?tapi(post_copystyle4m, "/some/page/B:c", ?COPYSTYLEc, 401, api_appendonly).

?tapi(post_copystyle1n, "/some/page/3:4", ?COPYSTYLEc, 401, api_subdirs).
?tapi(post_copystyle2n, "/some/page/3:4", ?COPYSTYLEc, 401, api).
?tapi(post_copystyle3n, "/some/page/3:4", ?COPYSTYLEc, 401, api_admin).
?tapi(post_copystyle4n, "/some/page/3:4", ?COPYSTYLEc, 401, api_appendonly).

?tapi(post_copystyle1o, "/some/page/", ?COPYSTYLEc, 401, api_subdirs).
?tapi(post_copystyle2o, "/some/page/", ?COPYSTYLEc, 401, api).
?tapi(post_copystyle3o, "/some/page/", ?COPYSTYLEc, 401, api_admin).
?tapi(post_copystyle4o, "/some/page/", ?COPYSTYLEc, 401, api_appendonly).

?tapi(post_copystyle1p, "/some/page/B4", ?COPYSTYLEd, 401, api_subdirs).
?tapi(post_copystyle2p, "/some/page/B4", ?COPYSTYLEd, 401, api).
?tapi(post_copystyle3p, "/some/page/B4", ?COPYSTYLEd, 401, api_admin).
?tapi(post_copystyle4p, "/some/page/B4", ?COPYSTYLEd, 401, api_appendonly).

?tapi(post_copystyle1q, "/some/page/B4:b5", ?COPYSTYLEd, 401, api_subdirs).
?tapi(post_copystyle2q, "/some/page/B4:b5", ?COPYSTYLEd, 401, api).
?tapi(post_copystyle3q, "/some/page/B4:b5", ?COPYSTYLEd, 401, api_admin).
?tapi(post_copystyle4q, "/some/page/B4:b5", ?COPYSTYLEd, 401, api_appendonly).

?tapi(post_copystyle1r, "/some/page/B:c", ?COPYSTYLEd, 401, api_subdirs).
?tapi(post_copystyle2r, "/some/page/B:c", ?COPYSTYLEd, 401, api).
?tapi(post_copystyle3r, "/some/page/B:c", ?COPYSTYLEd, 401, api_admin).
?tapi(post_copystyle4r, "/some/page/B:c", ?COPYSTYLEd, 401, api_appendonly).

?tapi(post_copystyle1s, "/some/page/3:4", ?COPYSTYLEd, 401, api_subdirs).
?tapi(post_copystyle2s, "/some/page/3:4", ?COPYSTYLEd, 401, api).
?tapi(post_copystyle3s, "/some/page/3:4", ?COPYSTYLEd, 401, api_admin).
?tapi(post_copystyle4s, "/some/page/3:4", ?COPYSTYLEd, 401, api_appendonly).

?tapi(post_copystyle1t, "/some/page/", ?COPYSTYLEd, 401, api_subdirs).
?tapi(post_copystyle2t, "/some/page/", ?COPYSTYLEd, 401, api).
?tapi(post_copystyle3t, "/some/page/", ?COPYSTYLEd, 401, api_admin).
?tapi(post_copystyle4t, "/some/page/", ?COPYSTYLEd, 401, api_appendonly).

-define(COPYVALUEa, "{\"copyvalue\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/A1:A1\"}}").
-define(COPYVALUEb, "{\"copyvalue\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/A1:A2\"}}").
-define(COPYVALUEc, "{\"copyvalue\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/g:g\"}}").
-define(COPYVALUEd, "{\"copyvalue\":{\"src\":\"http://tests.hypernumbers.dev:9000/some/page/10:10\"}}").
?tapi(post_copyvalue1a, "/some/page/B4", ?COPYVALUEa, 200, api_subdirs).
?tapi(post_copyvalue2a, "/some/page/B4", ?COPYVALUEa, 401, api).
?tapi(post_copyvalue3a, "/some/page/B4", ?COPYVALUEa, 200, api_admin).
?tapi(post_copyvalue4a, "/some/page/B4", ?COPYVALUEa, 401, api_appendonly).

?tapi(post_copyvalue1b, "/some/page/B4:b5", ?COPYVALUEa, 200, api_subdirs).
?tapi(post_copyvalue2b, "/some/page/B4:b5", ?COPYVALUEa, 401, api).
?tapi(post_copyvalue3b, "/some/page/B4:b5", ?COPYVALUEa, 200, api_admin).
?tapi(post_copyvalue4b, "/some/page/B4:b5", ?COPYVALUEa, 401, api_appendonly).

?tapi(post_copyvalue1c, "/some/page/B:c", ?COPYVALUEa, 401, api_subdirs).
?tapi(post_copyvalue2c, "/some/page/B:c", ?COPYVALUEa, 401, api).
?tapi(post_copyvalue3c, "/some/page/B:c", ?COPYVALUEa, 401, api_admin).
?tapi(post_copyvalue4c, "/some/page/B:c", ?COPYVALUEa, 401, api_appendonly).

?tapi(post_copyvalue1d, "/some/page/3:4", ?COPYVALUEa, 401, api_subdirs).
?tapi(post_copyvalue2d, "/some/page/3:4", ?COPYVALUEa, 401, api).
?tapi(post_copyvalue3d, "/some/page/3:4", ?COPYVALUEa, 401, api_admin).
?tapi(post_copyvalue4d, "/some/page/3:4", ?COPYVALUEa, 401, api_appendonly).

?tapi(post_copyvalue1e, "/some/page/", ?COPYVALUEa, 401, api_subdirs).
?tapi(post_copyvalue2e, "/some/page/", ?COPYVALUEa, 401, api).
?tapi(post_copyvalue3e, "/some/page/", ?COPYVALUEa, 401, api_admin).
?tapi(post_copyvalue4e, "/some/page/", ?COPYVALUEa, 401, api_appendonly).

?tapi(post_copyvalue1f, "/some/page/B4", ?COPYVALUEb, 200, api_subdirs).
?tapi(post_copyvalue2f, "/some/page/B4", ?COPYVALUEb, 401, api).
?tapi(post_copyvalue3f, "/some/page/B4", ?COPYVALUEb, 200, api_admin).
?tapi(post_copyvalue4f, "/some/page/B4", ?COPYVALUEb, 401, api_appendonly).

?tapi(post_copyvalue1g, "/some/page/B4:b5", ?COPYVALUEb, 200, api_subdirs).
?tapi(post_copyvalue2g, "/some/page/B4:b5", ?COPYVALUEb, 401, api).
?tapi(post_copyvalue3g, "/some/page/B4:b5", ?COPYVALUEb, 200, api_admin).
?tapi(post_copyvalue4g, "/some/page/B4:b5", ?COPYVALUEb, 401, api_appendonly).

?tapi(post_copyvalue1h, "/some/page/B:c", ?COPYVALUEb, 401, api_subdirs).
?tapi(post_copyvalue2h, "/some/page/B:c", ?COPYVALUEb, 401, api).
?tapi(post_copyvalue3h, "/some/page/B:c", ?COPYVALUEb, 401, api_admin).
?tapi(post_copyvalue4h, "/some/page/B:c", ?COPYVALUEb, 401, api_appendonly).

?tapi(post_copyvalue1i, "/some/page/3:4", ?COPYVALUEb, 401, api_subdirs).
?tapi(post_copyvalue2i, "/some/page/3:4", ?COPYVALUEb, 401, api).
?tapi(post_copyvalue3i, "/some/page/3:4", ?COPYVALUEb, 401, api_admin).
?tapi(post_copyvalue4i, "/some/page/3:4", ?COPYVALUEb, 401, api_appendonly).

?tapi(post_copyvalue1j, "/some/page/", ?COPYVALUEb, 401, api_subdirs).
?tapi(post_copyvalue2j, "/some/page/", ?COPYVALUEb, 401, api).
?tapi(post_copyvalue3j, "/some/page/", ?COPYVALUEb, 401, api_admin).
?tapi(post_copyvalue4j, "/some/page/", ?COPYVALUEb, 401, api_appendonly).

?tapi(post_copyvalue1k, "/some/page/B4", ?COPYVALUEc, 401, api_subdirs).
?tapi(post_copyvalue2k, "/some/page/B4", ?COPYVALUEc, 401, api).
?tapi(post_copyvalue3k, "/some/page/B4", ?COPYVALUEc, 401, api_admin).
?tapi(post_copyvalue4k, "/some/page/B4", ?COPYVALUEc, 401, api_appendonly).

?tapi(post_copyvalue1l, "/some/page/B4:b5", ?COPYVALUEc, 401, api_subdirs).
?tapi(post_copyvalue2l, "/some/page/B4:b5", ?COPYVALUEc, 401, api).
?tapi(post_copyvalue3l, "/some/page/B4:b5", ?COPYVALUEc, 401, api_admin).
?tapi(post_copyvalue4l, "/some/page/B4:b5", ?COPYVALUEc, 401, api_appendonly).

?tapi(post_copyvalue1m, "/some/page/B:c", ?COPYVALUEc, 401, api_subdirs).
?tapi(post_copyvalue2m, "/some/page/B:c", ?COPYVALUEc, 401, api).
?tapi(post_copyvalue3m, "/some/page/B:c", ?COPYVALUEc, 401, api_admin).
?tapi(post_copyvalue4m, "/some/page/B:c", ?COPYVALUEc, 401, api_appendonly).

?tapi(post_copyvalue1n, "/some/page/3:4", ?COPYVALUEc, 401, api_subdirs).
?tapi(post_copyvalue2n, "/some/page/3:4", ?COPYVALUEc, 401, api).
?tapi(post_copyvalue3n, "/some/page/3:4", ?COPYVALUEc, 401, api_admin).
?tapi(post_copyvalue4n, "/some/page/3:4", ?COPYVALUEc, 401, api_appendonly).

?tapi(post_copyvalue1o, "/some/page/", ?COPYVALUEc, 401, api_subdirs).
?tapi(post_copyvalue2o, "/some/page/", ?COPYVALUEc, 401, api).
?tapi(post_copyvalue3o, "/some/page/", ?COPYVALUEc, 401, api_admin).
?tapi(post_copyvalue4o, "/some/page/", ?COPYVALUEc, 401, api_appendonly).

?tapi(post_copyvalue1p, "/some/page/B4", ?COPYVALUEd, 401, api_subdirs).
?tapi(post_copyvalue2p, "/some/page/B4", ?COPYVALUEd, 401, api).
?tapi(post_copyvalue3p, "/some/page/B4", ?COPYVALUEd, 401, api_admin).
?tapi(post_copyvalue4p, "/some/page/B4", ?COPYVALUEd, 401, api_appendonly).

?tapi(post_copyvalue1q, "/some/page/B4:b5", ?COPYVALUEd, 401, api_subdirs).
?tapi(post_copyvalue2q, "/some/page/B4:b5", ?COPYVALUEd, 401, api).
?tapi(post_copyvalue3q, "/some/page/B4:b5", ?COPYVALUEd, 401, api_admin).
?tapi(post_copyvalue4q, "/some/page/B4:b5", ?COPYVALUEd, 401, api_appendonly).

?tapi(post_copyvalue1r, "/some/page/B:c", ?COPYVALUEd, 401, api_subdirs).
?tapi(post_copyvalue2r, "/some/page/B:c", ?COPYVALUEd, 401, api).
?tapi(post_copyvalue3r, "/some/page/B:c", ?COPYVALUEd, 401, api_admin).
?tapi(post_copyvalue4r, "/some/page/B:c", ?COPYVALUEd, 401, api_appendonly).

?tapi(post_copyvalue1s, "/some/page/3:4", ?COPYVALUEd, 401, api_subdirs).
?tapi(post_copyvalue2s, "/some/page/3:4", ?COPYVALUEd, 401, api).
?tapi(post_copyvalue3s, "/some/page/3:4", ?COPYVALUEd, 401, api_admin).
?tapi(post_copyvalue4s, "/some/page/3:4", ?COPYVALUEd, 401, api_appendonly).

?tapi(post_copyvalue1t, "/some/page/", ?COPYVALUEd, 401, api_subdirs).
?tapi(post_copyvalue2t, "/some/page/", ?COPYVALUEd, 401, api).
?tapi(post_copyvalue3t, "/some/page/", ?COPYVALUEd, 401, api_admin).
?tapi(post_copyvalue4t, "/some/page/", ?COPYVALUEd, 401, api_appendonly).

%% inline won't work because there is no inline formula there
-define(INLINE, "{\"postinline\": {\"formula\": \"=1+2\"}}").
?tapi(post_inline1a, "/some/page/B4", ?INLINE, 401, api_subdirs).
?tapi(post_inline2a, "/some/page/B4", ?INLINE, 401, api).
?tapi(post_inline3a, "/some/page/B4", ?INLINE, 401, api_admin).
?tapi(post_inline4a, "/some/page/B4", ?INLINE, 401, api_appendonly).

?tapi(post_inline1b, "/some/page/B4:b5", ?INLINE, 401, api_subdirs).
?tapi(post_inline2b, "/some/page/B4:b5", ?INLINE, 401, api).
?tapi(post_inline3b, "/some/page/B4:b5", ?INLINE, 401, api_admin).
?tapi(post_inline4b, "/some/page/B4:b5", ?INLINE, 401, api_appendonly).

?tapi(post_inline1c, "/some/page/B:c", ?INLINE, 401, api_subdirs).
?tapi(post_inline2c, "/some/page/B:c", ?INLINE, 401, api).
?tapi(post_inline3c, "/some/page/B:c", ?INLINE, 401, api_admin).
?tapi(post_inline4c, "/some/page/B:c", ?INLINE, 401, api_appendonly).

?tapi(post_inline1d, "/some/page/3:4", ?INLINE, 401, api_subdirs).
?tapi(post_inline2d, "/some/page/3:4", ?INLINE, 401, api).
?tapi(post_inline3d, "/some/page/3:4", ?INLINE, 401, api_admin).
?tapi(post_inline4d, "/some/page/3:4", ?INLINE, 401, api_appendonly).

?tapi(post_inline1e, "/some/page/", ?INLINE, 401, api_subdirs).
?tapi(post_inline2e, "/some/page/", ?INLINE, 401, api).
?tapi(post_inline3e, "/some/page/", ?INLINE, 401, api_admin).
?tapi(post_inline4e, "/some/page/", ?INLINE, 401, api_appendonly).

%% inlineclear won't work because there is no inlineclear formula there
-define(INLINECLEAR, "{\"postinline\": {\"clear\": \"contents\"}}").
?tapi(post_inlineclear1a, "/some/page/B4", ?INLINECLEAR, 401, api_subdirs).
?tapi(post_inlineclear2a, "/some/page/B4", ?INLINECLEAR, 401, api).
?tapi(post_inlineclear3a, "/some/page/B4", ?INLINECLEAR, 401, api_admin).
?tapi(post_inlineclear4a, "/some/page/B4", ?INLINECLEAR, 401, api_appendonly).

?tapi(post_inlineclear1b, "/some/page/B4:b5", ?INLINECLEAR, 401, api_subdirs).
?tapi(post_inlineclear2b, "/some/page/B4:b5", ?INLINECLEAR, 401, api).
?tapi(post_inlineclear3b, "/some/page/B4:b5", ?INLINECLEAR, 401, api_admin).
?tapi(post_inlineclear4b, "/some/page/B4:b5", ?INLINECLEAR, 401, api_appendonly).

?tapi(post_inlineclear1c, "/some/page/B:c", ?INLINECLEAR, 401, api_subdirs).
?tapi(post_inlineclear2c, "/some/page/B:c", ?INLINECLEAR, 401, api).
?tapi(post_inlineclear3c, "/some/page/B:c", ?INLINECLEAR, 401, api_admin).
?tapi(post_inlineclear4c, "/some/page/B:c", ?INLINECLEAR, 401, api_appendonly).

?tapi(post_inlineclear1d, "/some/page/3:4", ?INLINECLEAR, 401, api_subdirs).
?tapi(post_inlineclear2d, "/some/page/3:4", ?INLINECLEAR, 401, api).
?tapi(post_inlineclear3d, "/some/page/3:4", ?INLINECLEAR, 401, api_admin).
?tapi(post_inlineclear4d, "/some/page/3:4", ?INLINECLEAR, 401, api_appendonly).

?tapi(post_inlineclear1e, "/some/page/", ?INLINECLEAR, 401, api_subdirs).
?tapi(post_inlineclear2e, "/some/page/", ?INLINECLEAR, 401, api).
?tapi(post_inlineclear3e, "/some/page/", ?INLINECLEAR, 401, api_admin).
?tapi(post_inlineclear4e, "/some/page/", ?INLINECLEAR, 401, api_appendonly).

-define(POSTFORM, "{\"postform\": {\"results\": \"./_replies/\", \"values\":" ++
        "[{\"label\":\"Question 1\", \"formula\": \"erk\"}]}}").
?tapi(post_form1a, "/some/page/B4", ?POSTFORM, 401, api_subdirs).
?tapi(post_form2a, "/some/page/B4", ?POSTFORM, 401, api).
?tapi(post_form3a, "/some/page/B4", ?POSTFORM, 401, api_admin).
?tapi(post_form4a, "/some/page/B4", ?POSTFORM, 401, api_appendonly).

?tapi(post_form1b, "/some/page/B4:b5", ?POSTFORM, 401, api_subdirs).
?tapi(post_form2b, "/some/page/B4:b5", ?POSTFORM, 401, api).
?tapi(post_form3b, "/some/page/B4:b5", ?POSTFORM, 401, api_admin).
?tapi(post_form4b, "/some/page/B4:b5", ?POSTFORM, 401, api_appendonly).

?tapi(post_form1c, "/some/page/B:c", ?POSTFORM, 401, api_subdirs).
?tapi(post_form2c, "/some/page/B:c", ?POSTFORM, 401, api).
?tapi(post_form3c, "/some/page/B:c", ?POSTFORM, 401, api_admin).
?tapi(post_form4c, "/some/page/B:c", ?POSTFORM, 401, api_appendonly).

?tapi(post_form1d, "/some/page/3:4", ?POSTFORM, 401, api_subdirs).
?tapi(post_form2d, "/some/page/3:4", ?POSTFORM, 401, api).
?tapi(post_form3d, "/some/page/3:4", ?POSTFORM, 401, api_admin).
?tapi(post_form4d, "/some/page/3:4", ?POSTFORM, 401, api_appendonly).

?tapi(post_form1e, "/some/page/", ?POSTFORM, 401, api_subdirs).
?tapi(post_form2e, "/some/page/", ?POSTFORM, 401, api).
?tapi(post_form3e, "/some/page/", ?POSTFORM, 401, api_admin).
?tapi(post_form4e, "/some/page/", ?POSTFORM, 401, api_appendonly).

-define(POSTWEBCONTROL, "{\"postwebcontrols\": {\"chaos\": [{\"leper\":\"face\"}]}}").
?tapi(post_webcontrol1a, "/some/page/B4", ?POSTWEBCONTROL, 401, api_subdirs).
?tapi(post_webcontrol2a, "/some/page/B4", ?POSTWEBCONTROL, 401, api).
?tapi(post_webcontrol3a, "/some/page/B4", ?POSTWEBCONTROL, 401, api_admin).
?tapi(post_webcontrol4a, "/some/page/B4", ?POSTWEBCONTROL, 401, api_appendonly).

?tapi(post_webcontrol1b, "/some/page/B4:b5", ?POSTWEBCONTROL, 401, api_subdirs).
?tapi(post_webcontrol2b, "/some/page/B4:b5", ?POSTWEBCONTROL, 401, api).
?tapi(post_webcontrol3b, "/some/page/B4:b5", ?POSTWEBCONTROL, 401, api_admin).
?tapi(post_webcontrol4b, "/some/page/B4:b5", ?POSTWEBCONTROL, 401, api_appendonly).

?tapi(post_webcontrol1c, "/some/page/B:c", ?POSTWEBCONTROL, 401, api_subdirs).
?tapi(post_webcontrol2c, "/some/page/B:c", ?POSTWEBCONTROL, 401, api).
?tapi(post_webcontrol3c, "/some/page/B:c", ?POSTWEBCONTROL, 401, api_admin).
?tapi(post_webcontrol4c, "/some/page/B:c", ?POSTWEBCONTROL, 401, api_appendonly).

?tapi(post_webcontrol1d, "/some/page/3:4", ?POSTWEBCONTROL, 401, api_subdirs).
?tapi(post_webcontrol2d, "/some/page/3:4", ?POSTWEBCONTROL, 401, api).
?tapi(post_webcontrol3d, "/some/page/3:4", ?POSTWEBCONTROL, 401, api_admin).
?tapi(post_webcontrol4d, "/some/page/3:4", ?POSTWEBCONTROL, 401, api_appendonly).

?tapi(post_webcontrol1e, "/some/page/", ?POSTWEBCONTROL, 401, api_subdirs).
?tapi(post_webcontrol2e, "/some/page/", ?POSTWEBCONTROL, 401, api).
?tapi(post_webcontrol3e, "/some/page/", ?POSTWEBCONTROL, 401, api_admin).
?tapi(post_webcontrol4e, "/some/page/", ?POSTWEBCONTROL, 401, api_appendonly).

%% this revert won't work because we don't know the reversion index
%% so test for a 500
-define(POSTREVERT, "{\"revert_to\": \"yardle\"}").
?tapi(post_revert1a, "/some/page/B4", ?POSTREVERT, 500, api_subdirs).
?tapi(post_revert2a, "/some/page/B4", ?POSTREVERT, 401, api).
?tapi(post_revert3a, "/some/page/B4", ?POSTREVERT, 500, api_admin).
?tapi(post_revert4a, "/some/page/B4", ?POSTREVERT, 401, api_appendonly).

?tapi(post_revert1b, "/some/page/B4:b5", ?POSTREVERT, 401, api_subdirs).
?tapi(post_revert2b, "/some/page/B4:b5", ?POSTREVERT, 401, api).
?tapi(post_revert3b, "/some/page/B4:b5", ?POSTREVERT, 401, api_admin).
?tapi(post_revert4b, "/some/page/B4:b5", ?POSTREVERT, 401, api_appendonly).

?tapi(post_revert1c, "/some/page/B:c", ?POSTREVERT, 401, api_subdirs).
?tapi(post_revert2c, "/some/page/B:c", ?POSTREVERT, 401, api).
?tapi(post_revert3c, "/some/page/B:c", ?POSTREVERT, 401, api_admin).
?tapi(post_revert4c, "/some/page/B:c", ?POSTREVERT, 401, api_appendonly).

?tapi(post_revert1d, "/some/page/3:4", ?POSTREVERT, 401, api_subdirs).
?tapi(post_revert2d, "/some/page/3:4", ?POSTREVERT, 401, api).
?tapi(post_revert3d, "/some/page/3:4", ?POSTREVERT, 401, api_admin).
?tapi(post_revert4d, "/some/page/3:4", ?POSTREVERT, 401, api_appendonly).

?tapi(post_revert1e, "/some/page/", ?POSTREVERT, 401, api_subdirs).
?tapi(post_revert2e, "/some/page/", ?POSTREVERT, 401, api).
?tapi(post_revert3e, "/some/page/", ?POSTREVERT, 401, api_admin).
?tapi(post_revert4e, "/some/page/", ?POSTREVERT, 401, api_appendonly).

%% default_view
-define(POSTDEFAULT_VIEW, "{\"default_view\": {\"view\": \"table\"}}").
?tapi(post_default_view1a, "/some/page/B4", ?POSTDEFAULT_VIEW, 401, api_subdirs).
?tapi(post_default_view2a, "/some/page/B4", ?POSTDEFAULT_VIEW, 401, api).
?tapi(post_default_view3a, "/some/page/B4", ?POSTDEFAULT_VIEW, 401, api_admin).
?tapi(post_default_view4a, "/some/page/B4", ?POSTDEFAULT_VIEW, 401, api_appendonly).

?tapi(post_default_view1b, "/some/page/B4:b5", ?POSTDEFAULT_VIEW, 401, api_subdirs).
?tapi(post_default_view2b, "/some/page/B4:b5", ?POSTDEFAULT_VIEW, 401, api).
?tapi(post_default_view3b, "/some/page/B4:b5", ?POSTDEFAULT_VIEW, 401, api_admin).
?tapi(post_default_view4b, "/some/page/B4:b5", ?POSTDEFAULT_VIEW, 401, api_appendonly).

?tapi(post_default_view1c, "/some/page/B:c", ?POSTDEFAULT_VIEW, 401, api_subdirs).
?tapi(post_default_view2c, "/some/page/B:c", ?POSTDEFAULT_VIEW, 401, api).
?tapi(post_default_view3c, "/some/page/B:c", ?POSTDEFAULT_VIEW, 401, api_admin).
?tapi(post_default_view4c, "/some/page/B:c", ?POSTDEFAULT_VIEW, 401, api_appendonly).

?tapi(post_default_view1d, "/some/page/3:4", ?POSTDEFAULT_VIEW, 401, api_subdirs).
?tapi(post_default_view2d, "/some/page/3:4", ?POSTDEFAULT_VIEW, 401, api).
?tapi(post_default_view3d, "/some/page/3:4", ?POSTDEFAULT_VIEW, 401, api_admin).
?tapi(post_default_view4d, "/some/page/3:4", ?POSTDEFAULT_VIEW, 401, api_appendonly).

?tapi(post_default_view1e, "/some/page/", ?POSTDEFAULT_VIEW, 200, api_subdirs).
?tapi(post_default_view2e, "/some/page/", ?POSTDEFAULT_VIEW, 401, api).
?tapi(post_default_view3e, "/some/page/", ?POSTDEFAULT_VIEW, 200, api_admin).
?tapi(post_default_view4e, "/some/page/", ?POSTDEFAULT_VIEW, 401, api_appendonly).

%% set_view
-define(POSTSET_VIEW, "{\"set_view\": {\"view\": \"table\", \"everyone\": false, \"groups\": [\"admin\"]}}").
?tapi(post_set_view1a, "/some/page/B4", ?POSTSET_VIEW, 401, api_subdirs).
?tapi(post_set_view2a, "/some/page/B4", ?POSTSET_VIEW, 401, api).
?tapi(post_set_view3a, "/some/page/B4", ?POSTSET_VIEW, 401, api_admin).
?tapi(post_set_view4a, "/some/page/B4", ?POSTSET_VIEW, 401, api_appendonly).

?tapi(post_set_view1b, "/some/page/B4:b5", ?POSTSET_VIEW, 401, api_subdirs).
?tapi(post_set_view2b, "/some/page/B4:b5", ?POSTSET_VIEW, 401, api).
?tapi(post_set_view3b, "/some/page/B4:b5", ?POSTSET_VIEW, 401, api_admin).
?tapi(post_set_view4b, "/some/page/B4:b5", ?POSTSET_VIEW, 401, api_appendonly).

?tapi(post_set_view1c, "/some/page/B:c", ?POSTSET_VIEW, 401, api_subdirs).
?tapi(post_set_view2c, "/some/page/B:c", ?POSTSET_VIEW, 401, api).
?tapi(post_set_view3c, "/some/page/B:c", ?POSTSET_VIEW, 401, api_admin).
?tapi(post_set_view4c, "/some/page/B:c", ?POSTSET_VIEW, 401, api_appendonly).

?tapi(post_set_view1d, "/some/page/3:4", ?POSTSET_VIEW, 401, api_subdirs).
?tapi(post_set_view2d, "/some/page/3:4", ?POSTSET_VIEW, 401, api).
?tapi(post_set_view3d, "/some/page/3:4", ?POSTSET_VIEW, 401, api_admin).
?tapi(post_set_view4d, "/some/page/3:4", ?POSTSET_VIEW, 401, api_appendonly).

?tapi(post_set_view1e, "/some/page/", ?POSTSET_VIEW, 200, api_subdirs).
?tapi(post_set_view2e, "/some/page/", ?POSTSET_VIEW, 401, api).
?tapi(post_set_view3e, "/some/page/", ?POSTSET_VIEW, 200, api_admin).
?tapi(post_set_view4e, "/some/page/", ?POSTSET_VIEW, 401, api_appendonly).

%% save_template
-define(POSTSAVE_TEMPLATE, "{\"save_template\": {\"name\": \"banjolele\"}}").
?tapi(post_save_template1a, "/some/page/B4", ?POSTSAVE_TEMPLATE, 401, api_subdirs).
?tapi(post_save_template2a, "/some/page/B4", ?POSTSAVE_TEMPLATE, 401, api).
?tapi(post_save_template3a, "/some/page/B4", ?POSTSAVE_TEMPLATE, 401, api_admin).
?tapi(post_save_template4a, "/some/page/B4", ?POSTSAVE_TEMPLATE, 401, api_appendonly).

?tapi(post_save_template1b, "/some/page/B4:b5", ?POSTSAVE_TEMPLATE, 401, api_subdirs).
?tapi(post_save_template2b, "/some/page/B4:b5", ?POSTSAVE_TEMPLATE, 401, api).
?tapi(post_save_template3b, "/some/page/B4:b5", ?POSTSAVE_TEMPLATE, 401, api_admin).
?tapi(post_save_template4b, "/some/page/B4:b5", ?POSTSAVE_TEMPLATE, 401, api_appendonly).

?tapi(post_save_template1c, "/some/page/B:c", ?POSTSAVE_TEMPLATE, 401, api_subdirs).
?tapi(post_save_template2c, "/some/page/B:c", ?POSTSAVE_TEMPLATE, 401, api).
?tapi(post_save_template3c, "/some/page/B:c", ?POSTSAVE_TEMPLATE, 401, api_admin).
?tapi(post_save_template4c, "/some/page/B:c", ?POSTSAVE_TEMPLATE, 401, api_appendonly).

?tapi(post_save_template1d, "/some/page/3:4", ?POSTSAVE_TEMPLATE, 401, api_subdirs).
?tapi(post_save_template2d, "/some/page/3:4", ?POSTSAVE_TEMPLATE, 401, api).
?tapi(post_save_template3d, "/some/page/3:4", ?POSTSAVE_TEMPLATE, 401, api_admin).
?tapi(post_save_template4d, "/some/page/3:4", ?POSTSAVE_TEMPLATE, 401, api_appendonly).

?tapi(post_save_template1e, "/some/page/", ?POSTSAVE_TEMPLATE, 401, api_subdirs).
?tapi(post_save_template2e, "/some/page/", ?POSTSAVE_TEMPLATE, 401, api).
?tapi(post_save_template3e, "/some/page/", ?POSTSAVE_TEMPLATE, 200, api_admin).
?tapi(post_save_template4e, "/some/page/", ?POSTSAVE_TEMPLATE, 401, api_appendonly).

%% add_group
-define(POSTADD_GROUP, "{\"add_group\": {\"views\": [\"spreadsheet\"], \"group\": \"bernie inn\"}}").
?tapi(post_add_group1a, "/some/page/B4", ?POSTADD_GROUP, 401, api_subdirs).
?tapi(post_add_group2a, "/some/page/B4", ?POSTADD_GROUP, 401, api).
?tapi(post_add_group3a, "/some/page/B4", ?POSTADD_GROUP, 401, api_admin).
?tapi(post_add_group4a, "/some/page/B4", ?POSTADD_GROUP, 401, api_appendonly).

?tapi(post_add_group1b, "/some/page/B4:b5", ?POSTADD_GROUP, 401, api_subdirs).
?tapi(post_add_group2b, "/some/page/B4:b5", ?POSTADD_GROUP, 401, api).
?tapi(post_add_group3b, "/some/page/B4:b5", ?POSTADD_GROUP, 401, api_admin).
?tapi(post_add_group4b, "/some/page/B4:b5", ?POSTADD_GROUP, 401, api_appendonly).

?tapi(post_add_group1c, "/some/page/B:c", ?POSTADD_GROUP, 401, api_subdirs).
?tapi(post_add_group2c, "/some/page/B:c", ?POSTADD_GROUP, 401, api).
?tapi(post_add_group3c, "/some/page/B:c", ?POSTADD_GROUP, 401, api_admin).
?tapi(post_add_group4c, "/some/page/B:c", ?POSTADD_GROUP, 401, api_appendonly).

?tapi(post_add_group1d, "/some/page/3:4", ?POSTADD_GROUP, 401, api_subdirs).
?tapi(post_add_group2d, "/some/page/3:4", ?POSTADD_GROUP, 401, api).
?tapi(post_add_group3d, "/some/page/3:4", ?POSTADD_GROUP, 401, api_admin).
?tapi(post_add_group4d, "/some/page/3:4", ?POSTADD_GROUP, 401, api_appendonly).

?tapi(post_add_group1e, "/some/page/", ?POSTADD_GROUP, 200, api_subdirs).
?tapi(post_add_group2e, "/some/page/", ?POSTADD_GROUP, 401, api).
?tapi(post_add_group3e, "/some/page/", ?POSTADD_GROUP, 200, api_admin).
?tapi(post_add_group4e, "/some/page/", ?POSTADD_GROUP, 401, api_appendonly).

%% add_user
-define(POSTADD_USER, "{\"add_user\": {\"email\": \"yanko@example.com\", \"groups\": [\"berko\"], \"msg\": \"Hey, Black Jesus!\"}}").
?tapi(post_add_user1a, "/some/page/B4", ?POSTADD_USER, 401, api_subdirs).
?tapi(post_add_user2a, "/some/page/B4", ?POSTADD_USER, 401, api).
?tapi(post_add_user3a, "/some/page/B4", ?POSTADD_USER, 401, api_admin).
?tapi(post_add_user4a, "/some/page/B4", ?POSTADD_USER, 401, api_appendonly).

?tapi(post_add_user1b, "/some/page/B4:b5", ?POSTADD_USER, 401, api_subdirs).
?tapi(post_add_user2b, "/some/page/B4:b5", ?POSTADD_USER, 401, api).
?tapi(post_add_user3b, "/some/page/B4:b5", ?POSTADD_USER, 401, api_admin).
?tapi(post_add_user4b, "/some/page/B4:b5", ?POSTADD_USER, 401, api_appendonly).

?tapi(post_add_user1c, "/some/page/B:c", ?POSTADD_USER, 401, api_subdirs).
?tapi(post_add_user2c, "/some/page/B:c", ?POSTADD_USER, 401, api).
?tapi(post_add_user3c, "/some/page/B:c", ?POSTADD_USER, 401, api_admin).
?tapi(post_add_user4c, "/some/page/B:c", ?POSTADD_USER, 401, api_appendonly).

?tapi(post_add_user1d, "/some/page/3:4", ?POSTADD_USER, 401, api_subdirs).
?tapi(post_add_user2d, "/some/page/3:4", ?POSTADD_USER, 401, api).
?tapi(post_add_user3d, "/some/page/3:4", ?POSTADD_USER, 401, api_admin).
?tapi(post_add_user4d, "/some/page/3:4", ?POSTADD_USER, 401, api_appendonly).

?tapi(post_add_user1e, "/some/page/", ?POSTADD_USER, 200, api_subdirs).
?tapi(post_add_user2e, "/some/page/", ?POSTADD_USER, 401, api).
?tapi(post_add_user3e, "/some/page/", ?POSTADD_USER, 200, api_admin).
?tapi(post_add_user4e, "/some/page/", ?POSTADD_USER, 401, api_appendonly).

%% invite_user
-define(POSTINVITE_USER, "{\"invite_user\": {\"email\": \"yanko@example.com\", \"view\": \"spreadsheet\", \"msg\": \"Hey, Black Jesus!\"}}").
?tapi(post_invite_user1a, "/some/page/B4", ?POSTINVITE_USER, 401, api_subdirs).
?tapi(post_invite_user2a, "/some/page/B4", ?POSTINVITE_USER, 401, api).
?tapi(post_invite_user3a, "/some/page/B4", ?POSTINVITE_USER, 401, api_admin).
?tapi(post_invite_user4a, "/some/page/B4", ?POSTINVITE_USER, 401, api_appendonly).

?tapi(post_invite_user1b, "/some/page/B4:b5", ?POSTINVITE_USER, 401, api_subdirs).
?tapi(post_invite_user2b, "/some/page/B4:b5", ?POSTINVITE_USER, 401, api).
?tapi(post_invite_user3b, "/some/page/B4:b5", ?POSTINVITE_USER, 401, api_admin).
?tapi(post_invite_user4b, "/some/page/B4:b5", ?POSTINVITE_USER, 401, api_appendonly).

?tapi(post_invite_user1c, "/some/page/B:c", ?POSTINVITE_USER, 401, api_subdirs).
?tapi(post_invite_user2c, "/some/page/B:c", ?POSTINVITE_USER, 401, api).
?tapi(post_invite_user3c, "/some/page/B:c", ?POSTINVITE_USER, 401, api_admin).
?tapi(post_invite_user4c, "/some/page/B:c", ?POSTINVITE_USER, 401, api_appendonly).

?tapi(post_invite_user1d, "/some/page/3:4", ?POSTINVITE_USER, 401, api_subdirs).
?tapi(post_invite_user2d, "/some/page/3:4", ?POSTINVITE_USER, 401, api).
?tapi(post_invite_user3d, "/some/page/3:4", ?POSTINVITE_USER, 401, api_admin).
?tapi(post_invite_user4d, "/some/page/3:4", ?POSTINVITE_USER, 401, api_appendonly).

?tapi(post_invite_user1e, "/some/page/", ?POSTINVITE_USER, 200, api_subdirs).
?tapi(post_invite_user2e, "/some/page/", ?POSTINVITE_USER, 401, api).
?tapi(post_invite_user3e, "/some/page/", ?POSTINVITE_USER, 200, api_admin).
?tapi(post_invite_user4e, "/some/page/", ?POSTINVITE_USER, 401, api_appendonly).

%% append_row
-define(APPEND, "{\"append\":{\"values\":[{\"label\":\"Question 1:\",\"formula\":\"\"},{\"label\":\"Question 2:\",\"formula\":\"\"},{\"label\":\"Question 3:\",\"formula\":\"\"},{\"label\":\"Question 4:\",\"formula\":\"\"}]}}").
?tapi(post_append1a, "/some/page/B4", ?APPEND, 401, api_subdirs).
?tapi(post_append2a, "/some/page/B4", ?APPEND, 401, api).
?tapi(post_append3a, "/some/page/B4", ?APPEND, 401, api_admin).
?tapi(post_append4a, "/some/page/B4", ?APPEND, 401, api_appendonly).

?tapi(post_append1b, "/some/page/B4:b5", ?APPEND, 401, api_subdirs).
?tapi(post_append2b, "/some/page/B4:b5", ?APPEND, 401, api).
?tapi(post_append3b, "/some/page/B4:b5", ?APPEND, 401, api_admin).
?tapi(post_append4b, "/some/page/B4:b5", ?APPEND, 401, api_appendonly).

?tapi(post_append1c, "/some/page/B:c", ?APPEND, 401, api_subdirs).
?tapi(post_append2c, "/some/page/B:c", ?APPEND, 401, api).
?tapi(post_append3c, "/some/page/B:c", ?APPEND, 401, api_admin).
?tapi(post_append4c, "/some/page/B:c", ?APPEND, 401, api_appendonly).

?tapi(post_append1d, "/some/page/3:4", ?APPEND, 401, api_subdirs).
?tapi(post_append2d, "/some/page/3:4", ?APPEND, 401, api).
?tapi(post_append3d, "/some/page/3:4", ?APPEND, 401, api_admin).
?tapi(post_append4d, "/some/page/3:4", ?APPEND, 401, api_appendonly).

?tapi(post_append1e, "/some/page/", ?APPEND, 200, api_subdirs).
?tapi(post_append2e, "/some/page/", ?APPEND, 401, api).
?tapi(post_append3e, "/some/page/", ?APPEND, 200, api_admin).
?tapi(post_append4e, "/some/page/", ?APPEND, 200, api_appendonly).
