%%% -*- mode: erlang -*-
-module(~s).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").
-include("ct.hrl").

-include(~p).

init_per_suite(Config) ->
    Target = ~p,
    Test = ~p,
    case lists:member(Test, skipped()) of
        true  -> {skip, "Skipping"};
        false -> InitSite = fun(S) ->
                                    reset_perms(S, Test),
                                    hn_db_api:wait_for_dirty(S)
                            end,
                 [InitSite(S) || S <- sites()],
                 inets:start(httpc, [{profile, systest}]),
                 testsys:restore(Target, Test, systest),
                 [hn_db_api:wait_for_dirty(S) || S <- sites()], 
                 actions(),
                 [hn_db_api:wait_for_dirty(S) || S <- sites()], 
                 Config
    end.

skipped() ->
    ["hn_create", "hn_structural"].

reset_perms(Site, Test) ->
    View = "_g/core/spreadsheet",
    auth_srv2:add_view(Site, [Test], [everyone], View),
    auth_srv2:set_champion(Site, [Test], View).

end_per_suite(_Config) ->
    inets:stop(httpc, systest).

init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

get_val(Ref) ->
    case hn_db_api:read_attributes(Ref#refX{site=~p},["rawvalue"]) of
        [{_Ref, {"rawvalue", Val}}]           -> Val; 
        _Else                                 -> "" 
    end.

all() ->
    [~s].

~s
