%%% -*- mode: erlang -*-
-module(~s).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").
-include("ct.hrl").

-include(~p).

init_per_suite(Config) ->
    Init = fun(S) ->
                   auth_srv:add_perm(S, [{user, "*"}, {group, "*"}], ["[**]"],
                                     [read, write],
                                     "hypernumbers/index", ["hypernumbers/index"]),
                   hn_db_api:wait_for_dirty(S)
           end,
    [Init(S) || S <- sites()], 
    testsys:restore(~p, ~p),
    actions(),
    [hn_db_api:wait_for_dirty(S) || S <- sites()], 
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

get_val(Ref) ->
    case hn_db_api:read_attributes(Ref#refX{site=~p},["value"]) of
       [{_Ref, {"value", Val}}]           -> Val; 
       _Else                              -> "" 
    end.

all() ->
    [~s].

~s
