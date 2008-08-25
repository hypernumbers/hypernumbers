%%% @doc Tests propagation of updates between cells:
%%%   * On the same page and between different pages.
%%%   * Relative and absolute.
%%% @author <hasan@hypernumbers.com>

-module(update_propagation_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").
-include("../../priv/misc/hypernumbers_settings.hrl").

init_per_suite(Config) ->
    code:add_path("../../../../../priv/misc"),
    code:add_path("../../../../../lib/hypernumbers-1.0/ebin"),
    Hn = hypernumbers:new(#hypernumbers_settings{host = "127.0.0.1",
                                                 port = "9000"}),
    inets:start(),
    [Hn|Config].

end_per_suite(_Config) ->
    inets:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
     simple_same_page
    ].

%%% Set up a simple dependency chain on the same page, and:
%%%   1. Check that the values are all set up properly initially.
%%%   2. Check that the update to the parent cell updates the child.
%%% TODO: Macros for the repeated code.
simple_same_page(Config) ->
    Hn = case lists:keysearch(hypernumbers, 1, Config) of
             {value, H} -> H;
             false      -> throw("hypernumbers instance not created~n")
         end,

    io:format("Hn :: ~p~n", [Hn]),
    io:format("Settings: ~p~n", [Hn:settings()]),
    
    Hn:set_value([], a1, 42),
    Hn:set_value([], b1, "=A1"),
    Hn:set_value([], c1, "=B1"),
    Hn:set_value([], d1, "=C1"),

    timer:sleep(3000),
    
    First = lists:map(fun({Ref, Expval}) ->
                              Hn:get_value([], Ref) == Expval
                      end,
                      [{a1, 42}, {b1, 42}, {c1, 42}, {d1, 42}]),
    
    case lists:all(fun(X) -> X == true end, First) of
        true -> ok;
        false -> throw({exit, "First set failed."})
    end,
    
    Hn:set_value([], a1, 13),

    timer:sleep(3000),
    
    Second = lists:map(fun({Ref, Expval}) ->
                               Hn:get_value([], Ref) == Expval
                       end,
                       [{a1, 13}, {b1, 13}, {c1, 13}, {d1, 13}]),

    case lists:all(fun(X) -> X == true end, Second) of
        true -> ok;
        false -> throw({exit, "Second set failed."})
    end.
