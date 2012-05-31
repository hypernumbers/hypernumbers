%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc        A module for measuring code performance
%%%
%%% @end
%%% Created : 30 May 2012 by gordon@vixo.com

-module(systrace).

-export([
         profile_dbsrv/1,
         profile_db_wu/0
        ]).

-define(OneMinute, 60000). % in microseconds
-define(D, 68).

-record(timings, {
          pid = "",
          function = "",
          calls = "",
          time = ""
         }).

profile_db_wu() ->
    PIDs = [{all, new_db_wu}],
    TraceFlags = [all],
    Fun1 = fun(X) ->
                  Fns = X:module_info(functions),
                  Fun2 = fun({X1, Arity}, Acc) ->
                                 Hd = hd(lists:reverse(atom_to_list(X1))),
                                 case Hd of
                                     % ascii matching!
                                     ?D -> [{X1, Arity} | Acc];
                                     _  -> Acc
                                 end
                         end,
                   lists:foldl(Fun2, [], Fns)
          end,
    Prof = profile(PIDs, TraceFlags, Fun1),
    print(Prof),
    ok.

profile_dbsrv(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    PID = whereis(Id),
    TraceFlags = [all],
    Fun = fun(X) ->
                  X:module_info(functions)
          end,
    Prof = profile([{PID, dbsrv}], TraceFlags, Fun),
    print(Prof),
    ok.

profile(PIDs, TraceFlags, SelFun) ->
    % first set up the tracing
    Fun1 = fun({PID, Module}) ->
                   erlang:trace(PID, true, TraceFlags),
                   erlang:trace_pattern({Module, '_', '_'}, true,
                                        [local, call_time])
           end,
    [Fun1(X) || X <- PIDs],
    % now sample
    Fun2 = fun({_PID, Mod}) ->
                   Fns = SelFun(Mod),
                   timer:sleep(?OneMinute),
                   Fun3 = fun({Fn, Arity}) ->
                                  Trace = erlang:trace_info({Mod, Fn, Arity},
                                                            call_time),
                                  case Trace of
                                      {call_time, []} ->
                                          [];
                                      {call_time, [{P, C, S, Ms}]} ->
                                          Total = ((S * 1000 + Ms)/1000),
                                          #timings{pid = P,
                                                   function = {Mod, Fn, Arity},
                                                   calls = C,
                                                   time = Total}
                                  end
                          end,
                   lists:flatten([Fun3(X) || X <- Fns])
           end,
    Timings = [Fun2(X) || X <- PIDs],
    % now tidy up
    [erlang:trace(PID, false, [all]) || {PID, _M} <- PIDs],
    % return the timings
    Timings.

print([]) ->
    ok;
print([[]]) ->
    io:format("No activity~n");
print([H | T]) ->
    Fun = fun(#timings{time = Tm}, Acc) ->
                  Acc + Tm
          end,
    Total = lists:foldl(Fun, 0, H),
    io:format("Total is ~p~n", [Total]),
    p2(H, Total),
    print(T).

p2([], _Total) -> ok;
p2([H | T], Total) ->
    #timings{function = {Mod, Fn, Arity}, calls = C, time = Tm} = H,
    case {Total, C} of
        {Zero, C} when (Zero == 0 orelse Zero == 0.0)
        andalso (C == 0 orelse C == 0.0) ->
            io:format("~p,~p,~p,~p,~p,-,-~n",
                      [Mod, Fn, Arity, C, T]);
        {Zero, _} when Zero == 0 orelse Zero == 0.0 ->
            io:format("~p,~p,~p,~p,~p,-,~p~n",
                      [Mod, Fn, Arity, C, T, Tm/C]);
        {_, C} when C == 0 orelse C == 0.0 ->
            io:format("~p,~p,~p,~p,~p,~p%,-~n",
                      [Mod, Fn, Arity, C, Tm, (Tm/Total)*100]);
        _Other ->
            io:format("~p,~p,~p,~p,~p,~p%,~p~n",
                      [Mod, Fn, Arity, C, Tm, (Tm/Total)*100, Tm/C])
    end,
    p2(T, Total).
