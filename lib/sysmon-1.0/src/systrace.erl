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
    profile(PIDs, TraceFlags, Fun1).

profile_dbsrv(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    PID = whereis(Id),
    TraceFlags = [all],
    Fun = fun(X) ->
                  X:module_info(functions)
          end,
    profile([{PID, dbsrv}], TraceFlags, Fun).

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

