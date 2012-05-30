%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc        A module for measuring code performance
%%%
%%% @end
%%% Created : 30 May 2012 by gordon@vixo.com

-module(systrace).

-export([
         profile_dbsrv/1
        ]).

-define(OneMinute, 60000). % in microseconds

profile_dbsrv(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    PID = whereis(Id),
    erlang:trace(PID, true, [all]),
    erlang:trace_pattern({dbsrv, '_', '_'}, true, [local, call_time]),
    Fns = dbsrv:module_info(functions),
    timer:sleep(?OneMinute),
    Fun = fun({Fn, Arity}) ->
                  Trace = erlang:trace_info({dbsrv, Fn, Arity}, call_time),
                   case Trace of
                       {call_time, []} ->
                           ok;
                       {call_time, [{_Pid, C, S, Ms}]} ->
                           io:format("~p/~p has been called ~p times av ~p secs~n",
                                     [Fn, Arity, C, (((S * 1000 + Ms)/1000)/C)])
                   end
          end,
    [Fun(X) || X <- Fns],
    erlang:trace(PID, false, [all]).

