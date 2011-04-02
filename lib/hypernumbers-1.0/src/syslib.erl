%%% @author     Gordon Guthrie <>
%%% @copyright (C) 2011, Gordon Guthrie
%%% @doc       provides functions of sys admin
%%%
%%% @end
%%% Created :  2 Apr 2011 by Gordon Guthrie <>

-module(syslib).

-export([
         process_dump/0,
         top5/0
         ]).

process_dump() ->
    Procs = processes(),
    Info = [{X, process_info(X, [heap_size, message_queue_len, current_function])}
            || X <- Procs],
    io:format("Info is ~p~n", [Info]),
    ok.

 top5() ->
    Procs = processes(),
    Info = sort([info(X) || X <- Procs]),
    io:format("~p~n", [Info]),
    ok.

info(X) ->
    [{current_function, Fn}] = process_info(X, [current_function]),
    [{message_queue_len, Len}] = process_info(X, [message_queue_len]),
    [{heap_size, Heap}] = process_info(X, [heap_size]),
    [{reductions, Reductions}] = process_info(X, [reductions]),
    {X, Fn, Len, Heap, Reductions}.

sort(List) ->
    {Len5, _}  = lists:split(5, lists:reverse(lists:keysort(3, List))),
    {Heap5, _} = lists:split(5, lists:reverse(lists:keysort(4, List))),
    {Red5, _}  = lists:split(5, lists:reverse(lists:keysort(5, List))),
    [{longest, Len5}, {heapiest, Heap5}, {most_reductions, Red5}].
