
%%% @author     Gordon Guthrie <>
%%% @copyright (C) 2011, Gordon Guthrie
%%% @doc       provides functions of sys admin
%%%
%%% @end
%%% Created :  2 Apr 2011 by Gordon Guthrie <>

-module(syslib).

-export([
         show_queues/1,
         process_dump/0,
         top5/0,
         show_registered/1,
         log/2,
         limiter/1
         ]).

show_queues(Site) ->
    Dirty          = new_db_wu:trans(Site, dirty_queue),
    Dirty_Zinf     = new_db_wu:trans(Site, dirty_zinf),
    Dirty_For_Zinf = new_db_wu:trans(Site, dirty_for_zinf),
    showq([Dirty, Dirty_Zinf, Dirty_For_Zinf]).

showq([])      -> ok;
showq([H | T]) -> io:format("~p ~p~n", [H, mnesia:table_info(H, size)]),
                  showq(T).

show_registered("http://"++Site) ->
    Site2 = [case X of $: -> $&; X -> X end || X <- Site],
    GlobalNames = global:registered_names(),
    LocalNames = registered(),
    show_r(GlobalNames, Site2, "global"),
    show_r(LocalNames, Site2, "local").

show_r([], _Site, _) -> ok;
show_r([Local | T], Site, "local")
  when Local == hypernumbers_sup
       orelse Local == service_sup
       orelse Local == sitemaster_sup
       orelse Local == hn_mochi       ->
    Pid = whereis(Local),
    io:format("~p is ~p registered locally for the server...~n", [Local, Pid]),
    show_r(T, Site, "local");
show_r([H | T], Site, Type) ->
    Name = atom_to_list(H),
    Length1 = length(Site),
    Length2 = length(Name),
    if
        Length2 > Length1 ->
            case lists:split(Length1, Name) of
                {Site, Reg} ->
                    Atom = list_to_existing_atom(Name),
                    Pid = case Reg of
                              "_dbsrv" -> whereis(Atom);
                              _        -> global:whereis_name(Atom)
                          end,
                    io:format("~p is ~p " ++ Type ++"~n", [Name, Pid]);
                _            -> ok
            end;
        Length2 =< Length1 -> ok
    end,
    show_r(T, Site, Type).

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

log(String, File) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/logs/",
    _Return=filelib:ensure_dir(Dir ++ File),
    Date = dh_date:format("d-M-y h:m:s"),

    case file:open(Dir ++ File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [Date ++ "," ++ String]),
	    file:close(Id);
	_ ->
	    error
    end.

limiter(Site) ->
    Srv = hn_util:site_to_atom(Site, "_dbsrv"),
    Pid = whereis(Srv),
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    Locks = length(mnesia:system_info(held_locks)),
    DirtyQueue = mnesia:table_info('dla-piper.hypernumbers.com&80&dirty_queue', size),
    if
        Len > 100
            orelse Locks > 100
            orelse DirtyQueue > 1000  -> timer:sleep(100),
                                         limiter(Site);
        true -> ok
    end.
