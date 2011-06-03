%%% @author     Gordon Guthrie <>
%%% @copyright (C) 2011, Gordon Guthrie
%%% @doc       provides functions of sys admin
%%%
%%% @end
%%% Created :  2 Apr 2011 by Gordon Guthrie <>

-module(syslib).

-export([
         dump_queues/1,
         dump_queues/2,
         show_queues/1,
         process_dump/0,
         top5/0,
         show_registered/1,
         log/2,
         limiter/1,
         sample_fprof/1,
         sample_fprof/0
         ]).

-define(qs, [dirty_queue, dirty_zinf, dirty_for_zinf]).

sample_fprof() -> sample_fprof(10).

sample_fprof(N) when is_integer(N) andalso N > 0 ->
    Dir = "/media/logging/",
    Now = dh_date:format("y_M_d_h_i_s"),
    Log = Dir ++ "fprof_" ++ Now ++ ".log",
    Analysis = Dir ++ "fprof_analysis" ++ Now ++ ".log",
    sample_(Log, N),
    Analysis = Dir ++ "fprof_analysis" ++ Now ++ ".log",
    fprof:profile([{file, Log}]),
    fprof:analyse([{dest, Analysis}]),
    ok.

sample_(_Log, 0) -> ok;
sample_(Log, N) when is_integer(N) andalso N > 0 ->
    io:format("Taking fprof sample no ~p~n", [N]),
    fprof:trace([start, {file, Log}, {procs, all}]),
    timer:sleep(50),
    fprof:trace(stop),
    Rand = crypto:rand_uniform(0, 10000),
    timer:sleep(Rand),
    sample_(Log, N - 1).

dump_queues(Site) -> dump_queues(Site, ?qs).

dump_queues(_Site, [])     -> ok;
dump_queues(Site, [H | T]) -> dump(Site, H),
                              dump_queues(Site, T).

dump(Site, Table) ->
    Tab2 = new_db_wu:trans(Site, Table),
    io:format("Dumping table ~p~n", [Tab2]),
    Fun = fun() ->
                  Fun1 = fun(X, []) ->
                                 io:format("~p~n", [X]),
                                 []
                         end,
                  mnesia:foldl(Fun1, [], Tab2)
          end,
    mnesia:transaction(Fun).

show_queues(Site) ->
    Qs = [new_db_wu:trans(Site, X) || X <- ?qs],
    showq(Qs).

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
    Date = dh_date:format("d-M-y h:i:s"),

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
    DQ = hn_util:site_to_atom(Site, "&dirty_queue"),
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    DirtyQueue = mnesia:table_info(DQ, size),
    if
        Len > 100
            orelse DirtyQueue > 1000  -> timer:sleep(100),
                                         limiter(Site);
        true -> ok
    end.
