%%% @author     Gordon Guthrie <>
%%% @copyright (C) 2011, Gordon Guthrie
%%% @doc       provides functions of sys admin
%%%
%%% @end
%%% Created :  2 Apr 2011 by Gordon Guthrie <>

-module(syslib).

-export([
         overview/1,
         make_stats_page/1,
         check_supervisors/0,
         check_supervisors/1,
         table_status/0,
         table_status/1,
         %tail_queues/1,
         %tail_queues/2,
         dump_queues/0,
         dump_queues/1,
         dump_queues/2,
         show_queues/0,
         show_queues/1,
         process_dump/0,
         top5/0,
         top/0,
         show_registered/0,
         show_registered/1,
         log/2,
         log_term/2,
         clear_log/1,
         limiter/1,
         limit_global_mq/2,
         sample_fprof/1,
         sample_fprof/0,
         consult_term_log/1,
         log_memory/0,
         log_process/1
        ]).

-define(qs, [dirty_queue, dirty_zinf, dirty_for_zinf]).

overview(Site) ->
    Msg1 = check_supervisors(Site),
    io:format(Msg1),
    Msg2 = show_queues(Site),
    io:format(Msg2),
    Msg3 = table_status(Site),
    io:format(Msg3),
    ok.

make_stats_page(Site) ->
    Sups = convert_lf(check_supervisors(Site)),
    Qs = convert_lf(show_queues(Site)),
    Tables = [atom_to_list(X) || X <- mnesia:system_info(tables)],
    TableInfo = table_s2(Site, Tables, technical),
    TableInfo2 = convert_lf(io_lib:format(lists:flatten(TableInfo), [])),
    Billing = convert_lf(io_lib:format(lists:flatten(get_billing(Site)), [])),
    CPU = convert_lf(os:cmd("top -b -n 1 -u hypernumbers")),
    "<html><head></head><body><div style='font-family:monospace'>" ++
        "<h1 style='color:#ffcc00'>(Logical) Site And (Physical) " ++
        "Server Statistics</h1>" ++
        "<h2 style='color:#ffcc00'>" ++ Site ++ " Specific Stuff</h2>" ++
        "<h3>Are any supervisors borked?</h3>" ++
        "<small>(no news is good news here)</small><br />" ++
        Sups ++
        "<h3>Billing Statistics</h3>" ++
        "<small>Memory is in Erlang Words</small><br />" ++
        Billing ++
        "<h3>Status Of Queues</h3>" ++
        "<small>(should mostly be 0 but less than 250 is OK " ++
        "for dirty_zinf)</small><br />" ++
        Qs ++
        "<h3>Status Of Tables</h3>" ++
        "<small>ram_copy is in memory only, disc_only_copy is on disc only, " ++
        "disc_copy is on disc and in ram</small><br />" ++
        TableInfo2 ++
        "<h2 style='color:#ffcc00'>Server Specific Stuff</h2>" ++
        "<h3>Top Snapshot</h3>" ++
        CPU ++
        "</div></body></html>".

table_status() ->
    Sites = hn_setup:get_sites(),
    Tables = [atom_to_list(X) || X <- mnesia:system_info(tables)],
    io:format(lists:flatten([table_s2(X, Tables, technical) || X <- Sites])).

table_status(Site) ->
    Tables = [atom_to_list(X) || X <- mnesia:system_info(tables)],
    io:format(lists:flatten(table_s2(Site, Tables, technical))).

table_s2(Site, Tables, technical) ->
    Site2 = new_db_wu:get_prefix(Site),
    Tab2 = get_site_tables(Tables, Site2, []),
    [table_s3a(X) || X <- Tab2];
table_s2(Site, Tables, billing) ->
    Site2 = new_db_wu:get_prefix(Site),
    Tab2 = get_site_tables(Tables, Site2, []),
    {Mem, Cells} = lists:foldl(fun table_s3b/2, {0, 0}, Tab2),
    Pages = length(page_srv:get_flatpages(Site)),
    io_lib:format("Cells:   ~p~nPages:   ~p~nMemory: ~p~n",
                  [Cells, Pages, Mem]).

table_s3a(X) ->
    TabInfo = mnesia:table_info(list_to_atom(X), all),
    {size, Size} = lists:keyfind(size, 1, TabInfo),
    {memory, Memory} = lists:keyfind(memory, 1, TabInfo),
    Type = get_type(TabInfo),
    io_lib:format("~nTable: ~p~n~p Records~n~p~n~p Words of memory used~n",
                  [X, Size, Type, Memory]).

table_s3b(X, {Total, Items}) ->
    X1 = trim(X),
    TabInfo = mnesia:table_info(list_to_atom(X), all),
    {size, Size} = lists:keyfind(size, 1, TabInfo),
    {memory, Memory} = lists:keyfind(memory, 1, TabInfo),
    Type = get_type(TabInfo),
    NewTotal = case Type of
                   disc_copy      -> Total + Memory;
                   ram_copy       -> Total + Memory;
                   disc_only_copy -> Total
               end,
    NewItems = case X1 of
                   "item" -> Size;
                   _      -> Items
               end,
    {NewTotal, NewItems}.

trim(X) -> [_, _, X1] = string:tokens(X, "&"),
           X1.

get_type(TabInfo) ->
    {ram_copies, RamC}            = lists:keyfind(ram_copies, 1, TabInfo),
    {disc_copies, DiscC}          = lists:keyfind(disc_copies, 1, TabInfo),
    {disc_only_copies, DiscOnlyC} = lists:keyfind(disc_only_copies, 1, TabInfo),
    case {RamC, DiscC, DiscOnlyC} of
        {[_N], [], []} -> ram_copy;
        {[], [_N], []} -> disc_copy;
        {[], [], [_N]} -> disc_only_copy
    end.

get_site_tables([], _Site, Acc) ->
    lists:reverse(Acc);
get_site_tables([H | T], Site, Acc) ->
    Len = length(Site),
    case string:left(H, Len) of
        Site -> get_site_tables(T, Site, [H | Acc]);
        _    -> get_site_tables(T, Site, Acc)
    end.

check_supervisors() ->
    Sites = hn_setup:get_sites(),
    Globals = global:registered_names(),
    Locals = registered(),
    io:format("Checking all supervisors - " ++
              "will report ones that don't exist...~n"),
    Msg = [check_super2(X, Globals, Locals) || X <- Sites],
    Msg2 = lists:flatten(Msg),
    io:format(Msg2).

check_supervisors(Site) ->
    Globals = global:registered_names(),
    Locals = registered(),
    io:format("checking supervisors for ~p (no news is good news)~n",
              [Site]),
    check_super2(Site, Globals, Locals).

check_super2(Site, Globals, Locals) ->
    GlobalSups = get_sups(Site, global),
    LocalSups = get_sups(Site, local),
    Ret1 = [check(X, Globals) || X <- GlobalSups],
    Ret2 = [check(X, Locals) || X <- LocalSups],
    lists:flatten(lists:merge(lists:flatten(Ret1), lists:flatten(Ret2))).

get_sups(Site, global) ->
    get_sups2(["_status", "_auth", "_tick", "_remoting",
                     "_dbsrv_sup", "_pages", "_sup", "_zinf", "_calc_sup"],
              Site, []);
get_sups(Site, local) ->
    get_sups2(["_dbsrv"], Site, []).

get_sups2([], _Site, Acc) -> Acc;
get_sups2([H | T], Site, Acc) ->
    NewAcc = hn_util:site_to_atom(Site, H),
    get_sups2(T, Site, [NewAcc | Acc]).

check(Sup, List) ->
    case lists:member(Sup, List) of
        true  -> [];
        false -> io_lib:format("~p does not exist~n", [Sup])
    end.

log_process(Pid) when is_pid(Pid) ->
    {Pid, Fn, Length, Heap, Reductions} = info(Pid),
    Msg = io_lib:format("Pid,~p,Function,~p,Message Queue Length,~p,"
                        ++ "Heap,~p,Reductions,~p",
                        [Pid, Fn, Length, Heap, Reductions]),
    log(Msg, "process_log"),
    timer:sleep(100),
    log_process(Pid).


log_memory() ->
    Msg = io_lib:format("~w", [top5_()]),
    log(lists:flatten(Msg), "memory_log"),
    timer:sleep(100),
    log_memory().

clear_log(Log) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/logs/",
    ok = file:delete(Dir ++ Log).

consult_term_log(Log) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/logs/",
    {ok, Terms} = file:consult(Dir ++ Log),
    Terms.

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

%% tail_queues(Site) -> tail_queues(Site, ?qs).

%% tail_queues(Site, [H | T]) ->
%%     tail_(Site, H),
%%     tail_queues(Site, T).

%% tail_(Site, Q) ->
%%     Tab2 = new_db_wu:trans(Site, Q),
%%     io:format("Tailing table ~p~n", [Tab2]),
%%     ok.

dump_queues() ->
    Sites = hn_setup:get_sites(),
    [dump_queues(X) || X <- Sites],
    ok.

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

show_queues() ->
    Sites = hn_setup:get_sites(),
    Msg = [show_queues(X, notverbose) || X <- Sites],
    Msg2 = lists:flatten(Msg),
    io:format(Msg2).

show_queues(Site) -> show_queues(Site, verbose).

show_queues(Site, Verbose) ->
    Qs = [new_db_wu:trans(Site, X) || X <- ?qs],
    QLen = dbsrv:dump_q_len(Site),
    Msg1 = case {Verbose, QLen} of
               {verbose, _} ->
                   io_lib:format("The dbsrv workplan has ~p cells in it~n",
                                 [QLen]);
               {_, 0} ->
                   [];
               {_, _} ->
                   io_lib:format("The dbsrv workplan has ~p cells in it~n",
                                 [QLen])
           end,
    Msg2 = showq(Qs, Verbose, []),
    lists:flatten(lists:merge(Msg1, Msg2)).

showq([], _Verbose, Acc) ->
    lists:reverse(Acc);
showq([H | T], Verbose, Acc) ->
    N = mnesia:table_info(H, size),
    NewAcc = case N of
                 0 -> case Verbose of
                          verbose ->
                              io_lib:format("Queue ~p has ~p records~n",
                                            [H, N]);
                          _ ->
                              []
                      end;
                 1 ->
                     io_lib:format("Queue ~p has ~p record~n", [H, N]);
                 _ ->
                     io_lib:format("Queue ~p has ~p records~n", [H, N])
             end,
    showq(T, Verbose, [NewAcc | Acc]).

show_registered() ->
    Globals = [{global:whereis_name(X), global, X} ||
                  X <- global:registered_names()],
    Locals = [{whereis(X), local, X} || X <- registered()],
    Regs = lists:sort(lists:merge([Globals, Locals])),
    io:format("Registered processes are:~n"),
    [io:format("~p ~p ~p~n", [Pid, Type, Name]) || {Pid, Type, Name} <- Regs],
    ok.

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
    Info = [{X, process_info(X, [heap_size, message_queue_len,
                                 current_function])}
            || X <- Procs],
    io:format("Info is ~p~n", [Info]),
    ok.

top() ->
    Procs = processes(),
    Lens = [process_info(X, [message_queue_len]) || X <- Procs],
    Lens2 = [X || [{message_queue_len, X}] <- Lens],
    lists:max(Lens2).

top5() ->
    io:format("~p~n", [top5_()]),
    ok.

top5_() ->
    Procs = processes(),
    sort([info(X) || X <- Procs]).

info(X) ->
    Fn1  = case process_info(X, [current_function]) of
               [{current_function, Fn}] -> Fn;
               undefined                -> 0
           end,
    Len1  = case process_info(X, [message_queue_len]) of
                [{message_queue_len, Len}] -> Len;
                undefined                   -> 0
           end,
    Heap1  = case process_info(X, [heap_size]) of
               [{heap_size, Heap}] -> Heap;
               undefined           -> 0
             end,
    Reds1  = case process_info(X, [reductions]) of
               [{reductions, Reds}] -> Reds;
               undefined            -> 0
           end,
    {X, Fn1, Len1, Heap1, Reds1}.

sort(List) ->
    {Len5, _}  = lists:split(5, lists:reverse(lists:keysort(3, List))),
    {Heap5, _} = lists:split(5, lists:reverse(lists:keysort(4, List))),
    {Red5, _}  = lists:split(5, lists:reverse(lists:keysort(5, List))),
    [{longest, Len5}, {heapiest, Heap5}, {most_reductions, Red5}].

log_term(Term, File) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/logs/",
    _Return=filelib:ensure_dir(Dir ++ File),
    case file:open(Dir ++ File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~w.~n", [Term]),
            file:close(Id);
        _ ->
            error
    end.

log(String, File) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/logs/",
    _Return = filelib:ensure_dir(Dir ++ File),
    Date = dh_date:format("d-M-y h:i:s"),

    case file:open(Dir ++ File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Date ++ "," ++ String]),
            file:close(Id);
        _ ->
            error
    end.

limiter(Site) ->

    DBSrv = hn_util:site_to_atom(Site, "_dbsrv"),
    DBPid = whereis(DBSrv),
    RemSrv = hn_util:site_to_atom(Site, "_remoting"),
    RemPid = global:whereis_name(RemSrv),
    ZinfSrv = hn_util:site_to_atom(Site, "_zinf"),
    ZinfPid = global:whereis_name(ZinfSrv),
    DQ = hn_util:site_to_atom(Site, "&dirty_queue"),
    limiter1(DBPid, RemPid, ZinfPid, DQ).

limiter1(DBPid, RemPid, ZinfPid, DQ) ->
    {message_queue_len, DBLen} = process_info(DBPid, message_queue_len),
    {message_queue_len, RemLen} = process_info(RemPid, message_queue_len),
    {message_queue_len, ZinfLen} = process_info(ZinfPid, message_queue_len),
    DirtyQueue = mnesia:table_info(DQ, size),
    if
        DBLen > 100
        orelse ZinfLen > 100
        orelse RemLen > 100
        orelse DirtyQueue > 100  -> timer:sleep(100),
                                    limiter1(DBPid, RemPid, ZinfPid, DQ);
        true                     -> ok
    end.

limit_global_mq(Site, Q) ->
    Srv = hn_util:site_to_atom(Site, Q),
    Pid = global:whereis_name(Srv),
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    if
        Len > 100 -> timer:sleep(10),
                     limit_global_mq(Site, Q);
        true      -> ok
    end.

convert_lf(String) ->
    conv(String, []).

% turns line feeds '\n' ie ASCII Char 10 into br's
conv([], Acc)       -> lists:flatten(lists:reverse(Acc));
conv([10 | T], Acc) -> conv(T, ["<br />" | Acc]);
conv([32 | T], Acc) -> conv(T, ["&nbsp;" | Acc]);
conv([9 | T], Acc)  -> conv(T, ["&nbsp;&nbsp;&nbsp;&nbsp;" ++
                                "&nbsp;&nbsp;&nbsp;&nbsp;" | Acc]);
conv([H | T], Acc)  -> conv(T, [H | Acc]).

get_billing(Site) ->
    Tables = [atom_to_list(X) || X <- mnesia:system_info(tables)],
    table_s2(Site, Tables, billing).

