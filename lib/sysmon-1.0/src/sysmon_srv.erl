%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2011 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(sysmon_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([
         start_logging_memory/0,
         stop_logging_memory/0,
         monitor_mnesia/0,
         monitor_mnesia/1,
         monitor_mnesia/2,
         unmonitor_mnesia/0,
         analyze/0,
         clear_logs/0
        ]).

% for spawning
-export([
         run_analysis/0
        ]).

-include("sysmon.hrl").

-define(SERVER, ?MODULE).
-define(log, "sysmon.log").
-define(api_stats, "api.").
-define(tables, ["&item", "&form", "&group", "&timer", "&dirty_zinf",
                 "&relation", "&style", "&local_obj", "&dirty_queue",
                 "&dirty_for_zinf", "&include", "&logging", "&kvstore"]).

-record(state, {subs = [], log_pid = null}).

%%%===================================================================
%%% API
%%%===================================================================
start_logging_memory() ->
    gen_server:cast(?MODULE, start_logging_memory).

stop_logging_memory() ->
    gen_server:cast(?MODULE, stop_logging_memory).


monitor_mnesia() ->
    gen_server:cast(?MODULE, {monitor,
                              make_tables("http://hypernumbers.dev:9000")}).

monitor_mnesia(Site) ->
    gen_server:cast(?MODULE, {monitor, make_tables(Site)}).

monitor_mnesia(Site, Table) ->
    gen_server:cast(?MODULE, {monitor, [hn_util:site_to_atom(Site, Table)]}).

unmonitor_mnesia() ->
    gen_server:cast(?MODULE, unmonitor).

analyze() ->
    gen_server:cast(?MODULE, analyze).

clear_logs() ->
    gen_server:call(?MODULE, clear_logs).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(clear_logs, _From, State) ->
    syslib:clear_log(?log),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(start_logging_memory, State) ->
    io:format("Starting to log memory~n"),
    case State#state.log_pid of
        null -> NewPID = spawn(syslib, log_memory, []),
                {noreply, State#state{log_pid = NewPID}};
        PID  -> error_logger:info_msg("sysmon_srv: already logging memory ~p~n",
                                      [PID]),
                {noreply, State}
    end;
handle_cast(stop_logging_memory, State) ->
    io:format("Stopping logging memory~n"),
    case State#state.log_pid of
        null  -> error_logger:info_msg("sysmon_srv: memory not being logged!~n",
                                       []),
                {noreply, State};
        PID   -> exit(PID, kill),
                {noreply, State#state{log_pid = null}}
    end;
handle_cast(analyze, State) ->
    spawn(sysmon_srv, run_analysis, []),
    {noreply, State};
handle_cast({monitor, Tables}, State) ->
    io:format("process ~p monitoring mnesia debug events~n", [self()]),
    {noreply, _NewState} = handle_cast(unmonitor, State),
    mnesia:set_debug_level(debug),
    ok = subscribe(Tables),
    mnesia:subscribe(system),
    {noreply, #state{subs = Tables}};
handle_cast(unmonitor, State) ->
    mnesia:set_debug_level(none),
    ok = unsubscribe(State#state.subs),
    mnesia:unsubscribe(system),
    {noreply, State#state{subs = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    log(Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_tables(Site) ->
    make_t2(?tables, Site, []).

make_t2([], _Site, Acc) -> Acc;
make_t2([H | T], Site, Acc) ->
    Table = hn_util:site_to_atom(Site, H),
    make_t2(T, Site, [Table | Acc]).

subscribe([]) -> ok;
subscribe([H | T]) ->
    _Ret = mnesia:subscribe({table, H, detailed}),
    subscribe(T).

unsubscribe([]) -> ok;
unsubscribe([H | T]) ->
    _Ret = mnesia:subscribe({table, H, detailed}),
    unsubscribe(T).

log({mnesia_system_event,
     {mnesia_info, "Transaction log dump" ++ _Rest, _}}) ->
        ok;
log({mnesia_system_event,
     {mnesia_info, "Restarting transaction" ++ _Rest,
      [{tid, Tid, Pid}, _, Trans]}}) ->
    {cyclic, _Node, {Table, Lock}, Type1, Type2,
     {tid, OldTid, OldPid}} = Trans,
    Msg = io_lib:format("Restarting Transaction ~w;", [Trans]),
    Log = #restart{tid = Tid, pid = pid_to_list(Pid),
                         type = "Restarting Transaction",
                         dump = Msg,
                         lock = {Lock, Type1, Type2},
                         table = Table,
                         clashing_tid = OldTid,
                         clashing_pid = pid_to_list(OldPid)},
    syslib:log_term(Log, ?log),
    ok;
log({mnesia_system_event,
     {mnesia_user, {report, Tid, Pid, {Fun_id, Fun_inst}}}}) ->
    Log = #api{tid = Tid, pid = Pid, fn_id = Fun_id,
                      fn_instance = Fun_inst},
    syslib:log_term(Log, ?log),
    ok;
log({mnesia_system_event,
     {mnesia_user, {measurement, {Fun_id, Fun_inst}, Time}}}) ->
    Log = #api_call{fn_id = Fun_id, fn_instance = Fun_inst,
                     duration = Time},
    syslib:log_term(Log, ?log),
    ok;
log({mnesia_table_event,
     {Type, Table, _, _, {tid, Tid, Pid}}}) ->
    Log = #log{tid = Tid, pid = pid_to_list(Pid),
                      table = Table, type = Type},
    syslib:log_term(Log, ?log),
    ok;
log(Info) ->
    io:format("Info is ~p~n", [Info]),
    "erk".

dump_agg(#agg_stats{fn_id = Fn, total_calls = C,
                    total_logs = L, total_duration = T}) ->
    Msg = io_lib:format("~p,~p,~p,~p", [Fn, C, L, T]),
    io:format("x"),
    syslib:log(Msg, ?api_stats ++ "agg.log").

dump_api(#api_call{timestamp = Ts, fn_id = Fn_id, duration = D,
                   stats = #stats{no_calls = C, no_logs = L}}) ->
    Msg = io_lib:format("~p,~p,~p,~p,~p", [Ts, Fn_id, D, C, L]),
    io:format("d"),
    syslib:log(Msg, ?api_stats ++ Fn_id ++ ".log").

make_stats([], Acc) -> sort_calls(Acc);
make_stats([#api_call{restarts = R, logs = L} = H | T], Acc) ->
    io:format("q"),
    NoCalls = length(R) + 1,
    NoLogs = length(L),
    Stats = #stats{no_calls = NoCalls, no_logs = NoLogs},
    NewH = H#api_call{stats = Stats},
    make_stats(T, [NewH | Acc]).

sort_calls(List) ->
    Fun = fun(A, B) ->
                  if
                     A#api_call.fn_id == B#api_call.fn_id ->
                         if
                             A#api_call.timestamp <  B#api_call.timestamp ->
                                 true;
                             A#api_call.timestamp >= B#api_call.timestamp ->
                                 false
                         end;
                     A#api_call.fn_id <  B#api_call.fn_id ->
                         true;
                     A#api_call.fn_id >  B#api_call.fn_id ->
                         false
                 end
          end,
    lists:sort(Fun, List).

do_analysis([], Logs, ApiCalls, ApiTimings, Restarts) ->
    io:format("First Reconciliation~n"),
    Apis = reconcile(ApiTimings, ApiCalls, []),
    io:format("~nSecond Reconciliation~n"),
    Apis2 = reconcile2(Apis, Restarts, []),
    io:format("~nThird Reconciliation~n"),
    reconcile3(Apis2, Logs, []);
do_analysis([#api{} = H | T], Logs, ApiCalls, ApiTimings, Restarts) ->
    do_analysis(T, Logs, [H | ApiCalls], ApiTimings, Restarts);
do_analysis([#api_call{} = H | T], Logs, ApiCalls,
            ApiTimings, Restarts) ->
    do_analysis(T, Logs, ApiCalls, [H | ApiTimings], Restarts);
do_analysis([#log{} = H | T], Logs, ApiCalls, ApiTimings, Restarts) ->
    do_analysis(T, [H | Logs], ApiCalls, ApiTimings, Restarts);
do_analysis([#restart{} = H | T], Logs, ApiCalls, ApiTimings, Restarts) ->
    do_analysis(T, Logs, ApiCalls, ApiTimings, [H | Restarts]).

reconcile([], [], Acc) -> Acc;
reconcile([], Calls, Acc) ->
    io:format("Warning additional calls:~n~p~n", [Calls]),
    Acc;
reconcile([H | Timings], Calls, Acc) ->
    io:format("e"),
    Inst = H#api_call.fn_instance,
    Reps = H#api_call.reports,
    {NewTimings, NewCs, NewAcc}
        = case lists:keytake(Inst, 6, Calls) of
              false ->
                  Tid = get_tid(hd(H#api_call.reports)),
                  {Timings, Calls, [H#api_call{tid = Tid} | Acc]};
              {value, Call, NewCalls} ->
                  NewH = H#api_call{reports = [Call | Reps]},
                  {[NewH | Timings], NewCalls, Acc}
          end,
    reconcile(NewTimings, NewCs, NewAcc).

reconcile2([], [], Acc) -> Acc;
reconcile2([], Restarts, Acc) ->
    io:format("Warning additional restarts~n~p~n", [Restarts]),
    Acc;
reconcile2([H | Apis], Restarts, Acc) ->
    io:format("p"),
    Tid = H#api_call.tid,
    Rs  = H#api_call.restarts,
    {NewApis, NewRestarts, NewAcc}
        = case lists:keytake(Tid, 3, Restarts) of
              false ->
                  {Apis, Restarts, [H | Acc]};
              {value, Restart, NewRs} ->
                  NewH = H#api_call{restarts = [Restart | Rs]},
                  {[NewH | Apis], NewRs, Acc}
          end,
    reconcile2(NewApis, NewRestarts, NewAcc).

reconcile3([], [], Acc) -> Acc;
reconcile3([], Logs, Acc) ->
    io:format("WARNING!~nSome Logs unreconciled~n~p~n", [Logs]),
    Acc;
reconcile3([H | Apis], Logs, Acc) ->
    Tid = H#api_call.tid,
    Ls  = H#api_call.logs,
    {NewApis, NewLogs, NewAcc}
        = case lists:keytake(Tid, 3, Logs) of
              false ->
                  {Apis, Logs, [H | Acc]};
              {value, Log, NewLs} ->
                  NewH = H#api_call{logs = [Log | Ls]},
                  {[NewH | Apis], NewLs, Acc}
          end,
    reconcile3(NewApis, NewLogs, NewAcc).

get_tid(#api{tid = Tid}) -> Tid.

run_analysis() ->
    io:format("Consulting terms, please wait...~n"),
    Terms = syslib:consult_term_log(?log),
    io:format("Analysing terms.~n"),
    Records = do_analysis(Terms, [], [], [], []),
    io:format("~nMaking stats:"),
    Records2 = make_stats(Records, []),
    io:format("~nWriting stats out:"),
    lists:foreach(fun dump_api/1, Records2),
    Records3 = aggregate(Records2, #agg_stats{}, []),
    io:format("~nWriting aggregate stats:"),
    lists:foreach(fun dump_agg/1, Records3),
    io:format("~nAnalysis complete!~n"),
    ok.

aggregate([], _Agg, Acc) -> Acc;
aggregate([#api_call{fn_id = Fn_id} = H | T],
          #agg_stats{fn_id = Fn_id} = Agg, Acc) ->
    #api_call{duration = D1, stats = #stats{no_calls = C1, no_logs = L1}} = H,
    #agg_stats{total_calls = C2, total_logs = L2, total_duration = D2} = Agg,
    NewAgg = Agg#agg_stats{total_calls = C1 + C2, total_logs = L1 + L2,
                        total_duration = tconv:to_f(D1) + D2},
    aggregate(T, NewAgg, Acc);
% first time out initialise #agg_stats
aggregate([#api_call{fn_id = Fn_id} = H | T],
          #agg_stats{fn_id = undefined}, Acc) ->
    NewAgg = #agg_stats{fn_id = Fn_id},
    aggregate([H | T], NewAgg, Acc);
% change of api_call, so throw old aggregate onto the acc
aggregate([#api_call{fn_id = Fn_id} = H | T], Agg, Acc) ->
    NewAgg = #agg_stats{fn_id = Fn_id},
    aggregate([H | T], NewAgg, [Agg | Acc]).
