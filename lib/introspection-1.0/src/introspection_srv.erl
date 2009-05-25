%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Introspection server.
%%% The Introspection Application starts in a passive mode and responds to the
%%% API function toggle_mode to switch it to active mode
%%% In passive mode it is used to take memory snapshots. In active mode it 
%%% takes a timeseries of snapshots based on the time specified in the macro
%%% CLOCKTICK.
%%% TODO: Accept messages from system_monitor().
%%% TODO: Use disk_log.
%%% TODO: Allow the clocktick to be set via the API
%%% TODO: If you manually call take_mem_snapshot on an active server
%%%       you will get two interleaved traces in the dump file...

-module(introspection_srv).
-behaviour(gen_server).

%%% introspection server API for memory
-export([
         take_mem_snapshot/0, 
         take_mem_snapshot/1, 
         set_top/1
        ]).

%%% introspection server API for mnesia
-export([
         start_mnesia_system_log/0,
         start_mnesia_system_log/1,
         start_table_logging/2,
         start_table_logging/1,
         take_mnesia_snapshot/0,
         stop_table_logging/1
        ]).

%%% introspection server API for ad-hoc logging
-export([
         ad_hoc/2
        ]).


%%% introspection server API for memory
-export([
         toggle_mode/1
        ]).


%%% API for internal use only
-export([tick/1]).

%%% gen_server API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(lists, [append/2, filter/2, flatten/1, foldl/3, map/2, foreach/2, 
                keysearch/3, sort/2, sublist/2]).

-record(memory, 
        {
          mode = passive, % state can be active or passive
          top  = all      % what the introspector should measure
        }).
-record(mnesia,
        {
          mode = passive,
          tables = []
         }).
-record(state, 
        {
          file_id,             % id of the logfile
          memory  = #memory{},
          mnesia  = #mnesia{}
       }).
-define(SERVER, ?MODULE).

%%% default logging level for mnesia
-define(DEFAULTLOGLEVEL, simple).

%%% Properties to be read for each process when a snapshot is taken:
-define(SNAPSHOT_PROPERTIES, [registered_name, total_heap_size, current_function, 
                              initial_call, message_queue_len]).
%%% Name of the default log file
-define(LOGFILE, "watcher.log").
%%% How often the logger should snapshot the state in active mode
-define(CLOCKTICK, 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% gen_server API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Init -- open logfile.
%% @hidden
init([]) ->
    {ok, FileId} = file:open(?LOGFILE, [append]),
    {ok, #state{file_id = FileId}}.


%% Handle call messages.
%% @hidden

%%%
%%% these handles pertain to mnesia
%%%

handle_call({start_mnesia_system_log}, _From, State) ->
    {ok, _} = mnesia:subscribe(system),
    Reply = ok,
    {reply, Reply, State};

handle_call({start_table_logging, NewTable, LogLevel}, _From,
            #state{mnesia = #mnesia{tables = Tables} = M} = State) ->
    ok = mnesia:wait_for_tables([NewTable], 1000),
    io:format("subscribing to ~p with Log Level of ~p~n", [NewTable, LogLevel]),
    {ok, _} = mnesia:subscribe({table, NewTable, LogLevel}),
    Reply = ok,
    {reply, Reply, State#state{mnesia = M#mnesia{tables = [NewTable | Tables]}}};

handle_call({stop_table_logging, Table}, _From, State) ->
    #state{mnesia = #mnesia{tables = Tables} = M} = State,
    ok = mnesia:unsubscribe({table, Table}),
    Reply = ok,
    NewTables = lists:delete(Table, Tables),
    {reply, Reply, State#state{mnesia = M#mnesia{tables = NewTables}}};

handle_call({take_mnesia_snapshot, N}, _From, S) ->
    LogLines = mnesia_snapshot(),
    io:format("in handle_call for mnesia_shapshot Loglines is ~p~n", 
              [LogLines]),
    append_to_log(LogLines, S#state.file_id),
    #state{memory = M} = S,
    {reply, ok, S#state{memory = M#memory{top = N}}};

%%
%% These handles pertain to memory
%%

handle_call({set_top, N}, _From, S) ->
    NewS = S#state{memory = #memory{top = N}},
    {reply, ok, NewS};

handle_call({take_mem_snapshot, N}, _From, S) ->
    LogLines = memory_snapshot(N),
    append_to_log(LogLines, S#state.file_id),
    #state{memory = M} = S,
    {reply, ok, S#state{memory = M#memory{top = N}}};

%%
%% This handle pertains to mode
%%

handle_call({toggle_mode, ModeList}, _From, S) ->
    #state{memory = Mem, mnesia = Mn} = S,
    NewMemMode = case lists:keysearch(memory, 1, ModeList) of
                     false                      -> passive;
                     {value, {memory, active}}  -> active;
                     {value, {memory, passive}} -> passive
                 end,
    NewMnesiaMode = case lists:keysearch(mnesia, 1, ModeList) of
                        false                      -> passive;
                        {value, {mnesia, active}}  -> active;
                        {value, {mnesia, passive}} -> passive
                 end,
    NewS = S#state{memory = Mem#memory{mode = NewMemMode}, 
                   mnesia = Mn#mnesia{mode = NewMnesiaMode}},
    if 
        (NewMemMode == active) orelse (NewMnesiaMode == active) ->
            io:format("in toggle_mode spawning tick...~n"),
            spawn_link(?SERVER, tick, [NewS]);
        (NewMemMode == passive) andalso (NewMnesiaMode == passive) ->
            io:format("in toggle_mode NOT spawning tick...~n"),
            ok
    end,
    {reply, ok, NewS}.


%% Handle cast messages.
%% @hidden
handle_cast({ad_hoc, {Id, Msg}}, State) ->
    Date = get_now(),    
    Line1 = {Date, "ad_hoc" ++ Id, Msg, Id},
    append_to_log([Line1], State#state.file_id),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% Handle non-call/cast messages.
%% @hidden

handle_info({mnesia_table_event, Msg}, State) -> 
    Date = get_now(),    
    {Oper, Table, Item, OldRecords, TID} = Msg,
    OldRecordSize = length(OldRecords),
    {tid, TIDNo, TIDPid} = TID,
    Line1 = {Date, mnesia_table_event, operation, Oper},
    Line2 = {Date, mnesia_table_event, table, Table},
    Line3 = {Date, mnesia_table_event, item, Item},
    Line4 = {Date, mnesia_table_event, old_record_size, OldRecordSize},
    Line5 = {Date, mnesia_table_event, transaction_id, TIDNo},
    Line6 = {Date, mnesia_table_event, transaction_pid, TIDPid},
    append_to_log([Line1, Line2, Line3, Line4, Line5, Line6], State#state.file_id),
    {noreply, State};

handle_info({mnesia_system_event, {mnesia_info, Format, Args}}, State) -> 
    TID = hd(Args),
    {tid, TIDNo, TIDPid} = TID,
    Str = io_lib:format(Format, Args),
    Date = get_now(),    
    Line1 = {Date, 'mnesia_system_event (info)', message, list_to_binary(Str)},
    Line2 = {Date, 'mnesia_system_event (info)', format, Format},
    Line3 = {Date, 'mnesia_system_event (info)', args, Args},
    Line4 = {Date, 'mnesia_system_event (info)', transaction_id, TIDNo},
    Line5 = {Date, 'mnesia_system_event (info)', transaction_pid, TIDPid},
    append_to_log([Line1, Line2, Line3, Line4, Line5], State#state.file_id),
    {noreply, State};

handle_info({mnesia_system_event, {mnesia_user, Term}}, State) -> 
    Date = get_now(),    
    Line1 = {Date, 'mnesia_system_event (user)', message, Term},
    append_to_log([Line1], State#state.file_id),
    {noreply, State};
    
handle_info(Info, State) ->
    io:format("In handle_info Info is ~p~n-State is ~p~n", [Info, State]),
    {noreply, State}.

%% Terminate -- close the logfile.
%% @hidden
terminate(_Reason, State) ->
    ok = file:close(State#state.file_id),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% API calls for mnesia logging
%%%

%% @spec start_mnesia_system_log() -> ok
%% @doc This starts the log with the default name in 
%% the current working directory
start_mnesia_system_log() ->
    gen_server:call(?MODULE, {start_mnesia_system_log}).

%% @spec start_mnesia_system_log(DebugLevel) -> ok
%% @doc This starts the log with the default name in 
%% the current working directory
start_mnesia_system_log(DebugLevel) ->
    set_mnesia_debug_level(DebugLevel),
    gen_server:call(?MODULE, {start_mnesia_system_log}).

%% @spec start_table_loggging(Table, LogLevel) -> ok
%% @doc stars logging the table with the log level
%% LogLevel is one of:
%% <ul>
%% <li>none</li>
%% <li>verbose</li>
%% <li>debug</li>
%% <li>trace</li>
%% <li>false</li>
%% <li>true</li>
%% </ul>
start_table_logging(Table, LogLevel)
  when (LogLevel == simple orelse LogLevel == detailed) ->
    gen_server:call(?MODULE, {start_table_logging, Table, LogLevel}).

%% @spec start_log(Table) -> ok
%% @doc starts logging the table with the default log level
start_table_logging(Table) ->
    gen_server:call(?MODULE, {start_table_logging, Table, ?DEFAULTLOGLEVEL}).

%% @spec stop_table_logging(Table, LogLevel) -> ok
%% @doc stops logging the table
stop_table_logging(Table) ->
    gen_server:call(?MODULE, {stop_table_logging, Table}).

%% @spec take_mnesia_snapshot() -> Return
%% @doc take_mnesia_snapshot/0 is the same as take_mem_snapshot(all).
take_mnesia_snapshot() ->
    gen_server:call(?SERVER, {take_mnesia_snapshot, all}).

set_mnesia_debug_level(Level) when (Level == none
                                    orelse Level == verbose
                                    orelse Level == debug
                                    orelse Level == trace
                                    orelse Level == false
                                    orelse Level == true) ->
    %% mnesia:set_debug_level/1 automatically subscribes the
    %% calling process to mnesia system events.
    %% We want to do that separately in the introspection server and
    %% don't want it as a side-effect in the shell so we spawn'n'die
    %% the subscription
    _Pid = spawn(mnesia, set_debug_level, [Level]).

%%%
%%% API calls for memory management
%%%

%% @spec set_top(N) -> Return
%% N = integer()
%% @doc set_top takes an integer value and retricts the operation of the snap
%% shot to the N process with the largest memory utilisation
set_top(N) when is_integer(N) andalso (N > 0) ->
    gen_server:call(?SERVER, {set_top, N}).    

%% @spec take_mem_snapshot(N) -> Return
%% N = [ integer() | all]
%% @doc take_mem_snapshot/1 takes a snapshot of the N most memory-intensive. 
%% If the atom 'all' is passed it it takes a snapshot of all the processes.
%% The value of N is stored so that (if the server is in active mode) 
%% it is the value used by the tick function
take_mem_snapshot(N) when (is_integer(N) andalso (N > 0)) orelse (N == all) ->
    gen_server:call(?SERVER, {take_mem_snapshot, N}).

%% @spec take_mem_snapshot() -> Return
%% @doc take_mem_snapshot/0 is the same as take_mem_snapshot(all).
take_mem_snapshot() ->
    gen_server:call(?SERVER, {take_mem_snapshot, all}).

%%
%% API calls for ad-hoc logging
%%

ad_hoc(Id, Msg) ->
    gen_server:cast(?SERVER, {ad_hoc, {Id, Msg}}).

%%
%% API calls for continuous logging
%%

%% @spec toggle_mode(Mode) -> Return
%% Mode = [active | passive]
%% a passive introspection server just responds to a single 
toggle_mode(ModeList) when is_list(ModeList) ->
    gen_server:call(?SERVER, {toggle_mode, ModeList}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @hidden
tick(State) -> 
    io:format("ticking...~n"),
    #state{memory = #memory{mode = MemMode, top = N}, 
           mnesia = #mnesia{mode = MnesiaMode}} = State,
    timer:sleep(?CLOCKTICK),
    case MemMode of
        active  -> MemoryLogLines = memory_snapshot(N),
                   append_to_log(MemoryLogLines, State#state.file_id);
        passive -> ok
    end,
    case MnesiaMode of
        active  -> MnesiaLogLines = mnesia_snapshot(),
                   append_to_log(MnesiaLogLines, State#state.file_id);
        passive -> ok
    end,
    tick(State).

%% @hidden
%% @doc Take snapshot of all running processes.    
memory_snapshot(N) ->
    Date = get_now(),
    Snapshot = get_system_info(),
    TopSnapshot = case N of
                      all -> Snapshot;
                      _   -> top(Snapshot, N)
                  end,
    Fun = fun({ProcName, Props}, Acc) ->
                  Lines = foldl(fun({PropName, PropValue}, Acc1) ->
                                        NewAcc = {Date, ProcName, 
                                                  PropName, PropValue},
                                        [NewAcc | Acc1]
                                end,
                                [],
                                Props),
                  append(Acc, Lines)
          end,

    foldl(Fun, [], TopSnapshot).

mnesia_snapshot() ->
    Date = get_now(),    
    Line1 = {Date, mnesia_snapshot, transactions, 
             mnesia:system_info(transactions)},
    Line2 = {Date, mnesia_snapshot, transactions_failures, 
             mnesia:system_info(transaction_failures)},
    Line3 = {Date, mnesia_snapshot, transactions_restarts, 
             mnesia:system_info(transaction_restarts)},
    Line4 = {Date, mnesia_snapshot, transactions_commits, 
             mnesia:system_info(transaction_commits)},
    Line5 = {Date, mnesia_snapshot, lock_queue_length, 
             length(mnesia:system_info(lock_queue))},
    Line6 = {Date, mnesia_snapshot, no_of_ets_tables,
             length(ets:all())},
    [Line1, Line2, Line3, Line4, Line5, Line6].
    

%%% @doc Return proplist where values are proplists describing the state of
%%%      all running processes.
get_system_info() ->
    Pids = filter(fun erlang:is_process_alive/1, erlang:processes()),
    Fun = 
        fun(P) ->
                Info = erlang:process_info(P, ?SNAPSHOT_PROPERTIES),
                case Info of
                    undefined -> {erlang:pid_to_list(P), [{pid, deleted}]};
                    _         ->
                        Name
                            = case erlang:process_info(P, [registered_name]) of
                                  [{registered_name, []}] -> erlang:pid_to_list(P);
                                  [{registered_name, V}]  -> atom_to_list(V);
                                  Other                   -> 
                                      io_lib:format("duff name: ~p", [Other])
                              end,
                        {Name, Info}
                end
        end,
    map(Fun, Pids).


%%% @doc Add an entry to the logfile.  Each entry is a tuple of:
%%%      {Date = string(), ProcessId = string(), Property = atom(), Value = any()}
append_to_log(LogLines, FileId) ->
    foreach(fun({Date, ProcId, Prop, Val}) ->
                    Str = flatten(
                            io_lib:format("~s - ~s - ~p - ~p~n", 
                                          [Date, ProcId, Prop, Val])),
                    ok = file:write(FileId, list_to_binary(Str))
            end,
            LogLines).

top(Snapshot, N) when is_integer(N) andalso (N > 0) ->
    Fun = 
        fun(A, B) ->
                {_Name1, L1} = A,
                {_Name2, L2} = B,
                R1 = keysearch(total_heap_size, 1, L1),
                R2 = keysearch(total_heap_size, 1, L2),
                if 
                    R1 == false -> false;
                    R2 == false -> true;
                    true ->
                        {value, {_, S1}} = R1,
                        {value, {_, S2}} = R2,
                        if 
                            S1 >  S2  -> true;
                            S1 =< S2 -> false
                        end
                end
        end,
    Sorted = lists:sort(Fun, Snapshot),
    sublist(Sorted, N).

get_timestamp()->
    {Mega, Sec, Micro} = now(),
    1000000000000 * Mega + 1000000 * Sec + Micro.

get_now() ->
    %% {Day, Time} = calendar:now_to_universal_time(erlang:now()),
    %% Date = dh_date:format("d/m/Y H:m:s", {Day, Time}),
    integer_to_list(get_timestamp()).
    


