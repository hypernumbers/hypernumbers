%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Introspection server.

-module(introspection_srv).
-behaviour(gen_server).

%%% introspection server API
-export([take_snapshot/0, take_snapshot/1, toggle_mode/1, set_top/1]).

%%% API for internal use only
-export([tick/1]).

%%% gen_server API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(lists, [append/2, filter/2, flatten/1, foldl/3, map/2, foreach/2, 
                keysearch/3, sort/2, sublist/2]).

-record(state, {
          file_id,        % id of the logfile
          mode = passive, % state can be active or passive
          top = all       % what the introspector should measure
         }).
-define(SERVER, ?MODULE).

%%% TODO:  Accept messages from system_monitor().
%%% TODO:  Use disk_log.

%%% Properties to be read for each process when a snapshot is taken:
-define(SNAPSHOT_PROPERTIES, [registered_name, total_heap_size, current_function, 
                              initial_call]).
                              %backtrace, initial_call]).
%%% Name of the default log file
-define(LOGFILE, "watcher.log").
%%% How often the logger should snapshot the state in active mode
-define(CLOCKTICK, 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% gen_server API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% Init -- open logfile.

init([]) ->
    {ok, FileId} = file:open(?LOGFILE, [append]),
    {ok, #state{file_id = FileId}}.


%%% Handle call messages.

handle_call({set_top, N}, _From, S) ->
    NewS = S#state{top = N},
    {reply, ok, NewS};
    
handle_call({toggle_mode, Mode}, _From, S) ->
    case {Mode, S#state.mode} of
        {active, passive} -> spawn(?SERVER, tick, [S#state.top]);
        _                 -> ok
    end,
    NewS = S#state{mode = Mode},
    {reply, ok, NewS};
handle_call({take_snapshot, N}, _From, S) ->
    LogLines = snapshot(N),
    append_to_log(LogLines, S#state.file_id),
    case S#state.mode of
        active  -> spawn(?SERVER, tick, [N]);
        passive -> ok
    end,
    {reply, ok, S#state{top = N}}.

%%% Handle cast messages.

handle_cast(_Msg, State) ->
    {noreply, State}.


%%% Handle non-call/cast messages.

handle_info(_Info, State) ->
  {noreply, State}.


%%% Terminate -- close the logfile.

terminate(_Reason, S) ->
    ok = file:close(S#state.file_id),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_top(N) when is_integer(N) andalso (N > 0) ->
    gen_server:call(?SERVER, {set_top, N}).    

take_snapshot(N) when (is_integer(N) andalso (N > 0)) orelse (N == all) ->
    gen_server:call(?SERVER, {take_snapshot, N}).
    
take_snapshot() ->
    gen_server:call(?SERVER, {take_snapshot, all}).

toggle_mode(Mode) when (Mode == active) orelse (Mode == passive) ->
    gen_server:call(?SERVER, {toggle_mode, Mode}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tick(N) -> 
    timer:sleep(?CLOCKTICK),
    take_snapshot(N).
    
%%% @doc Take snapshot of all running processes.    

snapshot(N) ->
    {Day, Time} = calendar:now_to_universal_time(erlang:now()),
    Snapshot = get_system_info(),
    TopSnapshot = case N of
                      all -> Snapshot;
                      _   -> top(Snapshot, N)
                  end,
    Fun = fun({ProcName, Props}, Acc) ->
                  Lines = foldl(fun({PropName, PropValue}, Acc1) ->
                                        Date = dh_date:format("d/m/Y H:m:s", {Day, Time}),
                                        [{Date, ProcName, PropName, PropValue}|Acc1]
                                end,
                                [],
                                           Props),
                  append(Acc, Lines)
          end,

    foldl(Fun, [], TopSnapshot).


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
    
                 
                          
