%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Introspection server.

-module(introspection_srv).
-behaviour(gen_server).

-export([take_snapshot/0]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(lists, [append/2, filter/2, flatten/1, foldl/3, map/2, foreach/2]).

-record(state, {
          file_id % id of the logfile
         }).
-define(SERVER, ?MODULE).

%%% TODO:  Accept messages from system_monitor().
%%% TODO:  Use disk_log.

%%% Properties to be read for each process when a snapshot is taken:
-define(SNAPSHOT_PROPERTIES, [total_heap_size]).
-define(LOGFILE, "watcher.log").


take_snapshot() ->
    gen_server:call(?SERVER, take_snapshot).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% Init -- open logfile.

init([]) ->
    {ok, FileId} = file:open(?LOGFILE, [append]),
    {ok, #state{file_id = FileId}}.


%%% Handle call messages.

handle_call(take_snapshot, _From, S) ->
    LogLines = snapshot(),
    append_to_log(LogLines, S#state.file_id),
    {reply, ok, S}.


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


%%% @doc Take snapshot of all running processes.

snapshot() ->
    {Day, Time} = calendar:now_to_universal_time(erlang:now()),
    Snapshot = get_system_info(),
    LogLines = foldl(fun({ProcName, Props}, Acc) ->
                             Lines = foldl(fun({PropName, PropValue}, Acc1) ->
                                                   Date = dh_date:format("d/m/Y H:m:s", {Day, Time}),
                                                   [{Date, ProcName, PropName, PropValue}|Acc1]
                                           end,
                                           [],
                                           Props),
                             append(Acc, Lines)
                     end,
                     [],
                     Snapshot),
    LogLines.



%%% @doc Return proplist where values are proplists describing the state of
%%%      all running processes.

get_system_info() ->
    Pids = filter(fun erlang:is_process_alive/1, erlang:processes()),
    Info = map(fun(P) ->
                       Info = erlang:process_info(P),
                       Properties = [{Prop, proplists:get_value(Prop, Info)} ||
                                        Prop  <- ?SNAPSHOT_PROPERTIES],
                       Name = case proplists:get_value(registered_name, Info) of
                                  undefined -> erlang:pid_to_list(P);
                                  V         -> atom_to_list(V)
                              end,
                       {Name, Properties}
               end,
               Pids),
    Info.


%%% @doc Add an entry to the logfile.  Each entry is a tuple of:
%%%      {Date = string(), ProcessId = string(), Property = atom(), Value = any()}

append_to_log(LogLines, FileId) ->
    foreach(fun({Date, ProcId, Prop, Val}) ->
                    Str = flatten(
                            io_lib:format("~s - ~s - ~p - ~p~n", [Date, ProcId, Prop, Val])),
                    file:write(FileId, list_to_binary(Str))
            end,
            LogLines).
