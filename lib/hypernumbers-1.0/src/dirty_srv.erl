%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @TODO write a proper module description
-module(dirty_srv).
-behaviour(gen_server).

-export([start_link/1,
         init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2,
         terminate/2,
         listen_dirty_queue/3,
         code_change/3]).

-export([start/1, stop/0, listen/2]).

-include("handy_macros.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


-type listener() :: {pid(), string()}.
-record(state, {table = undefined :: atom(), 
                children = [] :: [listener()] }).


%%%
%%% API
%%% 

-spec start(string()) -> ok.
start(Site) ->
    [ok = gen_server:call(D, {start, Site}) || D <- dirty_tbls()],
    ok.

-spec stop() -> ok. 
stop() ->
    [ok = gen_server:call(D, stop) || D <- dirty_tbls()],
    ok.

%% @spec start_link(Arg) -> StartLink
%% @doc  start a link between this and supervisor
-spec start_link(atom()) -> {ok, pid()}. 
start_link(Arg) ->
    {ok, _Pid} = gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).


%%%
%%% Internal
%%% 

%% @spec init(Arg) -> {ok, State}
%% @doc  Start server
init([Type]) ->
    process_flag(trap_exit, true),
    {ok, #state{table = Type}}.

handle_info({'EXIT', Pid, stopping}, State) ->
    F = fun({NPid, _Tbl}) -> NPid =/= Pid end, 
    NChild = lists:filter(F, State#state.children),
    {noreply, State#state{children = NChild}};

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
        killed -> ok;
        _Else  -> report_error(Pid, Reason, State)
    end,
    NChild = restart_children(State#state.children, 
                              Pid, 
                              State#state.table, 
                              []),
    {noreply, State#state{children = NChild}};

handle_info(_Info, State) ->
    {noreply, State}.
handle_cast(_Info, State) ->
    {noreply, State}.

%% @doc  subscribe to table events from mnesia
-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}}.
handle_call({start, Site},  _From, State) ->
    Child = case lists:keysearch(Site, 2, State#state.children) of
                false -> 
                    {start_listen(State#state.table, Site), Site};
                {value, Tuple} -> 
                    Tuple
            end,
    NState = State#state{children = [Child | State#state.children]},
    {reply, ok, NState};

handle_call(stop,  _From, State) ->
    [ exit(Pid, stopping) ||
        {Pid, _Table} <- State#state.children],    
    {reply, ok, State#state{children=[]}};

handle_call(_Msg, _From, _State) ->
    {reply, ok, []}.

terminate(_Reason, State) ->
    [ exit(Pid, stopping) || {Pid, _Table} <- State#state.children],
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


-spec start_listen(atom(), string()) -> pid(). 
start_listen(dirty_queue=T, Site) ->
    Table = hn_db_wu:trans(Site, T),
    spawn_link(fun() -> listen_dirty_queue_init(Site, Table) end);
start_listen(T, Site) ->
    Table = hn_db_wu:trans(Site, T),
    spawn_link(fun() -> listen(Site, Table) end).

-spec restart_children([listener()], pid(), atom(), [listener()]) 
                      -> [listener()].
restart_children([], _Pid, _T, Acc) ->
    Acc;
restart_children([{Pid, Site} | Tl], Pid, T, Acc) ->
    restart_children(Tl, Pid, T, [{start_listen(T, Site), Site} | Acc]);
restart_children([Hd | Tl], Pid, T, Acc) ->
    restart_children(Tl, Pid, T, [Hd | Acc]).

-spec listen_dirty_queue_init(string(), atom()) -> no_return(). 
listen_dirty_queue_init(Site, Table) ->
    mnesia:subscribe({table, Table, simple}),
    Q = fill_queue(hn_workq:new(0), Table),
    listen_dirty_queue(Site, Table, Q).

-spec listen_dirty_queue(string(), atom(), hn_workq:work_queue()) 
                        -> no_return().
listen_dirty_queue(Site, Table, Q) ->
    case hn_workq:is_empty(Q) of
        true -> clear_dirty_queue(Q, Table);
        _Else -> ok
    end,
    Q2 = merge_latest(Q, Table),
    QNext = case hn_workq:next(Q2) of
                {empty, Q3} ->
                    %% shouldn't happen
                    Q3;
                {DirtyCellIdx, Q3} ->
                    hn_db_api:handle_dirty_cell(Site, DirtyCellIdx),
                    Q3
            end,
    ?MODULE:listen_dirty_queue(Site, Table, QNext).


%% Checks if new work queues are available. When work is available
%% *immediately*, the result of all the current and new workqueues are
%% merged and returned. When the current queue is empty and new work
%% is not available, we wait for new work to arrive. Otherwise we
%% simply return the current queue if it is not empty, and no new work
%% is available.
-spec merge_latest(hn_workq:work_queue(), atom()) -> hn_workq:work_queue().
merge_latest(Q, Table) ->    
    Wait = case hn_workq:is_empty(Q) of 
               true  -> infinity;
               false -> 0 end,
    receive 
        {mnesia_table_event, {write, _, _}} ->
            fill_queue(Q, Table);
        _Other ->
            merge_latest(Q, Table)
    after Wait ->
            Q
    end.

%% Merges all new work into the current queue. 
-spec fill_queue(hn_workq:work_queue(), atom()) -> hn_workq:work_queue(). 
fill_queue(Q, Table) ->
    Id = hn_workq:id(Q),
    M = ets:fun2ms(fun(#dirty_queue{id = T, queue = NQ}) when T > Id -> NQ end),
    F = fun() -> mnesia:select(Table, M, read) end,
    {atomic, Qs} = mnesia:transaction(F),
    hn_workq:merge(Q, Qs).

%% Clears out process work from the dirty_queue table.
-spec clear_dirty_queue(hn_workq:work_queue(), atom()) -> ok.
clear_dirty_queue(Q, Table) ->
    Id = hn_workq:id(Q),
    M = ets:fun2ms(fun(#dirty_queue{id = T}) when T =< Id -> T end), 
    F = fun() ->
                Keys = mnesia:select(Table, M, write),
                [mnesia:delete(Table, K, write) || K <- Keys],
                ok
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.
    
listen(Site, Table) ->
    case mnesia:activity(transaction, fun read_table/2, [Site, Table]) of
        ok ->
            %mnesia_recover:allow_garb(),
            %mnesia_recover:start_garb(),
            ok;
        no_dirty_cells ->
            receive _X ->
                    mnesia:unsubscribe({table, Table, simple})
            end
    end,
    ?MODULE:listen(Site, Table).

-spec read_table(string(), atom()) -> ok.
read_table(Site, Table) ->
    case mnesia:first(Table) of
        '$end_of_table' ->
            mnesia:subscribe({table, Table, simple}),
            no_dirty_cells;
        Id ->
            %% Eugh, shouldnt hangle missing cells
            case mnesia:read(Table, Id, write) of
                [Rec] ->
                    ok = mnesia:delete(Table, Id, write),
                    proc_dirty(Rec, Site);
                _ ->
                    ok
            end
    end.

proc_dirty(Rec, Site) when is_record(Rec, dirty_inc_hn_create) ->
    hn_db_api:notify_back_create(Site, Rec);
proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_in) ->
    hn_db_api:handle_dirty(Site, Rec);
proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_out) ->
    #dirty_notify_out{delay = D} = Rec,
    ok = timer:sleep(D),
    hn_db_api:handle_dirty(Site, Rec);
proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_back_in) ->
    hn_db_api:handle_dirty(Site, Rec);
proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_back_out) ->
    hn_db_api:handle_dirty(Site, Rec).


report_error(Pid, Reason, State) ->
    Table = proplists:get_value(Pid, State#state.children, undefined),
    {ok, Id} = mnesia:activity(transaction, fun delete_first/1, [Table]),
    
    ?ERROR(" Process ~p died in ~p ~p ~n Error: ~p~n Stacktrace: ~p",
           [Pid, Table, Id, Reason, erlang:get_stacktrace()]),
    ok.
    
delete_first(Table) ->
    Id = mnesia:first(Table),
    ok = mnesia:delete(Table, Id, write),
    {ok, Id}.

-spec dirty_tbls() -> [atom()]. 
dirty_tbls() ->
    [ dirty_queue,
      dirty_notify_in,
      dirty_notify_back_in,
      dirty_inc_hn_create,
      dirty_notify_out,
      dirty_notify_back_out ].
