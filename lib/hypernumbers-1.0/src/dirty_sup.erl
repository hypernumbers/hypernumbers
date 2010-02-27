-module(dirty_sup).

-behaviour(supervisor_bridge).

%% API
-export([start_link/2,
        listen_dirty_queue/3]).

%% supervisor_bridge callbacks
-export([init/1, terminate/2]).

-include("hypernumbers.hrl").
-include("spriki.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {table :: atom(),
                pid :: pid()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor bridge
%%--------------------------------------------------------------------
-spec start_link(string(), atom()) -> {ok,pid()} | ignore | {error,any()}.
start_link(Site, Type) ->
    Id = hn_util:site_to_atom(Site, atom_to_list(Type)),
    supervisor_bridge:start_link({local, Id}, ?MODULE, [Site, Type]).

%%====================================================================
%% supervisor_bridge callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Funcion: init(Args) -> {ok,  Pid, State} |
%%                        ignore            |
%%                        {error, Reason}    
%% Description:Creates a supervisor_bridge process, linked to the calling
%% process, which calls Module:init/1 to start the subsystem. To ensure a
%% synchronized start-up procedure, this function does not return until
%% Module:init/1 has returned. 
%%--------------------------------------------------------------------
init([Site, Type]) ->
    Table = hn_db_wu:trans(Site, Type),
    Pid = spawn_link(fun() -> 
                             listen_dirty_queue_init(Site, Table) 
                     end),
    {ok, Pid, #state{table = Table, pid = Pid}}.

%%--------------------------------------------------------------------
%% Func: terminate(Reason, State) -> void()
%% Description:This function is called by the supervisor_bridge when it is
%% about to terminate. It should be the opposite of Module:init/1 and stop
%% the subsystem and do any necessary cleaning up.The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{table = T}) ->
    mnesia:unsubscribe({table, T, simple}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec listen_dirty_queue_init(string(), atom()) -> no_return(). 
listen_dirty_queue_init(Site, Table) ->
    mnesia:subscribe({table, Table, simple}),
    Q = fill_queue(hn_workq:new(nil, 0), Table),
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
                    Q3;
                {DirtyCellIdx, Ar, Q3} ->
                    hn_db_api:handle_dirty_cell(Site, DirtyCellIdx, Ar),
                    Q3
            end,
    ?MODULE:listen_dirty_queue(Site, Table, QNext).


%% Checks if new work queues are available. When work is available
%% *immediately*, the result of all the current and new workqueues are
%% merged and returned. When the current queue is empty and new work
%% is not available, we wait for new work to arrive. Otherwise we
%% simply return the current queue if it is not empty and no new work
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


%%%
%%%  Code from the old dirty_srv. Kept for reference.
%%%

%% listen(Site, Table) ->
%%     case mnesia:activity(transaction, fun read_table/2, [Site, Table]) of
%%         ok ->
%%             %mnesia_recover:allow_garb(),
%%             %mnesia_recover:start_garb(),
%%             ok;
%%         no_dirty_cells ->
%%             receive _X ->
%%                     mnesia:unsubscribe({table, Table, simple})
%%             end
%%     end,
%%     ?MODULE:listen(Site, Table).

%% -spec read_table(string(), atom()) -> ok.
%% read_table(Site, Table) ->
%%     case mnesia:first(Table) of
%%         '$end_of_table' ->
%%             mnesia:subscribe({table, Table, simple}),
%%             no_dirty_cells;
%%         Id ->
%%             %% Eugh, shouldnt hangle missing cells
%%             case mnesia:read(Table, Id, write) of
%%                 [Rec] ->
%%                     ok = mnesia:delete(Table, Id, write),
%%                     proc_dirty(Rec, Site);
%%                 _ ->
%%                     ok
%%             end
%%     end.

%% proc_dirty(Rec, Site) when is_record(Rec, dirty_inc_hn_create) ->
%%     hn_db_api:notify_back_create(Site, Rec);
%% proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_in) ->
%%     hn_db_api:handle_dirty(Site, Rec);
%% proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_out) ->
%%     #dirty_notify_out{delay = D} = Rec,
%%     ok = timer:sleep(D),
%%     hn_db_api:handle_dirty(Site, Rec);
%% proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_back_in) ->
%%     hn_db_api:handle_dirty(Site, Rec);
%% proc_dirty(Rec, Site) when is_record(Rec, dirty_notify_back_out) ->
%%     hn_db_api:handle_dirty(Site, Rec).


%% report_error(Pid, Reason, State) ->
%%     Table = proplists:get_value(Pid, State#state.children, undefined),
%%     {ok, Id} = mnesia:activity(transaction, fun delete_first/1, [Table]),
    
%%     ?ERROR(" Process ~p died in ~p ~p ~n Error: ~p~n Stacktrace: ~p",
%%            [Pid, Table, Id, Reason, erlang:get_stacktrace()]),
%%     ok.
    
%% delete_first(Table) ->
%%     Id = mnesia:first(Table),
%%     ok = mnesia:delete(Table, Id, write),
%%     {ok, Id}.
