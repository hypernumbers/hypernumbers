-module(dirty_sup).

-behaviour(supervisor_bridge).

%% API
-export([start_link/2,
        listen_dirty_queue/2,
         enqueue_work/3]).

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

enqueue_work(Site, Type, Entry) ->
    Id = hn_util:site_to_atom(Site, atom_to_list(Type)++"work"),
    Id ! {new_work, Entry}.

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
    Id = hn_util:site_to_atom(Site, atom_to_list(Type)++"work"),
    register(Id, Pid),
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
    %% mnesia:subscribe({table, Table, simple}),
    %%Q = fill_queue(hn_workq:new(nil, 0), Table),
    listen_dirty_queue(Site, Table).

%% -spec listen_dirty_queue(string(), atom(), hn_workq:work_queue()) 
%%                         -> no_return().
listen_dirty_queue(Site, Table) ->
    receive 
        {new_work, #dirty_queue{dirty = Dirty, auth_req = AReq}} ->
            process_work(Site, Dirty, AReq)
    end,
    ?MODULE:listen_dirty_queue(Site, Table).

process_work(Site, Dirty, AReq) ->
    Graph = recalc_graph(Site, Dirty),
    Plan = digraph_utils:topsort(Graph),
    process_cells(Plan, Site, Graph, AReq),
    digraph:delete(Graph).

process_cells([], _, _, _) ->
    ok;
process_cells([C | T], Site, Graph, AReq) ->
    case digraph:vertex(Graph, C) of
        false ->
            process_cells(T, Site, Graph, AReq);
        _ ->
            Fixed = hn_db_api:handle_dirty_cell(Site, C, AReq),
            case Fixed of 
                true -> prune_path(digraph:out_neighbours(Graph, C), Graph);
                false -> ok
            end,
            process_cells(T, Site, Graph, AReq)
    end.
          
prune_path([], _Graph) ->
    ok;
prune_path([V|Rest], Graph) ->
    case digraph:in_degree(Graph, V) of
        1 -> 
            %% Vertex only has the fixed parent.
            Out = digraph:out_neighbours(Graph, V),
            prune_path(Out, Graph),
            digraph:del_vertex(Graph, V);
        _ ->
            %% Has other parents, halt walking path
            ok
    end,
    prune_path(Rest, Graph).
                
    %% case hn_workq:is_empty(Q) of
    %%     true -> clear_dirty_queue(Q, Table);
    %%     _Else -> ok
    %% end,
    %% Q2 = merge_latest(Q, Table),
    %% QNext = case hn_workq:next(Q2) of
    %%             {empty, Q3} ->
    %%                 Q3;
    %%             {DirtyCellIdx, Ar, Q3} ->
    %%                 hn_db_api:handle_dirty_cell(Site, DirtyCellIdx, Ar),
    %%                 Q3
%%         end,
%%?MODULE:listen_dirty_queue(Site, Table, QNext).


%% Checks if new work queues are available. When work is available
%% *immediately*, the result of all the current and new workqueues are
%% merged and returned. When the current queue is empty and new work
%% is not available, we wait for new work to arrive. Otherwise we
%% simply return the current queue if it is not empty and no new work
%% is available.
%% -spec merge_latest(hn_workq:work_queue(), atom()) -> hn_workq:work_queue().
%% merge_latest(Q, Table) ->    
%%     Wait = case hn_workq:is_empty(Q) of 
%%                true  -> infinity;
%%                false -> 0 end,
%%     receive 
%%         {mnesia_table_event, {write, _, _}} ->
%%             fill_queue(Q, Table);
%%         _Other ->
%%             merge_latest(Q, Table)
%%     after Wait ->
%%             Q
%%     end.

%% %% Merges all new work into the current queue. 
%% -spec fill_queue(hn_workq:work_queue(), atom()) -> hn_workq:work_queue(). 
%% fill_queue(Q, Table) ->
%%     Id = hn_workq:id(Q),
%%     M = ets:fun2ms(fun(#dirty_queue{id = T, queue = NQ}) 
%%                          when T > Id -> NQ 
%%                    end),
%%     F = fun() -> mnesia:select(Table, M, read) end,
%%     {atomic, Qs} = mnesia:transaction(F),
%%     hn_workq:merge(Q, Qs).

%% %% Clears out process work from the dirty_queue table.
%% -spec clear_dirty_queue(hn_workq:work_queue(), atom()) -> ok.
%% clear_dirty_queue(Q, Table) ->
%%     Id = hn_workq:id(Q),
%%     M = ets:fun2ms(fun(#dirty_queue{id = T}) when T =< Id -> T end), 
%%     F = fun() ->
%%                 Keys = mnesia:select(Table, M, write),
%%                 [mnesia:delete(Table, K, write) || K <- Keys],
%%                 ok
%%         end,
%%     {atomic, ok} = mnesia:transaction(F),
%%     ok.

%% Recursively walk child relation, adding entries into the work
%% queue.  
-spec recalc_graph(string(), [cellidx()]) -> digraph(). 
recalc_graph(Site, Idxs) ->
    Tbl = hn_db_wu:trans(Site, relation),
    Graph = digraph:new([private]),
    recalc_graph(Idxs, Tbl, Graph).

-spec recalc_graph([cellidx()], atom(), digraph()) -> digraph().
recalc_graph([], _Tbl, Graph) ->
    Graph;
recalc_graph([Idx|Rest], Tbl, Graph) ->
    case digraph:vertex(Graph, Idx) of
        false ->
            digraph:add_vertex(Graph, Idx),
            case mnesia:dirty_read(Tbl, Idx) of
                [R] ->
                    Children = ordsets:to_list(R#relation.children),
                    recalc_graph(Children, Tbl, Graph),
                    [digraph:add_edge(Graph, Idx, C) || C <- Children];
                _ -> 
                    ok
            end;
        _ ->
            ok
    end,
    recalc_graph(Rest, Tbl, Graph).


