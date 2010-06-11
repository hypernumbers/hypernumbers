-module(dirty_sup).

-behaviour(supervisor_bridge).

%% API
-export([start_link/2,
        listen_dirty_queue/5]).

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
    Pid = spawn_link(fun() -> listen_dirty_queue_init(Site, Table) end),
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
listen_dirty_queue_init(Site, QTbl) ->
    mnesia:subscribe({table, QTbl, simple}),
    {Since, Dirty} = load_since(0, QTbl),
    Graph = new_graph(),
    WorkPlan = build_workplan(Site, Dirty, Graph),
    listen_dirty_queue(Site, QTbl, Since, WorkPlan, Graph).

-spec listen_dirty_queue(string(), atom(), term(), [cellidx()], digraph()) 
                        -> no_return().
listen_dirty_queue(Site, QTbl, Since, WorkPlan, Graph) ->
    Graph2 = case WorkPlan of
                [] -> 
                     ok = clear_dirty_queue(Since, QTbl), 
                     digraph:delete(Graph),
                     new_graph();
                _ -> 
                    Graph
            end,
    {Since2, WorkPlan2} = check_new(Site, Since, QTbl, WorkPlan, Graph2),
    WorkPlan3 = case WorkPlan2 of 
                    [Cell | Rest] ->
                        execute_plan([Cell], Site, Graph2),
                        Rest;
                    _ ->
                        WorkPlan2
                end,
    ?MODULE:listen_dirty_queue(Site, QTbl, Since2, WorkPlan3, Graph2).


%% Checks if new work is waiting to be processed. 
-spec check_new(string(), term(), atom(), [cellidx()], digraph()) 
               -> {term(), [cellidx()]}.
check_new(Site, Since, QTbl, WorkPlan, Graph) ->
    Wait = case WorkPlan of 
               [] -> infinity;
               _  -> 0 end,
    receive 
        {mnesia_table_event, {write, _, _}} ->
            case load_since(Since, QTbl) of
                {_, []} ->
                    {Since, WorkPlan};
                {Since2, Dirty} ->
                    WorkPlan2 = build_workplan(Site, Dirty, Graph),
                    {Since2, WorkPlan2}
            end;
        _Other ->
            check_new(Site, Since, QTbl, WorkPlan, Graph)
    after Wait ->
            {Since, WorkPlan}
    end.    

%% Loads new dirty information into the recalc graph.
-spec load_since(term(), atom()) -> {term(), [cellidx()]}.
load_since(Since, QTbl) ->
    M = ets:fun2ms(fun(#dirty_queue{id = T, dirty = D}) 
                         when Since < T -> {T, D}
                   end),
    F = fun() -> mnesia:select(QTbl, M, read) end,
    case mnesia:activity(transaction, F) of
        [] -> {Since, []};
        Ret ->
            {SinceL, DirtyLL} = lists:unzip(Ret),
            Since2 = lists:max(SinceL),
            DirtyL = lists:usort(lists:flatten(DirtyLL)),
            {Since2, DirtyL}
    end.

%% Clears out process work from the dirty_queue table.
-spec clear_dirty_queue(term(), atom()) -> ok.
clear_dirty_queue(Since, QTbl) ->
    M = ets:fun2ms(fun(#dirty_queue{id = T}) when T =< Since -> T end), 
    F = fun() ->
                Keys = mnesia:select(QTbl, M, write),
                [mnesia:delete(QTbl, K, write) || K <- Keys],
                ok
        end,
    mnesia:activity(transaction, F).

-spec build_workplan(string(), [cellidx()], digraph()) -> [cellidx()]. 
build_workplan(Site, Dirty, Graph) ->
    RTbl = hn_db_wu:trans(Site, relation),
    update_recalc_graph(Dirty, RTbl, Graph),
    digraph_utils:topsort(Graph).        

%% Recursively walk child relation, adding entries into the work
%% queue.
-spec update_recalc_graph([cellidx()], atom(), digraph()) -> ok.
update_recalc_graph([], _RTbl, _Graph) ->
    ok;
update_recalc_graph([Idx|Rest], RTbl, Graph) when is_atom(RTbl) ->
    case digraph:vertex(Graph, Idx) of
        false ->
            digraph:add_vertex(Graph, Idx),
            case mnesia:dirty_read(RTbl, Idx) of
                [R] ->
                    Children = ordsets:to_list(R#relation.children),
                    update_recalc_graph(Children, RTbl, Graph),
                    [digraph:add_edge(Graph, Idx, C) || C <- Children];
                _ -> 
                    ok
            end;
        _ ->
            ok
    end,
    update_recalc_graph(Rest, RTbl, Graph).

-spec execute_plan([cellidx()], string(), digraph()) -> ok.
execute_plan([], _, _) ->
    ok;
execute_plan([C | T], Site, Graph) ->
    case digraph:vertex(Graph, C) of
        false ->
            execute_plan(T, Site, Graph);
        _ ->
            Fixed = hn_db_api:handle_dirty_cell(Site, C, nil),
            case Fixed of 
                true -> prune_path(digraph:out_neighbours(Graph, C), Graph);
                false -> ok
            end,
            execute_plan(T, Site, Graph)
    end.
          
prune_path([], _Graph) ->
    ok;
prune_path([V|Rest], Graph) ->
    case digraph:in_degree(Graph, V) of
        1 -> 
            %% Vertex only has the fixed parent.
            prune_path(digraph:out_neighbours(Graph, V), Graph),
            digraph:del_vertex(Graph, V);
        _ ->
            %% Has other parents, halt walking path
            ok
    end,
    prune_path(Rest, Graph).

-spec new_graph() -> digraph(). 
new_graph() -> digraph:new([private]).

