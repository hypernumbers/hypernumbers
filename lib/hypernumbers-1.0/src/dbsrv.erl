-module(dbsrv).

-behaviour(supervisor_bridge).

%% API
-export([start_link/1,
         read_only_activity/2,
         write_activity/2,
         dbsrv/5]).

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
-spec start_link(string()) -> {ok,pid()} | ignore | {error,any()}.
start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "dbsrv_sup"),
    supervisor_bridge:start_link({local, Id}, ?MODULE, [Site]).

read_only_activity(Site, Activity) ->
    Id = hn_util:site_to_atom(Site, "dbsrv"),
    Id ! {self(), read_only_activity, Activity},
    receive
        {dbsrv_reply, Reply} -> Reply
    end.

write_activity(Site, Activity) ->
    Id = hn_util:site_to_atom(Site, "dbsrv"),
    Id ! {self(), write_activity, Activity},
    receive
        {dbsrv_reply, Reply} -> Reply
    end.

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
init([Site]) ->
    QTbl = hn_db_wu:trans(Site, dirty_queue),
    Pid = spawn_link(fun() -> dbsrv_init(Site, QTbl) end),
    register(hn_util:site_to_atom(Site, "dbsrv"), Pid),
    {ok, Pid, #state{table = QTbl, pid = Pid}}.

%%--------------------------------------------------------------------
%% Func: terminate(Reason, State) -> void()
%% Description:This function is called by the supervisor_bridge when it is
%% about to terminate. It should be the opposite of Module:init/1 and stop
%% the subsystem and do any necessary cleaning up.The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{table = _T}) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec dbsrv_init(string(), atom()) -> no_return(). 
dbsrv_init(Site, QTbl) ->
    {Since, Dirty} = load_dirty_since(0, QTbl),
    Graph = new_graph(),
    WorkPlan = build_workplan(Site, Dirty, Graph),
    dbsrv(Site, QTbl, Since, WorkPlan, Graph).

-spec dbsrv(string(), atom(), term(), [cellidx()], digraph()) 
                        -> no_return().
dbsrv(Site, QTbl, Since, WorkPlan, Graph0) ->
    Graph = cleanup(WorkPlan, Since, QTbl, Graph0),
    {Since2, WorkPlan2} = check_messages(Site, Since, QTbl, WorkPlan, Graph),
    WorkPlan3 = case WorkPlan2 of 
                    [Cell | Rest] ->
                        execute_plan([Cell], Site, Graph),
                        Rest;
                    _ ->
                        WorkPlan2
                end,
    ?MODULE:dbsrv(Site, QTbl, Since2, WorkPlan3, Graph).

-spec cleanup([cellidx()], term(), atom(), digraph()) -> digraph().
cleanup([], Since, QTbl, Graph) -> 
    ok = clear_dirty_queue(Since, QTbl), 
    digraph:delete(Graph),
    new_graph();
cleanup(_, _, _, Graph) -> Graph.

%% Checks if new work is waiting to be processed. 
-spec check_messages(string(), term(), atom(), [cellidx()], digraph()) 
               -> {term(), [cellidx()]}.
check_messages(Site, Since, QTbl, WorkPlan, Graph) ->
    Wait = case WorkPlan of [] -> infinity; _ -> 0 end,
    receive 
        {From, read_only_activity, Activity} -> 
            Reply = Activity(),
            From ! {dbsrv_reply, Reply},
            check_messages(Site, Since, QTbl, WorkPlan, Graph);
        
        {From, write_activity, Activity} ->
            Reply = Activity(),
            From ! {dbsrv_reply, Reply},
            case load_dirty_since(Since, QTbl) of
                {_, []} ->
                    {Since, WorkPlan};
                {Since2, Dirty} ->
                    WorkPlan2 = build_workplan(Site, Dirty, Graph),
                    {Since2, WorkPlan2}
            end;
        _Other ->
            check_messages(Site, Since, QTbl, WorkPlan, Graph)
    after Wait ->
            {Since, WorkPlan}
    end.    

%% Loads new dirty information into the recalc graph.
-spec load_dirty_since(term(), atom()) -> {term(), [cellidx()]}.
load_dirty_since(Since, QTbl) ->
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

-spec build_workplan(string(), [cellidx()], digraph()) -> [cellidx()]. 
build_workplan(Site, Dirty, Graph) ->
    RTbl = hn_db_wu:trans(Site, relation),
    ok = mnesia:activity(transaction, fun update_recalc_graph/3, 
                         [Dirty, RTbl, Graph]),
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
            case mnesia:read(RTbl, Idx, read) of
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

-spec new_graph() -> digraph(). 
new_graph() -> digraph:new([private]).

