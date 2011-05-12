-module(dbsrv).

-behaviour(supervisor_bridge).

-include("syslib.hrl").

%% API
-export([
         start_link/1,
         read_only_activity/2,
         write_activity/2,
         dbsrv/5
        ]).

%% Testing API - used to limit the feed of data in
-export([
         is_busy/1
        ]).

%% supervisor_bridge callbacks
-export([init/1, terminate/2]).

-include("hypernumbers.hrl").
-include("spriki.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {table :: atom(),
                pid :: pid()}).

-define(HEAP_SIZE, 250000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor bridge
%%--------------------------------------------------------------------
-spec start_link(string()) -> {ok,pid()} | ignore | {error,any()}.
start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv_sup"),
    supervisor_bridge:start_link({global, Id}, ?MODULE, [Site]).

read_only_activity(Site, Activity) ->
    %% Fix for circular chain deadlock. eg. include() in a formula.
    case mnesia:is_transaction() of
        true -> Activity();
        false ->
            Id = hn_util:site_to_atom(Site, "_dbsrv"),
            Id ! {self(), read_only_activity, Activity},
            receive
                {dbsrv_reply, Reply} -> Reply
            end
    end.

write_activity(Site, Activity) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    Id ! {write_activity, Activity},
    ok.

is_busy(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    Id ! {self(), is_busy},
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
    QTbl = new_db_wu:trans(Site, dirty_queue),
    Pid = spawn_opt(fun() -> dbsrv_init(Site, QTbl) end, [{fullsweep_after, 0}]),
    true = link(Pid),
    register(hn_util:site_to_atom(Site, "_dbsrv"), Pid),
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
    {Since, Dirty} = new_db_api:load_dirty_since(0, QTbl),
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
    % check the state of memory usage and maybe run a garbage collect
    {heap_size, HSZ} = process_info(self(), heap_size),
    true = if
               HSZ >  ?HEAP_SIZE -> garbage_collect(self());
               HSZ =< ?HEAP_SIZE -> true
    end,
    Wait = case WorkPlan of
               [] -> infinity;
               _ -> 0
           end,
    receive
        {From, read_only_activity, Activity} ->
            Reply = Activity(),
            From ! {dbsrv_reply, Reply},
            check_messages(Site, Since, QTbl, WorkPlan, Graph);

        {write_activity, Activity} ->
            Activity(),
            ok = zinf_srv:check_zinfs(Site),
            case new_db_api:load_dirty_since(Since, QTbl) of
                {Since2, []} ->
                    {Since2, WorkPlan};
                {Since2, Dirty} ->
                    WorkPlan2 = build_workplan(Site, Dirty, Graph),
                    {Since2, WorkPlan2}
            end;

        {From, is_busy} ->
            From ! {dbsrv_reply, WorkPlan /= []},
            check_messages(Site, Since, QTbl, WorkPlan, Graph);

        _Other ->
            check_messages(Site, Since, QTbl, WorkPlan, Graph)
    after Wait ->
            {Since, WorkPlan}
    end.

-spec build_workplan(string(), [cellidx()], digraph()) -> [cellidx()].
build_workplan(Site, Dirty, Graph) ->
    RTbl = new_db_wu:trans(Site, relation),
    Trans = fun() ->
                    update_recalc_graph(Dirty, RTbl, Graph),
                    [digraph:add_edge(Graph, P, D)
                     || D <- Dirty,
                        P <- check_interference(D, RTbl, Graph)],
                    ok
            end,
    ok = mnesia:activity(transaction, Trans),
    case digraph_utils:topsort(Graph) of
        false -> eliminate_circ_ref(Site, Dirty, Graph);
        Work -> Work
    end.

%% When a formula is added, it is necessary to test whether or not
%% its parents are already present in the recalc tree. If so,
%% dependency edges must be added from these parents to the new
%% formula.
-spec check_interference(cellidx(), atom(), digraph()) -> [cellidx()].
check_interference(Cell, RTbl, Graph) ->
    case mnesia:read(RTbl, Cell, read) of
        [R] ->
            Parents = ordsets:to_list(R#relation.parents),
            [P || P <- Parents, false /= digraph:vertex(Graph, P)];
        _ ->
            []
    end.

%% Recursively walk child relation, adding entries into the work
%% queue.
-spec update_recalc_graph([cellidx()], atom(), digraph()) -> ok.
update_recalc_graph([], _RTbl, _Graph) ->
    ok;
update_recalc_graph([Idx|Rest], RTbl, Graph) ->
    case digraph:vertex(Graph, Idx) of
        false ->
            case mnesia:read(RTbl, Idx, read) of
                [R] ->
                    digraph:add_vertex(Graph, Idx),
                    Children = ordsets:to_list(R#relation.children),
                    %log_children(Children, R#relation.cellidx),
                    update_recalc_graph(Children, RTbl, Graph),
                    [digraph:add_edge(Graph, Idx, C) || C <- Children];
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    update_recalc_graph(Rest, RTbl, Graph).

%% A circular reference involving the Site, and Graph has been
%% detected. A non-empty subset of the given dirty cells are
%% culprits. The procedure is to detect and remove the cycle from the
%% graph, and rewrite the formula for any offending cells. A
%% co-recursive call back to build_workplan is made to complete
%% construction of the workplan.
-spec eliminate_circ_ref(string(), [cellidx()], digraph()) -> [cellidx()].
eliminate_circ_ref(Site, Dirty, Graph) ->
    Cycle = lists:flatten(digraph_utils:cyclic_strong_components(Graph)),
    [digraph:del_vertex(Graph, V) || V <- Cycle],
    [new_db_api:handle_circref_cell(Site, V, nil) || V <- Cycle,
                                                    lists:member(V, Dirty)],
    build_workplan(Site, Dirty, Graph).

-spec execute_plan([cellidx()], string(), digraph()) -> ok.
execute_plan([], _, _) ->
    ok;
execute_plan([C | T], Site, Graph) ->
    case digraph:vertex(Graph, C) of
        false ->
            execute_plan(T, Site, Graph);
        _ ->
            NewChildren = new_db_api:handle_dirty_cell(Site, C, nil),
            digraph:del_vertex(Graph, C),
            NewPlan = build_workplan(Site, NewChildren, Graph),
            execute_plan(NewPlan, Site, Graph)
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

-spec new_graph() -> digraph().
new_graph() -> digraph:new([private]).

%% log_children([], _Idx) -> ok;
%% log_children([H | T], Idx) ->
%%     Cell = new_db_wu:idx_to_xrefX("http://hypernumbers.dev:9000", Idx),
%%     XRefX = new_db_wu:idx_to_xrefX("http://hypernumbers.dev:9000", H),
%%     Msg = io_lib:format("for ~p ~p adding child ~p",
%%                         [Cell#xrefX.path,
%%                          hn_util:obj_to_ref(Cell#xrefX.obj),
%%                          hn_util:obj_to_ref(XRefX#xrefX.obj)]),
%%     syslib:log(Msg, ?recalc),
%%     log_children(T, Idx).

