-module(dbsrv).

-behaviour(supervisor_bridge).

-include("syslib.hrl").

%% API
-export([
         start_link/1,
         read_only_activity/2,
         write_activity/2,
         dbsrv/4
        ]).

%% Testing API - used to limit the feed of data in
-export([
         is_busy/1,
         dump_q_len/1
        ]).

%% Peformance Testing API
-export([
         start_fprof/1,
         stop_fprof/2
        ]).

%% supervisor_bridge callbacks
-export([init/1, terminate/2]).

-include("hypernumbers.hrl").
-include("spriki.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {pid :: pid()}).

-define(HEAP_SIZE, 400000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor bridge
%%--------------------------------------------------------------------
-spec start_link(string()) -> {ok,pid()} | ignore | {error,any()}.
start_link(Site) ->
    case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting db_srv for ~p~n", [Site]);
       _Other     -> ok
    end,
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
    Id ! {write_activity, self(), Activity},
    receive
        {response, ok} ->
            ok
    end,
    ok.

dump_q_len(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    Id ! {self(), dump_q_len},
    receive
        {dbsrv_reply, Reply} -> Reply
    end.

is_busy(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    Id ! {self(), is_busy},
    receive
        {dbsrv_reply, Reply} -> Reply
    end.

start_fprof(Site) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    Id ! {self(), start_fprof},
    receive
        {dbsrv_reply, Reply} -> Reply
    end.

stop_fprof(Site, TraceFile) ->
    Id = hn_util:site_to_atom(Site, "_dbsrv"),
    Id ! {self(), {stop_fprof, TraceFile}},
    receive
        {dbsrv_reply, Reply} -> Reply
    end.

%%====================================================================
%% supervisor_bridge callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok,  Pid, State} |
%%                         ignore            |
%%                         {error, Reason}
%% Description:Creates a supervisor_bridge process, linked to the calling
%% process, which calls Module:init/1 to start the subsystem. To ensure a
%% synchronized start-up procedure, this function does not return until
%% Module:init/1 has returned.
%%--------------------------------------------------------------------
init([Site]) ->
    Pid = spawn_opt(fun() -> dbsrv_init(Site) end,
                    [{fullsweep_after, 0}]),
    true = link(Pid),
    register(hn_util:site_to_atom(Site, "_dbsrv"), Pid),
    {ok, Pid, #state{pid = Pid}}.

%%--------------------------------------------------------------------
%% Func: terminate(Reason, State) -> void()
%% Description:This function is called by the supervisor_bridge when it is
%% about to terminate. It should be the opposite of Module:init/1 and stop
%% the subsystem and do any necessary cleaning up.The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{}) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec dbsrv_init(string()) -> no_return().
dbsrv_init(Site) ->
    ok = new_db_api:rollback_dirty_cacheD(Site),
    {Since, Dirty} = new_db_api:load_dirty_since(Site, 0),
    Graph = new_graph(),
    WorkPlan = build_workplan(Site, Dirty, Graph),
    dbsrv(Site, Since, WorkPlan, Graph).

-spec dbsrv(string(), term(), [cellidx()], digraph())
-> no_return().
dbsrv(Site, Since, WorkPlan, Graph0) ->
    Graph = cleanup(WorkPlan, Site, Graph0),
    {Since2, WorkPlan2} = check_messages(Site, Since, WorkPlan, Graph),
    WorkPlan3 = case WorkPlan2 of
                    [Cell | Rest] ->
                        execute_plan([Cell], Site, Graph),
                        Rest;
                    _ ->
                        WorkPlan2
                end,
    ?MODULE:dbsrv(Site, Since2, WorkPlan3, Graph).

-spec cleanup([cellidx()], string(), digraph()) -> digraph().
cleanup([], Site, Graph) ->
    ok = new_db_api:clear_dirty_cacheD(Site),
    digraph:delete(Graph),
    new_graph();
cleanup(_, _, Graph) -> Graph.

%% Checks if new work is waiting to be processed.
-spec check_messages(string(), term(), [cellidx()], digraph())
-> {term(), [cellidx()]}.
check_messages(Site, Since, WorkPlan, Graph) ->
    % check the state of memory usage and maybe run a garbage collect
    {heap_size, HSZ} = process_info(self(), heap_size),
    true = if
               HSZ >  ?HEAP_SIZE -> garbage_collect(self());
               HSZ =< ?HEAP_SIZE -> true
           end,
    Wait = case WorkPlan of
               [] -> infinity;
               _  -> 0
           end,
    receive
        {From, read_only_activity, Activity} ->
            Reply = Activity(),
            From ! {dbsrv_reply, Reply},
            check_messages(Site, Since, WorkPlan, Graph);

        {write_activity, From, Activity} ->
            Activity(),
            case new_db_api:load_dirty_since(Site, Since) of
                {Since2, []} ->
                    From ! {response, ok},
                    {Since2, WorkPlan};
                {Since2, Dirty} ->
                    From ! {response, ok},
                    WorkPlan2 = build_workplan(Site, Dirty, Graph),
                    {Since2, WorkPlan2}
            end;

        {From, is_busy} ->
            From ! {dbsrv_reply, WorkPlan /= []},
            check_messages(Site, Since, WorkPlan, Graph);

        {From, dump_q_len} ->
            From ! {dbsrv_reply, length(WorkPlan)},
            check_messages(Site, Since, WorkPlan, Graph);

        {From, start_fprof} ->
            Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
            Dir = "/media/logging/",
            TraceFile = Dir ++ "profile_dbsrv" ++ Stamp ++ ".trace",
            fprof:trace(start, TraceFile),
            io:format("TraceFile is ~p~n", [TraceFile]),
            From ! {dbsrv_reply, TraceFile},
            check_messages(Site, Since, WorkPlan, Graph);

        {From, {stop_fprof, TraceFile}} ->
            fprof:trace(stop),
            io:format("fprof has been stopped...~n"),
            fprof:profile(file, TraceFile),
            io:format("profile done...~n"),
            Root = filename:rootname(TraceFile),
            fprof:analyse([{dest, Root ++ ".analysis"}]),
            io:format("analysis over...~n"),
            From ! {dbsrv_reply, fprof_done},
            check_messages(Site, Since, WorkPlan, Graph);

        _Other ->
            check_messages(Site, Since, WorkPlan, Graph)
    after Wait ->
            {Since, WorkPlan}
    end.

-spec build_workplan(string(), [cellidx()], digraph()) -> [cellidx()].
build_workplan(Site, Dirty, Graph) ->
    Trans = fun() ->
                    % if the cell is a new cell then recalc if it is
                    % a circular dependency, otherwise don't
                    case update_recalc_graph(Dirty, Site, Graph) of
                        ok ->
                            ok;
                        _  ->
                            [digraph:add_edge(Graph, P, D)
                             || D <- Dirty,
                                P <- check_interference(D, Site, Graph)],
                            ok
                    end
            end,
    ok = mnesia:activity(transaction, Trans),
    case digraph_utils:topsort(Graph) of
        false -> eliminate_circ_ref(Site, Dirty, Graph);
        Work  -> Work
    end.

%% When a formula is added, it is necessary to test whether or not
%% its parents are already present in the recalc tree. If so,
%% dependency edges must be added from these parents to the new
%% formula. This checks for INDIRECT circular references
%% (eg =A2 in cell A1 and =A1 in cell A2) only.
%% Checks for DIRECT circular references where a formula
%% refers to itself (ie =A1 in cell A1) are done in muin.erl
-spec check_interference(cellidx(), atom(), digraph()) -> [cellidx()].
check_interference(Cell, Site, Graph) ->
    case new_db_wu:read_relationsD(Site, Cell, read) of
        [R] ->
            Parents = ordsets:to_list(R#relation.parents),
            [P || P <- Parents, false /= digraph:vertex(Graph, P)];
        _ ->
            []
    end.

%% Recursively walk child relation, adding entries into the work
%% queue.
-spec update_recalc_graph([cellidx()], atom(), digraph()) -> ok.
update_recalc_graph([], _Site, _Graph) ->
    ok;
update_recalc_graph([Idx|Rest], Site, Graph) ->
    case digraph:vertex(Graph, Idx) of
        false ->
            case new_db_wu:read_relationsD(Site, Idx, read) of
                [R] ->
                    digraph:add_vertex(Graph, Idx),
                    Children = ordsets:to_list(R#relation.children),
                    update_recalc_graph(Children, Site, Graph),
                    [digraph:add_edge(Graph, Idx, C) || C <- Children];
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    update_recalc_graph(Rest, Site, Graph).

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
            ok = new_db_api:handle_dirty_cell(Site, C, nil),
            digraph:del_vertex(Graph, C),
            execute_plan(T, Site, Graph)
    end.

-spec new_graph() -> digraph().
new_graph() -> digraph:new([private]).
