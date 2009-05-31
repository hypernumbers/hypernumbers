%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @TODO write a proper module description
-module(dirty_srv).
-behaviour(gen_server).

-include("handy_macros.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").

-record(state, {table=undefined, children=[]}).
-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([start_link/1,
         init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/1, stop/1, listen/1]).


%% @spec start_link(Arg) -> StartLink
%% @doc  start a link between this and supervisor
start_link(Arg) ->
    gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).

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
    
    NChild = restart(State#state.children, Pid),
    {noreply, State#state{children = NChild}};

handle_info(_Info, State) ->
    {noreply, State}.
handle_cast(_Info, State) ->
    {noreply, State}.

%% @spec handle_call(subscribe, State) -> {reply, Reply State}
%% @doc  subscribe to table events from mnesia
handle_call(start,  _From, State) ->
    Sites = hn_util:get_hosts(hn_config:get(hosts)),
    F = fun(Site) ->
                Tbl = hn_db_wu:trans(Site, State#state.table),
                {start_listen(Tbl), Tbl}
        end,
    NState = State#state{children = lists:map(F, Sites)},
    
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

%%%
%%% Utility Functions
%%% 

start(Type) ->
    ok = gen_server:call(Type, start).

stop(Type) ->
    ok = gen_server:call(Type, stop).

listen(Table) ->
    case mnesia:activity(transaction, fun read_table/1, [Table]) of
        ok -> ok;
        no_dirty_cells ->
            receive _X ->
                    mnesia:unsubscribe({table, Table, simple})
            end
    end,
    ?MODULE:listen(Table).
        
read_table(Table) ->
    case mnesia:first(Table) of
        '$end_of_table' ->
            mnesia:subscribe({table, Table, simple}),
            no_dirty_cells;
        Id ->
            [Rec] = mnesia:read(Table, Id, write),
            ok = mnesia:delete(Table, Id, write),
            proc_dirty(Table, Rec)
    end.

%% @spec handle_info(Else,State) -> {noreply, State}
%% @doc  catch / flush unhandled events
restart(List, Pid) ->
    restart(List, Pid, []).

restart([], _Pid, Acc) ->
    Acc;
restart([{Pid, Table} | Tl], Pid, Acc) ->
    restart(Tl, Pid, [{start_listen(Table), Table} | Acc]);
restart([Hd | Tl], Pid, Acc) ->
    restart(Tl, Pid, [Hd | Acc]).

start_listen(Table) ->
    spawn_link(fun() -> listen(Table) end).

report_error(Pid, Reason, State) ->
    Table = ?pget(Pid, State#state.children),
    {ok, Id} = mnesia:activity(transaction, fun delete_first/1, [Table]),
    
    ?ERROR(" Process ~p died in ~p ~p ~n Error: ~p~n Stacktrace: ~p",
           [Pid, Table, Id, Reason, erlang:get_stacktrace()]),
    ok.
    
delete_first(Table) ->
    Id = mnesia:first(Table),
    ok = mnesia:delete(Table, Id, write),
    {ok, Id}.    

%% @spec proc_dirty(Rec, Type) -> ok
%% @doc  processes the dirty record
proc_dirty(Table, Rec) ->

    [Host, Port, _Table] = string:tokens(atom_to_list(Table), "&"),
    Site = "http://"++Host++":"++Port,
    
    Ret = case element(1, Rec) of
              dirty_cell ->
                  hn_db_api:handle_dirty_cell(Site, Rec);
              dirty_inc_hn_create ->
                  hn_db_api:notify_back_create(Site, Rec);
              dirty_notify_in ->
                  hn_db_api:handle_dirty(Site, Rec);
              dirty_notify_out ->
                  #dirty_notify_out{delay = D} = Rec,
                  ok = timer:sleep(D),
                  hn_db_api:handle_dirty(Site, Rec);
              dirty_notify_back_in  ->
                  hn_db_api:handle_dirty(Site, Rec);
              dirty_notify_back_out ->
                  hn_db_api:handle_dirty(Site, Rec)
          end,
    Ret.
