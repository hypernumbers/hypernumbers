%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @TODO write a proper module description
%%% also this is totally not resilient
%%% the restart behaviour is broken. If this module wigs then it is restarted WITHOUT
%%% being resubscribed to mnesia for changes which is a mess...
%%% On restart it should process the appropriate dirty table and also resubscribe
%%% to the mnesia events...
-module(dirty_srv).
-behaviour(gen_server).

-include("handy_macros.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").
-record(state, {type = [], state = active}).

-export([start_link/1,
         init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(api, hn_db_api).

%% @spec start_link(Arg) -> StartLink
%% @doc  start a link between this and supervisor
start_link(Arg) ->
    gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).

%% @spec init(Arg) -> {ok, State}
%% @doc  Start server
init([Type]) ->
    {ok, #state{type = Type}}.

%% @spec handle_info(Event,State) -> {noreply, State}
%% @doc  handle events from subscription to mnesia
handle_info({mnesia_table_event, {write, Table, Rec, _OldRecs, _ActId}},
            State) ->
    case State#state.state of
        passive -> ok;

        active  -> proc_dirty(Table, Rec)
    end,
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply, State}
%% @doc  ignore delete events from mnesia
handle_info({mnesia_table_event, {delete, _, _, _, _}}, State) ->
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply, State}
%% @doc  catch / flush unhandled events
handle_info(_Info, State) ->
    ?INFO("Unmatched Event ~p", [_Info]),
    {noreply, State}.

%% @spec handle_call(subscribe, State) -> {reply, Reply State}
%% @doc  subscribe to table events from mnesia
handle_call({subscribe, Table}, _From, State) ->
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    [{ok,_} = sub_unsubscribe(Table, X, subscribe) || X <- Sites],
    Reply = ok,
    {reply, Reply, State};
handle_call(_Msg, _From, _State) ->
    {reply, ok, []}.

%% @spec handle_cast(flush, From, State) -> {reply, ok, State}
%% @doc  flush the table of dirty records
handle_cast({flush, Table}, State) ->
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    [ok = flush(X, Table) || X <- Sites],
    {noreply, State};

%% @spec handle_cast({setstate,NState}, State) -> {noreply, State}
%% @doc  active server will recalc on write, passive will ignore
handle_cast({setstate,active}, State) -> 
    {noreply, State#state{state = active}};
handle_cast({setstate,passive}, State) -> 
    {noreply, State#state{state = passive}};

%% @spec handle_cast(subscribe, State) -> {noreply,State}
%% @doc  unsubscribe from table events from mnesia
handle_cast({unsubscribe, Table}, State) ->
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    [{ok,_} = sub_unsubscribe(Table, X, unsubscribe) || X <- Sites],
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc  exit the gen_server
terminate(_Reason, _State) ->           
    ok.    
%% @spec code_change(Version, State, Extra) -> {ok, State}
%% @doc  handle code_change
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.

%% @spec proc_dirty(Rec, Type) -> ok
%% @doc  processes the dirty record
proc_dirty(Table, Rec) ->
    
    [Host, Port, _Table] = string:tokens(atom_to_list(Table), "&"),
    Site = "http://"++Host++":"++Port,
    
    {reductions, X} = erlang:process_info(self(), reductions),
    case X rem 50 of
        0 ->
            ?INFO("Shrinking dirty",[]),
            hn_db_api:shrink_dirty_cell(Site);
        _ -> ok
    end,
    
    % fprof:trace(start),
    Ret = case element(1, Rec) of
              dirty_cell ->
                  #dirty_cell{timestamp = T} = Rec,
                  hn_db_api:handle_dirty_cell(Site, T, Rec);
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
    % fprof:trace(stop),
    Ret.

%%%
%%% Utility Functions
%%% 

%% subscribe/unsubscribe to the mnesia tables
sub_unsubscribe(Table, Site, Action) ->
    NewTable = hn_db_wu:trans(Site, Table),
    case Action of
        subscribe   ->
            mnesia:subscribe({table, NewTable, detailed});
        unsubscribe ->
            mnesia:unsubscribe({table, NewTable, detailed})
    end.

flush(Site, Tbl) ->

    Table = hn_db_wu:trans(Site, Tbl),
    
    F = fun() ->
                Match = mnesia:table_info(Table, wild_pattern),
                mnesia:match_object(Table, Match, read)
        end,
    
    case mnesia:activity(transaction, F) of
        [] ->
            ok;
        List ->
            ?INFO("Flushing ~p records from  ~p - ~p ", [length(List), Table, Site]),
            lists:foreach(fun(X) -> proc_dirty(Table, X) end, List)
    end.
