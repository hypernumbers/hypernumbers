%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(dirty_srv).
-behaviour(gen_server).

-include("handy_macros.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").
-record(state, {type=[],state = active}).

-export([start_link/1, init/1, handle_call/3, 
    handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @spec start_link(Arg) -> StartLink.
%% @doc  start a link between this and supervisor
start_link(Arg) -> 
    gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).

%% @spec init(Arg) -> {ok,State}.
%% @doc  Start server
init([Type]) ->
    {ok, #state{type=Type}}.
 
%% @spec handle_info(Event,State) -> {noreply,State}.
%% @doc  handle events from subscription to mnesia
handle_info({mnesia_table_event,{write,_,Rec,_,_}},State) ->
    case State#state.state of
        passive -> ok;
        active  -> trigger_recalc(Rec,State#state.type)
    end,
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply,State}.
%% @doc  ignore delete events from mnesia
handle_info({mnesia_table_event,{delete,_,_,_,_}},State) ->
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply,State}.
%% @doc  catch / flush unhandled events
handle_info(_Info,State) ->
    ?INFO("UnMatched Event ~p",[_Info]),
    {noreply, State}.

%% @spec handle_call(flush,From,State) -> {reply,ok,State}.
%% @doc  flush the dirty table, read all the current dirty
%%       cells and trigger recalculation
handle_call(flush, _From, State = #state{type=Type}) ->
    Get = fun() ->
                  mnesia:match_object({Type,'_','_'})
          end,

    {atomic,List} = mnesia:transaction(Get),
    lists:foreach(fun(X) -> trigger_recalc(X,Type) end,List),
    
    {reply, ok, State}.

%% @spec handle_cast({setstate,NState}, State) -> {noreply,State}.
%% @doc  active server will recalc on write, passive will ignore
handle_cast({setstate,active}, State) -> 
    {noreply, State#state{state=active}};
handle_cast({setstate,passive}, State) -> 
    {noreply, State#state{state=passive}};

%% @spec handle_cast(subscribe, State) -> {noreply,State}.
%% @doc  subscribe to table events from mnesia
handle_cast(subscribe, State = #state{type=Type}) -> 
    mnesia:subscribe({table,Type,detailed}),
    {noreply,State};
%% @spec handle_cast(subscribe, State) -> {noreply,State}.
%% @doc  unsubscribe from table events from mnesia
handle_cast(unsubscribe,State = #state{type=Type}) ->
    mnesia:unsubscribe({table,Type,detailed}),
    {noreply, State}.
    
%% @spec terminate(Reason, State) -> ok.
%% @doc  exit the gen_server
terminate(_Reason, _State) ->           
    ok.    
%% @spec code_change(Version, State, Extra) -> {ok,State}.
%% @doc  handle code_change
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.

%% @spec trigger_recalc(Record, Type) -> ok.
%% @doc  trigger recalculation for cell Rec.index
trigger_recalc(Rec,Type) ->
    
    Index = ?COND(Type == dirty_cell,
                  Rec#dirty_cell.index,
                  Rec#dirty_hypernumber.index),
    
    ok = mnesia:dirty_delete({Type, Index}),
    ok = case Type of
             dirty_cell        -> hn_db:cell_changed(Index);
             dirty_hypernumber -> hn_db:hn_changed(Index)
         end,
    ok.

