%%%-----------------------------------------------------------------------------
%%% File        : dirty_srv.erl
%%% Author      : Gordon Guthrie <gordonguthrie@jeteasy.com>
%%% Description : Handles the recalculation of cells thats values 
%%%               have changed
%%%-----------------------------------------------------------------------------
-module(dirty_srv).
-behaviour(gen_server).

-include("handy_macros.hrl").
-include("spriki.hrl").
-record(state, {type=[],state = active}).

-export([start_link/1, init/1, handle_call/3, 
    handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------
%% Func: start_link/1
%%---------------------------------------------------------------------- 
start_link(Arg) -> 
    gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).

%%----------------------------------------------------------------------
%% Func: init/1
%% subscribe to mnesia for updates to table Type
%%----------------------------------------------------------------------   
init([Type]) ->
    mnesia:subscribe({table,Type,detailed}),
    {ok, #state{type=Type}}.
 
%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Handles incoming messages from mnesia:subscribe
%%----------------------------------------------------------------------   
handle_info({mnesia_table_event,{write,_,Rec,_,_}},State) ->

    case State#state.state of
        passive -> ok;
        active  -> trigger_recalc(Rec,State#state.type)
    end,
    
    {noreply, State};

handle_info(_Info,State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Rest of the gen_server callbacks
%%----------------------------------------------------------------------       
handle_call(flush, _From, State) ->

    {atomic,List} = mnesia:transaction(fun() ->
        mnesia:match_object({State#state.type,'_','_'})
    end),

    lists:foreach(
      fun(X) ->
              trigger_recalc(X,State#state.type)
      end,
      List),
    
    {reply, ok, State}.

handle_cast({setstate,NewState}, State) -> 
    {noreply, State#state{state=NewState}};
handle_cast(resubscribe, State) -> 
    mnesia:subscribe({table,State#state.type,detailed}),
    {noreply,State};
handle_cast(stop,State) ->
    mnesia:unsubscribe({table,State#state.type,detailed}),
    {noreply, State}.
    
terminate(_Reason, _State) ->           ok.    
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%----------------------------------------------------------------------
%% Rest of the gen_server callbacks
%%----------------------------------------------------------------------    
trigger_recalc(Rec,Type) ->
    
    Index = ?COND(Type == dirty_cell,
                  Rec#dirty_cell.index,
                  Rec#dirty_hypernumber.index),
    
    ok = mnesia:dirty_delete({Type, Index}),
    ok = hn_db:dirty_refs_changed(Type, Index),    

    ok.

