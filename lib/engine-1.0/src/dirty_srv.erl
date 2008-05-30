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
-record(state, {type=[]}).

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
handle_info({mnesia_table_event,{write,_,Rec,[],_}}, State) ->

    Index = ?COND(State#state.type == dirty_cell,
        Rec#dirty_cell.index,
        Rec#dirty_hypernumber.index),

    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:delete({State#state.type, Index})
    end),
    
    spawn(fun() ->
        hn_db:dirty_refs_changed(State#state.type, Index)
    end),
    
    {noreply, State};
    
handle_info(_Info,State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Rest of the gen_server callbacks
%%----------------------------------------------------------------------       
handle_call(_Request, _From, State) ->  {reply, ok, State}.
handle_cast(_Msg, State) ->             {noreply, State}.
terminate(_Reason, _State) ->           ok.    
code_change(_OldVsn, State, _Extra) ->  {ok, State}.
