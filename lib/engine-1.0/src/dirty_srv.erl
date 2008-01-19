%%%-----------------------------------------------------------------------------
%%% File        : dirty_srv.erl
%%% Author      : Gordon Guthrie <gordonguthrie@jeteasy.com>
%%% Description : This is the Gen FSM that handles recalc requests
%%%               This gen fsm reads the table dirty_refs and clears the
%%%               entries off one at a time and then switches state to a
%%%               passive mode where it subscribes to the table and waits
%%%               for the next write before flipping back
%%%
%%% Created     : 23 Oct 2007 by Gordon Guthrie <gordonguthrie@jeteasy.com>
%%%-----------------------------------------------------------------------------
-module(dirty_srv).

-behaviour(gen_fsm).

-include("spriki.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
	 clear_dirty/2, clear_dirty/3,
	 wait_on_write/2, wait_on_write/3,
	 handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4]).

-record(state, {type=[]}).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%==============================================================================
%% API
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%------------------------------------------------------------------------------
start_link(Arg) ->
     gen_fsm:start_link(?MODULE, [Arg], []).

%%==============================================================================
%% gen_fsm callbacks
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%------------------------------------------------------------------------------
init([Type]) ->
    gen_fsm:send_event(self(),{clear_dirty,[]}),
    case Type of
	dirty_refs         -> {ok, clear_dirty, #state{type=Type}};
	dirty_hypernumbers -> {ok, clear_dirty, #state{type=Type}}
    end.
%%------------------------------------------------------------------------------
%% Function:
%% clear_dirty(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%------------------------------------------------------------------------------
clear_dirty(_Event, State) ->
    Type=State#state.type,
    NewState=case db:get_first_dirty(Type) of
		 %% if there are no dirty records just wait on a table write
		 []       -> {ok,_Return}=mnesia:subscribe({table,Type,detailed}),
			     wait_on_write;
		 [Record] -> process(Record,Type),
			     %% come back here and see if there are
			     %% any more dirty records
			     gen_fsm:send_event(self(),{clear_dirty,[]}),
			     clear_dirty
	     end,
    {next_state, NewState, State}.

%%------------------------------------------------------------------------------
%% Function:
%% wait_on_write(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%------------------------------------------------------------------------------
wait_on_write(Event, State) ->
    Type=State#state.type,
    NewState=case Event of
		 {wait_on_write,dirty_written} ->
		     {ok,_}=mnesia:unsubscribe({table,Type,detailed}),
		     gen_fsm:send_event(self(),clear_dirty),
		     clear_dirty;
		 {wait_on_write,stay_waiting}      ->
		     wait_on_write
	     end,
    {next_state, NewState, State}.

%%------------------------------------------------------------------------------
%% Function:
%% clear_dirty(Event, From, State) -> {next_state,NextStateName,NextState}|
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%------------------------------------------------------------------------------
clear_dirty(_Event, _From, State) ->
    {reply, ok, clear_dirty, State}.

%%------------------------------------------------------------------------------
%% Function:
%% wait_on_write(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%------------------------------------------------------------------------------
wait_on_write(_Event, _From, State) ->
    {reply, ok, wait_on_write, State}.

%%------------------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%						  NextState} |
%%                                          {next_state, NextStateName,
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%------------------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%------------------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

%%------------------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%------------------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    case {StateName,Info} of
	{wait_on_write,{mnesia_table_event,{write,_,_,_,_}}}    ->
	    {ok,_}=mnesia:unsubscribe({table,State#state.type,detailed}),
	    gen_fsm:send_event(self(),{wait_on_write,dirty_written});
	_Other  -> ok % do nothing
    end,
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%------------------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%------------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%------------------------------------------------------------------------------
%%% Internal functions
%%------------------------------------------------------------------------------

%% Record is the record for the cell that's been changed. See dirty_refs in
%% spriki.hrl. Example:
%% {dirty_refs,{index,"http://127.0.0.1:9000","/",1,1},1966112354}
%% for A1 on /.
process(Record, dirty_refs)->
    db:trigger_recalcs(dirty_refs, Record#dirty_refs.index);
process(Record, dirty_hypernumbers) ->
    db:trigger_recalcs(dirty_hypernumbers,Record#dirty_hypernumbers.index).


