%%%-------------------------------------------------------------------
%%% @author U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%% @copyright (C) 2009, U-psytoo\gordonguthrie
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2009 by U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%%-------------------------------------------------------------------
-module(dirty_subscriber).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         monitor/1,
         unmonitor/0,
         flush/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-include("hypernumbers.hrl").

-record(state, {pid, name}).

%% @doc
%% Starts the server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
%% @doc
%% Initiates the server
init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

%% @private
%% @doc
%% Handling call messages
handle_call({monitor, Name}, _From, State) ->
    {ok, Pid} = setup_link(Name),
    {reply, ok, [ #state{pid = Pid, name = Name} | State]};

handle_call(unmonitor, _From, State) ->
    % clear the state
    [true = unlink(P) || #state{pid = P} <- State],
    {reply, ok, []};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
%% @doc
%% Handling cast messages
handle_cast({flush, Name}, State) ->
    Pid = whereis(Name),
    true = link(Pid),
    ok = gen_server:cast(Name, {flush, Name}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages

%% Dont try to resubscribe when shutting down
handle_info({'EXIT', _Pid, shutdown}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, _Why}, State) ->
    {value, #state{name = Name}} = lists:keysearch(Pid, 2, State),
    NewState = lists:keydelete(Pid, 2, State),
    %% This is still broke, there is no guarantee Name has been restarted
    %% yet, but its needed to spawn here, handle_into is synchronous, as is
    %% handle_call which monitor(Name) runs
    F = fun() ->
                ok = monitor(Name),
                ok = flush(Name)
        end,
    spawn(F),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


monitor(Name) ->
    gen_server:call(?MODULE, {monitor, Name}).

unmonitor() ->
    gen_server:call(?MODULE, unmonitor).

flush(Name) ->
    gen_server:cast(?MODULE, {flush, Name}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
setup_link(Name) ->
    Pid = whereis(Name),
    true = link(Pid),
    ok = gen_server:call(Name, {subscribe, Name}),
    {ok, Pid}.
