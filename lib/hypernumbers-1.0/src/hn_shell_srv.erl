%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_shell_srv).

-behaviour(gen_server).

-export([
         connect/0,
         disconnect/0
         % deactivate/0,
         % activate/0
        ]).

%% gen_server callbacks
-export([ start_link/0, init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3]).

-record(client, { notify = true      :: atom,
                  pid    = undefined :: pid(),
                  leader = undefined :: pid() }).

-record(state,  { clients  :: list() }).


-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{clients = []}}.

handle_call(connect, {From, _Tag}, #state{clients=Clients}=State) ->
    {reply, ok, State#state{clients = add_client(From, Clients) }};
handle_call(disconnect, {From, _Tag}, #state{clients=Clients}=State) ->
    {reply, ok, State#state{clients = remove_client(From, Clients) }}.
%% handle_call(deactivate, {From, _Tag}, #state{clients=Clients}=State) ->
%%     {reply, ok, State#state{ clients = set_notify(From, Clients, false) }};
%% handle_call(activate, {From, _Tag}, #state{clients=Clients}=State) ->
%%     {reply, ok, State#state{clients = set_notify(From, Clients, true) }}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->    
    {noreply, State};
handle_info({trace, _Pid,call, {_M, _F, [Format, Args]}}, State) ->    
    [ io:format(Client#client.leader, Format, Args)
      || Client <- State#state.clients, Client#client.notify == true ],
    {noreply, State};
handle_info(Else, State) ->    
    [ catch io:format(Client#client.leader, "Unrecognised ~p~n",[Else])
      || Client <- State#state.clients ],
    {noreply, State}.

terminate(_Reason, _State) ->
    do_trace(false),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect() ->
    gen_server:call(?MODULE, connect).
disconnect() ->
    gen_server:call(?MODULE, connect).

%% deactivate() ->
%%     gen_server:call(?MODULE, deactivate).
%% activate() ->
%%     gen_server:call(?MODULE, activate).
    
%% set_notify(From, Clients, Val) ->
%%     F = fun(#client{pid=Pid} = Client) ->
%%                 case Pid == From of
%%                     true  -> Client#client{notify=Val};
%%                     _Else -> Client
%%                 end
%%         end,    
%%     lists:map(F, Clients).

do_trace(Val) ->
    erlang:trace(all, Val, [call, {tracer, self()}]),
    erlang:trace_pattern({error_logger, info_msg, '_'}, Val, [local]).

add_client(Pid, Clients) ->
    
    (Clients == []) andalso
        do_trace(true),

    % TODO: I want to setup proper links so when the client disconnects
    % the server isnt left with dangling pid's
    link(Pid),

    {group_leader, Leader} = erlang:process_info(Pid, group_leader),
    [ #client{pid=Pid, leader=Leader} | Clients ].

remove_client(Pid, Clients) ->
    
    NewClients = [ X || X <- Clients, X#client.pid =/= Pid ],
    
    (NewClients == [])
        andalso do_trace(false),
    
    NewClients.
