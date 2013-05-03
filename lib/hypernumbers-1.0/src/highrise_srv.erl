%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc        populates the CRM system in HighRise with new site
%%%             commissioned
%%% @end
%%% Created : 18th March 2013
%%%-------------------------------------------------------------------
-module(highrise_srv).

-behaviour(gen_server).

-include("spriki.hrl").

-define(WRITELOG, new_db_api:write_commission_logD).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% API
-export([
         site_commissioned/0
        ]).

-define(SERVER, ?MODULE).

-record(state, {calls = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================
site_commissioned() ->
    gen_server:call(?SERVER, site_commissioned).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting highrise_srv~n");
       _Other     -> ok
    end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ok = push_to_highrise(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(site_commissioned, _From, State) ->
    io:format("Site commissioned...~n"),
    ok = push_to_highrise(),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
push_to_highrise() ->
    Comms = new_db_api:get_unprocessed_commissionsD(),
    io:format("Commissions is ~p~n", [Comms]),
    [ok = write_to_highrise(X)  || X <- Comms],
    [ok = ?WRITELOG(#refX{site = CS, path = CP, obj = CO}, NewS, STy, U, E, Z)
     || #commission{uid = U, email = E, site = NewS, sitetype = STy, zone = Z,
                    commissioning_site = CS, commissioning_path = CP,
                    commissioning_cell = CO, synched = true} <- Comms],
    ok.

write_to_highrise(X) ->
    io:format("Spoof write to highrise for ~p~n", [X]),
    ok.

