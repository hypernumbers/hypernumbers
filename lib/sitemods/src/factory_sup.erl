%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       The supervisor for the factory app
%%%
%%% @end
%%% Created :  6 Jan 2010 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(factory_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Factory_Srv = {factory_srv, {factory_srv, start_link, [Args]},
              Restart, Shutdown, Type, [factory_srv]},

    {ok, {SupFlags, [Factory_Srv]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
