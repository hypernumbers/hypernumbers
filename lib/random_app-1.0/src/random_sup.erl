%%%-------------------------------------------------------------------
%%% File        : random_sup.ler
%%% Author      : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : 
%%%
%%% Created     : 17 August 2008 by Gordon Guthrie <gordonguthrie@localhost>
%%%-------------------------------------------------------------------
-module(random_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Random_srv = {random_srv,{random_srv,start_link,[]},
		     permanent,2000,worker,[random_srv]},
    {ok,{{one_for_all,0,1}, [Random_srv]}}.

%%====================================================================
%% Internal functions
%%====================================================================
