%%%-----------------------------------------------------------------------------
%%% File        : engine_sup.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : This supervisor manages the recalculation engine
%%%
%%% Created     : 15 Oct 2007 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(engine_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%==============================================================================
%% API functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%------------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================
%%------------------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%------------------------------------------------------------------------------
init([]) ->
    Dirty_refs_srv = {dirty_cell,{dirty_srv,start_link,[dirty_cell]},
		      permanent,2000,worker,[start]},
    Dirty_hypn_srv = {dirty_hypn,{dirty_srv,start_link,[dirty_hypernumbers]},
		      permanent,2000,worker,[start]},

    {ok,{{one_for_all,0,1}, [Dirty_refs_srv,Dirty_hypn_srv]}}.

%%==============================================================================
%% Internal functions
%%==============================================================================
