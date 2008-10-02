%%%-----------------------------------------------------------------------------
%%% File        : engine_sup.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : This supervisor manages the recalculation engine
%%%
%%% Created     : 15 Oct 2007 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(hypernumbers_sup).
-behaviour(supervisor).

-export([start_link/0]).
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

    Random_srv     = {random_srv,{random_srv,start_link,[]},
                       permanent,2000,worker,[random_srv]},

    Remote_sup     = {remoting_sup,{remoting_sup,start_link,[]},
                       permanent,2000,supervisor,[start]},

    Dirty_refs_srv = {dirty_cell,{dirty_srv,start_link,[dirty_cell]},
                      permanent,2000,worker,[start]},

    Dirty_hypn_srv = {dirty_hypn,{dirty_srv,start_link,[dirty_hypernumber]},
                      permanent,2000,worker,[start]},

    {ok,{{one_for_one,2000,60}, [Dirty_refs_srv,
                                 Dirty_hypn_srv,
                                 Remote_sup,
                                 Random_srv]}}.
