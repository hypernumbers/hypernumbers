%%%-------------------------------------------------------------------
%%% File        : read_excel_sup.erl
%%% Author      : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : 
%%%
%%% Created     : 25 Jul 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-------------------------------------------------------------------
-module(read_excel_sup).

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
    Read_file_srv = {read_file_srv,{read_file_srv,start_link,[]},
		     permanent,2000,worker,[read_file_srv]},
    {ok,{{one_for_all,0,1}, [Read_file_srv]}}.

%%====================================================================
%% Internal functions
%%====================================================================
