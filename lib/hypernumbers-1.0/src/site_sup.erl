-module(site_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_sup"),
    supervisor:start_link({local, Id}, ?MODULE, [Site]).

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
init([Site]) ->

    {ok, { {one_for_one,1,10},
           [ {dbsrv, 
              {dbsrv, start_link, [Site]},
              permanent,
              infinity, 
              supervisor, 
              [dbsrv]},

             {remoting_reg, 
              {remoting_reg, start_link, [Site]},
              permanent, 
              2000, 
              worker, 
              [remoting_reg]},

             {auth_srv, 
              {auth_srv, start_link, [Site]},
              permanent, 
              2000, 
              worker, 
              [auth_srv]},
             
             {zinf_srv, 
              {zinf_srv, start_link, [Site]},
              permanent, 
              2000, 
              worker, 
              [zinf_srv]},

             {page_srv, 
              {page_srv, start_link, [Site]},
              permanent, 
              2000, 
              worker, 
              [page_srv]}

            ]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
