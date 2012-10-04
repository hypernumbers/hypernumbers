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
    case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting site sup for ~p~n", [Site]);
       _Other     -> ok
    end,
    Id = hn_util:site_to_atom(Site, "_sup"),
    supervisor:start_link({global, Id}, ?MODULE, [Site]).

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
% need the dbsrv (started by calc_sup) to be up before the status_srv.
init([Site]) ->
   {ok, { {one_for_one,1,10},
           [
            {calc_sup,
              {calc_sup, start_link, [Site]},
              permanent,
              infinity,
              supervisor,
              [calc_sup]},

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

            {softphone_srv,
              {softphone_srv, start_link, [Site]},
              permanent,
              2000,
              worker,
              [softphone_srv]},

             {page_srv,
              {page_srv, start_link, [Site]},
              permanent,
              2000,
              worker,
              [page_srv]},

             {tick_srv,
              {tick_srv, start_link, [Site]},
              permanent,
              2000,
              worker,
              [tick_srv]},

             {phonecall_sup,
              {phonecall_sup, start_link, [Site]},
              permanent,
              infinity,
              supervisor,
              [phonecall_sup]},

             {status_srv,
              {status_srv, start_link, [Site]},
              permanent,
              2000,
              worker,
              [status_srv]}

            ]
          }}.

%%====================================================================
%% Internal functions
%%====================================================================
