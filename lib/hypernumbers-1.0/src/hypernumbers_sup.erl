%% Top level Hypernumbers app supervisor.

-module(hypernumbers_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @spec start_link() -> Return
%% @doc  Supervisor call back
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> {ok,Children}
%% @doc  Supervisor call back
init([]) ->
    Status = {status_srv, {status_srv, start_link, []},
              permanent, 2000, worker, [status_srv]},

    SiteMaster = {sitemaster_sup, {sitemaster_sup, start_link, []},
                  permanent, infinity, supervisor, [sitemaster_sup]},
    
    {ok,{{one_for_one, 60, 1}, [ Status,
                                 SiteMaster
                                ]}}.

