%%% @copyright 2010 Hypernumbers Ltd.
%% Top level Hypernumbers app supervisor.

-module(hypernumbers_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([suspend_mochi/0, resume_mochi/0]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

suspend_mochi() ->
    ok = supervisor:terminate_child(?MODULE, mochi_srv).

resume_mochi() ->
    {ok, _} = supervisor:restart_child(?MODULE, mochi_srv),
    ok.

init([]) ->
    Service = {service_sup, {service_sup, start_link, []},
                permanent, infinity, supervisor, [service_sup]},

    Status = {status_srv, {status_srv, start_link, []},
              permanent, 2000, worker, [status_srv]},

    SiteMaster = {sitemaster_sup, {sitemaster_sup, start_link, []},
                  permanent, infinity, supervisor, [sitemaster_sup]},

    Mochi = {mochi_srv, {hn_mochi, start, []},
             permanent, 3000, worker, dynamic},

    {ok,{{one_for_one, 60, 1}, [ Service, 
                                 Status, 
                                 SiteMaster,
                                 Mochi
                               ]}}.
