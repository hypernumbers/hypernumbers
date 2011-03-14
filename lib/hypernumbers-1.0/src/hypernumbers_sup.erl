%%% @copyright 2010 Hypernumbers Ltd.
%% Top level Hypernumbers app supervisor.

-module(hypernumbers_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).
-export([suspend_mochi/0, resume_mochi/0]).


start_link() ->
    start_link(true).

start_link(StartSites) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StartSites]).

suspend_mochi() ->
    ok = supervisor:terminate_child(?MODULE, mochi_srv).

resume_mochi() ->
    {ok, _} = supervisor:restart_child(?MODULE, mochi_srv),
    ok.

init([StartSites]) ->
    Service = {service_sup, {service_sup, start_link, []},
                permanent, infinity, supervisor, [service_sup]},

    SiteMaster = {sitemaster_sup, {sitemaster_sup, start_link, [StartSites]},
                  permanent, infinity, supervisor, [sitemaster_sup]},

    Mochi = {mochi_srv, {hn_mochi, start, []},
             permanent, 3000, worker, dynamic},

    {ok,{{one_for_one, 60, 1}, [ Service,
                                 SiteMaster,
                                 Mochi
                               ]}}.
