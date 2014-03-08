%%% @copyright 2010-2014 Hypernumbers Ltd.
%% Top level Hypernumbers app supervisor.

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

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

    SiteMaster = {sitemaster_sup, {sitemaster_sup, start_link, []},
                  permanent, infinity, supervisor, [sitemaster_sup]},

    Mochi = {mochi_srv, {hn_mochi, start, []},
             permanent, 3000, worker, dynamic},

    {ok,{{one_for_one, 60, 1}, [ Service,
                                 SiteMaster,
                                 Mochi
                               ]}}.
