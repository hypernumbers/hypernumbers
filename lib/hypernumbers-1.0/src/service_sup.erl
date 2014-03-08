%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2008 - 2014, Hypernumbers.com
%%% @doc       Supervisors per-cluster services
%%% @end
%%% Created :  by gordon@hypernumbers.com
%%%-------------------------------------------------------------------

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

-module(service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting services~n");
       _Other     -> ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("Vixo Startup: the service_sup is initing...~n"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    IsDev = case  application:get_env(hypernumbers, environment) of
                {ok, development} -> true;
                {ok, server_dev}  -> true;
                {ok, production}  -> false
            end,

    {ok, Services} = application:get_env(hypernumbers, services),

    P = case application:get_env(hypernumbers, phoneredirect) of
            {ok, true}  -> [{phoneredir_srv, true} | Services];
            {ok, false} -> Services
        end,

    S2 = case application:get_env(hypernumbers, featureflag) of
             {ok, on} -> [{marketing_integration_srv, true} | P];
             _        -> P
         end,

    ChildSpecs = [gen_child_spec(S) || {S,X} <- S2, (IsDev or X)],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

gen_child_spec(Service) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    {Service, {Service, start_link, []},
     Restart, Shutdown, Type, [Service]}.
