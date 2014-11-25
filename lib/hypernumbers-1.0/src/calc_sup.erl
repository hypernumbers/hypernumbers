%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012-2014, Hypernumbers Ltd
%%% @doc       The recalculation stuff should restart as a unit
%%%            (ie dbsrv and zinf_srv)
%%%
%%% @end
%%% Created :  7 Jan 2012 by  <gordon@hypernumbers.com>
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

-module(calc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
start_link(Site) ->
    case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting calc_srv for ~p~n", [Site]);
       _Other     -> ok
    end,
    Id = hn_util:site_to_atom(Site, "_calc_sup"),
    supervisor:start_link({global, Id}, ?MODULE, [Site]).

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
init([Site]) ->
       {ok, { {one_for_all,1,10},
           [ {dbsrv,
              {dbsrv, start_link, [Site]},
              permanent,
              infinity,
              supervisor,
              [dbsrv]},

             {zinf_srv,
              {zinf_srv, start_link, [Site]},
              permanent,
              2000,
              worker,
              [zinf_srv]}

            ]
          }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
