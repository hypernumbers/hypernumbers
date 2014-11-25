%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011-2014, Hypernumbers Ltd
%%% @doc       This supervisor manages the individual sites
%%%
%%% @end
%%% Created :  5 Sep 2011 by gordon@hypernumbers.com
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

-module(sitemaster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         add_site/1,
         delete_site/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    case application:get_env(hypernumbers, startup_debug) of
        {ok, true} -> io:format("...starting sitemaster_sup~n");
        _Other     -> ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_site(string()) -> ok.
add_site(Site) ->
    ChildSpec = gen_child_spec(Site),
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_} -> ok;
        {error, {already_started, _}} -> ok;
        Else -> Else
    end.

-spec delete_site(string()) -> ok.
delete_site(Site) ->
    ok = supervisor:terminate_child(?MODULE, Site).

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
    io:format("Hypernumbers Startup: the sitemaster_sup is initing...~n"),
    Sites = hn_setup:get_sites(),
    ChildSpecs = [gen_child_spec(S) || S <- Sites],
    case application:get_env(hypernumbers, should_start_sites) of
        {ok, false} -> {ok,{{one_for_one,1,30}, []}};
        _           -> {ok,{{one_for_one,1,30}, ChildSpecs}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

gen_child_spec(S) ->
    {S, {site_sup, start_link, [S]},
     temporary, infinity, supervisor, [site_sup]}.
