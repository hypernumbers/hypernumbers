%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       This supervisor creates new registers for individual
%%%            pages
%%%
%%% @end
%%% Created :  5 Sep 2011 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(remoting_sup).

-behaviour(supervisor).

%% Register API
-export([
         add_page_register/2
        ]).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
add_page_register(Site, Path) ->
    ChildSpec = gen_child_spec(Site, binary_to_term(Path)),
    ID = hn_util:site_to_atom(Site, "_rem_sup"),
    supervisor:start_child({global, ID}, ChildSpec).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_rem_sup"),
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
init(_Args) -> {ok, {{one_for_one, 1, 30}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_child_spec(Site, Path) ->
    ID = util2:get_timestamp(),
    {ID, {remoting_srv, start_link, [Site, Path]},
     transient, 5, worker, [remoting_srv]}.
