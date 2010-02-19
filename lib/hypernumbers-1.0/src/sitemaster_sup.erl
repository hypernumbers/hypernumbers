-module(sitemaster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         add_site/1]).

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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_site(string()) -> {ok, pid()}.
add_site(Site) ->
    ChildSpec = gen_child_spec(Site),
    supervisor:start_child(?MODULE, ChildSpec).

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
    Sites = hn_setup:get_sites(),
    ChildSpecs = [gen_child_spec(S) || S <- Sites],
    {ok,{{one_for_one,1,30}, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

gen_child_spec(S) ->
    {S, {site_sup, start_link, [S]},
     permanent, infinity, supervisor, [site_sup]}.

