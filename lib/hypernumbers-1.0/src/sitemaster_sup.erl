-module(sitemaster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_link/1,
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
    start_link(true).

start_link(StartSites) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StartSites]).

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
    supervisor:terminate_child(?MODULE, Site),
    ok = supervisor:delete_child(?MODULE, Site).

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
init([StartSites]) ->
    io:format("Hypernumbers Startup: the sitemaster_sup is initing...~n"),
    Sites = hn_setup:get_sites(),
    ChildSpecs = [gen_child_spec(S) || S <- Sites],
    case StartSites of
        true  -> {ok,{{one_for_one,1,30}, ChildSpecs}};
        false -> {ok,{{one_for_one,1,30}, []}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

gen_child_spec(S) ->
    {S, {site_sup, start_link, [S]},
     permanent, infinity, supervisor, [site_sup]}.

