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
            {ok, true}  -> [{phoneredir_srv, true}];
            {ok, false} -> []
        end,

    HighRise = case application:get_env(hypernumbers, environment) of
                   {ok, development} -> [{highrise_srv, true}];
                   _                 -> []
               end,

    S2 = lists:merge([Services, P, HighRise]),

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
