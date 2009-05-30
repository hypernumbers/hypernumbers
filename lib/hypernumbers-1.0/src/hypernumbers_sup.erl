%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @TODO better documentation - what this supervisor is used for, etc, etc
-module(hypernumbers_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @spec start_link() -> Return
%% @doc  Supervisor call back
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> {ok,Children}
%% @doc  Supervisor call back
init([]) ->
    Random    = {random_srv, {random_srv, start_link, []},
                 permanent, 2000, worker, [random_srv]},
    Remote    = {remoting_sup, {remoting_sup, start_link, []},
                 permanent, 2000, supervisor, [start]},
    Dirty_Sup = {dirty_sup, {dirty_sup, start_link, []},
                 permanent, 2000, supervisor, [start]},
    Config    = {hn_config, {hn_config, start_link, []},
                 permanent, 2000, worker, [hn_config]},
    Dirty_Sub = {dirty_subscriber, {dirty_subscriber, start_link, []},
                 permanent, 2000, worker, [hn_config]},
    Status    = {status_srv, {status_srv, start_link, []},
                 permanent, 2000, worker, [status_srv]},

    {ok,{{one_for_one,60,1}, [Config, Random, Remote, 
                              Dirty_Sup, Dirty_Sub, Status]}}.

