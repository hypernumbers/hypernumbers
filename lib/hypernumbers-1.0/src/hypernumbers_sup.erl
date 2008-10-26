%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
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
    Random = {random_srv,{random_srv,start_link,[]},
              permanent,2000,worker,[random_srv]},
    Remote = {remoting_sup,{remoting_sup,start_link,[]},
              permanent,2000,supervisor,[start]},
    Dirty  = {dirty_sup,{dirty_sup,start_link,[]},
              permanent,2000,supervisor,[start]},

    {ok,{{one_for_one,60,1}, [Dirty, Remote, Random]}}.
