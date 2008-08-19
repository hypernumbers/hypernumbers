-module(hn_calc_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MOD, hn_calc).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Child = {?MOD,{?MOD,start_link,[]},permanent,2000,worker,[?MOD]},
    {ok,{{one_for_all,0,1}, [Child]}}.
