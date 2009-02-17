%%% @private

-module(dirty_sup).
-include("spriki.hrl").

-behaviour(supervisor).

-export([ start_link/0, init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Cell = {dirty_cell,{dirty_srv,start_link,[dirty_cell]},
            permanent,2000,worker,[start]},    
    Hypn = {dirty_hypn,{dirty_srv,start_link,[dirty_hypernumber]},
            permanent,2000,worker,[start]},

    {ok,{{one_for_one,60,1}, [ Cell,Hypn ]}}.
