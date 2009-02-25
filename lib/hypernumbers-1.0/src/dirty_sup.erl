%%% @private

-module(dirty_sup).
-include("spriki.hrl").

-behaviour(supervisor).

-export([ start_link/0, init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Cell      = {dirty_cell, {dirty_srv, start_link, [dirty_cell]},
                  permanent, 2000, worker, [start]},    
    HypnIn    = {dirty_hypn_in, {dirty_srv, start_link, [dirty_incoming_hn]},
                  permanent, 2000, worker, [start]},
    HypnOut   = {dirty_hypn_out, {dirty_srv, start_link, [dirty_outgoing_hn]},
                  permanent, 2000, worker, [start]},
    NotifyInc = {dirty_notify_incoming, {dirty_srv, start_link,
                                         [dirty_notify_incoming]},
                 permanent,2000,worker,[start]},

    {ok,{{one_for_one,60,1}, [ Cell, HypnIn, HypnOut, NotifyInc]}}.
