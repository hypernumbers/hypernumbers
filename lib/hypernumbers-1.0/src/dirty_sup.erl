%%% @private

-module(dirty_sup).
-include("spriki.hrl").

-behaviour(supervisor).

-export([ start_link/0, init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Cell        = {dirty_cell, {dirty_srv, start_link, [dirty_cell]},
                   permanent, 2000, worker, [start]},    
    Incoming    = {dirty_inc, {dirty_srv, start_link, [dirty_incoming_hn]},
                   permanent, 2000, worker, [start]},
    IncNotify   = {dirty_notify_inc, {dirty_srv, start_link,
                                      [dirty_notify_incoming]},
                   permanent,2000,worker,[start]},
    IncCreate   = {dirty_inc_create, {dirty_srv, start_link,
                                      [dirty_incoming_create]},
                   permanent, 2000, worker, [start]},
    Outgoing    = {dirty__outgoing, {dirty_srv, start_link, [dirty_outgoing_hn]},
                   permanent, 2000, worker, [start]},
    OutUpdate   = {dirty_out_update, {dirty_srv, start_link,
                                      [dirty_outgoing_update]},
                   permanent, 2000, worker, [start]},
    
    {ok,{{one_for_one,60,1}, [ Cell, Incoming, IncNotify, IncCreate,
                               Outgoing, OutUpdate]}}.
