%%% @private

-module(dirty_sup).
-include("spriki.hrl").

-behaviour(supervisor).

-export([ start_link/0, init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Cell      = {dirty_cell, {dirty_srv, start_link,
                              [dirty_cell]},
                 permanent, 2000, worker, [start]},    
    NotifyIn  = {notify_in, {dirty_srv, start_link,
                             [dirty_notify_in]},
                 permanent, 2000, worker, [start]},
    BackIn    = {notify_back_in, {dirty_srv, start_link,
                                  [dirty_notify_back_in]},
                 permanent,2000,worker,[start]},
    IncCreate = {inc_hn_create, {dirty_srv, start_link,
                                   [dirty_inc_hn_create]},
                 permanent, 2000, worker, [start]},
    NotifyOut = {notify_out, {dirty_srv, start_link,
                              [dirty_notify_out]},
                 permanent, 2000, worker, [start]},
    BackOut   = {notify_back_out, {dirty_srv, start_link,
                                   [dirty_notify_back_out]},
                 permanent, 2000, worker, [start]},
    
    {ok,{{one_for_one,60,1}, [ Cell, NotifyIn, BackIn, IncCreate,
                               NotifyOut, BackOut]}}.
