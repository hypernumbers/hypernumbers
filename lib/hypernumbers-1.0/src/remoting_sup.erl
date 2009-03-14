%%%-------------------------------------------------------------------
%%% File       remoting_sup.erl
%%% @author    Gordon Guthrie
%%% @doc       supervisor for the remoting server
%%% @copyright Hypernumbers Ltd
%%% @private
%%%
%%% Created     : 6th Feb 2007 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------
-module(remoting_sup).
-include("spriki.hrl").

-behaviour(supervisor).

-export([ start_link/0, init/1 ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    {ok,{{one_for_one,60,1}, [
        {remoting_reg, {remoting_reg, start_link,[]}, 
            permanent, 2000, worker, [remoting_reg]}
    ]}}.
