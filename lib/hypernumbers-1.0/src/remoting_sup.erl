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

%% export so that other modules can make child specs
-export([ make_child_spec/1 ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    Sites = hn_setup:get_sites(),
    ChildSpecs = [make_child_spec(X) || X <- Sites],
    {ok,{{one_for_one,60,1}, ChildSpecs}}.


make_child_spec(Site) ->
    Id = hn_util:site_to_name(Site, "_reg"),
    {Id, {remoting_reg, start_link, [Id]}, 
     permanent, 2000, worker, [remoting_reg]}.
     
