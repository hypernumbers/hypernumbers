%%%-------------------------------------------------------------------
%%% File        : remoting.erl
%%% Author      : Gordon Guthrie 
%%% Description : 
%%%
%%% Created     : 6th Feb 2007 by  <gordonguthrie@backawinner.gg>
%%%-------------------------------------------------------------------
-module(remoting).
-include("spriki.hrl").

-behaviour(application).

-export([ start/2, stop/1 ]).

start(_Type, _StartArgs) ->
    
    %% Start the supervisor handling incoming socket
    %% Connections
    remoting_sup:start_link().

stop(_State) -> 
    ok.