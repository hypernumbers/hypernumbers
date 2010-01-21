%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(sitemods_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
start(_Type, _Args) ->
    {ok, _Pid} = sitemods_sup:start_link().

%% @spec stop(State) -> ok
%% @doc  Application Callback
stop(_State) ->
    ok.

