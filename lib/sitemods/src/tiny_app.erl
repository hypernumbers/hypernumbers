%% @author Gordon Guthrie gordon@hypernumbers.com
%% @copyright Hypernumbers Ltd.
-module(tiny_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
start(_Type, _Args) ->
    case tiny_util:get_tiny_site() of
        no_site  -> error_logger:info_msg("the tiny app needs tiny.hn to exist!~n"),
                    application:stop(tiny);
        _Site    -> {ok , _Pid} = tiny_sup:start_link()
    end.

%% @spec stop(State) -> ok
%% @doc  Application Callback
stop(_State) ->
    ok.
