%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc    Introspection application.

-module(introspection_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_Type, _Args) ->
    case introspection_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error     -> Error
    end.


stop(_State) ->
    ok.
            
