%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc    GUI Generator application.

-module(gui_generator_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_Type, _Args) ->
    case gui_generator_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error     -> Error
    end.


stop(_State) ->
    ok.
            
