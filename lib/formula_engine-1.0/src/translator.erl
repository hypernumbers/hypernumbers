-module(translator).
-include("handy_macros.hrl").
-export([do/1]).

do(Formula) ->
    %% Just using the Russian frontend now. In future, the front-end to use can
    %% be read from app config, or we can run the formula through all available
    %% frontends.
    {ok, Tokens, _} = ru_fe:string(Formula),
    flatten(map(fun({_, YYtext}) ->
                        YYtext
                end,
                Tokens)).
