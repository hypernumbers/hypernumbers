-module(translator).
-include("handy_macros.hrl").
-export([do/1]).

do(Formula) ->
    R = foldl(fun(Frontend, NewFormula) ->
                      {ok, Tokens, _} = Frontend:string(NewFormula),
                      flatmap(fun({_, YYtext}) -> YYtext end, Tokens)
              end,
              [$=|Formula],
              [russian_lexer, spanish_lexer, portuguese_lexer, german_lexer,
               french_lexer, italian_lexer]),
    tl(R). % Don't want the equals sign.
