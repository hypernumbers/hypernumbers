%%% @private
-module(translator).
-export([do/1]).

do(Formula) ->

    % TODO: get french back in, stop it replacing trim with mirr
    %% Languages = [russian_lexer, spanish_lexer, portuguese_lexer,
    %%              german_lexer, french_lexer, italian_lexer],

    Languages = [russian_lexer, spanish_lexer, portuguese_lexer,
                 german_lexer, italian_lexer],

    Fun = fun(Frontend, NewFormula) ->
                  {ok, Tokens, _} = Frontend:string(NewFormula),
                  lists:flatmap(fun({_, YYtext}) -> YYtext end, Tokens)
          end,
    
    R = lists:foldl(Fun, [$=|Formula], Languages),
    tl(R). % Don't want the equals sign.
