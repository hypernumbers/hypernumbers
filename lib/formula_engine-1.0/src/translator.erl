%%% @copyright (C) 2009-2014, Hypernumbers Ltd.

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

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
