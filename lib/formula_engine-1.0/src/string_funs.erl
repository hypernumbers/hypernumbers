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

%%% Compile an Excel string function to an Erlang fun object.
%%% E.g. "> 5" ==> #fun(X) -> X > 5 end.
%%% <hasan@hypernumbers.com>
%%% @private

-module(string_funs).
-export([make/1]).

make(Str) ->
    NewStr = re:replace(Str, "\\s+", "", [{return, list}, global]),
    match(NewStr).

match([$<, $> | Tl]) ->
    compile("X=/=" ++ Tl);
match(Str = [$>, $= | _]) ->
    compile("X" ++ Str);
match([$<, $= | Tl]) ->
    compile("X =<" ++ Tl);
match(Str = [H | _]) when H == $> orelse H == $< ->
    compile("X" ++ Str);
match([$= | Tl]) ->
    compile("X==" ++ Tl);
match(Str) -> % match a string
    compile("X==\"" ++ Str ++ "\"").

compile(Body) ->
    Funstr = "fun(X)->" ++ Body ++ "end.",
    {ok, Toks, _} = erl_scan:string(Funstr),
    {ok, Ast} = erl_parse:parse_exprs(Toks),
    {value, Fun, []} = erl_eval:exprs(Ast, []),
    Fun.

%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").
-define(t(FunStr, Arg),
        ?_assert((make(FunStr))(Arg) == true)).
-define(f(FunStr, Arg),
        ?_assert((make(FunStr))(Arg) == false)).

cmp_test_() ->
    [
     ?f(">5", 5),
     ?f("   >  5  ", 5),
     ?t(">5", 6),

     ?t("<123", 0),
     ?f(" < 456", 457),

     ?t("= 20", 20),
     ?f("=15", 0),

     ?t(">= 10", 10),
     ?t(">= 10", 11),
     ?f(">= 10", 9),

     ?t("<= 10", 10),
     ?t("<= 10", 9),
     ?f("<= 10", 11),

     ?f("<> 10", 10),
     ?t("<> 10", 11),
     ?t("<> 10", 9),

     ?t("apple", "apple"),
     ?f("apple", "pear")
    ].
