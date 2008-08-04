%%% Engineering functions.
%%% <hasan@hypernumbers.com>

-module(stdfuns_eng).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

-export([
         delta/1,
         gestep/1
        ]).

-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

delta([V1, V2]) ->
    [Num1, Num2] = ?numbers([V1, V2], ?default_rules),
    ?COND(Num1 == Num2, 1, 0).

gestep([N]) ->
    gestep([N, 0]);
gestep([N, S]) ->
    Num = cast(N, num),
    Step = cast(S, num),
    ?ensure_numbers([Num, Step]),
    ?COND(Num >= Step, 1, 0).
    
