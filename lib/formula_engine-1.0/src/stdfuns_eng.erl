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

delta([N1, N2]) ->
    Num1 = cast(N1, num),
    Num2 = cast(N2, num),
    ?ensure_numbers([Num1, Num2]),
    ?COND(N1 == N2, 1, 0).

gestep([N]) ->
    gestep([N, 0]);
gestep([N, S]) ->
    Num = cast(N, num),
    Step = cast(S, num),
    ?ensure_numbers([Num, Step]),
    ?COND(Num >= Step, 1, 0).
    
