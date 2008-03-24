%%% @doc Logical functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(stdfuns_logical).
-include("handy_macros.hrl").
-export([
         eq/1,
         neq/1,
         lt/1,
         gt/1,
         lte/1,
         gte/1,
         'if'/1
        ]).

eq([A, B]) ->
    A == B.

neq([A, B]) ->
    not(eq([A, B])).

gt([A, B]) ->
    A > B.

lt([A, B]) ->
    A < B.

lte([A, B]) ->
    A =< B.

gte([A, B]) ->
    A >= B.

'if'([Test, TrueVal, FalseVal]) ->
    ?COND(value_to_boolean(Test), TrueVal, FalseVal).

value_to_boolean(true) ->
    true;
value_to_boolean(false) ->
    false.
