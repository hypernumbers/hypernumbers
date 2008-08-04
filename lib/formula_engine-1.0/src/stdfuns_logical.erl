%%% @doc Logical functions.
%%% @author <hasan@hypernumbers.com>

-module(stdfuns_logical).
-include("handy_macros.hrl").
-include("typechecks.hrl").
-import(xltconv, [to_b/1]).

-export([
         eq/1,
         neq/1,
         lt/1,
         gt/1,
         lte/1,
         gte/1,
         'if'/1,
         'and'/1,
         %%iferror/1,
         'not'/1,
         'or'/1
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
    ?COND(to_b(Test), TrueVal, FalseVal).

'and'([Vs]) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, false_blanks, cast_dates]),
    all(fun(X) -> X =/= false end, Bools).

'not'([Val]) ->
    ?ensure_no_errvals([Val]),
    not(to_b(Val)).

'or'([Vals]) ->
    Flatvals = flatten(Vals),
    ?ensure_no_errvals(Flatvals),
    any(fun(X) -> to_b(X) == true end,
        Flatvals).
