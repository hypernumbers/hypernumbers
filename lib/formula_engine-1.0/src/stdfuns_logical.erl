%%% @doc Logical functions.
%%% @author <hasan@hypernumbers.com>

-module(stdfuns_logical).
-include("handy_macros.hrl").
-include("typechecks.hrl").

-export([
         '='/1,
         '<>'/1,
         '<'/1,
         '>'/1,
         '<='/1,
         '>='/1,
         'if'/1,
         'and'/1,
         %%iferror/1,
         'not'/1,
         'or'/1]).

'='([A, B]) ->
    A == B.

'<>'([A, B]) ->
    not('='([A, B])).

'>'([A, B]) ->
    A > B.

'<'([A, B]) ->
    A < B.

'<='([A, B]) ->
    A =< B.

'>='([A, B]) ->
    A >= B.

'if'([Test, TrueVal, FalseVal]) ->
    ?COND(Test, TrueVal, FalseVal).

'and'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, cast_blanks, cast_dates]),
    all(fun(X) -> X =/= false end, Bools).

'not'([Val]) ->
    ?ensure_no_errvals([Val]),
    not(Val).

'or'([Vals]) ->
    Flatvals = flatten(Vals),
    ?ensure_no_errvals(Flatvals),
    any(fun(X) -> X == true end,
        Flatvals).
