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
         'and'/1,
         %%iferror/1,
         'not'/1,
         'or'/1]).

'='([V1, V2]) ->
    V1 == V2.

'<>'([V1, V2]) ->
    not('='([V1, V2])).

'>'([A, B]) ->
    A > B.

'<'([A, B]) ->
    A < B.

'<='([A, B]) ->
    A =< B.

'>='([A, B]) ->
    A >= B.

'and'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, cast_blanks, cast_dates]),
    all(fun(X) -> X =/= false end, Bools).

'not'([V]) ->
    not(?bool(V, [cast_strings, cast_numbers, cast_blanks, cast_dates])).

'or'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, cast_blanks, cast_dates]),
    any(fun(X) -> X == true end,
        Bools).
