%%% @doc Functions to convert between values of different types according to
%%% sets of rules rather than with a pre-defined one-way conversion.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% Rules are names of functions that the inputs get filtered through.

-module(muin_collect).

-export([flatten_ranges/1, flatten_arrays/1,
         collect_strings/2, collect_string/2,
         collect_numbers/2, collect_number/2,
         collect_bools/2, collect_bool/2]).

-compile(export_all). % For testing / to kill warnings.

-include("handy_macros.hrl").
-include("errvals.hrl").

%% Replaces range objects with the values that they contain.
%% flatten_ranges(Xs) ->
%%     foldl(fun(#range{elts = Elts}, Acc) ->
%%                   Vals = map(fun(#elt{val = Val}) ->
%%                                      Val
%%                              end,
%%                              Elts),
%%                   Acc ++ Vals;
%%              (X, Acc) ->
%%                   [X | Acc]
%%           end,
%%           [], Xs).

%% @spec flatten_arrays(Vs :: [V]) -> [V]
%% where
%%   V = Str | number() | Blank | bool() | tuple()
%%   Str = {ustr, binary()}
%%   Blank = blank

%% @doc Replaces array objects with the values they contain. (Used by
%% implementations of SUM and PRODUCT for example).
flatten_arrays(Vs) ->
    Vs.

%% @spec flatten_ranges(Vs :: [V]) -> [V]
%% where
%%   V = Str | number() | Blank | bool() | tuple()
%%   Str = {ustr, binary()}
%%   Blank = blank

%% @doc Replaces range objects with the values theycontain. (Used by
%% implementations of SUM and PRODUCT for example).
flatten_ranges(Vs) ->
    Vs.

collect_strings(Vs, _Rules) ->
    muin_checks:die_on_errval(Vs),
    foldl(fun(X, Acc) -> {X, Acc} end, [], []).

%% @doc Same as collect_strings but for one value.
collect_string(V, Rules) ->
    case collect_strings([V], Rules) of
        []    -> ?ERR_VAL;
        [Str] -> Str
    end.

collect_numbers(Vs, Rules) ->
    muin_checks:die_on_errval(Vs),
    foldl(fun(cast_strings, Acc) ->
                  cast_strings(Acc, num);
             (cast_bools, Acc) ->
                  cast_bools(Acc, num);
             (Func, Acc) ->
                  ?MODULE:Func(Acc)
          end,
          Vs, Rules).

%% @doc Same as <code>collect_numbers</code>
collect_number(V, Rules) ->
    case collect_numbers([V], Rules) of
        []    -> ?ERR_VAL;
        [Num] -> Num
    end.

%% @spec collect_bools(Vs :: [V], Rules :: [Rule]) -> [bool()]
%% where
%%   V = Str | number() | Blank | bool() | tuple()
%%   Str = {ustr, binary()}
%%   Blank = blank
%%   Rule = ignore_strings | cast_strings | cast_strings_false |
%%          ignore_numbers | cast_numbers | ignore_blanks | false_blanks

%% @doc <p>Returns a list of booleans sourced from a list of values according to
%% the specified rules.</p>
%% <p>If <code>cast_strings</code> is given then <code>#VALUE!</code> will be returned if
%%    a string cannot be coerced into a boolean. If <code>cast_strings_false</code>
%%    is given instead, such strings will be replaced by <code>false</code>.</p>
collect_bools(Vs, Rules) ->
    muin_checks:die_on_errval(Vs),
    foldl(fun(cast_strings, Acc) ->
                  cast_strings(Acc, bool);
             (cast_numbers, Acc) ->
                  cast_numbers(Acc, bool);
             (Func, Acc) ->
                  ?MODULE:Func(Acc)
          end,
          Vs, Rules).

%% @doc Same as <code>collect_bools/2</code> but for one value.
collect_bool(V, Rules) ->
    case collect_bool([V], Rules) of
        []  -> ?ERR_VAL;
        [B] -> B
    end.

%%% PRIVATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ignore_numbers(Xs) ->
    ignore(fun erlang:is_number/1, Xs).

ignore_strings(Xs) ->
    ignore(fun erlang:is_list/1, Xs). %% STR!

ignore_bools(Xs) ->
    ignore(fun erlang:is_boolean/1, Xs).

ignore_blanks(Xs) ->
    ignore(fun(X) -> X == blank end, Xs).

%% The opposite of lists:filter/2
ignore(Fun, Xs) ->
    filter(fun(X) -> not(Fun(X)) end, Xs).

cast_strings(Xs, Targtype) ->
    cast_strings_with_opt(Xs, Targtype, fun() -> ?ERR_VAL end).
cast_strings_false(Xs) ->
    cast_strings_with_opt(Xs, bool, fun() -> false end).
cast_strings_zero(Xs) ->
    cast_strings_with_opt(Xs, num, fun() -> 0 end).

%% Action = what to do when the string can't be coerced into a value of
%% target type.
cast_strings_with_opt(Xs, Targtype, Action) ->
    R = foldl(fun(X, Acc) when is_list(X) -> %% STR!
                      case muin_util:cast(X, Targtype) of
                          {error, _} -> Action();
                          Val        -> [Val | Acc]
                      end;
                 (X, Acc) ->
                      [X | Acc]
              end,
              [], Xs),
    reverse(R).

cast_bools(Xs, Targtype) ->
    R = foldl(fun(B, Acc) when is_boolean(B) ->
                      [muin_util:cast(B, Targtype) | Acc];
                 (X, Acc) ->
                      [X | Acc]
              end,
              [], Xs),
    reverse(R).

zero_blanks(Xs) ->
    cast_blanks(Xs, 0).

false_blanks(Xs) ->
    cast_blanks(Xs, false).

cast_blanks(Xs, Castval) ->
    R = foldl(fun(blank, Acc) ->
                      [Castval | Acc];
                 (X, Acc) ->
                      [X | Acc]
              end,
              [], Xs),
    reverse(R).

cast_numbers(Xs, Targtype) ->
    R = foldl(fun(N, Acc) when is_number(N) ->
                      [muin_util:cast(N, Targtype) | Acc];
                 (X, Acc) ->
                      [X | Acc]
              end,
              [], Xs),
    reverse(R).

%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").
-define(L1, [1, 2, 3, true, false, "11.22", blank, 99.9]).
-define(L2, [0, 0.0, 1, 999, "true", "TrUe", "FALse", blank, true, false]).
             
collect_numbers_test_() ->
    [
     ?_assert(collect_numbers(?L1, [ignore_strings, ignore_bools, ignore_blanks]) ==
              [1, 2, 3, 99.9]),
     ?_assert(collect_numbers(?L1, [cast_strings, ignore_bools, ignore_blanks]) ==
              [1, 2, 3, 11.22, 99.9]),
     ?_assert(collect_numbers(?L1, [cast_strings, cast_bools, ignore_blanks]) ==
              [1, 2, 3, 1, 0, 11.22, 99.9]),
     ?_assert(collect_numbers(?L1, [cast_strings, cast_bools, zero_blanks]) ==
              [1, 2, 3, 1, 0, 11.22, 0, 99.9]),
     ?_assert(collect_numbers(?L1, [cast_strings_zero, ignore_bools, zero_blanks]) ==
              [1, 2, 3, 11.22, 0, 99.9])
    ].

collect_bools_test_() ->
    [
     ?_assert(collect_bools(?L1, [ignore_strings, cast_numbers, ignore_blanks]) ==
              [true, true, true, true, false, true]),
     ?_assert(collect_bools(?L1, [ignore_strings, cast_numbers, false_blanks]) ==
              [true, true, true, true, false, false, true]),
     ?_assert(collect_bools(?L1, [ignore_strings, ignore_numbers, false_blanks]) ==
              [true, false, false]),
     ?_assert(collect_bools(?L2, [ignore_numbers, ignore_strings, ignore_blanks]) ==
              [true, false]),
     ?_assert(collect_bools(?L2, [cast_numbers, cast_strings, false_blanks]) ==
              [false, false, true, true, true, true, false, false, true, false])
    ].
