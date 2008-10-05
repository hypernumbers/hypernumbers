%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Type casting/coercion functions.

%%% Collect functions are not guaranteed to be stable, i.e. the result list
%%% may have cast values in different order from the original list.

%%% Things that may be encountered by a collector function:
%%% number, string, bool, date, blank, error value.

%%% Basic model:
%%% SOURCE type -> TARGET type
%%% Must supply a rule for each of the types that may be encountered: cast,
%%% ignore or ban.
%%% Two kinds of rules for cast: cast and throw error if not possible (e.g.
%%% trying coerce "hello" to number), and cast and return a default value
%%% if not possible (e.g. 0 for previous example).

%%% If an empty list comes out at the end of all pipes, an error is returned.

%%% FIXME: Collectors for areas are very inefficient.

-module(muin_collect).

-export([flatten_ranges/1,  flatten_arrays/1,
         collect_numbers/2, collect_number/2,
         collect_strings/2, collect_string/2,
         collect_bools/2,   collect_bool/2,
         collect_dates/2,   collect_date/2]).

-export([is_string/1, is_date/1, is_blank/1]).

-compile(export_all). % For testing / to kill warnings.

-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("typechecks.hrl").

-import(muin_util, [cast/2]).

%% @doc Replaces array objects with the values they contain. (Used by
%% implementations of SUM and PRODUCT for example).
flatten_arrays(Vs) ->
    foldl(fun({array, Rows}, Acc) ->
                  Allvs = foldl(fun(Row, Acc1) -> Acc1 ++ Row end,
                                [], Rows),
                  Acc ++ Allvs;
             (X, Acc) ->
                  Acc ++ [X]
          end,
          [], Vs).

%% @doc Replaces range objects with the values they contain. (Used by
%% implementations of SUM and PRODUCT for example).
flatten_ranges(Vs) ->
    foldl(fun({range, Rows}, Acc) ->
                  Allvs = foldl(fun(Row, Acc1) -> Acc1 ++ Row end,
                                [], Rows),
                  Acc ++ Allvs;
             (X, Acc) ->
                  Acc ++ [X]
          end,
          [], Vs).

%% Rules:
%% ignore_strings | cast_strings | cast_strings_zero | ban_strings
%% ignore_bools | cast_bools | ban_bools
%% ignore_dates | cast_dates | ban_dates
%% ignore_blanks | cast_blanks | ban_blanks
collect_numbers(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_number(X, Rules) end, A);
collect_numbers(Vs, Rules) ->
    generic_collect(Vs, Rules, fun erlang:is_number/1, num).

%% @doc Same as <code>collect_numbers</code>
collect_number(V, Rules) ->
    hd(collect_numbers([V], Rules)).

%% Rules:
%% cast_numbers | ignore_numbers | ban_numbers
%% cast_bools | ignore_bools | ban_bools
%% cast_dates | ignore_dates | ban_dates
%% cast_blanks | ignore_blanks | ban_blanks
collect_strings(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_string(X, Rules) end, A);
collect_strings(Vs, Rules) ->
    generic_collect(Vs, Rules, fun is_string/1, str).

%% @doc Same as collect_strings but for one value.
collect_string(V, Rules) ->
    hd(collect_strings([V], Rules)).

%% @doc
collect_bools(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_bool(X, Rules) end, A);
collect_bools(Vs, Rules) ->
    generic_collect(Vs, Rules, fun erlang:is_boolean/1, bool).

%% @doc Same as <code>collect_bools/2</code> but for one value.
collect_bool(V, Rules) ->
    hd(collect_bools([V], Rules)).

%% @doc
collect_dates(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_date(X, Rules) end, A);
collect_dates(Vs, Rules) ->
    generic_collect(Vs, Rules, fun is_date/1, date).

%% @doc Same as <code>collect_dates/2</code> but only for one value.
collect_date(V, Rules) ->
    hd(collect_dates([V], Rules)).

%%% PRIVATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

generic_collect(Vs, Rules, PartitionFun, Targtype) ->
    muin_checks:die_on_errval(Vs),
    {Ok, Notok} = lists:partition(PartitionFun, Vs),
    Ok2 = foldl(fun(cast_numbers, Acc) -> cast_numbers(Acc, Targtype);
                   (cast_strings, Acc) -> cast_strings(Acc, Targtype);
                   (cast_bools, Acc)   -> cast_bools(Acc, Targtype);
                   (cast_dates, Acc)   -> cast_dates(Acc, Targtype);
                   (cast_blanks, Acc)  -> cast_blanks(Acc, Targtype);
                   (Func, Acc)         -> ?MODULE:Func(Acc)
                end,
                Notok, Rules),

    Res = Ok ++ Ok2,

    if(Res == []) -> ?ERR_VAL;
      true        -> Res
    end.

%%% Ignores ~~~~~

ignore_numbers(Xs) ->
    ignore(fun erlang:is_number/1, Xs).

ignore_strings(Xs) ->
    ignore(fun is_string/1, Xs).

ignore_bools(Xs) ->
    ignore(fun erlang:is_boolean/1, Xs).

ignore_dates(Xs) ->
    ignore(fun is_date/1, Xs).

ignore_blanks(Xs) ->
    ignore(fun(X) -> X == blank end, Xs).

%% Complement of lists:filter/2
ignore(Fun, Xs) ->
    filter(fun(X) -> not(Fun(X)) end, Xs).

%%% Casts ~~~~~

cast_strings(Xs, Targtype) ->
    cast_strings_with_opt(Xs, Targtype, fun() -> ?ERR_VAL end).
cast_strings_false(Xs) ->
    cast_strings_with_opt(Xs, bool, fun() -> false end).
cast_strings_zero(Xs) ->
    cast_strings_with_opt(Xs, num, fun() -> 0 end).

cast_strings_with_opt(Xs, Targtype, Action) ->
    Res = generic_cast(Xs, Targtype, fun is_string/1),
    %% Swap all {error, _} for Action().
    foldl(fun({error, _}, Acc) ->
                  [Action() | Acc];
             (X, Acc) ->
                  [X | Acc]
          end,
          [], Res).

cast_numbers(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun erlang:is_number/1).

cast_bools(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun erlang:is_boolean/1).

cast_dates(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun is_date/1).

cast_blanks(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun is_blank/1).

generic_cast(Xs, Targtype, Guardfun) ->
    R = foldl(fun(X, Acc) ->
                      case Guardfun(X) of
                          true  -> [cast(X, Targtype) | Acc];
                          false -> [X | Acc]
                      end
              end,
              [], Xs),
    reverse(R).

%%% Bans ~~~~~

ban_numbers(Xs) ->
    generic_ban(Xs, fun erlang:is_number/1).

ban_strings(Xs) ->
    generic_ban(Xs, fun is_string/1).

ban_bools(Xs) ->
    generic_ban(Xs, fun erlang:is_boolean/1).

ban_dates(Xs) ->
    generic_ban(Xs, fun is_date/1).

ban_blanks(Xs) ->
    generic_ban(Xs, fun is_blank/1).

generic_ban(Xs, Detectorf) ->
    case any(Detectorf, Xs) of
        true  -> ?ERR_VAL;
        false -> Xs
    end.

%%% Type checks ~~~~~

is_string({ustr, Bin}) when is_binary(Bin) ->
    true;
is_string(L) when is_list(L) ->
    io_lib:char_list(L);
is_string(_) ->
    false.

is_date(X) when is_record(X, datetime) ->
    true;
is_date(_) ->
    false.

is_blank(blank) ->
    true;
is_blank(_) ->
    false.

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
