%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Type casting/coercion functions.
%%%
%%% Source values flow through functions that process them according
%%% to the appropriate rule. #VALUE! is returned if the resulting list
%%% is empty.
%%% A rule MUST be supplied for each type (number, string, bool, date,
%%% blank). Rules for error values are optional.
%%%
%%% There are two basic kinds of rules:
%%% 1. Try to cast and throw error if not possible, e.g. "bob" -> int
%%% 2. Try to cast and return default value if not possible. "bob" -> int = 0.
%%%
%%% All collector functions are stable.
%%%
%%% TODO: Collectors for areas are very inefficient.

-module(muin_collect).

-export([flatten_ranges/1,  flatten_arrays/1, flatten_areas/1,
         collect_numbers/2, collect_number/2,
         collect_strings/2, collect_string/2,
         collect_bools/2,   collect_bool/2,
         collect_dates/2,   collect_date/2,
         remove_errors/1]).

-export([is_string/1, is_date/1, is_blank/1]).

-compile(export_all). % For testing / to kill warnings.

-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("typechecks.hrl").

-import(muin_util, [cast/2]).

%% @doc removes any errors
remove_errors(Xs) ->
    Fun  = fun(X) ->
                   case X of
                       {error, _} -> false;
                       _          -> true
                   end
           end,
    Return = lists:filter(Fun, Xs),
    io:format("in remove_errors~n-Xs is ~p~n-Return is ~p~n", [Xs, Return]),
    Return.

%% @doc Replaces array objects with values they contain.
flatten_arrays([Hd|Tl]) ->
    flatten_areas(Hd, Tl, [], fun(X) -> ?is_array(X) end);
flatten_arrays(A) ->
    flatten_arrays([A]).

%% @doc Replaces range objects with values they contain.
flatten_ranges([Hd|Tl]) ->
    flatten_areas(Hd, Tl, [], fun(X) -> ?is_range(X) end);
flatten_ranges(A) ->
    flatten_ranges([A]).

%% @doc Replaces both array and range objects with values they contain.
flatten_areas([Hd|Tl]) ->
    flatten_areas(Hd, Tl, [], fun(X) -> ?is_area(X) end);
flatten_areas([]) ->
    [];
flatten_areas(A) ->
    flatten_areas([A]).

%% Rules:
%% ignore_strings | cast_strings | cast_strings_zero | 
%%  cast_strings_or_ignore | ban_strings
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

%%% TODO: Rewrite this.
%%% Instead of working rule-by-rule, go value-by-value and die on the first encountered error value.
%%% Checking for error values after everything has been [attempted to be] cast works but is not
%%% efficient.
%%% Clients of the interface aren't affected by these changes so the quick fix is ok for now.
%%% FIXME: PartitionFun is not used.
generic_collect(Vs, Rules, _PartitionFun, Targtype) ->

    io:format("hello ~p ~p ~p ~n", [Vs, Rules, Targtype]),
    
    Res = foldl(fun(cast_numbers, Acc) -> cast_numbers(Acc, Targtype);
                   (cast_strings, Acc) -> cast_strings(Acc, Targtype);
                   (cast_bools, Acc)   -> cast_bools(Acc, Targtype);
                   (cast_dates, Acc)   -> cast_dates(Acc, Targtype);
                   (cast_blanks, Acc)  -> cast_blanks(Acc, Targtype);
                   (Func, Acc)         -> ?MODULE:Func(Acc)
                end,
                Vs,
                Rules),

    io:format("bye ~p~n",[Res]),
    
    muin_checks:die_on_errval(Res),

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
    cast_strings_with_opt(Xs, Targtype, fun() -> ?ERRVAL_VAL end).

cast_strings_false(Xs) ->
    cast_strings_with_opt(Xs, bool, fun() -> false end).

cast_strings_zero(Xs) ->
    cast_strings_with_opt(Xs, num, fun() -> 0 end).

cast_strings_or_ignore(Xs) ->
             cast_strings_with_opt(Xs, num, ignore).

cast_strings_with_opt(Xs, Targtype, Action) ->
    Res = generic_cast(Xs, Targtype, fun is_string/1),
    % Swap all {error, _} for Action if Action is a fun()
    % if action is the atom 'ignore' will return nothing
    Fun = fun(X, Acc) ->
                  case X of
                      {error, _} -> case Action of
                                        ignore    -> Acc;
                                        _         -> [Action() | Acc]
                                    end;
                      _          -> [X | Acc]
                  end  
          end,
    reverse(foldl(Fun, [], Res)).

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
        true  -> ?ERR_VAL; % For this to work properly, generic_collect needs to go value-by-value.
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

%%% Generic flattener ~~~~~~~~~~

flatten_areas(Hd, [], Acc, Test) ->
    case Test(Hd) of
        true  -> Acc ++ area_util:to_list(Hd);
        false -> Acc ++ [Hd]
    end;
flatten_areas(Hd, [NHd|Tl], Acc, Test) ->
    case Test(Hd) of
        true  -> flatten_areas(NHd, Tl, Acc ++ area_util:to_list(Hd), Test);
        false -> flatten_areas(NHd, Tl, Acc ++ [Hd], Test)
    end.

%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% -include_lib("eunit/include/eunit.hrl").
%% -define(L1, [1, 2, 3, true, false, "11.22", blank, 99.9]).
%% -define(L2, [0, 0.0, 1, 999, "true", "TrUe", "FALse", blank, true, false]).

%% collect_numbers_test_() ->
%%     [
%%      ?_assert(collect_numbers(?L1, [ignore_strings, ignore_bools, ignore_blanks]) ==
%%               [1, 2, 3, 99.9]),
%%      ?_assert(collect_numbers(?L1, [cast_strings, ignore_bools, ignore_blanks]) ==
%%               [1, 2, 3, 11.22, 99.9]),
%%      ?_assert(collect_numbers(?L1, [cast_strings, cast_bools, ignore_blanks]) ==
%%               [1, 2, 3, 1, 0, 11.22, 99.9]),
%%      ?_assert(collect_numbers(?L1, [cast_strings, cast_bools, zero_blanks]) ==
%%               [1, 2, 3, 1, 0, 11.22, 0, 99.9]),
%%      ?_assert(collect_numbers(?L1, [cast_strings_zero, ignore_bools, zero_blanks]) ==
%%               [1, 2, 3, 11.22, 0, 99.9])
%%     ].

%% collect_bools_test_() ->
%%     [
%%      ?_assert(collect_bools(?L1, [ignore_strings, cast_numbers, ignore_blanks]) ==
%%               [true, true, true, true, false, true]),
%%      ?_assert(collect_bools(?L1, [ignore_strings, cast_numbers, false_blanks]) ==
%%               [true, true, true, true, false, false, true]),
%%      ?_assert(collect_bools(?L1, [ignore_strings, ignore_numbers, false_blanks]) ==
%%               [true, false, false]),
%%      ?_assert(collect_bools(?L2, [ignore_numbers, ignore_strings, ignore_blanks]) ==
%%               [true, false]),
%%      ?_assert(collect_bools(?L2, [cast_numbers, cast_strings, false_blanks]) ==
%%               [false, false, true, true, true, true, false, false, true, false])
%%     ].
