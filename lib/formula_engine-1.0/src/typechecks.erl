%%% @author    Gordon Guthrie <>
%%% @copyright (C) 2010, Gordon Guthrie
%%% @doc       A set of standard type checks
%%%
%%% @end
%%% Created : 31 Dec 2010 by Gordon Guthrie <>

-module(typechecks).

-export([
         % throws on error
         throw_std_bools/1,
         throw_std_strs/1,
         throw_std_ints/1,
         throw_std_nums/1,
         throw_flat_strs/1,
         throw_html_box_contents/1,
         throw_std_inc_strs/1,
         % returns errors to fn
         std_bools/1,
         std_strs/1,
         std_ints/1,
         std_pos_ints/1,
         std_nums/1,
         flat_strs/1
        ]).

% schmancies!
-export([
         html_box_contents/1,
         throw_std_phone_no/2,
         std_phone_no/2,
         throw_rgbcolours/1,
         rgbcolours/1
        ]).

-export([
         in_range/3,
         is_member/2
        ]).

-include("typechecks.hrl").

throw_std_phone_no(X, Y)    -> errthrow(std_phone_no(X, Y)).
throw_rgbcolours(X)         -> errthrow(rgbcolours(X)).
throw_std_bools(X)          -> errthrow(std_bools(X)).
throw_std_strs(X)           -> errthrow(std_strs(X)).
throw_std_ints(X)           -> errthrow(std_ints(X)).
throw_std_nums(X)           -> errthrow(std_nums(X)).
throw_flat_strs(X)          -> errthrow(flat_strs(X)).
throw_html_box_contents(X)  -> errthrow(html_box_contents(X)).
throw_std_inc_strs(X)       -> errthrow(std_inc_strs(X)).

errthrow(X) -> th(X, []).

th([], Acc)                           -> lists:reverse(Acc);
th([H | _T], _Acc) when ?is_errval(H) -> throw(H);
th([H | T], Acc)                      -> th(T, [H | Acc]);
% might not be a list returned if its an error...
th(X, []) when ?is_errval(X)          -> throw(X).

std_phone_no(Prefix, blank) ->
    std_phone_no(Prefix, "");
std_phone_no(Prefix, Number) when is_integer(Number) ->
    std_phone_no(Prefix, integer_to_list(Number));
std_phone_no("", "++" ++ Number) ->
    N2 = compress(Number),
    {"", N2};
std_phone_no("", "+" ++ Number) ->
    N2 = compress(Number),
    {"", N2};
std_phone_no("", Number) when is_list(Number) ->
    N2 = compress(Number),
    {"", N2};
std_phone_no(Prefix, Number) when is_list(Number) ->
    P2 = normalise(Prefix),
    N2 = compress(Number),
    {P2, N2};
% last chance throw it to integer
std_phone_no(Prefix, Number) ->
    [N2] = std_ints([Number]),
    std_phone_no(Prefix, N2).

rgbcolours([$#| Rest]) ->
    case length(Rest) of
       6  -> ok;
       3  -> ok;
       _O -> ?ERR_VAL
    end,
    [Colours] = std_strs([Rest]),
    Re = "^[a-fA-F0-9]+$", %"
    case re:run(Colours, Re) of
        {match, _} -> [$# | Colours];
        nomatch    -> ?ERR_VAL
    end.

std_bools(Vals) ->
    Rules = [first_array, fetch_name, fetch_ref, eval_funs, {cast, bool}],
    Passes = [return_errors, {all, fun is_atom/1}],
    muin_collect:col(Vals, Rules, Passes).

std_strs(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).

std_nums(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, num}],
    Passes = [return_errors, {all, fun is_number/1}],
    muin_collect:col(Vals, Rules, Passes).

std_ints(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, int}],
    Passes = [return_errors, {all, fun is_integer/1}],
    muin_collect:col(Vals, Rules, Passes).

std_pos_ints(Vals) ->
    Ret = std_ints(Vals),
    case is_positive(Ret) of
        true  -> Ret;
        false -> ?ERR_VAL
    end.

flat_strs(Vals) ->
    Rules = [eval_funs, fetch, flatten, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).

html_box_contents(Vals) ->
    Rules = [eval_funs, fetch, flatten, strip_spec, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).

std_inc_strs(Vals) ->
    Rules = [eval_funs, fetch_incs, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).

is_positive([])                  -> true;
is_positive([H | _T]) when H < 0 -> false;
is_positive([_H | T])            -> is_positive(T).

in_range(Num, Low, High) ->
        if
        Num  < Low                     -> ?ERR_VAL;
        Num  > High                    -> ?ERR_VAL;
        Num >= Low andalso Num =< High -> ok
    end.

is_member(Val, List) ->
    case lists:member(Val, List) of
        true  -> ok;
        false -> ?ERR_VAL
    end.

compress(List) ->
    L2 = re:replace(List, " |-|(|)", "", [{return, list}, global]),
    Num2 = case L2 of
        "00" ++ Rest1 -> Rest1;
        "0" ++ Rest2  -> Rest2;
        Other         -> Other
    end,
    case Num2 of
        "" -> Num2;
        _  -> case is_str_integer(Num2) of
                  true  -> Num2;
                  false -> ?ERR_VAL
              end
    end.

is_str_integer(Num) ->
    case tconv:to_num(Num) of
        {error, nan} ->
            false;
        Num2 ->
            Num3 = integer_to_list(tconv:to_i(Num2)),
            case Num3 of
                Num -> true;
                _   -> false
            end
    end.

normalise("+"++Prefix)  -> Prefix;
normalise("00"++Prefix) -> Prefix;
normalise(Prefix) ->
    P2 = case tconv:to_num(Prefix) of
             {error, nan} -> twilio_web_util:country_code_to_prefix(Prefix);
             Num          -> Num
         end,
    integer_to_list(P2).
