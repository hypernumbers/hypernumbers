%%% @author    Gordon Guthrie <>
%%% @copyright (C) 2010, Gordon Guthrie
%%% @doc       A set of standard type checks
%%%
%%% @end
%%% Created : 31 Dec 2010 by Gordon Guthrie <>

-module(typechecks).

-export([
         % throws on error
         throw_rgbcolours/1,
         throw_std_bools/1,
         throw_std_strs/1,
         throw_std_ints/1,
         throw_std_nums/1,
         throw_flat_strs/1,
         throw_html_box_contents/1,
         % returns errors to fn
         rgbcolours/1,
         std_bools/1,
         std_strs/1,
         std_ints/1,
         std_nums/1,
         flat_strs/1,
         html_box_contents/1
        ]).

-include("typechecks.hrl").

throw_rgbcolours(X)         -> errthrow(rgbcolours(X)).
throw_std_bools(X)          -> errthrow(std_bools(X)).
throw_std_strs(X)           -> errthrow(std_strs(X)).
throw_std_ints(X)           -> errthrow(std_ints(X)).
throw_std_nums(X)           -> errthrow(std_nums(X)).
throw_flat_strs(X)          -> errthrow(flat_strs(X)).
throw_html_box_contents(X)  -> errthrow(html_box_contents(X)).

errthrow(X) -> th(X, []).

th([], Acc)                           -> lists:reverse(Acc);
th([H | _T], _Acc) when ?is_errval(H) -> throw(H);
th([H | T], Acc)                      -> th(T, [H | Acc]);
% might not be a list returned if its an error...
th(X, []) when ?is_errval(X)          -> throw(X).

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
    Rules = [first_array, fetch_name, fetch_ref, eval_funs, {cast,bool}],
    Passes = [return_errors, {all, fun is_atom/1}],
    muin_collect:col(Vals, Rules, Passes).

std_strs(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).

std_nums(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, num}],
    Passes = [return_errors],
    muin_collect:col(Vals, Rules, Passes).

std_ints(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, int}],
    Passes = [return_errors],
    muin_collect:col(Vals, Rules, Passes).

flat_strs(Vals) ->
    Rules = [eval_funs, fetch, flatten, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).

html_box_contents(Vals) ->
    Rules = [eval_funs, fetch, flatten, strip_resize, strip_preview, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).

