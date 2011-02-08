%%% @author    Gordon Guthrie <>
%%% @copyright (C) 2010, Gordon Guthrie
%%% @doc       A set of standard type checks
%%%
%%% @end
%%% Created : 31 Dec 2010 by Gordon Guthrie <>

-module(typechecks).

-export([
         std_bools/1,
         std_strs/1,
         std_ints/1,
         std_nums/1,
         flat_strs/1,
         html_box_contents/1
        ]).

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

