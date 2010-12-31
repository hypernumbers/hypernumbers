%%% @author    Gordon Guthrie <>
%%% @copyright (C) 2010, Gordon Guthrie
%%% @doc       A set of standard type checks
%%%
%%% @end
%%% Created : 31 Dec 2010 by Gordon Guthrie <>

-module(typechecks).

-export([
         std_strs/1
        ]).

std_strs(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).
