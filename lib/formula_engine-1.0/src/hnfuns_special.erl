%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       This module implements special
%%%            hypernumbers functions that are
%%%            sui generis
%%% @end
%%% Created :  2 May 2011 by gordon@hypernumbers.com

-module(hnfuns_special).

-export([
         timestamp/1,
         tick/1,
         snapshot/1
         ]).

-include("spriki.hrl").
-include("errvals.hrl").

snapshot([Arg]) ->
    Rules = [first_array, fetch_name, fetch_ref, eval_funs],
    Passes = [],
    [Ret] = muin_collect:col([Arg], Rules, Passes),
    {Errors, _References} = get(retvals),
    % snapshot works by killng the parents so the fn never relcalcs
    put(retvals, {Errors, []}),
    put(infinite, []),
    Ret.

timestamp(_List) -> stdfuns_date:now([]).

tick([]) -> tick([1]);
tick(Options) ->
    Opts = typechecks:std_ints(Options),
    Spec = case Opts of
               [0]    -> hourly;
               [1]    -> daily;
               [2]    -> {weekly, 1}; % monday
               [2, 1] -> {weekly, 1}; % monday
               [2, 2] -> {weekly, 2}; % tuesday
               [2, 3] -> {weekly, 3}; % wednesday
               [2, 4] -> {weekly, 4}; % thursday
               [2, 5] -> {weekly, 5}; % friday
               [2, 6] -> {weekly, 6}; % saturday
               [2, 7] -> {weekly, 7}; % sunday
               [3]    -> {monthly, 1};
               [3, N] when N > 0 andalso N < 31 -> {monthly, N};
               _      -> ?ERR_VAL
           end,
    #spec_val{val = stdfuns_date:now([]), sp_timer = #sp_timer{spec = Spec}}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

