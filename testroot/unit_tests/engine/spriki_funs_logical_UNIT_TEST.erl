%%%----------------------------------------------------------------------------
%%% File        : excel_logical_funs_test_SUITE.erl
%%% Author      : Hasan Veldstra <hasan.veldstra@gmail.com>
%%% Description : Unit tests for Spriki's implementation of Excel's logical
%%%               functions.
%%%
%%% Created     : 22 Nov 2007 by Hasan Veldstra <hasan.veldstra@gmail.com>
%%%----------------------------------------------------------------------------

-module(excel_logical_funs_UNIT_TEST).
-include_lib("eunit/include/eunit.hrl").
-include("useful_constants.hrl").
-import(util2, [relative_error/2]).

andf_test_() ->
  [
    ?_assert(spriki_funs:andf([true])),
    ?_assert(spriki_funs:andf([false]) == false),
    ?_assert(spriki_funs:andf([true, false, true]) == false)
  ].

false_test_() ->
  [
    ?_assert(spriki_funs:false() == false)
  ].

iff_test_() ->
  [
    ?_assert(spriki_funs:iff(1 == 0 + 1, "yes", "no") == "yes"),
    ?_assert(spriki_funs:iff(false, "true", "false") == "false"),
    ?_assert(spriki_funs:iff(true, [1], [0]) == [1])
  ].

notf_test_() ->
  [
    ?_assert(spriki_funs:notf(true) == false),
    ?_assert(spriki_funs:notf(false))
  ].

orf_test_() ->
  [
    ?_assert(spriki_funs:orf([true, true])),
    ?_assert(spriki_funs:orf([true, false])),
    ?_assert(spriki_funs:orf([false, true])),
    ?_assert(spriki_funs:orf([false, false]) == false)
  ].

true_test_() ->
  [
    ?_assert(spriki_funs:true())
  ].