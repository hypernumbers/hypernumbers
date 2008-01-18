%%%----------------------------------------------------------------------------
%%% File        : spriki_funs_information_UNIT_TEST.erl
%%% Author      : Hasan Veldstra <hasan.veldstra@gmail.com>
%%% Description : Unit tests for Spriki's implementation of Excel's information
%%%               functions.
%%%
%%% Created     : 22 Nov 2007 by Hasan Veldstra <hasan.veldstra@gmail.com>
%%%----------------------------------------------------------------------------

%%%------------------------------------------------------------------------------
%%% IMPORTANT NOTES:
%%% 
%%% * The suite covers all text functions in Excel 97. There are no tests for 
%%%   Excel 2003 functions.
%%%
%%% INCOMPLETE:
%%%
%%% * cell : not implemented.
%%% * info : not implemented.
%%% * isref : not implemented.
%%% * n : test with dates.
%%%-------------------------------------------------------------------------------


-module(spriki_funs_information_UNIT_TEST).
-include_lib("eunit/include/eunit.hrl").
-include("useful_constants.hrl").
-import(util2, [relative_error/2]).

error_type_test_() ->
  [
    ?_assert(1 == spriki_funs:error_type({error, null})),
    ?_assert(2 == spriki_funs:error_type({error, div_by_zero})),
    ?_assert(3 == spriki_funs:error_type({error, value})),
    ?_assert(4 == spriki_funs:error_type({error, reference})),
    ?_assert(5 == spriki_funs:error_type({error, name})),
    ?_assert(6 == spriki_funs:error_type({error, number})),
    ?_assert(7 == spriki_funs:error_type({error, na})),
    ?_assert({error, na} == spriki_funs:error_type(any_other_input))
  ].

iserr_test_() ->
  [
    ?_assert(spriki_funs:iserr({error, null})),
    ?_assert(spriki_funs:iserr({error, div_by_zero})),
    ?_assert(spriki_funs:iserr({error, value})),
    ?_assert(spriki_funs:iserr({error, reference})),
    ?_assert(spriki_funs:iserr({error, number})),
    ?_assert(spriki_funs:iserr({error, name})),
    
    ?_assert(spriki_funs:iserr({error, na}) == false),
    ?_assert(spriki_funs:iserr("list") == false),
    ?_assert(spriki_funs:iserr(atom) == false),
    ?_assert(spriki_funs:iserr(10) == false)
  ].

iserror_test_() ->
  [
    ?_assert(spriki_funs:iserror({error, null})),
    ?_assert(spriki_funs:iserror({error, div_by_zero})),
    ?_assert(spriki_funs:iserror({error, value})),
    ?_assert(spriki_funs:iserror({error, reference})),
    ?_assert(spriki_funs:iserror({error, number})),
    ?_assert(spriki_funs:iserror({error, na})),
    
    ?_assert(spriki_funs:iserror({a, tuple}) == false),
    ?_assert(spriki_funs:iserror("list") == false),
    ?_assert(spriki_funs:iserror(atom) == false),
    ?_assert(spriki_funs:iserror(10) == false)    
  ].

islogical_test_() ->
  [
    ?_assert(true == spriki_funs:islogical(true)),
    ?_assert(true == spriki_funs:islogical(false)),
    ?_assert(false == spriki_funs:islogical("true")),
    ?_assert(false == spriki_funs:islogical(1))
  ].

isna_test_() ->
  [
    ?_assert(true == spriki_funs:isna({error, na})),
    ?_assert(false == spriki_funs:isna({error, name}))
  ].

isnontext_test_() ->
  [
    ?_assert(false == spriki_funs:isnontext("Stockholm, Sweden")),
    ?_assert(false == spriki_funs:isnontext("Apt. 2B")),
    ?_assert(true == spriki_funs:isnontext([]))
  ].

isnumber_test_() ->
  [
    ?_assert(true == spriki_funs:isnumber(?TINY_FLOAT)),
    ?_assert(true == spriki_funs:isnumber(?HUGE_INT)),
    ?_assert(false == spriki_funs:isnumber("1"))
  ].

istext_test_() ->
  [
    ?_assert(true == spriki_funs:istext("hello world")),
    ?_assert(true == spriki_funs:istext(["hello", "world"])),
    ?_assert(true == spriki_funs:istext(["hello world", 1])),
    ?_assert(false == spriki_funs:istext([]))
  ].

n_test_() ->
  [
    ?_assert(0.1 == spriki_funs:n(0.1)),
    ?_assert(1 == spriki_funs:n(true)),
    ?_assert(0 == spriki_funs:n(false)),
    ?_assert({error, div_by_zero} == spriki_funs:n({error, div_by_zero})),
    ?_assert(0 == spriki_funs:n("text"))
  ].

na_test_() ->
  [
    ?_assert({error, na} == spriki_funs:na())
  ].

type_test_() ->
  [
    ?_assert(1 == spriki_funs:type(?TINY_FLOAT)),
    ?_assert(2 == spriki_funs:type("some text\n\n")),
    ?_assert(4 == spriki_funs:type(true)),
    ?_assert(16 == spriki_funs:type({error, value})),
    ?_assert(64 == spriki_funs:type([]))
  ].