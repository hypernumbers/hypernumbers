%%%-----------------------------------------------------------------------------
%%% File        : spriki_funs_text_UNIT_TEST.erl
%%% Author      : Hasan Veldstra <hasan.veldstra@gmail.com>
%%% Description : Unit tests for Spriki's implementation of Excel's text
%%%               functions.
%%%
%%% Created     : 22 Nov 2007 by Hasan Veldstra <hasan.veldstra@gmail.com>
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% IMPORTANT NOTES:
%%% 
%%% * The suite covers all text functions in Excel 97. There are no tests for 
%%%   Excel 2003 functions.
%%%
%%% * Input to functions is ASCII 32-126 only for now.
%%%
%%% INCOMPLETE:
%%% * pound()
%%% * value() : this can take an argument in any of the formats for numbers, 
%%%   dates, or times that are recognized for Excel.
%%% * replace() : not sure how it works exactly, so some edge cases are probably
%%%   not covered.
%%% * substitute() : as above.
%%%-----------------------------------------------------------------------------

-module(spriki_funs_text_UNIT_TEST).
-include_lib("eunit/include/eunit.hrl").
-include("useful_constants.hrl").
-import(util2, [relative_error/2]).

char_test_() ->
  [
    ?_assert("A" == spriki_funs:char(65)),
    ?_assert("!" == spriki_funs:char(33)),
    ?_assert("\n" == spriki_funs:char(10))
  ].

clean_test_() ->
  [
    ?_assert([104, 101, 108, 108, 111] == spriki_funs:clean([104, 101, 108, 108, 111])),
    ?_assert([104, 101, 108, 108, 111] == spriki_funs:clean([104, 101, 108, 1, 108, 111])),
    ?_assert([104, 101, 108, 108, 111] == spriki_funs:clean([31, 104, 101, 108, 108, 29, 111])),
    ?_assert([] == spriki_funs:clean([7, 7])),
    ?_assert([] == spriki_funs:clean([]))
  ].

code_test_() ->
  [
    ?_assert(65 == spriki_funs:code("A")),
    ?_assert(33 == spriki_funs:code("!")),
    ?_assert(10 == spriki_funs:code("\n"))
  ].

concatenate_test_() ->
  [
    ?_assert("brook trout species population is 32/mile" == spriki_funs:concatenate(["brook trout ", "species ", "population is ", 32, "/mile"])),
    ?_assert("erlang" == spriki_funs:concatenate(["erlang"]))
  ].

exact_test_() ->
  [
    ?_assert(true == spriki_funs:exact("word", "word")),
    ?_assert(false == spriki_funs:exact("Word", "word")),
    ?_assert(false == spriki_funs:exact("word", "w ord")),
    ?_assert(false == spriki_funs:exact("hello world", "hello World")),
    ?_assert(true == spriki_funs:exact(spriki_funs:char(7) == spriki_funs:char(7))),
    ?_assert(false == spriki_funs:exact(spriki_funs:char(7) ++ " hello", "hello"))
  ].

find_test_() ->
  [
    ?_assert(1 == spriki_funs:find("M", "Miriam McGovern")),
    ?_assert(6 == spriki_funs:find("m", "Miriam McGovern")),
    ?_assert(6 == spriki_funs:find("m", "Miriam McGovern", 3))
  ].

fixed_test_() ->
  [
    ?_assert("1,234.6" == spriki_funs:fixed(1234.567, 1)),
    ?_assert("1,230" == spriki_funs:fixed(1234.567, -1)),
    ?_assert("-1230" == spriki_funs:fixed(-1234.567, -1, true)),
    ?_assert("44.33" == spriki_funs:fixed(44.332))
  ].

left_test_() ->
  [
    ?_assert("Sale" == spriki_funs:left("Sale Price", 4)),
    ?_assert("S" == spriki_funs:left("Sweden")),
    ?_assert("Sweden" == spriki_funs:left("Sweden", 20)),
    ?_assert("" == spriki_funs:left("Sweden", 0))
  ].

len_test_() ->
  [
    ?_assert(0 == spriki_funs:len("")),
    ?_assert(17 == spriki_funs:len("Stockholm, Sweden")),
    ?_assert(1 == spriki_funs:len(spriki_funs:char(7)))
  ].

lower_test_() ->
  [
    ?_assert("hello world" == spriki_funs:lower("Hello World")),
    ?_assert("apt. 2b" == spriki_funs:lower("Apt. 2B")),
    ?_assert(".::l337::." == spriki_funs:lower(".::L337::."))
  ].

mid_test_() ->
  [
    ?_assert("stock" == spriki_funs:mid("stockholm", 1, 5)),
    ?_assert("tockholm" == spriki_funs:mid("stockholm", 2, 50)),
    ?_assert("" == spriki_funs:mid("stockholm", 50, 2))
  ].

proper_test_() ->
  [
    ?_assert("Erlang" == spriki_funs:proper("ERLANG")),
    ?_assert("Erlang" == spriki_funs:proper("eRlAnG")),
    ?_assert("Mr. Jock, Tv Quiz Ph.D., Bags Few Lynx." == spriki_funs:proper("Mr. Jock, TV quiz Ph.D., bags few lynx.")),
    ?_assert("How Razorback-Jumping Frogs Can Level Six Piqued Gymnasts!" == spriki_funs:proper("How razorback-jumping frogs can level six piqued gymnasts!")),
    ?_assert("May Jo Equal The Fine Record By Solving Six Puzzles A Week Each 2-Cent'S Worth?" == spriki_funs:proper("May Jo equal the fine record by solving six puzzles a week each 2-cent's worth?")),
    ?_assert("76Budget" == spriki_funs:proper("76BudGet"))
  ].

replace_test_() ->
  [
    ?_assert("worldello" == spriki_funs:replace("hello", 1, 1, "world")),
    ?_assert("hellworld" == spriki_funs:replace("hello", 5, 5, "world")),
    ?_assert("wbeerld" == spriki_funs:replace("world", 2, 1, "beer"))
  ].

rept_test_() ->
  [
    ?_assert("" == spriki_funs:rept("-", 0)),
    ?_assert("--" == spriki_funs:rept("-", 2)),
    ?_assert("-*--*--*--*--*-" == spriki_funs:rept("-*-", 5))
  ].

right_test_() ->
  [
    ?_assert("Price" == spriki_funs:right("Sale Price", 5)),
    ?_assert("ber" == spriki_funs:right("Stock Number", 3)),
    ?_assert("Sweden" == spriki_funs:right("Sweden", 10)),
    ?_assert("n" == spriki_funs:right("Sweden")),
    ?_assert("" == spriki_funs:right("Sweden", 0))
  ].

search_test_() ->
  [
    ?_assert(1 == spriki_funs:search("Stockholm, Sweden", "Stockholm, Sweden")),
    ?_assert(12 == spriki_funs:search("sweden", "Stockholm, Sweden")),
    ?_assert(1 == spriki_funs:search("*", "Stockholm, Sweden")),
    ?_assert(3 == spriki_funs:search("o?", "Stockholm, Sweden")),
    ?_assert(7 == spriki_funs:search("o?", "Stockholm, Sweden", 5)),
    ?_assert(12 == spriki_funs:search("s*", "Stockholm, Sweden", 2)),
    ?_assert(11 == spriki_funs:search("~?", "Stockholm ? Sweden")),
    ?_assert(11 == spriki_funs:search("~*", "Stockholm * Sweden"))
  ].

substitute_test_() ->
  [
    ?_assert("Cost Data" == spriki_funs:substitute("Sales Data", "Sales", "Cost")),
    ?_assert("Quarter 2, 2008" == spriki_funs:substitute("Quarter 1, 2008", "1", "2", 1)),
    ?_assert("Quarter 1, 2012" == spriki_funs:substitute("Quarter 1, 2011", "1", "2", 3))
  ].

trim_test_() ->
  [
    ?_assert("" == spriki_funs:trim(" ")),
    ?_assert("hello" == spriki_funs:trim(" hello ")),
    ?_assert("hello there7" == spriki_funs:trim(" hello there7 ")),
    ?_assert("hello there" == spriki_funs:trim("hello there")),
    ?_assert("hello there " ++ [7] == spriki_funs:trim("hello there " ++ spriki_funs:char(7)))
  ].

upper_test_() ->
  [
    ?_assert("HELLO WORLD" == spriki_funs:upper("Hello World")),
    ?_assert("APT. 2B" == spriki_funs:upper("Apt. 2B")),
    ?_assert(".::L337::." == spriki_funs:upper(".::l337::."))
  ].

value_test_() ->
  [
    ?_assert(1 == spriki_funs:value("1")),
    ?_assert(0.00001 == spriki_funs:value("0.00001"))
  ].