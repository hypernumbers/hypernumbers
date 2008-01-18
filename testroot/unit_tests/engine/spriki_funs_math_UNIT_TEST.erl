%%%----------------------------------------------------------------------------
%%% File        : spriki_funs_math_UNIT_TEST.erl
%%% Author      : Hasan Veldstra <hasan.veldstra@gmail.com>
%%% Description : Unit tests for Spriki's implementation of Excel's math and 
%%%               trigonometric functions.
%%%
%%% Created     : 2 Nov 2007 by Hasan Veldstra <hasan.veldstra@gmail.com>
%%                (Converted from old unit tests)
%%%----------------------------------------------------------------------------

%%%------------------------------------------------------------------------------
%%% IMPORTANT NOTES:
%%%
%%% * Right now, the functions are fed only 100% valid input. (Need to sort out
%%%   where and how the errors are handled.)
%%% * Floats are tested to precision of 4dp.
%%% * plus() is tested only with numbers in this suite.
%%% * RAND is not tested.
%%% * There are no tests for Excel 2003 functions.
%%%
%%% * INCOMPLETE:
%%%   * SUM: This functions ignores logical values and text representations of numbers unless they are
%%%     typed directly into the formula bar (i.e. only numbers are counted in arrays and references).
%%%     Right now, there is no way of distinguishing between such values.
%%%
%%%   * PRODUCT: Same as SUM.
%%%
%%%   * SUBTOTAL: only tested with 3 functions (there are 11 of them), and not tested with multiple
%%%     lists.
%%%
%%%------------------------------------------------------------------------------

-module(spriki_funs_math_UNIT_TEST).
-include_lib("eunit/include/eunit.hrl").
-include("useful_constants.hrl").
-import(util2, [relative_error/2]).

abs_test_() ->
  [
    ?_assert(1 == spriki_funs:abs(-1)),
    ?_assert(1 == spriki_funs:abs(1)),
    ?_assert(0 == spriki_funs:abs(0))
  ].

acos_test_() ->
  [
    ?_assert(relative_error(3.14159, spriki_funs:acos(-1)) =< 0.0001),
    ?_assert(relative_error(1.57079, spriki_funs:acos(0)) =< 0.0001),
    ?_assert(relative_error(1.77215, spriki_funs:acos(-0.2)) =< 0.0001)
  ].

acosh_test_() ->
  [
    ?_assert(relative_error(0.44356, spriki_funs:acosh(1.1)) =< 0.0001),
    ?_assert(relative_error(23.71899, spriki_funs:acosh(?HUGE_INT)) =< 0.0001)
  ].

asin_test_() ->
  [
    ?_assert(relative_error(-1.57079, spriki_funs:asin(-1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:asin(0)) =< 0.0001),
    ?_assert(relative_error(1.57079, spriki_funs:asin(1)) =< 0.0001),
    ?_assert(relative_error(-0.20135, spriki_funs:asin(-0.2)) =< 0.0001)
  ].

asinh_test_() ->
  [
    ?_assert(relative_error(-0.88137, spriki_funs:asinh(-1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:asinh(0)) =< 0.0001),
    ?_assert(relative_error(0.88137, spriki_funs:asinh(1)) =< 0.0001),
    ?_assert(relative_error(-3.10301, spriki_funs:asinh(-11.11)) =< 0.0001)
  ].

atan2_test_() ->
  [
    ?_assert(relative_error(0.78539, spriki_funs:atan2(1, 1)) =< 0.0001),
    ?_assert(relative_error(3.12359, spriki_funs:atan2(-11.11, 0.2)) =< 0.0001),
    ?_assert(relative_error(1.57079, spriki_funs:atan2(0, 111.1)) =< 0.0001),
    ?_assert(relative_error(2.67794, spriki_funs:atan2(-2, 1)) =< 0.0001)
  ].

atan_test_() ->
  [
    ?_assert(relative_error(-0.78539, spriki_funs:atan(-1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:atan(0)) =< 0.0001),
    ?_assert(relative_error(0.78539, spriki_funs:atan(1)) =< 0.0001),
    ?_assert(relative_error(-1.48102, spriki_funs:atan(-11.11)) =< 0.0001)
  ].

atanh_test_() ->
  [
    ?_assert(relative_error(-6.10303, spriki_funs:atanh(-0.99999)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:atanh(0)) =< 0.0001),
    ?_assert(relative_error(6.10303, spriki_funs:atanh(0.99999)) =< 0.0001),
    ?_assert(relative_error(-0.20273, spriki_funs:atanh(-0.2)) =< 0.0001)
  ].

ceiling_test_() ->
  [
    ?_assert(relative_error(3, spriki_funs:ceiling(2.5, 1)) =< 0.0001),
    ?_assert(relative_error(-4, spriki_funs:ceiling(-2.5, -2)) =< 0.0001),
    ?_assert(relative_error(1.5, spriki_funs:ceiling(1.5, 0.1)) =< 0.0001),
    ?_assert(relative_error(0.24, spriki_funs:ceiling(0.234, 0.01)) =< 0.0001),
    ?_assert(relative_error(4, spriki_funs:ceiling(4, 2)) =< 0.0001),
    ?_assert(relative_error(4, spriki_funs:ceiling(2, 4)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:ceiling(1, 0)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:ceiling(0, 0)) =< 0.0001)
  ].

combin_test_() ->
  [
    ?_assert(relative_error(28, spriki_funs:combin(8, 2)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:combin(0, 0)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:combin(1, 0)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:combin(100, 0)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:combin(100, 100)) =< 0.0001),
    ?_assert(relative_error(100, spriki_funs:combin(100, 1)) =< 0.0001),
    ?_assert(relative_error(4280561376, spriki_funs:combin(42, 11)) =< 0.0001)
  ].

cos_test_() ->
  [
    ?_assert(relative_error(0.5403, spriki_funs:cos(-1)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:cos(0)) =< 0.0001),
    ?_assert(relative_error(0.5403, spriki_funs:cos(1)) =< 0.0001),
    ?_assert(relative_error(0.11417, spriki_funs:cos(-11.11)) =< 0.0001)
  ].

cosh_test_() ->
  [
    ?_assert(relative_error(1.54308, spriki_funs:cosh(-1)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:cosh(0)) =< 0.0001),
    ?_assert(relative_error(1.54308, spriki_funs:cosh(1)) =< 0.0001),
    ?_assert(relative_error(33418.0957, spriki_funs:cosh(-11.11)) =< 0.0001)
  ].

divide_test_() ->
  [
    ?_assert(relative_error(-1, spriki_funs:divide(?HUGE_NEGATIVE_INT, ?HUGE_INT)) =< 0.0001),
    ?_assert(relative_error(?TINY_FLOAT, spriki_funs:divide(?TINY_FLOAT, 1)) =< 0.0001)
  ].

even_test_() ->
  [
    ?_assert(relative_error(2, spriki_funs:even(1.5)) =< 0.0001),
    ?_assert(relative_error(4, spriki_funs:even(3)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:even(0)) =< 0.0001),
    ?_assert(relative_error(2, spriki_funs:even(2)) =< 0.0001),
    ?_assert(relative_error(-2, spriki_funs:even(-1.5)) =< 0.0001),
    ?_assert(relative_error(-2, spriki_funs:even(-?TINY_FLOAT)) =< 0.0001)
  ].

exp_test_() ->
  [
    ?_assert(relative_error(1, spriki_funs:exp(0)) =< 0.0001),
    ?_assert(relative_error(2.71828, spriki_funs:exp(1)) =< 0.0001),
    ?_assert(relative_error(12.18249, spriki_funs:exp(2.5)) =< 0.0001),
    ?_assert(relative_error(0.36787, spriki_funs:exp(-1)) =< 0.0001),
    ?_assert(relative_error(0.22313, spriki_funs:exp(-1.5)) =< 0.0001)
  ].

fact_test_() ->
  [
    ?_assert(relative_error(120, spriki_funs:fact(5)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:fact(0)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:fact(1)) =< 0.0001)
  ].

floor_test_() ->
  [
    ?_assert(relative_error(2, spriki_funs:floor(2.5, 1)) =< 0.0001),
    ?_assert(relative_error(-2, spriki_funs:floor(-2.5, -2)) =< 0.0001),
    ?_assert(relative_error(1.5, spriki_funs:floor(1.5, 0.1)) =< 0.0001),
    ?_assert(relative_error(0.23, spriki_funs:floor(0.234, 0.01)) =< 0.0001),
    ?_assert(relative_error(4, spriki_funs:floor(4, 2)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:floor(2, 4)) =< 0.0001),
    ?_assert(relative_error(-1, spriki_funs:floor(-1, -0.1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:floor(0, 0.1)) =< 0.0001)
  ].

int_test_() ->
  [
    ?_assert(relative_error(8, spriki_funs:int(8.9)) =< 0.0001),
    ?_assert(relative_error(-9, spriki_funs:int(-8.9)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:int(?TINY_FLOAT)) =< 0.0001)
  ].

ln_test_() ->
  [
    ?_assert(relative_error(-2.30258, spriki_funs:ln(0.1)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:ln(2.7182818)) =< 0.0001),
    ?_assert(relative_error(23.02585, spriki_funs:ln(?HUGE_INT)) =< 0.0001)
  ].

log10_test_() ->
  [
    ?_assert(relative_error(-5, spriki_funs:log10(?TINY_FLOAT)) =< 0.0001),
    ?_assert(relative_error(10, spriki_funs:log10(?HUGE_INT)) =< 0.0001),
    ?_assert(relative_error(1.04571, spriki_funs:log(11.11)) =< 0.0001)
  ].

log_test_() ->
  [
    ?_assert(relative_error(-16.60964, spriki_funs:log(?TINY_FLOAT, 2)) =< 0.0001),
    ?_assert(relative_error(9.56284, spriki_funs:log(?HUGE_INT, 11.11)) =< 0.0001),
    ?_assert(relative_error(2.40784, spriki_funs:log(11.11, 2.71828)) =< 0.0001),
    ?_assert(relative_error(-5, spriki_funs:log(?TINY_FLOAT)) =< 0.0001)
  ].

mdeterm_test_() ->
  [
    ?_assert(relative_error(88, spriki_funs:mdeterm([1, 3, 8, 5, 1, 3, 6, 1, 1, 1, 1, 0, 7, 3, 10, 2])) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:mdeterm([3, 6, 1, 1, 1, 0, 3, 10, 2])) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:mdeterm([0])) =< 0.0001)
  ].

minus_test_() ->
  [
    ?_assert(relative_error(?HUGE_INT + ?HUGE_INT, spriki_funs:minus(?HUGE_INT, ?HUGE_NEGATIVE_INT)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:minus(?TINY_FLOAT, ?TINY_FLOAT)) =< 0.0001),
    ?_assert(relative_error(?HUGE_INT - ?TINY_FLOAT, spriki_funs:minus(?HUGE_INT, ?TINY_FLOAT)) =< 0.0001)
  ].

minverse_test_() ->
  [
    ?_assert(matrix:are_equal(matrix:list_to_matrix([0, 0.5, -1, 2], 2), spriki_funs:minverse([4, -1, 2, 0], 2))),
    ?_assert(matrix:are_equal(matrix:list_to_matrix([1], 1), spriki_funs:minverse([1], 1))),
    ?_assert(matrix:are_equal(matrix:list_to_matrix([0.94786, 0.52132, 0.04739, -0.47393], 2), spriki_funs:minverse([1, 1.1, 0.1, -2], 2)))
  ].

mmult_test_() ->
  [
    ?_assert(matrix:are_equal(matrix:list_to_matrix([1], 1), spriki_funs:mmult([1], 1, [1], 1))),
    ?_assert(matrix:are_equal(matrix:list_to_matrix([300002.2, 24.34003, 230.33, 1111000, 0.00011, 854.4701, 26100, 12170, -180.7725], 3), spriki_funs:mmult([1, 2, 3, -1, 0, 11.11, -0.00001, 1000, 0.25], 3, [0, 0, 0, 1.1, 12.17, -0.2, 100000, 0.00001, 76.91], 3))),
    ?_assert(matrix:are_equal(matrix:list_to_matrix([-0.213, 1.111, -21.3, 111.1], 2), spriki_funs:mmult([0.1, 10], 1, [-2.13, 11.11], 2)))
  ].

mod_test_() ->
  [
    ?_assert(relative_error(1, spriki_funs:mod(3, 2)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:mod(-3, 2)) =< 0.0001),
    ?_assert(relative_error(-1, spriki_funs:mod(3, -2)) =< 0.0001),
    ?_assert(relative_error(-1, spriki_funs:mod(-3, -2)) =< 0.0001)
  ].

odd_test_() ->
  [
    ?_assert(relative_error(3, spriki_funs:odd(1.5)) =< 0.0001),
    ?_assert(relative_error(3, spriki_funs:odd(3)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:odd(0)) =< 0.0001),
    ?_assert(relative_error(3, spriki_funs:odd(2)) =< 0.0001),
    ?_assert(relative_error(-3, spriki_funs:odd(-1.5)) =< 0.0001),
    ?_assert(relative_error(-1, spriki_funs:odd(-?TINY_FLOAT)) =< 0.0001)
  ].

pi_test_() ->
  [
    ?_assert(relative_error(3.14159, spriki_funs:pi()) =< 0.0001)
  ].

plus_test_() ->
  [
    ?_assert(relative_error(0, spriki_funs:plus(?HUGE_INT, ?HUGE_NEGATIVE_INT)) =< 0.0001),
    ?_assert(relative_error(?HUGE_INT + ?TINY_FLOAT, spriki_funs:plus(?HUGE_INT, ?TINY_FLOAT)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:plus(?TINY_FLOAT, -?TINY_FLOAT)) =< 0.0001),
    ?_assert(relative_error(?TINY_FLOAT + ?TINY_FLOAT, spriki_funs:plus(?TINY_FLOAT, ?TINY_FLOAT)) =< 0.0001)
  ].

power_test_() ->
  [
    ?_assert(relative_error(25, spriki_funs:power(5, 2)) =< 0.0001),
    ?_assert(relative_error(2401077, spriki_funs:power(98.6, 3.2)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:power(?HUGE_INT, 0)) =< 0.0001),
    ?_assert(relative_error(0.00001, spriki_funs:power(100000, -1)) =< 0.0001)
  ].

product_test_() ->
  [
    ?_assert(relative_error(0, spriki_funs:product([10, 20, 0])) =< 0.0001),
    ?_assert(relative_error(1111000, spriki_funs:product([?HUGE_INT, ?TINY_FLOAT, 11.11])) =< 0.0001)
  ].

radians_test_() ->
  [
    ?_assert(relative_error(4.7123, spriki_funs:radians(270)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:radians(0)) =< 0.0001),
    ?_assert(relative_error(6.28318, spriki_funs:radians(360)) =< 0.0001),
    ?_assert(relative_error(-6.28318, spriki_funs:radians(-360)) =< 0.0001),
    ?_assert(relative_error(-4.71238, spriki_funs:radians(-270)) =< 0.0001)
  ].

round_test_() ->
  [
    ?_assert(relative_error(2.2, spriki_funs:round(2.15, 1)) =< 0.0001),
    ?_assert(relative_error(2.1, spriki_funs:round(2.149, 1)) =< 0.0001),
    ?_assert(relative_error(100, spriki_funs:round(140.75, -2)) =< 0.0001),
    ?_assert(relative_error(20, spriki_funs:round(21.5, -1)) =< 0.0001),
    ?_assert(relative_error(22, spriki_funs:round(21.5, 0)) =< 0.0001)
  ].

rounddown_test_() ->
  [
    ?_assert(relative_error(3, spriki_funs:rounddown(3.2, 0)) =< 0.0001),
    ?_assert(relative_error(76.1, spriki_funs:rounddown(76.19, 1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:rounddown(3.14159, -2)) =< 0.0001),
    ?_assert(relative_error(-3.1, spriki_funs:rounddown(-3.14159, 1)) =< 0.0001),
    ?_assert(relative_error(31000, spriki_funs:rounddown(31415.92654, -3)) =< 0.0001),
    ?_assert(relative_error(-12300, spriki_funs:rounddown(-12345.99, -2)) =< 0.0001)
  ].

roundup_test_() ->
  [
    ?_assert(relative_error(4, spriki_funs:roundup(3.2, 0)) =< 0.0001),
    ?_assert(relative_error(76.2, spriki_funs:roundup(76.19, 1)) =< 0.0001),
    ?_assert(relative_error(100, spriki_funs:roundup(3.14159, -2)) =< 0.0001),
    ?_assert(relative_error(-3.2, spriki_funs:roundup(-3.14159, 1)) =< 0.0001),
    ?_assert(relative_error(32000, spriki_funs:roundup(31415.92654, -3)) =< 0.0001),
    ?_assert(relative_error(-12400, spriki_funs:roundup(-12345.99, -2)) =< 0.0001)
  ].

sign_test_() ->
  [
    ?_assert(relative_error(0, spriki_funs:sign(0)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:sign(1)) =< 0.0001),
    ?_assert(relative_error(-1, spriki_funs:sign(-1)) =< 0.0001),
    ?_assert(relative_error(-1, spriki_funs:sign(-10 - ?TINY_FLOAT)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:sign(10 + ?TINY_FLOAT)) =< 0.0001),
    ?_assert(relative_error(-1, spriki_funs:sign(-?TINY_FLOAT)) =< 0.0001)
  ].

sin_test_() ->
  [
    ?_assert(relative_error(-0.84147, spriki_funs:sin(-1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:sin(0)) =< 0.0001),
    ?_assert(relative_error(0.84147, spriki_funs:sin(1)) =< 0.0001),
    ?_assert(relative_error(-0.19866, spriki_funs:sin(-0.2)) =< 0.0001)
  ].

sinh_test_() ->
  [
    ?_assert(relative_error(-1.1752, spriki_funs:sinh(-1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:sinh(0)) =< 0.0001),
    ?_assert(relative_error(1.1752, spriki_funs:sinh(1)) =< 0.0001),
    ?_assert(relative_error(-0.20133, spriki_funs:sinh(-0.2)) =< 0.0001)
  ].

sqrt_test_() ->
  [
    ?_assert(relative_error(0, spriki_funs:sqrt(0)) =< 0.0001),
    ?_assert(relative_error(1, spriki_funs:sqrt(1)) =< 0.0001),
    ?_assert(relative_error(4, spriki_funs:sqrt(16)) =< 0.0001),
    ?_assert(relative_error(3.16227, spriki_funs:sqrt(10 + ?TINY_FLOAT)) =< 0.0001)
  ].

subtotal_test_() ->
  [
    ?_assert(relative_error(39.30764, lists:nth(1, spriki_funs:subtotal(1, [?ARR1]))) =< 0.0001),
    ?_assert(relative_error(491, lists:nth(1, spriki_funs:subtotal(4, [?ARR1]))) =< 0.0001),
    ?_assert(relative_error(0, lists:nth(1, spriki_funs:subtotal(6, [?ARR1]))) =< 0.0001)
  ].

sum_test_() ->
  [
    ?_assert(relative_error(?HUGE_NEGATIVE_INT + ?TINY_FLOAT + 999, spriki_funs:sum([999, 0, ?TINY_FLOAT, ?HUGE_NEGATIVE_INT])) =< 0.0001)
  ].

sumif_test_() ->
  [
    ?_assert(relative_error(63000, spriki_funs:sumif([100000, 200000, 300000, 400000], fun(X) -> X > 160000 end, [7000, 14000, 21000, 28000])) =< 0.0001),
    ?_assert(relative_error(0.1, spriki_funs:sumif([1, -10, 9.1, 11.11], fun(X) -> X < 10 end)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:sumif([0], fun(X) -> X == 50 end, [0])) =< 0.0001)
  ].

sumproduct_test_() ->
  [
    ?_assert(relative_error(156, spriki_funs:sumproduct([3,4,8,6,1,9, 2,7,6,7,5,3], 2)) =< 0.0001),
    ?_assert(relative_error(12, spriki_funs:sumproduct([1, 2, 10, 1], 2)) =< 0.0001),
    ?_assert(relative_error(12, spriki_funs:sumproduct([1, 2, 10, 1], 1)) =< 0.0001)
  ].

sumsq_test_() ->
  [
    ?_assert(relative_error(1, spriki_funs:sumsq([0, -1])) =< 0.0001),
    ?_assert(relative_error(123.4721, spriki_funs:sumsq([-0.2, 11.11])) =< 0.0001)
  ].

sumx2my2_test_() ->
  [
    ?_assert(relative_error(-55, spriki_funs:sumx2my2([2,3,9,1,8,7,5], [6,5,11,7,5,4,4])) =< 0.0001),
    ?_assert(relative_error(541760.1259, spriki_funs:sumx2my2([-1.5, ?TINY_FLOAT, 11.11, -0.2, 981, 0, 452], [0.01, 10, 12.19, 1, 0, 766, 195])) =< 0.0001)
  ].

sumx2py2_test_() ->
  [
    ?_assert(relative_error(521, spriki_funs:sumx2py2([2,3,9,1,8,7,5], [6,5,11,7,5,4,4])) =< 0.0001),
    ?_assert(relative_error(1791821.318, spriki_funs:sumx2py2([-1.5, ?TINY_FLOAT, 11.11, -0.2, 981, 0, 452], [0.01, 10, 12.19, 1, 0, 766, 195])) =< 0.0001)
  ].

sumxmy2_test_() ->
  [
    ?_assert(relative_error(79, spriki_funs:sumxmy2([2,3,9,1,8,7,5], [6,5,11,7,5,4,4])) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:sumxmy2([1], [1])) =< 0.0001),
    ?_assert(relative_error(1615270.886, spriki_funs:sumxmy2([-1.5, ?TINY_FLOAT, 11.11, -0.2, 981, 0, 452], [0.01, 10, 12.19, 1, 0, 766, 195])) =< 0.0001)
  ].

tan_test_() ->
  [
    ?_assert(relative_error(-1.5574, spriki_funs:tan(-1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:tan(0)) =< 0.0001),
    ?_assert(relative_error(1.5574, spriki_funs:tan(1)) =< 0.0001),
    ?_assert(relative_error(-0.20271, spriki_funs:tan(-0.2)) =< 0.0001)
  ].

tanh_test_() ->
  [
    ?_assert(relative_error(-0.76159, spriki_funs:tanh(-1)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:tanh(0)) =< 0.0001),
    ?_assert(relative_error(0.76159, spriki_funs:tanh(1)) =< 0.0001),
    ?_assert(relative_error(-0.19737, spriki_funs:tanh(-0.2)) =< 0.0001)
  ].

times_test_() ->
  [
    ?_assert(relative_error(?HUGE_INT * ?TINY_FLOAT, spriki_funs:times(?HUGE_INT, ?TINY_FLOAT)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:times(?HUGE_NEGATIVE_INT, 0)) =< 0.0001)
  ].

trunc_test_() ->
  [
    ?_assert(relative_error(8, spriki_funs:trunc(8.9)) =< 0.0001),
    ?_assert(relative_error(-8, spriki_funs:trunc(-8.9)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:trunc(0)) =< 0.0001),
    ?_assert(relative_error(10, spriki_funs:trunc(11.23, -1)) =< 0.0001),
    ?_assert(relative_error(-1.123, spriki_funs:trunc(-1.123, 5)) =< 0.0001),
    ?_assert(relative_error(0, spriki_funs:trunc(?TINY_FLOAT, 4)) =< 0.0001),
    ?_assert(relative_error(-10, spriki_funs:trunc(-11.11, -1)) =< 0.0001)
  ].

