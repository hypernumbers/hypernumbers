%%%----------------------------------------------------------------------------
%%% File        : excel_stats_funs_test_SUITE.erl
%%% Author      : Hasan Veldstra <hasan.veldstra@gmail.com>
%%% Description : Unit tests for Spriki's implementation of Excel's statistical
%%%               functions.
%%%
%%% Created     : 30 Sep 2007 by Hasan Veldstra <hasan.veldstra@gmail.com>
%%%----------------------------------------------------------------------------

%------------------------------------------------------------------------------
% IMPORTANT NOTES:
%
% * Right now, the functions are fed only 100% valid input.
% * Floats are tested to precision of 4dp.
% * There are no tests for Excel 2003 functions (yet).
%
% * INCOMPLETE:
%   * COUNT (more tests with date arguments in more formats)
%   * COUNTA, COUNTBLANK, COUNTIF
%   * Randomized test for geomean() and harmean().
%   * CHITEST
%   * LINEST
%   * TREND
%------------------------------------------------------------------------------

-module(excel_stats_funs_test_SUITE).
-compile(export_all).
-include("test_server.hrl").
-include("../unit_tests/engine/useful_constants.hrl").

% Initiation before the whole suite.
init_per_suite(Config) ->
  PassConfig = [
  ],
  
  FailConfig = [],
  lists:merge([Config, PassConfig, FailConfig]).

% Cleanup after the whole suite.
end_per_suite(_Config) ->
    ok.

% Initiation before each test case.
init_per_testcase(_TestCase, Config) ->
  code:add_path("../../ebin"),
    Config.

% Cleanup after each test case.
end_per_testcase(_TestCase, _Config) ->
    ok.

% Return a list of all test cases in this test suite.
all(doc) -> ["Unit tests for Spriki's implementation of Excel's statistical functions."];
all(suite) ->
    Pass = [
      % Count and average
      average_test1, average_test2, average_test3, average_test4,
      averagea_test1, averagea_test2, averagea_test3, averagea_test4,
      count_test1, count_test2, count_test3,
      % counta_test1, counta_test2, counta_test3, counta_test4,
      % countblank_test1, countblank_test2, countblank_test3,
      % countif_test1, countif_test2, countif_test3,
      kurt_test1, kurt_test2, kurt_test3,
      large_test1, large_test2, large_test3,
      max_test1, max_test2, max_test3,
      maxa_test1, maxa_test2, maxa_test3,
      min_test1, min_test2, min_test3,
      mina_test1, mina_test2, mina_test3,
      mode_test1, mode_test2, mode_test3, mode_test4,
      percentile_test1, percentile_test2, percentile_test3, percentile_test4,
      permut_test1, permut_test2, permut_test3,
      quartile_test1, quartile_test2, quartile_test3, quartile_test4, quartile_test5, quartile_test6,
        quartile_test7, quartile_test8,
      rank_test1, rank_test2, rank_test3, rank_test4, rank_test5, rank_test6,
      small_test1, small_test2, small_test3,
      
      % Mean and deviation
      avedev_test1, avedev_test2, avedev_test3,
      confidence_test1, confidence_test2, confidence_test3,
      covar_test1, covar_test2, covar_test3, covar_test4,
      devsq_test1, devsq_test2, devsq_test3, devsq_test4,
      geomean_test1, geomean_test2, geomean_test3,
      harmean_test1, harmean_test2, harmean_test3,
      median_test1, median_test2, median_test3,
      stdev_test1, stdev_test2, stdev_test3,
      stdevp_test1, stdevp_test2, stdevp_test3,
      stdeva_test1, stdeva_test2, stdeva_test3,
      trimmean_test1, trimmean_test2, trimmean_test3,
      geomean_harmean_randomized_test, % Harmonic mean of a set is ALWAYS < geometric mean of the 
                                       % same set -- this test uses this property to test both.
    
      % Probability and distribution
      betadist_test1, betadist_test2, betadist_test3,
      betainv_test1, betainv_test2, betainv_test3,
      binomdist_test1, binomdist_test2, binomdist_test3, binomdist_test4, binomdist_test5, 
        binomdist_test6,
      chidist_test1, chidist_test2, chidist_test3,
      chiinv_test1, chiinv_test2, chiinv_test3,
      % chitest_test1, chitest_test2, chitest_test3,
      critbinom_test1, critbinom_test3, % NOTE: critbinom_test2 is taken out as it makes the whole suit crash at the moment
      expondist_test1, expondist_test2, expondist_test3, expondist_test4,
      fdist_test1, fdist_test2, fdist_test3, fdist_test4, fdist_test5,
      finv_test1, finv_test2, finv_test3,
      frequency_test1, frequency_test2, frequency_test3,
      ftest_test1, ftest_test2,
      gammadist_test1, gammadist_test2, gammadist_test3,
      gammainv_test1, gammainv_test2, gammainv_test3,
      gammaln_test1, gammaln_test2, gammaln_test3,
      hypgeomdist_test1,
      normdist_test1, normdist_test2, normdist_test3,
      normsdist_test1, normsdist_test2, normsdist_test3,
      skew_test1, skew_test2, skew_test3,
      weibull_test1, weibull_test2, weibull_test3,
      
      % Regression
      intercept_test1, intercept_test2, intercept_test3,
      slope_test1, slope_test2, slope_test3,
      steyx_test1, steyx_test2, steyx_test3,
      
      % Transformation and correlation
      correl_test1, correl_test2, correl_test3, correl_test4,
      fisher_test1, fisher_test2, fisher_test3,
      fisherinv_test1, fisherinv_test2, fisherinv_test3,
      pearson_test1, pearson_test2, pearson_test3,
      standardize_test1, standardize_test2, standardize_test3,
      
      % Trends
      forecast_test1, forecast_test2, forecast_test3,
      growth_test1,
      % linest_test1, linest_test2, linest_test3,
      % trend_test1, trend_test2, trend_test3,
      
      % Variance
      var_test1, var_test2, var_test3,
      varp_test1, varp_test2, varp_test3,
      vara_test1, vara_test2, vara_test3,
      varpa_test1, varpa_test2, varpa_test3
    ],
    Fail = [], 
    lists:merge([Pass, Fail]).

%----------
% THE TESTS
%----------

%------------------
% Count and average
%------------------

average_test1(doc) -> [""];
average_test1(suite) -> [];
average_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(11, spriki_funs:average([10, 7, 9, 27, 2])) =< 0.0001).
  
average_test2(doc) -> [""];
average_test2(suite) -> [];
average_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-539999997.0798, spriki_funs:average([10.1, -7, 9.001, -2700000000, 2.5])) =< 0.0001).
  
average_test3(doc) -> [""];
average_test3(suite) -> [];
average_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2500000000, spriki_funs:average([0, ?HUGE_INT, ?TINY_FLOAT, 0])) =< 0.0001).
  
average_test4(doc) -> [""];
average_test4(suite) -> [];
average_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2.9775, spriki_funs:average([11.11, -0.2, 0, 1])) =< 0.0001).


averagea_test1(doc) -> [""];
averagea_test1(suite) -> [];
averagea_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(8, spriki_funs:averagea([10, false, 7, 9, 27, 2, true])) =< 0.0001).

averagea_test2(doc) -> [""];
averagea_test2(suite) -> [];
averagea_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-385714283.48557, spriki_funs:averagea([true, 10.1, -7, 9.001, -2700000000, 2.5, false])) =< 0.0001).

averagea_test3(doc) -> [""];
averagea_test3(suite) -> [];
averagea_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:averagea([true])) =< 0.0001).

averagea_test4(doc) -> [""];
averagea_test4(suite) -> [];
averagea_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:averagea([false])) =< 0.0001).


count_test1(doc) -> [""];
count_test1(suite) -> [];
count_test1(Config) when is_list(Config) -> 
  test_util:expected(3, spriki_funs:count(2, true, "2")).

count_test2(doc) -> [""];
count_test2(suite) -> [];
count_test2(Config) when is_list(Config) -> 
  test_util:expected(3, spriki_funs:count([2, "20.5", "12/8/2008"])).

count_test3(doc) -> [""];
count_test3(suite) -> [];
count_test3(Config) when is_list(Config) -> 
  test_util:expected(3, spriki_funs:count([?TINY_FLOAT, "20.5", 32/32/99999])).


% counta_test1(doc) -> [""];
% counta_test1(suite) -> [];
% counta_test1(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:counta()).
% 
% counta_test2(doc) -> [""];
% counta_test2(suite) -> [];
% counta_test2(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:counta()).
% 
% counta_test3(doc) -> [""];
% counta_test3(suite) -> [];
% counta_test3(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:counta()).



% countblank_test1(doc) -> [""];
% countblank_test1(suite) -> [];
% countblank_test1(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:countblank()).
% 
% countblank_test2(doc) -> [""];
% countblank_test2(suite) -> [];
% countblank_test2(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:countblank()).
% 
% countblank_test3(doc) -> [""];
% countblank_test3(suite) -> [];
% countblank_test3(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:countblank()).

% countif_test1(doc) -> [""];
% countif_test1(suite) -> [];
% countif_test1(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:countif()).
% 
% countif_test2(doc) -> [""];
% countif_test2(suite) -> [];
% countif_test2(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:countif()).
% 
% countif_test3(doc) -> [""];
% countif_test3(suite) -> [];
% countif_test3(Config) when is_list(Config) -> 
%   test_util:expected(, spriki_funs:countif()).


kurt_test1(doc) -> [""];
kurt_test1(suite) -> [];
kurt_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-0.1518, spriki_funs:kurt([3,4,5,2,34,5,6,4,7])) =< 0.0001).

kurt_test2(doc) -> [""];
kurt_test2(suite) -> [];
kurt_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2.88758, spriki_funs:kurt([-1.5, ?TINY_FLOAT, 11.11, -0.2, 981, 0, 452])) =< 0.0001).

kurt_test3(doc) -> [""];
kurt_test3(suite) -> [];
kurt_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(5.57476, spriki_funs:kurt([0.01, 10, 12.19, 1, 0, 766, 195])) =< 0.0001).



large_test1(doc) -> [""];
large_test1(suite) -> [];
large_test1(Config) when is_list(Config) -> 
  test_util:expected(5, spriki_funs:large([3,5,3,5,4,4,2,4,6,7], 3)).

large_test2(doc) -> [""];
large_test2(suite) -> [];
large_test2(Config) when is_list(Config) -> 
  test_util:expected(4, spriki_funs:large([3,5,3,5,4,4,2,4,6,7], 7)).

large_test3(doc) -> [""];
large_test3(suite) -> [];
large_test3(Config) when is_list(Config) -> 
  test_util:expected(7, spriki_funs:large([3,5,3,5,4,4,2,4,6.99999,7], 1)).



max_test1(doc) -> [""];
max_test1(suite) -> [];
max_test1(Config) when is_list(Config) -> 
  test_util:expected(27, spriki_funs:max([10,7,9,27,2.5])).

max_test2(doc) -> [""];
max_test2(suite) -> [];
max_test2(Config) when is_list(Config) -> 
  test_util:expected(7, spriki_funs:max([3,5,3,5,4,4,2,4,6.99999,7])).

max_test3(doc) -> [""];
max_test3(suite) -> [];
max_test3(Config) when is_list(Config) -> 
  test_util:expected(?TINY_FLOAT, spriki_funs:max(?TINY_FLOAT, -1, ?TINY_FLOAT, ?HUGE_NEGATIVE_INT)).



maxa_test1(doc) -> [""];
maxa_test1(suite) -> [];
maxa_test1(Config) when is_list(Config) -> 
  test_util:expected(1, spriki_funs:maxa([-1, 0, 0.5, true])).

maxa_test2(doc) -> [""];
maxa_test2(suite) -> [];
maxa_test2(Config) when is_list(Config) -> 
  test_util:expected(10, spriki_funs:maxa([-1, 0, 0.5, true, "10"])).

maxa_test3(doc) -> [""];
maxa_test3(suite) -> [];
maxa_test3(Config) when is_list(Config) -> 
  test_util:expected(0, spriki_funs:maxa([-0.2, -?TINY_FLOAT, false, "-10"])).



min_test1(doc) -> [""];
min_test1(suite) -> [];
min_test1(Config) when is_list(Config) -> 
  test_util:expected(2, spriki_funs:min([10,7,9,27,2])).

min_test2(doc) -> [""];
min_test2(suite) -> [];
min_test2(Config) when is_list(Config) -> 
  test_util:expected(0, spriki_funs:min([?TINY_FLOAT, 0, 0.2, ?HUGE_INT])).

min_test3(doc) -> [""];
min_test3(suite) -> [];
min_test3(Config) when is_list(Config) -> 
  test_util:expected(2.99999, spriki_funs:min([2.99999,5,3,5,4,4,2,4,6.99999,7])).



mina_test1(doc) -> [""];
mina_test1(suite) -> [];
mina_test1(Config) when is_list(Config) -> 
  test_util:expected(0, spriki_funs:mina([0.1, 0.5, true, false, ?TINY_FLOAT])).

mina_test2(doc) -> [""];
mina_test2(suite) -> [];
mina_test2(Config) when is_list(Config) -> 
  test_util:expected(-1, spriki_funs:mina([-1, 0, 0.5, true, "10"])).

mina_test3(doc) -> [""];
mina_test3(suite) -> [];
mina_test3(Config) when is_list(Config) -> 
  test_util:expected(-10, spriki_funs:mina([-0.2, -?TINY_FLOAT, false, "-10"])).



mode_test1(doc) -> [""];
mode_test1(suite) -> [];
mode_test1(Config) when is_list(Config) -> 
  test_util:expected(4, spriki_funs:mode([5.6, 4, 3, 1, 4, 5, 77, 65, 4, 4])).

mode_test2(doc) -> [""];
mode_test2(suite) -> [];
mode_test2(Config) when is_list(Config) -> 
  test_util:expected(?TINY_FLOAT, spriki_funs:mode([?TINY_FLOAT, 0, 1, 20, 20, 20, ?TINY_FLOAT, ?TINY_FLOAT])).

mode_test3(doc) -> [""];
mode_test3(suite) -> [];
mode_test3(Config) when is_list(Config) -> 
  test_util:expected(1, spriki_funs:mode([1,2,1,2])).

mode_test4(doc) -> [""];
mode_test4(suite) -> [];
mode_test4(Config) when is_list(Config) -> 
  test_util:expected(2, spriki_funs:mode([2,1,2,1])).



percentile_test1(doc) -> [""];
percentile_test1(suite) -> [];
percentile_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1.9, spriki_funs:percentile([1,3,2,4], 0.3)) =< 0.0001).

percentile_test2(doc) -> [""];
percentile_test2(suite) -> [];
percentile_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:percentile([1,1,1,1,0,1,0], 0.1)) =< 0.0001).

percentile_test3(doc) -> [""];
percentile_test3(suite) -> [];
percentile_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-3.99999, spriki_funs:percentile([11.11,-10,1,1,?TINY_FLOAT,1,2], 0.1)) =< 0.0001).

percentile_test4(doc) -> [""];
percentile_test4(suite) -> [];
percentile_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(10.5634, spriki_funs:percentile([11.11,0.2,1,1,?TINY_FLOAT,1,2], 0.99)) =< 0.0001).



permut_test1(doc) -> [""];
permut_test1(suite) -> [];
permut_test1(Config) when is_list(Config) -> 
  test_util:expected(970200, spriki_funs:permut(100, 3)).

permut_test2(doc) -> [""];
permut_test2(suite) -> [];
permut_test2(Config) when is_list(Config) -> 
  test_util:expected(1, spriki_funs:permut(0, 0)).

permut_test3(doc) -> [""];
permut_test3(suite) -> [];
permut_test3(Config) when is_list(Config) -> 
  test_util:expected(1, spriki_funs:permut(?TINY_FLOAT, 0)).

permut_test4(doc) -> [""];
permut_test4(suite) -> [];
permut_test4(Config) when is_list(Config) -> 
  test_util:expected(1, spriki_funs:permut(10, 0.9)).



quartile_test1(doc) -> [""];
quartile_test1(suite) -> [];
quartile_test1(Config) when is_list(Config) -> 
  test_util:expected(2.5, spriki_funs:quartile([?TINY_FLOAT,2,4,-7.1,8,9,10,12], 1)).

quartile_test2(doc) -> [""];
quartile_test2(suite) -> [];
quartile_test2(Config) when is_list(Config) -> 
  test_util:expected(5, spriki_funs:quartile([?TINY_FLOAT,2,4,-7.1,8,9,10,12], 2)).

quartile_test3(doc) -> [""];
quartile_test3(suite) -> [];
quartile_test3(Config) when is_list(Config) -> 
  test_util:expected(8.5, spriki_funs:quartile([?TINY_FLOAT,2,4,-7.1,8,9,10,12], 3)).

quartile_test4(doc) -> [""];
quartile_test4(suite) -> [];
quartile_test4(Config) when is_list(Config) -> 
  test_util:expected(12, spriki_funs:quartile([?TINY_FLOAT,2,4,-7.1,8,9,10,12], 4)).

quartile_test5(doc) -> [""];
quartile_test5(suite) -> [];
quartile_test5(Config) when is_list(Config) -> 
  test_util:expected(0, spriki_funs:quartile([0], 1)).

quartile_test6(doc) -> [""];
quartile_test6(suite) -> [];
quartile_test6(Config) when is_list(Config) -> 
  test_util:expected(0, spriki_funs:quartile([0], 2)).

quartile_test7(doc) -> [""];
quartile_test7(suite) -> [];
quartile_test7(Config) when is_list(Config) -> 
  test_util:expected(0.75, spriki_funs:quartile([0,1], 3)).

quartile_test8(doc) -> [""];
quartile_test8(suite) -> [];
quartile_test8(Config) when is_list(Config) -> 
  test_util:expected(10, spriki_funs:quartile([10], 4)).



rank_test1(doc) -> [""];
rank_test1(suite) -> [];
rank_test1(Config) when is_list(Config) -> 
  test_util:expected(3, spriki_funs:rank(3.5, [7, 3.5, 3.5, 1, 2], 1)).

rank_test2(doc) -> [""];
rank_test2(suite) -> [];
rank_test2(Config) when is_list(Config) -> 
  test_util:expected(7, spriki_funs:rank(7,   [7, 3.5, 3.5, 1, 2], 1)).

rank_test3(doc) -> [""];
rank_test3(suite) -> [];
rank_test3(Config) when is_list(Config) -> 
  test_util:expected(5, spriki_funs:rank(1, [0, 1, 2.2, -7, -11.11, 2.2, 0, 7, 5, 0.1, ?TINY_FLOAT])).

rank_test4(doc) -> [""];
rank_test4(suite) -> [];
rank_test4(Config) when is_list(Config) -> 
  test_util:expected(5, spriki_funs:rank(1, [0, 1, 2.2, -7, -11.11, 2.2, 0, 7, 5, 0.1, ?TINY_FLOAT], 0)).

rank_test5(doc) -> [""];
rank_test5(suite) -> [];
rank_test5(Config) when is_list(Config) -> 
  test_util:expected(7, spriki_funs:rank(1, [0, 1, 2.2, -7, -11.11, 2.2, 0, 7, 5, 0.1, ?TINY_FLOAT], 1)).

rank_test6(doc) -> [""];
rank_test6(suite) -> [];
rank_test6(Config) when is_list(Config) -> 
  test_util:expected(7, spriki_funs:rank(?TINY_FLOAT, [0, 1, 2.2, -7, -11.11, 2.2, 0, 7, 5, 0.1, ?TINY_FLOAT])).



small_test1(doc) -> [""];
small_test1(suite) -> [];
small_test1(Config) when is_list(Config) -> 
  test_util:expected(2, spriki_funs:small([ 1, 2, 3, 4, 5, 6, 7 ], 2)).

small_test2(doc) -> [""];
small_test2(suite) -> [];
small_test2(Config) when is_list(Config) -> 
  test_util:expected(4.1, spriki_funs:small([ 1, 2, 3, 4.1, 5.2, 6, 7.01 ], 4)).

small_test3(doc) -> [""];
small_test3(suite) -> [];
small_test3(Config) when is_list(Config) -> 
  test_util:expected(2, spriki_funs:small([3,5,3,5,4,4,2,4,6.99999,7], 1)).



%-------------------
% Mean and deviation
%-------------------

avedev_test1(doc) -> [""];
avedev_test1(suite) -> [];
avedev_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1.0204, spriki_funs:avedev([4,5,6,7,5,4,3])) =< 0.0001).

avedev_test2(doc) -> [""];
avedev_test2(suite) -> [];
avedev_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1975305.511, spriki_funs:avedev([1, 2, 100, -20.5, 9999999.99, 0, -1, 45.21, 0])) =< 0.0001).

avedev_test3(doc) -> [""];
avedev_test3(suite) -> [];
avedev_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:avedev([1])) =< 0.0001).



confidence_test1(doc) -> [""];
confidence_test1(suite) -> [];
confidence_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.69295, spriki_funs:confidence(0.05, 2.5, 50)) =< 0.0001).

confidence_test2(doc) -> [""];
confidence_test2(suite) -> [];
confidence_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(12.81551, spriki_funs:confidence(0.2, 10, 1)) =< 0.0001).

confidence_test3(doc) -> [""];
confidence_test3(suite) -> [];
confidence_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.44171, spriki_funs:confidence(?TINY_FLOAT, 0.1, 1)) =< 0.0001).



covar_test1(doc) -> [""];
covar_test1(suite) -> [];
covar_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-5557154.683, spriki_funs:covar([0,0.5,0.00001,3.998,24,1.7,21.8761,491,-90,-21.79,1.1],
                                                                          [20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2])) =< 0.0001).

covar_test2(doc) -> [""];
covar_test2(suite) -> [];
covar_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-83146645121, spriki_funs:covar([20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2],
                                                                          [0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27]))  =< 0.0001).

covar_test3(doc) -> [""];
covar_test3(suite) -> [];
covar_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-35425331.14, spriki_funs:covar([0,0.5,0.00001,3.998,24,1.7,21.8761,491,-90,-21.79,1.1],
                                                                          [0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27])) =< 0.0001).

covar_test4(doc) -> [""];
covar_test4(suite) -> [];
covar_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:covar([0], [0])) =< 0.0001).



devsq_test1(doc) -> [""];
devsq_test1(suite) -> [];
devsq_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(233734.7002, spriki_funs:devsq([0,0.5,0.00001,3.998,24,1.7,21.8761,491,-90,-21.79,1.1])) =< 0.0001).

devsq_test2(doc) -> [""];
devsq_test2(suite) -> [];
devsq_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:devsq([0])) =< 0.0001).

devsq_test3(doc) -> [""];
devsq_test3(suite) -> [];
devsq_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:devsq([-10])) =< 0.0001).

devsq_test4(doc) -> [""];
devsq_test4(suite) -> [];
devsq_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:devsq([0,0,0])) =< 0.0001).



geomean_test1(doc) -> [""];
geomean_test1(suite) -> [];
geomean_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2.44773, spriki_funs:geomean([1,0.5,0.00001,3.998,24,1.7,21.8761,491,90,21.79,1.1])) =< 0.0001).

geomean_test2(doc) -> [""];
geomean_test2(suite) -> [];
geomean_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(193.51566, spriki_funs:geomean([20, 1, 100.01, 1, 781, 955, 100, 110.78511, 863.009, 1000001, 999.2])) =< 0.0001).

geomean_test3(doc) -> [""];
geomean_test3(suite) -> [];
geomean_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(34.47162, spriki_funs:geomean([1, 10000461, 78.91, 86.3, 23, 1, 0.0001, 11.11, 42.42, 27])) =< 0.0001).



harmean_test1(doc) -> [""];
harmean_test1(suite) -> [];
harmean_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.0001, spriki_funs:harmean([1,0.5,0.00001,3.998,24,1.7,21.8761,491,90,21.79,1.1])) =< 0.0001).

harmean_test2(doc) -> [""];
harmean_test2(suite) -> [];
harmean_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(5.27954, spriki_funs:harmean([20, 1, 100.01, 1, 781, 955, 100, 110.78511, 863.009, 1000001, 999.2])) =< 0.0001).

harmean_test3(doc) -> [""];
harmean_test3(suite) -> [];
harmean_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.00109, spriki_funs:harmean([1, 10000461, 78.91, 86.3, 23, 1, 0.0001, 11.11, 42.42, 27])) =< 0.0001).



% TODO: implement me.
geomean_harmean_randomized_test() ->
  ok.

median_test1(doc) -> [""];
median_test1(suite) -> [];
median_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(3, spriki_funs:median([1,2,3,4,5])) =< 0.0001).

median_test2(doc) -> [""];
median_test2(suite) -> [];
median_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1.1, spriki_funs:median([0,0.5,0.00001,3.998,24,1.7,21.8761,491,-90,-21.79,1.1])) =< 0.0001).

median_test3(doc) -> [""];
median_test3(suite) -> [];
median_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(100, spriki_funs:median([20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2])) =< 0.0001).



stdev_test1(doc) -> [""];
stdev_test1(suite) -> [];
stdev_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(27.46391, spriki_funs:stdev([1345, 1301, 1368, 1322, 1310, 1370, 1318, 1350, 1303, 1299])) =< 0.0001).

stdev_test2(doc) -> [""];
stdev_test2(suite) -> [];
stdev_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(152.88384, spriki_funs:stdev([0,0.5,0.00001,3.998,24,1.7,21.8761,491,-90,-21.79,1.1])) =< 0.0001).

stdev_test3(doc) -> [""];
stdev_test3(suite) -> [];
stdev_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(301452.3558, spriki_funs:stdev()) =< 0.0001).


stdevp_test1(doc) -> [""];
stdevp_test1(suite) -> [];
stdevp_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(26.05455, spriki_funs:stdevp([1345, 1301, 1368, 1322, 1310, 1370, 1318, 1350, 1303, 1299])) =< 0.0001).

stdevp_test2(doc) -> [""];
stdevp_test2(suite) -> [];
stdevp_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(145.76902, spriki_funs:stdevp([0,0.5,0.00001,3.998,24,1.7,21.8761,491,-90,-21.79,1.1])) =< 0.0001).

stdevp_test3(doc) -> [""];
stdevp_test3(suite) -> [];
stdevp_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(287423.5437, spriki_funs:stdevp([20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2])) =< 0.0001).



stdeva_test1(doc) -> [""];
stdeva_test1(suite) -> [];
stdeva_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(27.46391, spriki_funs:stdeva([1345, 1301, 1368, 1322, 1310, 1370, 1318, 1350, 1303, 1299])) =< 0.0001).

stdeva_test2(doc) -> [""];
stdeva_test2(suite) -> [];
stdeva_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(153.31351, spriki_funs:stdeva([false, 0.5, 0.00001, 3.998, "24", true, 21.8761, 491, -90, -21.79, true])) =< 0.0001).

stdeva_test3(doc) -> [""];
stdeva_test3(suite) -> [];
stdeva_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(542.86221, spriki_funs:stdeva(["2000", true, 100.01, true, 781, 955, 100, -110.78511, -863.009, false, 999.2])) =< 0.0001).



trimmean_test1(doc) -> [""];
trimmean_test1(suite) -> [];
trimmean_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(3.77777, spriki_funs:trimmean([4, 5, 6, 7, 2, 3, 4, 5, 1, 2, 3], 0.2)) =< 0.0001).

trimmean_test2(doc) -> [""];
trimmean_test2(suite) -> [];
trimmean_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.91682, spriki_funs:trimmean([false, 0.5, 0.00001, 3.998, "24", true, 21.8761, 491, -90, -21.79, true], 0.4)) =< 0.0001).

trimmean_test3(doc) -> [""];
trimmean_test3(suite) -> [];
trimmean_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(327.00333, spriki_funs:trimmean(["2000", true, 100.01, true, 781, 955, 100, -110.78511, -863.009, false, 999.2], 0.8)) =< 0.0001).



%-----------------------------
% Probability and distribution
%-----------------------------

betadist_test1(doc) -> [""];
betadist_test1(suite) -> [];
betadist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.68547, spriki_funs:betadist(2, 8, 10, 1, 3)) =< 0.0001).

% The last two parameters are 0 and 1 by default, so the result of this call should be the same as
% that in test3.
betadist_test2(doc) -> [""];
betadist_test2(suite) -> [];
betadist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.82458, spriki_funs:betadist(0.2, 0.1, 0.8)) =< 0.0001).

betadist_test3(doc) -> [""];
betadist_test3(suite) -> [];
betadist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.82458, spriki_funs:betadist(0.2, 0.1, 0.8, 0, 1)) =< 0.0001).



% This is the inverse of betadist. Compare tests here with ones above.
betainv_test1(doc) -> [""];
betainv_test1(suite) -> [];
betainv_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2, spriki_funs:betainv(0.68547, 2, 8, 1, 3)) =< 0.0001).

betainv_test2(doc) -> [""];
betainv_test2(suite) -> [];
betainv_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.2, spriki_funs:betainv(0.82458, 0.1, 0.8)) =< 0.0001).

betainv_test3(doc) -> [""];
betainv_test3(suite) -> [];
betainv_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.2, spriki_funs:betainv(0.82458, 0.1, 0.8, 0, 1)) =< 0.0001).



binomdist_test1(doc) -> [""];
binomdist_test1(suite) -> [];
binomdist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.20507, spriki_funs:binomdist(6, 10, 0.5, false)) =< 0.0001).

binomdist_test2(doc) -> [""];
binomdist_test2(suite) -> [];
binomdist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.82812, spriki_funs:binomdist(6, 10, 0.5, true)) =< 0.0001).

binomdist_test3(doc) -> [""];
binomdist_test3(suite) -> [];
binomdist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:binomdist(6, 10, 0, false)) =< 0.0001).
  
binomdist_test4(doc) -> [""];
binomdist_test4(suite) -> [];
binomdist_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:binomdist(6, 10, 0, true)) =< 0.0001).

binomdist_test5(doc) -> [""];
binomdist_test5(suite) -> [];
binomdist_test5(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:binomdist(1, 1, 0, true)) =< 0.0001).

binomdist_test6(doc) -> [""];
binomdist_test6(suite) -> [];
binomdist_test6(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:binomdist(20, 20, 0.01, true)) =< 0.0001).



chidist_test1(doc) -> [""];
chidist_test1(suite) -> [];
chidist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.05, spriki_funs:chidist(18.307, 10)) =< 0.0001).

chidist_test2(doc) -> [""];
chidist_test2(suite) -> [];
chidist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.99747, spriki_funs:chidist(?TINY_FLOAT, 1)) =< 0.0001).

chidist_test3(doc) -> [""];
chidist_test3(suite) -> [];
chidist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.26824, spriki_funs:chidist(11.11, 9)) =< 0.0001).

% This is the inverse of chidist.
chiinv_test1(doc) -> [""];
chiinv_test1(suite) -> [];
chiinv_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(18.307, spriki_funs:chiinv(0.05, 10)) =< 0.0001).

chiinv_test2(doc) -> [""];
chiinv_test2(suite) -> [];
chiinv_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(?TINY_FLOAT, spriki_funs:chiinv(0.99747, 1)) =< 0.0001).

chiinv_test3(doc) -> [""];
chiinv_test3(suite) -> [];
chiinv_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(11.11, spriki_funs:chiinv(0.26824, 9)) =< 0.0001).



% chitest_test1(doc) -> [""];
% chitest_test1(suite) -> [];
% chitest_test1(Config) when is_list(Config) -> 
%   test_util:expected(true, relative_error(, spriki_funs:chitest()) =< 0.0001).
% 
% chitest_test2(doc) -> [""];
% chitest_test2(suite) -> [];
% chitest_test2(Config) when is_list(Config) -> 
%   test_util:expected(true, relative_error(, spriki_funs:chitest()) =< 0.0001).
% 
% chitest_test3(doc) -> [""];
% chitest_test3(suite) -> [];
% chitest_test3(Config) when is_list(Config) -> 
%   test_util:expected(true, relative_error(, spriki_funs:chitest()) =< 0.0001).


critbinom_test1(doc) -> [""];
critbinom_test1(suite) -> [];
critbinom_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(4, spriki_funs:critbinom(6, 0.5, 0.75)) =< 0.0001).

critbinom_test2(doc) -> [""];
critbinom_test2(suite) -> [];
critbinom_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2, spriki_funs:critbinom(100000, ?TINY_FLOAT, 0.8)) =< 0.0001).

critbinom_test3(doc) -> [""];
critbinom_test3(suite) -> [];
critbinom_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:critbinom(1000, ?TINY_FLOAT, 0.8)) =< 0.0001).



expondist_test1(doc) -> [""];
expondist_test1(suite) -> [];
expondist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.86466, spriki_funs:expondist(0.2, 10, true)) =< 0.0001).

expondist_test2(doc) -> [""];
expondist_test2(suite) -> [];
expondist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1.35335, spriki_funs:expondist(0.2, 10, false)) =< 0.0001).

expondist_test3(doc) -> [""];
expondist_test3(suite) -> [];
expondist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:expondist(0, ?HUGE_INT, true)) =< 0.0001).

expondist_test4(doc) -> [""];
expondist_test4(suite) -> [];
expondist_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(10, spriki_funs:expondist(0, 10, false)) =< 0.0001).


fdist_test1(doc) -> [""];
fdist_test1(suite) -> [];
fdist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.01, spriki_funs:fdist(15.20675, 6, 4)) =< 0.0001).

fdist_test2(doc) -> [""];
fdist_test2(suite) -> [];
fdist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.49999, spriki_funs:fdist(1, 1, 1)) =< 0.0001).

fdist_test3(doc) -> [""];
fdist_test3(suite) -> [];
fdist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.18555, spriki_funs:fdist(11.11, 1, 2)) =< 0.0001).

fdist_test4(doc) -> [""];
fdist_test4(suite) -> [];
fdist_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(3.55555, spriki_funs:fdist(0.2, 1, 2)) =< 0.0001).

fdist_test5(doc) -> [""];
fdist_test5(suite) -> [];
fdist_test5(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(12, spriki_funs:fdist(0.2, 2, 1)) =< 0.0001).



finv_test1(doc) -> [""];
finv_test1(suite) -> [];
finv_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(15.20675, spriki_funs:finv(0.01, 6, 4)) =< 0.0001).

finv_test2(doc) -> [""];
finv_test2(suite) -> [];
finv_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:finv(0.49999, 1, 1)) =< 0.0001).

finv_test3(doc) -> [""];
finv_test3(suite) -> [];
finv_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(11.11, spriki_funs:finv(0.18555, 1, 1)) =< 0.0001).



frequency_test1(doc) -> [""];
frequency_test1(suite) -> [];
frequency_test1(Config) when is_list(Config) -> 
  test_util:expected([1,2,4,2], spriki_funs:frequency([79,85,78,85,50,81,95,88,97], [70,79,89])).

frequency_test2(doc) -> [""];
frequency_test2(suite) -> [];
frequency_test2(Config) when is_list(Config) -> 
  test_util:expected(1, spriki_funs:frequency([1], [0])).

frequency_test3(doc) -> [""];
frequency_test3(suite) -> [];
frequency_test3(Config) when is_list(Config) -> 
  test_util:expected(1, spriki_funs:frequency([0], [10])).



ftest_test1(doc) -> [""];
ftest_test1(suite) -> [];
ftest_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.46831, spriki_funs:ftest([6, 7, 9, 15, 21], [20, 28, 31, 38, 40])) =< 0.0001).

ftest_test2(doc) -> [""];
ftest_test2(suite) -> [];
ftest_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:ftest([1, 2, 3], [1, 2, 3])) =< 0.0001).



gammadist_test1(doc) -> [""];
gammadist_test1(suite) -> [];
gammadist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.03263, spriki_funs:gammadist(10, 9, 2, false)) =< 0.0001).

gammadist_test2(doc) -> [""];
gammadist_test2(suite) -> [];
gammadist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.06809, spriki_funs:gammadist(10, 9, 2, true)) =< 0.0001).

gammadist_test3(doc) -> [""];
gammadist_test3(suite) -> [];
gammadist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.99995, spriki_funs:gammadist(10, 1, 1, true)) =< 0.0001).



gammainv_test1(doc) -> [""];
gammainv_test1(suite) -> [];
gammainv_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(8.64689, spriki_funs:gammainv(0.03263, 9, 2)) =< 0.0001).

gammainv_test2(doc) -> [""];
gammainv_test2(suite) -> [];
gammainv_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(9.99988, spriki_funs:gammainv(0.06809, 9, 2)) =< 0.0001).

gammainv_test3(doc) -> [""];
gammainv_test3(suite) -> [];
gammainv_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(9.90348, spriki_funs:gammainv(0.99995, 1, 1)) =< 0.0001).



gammaln_test1(doc) -> [""];
gammaln_test1(suite) -> [];
gammaln_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(12.80182, spriki_funs:gammaln(10)) =< 0.0001).

gammaln_test2(doc) -> [""];
gammaln_test2(suite) -> [];
gammaln_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(11.51291, spriki_funs:gammaln(0.00001)) =< 0.0001).

gammaln_test3(doc) -> [""];
gammaln_test3(suite) -> [];
gammaln_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(12815504.57, spriki_funs:gammaln(1000000)) =< 0.0001).


hypgeomdist_test1(doc) -> [""];
hypgeomdist_test1(suite) -> [];
hypgeomdist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.36326, spriki_funs:hypgeomdist(1, 4, 8, 20)) =< 0.0001).



normdist_test1(doc) -> [""];
normdist_test1(suite) -> [];
normdist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.90878, spriki_funs:normdist(42, 40, 1.5, true)) =< 0.0001).

normdist_test2(doc) -> [""];
normdist_test2(suite) -> [];
normdist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.10934, spriki_funs:normdist(42, 40, 1.5, false)) =< 0.0001).

normdist_test3(doc) -> [""];
normdist_test3(suite) -> [];
normdist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:normdist(42, 1, 1.5, true)) =< 0.0001).



normsdist_test1(doc) -> [""];
normsdist_test1(suite) -> [];
normsdist_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.15865, spriki_funs:normsdist(-1)) =< 0.0001).

normsdist_test2(doc) -> [""];
normsdist_test2(suite) -> [];
normsdist_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.5, spriki_funs:normsdist(0)) =< 0.0001).

normsdist_test3(doc) -> [""];
normsdist_test3(suite) -> [];
normsdist_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.84134, spriki_funs:normsdist(1)) =< 0.0001).



skew_test1(doc) -> [""];
skew_test1(suite) -> [];
skew_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0, spriki_funs:skew([0, 1, 2])) =< 0.0001).

skew_test2(doc) -> [""];
skew_test2(suite) -> [];
skew_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2.23606, spriki_funs:skew([?TINY_FLOAT, -11.11, 1, 1000000, -0.2])) =< 0.0001).

skew_test3(doc) -> [""];
skew_test3(suite) -> [];
skew_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-2.44948, spriki_funs:skew([0.2, 0.2, 0.2])) =< 0.0001).



weibull_test1(doc) -> [""];
weibull_test1(suite) -> [];
weibull_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.92958, spriki_funs:weibull(105, 20, 100, true)) =< 0.0001).

weibull_test2(doc) -> [""];
weibull_test2(suite) -> [];
weibull_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.03558, spriki_funs:weibull(105, 20, 100, false)) =< 0.0001).

weibull_test3(doc) -> [""];
weibull_test3(suite) -> [];
weibull_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:weibull(105, 100, 20, true)) =< 0.0001).


%-----------
% Regression
%-----------

intercept_test1(doc) -> [""];
intercept_test1(suite) -> [];
intercept_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.04838, spriki_funs:intercept([2,3,9,1,8], [6,5,11,7,5])) =< 0.0001).

intercept_test2(doc) -> [""];
intercept_test2(suite) -> [];
intercept_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(45.43503, spriki_funs:intercept([0, 0.5, 0.00001, 3.998, 24, 1.7, 21.8761, 491, -90, -21.79, 1.1])) =< 0.0001).

intercept_test3(doc) -> [""];
intercept_test3(suite) -> [];
intercept_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(100280.234, spriki_funs:intercept([20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2], [0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27, 40976.99], 100280.234)) =< 0.0001).



slope_test1(doc) -> [""];
slope_test1(suite) -> [];
slope_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.30555, spriki_funs:slope([2,3,9,1,8,7,5], [6,5,11,7,5,4,4])) =< 0.0001).

slope_test2(doc) -> [""];
slope_test2(suite) -> [];
slope_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-0.01006, spriki_funs:slope([20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2], [0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27, 40976.99])) =< 0.0001).

slope_test3(doc) -> [""];
slope_test3(suite) -> [];
slope_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-1.00646, spriki_funs:slope([0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27, 40976.99], [20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2])) =< 0.0001).



steyx_test1(doc) -> [""];
steyx_test1(suite) -> [];
steyx_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(3.30571, spriki_funs:steyx([2,3,9,1,8,7,5], [6,5,11,7,5,4,4])) =< 0.0001).

steyx_test2(doc) -> [""];
steyx_test2(suite) -> [];
steyx_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(159.72987, spriki_funs:steyx([0, 0.5, 0.00001, 3.998, 24, 1.7, 21.8761, 491, -90, -21.79, 1.1], [20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2])) =< 0.0001).

steyx_test3(doc) -> [""];
steyx_test3(suite) -> [];
steyx_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(316144.6489, spriki_funs:steyx([20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2], [0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27, 40976.99])) =< 0.0001).


%-------------------------------
% Transformation and correlation
%-------------------------------

correl_test1(doc) -> [""];
correl_test1(suite) -> [];
correl_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.99705, spriki_funs:correl([3, 2, 4, 5, 6], [9, 7, 12, 15, 17])) =< 0.0001).

correl_test2(doc) -> [""];
correl_test2(suite) -> [];
correl_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-0.15319, spriki_funs:correl([100, 0, 20, 43, 90, 0, 1, 40.5, -120, 456], [0, 10, 33, 567, -120.3332, 6991, 2, 1, 0, 0])) =< 0.0001).

correl_test3(doc) -> [""];
correl_test3(suite) -> [];
correl_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-0.13263, spriki_funs:correl([0, 0.5, 0.00001, 3.998, 24, 1.7, 21.8761, 491, -90, -21.79, 1.1], [20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2])) =< 0.0001).

correl_test4(doc) -> [""];
correl_test4(suite) -> [];
correl_test4(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-1, spriki_funs:correl([1, 0], [0, 1])) =< 0.0001).


fisher_test1(doc) -> [""];
fisher_test1(suite) -> [];
fisher_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.10033, spriki_funs:fisher(0.1)) =< 0.0001).

fisher_test2(doc) -> [""];
fisher_test2(suite) -> [];
fisher_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(2.64665, spriki_funs:fisher(0.99)) =< 0.0001).

fisher_test3(doc) -> [""];
fisher_test3(suite) -> [];
fisher_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.5493, spriki_funs:fisher(0.5)) =< 0.0001).


fisherinv_test1(doc) -> [""];
fisherinv_test1(suite) -> [];
fisherinv_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.09966, spriki_funs:fisherinv(0.10033)) =< 0.0001).

fisherinv_test2(doc) -> [""];
fisherinv_test2(suite) -> [];
fisherinv_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.98999, spriki_funs:fisherinv(2.64665)) =< 0.0001).

fisherinv_test3(doc) -> [""];
fisherinv_test3(suite) -> [];
fisherinv_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.49999, spriki_funs:fisherinv(0.5493)) =< 0.0001).



pearson_test1(doc) -> [""];
pearson_test1(suite) -> [];
pearson_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.69937, spriki_funs:pearson([9,7,5,3,1], [10,6,1,5,3])) =< 0.0001).

pearson_test2(doc) -> [""];
pearson_test2(suite) -> [];
pearson_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-0.13263, spriki_funs:pearson([0, 0.5, 0.00001, 3.998, 24, 1.7, 21.8761, 491, -90, -21.79, 1.1], [20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2])) =< 0.0001).

pearson_test3(doc) -> [""];
pearson_test3(suite) -> [];
pearson_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-0.08456, spriki_funs:pearson([0, 0.5, 0.00001, 3.998, 24, 1.7, 21.8761, 491, -90, -21.79, 1.1], [0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27, 40976.99])) =< 0.0001).



standardize_test1(doc) -> [""];
standardize_test1(suite) -> [];
standardize_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1.33333, spriki_funs:standardize(42, 40, 1.5)) =< 0.0001).

standardize_test2(doc) -> [""];
standardize_test2(suite) -> [];
standardize_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(1, spriki_funs:standardize(0.1, 0, 0.1)) =< 0.0001).

standardize_test3(doc) -> [""];
standardize_test3(suite) -> [];
standardize_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(-10, spriki_funs:standardize(-0.1, 0, 0.01)) =< 0.0001).


%-------
% Trends
%-------

forecast_test1(doc) -> [""];
forecast_test1(suite) -> [];
forecast_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(10.60725, spriki_funs:forecast(30, [6,7,9,15,21], [20, 28, 31, 38, 40])) =< 0.0001).

forecast_test2(doc) -> [""];
forecast_test2(suite) -> [];
forecast_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(45.43503, spriki_funs:forecast(0, ?ARR1, ?ARR2)) =< 0.0001).

forecast_test3(doc) -> [""];
forecast_test3(suite) -> [];
forecast_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(100280.234, spriki_funs:forecast(?TINY_FLOAT, ?ARR2, ?ARR3)) =< 0.0001).



growth_test1(doc) -> [""];
growth_test1(suite) -> [];
growth_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(72.95406, spriki_funs:growth(?ARR4)) =< 0.0001).


%---------
% Variance
%---------

var_test1(doc) -> [""];
var_test1(suite) -> [];
var_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(754.26666, spriki_funs:var([1345, 1301, 1368, 1322, 1310, 1370, 1318, 1350, 1303, 1299])) =< 0.0001).

var_test2(doc) -> [""];
var_test2(suite) -> [];
var_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(23373.47002, spriki_funs:var(?ARR1)) =< 0.0001).

var_test3(doc) -> [""];
var_test3(suite) -> [];
var_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(90873522824, spriki_funs:var(?ARR2)) =< 0.0001).



varp_test1(doc) -> [""];
varp_test1(suite) -> [];
varp_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(678.84, spriki_funs:varp([1345, 1301, 1368, 1322, 1310, 1370, 1318, 1350, 1303, 1299])) =< 0.0001).

varp_test2(doc) -> [""];
varp_test2(suite) -> [];
varp_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(21248.60911, spriki_funs:varp(?ARR1)) =< 0.0001).

varp_test3(doc) -> [""];
varp_test3(suite) -> [];
varp_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(82612293476, spriki_funs:varp(?ARR2)) =< 0.0001).



vara_test1(doc) -> [""];
vara_test1(suite) -> [];
vara_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.5, spriki_funs:vara([true, false])) =< 0.0001).

vara_test2(doc) -> [""];
vara_test2(suite) -> [];
vara_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(19690.32268, spriki_funs:vara(lists:merge(?ARR1, [true, false]))) =< 0.0001).

vara_test3(doc) -> [""];
vara_test3(suite) -> [];
vara_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(76898051938, spriki_funs:vara(lists:merge(?ARR2, [false, true]))) =< 0.0001).



varpa_test1(doc) -> [""];
varpa_test1(suite) -> [];
varpa_test1(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(0.22222, spriki_funs:varpa([true, true, false])) =< 0.0001).

varpa_test2(doc) -> [""];
varpa_test2(suite) -> [];
varpa_test2(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(19595.91947, spriki_funs:varpa(lists:merge(?ARR1, [true, true, false]))) =< 0.0001).

varpa_test3(doc) -> [""];
varpa_test3(suite) -> [];
varpa_test3(Config) when is_list(Config) -> 
  test_util:expected(true, relative_error(66306639797, spriki_funs:varpa(lists:merge(?ARR2, [false, false, true]))) =< 0.0001).


%-----------------
% HELPER FUNCTIONS
%-----------------
relative_error(X, Y) -> util2:relative_error(X, Y).
