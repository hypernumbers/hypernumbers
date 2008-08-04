%%% Stats functions.
%%% <hasan@hypernumbers.com>

%%% IMPORTANT NOTES:
%%%
%%% In CHIDIST(X, DegreesOfFreedom) in Excel DegreesOfFreedom is capped at
%%% 10^10. We're doing that too (for now anyway).
%%%
%%% 

-module(stdfuns_stats).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

-export([
         avedev/1,
         average/1,
         averagea/1,
         %%averageif/1,
         %%averageifs/1,
         %%betadist/1,
         %%betainv/1,
         binomdist/1,
         chidist/1,
         %%chiinv/1,
         %%chitest/1,
         %%confidence/1,
         correl/1,
         count/1,
         %%counta/1,
         countblank/1,
         %%countif/1,
         %%countifs/1,
         covar/1,
         critbinom/1,
         devsq/1,
         expondist/1,
         %%fdist/1,
         %%finv/1,
         %%fisher/1,
         %%fisherinv/1,
         forecast/1,
         frequency/1,
         %%ftest/1,
         gammadist/1,
         %%gammainv/1,
         %%gammaln/1,
         %%geomean/1,
         %%growth/1,
         %%harmean/1,
         %%hypgeomdist/1,
         intercept/1,
         kurt/1,
         large/1,
         linest/1,
         %%logest/1,
         %%loginv/1,
         %%lognormdist/1,
         max/1,
         maxa/1,
         median/1,
         min/1,
         mina/1,
         mode/1,
         %%negbinomdist/1,
         normdist/1,
         %%norminv/1,
         normsdist/1,
         %%normsinv/1,
         pearson/1,
         percentile/1,
         %%percentrank/1,
         permut/1,
         %%poisson/1,
         %%prob/1,
         quartile/1,
         rank/1,
         %%rsq/1,
         skew/1,
         %%slope/1,
         small/1,
         standardize/1,
         stdev/1,
         stdeva/1,
         stdevp/1,
         stdevpa/1,
         steyx/1,
         %%tdist/1,
         %%tinv/1,
         trend/1,
         trimmean/1,
         %%ttest/1,
         var/1,
         vara/1,
         varp/1,
         varpa/1,
         weibull/1
         %%ztest/1
        ]).

-define(default_rules, [cast_strings, cast_bools, zero_blanks, cast_dates]).

avedev(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, ?default_rules),
    ?ensure_nonzero(length(Nums)),
    avedev1(Nums).
avedev1(Nums) ->
    Avg = average1(Nums),
    Deviation = foldl(fun(X, Acc) ->
                              Acc + erlang:abs(Avg - X)
                      end,
                      0, Nums),
    Deviation / length(Nums).

average(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, ?default_rules),
    ?ensure_nonzero(length(Nums)),
    average1(Nums).
average1(Nums) ->
    lists:sum(Nums) / length(Nums).

averagea(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, ?default_rules ++ [zero_errvals]),
    ?ensure_nonzero(length(Nums)),
    average1(Nums).

binomdist([V1, V2, V3, V4]) ->
    [Succn, Trials] = ?ints([V1, V2], ?default_rules),
    Succprob = ?number(V3, ?default_rules),
    ?ensure(Succn =< Trials, ?ERR_NUM),
    ?ensure_non_negatives([Succn, Succprob]),
    ?ensure(Succprob =< 1, ?ERR_NUM),
    Cumul = ?bool(V4, [cast_strings, false_blanks, cast_dates]),
    binomdist1(Succn, Trials, Succprob, Cumul).
binomdist1(Ns, Nt, Ps, false) ->
    stdfuns_math:combin([Nt, Ps]) * math:pow(Ps, Ns) * math:pow((1 - Ps),
                                                                (Nt - Ns));
%% TODO: Rewrite to tail-recursive.
binomdist1(Ns, Nt, Ps, true) ->
    binomdist1(Ns, Nt, Ps, false) + binomdist1(Ns - 1, Nt, Ps, true).

chidist([X, Degfree_]) ->
    ?ensure_numbers([X, Degfree_]),
    Degfree = erlang:trunc(Degfree_),
    ?ensure_non_negative(X),
    ?ensure(Degfree >= 1, ?ERR_NUM),
    ?ensure(Degfree =< 1.0e+10, ?ERR_NUM),
    chidist1(X, Degfree).
chidist1(X, Degfree) ->
    Alpha = Degfree / 2, % 2 is beta
    Chi = 1 / (math:pow(2, Alpha) * stdfuns_math:fact1(Alpha)),
    math:pow(Chi, (Alpha - 1)) * math:exp(X * -0.5).

correl([L1, L2]) ->
    _Nums1 = ?filter_numbers(?ensure_no_errvals(?flatten(L1))),
    _Nums2 = ?filter_numbers(?ensure_no_errvals(?flatten(L2))),
    0. %% TODO:

count([L0]) ->
    L = ?flatten(L0),
    Nums = ?filter_numbers(L),
    Dates = [X || X <- L, element(1, X) == date],
    Strs  = [X || X <- L, tconv:to_num(X) =/= {error, nan}],
    length(Nums) + length(Dates) + length(Strs).
                 
countblank([L]) ->
    length([X || X <- L, X == blank]).

covar([L1, L2]) ->
    Ary1 = ?filter_numbers(L1),
    Ary2 = ?filter_numbers(L2),
    ?ensure_nonzero(length(Ary1)),
    ?ensure_nonzero(length(Ary2)),
    ?ensure(length(Ary1) == length(Ary2), ?ERR_NA),
    covar1(Ary1, Ary2).
covar1(_Ary1, _Ary2) ->
    0. %% TODO:

critbinom([Trials0, Prob, Alpha]) ->
    ?ensure_numbers([Trials0, Prob, Alpha]),
    ?ensure(Trials0 >= 0, ?ERR_NUM),
    ?ensure(Prob >= 0 andalso Prob =< 1, ?ERR_NUM),
    ?ensure(Alpha >= 0 andalso Alpha =< 1, ?ERR_NUM),
    critbinom1(trunc(Trials0), Prob, Alpha, 0).
critbinom1(Trials, Prob, Alpha, X) ->    
    Val = binomdist1(X, Trials, Prob, true),
    ?COND(Val >= Alpha,
          X,
          critbinom1(Trials, Prob, Alpha, X + 1)).

devsq([L]) ->
    Vals = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    devsq1(Vals).
devsq1(Vals) ->
    moment(Vals, 2) * length(Vals).

expondist([X, Lambda, Cum]) ->
    ?ensure_numbers([X, Lambda]),
    ?ensure_non_negatives([X, Lambda]),
    expondist1(X, Lambda, cast(Cum, bool)).
expondist1(X, Lambda, true) ->
    1 - math:exp(-1 * X / Lambda);
expondist1(X, Lambda, false) ->
    math:exp(-1 * X / Lambda) / Lambda.

forecast([N, L1, L2]) ->
    ?ensure_number(N),
    Kys = ?filter_numbers(?ensure_no_errvals(?flatten(L1))),
    Kxs = ?filter_numbers(?ensure_no_errvals(?flatten(L2))),
    ?ensure(length(Kys) > 0 andalso length(Kys) == length(Kxs), ?ERR_NA),
    forecast1(N, Kys, Kxs).
forecast1(X, Kys, Kxs) ->
    {matrix, [B1, B0]} = linest1(Kys, Kxs),
    B1 * X + B0.

frequency([L1, L2]) ->
    Data = ?filter_numbers(?ensure_no_errvals(?flatten(L1))),
    Bins = ?filter_numbers(?ensure_no_errvals(?flatten(L2))),
    frequency1(Data, Bins).
frequency1(_Data, _Bins) ->
    0. %% TODO:

gammadist([X, Alpha, Beta, Cum]) ->
    ?ensure_numbers([X, Alpha, Beta]),
    ?ensure_non_negative(X),
    ?ensure_positive(Alpha),
    ?ensure_positive(Beta),
    gammadist1(X, Alpha, Beta, cast(Cum, bool)).
gammadist1(X, Alpha, Beta, false) ->
    Top = math:pow(X, Alpha - 1) * math:exp(-1 * X / Beta),
    Top / (math:pow(Beta, Alpha) * stdfuns_math:fact1(round(Alpha)));
gammadist1(_X, _Alpha, _Beta, true) ->
    0. %% TODO:

intercept([L1, L2]) ->
    Kys = ?filter_numbers(?ensure_no_errvals(?flatten(L1))),
    Kxs = ?filter_numbers(?ensure_no_errvals(?flatten(L2))),
    intercept1(Kys, Kxs).
intercept1(Kys, Kxs) ->
    {matrix, [M, C]} = linest1(Kys, Kxs),
    C / M.

kurt([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    ?ensure(length(Nums) > 3, ?ERR_DIV),
    kurt1(Nums).
kurt1(Nums) ->
    (moment(Nums, 4) / math:pow(moment(Nums, 2), 2)) - 3.

large([L, K]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    ?ensure(length(Nums) > 0, ?ERR_NUM),
    ?ensure_number(K),
    ?ensure_positive(K),
    ?ensure(length(Nums) >= K, ?ERR_NUM),
    large1(Nums, K).
large1(Nums, K) ->
    nth(K, reverse(sort(Nums))).

linest(_) ->
    linest1(0, 0).
linest1(_, _) ->
    {matrix, [0, 0]}. %% TODO:

max([L]) ->
    Flatl = ?ensure_no_errvals(?flatten(L)),
    Nums = map(fun(X) when is_number(X) ->
                       X;
                  (S) when is_list(S) ->
                       case tconv:to_num(S) of
                           {error, nan} -> ?ERR_VAL;
                           V            -> V
                       end
               end,
               Flatl),
    ?COND(length(Nums) == 0, 0, lists:max(Nums)).
    
maxa([L]) ->
    Flatl = ?ensure_no_errvals(?flatten(L)),
    Nums = map(fun(X) -> cast(X, num) end, Flatl),
    ?COND(length(Nums) == 0, 0, lists:max(Nums)).

median([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    quartile1(Nums, 2).

min([L]) ->
    Flatl = ?ensure_no_errvals(?flatten(L)),
    Nums = map(fun(X) when is_number(X) ->
                       X;
                  (S) when is_list(S) ->
                       case tconv:to_num(S) of
                           {error, nan} -> ?ERR_VAL;
                           V            -> V
                       end
               end,
               Flatl),
    ?COND(length(Nums) == 0, 0, lists:min(Nums)).

mina([L]) ->
    Flatl = ?ensure_no_errvals(?flatten(L)),
    Nums = map(fun(X) -> cast(X, num) end, Flatl),
    ?COND(length(Nums) == 0, 0, lists:min(Nums)).

mode([L]) ->
    Flatl = ?filter_numbers_with_cast(?ensure_no_errvals(?flatten(L))),
    Nums = map(fun(X) -> cast(X, num) end, Flatl),
    mode1(Nums).
mode1(Nums) ->
    Maptbl = mode1(Nums, []),
    {Num, Count} = foldl(fun(Elt = {_, Cnt}, Acc = {_, Maxcnt}) ->
                                 ?COND(Cnt > Maxcnt, Elt, Acc)
                         end,
                         hd(Maptbl), tl(Maptbl)),
    ?COND(Count == 1,
          ?ERR_NA, % no duplicates
          Num).
                
mode1([H|T], Maptbl) ->
    case keysearch(H, 1, Maptbl) of
        {value, {H, Cnt}} -> % update count
            mode1(T, lists:keyreplace(H, 1, {H, Cnt + 1}, Maptbl));
        false -> % create entry
            mode1(T, [Maptbl | [{H, 1}]])
    end;
mode1([], Maptbl) ->
    Maptbl.

normdist([N0, Mean, Stdev, Cum]) ->
    N = cast(N0, num),
    ?ensure_numbers([N, Mean, Stdev]),
    ?ensure_positive(Stdev),
    normdist1(N, Mean, Stdev, cast(Cum, bool)).
normdist1(_N, _Mean, _Stdev, true) ->
    0; %% TODO:
normdist1(_N, _Mean, _Stdev, false) ->
    0. %% TODO:

normsdist([Z]) ->
    normdist([Z, 0, 1, true]).

pearson([A1, A2]) ->
    Nums1 = ?filter_numbers(?ensure_no_errvals(?flatten(A1))),
    Nums2 = ?filter_numbers(?ensure_no_errvals(?flatten(A2))),
    pearson1(Nums1, Nums2).
pearson1(_Ys, _Xs) ->
    0. %% TODO:

percentile([L, K]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    ?ensure(length(Nums) > 0, ?ERR_NUM),
    ?ensure_number(K),
    ?ensure((K >= 0) and (K =< 1), ?ERR_NUM),
    percentile1(Nums, K).
percentile1(Nums, K) ->
    L = map(fun(X) -> X / lists:sum(Nums) end,
            cumulate(Nums)),
    firstgte(L, K).

permut([N, K]) ->
    ?ensure_numbers([N, K]),
    ?ensure_positive(N),
    ?ensure(N >= K, ?ERR_NUM),
    ?ensure_non_negative(K),
    permut1(trunc(N), trunc(K)).
permut1(N, K) ->
    stdfuns_math:fact1(N) div stdfuns_math:fact1(N - K).

quartile([L, Q]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    ?ensure(length(Nums) > 0, ?ERR_NUM),
    ?ensure_number(Q),
    ?ensure((Q >= 0) and (Q =< 4), ?ERR_NUM),
    quartile1(Nums, trunc(Q)).
quartile1(Nums, Q) ->
    nth(percentile1(Nums, Q * 0.25), Nums).

rank([Num, L]) ->
    rank([Num, L, 0]);
rank([Num, L, Order]) ->
    ?ensure_number(Num),
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    rank1(Num, Nums, cast(Order, bool)).
rank1(N, Nums, true) ->
    firstgte(sort(Nums), N);
rank1(N, Nums, false) ->
    (length(Nums) + 1) - rank1(N, Nums, true).

skew([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    ?ensure(length(Nums) >= 3, ?ERR_DIV),
    skew1(Nums).
skew1(Nums) ->
    moment(Nums, 3) / math:pow(moment(Nums, 2), 1.5).

small([A, K]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(A))),
    ?ensure_number(K),
    ?ensure_positive(K),
    small1(Nums, K).
small1(Nums, K) ->
    nth(K, sort(Nums)).

standardize([Num, Mean, Stdev]) ->
    ?ensure_number(Num),
    ?ensure_number(Mean),
    ?ensure_number(Stdev),
    ?ensure_positive(Stdev),
    standardize1(Num, Mean, Stdev).
standardize1(Num, Mean, Stdev) ->
    (Num - Mean) / math:sqrt(Stdev).

stdev([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    stdev1(Nums).
stdev1(Nums) ->
    math:sqrt(devsq1(Nums) / (length(Nums) - 1)).

stdeva([L]) ->
    Nums = ?filter_numbers_with_cast(?ensure_no_errvals(?flatten(L))),
    stdev1(Nums).

stdevp([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    stdevp1(Nums).
stdevp1(Nums) ->
    math:sqrt(devsq1(Nums) / (length(Nums) - 1)).

stdevpa([L]) ->
    Nums = ?filter_numbers_with_cast(?ensure_no_errvals(?flatten(L))),
    stdevp1(Nums).

steyx([Ys0, Xs0]) ->
    Ys = ?filter_numbers(?ensure_no_errvals(?flatten(Ys0))),
    Xs = ?filter_numbers(?ensure_no_errvals(?flatten(Xs0))),
    steyx1(Ys, Xs).
steyx1(Ys, Xs) ->
    math:pow(pearson1(Ys, Xs), 2).

trend([_Kys0, _Kxs0, _Nxs0, _Const]) -> 
    0. %% TODO:

trimmean([A0, Percent]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(A0))),
    ?ensure_number(Percent),
    ?ensure_non_negative(Percent),
    ?ensure(Percent =< 1, ?ERR_NUM),
    trimmean1(Nums, Percent).
trimmean1(Nums, Percent) ->
    N = round((Percent / 100) * length(Nums)) div 2,
    average1(sublist(sort(Nums), N + 1, length(Nums) - 2 * N)).

var([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    var1(Nums).
var1(Nums) ->
    math:pow(stdev1(Nums), 2).

vara([L]) ->
    Nums = ?filter_numbers_with_cast(?ensure_no_errvals(?flatten(L))),
    var1(Nums).

varp([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    varp1(Nums).
varp1(Nums) ->
    math:pow(stdevp1(Nums), 2).

varpa([L]) ->
    Nums = ?filter_numbers_with_cast(?ensure_no_errvals(?flatten(L))),
    varp1(Nums).

weibull([X, Alpha, Beta, Cum]) ->
    ?ensure_numbers([X, Alpha, Beta]),
    ?ensure_non_negative(X),
    ?ensure_non_negative(Alpha),
    ?ensure_non_negative(Beta),
    weibull1(X, Alpha, Beta, cast(Cum, bool)).
weibull1(X, Alpha, Beta, true) ->
    1 - math:exp(-1 * math:pow(X, Alpha) / Beta);
weibull1(X, Alpha, Beta, false) ->
    (Alpha / Beta) * math:pow(X, Alpha - 1) * math:exp(-1 * math:pow(X, Alpha) / Beta).
    

%%% Private functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% Finds index of first element in L that's >= N.
firstgte(L, N) ->
    firstgte(L, N, 1).
firstgte([H|T], N, C) when H < N ->
    firstgte(T, N, C + 1);
firstgte([H|_T], N, C) when H >= N ->
    C;
firstgte([], _, _) ->
    0.

%% Returns list in element at position n equals the sum of elements 1 to n in
%% the original list.
cumulate([Hd | Tl]) ->
    reverse(foldl(fun(X, [H | T]) ->
                          [X + H, H | T]
                  end,
                  [Hd], Tl)).

moment(Vals, M) ->
    Avg = average1(Vals),
    lists:foldl(fun(X, Acc) -> Acc + math:pow((Avg - X), M) end,
                0, Vals) / length(Vals).
