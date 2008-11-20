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
%%-import(muin_util, [conv/2, cast/2]).

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
         %%correl/1,
         count/1,
         counta/1,
         countblank/1,
         %%countif/1,
         %%countifs/1,
         %%covar/1,
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
         harmean/1,
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
         %%normdist/1,
         %%norminv/1,
         normsdist/1,
         %%normsinv/1,
         pearson/1,
         percentile/1,
         %%percentrank/1,
         permut/1,
         poisson/1,
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
         %%trend/1,
         trimmean/1,
         %%ttest/1,
         var/1,
         vara/1,
         varp/1,
         varpa/1,
         weibull/1
         %%ztest/1
        ]).

-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).
-define(default_rules_bools, [cast_numbers, cast_strings, cast_blanks, cast_dates]).

avedev(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = Nums = ?numbers(Flatvs, [cast_strings, cast_bools, ignore_blanks, ban_dates]),
    ?ensure_nonzero(length(Nums)),
    avedev1(Nums).
avedev1(Nums) ->
    Avg = average(Nums),
    Deviation = foldl(fun(X, Acc) -> Acc + erlang:abs(Avg - X) end, 0, Nums),
    Deviation / length(Nums).

average(Vs) ->
    Flatvs = ?flatten_all(Vs),
    ?ensure(Flatvs =/= [], ?ERR_DIV),
    %% A bit of a special case here for backward compatibility with Excel which
    %% returns #DIV/0! when none of the arguments to AVERAGE can be cast (e.g.
    %% a range of empty cells). Our default is to return #VALUE! in such case.
    case muin_util:attempt(?DEFER(?numbers(Flatvs, [ignore_strings, cast_bools, ignore_blanks, ban_dates]))) of
        {ok, Nums} -> average1(Nums);
        {error, _} -> ?ERR_DIV
    end.
average1(Nums) ->
    lists:sum(Nums)/length(Nums).

%% TODO: errvals -> 0s in args.
averagea(Vs) ->
    Flatvs = ?flatten_all(Vs),
    ?ensure(Flatvs =/= [], ?ERR_DIV),
    Nums = ?numbers(Flatvs, ?default_rules),
    average1(Nums).

binomdist([V1, V2, V3, V4]) ->
    [Succn, Trials] = ?ints([V1, V2], ?default_rules),
    Succprob = ?number(V3, ?default_rules),
    Cumul = ?bool(V4, ?default_rules_bools),
    ?ensure(Succn =< Trials, ?ERR_NUM),
    ?ensure(Succn >= 0, ?ERR_NUM),
    ?ensure(Succprob >= 0, ?ERR_NUM),
    ?ensure(Succprob =< 1, ?ERR_NUM),
    binomdist1(Succn, Trials, trunc(Succprob * 100), Cumul).
binomdist1(Ns, Nt, Ps, false) ->
    stdfuns_math:combin([Ps, Nt]) * math:pow(Ps, Ns) * math:pow((1 - Ps),
                                                                (Nt - Ns));
%% TODO: Rewrite to tail-recursive.
binomdist1(Ns, Nt, Ps, true) ->
    binomdist1(Ns, Nt, Ps, false) + binomdist1(Ns - 1, Nt, Ps, true).

chidist([V1, V2]) ->
    X = ?number(V1, ?default_rules),
    Degfree = ?int(V2, ?default_rules),
    ?ensure_non_negative(X),
    ?ensure(Degfree >= 1, ?ERR_NUM),
    ?ensure(Degfree =< 1.0e+10, ?ERR_NUM),
    chidist1(X, Degfree).
chidist1(X, Degfree) ->
    Alpha = Degfree / 2, % 2 is beta
    Chi = 1 / (math:pow(2, Alpha) * stdfuns_math:fact1(Alpha)),
    math:pow(Chi, (Alpha - 1)) * math:exp(X * -0.5).

counta(Vs) ->
    Fvs = ?flatten_all(Vs),
    length(Fvs).

count(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, [ignore_strings, ignore_bools, ignore_dates, ignore_blanks]),
    length(Nums).

countblank(Vs) ->
    Flatvs = ?flatten_all(Vs),
    length([X || X <- Flatvs, muin_collect:is_blank(X)]).

critbinom([V1, V2, V3]) ->
    Trials = ?int(V1, ?default_rules),
    [Prob, Alpha] = ?numbers([V2, V3], ?default_rules),
    ?ensure(Trials >= 0, ?ERR_NUM),
    ?ensure(Prob >= 0 andalso Prob =< 1, ?ERR_NUM),
    ?ensure(Alpha >= 0 andalso Alpha =< 1, ?ERR_NUM),
    critbinom1(Trials, Prob, Alpha, 0).
critbinom1(Trials, Prob, Alpha, X) ->
    Val = binomdist1(X, Trials, Prob, true),
    ?COND(Val >= Alpha,
          X,
          critbinom1(Trials, Prob, Alpha, X + 1)).

devsq(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, ?default_rules),
    devsq1(Nums).
devsq1(Vals) ->
    moment(Vals, 2) * length(Vals).

expondist([V1, V2, V3]) ->
    [X, Lambda] = ?numbers([V1, V2], ?default_rules),
    Cumul = ?bool(V3, ?default_rules_bools),
    ?ensure(X >= 0, ?ERR_NUM),
    ?ensure(Lambda >= 0, ?ERR_NUM),
    expondist1(X, Lambda, Cumul).
expondist1(X, Lambda, true) ->
    1 - math:exp(-1 * X / Lambda);
expondist1(X, Lambda, false) ->
    math:exp(-1 * X / Lambda) / Lambda.

%% TODO:
forecast([_, _, _]) ->
    0.
%% forecast1(X, Kys, Kxs) ->
%%     {matrix, [B1, B0]} = linest1(Kys, Kxs),
%%     B1 * X + B0.

%% FIXME: Bins should NOT be sorted (see Excel).
%% FIXME: Current algorithm is O(n^2).
frequency([A, B]) when ?is_area(A) andalso ?is_area(B) ->
    {_, DataRows} = ?numbers(A, [cast_strings, cast_bools, ban_dates, cast_blanks]),
    {_, BinsRows} = ?numbers(B, [cast_strings, cast_bools, ban_dates, cast_blanks]),
    Data = sort(flatten(DataRows)),
    Bins = [hd(Data)-1] ++ sort(flatten(BinsRows)) ++ [last(Data)+1],

    Boundaries = hslists:init(zip(Bins, hslists:drop(Bins, 1) ++ [0])),
    R = reverse(foldl(fun({X, Y}, Acc) ->
                              Count = length(filter(fun(Z) -> Z > X andalso Z =< Y end, Data)),
                              [Count|Acc]
                      end,
                      [],
                      Boundaries)),
    stdfuns_math:transpose([{array, [R]}]);
frequency(_) ->
    ?ERR_VAL.

harmean(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, ?default_rules), % TODO: Ignore strs, bools, blanks.
    Any0s = any(fun(0) -> true; (_) -> false end,
                Nums),
    ?ensure(not(Any0s), ?ERR_NUM),
    harmean1(Nums, 0, 0).
harmean1([], Num, Acc) ->
    Num / Acc;
harmean1([Hd|Tl], Num, Acc) ->
    harmean1(Tl, Num+1, (1/Hd)+Acc).

gammadist([V1, V2, V3, V4]) ->
    [X, Alpha, Beta] = ?numbers([V1, V2, V3], ?default_rules),
    Cumul = ?bool(V4, ?default_rules_bools),
    ?ensure(X >= 0, ?ERR_NUM),
    ?ensure(Alpha > 0, ?ERR_NUM),
    ?ensure(Beta > 0, ?ERR_NUM),
    gammadist1(X, Alpha, Beta, Cumul).
gammadist1(X, Alpha, Beta, false) ->
    Top = math:pow(X, Alpha - 1) * math:exp(-1 * X / Beta),
    Top / (math:pow(Beta, Alpha) * stdfuns_math:fact1(round(Alpha)));
gammadist1(_X, _Alpha, _Beta, true) ->
    0. %% TODO:

%% TODO:
intercept([_, _]) ->
    0.
%% intercept1(Kys, Kxs) ->
%%     {matrix, [M, C]} = linest1(Kys, Kxs),
%%     C / M.

kurt(V1) ->
    Flatvs = ?flatten_all(V1),
    Nums = ?numbers(Flatvs, ?default_rules),
    ?ensure(length(Nums) > 3, ?ERR_DIV),
    kurt1(Nums).
kurt1(Nums) ->
    (moment(Nums, 4) / math:pow(moment(Nums, 2), 2)) - 3.

large([V1, V2]) ->
    Nums = ?flatten_all(V1),
    K = ?number(V2, ?default_rules),
    ?ensure(K > 0, ?ERR_NUM),
    ?ensure(length(Nums) >= K, ?ERR_NUM),
    large1(Nums, K).
large1(Nums, K) ->
    nth(K, reverse(sort(Nums))).

%% TODO:
linest(_) ->
    linest1(0, 0).
linest1(_, _) ->
    0.

max([V1]) ->
    Flatvs = ?flatten_all(V1),
    Nums = ?numbers(Flatvs, ?default_rules),
    ?COND(length(Nums) == 0, 0, lists:max(Nums)).

maxa([V1]) ->
    Flatvs = ?flatten_all(V1),
    Nums = ?numbers(Flatvs,
                    ?default_rules -- [cast_strings] ++ [cast_strings_zero]),
    ?COND(length(Nums) == 0, 0, lists:max(Nums)).

median([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    quartile1(Nums, 2).

min([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    ?COND(length(Nums) == 0, 0, lists:min(Nums)).

mina([V1]) ->
    Nums = ?numbers(?flatten_all(V1),
                    ?default_rules -- [cast_strings] ++ [cast_strings_zero]),
    ?COND(length(Nums) == 0, 0, lists:min(Nums)).

mode([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
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

%% TODO:
normsdist([_, _, _, _]) ->
    0.

%% TODO:
pearson([_, _]) ->
    0.
pearson1(_, _) ->
    0.

percentile([V1, V2]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    K = ?number(V2, ?default_rules),
    ?ensure(length(Nums) > 0, ?ERR_NUM),
    ?ensure((K >= 0) andalso (K =< 1), ?ERR_NUM),
    percentile1(Nums, K).
percentile1(Nums, K) ->
    L = map(fun(X) -> X / lists:sum(Nums) end,
            cumulate(Nums)),
    firstgte(L, K).

permut([V1, V2]) ->
    [N, K] = ?numbers([V1, V2], ?default_rules),
    ?ensure(N > 0, ?ERR_NUM),
    ?ensure(K >= 0, ?ERR_NUM),
    ?ensure(N >= K, ?ERR_NUM),
    permut1(trunc(N), trunc(K)).
permut1(N, K) ->
    stdfuns_math:fact1(N) div stdfuns_math:fact1(N - K).

poisson([V1, V2, V3]) ->
    X = ?int(V1, ?default_rules),
    Mean = ?number(V2, ?default_rules),
    Cumul = ?bool(V3, ?default_rules_bools),
    ?ensure(X >= 0, ?ERR_NUM),
    ?ensure(Mean > 0, ?ERR_NUM),
    poisson1(X, Mean, Cumul).
poisson1(X, Mean, false) ->
    noncumpoisson(X, Mean);
poisson1(X, Mean, true) ->
    cumpoisson(X, Mean, 0).
noncumpoisson(X, Mean) ->
    E = math:exp(1),
    math:pow(E, -Mean) * math:pow(Mean, X) / stdfuns_math:fact1(X).
%% cumpoisson(X, Mean) ->
%%     cumpoisson(X, Mean, 0).
cumpoisson(-1, _Mean, Acc) ->
    Acc;
cumpoisson(K, Mean, Acc) ->
    cumpoisson(K-1, Mean, Acc + noncumpoisson(K, Mean)).

quartile([V1, V2]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    Q = ?int(V2, ?default_rules),
    ?ensure(length(Nums) > 0, ?ERR_NUM),
    ?ensure((Q >= 0) and (Q =< 4), ?ERR_NUM),
    quartile1(Nums, Q).
quartile1(Nums, Q) ->
    nth(percentile1(Nums, Q * 0.25), Nums).

rank([V1, V2]) ->
    rank([V1, V2, 0]);
rank([V1, V2, V3]) ->
    Num = ?number(V1, ?default_rules),
    Nums = ?numbers(?flatten_all(V2), ?default_rules),
    Order = ?bool(V3, ?default_rules_bools),
    rank1(Num, Nums, Order).
rank1(N, Nums, true) ->
    firstgte(sort(Nums), N);
rank1(N, Nums, false) ->
    (length(Nums) + 1) - rank1(N, Nums, true).

skew(V1) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    ?ensure(length(Nums) >= 3, ?ERR_DIV),
    skew1(Nums).
skew1(Nums) ->
    moment(Nums, 3) / math:pow(moment(Nums, 2), 1.5).

small([V1, V2]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    K = ?number(V2, ?default_rules),
    ?ensure(K > 0, ?ERR_NUM),
    small1(Nums, K).
small1(Nums, K) ->
    nth(K, sort(Nums)).

standardize(Arg = [_, _, _]) ->
    [Num, Mean, Stdev] = ?numbers(Arg, ?default_rules),
    ?ensure(Stdev > 0, ?ERR_NUM),
    standardize1(Num, Mean, Stdev).
standardize1(Num, Mean, Stdev) ->
    (Num - Mean) / math:sqrt(Stdev).

stdev([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    stdev1(Nums).
stdev1(Nums) ->
    math:sqrt(devsq1(Nums) / (length(Nums) - 1)).

stdeva([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    stdev1(Nums).

stdevp([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    stdevp1(Nums).
stdevp1(Nums) ->
    math:sqrt(devsq1(Nums) / (length(Nums) - 1)).

stdevpa([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    stdevp1(Nums).

steyx([V1, V2]) ->
    Ys = ?numbers(?flatten_all(V1), ?default_rules),
    Xs = ?numbers(?flatten_all(V2), ?default_rules),
    steyx1(Ys, Xs).
steyx1(Ys, Xs) ->
    math:pow(pearson1(Ys, Xs), 2).

trimmean([V1, V2]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    Percent = ?number(V2, ?default_rules),
    ?ensure(Percent >= 0, ?ERR_NUM),
    ?ensure(Percent =< 1, ?ERR_NUM),
    trimmean1(Nums, Percent).
trimmean1(Nums, Percent) ->
    N = round((Percent / 100) * length(Nums)) div 2,
    average1(sublist(sort(Nums), N + 1, length(Nums) - 2 * N)).

var([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    var1(Nums).
var1(Nums) ->
    math:pow(stdev1(Nums), 2).

vara([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    var1(Nums).

varp([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    varp1(Nums).
varp1(Nums) ->
    math:pow(stdevp1(Nums), 2).

varpa([V1]) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    varp1(Nums).

weibull([V1, V2, V3, V4]) ->
    [X, Alpha, Beta] = ?numbers([V1, V2, V3], ?default_rules),
    Cumul = ?bool(V4, ?default_rules_bools),
    ?ensure(X >= 0, ?ERR_NUM),
    ?ensure(Alpha >= 0, ?ERR_NUM),
    ?ensure(Beta >= 0, ?ERR_NUM),
    weibull1(X, Alpha, Beta, Cumul).
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
