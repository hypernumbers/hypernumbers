%%% <hasan@hypernumbers.com>

%%% @doc Statistical functions
%%% IMPORTANT NOTES:
%%%
%%% In CHIDIST(X, DegreesOfFreedom) in Excel DegreesOfFreedom is capped at
%%% 10^10. We're doing that too (for now anyway).
%%%
%%% @private

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
         countif/1,
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
         geomean/1,
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
         standardise/1,
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

-import(muin_collect, [col/2, col/3, col/4]).

-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).
-define(default_rules_bools, [cast_numbers, cast_strings, cast_blanks, cast_dates]).

avedev(Args) ->
    col(Args,
        [eval_funs, {cast, str, num, ?ERRVAL_VAL}, fetch, flatten,
         {ignore, blank}, {ignore, array}, {ignore, str}, {cast, num}],
        [return_errors, {all, fun is_number/1}],
        fun avedev1/1).
avedev1([]) ->
    ?ERRVAL_NUM;
avedev1(Nums) ->
    Avg = average(Nums),
    Deviation = foldl(fun(X, Acc) -> Acc + erlang:abs(Avg - X) end, 0, Nums),
    Deviation / length(Nums).

average(Args) ->
    col(Args,
        [eval_funs, {cast, str, num, ?ERRVAL_VAL}, {cast, num}, fetch,
         flatten, {ignore, blank}, {ignore, bool}, {ignore, str}],
        [return_errors, {all, fun is_number/1}],
        fun average1/1).

average1([]) ->
    ?ERRVAL_DIV;
average1(Nums) ->
    lists:sum(Nums)/length(Nums).

%% TODO: errvals -> 0s in args.
averagea(Args) ->    
    col(Args,
        [eval_funs, {cast, str, num, ?ERRVAL_VAL}, {cast, num}, fetch,
         flatten, {ignore, blank}, {cast, bool, num}, {conv, str, 0}],
        [return_errors, {all, fun is_number/1}],
        fun average1/1).

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
    io:format("In binomddist1 (false) Ns is ~p Nt is ~p Ps is ~p~n",
              [Ns, Nt, Ps]), 
    stdfuns_math:combin([Ps, Nt]) * math:pow(Ps, Ns) * math:pow((1 - Ps),
                                                                (Nt - Ns));
%% TODO: Rewrite to tail-recursive.
binomdist1(Ns, Nt, Ps, true) ->
    io:format("In binomddist1 (true) Ns is ~p Nt is ~p Ps is ~p~n",
              [Ns, Nt, Ps]), 
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
    length(filter(fun(X) -> not(muin_collect:is_blank(X)) end, Fvs)). % Discard blanks.

count(Vs) ->
    Vals = col(Vs, [flatten_as_str, ignore_blanks, cast_num,
                    ignore_errors, ignore_strings]),
    length(Vals).

countblank([Err]) when ?is_errval(Err) -> Err;
countblank(Vs) ->
    Flatvs = ?flatten_all(Vs),
    length([X || X <- Flatvs, muin_collect:is_blank(X)]).


countif([A, CritSpec]) ->
    ?ensure(?is_area(A), ?ERR_VAL),
    ?ensure(?is_string(CritSpec), ?ERR_VAL),
    case odf_criteria:create(CritSpec) of
        {error, _Reason} ->
            0;
        Fun              ->
            case filter(Fun, area_util:to_list(A)) of
                [] -> 0;
                L  -> count(L)
            end
    end.    

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
    A = col([V1, V2],
            [eval_funs, fetch, area_first, {cast, num}],
            [return_errors, {all, fun is_number/1}]),
    B = col([V3],
            [eval_funs, fetch, area_first, {cast, bool}],
            [return_errors, {all, fun is_boolean/1}]),
    
    muin_util:apply([A, B], fun expondist1/2).

expondist1([X, Lambda], _) when X < 0; Lambda =< 0 ->
    ?ERRVAL_NUM;
expondist1([X, Lambda], [true]) ->
    1 - math:exp(-Lambda * X);
expondist1([X, Lambda], [false]) ->
    Lambda * math:exp( -Lambda * X).

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

geomean(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, [ignore_strings, ignore_bools,
                             ignore_dates, ignore_blanks]),
    AnyZeros = any(fun(X) -> X == 0 end, Nums),
    ?ensure(not(AnyZeros), ?ERR_NUM),
    math:pow(stdfuns_math:product(Nums), 1/erlang:length(Nums)).

harmean(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers(Flatvs, [ignore_strings, ignore_bools, ignore_dates, ignore_blanks]),
    AnyZeros = any(fun(X) -> X == 0 end,
                   Nums),
    ?ensure(not(AnyZeros), ?ERR_NUM),
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

%% the casting for this is all over the place
%% doens't cast left to right, blah-blah
large([V1, V2]) ->
    Arr = col([V1],
              [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL},
               fetch, flatten, 
               {ignore, str}, {ignore, bool}, {ignore, blank}],
              [return_errors, {all, fun is_number/1}]),
    
    N = col([V2],[eval_funs, fetch, area_first, {cast, num}],
            [return_errors, {all, fun is_number/1}]),
    
    muin_util:apply([Arr, N], fun large_/2).    

large_(Arr, [N]) when N < 1; N > length(Arr) ->
    ?ERRVAL_NUM;
large_(Nums, [N]) ->
    nth(erlang:round(N), reverse(sort(Nums))).

%% TODO:
linest(_) ->
    linest1(0, 0).
linest1(_, _) ->
    0.

max(Args) ->
    col(Args,
        [eval_funs, flatten, {cast, str, num, ?ERRVAL_VAL},
         {cast, bool, num}, fetch_name, fetch_ref,
         flatten, {ignore, blank}, {ignore, str}, {ignore, bool}],
        [return_errors, {all, fun is_number/1}],
        fun max_/1).

max_(Nums) ->
    ?COND(length(Nums) == 0, 0, lists:max(Nums)).

min(Args) ->
    col(Args,
        [eval_funs, flatten, {cast, str, num, ?ERRVAL_VAL},
         {cast, bool, num}, fetch_name, fetch_ref,
         flatten, {ignore, blank}, {ignore, str}, {ignore, bool}],
        [return_errors, {all, fun is_number/1}],
        fun min_/1).

min_(Nums) ->
    ?COND(length(Nums) == 0, 0, lists:min(Nums)).

mina(Args) ->
    col(Args,
        [eval_funs, flatten, {cast, str, num, ?ERRVAL_VAL},
         {cast, bool, num}, fetch_name, fetch_ref,
         flatten, {ignore, blank}, {ignore, str}, {ignore, bool}],
        [return_errors, {all, fun is_number/1}],
        fun mina_/1).

mina_(Nums) ->
    ?COND(length(Nums) == 0, 0, lists:min(Nums)).

maxa(Args) ->
    col(Args,
        [eval_funs, flatten, {cast, str, num, ?ERRVAL_VAL},
         {cast, bool, num}, fetch_name, fetch_ref,
         flatten, {ignore, blank}, {ignore, str}, {cast, bool, num}],
        [return_errors, {all, fun is_number/1}],
        fun maxa_/1).

maxa_(Nums) ->
    ?COND(length(Nums) == 0, 0, lists:max(Nums)).

median(Args) ->
    col(Args,
        [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL},
         fetch, flatten, {ignore, str}, {ignore, bool}, {ignore, blank}],
        [return_errors, {all, fun is_number/1}],
        fun median_/1).

median_([]) ->
    ?ERRVAL_NUM;
median_(Args) ->
    Nums = lists:sort(Args),
    case count(Args) of
        1 -> hd(Args);
        X when (X rem 2) == 1 -> lists:nth(erlang:trunc(X / 2), Nums);
        X when (X rem 2) == 0 ->
            C = erlang:trunc(X / 2),
            (lists:nth(C, Nums) + lists:nth(C+1, Nums)) / 2
    end.

mode(V1) ->
    Nums = ?numbers(?flatten_all(V1), ?default_rules),
    mode1(Nums).

mode1(Nums) ->
    Maptbl = mode1(Nums, []),
    {N,C,Uniq} = foldl(fun({V, Cnt, _}, Acc = {_, Maxcnt,_}) ->
                               {NewV,NewCnt,_}=?COND(Cnt >= Maxcnt,{V,Cnt,false},Acc),
                               NewUniq=?COND(Cnt == Maxcnt,false,true),
                               {NewV,NewCnt,NewUniq}
                         end,
                         hd(Maptbl), tl(Maptbl)),
    ?COND(C == 1,
          ?ERR_NA, % no duplicates
          ?COND(Uniq == false,
                ?ERR_VAL, % must have one median (ie 1,2,2,3,3 is not valid)
                N)).

mode1([H|T], Maptbl) ->
    case keysearch(H, 1, Maptbl) of
        % update count
        {value, {H, Cnt,true}} ->
            mode1(T, lists:keyreplace(H, 1, Maptbl,{H, Cnt + 1,true}));
        % create entry
        % the third parameter true is used in the foldl of mode1/1
        false ->
            mode1(T, lists:merge([Maptbl,[{H,1,true}]]))
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

skew(Arg) ->
    col(Arg,
        [eval_funs, fetch, area_first, {cast, num}],
        [return_errors, {all, fun is_number/1}],
        fun skew1/1).

    %% Nums = ?numbers(?flatten_all(V1), ?default_rules),
    %% ?ensure(length(Nums) >= 3, ?ERR_DIV),
    %% skew1(Nums).
skew1(Nums) when length(Nums) < 3 ->
    ?ERRVAL_DIV;
skew1(Nums) ->
    moment(Nums, 3) / math:pow(moment(Nums, 2), 3).

%% the casting for this is all over the place
%% doens't cast left to right, blah-blah
small([V1, V2]) ->
    Arr = col([V1],
              [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL},
               fetch, flatten, 
               {ignore, str}, {ignore, bool}, {ignore, blank}],
              [return_errors, {all, fun is_number/1}]),
    
    N = col([V2],[eval_funs, fetch, area_first, {cast, num}],
            [return_errors, {all, fun is_number/1}]),
    
    muin_util:apply([Arr, N], fun small_/2).    


small_(Arr, [N]) when N < 1; N > length(Arr) ->
    ?ERRVAL_NUM;
small_(Nums, [N]) ->
    nth(erlang:round(N), sort(Nums)).

% Yup, Excel silently recognises both spelling variants
standardise(L) -> standardize(L).

standardize(Arg = [_, _, _]) ->
    col(Arg,
        [eval_funs, fetch, area_first, {cast, num}],
        [return_errors, {all, fun is_number/1}],
        fun standardize1/1).

standardize1([_Num, _Mean, Stdev]) when Stdev =< 0 ->
    ?ERRVAL_NUM;
standardize1([Num, Mean, Stdev]) ->
    (Num - Mean) / Stdev.

stdev(V1) ->
    col(V1,
        [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL}, fetch, flatten,
         {ignore, str}, {ignore, bool}, {ignore, blank}],
        [return_errors, {all, fun is_number/1}],
        fun stdev1/1).

stdev1(Nums) when length(Nums) < 2 ->
    ?ERRVAL_DIV;
stdev1(Nums) ->
    math:sqrt(devsq1(Nums) / (length(Nums) - 1)).

stdeva(V1) ->
    col(V1,
        [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL}, fetch, flatten,
         {conv, str, 0}, {cast, bool, num}, {ignore, blank}],
        [return_errors, {all, fun is_number/1}],
        fun stdev/1).    

stdevp(V1) ->
    col(V1,
        [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL}, fetch, flatten,
         {ignore, str}, {ignore, bool}, {ignore, blank}],
        [return_errors, {all, fun is_number/1}],
        fun stdevp1/1).

stdevp1(Nums) when length(Nums) == 1 ->
    0;
stdevp1(Nums) when length(Nums) < 2 ->
    ?ERRVAL_DIV;
stdevp1(Nums) ->
    math:sqrt(devsq1(Nums) / length(Nums)).

stdevpa(V1) ->
    col(V1,
        [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL}, fetch, flatten,
         {conv, str, 0}, {cast, bool, num}, {ignore, blank}],
        [return_errors, {all, fun is_number/1}],
        fun stdevp/1).    

steyx([V1, V2]) ->
    Ys = ?numbers(?flatten_all(V1), ?default_rules),
    Xs = ?numbers(?flatten_all(V2), ?default_rules),
    steyx1(Ys, Xs).
steyx1(Ys, Xs) -> 
    math:pow(pearson1(Ys, Xs), 2).

trimmean([V1, V2]) ->
    Nums = col([V1],
               [eval_funs, {cast, num}, fetch, flatten,
                {ignore, str}, {ignore, bool}, {ignore, blank}],
               [return_errors, {all, fun is_number/1}]),    
    Perc = col([V2],
                [eval_funs, fetch, flatten, {cast, num}],
               [return_errors, {all, fun is_number/1}]),
    muin_util:apply([Nums, Perc], fun trimmean_/2).

trimmean_(Nums, [Perc]) when Perc >= 0, Perc < 1, length(Nums) > 0 ->
    trimmean1(lists:reverse(Nums), Perc);
trimmean_(_Nums, [_Perc]) ->
    ?ERRVAL_NUM.

trimmean1(Nums, Percent) ->
    N = erlang:trunc(((Percent * length(Nums)) / 2)),
    average1(sublist(sort(Nums), N + 1, length(Nums) - 2 * N)).

var(V1) ->
    col(V1, [eval_funs, {cast, str, num}, {cast, bool, num},
             {conv, str, ?ERRVAL_VAL}, fetch, flatten,
             {ignore, bool}, {ignore, blank}, {ignore, str}],
        [return_errors, {all, fun is_number/1}],
        fun var1/1).

vara(V1) ->
    col(V1, [eval_funs, {cast, str, num}, fetch, flatten, {cast, bool, num},
             {ignore, blank}, {conv, str, 0}],
        [return_errors, {all, fun is_number/1}],
        fun var1/1).

varp(V1) ->
    col(V1, [eval_funs, {cast, str, num}, {cast, bool, num}, fetch, flatten,
             {ignore, bool}, {ignore, blank}, {ignore, str}],
        [return_errors, {all, fun is_number/1}],
        fun varp1/1).

varpa(V1) ->
    col(V1, [eval_funs, {cast, str, num}, fetch, flatten, {cast, bool, num},
             {ignore, blank}, {conv, str, 0}],
        [return_errors, {all, fun is_number/1}],
        fun varp1/1).

varp1([]) ->
    ?ERRVAL_VAL;
varp1(Nums) ->
    (lists:sum([ X * X || X <- Nums]) / erlang:length(Nums))
      - math:pow(lists:sum(Nums) / erlang:length(Nums), 2).

var1(Nums) ->
    case stdev1(Nums) of
        Err when ?is_errval(Err) -> Err;
        Else                     -> math:pow(Else, 2)
    end.


weibull([V1, V2, V3, V4]) ->
    XArgs = col([V1, V2, V3],
                [eval_funs, area_first, fetch, {cast, bool, num}],
                [return_errors, {all, fun is_number/1}]),    
    XBool = col([V4],
                [eval_funs, area_first, fetch, {cast, bool}],
                [return_errors, {all, fun is_boolean/1}]),
    muin_util:apply([XArgs, XBool], fun weibull_/2).

weibull_([X, Alpha, Beta], [Cumul])
  when X >= 0, Alpha > 0, Beta > 0 ->
    weibull1(X, Alpha, Beta, Cumul);
weibull_(_X1, _X2) ->
    ?ERRVAL_NUM.

% Args reversed, dunno why
weibull1(X, Beta, Alpha, true) ->
    1 - math:exp(-math:pow(X/Alpha, Beta));
weibull1(X, Beta, Alpha, false) ->
    (Beta / Alpha)
        * math:pow(X/Alpha, Beta-1)
        * math:exp(-math:pow(X/Alpha, Beta)).


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
