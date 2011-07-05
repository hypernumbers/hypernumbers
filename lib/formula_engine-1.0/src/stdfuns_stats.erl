%%% <hasan@hypernumbers.com>

%%% @doc Statistical functions
%%% IMPORTANT NOTES:
%%%
%%% In CHIDIST(X, DegreesOfFreedom) in Excel DegreesOfFreedom is capped at
%%% 10^10. We're doing that too (for now anyway).
%%%
%%% @private

-module(stdfuns_stats).

-include("typechecks.hrl").
-include("muin_proc_dict.hrl").

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
         countz/1,
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
         %%pearson/1,
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
    Deviation = lists:foldl(fun(X, Acc) -> Acc + erlang:abs(Avg - X) end, 0, Nums),
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
    Succprob = muin_col_DEPR:collect_number(V3, ?default_rules),
    Cumul = ?bool(V4, ?default_rules_bools),
    muin_checks:ensure(Succn =< Trials, ?ERRVAL_NUM),
    muin_checks:ensure(Succn >= 0, ?ERRVAL_NUM),
    muin_checks:ensure(Succprob >= 0, ?ERRVAL_NUM),
    muin_checks:ensure(Succprob =< 1, ?ERRVAL_NUM),
    binomdist1(Succn, Trials, trunc(Succprob * 100), Cumul).
binomdist1(Ns, Nt, Ps, false) ->
    stdfuns_math:combin([Ps, Nt]) * math:pow(Ps, Ns) * math:pow((1 - Ps),
                                                                (Nt - Ns));
%% TODO: Rewrite to tail-recursive.
binomdist1(Ns, Nt, Ps, true) ->
    binomdist1(Ns, Nt, Ps, false) + binomdist1(Ns - 1, Nt, Ps, true).

chidist([V1, V2]) ->
    col([V1, V2],
        [eval_funs, fetch, area_first, {conv, blank, ?ERRVAL_NUM}, {cast, num}],
        [return_errors, {all, fun is_number/1}],
        fun chidist_/1).

chidist_([X, _Degfree]) when X < 0 ->
    ?ERRVAL_NUM;
chidist_([_X, Degfree]) when Degfree < 1, Degfree > 1.0e+10 ->
    ?ERRVAL_NUM;

chidist_([X, Degfree]) when is_float(Degfree) ->
    chidist_([X, erlang:trunc(Degfree)]);
chidist_([X, Degfree]) ->
    Alpha = Degfree / 2, % 2 is beta
    Chi = 1/(math:pow(Alpha, 2) * stdfuns_math:fact1(erlang:trunc(Alpha))),
    math:pow(Chi, (Alpha - 1)) * math:exp(X * -0.5).

countz(List) ->
    put(recompile, true),
    Strs = typechecks:std_strs(List),
    Zs = countz_(Strs, []),
    muin_collect:col(Zs, [eval_funs, {cast, str, num, ?ERRVAL_VAL},
                          {cast, bool, num}, fetch, fetch_z_all, flatten,
                          {ignore, blank}, {ignore, str},
                          {ignore, bool}, {ignore, error}],
                     [{all, fun is_number/1}],
                     fun length/1).

countz_([], Acc) -> Acc;
countz_([H | T], Acc) ->
    NewAcc = case muin:parse(H, {?mx, ?my}) of
              {ok, Ast} ->
                  case muin:external_eval(Ast) of
                      X when ?is_cellref(X);
                             ?is_rangeref(X);
                             ?is_zcellref(X) -> X;
                      _Else                  -> ?ERRVAL_REF
                  end;
              {error, syntax_error} -> ?ERRVAL_REF
          end,
    countz_(T, [NewAcc | Acc]).

counta(Vs) ->
    Vals = col(Vs, [eval_funs, fetch, flatten, {ignore, blank}]),
    length(Vals).

count(Vs) ->
    Vals = col(Vs, [eval_funs, fetch, fetch_z_all, flatten_as_str, {ignore, blank},
                    {cast, num}, {ignore, error}, {ignore, str}]),
    length(Vals).

countblank(Vs) ->
    col(Vs, [eval_funs, fetch, flatten], [], fun countblank_/1).

countblank_([Err]) when ?is_errval(Err) -> Err;
countblank_(Vals) ->
    length([X || X <- Vals, muin_collect:is_blank(X)]).

countif([A, Cr]) ->

    Vals   = col([A],  [eval_funs, fetch, flatten, {ignore, blank}]),
    [Crit] = col([Cr], [eval_funs, fetch, flatten]),

    case odf_criteria:create(Crit) of
        {error, _Reason} -> 0;
        Fun              -> length(lists:filter(Fun, Vals))
    end.

critbinom([V1, V2, V3]) ->
    Trials = ?int(V1, ?default_rules),
    [Prob, Alpha] = muin_col_DEPR:collect_numbers([V2, V3], ?default_rules),
    muin_checks:ensure(Trials >= 0, ?ERRVAL_NUM),
    muin_checks:ensure(Prob >= 0 andalso Prob =< 1, ?ERRVAL_NUM),
    muin_checks:ensure(Alpha >= 0 andalso Alpha =< 1, ?ERRVAL_NUM),
    critbinom1(Trials, Prob, Alpha, 0).

critbinom1(Trials, Prob, Alpha, X) ->
    Val = binomdist1(X, Trials, Prob, true),
    case (Val >= Alpha) of
        true  -> X;
        false -> critbinom1(Trials, Prob, Alpha, X + 1)
    end.

devsq(Vs) ->
    Flatvs = muin_col_DEPR:flatten_areas(Vs),
    Nums = muin_col_DEPR:collect_numbers(Flatvs, ?default_rules),
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
    {_, DataRows} = muin_col_DEPR:collect_numbers(A, [cast_strings, cast_bools, ban_dates, cast_blanks]),
    {_, BinsRows} = muin_col_DEPR:collect_numbers(B, [cast_strings, cast_bools, ban_dates, cast_blanks]),
    Data = lists:sort(lists:flatten(DataRows)),
    Bins = [hd(Data)-1] ++ lists:sort(lists:flatten(BinsRows)) ++ [lists:last(Data)+1],

    Boundaries = hslists:init(lists:zip(Bins, hslists:drop(Bins, 1) ++ [0])),
    R = lists:reverse(lists:foldl(fun({X, Y}, Acc) ->
                              Count = length(lists:filter(fun(Z) -> Z > X andalso Z =< Y end, Data)),
                              [Count|Acc]
                      end,
                      [],
                      Boundaries)),
    stdfuns_math:transpose([{array, [R]}]);
frequency(_) ->
    ?ERRVAL_VAL.

geomean(Vs) ->
    Flatvs = muin_col_DEPR:flatten_areas(Vs),
    Nums = muin_col_DEPR:collect_numbers(Flatvs, [ignore_strings, ignore_bools,
                             ignore_dates, ignore_blanks]),
    AnyZeros = lists:any(fun(X) -> X == 0 end, Nums),
    muin_checks:ensure(not(AnyZeros), ?ERRVAL_NUM),
    math:pow(stdfuns_math:product(Nums), 1/erlang:length(Nums)).

harmean(Vs) ->
    Flatvs = muin_col_DEPR:flatten_areas(Vs),
    Nums = muin_col_DEPR:collect_numbers(Flatvs, [ignore_strings, ignore_bools, ignore_dates, ignore_blanks]),
    AnyZeros = lists:any(fun(X) -> X == 0 end,
                   Nums),
    muin_checks:ensure(not(AnyZeros), ?ERRVAL_NUM),
    harmean1(Nums, 0, 0).
harmean1([], Num, Acc) ->
    Num / Acc;
harmean1([Hd|Tl], Num, Acc) ->
    harmean1(Tl, Num+1, (1/Hd)+Acc).

gammadist([V1, V2, V3, V4]) ->
    [X, Alpha, Beta] = muin_col_DEPR:collect_numbers([V1, V2, V3], ?default_rules),
    Cumul = ?bool(V4, ?default_rules_bools),
    muin_checks:ensure(X >= 0, ?ERRVAL_NUM),
    muin_checks:ensure(Alpha > 0, ?ERRVAL_NUM),
    muin_checks:ensure(Beta > 0, ?ERRVAL_NUM),
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
    Flatvs = muin_col_DEPR:flatten_areas(V1),
    Nums = muin_col_DEPR:collect_numbers(Flatvs, ?default_rules),
    muin_checks:ensure(length(Nums) > 3, ?ERRVAL_DIV),
    kurt1(Nums).
kurt1(Nums) ->
    N = length(Nums),
    S = stdev(Nums),
    Xm = lists:sum(Nums) / N,
    Z = lists:sum([ math:pow((X - Xm) / S, 4) || X<-Nums ]),

    (((N * (N+1)) / ((N-1)*(N-2)*(N-3))) * Z) -
        ((3*math:pow(N-1, 2)) / ((N-2)*(N-3))).

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
    lists:nth(erlang:round(N), lists:reverse(lists:sort(Nums))).

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
        [return_errors],
        fun max_/1).

max_(Nums) ->
    case (length(Nums) == 0) of
        true  -> 0;
        false -> lists:max(Nums)
    end.

min(Args) ->
    col(Args,
        [eval_funs, flatten, {cast, str, num, ?ERRVAL_VAL},
         {cast, bool, num}, fetch_name, fetch_ref,
         flatten, {ignore, blank}, {ignore, str}, {ignore, bool}],
        [return_errors],
        fun min_/1).

min_(Nums) ->
    case (length(Nums) == 0) of
        true  -> 0;
        false -> lists:min(Nums)
    end.

mina(Args) ->
    col(Args,
        [eval_funs, flatten, {cast, str, num, ?ERRVAL_VAL},
         {cast, bool, num}, fetch_name, fetch_ref,
         flatten, {ignore, blank}, {ignore, str}, {ignore, bool}],
        [return_errors, {all, fun is_number/1}],
        fun mina_/1).

mina_(Nums) ->
    case (length(Nums) == 0) of
        true  -> 0;
        false -> lists:min(Nums)
    end.

maxa(Args) ->
    col(Args,
        [eval_funs, flatten, {cast, str, num, ?ERRVAL_VAL},
         {cast, bool, num}, fetch_name, fetch_ref,
         flatten, {ignore, blank}, {ignore, str}, {cast, bool, num}],
        [return_errors, {all, fun is_number/1}],
        fun maxa_/1).

maxa_(Nums) ->
    case (length(Nums) == 0) of
        true  -> 0;
        false -> lists:max(Nums)
    end.

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

mode(Args) ->
    col(Args,
        [eval_funs, fetch, flatten, {ignore, str}, {ignore, bool},
         {ignore, blank}], [return_errors, {all, fun is_number/1}],
        fun mode_/1).

mode_([]) ->
    ?ERRVAL_VAL;
mode_(Args) ->

    F = fun({Num1, Freq}, {Num2, Freq})  -> Num1  > Num2;
           ({_,    Freq1}, {_,   Freq2}) -> Freq1 > Freq2
        end,

    Tree = mode_(Args, dh_tree:new()),
    case lists:sort(F, dict:to_list(Tree)) of
        [{_X, 1} | _ ] -> ?ERRVAL_NA;
        [{X, _}  | _ ] -> X
    end.

mode_([], Tree) ->
    Tree;
mode_([H|T], Tree) ->
    F = fun(undefined) -> 1; (X) -> X+1 end,
    mode_(T, dh_tree:update([H], Tree, F)).

%% TODO:
normsdist([_, _, _, _]) ->
    0.

percentile([V1, V2]) ->
    Nums = muin_col_DEPR:collect_numbers(muin_col_DEPR:flatten_areas(V1), ?default_rules),
    K = muin_col_DEPR:collect_number(V2, ?default_rules),
    muin_checks:ensure(length(Nums) > 0, ?ERRVAL_NUM),
    muin_checks:ensure((K >= 0) andalso (K =< 1), ?ERRVAL_NUM),
    percentile1(Nums, K).
percentile1(Nums, K) ->
    L = lists:map(fun(X) -> X / lists:sum(Nums) end,
            cumulate(Nums)),
    firstgte(L, K).

permut([V1, V2]) ->
    [N, K] = muin_col_DEPR:collect_numbers([V1, V2], ?default_rules),
    muin_checks:ensure(N > 0, ?ERRVAL_NUM),
    muin_checks:ensure(K >= 0, ?ERRVAL_NUM),
    muin_checks:ensure(N >= K, ?ERRVAL_NUM),
    permut1(trunc(N), trunc(K)).
permut1(N, K) ->
    stdfuns_math:fact1(N) div stdfuns_math:fact1(N - K).

poisson([V1, V2, V3]) ->
    X = ?int(V1, ?default_rules),
    Mean = muin_col_DEPR:collect_number(V2, ?default_rules),
    Cumul = ?bool(V3, ?default_rules_bools),
    muin_checks:ensure(X >= 0, ?ERRVAL_NUM),
    muin_checks:ensure(Mean > 0, ?ERRVAL_NUM),
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
    Nums = muin_col_DEPR:collect_numbers(muin_col_DEPR:flatten_areas(V1), ?default_rules),
    Q = ?int(V2, ?default_rules),
    muin_checks:ensure(length(Nums) > 0, ?ERRVAL_NUM),
    muin_checks:ensure((Q >= 0) and (Q =< 4), ?ERRVAL_NUM),
    quartile1(Nums, Q).
quartile1(Nums, Q) ->
    lists:nth(percentile1(Nums, Q * 0.25), Nums).

rank([V1, V2]) ->
    rank([V1, V2, 1]);
rank([V1, V2, V3]) ->
    Num = muin_col_DEPR:collect_number(V1, ?default_rules),
    Nums = muin_col_DEPR:collect_numbers(muin_col_DEPR:flatten_areas(V2),
                                         ?default_rules),
    Order = ?bool(V3, ?default_rules_bools),
    rank1(Num, Nums, Order).
rank1(N, Nums, true) ->
    Ranks = generate_rank(lists:sort(Nums), 0, 0, 0, []),
    case lists:keyfind(N, 1, Ranks) of
        false     -> ?ERRVAL_NA;
        {N, Rank} ->
            Rank
    end;
rank1(N, Nums, false) ->
    (length(Nums) + 1) - rank1(N, Nums, true).

generate_rank([], _, _, _, Acc) ->
    lists:reverse(Acc);

generate_rank([H|T], CurrentRank, Count, LastNumRanked, Acc)
  when H =:= LastNumRanked ->
    generate_rank(T, CurrentRank, Count+1, H, Acc);
generate_rank([H|T], _CurrentRank, Count, _LastNumRanked, Acc) ->
    generate_rank(T, Count+1, Count+1, H, [{H, Count+1} | Acc]).

skew(Arg) ->
    col(Arg,
        [eval_funs, {cast, num}, {conv, str, ?ERRVAL_VAL}, fetch, flatten,
         {ignore, str}, {ignore, blank}, {ignore, bool}],
        [return_errors, {all, fun is_number/1}],
        fun skew1/1).

skew1(Nums) when length(Nums) < 3 ->
    ?ERRVAL_DIV;
skew1(Nums) ->
    Mean = lists:sum(Nums) / length(Nums),
    Stdev = stdev(Nums),
    N = length(Nums),
    case Stdev of
        X when X == 0, X == 0.0 -> ?ERRVAL_DIV;
        _ -> lists:sum([ math:pow((X - Mean) / Stdev, 3) || X<-Nums])
                 * (N / ((N - 1)*(N-2)))
    end.

small([V1, V2]) ->
    Arr = col([V1],
              [eval_funs, {cast, num}, {convflat, str, ?ERRVAL_VAL}, fetch,
               flatten,  {ignore, str}, {ignore, bool}, {ignore, blank}],
              [return_errors, {all, fun is_number/1}]),

    N = col([V2],[eval_funs, fetch, area_first, {cast, num}],
            [return_errors, {all, fun is_number/1}]),

    muin_util:apply([Arr, N], fun small_/2).


small_(Arr, [N]) when N < 1; N > length(Arr) ->
    ?ERRVAL_NUM;
small_(Nums, [N]) ->
    lists:nth(erlang:round(N), lists:sort(Nums)).

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
    One = col([V1],
              [eval_funs, {cast, num},
               fetch, flatten], [return_errors]),

    Two = col([V2],
              [eval_funs, {cast, num},
               fetch, flatten], [return_errors]),

    muin_util:apply([One, Two], fun steyx_pass_/2).

steyx_pass_(Xs, Ys) when length(Ys) =/= length(Xs) ->
    ?ERRVAL_NA;
steyx_pass_(Xs, Ys) ->
    NVals = [ {Y,X} || {X, Y} <- lists:zip(Xs, Ys),
                       is_number(X), is_number(Y) ],
    case {length(NVals), length(Xs)} of
        {0, _O}           -> ?ERRVAL_VAL;
        {_, O} when O < 3 -> ?ERRVAL_DIV;
        _                 -> steyx_(NVals)
    end.

steyx_(Vals) ->
    N = length(Vals),
    Ym = lists:sum([Y||{_X,Y}<-Vals]) / N,
    Xm = lists:sum([X||{X,_Y}<-Vals]) / N,
    Y1 = lists:sum([ math:pow((Y-Ym), 2) || {_,Y}<-Vals ]),
    X1 = lists:sum([ math:pow((X-Xm), 2) || {X,_}<-Vals ]),
    XY1 = lists:sum([ (X-Xm)*(Y-Ym) || {X, Y} <- Vals ]),
    XY2 = math:pow(XY1, 2),

    case X1 of
        X when X == 0, X==0.0 -> ?ERRVAL_DIV;
        _ -> math:sqrt( (1/(N-2)) * (Y1 - (XY2 / X1)))
    end.

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
    average1(lists:sublist(lists:sort(Nums), N + 1, length(Nums) - 2 * N)).

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
firstgte([H|T], N, C) when H == N ->
    firstgte(T, N, C);
firstgte([H|T], N, C) when H < N ->
    firstgte(T, N, C + 1);
firstgte([H|_T], N, C) when H >= N ->
    C;
firstgte([], _, _) ->
    0.

%% Returns list in element at position n equals the sum of elements 1 to n in
%% the original list.
cumulate([Hd | Tl]) ->
    lists:reverse(lists:foldl(fun(X, [H | T]) ->
                          [X + H, H | T]
                  end,
                  [Hd], Tl)).

moment(Vals, M) ->
    Avg = average1(Vals),
    lists:foldl(fun(X, Acc) -> Acc + math:pow((Avg - X), M) end,
                0, Vals) / length(Vals).
