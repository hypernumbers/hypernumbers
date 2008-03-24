%%% Stats functions fosho.
%%% <hasan@hypernumbers.com>

%%% IMPORTANT NOTES:
%%%
%%% In CHIDIST(X, DegreesOfFreedom) in Excel DegreesOfFreedom is capped at
%%% 10^10. We're doing that too (for now anyway).
%%%
%%% 

-module(stdfuns_stats).

-include("handy_macros.hrl").

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
         
         exp/1,
         expondist/1

        ]).

avedev(Vals) ->
    Flatvals = flatten(Vals),
    ?ensure_no_errvals(Flatvals),
    Nums = [X || X <- Flatvals, is_number(X)],
    ?ensure_nonzero(length(Nums)),
    avedev1(Nums).
avedev1(Nums) ->
    Avg = average1(Nums),
    Deviation = foldl(fun(X, Acc) ->
                              Acc + erlang:abs(Avg - X)
                      end,
                      0,
                      Nums),
    Deviation / length(Nums).

average(Vals) ->
    Flatvals = flatten(Vals),
    ?ensure_no_errvals(Flatvals),
    Nums = [X || X <- Vals, is_number(X)],
    ?ensure_nonzero(length(Nums)),
    average1(Nums).
average1(Nums) ->
    lists:sum(Nums) / length(Nums).

averagea(Vals) ->
    Flatvals = flatten(Vals),
    ?ensure_no_errvals(Flatvals),
    MaybeNums = map(fun tonum/1, Flatvals),     % Can contain {error, value}
                                                % tuples from tonum.
    Nums = foldl(fun({error, value}, Acc) ->    % Convert them to 0s.
                         Acc ++ [0];
                    (X, Acc) ->
                         Acc ++ [X]
                 end,
                 [],
                 MaybeNums),
    ?ensure_nonzero(Nums),
    average1(Nums).

binomdist([Succn_, Trials_, Succprob, Cumul]) ->
    ?ensure_numbers([Succn_, Trials, Succprob]),
    Succn = erlang:trunc(Succn_),
    Trials = erlang:trunc(Trials_),
    ?ensure(Succn =< Trials, ?ERR_NUM),
    ?ensure_non_negative_ex(Succn, ?ERR_NUM),
    ?ensure_non_negative_ex(Succprob),
    ?ensure(Succprob =< 1, ?ERR_NUM),
    ?ensure(is_boolean(Cumul), ?ERR_VAL),
    binomdist1(Succn, Trials, Succprob, Cumul).
binomdist1(Ns, Nt, Ps, false) ->
    combin(Nt, Ps) * power(Ps, Ns) * power((1 - Ps), (Nt - Ns));
%% TODO: Rewrite to tail-recursive.
binomdist1(Ns, Nt, Ps, true) ->
    binomdist(Ns, Nt, Ps, false) + binomdist(Ns - 1, Nt, Ps, true).

chidist([X, Degfree_]) ->
    ?ensure_numbers([X, Degfree_]),
    Degfree = erlang:trunc(Degfree_),
    ?ensure_non_negative_ex(X, ?ERR_NUM),
    ?ensure(Degfree >= 1, ?ERR_NUM),
    ?ensure(Degfree =< 1.0e+10, ?ERR_NUM),
    chidist1(X, Degfree).
chidist1(X, Degfree) ->
    Alpha = Degfree / 2, % 2 is beta
    Chi = 1 / (math:pow(2, Alpha) * stdfuns_math:fact([Alpha])),
    math:pow(Chi, (Alpha - 1)) * exp(X * -0.5).
    
    
    

exp(X) ->
    math:exp(X).

expondist([X, Lamda, false]) when not (X < 0) ->
    exp(-1 * X / Lamda) / Lamda;
expondist([X, Lamda, true]) when not (X < 0)  ->
    1 - exp(-1 * X / Lamda).




%%% ----------------- %%%
%%% Private functions %%%
%%% ----------------- %%%

tonum(N) when is_number(N) ->
    N;
tonum(true) ->
    1;
tonum(0) ->
    false;
tonum(S) when is_list(S) ->
    tconv:to_num(S).
