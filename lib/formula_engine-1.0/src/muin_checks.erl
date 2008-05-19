%%% Helper functions to check argument types, do type casts etc for stdfuns.
%%% <hasan@hypernumbers.com>

%%% All functions are like pipes with filters. Good arguments simply flow
%%% through (sometimes getting "cleaned up" in the process), and bad arguments
%%% clog them up, i.e. an error is thrown.

-module(muin_checks).
-compile(export_all).

-import(muin_util, [cast/2]).
-include("errvals.hrl").
-include("handy_macros.hrl").

number(N) when is_number(N) ->
    N;
number(N) ->
    N2 = cast(N, num),
    ?COND(is_number(N2), N2, ?ERR_VAL).

numbers(L) ->
    L2 = map(fun(X) -> cast(X, num) end, L),
    Ok = all(fun(X) -> is_number(X) end, L2),
    ?COND(Ok, L2, ?ERR_VAL).

nonzero(0) ->
    ?ERR_DIV;
nonzero(N) when is_number(N) ->
    N;
nonzero(N) ->
    N2 = cast(N, num),
    ?COND(N2 =/= 0, N2, ?ERR_DIV).

%% Ensure number(s) are non-negative.
gte0(N) when is_number(N) andalso N >= 0 ->
    N;
gte0(N) ->
    N2 = cast(N, num),
    ?COND(N2 >= 0, N2, ?ERR_NUM).

gte0s(Ns) ->
    Ns2 = map(fun(X) -> cast(X, num) end,
              Ns),
    Ok = all(fun(X) -> is_number(X) andalso X >= 0 end,
             Ns),
    ?COND(Ok, Ns, ?ERR_NUM).

%% Ensure number(s) are positive.
gt0(N) when is_number(N) andalso N > 0 ->
    N;
gt0(N) ->
    N2 = cast(N, num),
    ?COND(N2 > 0, N2, ?ERR_NUM).

%% Checks a list of values for errvals, the first one found is returned.
die_on_errval(Vs) ->
    map(fun(X) -> ?COND(stdfuns_info:iserror(X), muin_util:error(X), X) end,
        Vs).

%% Lazy test.
ensure(true, Action) ->
    Action();
ensure(false, _Action) ->
    ok.

filter_numbers(Vs) ->
    [X || X <- Vs, is_number(Vs)].

filter_numbers_all(Vs) ->
    [cast(X, num) || X <- Vs, is_number(cast(X, num))].
                      
deck([H|T]) ->
    deck1(H, T, []).
deck1(_, [], Acc) ->
    Acc;
deck1({matrix, _, L}, [H|T], Acc) ->
    deck1(H, T, append([Acc, L]));
deck1(Val, [H|T], Acc) ->
    deck1(H, T, append([Acc, [Val]])).
