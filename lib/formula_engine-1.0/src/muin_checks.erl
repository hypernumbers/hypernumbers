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

number_rule(V, Rules) ->
    ?COND(member(type(V), Rules), % if type of V is in the rules
          cast(V, num),           % try to cast
          ?ERR_VAL).              % otherwise throw error

numbers(L) ->
    L2 = map(fun(X) -> cast(X, num) end, L),
    Ok = all(fun(X) -> is_number(X) end, L2),
    ?COND(Ok, L2, ?ERR_VAL).

int(N) ->
    erlang:trunc(number(N)).

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
             Ns2),
    ?COND(Ok, Ns2, ?ERR_NUM).

%% Ensure number(s) are positive.
gt0(N) when is_number(N) andalso N > 0 ->
    N;
gt0(N) ->
    N2 = cast(N, num),
    ?COND(N2 > 0, N2, ?ERR_NUM).

%% Checks a list of values for errvals, the first one found is returned.
die_on_errval(Vs) ->
    foreach(fun(?ERRVAL_NULL)    -> ?ERR_NULL;
               (?ERRVAL_DIV)     -> ?ERR_DIV;
               (?ERRVAL_VAL)     -> ?ERR_VAL;
               (?ERRVAL_REF)     -> ?ERR_REF;
               (?ERRVAL_NAME)    -> ?ERR_NAME;
               (?ERRVAL_NUM)     -> ?ERR_NUM;
               (?ERRVAL_NA)      -> ?ERR_NA;
               (?ERRVAL_CIRCREF) -> ?ERR_CIRCREF;
               (_)               -> nothing
            end,
            Vs).

%% Lazy test.
ensure(true, _Action) ->
    ok;
ensure(false, Action) ->
    Action().

filter_numbers(Vs) ->
    [X || X <- Vs, is_number(Vs)].

filter_numbers_all(Vs) ->
    [cast(X, num) || X <- Vs, is_number(cast(X, num))].


%% List of values -> list of booleans or error if a value can't be cast.
filter_bools_with_cast(Vs) ->
    [cast(X, bool) || X <- Vs].

deck([H|T]) ->
    deck1(H, T, []);
deck(Z) ->
    deck1(Z, [], []).
deck1(_, [], Acc) ->
    Acc;
deck1({matrix, _, L}, [H|T], Acc) ->
    deck1(H, T, append([Acc, L]));
deck1(Val, [H|T], Acc) ->
    deck1(H, T, append([Acc, [Val]])).

type(V) when is_number(V) ->
    num;
type(V) when is_boolean(V) ->
    bool;
type({date, _}) ->
    date;
type(V) when is_list(V) ->
    str.
