%%% Helper functions to check argument types, do type casts etc for stdfuns.
%%% <hasan@hypernumbers.com>

%%% All functions are like pipes with filters. Good arguments simply flow
%%% through (sometimes getting "cleaned up" in the process), and bad arguments
%%% clog them up, i.e. an error is thrown.
%%% @private

-module(muin_checks).

-export([
         %% number/1,
         %% number_rule/2,
         %% numbers/1,
         %% int/1,
         %% nonzero/1,
         %% gte0s/1,
         %% gte0/1,
         %% gt0/1,
         die_on_errval/1
         %% ensure/2,
         %% filter_numbers/1,
         %% filter_numbers_all/1,
         %% filter_bools_with_cast/1,
         %% deck/1,
         %% type/1         
        ]).

-include("errvals.hrl").

%% number(N) when is_number(N) ->
%%     N;
%% number(N) ->
%%     N2 = muin_util:cast(N, num),
%%     case is_number(N2) of
%%         true  -> N2;
%%         false -> ?ERR_VAL
%%     end.

%% number_rule(V, Rules) ->
%%     case lists:member(type(V), Rules) of % if type of V is in the rules
%%         true  -> muin_util:cast(V, num);           % try to cast
%%         false -> ?ERR_VAL                % otherwise throw error
%%     end.

%% numbers(L) ->
%%     L2 = lists:map(fun(X) -> muin_util:cast(X, num) end, L),
%%     Ok = lists:all(fun(X) -> is_number(X) end, L2),
%%     case Ok of
%%         true  -> L2;
%%         false -> ?ERR_VAL
%%     end.

%% int(N) ->
%%     erlang:trunc(number(N)).

%% nonzero(0) ->
%%     ?ERR_DIV;
%% nonzero(N) when is_number(N) ->
%%     N;
%% nonzero(N) ->
%%     N2 = muin_util:cast(N, num),
%%     case (N2 =/= 0) of
%%         true  -> N2;
%%         false -> ?ERR_DIV
%%     end.

%% %% Ensure number(s) are non-negative.
%% gte0(N) when is_number(N) andalso N >= 0 ->
%%     N;
%% gte0(N) ->
%%     N2 = muin_util:cast(N, num),
%%     case (N2 >= 0) of
%%         true  -> N2;
%%         false -> ?ERR_NUM
%%     end.

%% gte0s(Ns) ->
%%     Ns2 = lists:map(fun(X) -> muin_util:cast(X, num) end,
%%               Ns),
%%     Ok = lists:all(fun(X) -> is_number(X) andalso X >= 0 end,
%%              Ns2),
%%     case Ok of
%%         true  -> Ns2;
%%         false -> ?ERR_NUM
%%     end.

%% %% Ensure number(s) are positive.
%% gt0(N) when is_number(N) andalso N > 0 ->
%%     N;
%% gt0(N) ->
%%     N2 = muin_util:cast(N, num),
%%     case (N2 > 0) of
%%         true  -> N2;
%%         false ->?ERR_NUM
%%     end.
 
%% Checks a list of values for errvals, the first one found is returned.
die_on_errval(Vs) ->
    lists:foreach(fun(?ERRVAL_NULL) -> ?ERR_NULL;
               (?ERRVAL_DIV)        -> ?ERR_DIV;
               (?ERRVAL_VAL)        -> ?ERR_VAL;
               (?ERRVAL_REF)        -> ?ERR_REF;
               (?ERRVAL_NAME)       -> ?ERR_NAME;
               (?ERRVAL_NUM)        -> ?ERR_NUM;
               (?ERRVAL_NA)         -> ?ERR_NA;
               (?ERRVAL_CIRCREF)    -> ?ERR_CIRCREF;
               (?ERRVAL_AUTH)       -> ?ERR_AUTH;
               (_)                  -> nothing
            end,
            Vs),
    Vs.

%% %% Lazy test.
%% ensure(true, _Action) ->
%%     ok;
%% ensure(false, Action) ->
%%     Action().

%% filter_numbers(Vs) ->
%%     [X || X <- Vs, is_number(X)].

%% filter_numbers_all(Vs) ->
%%     [muin_util:cast(X, num) || X <- Vs, is_number(muin_util:cast(X, num))].


%% %% List of values -> list of booleans or error if a value can't be cast.
%% filter_bools_with_cast(Vs) ->
%%     [muin_util:cast(X, bool) || X <- Vs].

%% deck([H|T]) ->
%%     deck1(H, T, []);
%% deck(Z) ->
%%     deck1(Z, [], []).
%% deck1(Val, [], Acc) ->
%%     lists:append([Acc,[Val]]);
%% deck1({matrix, _, L}, [H|T], Acc) ->
%%     deck1(H, T, lists:append([Acc, L]));
%% deck1(Val, [H|T], Acc) ->
%%     deck1(H, T, lists:append([Acc, [Val]])).

%% type(V) when is_number(V) ->
%%     num;
%% type(V) when is_boolean(V) ->
%%     bool;
%% type({date, _}) ->
%%     date;
%% type(V) when is_list(V) ->
%%     str.
