%%% @doc Logical functions.
%%% @author <hasan@hypernumbers.com>

-module(stdfuns_logical).
-include("handy_macros.hrl").
-include("typechecks.hrl").

-export([
         '='/1,
         '<>'/1,
         '<'/1,
         '>'/1,
         '<='/1,
         '>='/1,
         'and'/1,
         'if'/1,
         iferror/1,
         'not'/1,
         'or'/1]).

-define(default_rules_bools, [cast_numbers, cast_strings, cast_blanks, cast_dates]).

'='([V1, V2]) ->
    V1 == V2.

'<>'([V1, V2]) ->
    not('='([V1, V2])).

%% string is greater than any number
%% two strings are compared based on length
%% blanks are simply cast to 0s
%% true > false
%% true > any number
%% true > any string
%% same for false...
%% array < positive numbers, but not negative ones (WTF?)
'>'([N1, N2]) when is_number(N1) andalso is_number(N2)   -> N1 > N2;
'>'([S, N]) when ?is_string(S) andalso is_number(N)      -> true;
'>'([S1, S2]) when ?is_string(S1) andalso ?is_string(S2) -> stdfuns_text:len([S1]) > stdfuns_text:len([S2]);
'>'([true, _])                                     -> true;
'>'([false, _])                                    -> true;
'>'([A, N]) when ?is_area(A) andalso N > 0               -> true;
'>'([A, N]) when ?is_area(A) andalso N =< 0               -> false;
'>'([blank, X])                                    -> '>'([0, X]);
'>'([X, blank])                                    -> '>'([X, 0]).

'<'([A, B]) ->
    not('>'([A, B])).

'<='([A, B]) ->
    (A == B) orelse '<'([A, B]).

'>='([A, B]) ->
    (A == B) orelse '>'([A, B]).

'and'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, cast_blanks, cast_dates]),
    all(fun(X) -> X =/= false end, Bools).

'not'([V]) ->
    not(?bool(V, [cast_strings, cast_numbers, cast_blanks, cast_dates])).

'or'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, cast_blanks, cast_dates]),
    any(fun(X) -> X == true end,
        Bools).

'if'([Test, TrueExpr, FalseExpr]) ->
    V = muin:eval(Test),
    B = ?bool(V, [cast_strings, cast_numbers, cast_blanks, ban_dates]),
    ?COND(B, muin:eval(TrueExpr), muin:eval(FalseExpr)).

iferror([Test, TrueExpr, FalseExpr]) ->
    'if'([stdfuns_info:iserror([muin:eval(Test)]), TrueExpr, FalseExpr]).
