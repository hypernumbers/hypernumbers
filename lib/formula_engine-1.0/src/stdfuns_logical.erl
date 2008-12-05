%%% @doc Built-in logical functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(stdfuns_logical).
-include("handy_macros.hrl").
-include("typechecks.hrl").

-export(['='/1, '<>'/1, '<'/1, '>'/1, '<='/1, '>='/1]).
-export(['and'/1, 'if'/1, iferror/1, 'not'/1, 'or'/1]).

-define(default_rules_bools, [cast_numbers, cast_strings, cast_blanks, cast_dates]).

%% numbers & numbers
'='([N1, N2]) when is_number(N1) andalso is_number(N2) -> N1 == N2;
%% numbers & blanks
'='([blank, N]) when N == 0                      -> true;
'='([N, blank]) when N == 0                      -> true;
%% numbers & bools
'='([N, B]) when is_number(N) andalso is_boolean(B)    -> false;
'='([B, N]) when is_number(N) andalso is_boolean(B)    -> false;
%% strings & blanks
'='([blank, S]) when ?is_string(S)               -> stdfuns_text:len([S]) == 0;
'='([S, blank]) when ?is_string(S)               -> stdfuns_text:len([S]) == 0;
%% bools & blanks
'='([false, blank])                              -> true;
'='([blank, true])                               -> true;
%% strings & numbers
'='([S, N]) when ?is_string(S) andalso is_number(N)    -> false;
'='([N, S]) when ?is_string(S) andalso is_number(N)    -> false;
%% default: true & blank, bool & bool, blank & blank, string & string.
'='([V1, V2])                                    -> V1 == V2.

'<>'([V1, V2]) ->
    not('='([V1, V2])).

'>'([A,B]) ->
    ?ensure_no_errvals([A]),
    ?ensure_no_errvals([B]),
    gt([A,B]).

gt([N1, N2]) when is_number(N1) andalso is_number(N2)   -> N1 > N2;
gt([S, N]) when ?is_string(S) andalso is_number(N)      -> true;
gt([N, S]) when is_number(N) andalso ?is_string(S)      -> false;
%% FIXME: 1) Will break on non Latin-1 strings.
%%        2) Depends on Erlang's list representation of strings.
gt([S1, S2]) when ?is_string(S1) andalso ?is_string(S2) ->
    if hd(S1) > hd(S2) -> true;  % compare alphabetically first
       hd(S1) < hd(S2) -> false;
       true            -> stdfuns_text:len([S1]) > stdfuns_text:len([S2]) % otherwise compare on length
    end;
gt([true, _])                                     -> true;
gt([false, _])                                    -> true;
gt([_, true])                                     -> false;
gt([_, false])                                    -> false;
gt([A, N]) when ?is_area(A) andalso N > 0         -> true;
gt([A, N]) when ?is_area(A) andalso N =< 0        -> false;
gt([blank, N]) when is_number(N)                  -> '>'([0, N]);
gt([N, blank]) when is_number(N)                  -> '>'([N, 0]);
gt([blank, S]) when ?is_string(S)                 -> false;
gt([S, blank]) when ?is_string(S)                 -> true;
gt([blank,blank])                                 -> false.

'<'([A, B]) -> '>'([B, A]).

'<='(Args = [_, _]) -> '<'(Args) orelse '='(Args).

'>='(Args = [_, _]) ->
    '>'(Args) orelse '='(Args).

'and'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, cast_blanks, cast_dates]),
    all(fun(X) -> X =/= false end, Bools).

'not'([V]) ->
    not(?bool(V, [cast_strings, cast_numbers, cast_blanks, cast_dates])).

'or'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers, cast_blanks, cast_dates]),
    any(fun(X) -> X == true end, Bools).

'if'([Test, TrueExpr, FalseExpr]) ->
    V = muin:eval(Test),
    B = ?bool(V, [cast_strings, cast_numbers, cast_blanks, ban_dates]),
    ?COND(B, muin:eval(TrueExpr), muin:eval(FalseExpr)).

iferror([Test, TrueExpr, FalseExpr]) ->
    'if'([stdfuns_info:iserror([muin:eval(Test)]), TrueExpr, FalseExpr]).
