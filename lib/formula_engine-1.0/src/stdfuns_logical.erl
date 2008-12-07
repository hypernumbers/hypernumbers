%%% @doc Built-in logical functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(stdfuns_logical).
-include("handy_macros.hrl").
-include("typechecks.hrl").

-export(['='/1, '<>'/1, '<'/1, '>'/1, '<='/1, '>='/1]).
-export(['and'/1, 'if'/1, iferror/1, 'not'/1, 'or'/1]).

-define(default_rules_bools, [cast_numbers, cast_strings, cast_blanks, cast_dates]).

'='([A, B]) -> muin_checks:die_on_errval([A]),
               muin_checks:die_on_errval([B]),
               '=1'(A, B).
%% numbers & numbers
'=1'(N1, N2) when is_number(N1) andalso is_number(N2) -> N1 == N2;
%% numbers & blanks
'=1'(blank, N) when N == 0                      -> true;
'=1'(N, blank) when N == 0                      -> true;
%% numbers & bools
'=1'(N, B) when is_number(N) andalso is_boolean(B)    -> false;
'=1'(B, N) when is_number(N) andalso is_boolean(B)    -> false;
%% strings & blanks
'=1'(blank, S) when ?is_string(S)               -> stdfuns_text:len([S]) == 0;
'=1'(S, blank) when ?is_string(S)               -> stdfuns_text:len([S]) == 0;
%% bools & blanks
'=1'(false, blank)                              -> true;
'=1'(blank, false)                              -> true;
'=1'(true, blank)                               -> false;
'=1'(blank, true)                               -> false;
%% strings & numbers
'=1'(S, N) when ?is_string(S) andalso is_number(N)    -> false;
'=1'(N, S) when ?is_string(S) andalso is_number(N)    -> false;
%% default: true & blank, bool & bool, blank & blank, string & string.
'=1'(V1, V2)                                    -> V1 == V2.

'<>'([V1, V2]) -> not('='([V1, V2])).

'>'([A, B]) -> muin_checks:die_on_errval([A]),
               muin_checks:die_on_errval([B]),
               '>1'(A, B).

'>1'(N1, N2) when is_number(N1) andalso is_number(N2)   -> N1 > N2;
'>1'(S, N) when ?is_string(S) andalso is_number(N)      -> true;
'>1'(N, S) when is_number(N) andalso ?is_string(S)      -> false;
%% FIXME: 1) Will break on non Latin-1 strings.
%%        2) Depends on Erlang's list representation of strings.
'>1'(S1, S2) when ?is_string(S1) andalso ?is_string(S2) ->
    if hd(S1) > hd(S2) -> true;  % compare alphabetically first
       hd(S1) < hd(S2) -> false;
       true            -> stdfuns_text:len([S1]) > stdfuns_text:len([S2]) % otherwise compare on length
    end;
'>1'(true, _)                                     -> true;
'>1'(false, _)                                    -> true;
'>1'(_, true)                                     -> false;
'>1'(_, false)                                    -> false;
'>1'(A, N) when ?is_area(A) andalso N > 0         -> true;
'>1'(A, N) when ?is_area(A) andalso N =< 0        -> false;
'>1'(blank, N) when is_number(N)                  -> '>1'(0, N);
'>1'(N, blank) when is_number(N)                  -> '>1'(N, 0);
'>1'(blank, S) when ?is_string(S)                 -> false;
'>1'(S, blank) when ?is_string(S)                 -> true;
'>1'(blank, blank)                                -> false.

'<'([A, B]) -> '>'([B, A]).

'<='(Args = [_, _]) -> '='(Args) orelse '<'(Args).

'>='(Args = [_, _]) -> '='(Args) orelse '>'(Args).

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
