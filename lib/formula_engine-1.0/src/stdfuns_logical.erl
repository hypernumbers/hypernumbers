%%% @doc Built-in logical functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @private

-module(stdfuns_logical).
-include("handy_macros.hrl").
-include("typechecks.hrl").

-export(['='/1, '<>'/1, '<'/1, '>'/1, '<='/1, '>='/1]).
-export(['and'/1, 'if'/1, iferror/1, 'not'/1, 'or'/1]).

-import( muin_collect, [ col/2, col/3 ]).

-define(default_rules_bools, [cast_numbers, cast_strings,
                              cast_blanks, cast_dates]).

'='([A, B]) ->
    [A1, B1] = [ muin_collect:pick_first(X) || X <- [A, B] ],
    muin_checks:die_on_errval([A1]),
    muin_checks:die_on_errval([B1]),
    io:format("~p ~p~n",[A1, B1]),
    '=1'(A1, B1).

%% numbers & numbers
'=1'(N1, N2) when is_integer(N1), is_integer(N2) ->
    N1 == N2; 
'=1'(N1, N2) when is_number(N1), is_number(N2) ->
    test_util:float_cmp(float(N1), float(N2), 10); %% FIXME: Precision.
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

'>'([A, B]) ->
    muin_checks:die_on_errval([A]),
    muin_checks:die_on_errval([B]),
    [A1, B1] = ?numbers([A, B], [first_array, cast_dates]),
    '>1'(A1, B1).

'>1'(N, N) -> false;
'>1'(N1, N2) when is_number(N1) andalso is_number(N2)   -> N1 > N2;
'>1'(S, N) when ?is_string(S) andalso is_number(N)      -> true;
'>1'(N, S) when is_number(N) andalso ?is_string(S)      -> false;
%% FIXME: 1) Will break on non Latin-1 strings.
%%        2) Depends on Erlang's list representation of strings.
'>1'(S1, S2) when ?is_string(S1) andalso ?is_string(S2) ->
    if hd(S1) > hd(S2) -> true;  % compare alphabetically first
       hd(S1) < hd(S2) -> false;
       % otherwise compare on length
       true            -> stdfuns_text:len([S1]) > stdfuns_text:len([S2]) 
    end;
'>1'(true, false)                          -> true;
'>1'(false, true)                          -> false;
'>1'(false, blank)                         -> false;
'>1'(true, _)                              -> true; 
'>1'(false, _)                             -> true;
'>1'(_, true)                              -> false;
'>1'(_, false)                             -> false;
'>1'("",0)                                 -> true;
'>1'(0,"")                                 -> false;
'>1'("",blank)                             -> false;
'>1'("",0.0)                                 -> true;
'>1'(0.0,"")                                 -> false;
'>1'(blank,"")                             -> false;
'>1'(A, N) when ?is_area(A) andalso N > 0  -> true;
'>1'(A, N) when ?is_area(A) andalso N =< 0 -> false;
'>1'(blank, N) when is_number(N)           -> '>1'(0, N);
'>1'(N, blank) when is_number(N)           -> '>1'(N, 0);
'>1'(blank, S) when ?is_string(S)          -> false;
'>1'(S, blank) when ?is_string(S)          -> true;
'>1'(blank, blank)                         -> false.

'<'([A, B]) ->
    muin_checks:die_on_errval([A]),
    muin_checks:die_on_errval([B]),
    [A1, B1] = ?numbers([A, B], [first_array, cast_dates]),
    '>1'(B1, A1).

'<='(Args = [_, _]) ->
    '='(Args) orelse '<'(Args).

'>='(Args = [_, _]) ->
    '='(Args) orelse '>'(Args).

'and'(Vs) ->
    
    Rules = [eval_funs, first_array_as_bool, ref_as_bool,
             num_as_bool, str_as_bool, name_as_bool, ignore_blanks],
    
    case col(Vs, Rules, [return_errors, {all, fun muin_collect:is_bool/1}]) of
        Err when ?is_errval(Err) -> Err;
        []                       -> ?ERRVAL_VAL;
        Vals                     -> all(fun(X) -> X =/= false end, Vals)
    end.

'not'([V]) ->
    not(?bool(V, [cast_strings, cast_numbers, cast_blanks, cast_dates])).

'or'(Vs) ->
    Flatvs = ?flatten_all(Vs),
    ?ensure(Flatvs =/= [blank], ?ERR_VAL),
    Bools = ?bools(Flatvs, [cast_strings, cast_numbers,
                            cast_blanks, cast_dates]),
    any(fun(X) -> X == true end, Bools).

'if'([Test, TrueExpr]) ->
    'if'([Test, TrueExpr, false]);
'if'([Test, TrueExpr, FalseExpr]) ->
    
    V = muin:eval_formula(Test),
    B = ?bool(V, [first_array, cast_strings, cast_numbers, cast_blanks, ban_dates]),
    muin:eval_formula(?COND(B, TrueExpr, FalseExpr)).

%% @TODO write a test suite for iferror which is not an Excel 97 function
iferror([Test, TrueExpr, FalseExpr]) ->
    'if'([stdfuns_info:iserror([muin:eval(Test)]), TrueExpr, FalseExpr]).
