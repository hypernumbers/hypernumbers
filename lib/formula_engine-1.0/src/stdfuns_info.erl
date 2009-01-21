%%% @doc Built-in information functions.
%%% CELL, INFO, and ISREF are implemented in the evaluator because they need
%%% higher-level information.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(stdfuns_info).
-export([error_type/1, iserr/1, iserror/1, iseven/1, islogical/1, isna/1,
         isnontext/1, isnumber/1, isodd/1, istext/1, n/1, na/0, type/1,
         isblank/1, isnonblank/1, info/1]).
-export([rows/1, columns/1]).

-include("handy_macros.hrl").
-include("typechecks.hrl").

error_type([?ERRVAL_NULL]) -> 1;
error_type([?ERRVAL_DIV])  -> 2;
error_type([?ERRVAL_VAL])  -> 3;
error_type([?ERRVAL_REF])  -> 4;
error_type([?ERRVAL_NAME]) -> 5;
error_type([?ERRVAL_NUM])  -> 6;
error_type([?ERRVAL_NA])   -> 7;
error_type(_)              -> ?ERR_NA.

%% Returns the logical value TRUE if value refers to any error value except
%% #N/A; otherwise it returns FALSE.
iserr([?ERRVAL_NULL]) -> true;
iserr([?ERRVAL_DIV])  -> true;
iserr([?ERRVAL_VAL])  -> true;
iserr([?ERRVAL_REF])  -> true;
iserr([?ERRVAL_NAME]) -> true;
iserr([?ERRVAL_NUM])  -> true;
iserr(_)              -> false.

%% Returns true if argument is any error value.
iserror([?ERRVAL_NULL]) -> true;
iserror([?ERRVAL_DIV])  -> true;
iserror([?ERRVAL_VAL])  -> true;
iserror([?ERRVAL_REF])  -> true;
iserror([?ERRVAL_NAME]) -> true;
iserror([?ERRVAL_NUM])  -> true;
iserror([?ERRVAL_NA])   -> true;
iserror(_)              -> false.

%% Returns TRUE if number is even, or FALSE if number is odd.
%% The number is truncated, so ISEVEN(2.5) is true.
iseven([V1]) ->
    Num = ?number(V1, [cast_strings, cast_bools, cast_dates]),
    (trunc(Num) div 2) * 2 == trunc(Num).

isodd([Num]) -> not(iseven([Num])).

%% Returns true only for booleans or arrays where element (1,1) is a boolean.
%% (Returns false for ranges regardless of what's in them.)

islogical([B]) when is_boolean(B) -> true;
islogical(A) when ?is_array(A)    -> is_boolean(area_util:at(1, 1, A));
islogical(_)                      -> false.

isna([{error, na}]) -> true;
isna(_)             -> false.

isnontext([X]) -> case istext([X]) of
                      true  -> false;
                      false -> true
                  end.

isnumber([X]) when is_number(X) -> true;
isnumber([_])                  -> false.


istext([X]) when ?is_string(X) -> true;  %% complement(fun isnontext/1)
istext([_])               -> false.

%% TODO: dates.
n([Num]) when is_number(Num) ->
    Num;
n([true]) ->
    1;
n([false]) ->
    0;
n([{error, X}]) ->
    {error, X};
n(_) ->
    0.
    
na() ->
    {error, na}.

%% ~~~~~ INCOMPATIBILITY NOTE:
%% TYPE(A1:B10) in Excel = 16, in Hypernumbers it's 64.
%% TYPE(INDIRECT("A1")) in Excel = 0 regardless of contents of A1. In Hypernumbers it's same as TYPE(A1)
type([A]) when ?is_area(A)   -> 64;
type([N]) when is_number(N)  -> 1;
type([S]) when is_list(S)    -> 2;
type([B]) when is_boolean(B) -> 4;
type([{errval, _X}])         -> 16;
type([blank])                -> 1;
type(_)                      -> 0.

isblank([blank]) ->
    true;
isblank(Vs) ->
    Flatvs = ?flatten_all(Vs),
    all(fun muin_collect:is_blank/1, Flatvs).

isnonblank(Vs) ->
    Flatvs = ?flatten_all(Vs),
    all(fun(X) -> not(muin_collect:is_blank(X)) end, Flatvs).

info(["site"]) ->
    get(site);
info(["path"]) ->
    case "/" ++ string:join(get(path), "/") ++ "/" of
        "//" -> "/";
        V    -> V
    end.

rows([A]) when ?is_area(A) -> area_util:height(A);
rows([_])                  -> 1;
rows(_)                    -> ?ERR_VAL.

columns([A]) when ?is_area(A) ->
    area_util:width(A);
columns([V])                  ->
    _N = ?number(V, [ban_strings, ban_dates, ban_bools, cast_blanks]),
    1;
columns([A, V]) when ?is_area(A) ->
    B = ?bool(V, [cast_numbers, ban_strings, ban_dates, cast_blanks]), % strict?
    ?COND(B, area_util:width(A), columns([A]));
columns([V1, V2]) ->
    _N = ?number(V1, [ban_strings, ban_dates, ban_bools, cast_blanks]),
    B = ?bool(V2, [cast_numbers, ban_strings, ban_dates, cast_blanks]),
    ?COND(B, ?ERR_VAL, 1);
columns(_)                    -> ?ERR_VAL.
