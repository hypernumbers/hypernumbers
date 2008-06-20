%%% @doc Built-in information functions.
%%% CELL, INFO, and ISREF are implemented in the interpreter because they need
%%% higher-level information.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(stdfuns_info).
-export([
         error_type/1,
         iserr/1,
         iserror/1,
         iseven/1,
         islogical/1,
         isna/1,
         isnontext/1,
         isnumber/1,
         isodd/1,
         istext/1,
         n/1,
         na/0,
         type/1
        ]).

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
iserr([V]) when V == ?ERRVAL_NULL orelse
                V == ?ERRVAL_DIV orelse
                V == ?ERRVAL_VAL orelse
                V == ?ERRVAL_REF orelse
                V == ?ERRVAL_NAME orelse
                V == ?ERRVAL_NUM ->
    true;
iserr(_) ->
    false.

%% Returns true if argument is any error value.
iserror([V]) when V == ?ERRVAL_NULL orelse
                  V == ?ERRVAL_DIV orelse
                  V == ?ERRVAL_VAL orelse
                  V == ?ERRVAL_REF orelse
                  V == ?ERRVAL_NAME orelse
                  V == ?ERRVAL_NUM orelse
                  V == ?ERRVAL_NA ->
    true;
iserror(_) ->
    false.

%% Returns TRUE if number is even, or FALSE if number is odd.
%% The number is truncated, so ISEVEN(2.5) is true.
iseven([Num]) ->
    ?ensure_number(Num),
    (trunc(Num) div 2) * 2 == trunc(Num).

isodd([Num]) ->
    not(iseven([Num])).

%% Returns the result TRUE if value refers to a logical value; otherwise returns
%% FALSE.

%% COMPATIBILITY NOTES:
%% Returns false for ranges, even if they contain logical values. -- YES.
%% Returns true for cells when they contain logical values. -- YES.
%% Returns true for arrays that contain logical values. -- NOT YET.
%% TODO: Above.
islogical([true]) ->
    true;
islogical(_) ->
    false.

isna([{error, na}]) ->
    true;
isna(_) ->
    false.


%% TODO:
isnontext([_]) ->
    true.


isnumber([Num]) when is_number(Num) ->
    true;
isnumber(_) ->
    false.

%% TODO:
istext([_]) ->
    false. %% complement(fun isnontext/1)

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

%% TODO / COMPATIBILITY NOTE:
%% TYPE(A1) when A1 is blank => 1.
%% TYPE(INDIRECT("A1")) regardless of what's in A1 => 0
%% TYPE({1}) => 64
%% TYPE(A1:B10) regardless of what's in A1:B10 => 16
type([Num]) when is_number(Num) ->
    1;
type([Ustr]) when is_binary(Ustr) ->
    2;
type([Bool]) when is_boolean(Bool) ->
    4;
type([{error, _X}]) ->
    16;
type([List]) when is_list(List) ->
    64;
type(_) ->
    0.
