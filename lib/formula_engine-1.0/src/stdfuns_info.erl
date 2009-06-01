%%% @doc Built-in information functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @private

-module(stdfuns_info).
-export([cell/1,
         errortype/1,
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
         na/1,
         type/1,
         isblank/1,
         isnonblank/1,
         info/1]).

-export([rows/1,
         columns/1]).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-include("spriki.hrl").
-include("muin_records.hrl").


%% TODO: Range references (literal and from INDIRECT)
%% TODO: Other info types.
cell([V1, V2]) ->
    InfoType = muin:eval_formula(V1),
    ?ensure(?is_string(InfoType), ?ERR_VAL),
    R = case V2 of
            [indirect, _]                          -> muin:eval(V2);
            X when ?is_cellref(X)                  -> X;
            %% TODO: X when ?is_rangeref(X)                 -> todo;
            _                                      -> ?ERR_REF
        end,
    muin:do_cell(R#cellref.path, muin:row_index(R#cellref.row), muin:col_index(R#cellref.col)),
    Path = muin_util:walk_path(muin:context_setting(path), R#cellref.path),               
    RefX = #refX{site = muin:context_setting(site),
                 path = Path,
                 obj  = {cell, {muin:col_index(R#cellref.col), muin:row_index(R#cellref.row)}}},
    cell1(InfoType, RefX).

cell1("formula", RefX) ->
    Attr = hn_db_api:read_attributes(RefX, ["formula"]),
    case Attr of
        [{_, {"formula", F}}] ->
            case string:substr(F, 1, 1) of
                "=" -> string:substr(F, 2); % drop the equals sign "="
                _   -> F
            end;
        [] ->
            ""
    end;
cell1("address", RefX) ->
    {cell, {Col, Row}} = RefX#refX.obj,
    stdfuns_lookup_ref:address([Row, Col]);
cell1("col", RefX) ->
    {cell, {Col, _Row}} = RefX#refX.obj,
    Col;
cell1("row", RefX) ->
    {cell, {_Col, Row}} = RefX#refX.obj,
    Row;
cell1("contents", RefX) ->
    Attr = hn_db_api:read_attributes(RefX, ["value"]),
    [{_, {"value", V}}] = Attr,
    case V of
        blank -> 0;
        Else  -> Else
    end;
cell1(_, _) ->
    ?ERR_VAL.
    

errortype([?ERRVAL_NULL]) -> 1;
errortype([?ERRVAL_DIV])  -> 2;
errortype([?ERRVAL_VAL])  -> 3;
errortype([?ERRVAL_REF])  -> 4;
errortype([?ERRVAL_NAME]) -> 5;
errortype([?ERRVAL_NUM])  -> 6;
errortype([?ERRVAL_NA])   -> 7;
errortype(_)              -> ?ERR_NA.


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
%% @todo needs a test case written because it is not an Excel 97 function
iseven([V1]) ->
    Num = ?number(V1, [cast_strings, cast_bools, cast_dates]),
    (trunc(Num) div 2) * 2 == trunc(Num).


%% @todo needs a test case written because it is not an Excel 97 function
isodd([Num]) -> not(iseven([Num])).


%% Returns true only for booleans or arrays where element (1,1) is a boolean.
%% (Returns false for ranges regardless of what's in them.)

islogical([B]) when is_boolean(B) -> true;
islogical(A) when ?is_array(A)    -> is_boolean(area_util:at(1, 1, A));
islogical(_)                      -> false.


isna([{errval, '#N/A'}]) -> true;
isna(X)                  -> io:format("in stdfuns_info:isna X is ~p~n", [X]),
                            false.


isnontext([X]) -> case istext([X]) of
                      true  -> false;
                      false -> true
                  end.


isnumber([X]) when is_number(X) -> true;
isnumber([_])                  -> false.


istext([X]) when ?is_string(X) -> true;  %% complement(fun isnontext/1)
istext([_])                    -> false.


%% TODO: dates.
n([Num]) when is_number(Num) -> Num;
n([true])                    -> 1;
n([false])                   -> 0;
n([{errval, X}])             -> {errval, X};
n([{datetime, Y, D}])        -> {datetime, Y, D};
n(X)                         -> io:format("in n X is ~p~n", [X]),
                                0.
    
na([]) -> {errval, '#N/A'}.


%% TYPE(A1:B10) in Excel = 16, in Hypernumbers it's 64.
%% TYPE(INDIRECT("A1")) in Excel = 0 regardless of contents of A1. In Hypernumbers it's same as TYPE(A1)
type([A]) when ?is_area(A)   -> 64;
type([N]) when is_number(N)  -> 1;
type([S]) when is_list(S)    -> 2;
type([B]) when is_boolean(B) -> 4;
type([{errval, _X}])         -> 16;
type([blank])                -> 1;
type(_)                      -> 0.


isblank([blank]) -> true;
isblank(Vs)      -> Flatvs = ?flatten_all(Vs),
                    all(fun muin_collect:is_blank/1, Flatvs).


%% @todo needs a test case written because it is not an Excel 97 function
isnonblank(Vs) -> Flatvs = ?flatten_all(Vs),
                  all(fun(X) -> not(muin_collect:is_blank(X)) end, Flatvs).


info(["site"]) -> get(site);
info(["path"]) -> case "/" ++ string:join(get(path), "/") ++ "/" of
                      "//" -> "/";
                      V    -> V
                  end.


rows([A]) when ?is_area(A) -> area_util:height(A);
rows([_])                  -> 1;
rows(_)                    -> ?ERR_VAL.


columns([R]) when ?is_rangeref(R) ->
    R#rangeref.width;
columns([A]) when ?is_array(A) ->
    area_util:width(A);
columns([Expr]) ->
    V = muin:eval_formula(Expr),
    %% Argument must be castable to number, but 1 is returned regardless of
    %% what its value is.
    _N = ?number(V, [ban_strings, ban_dates, ban_bools, cast_blanks]),
    1.
