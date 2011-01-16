%%% @doc Built-in information functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @private

-module(stdfuns_info).
-export([cell/1,
         'error.type'/1,
         iserr/1,
         iserror/1,
         iseven/1,
         islogical/1,
         isna/1,
         isnontext/1,
         isnumber/1,
         isodd/1,
         isref/1,
         istext/1,
         n/1,
         na/1,
         type/1,
         isblank/1,
         isnonblank/1,
         info/1]).

-export([rows/1,
         columns/1]).

-include("typechecks.hrl").
-include("spriki.hrl").
-include("muin_records.hrl").

-define(ext, muin:external_eval_formula).

%% TODO: Range references (literal and from INDIRECT)
%% TODO: Other info types.
cell([V1, V2]) ->
    InfoType = muin:external_eval_formula(V1),
    muin_checks:ensure(?is_string(InfoType), ?ERRVAL_VAL),
    R = case V2 of
            [indirect, _]                          -> muin:external_eval(V2);
            X when ?is_cellref(X)                  -> X;
            %% TODO: X when ?is_rangeref(X)                 -> todo;
            _                                      -> ?ERR_REF
        end,
    muin:do_cell(R#cellref.path, muin:row_index(R#cellref.row),
                 muin:col_index(R#cellref.col), finite),
    Path = muin_util:walk_path(muin:context_setting(path), R#cellref.path),
    RefX = #refX{site = muin:context_setting(site),
                 path = Path,
                 obj  = {cell, {muin:col_index(R#cellref.col),
                                muin:row_index(R#cellref.row)}}},
    cell1(InfoType, RefX).

cell1("formula", RefX) ->
    Ret = hn_db_api:read_attribute(RefX, "formula"),
    case Ret of
        [{_, F}] ->
            case string:substr(F, 1, 1) of
                "=" -> string:substr(F, 2); % drop the equals sign "="
                _   -> F
            end;
        _ ->
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
    Ret = hn_db_api:read_attribute(RefX, "value"),
    [{_, V}] = Ret,
    case V of
        blank -> 0;
        Else  -> Else
    end;
cell1(_, _) ->
    ?ERR_VAL.

errornum(?ERRVAL_NULL)    -> 1;
errornum(?ERRVAL_DIV)     -> 2;
errornum(?ERRVAL_VAL)     -> 3;                                   
errornum(?ERRVAL_REF)     -> 4;
errornum(?ERRVAL_NAME)    -> 5;
errornum(?ERRVAL_NUM)     -> 6;
errornum(?ERRVAL_NA)      -> 7;
errornum(?ERRVAL_CIRCREF) -> 8;
errornum(?ERRVAL_AUTH)    -> 9;
errornum(?ERRVAL_FORM)    -> 10.
%% THERE IS NO errornum FOR #MOCHIJSON! AS IT IS A BUG NOT A PROPER ERROR MESSAGE!

'error.type'([X]) when ?is_errval(X)                  -> errornum(X);
'error.type'([{array, [[X|_]|_]}]) when ?is_errval(X) -> errornum(X);
'error.type'(_)                                       -> ?ERRVAL_NA.

%% Returns the logical value TRUE if value refers to any error value except
%% #N/A; otherwise it returns FALSE.
iserr([?ERRVAL_NA])                            -> false;
iserr([X]) when ?is_errval(X)                  -> true;
iserr([{array, [[X|_]|_]}]) when ?is_errval(X) -> true;
iserr(_)                                       -> false.

%% Returns true if argument is any error value.
iserror([X]) when ?is_errval(X)                  -> true;
iserror([{array, [[X|_]|_]}]) when ?is_errval(X) -> true;
iserror(_)                                       -> false.

%% Returns TRUE if number is even, or FALSE if number is odd.
%% The number is truncated, so ISEVEN(2.5) is true.
%% @todo needs a test case written because it is not an Excel 97 function
iseven([V1]) ->
    Num = muin_col_DEPR:collect_number(V1, [cast_strings, cast_bools, cast_dates]),
    (trunc(Num) div 2) * 2 == trunc(Num).

%% @todo needs a test case written because it is not an Excel 97 function
isodd([Num]) -> not(iseven([Num])).


%% Returns true only for booleans or arrays where element (1,1) is a boolean.
%% (Returns false for ranges regardless of what's in them.)
islogical([B]) when is_boolean(B) -> true;
islogical([A]) when ?is_array(A)    ->
    {ok, Val} = area_util:at(1, 1, A),
    is_boolean(Val);
islogical(_Else) ->
    false.

isna([{errval, '#N/A'}]) -> true;
isna([X]) when ?is_array(X) ->
    {ok, N} = area_util:at(1, 1, X),
    N == {errval, '#N/A'};
isna(_X) ->
    false.

isnumber([X]) when is_number(X) -> true;
isnumber([X]) when ?is_array(X)    ->
    {ok, N} = area_util:at(1, 1, X),
    is_number(N);
isnumber([_])                  -> false.

isnontext([X]) -> not istext([X]).

istext([X]) when ?is_string(X) -> true;  %% complement(fun isnontext/1)
istext([X]) when ?is_array(X)    ->
    {ok, N} = area_util:at(1, 1, X),
    ?is_string(N);
istext([_])                    -> false.


%% TODO: dates.
n([Num]) when is_number(Num) -> Num;
n([true])                    -> 1;
n([false])                   -> 0;
n([{errval, X}])             -> {errval, X};
n([{datetime, _Y, _D}=Date]) -> case muin_util:cast(Date, date, num) of
                                    {error, _Err} -> ?ERR_VAL;
                                    Else          -> Else
                                end;
n([X]) when ?is_array(X)     -> 1;
n(_X)                        -> 0.
    
na([]) -> {errval, '#N/A'}.

%% TYPE(INDIRECT("A1")) in Excel = 0 regardless of contents of A1. In Hypernumbers it's same as TYPE(A1)
type([A]) when ?is_range(A)  -> 16;
type([A]) when ?is_array(A)  -> 64;
type([N]) when is_number(N)  -> 1;
type([S]) when is_list(S)    -> 2;
type([B]) when is_boolean(B) -> 4;
type([{errval, _X}])         -> 16;
type([blank])                -> 1;
type(_)                      -> 0.

%% Excel's ISBLANK takes one argument.  Ours will work with a list too.
isblank([B]) when ?is_blank(B) ->
    true;
isblank(Vs)      ->
    Flatvs = muin_col_DEPR:flatten_areas(Vs),
    lists:all(fun muin_collect:is_blank/1, Flatvs).

%% @todo needs a test case written because it is not an Excel 97 function
isnonblank(Vs) -> Flatvs = muin_col_DEPR:flatten_areas(Vs),
                  lists:all(fun(X) -> not(muin_collect:is_blank(X)) end, Flatvs).

info(["site"]) -> get(site);
info(["path"]) -> case "/" ++ string:join(get(path), "/") ++ "/" of
                      "//" -> "/";
                      V    -> V
                  end.

isref([R]) when ?is_rangeref(R) orelse ?is_cellref(R) -> true;
isref(_R) ->
    false.

rows([R]) when ?is_rangeref(R)        -> R#rangeref.height;
rows([A]) when ?is_array(A)           -> area_util:height(A);
rows([Expr]) when ?is_cellref(Expr)   -> 1;
rows([Expr]) when ?is_namedexpr(Expr) -> ?ERRVAL_NAME;
rows([Expr]) when is_number(Expr)     -> 1;
rows([Expr]) when ?is_errval(Expr)    -> Expr;
rows([Expr]) when ?is_funcall(Expr)   -> rows([?ext(Expr)]);
rows([_Expr])                         -> ?ERRVAL_VAL.

columns([R]) when ?is_rangeref(R)        -> R#rangeref.width;
columns([A]) when ?is_array(A)           -> area_util:width(A);
columns([Expr]) when ?is_cellref(Expr)   -> 1;
columns([Expr]) when ?is_namedexpr(Expr) -> ?ERRVAL_NAME;
columns([Expr]) when is_number(Expr)     -> 1;
columns([Expr]) when ?is_errval(Expr)    -> Expr;
columns([Expr]) when ?is_funcall(Expr)   -> columns([?ext(Expr)]);
columns([_Expr])                         -> ?ERRVAL_VAL.

