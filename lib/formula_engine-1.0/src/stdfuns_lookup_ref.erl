%%% @doc Lookup and reference functions.
%%% @author <hasan@hypernumbers.com>
%%% @private
-module(stdfuns_lookup_ref).

-import(muin_collect, [ collect/3 ]).
-export([address/1, choose/1, column/1, index/1, match/1, row/1]).

-compile(export_all).
-include("typechecks.hrl").
-include("handy_macros.hrl").

choose([V|Vs]) ->
    [Idx]  = collect([V], int, [fetch_refs, pick_first_array,
                                die_on_err, cast_or_err]),
    List = collect(Vs, any, [pick_first_array]),
    ?ensure(Idx > 0 andalso Idx =< length(Vs), ?ERR_VAL),
    choose(Idx, List).

choose(Idx, List) ->
    case muin:eval(nth(Idx, List)) of
        % TODO: eugh
        {array,[Arr]} -> "{"++string:join([tconv:to_s(X)||X<-Arr], ",")++"}";
        {namedexpr, _, _} -> ?ERRVAL_NAME;
        Else              -> Else
    end.

column([])                          -> muin:context_setting(col);
column([C]) when ?is_namedexpr(C)   -> ?ERRVAL_NAME;
column([C]) when ?is_cellref(C)     -> muin:col_index(muin:col(C));
column([Err]) when ?is_errval(Err)  -> Err;
column(_Else)                       -> ?ERRVAL_VAL.
 

%% TODO: Needs to be recompiled every time -- how to handle that cleanly?
%% (without writing to proc dict)
indirect([S]) ->
    ?ensure(?is_string(S), ?ERR_REF),
    {ok, Ast} = muin:parse(
                  S, {muin:context_setting(col), muin:context_setting(row)}),
    muin:eval(Ast). % want to return a ref here, not a value.


row([])                      -> muin:context_setting(row);
row([C]) when ?is_cellref(C) -> muin:row_index(muin:row(C));
row(_X)                       -> ?ERR_VAL.

address([Row, Col]) ->
    address([Row, Col, 1, true, ""]);
address([Row, Col, IsAbs]) ->
    address([Row, Col, IsAbs, true, ""]);
address([Row, Col, IsAbs, IsA1]) ->
    address([Row, Col, IsAbs, IsA1, ""]);
address([V1, V2, V3, V4, V5]) ->
    [Row, Col, AbsNum] = ?numbers([V1, V2, V3], [cast_strings, cast_bools, ban_blanks, ban_dates]),
    IsA1 = ?bool(V4, [cast_numbers, cast_strings, cast_blanks, ban_dates]),
    PagePath = ?estring(V5),
    ?ensure(Row > 0, ?ERR_VAL),
    ?ensure(Col > 0, ?ERR_VAL),
    ?ensure(AbsNum > 0, ?ERR_VAL),
    Addr = address1(Row, Col, AbsNum, IsA1),
    ?COND(PagePath == "", Addr, Addr ++ "/" ++ PagePath).
address1(Row, Col, 1, true) ->
    "$" ++ tconv:to_b26(Col) ++ "$" ++ tconv:to_s(Row);
address1(Row, Col, 2, true) ->
    tconv:to_b26(Col) ++ "$" ++ tconv:to_s(Row);
address1(Row, Col, 3, true) ->
    "$" ++ tconv:to_b26(Col) ++ tconv:to_s(Row);
address1(Row, Col, 4, true) ->
    tconv:to_b26(Col) ++ tconv:to_s(Row);
address1(Row, Col, 1, false) ->
    tconv:to_s(Row) ++ "R" ++ tconv:to_s(Col) ++ "C";
address1(Row, Col, 2, false) ->
    tconv:to_s(Row) ++ "R" ++ "[" ++ tconv:to_s(Col) ++ "]C";
address1(Row, Col, 3, false) ->
    "[" ++ tconv:to_s(Row) ++ "]R" ++ tconv:to_s(Col) ++ "C";
address1(Row, Col, 4, false) ->
    "[" ++ tconv:to_s(Row) ++ "]R[" ++ tconv:to_s(Col) ++ "]C".

%% This is the array form of INDEX. The reference form is in preproc.

%% Arg1 should be an array or range, but constants and cellrefs are also allowed.
%% Row number or column number can be omitted for areas of height/width of 1.
%% If either of them is omitted for areas that aren't of height/width 1, a
%% vertical/horizontal array is returned.

index([A, V]) when ?is_area(A) ->
    W = area_util:width(A),
    if W == 1 -> index([A, V, 1]);
       ?else  -> index([A, 1, V])
    end;
index([X, V]) ->
    index([area_util:make_array([[X]]), V]);
index([A, V1, V2]) when ?is_area(A) ->
    [Rown, Coln] = ?numbers([V1, V2], [cast_strings, cast_bools, ban_dates, ban_blanks]),
    if Rown =< 0 andalso Coln =< 0 -> A;
       Rown =< 0 -> area_util:col(Coln, A);
       Coln =< 0 -> area_util:row(Rown, A);
       ?else ->
            case area_util:at(Coln, Rown, A) of
                {ok, E}               -> E;
                {error, out_of_range} -> ?ERR_REF
            end
    end;
index([X, V1, V2]) ->
    index([area_util:make_array([[X]]), V1, V2]).


match([V1, V2]) ->
    match([V1, V2, 1]);
match([V1, V2, V3]) ->
    ?ensure(?is_area(V2), ?ERR_VAL),
    MatchType = ?int(V3, [cast_strings, ban_bools, ban_dates, cast_blanks]),
    ?ensure(MatchType =< 1 andalso MatchType >= -1, ?ERR_NA),
    ?ensure(area_util:height(V2) == 1, ?ERR_NA), % area must be horizontal
    match1(V1, area_util:to_list(V2), MatchType).            
match1(LookupVal, List, -1) ->
    %% List must be in descending order.
    IsDesc = all(fun({X1, X2}) -> stdfuns_logical:'>'([X1, X2]) end,
                 zip(hslists:init(List), tl(List))),
    ?ensure(IsDesc, ?ERR_NA),
    %% Find the smallest value that's >= to LookupVal.
    case find_first(fun(X) -> stdfuns_logical:'>='([X, LookupVal]) end, List) of
        {ok, V} -> pos(V, List);
        false   -> ?ERR_NA
    end;
match1(LookupVal, List, 0) ->
    %% List can be in any order, value must match exactly.
    case find_first(fun(X) -> stdfuns_logical:'='([X, LookupVal]) end, List) of
        {ok, V} -> pos(V, List);
        false   -> ?ERR_NA
    end;
match1(LookupVal, List, 1) ->
    %% List must be in ascending order.
    IsAsc = all(fun({X1, X2}) -> stdfuns_logical:'<'([X1, X2]) end,
                zip(hslists:init(List), tl(List))),
    ?ensure(IsAsc, ?ERR_NA),
    %% Find the largest value that's <= to LookupVal.
    case find_first(fun(X) -> stdfuns_logical:'<='([X, LookupVal]) end, reverse(List)) of
        {ok, V} -> pos(V, List);
        false   -> ?ERR_NA
    end.


vlookup([V, A, I]) ->
    vlookup([V, A, I, true]);
vlookup([V, A, I, B]) ->
    {Tag, L} = A,
    NewA = {Tag, transpose(L)},
    hlookup([V, NewA, I, B]).
    

hlookup([V, A, I]) ->
    hlookup([V, A, I, true]);
hlookup([V, A, I0, B]) ->
    
    I = ?int(I0, [cast_strings, cast_bools, ban_blanks, ban_dates]),
    
    ?ensure(?is_area(A), ?ERR_REF),
    ?ensure(I =< area_util:height(A), ?ERR_REF),
    ?ensure(I >= 1, ?ERR_VAL),

    Row = area_util:row(1, A),
    case find(V, Row, B) of
        0 ->
            ?ERR_NA;
        VIndex ->
            {ok, Ret} = area_util:at(VIndex, I, A),
            Ret
    end.

%%% private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% Area is either 1-row or 1-column.
%% If NonExact is true, then the position of the largest value that is less than
%% Value is returned.
find(Value, Area, true) ->
    L = area_util:to_list(Area),
    case pos(Value, L) of
        0 -> non_exact_find(Value, L);
        I -> I
    end;
find(Value, Area, false) ->
    L = area_util:to_list(Area),
    pos(Value, L).

%% TODO: Searching for floats.
non_exact_find(Value, L) ->
    Sorted = qsort(L),
    case find_first(fun(X) -> stdfuns_logical:'<'([X, Value]) end, reverse(Sorted)) of
        {ok, PrevLargest} -> pos(PrevLargest, L);
        false             -> 0
    end.


%% Straightforward Quicksort with our own comparators (result is in ascending
%% order).
%% Should this be a common utility function?
%% TODO: Unit tests for this.
%% TODO: Rewrite to tail-recursive.

qsort([]) ->
    [];
qsort([H|T]) ->
    L1 = [ X || X <- T, stdfuns_logical:'<'([X, H]) ],
    L2 = [ X || X <- T, stdfuns_logical:'>='([X, H]) ],
    qsort(L1) ++ [H] ++ qsort(L2).


%% @doc Find the position of element E in list L.
%% TODO: Remove. Replace calls above with nglists.

pos(E, L)                    -> pos(E, L, 1).
pos(_, [], _)                -> 0;
pos(E, [H|_], I) when E == H -> I;
pos(E, [_|T], I)             -> pos(E, T, I+1).


%% TODO: as in pos.

transpose(L) ->
    Len = length(hd(L)),
    transpose1(L, lists:duplicate(Len, [])).
transpose1([R|T], Acc) ->
    NewAcc = lists:zipwith(fun(X, Y) -> Y ++ [X] end, R, Acc),
    transpose1(T, NewAcc);
transpose1([], Acc) ->
    Acc.


%%% @doc Return first element that satisfies a predicate.
%%% TODO: Move to nglists and use that version here.

find_first(Pred, L) ->
    find_first(Pred, fun() -> false end, L).
find_first(Pred, IfNone, [Hd|Tl]) ->
    case Pred(Hd) of
        true -> {ok, Hd};
        _    -> find_first(Pred, IfNone, Tl)
    end;
find_first(_Pred, IfNone, []) ->
    IfNone().
