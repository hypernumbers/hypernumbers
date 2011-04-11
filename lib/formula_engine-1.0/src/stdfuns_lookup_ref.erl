%%% @doc Lookup and reference functions.
%%% @author <hasan@hypernumbers.com>
%%% @private
-module(stdfuns_lookup_ref).

-export([
         address/1,
         choose/1,
         column/1,
         index/1,
         match/1,
         row/1,
         non_exact_find/2,
         qsort/1,
         transpose/1,
         indirect/1,
         vlookup/1,
         hlookup/1,
         find/3
        ]).

-import(muin_collect, [ collect/3 ]).
-import(muin_collect, [ col/2, col/3, col/4 ]).

-include("typechecks.hrl").
-include("muin_records.hrl").

choose([V|Vs]) ->
    Idx = col([V], [eval_funs, fetch, area_first, {cast, int}],
              [return_errors, {all, fun is_integer/1}]),
    List = col(Vs, [eval_funs, fetch, area_first]),
    muin_util:apply([Idx, List], fun choose/2).
choose([Idx], List) when Idx =< 0 orelse Idx > length(List) ->
    ?ERRVAL_VAL;
choose([Idx], List) ->
    case muin:external_eval(lists:nth(Idx, List)) of
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
    indirect([S, true]);
indirect([S, R1]) ->
    Str  = col([S], [eval_funs, fetch, area_first, {cast, str}],
              [return_errors, {all, fun muin_collect:is_string/1}]),
    Bool = col([R1], [eval_funs, fetch, area_first, {cast, bool}],
               [return_errors, {all, fun is_boolean/1}]),
    muin_util:apply([Str, Bool], fun indirect_/2).

indirect_([Str], [_Bool]) ->
    put(recompile, true),
    case muin:parse(Str, {muin:context_setting(col),
                          muin:context_setting(row)}) of
        {ok, Ast} ->
            case muin:external_eval(Ast) of
                X when ?is_cellref(X); ?is_rangeref(X) -> X;
                _Else                                  -> ?ERRVAL_REF
            end;
        {error, syntax_error} -> ?ERRVAL_REF
    end.

row([])                      -> muin:context_setting(row);
row([C]) when ?is_cellref(C) -> muin:row_index(muin:row(C));
row(_X)                       -> ?ERR_VAL.

address([Row, Col]) ->
    address([Row, Col, 1]);
address([Row, Col, IsAbs]) ->
    address([Row, Col, IsAbs, true]);
address([Row, Col, IsAbs, IsA1]) ->
    address([Row, Col, IsAbs, IsA1, ""]);

address([Row, Col, undef, IsA1, Path]) ->
    address([Row, Col, 1, IsA1, Path]);
address([Row, Col, IsAbs, undef, Path]) ->
    address([Row, Col, IsAbs, true, Path]);
address([Row, Col, undef, undef, Path]) ->
    address([Row, Col, 1, true, Path]);

address([V1, V2, V3, V4, V5]) ->
    N1 = col([V1, V2, V3],
             [eval_funs, fetch, area_first, {cast, num}, {cast, int}],
             [return_errors, {all, fun is_integer/1}]),
    N2 = col([V4],
             [eval_funs, fetch, area_first, {cast, bool}],
             [return_errors, {all, fun is_boolean/1}]),
    N3 = col([V5],
             [eval_funs, fetch, area_first, {cast, str}],
             [return_errors, {all, fun muin_collect:is_string/1}]),
    muin_util:apply([N1, N2, N3], fun address_/3).

address_([Row, Col, AbsNum], [_IsA1], [_Page])
  when Row < 1; Col < 1; AbsNum < 1; AbsNum > 4 ->
    ?ERRVAL_VAL;
address_([Row, Col, AbsNum], [IsA1], [Page]) ->
    Addr = address1(tconv:to_s(Row), tconv:to_b26(Col), AbsNum, IsA1),
    case (Page == "") of
        true  -> Addr;
        false -> "../"++Page++"/"++Addr
    end.

address1(Row, Col, 1, true)  -> "$" ++ Col ++ "$" ++ Row;
address1(Row, Col, 2, true)  -> Col ++ "$" ++ Row; %"
address1(Row, Col, 3, true)  -> "$" ++ Col ++ Row; %"
address1(Row, Col, 4, true)  -> Col ++ Row;
address1(Row, Col, 1, false) -> "R" ++ Row ++"C"++ Col;
address1(Row, Col, 2, false) -> "R" ++ Row ++ "C[" ++ Col ++ "]";
address1(Row, Col, 3, false) -> "R[" ++ Row ++ "]C" ++ Col;
address1(Row, Col, 4, false) -> "R[" ++ Row ++ "]C[" ++ Col ++ "]".


%% Arg1 should be an array or range, but constants and cellrefs are also allowed
%% Row number or column number can be omitted for areas of height/width of 1.
%% If either of them is omitted for areas that aren't of height/width 1, a
%% vertical/horizontal array is returned.
index([{list, V1}, V2, V3, V4])  ->

    Num = col([V4],
              [eval_funs, fetch, area_first, cast_num],
              [return_errors, {all, fun is_number/1}]),

    case Num of
        X   when ?is_errval(X)  -> X;
        [X] when X < 1          -> ?ERRVAL_VAL;
        [X] when X > length(V1) -> ?ERRVAL_REF;
        [X]                     -> index([lists:nth(X, V1), V2, V3])
    end;

index([A, V]) ->
    case (?is_area(A) orelse ?is_rangeref(A)) of
        true ->
            case V > area_util:height(A)  of
                true -> index([A, 1, V]);
                _    -> index([A, V, 1])
            end;
        false ->
            index([A, V, 1])
    end;

index([A, _V1, _V2]) when ?is_errval(A)    -> A;
index([A, V1, V2]) when ?is_funcall(A)     -> index([muin:external_eval(A), V1, V2]);
index([A, _V1, _V2]) when ?is_namedexpr(A) -> ?ERRVAL_NAME;
index([A, V1, V2]) when is_number(A) ->
    index([area_util:make_array([[A]]), V1, V2]);
index([A, _V1, _V2]) when not( ?is_area(A) orelse ?is_rangeref(A) ) ->
    ?ERRVAL_VAL;

index([A, V1, V2]) when ?is_area(A) orelse ?is_rangeref(A) ->
    Ind = col([V1, V2],
              [eval_funs, fetch, area_first, cast_num],
              [return_errors, {all, fun is_number/1}]),
    muin_util:apply([A, Ind], fun index_/2).

index_(_Area, [X, Y]) when X < 0; Y < 0 ->
    ?ERRVAL_VAL;

index_(Area, [FY, FX]) when ?is_area(Area) orelse ?is_rangeref(Area) ->

    %% excel does boundary checking before truncating (dumb)
    [Y, X] = [ erlang:trunc(X) || X<-[FY, FX]],

    case (Y > area_util:height(Area) orelse X > area_util:width(Area)) of
        true  -> ?ERRVAL_REF;
        false ->
            case ?is_array(Area) of
                true ->
                    [{_, Rows}] = col([Area], [fetch, {conv, blank, 0}]),

                    case {Y, X} of
                        {0, 0} -> {array, Rows};
                        {0, _} -> area_util:col(X, {array, Rows});
                        {_, 0} -> area_util:row(Y, {array, Rows});
                        _      ->
                            {ok, Val} = area_util:at(X, Y, {array, Rows}),
                            Val
                    end;
                false ->
                    #rangeref{tl = {{offset, X1}, {offset, Y1}},
                              br = {{offset, X2}, {offset, Y2}}}
                        = area_util:to_relative(Area,
                                                muin:context_setting(col),
                                                muin:context_setting(row)),

                    case {Y, X} of
                        {0, 0} ->
                            Area;
                        {0, _} ->
                            Area#rangeref{tl={{offset,(X1+X)-1},{offset,Y1}},
                                          br={{offset,(X1+X)-1},{offset,Y2}},
                                          width=1, height=Y2-Y1};
                        {_, 0} ->
                            Area#rangeref{tl={{offset,X1},{offset,(Y1+X)-1}},
                                          br={{offset,X2},{offset,(Y1+X)-1}},
                                          width=X2-X1, height=1};
                        _ ->
                            #cellref{ row={offset, (Y1+Y)-1},
                                      col={offset, (X1+X)-1},
                                      path=Area#rangeref.path}
                    end
            end
    end.

match([V1, V2]) ->
    match([V1, V2, 1]);
match([V1, V2, V3]) ->
    MatchType = ?int(V3, [cast_strings, ban_bools, ban_dates, cast_blanks]),
    muin_checks:ensure(MatchType =< 1 andalso MatchType >= -1, ?ERRVAL_NA),

    if ?is_area(V2) ->
            match1(V1, area_util:to_list(V2), MatchType);
       ?is_rangeref(V2) ->
            {range, DList} = muin:fetch(V2),
            match1(V1, lists:concat(DList), MatchType);
       true ->
            ?ERR_VAL
    end.
match1(LookupVal, List, -1) ->
    %% List must be in descending order.
    IsDesc = lists:all(fun({X1, X2}) -> stdfuns_logical:'>'([X1, X2]) end,
                 lists:zip(hslists:init(List), tl(List))),
    muin_checks:ensure(IsDesc, ?ERRVAL_NA),
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
    IsAsc = lists:all(fun({X1, X2}) -> stdfuns_logical:'<'([X1, X2]) end,
                lists:zip(hslists:init(List), tl(List))),
    muin_checks:ensure(IsAsc, ?ERRVAL_NA),
    %% Find the largest value that's <= to LookupVal.
    case find_first(fun(X) -> stdfuns_logical:'<='([X, LookupVal]) end, lists:reverse(List)) of
        {ok, V} -> pos(V, List);
        false   -> ?ERR_NA
    end.

vlookup([V, A, I]) ->
    vlookup([V, A, I, true]);
vlookup([V, IA, I0, IB]) ->

    I = ?int(I0, [cast_strings, cast_bools, ban_blanks, ban_dates]),
    A = if ?is_rangeref(IA) -> muin:fetch(IA);
           true             -> IA
        end,
    B = ?bool(IB, [cast_numbers, cast_dates, cast_blanks, ban_strings]),

    muin_checks:ensure(?is_area(A), ?ERRVAL_REF),
    muin_checks:ensure(I =< area_util:width(A), ?ERRVAL_REF),
    muin_checks:ensure(I >= 1, ?ERRVAL_VAL),

    Row = area_util:col(1, A),

    case find(V, Row, B) of
        0 ->
            ?ERRVAL_NA;
        VIndex ->
            {ok, Ret} = area_util:at(I, VIndex, A),
            Ret
    end.

    %% {Tag, L} = A,
    %% NewA = {Tag, transpose(L)},
    %% hlookup([V, NewA, I, B]).


hlookup([V, A, I]) ->
    hlookup([V, A, I, true]);
hlookup([V, IA, I0, B]) ->

    I = ?int(I0, [cast_strings, cast_bools, ban_blanks, ban_dates]),
    A = if ?is_rangeref(IA) -> muin:fetch(IA);
           true             -> IA
        end,

    muin_checks:ensure(?is_area(A), ?ERRVAL_REF),
    muin_checks:ensure(I =< area_util:height(A), ?ERRVAL_REF),
    muin_checks:ensure(I >= 1, ?ERRVAL_VAL),

    Row = area_util:row(1, A),
    case find(V, Row, B) of
        0 ->
            ?ERRVAL_NA;
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
    case find_first(fun(X) -> stdfuns_logical:'<'([X, Value]) end, lists:reverse(Sorted)) of
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
