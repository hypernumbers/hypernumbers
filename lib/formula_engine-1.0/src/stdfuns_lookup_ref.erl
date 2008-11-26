%%% @doc Lookup and reference functions.
%%% @author <hasan@hypernumbers.com>

-module(stdfuns_lookup_ref).
-export([address/1, index/1]).
-compile(export_all).
-include("typechecks.hrl").
-include("handy_macros.hrl").

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
    "R" ++ tconv:to_s(Row) ++ "C" ++ tconv:to_s(Col);
address1(Row, Col, 2, false) ->
    "R" ++ tconv:to_s(Row) ++ "C[" ++ tconv:to_s(Col) ++ "]";
address1(Row, Col, 3, false) ->
    "R[" ++ tconv:to_s(Row) ++ "]C" ++ tconv:to_s(Col); 
address1(Row, Col, 4, false) ->
    "R[" ++ tconv:to_s(Row) ++ "]C[" ++ tconv:to_s(Col) ++ "]".

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
