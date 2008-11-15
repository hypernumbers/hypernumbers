%%% @doc Lookup and reference functions.
%%% @author <hasan@hypernumbers.com>

-module(stdfuns_lookup_ref).
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
