%% @author Dale Harvey
%% @copyright 2008 Hypernumbers Ltd
%% @doc Import external spreadsheets into hypernumbers
-module(hn_import).
-include("hypernumbers.hrl").
-include("spriki.hrl").
-define(pget(Key, List), proplists:get_value(Key, List, undefined)).
-export([ json_file/2 ]).

json_file(Url, FileName) -> 

    Ref = hn_util:parse_url(Url),
    {ok, JsonTxt}    = file:read_file(FileName),
    {struct, Json}   = hn_util:js_to_utf8(mochijson:decode(JsonTxt)),
    {struct, Styles} = ?pget("styles", Json),
    {struct, Cells}  = ?pget("cell", Json),
    {struct, Rows}   = ?pget("row", Json),
    {struct, Cols}   = ?pget("column", Json),

    StyleRecs = [make_style_rec(X) || X <- Styles],

    [ rows(Ref, X, StyleRecs, row,    fun write_col_row/3) || X <- Rows],
    [ rows(Ref, X, StyleRecs, column, fun write_col_row/3) || X <- Cols],
        
    [ rows(Ref, X, StyleRecs, cell,   fun write_cells/3) || X <- Cells],

    ok.

rows(Ref, {Row, {struct, Cells}}, Styles, Type, Fun) ->
    [ cells(Ref, Row, X, Styles, Type, Fun) || X <- Cells],
    ok.

cells(Ref, Row, {Col, {struct, Attrs}}, Styles, Type, Fun) ->
    NRef = Ref#refX{ obj={Type, {ltoi(Col), ltoi(Row)}}},
    Fun(NRef, Attrs, Styles),
    ok.

write_col_row(_NRef, [], _Styles)   -> ok;
write_col_row(NRef, Attrs, _Styles) ->
    ok = hn_db_api:write_attributes(NRef, Attrs).    

write_cells(NRef, Attrs, Styles) ->

    case lists:keyfind("formula", 1, Attrs) of
        false           -> ok;
        {"formula", F1} ->
            ok = hn_db_api:write_attributes(NRef, [{"formula", F1}])
    end,

    case lists:keyfind("format", 1, Attrs) of
        false          -> ok;
        {"format", F2} ->
            ok = hn_db_api:write_attributes(NRef, [{"format", F2}])
    end,

    case lists:keyfind("style", 1, Attrs) of
        {_, SIdx} -> Idx = integer_to_list(SIdx),
                     {Idx, Style} = lists:keyfind(Idx, 1, Styles),
                     ok = hn_db_api:write_style_IMPORT(NRef, Style);
        false     -> ok
    end,
    ok.

ltoi(X) ->        
    list_to_integer(X).
    
% tos(X) when is_atom(X)    -> atom_to_list(X);
% tos(X) when is_integer(X) -> integer_to_list(X);
% tos(X) when is_float(X)   -> float_to_list(X);
% tos(X) -> X.

make_style_rec({Idx, Style}) ->
    L = string:tokens(Style, ";"),
    F = fun(X, Acc) ->
                [Key, Val] = string:tokens(X, ":"),
                [{ms_util2:get_index(magic_style, Key), Val} | Acc]      
        end,
    
    Rec = lists:foldl(F, [], L),
    F2 = fun(X, R) ->
                 case ?pget(X, R) of
                     undefined -> [];
                     Else      -> Else
                 end
         end,

    NoOfFields = ms_util2:no_of_fields(magic_style),
    Rec2 = [ F2(X, Rec) || X <- lists:seq(1, NoOfFields)],
    
    {Idx, list_to_tuple([magic_style| Rec2])}.
