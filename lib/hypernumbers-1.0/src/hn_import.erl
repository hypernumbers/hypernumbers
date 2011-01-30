%% @author Dale Harvey
%% @copyright 2008 Hypernumbers Ltd
%% @doc Import external spreadsheets into hypernumbers

-module(hn_import).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([
         json_file/2,
         csv_file/2,
         csv_append/2
        ]).

csv_file(Url, FileName) ->

    Ref = hn_util:url_to_refX(Url),

    % first read the file
    Recs = parse_csv:parse_file(FileName),
    Refs  = make_refs(Recs, Ref),

    % first clear the page
    ok = hn_db_api:clear(Ref, all, nil),
    % now write it 
    [ok = hn_db_api:write_attributes([X]) || X <- Refs],
    ok.
                
csv_append(Url, FileName) ->

    Ref = hn_util:url_to_refX(Url),
    % first read the file
    Recs = parse_csv:parse_file(FileName),
    Refs  = make_append_refs(Recs, Ref),
    
    % now write it 
    [ok = hn_db_api:append_row(X, nil, nil) || X <- Refs],
    ok.
    
json_file(Url, FileName) -> 
    {ok, JsonTxt} = file:read_file(FileName),
    Ref = hn_util:url_to_refX(Url),

    {struct, Json} = hn_util:js_to_utf8(mochijson:decode(JsonTxt)),
    {struct, StyleStrs} = ?pget("styles", Json),
    {struct, Cells} = ?pget("cell", Json),
    {struct, Rows} = ?pget("row", Json),
    {struct, Cols} = ?pget("column", Json),

    Styles = hn_db_api:read_styles_IMPORT(Ref),
    ImportStyles = [make_style_rec(X) || X <- StyleStrs],
    {ImportStyles2, RewriteT} = rewrite_styles(Styles, ImportStyles),
    hn_db_api:write_styles_IMPORT(Ref, ImportStyles2),

    ok = hn_db_api:clear(Ref, all, nil),
    [ rows(Ref, X, RewriteT, row,    fun write_col_row/3) || X <- Rows],
    [ rows(Ref, X, RewriteT, column, fun write_col_row/3) || X <- Cols],
    [ rows(Ref, X, RewriteT, cell,   fun write_cells/3) || X <- Cells],
    ok.

rows(Ref, {Row, {struct, Cells}}, RewriteT, Type, Fun) ->
    [ cells(Ref, Row, X, RewriteT, Type, Fun) || X <- Cells],
    ok.

cells(Ref, Row, {Col, {struct, Attrs}}, RewriteT, Type, Fun) ->
    NRef = Ref#refX{ obj={Type, {ltoi(Col), ltoi(Row)}}},
    Fun(NRef, RewriteT, Attrs),
    ok.

write_col_row(_NRef, _, [])   -> ok;
write_col_row(NRef, _, Attrs) ->
    hn_db_api:write_attributes([{NRef, Attrs}]).    
             
write_cells(Ref, RewriteT, Attrs) ->
    Attrs2 = copy_attrs(Attrs, [], RewriteT, ["merge",
                                              "formula",
                                              "style",
                                              "format",
                                              "input"]),
    hn_db_api:write_attributes([{Ref, Attrs2}]).
    
copy_attrs(_Source, Dest, _RT, []) -> Dest;
copy_attrs(Source, Dest, RT, ["style"=Key|T]) ->
    case proplists:get_value(Key, Source, undefined) of
        undefined -> copy_attrs(Source, Dest, RT, T);
        Idx -> case gb_trees:lookup(Idx, RT) of
                   {value, NIdx} ->
                       copy_attrs(Source, [{Key,NIdx}|Dest], RT, T);
                   _ -> 
                       copy_attrs(Source, [{Key,Idx}|Dest], RT, T)
               end
    end;
copy_attrs(Source, Dest, RT, [Key|T]) ->
    case proplists:get_value(Key, Source, undefined) of
        undefined -> copy_attrs(Source, Dest, RT, T);
        V -> copy_attrs(Source, [{Key,V}|Dest], RT, T)
    end.
            
ltoi(X) ->        
    list_to_integer(X).

rewrite_styles(Styles, ImportStyles) ->
    StyleTree = gb_trees:from_orddict(
                  lists:sort([{MS,Idx} || #style{magic_style = MS, 
                                                 idx = Idx} <- Styles])),
    RewriteF = fun(#style{magic_style=MS, idx=OldIdx}, RT) ->
                       NewIdx = case gb_trees:lookup(MS, StyleTree) of
                                    {value, NI} -> NI;
                                    _ -> util2:get_timestamp()
                                end,
                       gb_trees:insert(OldIdx, NewIdx, RT)
               end,
    RewriteT = lists:foldl(RewriteF, gb_trees:empty(), ImportStyles),
    ImportStyles2 = [S#style{idx = gb_trees:get(OldIdx, RewriteT)} 
                     || S=#style{idx=OldIdx} <- ImportStyles],
    {ImportStyles2, RewriteT}.
    
-spec make_style_rec({string(), string()}) -> #style{}. 
make_style_rec({IdxS, Style}) ->
    L = string:tokens(Style, ";"),
    F = fun(X, MS) ->
                [Key, Val] = string:tokens(X, ":"),
                Pos = ms_util2:get_index(magic_style, Key) + 1,
                setelement(Pos, MS, Val)
        end,
    Idx = list_to_integer(IdxS),
    #style{magic_style = lists:foldl(F, #magic_style{}, L), idx = Idx}.

make_append_refs(List, Ref) -> make_a1(List, Ref, []).

make_a1([], _Ref, Acc)     -> lists:reverse(Acc);
make_a1([H | T], Ref, Acc) ->
    NewRefs = make_a2(tuple_to_list(H), Ref, 1, []),
    make_a1(T, Ref, [NewRefs | Acc]).

make_a2([], _Ref, _C, Acc)    -> Acc;
make_a2([H | T], Ref, C, Acc) ->
    NewRef = Ref#refX{obj={column, {C, C}}},
    make_a2(T, Ref, C + 1, [{NewRef, H} | Acc]).
    
make_refs(List, Ref) -> make_r1(List, Ref, 1, []).

make_r1([], _Ref, _R,  Acc)   -> lists:merge(Acc);
make_r1([H | T], Ref, R, Acc) ->
    NewAcc = make_r2(tuple_to_list(H), Ref, R, 1, []),
    make_r1(T, Ref, R + 1, [NewAcc | Acc]).

make_r2([], _Ref, _R, _C, Acc)   -> Acc;
make_r2([H | T], Ref, R, C, Acc) ->
    NewRef = Ref#refX{obj={cell, {C, R}}},
    make_r2(T, Ref, R, C + 1, [{NewRef, [{"formula", H}]} | Acc]).
