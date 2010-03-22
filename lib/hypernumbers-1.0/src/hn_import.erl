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
    ?INFO("in csv_file URL is ~p~nFileName is ~p~n", [Url, FileName]),

    Ref = hn_util:parse_url(Url),

    % first read the file
    Recs = parse_csv:parse_file(FileName),
    Refs  = make_refs(Recs, Ref),

    % first clear the page
    ok = hn_db_api:clear(Ref, all, nil),
    
    % now write it 
    hn_db_api:write_attributes(Refs).
                
csv_append(Url, FileName) ->

    Ref = hn_util:parse_url(Url),
    % first read the file
    Recs = parse_csv:parse_file(FileName),
    Refs  = make_append_refs(Recs, Ref),
    
    % now write it 
    [ok = hn_db_api:write_last(X, nil, nil) || X <- Refs],
    ok.
    
json_file(Url, FileName) -> 

    % first read the file
    {ok, JsonTxt}    = file:read_file(FileName),

    Ref = hn_util:parse_url(Url),

    % second unpack the json
    {struct, Json}   = hn_util:js_to_utf8(mochijson:decode(JsonTxt)),
    {struct, Styles} = ?pget("styles", Json),
    {struct, Cells}  = ?pget("cell", Json),
    {struct, Rows}   = ?pget("row", Json),
    {struct, Cols}   = ?pget("column", Json),

    StyleRecs = [make_style_rec(X) || X <- Styles],

    % now clear the page
    ok = hn_db_api:clear(Ref, all, nil),

    % finally write out the data
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
    ok = hn_db_api:write_attributes([{NRef, Attrs}]).    

write_cells(NRef, Attrs, Styles) ->

    case lists:keyfind("formula", 1, Attrs) of
        false           -> ok;
        {"formula", F1} ->
            ok = hn_db_api:write_attributes([{NRef, [{"formula", F1}]}])
    end,

    case lists:keyfind("format", 1, Attrs) of
        false          -> ok;
        {"format", F2} ->
            ok = hn_db_api:write_attributes([{NRef, [{"format", F2}]}])
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
