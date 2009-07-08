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
    %{struct, Style} = ?pget("styles", Json),
    {struct, Cells} = ?pget("cell", Json),
    [ rows(Ref, X) || X <- Cells],
    %[ style(Ref, X) || X <- Style],

    %mnesia:dirty_write(hn_db_wu:trans(Ref#refX.site, style_counters),
    %                   {style_counters,Ref,length(Style)}),

    ok.

%style(Ref, {Index, Styles}) ->
%    L = string:tokens(Styles, ";"),
%    F = fun(X, Acc) ->
%                [Key, Val] = string:tokens(X, ":"),
%                [{ms_util2:get_index(magic_style, Key), Val} | Acc]      
%        end,
    
%    Rec = lists:foldl(F, [], L),

%    F2 = fun(X, R) ->
%                 case ?pget(X, R) of
%                     undefined -> [];
%                     Else      -> Else
%                 end
%         end,
    
%    Rec2 = [ F2(X, Rec) || X <- lists:seq(1, 23)],

%    MG = list_to_tuple([magic_style| Rec2]),

%    Tbl = hn_db_wu:trans(Ref#refX.site, styles),
%    mnesia:dirty_write(Tbl, #styles{index=list_to_integer(Index),
%                                    refX = Ref,
%                                    magic_style = MG}),    
%    ok.

rows(Ref, {Row, {struct, Cells}}) ->
    [ cells(Ref, Row, X) || X <- Cells],
    ok.

cells(Ref, Row, {Col, {struct, Attr}}) ->
    NRef = Ref#refX{ obj={cell, {ltoi(Col), ltoi(Row)}}},
    [ write(NRef, X) || X <- Attr],
    ok.

write(_Ref, {"parents", _}) -> ok;
write(_Ref, {"__dependency-tree", _})    -> ok;
write(_Ref, {"overwrite-color", _}) -> ok;
write(_Ref, {"style", _})    -> ok;
write(_Ref, {"format", _})    -> ok;
write(_Ref, {"rawvalue", _}) -> ok;
write(_Ref, {"value", _})    -> ok;
write(Ref, {Key, Val}) ->
    %?INFO("Writing ~p ~p ~p", [Key, Val, Ref]),
    hn_db_api:write_attributes(Ref, [{Key, tos(Val)}]),
    ok.

ltoi(X) ->        
    list_to_integer(X).
    
tos(X) when is_atom(X)    -> atom_to_list(X);
tos(X) when is_integer(X) -> integer_to_list(X);
tos(X) when is_float(X)   -> float_to_list(X);
tos(X) -> X.
