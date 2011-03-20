%% @author Dale Harvey
%% @copyright 2008 Hypernumbers Ltd
%% @doc Import external spreadsheets into hypernumbers

-module(hn_import).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([
         testing/0
        ]).

-export([
         etl/4,
         xls_file/3,
         json_file/2,
         csv_file/2,
         csv_append/2
        ]).

testing() ->
    Dir = "/home/gordon/hypernumbers/lib/hypernumbers-1.0/priv/site_types/sust_adv/etl_maps/",
    File1 = "test.csv",
    File2 = "test.xls",
    Path = tconv:to_s(util2:get_timestamp()),
    Dest = "http://hypernumbers.dev:9000/page" ++ Path ++ "/",
    etl(Dir ++ File1, csv, Dest, Dir ++ "csv.map"),
    etl(Dir ++ File2, xls, Dest, Dir ++ "xls.map").

etl(FileName, FileType, Destination, Map) ->
    case file:consult(Map) of
        {ok, Terms}     -> etl2(Terms, FileName, FileType, Destination);
        {error, enoent} -> exit("map doesn't exist")
    end.

etl2(Terms, FileName, FileType, Dest) ->
    {Pages, Validation, Mapping} = split_map(Terms, [], [], []),
    Input = case FileType of
                csv -> read_csv(FileName);
                xls -> read_xls(FileName)
            end,
    case validate(Validation, Input) of
        valid            -> write(map(Mapping, Dest, Pages, Input));
        {not_valid, Msg} -> {not_valid, Msg}
    end.

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

xls_file(Path, FileName, SheetName) ->
    Input = read_xls(FileName),
    Input2 = get_namedsheet(Input, SheetName, Path),
    write(Input2).

get_namedsheet(Input, SheetName, URL) ->
    Sh2 = excel_util:esc_tab_name(SheetName),
    case lists:keysearch(Sh2, 1, Input) of
        {value, {Sh2, L}} -> [{URL, L}];
        false             -> exit('no_such_sheetname')
    end.

json_file(Url, FileName) ->
    {ok, JsonTxt} = file:read_file(FileName),
    Ref = hn_util:url_to_refX(Url),
    #refX{site = S, path = P} = Ref,

    {struct, Json} = hn_util:js_to_utf8(mochijson:decode(JsonTxt)),

    {struct, StyleStrs} = ?pget("styles", Json),
    {struct, Cells}     = ?pget("cell", Json),
    {struct, Rows}      = ?pget("row", Json),
    {struct, Cols}      = ?pget("column", Json),
    {struct, Perms}     = ?pget("permissions", Json),

    Champion = ?pget("champion", Perms),
    {struct, Views} = ?pget("views", Perms),

    % set the champion
    Path = hn_util:list_to_path(P),
    ok = hn_web_admin:rpc(not_used, S, "set_champion", [{"path", Path},
                                                        {"view", Champion}]),
    [ok = set_view(S, Path, X) || X <- Views],

    Styles = hn_db_api:read_styles_IMPORT(Ref),
    ImportStyles = [make_style_rec(X) || X <- StyleStrs],
    {ImportStyles2, RewriteT} = rewrite_styles(Styles, ImportStyles),
    hn_db_api:write_styles_IMPORT(Ref, ImportStyles2),

    ok = hn_db_api:clear(Ref, all, nil),
    [rows(Ref, X, RewriteT, row,    fun write_col_row/3) || X <- Rows],
    [rows(Ref, X, RewriteT, column, fun write_col_row/3) || X <- Cols],
    [rows(Ref, X, RewriteT, cell,   fun write_cells/3)   || X <- Cells],
    ok.

set_view(Site, Path, {View, {struct, Propslist}}) ->
    Everyone = ?pget("everyone", Propslist),
    Groups = ?pget("groups", Propslist),
    ok = hn_web_admin:rpc(not_used, Site, "set_view", [{"path",     Path},
                                                       {"view",     View},
                                                       {"groups",   Groups},
                                                       {"everyone", Everyone}]).


rows(Ref, {Row, {struct, Cells}}, RewriteT, Type, Fun) ->
    [ cells(Ref, Row, X, RewriteT, Type, Fun) || X <- Cells],
    ok.

cells(Ref, Row, {Col, {struct, Attrs}}, RewriteT, Type, Fun) ->
    NRef = Ref#refX{obj = {Type, {ltoi(Col), ltoi(Row)}}},
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
copy_attrs(Source, Dest, RT, ["style" = Key | T]) ->
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
                       case gb_trees:lookup(MS, StyleTree) of
                           {value, _} -> RT;
                           _          -> NewIdx = util2:get_timestamp(),
                                         gb_trees:insert(OldIdx, NewIdx, RT)
                       end
               end,
    RewriteT = lists:foldl(RewriteF, gb_trees:empty(), ImportStyles),
    Fun = fun(#style{idx = Idx} = X, Acc) ->
                  case gb_trees:lookup(Idx, RewriteT) of
                      {value, Val} -> [X#style{idx = Val} | Acc];
                      none         -> Acc
                  end
          end,
    ImportStyles2 = lists:foldl(Fun, [], ImportStyles),
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

%%%
%%% Internal fns
%%%

read_csv(FileName) ->
    Rows = parse_csv:parse_file(FileName),
    [{"csv", reformat_csv(Rows)}].

read_xls(FileName) ->
    {Cells, _Nm, _Fmt, _CSS, _Warns, _Sheets} =filefilters:read(excel, FileName),
    reformat_xls(Cells).

reformat_csv(Rows) -> ref_csv1(Rows, 1, []).

ref_csv1([], _N, Acc)     -> lists:sort(lists:merge(Acc));
ref_csv1([H | T], N, Acc) -> List = tuple_to_list(H),
                             NewAcc = ref_csv2(List, N, 1, []),
                             ref_csv1(T, N + 1, [NewAcc | Acc]).

ref_csv2([], _N, _M, Acc)     -> Acc;
% drop empty cells
ref_csv2([[] | T], N, M, Acc) -> ref_csv2(T, N, M + 1, Acc);
ref_csv2([H | T], N, M, Acc)  -> NewAcc = {{cell, {M, N}}, H},
                                 ref_csv2(T, N, M + 1, [NewAcc | Acc]).


reformat_xls(Cells) -> ref_xls1(Cells, []).

ref_xls1([], Acc)      -> sort_xls(Acc, []);
ref_xls1([H | T], Acc) ->
    {{{sheet, Sh}, {row_index, N}, {col_index, M}}, V} = H,
    Sh2 = excel_util:esc_tab_name(Sh),
    NewAcc = case lists:keysearch(Sh2, 1, Acc) of
                  {value, {Sh2, L}} ->
                     New = {{cell, {M + 1, N + 1}},
                            normalise(V)},
                     NewT = {Sh2, [New | L]},
                     lists:keyreplace(Sh2, 1, Acc, NewT);
                 false ->
                     NewK = {Sh2, [{{cell, {M + 1, N + 1}},
                                    normalise(V)}]},
                     [NewK | Acc]
             end,
    ref_xls1(T, NewAcc).

sort_xls([], Acc)            -> lists:sort(Acc);
sort_xls([{Sh, L} | T], Acc) -> NewAcc = {Sh, lists:sort(L)},
                                sort_xls(T, [NewAcc | Acc]).

normalise({_, X}) when is_list(X)       -> X;
normalise({value, date, {datetime, X}}) -> tconv:to_s(X);
normalise({_, X})                       -> tconv:to_s(X);
normalise({_, _, X})                    -> tconv:to_s(X).

split_map([], P, V, M) ->
    {P, V, M};
split_map([{page, Sh, A1} | T], P, V, M) ->
    Sh2 = excel_util:esc_tab_name(Sh),
    split_map(T, [{page, Sh2, A1} | P], V, M);
split_map([{validate, Sh, A1, A2} | T], P, V, M) ->
    Sh2 = excel_util:esc_tab_name(Sh),
    split_map(T, P, [{validate, Sh2, A1, A2} | V], M);
split_map([{map, Sh, A1, A2} | T], P, V, M) ->
    Sh2 = excel_util:esc_tab_name(Sh),
    split_map(T, P, V, [{map, Sh2, A1, A2} | M]).

map(Map, Dest, Pages, Data) -> map2(Map, Dest, Pages, Data, []).

map2([], _Dest, _Pages, _Input, Acc) -> Acc;
map2([{map, Sheet, From, To} | T], Dest, Pages, Input, Acc) ->
    NewFrom = hn_util:parse_ref(tconv:to_s(From)),
    NewTo   = hn_util:parse_ref(tconv:to_s(To)),
    Path = get_dest(Dest, Sheet, Pages),
    NewAcc = case lists:keysearch(Sheet, 1, Input) of
                 false ->
                     Acc;
                 {value, {Sheet, List}} ->
                     case lists:keysearch(NewFrom, 1, List) of
                         false ->
                             Acc;
                         {value, {NewFrom, V}} ->
                             New = {NewTo, V},
                             case lists:keysearch(Path, 1, Acc) of
                                 false ->
                                     [{Path, [New]} | Acc];
                                 {value, {Path, AccList}} ->
                                     NewTup = {Path, [New | AccList]},
                                     lists:keyreplace(Path, 1, Acc, NewTup)
                             end
                     end
             end,
    map2(T, Dest, Pages, Input, NewAcc).

validate(Validation, Input) -> val1(Validation, Input, []).

val1([], _Input, [])  -> valid;
val1([], _Input, Acc) -> {not_valid, Acc};
val1([{validate, Sheet, Ref, Val} | T], Input, Acc) ->
    Obj = hn_util:parse_ref(tconv:to_s(Ref)),
    NewAcc = case lists:keysearch(Sheet, 1, Input) of
                 false ->
                     case Val of
                         not_null -> [{is_null, Obj} | Acc];
                         _        -> Acc
                     end;
                 {value, {Sheet, List}} ->
                     case lists:keysearch(Obj, 1, List) of
                         false ->
                             case Val of
                                 not_null -> [{is_null, Ref} | Acc];
                                                   _        -> Acc
                             end;
                         {value, {Obj, V}}  -> val2(Ref, Val, V, Acc)
                     end
             end,
    val1(T, Input, NewAcc).

% if we are here V is not null
val2(_Obj, not_null, _V, Acc) -> Acc;
val2(Obj, is_boolean, V, Acc) ->
    case tconv:to_bool(V) of
        true                   -> Acc;
        false                  -> Acc;
        {error, not_a_boolean} -> [{not_boolean, Obj, V} | Acc]
    end;
val2(Obj, is_number, V, Acc) ->
    case tconv:to_num(V) of
        {error, nan} -> [{not_a_number, Obj, V} | Acc];
        _            -> Acc
    end;
val2(_Obj, is_string, V, Acc) when is_list(V) -> Acc;
val2(Obj, is_string, V, Acc) -> [{not_a_string, Obj, V} | Acc];
val2(Obj, is_date, V, Acc) ->
    case dh_date:parse(V) of
        {error, bad_date} -> [{not_a_date, Obj, V} | Acc];
        _                 -> Acc
    end;
val2(_Obj, V, V, Acc) -> Acc;
val2(Obj, Test, V, Acc) -> [{{unknown, Test}, Obj, V} | Acc].

write(Recs) ->
    Refs = transform_recs(Recs),
    % now write it
    [hn_db_api:write_attributes([X]) || X <- Refs].

transform_recs(Recs) -> trans2(Recs, []).

trans2([], Acc) -> Acc;
trans2([{Path, Cells} | T], Acc) ->
    RefX = hn_util:url_to_refX(Path),
    NewAcc = trans3(Cells, RefX, []),
    trans2(T, lists:merge(NewAcc, Acc)).

trans3([], _RefX, Acc) -> Acc;
trans3([{Cell, Formula} | T], RefX, Acc) ->
    NewAcc = {RefX#refX{obj = Cell}, [{"formula", Formula}]},
    trans3(T, RefX, [NewAcc | Acc]).

get_dest(Dest, Sheet, Pages) ->
    case lists:keysearch(Sheet, 2, Pages) of
        false ->
            string:join([Dest, Sheet], "/");
        {value, {page, Sheet, []}} ->
            Dest;
        {value, {page, Sheet, Path}} ->
            string:join([Dest, Path], "/")
    end.
