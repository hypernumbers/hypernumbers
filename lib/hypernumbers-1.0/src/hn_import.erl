%%% @author Dale Harvey
%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Import external spreadsheets into hypernumbers

-module(hn_import).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([
         testing/0,
         testing2/0
        ]).

-export([
         load_template_file/3,
         etl_to_custom/3,
         % etl_to_row_append/3,
         etl_to_row/3,
         etl_to_sheet/3,
         xls_file/3,
         json_file/2,
         json_file/3,
         csv_file/2,
         csv_append/2,
         save_map/5,
         read_map/2
        ]).

read_map(Site, Name) ->
    ETLDir = hn_util:etlroot(Site),
    FileName = filename:join(ETLDir, Name ++ ".map"),
    case file:consult(FileName) of
        {ok, Terms} ->
            make_json(Terms);
        {error, enonet} -> {error, "map doesn't exist"}
    end.

save_map(Site, Name, Head, Validation, Mapping) ->
    ETLDir = hn_util:etlroot(Site),
    FileName = filename:join(ETLDir, Name ++ ".map"),
    File = lists:concat([[Head], Validation, Mapping]),
    File2 = [io_lib:format("~p.~n", [X]) || X <- File],
    ok = filelib:ensure_dir(FileName),
    ok = file:write_file(FileName, lists:flatten(File2)),
    remoting_reg:notify_site(Site).

%% only used for 'row_append' operations - there is no mapping
%% each row is loaded to the page that appears in column 'A'
%% etl_to_row_append(FileName, Site, Type) ->
%%     Input = case Type of
%%                 csv -> read_csv(FileName);
%%                 xls -> read_xls(FileName)
%%             end,
%%     Chunk = chunk(Input, false), % mebbies do something about header rows?
%%     write_rows(Chunk, Site).

%% write_rows([], _Site) -> ok;
%% write_rows([{_, Rows} | T], Site) ->
%%     ok = write_row(Rows, Site),
%%     write_rows(T, Site).

%% write_row([], _Site) -> ok;
%% write_row([H | T], Site) ->
%%     {_Y, List} = H,
%%     RefXs = make_refXs(Site, List),
%%     ok = new_db_api:append_row(RefXs, nil, nil),
%%     syslib:limiter(Site),
%%     write_row(T, Site).

%% make_refXs(Site, List) ->
%%     {1, Path} = lists:keyfind(1, 1, List),
%%     [make_r2(Site, hn_util:path_tokens(Path), X) || X <- List].

%% make_r2(Site, Path, {1, _}) ->
%%     RefX = #refX{site = Site, type = gurl, path = Path, obj = {column, {1, 1}}},
%%     {RefX, "=now()"};
%% make_r2(Site, Path, {X, Val}) ->
%%     RefX = #refX{site = Site, type = gurl, path = Path, obj = {column, {X, X}}},
%%     {RefX, Val}.

%% used to load a file of pages and apply a template to each of them
load_template_file(File, Template, Site) ->
    {ok, Lines} = hn_util:read_lines(File),
    Lines2 = [string:strip(X, both, 10) || X <- Lines],
    RefXs = make_refXs(Lines2, Site, []),
    Fun = fun(X) ->
                  ok = hn_templates:load_template_if_no_page(X, Template)
                  %syslib:limiter(Site)
          end,
    [Fun(X) || X <- RefXs],
    ok.

%% only used for 'row' type maps (file contains the destination)
%% a 'row' type map has one record per line
etl_to_row(FileName, Site, Map) ->
    case file:consult(Map) of
        {ok, Terms}     -> etl_row(Site, Terms, FileName);
        {error, enoent} -> exit("map " ++ Map ++ " doesn't exist")
    end.

% used for custom etl
etl_to_custom(FileName, Site, Map) ->
    case file:consult(Map) of
        {ok, Terms}     -> etl_custom(Site, Terms, FileName);
        {error, enoent} -> {not_valid, "Map " ++ Map
                            ++ "doesn't exist"}
    end.

% only used for 'sheet' type maps (destination must be supplied)
etl_to_sheet(FileName, Destination, Map) ->
    case file:consult(Map) of
        {ok, Terms}     -> etl_sheet(Terms, FileName, Destination);
        {error, enoent} -> {not_valid, "Map " ++ Map
                            ++ " doesn't exist"}
    end.

etl_custom(Site, Terms, FileName) ->
    {Input, {Head, Templates, _Pages, Validation, Mapping}}
        = get_data(Terms, FileName),
    Type = Head#head.type,
    HasHeaders = Head#head.has_headers,
    Overwrite = Head#head.overwrite,
    Mod = Head#head.pagemod,
    Fn = Head#head.pagefn,
    Args = Head#head.pagefnargs,
    LoadPages = get_load_pages(Mapping),
    case Type of
        "custom" ->
            Chunked = chunk(Input, LoadPages, HasHeaders),
            Chunked2 = get_pages(Chunked, Mod, Fn, Args),
            case custom_validation(Chunked2, Site, Overwrite, Validation,
                                   [], []) of
                {valid, Pages} ->
                    case write2(map_custom(Chunked2, Site, Mapping),
                                Site, Pages, Head#head.overwrite,
                                Head#head.template) of
                        ok ->
                            #templates{templates = Tps} = Templates,
                            ok = load_parent_tps(flatten(Pages, []), Tps, Site);
                        Err ->
                            Err
                    end;
                {not_valid, Msg} ->
                    {not_valid, Msg}
            end;
        Other ->
            {not_valid, "Map has " ++ Other ++ " type not custom type"}
    end.

get_load_pages(Mapping) ->
    hslists:uniq([X || {_, X, _, _} <- Mapping]).

load_parent_tps([], _, _) ->
    ok;
load_parent_tps([H | T], Templates, Site) ->
    ok = load_t2(Templates, H, Site),
    load_parent_tps(T, Templates, Site).

load_t2([], _, _) ->
    ok;
load_t2([{Page1, Template} | T], Page2, Site) ->
    NewPath = muin_util:walk_path(string:tokens(Page2, "/"), Page1),
    RefX = #refX{site = Site, path = NewPath, obj = {page, "/"}},
    ok = hn_templates:load_template_if_no_page(RefX, Template),
    load_t2(T, Page2, Site).

etl_row(Site, Terms, FileName) ->
    {Input, {Head, _Templates, _Pages, Validation, Mapping}}
        = get_data(Terms, FileName),
    Type = Head#head.type,
    HasHeaders = Head#head.has_headers,
    LoadPages = get_load_pages(Mapping),
    case Type of
        "row"   ->
            Chunked = chunk(Input, LoadPages, HasHeaders),
            case validate_rows(Site, Head#head.overwrite,
                               Validation, Chunked) of
                {valid, Pages} ->
                    write2(map_row(Chunked, Site, Mapping),
                           Site, Pages, Head#head.overwrite,
                           Head#head.template);
                {not_valid, Msg} ->
                    {not_valid, Msg}
            end;
        Other ->
            {not_valid, "Map has " ++ Other ++ " type not row type"}
    end.

etl_sheet(Terms, FileName, Dest) ->
    {Input, {Head, _Templates, Pages, Validation, Mapping}}
        = get_data(Terms, FileName),
    Type = Head#head.type,
    case Type of
        "sheet" ->
            case validate_sheet(Validation, Input) of
                valid ->
                    write(map(Mapping, Dest, Pages, Input),
                          Head#head.overwrite,
                          Head#head.template);
                {not_valid, Msg} ->
                    {not_valid, Msg}
            end;
        Other ->
            {not_valid, "Map has " ++ Other ++ " type not sheet type"}
    end.

get_data(Terms, FileName) ->
    {Head, Templates, Pages, Validation, Mapping}
        = split_map(Terms, [], [], [], [], []),
    Input = case Head#head.filetype of
                "csv" -> read_csv(FileName);
                "xls" -> read_xls(FileName)
            end,
    {Input, {Head, Templates, Pages, Validation, Mapping}}.

csv_file(Url, FileName) ->

    Ref = hn_util:url_to_refX(Url),

    % first read the file
    Recs = parse_csv:parse_file(FileName),
    Refs  = make_refs(Recs, Ref),

    % first clear the page
    ok = new_db_api:clear(Ref, all, nil),

    % now write it
    [ok = new_db_api:write_attributes([X]) || X <- Refs],
    ok.

csv_append(Url, FileName) ->

    Ref = hn_util:url_to_refX(Url),
    % first read the file
    Recs = parse_csv:parse_file(FileName),
    Refs  = make_append_refs(Recs, Ref),

    % now write it
    [ok = new_db_api:append_row(X, nil, nil) || X <- Refs],
    ok.

xls_file(Path, FileName, SheetName) ->
    Input = read_xls(FileName),
    Input2 = get_namedsheet(Input, SheetName, Path),
    write(Input2, overwrite, blank).

get_namedsheet(Input, SheetName, URL) ->
    Sh2 = excel_util:esc_tab_name(SheetName),
    case lists:keysearch(Sh2, 1, Input) of
        {value, {Sh2, L}} -> [{URL, L}];
        false             -> exit('no_such_sheetname')
    end.

json_file(Url, FileName) -> json_file(Url, FileName, nil).

json_file(Url, FileName, Uid) ->
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
    ok = hn_web_admin:rpc(not_used, S, "set_champion",
                          [{"path", Path},
                           {"view", Champion}]),
    [ok = set_view(S, Path, X) || X <- Views],

    Styles = make_styles(StyleStrs, []),

    ok = new_db_api:clear(Ref, all, Uid),
    [rows(Ref, X, Styles, row,    fun write_col_row/4, Uid)
     || X <- Rows],
    [rows(Ref, X, Styles, column, fun write_col_row/4, Uid)
     || X <- Cols],

    Cells2 = struct_flatten(Cells, Ref, Styles, []),

    ok = new_write_cells(Cells2, Uid).

make_styles([], Acc) -> Acc;
make_styles([{Idx, Str} | T], Acc) ->
    Idx2 = list_to_integer(Idx),
    List = [list_to_tuple(string:tokens(X, ":"))
            || X <- string:tokens(Str, ";")],
    make_styles(T, [{Idx2, List} | Acc]).

set_view(Site, Path, {View, {struct, Propslist}}) ->
    Everyone = ?pget("everyone", Propslist),
    Groups = ?pget("groups", Propslist),
    ok = hn_web_admin:rpc(not_used, Site, "set_view",
                          [{"path",     Path},
                           {"view",     View},
                           {"groups",   Groups},
                           {"everyone", Everyone}]).

rows(#refX{site = _S} = Ref, {Row, {struct, Cells}}, Styles, Type, Fun, Uid) ->
    Cells2 = lists:sort(fun int_sort/2, Cells),
    %syslib:limiter(S),
    [cells(Ref, Row, X, Styles, Type, Fun, Uid) || X <- Cells2],
    ok.

cells(Ref, Row, {Col, {struct, Attrs}}, Styles, Type, Fun, Uid) ->
    NRef = case Type of
               cell   -> Ref#refX{type = url,  obj = {Type, {ltoi(Col), ltoi(Row)}}};
               row    -> Ref#refX{type = gurl, obj = {Type, {ltoi(Col), ltoi(Row)}}};
               column -> Ref#refX{type = gurl, obj = {Type, {ltoi(Col), ltoi(Row)}}}
           end,
    Fun(NRef, Styles, Attrs, Uid),
    ok.

struct_flatten([], _Ref, _Styles,  Acc) -> Acc;
struct_flatten([{Y, {struct, H}} | T], Ref, Styles, Acc) ->
    NewAcc = struct_f2(H, Y, Ref, Styles, Acc),
    struct_flatten(T, Ref, Styles, NewAcc).

struct_f2([], _Y, _Ref, _Styles, Acc) -> Acc;
struct_f2([{X, {struct, Attrs}} | T], Y, Ref, Styles, Acc) ->
    Obj = {cell, {tconv:to_i(X), tconv:to_i(Y)}},
    NewRef = Ref#refX{obj = Obj},
    Attrs2 = copy_attrs(Attrs, [], Styles, ["merge", "formula", "style",
                                            "format", "input"]),
    struct_f2(T, Y, Ref, Styles, [{NewRef, Attrs2} | Acc]).

new_write_cells(Cells, Uid) ->
    new_db_api:write_attributes(Cells, Uid).

write_col_row(_NRef, _, [], _) ->
    ok;
write_col_row(NRef, _, Attrs, Uid) ->
    new_db_api:write_attributes([{NRef, Attrs}], Uid).

copy_attrs(_Source, Dest, _Styles, []) -> Dest;
copy_attrs(Source, Dest, Styles, ["style" = Key | T]) ->
    case proplists:get_value(Key, Source, undefined) of
        undefined -> copy_attrs(Source, Dest, Styles, T);
        Idx -> case lists:keyfind(Idx, 1, Styles) of
                   {Idx, St} ->
                       copy_attrs(Source, lists:merge([St, Dest]),
                                   Styles, T);
                   false -> copy_attrs(Source, Dest, Styles, T)
               end
    end;
copy_attrs(Source, Dest, RT, [Key|T]) ->
    case {Key, proplists:get_value(Key, Source, undefined)} of
        {_, undefined} ->
            copy_attrs(Source, Dest, RT, T);
        {"input", {struct, V}} ->
            V2 = case lists:sort(V) of
                     [{"select", {array, Array}}] ->
                         {"select", Array};
                     [{"dynamic_select", DS}, {"values", {array, Array}}] ->
                         {"dynamic_select", DS, Array}
                 end,
            copy_attrs(Source, [{Key,V2}|Dest], RT, T);
        {_K, V} ->
            copy_attrs(Source, [{Key,V}|Dest], RT, T)
    end.

ltoi(X) ->
    list_to_integer(X).

make_append_refs(List, Ref) -> make_a1(List, Ref, []).

make_a1([], _Ref, Acc)     -> lists:reverse(Acc);
make_a1([H | T], Ref, Acc) ->
    NewRefs = make_a2(tuple_to_list(H), Ref, 1, []),
    make_a1(T, Ref, [NewRefs | Acc]).

make_a2([], _Ref, _C, Acc)    -> Acc;
make_a2([H | T], Ref, C, Acc) ->
    NewRef = Ref#refX{obj = {column, {C, C}}},
    make_a2(T, Ref, C + 1, [{NewRef, H} | Acc]).

make_refs(List, Ref) -> make_r1(List, Ref, 1, []).

make_r1([], _Ref, _R,  Acc)   -> lists:merge(Acc);
make_r1([H | T], Ref, R, Acc) ->
    NewAcc = make_r2(tuple_to_list(H), Ref, R, 1, []),
    make_r1(T, Ref, R + 1, [NewAcc | Acc]).

make_r2([], _Ref, _R, _C, Acc)   -> Acc;
make_r2([H | T], Ref, R, C, Acc) ->
    NewRef = Ref#refX{obj = {cell, {C, R}}},
    make_r2(T, Ref, R, C + 1, [{NewRef, [{"formula", H}]} | Acc]).

%%%
%%% Internal fns
%%%

read_csv(FileName) ->
    Rows = parse_csv:parse_file(FileName),
    [{"csv", reformat_csv(Rows)}].

read_xls(FileName) ->
    {Cells, _Nm, _Fmt, _CSS, _Warns, _Sheets}
        = filefilters:read(excel, FileName),
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

split_map([], Hd, Tps, P, V, M) ->
    {Hd, Tps, P, V, M};
% there should only be one head record
split_map([#head{} = Head | T], [], Tps, P, V, M) ->
    split_map(T, Head, Tps, P, V, M);
% there should only be one templates record
split_map([#templates{} = Templates | T], Hd, [], P, V, M) ->
    split_map(T, Hd, Templates, P, V, M);
split_map([#validation{} = Vld | T], Hd, Tps, P, V, M) ->
    Sh2 = excel_util:esc_tab_name(Vld#validation.sheet),
    P2 = add(Sh2, P),
    split_map(T, Hd, Tps, P2, [Vld#validation{sheet = Sh2} | V], M);
split_map([#mapping{} = Map | T], Hd, Tps, P, V, M) ->
    Sh2 = excel_util:esc_tab_name(Map#mapping.sheet),
    P2 = add(Sh2, P),
    split_map(T, Hd, Tps, P2, V, [Map#mapping{sheet = Sh2} | M]).

map_custom(Chunks, Site, Map) ->
    map_c2(Chunks, Site, Map, []).

map_c2([], _Site, _Map, Acc) -> Acc;
map_c2([{Sheet, List} | T], Site, Map, Acc) ->
    NewAcc = map_c3(List, Site, Sheet, Map, Acc),
    map_c2(T, Site, Map, NewAcc).

map_c3([], _Site, _Sheet, _Mapping, Acc) ->
    Acc;
map_c3([{_Row, Cells, Path} | T], Site, Sheet, Mapping, Acc) ->
    % use map_r4 now 'cos we have eliminated the custom stuff
    NewAcc = map_c_and_r4(Mapping, Site, Sheet, Cells, Path, Acc),
    map_c3(T, Site, Sheet, Mapping, NewAcc).

map_row(Chunks, Site, Mapping) -> map_r2(Chunks, Site, Mapping, []).

map_r2([], _Site, _Mapping, Acc) -> Acc;
map_r2([{Sheet, H} | T], Site, Mapping, Acc) ->
    NewAcc = map_r3(H, Site, Sheet, Mapping, Acc),
    map_r2(T, Site, Mapping, NewAcc).

map_r3([], _Site, _Sheet, _Mapping, Acc) -> Acc;
map_r3([{_Y, H} | T], Site, Sheet, Mapping, Acc) ->
    Page = case lists:keysearch(1, 1, H) of
               false              -> [];
               {value, {1, Path}} -> Path
           end,
    NewAcc = map_c_and_r4(Mapping, Site, Sheet, H, Page, Acc),
    map_r3(T, Site, Sheet, Mapping, NewAcc).

map_c_and_r4([], _Site, _Sheet, _Cells, _Page, Acc) -> Acc;
map_c_and_r4([{mapping, Sheet, From, To} | T], Site,
       Sheet, Cells, Page, Acc) ->
    {column, {X, X}} = hn_util:parse_ref(From),
    To2 = hn_util:parse_ref(To),
    Path = string:tokens(Page, "/"),
    RefX = #refX{site = Site, type = url, path = Path, obj = To2},
    NewAcc = case lists:keysearch(X, 1, Cells) of
                 false             -> Acc;
                 {value, {X, Val}} -> [{RefX, Val} | Acc]
             end,
    map_c_and_r4(T, Site, Sheet, Cells, Page, NewAcc);
map_c_and_r4([_H | T], Site, Sheet, Cells, Page, Acc) ->
    map_c_and_r4(T, Site, Sheet, Cells, Page, Acc).

map(Map, Dest, Pages, Data) -> map2(Map, Dest, Pages, Data, []).

map2([], _Dest, _Pages, _Input, Acc) -> Acc;
map2([#mapping{} = Map | T], Dest, Pages, Input, Acc) ->
    #mapping{sheet = Sheet, from = From, to = To} = Map,
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
                                     lists:keyreplace(Path, 1, Acc,
                                                      NewTup)
                             end
                     end
             end,
    map2(T, Dest, Pages, Input, NewAcc).

custom_validation([], _S, _O, _V, Acc1, Acc2) ->
    case lists:flatten(lists:merge(Acc2)) of
        []    -> {valid, Acc1};
        Msgs2 -> [M2] = io_lib:format("~s", [Msgs2]),
                 {not_valid, M2}
    end;
custom_validation([{Type, Chunks} | T], S, "overwrite", V, Acc1, Acc2) ->
    {OldChunks, Pages} = lists:unzip([{{X, Y}, Z} || {X, Y, Z} <- Chunks]),
    Checks = val_r1(V, Type, OldChunks, []),
    custom_validation(T, S, "overwrite", V, [Pages | Acc1], [Checks | Acc2]);
custom_validation([{Type, Chunks} | T], S, "dont_overwrite", V, Acc1, Acc2) ->
    {OldChunks, Pages} = lists:unzip([{{X, Y}, Z} || {X, Y, Z} <- Chunks]),
    Checks1 = check_for_custom_dups(Pages, []),
    Checks2 = check_no_custom_page(Pages, S, []),
    Checks3 = val_r1(V, Type, OldChunks, []),
    custom_validation(T, S, "dont_overwrite", V, [Pages | Acc1],
                      [Checks1, Checks2, Checks3 | Acc2]).

validate_rows(S, O, V, Chunked) ->
    {Msgs, Pages} = lists:unzip([validate_row(S, O, V, X) || X <- Chunked]),
    case lists:flatten(lists:merge(Msgs)) of
        []    -> {valid, Pages};
        Msgs2 -> M2 = io_lib:format("~s", [Msgs2]),
                 {not_valid, M2}
    end.

get_pages([{Type, Chunks}], Mod, Fn, Args) ->
    [{Type, get_pages2(Chunks, Mod, Fn, Args, [])}].

get_pages2([], _Mod, _Fn, _Args, Acc) ->
    lists:reverse(Acc);
get_pages2([{N, List} | T], Mod, Fn, Args, Acc) ->
    Args2 = [tconv:b26_to_i(X) || X <- Args],
    Args3 = [lookup(X, List) || X <- Args2],
    Page = erlang:apply(Mod, Fn, Args3),
    Page2 = string:to_lower(Page),
    NewAcc = {N, List, Page2},
    get_pages2(T, Mod, Fn, Args, [NewAcc | Acc]).

lookup(N, List) ->
    case lists:keyfind(N, 1, List) of
        {N, V} -> V;
        false  -> ""
    end.

validate_row(Site, "dont_overwrite", Validation, {Sheet, Chunk}) ->
    Acc = check_no_pages(Chunk, Site, []),
    {Acc2, Pages} = check_uniq_col_a(Chunk, Acc),
    {val_r1(Validation, Sheet, Chunk, Acc2), Pages};
validate_row(_Site, "overwrite", Validation, {Sheet, Chunk}) ->
    {Acc, Pages} = check_uniq_col_a(Chunk, []),
    {val_r1(Validation, Sheet, Chunk, Acc), Pages}.

check_uniq_col_a(List, Acc) ->
    {_, Cells} = lists:unzip(List),
    case check_for_dups(Cells, [], []) of
        {[], Pages}  -> {Acc, Pages};
        {Msg, Pages} -> {[Msg | Acc], Pages}
    end.

check_for_custom_dups([], Acc) ->
    Acc;
check_for_custom_dups([H  | T], Acc) ->
    NewAcc = case lists:member(H, T) of
                 true -> [{duplicate, H} | Acc];
                 false -> Acc
             end,
    check_for_custom_dups(T, NewAcc).

check_for_dups([], Pages, Acc) ->
    {Acc, Pages};
check_for_dups([H | T], Pages, Acc) ->
    {_, [Page | _R]} = lists:unzip(H),
    {NewAcc, NewPages} = has_dups(Page, Pages, Acc),
    check_for_dups(T, NewPages, NewAcc).

has_dups(Page, Pages, Acc) ->
    case lists:member(Page, Pages) of
        true  -> {[Page ++ " is duplicated. " | Acc], Pages};
        false -> {Acc, [Page | Pages]}
    end.

check_no_custom_page([], _Site, Acc) ->
    lists:reverse(Acc);
check_no_custom_page([Page | T], Site, Acc) ->
    Page2 = string:tokens(Page, "/"),
    RefX = #refX{site = Site, path = Page2, obj = {page, "/"}},
    NewAcc = case new_db_api:does_page_exist(RefX) of
                 true  -> P2 = string:join(Page2, "/"),
                          Msg = io_lib:format("page ~p exists~n", [P2]),
                          [Msg | Acc];
                 false -> Acc
    end,
    check_no_custom_page(T, Site, NewAcc).

check_no_pages([], _, Acc) -> Acc;
check_no_pages([{{cell, {1, _}}, Page} | T], Site, Acc) ->
    Page2 = string:tokens(Page, "/"),
    RefX = #refX{site = Site, path = Page2, obj = {page, "/"}},
    NewAcc = case new_db_api:does_page_exist(RefX) of
                 true  -> P2 = string:join(Page2, "/"),
                          Msg = io_lib:format("page ~p exists~n", [P2]),
                          [Msg | Acc];
                 false -> Acc
             end,
    check_no_pages(T, Site, NewAcc);
check_no_pages([_H | T], Site, Acc) -> check_no_pages(T, Site, Acc).

val_r1([], _,  _, Acc) -> Acc;
val_r1([H | T], Sheet, List, Acc) ->
    NewAcc = val_r2(List, Sheet, H, Acc),
    val_r1(T, Sheet, List, NewAcc).

val_r2([], _, _, Acc) -> lists:reverse(Acc);
% if the sheet of the input and the sheet of the constraint match then
% test the constraint
val_r2([H | T], S, #validation{sheet = S, cell = R,
                               constraint = C} = Vld,
       Acc) ->
    {column, {X, X}} = hn_util:parse_ref(tconv:to_s(R)),
    NewAcc = case C of
                 "not_null" -> check_exists(S, H, X, Acc);
                 _          -> check_constraint(S, H, X, C, Acc)
             end,
    val_r2(T, S, Vld, NewAcc);
% if the sheet of the input and the sheet of the constraint don't
% match then who cares
val_r2([_H | T], Sheet, Vld, Acc) ->
    val_r2(T, Sheet, Vld, Acc).

validate_sheet(Validation, Input) -> val_s1(Validation, Input, []).

val_s1([], _Input, [])  -> valid;
val_s1([], _Input, Acc) -> {not_valid, Acc};
val_s1([#validation{} = Vld | T], Input, Acc) ->
    #validation{sheet = Sheet, cell = Ref, constraint = Cons} = Vld,
    Obj = hn_util:parse_ref(tconv:to_s(Ref)),
    NewAcc = case lists:keysearch(Sheet, 1, Input) of
                 false ->
                     case Cons of
                         "not_null" -> [{is_null, Obj} | Acc];
                         _          -> Acc
                     end;
                 {value, {Sheet, List}} ->
                     case lists:keysearch(Obj, 1, List) of
                         false ->
                             case Cons of
                                 "not_null" -> [{is_null, Ref} | Acc];
                                 _          -> Acc
                             end;
                         {value, {Obj, V}}  -> val2(Ref, Cons, V, Acc)
                     end
             end,
    val_s1(T, Input, NewAcc).

% if we are here V is not null
val2(_Obj, "not_null", _V, Acc) -> Acc;
val2(Obj, "is_boolean", V, Acc) ->
    case tconv:to_bool(V) of
        true                   -> Acc;
        false                  -> Acc;
        {error, not_a_boolean} -> [{not_boolean, Obj, V} | Acc]
    end;
val2(Obj, "is_number", V, Acc) ->
    case tconv:to_num(V) of
        {error, nan} -> [{not_a_number, Obj, V} | Acc];
        _            -> Acc
    end;
val2(_Obj, "is_string", V, Acc) when is_list(V) -> Acc;
val2(Obj,  "is_string", V, Acc) -> [{not_a_string, Obj, V} | Acc];
val2(Obj,  "is_date", V, Acc) ->
    case dh_date:parse(V) of
        {error, bad_date} -> [{not_a_date, Obj, V} | Acc];
        _                 -> Acc
    end;
val2(_Obj, V, V, Acc) -> Acc;
val2(Obj, Test, V, Acc) ->
    [{{unknown, Test}, Obj, V} | Acc].

write([], _, _) -> ok;
write(Recs, Overwrite, Template) ->
    Refs = transform_recs(Recs),
    Fun = fun({X, _F} = Ref) ->
                  #refX{obj = {page, "/"}} = X,
                  case {Overwrite, new_db_api:does_page_exist(X)} of
                      {"dont_overwrite", true} -> ok;
                      _                        ->
                          case Template of
                              blank -> ok;
                              _     -> hn_templates:load_template(X, Template)
                          end,
                          % now write it
                          new_db_api:write_attributes([Ref]),
                          ok
                  end
          end,
    [Fun(X) || X <- Refs],
    ok.

write2(Recs, Site, Pages, Overwrite, Template) ->
    Pages2 = [refX_from_page(Site, X) || X <- flatten(Pages, [])],
    case load_templates(Pages2, Overwrite, Template) of
        ok           -> load_records(Recs, Site);
        {error, Msg} -> {error, Msg}
    end.

refX_from_page(Site, Page) ->
    % Postel's Law
    %
    %  be conservative in what you do,
    %  be liberal in what you accept from others.
    %
    % See RFC 793 Section 2.10 http://tools.ietf.org/html/rfc793
    %
    % this will force all paths to be well-behaved
    % (ie start and end with a "/"
    Path = string:tokens(Page, "/"),
    Path2 = hn_util:list_to_path(Path),
    url_parser:make_refX(Site ++ Path2).

flatten([], Acc)      -> Acc;
flatten([H | T], Acc) -> flatten(T, lists:merge([H, Acc])).

load_records(List, _Site) -> Recs = load_r2(List, []),
                            ok = new_db_api:write_attributes(Recs),
                            %syslib:limiter(Site),
                            ok.

load_r2([], Acc)              -> Acc;
load_r2([{RefX, V} | T], Acc) -> load_r2(T, [{RefX, [{"formula", V}]} | Acc]).

load_templates([], _, _) ->
    ok;
load_templates([H | T], "overwrite", Template) ->
    io:format("Loading ~p on ~p~n", [Template, hn_util:refX_to_url(H)]),
    ok = hn_templates:load_template(H, Template),
    ok = load_templates(T, "overwrite", Template);
load_templates([H | T], "dont_overwrite", Template) ->
    #refX{path = P} = H,
    case new_db_api:does_page_exist(H) of
        true  -> {error, hn_util:list_to_path(P) ++ " exists"};
        false -> io:format("Loading ~p on ~p~n",
                           [Template, hn_util:refX_to_url(H)]),
                 ok = hn_templates:load_template(H, Template),
                 load_templates(T, "dont_overwrite", Template)
    end.

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
            Dest ++ Sheet ++ "/";
        {value, {page, Sheet, []}} ->
            Dest;
        {value, {page, Sheet, Path}} ->
            string:join([Dest, Path], "/")
    end.

add(Elem, List) when is_list(List) ->
    case lists:member(Elem, List) of
        true  -> List;
        false -> lists:merge([[Elem], List])
    end.

make_json(Terms) -> col(Terms, [], [], []).

col([], Hd, V, M) -> {"map", {struct,
                      [Hd,
                       {"validation", {array, lists:reverse(V)}},
                       {"mapping",    {array, lists:reverse(M)}}]}};
% should only ever be one head record
col([#head{} = H | T], [], V, M) ->
    col(T, head_to_json(H), V, M);
col([#validation{} = H | T], Hd, V, M) ->
    col(T, Hd, [validation_to_json(H) | V], M);
col([#mapping{} = H | T], Hd, V, M ) ->
    col(T, Hd, V, [mapping_to_json(H) | M]).

head_to_json(#head{type = Ty, filetype = F, template = Tp, overwrite = O}) ->
    {"head", {struct, [{"type", Ty}, {"filetype", F},
                       {"template", Tp}, {"overwrite", O}]}}.

validation_to_json(#validation{sheet = S, cell = C, constraint = Cn}) ->
    {struct, [{"sheet", S}, {"cell", C}, {"constraint", Cn}]}.

mapping_to_json(#mapping{sheet = S, from = F, to = T}) ->
    {struct, [{"sheet", S}, {"from", F}, {"to", T}]}.

check_exists(Sheet, {Y, List}, X, Acc) ->
    case lists:keyfind(X, 1, List) of
        {X, _Val} -> Acc;
        false     -> Ref = util2:make_ref({X, Y}),
                     Msg = Sheet ++ "/" ++ Ref ++ "doesn't exist",
                     [Msg | Acc]
    end.

check_constraint(Sheet, {Y, List}, X, Cons, Acc) ->
    NAcc = case lists:keysearch(X, 1, List) of
               {value, {X, Val}} -> case check_c2(Val, Cons) of
                                        true  -> Acc;
                                        false ->
                                            Ref = util2:make_ref({X, Y}),
                                            Ref2 = Sheet ++ "/" ++ Ref,
                                            [Ref2 ++ " is " ++ Val ++
                                             " not " ++ Cons ++ " " | Acc]
                                    end;
               false             -> Acc
           end,
    NAcc.

% checks if "is_boolean", "is_number", "is_string", "is_date"
check_c2(Val, "is_boolean") ->
    case tconv:to_bool(Val) of
        true                   -> true;
        false                  -> true;
        {error, not_a_boolean} -> false
    end;
check_c2(Val, "is_number") ->
    case tconv:to_num(Val) of
        false         -> false;
        {error, nan}  -> false;
        _Num          -> true
    end;
check_c2(Val, "is_string") when is_list(Val) ->
    true;
check_c2(_Val, "is_string") ->
    false;
check_c2(Val, "is_date") ->
    case dh_date:parse(Val) of
        {{_, _, _}, {_, _, _}} -> true;
        {error, bad_date}      -> false
    end.

chunk(List, LoadPages, HasHeaders) ->
    Chunks = [{Sheet, chunk2(X)} || {Sheet, X} <- List],
    Chunks2 = remove_nonload_pages(Chunks, LoadPages, []),
    case HasHeaders of
        false -> Chunks2;
        true  -> [{X, lists:keydelete(1, 1, Y)} || {X, Y} <- Chunks2]
    end.

chunk2(List) -> L2 = lists:sort(fun row_sort/2, List),
                {{cell, {_X, Y}}, _V} = hd(L2),
                chunk3(L2, Y, Y, [], []).

% gonnae return lists of {X, Val} all tagged with the common Y
% this algorithm assumes the data is sorted in row (ie Y) order
% we thrown the Accumulator1 onto Accumulator2 when we hit a new
% row which is whys we carry over OldY as well as Y so we can look back
chunk3([], _, OldY, Acc1, Acc2) ->
    [{OldY, lists:reverse(Acc1)} | Acc2];
chunk3([{{cell, {X, Y}}, Val} | T], Y, OldY, Acc1, Acc2) ->
    chunk3(T, Y, OldY, [{X, Val} | Acc1], Acc2);
chunk3([{{cell, {X, Y}}, Val}  | T], _Y1, OldY, Acc1, Acc2) ->
    chunk3(T, Y, Y, [{X, Val}], [{OldY, lists:reverse(Acc1)} | Acc2]).

remove_nonload_pages([], _LoadPages, Acc) ->
   Acc;
remove_nonload_pages([{Sheet, Cells} | T], LoadPages, Acc) ->
    NewAcc = case lists:member(Sheet, LoadPages) of
                 true  -> [{Sheet, Cells} | Acc];
                 false -> Acc
             end,
    remove_nonload_pages(T, LoadPages, NewAcc).

row_sort({{cell, {_, Y1}}, _}, {{cell, {_, Y2}}, _}) when Y1 >= Y2 -> true;
row_sort({{cell, {_, Y1}}, _}, {{cell, {_, Y2}}, _}) when Y1 <  Y2 -> false.

int_sort({A, _}, {B, _}) ->
    A1 = list_to_integer(A),
    B1 = list_to_integer(B),
    if A1 >  B1 -> false;
       A1 =< B1 -> true
    end.

make_refXs([], _Site, Acc) -> Acc;
make_refXs([H | T], Site, Acc) ->
    Url = Site ++ H,
    RefX = hn_util:url_to_refX(Url),
    make_refXs(T, Site, [RefX | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

testing() ->
    Dir = code:priv_dir(hypernumbers) ++ "/upload_test/",
    File1 = "map_test.csv",
    File2 = "map_test.xls",
    Path = tconv:to_s(util2:get_timestamp()),
    Dest = "http://hypernumbers.dev:9000/page" ++ Path ++ "/",
    io:format("Test map_test_csv.map~n"),
    etl_to_sheet(Dir ++ File1, Dest, Dir ++ "map_test_csv.map"),
    io:format("Test map_test.map~n"),
    etl_to_sheet(Dir ++ File2, Dest, Dir ++ "map_test.map").

testing2() ->
    Dir = code:priv_dir(hypernumbers) ++ "/upload_test/",
    io:format("Dir is ~p~n", [Dir]),
    %File1 = "row_test.xls",
    File2 = "row_test_csv.csv",
    Site = "http://hypernumbers.dev:9000",
    %io:format("Test row_test.map~n"),
    %etl_to_row(Dir ++ File1, Site, Dir ++ "row_test.map").
    io:format("Test row_test_csv.map~n"),
    etl_to_row(Dir ++ File2, Site, Dir ++ "row_test_csv.map").
