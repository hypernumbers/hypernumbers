%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Web Spreadsheet functions
-module(hnfuns_web).

-export([
         'horizontal.line.'/1,
         'vertical.line.'/1,
         include/1,
         table/1,
         background/1,
         'google.map'/1,
         'twitter.search'/1,
         link/1,
         img/1,
         html/1,
         page/1,
         site/1,
         'crumb.trail'/1,
         'lorem.ipsum'/1,
         'lorem.headline'/1
        ]).

%-export([fail/1]).

%-export([get_lorem/0]).

-include("spriki.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-define(default_str_rules, [first_array, cast_numbers, cast_bools,
                            cast_blanks, cast_dates ]).

-define(lorem1, "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ").
-define(lorem2, "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ").
-define(lorem3, "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. ").
-define(lorem4, "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. ").
-define(lorem_length, 447).

-type html() :: string().
-type zoom() :: 1..20.

'vertical.line.'([H, N, M, Colour]) ->
    vline1(H, N, M, Colour);
'vertical.line.'([H, N, M, "#000000"]) ->
    vline1(H, N, M, "#000000");
'vertical.line.'([H, N]) ->
    vline1(H, N, 0, "#000000");
'vertical.line.'([H]) ->
    vline1(H, 1, 0, "#000000").

vline1(H, N, M, Colour) ->
    [H2, N2, M2] = typechecks:std_ints([H, N, M]),
    Col = typechecks:rgbcolours(Colour),
    Style = make_style(M2),
    Div = "<div style='display:block;height:100%;width:50%;border-right:"
        ++ integer_to_list(N2) ++ "px " ++ Style ++ " " ++ Col ++ "'></div>"
        ++ "<div style='display:block;width:50%;'></div>",
    {resize, 1, H2, Div}. 

'horizontal.line.'([W, N, M, Colour]) ->
    hline1(W, N, M, Colour);
'horizontal.line.'([W, N, M, "#000000"]) ->

    hline1(W, N, M, "#000000");
'horizontal.line.'([W, N]) ->
    hline1(W, N, 0, "#000000");
'horizontal.line.'([W]) ->
    hline1(W, 1, 0, "#000000").

hline1(W, N, M, Colour) ->
    [W2, N2, M2] = typechecks:std_ints([W, N, M]),
    Col = typechecks:rgbcolours(Colour),
    Style = make_style(M2),
    Div = "<div style='display:block;height:50%;width:100%;border-bottom:"
        ++ integer_to_list(N2) ++ "px " ++ Style ++ " " ++ Col ++ "'></div>"
        ++ "<div style='display:block;width:100%;'></div>",
    {resize, W2, 1, Div}.

make_style(N) -> case N of
                     0 -> "solid";
                     1 -> "dotted";
                     2 -> "dashed";
                     3 -> "double";
                     4 -> "groove";
                     5 -> "ridge";
                     6 -> "inset";
                     _ -> ?ERR_VAL
                 end. 

'lorem.ipsum'(Vals) ->
    N = typechecks:std_ints(Vals),
    'lorem.ipsum1'(N).

'lorem.ipsum1'([]) -> get_lorem();
'lorem.ipsum1'([N]) when is_integer(N) ->
    Num = trunc(N/?lorem_length),
    Surplus = N rem ?lorem_length,
    L = get_lorem(),
    End = case Surplus of
              0 -> "";
              _ ->
                  Sub = string:sub_string(L, Surplus + 1),
                  [E2 | _R] = string:tokens(Sub, " "),
                  E2
          end,
    string:copies(L, Num) ++ string:left(L, Surplus) ++ End.

get_lorem() ->
    random:seed(now()),
    A = [
         {random:uniform(), ?lorem1},
         {random:uniform(), ?lorem2},
         {random:uniform(), ?lorem3},
         {random:uniform(), ?lorem4}
        ],
    lists:foldl(fun({_, S}, Acc) -> S ++ Acc end, "", lists:sort(A)).

'lorem.headline'(Vals) ->
    N = typechecks:std_ints(Vals),
    'lorem.h1'(N).

'lorem.h1'([])                       -> "Cicatrix Manet";
'lorem.h1'([N]) when N =< 4          -> "Cave";
'lorem.h1'([5])                      -> "Mingo";
'lorem.h1'([6])                      -> "Bombax";
'lorem.h1'([7])                      -> "Valete";
'lorem.h1'([8])                      -> "Salvete";
'lorem.h1'([N]) when N =< 9
                     andalso N =< 10 -> "Disce Pati";
'lorem.h1'([11])                     -> "Dolus Bonus";
'lorem.h1'([N]) when N =< 12
                     andalso N =< 13 -> "Mirable Visu";  
'lorem.h1'([N]) when N =< 13
                     andalso N =< 14 -> "Cicatrix Manet";  
'lorem.h1'([N]) when N =< 15
                     andalso N =< 17 -> "Amor Vincit Omnia";  
'lorem.h1'([N]) when N =< 18
                     andalso N =< 19 -> "Dum Docent, Discunt"; 
'lorem.h1'([N]) when N =< 20
                     andalso N =< 21 -> "Ad Unguem Factus Homo";  
'lorem.h1'([N]) when  N >= 22        -> "Fiat Justitia Ruat Caelum".

'crumb.trail'([]) ->
    trail2(lists:reverse(get(path)), []).

trail2([], Acc) -> lists:flatten(["<a href=\"/\">" ++ get(site) ++
                                  "</a>" | Acc]);
trail2([H | T] = L, Acc) ->
    Path = "/" ++ string:join(lists:reverse(L), "/") ++ "/",
    NewAcc = " -> <a href=\"" ++ Path ++ "\">" ++ H ++ "</a>",
    trail2(T, [NewAcc | Acc]).

%fail([_]) -> [forced_error, "should_wig"].

%% Safe functions (after typecheck)
%%
html([Html]) ->
    Html.

-spec 'google.map_'(number(), number(), zoom()) -> html().
'google.map_'(Lat, Long, Zoom) ->
    Lat2 = muin_util:cast(Lat, str),
    Long2 = muin_util:cast(Long, str),
    HTML = "<iframe width='100%' height='100%' frameborder='0' scrolling='no' "
        ++ "marginheight='0' marginwidth='0' src='http://maps.google.com"
        ++ "/?ie=UTF8&amp;ll=" ++ Lat2 ++ "," ++ Long2
        ++ "&amp;z=" ++ muin_util:cast(Zoom, str) ++ "&amp;output=embed'></iframe>",
    {preview, {"Google Map for Lat: " ++ Lat2 ++ " Long: " ++ Long2, 8, 4}, HTML}.

'twitter.search_'(_Term, _Title) ->
    "Todo".

background_(Url, Rest) ->
    lists:flatten("<style type='text/css'>body{background:url("
                  ++ Url ++ ") " ++ Rest ++ "};</style>").

link_(Src, Text) ->
    lists:flatten("<a href='" ++ Src ++ "'>" ++ Text ++ "</a>").
img_(Src) ->
    lists:flatten("<img src='" ++ Src ++ "' />").

%% Type checking and default values
%%

%% site just returns the site url
site([]) ->
    Site = get(site),
    [Proto, Domain, _Port] = string:tokens(Site, ":"),
    Proto ++ Domain.

page([]) ->
    case get(path) of
        [] -> "/";
        L  -> hn_util:list_to_path(L)
    end;
page([N]) ->
    [N1] = typechecks:std_ints([N]),
    List = get(path),
    Len = length(List),
    if
        N1 >= Len                -> hn_util:list_to_path(List);
        N1 <  1                  -> ?ERRVAL_VAL;
        N1 >= 1 andalso N1 < Len -> L2 = lists:reverse(List),
                                    {Sub, _Rest} = lists:split(N1, L2),
                                    Sub2 = lists:reverse(Sub),
                                    hn_util:list_to_path(Sub2)
    end.

link([Src, Text]) ->
    muin_collect:col([Src, Text], [eval_funs, fetch, {cast, str}], [return_errors],
        fun([NSrc, NText]) -> link_(NSrc, NText) end).

img([Src]) ->
    muin_collect:col([Src], [eval_funs, fetch, {cast, str}], [return_errors],
        fun([NSrc]) -> img_(NSrc) end).


'twitter.search'([])          -> 'twitter.search'(["hello"]);
'twitter.search'([Term])      -> 'twitter.search'([Term, "title"]);
'twitter.search'([Term, Title]) ->
    muin_collect:col([Term, Title], [eval_funs, fetch, {cast, str}], [return_errors],
        fun([NTerm, NTitle]) -> 'twitter.search_'(NTerm, NTitle) end).

table([Ref]) ->
    table([Ref, 0]);
table([#rangeref{height = Len} = Ref, Sort]) ->
    table2(Len, Ref, Sort);
table([{range, R} = Ref, Sort]) ->
    Len = length(R),
    table2(Len, Ref, Sort).

table2(Len, Ref, Sort) ->
    Rules = [eval_funs, fetch, flatten, err_as_str, {cast, str}],
    Passes = [],
    Ref2 = muin_collect:col([Ref], Rules, Passes),
    SubLen = trunc(length(Ref2)/Len),
    Ref3 = make_ref3(Ref2, SubLen, []),
    Sort2 = typechecks:std_strs([Sort]),
    table_(Ref3, Sort2).

'google.map'([])          -> 'google.map'([0]);
'google.map'([Long])      -> 'google.map'([Long, 0]);
'google.map'([Long, Lat]) -> 'google.map'([Long, Lat, 10]);
'google.map'([Long, Lat, Zoom]) ->
    muin_collect:col([Long, Lat, Zoom], [eval_funs, fetch, {cast, num}],
        [return_errors, {all, fun is_number/1}],
        fun([NLong, NLat, NZoom]) ->
                'google.map_'(NLong, NLat, NZoom)
        end).

background([Url]) -> background([Url, ""]);
background([V1, V2]) ->
    muin_collect:col([V1, V2], [first_array, fetch, {cast,str}],
        [return_errors, {all, fun muin_collect:is_string/1}],
        fun([Url, Extra]) -> background_(Url, Extra) end).


include([CellRef]) when ?is_cellref(CellRef) ->
    #cellref{col={offset, X}, row={offset, Y}, path=Path} = CellRef,
    RelRan = #rangeref{type=finite,
                       path=Path,
                       tl = {{offset, X}, {offset, Y}},
                       br = {{offset, X}, {offset, Y}}},
    include([RelRan]);
include([RelRan]) when ?is_rangeref(RelRan) ->
    %% DIRTY HACK. This forces muin to setup dependencies, and checks
    %% for circ errors.
    Ret = muin:fetch(RelRan),
    case has_circref(Ret) of
        true  -> {errval, '#CIRCREF'};
        false ->
            AbsRan = area_util:to_absolute(RelRan, 
                                           muin:context_setting(col),
                                           muin:context_setting(row)),
            #rangeref{path=RelPath, tl = {X1,Y1}, br = {X2,Y2}} = AbsRan,
            Site = muin:context_setting(site),
            Path = muin_util:walk_path(muin:context_setting(path), RelPath),
            Obj = {range, {X1, Y1, X2, Y2}},
            Ref = #refX{site = Site, path = Path, obj = Obj},
            {{Html, Width, Height}, _Addons} = hn_render:content(Ref),
            {include, {"Included Cells", Width, Height},
             lists:flatten(hn_render:wrap_region(Html, Width, Height))}
    end.

has_circref({range, List}) -> has_c1(List).

has_c1([])                                -> false;
has_c1([[{errval, '#CIRCREF!'} , _] | _T]) -> true;
has_c1([_H | T])                          -> has_c1(T).

table_([THead | Range], Sort) ->  
    Id = "tbl_"++muin_util:create_name(),
    
    Head = ["<thead><tr>",
            [["<th>", X,"</th>"] || X <- THead ],
            "</tr></thead>"],
    
    Rows = [ ["<tr>", [ ["<td>", Cell,"</td>"] || Cell <- Row ],"</tr>"]
              || Row <- Range ],
    
    Script = ["<script type='text/javascript'>$(\"#", Id,
              "\").tablesorter({headers: { 1: { sorter:'digit' }}, sortList:[[",
              Sort, ",0]]});</script>"],
    
    lists:flatten(["<table id='", Id,"' class='tablesorter'>", Head, Rows,
                   "</table>", Script]).

make_ref3([], _SubLen, Acc) -> lists:reverse(Acc);
make_ref3(List, SubLen, Acc) ->
    {Row, Rest} = lists:split(SubLen, List),
    make_ref3(Rest, SubLen,[Row | Acc]).
