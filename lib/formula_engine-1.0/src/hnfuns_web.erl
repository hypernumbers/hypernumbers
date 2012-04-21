%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Web Spreadsheet functions
-module(hnfuns_web).

-export([
         blink/1,
         bullets/1,
         'horizontal.line.'/1,
         'vertical.line.'/1,
         include/1,
         'ztable.'/1,
         'table.'/1,
         %background/1,
         link/1,
         img/1,
         'html.'/1,
         'iframe.'/1,
         page/1,
         pageurl/1,
         segment/1,
         site/1,
         siteurl/1,
         'crumb.trail'/1,
         'lorem.ipsum'/1,
         'lorem.headline'/1
        ]).

%-export([fail/1]).

%-export([get_lorem/0]).

-include("spriki.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("muin_proc_dict.hrl").
-include("hypernumbers.hrl").

-define(default_str_rules, [first_array, cast_numbers, cast_bools,
                            cast_blanks, cast_dates ]).

-define(lorem1, "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ").
-define(lorem2, "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ").
-define(lorem3, "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. ").
-define(lorem4, "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. ").
-define(lorem_length, 447).

% ha ha an easter egg
blink([String]) ->
    blink([String, true]);
blink([String, Bool]) ->
    [Str2] = typechecks:std_strs([String]),
    [Bool2] = typechecks:std_bools([Bool]),
    case Bool2 of
        true  -> "<blink>" ++ Str2 ++ "</blink>";
        false -> Str2
    end.

% a wee fun for stevie - not official or supported
bullets(List) ->
    Rules = [eval_funs, fetch, flatten, {cast, str}],
    Passes = [return_errors,
              {all, fun muin_collect:is_string/1}],
    List2 = muin_collect:col(List, Rules, Passes),
    bullets2(List2, []).

bullets2([], Acc)       -> List = lists:reverse(Acc),
                           lists:flatten("<ul>" ++ List ++ "</ul>");
bullets2([[] | T], Acc) -> bullets2(T, Acc);
bullets2([H | T], Acc)  -> bullets2(T, ["<li>" ++ H ++ "</li>" | Acc]).


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
    {resize, {1, H2, #incs{}}, Div}.

'horizontal.line.'([W, N, M, Colour]) ->
    hline1(W, N, M, Colour);
'horizontal.line.'([W, N, M]) ->
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
    {resize, {W2, 1, #incs{}}, Div}.

make_style(I) -> case I of
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

trail2([], Acc) -> lists:flatten(["<a href=\"/\">" ++ ?msite ++
                                  "</a>" | Acc]);
trail2([H | T] = L, Acc) ->
    Path = "/" ++ string:join(lists:reverse(L), "/") ++ "/",
    NewAcc = " -> <a href=\"" ++ Path ++ "\">" ++ H ++ "</a>",
    trail2(T, [NewAcc | Acc]).

%fail([_]) -> [forced_error, "should_wig"].

%% Safe functions (after typecheck)
%%
'html.'([W, H, HTML]) ->
    [Width] = typechecks:throw_std_ints([W]),
    [Height] = typechecks:throw_std_ints([H]),
    [HTML2] = typechecks:throw_std_strs([HTML]),
    {preview, {"Raw HTML", Width, Height, #incs{}}, HTML2}.

'iframe.'([W, H, URL]) ->
    [Width] = typechecks:throw_std_ints([W]),
    [Height] = typechecks:throw_std_ints([H]),
    [URL2] = typechecks:throw_std_strs([URL]),
    IFrame = "<iframe class='hn-wc-ht-" ++ integer_to_list(Height)
        ++ " hn-wc-wd-" ++ integer_to_list(Width) ++ "' src='" ++
        URL2 ++ "' frameborder='0'></iframe>",
    {preview, {"IFrame: " ++ URL2, Width, Height, #incs{}}, IFrame}.

%background_(Url, Rest) ->
%    lists:flatten("<style type='text/css'>body{background:url("
%                  ++ Url ++ ") " ++ Rest ++ "};</style>").

link_(Src, Text, 0, 0) ->
    lists:flatten("<a href='" ++ Src ++ "'>" ++ Text ++ "</a>");
link_(Src, Text, 0, _Option) ->
    lists:flatten("<a href='" ++ Src ++ "' style='text-decoration:none;'>"
                  ++ Text ++ "</a>");
link_(Src, Text, _N, 0) ->
    lists:flatten("<a href='" ++ Src ++ "' target='_blank'>" ++ Text ++ "</a>");
link_(Src, Text, _N, _O) ->
    lists:flatten("<a href='" ++ Src ++ "' target='_blank' "
                  ++ "style='text-decoration:none;'>" ++ Text ++ "</a>").

img_(Src, Alt, Style) ->
    lists:flatten("<img src='" ++ Src ++ "' alt='" ++ Alt
                  ++ "' style ='" ++ Style ++ "' />").

img_(Src, Alt) ->
    lists:flatten("<img src='" ++ Src ++ "' alt='" ++ Alt ++ "'/>").

img_(Src) ->
    lists:flatten("<img src='" ++ Src ++ "' />").

%% Type checking and default values
%%

%% site just returns the site url
site([]) ->
    Site = get(site),
    [_Proto, [$/, $/ | Domain], _Port] = string:tokens(Site, ":"),
    Domain.

siteurl([]) ->
    Site = get(site),
    [Proto, Domain, _Port] = string:tokens(Site, ":"),
    Proto ++ ":" ++ Domain.

segment([]) ->
    case get(path) of
        []   -> "";
        List -> hd(lists:reverse(List))
    end.

pageurl([]) -> site([]) ++ page([]).

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

link([Src, Text, Type, Option]) ->
    [NSrc, NText] = muin_collect:col([Src, Text],
                                     [eval_funs, fetch, {cast, str}],
                                     [return_errors]),
    [NType, NOption] = typechecks:std_ints([Type, Option]),
    link_(NSrc, NText, NType, NOption);
link([Src, Text, Type]) ->
    [NSrc, NText] = muin_collect:col([Src, Text],
                                              [eval_funs, fetch, {cast, str}],
                                              [return_errors]),
    [NType] = typechecks:std_ints([Type]),
    link_(NSrc, NText, NType, 0);
link([Src, Text]) ->
    muin_collect:col([Src, Text], [eval_funs, fetch, {cast, str}],
                     [return_errors],
                     fun([NSrc, NText]) -> link_(NSrc, NText, 0, 0) end).

img([Src, Alt, Effects]) ->
    [Eff2] = typechecks:std_ints([Effects]),
    Style = if
                Eff2 == 0 ->
                    "-moz-box-shadow: 2px 2px 4px #AAA;"
                        ++ "-webkit-box-shadow: 2px 2px 4px #AAA;";
                Eff2 == 1 ->
                    "-moz-box-shadow: 3px 3px 6px #AAA;"
                        ++ "-webkit-box-shadow: 3px 3px 6px #AAA;";
                Eff2 == 2 ->
                    "-moz-box-shadow: 4px 4px 8px #AAA;"
                        ++ "-webkit-box-shadow: 4px 4px 8px #AAA;";
                true -> ?ERR_VAL
            end,
    muin_collect:col([Src, Alt], [eval_funs, fetch, {cast, str}],
                     [return_errors],
                     fun([NSrc, NAlt]) ->
                             img_(NSrc, NAlt, Style) end);

img([Src, Alt]) ->
    muin_collect:col([Src, Alt], [eval_funs, fetch, {cast, str}],
                     [return_errors], fun([NSrc, NAlt]) ->
                                              img_(NSrc, NAlt) end);
img([Src]) ->
    muin_collect:col([Src], [eval_funs, fetch, {cast, str}], [return_errors],
                     fun([NSrc]) -> img_(NSrc) end).


%'twitter.search'([])          -> 'twitter.search'(["hello"]);
%'twitter.search'([Term])      -> 'twitter.search'([Term, "title"]);
%'twitter.search'([Term, Title]) ->
%    muin_collect:col([Term, Title], [eval_funs, fetch, {cast, str}], [return_errors],
%        fun([NTerm, NTitle]) -> 'twitter.search_'(NTerm, NTitle) end).

'ztable.'([W, H, Headers, Z]) ->
    'ztable.'([W, H, Headers, Z, 0, true]);
'ztable.'([W, H, Headers, Z, Sort]) ->
    'ztable.'([W, H, Headers, Z, Sort, true]);
'ztable.'([W, H, Headers, Z, Sort, HasLink]) ->
    [Width] = typechecks:throw_std_ints([W]),
    [Height] = typechecks:throw_std_ints([H]),
    funs_util:check_size(Width, Height),
    Hds1 = lists:reverse(typechecks:html_box_contents([Headers])),
    Hds2 = case HasLink of
               true  -> ["Links" | Hds1];
               false -> Hds1
          end,
    ZRef = case Z of
             List when is_list(List) ->
                   [Z2] = typechecks:std_strs([Z]),
                   case muin:parse(Z2, {?mx, ?my}) of
                       {ok, AST} ->
                           case muin:external_eval(AST) of
                               X when ?is_zcellref(X);
                                      ?is_zrangeref(X) -> X;
                               _Else                   -> ?ERRVAL_REF
                           end;
                       {error, syntax_error} -> ?ERRVAL_REF
                   end;
               ZRange when ?is_zrangeref(ZRange) ->
                   ZRange;
               _Else ->
                   ?ERR_VAL
               end,
    [Sort2] = typechecks:std_ints([Sort]),
    case HasLink of
        true  -> ok;
        false -> ok;
        _     -> ?ERR_VAL
    end,
    put(recompile, true),
    Rules = [fetch_ztable],
    Passes = [],
    [{zeds, Ranges, _, _}] = muin_collect:col([ZRef], Rules, Passes),
    case Ranges of
        % empty table
        [] ->
            {include, {"ZTable ", Width, Height, #incs{}}, ""};
        Ranges ->
            Ranges2 = fix_upzrange(Ranges, HasLink, []),
            Cols1 = length(Hds2),
            Cols2 = length(hd(Ranges2)),
            case Cols1 of
                Cols2 -> table_("ZTable ", Width, Height, [Hds2 | Ranges2],
                                Sort2, true);
                _     -> ?ERRVAL_VAL
            end
    end.

fix_upzrange([], _HasLink, Acc) -> lists:reverse(Acc);
fix_upzrange([H | T], HasLink, Acc) ->
    {Paths, Vals} = lists:unzip(H),
    {Path, _} = hd(Paths),
    Vals2 = lists:reverse(lists:foldl(fun fix_upz2/2, [], Vals)),
    NewAcc = case HasLink of
                 true  -> ["<a href=\"" ++ Path ++"\">Link</a>" | Vals2];
                 false -> Vals2
             end,
   fix_upzrange(T, HasLink, [NewAcc | Acc]).

fix_upz2({errval, Err}, Acc) -> [atom_to_list(Err) | Acc];
fix_upz2(blank, Acc)         -> [""| Acc];
fix_upz2(X, Acc)             -> [tconv:to_s(X) | Acc].

'table.'([W, H, Ref]) ->
    'table.'([W, H, Ref, -99, false]); % negative number means no sorting
'table.'([W, H, #rangeref{height = Len} = Ref, Sort]) ->
    table2(W, H, Len, Ref, Sort, true);
'table.'([W, H, {range, R} = Ref, Sort]) ->
    Len = length(R),
    table2(W, H, Len, Ref, Sort, true);
'table.'([W, H, #rangeref{height = Len} = Ref, Sort, Dirc]) ->
    table2(W, H, Len, Ref, Sort, Dirc);
'table.'([W, H, {range, R} = Ref, Sort, Dirc]) ->
    Len = length(R),
    table2(W, H, Len, Ref, Sort, Dirc).

table2(W, H, Len, Ref, Sort, Dirc) when ?is_rangeref(Ref) ->
    [Width] = typechecks:throw_std_ints([W]),
    [Height] = typechecks:throw_std_ints([H]),
    funs_util:check_size(Width, Height),
    % DIRTY HACK. This forces muin to setup dependencies, and checks
    %% for circ errors.
    Ret = muin:fetch(Ref, "__rawvalue"), % this can be made to work properly now
    case has_circref(Ret) of
        true  -> {errval, '#CIRCREF'};
        false ->
            [Sort2] = typechecks:std_ints([Sort]),
            Ref2 = table_collect(Ref),
            SubLen = trunc(length(Ref2)/Len),
            case make_ref3(Ref2, SubLen, []) of
                [] -> % empty table
                    {include, {"Table ", Width, Height, #incs{}}, ""};
                Ref3 ->
                    [Hd | Body] = Ref3,
                    % negative sort index means reverse the natural order
                    Ref4 = if
                               Sort2 <  0 -> [Hd | lists:reverse(Body)];
                               Sort2 >= 0 -> Ref3
                           end,
                    [Dirc2] = typechecks:std_bools([Dirc]),
                    % users sort from 1 not 0
                    table_("Table ", Width, Height, Ref4, Sort2 - 1, Dirc2)
            end
    end.

%background([Url]) -> background([Url, ""]);
%background([V1, V2]) ->
%    muin_collect:col([V1, V2], [first_array, fetch, {cast,str}],
%        [return_errors, {all, fun muin_collect:is_string/1}],
%        fun([Url, Extra]) -> background_(Url, Extra) end).


include([CellRef]) when ?is_cellref(CellRef) ->
    #cellref{col={offset, X}, row={offset, Y}, path=Path} = CellRef,
    RelRan = #rangeref{type=finite,
                       path=Path,
                       tl = {{offset, X}, {offset, Y}},
                       br = {{offset, X}, {offset, Y}}},
    include([RelRan]);
include([RelRan]) when ?is_rangeref(RelRan) ->
    OldPath = RelRan#rangeref.path,
    OrigPath = get(path),
    NewPath = muin_util:walk_path(OrigPath, OldPath),
    %% DIRTY HACK. This forces muin to setup dependencies, and checks
    %% for circ errors.
    Ret = muin:fetch(RelRan, "__rawvalue"),
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
            Ref = #refX{site = Site, type = url, path = Path, obj = Obj},
            % throw an error if we are trying to bring controls through
            Title = "<div class='hn-include'>Including data from "
                ++ hn_util:list_to_path(NewPath)
                ++ hn_util:obj_to_ref(Obj) ++ "</div>",
            case new_db_wu:has_forms(Ref) of
                false ->  Content = hn_render:content(Ref),
                          {{Html, Width, Height}, _Addons} = Content,
                          {W2, H2} = get_preview(Width, Height),
                          HTML = hn_render:wrap_region(Html, Width, Height),
                          HTML2 = lists:flatten(HTML),
                          {include, {Title, W2, H2, #incs{}}, HTML2};
                true  -> ?ERRVAL_CANTINC
            end
    end.

get_preview(Width, Height) ->
    W = trunc(Width/80),
    H = trunc(Height/22),
    W2 = if
             W == 0 -> 1;
             W  > 0 -> W
         end,
    H2 = if
             H == 0 -> 1;
             H  > 0 -> H
         end,
    {W2, H2}.

has_circref({range, List}) -> has_c1(List).

has_c1([])                                 -> false;
has_c1([[{errval, '#CIRCREF!'} , _] | _T]) -> true;
has_c1([_H | T])                           -> has_c1(T).

table_(Title, W, H, [THead | Range], Sort, Dirc) ->
    Dirc2 = case Dirc of
                true  -> "0";
                false -> "1"
            end,
    Id = "tbl_" ++ muin_util:create_name(),

    Head = ["<thead><tr>",
            [["<th>", X,"</th>"] || X <- THead ],
            "</tr></thead>"],

    Rows = [ ["<tr>", [ ["<td>", Cell,"</td>"] || Cell <- Row ],"</tr>"]
              || Row <- Range ],
    Script = if
                 Sort <  0 ->
                     ["$(\".tablesorter\").tablesorter();",
                     "$(\".tablesorter\").parent().css('overflow', ",
                      "'auto');"];
                 Sort >= 0 ->
                     ["$(\".tablesorter\").tablesorter({ sortList:[[",
                      integer_to_list(Sort), ",", Dirc2, "]]});",
                      "$(\".tablesorter\").parent().css('overflow', ",
                      "'auto');"]
             end,
    Js = "/hypernumbers/jquery.tablesorter.js",
    Script2 = lists:flatten(Script),
    Incs = #incs{js = [Js], js_reload = [Script2]},
    HTML = lists:flatten(["<table id='", Id,"' class='tablesorter'>",
                          Head, Rows, "</table>"]),
    {include, {Title, W, H, Incs}, HTML}.

make_ref3([], _SubLen, Acc) -> lists:reverse(Acc);
make_ref3(List, SubLen, Acc) ->
    {Row, Rest} = lists:split(SubLen, List),
    case is_blank(Row) of
        true  -> make_ref3(Rest, SubLen, Acc);
        false -> make_ref3(Rest, SubLen, [Row | Acc])
    end.

is_blank([])       -> true;
is_blank([[] | T]) -> is_blank(T);
is_blank(_List)    -> false.

table_collect(Ref) ->
    case ?is_rangeref(Ref) of
        false -> ?ERR_VAL;
        true  ->
            Site = ?msite,
            Path = ?mpath,
            NewPath = muin_util:walk_path(Path, Ref#rangeref.path),
            RefX = muin_util:make_refX(Site, NewPath, Ref),
            Attrs = new_db_api:read_attribute(RefX, "value"),
            % this is a bit of a bollox - need to create the actual
            % underlying XRefX's for the range and this next one does it
            % prolly should fix...
            RefXS = new_db_wu:expand_ref(RefX),
            fix_up(RefXS, Attrs)
    end.

sort({{_, _, _, _, {cell, {X1, Y1}}}, _},
     {{_, _, _, _, {cell, {X2, Y2}}}, _}) ->
    if
        Y1 >  Y2 -> false;
        Y1 <  Y2 -> true;
        Y1 == Y2 -> if
                        X1 >  X2 -> false;
                        X1 =< X2 -> true
                    end
    end.

fix_up(RefS, List) ->
    List2 = fix2(lists:sort(RefS), lists:sort(List), []),
    List3 = lists:sort(fun sort/2, List2),
    [X || {_, X} <- List3].

% the database only returns values for which there is an attribute.
% This fun compares what is returned with what oughta've bin and
% sticks in blanks as appropriate...
fix2([], [], Acc) ->
    lists:reverse(Acc);
fix2([H1 | T1], [{H1, _} = H2 | T2], Acc) ->
    fix2(T1, T2, [H2 | Acc]);
fix2([H1 | T1], List, Acc) ->
    fix2(T1, List, [{H1, ""} | Acc]).
