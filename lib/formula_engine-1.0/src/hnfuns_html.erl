%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       functions for building websites
%%%
%%% @end
%%% Created : 11 Dec 2010 by Gordon Guthrie <>

-module(hnfuns_html).

-include("spriki.hrl").
-include("typechecks.hrl").

-export([
         anchor/1,
         'html.panel.'/1,
         'html.headline.'/1,
         'html.plainbox.'/1,
         'html.box.'/1,
         'html.alert.'/1,
         'html.ruledbox.'/1,
         'html.menu.'/1,
         'html.submenu'/1,
         'html.zsubmenu'/1,
         'html.tabs.'/1,
         'link.box.'/1,
         'toggle.views'/1
        ]).

'html.panel.'([W, H, Text, BgCol1, BgCol2, TextColour]) ->
    'html.panel.'([W, H, Text, BgCol1, BgCol2, TextColour, 0]) ;
'html.panel.'([W, H, Text, BgCol1, BgCol2, TextColour, Style]) ->
    'html.panel.'([W, H, Text, BgCol1, BgCol2, TextColour, Style, "#eee"]);
'html.panel.'([W, H, Text, BgCol1, BgCol2, TextColour, Style, BorderColour]) ->
    [W2, H2, S2] = typechecks:std_ints([W, H, Style]),
    [Txt2] = typechecks:std_strs([Text]),
    BgC1 = typechecks:rgbcolours(BgCol1),
    BgC2 = typechecks:rgbcolours(BgCol2),
    TxtC = typechecks:rgbcolours(TextColour),
    BrC = typechecks:rgbcolours(BorderColour),
    Resize = #resize{width = W2, height = H2},
    BaseStyle = "color:" ++ TxtC ++ ";"
        ++ "background:" ++ "-webkit-gradient(linear, 0 40%, 0 70%, from("
        ++ BgC1 ++ "), to(" ++ BgC2 ++ "));"
        ++ "background:" ++ "-moz-linear-gradient(center top, " ++ BgC1
        ++ ", " ++ BgC2 ++ ") repeat scroll 0 0 transparent;"
        ++ "bacground-color:" ++ BgC1 ++ ";"
        ++ "-moz-border-radius:12px;"
        ++ "-webkit-border-radius:12px;"
        ++ "border-radius:12px;",
    Style2 = case S2 of
                 0 -> BaseStyle;
                 1 -> BaseStyle ++ "border:1px solid " ++ BrC ++ ";";
                 2 -> BaseStyle ++ "border:1px solid " ++ BrC ++ ";"
                          ++ "-moz-box-shadow: 2px 2px 4px #AAA;"
                          ++ "-webkit-box-shadow: 2px 2px 4px #AAA;";
                 _ -> ?ERR_VAL
             end,
    HTML = "<div class='hn_html_panel' style='" ++ Style2 ++ "' >"
        ++ Txt2 ++ "</div>",
    JS = ["/webcomponents/hn.htmlpanel.js"],
    Reload = ["HN.HTMLPanel.reload();"],
    Incs = #incs{js = JS, js_reload = Reload},
    #spec_val{val = HTML, resize = Resize, sp_incs = Incs}.

% Chrome and Safari (basically Webkit) won't scroll properly to anchor tags
% that don't have any content - so hoy in a non-breaking space
anchor([Anchor]) ->
    [A2] = typechecks:std_strs([Anchor]),
    A3 = case A2 of
             "#" ++ Rest -> Rest;
             _           -> A2
         end,
    Preview = "Anchor " ++ Anchor,
    HTML = "<a class='hn_anchor' name='" ++ A3 ++ "'>&nbsp;</a>",
    #spec_val{val = HTML, preview = Preview};
anchor([Anchor, Text]) ->
    [A2, Txt2] = typechecks:std_strs([Anchor, Text]),
    A3 = case A2 of
             "#" ++ Rest -> Rest;
             _           -> A2
         end,
    Preview = "Anchor " ++ Anchor,
    HTML = "<a class='hn_anchor' name='" ++ A3 ++ "'>" ++ Txt2 ++ "</a>",
    #spec_val{val = HTML, preview = Preview}.

'toggle.views'([]) ->
    HTML = lists:flatten("<input type='submit' value='' "
                         ++ "class='hn-toggleviews' style='display:none;'/>"),
    JS = ["/webcomponents/hn.toggle.js"],
    Js_R = ["HN.Toggle.reload();"],
    Incs = #incs{js = JS, js_reload = Js_R},
    Preview = "Toggle Views Button",
    Resize = #resize{width = 2, height = 2},
    #spec_val{val = HTML, preview = Preview, resize = Resize, sp_incs = Incs}.

'link.box.'([H, W, Z]) ->
    'link.box.'([H, W, Z, 0]);
'link.box.'([H, W, Z, Style]) ->
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Z2],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline]) ->
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Headline, Z2],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer]) ->
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 0]) ->
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 1]) ->
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("white", "none", 99, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 2]) ->
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("white", "none", 0, "none", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 3]) ->
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("white", check_style(0), 0, "none", L);
'link.box.'([H, W, Z, Style, Headline, Footer, BoxType, Alert]) ->
    [BT1] = typechecks:std_ints([BoxType]),
    [Al1] = typechecks:std_ints([Alert]),
    Z2 = get_z(fun make_links/2, Z, Style),
    L = [H, W, Headline, Z2, Footer],
    case BT1 of
        3 -> 'html.box.1'("grey", "single", Al1, "none", L);
        _ -> ?ERR_VAL
    end.

get_z(Fun, Z, Style) ->
    case muin_collect:col([Z], [fetch, fetch_z_debug, blank_as_str],
                          [return_errors]) of
        [{zeds, Matches, _, []}] -> Fun(Matches, Style);
        [{zeds, _, _, [H | _]}]  -> {error, _, {errval, Err}} = H,
                                    Err
    end.

zsubmenu2([], _, Acc) ->
    Acc2 = lists:sort(Acc),
    {_, Links} = lists:unzip(Acc2),
    "<ul class='first_level'>" ++
        lists:flatten(Links) ++ "</ul>";
zsubmenu2([{{P, _}, _} | T], 0, Acc) ->
    P2 = hn_util:list_to_path(P),
    NewAcc = {P2, "<li><a href='" ++ P2 ++ "'>"
              ++ P2 ++ "</a></li>"},
    zsubmenu2(T, 0, [NewAcc | Acc]);
zsubmenu2([{{P, _}, V} | T], 1, Acc) ->
    P2 = hn_util:list_to_path(P),
    V2 = case V of
             blank -> "<i>blank</i>";
             _     -> tconv:to_s(V)
         end,
    NewAcc = {V2, "<li><a href='" ++ P2 ++ "'>"
              ++ V2 ++ "</a></li>"},
    zsubmenu2(T, 1, [NewAcc | Acc]);
zsubmenu2([{{P, _}, _} | T], 2, Acc) ->
    P2 = hn_util:list_to_path(P),
    V = lists:last(P),
    NewAcc = {V, "<li><a href='" ++ P2 ++ "'>"
              ++ V ++ "</a></li>"},
    zsubmenu2(T, 2, [NewAcc | Acc]).

links2([], 2, Acc) ->
    Acc2 = lists:sort(Acc),
    {_, Links} = lists:unzip(Acc2),
    "<table>" ++ lists:flatten(Links) ++ "</table>";
links2([], _St, Acc) ->
    Acc2 = lists:sort(Acc),
    {_, Links} = lists:unzip(Acc2),
    lists:flatten(Links);
links2([{{P, _}, _} | T], 0, Acc) ->
    P2 = hn_util:list_to_path(P),
    NewAcc = {P2, "<div><a href='" ++ P2 ++ "'>"
              ++ P2 ++ "</a></div>"},
    links2(T, 0, [NewAcc | Acc]);
links2([{{P, _}, V} | T], 1, Acc) ->
    P2 = hn_util:list_to_path(P),
    V2 = case V of
             blank -> "";
             _     -> tconv:to_s(V)
         end,
    NewAcc = {V2, "<div><a href='" ++ P2 ++ "'>"
              ++ V2 ++ "</a></div>"},
    links2(T, 1, [NewAcc | Acc]);
links2([{{P, _}, V} | T], 2, Acc) ->
    P2 = hn_util:list_to_path(P),
    V2 = case V of
             blank -> "";
             _     -> tconv:to_s(V)
         end,
    NewAcc = {P2, "<tr><td><a href='" ++ P2 ++ "'>"
              ++ P2 ++ "</a></td><td>" ++ V2
              ++ "</td></tr>"},
    links2(T, 2, [NewAcc | Acc]).

'html.headline.'([W, H, Text]) ->
    [W2] = typechecks:throw_std_ints([W]),
    [H2] = typechecks:throw_std_ints([H]),
    [T2] = typechecks:throw_std_strs([Text]),
    funs_util:check_size(W2, H2),
    Resize = #resize{width = W2, height = H2},
    HTML =  "<span class='hn-wc-headline hn-wc-wd-"
        ++ integer_to_list(W2) ++
        " hn-wc-ht-" ++ integer_to_list(H2) ++ "' " ++
        "style='position:absolute;top:25%;left:0%;'>"
        ++ T2 ++ "</span>",
    #spec_val{val = HTML, resize = Resize}.

'html.ruledbox.'(List) -> 'html.box.1'("white", "none", 99, "single", List).

'html.plainbox.'(List) -> 'html.box.1'("white", "none", 0, "none", List).

'html.box.'(List) -> 'html.box.1'("grey", "single", 0, "none", List).

'html.alert.'([H, W, Style | List]) ->
    'html.box.1'("grey", "single", Style, "none", [H, W | List]).

'html.box.1'(Background, Border, Style, Lines,
             [Width, Height, Headline, Content, Footer]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    funs_util:check_size(W, H),
    BodyStyle = "hn-wc-ht-body2-" ++ Height,
    [H1] = typechecks:throw_html_box_contents([Headline]),
    Resize = #resize{width = W, height = H},
    HTML = box(Width, Height, Background, Border, Style, Lines,
               BodyStyle, [H1, Content, Footer]),
    #spec_val{val = HTML, resize = Resize};
'html.box.1'(Background, Border, Style, Lines,
             [Width, Height, Headline, Content]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    funs_util:check_size(W, H),
    BodyStyle = "hn-wc-ht-body1-" ++ Height,
    [H1] = typechecks:throw_html_box_contents([Headline]),
    Resize = #resize{width = W, height = H},
    HTML = box(Width, Height, Background, Border, Style, Lines,
               BodyStyle, [H1, Content]),
    #spec_val{val = HTML, resize = Resize};
'html.box.1'(Background, Border, Style, Lines, [Width, Height, Content]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    funs_util:check_size(W, H),
    BodyStyle = "hn-wc-ht-" ++ Height,
    Resize = #resize{width = W, height = H},
    HTML = box(Width, Height, Background, Border, Style, Lines,
               BodyStyle, [Content]),
    #spec_val{val = HTML, resize = Resize}.

box(W, H, Bk, Bd, St, Ln, BodyStyle, [Content]) ->
    [C1] = typechecks:throw_html_box_contents([Content]),
    Style = check_style(St),
    "<div class='hn-wc-wd-"++W++" hn-wc-ht-"++H++
        " hn-wc-box hn-wc-style-"++Style++" hn-wc-border-"++Bd++
        " hn-wc-background-"++Bk++ "hn-wc-line-"++Ln++"'>"++
        "<div class='hn-wc-body "++BodyStyle++"'>"++
        "<div class='hn-wc-inner'>"++C1++"</div></div>"++
        "</div>";
box(W, H, Bk, Bd, St, Ln, BodyStyle, [Headline, Content]) ->
    [H1, C1] = typechecks:throw_html_box_contents([Headline, Content]),
    Style = check_style(St),
    "<div class='hn-wc-wd-"++W++" hn-wc-ht-"++H++
        " hn-wc-box hn-wc-style-"++Style++" hn-wc-border-"++Bd++
        " hn-wc-background-"++Bk++" hn-wc-line-"++Ln++"'>"++
        "<div class='hn-wc-headline'>"++
        "<div class='hn-wc-inner'>"++H1++"</div></div>"++
        "<div class='hn-wc-body "++BodyStyle++"'>"++
        "<div class='hn-wc-inner'>"++C1++"</div></div>"++
        "</div>";
box(W, H, Bk, Bd, St, Ln, BodyStyle, [Headline, Content, Footer]) ->
    [H1, C1, F1] = typechecks:throw_html_box_contents([Headline, Content, Footer]),
    Style = check_style(St),
    "<div class='hn-wc-wd-"++W++" hn-wc-ht-"++H++
        " hn-wc-box hn-wc-style-"++Style++" hn-wc-border-"++Bd++
        " hn-wc-background-"++Bk++" hn-wc-line-"++Ln++"'>"++
        "<div class='hn-wc-headline'>"
        "<div class='hn-wc-inner'>"++H1++"</div></div>"++
        "<div class='hn-wc-body "++BodyStyle++"'>"++
        "<div class='hn-wc-inner'>"++C1++"</div></div>"++
        "<div class='hn-wc-footer'>"
        "<div class='hn-wc-inner'>"++F1++"</div></div>"++
        "</div>".

check_style(St) ->
    [NStyle] = typechecks:throw_std_ints([St]),
    case NStyle of
        0  -> "plain";
        1  -> "alert1";
        2  -> "alert2";
        3  -> "alert3";
        99 -> "ruledbox";
        _ -> ?ERR_VAL
    end.

'html.zsubmenu'([Title, Z]) -> 'html.zsubmenu'([Title, Z, 0]);
'html.zsubmenu'([Title, Z, Style]) ->
    [T2] = typechecks:throw_std_strs([Title]),
    [St2] = typechecks:throw_std_ints([Style]),
    case St2 of
        0 -> z2(T2, Z, St2);
        1 -> z2(T2, Z, St2);
        2 -> z2(T2, Z, St2);
        _ -> ?ERR_VAL
    end.

z2(T2, Z, St2) ->
    Fun = fun(M, S) ->
                  zsubmenu2(M, S, [])
          end,
    Z2 = get_z(Fun, Z, St2),
    SubMenu = "<span>" ++ T2 ++ "</span>" ++ Z2,
    Preview = "Submenu",
    Resize = #resize{width = 1, height = 2},
    #spec_val{val = SubMenu, preview = Preview, resize = Resize}.

'html.menu.'([W | Rest]) ->
    [W2] = typechecks:throw_std_ints([W]),
    Strings = typechecks:throw_html_box_contents(Rest),
    Menu = 'html.menu1'(Strings, "hn_sld_menu sld_menu1", []),
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{js = Js, js_reload = Js_R, css = CSS},
    Preview = "Menu " ++ hd(Strings),
    Resize = #resize{width = W2, height = 3},
    #spec_val{val = Menu, preview = Preview, resize = Resize, sp_incs = Incs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Propsed new components                                                   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'html.tabs.'([Width, Height | List]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    funs_util:check_size2(W, H, 6),
    {HeaderTxt, TabTxt} = split(List),
    HeaderTxt2 = typechecks:std_strs(HeaderTxt),
    TabTxt2 = tabs_cast(TabTxt, []),
    Class = "hn_box_height_" ++ integer_to_list(H - 6 + 3),
    Name = muin_util:create_name(),
    Headers = make_headers(HeaderTxt2, Name, 1, []),
    Tabs = make_tabs(TabTxt2, Name, Class, 1, []),
    HTML = lists:flatten(["<div id='hn_sld_tabs-" ++ Name ++
                          "' class='hn_sld_tabs'>",
                          Headers, Tabs, "</div>"]),
    Js   = ["/webcomponents/hn.newwebcomponents.js",
            "/webcomponents/jquery.tabs.js"],
    Js_R = ["HN.NewWebComponents.reload_tabs();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{css = CSS, js = Js, js_reload = Js_R},
    Preview = "Tabs box",
    Resize = #resize{width = W, height = H},
    #spec_val{val = HTML, preview = Preview, sp_incs = Incs, resize = Resize,
              include = true}.

split(List) ->
    Len = length(List),
    % needs an even number of parameters
    case Len rem 2 of
        1 -> ?ERR_VAL;
        0 -> unzip(List, [], [])
    end.

'html.submenu'(List) ->
    [Header | Strings] = typechecks:throw_html_box_contents(List),
    SubMenu = 'html.submenu1'(Strings, Header, "first_level", []),
    Preview = "Sub Menu " ++ Header,
    Resize = #resize{width = 1, height = 1},
    #spec_val{val = SubMenu, resize = Resize, preview = Preview}.

'html.menu1'([], Klass, Acc) ->
    "<ul class='" ++ Klass ++ "'>" ++ lists:flatten(lists:reverse(Acc))
        ++ "</ul>";
'html.menu1'([H | T], Klass, Acc) ->
    Line = "<li>" ++ H ++ "</li>",
    'html.menu1'(T, Klass, [Line | Acc]).

'html.submenu1'([], Header, Klass, Acc) ->
    Header ++ "<ul class='" ++ Klass ++ "'>"
        ++ lists:flatten(lists:reverse(Acc))
        ++ "</ul>";
'html.submenu1'([H | T], Header,Klass, Acc) ->
    Line = "<li>" ++ H ++ "</li>",
    'html.submenu1'(T, Header, Klass, [Line | Acc]).

check_alerts(N) when 0 =< N andalso N < 4 -> ok;
check_alerts(_N)                         -> ?ERR_VAL.

unzip([], Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)};
unzip([Hdr, Tb | Rest], Acc1, Acc2) ->
    unzip(Rest, [Hdr | Acc1], [Tb | Acc2]).

make_headers([], _Name, _N, Acc) ->
    "<ul>" ++ lists:reverse(Acc) ++ "</ul>";
make_headers([H | T], Name, N, Acc) ->
    NewAcc = "<li><a href='#hn_sld_tabs-" ++ Name ++ "-"
        ++ integer_to_list(N) ++ "'>"
        ++ H ++ "</a></li>",
    make_headers(T, Name, N + 1, [NewAcc | Acc]).

make_tabs([], _Name, _Class, _N, Acc) -> lists:reverse(Acc);
make_tabs([H | T], Name, Class, N, Acc) ->
    NewAcc = "<div id='hn_sld_tabs-" ++ Name ++ "-"
        ++ integer_to_list(N) ++ "' "
        ++ "class='" ++ Class ++ "'>"
        ++ "<p>" ++ H ++ "</p></div>",
    make_tabs(T, Name, Class, N + 1, [NewAcc | Acc]).

make_links(Matches, N) when N == 0 orelse N == 1 orelse N == 2 ->
    links2(Matches, N, []);
make_links(_, _) -> ?ERR_VAL.

tabs_cast([], Acc) -> lists:reverse(Acc);
tabs_cast([Ref | T], Acc) when ?is_cellref(Ref) orelse ?is_rangeref(Ref) ->
    Ret = hnfuns_web:include([Ref]),
    #spec_val{val = NewAcc} = Ret,
    tabs_cast(T, [NewAcc | Acc]);
tabs_cast([H | T], Acc) ->
    [NewAcc] = typechecks:std_strs([H]),
    tabs_cast(T, [NewAcc | Acc]).
