%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       functions for building websites
%%%
%%% @end
%%% Created : 11 Dec 2010 by Gordon Guthrie <>

-module(hnfuns_html).

-include("errvals.hrl").
-include("spriki.hrl").

-export(['html.headline.'/1,
         'html.plainbox.'/1,
         'html.box.'/1,
         'html.alert.'/1,
         'html.ruledbox.'/1,
         'html.menu.'/1,
         'html.submenu'/1,
         'link.box.'/1
         ]).

-export([
         'tim.alert.'/1,
         'tim.box.'/1,
         'tim.plainbox.'/1,
         'tim.ruledbox.'/1,
         'tim.headline.'/1,
         'tim.horizontal.line.'/1,
         'tim.vertical.line.'/1,
         'tim.menu.'/1,
         'tim.submenu'/1,
         'tim.tabs.'/1
        ]).

'link.box.'([H, W, Z]) ->
    'link.box.'([H, W, Z, 0]);
'link.box.'([H, W, Z, Style]) ->
    Z2 = get_z(Z, Style),
    L = [H, W, Z2],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline]) ->
    Z2 = get_z(Z, Style),
    L = [H, W, Headline, Z2],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer]) ->
    Z2 = get_z(Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 0]) ->
    Z2 = get_z(Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("grey", "single", 0, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 1]) ->
    Z2 = get_z(Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("white", "none", 99, "single", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 2]) ->
    Z2 = get_z(Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("white", "none", 0, "none", L);
'link.box.'([H, W, Z, Style, Headline, Footer, 3]) ->
    Z2 = get_z(Z, Style),
    L = [H, W, Headline, Z2, Footer],
    'html.box.1'("white", check_style(0), 0, "none", L);
'link.box.'([H, W, Z, Style, Headline, Footer, BoxType, Alert]) ->
    [BT1] = typechecks:std_ints([BoxType]),
    [Al1] = typechecks:std_ints([Alert]),
    Z2 = get_z(Z, Style),
    L = [H, W, Headline, Z2, Footer],
     case BT1 of
         3 -> 'html.box.1'("grey", "single", Al1, "none", L);
         _ -> ?ERR_VAL
     end.

get_z(Z, Style) ->
    case muin_collect:col([Z], [fetch, fetch_z_debug, blank_as_str],
                          [return_errors]) of
        [{zeds, Matches, _, []}] -> links2(Matches, Style, []);
        [{zeds, _, _, [H | _]}]  -> {error, _, {errval, Err}} = H,
                                    Err
    end.

links2([], 2, Acc)                -> "<table>"
                                         ++ lists:flatten(lists:reverse(Acc))
                                         ++ "</table>";
links2([], _St, Acc)              -> lists:flatten(lists:reverse(Acc));
links2([{{P, _}, _} | T], 0, Acc) -> P2 = hn_util:list_to_path(P),
                                     NewAcc = "<div><a href='" ++ P2 ++ "'>"
                                         ++ P2 ++ "</a></div>",
                                     links2(T, 0, [NewAcc | Acc]);
links2([{{P, _}, V} | T], 1, Acc) -> P2 = hn_util:list_to_path(P),
                                     V2 = case V of
                                              blank -> "";
                                              _     -> tconv:to_s(V)
                                          end,
                                     NewAcc = "<div><a href='" ++ P2 ++ "'>"
                                         ++ V2 ++ "</a></div>",
                                     links2(T, 1, [NewAcc | Acc]);
links2([{{P, _}, V} | T], 2, Acc) -> P2 = hn_util:list_to_path(P),
                                     V2 = case V of
                                              blank -> "";
                                              _     -> tconv:to_s(V)
                                          end,
                                     NewAcc = "<tr><td><a href='" ++ P2 ++ "'>"
                                         ++ P2 ++ "</a></td><td>" ++ V2
                                         ++ "</td></tr>",
                                     links2(T, 2, [NewAcc | Acc]).

'html.headline.'([W, H, Text]) ->
    [W2] = typechecks:throw_std_ints([W]),
    [H2] = typechecks:throw_std_ints([H]),
    [T2] = typechecks:throw_std_strs([Text]),
    check_size(W2, H2),
    {resize, {W2, H2, #incs{}}, "<span class='hn-wc-headline hn-wc-wd-"
     ++ integer_to_list(W2) ++
     " hn-wc-ht-" ++ integer_to_list(H2) ++ "' " ++
     "style='position:absolute;top:25%;left:0%;'>"
     ++ T2 ++ "</span>"}.

'html.ruledbox.'(List) -> 'html.box.1'("white", "none", 99, "single", List).

'html.plainbox.'(List) -> 'html.box.1'("white", "none", 0, "none", List).

'html.box.'(List) -> 'html.box.1'("grey", "single", 0, "none", List).

'html.alert.'([H, W, Style | List]) ->
    'html.box.1'("grey", "single", Style, "none", [H, W | List]).

'html.box.1'(Background, Border, Style, Lines,
             [Width, Height, Headline, Content, Footer]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    check_size(W, H),
    BodyStyle = "hn-wc-ht-body2-" ++ Height,
    [H1] = typechecks:throw_html_box_contents([Headline]),
    {resize, {W, H, #incs{}}, box(Width, Height, Background, Border, Style, Lines,
                                 BodyStyle, [H1, Content, Footer])};
'html.box.1'(Background, Border, Style, Lines, [Width, Height, Headline, Content]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    check_size(W, H),
    BodyStyle = "hn-wc-ht-body1-" ++ Height,
    [H1] = typechecks:throw_html_box_contents([Headline]),
    {resize, {W, H, #incs{}}, box(Width, Height, Background, Border, Style, Lines,
                                 BodyStyle, [H1, Content])};
'html.box.1'(Background, Border, Style, Lines, [Width, Height, Content]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    check_size(W, H),
    BodyStyle = "hn-wc-ht-" ++ Height,
    {resize, {W, H, #incs{}}, box(Width, Height, Background, Border, Style, Lines,
                                 BodyStyle, [Content])}.

check_size(W, H) when W > 0 andalso W < 16 andalso H > 1 andalso H < 26 -> ok;
check_size(_W, _H) -> ?ERR_VAL.

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

'html.submenu'(List) ->
    Rules = [eval_funs, fetch, flatten, {cast, str}],
    Passes = [return_errors],
    [Menu | Subs] = muin_collect:col(List, Rules, Passes),
    SubMenu = "<span>"++Menu++"</span>"++menu1(lists:reverse(Subs), "", []),
    {preview, {"Submenu", 1, 1, #incs{}}, SubMenu}.

'html.menu.'(List) when is_list(List) ->
    [Width | Rest] = List,
    [Width2] = typechecks:throw_std_ints([Width]),
    Strings = typechecks:throw_flat_strs(Rest),
    Menu = menu1(Strings, "potato-menu", []),
    CSS = ["/webcomponents/jquery.ui.potato.menu.css"],
    Js = ["/webcomponents/jquery.ui.potato.menu.js",
              "/webcomponents/hn.webcomponents.js"],
    Js_R = ["HN.WebComponents.reload();"],
    Incs = #incs{css = CSS, js = Js, js_reload = Js_R},
    {preview, {"Type 1 Menu", Width2, 1, Incs}, Menu}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

menu1([], Class, Acc) ->
    Klass = case Class of
                []    -> "";
                Other -> " class="++Other
            end,
    "<ul"++Klass++" style='display:none'>"++lists:flatten(lists:reverse(Acc))++"</ul>";
menu1([H | T], Cl, Acc) ->
    Line = "<li>"++H++"</li>",
    menu1(T, Cl, [Line | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Propsed new components                                                   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'tim.tabs.'([Width, Height | List]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    check_size2(W, H, 6),
    {HeaderTxt, TabTxt} = split(List),
    HeaderTxt2 = typechecks:std_strs(HeaderTxt),
    TabTxt2 = typechecks:std_strs(TabTxt),
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
    {preview, {"Tabs box", W, H, Incs}, HTML}.

split(List) ->
    Len = length(List),
    % needs an even number of parameters
    case Len rem 2 of
        1 -> ?ERR_VAL;
        0 -> unzip(List, [], [])
    end.

'tim.plainbox.'([Width, Height | List]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    Class = "hn_sld_plainbox",
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{js = Js, js_reload = Js_R, css = CSS},
    'tim.box.1'(Class, W, H, List, Incs).

'tim.ruledbox.'([Width, Height | List]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    Class = "hn_sld_ruledbox",
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{js = Js, js_reload = Js_R, css = CSS},
    'tim.box.1'(Class, W, H, List, Incs).

'tim.box.'([Width, Height | List]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    Class = "hn_sld_box",
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{js = Js, js_reload = Js_R, css = CSS},
    'tim.box.1'(Class, W, H, List, Incs).

'tim.alert.'([Width, Height, Style | List]) ->
    [W] = typechecks:throw_std_ints([Width]),
    [H] = typechecks:throw_std_ints([Height]),
    [S] = typechecks:throw_std_ints([Style]),
    check_alerts(S),
    Class = case S of
                0 -> "hn_sld_alert";
                _ -> "hn_sld_alert hn_sld_level" ++ integer_to_list(Style)
            end,
    % always want a header
    List2 = case length(List) of
                1 -> [C] = List,
                     case Style of
                         0 -> [C, "Notice"];
                         1 -> [C, "Alert"];
                         2 -> [C, "Warning"];
                         3 -> [C, "Strong Warning"]
                     end;
                _ -> List
            end,
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{js = Js, js_reload = Js_R, css = CSS},
    'tim.box.1'(Class, W, H, List2, Incs).

'tim.box.1'(Class, W, H, [Content, Headline, Footer], Incs) ->
    [H1] = typechecks:throw_html_box_contents([Headline]),
    [C1] = typechecks:throw_html_box_contents([Content]),
    [F1] = typechecks:throw_html_box_contents([Footer]),
    check_size2(W, H, 8),
    CHBox = "hn_box_height_" ++ integer_to_list(H),
    Class2 = Class ++ " " ++ CHBox,
    CH = "hn_box_height_" ++ integer_to_list(H - 8 + 3),
    Box = "<div class='"  ++ Class2 ++ "'>"
        ++ "<h4>" ++ H1 ++ "</h4>"
        ++ "<div class='" ++ CH ++ "'><p>" ++ C1 ++ "</p></div>"
        ++ "<h6 class='hn_sld_footer'>" ++ F1 ++ "</h6>"
        ++ "</div>",
    {resize, {W, H, Incs}, Box};

'tim.box.1'(Class, W, H, [Content, Headline], Incs) ->
    [H1] = typechecks:throw_html_box_contents([Headline]),
    [C1] = typechecks:throw_html_box_contents([Content]),
    check_size2(W, H, 6),
    CHBox = "hn_box_height_" ++ integer_to_list(H),
    Class2 = Class ++ " " ++ CHBox,
    CH = "hn_box_height_" ++ integer_to_list(H - 6 + 3),
    Box = "<div class='" ++ Class2 ++ "'>"
        ++ "<h4>" ++ H1 ++ "</h4>"
        ++ "<div class='" ++ CH ++ "'><p>" ++ C1 ++ "</p></div>"
        ++ "</div>",
    {resize, {W, H, Incs}, Box};

'tim.box.1'(Class, W, H, [Content], Incs) ->
    [C1] = typechecks:throw_html_box_contents([Content]),
    check_size2(W, H, 3),
    CH = "hn_box_height_" ++ integer_to_list(H),
    Class2 = Class ++ " " ++ CH,
    Box = "<div class='" ++ Class2 ++ "'>"
        ++ "<div class='" ++ CH ++ "'><p>" ++ C1 ++ "</p></div>"
        ++ "</div>",
    {resize, {W, H, Incs}, Box}.

'tim.headline.'([W, Text]) ->
    [W2] = typechecks:throw_std_ints([W]),
    [T2] = typechecks:throw_std_strs([Text]),
    Height = 2,
    check_size(W2, Height),
    Class = "hn_sld_headline" ++ " hn_box_width_" ++ integer_to_list(W2)
        ++ " hn_box_height_2",
    HTML = "<h3 class='" ++ Class ++ "'>" ++ T2 ++ "</h3>",
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{css = CSS, js = Js, js_reload = Js_R},
    {resize, {W2, Height, Incs}, HTML}.

'tim.horizontal.line.'([W]) ->
    [W2] = typechecks:throw_std_ints([W]),
    HTML = "<hr class='hn_sld_hr' />",
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{css = CSS, js = Js, js_reload = Js_R},
    {resize, {W2, 1, Incs}, HTML}.

'tim.vertical.line.'([H]) ->
    [H2] = typechecks:throw_std_ints([H]),
    HTML = "<div class='hn_sld_vertline'></div>",
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{css = CSS, js = Js, js_reload = Js_R},
    {resize, {1, H2, Incs}, HTML}.

'tim.menu.'([W | Rest]) ->
    [W2] = typechecks:throw_std_ints([W]),
    Strings = typechecks:throw_html_box_contents(Rest),
    Menu = 'tim.menu1'(Strings, "hn_sld_menu sld_menu1", []),
    Js   = ["/webcomponents/hn.newwebcomponents.js"],
    Js_R = ["HN.NewWebComponents.reload();"],
    CSS  = ["/webcomponents/newwebcomponents.css"],
    Incs = #incs{js = Js, js_reload = Js_R, css = CSS},
    {preview, {"Menu " ++ hd(Strings), W2, 3, Incs}, Menu}.

'tim.submenu'(List) ->
    [Header | Strings] = typechecks:throw_html_box_contents(List),
    SubMenu = 'tim.submenu1'(Strings, Header, "first_level", []),
    {preview, {"Sub Menu " ++ Header, 1, 1, #incs{}}, SubMenu}.

'tim.menu1'([], Klass, Acc) ->
    "<ul class='" ++ Klass ++ "'>" ++ lists:flatten(lists:reverse(Acc))
++ "</ul>";
'tim.menu1'([H | T], Klass, Acc) ->
    Line = "<li>" ++ H ++ "</li>",
    'tim.menu1'(T, Klass, [Line | Acc]).

'tim.submenu1'([], Header, Klass, Acc) ->
    Header ++ "<ul class='" ++ Klass ++ "'>"
        ++ lists:flatten(lists:reverse(Acc))
        ++ "</ul>";
'tim.submenu1'([H | T], Header,Klass, Acc) ->
    Line = "<li>" ++ H ++ "</li>",
    'tim.submenu1'(T, Header, Klass, [Line | Acc]).

check_alerts(N) when 0 =< N andalso N < 4 -> ok;
check_alerts(_N)                         -> ?ERR_VAL.

check_size2(W, H, MinH) when W > 1 andalso W < 16 andalso H >= MinH andalso H < 26 -> ok;
check_size2(_W, _H, _MinH) -> ?ERR_VAL.

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

