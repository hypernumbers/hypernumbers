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
check_size(_W, _H)                                                      -> ?ERR_VAL.

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
