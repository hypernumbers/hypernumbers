%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       functions for building websites
%%%
%%% @end
%%% Created : 11 Dec 2010 by Gordon Guthrie <>

-module(hnfuns_html).

-include("errvals.hrl").

-export([
         'html.block.'/1,
         'html.box.'/1,
         'html.alert.'/1,
         'html.menu1'/1,
         'html.submenu'/1
         ]).

'html.block.'(List) -> 'html.box.1'("white", "none", 0, "single", List).

'html.box.'(List) -> 'html.box.1'("grey", "single", 0, "none", List).

'html.alert.'([H, W, Style | List]) ->
    'html.box.1'("grey", "single", Style, "none", [H, W | List]).

'html.box.1'(Background, Border, Style, Lines,
             [Width, Height, Headline, Content, Footer]) ->
    W = tconv:to_i(Width),
    H = tconv:to_i(Height),
    check_size(W, H),
    BodyStyle = "hn-wc-ht-body2-" ++ Height, 
    [H1] = typechecks:flat_strs([Headline]),
    {preview, {H1, W, H}, box(Width, Height, Background, Border, Style, Lines,
                              BodyStyle, [H1, Content, Footer])};
'html.box.1'(Background, Border, Style, Lines, [Width, Height, Headline, Content]) ->
    W = tconv:to_i(Width),
    H = tconv:to_i(Height),
    check_size(W, H),
    BodyStyle = "hn-wc-ht-body1-" ++ Height, 
    [H1] = typechecks:flat_strs([Headline]),
    {preview, {H1, W, H}, box(Width, Height, Background, Border, Style, Lines,
                              BodyStyle, [H1, Content])};
'html.box.1'(Background, Border, Style, Lines, [Width, Height, Content]) ->
    W = tconv:to_i(Width),
    H = tconv:to_i(Height),
    check_size(W, H),
    BodyStyle = "hn-wc-ht-" ++ Height,
    {preview, {"Box", W, H}, box(Width, Height, Background, Border, Style, Lines,
                                 BodyStyle, [Content])}.

check_size(W, H) when W > 0 andalso W < 13 andalso H > 1 andalso H < 21 -> ok;
check_size(_W, _H) -> ?ERR_VAL.

box(W, H, Bk, Bd, St, Ln, BodyStyle, [Content]) ->
    [C1] = typechecks:flat_strs([Content]),
    Style = check_style(St),
    "<div class='hn-wc-wd-"++W++" hn-wc-ht-"++H++
        " hn-wc-box hn-wc-style-"++Style++" hn-wc-border-"++Bd++
        " hn-wc-background-"++Bk++ "hn-wc-line-"++Ln++"'>"++
        "<div class='hn-wc-body "++BodyStyle++"'>"++
        "<div class='hn-wc-inner'>"++C1++"</div></div>"++
        "</div>";
box(W, H, Bk, Bd, St, Ln, BodyStyle, [Headline, Content]) ->
    [H1, C1] = typechecks:flat_strs([Headline, Content]),
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
    [H1, C1, F1] = typechecks:flat_strs([Headline, Content, Footer]),
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
    [NStyle] = typechecks:std_ints([St]),
    case NStyle of
        0 -> "plain";
        1 -> "alert1";
        2 -> "alert2";
        3 -> "alert3";
        _ -> ?ERR_VAL
    end.

'html.submenu'(List) ->
    Rules = [eval_funs, fetch, flatten, {cast, str}],
    Passes = [return_errors],
    [Menu | Subs] = muin_collect:col(List, Rules, Passes),
    SubMenu = "<span>"++Menu++"</span>"++menu1(lists:reverse(Subs), "", []),
    {preview, {"Submenu", 1, 1}, SubMenu}.

'html.menu1'(List) when is_list(List) ->
    Strings = typechecks:flat_strs(List),    
    Menu = menu1(Strings, "potato-menu", []),
    {preview, {"Type 1 Menu", 1, 1}, Menu}.

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
