%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       functions for building websites
%%%
%%% @end
%%% Created : 11 Dec 2010 by Gordon Guthrie <>

-module(hnfuns_html).

-export([
         'html.box1'/1,
         'html.box2'/1,
         'html.box3'/1,
         'html.box4'/1,
         'html.box6'/1,
         'html.box12'/1,
         % 'html.box1p'/1,
         % 'html.box2p'/1,
         % 'html.box3p'/1,
         % 'html.box4p'/1,
         % 'html.box6p'/1,
         % 'html.box12p'/1,
         'html.menu1'/1,
         'html.submenu'/1
         ]).

-define(Width1,  80).
-define(Width2,  160).
-define(Width3,  240).
-define(Width4,  320).
-define(Width6,  480).
-define(Width12, 960).

'html.box1'(List)  -> wrap_box(?Width1,  List).
'html.box2'(List)  -> wrap_box(?Width2,  List).
'html.box3'(List)  -> wrap_box(?Width3,  List).
'html.box4'(List)  -> wrap_box(?Width4,  List).
'html.box6'(List)  -> wrap_box(?Width6,  List).
'html.box12'(List) -> wrap_box(?Width12, List).

% 'html.box1'(List)  -> box(?Width1,  List).
% 'html.box2'(List)  -> box(?Width2,  List).
% 'html.box3'(List)  -> box(?Width3,  List).
% 'html.box4'(List)  -> box(?Width4,  List).
% 'html.box6'(List)  -> box(?Width6,  List).
% 'html.box12'(List) -> box(?Width12, List).

wrap_box(Width, [Content]) ->
        {preview, {"1 Cell Box", Width, 21}, box(Width, [Content])};
wrap_box(Width, [Headline, Content]) ->
        [H1] = typechecks:flat_strs([Headline]),
        {preview, {H1, Width, 21}, box(Width, [H1, Content])};
wrap_box(Width, [Headline, Content, Footer]) ->
        [H1] = typechecks:flat_strs([Headline]),
        {preview, {H1, Width, 21}, box(Width, [H1, Content, Footer])}.

box(Width, [Content]) ->
    [C1] = typechecks:flat_strs([Content]),
    W = tconv:to_s(Width),
    "<div class='hn-wc-wd-"++W++" hn-wc-box'>"++
        "<div class='hn-wc-body'><div class='hn-wc-inner'>"++C1++"</div></div>"++
        "</div>";
box(Width, [Headline, Content]) ->
    [H1, C1] = typechecks:flat_strs([Headline, Content]),
    W = tconv:to_s(Width),
    "<div class='hn-wc-wd-"++W++" hn-wc-box'>"++
        "<div class='hn-wc-headline'><div class='hn-wc-inner'>"++H1++"</div></div>"++
        "<div class='hn-wc-body'><div class='hn-wc-inner'>"++C1++"</div></div>"++
        "</div>";
box(Width, [Headline, Content, Footer]) ->
    [H1, C1, F1] = typechecks:flat_strs([Headline, Content, Footer]),
    W = tconv:to_s(Width),
    "<div class='hn-wc-wd-"++W++" hn-wc-box'>"++
        "<div class='hn-wc-headline'><div class='hn-wc-inner'>"++H1++"</div></div>"++
        "<div class='hn-wc-body'><div class='hn-wc-inner'>"++C1++"</div></div>"++
        "<div class='hn-wc-footer'><div class='hn-wc-inner'>"++F1++"</div></div>"++
        "</div>".

'html.submenu'(List) ->
    Rules = [eval_funs, fetch, flatten, {cast, str}],
    Passes = [return_errors],
    [Menu | Subs] = muin_collect:col(List, Rules, Passes),
    SubMenu = "<span>"++Menu++"</span>"++menu1(lists:reverse(Subs), "", []),
    {preview, {"Submenu", 120, 40}, SubMenu}.

'html.menu1'(List) when is_list(List) ->
    Strings = typechecks:flat_strs(List),    
    Menu = menu1(Strings, "potato-menu", []),
    {preview, {"Type 1 Menu", 120, 40}, Menu}.

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
