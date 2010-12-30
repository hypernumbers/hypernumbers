%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       functions for building websites
%%%
%%% @end
%%% Created : 11 Dec 2010 by Gordon Guthrie <>

-module(hnfuns_html).

-export([
         'html.box1'/1,
         'html.menu1'/1,
         'html.submenu'/1
         ]).

'html.box1'([Content]) ->
    [String] = std_string([Content]),
    {html, {"Type 1 Box", 40, 120}, "<div>"++String++"</div>"}.

'html.submenu'(List) ->
    Rules = [eval_funs, fetch, {cast, str}],
    Passes = [return_errors],
    [Menu | Subs] = muin_collect:col(List, Rules, Passes),
    "<div>"++Menu++"</div>"++menu1(Subs, "", []).
    
'html.menu1'(List) when is_list(List) ->
    Strings = std_string(List),    
    Menu = menu1(Strings, "potato-menu", []),
    {html, {"Type 1 Menu", 40, 120}, Menu}.

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
    "<ul"++Klass++">"++lists:flatten(lists:reverse(Acc))++"</ul>";
menu1([H | T], Cl, Acc) ->
    Line = "<li>"++H++"</li>",
    menu1(T, Cl, [Line | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Some standard collectors and stuff                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

std_string(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).
 
