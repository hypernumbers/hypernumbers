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
    [String] = typechecks:flat_strs([Content]),
    {html, {"Type 1 Box", 120, 40}, "<div>"++String++"</div>"}.

'html.submenu'(List) ->
    Rules = [eval_funs, fetch, flatten, {cast, str}],
    Passes = [return_errors],
    [Menu | Subs] = muin_collect:col(List, Rules, Passes),
    "<span>"++Menu++"</span>"++menu1(Subs, "", []).
    
'html.menu1'(List) when is_list(List) ->
    Strings = typechecks:flat_strs(List),    
    Menu = menu1(Strings, "potato-menu", []),
    {html, {"Type 1 Menu", 120, 40}, Menu}.

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
