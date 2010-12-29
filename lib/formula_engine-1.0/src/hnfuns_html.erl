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

-define(col, muin_collect:col).

'html.box1'([Content]) ->
    [String] = std_string([Content]),
    {html, {"Type 1 Box", 40, 120}, "<div>"++String++"</div>"}.

'html.submenu'(List) ->
    Rules = [eval_funs, fetch, {cast, str}],
    Passes = [return_errors],
    Strings = ?col(List, Rules, Passes),
    menu1(Strings, "", []).

'html.menu1'(List) when is_list(List) ->
    Rules = [eval_funs, fetch, {cast, str}],
    Passes = [return_errors],
    io:format("List is ~p~n", [List]),
    Strings = [?col([X], Rules, Passes) || X <- List],
    io:format("Strings is ~p~n", [Strings]),
    Menu = menu1(Strings, "hn-html-menu", []),
    io:format("Menu is ~p~n", [Menu]),
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
menu1([{range, R} | T], Cl, Acc) ->
    NewA = case length(R) of
               2 -> m_range(R, []);
               _ -> m_range2(R)
           end,
    menu1(T, Cl, [NewA | Acc]);
menu1([Txt, URL | T], Cl, Acc) ->
    Line = "<li><a href='"++URL++"'>"++
        Txt++"</a></li>",
    menu1(T, Cl, [Line | Acc]).

m_range([], Acc) ->
    lists:flatten(lists:reverse(Acc));
m_range([[Txt, URL] | T], Acc) ->
    io:format("Txt is ~p~n", [Txt]),
    Line = "<li><a href='"++URL++"'>"++
        Txt++"</a></li>",
    m_range(T, [Line, Acc]).

m_range2([Txts, URLs]) -> m_range3(Txts, URLs, []).

m_range3([], [], Acc) ->
    lists:flatten(lists:reverse(Acc));
m_range3([Txt | T1], [URL | T2], Acc) ->
    Line = "<li><a href='"++URL++"'>"++
        Txt++"</a></li>",
    m_range3(T1, T2, [Line | Acc]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Some standard collectors and stuff                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

std_string(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    ?col(Vals, Rules, Passes).
 
