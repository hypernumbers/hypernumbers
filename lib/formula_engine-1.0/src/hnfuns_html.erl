%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       functions for building websites
%%%
%%% @end
%%% Created : 11 Dec 2010 by Gordon Guthrie <>

-module(hnfuns_html).

-export([
         'html.box1'/1,
         %'html.menu1'/1,
         'html.menu2'/1,
         'html.submenu'/1
         ]).

'html.box1'([Content]) ->
    [String] = std_string([Content]),
    {html, {"Type 1 Box", 40, 120}, "<div>"++String++"</div>"}.

'html.submenu'(List) ->
    Rules = [eval_funs, fetch, {cast, str}],
    Passes = [return_errors],
    [Menu | Subs] = muin_collect:col(List, Rules, Passes),
    Ret = Menu++menu2(Subs, "", []),
    io:format("Ret from submenu is ~p~n", [Ret]),
    Ret.

'html.menu2'(List) when is_list(List) ->
    io:format("in html.menu2 with List of ~p~n", [List]),
    Strings = std_string(List),    
    io:format("Strings is ~p~n", [Strings]),
    Menu = menu2(Strings, "hn-html-menu", []),
    {html, {"Type 2 Menu", 40, 120}, Menu}.

%% 'html.menu1'(List) when is_list(List) ->
%%     io:format("arrived in menu1~n"),
%%     Rules = [eval_funs, fetch, {cast, str}],
%%     Passes = [return_errors],
%%     io:format("List is ~p~n", [List]),
%%     %Strings = [muin_collect:col([X], Rules, Passes) || X <- List],
%%     Strings = muin_collect:col(List, Rules, Passes),
%%     io:format("Strings is ~p~n", [Strings]),
%%     Menu = menu1(Strings, "hn-html-menu", []),
%%     io:format("Menu is ~p~n", [Menu]),
%%     {html, {"Type 1 Menu", 40, 120}, Menu}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

menu2([], Class, Acc) -> 
    io:format("in menu2 (1)~n"),
    Klass = case Class of
                []    -> "";
                Other -> " class="++Other
            end,
    "<ul"++Klass++">"++lists:flatten(lists:reverse(Acc))++"</ul>";
menu2([H | T], Cl, Acc) ->
    io:format("In menu2 (2)~n"),
    Line = "<li>"++H++"</li>",
    menu2(T, Cl, [Line | Acc]).

%% menu1([], Class, Acc) ->
%%     io:format("in menu1 (1)~n"),
%%     Klass = case Class of
%%                 []    -> "";
%%                 Other -> " class="++Other
%%             end,
%%     "<ul"++Klass++">"++lists:flatten(lists:reverse(Acc))++"</ul>";
%% menu1([{range, R} | T], Cl, Acc) ->
%%     io:format("In menu1 (2) length is ~p~n-R us ~p~n", [length(R), R]),
%%     NewA = case length(R) of
%%                2 -> m_range(R, []);
%%                _ -> m_range2(R, [])
%%            end,
%%     menu1(T, Cl, [NewA | Acc]);
%% menu1([Txt, URL | T], Cl, Acc) ->
%%     io:format("In menu1 (3)~n"),
%%     Line = "<li><a href='"++URL++"'>"++
%%         Txt++"</a></li>",
%%     menu1(T, Cl, [Line | Acc]).

%% m_range([], Acc) ->
%%     "<ul>"++lists:flatten(lists:reverse(Acc))++"</ul>";
%% m_range([[Txt, URL] | T], Acc) ->
%%     io:format("Txt is ~p~n", [Txt]),
%%     Line = "<li><a href='"++URL++"'>"++
%%         Txt++"</a></li>",
%%     m_range(T, [Line, Acc]).

%% m_range2([], Acc) ->
%%     lists:flatten(lists:reverse(Acc));
%% m_range2([[Txt, URL] | T], Acc) ->
%%     Line = "<li><a href='"++URL++"'>"++
%%         Txt++"</a></li>",
%%     m_range2(T, [Line | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Some standard collectors and stuff                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

std_string(Vals) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    muin_collect:col(Vals, Rules, Passes).
 
