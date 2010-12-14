%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       functions for building websites
%%%
%%% @end
%%% Created : 11 Dec 2010 by Gordon Guthrie <>

-module(hnfuns_html).

-export([
         'html.box1'/1,
         'html.menu1'/1
         ]).

-define(col, muin_collect:col).

'html.box1'([Content]) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    [String] = ?col([Content], Rules, Passes),
    io:format("Content is ~p~n-String is ~p~n", [Content, String]),
    {html, {"Type 1 Box", 40, 120}, "<div>"++String++"</div>"}.

'html.menu1'(List) when is_list(List) ->
    Rules = [eval_funs, fetch, area_first, {cast, str}],
    Passes = [return_errors, {all, fun muin_collect:is_string/1}],
    Strings = ?col(List, Rules, Passes),
    io:format("List is ~p~n-Strings is ~p~n", [List, Strings]),
    {html, {"Type 1 Menu", 40, 120}, "<div>should be a menu</div>"}.
