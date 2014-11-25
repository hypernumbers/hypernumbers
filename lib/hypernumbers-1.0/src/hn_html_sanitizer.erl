%%% @author    Gordon Guthrie
%%% @copyright (C) 2012-2014, Hypernumbers Ltd
%%% @doc       This document does basic sanitation for
%%%            html coming in from the rich text editor
%%%
%%% @end
%%% Created :  7 Mar 2012 by gordon@hypernumbers.com

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(hn_html_sanitizer).

-export([
         is_sane/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(DBL, $").
-define(SNGL, $').

-spec is_sane(string) -> boolean().
is_sane(List) ->
    case re:run(List, "^=html\.[0-9]+x[0-9]+\\(") of
        {match, _} -> is_s2(List, []);
        _          -> false
    end.

is_s2([], Acc) ->
    is_s3(Acc, []);
is_s2([$< | _Rest] = L, Acc) ->
    {Token, Rest2} = retrieve(token, L),
    is_s2(Rest2, [{token, string:to_lower(Token)} | Acc]);
is_s2([?DBL | _Rest] = L, Acc) ->
    {String, Rest2} = retrieve(?DBL, L),
    is_s2(Rest2, [{string, String} | Acc]);
is_s2([?SNGL | _Rest] = L, Acc)->
    {String, Rest2} = retrieve(?SNGL, L),
    is_s2(Rest2, [{string, String} | Acc]);
is_s2([H | T], Acc) ->
    is_s2(T, [{char, H} | Acc]).

is_s3([], Acc) ->
    {true, lists:flatten(Acc)};
is_s3([{char, C}| T], Acc) ->
    is_s3(T, [C | Acc]);
is_s3([{string, S} | T], Acc) ->
    is_s3(T, [S | Acc]);
% these are the only valid tokens
is_s3([{token, "<font>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<font " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</font>"} | T], Acc) ->
    is_s3(T, ["</font>" | Acc]);

is_s3([{token, "<div>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<div " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</div>"} | T], Acc) ->
    is_s3(T, ["</div>" | Acc]);

is_s3([{token, "<span>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<span " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</span>"} | T], Acc) ->
    is_s3(T, ["</span>" | Acc]);

is_s3([{token, "<h1>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<h1 " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</h1>"} | T], Acc) ->
    is_s3(T, ["</h1>" | Acc]);

is_s3([{token, "<h2>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<h2 " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</h2>"} | T], Acc) ->
    is_s3(T, ["</h2>" | Acc]);

is_s3([{token, "<h3>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<h3 " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</h3>"} | T], Acc) ->
    is_s3(T, ["</h3>" | Acc]);

is_s3([{token, "<h4>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<h4 " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</h4>"} | T], Acc) ->
    is_s3(T, ["</h4>" | Acc]);

is_s3([{token, "<h5>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<h5 " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</h5>"} | T], Acc) ->
    is_s3(T, ["</h5>" | Acc]);

is_s3([{token, "<h6>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<h6 " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</h6>"} | T], Acc) ->
    is_s3(T, ["</h6>" | Acc]);

is_s3([{token, "<strike>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<strike " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</strike>"} | T], Acc) ->
    is_s3(T, ["</strike>" | Acc]);

is_s3([{token, "<ul>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<ul " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</ul>"} | T], Acc) ->
    is_s3(T, ["</ul>" | Acc]);

is_s3([{token, "<ol>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<ol " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</ol>"} | T], Acc) ->
    is_s3(T, ["</ol>" | Acc]);

is_s3([{token, "<li>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<li " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</li>"} | T], Acc) ->
    is_s3(T, ["</li>" | Acc]);

is_s3([{token, "<blockquote>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<blockquote " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</blockquote>"} | T], Acc) ->
    is_s3(T, ["</blockquote>" | Acc]);

is_s3([{token, "<br>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<br/>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<br " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</br>"} | T], Acc) ->
    is_s3(T, ["</br>" | Acc]);

is_s3([{token, "<p>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<p " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</p>"} | T], Acc) ->
    is_s3(T, ["</p>" | Acc]);

is_s3([{token, "<b>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<b " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</b>"} | T], Acc) ->
    is_s3(T, ["</b>" | Acc]);

is_s3([{token, "<i>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<i " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</i>"} | T], Acc) ->
    is_s3(T, ["</i>" | Acc]);

is_s3([{token, "<hr>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<hr/>" ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "<hr " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</hr>"} | T], Acc) ->
    is_s3(T, ["</hr>" | Acc]);

is_s3([{token, "<img " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</img>"} | T], Acc) ->
    is_s3(T, ["</img>" | Acc]);

is_s3([{token, "<a " ++ _Rest = Tk} | T], Acc) ->
    is_s3(T, [esc(Tk) | Acc]);
is_s3([{token, "</a>"} | T], Acc) ->
    is_s3(T, ["</a>" | Acc]);

% anything else just fail
is_s3([{token, _Tk} | _T], _Acc) ->
    false.

retrieve(token, Rest) ->
    retrieve2(Rest, []);
retrieve(Other, Rest) ->
    retrieve3(Other, Rest, []).

retrieve2([$> | Rest], Acc) ->
    {lists:reverse([$> | Acc]), Rest};
retrieve2([?DBL | Rest], Acc) ->
    {String, Rest2} = retrieve3(?DBL, Rest, []),
    retrieve2(Rest2, [String, ?DBL | Acc]);
retrieve2([?SNGL | Rest], Acc) ->
    {String, Rest2} = retrieve3(?SNGL, Rest, []),
    retrieve2(Rest2, [String, ?SNGL |  Acc]);
retrieve2([H | T], Acc) ->
    retrieve2(T, [H | Acc]).

retrieve3(Token, [Token | Rest], Acc) ->
    {lists:reverse([Token | Acc]), Rest};
retrieve3(Token, [H | T], Acc) ->
    retrieve3(Token, T, [H| Acc]).

esc(Tk) -> re:replace(Tk, "\"", "'", [{return, list}, global]).

%%%===================================================================
%% EUnit Tests
%%%===================================================================

simplest_test_() ->
    HTML = "=html.5x6(\"<div>dandy</div>\")",
    {Resp, _} = is_sane(HTML),
    [?_assertEqual(Resp, true)].

simple_fail_test_() ->
    HTML = "<div>dandy</div>",
    Resp = is_sane(HTML),
    [?_assertEqual(Resp, false)].
