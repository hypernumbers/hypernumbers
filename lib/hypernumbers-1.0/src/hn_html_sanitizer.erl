%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       This document does basic sanitation for
%%%            html coming in from the rich text editor
%%%
%%% @end
%%% Created :  7 Mar 2012 by gordon@hypernumbers.com

-module(hn_html_sanitizer).

-export([
         is_sane/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(DBL, $").
-define(SNGL, $').

-spec is_sane(string) -> boolean().
is_sane(List) -> is_s2(List, []).

is_s2([], Acc) ->
    io:format("Acc is ~p~n", [Acc]),
    is_s3(Acc);
is_s2([$< | _Rest] = L, Acc) ->
    {Token, Rest2} = get(token, L),
    is_s2(Rest2, [{token, string:to_lower(Token)} | Acc]);
is_s2([?DBL | _Rest] = L, Acc) ->
    {String, Rest2} = get(?DBL, L),
    is_s2(Rest2, [{string, String} | Acc]);
is_s2([?SNGL | _Rest] = L, Acc)->
    {String, Rest2} = get(?SNGL, L),
    is_s2(Rest2, [{string, String} | Acc]);
is_s2([H | T], Acc) ->
    is_s2(T, [{char, H} | Acc]).

is_s3([])                              -> true;
is_s3([{char, _}| T])                  -> is_s3(T);
is_s3([{string, _} | T])               -> is_s3(T);
% these are the only valid tokens
is_s3([{token, "<div" ++ _Rest} | T])  -> is_s3(T);
is_s3([{token, "</div>"} | T])         -> is_s3(T);
is_s3([{token, "<span" ++ _Rest} | T]) -> is_s3(T);
is_s3([{token, "</span>"} | T])        -> is_s3(T);
is_s3([{token, "<b" ++ _Rest} | T])    -> is_s3(T);
is_s3([{token, "</b>"} | T])           -> is_s3(T);
% anything else just fail
is_s3([{token, Tk} | _T])              ->
    io:format("failing with ~p~n", [Tk]),
    false.

get(token, Rest) ->
    get2(Rest, []);
get(Other, Rest) ->
    get3(Other, Rest, []).

get2([$> | Rest], Acc) ->
    {lists:reverse([$> | Acc]), Rest};
get2([?DBL | Rest], Acc) ->
    {String, Rest2} = get3(?DBL, Rest, []),
    get2(Rest2, lists:merge([?DBL | lists:reverse(String)], Acc));
get2([?SNGL | Rest], Acc) ->
    {String, Rest2} = get3(?SNGL, Rest, []),
    get2(Rest2, lists:merge([?SNGL | lists:reverse(String)], Acc));
get2([H | T], Acc) ->
    get2(T, [H | Acc]).

get3(Token, [Token | Rest], Acc) ->
    {lists:reverse([Token | Acc]), Rest};
get3(Token, [H | T], Acc) ->
    get3(Token, T, [H| Acc]).

%%%===================================================================
%% EUnit Tests
%%%===================================================================

simplest_test_() ->
    HTML = "<div>dandy</div>",
    Resp = is_sane(HTML),
    [?_assertEqual(Resp, true)].
