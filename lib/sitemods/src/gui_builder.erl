%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       This module handles the server-side parts of the
%%%            GUI builder
%%%
%%% @end
%%% Created    26 Aug 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------
-module(gui_builder).

-export([
         get_file/1
        ]).

%%-export([test/0]).

-include_lib("xmerl/include/xmerl.hrl").

get_file(File) ->
    {ParsResult, _Misc} = xmerl_scan:file(File),
    {ok, Canvas} = get_canvas(ParsResult),
    DocHTML = xmerl:export([Canvas], xmerl_html),
    strip_doctype(lists:flatten(DocHTML)).

get_canvas([]) ->
    {error, no_canvas};
get_canvas([H | T]) ->
    case get_canvas(H) of
        {ok, C}            -> {ok, C};
        {error, no_canvas} -> get_canvas(T)
    end;
get_canvas(#xmlText{value = _V}) ->
    % io:format("(1) Value is ~p~n", [V]),
    {error, no_canvas};
get_canvas(#xmlComment{value = _V}) ->
    % io:format("(2) Value is ~p~n", [V]);
    {error, no_canvas};
get_canvas(#xmlElement{name = _N, content = C, attributes = A} = X) ->
    % io:format("(1) Name is ~p~n", [N]),
    % io:format("Attributes are ~p~n", [A]),
    % io:format("is_canvas is ~p~n", [is_canvas(A)]),
    case is_canvas(A) of
        true  -> {ok, X};
        false -> get_canvas(C)
    end;
get_canvas({_Tag, _Attributes, Content}) ->
    %%io:format("(1) Tag is ~p~n", [Tag]),
    get_canvas(Content);
get_canvas({_Tag, Content}) ->
    %%io:format("(2) Tag is ~p~n", [Tag]),
    get_canvas(Content);
get_canvas(_Tag) ->
    %%io:format("(3) Tag is ~p~n", [Tag]).
    {error, no_canvas}.

is_canvas([]) -> false;
is_canvas([#xmlAttribute{name = id, value = "canvas"} | _T]) -> true;
is_canvas([_H | T]) -> is_canvas(T).

strip_doctype([$<,$!,$D,$O,$C,$T,$Y,$P,$E,32,$H,$T,$M,$L,32,$P,$U,$B,$L,$I,$C,32,$",$-,$/,$/,$W,$3,$C,$/,$/,$D,$T,$D,32,$H,$T,$M,$L,32,$4,$.,$0,$1,32,$T,$r,$a,$n,$s,$i,$t,$i,$o,$n,$a,$l,$/,$/,$E,$N,$",$>,10 | T]) -> T.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Test Function
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % test() ->
% %     FileName = "/opt/code/trunk/lib/hypernumbers-1.0/priv/docroot/anonymous/test2.html",
% %     {ParsResult, _Misc} = xmerl_scan:file(FileName),
% %     {ok, Canvas} = get_canvas(ParsResult),
% %     io:format("Canvas is ~p~n", [Canvas]),
% %     DocHTML = xmerl:export([Canvas], xmerl_html),
% %     io:format("DocHtml is ~p~n", [strip_doctype(lists:flatten(DocHTML))]),
% %     ok.
