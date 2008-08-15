%%% @doc Pre-processes incoming data.
%%% <hasan@hypernumbers.com>

-module(superparser).
-export([process/1]).
-define(upcase(S),
        ustring:pr(ustring:to_upper(ustring:new(S)))).

process([$= | Tl]) when Tl =/= [] ->
    {formula, upcase(Tl)};
process([39 | Tl]) -> %% singly quoted string
    {string, Tl};
process(Input) ->
    {ok, Toks} = xfl_lexer:lex(upcase(Input), {1, 1}),
    case Toks of
        [{bool, B}]         -> {bool, B};
        [{float, F}]        -> {float, F};
        [{int, I}]          -> {int, I};
        [{'-'}, {float, F}] -> {float, -F};
        [{'-'}, {int, I}]   -> {int, -I};
        [{errval, E}]       -> {errval, E};
        _Else               -> {string, Input} %% TODO: What can go wrong here?
    end.

%% Converts formula to upper-case, leaving only string literals as-is.
upcase(Str) ->
    {ok, Tokens, _} = superlex:lex(Str),
    %% List of numbers (codepoints) interspersed with {string, _} tuples.
    Str2 = 
        tl(lists:foldl(fun({stuff, X}, Acc) ->
                               hslists:init(Acc) ++ ([lists:last(Acc)] ++ X);
                          (Tok = {string, _}, Acc) ->
                               Acc ++ [Tok]
                       end,
                       [junk],
                       Tokens)),
    upcase1(Str2, [], []).

upcase1([Hd | Tl], Intermbuf, Res) when is_number(Hd) ->
    upcase1(Tl, Intermbuf ++ [Hd], Res);
upcase1([{string, Val} | Tl], Intermbuf, Res) ->
    upcase1(Tl, [], lists:append([Res, ?upcase(Intermbuf), Val]));
upcase1([], Intermbuf, Res) ->
    lists:append([Res, ?upcase(Intermbuf)]).
