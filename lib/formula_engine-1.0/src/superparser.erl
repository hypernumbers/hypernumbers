%%% @doc The super-parser: all input goes through it, and it decides
%%% what to do with it.
%%% <hasan@hypernumbers.com>

-module(superparser).
-export([process/1]).
-define(upcase(S),
        ustring:pr(ustring:upcase(ustring:new(S)))).

process(Input) ->
    {ok, Toks} = muin_lexer:lex(Input, {1,1}), %% FIXME: If fails, complain.
    case Toks of
        [{str, Str}] ->
            {string, Str};
        _ ->
            Up = upcase(Input),
            case Up of
                [$= | Tl] ->
                    {formula, Tl};
                _ ->
                    %% TODO: Try to detect dates here (locale-specific,
                    %% default to en_US).
                    {ok, Toks2} = muin_lexer:lex(Up, {1,1}),
                    case Toks2 of
                        [{bool, B}]         -> {bool, B};
                        [{float, F}]        -> {number, F};
                        [{int, I}]          -> {number, I};
                        [{'-'}, {float, F}] -> {number, -F};
                        [{'-'}, {int, I}]   -> {number, -I};
                        [{error, E}]        -> {error, E};
                        _ ->
                            io:format("UNEXPECTED in superparser:process ~n~p~n",
                                      [Toks2])
                    
                    end
            end
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
    upcase1(Tl, [], lists:append([Res,
                                  ?upcase(Intermbuf),
                                  Val]));
upcase1([], Intermbuf, Res) ->
    lists:append([Res, ?upcase(Intermbuf)]).
