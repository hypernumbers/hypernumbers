%%% @doc The super-parser: all input goes through it, and it decides
%%% what to do with it.
%%% <hasan@hypernumbers.com>

-module(superparser).
-export([process/1]).
-import(muin_util, [init/1]).
-define(upcase(S),
        ustring:pr(ustring:upcase(ustring:new(S)))).

process(Str) ->
    case Str of
        [$= | _] ->
            %% Formulas are upcased to make things easier in Muin.
            {formula, upcase_formula(Str)};
        _ ->
            {value, Str}
    end.

%% Converts formula to upper-case, leaving only string literals as-is.
upcase_formula(Str) ->
    {ok, Tokens, _} = superlex:lex(Str),
    %% List of numbers (codepoints) interspersed with {string, _} tuples.
    Str2 = 
        tl(lists:foldl(fun({stuff, X}, Acc) ->
                               init(Acc) ++ ([lists:last(Acc)] ++ X);
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
