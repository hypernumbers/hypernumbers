%%% @doc Pre-processes incoming data.
%%% <hasan@hypernumbers.com>

-module(superparser).

-export([process/1]).

-define(upcase(S),
        ustring:pr(ustring:to_upper(ustring:new(S)))).

process([$= | Tl]) when Tl =/= [] ->
    {formula, upcase(Tl)};
process([39 | Tl]) -> % singly quoted string
    [{string, Tl},{'text-align', "left"},{format, "null"}];
process(Input) ->
    {ok, Toks} = xfl_lexer:lex(upcase(Input), {1, 1}),
    Return = case Toks of
                 [{bool, B}]                -> [{bool, B},
                                                {'text-align', "center"}, % css is yanktastic
                                                {format, "null"}];
                 [{float, F}]               -> [{float, F},
                                                {'text-align', "right"},
                                                {format, "null"}];
                 [{int, I}]                 -> [{int, I},
                                                {'text-align', "right"},
                                                {format, "null"}];
                 [{'-'}, {float, F}]        -> [{float, -F},
                                                {'text-align', "right"},
                                                {format, "null"}];
                 [{'-'}, {int, I}]          -> [{int, -I},
                                                {'text-align', "right"},
                                                {format, "null"}];
                 [{float, F}, {'%'}]        -> [{float, F/100},
                                                {'text-align', "right"},
                                                {format, "0.00%"}];
                 [{int, I}, {'%'}]          -> [{float, I/100},
                                                {'text-align', "right"},
                                                {format, "0.00%"}];
                 [{'-'}, {float, F}, {'%'}] -> [{float, -F/100},
                                                {'text-align', "right"},
                                                {format, "0.00%"}];
                 [{'-'}, {int, I}, {'%'}]   -> [{float, -I/100},
                                                {'text-align', "right"},
                                                {format, "0.00%"}];
                 % type tag gets discarded by caller which is ok for 
                 % the rest of them, but not here
                 [{errval, E}]       -> [{errval, {errval, E}}, 
                                         {'text-align', "center"},  % css is yanktastic
                                         {format, "null"}]; 
                 _Other              ->
                     case super_util:autoparse(Toks) of
                         {ok, bad_date} -> [{string, Input},
                                            {'text-align', "left"},
                                            {format, "null"}];
                         Other          -> Other
                     end
             end,
    Return.

%% Converts formula to upper-case, leaving only string literals as-is.
upcase(Str) ->
    {ok, Tokens, _} = superlex:string(Str),
    % List of numbers (codepoints) interspersed with {string, _} tuples.
    Str2 = 
        tl(lists:foldl(fun({stuff, X}, Acc) ->
                               hslists:init(Acc) ++ ([lists:last(Acc)] ++ X);
                          (Tok = {string, _}, Acc) ->
                               Acc ++ [Tok]
                       end,
                       [junk],
                       Tokens)),
    R = upcase1(Str2, [], []),
    R.

upcase1([Hd | Tl], Intermbuf, Res) when is_number(Hd) ->
    upcase1(Tl, Intermbuf ++ [Hd], Res);
upcase1([{string, Val} | Tl], Intermbuf, Res) ->
    upcase1(Tl, [], lists:append([Res, ?upcase(Intermbuf), Val]));
upcase1([], Intermbuf, Res) ->
    lists:append([Res, ?upcase(Intermbuf)]).
