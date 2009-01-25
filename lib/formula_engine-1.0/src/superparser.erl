%%% @doc Pre-processes incoming data.
%%% @author hasan@hypernumbers.com

-module(superparser).

-export([process/1]).

process([$= | Tl]) when Tl =/= [] ->
    {formula, super_util:upcase(Tl)};
process([39 | Tl]) -> % singly quoted string
    [{quote, Tl},{'text-align', "left"},{format, "null"}];
process(Input) ->
    % the xfl_lexer:lex takes a cell address to lex against
    % in this case {1, 1} is used because the results of this
    % are not actually going to be used here (ie {1, 1} is a dummy!)
    {ok, Toks} = xfl_lexer:lex(super_util:upcase(Input), {1, 1}),
    Return = case Toks of
                 [{bool, B}]                -> [{bool, B},
                                                {'text-align', "center"},
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
                 [{float, F}, {'%'}]        -> [{float,F/100},
                                                {'text-align', "right"},
                                                {format, "0.00%"}];
                 [{int, I}, {'%'}]          -> [{float,I/100},
                                                {'text-align', "right"},
                                                {format, "0%"}];
                 [{'-'}, {float, F}, {'%'}] -> [{float,F/100},
                                                {'text-align', "right"},
                                                {format, "0.00%"}];
                 [{'-'}, {int, I}, {'%'}]   -> [{float,I/100},
                                                {'text-align', "right"},
                                                {format, "0%"}];
                 % type tag gets discarded by caller which is ok for 
                 % the rest of them, but not here
                 [{errval, E}]       -> [{errval, {errval, E}}, 
                                         {'text-align', "center"},
                                         {format, "null"}]; 
                 _Other              ->
                     case super_util:autoparse(Toks) of
                         {ok, maybe_bad_date} ->
                             Date = muin_date:from_rfc1123_string(Input),
                             case Date of
                                 bad_date -> [{string, Input},
                                              {'text-align', "left"},
                                              {format, "null"}];
                                 _        -> [{string, Date},
                                              {'text-align', "center"},
                                              {format, "dd-mm-yyyy"}]
                             end;
                         Other          -> Other
                     end
             end,
    Return.
