%%% @doc Pre-processes incoming data.
%%% @author hasan@hypernumbers.com
%%% @copyright 2008 Hypernumbers Ltd
%%% @private

-module(superparser).
-export([process/1]).

process(List) -> process2(fixup(List)).

% first swap - and $ if required
process2([$-, $$ | Tl]) when Tl =/= [] ->
    process([$$, $- | Tl]);
process2([$= | Tl]) when Tl =/= [] ->
    {formula, super_util:upcase(Tl)};
process2([$+ | Tl] = L) when Tl =/= [] ->
    NewTl = fixup(Tl),
    case tconv:to_num(NewTl) of
        {error, nan} -> {formula, super_util:upcase(L)};
        _            -> process3(NewTl)
    end;
process2([$- | Tl] = L) when Tl =/= [] ->
    NewTl = fixup(Tl),
    case tconv:to_num(NewTl) of
        {error, nan} -> {formula, super_util:upcase(L)};
        _            -> process3([$-|NewTl])
    end;
process2([$$, $- | Tl] = L) when Tl =/= [] ->
    NewTl= fixup(Tl),
    case tconv:to_num(NewTl) of
        {error, nan} -> {formula, super_util:upcase(L)};
        _            -> lists:keyreplace("format", 1, process2([$- | NewTl]),
                                         {"format", "\"$\"###,0"}) %"}) syntax
    end;
process2([39 | Tl]) -> % singly quoted string
    [{quote, Tl},{"text-align", "left"},{"format", "null"}];
process2([$$ | Tl] = L) ->
    NewTl = fixup(Tl),
    case tconv:to_num(NewTl) of
        {error, nan} -> [{string, L}, {"text-align", "left"}, {"format", "null"}];
        _            -> lists:keyreplace("format", 1, process(NewTl),
                                         {"format", "\"$\"###,0"}) %"}) syntax
    end;
% pass html through plain and unformatted
% COMMENTED OUT - WAITING FOR MARKDOWN DIALOG BOX TO BEREJIGGED
%% process([$< | Tl] = L) when Tl =/= [] ->
%%    [{string, L}, {"text-align", "left"}, {"format", "plain"}];
process2(Input) -> process3(Input).

process3(Input) ->
    % the xfl_lexer:lex takes a cell address to lex against
    % in this case {1, 1} is used because the results of this
    % are not actually going to be used here (ie {1, 1} is a dummy!)
    {ok, Toks} = xfl_lexer:lex(super_util:upcase(Input), {1, 1}),
    case Toks of
        [{bool, _, B}] ->
            [{bool, B}, {"text-align", "center"}, {"format", "null"}];
        [{float, _, {F, _OrigStr}}] ->
            [{float, F}, {"text-align", "right"}, {"format", "null"}];
        [{'(',_},{float, _, {F, _OrigStr}},{')',_}] ->
            [{float, -F}, {"text-align", "right"}, {"format", "null"}];
        [{int, _, I}] ->
            [{int, I}, {"text-align", "right"}, {"format", "null"}];
        [{'(',_}, {int, _, I}, {')',_}] ->
            [{int, -I}, {"text-align", "right"}, {"format", "null"}];
        [{'-',_}, {float, _, {F, _OrigStr}}] ->
            [{float, -F}, {"text-align", "right"}, {"format", "null"}];
        [{'-',_}, {int, _, I}] ->
            [{int, -I}, {"text-align", "right"}, {"format", "null"}];
        [{float, _, {F, _OrigStr}}, {'%',_}] -> 
            [{float,F/100}, {"text-align", "right"}, {"format", "0.00%"}];
        [{int, _, I}, {'%',_}] ->
            [{float,I/100}, {"text-align", "right"}, {"format", "0%"}];
        [{'-',_}, {float, _, {F, _OrigStr}}, {'%',_}] ->
            [{float,F/100}, {"text-align", "right"}, {"format", "0.00%"}];
         [{'-',_}, {int, _, I}, {'%',_}]   ->
            [{float,I/100}, {"text-align", "right"}, {"format", "0%"}];
        % type tag gets discarded by caller which is ok for 
        % the rest of them, but not here
        [{errval, _, E}] ->
            [{errval, {errval, E}}, {"text-align", "center"},
             {"format", "null"}]; 
        _Other              ->
                 case super_util:autoparse(Toks) of
                     {ok, maybe_bad_date} ->
                         Date = muin_date:from_rfc1123_string(Input),
                         case Date of
                             bad_date -> [{string, Input},
                                          {"text-align", "left"},
                                          {"format", "null"}];
                             _        -> [{string, Date},
                                          {"text-align", "center"},
                                          {"format", "dd-mm-yyyy"}]
                         end;
                     Other ->
                         Other
            end
    end.

%% fixup is going to strip the &nbsp; off both end of the string
fixup(List) -> Ret = fixup3(lists:reverse(fixup2(string:strip(List)))),
               Ret.

fixup2([32 | Rest]) -> fixup2(Rest);
fixup2([$&,$n,$b,$s,$p,$;|Rest]) -> fixup2(Rest);
fixup2(List) -> List.

fixup3([32 | Rest]) -> fixup2(Rest);
fixup3([$;,$p,$s,$b,$n,$& | Rest]) -> fixup3(Rest);
fixup3(List) -> lists:reverse(List).
