%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Gordon Guthrie
%%% @docws,
%%%
%%% @end
%%% Created : 10 Sep 2009 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------

-module(markdown).

-export([conv/1,
        conv_file/2]).

-import(lists, [flatten/1, reverse/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).
-define(TAB, 9).
-define(LF, 10).
-define(CR, 13).
-define(NBSP, 160).

%%% the lexer first lexes the input
%%% make_lines does 2 passes:
%%% * it chops the lexed strings into lines which it represents as a
%%%   list of lists
%%% * it then types the lines into the following:
%%% * normal lines
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - SETEXT header lines
%%%   - ATX header lines
%%%   - blockquote
%%%   - unordered lists
%%%   - ordered lists
%%%   - code blocks
%%%   - horizontal rules
%%% the parser then does its magic interpolating the references as appropriate
conv(String) -> Lex = lex(String),
                io:format("Lex is ~p~n", [Lex]),
                UntypedLines = make_lines(Lex),
                io:format("UntypedLines are ~p~n", [UntypedLines]),
                {Refs, TypedLines} = type_lines(UntypedLines),
                io:format("TypedLines are ~p~nRefs are ~p~n",
                          [TypedLines, Refs]),
                parse(Refs, TypedLines).

conv_file(FileIn, FileOut) ->
     case file:open(FileIn, [read]) of
         {ok, Device} -> Input = get_all_lines(Device,[]),
                         Output = conv(Input),
                         write(FileOut, Output);
         _            -> error
     end.
 
get_all_lines(Device, Accum) ->
    case io:get_line(Device,"") of
        eof  -> file:close(Device),
                Accum;
        Line ->
            get_all_lines(Device,Accum ++ Line)
    end.

write(File, Text) ->
    _Return=filelib:ensure_dir(File),
    case file:open(File, [write]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Text]),
            file:close(Id);
        _ ->
            error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parse the lines interpolating the references as appropriate
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% special hypernumbers parse for a single normal line
% parse([], [{normal, P} | []]) -> make_str(P, []);
parse(Refs, TypedLines)       -> string:strip(p1(TypedLines, Refs, "",
                                                 [], []), both, ?LF).

%% goes through the lines
%% If it hits a 'start' like 'blockquote' it throws a HTML fragment into the mix
%% (eg <blockquote>) and then the matching closing one onto the stack (eg </blockquote>)

%% when it hits a hard return it then throws the matching closing one back in...
p1([], _R, _I, [], Acc)    -> flatten(reverse(Acc)); 
p1([], _R, _I, Stack, Acc) -> flatten(reverse([reverse(Stack) | Acc]));

%% Unescaped Tags have the highest precedence...
p1([{unesc_tag, Tag} | T], R, I, Stack, Acc) ->
    case T of
        [{blank, _} | T2] -> p1(T2, R, I, Stack,
                                [I ++ make_tag_str(Tag) | Acc]);
        _Other            -> p1(T, R, I, Stack,
                                [I ++ "<p>" ++ make_str([Tag], R) ++ "</p>" | Acc])
    end;
%% These clauses handle state issues
%% hit a linebreak - you close the last open state (if it exists)
p1([{br, _} | T], R, I, [], Acc) ->
    p1(T, R, I, [], [I ++ "<br />" | Acc]); 
p1([{br, _} | T], R, I, ["\n<blockquote>" | Open], Acc) ->
    p1(T, R, "", Open, ["\n<blockquote>", "\n" ++ I ++ "\n" | Acc]);
p1([{br, _} | T], R, I, [Close | Open], Acc) ->
    p1(T, R, I, Open, [Close, "<br />" ++ I ++ "\n" | Acc]);

%% handle a blank line
p1([{blank, _} | T], R, I, [], Acc) ->
    p1(T, R, I, [], [I ++ "\n" | Acc]); 

%% two consecutive normal lines should be concatenated..
p1([{normal, P1}, {blank, _}, {normal, P2} | T], R, I, Stack, Acc) ->
    p1([{normal, flatten([P1, {string, "\n"} | P2])} | T], R, I, Stack, Acc);

%% setext h1 is a look behind and it overrides blockquote and code...
p1([{normal, P}, {setext_h1, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [I ++ "<h1>" ++ make_str(snip(P), R) ++ "</h1>" | Acc]); 
p1([{blockquote, P}, {setext_h1, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [I ++ "<h1>" ++ make_str(snip(P), R) ++ "</h1>" | Acc]); 
p1([{{codeblock, P}, _}, {setext_h1, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [I ++ "<h1>" ++ make_str(snip(P), R) ++ "</h1>" | Acc]); 
p1([{{codeblock, P}, _}, {h2_or_hr, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [I ++ "<h2>" ++ make_str(snip(P), R) ++ "</h2>" | Acc]); 

%% but a setext with no lookbehind is just rendered as a normal line,
%% so change its type and rethrow it
p1([{setext_h1, P} | T], R, I, Stack, Acc) ->
    p1([{normal, P} | T], R, I, Stack, Acc); 

%% setext h2 might be a look behind
p1([{normal, P}, {blank, _}, {h2_or_hr, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [I ++ "<h2>" ++ make_str(snip(P), R) ++ "</h2>" | Acc]); 

%% blockquote is look ahead push blockquote onto the stack
p1([{blockquote, _P} | T], R, _I, Stack, Acc) ->
    p1(T, R, "  ", ["\n</blockquote>" | Stack], ["<blockquote>\n" | Acc]);

%% but one is just normal...
p1([{normal, P} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [I ++ "<p>" ++ make_str(snip(P), R) ++ "</p>" | Acc]);

%% atx headings
p1([{{h1, P}, _} | T], R, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, R), right),
    p1(T, R, I, Stack, [I ++ "<h1>" ++ NewP ++ "</h1>" | Acc]); 
p1([{{h2, P}, _} | T], R, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, R), right),
    p1(T, R, I, Stack, [I ++ "<h2>" ++ NewP ++ "</h2>" | Acc]); 
p1([{{h3, P}, _} | T], R, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, R), right),
    p1(T, R, I, Stack, [I ++ "<h3>" ++ NewP ++ "</h3>" | Acc]); 
p1([{{h4, P}, _} | T], R, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, R), right),
    p1(T, R, I, Stack, [I ++ "<h4>" ++ NewP ++ "</h4>" | Acc]); 
p1([{{h5, P}, _} | T], R, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, R), right),
    p1(T, R, I, Stack, [I ++ "<h5>" ++ NewP ++ "</h5>" | Acc]); 
p1([{{h6, P}, _} | T], R, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, R), right),
    p1(T, R, I, Stack, [I ++ "<h6>" ++ NewP ++ "</h6>" | Acc]); 

%% unordered lists
p1([{{ul, _P}, _} | _T] = List, R, I, Stack, Acc) ->
    {Rest, NewAcc} = parse_list(ul, List, R, []),
    p1(Rest, R, I, Stack, [I ++ "<ul>" ++ NewAcc ++ "</ul>" | Acc]);

%% unordered lists
p1([{{ol, _P}, _} | _T] = List, R, I, Stack, Acc) ->
    {Rest, NewAcc} = parse_list(ol, List, R, []),
    p1(Rest, R, I, Stack, [I ++ "<ol>" ++ NewAcc ++ "</ol>" | Acc]);

%% codeblock
p1([{{codeblock, P}, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [I ++ "<pre><code>" ++ make_str(P, R)
                        ++ "\n</code></pre>" | Acc]);

%% horizontal rules
p1([{hr, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, ["<hr />" | Acc]);
%% the clause with a normal before an 'h2_or_hr' has already been
%% handled further up the tree, so this is a bona fide 'hr'...
p1([{h2_or_hr, _} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, ["<hr />" | Acc]); 

%% line breaks
p1([{br, P} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, [P | Acc]);

%% Now start pulling out inline refs etc, etc
p1([{inlineref, _P} | T], R, I, Stack, Acc) ->
    p1(T, R, I, Stack, Acc).

%% this is a bit messy because of the way that hard lines are treated...
%% If your li's have a blank line between them the item gets wrapped in a para,
%% if not, they don't
parse_list(_Type, [], _R, A) ->
    {[], reverse(A)};
%% if the '<li>' is followed by a blank then wrap it, throw the blank
%% back onto the tail
parse_list(Type, [{{Type, P}, _}, {blank, _} = B | T], R, A) ->
     parse_list(Type, [B | T], R, ["<li>" ++ make_esc_str(P, R)
                                      ++ "</li>" | A]);
%% this is the partner of the previous line...
parse_list(Type, [{blank, _}, {{Type, P}, _} | T], R, A) ->
    {Rest, NewP} = grab(T, R, []),
    parse_list(Type, Rest, R, ["<li>" ++ make_esc_str(P, R)
                                  ++ NewP ++"</li>" | A]);
%% this is a plain old '<li>'
parse_list(Type, [{{Type, P}, _} | T], R, A) ->
    {Rest, NewP} = grab(T, R, []),
    parse_list(Type, Rest, R, ["<li>" ++ make_esc_str(P, R)
                                  ++ NewP ++ "</li>" | A]);
parse_list(_Type, List, _R, A) ->
    {List, reverse(A)}.

%% grab grabs normals and double codeblocks
grab([{{codeblock, _}, S} | T] = List, R, Acc) ->
    case is_blockquote(S, T) of
        {{true, R1}, T2}       -> grab(T2, R,
                                       ["\n</blockquote>",
                                        make_esc_str(R1, R),
                                        "<blockquote>\n" | Acc]);
        {{esc_false, R1}, _T2} -> {R1, reverse(Acc)};
        {false, T2}            -> 
            case is_double_indent(S) of  % try codeblock
                false      -> {List, reverse(Acc)};
                {true, R2} ->
                    grab(T2, R, [make_esc_str(R2, R), "<br />" | Acc])
            end
    end;
grab([{normal, P} | T], R, Acc) -> grab(T, R, [make_esc_str(P, R) | Acc]);
grab(List, _R, Acc)             -> {List, reverse(Acc)}.

is_double_indent(List) -> is_double_indent1(List, 0).

%% double indent is any combination of tabs and spaces that add
%% up to 8
is_double_indent1([], _N)                  -> false;
is_double_indent1(Rest, N) when N > 7      -> {true, Rest};
is_double_indent1([{{ws, sp}, _} | T], N)  -> is_double_indent1(T, N + 1);
is_double_indent1([{{ws, tab}, _} | T], N) -> is_double_indent1(T, N + 4);
is_double_indent1(_List, _N)               -> false.

is_blockquote(List, T) ->
    case is_bq1(List, 0) of
        false          -> {false, T};
        {esc_false, R} -> {{esc_false, R}, T};
        {true, R}      -> {NewT, NewR} = grab2(T, R),
                          {{true, NewR}, NewT}
    end.
        
is_bq1([], _N)                            -> false;
is_bq1([{{ws, sp}, _} | T], N)            -> is_bq1(T, N + 1);
is_bq1([{{ws, tab}, _} | T], N)           -> is_bq1(T, N + 4);
is_bq1([{{md, gt}, _},
        {{ws, _}, _} | T], N) when N > 3  -> {true, T};
is_bq1([{{punc, bslash}, _},
        {{md, gt}, GT},
        {{ws, _}, WS} | T], N) when N > 3 -> {esc_false, [GT, WS | T]};
is_bq1(_List, _N)                         -> false.

grab2(List, R) -> gb2(List, reverse(R)).

gb2([], Acc)               -> {[], flatten(reverse(Acc))};
gb2([{blank, _} | T], Acc) -> {T, flatten(reverse(Acc))};
gb2([{_Type, P} | T], Acc) -> gb2(T, [P | Acc]).
             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Make the lines from the raw tokens
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_lines(Tokens) -> ml1(Tokens, [], []).

ml1([], [], A2)                      -> reverse(A2);
ml1([], A1, A2)                      -> ml1([], [], [reverse(A1) | A2]);
%% this clause forces a <BR /> line before a blockquote...
ml1([{{ws, _}, _},
     {{ws, _}, _},
     {{lf, _}, _} = LF,
     {{md, gt}, _} = BQ,
     {{ws, _}, _} | T], A1, A2)      -> ml1(T, [], [[BQ], [{br, "<br />"}],
                                                    ml2(LF, A1) | A2]);
%% this is a normal blockquote
ml1([{{lf, _}, _} = LF,
     {{md, gt}, _} = BQ,
     {{ws, _}, _} | T], A1, A2)      -> ml1(T, [], [[BQ], [LF],
                                                      reverse(A1) | A2]);
ml1([{{lf, _}, _} = LF | T], [], A2) -> ml1(T, [], [[LF] | A2]);
%% this clause forces a <BR /> line...
ml1([{{ws, _}, _},
     {{ws, _}, _},
     {{lf, _}, _} = H | []], A1, A2) -> ml1([], [], [[{br, "<br />"}],
                                                     ml2(H, A1) | A2]);
ml1([{{lf, _}, _} = LF | T], A1, A2) -> ml1(T, [], [[LF], reverse(A1) | A2]);
ml1([H | T], A1, A2)                 -> ml1(T, [H | A1], A2).

ml2(H, List) -> reverse([H | List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Process the lines and give each line a type. The valid types are:
%%% * normal line
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - SETEXT header lines
%%%   - ATX header lines
%%%   - unordered lists (including code blocks)
%%%   - ordered lists (including code blocks)
%%%   - blockquotes
%%%   - code blocks
%%%   - horizontal rules
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_lines(Lines) ->
    {Refs, TypedLines} = t_l1(Lines, [], []),
    TypedLines2 = compress(TypedLines),
    {Refs, TypedLines2}.

compress(List) -> comp1(List, []).

comp1([], A)                             -> reverse(A);
comp1([{blank, B1}, {blank, B2} | T], A) -> B = lists:merge(B1, B2),
                                            comp1([{blank, B} | T], A);
comp1([H | T], A)                        -> comp1(T, [H | A]).

t_l1([], A1, A2) -> {A1, reverse(A2)};
%% this clause extracts URL and Image refs
%% (it is the only one that uses A1 and A2...
%% inlines can have up to 3 spaces before it
t_l1([[{{ws, sp}, _},
       {{ws, sp}, _},
       {{ws, sp}, _},
       {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);
t_l1([[{{ws, sp}, _},
       {{ws, sp}, _},
       {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);
t_l1([[{{ws, sp}, _},
       {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);
t_l1([[{{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);

%% types a blank line or a code block
t_l1([[{{lf, _}, _}| []]  = H | T], A1, A2) ->
    t_l1(T, A1, [{blank, H} | A2]);
t_l1([[{{ws, _}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [type_ws(H) | A2]);

%% types setext lines
t_l1([[{{md, eq}, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_setext_h1(H) | A2]);
%% NOTE 1: generates a ul as the default not a normal line
%% NOTE 2: depending on the context this might generate an <h2> header or an <hr />
t_l1([[{{md, dash}, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_setext_h2(H) | A2]);

%% types atx lines
t_l1([[{{md, atx}, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_atx(H) | A2]);
 
%% types blockquotes
t_l1([[{{md, gt}, _} | []] = H | T], A1, A2) ->
     t_l1(T, A1, [{blockquote, H} | A2]);

%% types unordered lists lines
%% NOTE 1: the dashed version is generated in type_setext_h2
%% NOTE 2: the asterix version also might generate a horizontal rule
%%         which is why it jumps to type_star2 <-- note the 2!!
t_l1([[{{md, star}, _}, {{ws, _}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{type_star2(H), H} | A2]);
t_l1([[{{md, plus}, _}, {{ws, _}, _} = W | T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{{ul, make_list_str([W | T1])}, H} | A2]);

%% types ordered lists...
t_l1([[{num, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_ol(H) | A2]);

%% types horizontal rules for stars and underscores
%% dashes and some stars are done elsewhere...
t_l1([[{{md, underscore}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [type_underscore(H) | A2]);
t_l1([[{{md, star}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [type_star(H) | A2]);

%% <BR /> typing
t_l1([[{br, P}] | T], A1, A2) ->
    t_l1(T, A1, [{br, P} | A2]);

%% Block level tags - these are look ahead they must be
%% on a single line (ie directly followed by a lf and nothing else
t_l1([[{{{tag, _Type}, Tag}, _} = H | T1] = List | T], A1, A2) ->
    case is_blank(T1) of
        false -> t_l1(T, A1, [{normal , List} | A2]);
        true  -> case is_block_tag(Tag) of
                     true  -> t_l1(T, A1, [{unesc_tag , H} | A2]);
                     false -> t_l1(T, A1, [{normal , List} | A2])
                 end
    end;

%% Final clause...
t_l1([H | T], A1, A2) ->
    t_l1(T, A1, [{normal , H} | A2]).

t_inline(H, T1, T2, A1, A2) ->
    case snip_ref(T1) of
        {Type, {Id, {Url, Title}}} -> t_l1(T2, flatten([{Id, {Url, Title}} | A1]),
                                           [{Type, H} | A2]);
        normal                     -> t_l1(T2, A1, [{normal, H} | A2])
    end.

%%
%% Loads of type rules...
%%

is_blank([])                  -> true;
is_blank([{{lf, _}, _} | []]) -> true;
is_blank([{{ws, _}, _} | T])  -> is_blank(T);
is_blank(_List)               -> false.

is_block_tag("ADDRESS")    -> true;
is_block_tag("BLOCKQUOTE") -> true;
is_block_tag("CENTER")     -> true;
is_block_tag("DIR")        -> true;
is_block_tag("DIV")        -> true;
is_block_tag("DL")         -> true;
is_block_tag("FIELDSET")   -> true;
is_block_tag("FORM")       -> true;
is_block_tag("H1")         -> true;
is_block_tag("H2")         -> true;
is_block_tag("H3")         -> true;
is_block_tag("H4")         -> true;
is_block_tag("H5")         -> true;
is_block_tag("H6")         -> true;
is_block_tag("HR")         -> true;
is_block_tag("ISINDEX")    -> true;
is_block_tag("MENU")       -> true;
is_block_tag("NOFRAMES")   -> true;
is_block_tag("NOSCRIPT")   -> true;
is_block_tag("OL")         -> true;
is_block_tag("P")          -> true;
is_block_tag("PRE")        -> true;
is_block_tag("TABLE")      -> true;
is_block_tag("TR")      -> true;
is_block_tag("TD")      -> true;
is_block_tag("UL")         -> true;
is_block_tag(_Other)       -> false.

type_underscore(List) -> case type_underscore1(trim_right(List)) of
                             hr    -> {hr, List};
                             maybe -> Type = type_underscore2(List),
                                      case Type of
                                          normal -> {normal, [{{ws, none}, none} | List]};
                                          Other  -> {Other, List}
                                      end
                         end.

type_underscore1([])                          -> hr;
type_underscore1([{{md, underscore}, _} | T]) -> type_underscore1(T);
type_underscore1(_List)                       -> maybe.

type_underscore2(List) -> case trim_right(List) of % be permissive of trailing spaces
                       [{{md, underscore}, _}, {{ws, _}, _},
                        {{md, underscore}, _}, {{ws, _}, _},
                        {{md, underscore}, _}]               -> hr;
                       _Other                                -> normal
                   end.

type_star(List) -> Trim = trim_right(List),
                   case type_star1(Trim) of % be permssive of trailing spaces
                       hr    -> {hr, trim_right(Trim)};
                       maybe -> Type = type_star2(List),
                                % if it is a normal line we prepend it with a special
                                % non-space filling white space character
                                case Type of
                                    normal -> {normal, [{{ws, none}, none} | List]};
                                    _      -> {Type, List}
                                end
                   end.

type_star1([])                    -> hr;
type_star1([{{md, star}, _} | T]) -> type_star1(T);
type_star1(_List)                 -> maybe.

type_star2(List) ->
    case trim_right(List) of
        [{{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}]                -> hr;
        _Other ->
            case List of
                [{{md, star}, _},
                 {{ws, _}, _}= WS | T] -> {ul, make_list_str([WS | T])};
                _Other2                -> normal
            end
    end.

type_ol(List) ->
    case type_ol1(List, []) of
        normal            -> {normal, List};
        {ol, Str}         -> {{ol, Str}, List};
        {esc_normal, Str} -> {normal, Str}
    end.
                         

%% this line terminates on an escaped fullstop after a number
%% (but you need to drop the bslash...)
type_ol1([{num, _} = N,
          {{punc, bslash}, _},
          {{punc, fullstop}, _} = P | T], Acc) ->
    {esc_normal, flatten([reverse(Acc), N, P | T])};
%% we accumulate the digits in case we need to escape a full stop in a normal line
type_ol1([{num, _} = H | T], Acc)  -> type_ol1(T, [H | Acc]);
type_ol1([{{punc, fullstop}, _},
          {{ws, _}, _} | T], _Acc) -> {ol, T};
type_ol1(_List, _Acc)              -> normal.

%% You need to understand what this function is trying to d...
%% '### blah' is fine
%% '### blah ###' is reduced to '### blah' because trailing #'s are
%% just for show but...
%% '##' is like appling '#' to '#' <-- applying 1 less styling to a single #
%% and '###' is like appling '##' to '#' etc, etc
%% but after you hit 6#'s you just get this for a single hash
%% ie '#############' is like applying '######' to a single '#'
%% but/and '######## blah' is like apply '######' to '## blah'
%% strip trailing #'s as they are decorative only...
type_atx(List) ->
    {Sz, R} = get_atx_size(List),
    A = [{{md, atx}, "#"}],
    Type =
        case is_all_hashes(R) of
            true  ->
                if
                    Sz == 1 ->
                        normal; 
                    ((Sz > 1) andalso (Sz < 6)) ->
                        Ns = integer_to_list(Sz - 1),
                        Hn = list_to_atom("h" ++ Ns),
                        {Hn, A};
                    ((Sz == 6) andalso (R == [])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, lf}, "\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, crlf}, "\r\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R =/= [])) ->
                        {h6, A}
                end;
            false ->
                Ns = integer_to_list(Sz),
                Hn = list_to_atom("h" ++ Ns),
                {Hn, strip_atx(R)}
        end,
    {Type, List}.

is_all_hashes([])                   -> true;
is_all_hashes([{{md, atx}, _} | T]) -> is_all_hashes(T);
is_all_hashes([{{lf, _}, _} | []])  -> true;
is_all_hashes(_List)                -> false.
              
get_atx_size(List) -> g_atx_size1(List, 0).

% this function also strips whitespace to the left...
g_atx_size1([{{md, atx}, _} = A | T], N) when N == 6 -> {6, [A | T]};
g_atx_size1([{{md, atx}, _} | T], N)                 -> g_atx_size1(T, N + 1);
g_atx_size1([{{ws, _}, _} | T], N)                   -> g_atx_size1(T, N);
g_atx_size1(List, N)                                 -> {N, List}.

strip_atx(List) -> reverse(s_atx1(reverse(List))).

s_atx1([{{lf, _}, _}, {{md, atx}, _} | T]) -> s_atx1(T);
s_atx1([{{md, atx}, _} | T])               -> s_atx1(T);
s_atx1(List)                               -> List.

type_setext_h1(List) -> type_s_h1_1(List, []).

%% terminates on running out or new line
type_s_h1_1([{{lf, _}, _} = L | []], Acc) -> {setext_h1, reverse([L | Acc])};
type_s_h1_1([], Acc)                      -> {setext_h1, reverse(Acc)};
type_s_h1_1([[] | T], Acc)                -> type_s_h1_1(T, Acc);
type_s_h1_1([{{md, eq}, _} = H | T], Acc) -> type_s_h1_1(T, [H | Acc]);
type_s_h1_1(L, Acc)                       ->  {normal, flatten([Acc | L])}.

type_setext_h2(List) ->
    case type_s_h2_1(List) of
        h2_or_hr -> {h2_or_hr, List};
        not_h2   -> {type_s_h2_2(trim_right(List)), List}
    end.                            
%% terminates on running out or new line
type_s_h2_1([{{lf, _}, _} | []])   -> h2_or_hr;
type_s_h2_1([])                    -> h2_or_hr;
type_s_h2_1([[] | T])              -> type_s_h2_1(T);
type_s_h2_1([{{md, dash}, _} | T]) -> type_s_h2_1(T);
type_s_h2_1(_L)                    -> not_h2.

type_s_h2_2([{{md, dash}, _}, {{ws,_}, _},
             {{md, dash}, _}, {{ws, _}, _},
             {{md, dash}, _}])              -> hr;
type_s_h2_2([{{md, dash}, _},
             {{ws, _}, _} = WS | T])        -> {ul, make_list_str([WS | T])};
type_s_h2_2(_List)                          -> normal.
 
type_ws(List) ->
    case type_ws1(List) of
        blank         -> {blank, List};
        try_codeblock ->
            case type_ws2(List) of
                normal           -> {normal, List};
                {codeblock, Ret} -> {{codeblock, Ret}, List}
            end
    end.

type_ws1([])                        -> blank;
type_ws1([{{lf, _}, _} | []]) -> blank;
type_ws1([[] | T])                  -> type_ws1(T);
type_ws1([{{ws, _}, _} | T])        -> type_ws1(T);
type_ws1(_L)                        -> try_codeblock.

type_ws2(List) -> t_ws2(List, 0).

%% 4 or more spaces takes you over the limit
%% (a tab is 4...)
t_ws2([{{ws, tab}, _} | T], _N) -> {codeblock, T};
t_ws2(List, N) when N > 3       -> {codeblock, List};
t_ws2([{{ws, sp}, _} | T], N)   -> t_ws2(T, N + 1);
t_ws2(_List, _N)                -> normal.

%% make a tag into a string
make_tag_str({{{tag, Type}, Tag}, _}) ->
    case Type of
        open         ->  "<" ++ Tag ++ ">";
        close        -> "</" ++ Tag ++ ">";
        self_closing ->  "<" ++ Tag ++ " />"
    end.

%% if it is a list we need to discard the initial white space...
make_list_str([{{ws, _}, _} | T] = List) ->
    case is_double_indent(List) of
        false     -> T;
        {true, R} -> flatten([{tag, "<pre><code>"} ,R ,
                              {tag, "</code></pre>"} | []])
    end.

%% All ref processing can ignore the original values 'cos those
%% have already been captured at a higher level
snip_ref(List) ->
    case get_id(List) of
        {Id, Rest} -> {_Rest2, Ref, Title} = parse_inline(Rest),
                      Ref2 = trim(Ref),
                      Rs = make_plain_str(Ref2),
                      Ts = make_plain_str(Title),
                      {inlineref, {Id, {Rs, Ts}}};
        normal     -> normal
    end.

get_id(List) -> g_id1(List, []).

g_id1([], _Acc)                          -> normal;
g_id1([{{inline, close}, _},
       {{punc, colon}, _}, {{ws, _}, _}
       | T], Acc)                        -> {make_plain_str(reverse(Acc)), T};
g_id1([H | T], Acc)                      -> g_id1(T, [H | Acc]).

parse_inline(List) -> p_in1(List, []).

%% snip off the terminal linefeed (if there is one...)
p_in1([{{lf, _}, _} | []], A)            -> {[], reverse(A), []}; 
p_in1([], A)                             -> {[], reverse(A), []}; 
%% brackets can be escaped
p_in1([{{punc, bslash}, _},
       {bra, _} = B | T], A)             -> p_in1(T, [B | A]);
p_in1([{{punc, bslash}, _},
       {ket, _} = B | T], A)             -> p_in1(T, [B | A]);
p_in1([{{punc, bslash}, _},
       {{punc, doubleq}, _} = Q | T], A) -> p_in1(T, [Q | A]);
p_in1([{{punc, bslash}, _},
       {{punc, singleq}, _} = Q | T], A) -> p_in1(T, [Q | A]);
%% these clauses capture the start of the title...
p_in1([{{punc, doubleq}, _} | T], A)     -> p_in2(T, reverse(A), doubleq, []);
p_in1([{{punc, singleq}, _} | T], A)     -> p_in2(T, reverse(A), singleq, []);
%% 'bra' is a valid title surround...
p_in1([{bra, _} | T], A)                 -> p_in2(T, reverse(A), brackets, []);
p_in1([{ket, _} | T], A)                 -> {T, reverse(A), []};
p_in1([H | T], A)                        -> p_in1(T, [H | A]).

%% this gets titles in single and double quotes and brackets
%% the delimiter type is passed in as 'D'
p_in2([], Url, _D, A)                              -> {[], Url, flatten(reverse(A))};
%% brackets can be escaped
p_in2([{{punc, bslash}, _},
       {bra, _} = B | T], Url, D, A)               -> p_in2(T, Url, D, [B | A]);
p_in2([{{punc, bslash}, _},
       {ket, _} = B | T], Url, D, A)               -> p_in2(T, Url, D, [B | A]);
%% quotes can be escaped
p_in2([{{punc, bslash}, _},
       {{punc, doubleq}, _}= Q | T], Url, D, A)    -> p_in2(T, Url, D, [Q | A]);
p_in2([{{punc, bslash}, _},
       {{punc, singleq}, _} = Q | T], Url, D, A)   -> p_in2(T, Url, D, [Q | A]);
%% these clauses capture the end of the title and drop the delimiter...
p_in2([{{punc, doubleq}, _} | T], Url, doubleq, A) -> p_in2(T, Url, none, A);
p_in2([{{punc, singleq}, _} | T], Url, singleq, A) -> p_in2(T, Url, none, A);
p_in2([{ket, _} | T], Url, brackets, A)            -> p_in2(T, Url, none, A);
%% terminator clause
p_in2([{ket, _} | T], Url, none, A)                -> {T, Url, flatten(reverse(A))};
%% this clause silently discards stuff after the delimiter...
p_in2([_H | T], Url, none, A)                      -> p_in2(T, Url, none, [A]);
p_in2([H | T], Url, D, A)                          -> p_in2(T, Url, D, [H | A]).

trim(String) -> trim_left(trim_right(String)).

trim_right(String) -> reverse(trim_left(reverse(String))).

trim_left([{{ws, _}, _} | T]) -> trim_left(T);
trim_left([[] | T])           -> trim_left(T);
trim_left(List)               -> List.

snip(List) -> List2 = reverse(List),
              case List2 of
                   [{{lf, _}, _} | T] -> lists:reverse(T);
                  _                        -> List
              end.

%% end of ref processing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Build the Lexed Token List
%%% This is a two part lexer, first it chunks the input and then on the second
%%% pass it gathers it into lines and types the lines
%%%
%%% NOTE that there are two different styles of processing lines:
%%% * markdown transformed
%%% * block
%%% inside block processing the whole text is dumped and just url encoded
%%% and the original text is always maintained during the lexing/parsing
%%% so that it can be recreated if the context requires it...
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(String) -> l1(String, [], []).

%% this is the terminal head which ends the parsing...
l1([], [], A2)             -> flatten(reverse(A2));
l1([], A1, A2)             -> l1([], [], [l2(A1) | A2]);
%% these two heads capture opening and closing tags
l1([$<, $/|T], A1, A2)     -> {Tag, NewT} = closingdiv(T, []),
                              l1(NewT, [], [Tag, l2(A1) | A2]);
l1([$< | T], A1, A2)       -> {Tag, NewT} = openingdiv(T),
                              l1(NewT, [], [Tag , l2(A1) | A2]);
%% these clauses are the normal lexer clauses
l1([$= | T], A1, A2)       -> l1(T, [], [{{md, eq}, "="},   l2(A1) | A2]);
l1([$- | T], A1, A2)       -> l1(T, [], [{{md, dash}, "-"}, l2(A1) | A2]);
l1([$# | T], A1, A2)       -> l1(T, [], [{{md, atx}, "#"},  l2(A1) | A2]);
l1([$> | T], A1, A2)       -> l1(T, [], [{{md, gt}, ">"},   l2(A1) | A2]);
l1([$+ | T], A1, A2)       -> l1(T, [], [{{md, plus}, "+"}, l2(A1) | A2]);
l1([$* | T], A1, A2)       -> l1(T, [], [{{md, star}, "*"}, l2(A1) | A2]);
l1([$_ | T], A1, A2)       -> l1(T, [], [{{md, underscore}, "_"}, l2(A1) | A2]);
l1([$1 | T], A1, A2)       -> l1(T, [], [{num, "1"}, l2(A1) | A2]);
l1([$2 | T], A1, A2)       -> l1(T, [], [{num, "2"}, l2(A1) | A2]);
l1([$3 | T], A1, A2)       -> l1(T, [], [{num, "3"}, l2(A1) | A2]);
l1([$4 | T], A1, A2)       -> l1(T, [], [{num, "4"}, l2(A1) | A2]);
l1([$5 | T], A1, A2)       -> l1(T, [], [{num, "5"}, l2(A1) | A2]);
l1([$6 | T], A1, A2)       -> l1(T, [], [{num, "6"}, l2(A1) | A2]);
l1([$7 | T], A1, A2)       -> l1(T, [], [{num, "7"}, l2(A1) | A2]);
l1([$8 | T], A1, A2)       -> l1(T, [], [{num, "8"}, l2(A1) | A2]);
l1([$9 | T], A1, A2)       -> l1(T, [], [{num, "9"}, l2(A1) | A2]);
l1([$0 | T], A1, A2)       -> l1(T, [], [{num, "0"}, l2(A1) | A2]);
l1([$. | T], A1, A2)       -> l1(T, [], [{{punc, fullstop}, "."}, l2(A1) | A2]);
l1([$: | T], A1, A2)       -> l1(T, [], [{{punc, colon}, ":"}, l2(A1) | A2]);
l1([$' | T], A1, A2)       -> l1(T, [], [{{punc, singleq}, "'"}, l2(A1) | A2]); %'
l1([$" | T], A1, A2)       -> l1(T, [], [{{punc, doubleq}, "\""}, l2(A1) | A2]); %"
l1([$` | T], A1, A2)       -> l1(T, [], [{{punc, backtick}, "`"}, l2(A1) | A2]); %"
l1([$! | T], A1, A2)       -> l1(T, [], [{{punc, bang}, "!"}, l2(A1) | A2]); %"
l1([$\\ | T], A1, A2)      -> l1(T, [], [{{punc, bslash}, "\\"}, l2(A1) | A2]); %"
l1([$/ | T], A1, A2)       -> l1(T, [], [{{punc, fslash}, "/"}, l2(A1) | A2]); %"
l1([$( | T], A1, A2)       -> l1(T, [], [{bra, "("}, l2(A1) | A2]);
l1([$) | T], A1, A2)       -> l1(T, [], [{ket, ")"}, l2(A1) | A2]);
l1([$[ | T], A1, A2)       -> l1(T, [], [{{inline, open}, "["}, l2(A1) | A2]);
l1([$] | T], A1, A2)       -> l1(T, [], [{{inline, close}, "]"}, l2(A1) | A2]);
%% note there is a special 'whitespace' {{ws, none}, ""} which is used to generate non-space
%% filling whitespace for cases like '*bob* is great' which needs a non-space filling
%% whitespace prepended to trigger emphasis so it renders as "<em>bob</em> is great...
%% that 'character' doesn't exist so isn't in the lexer but appears in the parser
l1([?SPACE | T], A1, A2)   -> l1(T, [], [{{ws, sp}, " "}, l2(A1) | A2]);
l1([?TAB | T], A1, A2)     -> l1(T, [], [{{ws, tab}, "\t"}, l2(A1) | A2]);
l1([?CR, ?LF | T], A1, A2) -> l1(T, [], [{{lf, crlf}, [?CR , ?LF]}, l2(A1) | A2]);
l1([?LF | T], A1, A2)      -> l1(T, [], [{{lf, lf}, [?LF]}, l2(A1) | A2]);
%% this final clause accumulates line fragments
l1([H|T], A1, A2)          -> l1(T, [H |A1] , A2).

l2([])   -> [];
l2(List) -> {string, flatten(reverse(List))}.

%% need to put in regexes for urls and e-mail addies
openingdiv(String) ->
    case get_url(String) of
        {{url, URL}, R1} -> {{url, URL}, R1};
        not_url          ->
            case get_email_addie(String) of
                {{email, EM}, R2} -> {{email, EM}, R2};
                not_email         -> openingdiv1(String, [])
            end
    end.

% dumps out a list if it is not an opening div
openingdiv1([], Acc)         -> {flatten([{{punc, bra}, "<"}
                                         | lex(reverse(Acc))]), []};  
openingdiv1([$/,$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                                Tag = string:to_upper(Acc2),
                                {{{{{tag, self_closing}, Tag}, "<"
                                   ++ Acc2 ++ ">"}, Acc2}, T};
openingdiv1([$>| T], Acc)    -> Acc2 = flatten(reverse(Acc)),
                                Tag = string:to_upper(Acc2),
                                {{{{tag, open}, Tag}, "<"
                                  ++ Acc2 ++ ">"}, T};
openingdiv1([H|T], Acc)      -> openingdiv1(T, [H | Acc]).

% dumps out a list if it is not an closing div
closingdiv([], Acc)     -> {flatten([{{punc, bra}, "<"},
                                     {{punc, fslash}, "/"}
                                     | lex(reverse(Acc))]), []};  
closingdiv([$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                            Tag = string:to_upper(Acc2),
                            {{{{tag, close}, Tag}, "</"
                              ++ Acc2 ++ ">"}, T};
closingdiv([H|T], Acc)   -> closingdiv(T, [H | Acc]).

get_url(String) -> HTTP_regex = "^(H|h)(T|t)(T|t)(P|p)(S|s)*://",
                   case re:run(String, HTTP_regex) of
                       nomatch    -> not_url;
                       {match, _} -> get_url1(String, [])
                   end.

get_url1([], Acc)            -> URL = flatten(reverse(Acc)),
                                {{url, URL}, []};
% allow escaped kets
get_url1([$\\, $> | T], Acc) -> get_url1(T, [$>, $\\ | Acc]);
get_url1([$> | T], Acc)      -> URL = flatten(reverse(Acc)),
                                {{url, URL}, T};
get_url1([H | T], Acc)       -> get_url1(T, [H | Acc]).

get_email_addie(String) ->
    Snip_regex = ">",
    case re:run(String, Snip_regex) of
        nomatch                -> not_email;
        {match, [{N, _} | _T]} ->
            {Possible, [$> | T]} = lists:split(N, String),
            EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
                ++ "(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
                ++ "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
                ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
                ++ "|biz|info|mobi|name|aero|jobs|museum)",
            case re:run(Possible, EMail_regex) of
                nomatch    -> not_email;
                {match, _} -> {{email, Possible}, T}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_plain_str(List) -> m_plain(List, []).

m_plain([], Acc)             -> flatten(reverse(Acc));
m_plain([{_, Str} | T], Acc) -> m_plain(T, [Str | Acc]).

make_esc_str(List, Refs) -> m_esc(List, Refs, []).

m_esc([], _R, A)               -> flatten(reverse(A));
m_esc([{tag, Tag} | T], R, A)  -> m_esc(T, R, [Tag | A]);
m_esc([H | T], R, A)           -> m_esc(T, R, [make_str([H], R) | A]).

    
make_str(List, Refs) -> m_str1(List, Refs, []).

m_str1([], _R, A) ->
    Flat = flatten(reverse (A)),
    htmlchars(Flat);
m_str1([{{punc, bang}, B}, {{inline, open}, O} | T], R, A) ->
    case get_inline(T, R, []) of
        {Rest, {Url, Title, Acc}} -> Tag = [{unesc_tag, "<img src=\""}, Url 
                                            ++ "\" alt=\"" ++ Acc ++ "\""
                                            ++ " title=\"" ++ Title ++ "\"" ,
                                            {unesc_tag," />"}],
                                     m_str1(Rest, R, [Tag | A]);
        {Rest, Tag}               -> m_str1(Rest, R, [Tag, O, B | A])
    end;
%% escape inline open's...
m_str1([{{punc, bslash}, _}, {{inline, open}, O} | T], R, A) ->
    m_str1(T, R, [O | A]);
m_str1([{{inline, open}, O} | T], R, A) ->
    case get_inline(T, R, []) of
        %% this tag is for compatibility with showdown
        {Rest, {Url, [], Acc}} -> Tag = [{unesc_tag, "<a href=\""}, Url, 
                                            {unesc_tag, "\">"},  Acc,
                                            {unesc_tag, "</a>"} | []],
                                     m_str1(Rest, R, [Tag | A]);
        {Rest, {Url, Title, Acc}} -> Tag = [{unesc_tag, "<a href=\""}, Url 
                                            ++ "\" title=\"" ++ Title,
                                            {unesc_tag, "\">"},  Acc,
                                            {unesc_tag, "</a>"} | []],
                                     m_str1(Rest, R, [Tag | A]);
        {Rest, Tag}               -> m_str1(Rest, R, [Tag, O | A])
    end;
m_str1([{email, Addie} | T], R, A) ->
    m_str1(T, R, [{unesc_tag, "\" />"}, Addie, {unesc_tag, "<a href=\"mailto:"}| A]);
m_str1([{url, Url} | T], R, A) ->
    m_str1(T, R, [ {unesc_tag, "</a>"}, Url, {unesc_tag, "\">"}, Url,
                   {unesc_tag, "<a href=\""} | A]);
m_str1([{{{tag, _}, _}, Tag} | T], R, A)  ->
    m_str1(T, R, [{tag, Tag} | A]);
m_str1([{_, Orig} | T], R, A)  ->
    m_str1(T, R, [Orig | A]).

% if the inline doesn't terminate its not an inline...
get_inline([], _R, A) ->
    {[], make_plain_str(reverse(A))};
get_inline([{{inline, close}, _}, {bra, _} | T], _R, A) ->
    {Rest, Url, Title} = parse_inline(T),
    Tag = {string:strip(make_plain_str(Url)),
           make_plain_str(Title),
           make_plain_str(reverse(A))},
    {Rest, Tag};
%% this clause detects references to images/links...
get_inline([{{inline, close}, _}, {{inline, open}, _} | T], R, A) ->
    Text = make_plain_str(reverse(A)),
    {Id, Rest} = get_id_diff(T),
    {Url, Title} = case lists:keyfind(Id, 1, R) of
                       false          -> {"", ""};
                       {Id, {U, Tit}} -> {U, Tit}
          end,
    Tag = {Url, Title, Text},
    {Rest, Tag};
%% so does this one - just delete the space and rethrow it
get_inline([{{inline, close}, _} = C , {{ws, _}, _}, {{inline, open}, _} = O | T], R, A) ->
    get_inline([C, O | T], R, A);
%% this is the markdown extension clause that takes an id in square brackets without
%% any additional stuff as a valid id marker
get_inline([{{inline, close}, _} | T], R, A) ->
    Id = make_plain_str(reverse(A)),
    Text = "",
    case lists:keyfind(Id, 1, R) of
                       false              -> {T, flatten([Id , $]])};
                       {Id, {Url, Title}} -> Tag = {Url, Title, Text},
                                             {T, Tag}
          end;
get_inline([H | T], R, A) ->
    get_inline(T, R, [H | A]).

get_id_diff(List) -> g_id_diff1(List, []).

g_id_diff1([], _Acc)                         -> normal;
g_id_diff1([{{inline, close}, _}| T], Acc)   -> {make_plain_str(reverse(Acc)),
                                                 T};
g_id_diff1([H | T], Acc)                     -> g_id_diff1(T, [H | Acc]).

htmlchars(List) -> htmlchars(List, []).
 
htmlchars([], Acc) -> flatten(reverse(Acc));
%% unescapted tags are just wheeched out unescaped
htmlchars([{unesc_tag, Tag} | T], Acc) -> htmlchars(T, [Tag | Acc]);
%% normal tags are escaped...
htmlchars([{tag, Tag} | T], Acc)       -> htmlchars(T, [esc(Tag) | Acc]);
%% line ends are pushed to a space..
htmlchars([?LF | T], Acc)          -> htmlchars(T, ["\n" | Acc]);
htmlchars([?CR, ?LF | T], Acc)     -> htmlchars(T, ["\n" | Acc]);
%% emphasis is a bit strange - must be preceeded by or followed by
%% white space to work and can also be escaped
%% there is a non-space filling white space represented by the atom 'none'
%% which is created in the parser (NOT IN THE LEXER!) and which triggers
%% emphasis or strong tags being turned on...
htmlchars([$\\, $*, $*, $* | T], A)    -> htmlchars(T, [$*, $*, $* | A]);
htmlchars([?TAB, $*, $*, $* | T], A)   -> {T2, NewA} = superstrong(T, $*),
                                          htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $*, $*, $* | T], A) -> {T2, NewA} = superstrong(T, $*),
                                          htmlchars(T2, [NewA, ?SPACE | A]);
%% the none atom is the non-space filling whitespace 
htmlchars([none, $*, $*, $* | T], A)   -> {T2, NewA} = superstrong(T, $*),
                                          htmlchars(T2, [NewA | A]);
htmlchars([$*, $*, $* | T], A)         -> htmlchars(T, [$*, $*, $* | A]);
% repeat for strong
htmlchars([$\\, $*, $* | T], A)    -> htmlchars(T, [$*, $* | A]);
htmlchars([?TAB, $*, $* | T], A)   -> {T2, NewA} = strong(T, $*),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $*, $* | T], A) -> {T2, NewA} = strong(T, $*),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
%% the none atom is the non-space filling whitespace 
htmlchars([none, $*, $* | T], A)   -> {T2, NewA} = strong(T, $*),
                                      htmlchars(T2, [NewA | A]);
htmlchars([$*, $* | T], A)         -> htmlchars(T, [$*, $* | A]);
%% likewise for emphasis
htmlchars([$\\, $* | T], A)        -> htmlchars(T, [$* | A]);
htmlchars([?TAB, $* | T], A)       -> {T2, NewA} = emphasis(T, $*),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $* | T], A)     -> {T2, NewA} = emphasis(T, $*),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
%% the null list character is the non-space filling whitespace 
htmlchars([none, $* | T], A)       -> {T2, NewA} = emphasis(T, $*),
                                      htmlchars(T2, [NewA | A]);
htmlchars([$* | T], A)             -> htmlchars(T, [$* | A]);
%% and again for underscores
htmlchars([$\\, $_, $_, $_ | T], A)    -> htmlchars(T, [$_, $_, $_ | A]);
htmlchars([?TAB, $_, $_, $_ | T], A)   -> {T2, NewA} = superstrong(T, $_),
                                          htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $_, $_, $_ | T], A) -> {T2, NewA} = superstrong(T, $_),
                                          htmlchars(T2, [NewA, ?SPACE | A]);
%% the none atom is the non-space filling whitespace 
htmlchars([none, $_, $_, $_ | T], A)   -> {T2, NewA} = superstrong(T, $_),
                                          htmlchars(T2, [NewA | A]);
htmlchars([$_, $_, $_ | T], A)         -> htmlchars(T, [$_, $_, $_ | A]);
% and strong
htmlchars([$\\, $_, $_ | T], A)    -> htmlchars(T, [$_, $_ | A]);
htmlchars([?TAB, $_, $_ | T], A)   -> {T2, NewA} = strong(T, $_),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $_, $_ | T], A) -> {T2, NewA} = strong(T, $_),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
%% the null list character is the non-space filling whitespace 
htmlchars([none, $_, $_| T], A)   -> {T2, NewA} = strong(T, $_),
                                     htmlchars(T2, [NewA | A]);
htmlchars([$_, $_ | T], A)         -> htmlchars(T, [$_, $_ | A]);
%% likewise for emphasis
htmlchars([$\\, $_ | T], A)        -> htmlchars(T, [$_ | A]);
htmlchars([?TAB, $_ | T], A)       -> {T2, NewA} = emphasis(T, $_),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $_ | T], A)     -> {T2, NewA} = emphasis(T, $_),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
%% the null list character is the non-space filling whitespace 
htmlchars([none, $_ | T], A)       -> {T2, NewA} = emphasis(T, $_),
                                      htmlchars(T2, [NewA | A]);
htmlchars([$_ | T], A)             -> htmlchars(T, [$_ | A]);

%% handle backtick escaping
htmlchars([$\\, $` | T], A)        ->  htmlchars(T, [$` | A]);
htmlchars([$`, $` | T], A)         -> {T2, NewA} = dblcode(T),  
                                       htmlchars(T2, [NewA | A]);
htmlchars([$` | T], A)             -> {T2, NewA} = code(T),
                                      NewA2 = flatten_code(NewA),
                                      htmlchars(T2, [NewA2 | A]);
htmlchars([$& | T], A)             -> htmlchars(T, ["&amp;" | A]);
% htmlchars([$< | T], A)             -> htmlchars(T, ["&lt;" | A]);
% htmlchars([$> | T], A)             -> htmlchars(T, ["&gt;" | A]);
htmlchars([?NBSP | T], A)          -> htmlchars(T, ["&nbsp;" | A]);
htmlchars([H | T], A)              -> htmlchars(T, [H | A]).

flatten_code(List) -> f_code1(List, []).

f_code1([], Acc)               -> reverse(Acc);
f_code1([{tag, Tag} | T], Acc) -> f_code1(T, [esc(Tag) | Acc]);
f_code1([H | T], Acc)          -> f_code1(T, [H | Acc]).

esc(List) -> e1(List, []).

e1([], Acc)       -> reverse(Acc);
e1([$< | T], Acc) -> e1(T, ["&lt;" | Acc]);
e1([$> | T], Acc) -> e1(T, ["&gt;" | Acc]);
e1([H | T], Acc)  -> e1(T, [H | Acc]).
                             
emphasis(List, Delim)      -> interpolate(List, Delim, "em", []).
strong(List, Delim)        -> interpolate2(List, Delim, "strong", []).
superstrong(List, Delim)   -> interpolate3(List, Delim, "strong", "em", []).
dblcode(List)              -> {T, Tag} = interpolate2(List, $`, "code", []),
                              {T, "<pre>" ++ Tag ++ "</pre>"}.
code(List)                 -> interpolate(List, $`, "code", []).

%% interpolate is for single delimiters...
interpolate([], _Delim, Tag,  Acc)        -> {[], "<" ++ Tag ++ ">"
                                                 ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate([Delim | T], Delim, Tag, Acc) -> {T,  "<" ++ Tag ++ ">"
                                              ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate([H | T], Delim, Tag,  Acc)    -> interpolate(T, Delim, Tag, [H | Acc]).

%% interpolate two is for double delimiters...
interpolate2([], _Delim, Tag,  Acc)               -> {[], "<" ++ Tag ++ ">"
                                                      ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate2([Delim, Delim | T], Delim, Tag, Acc) -> {T,  "<" ++ Tag ++ ">"
                                                  ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate2([H | T], Delim, Tag,  Acc)           -> interpolate2(T, Delim, Tag, [H | Acc]).

%% interpolate three is for double delimiters...
interpolate3([], _D, Tag1, Tag2,  Acc)          -> {[], "<" ++ Tag1 ++ ">"
                                                    ++ "<" ++ Tag2 ++ ">"
                                                    ++ reverse(Acc)
                                                    ++ "</" ++ Tag2 ++ ">"
                                                    ++ "</" ++ Tag1 ++ ">"};
interpolate3([D, D, D | T], D, Tag1, Tag2, Acc) -> {T,  "<" ++ Tag1 ++ ">"
                                                    ++  "<" ++ Tag2 ++ ">"
                                                    ++ reverse(Acc)
                                                    ++ "</" ++ Tag2 ++ ">"
                                                    ++ "</" ++ Tag1 ++ ">"};
interpolate3([H | T], D, Tag1, Tag2,  Acc)      -> interpolate3(T, D, Tag1, Tag2, [H | Acc]).


%%%-------------------------------------------------------------------
%%%
%%% Unit Tests
%%%
%%%-------------------------------------------------------------------

%% we use a (very slightly) patched version which does no mark down if the input
%% consists of a single line of text, so all the tests are written to be at
%% least two lines just to stop all the unit tests breaking when we patch
%% the code for our implementation
%% Gordon Guthrie
%% (our patch is the first commented out clause of the function parse/1)

unit_test_() -> 
    [
     ?_assert(conv("3 > 4\na") == "<p>3 > 4\na</p>"),
     ?_assert(conv(" 3 < 4\na") == "<p>3 &lt; 4\na</p>"),
     ?_assert(conv("Hey Ho!\na") == "<p>Hey Ho!\na</p>"),
     ?_assert(conv("Hey\nHo!\nHardy\n\n") == "<p>Hey\nHo!\nHardy</p>"),
     ?_assert(conv("Hey Ho  \nLets Go") == "<p>Hey Ho <br />\nLets Go</p>")
%      ?_assert(conv("<ab:c\na") == "<p><ab:c\na</p>"),
%      ?_assert(conv("</ab:c\na") == "<p></ab:c\na</p>"),
%      ?_assert(conv("/ab:c\na") == "<p>/ab:c\na</p>"),
%      ?_assert(conv("=ab:c\na") == "<p>=ab:c\na</p>"),
%      ?_assert(conv("-ab:c\na") == "<p>-ab:c\na</p>"),
%      ?_assert(conv("#ab:c") == "<h1>ab:c</h1>"),
%      ?_assert(conv(">ab:c\na") == "<blockquote>\n  <p>ab:c\n  a</p>\n</blockquote>"),
%      ?_assert(conv("+ab:c\na") == "<p>+ab:c\na</p>"),
%      ?_assert(conv("*ab:c\na") == "<p>*ab:c\na</p>"),
%      ?_assert(conv("_ab:c\na") == "<p>_ab:c\na</p>"),
%      ?_assert(conv("1ab:c\na") == "<p>1ab:c\na</p>"),
%      ?_assert(conv("2ab:c\na") == "<p>2ab:c\na</p>"),
%      ?_assert(conv("3ab:c\na") == "<p>3ab:c\na</p>"),
%      ?_assert(conv("4ab:c\na") == "<p>4ab:c\na</p>"),
%      ?_assert(conv("5ab:c\na") == "<p>5ab:c\na</p>"),
%      ?_assert(conv("6ab:c\na") == "<p>6ab:c\na</p>"),
%      ?_assert(conv("7ab:c\na") == "<p>7ab:c\na</p>"),
%      ?_assert(conv("8ab:c\na") == "<p>8ab:c\na</p>"),
%      ?_assert(conv("9ab:c\na") == "<p>9ab:c\na</p>"),
%      ?_assert(conv("0ab:c\na") == "<p>0ab:c\na</p>"),
%      ?_assert(conv(".ab:c\na") == "<p>.ab:c\na</p>"),
%      ?_assert(conv(":ab:c\na") == "<p>:ab:c\na</p>"),
%      ?_assert(conv("'ab:c\na") == "<p>'ab:c\na</p>"),
%      ?_assert(conv("\"ab:c\na") == "<p>\"ab:c\na</p>"),
%      ?_assert(conv("`ab:c\na") == "<p>`ab:c\na</p>"),
%      ?_assert(conv("!ab:c\na") == "<p>!ab:c\na</p>"),
%      ?_assert(conv("\ab:c\na") == "<p>\ab:c\na</p>"),
%      ?_assert(conv("/ab:c\na") == "<p>/ab:c\na</p>"),
%      ?_assert(conv("(ab:c\na") == "<p>(ab:c\na</p>"),
%      ?_assert(conv(")ab:c\na") == "<p>)ab:c\na</p>"),
%      ?_assert(conv("[ab:c\na") == "<p>[ab:c\na</p>"),
%      ?_assert(conv("]ab:c\na") == "<p>]ab:c\na</p>"),
%       ?_assert(conv("(ab:c\na") == "<p>(ab:c\na</p>"),
%      ?_assert(conv(" ab:c\na") == "<p>ab:c\na</p>"),
%      ?_assert(conv("	ab:c") == "<pre><code>ab:c\n</code></pre>"),
%      ?_assert(conv("\nab:c\na") == "<p>ab:c\na</p>")
     %      ?_assert(conv("
% \nab:c\na") == "<p>ab:c\na</p>"),
%      ?_assert(conv("< ab:c\na") == "<p>&lt; ab:c\na</p>"),
%      ?_assert(conv("< /ab:c\na") == "<p>&lt; /ab:c\na</p>"),
%      ?_assert(conv("/ ab:c\na") == "<p>/ ab:c\na</p>"),
%      ?_assert(conv("= ab:c\na") == "<p>= ab:c\na</p>"),
%      ?_assert(conv("- ab:c") == "<ul>\n<li>ab:c</li>\n</ul>"),
%      ?_assert(conv("# ab:c") == "<h1>ab:c</h1>"),
%      ?_assert(conv("> ab:c\na") == "<blockquote>\n  <p>ab:c\n  a</p>\n</blockquote>"),
%      ?_assert(conv("+ ab:c") == "<ul>\n<li>ab:c</li>\n</ul>"),
%      ?_assert(conv("* ab:c") == "<ul>\n<li>ab:c</li>\n</ul>"),
%      ?_assert(conv("_ ab:c\na") == "<p>_ ab:c\na</p>"),
%      ?_assert(conv("1 ab:c\na") == "<p>1 ab:c\na</p>"),
%      ?_assert(conv("2 ab:c\na") == "<p>2 ab:c\na</p>"),
%      ?_assert(conv("3 ab:c\na") == "<p>3 ab:c\na</p>"),
%      ?_assert(conv("4 ab:c\na") == "<p>4 ab:c\na</p>"),
%      ?_assert(conv("5 ab:c\na") == "<p>5 ab:c\na</p>"),
%      ?_assert(conv("6 ab:c\na") == "<p>6 ab:c\na</p>"),
%      ?_assert(conv("7 ab:c\na") == "<p>7 ab:c\na</p>"),
%      ?_assert(conv("8 ab:c\na") == "<p>8 ab:c\na</p>"),
%      ?_assert(conv("9 ab:c\na") == "<p>9 ab:c\na</p>"),
%      ?_assert(conv("0 ab:c\na") == "<p>0 ab:c\na</p>"),
%      ?_assert(conv(". ab:c\na") == "<p>. ab:c\na</p>"),
%      ?_assert(conv(": ab:c\na") == "<p>: ab:c\na</p>"),
%      ?_assert(conv("' ab:c\na") == "<p>' ab:c\na</p>"),
%      ?_assert(conv("\" ab:c\na") == "<p>\" ab:c\na</p>"),
%      ?_assert(conv("` ab:c\na") == "<p>` ab:c\na</p>"),
%      ?_assert(conv("! ab:c\na") == "<p>! ab:c\na</p>"),
%      ?_assert(conv("\ ab:c\na") == "<p>\ ab:c\na</p>"),
%      ?_assert(conv("/ ab:c\na") == "<p>/ ab:c\na</p>"),
%      ?_assert(conv("( ab:c\na") == "<p>( ab:c\na</p>"),
%      ?_assert(conv(") ab:c\na") == "<p>) ab:c\na</p>"),
%      ?_assert(conv("[ ab:c\na") == "<p>[ ab:c\na</p>"),
%      ?_assert(conv("] ab:c\na") == "<p>] ab:c\na</p>"),
%      ?_assert(conv("( ab:c\na") == "<p>( ab:c\na</p>"),
%      ?_assert(conv("  ab:c\na") == "<p>ab:c\na</p>"),
%      ?_assert(conv("	 ab:c") == "<pre><code> ab:c\n</code></pre>"),
%      ?_assert(conv("\n ab:c\na") == "<p>ab:c\na</p>"),
%      ?_assert(conv("
% \n ab:c\na") == "<p>ab:c\na</p>"),
%      ?_assert(conv("xyz<ab:c\na") == "<p>xyz<ab:c\na</p>"),
%      ?_assert(conv("xyz</ab:c\na") == "<p>xyz</ab:c\na</p>"),
%      ?_assert(conv("xyz/ab:c\na") == "<p>xyz/ab:c\na</p>"),
%      ?_assert(conv("xyz=ab:c\na") == "<p>xyz=ab:c\na</p>"),
%      ?_assert(conv("xyz-ab:c\na") == "<p>xyz-ab:c\na</p>"),
%      ?_assert(conv("xyz#ab:c\na") == "<p>xyz#ab:c\na</p>"),
%      ?_assert(conv("xyz>ab:c\na") == "<p>xyz>ab:c\na</p>"),
%      ?_assert(conv("xyz+ab:c\na") == "<p>xyz+ab:c\na</p>"),
%      ?_assert(conv("xyz*ab:c\na") == "<p>xyz*ab:c\na</p>"),
%      ?_assert(conv("xyz_ab:c\na") == "<p>xyz_ab:c\na</p>"),
%      ?_assert(conv("xyz1ab:c\na") == "<p>xyz1ab:c\na</p>"),
%      ?_assert(conv("xyz2ab:c\na") == "<p>xyz2ab:c\na</p>"),
%      ?_assert(conv("xyz3ab:c\na") == "<p>xyz3ab:c\na</p>"),
%      ?_assert(conv("xyz4ab:c\na") == "<p>xyz4ab:c\na</p>"),
%      ?_assert(conv("xyz5ab:c\na") == "<p>xyz5ab:c\na</p>"),
%      ?_assert(conv("xyz6ab:c\na") == "<p>xyz6ab:c\na</p>"),
%      ?_assert(conv("xyz7ab:c\na") == "<p>xyz7ab:c\na</p>"),
%      ?_assert(conv("xyz8ab:c\na") == "<p>xyz8ab:c\na</p>"),
%      ?_assert(conv("xyz9ab:c\na") == "<p>xyz9ab:c\na</p>"),
%      ?_assert(conv("xyz0ab:c\na") == "<p>xyz0ab:c\na</p>"),
%      ?_assert(conv("xyz.ab:c\na") == "<p>xyz.ab:c\na</p>"),
%      ?_assert(conv("xyz:ab:c\na") == "<p>xyz:ab:c\na</p>"),
%      ?_assert(conv("xyz'ab:c\na") == "<p>xyz'ab:c\na</p>"),
%      ?_assert(conv("xyz\"ab:c\na") == "<p>xyz\"ab:c\na</p>"),
%      ?_assert(conv("xyz`ab:c\na") == "<p>xyz`ab:c\na</p>"),
%      ?_assert(conv("xyz!ab:c\na") == "<p>xyz!ab:c\na</p>"),
%      ?_assert(conv("xyz\ab:c\na") == "<p>xyz\ab:c\na</p>"),
%      ?_assert(conv("xyz/ab:c\na") == "<p>xyz/ab:c\na</p>"),
%      ?_assert(conv("xyz(ab:c\na") == "<p>xyz(ab:c\na</p>"),
%      ?_assert(conv("xyz)ab:c\na") == "<p>xyz)ab:c\na</p>"),
%      ?_assert(conv("xyz[ab:c\na") == "<p>xyz[ab:c\na</p>"),
%      ?_assert(conv("xyz]ab:c\na") == "<p>xyz]ab:c\na</p>"),
%      ?_assert(conv("xyz(ab:c\na") == "<p>xyz(ab:c\na</p>"),
%      ?_assert(conv("xyz ab:c\na") == "<p>xyz ab:c\na</p>"),
%      ?_assert(conv("xyz	ab:c\na") == "<p>xyz    ab:c\na</p>"),
%      ?_assert(conv("xyz\nab:c\na") == "<p>xyz\nab:c\na</p>"),
%      ?_assert(conv("xyz
% \nab:c\na") == "<p>xyz\nab:c\na</p>"),
%      ?_assert(conv("abc\`def\na") == "<p>abc`def\na</p>"),
%      ?_assert(conv("=\na") == "<p>=\na</p>"),
%      ?_assert(conv("-\na") == "<p>-\na</p>"),
%      ?_assert(conv(">\na") == "<p>>\na</p>"),
%      ?_assert(conv("[\na") == "<p>[\na</p>"),
%      ?_assert(conv("\n=\na") == "<p>=\na</p>"),
%      ?_assert(conv("\n-\na") == "<p>-\na</p>"),
%      ?_assert(conv("\n>\na") == "<p>>\na</p>"),
%      ?_assert(conv("\n[\na") == "<p>[\na</p>"),
%      ?_assert(conv("#\na") == "<p>#\na</p>"),
%      ?_assert(conv("##\na") == "<h1>#</h1>\n\n<p>a</p>"),
%      ?_assert(conv("###\na") == "<h2>#</h2>\n\n<p>a</p>"),
%      ?_assert(conv("####\na") == "<h3>#</h3>\n\n<p>a</p>"),
%      ?_assert(conv("#####\na") == "<h4>#</h4>\n\n<p>a</p>"),
%      ?_assert(conv("######\na") == "<h5>#</h5>\n\n<p>a</p>"),
%      ?_assert(conv("#######\na") == "<h6>#</h6>\n\n<p>a</p>"),
%      ?_assert(conv("########\na") == "<h6>#</h6>\n\n<p>a</p>"),
%      ?_assert(conv("you *sad* bastard\na") == "<p>you <em>sad</em> bastard\na</p>"),
%      ?_assert(conv("you **sad** bastard\na") == "<p>you <strong>sad</strong> bastard\na</p>"),
%      ?_assert(conv("you ***sad*** bastard\na") == "<p>you <strong><em>sad</em></strong> bastard\na</p>"),
%      ?_assert(conv("you _sad_ bastard\na") == "<p>you <em>sad</em> bastard\na</p>"),
%      ?_assert(conv("you __sad__ bastard\na") == "<p>you <strong>sad</strong> bastard\na</p>"),
%      ?_assert(conv("you ___sad___ bastard\na") == "<p>you <strong><em>sad</em></strong> bastard\na</p>"),
%      ?_assert(conv("you*sad*bastard\na") == "<p>you<em>sad</em>bastard\na</p>"),
%      ?_assert(conv("you_sad_bastard\na") == "<p>you<em>sad</em>bastard\na</p>"),
%      ?_assert(conv("you \*sad\* bastard\na") == "<p>you *sad* bastard\na</p>"),
%      ?_assert(conv("you \_sad\_ bastard\na") == "<p>you _sad_ bastard\na</p>"),
%      ?_assert(conv("*you* sad bastard\na") == "<p><em>you</em> sad bastard\na</p>"),
%      ?_assert(conv("**you** sad bastard\na") == "<p><strong>you</strong> sad bastard\na</p>"),
%      ?_assert(conv("***you*** sad bastard\na") == "<p><strong><em>you</em></strong> sad bastard\na</p>"),
%      ?_assert(conv("_you_ sad bastard\na") == "<p><em>you</em> sad bastard\na</p>"),
%      ?_assert(conv("__you__ sad bastard\na") == "<p><strong>you</strong> sad bastard\na</p>"),
%      ?_assert(conv("___you___ sad bastard\na") == "<p><strong><em>you</em></strong> sad bastard\na</p>"),
%      ?_assert(conv("blah\nblah") == "<p>blah\nblah</p>"),
%      ?_assert(conv("blah
% \nblah") == "<p>blah\nblah</p>"),
%      ?_assert(conv("blah
% \nblah  \n") == "<p>blah\nblah  </p>"),
%      ?_assert(conv("blahblah\n====") == "<h1>blahblah</h1>"),
%      ?_assert(conv("blahblah\n-----") == "<h2>blahblah</h2>"),
%      ?_assert(conv("blahblah\n====\nblah") == "<h1>blahblah</h1>\n\n<p>blah</p>"),
%      ?_assert(conv("blahblah\n-----\nblah") == "<h2>blahblah</h2>\n\n<p>blah</p>"),
%      ?_assert(conv("> a\n=") == "<h1>> a</h1>"),
%      ?_assert(conv("    a\n=") == "<h1>    a</h1>"),
%      ?_assert(conv("> a\n-") == "<h2>> a</h2>"),
%      ?_assert(conv("    a\n-") == "<h2>    a</h2>"),
%      ?_assert(conv("# blahblah") == "<h1>blahblah</h1>"),
%      ?_assert(conv("## blahblah") == "<h2>blahblah</h2>"),
%      ?_assert(conv("### blahblah") == "<h3>blahblah</h3>"),
%      ?_assert(conv("#### blahblah") == "<h4>blahblah</h4>"),
%      ?_assert(conv("##### blahblah") == "<h5>blahblah</h5>"),
%      ?_assert(conv("###### blahblah") == "<h6>blahblah</h6>"),
%      ?_assert(conv("####### blahblah") == "<h6># blahblah</h6>"),
%      ?_assert(conv("# blahblah ###") == "<h1>blahblah</h1>"),
%      ?_assert(conv("# blahblah\nbleh") == "<h1>blahblah</h1>\n\n<p>bleh</p>"),
%      ?_assert(conv("## blahblah\nbleh") == "<h2>blahblah</h2>\n\n<p>bleh</p>"),
%      ?_assert(conv("### blahblah\nbleh") == "<h3>blahblah</h3>\n\n<p>bleh</p>"),
%      ?_assert(conv("#### blahblah\nbleh") == "<h4>blahblah</h4>\n\n<p>bleh</p>"),
%      ?_assert(conv("##### blahblah\nbleh") == "<h5>blahblah</h5>\n\n<p>bleh</p>"),
%      ?_assert(conv("###### blahblah\nbleh") == "<h6>blahblah</h6>\n\n<p>bleh</p>"),
%      ?_assert(conv("####### blahblah\nbleh") == "<h6># blahblah</h6>\n\n<p>bleh</p>"),
%      ?_assert(conv("# blahblah ###\nbleh") == "<h1>blahblah</h1>\n\n<p>bleh</p>"),
%      ?_assert(conv("> blah\na") == "<blockquote>\n  <p>blah\n  a</p>\n</blockquote>"),
%      ?_assert(conv("bleh\n> blah") == "<p>bleh</p>\n\n<blockquote>\n  <p>blah</p>\n</blockquote>"),
%      ?_assert(conv("bleh  \n> blah") == "<p>bleh  </p>\n\n<blockquote>\n  <p>blah</p>\n</blockquote>"),
%      ?_assert(conv("bleh  \n> > blah") == "<p>bleh  </p>\n\n<blockquote>\n  <blockquote>\n    <p>blah</p>\n  </blockquote>\n</blockquote>"),
%      ?_assert(conv("+ blah") == "<ul>\n<li>blah</li>\n</ul>"),
%      ?_assert(conv("+blah\na") == "<p>+blah\na</p>"),
%      ?_assert(conv("* blah") == "<ul>\n<li>blah</li>\n</ul>"),
%      ?_assert(conv("*blah\na") == "<p>*blah\na</p>"),
%      ?_assert(conv("- blah") == "<ul>\n<li>blah</li>\n</ul>"),
%      ?_assert(conv("-blah\na") == "<p>-blah\na</p>"),
%      ?_assert(conv("- a\n+ b\n- c") == "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n</ul>"),
%      ?_assert(conv("- a\n\n+ b") == "<ul>\n<li><p>a</p></li>\n<li><p>b</p></li>\n</ul>"),
%      ?_assert(conv("- a\n\n+ b\n\n+ c\n* d") == "<ul>\n<li><p>a</p></li>\n<li><p>b</p></li>\n<li><p>c</p></li>\n<li>d</li>\n</ul>"),
%      ?_assert(conv("- blah\nblah") == "<ul>\n<li>blah\nblah</li>\n</ul>"),
%      ?_assert(conv("1. blah") == "<ol>\n<li>blah</li>\n</ol>"),
%      ?_assert(conv("4. blah") == "<ol>\n<li>blah</li>\n</ol>"),
%      ?_assert(conv("555. blah") == "<ol>\n<li>blah</li>\n</ol>"),
%      ?_assert(conv("555 @blah\na") == "<p>555 @blah\na</p>"),
%      ?_assert(conv("555.blah\na") == "<p>555.blah\na</p>"),
%      ?_assert(conv("4. blah\nblah") == "<ol>\n<li>blah\nblah</li>\n</ol>"),
%      ?_assert(conv("4. a\n5. b\n6. c") == "<ol>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n</ol>"),
%      ?_assert(conv("4. a\n\n5. b\n\n6. c") == "<ol>\n<li><p>a</p></li>\n<li><p>b</p></li>\n<li><p>c</p></li>\n</ol>"),
%      ?_assert(conv("    b") == "<pre><code>b\n</code></pre>"),
%      ?_assert(conv("	b") == "<pre><code>b\n</code></pre>"),
%      ?_assert(conv("	b\nc") == "<pre><code>b\n</code></pre>\n\n<p>c</p>"),
%      ?_assert(conv("	b\n\nc") == "<pre><code>b\n</code></pre>\n\n<p>c</p>"),
%      ?_assert(conv("+ a\n	    b") == "<ul>\n<li>a\n    b</li>\n</ul>"),
%      ?_assert(conv("+ a\n  	  b") == "<ul>\n<li>a\n    b</li>\n</ul>"),
%      ?_assert(conv("+ a\n		b") == "<ul>\n<li>a\n    b</li>\n</ul>"),
%      ?_assert(conv("+ a\n    > b") == "<p><ul>\n<li>a</p>\n\n<blockquote>\n  <p>b</li>\n  </ul></p>\n</blockquote>"),
%      ?_assert(conv("+ a\n	> b\nc") == "<p><ul>\n<li>a</p>\n\n<blockquote>\n  <p>b\n  c</li>\n  </ul></p>\n</blockquote>"),
%      ?_assert(conv("+ a\n	> b\nc\n\nd") == "<p><ul>\n<li>a</p>\n\n<blockquote>\n  <p>b\n  c</li>\n  </ul>\n  d</p>\n</blockquote>"),
%      ?_assert(conv("+ a\n	> b\nc\n\n\nd") == "<p><ul>\n<li>a</p>\n\n<blockquote>\n  <p>b\n  c</li>\n  </ul>\n  d</p>\n</blockquote>"),
%      ?_assert(conv("	<div>") == "<pre><code>&lt;div&gt;\n</code></pre>"),
%      ?_assert(conv("	<div>&") == "<pre><code>&lt;div&gt;&amp;\n</code></pre>"),
%      ?_assert(conv("+     blah<div>blah") == "<ul>\n<li>blah<div>blah</li>\n</ul>"),
%      ?_assert(conv("+        blah<div>blah") == "<ul>\n<li>blah<div>blah</li>\n</ul>"),
%      ?_assert(conv("-        blah<div>blah") == "<ul>\n<li>blah<div>blah</li>\n</ul>"),
%      ?_assert(conv("*        blah<div>blah") == "<ul>\n<li>blah<div>blah</li>\n</ul>"),
%      ?_assert(conv("\n\n<div>\n\n<table>\n\n</table>\n\n") == "<p><div></p>\n\n<table>\n\n</table>"),
%      ?_assert(conv("blah<div>blah\na") == "<p>blah<div>blah\na</p>"),
%      ?_assert(conv("***") == "<hr />"),
%      ?_assert(conv("---") == "<hr />"),
%      ?_assert(conv("___") == "<hr />"),
%      ?_assert(conv("*****") == "<hr />"),
%      ?_assert(conv("-----") == "<hr />"),
%      ?_assert(conv("_____") == "<hr />"),
%      ?_assert(conv("* * *") == "<hr />"),
%      ?_assert(conv("- - -") == "<hr />"),
%      ?_assert(conv("_ _ _") == "<hr />"),
%      ?_assert(conv("***blah\na") == "<p><em>*</em>blah\na</p>"),
%      ?_assert(conv("---blah\na") == "<p>---blah\na</p>"),
%      ?_assert(conv("___blah\na") == "<p><em>_</em>blah\na</p>"),
%      ?_assert(conv("a\na\n[id]: http://example.com \"Title\"") == "<p>a\na</p>"),
%      ?_assert(conv("a\na\n[id]: http://example.com 'Title'") == "<p>a\na\n[id]: http://example.com 'Title'</p>"),
%      ?_assert(conv("a\na\n[id]: http://example.com (Title)") == "<p>a\na</p>"),
%      ?_assert(conv("a\na\n[id]: http://example.com") == "<p>a\na</p>"),
%      ?_assert(conv("a\na\n[id]: <http://example.com>") == "<p>a\na</p>"),
%      ?_assert(conv("a\na\n   [id]: http://example.com") == "<p>a\na</p>"),
%      ?_assert(conv("a\na\n   [id]: /example.com") == "<p>a\na</p>"),
%      ?_assert(conv("a\n    \[id]: http://example.com/") == "<p>a\n    [id]: http://example.com/</p>"),
%      ?_assert(conv("a\n    [id]: http://example.com/") == "<p>a\n    [id]: http://example.com/</p>"),
%      ?_assert(conv("An [example] (http://example.com/ \"Title\") of link\na") == "<p>An [example] (http://example.com/ \"Title\") of link\na</p>"),
%      ?_assert(conv("An [example](http://example.com/ \"Title\") of link\na") == "<p>An <a href=\"http://example.com/\" title=\"Title\">example</a> of link\na</p>"),
%      ?_assert(conv("An [](http://example.com/ \"Title\") of link\na") == "<p>An <a href=\"http://example.com/\" title=\"Title\"></a> of link\na</p>"),
%      ?_assert(conv("An [example](http://example.com/) of link\na") == "<p>An <a href=\"http://example.com/\">example</a> of link\na</p>"),
%      ?_assert(conv("an ![Alt] (path/jpg.jpg \"Title\") image\na") == "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"Title\" /> image\na</p>"),
%      ?_assert(conv("an ![Alt](path/jpg.jpg \"Title\") image\na") == "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"Title\" /> image\na</p>"),
%      ?_assert(conv("an ![Alt](path/jpg.jpg 'Title') image\na") == "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"Title\" /> image\na</p>"),
%      ?_assert(conv("an ![Alt](path/jpg.jpg (Title)) image\na") == "<p>an !<a href=\"path/jpg.jpg (Title\">Alt</a>) image\na</p>"),
%      ?_assert(conv("an ![Alt](path/jpg.jpg ) image\na") == "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"\" /> image\na</p>"),
%      ?_assert(conv("an ![](path/jpg.jpg ) image\na") == "<p>an <img src=\"path/jpg.jpg\" alt=\"\" title=\"\" /> image\na</p>"),
%      ?_assert(conv("[id]: /a/path\nSome text [hey][id] there\na") == "<p>Some text <a href=\"/a/path\">hey</a> there\na</p>"),
%      ?_assert(conv("[id]: /a/path\nSome text [hey] [id] there\na") == "<p>Some text <a href=\"/a/path\">hey</a> there\na</p>"),
%      ?_assert(conv("[id]: /a/path\nSome text [hey]  [id] there\na") == "<p>Some text [hey]  <a href=\"/a/path\">id</a> there\na</p>"),
%      ?_assert(conv("[id]: /a/path\nSome text \[id] there\na") == "<p>Some text [id] there\na</p>"),
%      ?_assert(conv("[id]:   	 	   /a/path\nSome text [hey][id] there\na") == "<p>Some text <a href=\"/a/path\">hey</a> there\na</p>"),
%      ?_assert(conv("[id]: /a/path\nSome text ![hey][id] there\na") == "<p>Some text <img src=\"/a/path\" alt=\"hey\" title=\"\" /> there\na</p>"),
%      ?_assert(conv("[id]: /a/path\nSome text ![hey] [id] there\na") == "<p>Some text <img src=\"/a/path\" alt=\"hey\" title=\"\" /> there\na</p>"),
%      ?_assert(conv("[id]: /a/path\nSome text ![hey]  [id] there\na") == "<p>Some text ![hey]  <a href=\"/a/path\">id</a> there\na</p>"),
%      ?_assert(conv("blah <http://something.com:1234/a/path> blah\na") == "<p>blah <a href=\"http://something.com:1234/a/path\">http://something.com:1234/a/path</a> blah\na</p>"),
%      ?_assert(conv("blah <https://something.com:1234/a/path> blah\na") == "<p>blah <a href=\"https://something.com:1234/a/path\">https://something.com:1234/a/path</a> blah\na</p>"),
%      ?_assert(conv("blah <httpx://something.com:1234/a/path> blah\na") == "<p>blah <httpx://something.com:1234/a/path> blah\na</p>"),
%      ?_assert(conv("blah <junk@spam.com> blah\na") == "<p>blah <a href=\"&#109;&#97;&#105;&#108;&#116;&#111;:&#x6A;&#117;&#110;&#x6B;&#x40;&#x73;p&#x61;&#x6D;&#x2E;&#99;&#x6F;&#x6D;\">&#x6A;&#117;&#110;&#x6B;&#x40;&#x73;p&#x61;&#x6D;&#x2E;&#99;&#x6F;&#x6D;</a> blah\na</p>"),
%      ?_assert(conv("My [id] test\n[id]: http://example.com") == "<p>My <a href=\"http://example.com\">id</a> test</p>"),
%      ?_assert(conv("My [link][id] test\n[id]: http://example.com") == "<p>My <a href=\"http://example.com\">link</a> test</p>"),
%      ?_assert(conv("Now\n\n    who\n\n> swine\n\n") == "<p>Now</p>\n\n<pre><code>who\n</code></pre>\n\n<blockquote>\n  <p>swine</p>\n</blockquote>"),
%      ?_assert(conv("`<div>blah</div>`") == "<p><code>&lt;div&gt;blah&lt;/div&gt;</code></p>"),
%      ?_assert(conv("[link text][1]\n\n[1]: http://example.com \"optional title\"") == "<p><a href=\"http://example.com\" title=\"optional title\">link text</a></p>"),
%      ?_assert(conv("[link text][1]\n\n[1]: http://example.com") == "<p><a href=\"http://example.com\">link text</a></p>"),
%      ?_assert(conv("![alt text][1]\n\n[1]: http://example.com \"optional title\"") == "<p><img src=\"http://example.com\" alt=\"alt text\" title=\"optional title\" /></p>"),
%      ?_assert(conv("![alt text][1]\n\n[1]: http://example.com") == "<p><img src=\"http://example.com\" alt=\"alt text\" title=\"\" /></p>"),
%      ?_assert(conv("blah`tick`blah") == "<p>blah<code>tick</code>blah</p>"),
%      ?_assert(conv("  \n") == ""),
%      ?_assert(conv("  \n> ") == "<blockquote>\n  \n</blockquote>"),
%      ?_assert(conv("  \n> blah\n blah") == "<blockquote>\n  <p>blah\n   blah</p>\n</blockquote>"),
%      ?_assert(conv("  \n> blah\n> bleh\n\n") == "<blockquote>\n  <p>blah\n  bleh</p>\n</blockquote>"),
%      ?_assert(conv("  \n> blah\n bleh\n\n") == "<blockquote>\n  <p>blah\n   bleh</p>\n</blockquote>"),
%      ?_assert(conv("blah\n\n\nbleh") == "<p>blah</p>\n\n<p>bleh</p>"),
%      ?_assert(conv("> blah\n-------------") == "<h2>> blah</h2>")
    ].
