%%% @doc    Lexer for Muin's formula language.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% NOTE: Expects all input to be in upper case (apart from
%%% contents of strings)!

Definitions.

%%% Basic types.
INT = ([0-9]+)
FLOATDEC = ([0-9]+\.[0-9]*)
FLOATSCI = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)
BOOL = TRUE|FALSE
STR    = (\"[^"\n]*\")
%%" % Syntax highlighting fix.

%% Column names, function names.
ATOM = ([a-zA-Z][a-zA-Z0-9_\.]*)
%% Named cells / ranges.
NAME = \@{ATOM}

ERROR = \#NULL\!|\#DIV\/0\!|\#VALUE\!|\#REF\!|\#NAME\?|\#NUM\!|\#N\/A

%%% A1-style references.
A1REF = ((\$)?([a-zA-Z]+)(\$)?([0-9]+))
%%% RC-style references.
%% Relative offset: int in brackets with optional + or - in front.
OFFSET_RC = (\[(\+|\-)?({INT})\])
RCREF = ((R|r)({INT}|{OFFSET_RC})(C|c)({INT}|{OFFSET_RC}))

%% Same-site references.
%% "/" or "./" or "../" or the same but with "!"s instead.
START_OF_SSREF = ((\/|\!)|(\.\.(\/|\!))+|\.(\/|\!))
%% ATOMs or "."s or ".." separated with "/"s or "!"s.
MAYBE_PATH = (((({ATOM})|(\.)|(\.\.))(\/|\!))*)

SSA1REF  = {START_OF_SSREF}{MAYBE_PATH}{A1REF}
SSRCREF  = {START_OF_SSREF}{MAYBE_PATH}{RCREF}
SSNAMEREF = {START_OF_SSREF}{MAYBE_PATH}{NAME}

%% Whitespace.
WHITESPACE = ([\000-\s]*)


Rules.

%% These are converted to universal refs, see lex/1.
{A1REF}    : {token, {a1ref, YYtext}}.
{SSA1REF}  : {token, {ssa1ref, debang(YYtext)}}.
{RCREF}    : {token, {rcref, YYtext}}.
{SSRCREF}  : {token, {ssrcref, debang(YYtext)}}.
{SSNAMEREF} : {token, {ssnameref, debang(YYtext)}}.
                
%% Basic data types.
{INT}      : {token, {int, tconv:to_i(YYtext)}}.
{FLOATDEC} : {token, {float, tconv:to_f(YYtext)}}.
{FLOATSCI} : {token, {float, tconv:to_f(YYtext)}}.
{BOOL}     : {token, {bool, YYtext == "TRUE"}}.
{STR}      : {token, {str, hslists:mid(YYtext)}}.

{ERROR} : {token, {error, list_to_atom(YYtext)}}.
{ATOM}  : {token, {atom,  string:to_lower(YYtext)}}.
{NAME}  : {token, {name,  tl(string:to_lower(YYtext))}}.

%% Discard whitespace.
{WHITESPACE} : .

%% Punctuation.
=  : {token, {'='}}.
,  : {token, {','}}.
\( : {token, {'('}}.
\) : {token, {')'}}.
\{ : {token, {'{'}}.
\} : {token, {'}'}}.
\; : {token, {';'}}.

%% Operators.
%% Arithmetic.
\+ : {token, {'+'}}.
\- : {token, {'-'}}.
\* : {token, {'*'}}.
\/ : {token, {'/'}}.
\^ : {token, {'^'}}.

%% Logical.
>  : {token, {'>'}}.
<  : {token, {'<'}}.
>= : {token, {'>='}}.
<= : {token, {'<='}}.
<> : {token, {'<>'}}.

%% Other.
&  : {token, {'&'}}.
\% : {token, {'%'}}.
\: : {token, {':'}}.

%% Newline signifies end of input.
\n : {end_token, {'$end'}}.
%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, YYtext}}.

Erlang code.

-export([lex/2]).
-include("handy_macros.hrl").

%% These are read-only.
-define(mx, get(mx)).
-define(my, get(my)).

lex(Input, {Mx, My}) ->
    put(mx, Mx),
    put(my, My),
    case string(Input) of
        {ok, Toks, _} ->
            {ok, map(fun normalize/1, Toks)};
        _ ->
            lexer_error
    end.

%% Lexing functions used in tests.
tlex(Input) ->
    ?ifmatch(string(Input),
             {ok, [{_Type, Val}], _},
             Val,
             foadplzkthx).
tlex(Input, Currcell) when is_list(Currcell) ->
    lex(Input, getxy(Currcell)).

%% Convert refs to universal format.
normalize({a1ref, Refstr}) ->
    Iscolfixed = (hd(Refstr) == $$), % Starts with dollar?
    Isrowfixed = member($$, tl(Refstr)), % There's a dollar in the tail?
    {X, Y} = getxy(Refstr),
    {ref, R, C} = norma1({X, Y}, {Iscolfixed, Isrowfixed}),
    {ref, R, C, "./"};
normalize({ssa1ref, Refstr}) ->
    {Path, Cellref} = split_ssref(Refstr),
    {ref, R, C, "./"} = normalize({a1ref, Cellref}),
    {ref, R, C, Path};
%% TODO: Rewrite this, too ugly.
normalize({rcref, Refstr}) ->
    %% Extract ints or [ints].
    {match, [{St, Len}, {St2, Len2}]} =
        regexp:matches(Refstr, "(\\[(\\+|\\-)?(([0-9]+))\\])|([0-9]+)"),
    Rowpart = string:substr(Refstr, St, Len),
    Colpart = string:substr(Refstr, St2, Len2),
    %% TODO: Same here as above ^^
    {match, St3, Len3} = regexp:match(Rowpart, "(\\+|\\-)?([0-9]+)"),
    {match, St4, Len4} = regexp:match(Colpart, "(\\+|\\-)?([0-9]+)"),
    Rowint = tconv:to_i(string:substr(Rowpart, St3, Len3)),
    Colint = tconv:to_i(string:substr(Colpart, St4, Len4)),
    Isrowfixed = not(member(91, Rowpart)), %% $[ -> 91
    Iscolfixed = not(member(91, Colpart)),
    {ref, R, C} = normrc({Colint, Rowint}, {Iscolfixed, Isrowfixed}),
    {ref, R, C, "./"};
normalize({ssrcref, Refstr}) ->
    {Path, Cellref} = split_ssref(Refstr),
    {ref, R, C, "./"} = normalize({rcref, Cellref}),
    {ref, R, C, Path};
normalize({ssnameref, Refstr}) ->
    {Path, Name} = split_ssref(Refstr),
    {name, Name, Path};
normalize({name, Name}) ->
    {name, Name, "./"};
normalize(Other) ->
    Other.

%% A1-style ref => universal ref.
norma1({X, Y}, _Col_Fixed_Row_Fixed = {false, false}) ->
    {ref, {col, X - ?mx}, {row, Y - ?my}};
norma1({X, Y}, {false, true}) ->
    {ref, {col, X - ?mx}, Y};
norma1({X, Y}, {true, false}) ->
    {ref, X, {row, Y - ?my}};
norma1({X, Y}, {true, true}) ->
    {ref, X, Y}.

%% RC ref => universal ref.
normrc({X, Y}, _Col_Fixed_Row_Fixed = {false, false}) ->
    {ref, {col, X}, {row, Y}};
normrc({X, Y}, {false, true}) ->
    {ref, {col, X}, Y};
normrc({X, Y}, {true, false}) ->
    {ref, X, {row, Y}};
normrc({X, Y}, {true, true}) ->
    {ref, X, Y}.

%% A1 ref => coordinates.
getxy(Cellref0) ->
    {ok, Cellref, _} = regexp:gsub(Cellref0, "\\$", ""), % Kill all $s.
    Colname = takewhile(fun is_alpha/1,
                        string:to_lower(Cellref)),
    Rowstr = lists:nthtail(length(Colname), Cellref),
    {tconv:to_i(Colname, b26), tconv:to_i(Rowstr)}.

%% Replaces all !s with /s in a same-site ref.
debang(Ssref) ->
    {ok, Newssref, _} = regexp:gsub(Ssref, "!", "/"),
    Newssref.

%% Splits same-site ref into path to page and cellref.
split_ssref(Ssref) ->
    Lastslash = string:rstr(Ssref, "/"),
    Path = lists:sublist(Ssref, Lastslash),
    Cellref = lists:nthtail(Lastslash, Ssref),
    {Path, Cellref}.

is_alpha(Char) when is_integer(Char) ->
    (member(Char, seq($A, $Z)) orelse
     member(Char, seq($a, $z)));
is_alpha([Char]) ->
    is_alpha(Char).


%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").
%% Check representation of cells C1 and C2 are same from cell Cfrom.
-define(check_same_from(C1, C2, Cfrom),
        ?_assert(tlex(C1, Cfrom) == tlex(C2, Cfrom))).

literals_test_() ->
    [
     %% **Floats**
     %% Floats can be in normal or scientific notation.
     %% Scientific notation is not case-sensitive.
     %% Scientific notation and normal notations are equivalent.
     ?_assert(tlex("1.2") == tlex("1.2E+0")),
     ?_assert(tlex("12.0") == tlex("1.2E+1")),
     ?_assert(tlex("12.0") == tlex("1.2e+1")),

     %% **Booleans**
     ?_assert(tlex(" TRUE ") == true),
     ?_assert(tlex(" FALSE  ") == false),
     %% Only stand-alone strings are matched.
     ?_assert(tlex("_TRUE_") =/= true),
     ?_assert(tlex("_FALSE") =/= false),
     ?_assert(tlex("TRUEfalse") =/= true),
     ?_assert(tlex("trueFALSE") =/= false)
    ].

a1refs_test_() ->
    [
     %% Straight-up same-page references can be done with ./ too.
     ?check_same_from("A1", "./A1", "B2"),
     ?check_same_from("a1", "./a1", "B2"),
     ?check_same_from("A1", "./a1", "B2")
    ].

rcrefs_test_() ->
    [
     ?check_same_from("R1C1", "r1C1", "B2"),
     ?check_same_from("R1C1", "./R1C1", "B2"),
     ?check_same_from("R[-1]C[+2]", "./r[-1]C[2]", "B2")
    ].

%% RC and A1-style refs are equivalent.
rc_a1_equiv_test_() ->
    [
     ?check_same_from("$A$1", "R1C1",       "B2"),
     ?check_same_from("A$1",  "R1C[-1]",    "B2"),
     ?check_same_from("$A1",  "R[-1]C1",    "B2"),
     ?check_same_from("A1",   "R[-1]C[-1]", "B2"),
     ?check_same_from("C3",   "R[+2]C[2]",  "A1")
    ].
