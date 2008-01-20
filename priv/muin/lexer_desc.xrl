%%% @doc    Lexer for the main (Excel formula language) frontend.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

Definitions.

%% ----- Basic types.

INTEGER = ([0-9]+)

FLOAT_DECIMAL = ([0-9]+\.[0-9]*)
FLOAT_SCIENTIFIC = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)

%% Leex can't do case-insensitive. 14/32 variations for true/false -> full coverage.
TRUE = (true|TRUE|True|tRue|trUe|truE|TRue|TrUe|TruE|tRUe|tRuE|trUE|TRUe|TRuE|TrUE|tRUE)
FALSE = (false|FALSE|False|fAlse|faLse|falSe|falsE|FAlse|FaLse|FalSe|FalsE|fALse|fAlSe|fAlsE|faLSe|faLsE|falSE|faLSE|fAlSE|fALsE|fALSe|FalSE|FaLsE|FaLSe|FAlsE|FAlSe|FALse|fALSE|FaLSE|FAlSE|FALsE|FALSe)
BOOLEAN = {TRUE}|{FALSE}

STRING    = (\"[^"\n]*\")
%%" % Syntax highlighting fix.

%% Column names / function names.
ID = [a-zA-Z][a-zA-Z0-9_]*

%% Variables ("names" in Excel-speak).
VAR = _{ID}

ERROR = \#NULL\!|\#DIV\/0\!|\#VALUE\!|\#REF\!|\#NAME\?|\#NUM\!|\#N\/A\!

%%% ----- References

%% Single same-page cell reference, e.g. A1, B22, $A$5 etc.
CELLREF = ((\$)?[a-zA-Z]+(\$)?[0-9]+)
%% Same-page range reference, e.g. A1:B10
RANGEREF = ({CELLREF}:{CELLREF})

%% Range intersection operator (whitespace). Doing in the lexer for now.
INTERSECTION = (({RANGEREF})(\s+)({RANGEREF}))((\s+)({RANGEREF}))*

%% Same-site references.
%% "/" or "./" or "../" or the same but with "!"s instead.
START_OF_SSREF = ((\/|\!)|(\.\.(\/|\!))+|\.(\/|\!))
%% IDs or "."s or ".." separated with "/"s or "!"s.
MAYBE_PATH = (((({ID})|(\.)|(\.\.))(\/|\!))*)

SSCELLREF = {START_OF_SSREF}{MAYBE_PATH}{CELLREF}
SSCOLREF  = {START_OF_SSREF}{MAYBE_PATH}{ID}
SSROWREF  = {START_OF_SSREF}{MAYBE_PATH}{INTEGER}

%% Whitespace, duh.
WHITESPACE = ([\000-\s]*)


Rules.

%% Reference tokens.
{INTERSECTION}  : {token, {intersection, YYtext}}.
{CELLREF}       : {token, {cellref,      string:to_lower(YYtext)}}.
{SSCELLREF}     : {token, {sscellref,    muin_util:normalize_ssref(YYtext)}}.
{SSCOLREF}      : {token, {sscolref,     muin_util:normalize_ssref(YYtext)}}.
{SSROWREF}      : {token, {ssrowref,     muin_util:normalize_ssref(YYtext)}}.
                        
%% Basic data types.
{INTEGER}          : {token, {integer, muin_util:to_i(YYtext)}}.
{FLOAT_DECIMAL}    : {token, {float,   muin_util:to_f(YYtext)}}.
{FLOAT_SCIENTIFIC} : {token, {float,   muin_util:to_f(string:to_lower(YYtext))}}.
{BOOLEAN}          : {token, {boolean,
                              case string:to_lower(YYtext) of
                                  "true" -> true;
                                  _      -> false
                              end}}.
{STRING}  : {token, {string, muin_util:mid(YYtext)}}.

{ERROR}   : {token, {error, list_to_atom(YYtext)}}.

%% Stuff.
{VAR}     : {token, {var, string:to_lower(YYtext)}}.
{ID}      : {token, {id,  string:to_lower(YYtext)}}.

%% Discard whitespace.
{WHITESPACE} : .

%% Punctuation.
=  : {token, {eq}}.
,  : {token, {comma}}.
\( : {token, {open_paren}}.
\) : {token, {close_paren}}.
\{ : {token, {open_curly}}.
\} : {token, {close_curly}}.
\; : {token, {semicolon}}.

%% Operators.
%% Arithmetic.
\+ : {token, {plus}}.
\- : {token, {minus}}.
\* : {token, {times}}.
\/ : {token, {slash}}.
\^ : {token, {caret}}.

%% Logical.
>  : {token, {gt}}.
<  : {token, {lt}}.
>= : {token, {gte}}.
<= : {token, {lte}}.
<> : {token, {neq}}.

%% Other.
&  : {token, {concat}}.
\% : {token, {percent}}.
\: : {token, {colon}}.

%% Newline signifies end of input.
\n : {end_token, {'$end'}}.
%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, YYtext}}.
