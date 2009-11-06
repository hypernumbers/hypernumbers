Definitions.

EQ               = (=)
NE               = (!=)
GT               = (>)
LT               = (<)
GE               = ([(>=)|(=>)])
LE               = ([(<=)|(=<)])

STRING           = ([a-zA-Z]+)
INTEGER          = ([0-9]+)
FLOAT_DECIMAL    = ([0-9]+\.[0-9]*)
FLOAT_SCIENTIFIC = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)
BOOLEAN          = {TRUE}|{FALSE}

Rules.


{EQ}               : {token, {equals,      TokenLine, '='}}.
{NE}               : {token, {notequals,   TokenLine, '<>'}}.
{GT}               : {token, {greaterthan, TokenLine, '>'}}.
{LT}               : {token, {lessthan,    TokenLine, '<'}}.
{GE}               : {token, {gtorequal,   TokenLine, '>='}}.
{LE}               : {token, {ltorequal,   TokenLine, '=<'}}.

{STRING}           : {token, {string,     TokenLine, TokenChars}}.
{INTEGER}          : {token, {integer,    TokenLine, TokenChars}}.
{FLOAT_DECIMAL}    : {token, {float,      TokenLine, TokenChars}}.
{FLOAT_SCIENTIFIC} : {token, {float,      TokenLine, TokenChars}}.
{BOOLEAN}          : {token, {string,     TokenLine, TokenChars}}.

\(           : {token, {open_bra, TokenLine, "("}}.
\)           : {token, {close_ket, TokenLine, ")"}}.
\{           : {token, {open_curly, TokenLine, "{"}}.
\}           : {token, {close_curly, TokenLine, "}"}}.
\[           : {token, {open_sq, TokenLine, "["}}.
\]           : {token, {close_sq, TokenLine, "]"}}.
;            : {token, {semicolon, TokenLine, ";"}}.
,            : {token, {comma, TokenLine, ","}}.
\*           : {token, {star, TokenLine, "*"}}.

\n           : {end_token,{'$end', TokenLine}}.


Erlang code.
