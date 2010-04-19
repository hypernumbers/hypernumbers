%% -*- erlang -*-

Definitions.

CONDITION1 = (\[([><=]|(>=)|(=>)|(=<)|(<=))(-)?([0-9]+)\])
CONDITION2 = (\[([><=]|(>=)|(=>)|(=<)|(<=))(-)?([0-9]+\.[0-9])*\])
CONDITION3 = (\[([><=]|(>=)|(=>)|(=<)|(<=))(-)?([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)\])

BLACK      = (\[(B|b)(L|l)(A|a)(C|c)(K|k)\])
BLUE       = (\[(B|b)(L|l)(U|u)(E|e)\])
CYAN       = (\[(C|c)(Y|y)(A|a)(N|n)\])
GREEN      = (\[(G|g)(R|r)(E|e)(E|e)(N|n)\])
MAGENTA    = (\[(M|m)(A|a)(G|g)(E|e)(N|n)(T|t)(A|a)\])
RED        = (\[(R|r)(E|e)(D|d)\])
WHITE      = (\[(W|w)(H|h)(I|i)(T|t)(E|e)\])
YELLOW     = (\[(Y|y)(E|e)(L|l)(L|l)(O|o)(W|w)\])

PLAIN      = (P|p)(L|l)(A|a)(I|i)(N|n)
GENERAL    = (G|g)(E|e)(N|n)(E|e)(R|r)(A|a)(L|l)
MARKDOWN   = (M|m)(A|a)(R|r)(K|k)(D|d)(O|o)(W|w)(N|n)

STRING     = (\"[^"\n]*\")

FORMAT1    = ((([#\?0]+,)*)([#\?0]+(\.([#\?0]+([E|e][\+|-][0-9]+)?)?)?))
FORMAT2    = ([#\?0]+([E|e][\+|-][0-9]+))
FRACTION   = ([#\?0]+(\s)[#\?0]+/[#\?O]+)

ELAPSED_H = (\[(H|h)\])
ELAPSED_M = (\[(M|m)\])
ELAPSED_S = (\[(S|s)\])

Rules.

{BLACK}      : {token, {colour, TokenLine, black}}.
{BLUE}       : {token, {colour, TokenLine, blue}}.
{CYAN}       : {token, {colour, TokenLine, cyan}}.
{GREEN}      : {token, {colour, TokenLine, green}}.
{MAGENTA}    : {token, {colour, TokenLine, magenta}}.
{RED}        : {token, {colour, TokenLine, red}}.
{WHITE}      : {token, {colour, TokenLine, white}}.
{YELLOW}     : {token, {colour, TokenLine, yellow}}.

{GENERAL}    : {token, {general, TokenLine, TokenChars}}.
{MARKDOWN}   : {token, {markdown, TokenLine, TokenChars}}.

{STRING}     : {token, {string, TokenLine, TokenChars}}. 

{FORMAT1}    : {token, {format, TokenLine, TokenChars}}.
{FORMAT2}    : {token, {format, TokenLine, TokenChars}}.
{FRACTION}   : {token, {fraction, TokenLine, TokenChars}}.

{CONDITION1} : {token, {condition, TokenLine, TokenChars}}.
{CONDITION2} : {token, {condition, TokenLine, TokenChars}}.
{CONDITION3} : {token, {condition, TokenLine, TokenChars}}.

{ELAPSED_M}  : {token, {min,     TokenLine, elapsed}}.
{ELAPSED_H}  : {token, {hour,    TokenLine, elapsed}}.
{ELAPSED_S}  : {token, {sec,     TokenLine, elapsed}}.

\\"          : {token, {quote, TokenLine, "\""}}.
\$           : {token, {dollar, TokenLine, "$"}}.
\-           : {token, {minus, TokenLine, "-"}}.
\+           : {token, {plus, TokenLine, "+"}}.
/            : {token, {fwdslash, TokenLine, "/"}}.
\(           : {token, {open_bra, TokenLine, "("}}.
\)           : {token, {close_ket, TokenLine, ")"}}.
:            : {token, {colon, TokenLine, ":"}}.
\s           : {token, {space, TokenLine, " "}}.


;            : {token, {semicolon, TokenLine, ";"}}.

_            : {token, {underscore, TokenLine, "_"}}.
\%           : {token, {percent, TokenLine, "%"}}.

\\           : {token, {esc, TokenLine, "\\"}}.
@            : {token, {at, TokenLine, "@"}}.

yy           : {token, {year,    TokenLine, two_digit}}.
YY           : {token, {year,    TokenLine, two_digit}}.
yyyy         : {token, {year,    TokenLine, four_digit}}.
YYYY         : {token, {year,    TokenLine, four_digit}}.
m            : {token, {mon_min, TokenLine, no_zero}}.
M            : {token, {mon_min, TokenLine, no_zero}}.
mm           : {token, {mon_min, TokenLine, zero}}.
MM           : {token, {mon_min, TokenLine, zero}}.
mmm          : {token, {mon,     TokenLine, abbr}}.
MMM          : {token, {mon,     TokenLine, abbr}}.
mmmm         : {token, {mon,     TokenLine, full}}.
MMMM         : {token, {mon,     TokenLine, full}}.
d            : {token, {day,     TokenLine, no_zero}}.
D            : {token, {day,     TokenLine, no_zero}}.
dd           : {token, {day,     TokenLine, zero}}.
DD           : {token, {day,     TokenLine, zero}}.
ddd          : {token, {day,     TokenLine, abbr}}.
DDD          : {token, {day,     TokenLine, abbr}}.
dddd         : {token, {day,     TokenLine, full}}.
DDDD         : {token, {day,     TokenLine, full}}.
h            : {token, {hour,    TokenLine, no_zero}}.
H            : {token, {hour,    TokenLine, no_zero}}.
hh           : {token, {hour,    TokenLine, zero}}.
HH           : {token, {hour,    TokenLine, zero}}.
s            : {token, {sec,     TokenLine, no_zero}}.
S            : {token, {sec,     TokenLine, no_zero}}.
ss           : {token, {sec,     TokenLine, zero}}.
SS           : {token, {sec,     TokenLine, zero}}.
AM/PM        : {token, {ampm,    TokenLine, full_caps}}.
am/pm        : {token, {ampm,    TokenLine, full_lowercase}}.
A/P          : {token, {ampm,    TokenLine, abbr_caps}}.
a/p          : {token, {ampm,    TokenLine, abbr_lowercase}}.

.            : {token, {char, TokenLine, TokenChars}}.

\n           : {end_token,{'$end', TokenLine}}.


Erlang code.
