Definitions.

BLACK      = (\[(B|b)(L|l)(A|a)(C|c)(K|k)\])
BLUE       = (\[(B|b)(L|l)(U|u)(E|e)\])
CYAN       = (\[(C|c)(Y|y)(A|a)(N|n)\])
GREEN      = (\[(G|c)(R|r)(E|e)(E|e)(N|n)\])
MAGENTA    = (\[(M|m)(A|a)(G|g)(E|e)(N|n)(T|t)(A|a)\])
RED        = (\[(R|r)(E|e)(D|d)\])
WHITE      = (\[(W|w)(H|h)(I|i)(T|t)(E|e)\])
YELLOW     = (\[(Y|y)(E|e)(L|l)(L|l)(O|o)(W|w)\])

CONDITION1 = (\[([[><=]|((=>)|(>=)|(=<)|(<=)))(-)?([0-9]+)\])
CONDITION2 = (\[([[><=]|((=>)|(>=)|(=<)|(<=)))(-)?([0-9]+\.[0-9])*\])
CONDITION3 = (\[([[><=]|((=>)|(>=)|(=<)|(<=)))(-)?([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)\])

STRING     = (\"[^"\n]*\")

FORMAT     = ((([#\?0]+,)*)([#\?0]+(\.([#\?0]+([E|e][+|-][0-9]+)?)?)?),*)
FRACTION   = ([#\?0]+(\s)[#\?0]+/[#\?O]+)

Rules.

{BLACK}      : {token, {colour, black}}.
{BLUE}       : {token, {colour, blue}}.
{CYAN}       : {token, {colour, cyan}}.
{GREEN}      : {token, {colour, green}}.
{MAGENTA}    : {token, {colour, magenta}}.
{RED}        : {token, {colour, red}}.
{WHITE}      : {token, {colour, white}}.
{YELLOW}     : {token, {colour, yellow}}.

{STRING}     : {token, {string, YYtext}}. 

{FORMAT}     : {token, {format, YYtext}}.
{FRACTION}   : {token, {fraction, YYtext}}.

{CONDITION1} : {token, {condition, YYtext}}.
{CONDITION2} : {token, {condition, YYtext}}.
{CONDITION3} : {token, {condition, YYtext}}.

\$           : {token, {dollar, "$"}}.
\-           : {token, {minus, "-"}}.
\+           : {token, {plus,"+"}}.
/            : {token, {forwardslash,"/"}}.
\(           : {token, {open_bra,"("}}.
\)           : {token, {close_ket,")"}}.
:            : {token, {colon,":"}}.
\s           : {token, {space," "}}.


;            : {token, {semicolon,";"}}.

_            : {token, {underscore,"_"}}.
\%           : {token, {percent,"%"}}.

\\           : {token, {esc, esc}}.
@            : {token, {at, "@"}}.

yy           : {token, {year,    two_digit}}.
yyyy         : {token, {year,    four_digit}}.
m            : {token, {mon_min, no_zero}}.
mm           : {token, {mon_min, zero}}.
mmm          : {token, {mon,     abbr}}.
mmmm         : {token, {mon,     full}}.
d            : {token, {day,     no_zero}}.
dd           : {token, {day,     zero}}.
ddd          : {token, {day,     abbr}}.
dddd         : {token, {day,     full}}.
h            : {token, {hour,    no_zero}}.
hh           : {token, {hour,    zero}}.
s            : {token, {sec,     no_zero}}.
ss           : {token, {sec,     zero}}.
AM/PM        : {token, {ampm,    full_caps}}.
am/pm        : {token, {ampm,    full_lowercase}}.
A/P          : {token, {ampm,    abbr_caps}}.
a/p          : {token, {ampm,    abbr_lowercase}}.

.            : {token, {char, YYtext}}.

\n           : {end_token,{'$end', YYline}}.
