Definitions.

BLACK      = (\[(B|b)(L|l)(A|a)(C|c)(K|k)\])
BLUE       = (\[(B|b)(L|l)(U|u)(E|e)\])
CYAN       = (\[(C|c)(Y|y)(A|a)(N|n)\])
GREEN      = (\[(G|c)(R|r)(E|e)(E|e)(N|n)\])
MAGENTA    = (\[(M|m)(A|a)(G|g)(E|e)(N|n)(T|t)(A|a)\])
RED        = (\[(R|r)(E|e)(D|d)\])
WHITE      = (\[(W|w)(H|h)(I|i)(T|t)(E|e)\])
YELLOW     = (\[(Y|y)(E|e)(L|l)(L|l)(O|o)(W|w)\])

CONDITION1 = (\[([[><=+-]|[(=>)|(>=)|(=<)|(<=)])([0-9]+)\])
CONDITION2 = (\[([[><=+-]|[(=>)|(>=)|(=<)|(<=)])([0-9]+\.[0-9]*)\])
CONDITION3 = (\[([[><=+-]|[(=>)|(>=)|(=<)|(<=)])([0-9]++\.[0-9]+((E|e))(\+|\-)?[0-9]+)\])

STRING     = (\".*\")


INTEGER   = ([0-9]+)
FLOAT     = ([0-9]+\.[0-9]*)
FLOAT_SCI = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)

Rules.


{BLACK}      : {token, {colour,black}}.
{BLUE}       : {token, {colour,blue}}.
{CYAN}       : {token, {colour,cyan}}.
{GREEN}      : {token, {colour,green}}.
{MAGENTA}    : {token, {colour,magenta}}.
{RED}        : {token, {colour,red}}.
{WHITE}      : {token, {colour,white}}.
{YELLOW}     : {token, {colour,yellow}}.

{STRING}     : {token, {string,YYtext}}. 

{INTEGER}    : {token, {number, YYtext}}.
{FLOAT}      : {token, {number, YYtext}}.
{FLOAT_SCI}  : {token, {number, YYtext}}.

{CONDITION1} : {token, {condition, YYtext}}.
{CONDITION2} : {token, {condition, YYtext}}.
{CONDITION3} : {token, {condition, YYtext}}.

\[           : {token, {open_sq_bk}}.
\]           : {token, {close_sq_bk}}.

>            : {token, {gt}}.
<            : {token, {lt}}.
=            : {token, {eq}}.

\s           : {token, {space}}.
#            : {token, {hash}}.
\?           : {token, {question}}.
\.           : {token, {dot}}.
\%           : {token, {percent}}.
,            : {token, {comma}}.
e            : {token, {exponent}}.
E            : {token, {exponent}}.
\-           : {token, {minus}}.
\+           : {token, {plus}}.
_            : {token, {underscore}}.
\$           : {token, {dollar}}.
/            : {token, {forwardslash}}.
\(           : {token, {open_bra}}.
\)           : {token, {close_ket}}.
:            : {token, {colon}}.
\\           : {token, {esc}}.
@            : {token, {at}}.

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
