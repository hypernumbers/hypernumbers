Definitions.

BLACK      = (\[BLACK\])
BLUE       = (\[BLUE\])
CYAN       = (\[CYAN\])
GREEN      = (\[GREEN\])
MAGENTA    = (\[MAGENTA\])
RED        = (\[RED\])
WHITE      = (\[WHITE\])
YELLOW     = (\[YELLOW\])

INTEGER   = ([0-9]+)
FLOAT     = ([0-9]+\.[0-9]*)
FLOAT_SCI = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)


STRING     = (\".*\")

Rules.


{BLACK}      : {token, {colour,black}}.
{BLUE}       : {token, {colour,blue}}.
{CYAN}       : {token, {colour,cyan}}.
{GREEN}      : {token, {colour,green}}.
{MAGENTA}    : {token, {colour,magenta}}.
{RED}        : {token, {colour,red}}.
{WHITE}      : {token, {colour,white}}.
{YELLOW}     : {token, {colour,yellow}}.

{INTEGER}    : {token, {number,YYtext}}.
{FLOAT}      : {token, {number,YYtext}}.
{FLOAT_SCI}  : {token, {number,YYtext}}.

{STRING}     : {token, {string,YYtext}}. 

\[           : {token, open_sq_brackets}.
\]           : {token, close_sq_brackets}.

>            : {token, gt}.
<            : {token, lt}.
=            : {token, eq}.

\s           : {token, space}.
0            : {token, zero}.
#            : {token, hash}.
\?           : {token, question}.
\.           : {token, dot}.
\%           : {token, percent}.
,            : {token, comma}.
e            : {token, exponent}.
E            : {token, exponent}.
\-           : {token, minus}.
\+           : {token, plus}.
_            : {token, underscore}.
\$           : {token, dollar}.
/            : {token, forwardslash}.
\(           : {token, open_bra}.
\)           : {token, close_ket}.
:            : {token, colon}.
\\           : {token, backslash}.
@            : {token, at}.

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

.            : {token, duff}.

\n           : {end_token,{'$end', YYline}}.
