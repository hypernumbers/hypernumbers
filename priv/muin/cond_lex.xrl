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

{EQ}               : {token, {equals,      "="}}.
{NE}               : {token, {notequals,   "<>"}}.
{GT}               : {token, {greaterthan, ">"}}.
{LT}               : {token, {lessthan,    "<"}}.
{GE}               : {token, {gtorequal,   ">="}}.
{LE}               : {token, {ltorequal,   "=<"}}.

{STRING}           : {token, {string,    YYtext}}.
{INTEGER}          : {token, {integer,   YYtext}}.
{FLOAT_DECIMAL}    : {token, {float,     YYtext}}.
{FLOAT_SCIENTIFIC} : {token, {float,     YYtext}}.
{BOOLEAN}          : {token, {string,    YYtext}}.

\n                 : {end_token,{'$end', YYline}}.
