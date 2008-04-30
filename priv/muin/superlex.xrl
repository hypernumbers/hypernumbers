%%% Part of the super parser, which lives under lib/formula-engine/
%%% <hasan@hypernumbers.com>

Definitions.

INTEGER = ([0-9]+)
FLOAT_DECIMAL = ([0-9]+\.[0-9]*)
FLOAT_SCIENTIFIC = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)
BOOLEAN = {TRUE}|{FALSE}
ERROR = \#NULL\!|\#DIV\/0\!|\#VALUE\!|\#REF\!|\#NAME\?|\#NUM\!|\#N\/A\!
STRING = (\"[^"\n]*\")
%" % Syntax highlighting fix.
WHITESPACE = ([\000-\s]*)

Rules.

{STRING} : {token, {string, YYtext}}.
{WHITESPACE} : .
\n : {end_token, {'$end'}}.
.  : {token, {stuff, YYtext}}.

Erlang code.

-export([lex/1]).

lex(Str) ->
    string(Str).
