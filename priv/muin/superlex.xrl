%%% Part of the super parser, which lives under lib/formula-engine/
%%% <hasan@hypernumbers.com>

Definitions.

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
