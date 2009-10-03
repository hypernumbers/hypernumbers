%%% @doc Lexer that recognizes references only. Used for structural updates.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

Definitions.

%%% Same-page references.
CELLREF  = ((\$)?[a-zA-Z]+(\$)?[0-9]+)
RANGEREF = ({CELLREF}:{CELLREF})

WHITESPACE = ([\000-\s]*)


Rules.

{CELLREF}  : {token, {cellref,  string:to_lower(YYtext)}}.
{RANGEREF} : {token, {rangeref, string:to_lower(YYtext)}}.

%% Preserve whitespace for reverse lexing.
{WHITESPACE} : {token, {whitespace, YYtext}}.

\n : {end_token, {'$end'}}.

%% Anything else -- we don't care about.
.  : {token, {stuff, YYtext}}.
