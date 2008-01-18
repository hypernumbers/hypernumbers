%%%-----------------------------------------------------------------------------
%%% This is the lexer description file.
%%%
%%% Part of Lisp front-end for Muin.
%%%
%%% Author: Hasan Veldstra <hasan@hypernumbers.com>
%%%-----------------------------------------------------------------------------

%%% Nothing new here. Just copy-pasted and edited from the main lexer file.

Definitions.

INTEGER   = ([0-9]+)
FLOAT     = ([0-9]+\.[0-9]*)
FLOAT_SCI = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)
%%% NOTE: Variants in  mixed-case are handled in the rule for ID that follows.
BOOLEAN   = true|false|TRUE|FALSE
% This handles both single and double quoted strings.
STRING    = (\'[^'\n]*\')|(\"[^"\n]*\") 
%%%' % syntax highlighting fix for Emacs & TextMate.

%%% Single cell reference, e.g. A1, B22 etc.
CELLREF   = ((\$)?[a-zA-Z]+(\$)?[0-9]+)

ID        = [a-zA-Z][a-zA-Z0-9_]*

WHITESPACE = ([\000-\s]*)

Rules.

{INTEGER}       : {token, {integer,   YYtext}}.
{FLOAT}         : {token, {float,     YYtext}}.
{FLOAT_SCI}     : {token, {float_sci, YYtext}}.
{BOOLEAN}       : {token, {boolean,   YYtext}}.
{STRING}        : {token, {string,    YYtext}}.
{CELLREF}       : {token, {cellref,   YYtext}}.
{ID}            : 
  %%% Leex doesn't have an option to generate a case-insensitive lexer, so we
  %%% need to resort to doing rather ugly things like this. (Of course, spelling
  %%% out all alternatives of true & false in the definition of BOOLEAN is an
  %%% option -- but that's even uglier.)
  case string:to_lower(YYtext) of
      "true" ->
	  {token, {boolean, "true"}};
      "false" ->
	  {token, {boolean, "false"}};
      _ ->
	  {token, {id,      YYtext}}
  end.


% Ignore whitespace.
{WHITESPACE} : .

=  : {token, {eq}}.
\( : {token, {open_paren}}.
\) : {token, {close_paren}}.

%% Operators.
% Arithmetic.
\+ : {token, {plus}}.
\- : {token, {minus}}.
\* : {token, {times}}.
\/ : {token, {divide}}.
\^ : {token, {power}}.

% Logical.
>  : {token, {gt}}.
<  : {token, {lt}}.
>= : {token, {gte}}.
<= : {token, {lte}}.
<> : {token, {neq}}.

%\: : {token, {colon, YYline}}.

\n : {end_token, {'$end', YYline}}.
.  : {token, {invalid_token, YYline, YYtext}}.
