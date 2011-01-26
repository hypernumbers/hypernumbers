Definitions.

WHITESPACE = ([\000-\s]*)

PATH_COMP  = ([a-zA-Z0-9_\-~]+)

Rules.

{PATH_COMP}  : {token,     {path,  TokenChars}}.
\[           : {token,     {open,  TokenChars}}.
\]           : {token,     {close, TokenChars}}.
\/           : {token,     {slash, TokenChars}}.
\,           : {token,     {comma, TokenChars}}.

%% Discard whitespace:
{WHITESPACE} : skip_token.

\n           : {end_token, {'$end', TokenLine}}.

%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, TokenLine, TokenChars}}.

Erlang code.

-export([
         lex/1
         ]).

lex(String) ->
    NewString = lists:flatten(io_lib:write(String++"~n")),
    io:format("String to be lexed is ~p~n", [NewString]),
    Ret = string(String),
    io:format("Returning with ~p~n", [Ret]),
    Ret.
