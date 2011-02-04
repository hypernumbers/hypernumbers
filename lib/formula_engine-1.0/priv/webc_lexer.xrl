Definitions.

WHITESPACE = ([\000-\s]*)

PATH_COMP  = ([a-zA-Z0-9_\-~]+)

Rules.

{PATH_COMP}  : {token,     {path,  string:to_lower(TokenChars)}}.
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
    Ret = string(String),
    io:format("Returning with ~p~n", [Ret]),
    Ret.

%%% Tests:
-include_lib("eunit/include/eunit.hrl").

%% @doc Lexing functions for testing.
tlex(Input) ->
    case string(Input) of
        {ok, Toks, _} -> io:format("Input ~p lexes to ~p~n", [Input, Toks]),
                         Toks;
        Error               -> io:format("Input ~p fails to parse with ~p~n",
                                         [Input, Error]),
                               {error, lex_error}
    end.

seg_test_() ->
    [
     ?_assert(tlex("/blah/blah, bleh, bloh/bluh/") == [
                                      {slash, "/"},
                                      {path, "blah"},
                                      {slash, "/"},
                                      {path, "blah"},
                                      {comma, ","},
                                      {path, "bleh"},
                                      {comma, ","},
                                      {path, "bloh"},
                                      {slash, "/"},
                                      {path, "bluh"},
                                      {slash, "/"}
                                     ])
    ].
