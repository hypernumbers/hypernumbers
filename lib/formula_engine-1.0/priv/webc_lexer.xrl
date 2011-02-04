Definitions.

WHITESPACE = ([\000-\s]*)

PATH_COMP  = ([a-zA-Z0-9_\-~]+)

Rules.

{PATH_COMP}  : {token,     {wcpath,  string:to_lower(TokenChars)}}.
\[           : {token,     {open,    TokenChars}}.
\]           : {token,     {close,   TokenChars}}.
\/           : {token,     {slash,   TokenChars}}.
\,           : {token,     {comma,   TokenChars}}.

%% Discard whitespace:
{WHITESPACE} : skip_token.

\n           : {end_token, {'$end', TokenLine}}.

%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, TokenLine, TokenChars}}.

Erlang code.

-export([
         lex/1
         ]).

lex(String) -> string(String).

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
                                      {wcpath, "blah"},
                                      {slash, "/"},
                                      {wcpath, "blah"},
                                      {comma, ","},
                                      {wcpath, "bleh"},
                                      {comma, ","},
                                      {wcpath, "bloh"},
                                      {slash, "/"},
                                      {wcpath, "bluh"},
                                      {slash, "/"}
                                     ])
    ].
