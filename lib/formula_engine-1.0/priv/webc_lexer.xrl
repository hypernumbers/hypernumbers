Definitions.

WHITESPACE = ([\000-\s]*)

PATH_COMP  = ([a-zA-Z0-9_\-~]+)

% two spaces in html give you a webfart
WEBFART = (Â )

Rules.

{PATH_COMP}  : {token,     {plainpath,  string:to_lower(TokenChars)}}.
\.           : {token,     {fullstop,   TokenChars}}.
\[           : {token,     {open,       TokenChars}}.
\]           : {token,     {close,      TokenChars}}.
\/           : {token,     {slash,      TokenChars}}.
\$           : {token,     {dollar,     TokenChars}}.
\;           : {token,     {semicolon,  TokenChars}}.
\,           : {token,     {comma,      TokenChars}}.

%% Discard whitespace and webfarts:
{WHITESPACE} : skip_token.
{WEBFART}    : skip_token.

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
        {ok, Toks, _} -> io:format("Input ~p~n- It lexes to ~p~n",
                                   [Input, Toks]),
                         Toks;
        Error               -> io:format("Input ~p fails to parse with ~p~n",
                                         [Input, Error]),
                               {error, lex_error}
    end.

seg_test_() ->
    [
     ?_assert(tlex("/blah/blah, bleh, bloh/bluh/")
              == [
                  {slash, "/"},
                  {plainpath, "blah"},
                  {slash, "/"},
                  {plainpath, "blah"},
                  {comma, ","},
                  {plainpath, "bleh"},
                  {comma, ","},
                  {plainpath, "bloh"},
                  {slash, "/"},
                  {plainpath, "bluh"},
                  {slash, "/"}
                 ]),

     ?_assert(tlex("/blah/blah, bleh; bloh/bluh/")
              == [
                  {slash, "/"},
                  {plainpath, "blah"},
                  {slash, "/"},
                  {plainpath, "blah"},
                  {comma, ","},
                  {plainpath, "bleh"},
                  {semicolon, ";"},
                  {plainpath, "bloh"},
                  {slash, "/"},
                  {plainpath, "bluh"},
                  {slash, "/"}
                 ])

    ].

prod_bug_test_() ->
    [
         ?_assert(tlex("./[loop_1inst, loop_1inst;Â  wikipage, "
                       ++ "team01; webpage, teacher01, team01]/")
                  == [
                      {fullstop,"."},
                      {slash,"/"},
                      {open,"["},
                      {plainpath,"loop_1inst"},
                      {comma,","},
                      {plainpath,"loop_1inst"},
                      {semicolon,";"},
                      {plainpath,"wikipage"},
                      {comma,","},
                      {plainpath,"team01"},
                      {semicolon,";"},
                      {plainpath,"webpage"},
                      {comma,","},
                      {plainpath,"teacher01"},
                      {comma,","},
                      {plainpath,"team01"},
                      {close,"]"},
                      {slash,"/"}
                     ])
    ].
