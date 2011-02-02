%%% -*- mode: erlang -*-
%%% @doc    Lexer for urls.
%%% @author gordon@hypernumbers
Definitions.

INT = ([0-9]+)
WORD = ([a-zA-Z]+)

WHITESPACE = ([\000-\s]*)

PATH_COMP  = ([a-zA-Z0-9_\-~|]+)

A1_REF_REL  = ({WORD})({INT})
A1_REF_FIX  = (\$)({WORD})(\$)({INT})
A1_REF_MIX1 = (\$)({WORD})({INT})
A1_REF_MIX2 = ({WORD})(\$)({INT})

A1_REF = ({A1_REF_REL}|{A1_REF_FIX}|{A1_REF_MIX1}|{A1_REF_MIX2})

FINITE_RANGE_A1 = ({A1_REF})(\:)({A1_REF})

A1_COL_REF_REL  = (({WORD})(\:)({WORD}))
A1_COL_REF_FIX  = ((\$)({WORD})(\:)(\$)({WORD}))
A1_COL_REF_MIX1 = ((\$)({WORD})(\:)({WORD}))
A1_COL_REF_MIX2 = (({WORD})(\:)(\$)({WORD}))

A1_ROW_REF_REL  = (({INT})(\:)({INT}))
A1_ROW_REF_FIX  = ((\$)({INT})(\:)(\$)({INT}))
A1_ROW_REF_MIX1 = ((\$)({INT})(\:)({INT}))
A1_ROW_REF_MIX2 = (({INT})(\:)(\$)({INT}))

%% Helper classes

A1_REF = ({A1_REF_REL}|{A1_REF_FIX}|{A1_REF_MIX1}|{A1_REF_MIX2})

A1_COL_RANGE = ({A1_COL_REF_REL}|{A1_COL_REF_FIX}|{A1_COL_REF_MIX1}|{A1_COL_REF_MIX2})
A1_ROW_RANGE = ({A1_ROW_REF_REL}|{A1_ROW_REF_FIX}|{A1_ROW_REF_MIX1}|{A1_ROW_REF_MIX2})

Rules.

{A1_REF} : {token, to_cellref(TokenChars, TokenLine)}.

{FINITE_RANGE_A1} : {token, finite_range(TokenChars, TokenLine)}.

{A1_COL_RANGE} : {token, col_range(TokenChars, TokenLine)}.
{A1_ROW_RANGE} : {token, row_range(TokenChars, TokenLine)}.

{PATH_COMP}  : {token,     {path,       TokenChars}}.
\[           : {token,     {open,       TokenChars}}.
\]           : {token,     {close,      TokenChars}}.
\/           : {token,     {slash,      TokenChars}}.
\,           : {token,     {comma,      TokenChars}}.
\.           : {token,     {fullstop,   TokenChars}}.
\?           : {token,     {question,   TokenChars}}.
\#           : {token,     {hash,       TokenChars}}.
% now scoop up every other token that is valid in an Excel expression

%% Discard whitespace:
{WHITESPACE} : skip_token.

\n           : {end_token, {'$end', TokenLine}}.

%% Excel can get most things inside expression so hoover 'em up and let
%% the Excel parser take the strain later on
.  : {token, {excel_expr, TokenChars}}.

Erlang code.

-export([
         lex/1
        ]).

lex(String) -> {ok, Toks, 1} = string(String),
               {ok, post_process(Toks, closed, [], [])}.

to_cellref(Chars, _line) -> {cellref, Chars}.

finite_range(Chars, _Line) -> {range, Chars}.

col_range(Chars, _Line) -> {col, Chars}.

row_range(Chars, _Line) -> {row, Chars}.

post_process([], _status, [], Acc2) -> lists:reverse(Acc2);
post_process([{open, _} | T], closed, Acc1, Acc2) ->
    post_process(T, open, Acc1, Acc2);
post_process([{close, _} | T], open, Acc1, Acc2) ->
    NewAcc = {zseg, lists:flatten(lists:reverse(Acc1))},
    post_process(T, closed, [], [NewAcc |  Acc2]);
post_process([{_, H} | T], open, Acc1, Acc2) ->
    post_process(T, open, [H | Acc1], Acc2);
% if we hit a question mark or has when we are open we can ditch the rest of
% the string 'cos it is them ?a=b or #anchor bits at the end
post_process([{Type, _H} | _T], closed, [], Acc2)
  when Type == question orelse Type == has ->
    post_process([], closed, [], Acc2);
post_process([H | T], closed, [], Acc2) ->
    post_process(T, closed, [], [H | Acc2]).

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
     ?_assert(tlex("/blah/blah/") == [
                                      {slash, "/"},
                                      {path, "blah"},
                                      {slash, "/"},
                                      {path, "blah"},
                                      {slash, "/"}
                                     ]),

     ?_assert(tlex("/blah/[blah]/") == [
                                        {slash, "/"},
                                        {path, "blah"},
                                        {slash, "/"},
                                        {open, "["},
                                        {path, "blah"},
                                        {close, "]"},
                                        {slash, "/"}
                                       ]),

     ?_assert(tlex("/blah/[bloh]/bleh/") == [
                                             {slash, "/"},
                                             {path, "blah"},
                                             {slash, "/"},
                                             {open, "["},
                                             {path, "bloh"},
                                             {close, "]"},
                                             {slash, "/"},
                                             {path, "bleh"},
                                             {slash, "/"}
                                            ]),

     ?_assert(tlex("/a1/[blah]/") == [
                                      {slash, "/"},
                                      {cellref, "a1"},
                                      {slash, "/"},
                                      {open, "["},
                                      {path, "blah"},
                                      {close, "]"},
                                      {slash, "/"}
                                     ]),

     ?_assert(tlex("/a1/[blah]/") == [
                                      {slash, "/"},
                                      {cellref, "a1"},
                                      {slash, "/"},
                                      {open, "["},
                                      {path, "blah"},
                                      {close, "]"},
                                      {slash, "/"}
                                     ]),

     ?_assert(tlex("/a1:b4/[blah]/") == [
                                         {slash, "/"},
                                         {range, "a1:b4"},
                                         {slash, "/"},
                                         {open, "["},
                                         {path, "blah"},
                                         {close, "]"},
                                         {slash, "/"}
                                        ]),

     ?_assert(tlex("/1:1/[blah]/") == [
                                       {slash, "/"},
                                       {row, "1:1"},
                                       {slash, "/"},
                                       {open, "["},
                                       {path, "blah"},
                                       {close, "]"},
                                       {slash, "/"}
                                      ]),

     ?_assert(tlex("/a:a/[blah]/") == [
                                       {slash, "/"},
                                       {col, "a:a"},
                                       {slash, "/"},
                                       {open, "["},
                                       {path, "blah"},
                                       {close, "]"},
                                       {slash, "/"}
                                      ]),
     
     ?_assert(tlex("/blah/a1") == [
                                   {slash, "/"},
                                   {path, "blah"},
                                   {slash, "/"},
                                   {cellref, "a1"}
                                  ]),

     ?_assert(tlex("/blah/$a1") == [
                                    {slash, "/"},
                                    {path, "blah"},
                                    {slash, "/"},
                                    {cellref, "$a1"}
                                   ]),

     ?_assert(tlex("/blah/a$1") == [
                                    {slash, "/"},
                                    {path, "blah"},
                                    {slash, "/"},
                                    {cellref, "a$1"}
                                   ]),

     ?_assert(tlex("/blah/a1:b4") == [
                                      {slash, "/"},
                                      {path, "blah"},
                                      {slash, "/"},
                                      {range, "a1:b4"}
                                     ]),

     ?_assert(tlex("/blah/a:c") == [
                                    {slash, "/"},
                                    {path, "blah"},
                                    {slash, "/"},
                                    {col, "a:c"}
                                   ]),

     ?_assert(tlex("/blah/3:5") == [
                                    {slash, "/"},
                                    {path, "blah"},
                                    {slash, "/"},
                                    {row, "3:5"}
                                   ]),

     ?_assert(tlex("/blah/bingo.xls") == [
                                          {slash, "/"},
                                          {path, "blah"},
                                          {slash, "/"},
                                          {path, "bingo"},
                                          {fullstop, "."},
                                          {path, "xls"}
                                   ])

    ].
