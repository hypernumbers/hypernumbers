%% Mostly a copy-paste job from xfl_lexer. See that for comments on lexeme definitions.
%% <hasan@hypernumbers.com>
%%% @copyright (C) 2009-2014, Hypernumbers Ltd.

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

Definitions.

INT = ([0-9]+)
ATOM = ([a-zA-Z0-9_\.-]*)
NAME = \@{ATOM}

START_OF_SSREF = ((\/|\!)|(\.\.(\/|\!))+|\.(\/|\!))
MAYBE_PATH = (((({ATOM})|({INT})|(\.)|(\.\.))(\/|\!))*)

A1REF = ((\$)?([a-zA-Z]+)(\$)?([0-9]+))
COLREF = ((\$)?([a-zA-Z]+))
ROWREF = ((\$)?([0-9]+))

OFFSET_RC = (\[(\+|\-)?({INT})\])
RCREF = ((R|r)({INT}|{OFFSET_RC})(C|c)({INT}|{OFFSET_RC}))

Z_EXPR = ((\[)([^\]]+)(\])(\/|\!)*)
MAYBE_Z_PATH = ({START_OF_SSREF})*(({MAYBE_PATH})*({Z_EXPR})+({MAYBE_PATH})*)+

SSA1REF   = {START_OF_SSREF}{MAYBE_PATH}{A1REF}
SSRCREF   = {START_OF_SSREF}{MAYBE_PATH}{RCREF}
SSCOLREF  = {START_OF_SSREF}{MAYBE_PATH}{COLREF}
SSROWREF  = {START_OF_SSREF}{MAYBE_PATH}{ROWREF}
SSNAMEREF = {START_OF_SSREF}{MAYBE_PATH}{NAME}

ZREF = {MAYBE_Z_PATH}{A1REF}

STRING = (\"[^"\n]*\")
%"%
SPACE = ([\s]+)
WHITESPACE = ([\t]+|[\n]+)

Rules.

%% string tag means "leave as-is". TODO: rename.
{SSA1REF}    : {token, {string, xfl_lexer:debang(TokenChars)}}.
{SSRCREF}    : {token, {string, xfl_lexer:debang(TokenChars)}}.
{SSCOLREF}   : {token, {string, xfl_lexer:debang(TokenChars)}}.
{SSROWREF}   : {token, {string, xfl_lexer:debang(TokenChars)}}.
{SSNAMEREF}  : {token, {string, xfl_lexer:debang(TokenChars)}}.
{STRING}     : {token, {string, TokenChars}}.
{SPACE}      : {token, {string, TokenChars}}.
{ZREF}       : {token, {string, xfl_lexer:debang(TokenChars)}}.
{WHITESPACE} : skip_token.
\n           : skip_token.
.            : {token, {stuff, TokenChars}}.


Erlang code.
