%%% @doc Lexer that recognizes references only. Used for structural updates.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
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

%%% Same-page references.
CELLREF  = ((\$)?[a-zA-Z]+(\$)?[0-9]+)
RANGEREF = ({CELLREF}:{CELLREF})

WHITESPACE = ([\000-\s]*)


Rules.

{CELLREF}  : {token, {cellref,  TokenLine, string:to_lower(TokenChars)}}.
{RANGEREF} : {token, {rangeref, TokenLine, string:to_lower(TokenChars)}}.

%% Preserve whitespace for reverse lexing.
{WHITESPACE} : {token, {whitespace, TokenLine, TokenChars}}.

\n : {end_token, {'$end'}}.

%% Anything else -- we don't care about.
.  : {token, {stuff, TokenLine, TokenChars}}.
