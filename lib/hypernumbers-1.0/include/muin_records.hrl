%%% @doc Various records used in Muin.
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

%% Runtime information: describes the context in which to run a formula.
-record(muin_rti, {
          site,
          path,
          col,
          row,
          idx,                   % target cell
          auth_req      = nil,
          array_context = false, % array formula?
          finite_refs   = [],
          range_refs    = []    :: list(),
          infinite_refs = []    :: list(),
          errors        = []    :: list(),
          is_zcalc      = false :: boolean()
         }).

%% DateTime: fields are in the format expected by functions in the
%% calendar module in stdlib.
-record(datetime, {
          date = {1, 1, 1},   % year, month, day
          time = {0, 0, 0}    % hour, minute, second
        }).

%% col & row are either integers, or {offset, integer()}
-record(cellref, {
          col, row,       % pos_integer()|{offset, pos_integer()}
          path,           % string like "./" or "/foo/bar"
          text = ""       % token text (used in the parser for
                          % function names like ATAN2)
         }).

-record(zcellref, {
          zpath,                % a z-order path
          cellref = #cellref{}  % a standard #cellref{} record
         }).

%% super_util actually depends on rangeref order!!!
-record(rangeref, {
          type,          % finite|col|row
          path,          % like in #cellref{}
          tl, br,        % {Col, Row} | {col|row, pos_integer()
                         % | {offset, pos_integer()}}
          width, height, % pos_integer()|na
          text = ""      % token text
         }).

-record(zrangeref, {
          zpath,                 % a z-order path
          rangeref = #rangeref{} % a standard #rangeref{} record
         }).

-record(namedexpr, {
          path,
          text
         }).
