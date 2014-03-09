%%% Engineering functions.
%%% <hasan@hypernumbers.com>
%%% @private
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


-module(stdfuns_eng).

-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

% passing test suite
-export([
         gestep/1
        ]).

% no test suite
%-export([
%         delta/1
%         ]).

-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

%% @todo non-Excel 97 function - needs a test suite
delta([V1, V2]) ->
    [Num1, Num2] = muin_col_DEPR:collect_number([V1, V2], ?default_rules),
    case (Num1 == Num2) of
        true  -> 1;
        false -> 0
    end.

gestep([V1]) ->
    gestep([V1, 0]);
gestep([V1, V2]) ->
    % gestep is wierd - it evaluates back to front for the purpose
    % of throwing errors so reverse the arguements in the cast
    [Step, Num] = muin_col_DEPR:collect_numbers([V2, V1],
                                                [cast_strings, ban_bools,
                                                 cast_blanks, cast_dates]),
    case (Num >= Step) of
        true  -> 1;
        false -> 0
    end.

