%%% @author     Gordon Guthrie
%%% @copyright (C) 2011-2014, Hypernumbers Ltd
%%% @doc       some utilities for hnfuns
%%%
%%% @end
%%% Created : 22 Aug 2011 by <gordon@hypernumbers.com>

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

-module(funs_util).

-export([
         check_size/2,
         check_size2/3
         ]).

-include("typechecks.hrl").

check_size(W, H) when W > 0 andalso W < 16 andalso H > 1 andalso H < 26 -> ok;
check_size(_W, _H) -> ?ERR_VAL.

check_size2(W, H, MinH)
  when W > 1 andalso W < 16
andalso H >= MinH andalso H < 26 -> ok;
check_size2(_W, _H, _MinH) -> ?ERR_VAL.

