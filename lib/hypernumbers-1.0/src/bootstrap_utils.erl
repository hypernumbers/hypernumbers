%%% @author    Gordon Guthrie
%%% @copyright (C) 2012-2014, Hypernumbers Ltd
%%% @doc       Utilities for bootstrap fns
%%%
%%% @end
%%% Created : 22 Oct 2012 by gordon@vixo.com

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

-module(bootstrap_utils).

-export([
         get_colour/1
        ]).

-include("errvals.hrl").

get_colour(0) -> "";
get_colour(1) -> "btn-primary";
get_colour(2) -> "btn-warning";
get_colour(3) -> "btn-danger";
get_colour(4) -> "btn-success";
get_colour(5) -> "btn-info";
get_colour(6) -> "btn-inverse";
get_colour(_) -> ?ERR_VAL.

