%%% @author     Gordon Guthrie
%%% @copyright (C) 2011-2014, Gordon Guthrie
%%% @doc       Macros to manipulate the process dictionary in muin
%%%            May the Lord have mercy on my soul!
%%% @end
%%% Created : 29 Mar 2011 by Gordon Guthrie <>

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

-define(mx,            muin:pd_retrieve(x)).
-define(my,            muin:pd_retrieve(y)).
-define(mpath,         muin:pd_retrieve(path)).
-define(msite,         muin:pd_retrieve(site)).
-define(mar,           muin:pd_retrieve(auth_req)).
-define(array_context, muin:pd_retrieve(array_context)).

-define(error_in_formula, {errval, '#FORMULA!'}).
-define(syntax_err,       {error, syntax_error}).

-record(muin_context, {
          idx,
          site           = []    :: list(),
          path           = []    :: list(),
          row                    :: integer(),
          col                    :: integer(),
          x                      :: integer(),
          y                      :: integer(),
          auth_req       = []    :: list(),
          array_context  = []    :: list(),
          finite_refs    = []    :: list(),
          range_refs     = []    :: list(),
          infinite_refs  = []    :: list(),
          errors         = []    :: list(),
          recompile      = false :: boolean(),
          circref        = false :: boolean(),
          selfreference  = false :: boolean(),
          is_zcalc       = false :: boolean()
         }).

-record(old_muin_context, {
          path :: list(),
          row  :: integer(),
          col  :: integer()
         }).
