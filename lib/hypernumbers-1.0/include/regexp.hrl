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
-define(RG_cell,      "^\\$?[a-zA-Z]+\\$?[0-9]+$").
-define(RG_range,     "^\\$?[a-zA-Z]+\\$?[0-9]+:\\$?[a-zA-Z]+\\$?[0-9]+$").
-define(RG_col_range, "^\\$?[a-zA-Z]+:\\$?[a-zA-Z]+$").
-define(RG_row_range, "^\\$?[0-9]+:\\$?[0-9]+$").
-define(RG_num,       "(^-?[0-9]+\\.[0-9]*$)|(^[0-9]*$)").
