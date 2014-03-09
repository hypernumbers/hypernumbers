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
%% Anything to string
-define(STR(X),lists:flatten(io_lib:format("~p",[X]))).
%% Formatted String
-define(FORMAT(X,Y),lists:flatten(io_lib:format(X,Y))).
%% Generate SASL info message
-define(INFO(X,Y),error_logger:info_msg(X++"~n",Y)).
-define(ERROR(X,Y), error_logger:error_msg(X++"~n",Y)).
-define(DEV_ZONE, "hypernumbers.dev").
