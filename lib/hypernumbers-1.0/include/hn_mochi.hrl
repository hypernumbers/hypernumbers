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
%% HTTP Environment
-record(env, { accept,
               body :: multipart | list(),
               raw_body :: {upload, string()} | binary(),
               headers = [] :: list(), %% These are 'response' headers.
               method,
               mochi,
               uid :: string(),
               email = "anonymous" :: string(),
               auth = null
             }).

% useful macros

-define(SHEETVIEW,   "spreadsheet").
-define(WEBPAGE,     "webpage").
-define(WIKI,        "wikipage").
-define(LOGVIEW,     "logs").
-define(DEBUG,       "debug").
-define(RECALC,      "recalc").
-define(PHONE,       "phone").
-define(RECORDING,   "recording").

-define(NO_STAMP, undefined).
