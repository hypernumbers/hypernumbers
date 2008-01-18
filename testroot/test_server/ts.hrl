%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id: ts.hrl 656 2006-11-13 22:23:01Z daleharvey $
%%
%% Defines ripped out from test_server (these must remain the same
%% as in test_server).

-define(logdir_ext, ".logs").
-define(suitelog_name, "suite.log").
-define(last_file, "last_name").
-define(last_link, "last_link").
-define(last_test, "last_test").
-define(run_summary, "suite.summary").
-define(cover_total,"total_cover.log").
-define(variables, "variables").
-define(LF, [10]).                              % Newline in VxWorks script
-define(CHAR_PER_LINE, 60).                     % Characters per VxWorks script building line
-define(CROSS_COOKIE, "cross").                 % cookie used when cross platform testing
-define(TS_PORT, 7887).
-define(TEST_SERVER_SCRIPT, "test_server_vx.script").

