%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc    Gui_Generator application.
%%% @copyright (C) 2011 - 2014, Hypernumbers.com

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

-module(gui_generator_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Child = {gui_generator, % child spec ID -- used only internally
             {gui_generator_srv, start_link, []},
             permanent, % always restart
             10000,     % time to allow for termination
             worker,
             [gui_generator_srv]},
    %% terminates if there are more than 1 restarts in 10 seconds
    {ok, {{one_for_all, 1, 10}, [Child]}}.

