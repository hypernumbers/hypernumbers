%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc    Gui_Generator application.

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

