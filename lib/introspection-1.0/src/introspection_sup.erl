%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc    Introspection application.

-module(introspection_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Child = {introspection, % child spec ID -- used only internall
             {introspection_srv, start_link, []},
             permanent, % always restart
             10000,     % time to allow for termination
             worker,
             [introspection_srv]},
    %% terminates if there are more than 1 restarts in 10 seconds
    {ok, {{one_for_all, 1, 10}, [Child]}}.

