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

-module(zload).

-define(ZSITE, "http://zload.hypernumbers.dev:9000").

-export([
        load/0
       ]).

load() ->
    case hn_setup:site_exists(?ZSITE) of
        false -> ok;
        true  -> hn_setup:delete_site(?ZSITE)
    end,
    {initial_view, []} = hn_setup:site(?ZSITE, blank, []),
    io:format("Created blank site - about to load data~n"),
    % now load up zload pages into blank
    Dir = "/home/gordon/hypernumbers/lib/hypernumbers-1.0/priv/"
        ++ "site_types/z_load",
    io:format("setting up site ~p~n", [erlang:time()]),
    ok = hn_setup:import_json_DEBUG("http://zload.hypernumbers.dev:9000", Dir),
    check().

check() ->
    case dbsrv:is_busy("http://zload.hypernumbers.dev:9000") of
        true ->
            timer:sleep(5000),
            check();
        false ->
            io:format("Finished ~p~n", [erlang:time()])
    end.
