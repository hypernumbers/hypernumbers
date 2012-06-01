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
    io:format("setting up site~n"),
    ok = hn_setup:import_json_DEBUG("http://zload.hypernumbers.dev:9000", Dir),
    io:format("finished...~n").
