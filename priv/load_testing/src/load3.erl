-module(load3).

-export([
         load/0
        ]).

-record(refX,
        {
          site        = [],
          type,
          path        = [],
          obj         = null
         }).

-define(size, 10).
-define(site, "http://hypernumbers.dev:9000").

load() -> Templates = ["calcs", "data"],
          copy(Templates),
          l2(?size).

l2(0) -> ok;
l2(N) -> Seg1 = "calcs" ++ integer_to_list(N),
         Seg2 = "data",
         RefX1 = #refX{site = ?site, path = [Seg1], obj = {page, "/"}},
         RefX2 = #refX{site = ?site, path = [Seg1, Seg2], obj = {page, "/"}},
         io:format("loading ~p~n", [RefX1]),
         ok = hn_templates:load_template(RefX1, "calcs"),
         io:format("loading ~p~n", [RefX2]),
         ok = hn_templates:load_template(RefX2, "data"),
         syslib:limiter(?site),
         l2(N - 1).

copy([])      -> ok;
copy([H | T]) ->
    Site = "hypernumbers.dev\\&9000",
    Root = code:lib_dir(hypernumbers) ++ "/../../",
    Cmd = "cp " ++ [$"] ++ Root ++ "priv/load_testing/priv/" ++ H ++ ".json" ++
                [$"] ++ " " ++ Root ++ "var/sites/" ++ Site ++ "/templates/",
    _Ret = os:cmd(Cmd),
    copy(T).
