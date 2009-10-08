-module(copynpaste_actions).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").

run() ->
    Url  = "http://127.0.0.1:9000/copynpaste/",
    Type = "application/json",
    Data = "{\"copy\":{\"src\":\"http://127.0.0.1:9000/copynpaste/F4:F9\"}}",
    http:request(post,{Url++"G5:G5", [{"Accept", "application/json"}],
                       Type, Data}, [], []),
    ok.
