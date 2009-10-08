-module(delete_actions).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").

run() ->
    Url  = "http://127.0.0.1:9000/delete/",
    Type = "application/json",
    Data = "{\"delete\":\"all\"}",
    http:request(post,{Url, [{"Accept", "application/json"}], Type, Data}, [], []),
    ok.
