-module(delete_rows_actions).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").

run() ->
    Url  = "http://127.0.0.1:9000/delete_rows/",
    Type = "application/json",
    Accept = [{"Accept", "application/json"}],
    Data = "{\"delete\":\"all\"}",
    http:request(post,{Url++"11:11?attr", Accept, Type, Data}, [], []),
    http:request(post,{Url++"10:11?attr", Accept, Type, Data}, [], []),
    ok.
