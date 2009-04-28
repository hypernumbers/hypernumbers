-module(delete_columns_actions).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").

run() ->
    Url  = "http://127.0.0.1:9000/delete_columns/",
    Type = "application/json",
    Data = "{\"delete\":\"all\"}",
    http:request(post,{Url++"E:E?attr", [], Type, Data}, [], []),
    http:request(post,{Url++"F:H?attr", [], Type, Data}, [], []),
    ok.

