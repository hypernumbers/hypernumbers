-module(dragndrop_actions).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").

run() ->
    Url  = "http://127.0.0.1:9000/dragndrop/",
    Type = "application/json",
    Data1 = "{\"drag\":{\"range\":\"E4:E7\"}}",
    Data2 = "{\"drag\":{\"range\":\"F4:F7\"}}",
    Data3 = "{\"drag\":{\"range\":\"G4:G7\"}}",
    http:request(post,{Url++"E3?attr", [], Type, Data1}, [], []),
    http:request(post,{Url++"F3?attr", [], Type, Data2}, [], []),
    http:request(post,{Url++"G3?attr", [], Type, Data3}, [], []),
    ok.

