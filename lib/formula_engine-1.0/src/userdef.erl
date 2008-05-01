-module(userdef).
-export([inc/1]).

inc([Num]) ->
    Num + 1.

bla([]) ->
    "blablabla".
