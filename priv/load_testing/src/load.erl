%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Loads up a test server
%%%
%%% @end
%%% Created : 21 Feb 2011 by Gordon Guthrie <>

-module(load).

-export([load/2]).

load(Site, Size) when is_integer(Size) ->
    if
        Size < 0      -> exit("Need a postive size");
        Size > 20     -> exit("You're 'avin a laugh, int'ya...");
        0 < Size andalso Size < 21 -> l2(Site, Size)
    end.

l2(_Site, 0) -> ok;
l2(Site, N)  -> ok = l3(Site, N, 365),
               l2(Site, N - 1).

l3(_Site, _N, 0) -> ok;
l3(Site, N, M)   ->
    io:format("Loading day ~p for building ~p~n", [M, N]),
    P1 = [integer_to_list(N), integer_to_list(M), "calculations"],
    P2 = [integer_to_list(N), integer_to_list(M), "calculations", "data"],
    URL1 = Site ++ hn_util:list_to_path(P1),
    URL2 = Site ++ hn_util:list_to_path(P2),
    Root = code:lib_dir(hypernumbers),
    Dir = Root ++ "/../../priv/load_testing/json/",
    hn_import:json_file(URL1, Dir ++ "calculations.json"),
    hn_import:json_file(URL2, Dir ++ "data.json"),
    l3(Site, N, M - 1).
