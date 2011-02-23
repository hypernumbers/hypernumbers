%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Loads up a test server
%%%
%%% @end
%%% Created : 21 Feb 2011 by Gordon Guthrie <>

-module(load).

-export([
         test_dbsrv/1,
         load/2,
         dump/0,
         start_trace/1,
         stop_trace/1
        ]).

-define(daysinyear, 20).

test_dbsrv(Site) ->
    Server = list_to_atom(hn_util:site_to_fs(Site) ++ "_dbsrv"),
    io:format("Start tracing...~n"),
    FileName = start_trace([Server]),
    URL1 = Site ++ "/test/calculations/",
    URL2 = Site ++ "/test/calculations/data/",
    RefX1 = hn_util:url_to_refX(URL1),
    RefX2 = hn_util:url_to_refX(URL2),
    Root = code:lib_dir(hypernumbers),
    Dir = Root ++ "/../../priv/load_testing/json/",
    io:format("Adding new data...~n"),
    hn_import:json_file(URL1, Dir ++ "calculations.json"),
    hn_import:json_file(URL2, Dir ++ "data.json"),
    io:format("about to stop tracing and begin analysis (might take some time!)~n"),
    stop_trace(FileName),
    io:format("Now clean up...~n"),
    ok = hn_db_api:delete(RefX1, nil),
    ok = hn_db_api:delete(RefX2, nil).

start_trace(Supervisors) when is_list(Supervisors) ->
    Sups2 = [whereis(X) || X <- Supervisors],
    FileName = "fprof.trace." ++ tconv:to_s(util2:get_timestamp()),
    fprof:trace([start, {procs, Sups2}, {file, FileName}]),
    FileName.

stop_trace(FileName) ->
    fprof:trace([stop]),
    fprof:profile([{file, FileName}]),
    "fprof.trace."++Stamp = FileName,
    fprof:analyse([{dest, "fprof.analyze."++Stamp}]).

dump() ->
    Registered = lists:sort([atom_to_list(X) || X <- erlang:registered()]),
    io:format("Registered is ~p~n", [Registered]),
    Sites = lists:sort([hn_util:site_to_fs(X) || X <- get_sites()]),
    io:format("Sites is ~p~n", [Sites]),
    ok.

load(Site, Size) when is_integer(Size) ->
    if
        Size < 0      -> exit("Need a postive size");
        Size > 20     -> exit("You're 'avin a laugh, int'ya...");
        0 < Size andalso Size < 21 -> l2(Site, Size)
    end.

l2(_Site, 0) -> ok;
l2(Site, N)  -> ok = l3(Site, N, ?daysinyear),
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
    N1 = util2:get_timestamp(),
    hn_import:json_file(URL1, Dir ++ "calculations.json"),
    N2 = util2:get_timestamp(),
    hn_import:json_file(URL2, Dir ++ "data.json"),
    N3 = util2:get_timestamp(),
    log(io_lib:format("~p\t~p\t~p\t~p\t~p", [N, M, N1, N2, N3])),
    l3(Site, N, M - 1).

log(String) ->
    log(String, "../logs/load_logs.txt").

log(String, File) ->
    _Return=filelib:ensure_dir(File),
    
    case file:open(File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [String]),
	    file:close(Id);
	_ ->
	    error
    end.

get_sites() -> 
    mnesia:activity(transaction, fun mnesia:all_keys/1, [core_site]).
