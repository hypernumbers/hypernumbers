%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Loads up a test server
%%%
%%% @end
%%% Created : 21 Feb 2011 by Gordon Guthrie <>

-module(load).

-export([
         test_starling/0,
         test_dbsrv/1,
         test_clear/1,
         load/2,
         dump/0,
         start_trace/1,
         stop_trace/1,
         write_local_obj/0
        ]).

-define(daysinyear, 365).

-define(starling_load, 100000000).

-record(refX,
        {
          site        = [],
          type,
          path        = [],
          obj         = null
         }).

-record(local_obj,
        {
          idx,
          type,
          path,
          obj,
          revidx
         }).

write_local_obj() ->
    write_lo(100000).

write_lo(0) -> ok;
write_lo(N) ->
    io:format("N is ~p~n", [N]),
    T = util2:get_timestamp(),
    R = #local_obj{idx = T},
    Fun = fun() ->
                  mnesia:write('hypernumbers.dev&9000&local_obj', R, write)
          end,
    N1 = util2:get_timestamp(),
    mnesia:activity(transaction, Fun),
    N2 = util2:get_timestamp(),
    log(io_lib:format("~p", [(N2 - N1)/100000])),
    write_lo(N - 1).

test_starling() ->
    test_st(?starling_load).

test_st(0) -> io:format("over and out...~n");
test_st(N) ->
    Ticker = (N/1000 - trunc(N/1000)),
    if
         Ticker ==  0 -> io:format("starling load ~p~n", [N]),
                         Mem = os:cmd("free"),
                         log(io_lib:format("~p~n", [Mem]));
         Ticker =/= 0 -> ok
    end,
    Text = list_to_binary("Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Lorem"),
    ustring:to_upper(Text),
    test_st(N -1).

test_clear(Site) ->
    FileName = start_trace([self()]),
    Ref = #refX{site = Site, path = [], obj = {page, "/"}},
    [ok = new_db_api:clear(Ref, all, nil) || _X <- lists:duplicate(100, "bleh")],
    stop_trace(FileName),
    ok.

test_dbsrv(Site) ->
    % put some load on#
    load(Site, 10).
    %% Server = list_to_atom(hn_util:site_to_fs(Site) ++ "_dbsrv"),
    %% io:format("Start tracing...~n"),
    %% FileName = start_trace([Server]),
    %% URL0 = Site ++ "/data/data_alerts/",
    %% URL1 = Site ++ "/test/calculations/",
    %% URL2 = Site ++ "/test/calculations/data/",
    %% RefX0 = hn_util:url_to_refX(URL0),
    %% RefX1 = hn_util:url_to_refX(URL1),
    %% RefX2 = hn_util:url_to_refX(URL2),
    %% Root = code:lib_dir(hypernumbers),
    %% Dir = Root ++ "/../../priv/load_testing/json/",
    %% io:format("Adding new data...~n"),
    %% hn_import:json_file(URL1, Dir ++ "alerts.json"),
    %% hn_import:json_file(URL1, Dir ++ "calculations.json"),
    %% hn_import:json_file(URL2, Dir ++ "data.json"),
    %% io:format("about to stop tracing and begin analysis (might take some time!)~n"),
    %% stop_trace(FileName),
    %% io:format("Now clean up...~n"),
    %% ok = new_db_api:delete(RefX0, nil),
    %% ok = new_db_api:delete(RefX1, nil),
    %% ok = new_db_api:delete(RefX2, nil).

start_trace(Supervisors) when is_list(Supervisors) ->
    Fun = fun(X) when is_pid(X) -> X;
             (X) when is_atom(X) -> whereis(X)
          end,
    Sups2 = [Fun(X) || X <- Supervisors],
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
        Size > 21     -> exit("You're 'avin a laugh, int'ya...");
        0 < Size andalso Size < 21 ->
            % P1 = ["admin", "data_alerts"],
            % URL1 = Site ++ hn_util:list_to_path(P1),
            % Root = code:lib_dir(hypernumbers),
            % Dir = Root ++ "/../../priv/load_testing/json/",
            %hn_import:json_file(URL1, Dir ++ "alerts.json"),
            l2(Site, Size)
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
    %N1 = util2:get_timestamp(),
    %hn_import:json_file(URL1, Dir ++ "calculations.json"),
    %hn_import:json_file(URL1, Dir ++ "meter_page.json"),
    N2 = util2:get_timestamp(),
    hn_import:json_file(URL2, Dir ++ "data.json"),
    N3 = util2:get_timestamp(),
    NLocks = length(mnesia:system_info(held_locks)),
    %log(io_lib:format("~p\t~p\t~p\t~p\t~p", [N, M, (N2 - N1)/100000,
    %                                         (N3 - N2)/1000000, NLocks])),
    log(io_lib:format("~p\t~p\t~p\t~p", [N, M, (N3 - N2)/1000000, NLocks])),
    %io:format("Logs are ~p~n", [mnesia:system_info(held_locks)]),
    limiter(),
    l3(Site, N, M - 1).

limiter() ->
    Srv = hn_util:site_to_atom("http://hypernumbers.dev:9000", "_dbsrv"),
    Pid = whereis(Srv),
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    Locks = length(mnesia:system_info(held_locks)),
    DirtyQueue = mnesia:table_info('hypernumbers.dev&9000&dirty_queue', size),
    if
        Len > 100
            orelse Locks > 100
            orelse DirtyQueue > 1000  -> timer:sleep(100),
                                         limiter();
        true -> ok
    end.

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
