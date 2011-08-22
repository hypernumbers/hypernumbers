%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Systemic repeatable load testing
%%%
%%% @end
%%% Created : 24 Jul 2011 by <gordon@hypernumbers.com>

-module(load_testing).

-include("spriki.hrl").
-include("load_testing.hrl").

-export([
         profile_zinf_srv/0,
         profile_zs/0,
         test_zs/0,
         load_only/0,
         load_only/1,
         load_test/0,
         load_test/1
        ]).

% exports for spawning
-export([
         log_memory/1,
         log_memory_LOOP/2
         ]).

profile_zinf_srv() ->
    TraceFile = zinf_srv:start_fprof(?site),
    load_only(),
    zinf_srv:stop_fprof(?site, TraceFile).

load_only() -> load_2(disc_only, load_only).

load_only(Type) -> load_2(Type, load_only).

load_test() -> load_2(disc_only, all).

load_test(Type) -> load_2(Type, all).

profile_zs() ->
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    Dir = code:lib_dir(hypernumbers) ++ "/../../priv/load_testing/logs/",
    TraceFile = Dir ++ "profile_zs" ++ Stamp ++ ".trace",
    %fprof:trace(start, TraceFile),
    Path = ["profile_zs"],
    RefX = #refX{site = ?site, path = Path, obj = {page, "/"}},
    Template = "minizs",
    ok = hn_templates:load_template(RefX, Template).
    %fprof:trace(stop),
    %fprof:profile(file, TraceFile),
    %fprof:analyse([{dest, Dir ++ "profile_zs" ++ Stamp ++ ".analysis"}]).

test_zs() ->
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    Dir = code:lib_dir(hypernumbers) ++ "/../../priv/load_testing/logs/",
    TraceFile = Dir ++ "test_zs" ++ Stamp ++ ".trace",
    %%fprof:trace(start, TraceFile),
    test_z2(?no_of_zquery_profiles, ?no_of_zquery_profiles).
    %%fprof:trace(stop),
    %%fprof:profile(file, TraceFile),
    %%fprof:analyse([{dest, Dir ++ "test_zs" ++ Stamp ++ ".analysis"}]).

test_z2(_Max, 0) -> ok;
test_z2(Max, N) ->
    Path = [?zquery_profile_page],
    RefX = #refX{site = ?site, path = Path, obj = {cell, {1, 1}}},
    io:format("forcing recalcs ~p on ~p~n", [N, Path]),
    StartTime = get_time(),
    new_db_api:write_attributes([{RefX, [{"formula",
                                          "=sum(/data/[true]/a1)"}]}]),
    new_db_api:wait_for_dirty(?site),
    EndTime = get_time(),
    Msg = io_lib:format("~p,~p", [Max - N + 1,
                                  EndTime - StartTime]),
    log(Msg, ?zquery_profile_page ++ ".csv"),
    test_z2(Max, N - 1).

load_2(Type, Extent) ->

    % check the type
    case Type of
        disc_only    -> load_3(Type, Extent);
        disc_and_mem -> load_3(Type, Extent);
        Other     -> io:format("Invalid parameter. Type is ~p and it should "
                               ++ "be one of 'disc_only' or 'disc_and_mem'~n",
                              [Other])
    end.

load_3(Type, Extent) ->

    % first get a filestamp
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),

    % trash the load database if it exists
    case hn_setup:site_exists(?site) of
        true  -> io:format("Deleting ~p~n", [?site]),
                 hn_setup:delete_site(?site);
        false -> ok
    end,
    hn_setup:site(?site, load_testing, []),
    case Type of
        disc_only    -> io:format("~nSetting the site to run from disc only~n"),
                        hn_db_admin:disc_only(?site);
        disc_and_mem -> ok
    end,

    % bulk up on pages
    io:format("~nabout to bulk up pages...~n"),
    % put the auth_srv into memory first
    ok = hn_db_admin:mem_only(?site, "kvstore"),
    ok = bulk_pages(?pageload, ?pageload, ?pageload, ?pageload, ?pageload),
    % now put the auth_srv back in place
    ok = hn_db_admin:disc_only(?site, "kvstore"),

    spawn(load_testing, log_memory, [Stamp]),

    % load datapoints
    io:format("~nabout to load data pages...~n"),
    ok = load_pages("data" ++ Stamp, ?datapage, ?dataprefix,
                    ?no_of_datapages, ?no_of_datapages),

    % load calculations
    io:format("~nabout to load calculation pages...~n"),
    ok = load_pages("calculations" ++ Stamp, ?calcspage, ?calcsprefix,
                    ?no_of_calcpages, ?no_of_calcpages),

    % load zqueries
    io:format("~nabout to load zquery pages...~n"),
    ok = load_pages("zqueries" ++ Stamp, ?zquerypage, ?zqueryprefix,
                    ?no_of_zquerypages, ?no_of_zquerypages),

    % test impact of zqueries on load
    % load datapoints
    io:format("~nabout to load post-z data pages...~n"),
    ok = load_pages("data" ++ Stamp, ?datapage, ?additionaldata,
                    ?no_of_additional_data, ?no_of_additional_data),

    % load calculations
    io:format("~nabout to load post-z calculation pages...~n"),
    ok = load_pages("calculations" ++ Stamp, ?calcspage,
                    ?additionalcalcs,
                    ?no_of_additional_calcs, ?no_of_additional_calcs),

    % now start some tests
    case Extent of
        load_only ->

            % just stop
            io:format("Site loaded. Over and out...~n");

        all ->

            % create a page, delete it and then create it again
            io:format("~nabout to test page deletes...~n"),
            ok = force_page_del_test("delete_with_calcs" ++ Stamp,
                                     "delete_page_with_calcs", ?calcspage,
                                     ?no_of_deletes, ?no_of_deletes),
            ok = force_page_del_test("delete_with_data" ++ Stamp,
                                     "delete_page_with_data", ?datapage,
                                     ?no_of_deletes, ?no_of_deletes),

            % force a recalc on a calcs page by updating cell A1
            io:format("~nabout to force recalcs...~n"),
            ok = force_recalc_test("force_recalc" ++ Stamp, ?calcsprefix,
                                   ?no_of_recalcs, ?no_of_recalcs),

            % force the zqueries to recalc by updating a data page
            io:format("~nabout to test zqueries...~n"),
            ok = force_zquery_test("force_zquery" ++ Stamp, ?dataprefix,
                                   ?no_of_forcedzs, ?no_of_forcedzs),

            io:format("Over and out...~n"),
            ok
    end.

bulk_pages(_Max, 0, 0, 0, 0) -> ok;
bulk_pages(Max, I, 0, 0, 0) -> bulk_pages(Max, I - 1, Max, Max, Max);
bulk_pages(Max, I, J, 0, 0) -> io:format("bulking pages: ~p, ~p~n",
                                         [I, J]),
                               bulk_pages(Max, I, J - 1, Max, Max);
bulk_pages(Max, I, J, K, 0) -> syslib:limit_global_mq(?site, "_pages"),
                               new_db_api:wait_for_dirty(?site),
                               bulk_pages(Max, I, J, K - 1, Max);
bulk_pages(Max, I, J, K, L) ->
    Path = [hn_webcontrols:pad(I), hn_webcontrols:pad(J),
            hn_webcontrols:pad(K), hn_webcontrols:pad(L)],
    RefX = #refX{site = ?site, path = Path, obj = {cell, {1,2}}},
    new_db_api:write_attributes([{RefX, [{"formula", "xxx"}]}]),
    bulk_pages(Max, I, J, K, L - 1).

force_recalc_test(_Label, _Prefix, _Max, 0) -> ok;
force_recalc_test(Label, Prefix, Max, N) ->
    Seg = hn_webcontrols:pad(N),
    Path = [Prefix, Seg],
    RefX = #refX{site = ?site, path = Path, obj = {cell, {1, 1}}},
    io:format("forcing recalcs ~p on ~p~n", [N, Path]),
    StartTime = get_time(),
    new_db_api:write_attributes([{RefX, [{"formula", "9999"}]}]),
    new_db_api:wait_for_dirty(?site),
    EndTime = get_time(),
    Msg = io_lib:format("~p,~p", [Max - N + 1,
                                  EndTime - StartTime]),
    log(Msg, Label ++ ".csv"),
    force_recalc_test(Label, Prefix, Max, N - 1).

force_zquery_test(_Label, _Prefix, _Max, 0) -> ok;
force_zquery_test(Label, Prefix, Max, N) ->
    Seg = hn_webcontrols:pad(N),
    Path = [Prefix, Seg],
    RefX = #refX{site = ?site, path = Path, obj = {cell, {1, 1}}},
    io:format("forcing z-queries ~p on ~p~n", [N, Path]),
    StartTime = get_time(),
    new_db_api:write_attributes([{RefX, [{"formula", "9999"}]}]),
    new_db_api:wait_for_dirty(?site),
    EndTime = get_time(),
    Msg = io_lib:format("~p,~p", [Max - N + 1,
                                  EndTime - StartTime]),
    log(Msg, Label ++ ".csv"),
    force_zquery_test(Label, Prefix, Max, N - 1).

force_page_del_test(_, _, _, _, 0) -> ok;
force_page_del_test(Label, Page, Template, Max, N) ->
    io:format("~p: doing a page delete (~p)~n", [Label, Max - N + 1]),
    RefX = #refX{site = ?site, path = [Page], obj = {page, "/"}},
    ok = hn_templates:load_template(RefX, Template),
    StartTime = get_time(),
    ok = new_db_api:delete(RefX, nil),
    MidTime = get_time(),
    new_db_api:wait_for_dirty(?site),
    EndTime = get_time(),
    Msg = io_lib:format("~p,~p,~p", [Max - N + 1,
                                     MidTime - StartTime,
                                     EndTime - StartTime]),
    log(Msg, Label ++ ".csv"),
    force_page_del_test(Label, Page, Template, Max, N - 1).

load_pages(_Label, _Template, _Prefix, _Max, 0) -> ok;
load_pages(Label, Template, Prefix, Max, N) ->
    Path = [Prefix, "page" ++ hn_webcontrols:pad(N)],
    io:format("~p: (~p) loading ~p as ~p ~n", [Label, Max - N + 1, Template,
                                          hn_util:list_to_path(Path)]),
    RefX = #refX{site = ?site, path = Path, obj = {page, "/"}},
    StartTime = get_time(),
    ok = hn_templates:load_template(RefX, Template),
    MidTime = get_time(),
    new_db_api:wait_for_dirty(?site),
    EndTime = get_time(),
    Msg = io_lib:format("~p,~p,~p", [Max - N + 1,
                                     MidTime - StartTime,
                                     EndTime - StartTime]),
    log(Msg, Label ++ ".csv"),
    load_pages(Label, Template, Prefix, Max, N - 1).

get_time() -> util2:get_timestamp()/1000000.

log(String, File) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../priv/load_testing/logs/",
    _Return = filelib:ensure_dir(Dir ++ File),
    Date = dh_date:format("d-M-y h:i:s"),

    case file:open(Dir ++ File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Date ++ "," ++ String]),
            file:close(Id);
        _ ->
            error
    end.

log_memory(Stamp) ->
    Names = get_names(),
    log_memory_LOOP(Stamp, Names).

log_memory_LOOP(Stamp, Names) ->
    Msg = io_lib:format("~w", [top5(Names)]),
    log(lists:flatten(Msg), "memory_log" ++ Stamp),
    timer:sleep(100),
    log_memory_LOOP(Stamp, Names).

top5(Names) ->
    Procs = processes(),
    sort(Names, [info(X) || X <- Procs]).

info(X) ->
    Fn1  = case process_info(X, [current_function]) of
               [{current_function, Fn}] -> Fn;
               undefined                -> 0
           end,
    Len1  = case process_info(X, [message_queue_len]) of
                [{message_queue_len, Len}] -> Len;
                undefined                   -> 0
           end,
    Heap1  = case process_info(X, [heap_size]) of
               [{heap_size, Heap}] -> Heap;
               undefined           -> 0
             end,
    Reds1  = case process_info(X, [reductions]) of
               [{reductions, Reds}] -> Reds;
               undefined            -> 0
           end,
    {X, Fn1, Len1, Heap1, Reds1}.

sort(Names, List) ->
    {Len5, _}  = lists:split(5, lists:reverse(lists:keysort(3, List))),
    {Heap5, _} = lists:split(5, lists:reverse(lists:keysort(4, List))),
    {Red5, _}  = lists:split(5, lists:reverse(lists:keysort(5, List))),
    [{longest, subst(Len5, Names, [])}, {heapiest, subst(Heap5, Names, [])},
     {most_reductions, subst(Red5, Names, [])}].

subst([], _Names, Acc) -> lists:reverse(Acc);
subst([{Pid, A, B, C, D} | T], Names, Acc) ->
    Pid2 = case lists:keyfind(Pid, 1, Names) of
               false       -> Pid;
               {Pid, Name} -> Name
           end,
    subst(T, Names, [{Pid2, A, B, C, D} | Acc]).

get_names() ->
    Global = [{global:whereis_name(X), X} || X <- global:registered_names()],
    Local  = [{whereis(X), X}             || X <- registered()],
    lists:merge([Global, Local]).
