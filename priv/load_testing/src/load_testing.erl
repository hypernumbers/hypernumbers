%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Systemic repeatable load testing
%%%
%%% @end
%%% Created : 24 Jul 2011 by <gordon@hypernumbers.com>

-module(load_testing).

-include("spriki.hrl").
-include("load_testing.hrl").

% note that the names service 'site_dbsrv' looks like  global name
% but is infact a local one and is handled by a special case in get_pid/1
-define(globalnames, [
                      status,
                      auth,
                      tick,
                      remoting,
                      dbsrv_sup,
                      pages,
                      sup,
                      zinf,
                      dbsrv
                     ]).
-define(localnames, [
                     sitemaster_sup,
                     service_sup,
                     hypernumbers_sup,
                     hn_mochi
                    ]).

-export([
         percept/0,
         test/0,
         test_SPAWN/0
        ]).

-export([
         profile_zinf_srv/0,
         profile_zs/0,
         test_zs/0,
         load/0,
         load/1,
         load/2,
         load/3
        ]).

% exports for spawning
-export([
         log_memory/1,
         log_memory_LOOP/2
         ]).

percept() ->
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    % Dir = get_dir(),
    File = "percept" ++ Stamp ++ ".trace",
    PerceptSpec = {load_testing, load, [disc_only,
                                        [
                                         {run, [
                                                trash_db,
                                                load_data
                                                %load_calcs,
                                                %load_zs,
                                                %afterz_data,
                                                %afterz_calcs
                                               ]
                                         }
                                        ]
                                       ]
                  },
    Ret1 = percept:profile(File, PerceptSpec, [procs]),
    io:format("Ret1 is ~p~n", [Ret1]),
    Ret2 = percept:analyze(File),
    io:format("Ret2 is ~p~n", [Ret2]),
    Ret3 = percept:start_webserver(8888),
    io:format("Ret3 is ~p~n", [Ret3]).

test() -> spawn(load_testing, test_SPAWN, []).

test_SPAWN() ->
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    load_testing:load(Stamp, disc_only,
                      [{run, [
                              trash_db,
                              load_data,
                              load_calcs,
                              load_zs,
                              afterz_data,
                              afterz_calcs
                             ]}
                      ]).
%% load_testing:load(disc_only,
%%                            [{run, [
%%                                    afterz_data
%%                                   ]},
%%                             {fprof, [
%%                                      dbsrv,
%%                                      zinf
%%                                     ]}]).

profile_zinf_srv() ->
    TraceFile = zinf_srv:start_fprof(?site),
    load(),
    zinf_srv:stop_fprof(?site, TraceFile).


load() ->
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    load_2(Stamp, disc_only, [{run, [
                                     trash_db,
                                     bulk_pages,
                                     load_data,
                                     load_calcs,
                                     load_zs,
                                     afterz_data,
                                     afterz_calcs,
                                     tests
                                    ]}
                             ]).

load(Stamp) ->
    load_2(Stamp, disc_only, [{run, [
                                     trash_db,
                                     bulk_pages,
                                     load_data,
                                     load_calcs,
                                     load_zs,
                                     afterz_data,
                                     afterz_calcs,
                                     tests
                                    ]}
                             ]).


load(Type, Spec) ->
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    load_2(Stamp, Type, Spec).

load(Stamp, Type, Spec) ->
    load_2(Stamp, Type, Spec).

profile_zs() ->
    %Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    %Dir = code:lib_dir(hypernumbers) ++ "/../../priv/load_testing/logs/",
    %TraceFile = Dir ++ "profile_zs" ++ Stamp ++ ".trace",
    %fprof:trace(start, TraceFile),
    Path = ["profile_zs"],
    RefX = #refX{site = ?site, path = Path, obj = {page, "/"}},
    Template = "minizs",
    ok = hn_templates:load_template(RefX, Template).
    %fprof:trace(stop),
    %fprof:profile(file, TraceFile),
    %fprof:analyse([{dest, Dir ++ "profile_zs" ++ Stamp ++ ".analysis"}]).

test_zs() ->
    %Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    %Dir = code:lib_dir(hypernumbers) ++ "/../../priv/load_testing/logs/",
    %TraceFile = Dir ++ "test_zs" ++ Stamp ++ ".trace",
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

load_2(Stamp, Type, Spec) when Type == disc_only orelse Type == disc_and_mem ->
    load_3(Stamp, Type, Spec);
load_2(_Stamp, Type, _Spec) ->
    io:format("Invalid parameter. Type is ~p and it should "
                 ++ "be one of 'disc_only' or 'disc_and_mem'~n",
                 [Type]).

load_3(Stamp, Type, Spec) ->

    RSpc = get_spec(run, Spec),
    CSpc = get_spec(cprof, Spec),
    FSpc = get_spec(fprof, Spec),

    io:format("Running load tests~nRunSpec is   ~p~nCProfSpec is ~p~n"
              ++ "FprofSpec is ~p~n", [RSpc, CSpc, FSpc]),

    % trash_db will delete all running processes so need to only
    % start fprof and stuff after it has fun
    run(RSpc, [],   trash_db,      Type,           fun trash_db/1),

    start_fprof(FSpc, Stamp),

    run(RSpc, [],   bulk_pages,    [],             fun bulk_pages/1),
    run(RSpc, CSpc, log_memory,    Stamp,          fun log_memory/1),
    run(RSpc, CSpc, load_data,     Stamp,          fun load_data/1),
    run(RSpc, CSpc, load_calcs,    Stamp,          fun load_calcs/1),
    run(RSpc, CSpc, load_zs,       Stamp,          fun load_zs/1),
    run(RSpc, CSpc, load_zs_cprof, {?site, Stamp}, fun load_zs_cprof/1),
    run(RSpc, CSpc, afterz_data,   Stamp,          fun afterz_data/1),
    run(RSpc, CSpc, afterz_calcs,  Stamp,          fun afterz_calcs/1),
    run(RSpc, CSpc, tests,         Stamp,          fun run_tests/1),

    stop_fprof(FSpc, Stamp),
    io:format("over and out...~n").

run_tests(Stamp) ->
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
                           ?no_of_forcedzs, ?no_of_forcedzs).

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

%% get_names() ->
%%     Global = [{global:whereis_name(X), X} || X <- global:registered_names()],
%%     Local  = [{whereis(X), X}             || X <- registered()],
%%     lists:merge([Global, Local]).

run(Spec, CProf, Test, Stamp, Fun) ->
    RunCProf = lists:member(Test, CProf),
    case  RunCProf of
        true  -> cprof:start();
        false -> ok
    end,
    case {lists:member(Test, Spec), lists:member(all, Spec)} of
        {true, _}     -> Fun(Stamp);
        {false, true} -> Fun(Stamp);
        _             -> ok
    end,
    case  RunCProf of
        true  -> io:format("Starting cprof analysis...~n"),
                 cprof:pause(),
                 {_Total, Log} = cprof:analyse(),
                 File = "cprof" ++ "." ++ atom_to_list(Test)
                     ++ "." ++ Stamp ++ ".output",
                 Msg = io_lib:format("~w", [Log]),
                 log(Msg, File),
                 io:format("cprof written.~n");
        false -> ok
    end.

load_zs_cprof({Site, Stamp}) ->
    zinf_srv:start_cprof(Site),
    load_zs(Stamp),
    zinf_srv:stop_cprof(Site).

load_zs(Stamp) ->
    io:format("~nabout to load zquery pages...~n"),
    ok = load_pages("zqueries" ++ Stamp, ?zquerypage, ?zqueryprefix,
                    ?no_of_zquerypages, ?no_of_zquerypages).

load_calcs(Stamp) ->
    load_calcs2(Stamp, ?calcsprefix, ?no_of_calcpages).

afterz_calcs(Stamp) ->
    load_calcs2(Stamp, ?additionalcalcs, ?no_of_additional_calcs).

load_calcs2(Stamp, Prefix, NoOfPages) ->
    io:format("~nabout to load calculation pages...~n"),
    ok = load_pages("calculations" ++ Stamp, ?calcspage, Prefix,
                    NoOfPages, NoOfPages).

load_data(Stamp) ->
    load_data2(Stamp, ?dataprefix, ?no_of_datapages).

afterz_data(Stamp) ->
    load_data2(Stamp, ?additionaldata, ?no_of_additional_data).

load_data2(Stamp, Prefix, NoOfPages) ->
    io:format("~nabout to load data pages...~n"),
    ok = load_pages("data" ++ Stamp, ?datapage, Prefix,
                    NoOfPages, NoOfPages).

log_memory(Stamp) -> spawn(load_testing, log_memory2, [Stamp]).

trash_db(Type) ->
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
    end.

bulk_pages([]) ->
        % bulk up on pages
    io:format("~nabout to bulk up pages...~n"),
    % put the auth_srv into memory first
    ok = hn_db_admin:mem_only(?site, "kvstore"),
    ok = bulk_pages(?pageload, ?pageload, ?pageload, ?pageload, ?pageload),
    % now put the auth_srv back in place
    ok = hn_db_admin:disc_only(?site, "kvstore").

get_spec(Type, Spec) ->
    case lists:keyfind(Type, 1, Spec) of
        {Type, TypeSpec} -> TypeSpec;
        false            -> []
    end.

start_fprof([], _Stamp)   -> ok;
start_fprof(all, Stamp) ->
    Dir = get_dir(),
    File = make_fprof_file(Stamp) ++ ".trace",
    fprof:trace([start, {file, Dir ++ File}, {procs, all}]);
start_fprof(Spec, Stamp) ->
    Dir = get_dir(),
    Spec2 = [get_pid(X) || X <- Spec],
    io:format("In start_fprof Spec is ~p Spec2 is ~p~n", [Spec, Spec2]),
    File = make_fprof_file(Stamp) ++ ".trace",
    io:format("File is ~p~n", [File]),
    fprof:trace([start, {file, Dir ++ File}, {procs, Spec2}]).

stop_fprof([], _Stamp) -> ok;
stop_fprof(Spec, Stamp)  ->
    FileRoot = make_fprof_file(Stamp),
    Dir = get_dir(),
    io:format("Spec is ~p FileRoot is ~p~n", [Spec, FileRoot]),
    fprof:profile([{file, Dir ++ FileRoot ++ ".trace"}]),
    fprof:analyse([{dest, Dir ++ FileRoot ++ ".analysis"}]),
    ok.

get_pid(X) when is_pid(X) ->
    X;
get_pid(dbsrv) ->
    Name = hn_util:site_to_atom(?site, "_dbsrv"),
    whereis(Name);
get_pid(X) when is_atom(X) ->
    case lists:member(X, ?globalnames) of
        true -> Name = hn_util:site_to_atom(?site, "_" ++ atom_to_list(X)),
                io:format("Name is ~p~n", [Name]),
                global:whereis_name(Name);
        false -> whereis(X)
    end.

make_fprof_file(Stamp) -> "fprof" ++ Stamp.

get_dir() ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../priv/load_testing/logs/",
    _Return = filelib:ensure_dir(Dir),
    Dir.
