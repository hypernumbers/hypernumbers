%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Systemic repeatable load testing
%%%
%%% @end
%%% Created : 24 Jul 2011 by <gordon@hypernumbers.com>

-module(load_testing).

-include("spriki.hrl").

-define(site, "http://loadtesting.hypernumbers.com:80").

% basic load parameters
-define(no_of_datapages, 100).
-define(no_of_calcpages, 100).
-define(no_of_zquerypages, 100).
-define(dataprefix, "datapages").
-define(datapage, "400datapoints").
-define(calcsprefix, "calcspages").
-define(calcspage, "400calcs").
-define(zqueryprefix, "zquerypages").
-define(zquerypage, "400zqueries").
-define(bulkpage, "onecell").
-define(pageload, 10). % creates N^4 pages

% now the number times the tests have to run
-define(no_of_deletes, 25).
-define(no_of_recalcs, 25).
-define(no_of_forcedzs, 25).

-export([
         load_test/0
        ]).

load_test() ->

    % first get a filestamp
    Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),

    % trash the load database if it exists
    case hn_setup:site_exists(?site) of
        true  -> io:format("Deleting ~p~n", [?site]),
                 hn_setup:delete_site(?site);
        false -> ok
    end,
    hn_setup:site(?site, load_testing, []),
    io:format("~nSetting the site to run from disc only~n"),
    hn_db_admin:disc_only(?site),

    % load datapoints
    io:format("~nabout to load data pages...~n"),
    ok = load_pages("data" ++ Stamp, ?datapage, ?dataprefix,
                    ?no_of_datapages, ?no_of_datapages),

    % load calculations
    io:format("~nabout to load calculation pages...~n"),
    ok = load_pages("calculations" ++ Stamp, ?calcspage, ?calcsprefix,
                    ?no_of_calcpages, ?no_of_calcpages),

    % bulk up on pages
    io:format("~nabout to bulk up pages...~n"),
    % put the auth_srv into memory first
    ok = hn_db_admin:mem_only(?site, "kvstore"),
    ok = bulk_pages(?pageload, ?pageload, ?pageload, ?pageload, ?pageload),
    ok = hn_db_admin:disc_only(?site, "kvstore"),

    % load zqueries
    io:format("~nabout to load zquery pages...~n"),
    ok = load_pages("zqueries" ++ Stamp, ?zquerypage, ?zqueryprefix,
                    ?no_of_zquerypages, ?no_of_zquerypages),

    % now start some tests

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
    ok.

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

