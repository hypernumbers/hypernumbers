-module(upload).

-define(templates, ["buildings","building", null, "year_page", "month_page",
                    "day_page", "meter_page"]).
-define(final_template, null).
-define(alerts, [
          {"static_data",         ["static-data"]},
          {"admin",               ["admin"]},
          {"admin_data_alerts",   ["admin", "data_alerts"]}
         ]).
-define(bits_and_bobs, [
          {"lookup-data",         ["admin", "lookup-data"]},
          {"budget_group",        ["budgets", "dla-piper"]},
          {"budget_building",     ["budgets", "dla-piper", "st-pauls-place"]},
          {"budget_building",     ["budgets", "dla-piper", "noble-street"]},
          {"budget_building",     ["budgets", "dla-piper", "dibb-lipton"]},
          {"budget_building",     ["budgets", "dla-piper", "collins-house"]},
          {"budget_building",     ["budgets", "dla-piper", "west-george-street"]},
          {"initiative_page",     ["initiatives"]},
          {"initiative_new",      ["initiatives", "new"]},
          {"alerts",              ["alerts"]},
          {"my_page",             ["my-page", "stephen"]},
          {"chart_builder",       ["my-page", "stephen", "chartbuilder"]},
          {"crc_report",          ["compliance", "crc"]},
          {"suppliers",           ["suppliers"]},
          {"supplier_page",       ["suppliers", "cory-environmental"]},
          {"supplier_admin_page", ["suppliers", "cory-environmental", "admin"]},
          {"client_contentmgt",   ["clients"]},
          {"client_page",         ["clients", "europa"]},
          {"twitter_inserts",     ["social", "twitter"]},
          {"news_create_page",    ["social", "news"]},
          {"news_story_page",     ["social", "news", "news-item"]}
         ]).


-define(site, "http://dla-piper.hypernumbers.com:80").
%-define(site, "http://hypernumbers.dev:9000").

-record(refX,
        {
          site        = [],
          type,
          path        = [],
          obj         = null
         }).

-export([
         upload/0,
         perf_test/0
        ]).

perf_test() ->
    Stamp = tconv:to_s(util2:get_timestamp()),
    FileName = "fprof.trace." ++ Stamp,
    fprof:trace([start, {file, FileName}]),
    RefX = #refX{site = ?site, path = [], obj = {page, "/"}},
    hn_templates:load_template(RefX, "meter_page"),
    fprof:trace([stop]),
    fprof:profile([{file, FileName}]),
    fprof:analyse([{dest, "fprof.analyze."++Stamp}]).

upload() ->
    run_templates(?alerts),
    Dir = "/hn/hypernumbers/priv/dataupload/",
    Files = filelib:wildcard(Dir ++ "/*.xls"),
    io:format("There are ~p files~n", [length(Files)]),
    io:format("An example is ~p~n", [hd(Files)]),
    Hierarchy = upload2(Files, []),
    Hierarchy2 = sort(hslists:uniq(lists:flatten(Hierarchy))),
    run_templates(Hierarchy2),
    run_templates(?bits_and_bobs).

% gonnae load longest first
sort(List) ->
    Fun = fun({_Temp1, Path1}, {_Temp2, Path2}) ->
                  if
                      length(Path1) =< length(Path2) -> false;
                      length(Path1) > length(Path2)   -> true
                  end
          end,
    lists:sort(Fun, List).

run_templates(Script) ->
    Fun = fun(X, Y) ->
                  io:format("loading ~p on page ~p~n", [X, Y]),
                  #refX{site = ?site, path = Y, obj = {page, "/"}}
          end,
    Fun2 = fun(_X, null) -> ok;
              (#refX{path = P} = X, Y) ->
                   %log_memory(P),
                   hn_templates:load_template_if_no_page(X, Y),
		   limiter()
           end,
    syslib:top5(),
    %garbage_collect(),
    [ok = Fun2(Fun(X, Y), X) || {X, Y} <- Script].

log_memory(Path) ->
    N1 = util2:get_timestamp(),
    Memory = erlang:memory(),
    {value, {total, Total}} = lists:keysearch(total, 1, Memory),
    {value, {processes, Processes}} = lists:keysearch(processes, 1, Memory),
    {value, {processes_used, Proc_U}} = lists:keysearch(processes_used, 1, Memory),
    {value, {system, System}} = lists:keysearch(system, 1, Memory),
    {value, {atom, Atom}} = lists:keysearch(atom, 1, Memory),
    {value, {atom_used, Atom_U}} = lists:keysearch(atom_used, 1, Memory),
    {value, {binary, Binary}} = lists:keysearch(binary, 1, Memory),
    {value, {code, Code}} = lists:keysearch(code, 1, Memory),
    {value, {ets, Ets}} = lists:keysearch(ets, 1, Memory),
    Server = hn_util:site_to_atom(?site, "_dbsrv"),
    PID = whereis(Server),
    [{heap_size, Heap}] = process_info(PID, [heap_size]),
    [{message_queue_len, Msgs}] = process_info(PID, [message_queue_len]),
    Server2 = hn_util:site_to_atom(?site, "_zinf"),
    PID2 = global:whereis_name(Server2),
    [{heap_size, Heap2}] = process_info(PID2, [heap_size]),
    [{message_queue_len, Msgs2}] = process_info(PID2, [message_queue_len]),
    log(io_lib:format("~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p",
                      [hn_util:list_to_path(Path), N1, Total, Processes,
                       Proc_U, System, Atom, Atom_U, Binary, Code, Ets,
                       Heap, Msgs, Heap2, Msgs2])).

upload2([], Acc) -> Acc;
upload2([H | T], Acc) ->
    io:format("Uploading ~p~n", [H]),
    File = hd(lists:reverse(string:tokens(H, "/"))),
    Segs = string:tokens(File, "."),
    {Segs1, _xls} = lists:split(length(Segs) - 1, Segs),
    {Segs2, _end} = lists:split(length(Segs) - 2, Segs),
    Segs3 = make_paths(Segs2, [], []),
    RefX = #refX{site = ?site, path = Segs1, obj = {page, "/"}},
    % first upload the data at the bottom of the tree
    case page_srv:does_page_exist(?site, Segs1) of
        false -> case ?final_template of
                     null -> ok;
                     Temp -> hn_templates:load_template(RefX, Temp)
                 end,
                 hn_import:xls_file(hn_util:refX_to_url(RefX), H, "sheet1"),
                 %garbage_collect(),
                 %log_remoting(),
                 limiter();
        true  -> ok
    end,
    % now work your way up the tree loading templates
    NewAcc = [lists:zip(lists:reverse(?templates), lists:reverse(Segs3)) | Acc],
    upload2(T, NewAcc).

make_paths([], _, Acc) -> lists:reverse(Acc);
make_paths([H | T], [], []) -> make_paths(T, [H], [[H]]);
make_paths([H | T], Prefix, Acc) ->
    Seg = lists:concat([Prefix, [H]]),
    make_paths(T, Seg, [Seg | Acc]).

log_remoting() ->
    %Srv = hn_util:site_to_atom(?site, "_remoting"),
    %garbage_collect(global:whereis_name(Srv)),
    %io:format("Heap size of remoting is ~p~n",
    %          [process_info(whereis(Srv), heap_size)]),
    ok.

limiter() ->
    Srv = hn_util:site_to_atom(?site, "_dbsrv"),
    Pid = whereis(Srv),
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    Locks = length(mnesia:system_info(held_locks)),
    DirtyQueue = mnesia:table_info('dla-piper.hypernumbers.com&80&dirty_queue', size),
    if
        Len > 100
            orelse Locks > 100
            orelse DirtyQueue > 1000  -> timer:sleep(100),
                                         limiter();
        true -> ok
    end.

log(String) ->
    log(String, "../logs/memory_logs.txt").

log(String, File) ->
    _Return=filelib:ensure_dir(File),

    case file:open(File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [String]),
	    file:close(Id);
	_ ->
	    error
    end.
