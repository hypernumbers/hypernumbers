-module(upload).

-define(templates, [null, null, null, null, null, null, null]).
-define(final_template, null).

-define(site, "http://hypernumbers.dev:9000").

-record(refX,
        {
          site        = [],
          type,
          path        = [],
          obj         = null
         }).

-export([
         upload/0
        ]).

upload() ->
    Dir = "/hn/hypernumbers/priv/dataupload/",
    Files = filelib:wildcard(Dir ++ "/*.xls"),
    io:format("There are ~p files~n", [length(Files)]),
    io:format("An example is ~p~n", [hd(Files)]),
    upload2(Files).

upload2([]) ->
    "all uploaded...";
upload2([H | T]) ->
    io:format("Uploading ~p~n", [H]),
    File = hd(lists:reverse(string:tokens(H, "/"))),
    Segs = string:tokens(File, "."),
    {Segs1, _xls} = lists:split(length(Segs) - 1, Segs),
    {Segs2, _end} = lists:split(length(Segs) - 2, Segs),
    Segs3 = make_paths(Segs2, [], []),
    ok = upload3(?templates, Segs3),
    RefX = #refX{site = ?site, path = Segs1, obj = {page, "/"}},
    case ?final_template of
        null -> ok;
        Temp -> hn_templates:load_template(RefX, Temp)
    end,
    hn_import:xls_file(hn_util:refX_to_url(RefX), H, "sheet1"),
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
    log(io_lib:format("~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p", 
       [N1, Total, Processes, Proc_U, System, Atom, Atom_U, Binary, Code, Ets])),
    garbage_collect(),
    upload2(T).

make_paths([], _, Acc) -> lists:reverse(Acc);
make_paths([H | T], [], []) -> make_paths(T, [H], [H]);
make_paths([H | T], Prefix, Acc) ->
    Seg = lists:concat([Prefix, [H]]),
    make_paths(T, Seg, [Seg | Acc]).
    
upload3([], []) -> ok;
upload3([null | T1], [_Path | T2]) -> upload3(T1, T2);
upload3([Template | T1], [Path | T2]) ->
    RefX = #refX{site = ?site, path = Path, obj = {page, "/"}},
    hn_templates:load_template(RefX, Template),
    upload3(T1, T2).

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
