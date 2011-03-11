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
    Dir = "/home/gordon/hypernumbers/priv/dataupload/",
    Files = filelib:wildcard(Dir ++ "/*.xls"),
    io:format("There are ~p files~n", [length(Files)]),
    io:format("An example is ~p~n", [hd(Files)]),
    upload2(Files).

upload2([]) ->
    "all uploaded...";
upload2([H | T]) ->
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
