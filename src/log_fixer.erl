-module(log_fixer).
-compile(export_all).


fix(Name) ->
    {ok, Cont} = wrap_log_reader:open(logfile(Name)),
    Second = Name ++ "_new",
    filelib:ensure_dir(logfile(Second)),
    Opts = [{name,Second}, {file,logfile(Second)},
            {type,wrap},  {size, {2097152, 99}}],
    {ok, _} = disk_log:open(Opts),
    {ok, NCont} = fix_loop(Cont, Second),
    wrap_log_reader:close(NCont).

fix_loop(Cont, Second) ->
    case wrap_log_reader:chunk(Cont) of
        {NCont, eof} ->
            {ok, NCont};
        {NCont, Terms} ->
            Fixed = [fix_term(T) || T <- Terms],
            disk_log:alog_terms(Second, Fixed),
            fix_loop(NCont, Second)
    end.
            
fix_term(Post) ->
    case proplists:get_value(body, Post) of
        undefined -> Post;
        BodyJson  ->
            Body = (mochijson:encoder([{input_encoding, utf8}]))({struct, BodyJson}),
            Body2 = list_to_binary(Body),
            lists:keyreplace(body, 1, Post, {body, Body2})
    end.

swap(Name) ->
    disk_log:close(Name),
    file:delete(logfile(Name)),
    ok = disk_log:reopen(Name ++ "_new", logfile(Name)).
    
logfile(Name) ->
    lists:concat([code:lib_dir(hypernumbers),
                  "/../../var/mochilog/",Name,".LOG"]).

