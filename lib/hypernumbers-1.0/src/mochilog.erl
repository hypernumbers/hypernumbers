%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc Module to replay logs from a live server to a dev
%%% environment for debugging
-module(mochilog).

-include("hypernumbers.hrl").
-include("spriki.hrl").
-include("hn_mochi.hrl").

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).
-define(NAME, "post_log").

-export([log/2, start/0, stop/0, replay/2, replay/3, clear/0, repair/1,
         browse/1, browse/2, browse_marks/1, info/2 ]).

-export([ upload_file/3 ]).

-export([stream_log/4]).

%% @spec start() -> ok
%% @doc This starts the log
start() ->

    filelib:ensure_dir(logfile()),
    
    Opts = [{name,?NAME}, {file,logfile()},
            {type,wrap},  {size, {2097152, 99}}],

    case disk_log:open(Opts) of
        {ok, _Log}          -> ok;
        {repaired, _, _, _} -> ok;
        _Else               -> throw({failed_open_log})
    end.

%% @doc Logs individual requests
-spec log(#env{}, #refX{}) -> ok.
log(Env=#env{mochi = Mochi, uid = Uid, raw_body = Body}, Ref) ->
    Post = [{time, erlang:now()},
            {site, Ref#refX.site},
            {path, Mochi:get(raw_path)},
            {method, Mochi:get(method)},
            {body, Body},
            {user, Uid},
            {peer, Mochi:get_header_value("x-forwarded-for")},
            {referer, Mochi:get_header_value("Referer")},
            {browser, Mochi:get_header_value("User-Agent")},
            {accept, Mochi:get_header_value("Accept")} ],
    disk_log:alog(?NAME, Post).

%% @spec stop() -> ok
%% @doc Closes log
stop() ->
    ok = disk_log:close(?NAME).

%% @spec clear() -> ok
%% @doc Deletes current log
clear() ->
    stop(),
    file:delete(logfile()).


%% Repair a logfile, useful for fixing specific copylogs
-spec repair(string()) -> ok | {error, term()}. 
repair(Name) ->
    Opts = [{name, Name}, {file,logfile(Name)},
            {type,wrap},  {size, {2097152, 99}},
            {repair, true}],
    case disk_log:open(Opts) of
        {ok, Log} -> 
            disk_log:close(Log);
        {repaired, Log, Recover, Bad} ->
            io:format("Repaired: recovered: ~p bad: ~p~n", [Recover, Bad]),
            disk_log:close(Log)
    end.


%% 
stream_log(Name, StartD, EndD, Remote) ->
    Log = logfile(Name ++ "/post_log"),
    Filter = make_filter([{method, all}, {between, StartD, EndD}]),
    case filelib:is_file(Log++".siz") of 
        false ->
            Remote ! {self(), {log_error, no_such_log}};
        true ->
            Remote ! {self(), log_start},
            Remote ! {self(), {log_chunk,
                               ["Date,Site,Path,Body,Method,IP,User,"
                                "Referer,User-Agent,Accept\n"]}},
            {ok, Cont} = wrap_log_reader:open(Log),
            {ok, End}  = walk(fun(Terms, _) -> 
                                      do_stream(Remote, Filter, Terms) 
                              end, Cont, 0),
            wrap_log_reader:close(End),
            Remote ! {self(), log_finished}
    end.

do_stream(Remote, Filter, Terms) ->
    Messages = [handle_term(Filter, T, 0, fun mi_entry/2) || T <- Terms],
    Remote ! {self(), {log_chunk, Messages}},
    receive 
        {Remote, log_continue_stream} ->
            ok
    after 2000 ->
            exit(log_stream_timeout)
    end.
        
mi_entry(Post, _) ->
    Date = dh_date:format("r", proplists:get_value(time, Post)),
    Body = proplists:get_value(body, Post),
    Site = proplists:get_value(site, Post),
    Path = proplists:get_value(path, Post),
    Mthd = proplists:get_value(method, Post),
    Peer = proplists:get_value(peer, Post),
    Uid = proplists:get_value(user, Post),
    Rfr = proplists:get_value(referer, Post),
    UA = proplists:get_value(browser, Post),
    Accept = proplists:get_value(accept, Post),

    S = case Body of
            {upload, F} -> F;
            _-> case io_lib:printable_list(btol(Body)) of
                   true  -> btol(Body);
                    false -> ""
                end
        end,
    {ok, Email} = passport:uid_to_email(Uid),
    Format = "~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p~n",
    io_lib:format(Format, [Date, Site, Path, S, atol(Mthd),
                           Peer, Email, Uid, Rfr, UA, Accept]).


-spec replay(string(), string()) -> ok.
%% @doc alias for replay(Name, LogSite, NewSite, deep)
replay(Name, NewSite) ->
    replay(Name, NewSite, default_filter()).

%% @spec replay(Name, Url, Options) -> ok
%% @doc Name is the name of the log file to read from (must be 
%% stored in /lib/hypernumbers-1.0/log/), Old is the site to copy
%% all posts from, New is the new location to post them too. Deep
%% decides whether to copy subpages or not
replay(Name, Url, Options) ->
    Ref = hn_util:parse_url(Url), 
    F   = fun(Post, Id) ->
                  print(post, Post, Id),
                  repost(Name, Post, Ref)
          end,    
    run_log(Name, F, make_filter(Options)),
    io:format("~nReplay finished....~n"),
    ok.

transform_date([], Acc) ->
    Acc;
transform_date([{date, all} | T], Acc) ->
    transform_date(T, [{date, all} | Acc]);
transform_date([{until, Date} | T], Acc) ->
    transform_date(T, [{date, {dh_date:nparse("01/01/1900"),
                               dh_date:nparse(Date)}} | Acc]);
transform_date([{since, Date} | T], Acc) ->
    transform_date(T, [{date, {dh_date:nparse(Date),
                               now()}} | Acc]);
transform_date([{between, Date1, Date2} | T], Acc) ->
    transform_date(T, [{date, {dh_date:nparse(Date1),
                               dh_date:nparse(Date2)}} | Acc]);
transform_date([{date, Date} | T], Acc) ->
    %% backwards compatible with date, just replaces with since
    transform_date([{since, Date} | T], Acc);
transform_date([H | T], Acc) ->
    transform_date(T, [H|Acc]).
    
make_filter(List) ->
    reduce(default_filter(), transform_date(List, [])).

reduce(Defaults, Users) ->
    reduce(Defaults, Users, []).

reduce([], _User, Acc) ->
    Acc;
reduce([{Key, Val} | T], User, Acc) ->
    case proplists:get_value(Key, User, undefined) of
        undefined -> reduce(T, User, [{Key, Val}    | Acc]);
        NewVal    -> reduce(T, User, [{Key, NewVal} | Acc])
    end.

default_filter() ->
    [{method, post}, {date, all}, {id, all}, {deep, true}, {path, "/"},
     {user, all}, {pause, 0}, {body, all}].

-spec info(string(), any()) -> ok.
%% @doc Dumps the logfile with Name to the shell
info(Name, Id) ->
    F = fun(Post, NId) -> print(long, Post, NId) end,
    run_log(Name, F, make_filter([{id, Id}, {method, all}])).

%% @doc Dumps the marks in logfile to the shell
browse_marks(Name) ->
    F = fun(Post, Id) -> print(long, Post, Id) end,
    run_log(Name, F, make_filter([{body, mark}])).
    
%% @doc Dumps the logfile with Name to the shell
browse(Name) ->
    browse(Name, default_filter()).

browse(Name, Filter) ->
    F = fun(Post, Id) -> print(short, Post, Id) end,
    run_log(Name, F, make_filter(Filter)).

bodystr(undefined) ->
    "undefined";
bodystr({upload, File}) ->
    "UPLOAD "++File;
bodystr(Body) ->
    io_lib:format("~p",[Body]).
%binary_to_list(Body).

print(short, Post, Id) ->
    Time       = proplists:get_value(time, Post),
    FullPath   = proplists:get_value(path, Post),
    Body       = proplists:get_value(body, Post),
    Uid        = proplists:get_value(user, Post),
    Date       = dh_date:format("j/m/y, g:ia", Time),
    [Path | _] = string:tokens(FullPath, "?"),
    {ok, Email} = passport:uid_to_email(Uid),
    io:format("~6B ~-17s ~-16s ~-20s ~-35s~n",
              [Id, Date, Email, Path, bodystr(Body)]);

print(post, Post, Id) ->
    FullPath   = proplists:get_value(path, Post),
    Body       = proplists:get_value(body, Post),
    [Path | _] = string:tokens(FullPath, "?"),
    io:format("P: ~6B ~-26s ~-35s~n", [Id, Path, bodystr(Body)]);

print(long, Post, Id) ->
    Time    = proplists:get_value(time, Post),
    Site    = proplists:get_value(site, Post),
    Path    = proplists:get_value(path, Post),
    Method  = proplists:get_value(method, Post),
    Body    = proplists:get_value(body, Post),
    Uid     = proplists:get_value(user, Post),
    Peer    = proplists:get_value(peer, Post),
    Referer = proplists:get_value(referer, Post),
    Accept  = proplists:get_value(accept, Post),
    Browser = proplists:get_value(browser, Post),
    {ok, Email} = passport:uid_to_email(Uid),

    Msg = "~nId: ~p ~s Request on ~s~n"
        "Email:      ~s~n"
        "Url:        ~s~n"
        "User-Agent: ~s~n"
        "Referrer:   ~s~n"
        "Accept:     ~s~n"
        "Body:       ~s~n~n",
    
    io:format(Msg,[Id, Method, dh_date:format("m.d.y, g:ia", Time), 
                   Email, Peer, Site++Path, Browser, Referer, 
                   Accept, bodystr(Body)]).

filter([], _Post, _Id) ->
    true;

filter([{method, P} | T], Post, Id) ->
    case proplists:get_value(method, Post) of
        'GET' when P == get -> filter(T, Post, Id);
        'POST' when P == post -> filter(T, Post, Id);
        _X when P == all ->  filter(T, Post, Id);
        _ -> false
    end;

filter([{date, all} | T], Post, Id) ->
    filter(T, Post, Id);
filter([{date, {Start, End}} | T], Post, Id) ->
    case proplists:get_value(time, Post) of
        Time when Time >= Start, Time < End -> 
            filter(T, Post, Id);
        _ ->
            false
    end;

filter([{id, all} | T], Post, Id) ->
    filter(T, Post, Id);
filter([{id, {Start, End}} | T], Post, Id) when Id >= Start, Id =< End ->
    filter(T, Post, Id);
filter([{id, Id} | T], Post, Id) ->
    filter(T, Post, Id);
filter([{id, _Id} | _T], _Post, _NId) ->
    false;

filter([{user, all} | T], Post, Id) ->
    filter(T, Post, Id);
filter([{user, User} | T], Post, Id) ->
    case proplists:get_value(user, Post) of
        User -> filter(T, Post, Id);
        _ -> false
    end;

filter([{body, all} | T], Post, Id) ->
    filter(T, Post, Id);
filter([{body, mark} | T], Post, Id) ->
    case proplists:get_value(body, Post) of
        <<"{\"set\":{\"mark\":",_Rest/binary>> -> filter(T, Post, Id);
        _Other                                 -> false
    end;

filter([_H | T], Post, Id) ->
    filter(T, Post, Id).

run_log(Name, Fun, Filter) ->
    Log = logfile(Name),
    case filelib:is_file(Log++".siz") of 
        false ->
            {error, no_file};
        true ->
            Fun2 = 
                fun(Terms, N0) -> 
                        lists:foldl(fun(T, N) ->
                                            handle_term(Filter, T, N, Fun),
                                            N + 1
                                    end, N0, Terms)
                end,
            {ok, Cont} = wrap_log_reader:open(Log),
            {ok, End}  = walk(Fun2, Cont, 0),
            wrap_log_reader:close(End)
    end.

walk(F, Cont, N) ->
    case wrap_log_reader:chunk(Cont) of
        {error, E} ->
            throw(E);
        {NCont, eof} ->
            {ok, NCont};
        {NCont, Terms} ->
            N2 = F(Terms, N),
            walk(F, NCont, N2)
    end.

handle_term(Opts, Post, N, F) ->
    
    Path = string:tokens(?pget(path, Opts), "/"),
    Deep = ?pget(deep, Opts),
    
    [Raw | _ ] = string:tokens(proplists:get_value(path, Post), "?"),
    Path2 = string:tokens(Raw, "/"),

    case in_path(Path, Path2, Deep) andalso filter(Opts, Post, N) of
        true  ->
            R = F(Post, N),
            timer:sleep(?pget(pause, Opts));
        false ->
            R = ok
    end,
    R.

in_path(Path, Path, false) ->
    true;
in_path(_Path1, _Path2, false) ->
    false;
in_path([], _Path, true) ->
    true;
in_path(Path1, Path2, true) ->
    startswith(Path2, Path1).

upload_file(Url, Path, Field) ->
    upload_file(Url, Path, filename:basename(Path), Field).

upload_file(Url, Path, Name, Field) ->
    
    Boundary   = "frontier",
    {ok, File} = file:read_file(Path),
    
    Data = ["--"++Boundary,
            "Content-disposition: form-data;name="++Field++"; filename="++Name,
            "Content-type: application/octet-stream"
            "Content-transfer-encoding: base64",
            "",
            binary_to_list(File),
            "--"++Boundary++"--"],
    
    Post = string:join(Data, "\r\n") ++ "\r\n",
    Type = "multipart/form-data; boundary="++Boundary,
    
    httpc:request(post,{Url, [], Type, Post}, [], []).


repost(Logname, Post, New) ->
    Method = proplists:get_value(method, Post),
    Body = proplists:get_value(body, Post),
    repost_(Method, Body, Logname, Post, New).

repost_('POST', {upload, Name}, LogName, Post, New) ->
    [Dir, _Nm] = string:tokens(LogName, "/"),
    Url  = New#refX.site ++ proplists:get_value(path, Post),
    Root = code:lib_dir(hypernumbers),
    Path = filename:join([Root, "log", Dir, "uploads", Name]),
    case filelib:is_file(Path) of
        false ->
            io:format("WARNING! file does not exist locally:~n ~p", [Path]);
        true ->
            upload_file(Url, Path, "Filedata")
    end,
    ok;
repost_('POST', _Body, _Name, Post, New) ->
    Body = rewrite_command(New#refX.site, proplists:get_value(body, Post)),
    Url  = New#refX.site ++ proplists:get_value(path, Post),
    Headers = [{"Accept", "application/json"}],
    httpc:request(post,{Url, Headers, "application/json", Body}, [], []),
    ok;
repost_(_, _Body, _Name, _Post, _New) ->
    ok.

rewrite_command(NewSite, Body) ->
    {ok, Json} = hn_mochi:get_json_post(Body),
    Json2 = rewrite_command0(Json, NewSite),
    iolist_to_binary(
      (mochijson:encoder([{input_encoding, utf8}])) ({struct, Json2})).

rewrite_command0([{"copy", {struct, [{"src", OldUrl}]}}], NewSite) ->
    [{"copy", {struct, [{"src", rewrite_url(OldUrl, NewSite)}]}}];
rewrite_command0(Json, _NewSite) ->
    %%io:format("~p ~p~n", [Json, NewSite]),
    Json.

rewrite_url(OldUrl, NewSite) ->
    [_Proto, _OldSite | Rest] = string:tokens(OldUrl, "/"),
    string:join([NewSite] ++ Rest, "/").

  
startswith(_List1, []) ->    
    true;
startswith([], _List2) ->    
    false;
startswith([Head | R1], [Head | R2]) ->
    startswith(R1, R2);
startswith(_List1, _List2) ->
    false.

logfile() ->
    logfile(?NAME).
logfile(Name) ->
    lists:concat([code:lib_dir(hypernumbers),
                  "/../../var/mochilog/",Name,".LOG"]).

atol(X) ->
    atom_to_list(X).

btol(X) when is_atom(X) ->
    X;
btol(X) ->
    binary_to_list(X).

