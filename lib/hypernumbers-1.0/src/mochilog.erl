%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc Module to replay logs from a live server to a dev
%%% environment for debugging
-module(mochilog).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).
-define(NAME, "post_log").

-record(post, {time, site, path, method, body, peer,
               user, referer, browser, accept}).

-export([log/4, start/0, stop/0, replay/2, replay/3, clear/0, repair/1,
         browse/1, browse/2, browse_marks/1, info/2 ]).

-export([stream_log/4]).

%% @spec start() -> ok
%% @doc This starts the log
start() ->
    Opts = [{name,?NAME}, {file,logfile()},
            {type,wrap},  {size, {2097152, 99}}],

    case disk_log:open(Opts) of
        {ok, _Log}          -> ok;
        {repaired, _, _, _} -> ok;
        _Else               -> throw({failed_open_log})
    end.

%% @spec log(term(), term(), term(), term()) -> ok
%% @doc Logs individual requests
log(Req, Ref, User, Body) ->

    Log = #post { 
      time    = erlang:now(),
      site    = Ref#refX.site, 
      path    = Req:get(raw_path),
      method  = Req:get(method),
      body    = Body,
      user    = User,
      peer    = Req:get_header_value("x-forwarded-for"),
      referer = Req:get_header_value("Referer"),
      browser = Req:get_header_value("User-Agent"),
      accept  = Req:get_header_value("Accept")
     },
    
    disk_log:alog(?NAME, Log).

%% @spec stop() -> ok
%% @doc Closes log
stop() ->
    disk_log:close(?NAME).

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
    
    Date = dh_date:format("r", Post#post.time),
    #post{ site=Site, path=Path, body=Body, method=Mthd, peer=Peer,
           user=Usr, referer=Rfr, browser=UA, accept=Accept} = Post,
    
    S = case Body of
            {upload, F} -> F;
            _-> case io_lib:printable_list(btol(Body)) of
                    true  -> btol(Body);
                    false -> ""
                end
        end,
    
    Format = "~p,~p,~p,~p,~p,~p,~p,~p,~p~p~n",
    io_lib:format(Format, [Date, Site, Path, S, atol(Mthd),
                           Peer, Usr, Rfr, UA, Accept]).


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
    binary_to_list(Body).

print(short, Post, Id) ->
    #post{time=Time, path=FullPath, body=Body, user=User} = Post,
    Date = dh_date:format("j/m/y, g:ia", Time),
    [Path | _] = string:tokens(FullPath, "?"),
    io:format("~6B  ~-18s  ~-10s  ~-20s  ~-35s~n",
              [Id, Date, User, Path, bodystr(Body)]);

print(post, Post, Id) ->
    #post{path=FullPath, body=Body} = Post,
    [Path | _] = string:tokens(FullPath, "?"),
    io:format("P: ~6B  ~-26s  ~-35s~n", [Id, Path, bodystr(Body)]);

print(long, Post, Id) ->
    
    #post { 
      time = Time,
      site = Site, 
      path = Path,
      method = Method,
      body = Body,
      user = User,
      peer = Peer,
      referer = Referer,
      browser = Browser
     } = Post,
    
    Msg = "~nId: ~p ~s Request on ~s~n"
        "User:       ~s(~s)~nUrl:        ~s~n"
        "User-Agent: ~s~nReferrer:   ~s~n"
        "Body:       ~s~n~n",
    
    io:format(Msg,[Id, Method, dh_date:format("m.d.y, g:ia", Time), User,
                   Peer, Site++Path, Browser, Referer, bodystr(Body)]).

filter([], _Post, _Id) ->
    true;

filter([{method, post} | _T], Post, _Id) when Post#post.method == 'GET' ->
    false;
filter([{method, _} | T], Post, Id) ->
    filter(T, Post, Id);

filter([{date, all} | T], Post, Id) ->
    filter(T, Post, Id);
filter([{date, {Start, End}} | T], #post{time=Time}=Post, Id)
  when Time >= Start, Time < End ->
    filter(T, Post, Id);
filter([{date, _Date} | _T], _Post, _Id) ->
    false;

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
filter([{user, User} | T], #post{user=User} = Post, Id) ->
    filter(T, Post, Id);
filter([{user, _User} | _T], #post{user=_NUser}, _NId) ->
    false;

filter([{body, all} | T], Post, Id) ->
    filter(T, Post, Id);
filter([{body, mark} | T], #post{body = Body} = Post, Id) ->
    case Body of
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
        {NCont, eof} ->
            {ok, NCont};
        {NCont, Terms} ->
            N2 = F(Terms, N),
            walk(F, NCont, N2)
    end.

handle_term(Opts, Post, N, F) ->
    
    Path = string:tokens(?pget(path, Opts), "/"),
    Deep = ?pget(deep, Opts),
    
    [Raw | _ ] = string:tokens(Post#post.path, "?"),
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
    
    Boundary          = "frontier",
    [_Usr, _Dt, Name] = re:split(filename:basename(Path),"__",[{return,list}]),
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
    
    http:request(post,{Url, [], Type, Post}, [], []).


repost(LogName, #post{method='POST', body={upload, Name}} = Post, New) ->
    [Dir, _Nm] = string:tokens(LogName, "/"),
    Url  = New#refX.site ++ Post#post.path,
    Root = code:lib_dir(hypernumbers),
    Path = filename:join([Root, "log", Dir, "uploads", Name]),
    case filelib:is_file(Path) of
        false ->
            io:format("WARNING! file does not exist locally:~n ~p", [Path]);
        true ->
            upload_file(Url, Path, "Filedata")
    end,
    ok;

repost(_Name, Post, New) when Post#post.method == 'POST' ->
    Body = rewrite_command(New#refX.site, Post#post.body),
    Url  = New#refX.site ++ Post#post.path,
    http:request(post,{Url, [], "application/json", Body}, [], []),
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
    lists:concat([code:lib_dir(hypernumbers),"/log/",Name,".LOG"]).

atol(X) ->
    atom_to_list(X).

btol(X) when is_atom(X) ->
    X;
btol(X) ->
    binary_to_list(X).

