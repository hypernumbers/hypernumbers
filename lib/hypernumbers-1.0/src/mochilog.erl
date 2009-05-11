%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc Module to replay logs from a live server to a dev
%%% environment for debugging
-module(mochilog).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).
-define(NAME, "post_log").

-record(post, {time, site, path, method, body, peer, user, referer, browser}).

-export([log/4, start/0, stop/0, replay/2, replay/3, clear/0,
         browse/1, browse/2, info/2 ]).

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
      time = erlang:now(),
      site = Ref#refX.site, 
      path = Req:get(raw_path),
      method = Req:get(method),
      body = Body,
      user = User,
      peer = Req:get_header_value("x-forwarded-for"),
      referer = Req:get_header_value("Referer"),
      browser = Req:get_header_value("User-Agent")
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

%% @spec replay(Name, LogPath, NewSite) -> ok
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
                  repost(Post, Ref)
          end,    
    run_log(Name, F, make_filter(Options)),
    io:format("~nReplay finished....~n"),
    ok.

%% @spec generate_mi(Name, Path) -> ok
%% @doc Name is the name of the log file to read from (must be 
%% stored in /lib/hypernumbers-1.0/log/), Path is the path of 
%% the file to write mi to
%% generate_mi(Name, Path) ->
%%     {ok, File} = file:open(Path, [write]),
%%     io:format(File, "Date,Site,Path,Body,Method,IP,User,Referer,User-Agent~n",[]),
%%     run_log(Name, fun([Post]) -> do_mi(File, Post) end, default_filter()),
%%     file:close(File).

%% do_mi(File, Post) ->
%%     Date = dh_date:format("r", Post#post.time),
%%     #post{ site=Site, path=Path, body=Body, method=Mthd, peer=Peer, user=Usr,
%%            referer=Rfr, browser=UA} = Post,
%%     Str  = "~p,~p,~p,~p,~p,~p,~p,~p,~p~n",
%%     io:format(File, Str, [Date, Site, Path, btol(Body), atol(Mthd), Peer, Usr, Rfr, UA]).
transform_date([], Acc) ->
    Acc;
transform_date([{date, all} | T], Acc) ->
    transform_date(T, [{date, all} | Acc]);
transform_date([{date, Date} | T], Acc) ->
    transform_date(T, [{date, fuzzy_date:nrange(Date)} | Acc]);
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
     {user, all}].

%% @spec dump(Name) -> ok
%% @doc Dumps the logfile with Name to the shell
info(Name, Id) ->
    F = fun(Post, NId) -> print(long, Post, NId) end,
    run_log(Name, F, make_filter([{id, Id}, {method, all}])).

%% @doc Dumps the logfile with Name to the shell
browse(Name) ->
    browse(Name, default_filter()).

browse(Name, Filter) ->
    F = fun(Post, Id) -> print(short, Post, Id) end,
    run_log(Name, F, make_filter(Filter)).

bodystr(undefined) ->
    "undefined";
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
  when Time >= Start, Time =< End ->
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

filter([_H | T], Post, Id) ->
    filter(T, Post, Id).

run_log(Name, Fun, Filter) ->
    Log = logfile(Name),
    case filelib:is_file(Log++".siz") of 
        false ->
            {error, no_file};
        true ->
            {ok, Cont} = wrap_log_reader:open(Log),
            {ok, End}  = walk(Fun, Cont, 0, Filter),
            wrap_log_reader:close(End)
    end.

walk(F, Cont, N, Filter) ->
    case wrap_log_reader:chunk(Cont, 1) of
        {NCont, eof} ->
            {ok, NCont};
        {NCont, [Term]} ->
            handle_term(Filter, Term, N, F),
            walk(F, NCont, N+1, Filter)
    end.

handle_term(Opts, Post, N, F) ->
    
    Path = string:tokens(?pget(path, Opts), "/"),
    Deep = ?pget(deep, Opts),

    [Raw | _ ] = string:tokens(Post#post.path, "?"),
    Path2 = string:tokens(Raw, "/"),

    case in_path(Path, Path2, Deep) andalso filter(Opts, Post, N) of
        true  -> F(Post, N);
        false -> ok
    end.

in_path(Path, Path, false) ->
    true;
in_path(_Path1, _Path2, false) ->
    false;
in_path([], _Path, true) ->
    true;
in_path(Path1, Path2, true) ->
    startswith(Path2, Path1).

repost(Post, New) ->
    Url  = New#refX.site ++ Post#post.path,
    http:request(post,{Url, [], "application/json", Post#post.body}, [], []),
    ok.

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

%% atol(X) ->
%%     atom_to_list(X).

%% btol(X) when is_atom(X) ->
%%     X;
%% btol(X) ->
%%     binary_to_list(X).

