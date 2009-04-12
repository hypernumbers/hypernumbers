%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc Module to replay logs from a live server to a dev
%%% environment for debugging
-module(mochilog).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(NAME, "post_log").
-record(post, {site, path, method, body, peer, user, referer, browser}).

-export([log/4, start/0, stop/0, replay/3, replay/4, clear/0, dump/1 ]).

%% @spec start() -> ok
%% @doc This starts the log
start() ->
    {ok, _Ret} = disk_log:open([{name, ?NAME}, {file, logfile()}]),
    ok.

%% @spec log(term(), term(), term(), term()) -> ok
%% @doc Logs individual requests
log(Req, Ref, User, Body) ->

    Log = #post { 
      site = Ref#refX.site, 
      path = Req:get(raw_path),
      method = Req:get(method),
      body = Body,
      user = User,
      peer = Req:get(peer),
      referer = Req:get_header_value("Referer"),
      browser = Req:get_header_value("User-Agent")
     },
    
    disk_log:alog(?NAME, Log).

%% @spec close() -> ok
%% @doc Closes log
stop() ->
    disk_log:close(?NAME).

%% @spec clear() -> ok
%% @doc Deletes current log
clear() ->
    stop(),
    file:delete(logfile()).

%% @spec replay(Name, LogSite, NewSite) -> ok
%% @doc alias for replay(Name, LogSite, NewSite, deep)
replay(Name, LogSite, NewSite) ->
    replay(Name, LogSite, NewSite, deep).

%% @spec replay(Name, Old, New, Deep) -> ok
%% @doc Name is the name of the log file to read from (must be 
%% stored in /lib/hypernumbers-1.0/log/), Old is the site to copy
%% all posts from, New is the new location to post them too. Deep
%% decides whether to copy subpages or not
replay(Name, Old, New, Deep) ->
    NRef = hn_mochi:parse_ref(New), 
    ORef = hn_mochi:parse_ref(Old), 
    F    = fun([Post]) -> repost(Post, ORef, NRef, Deep) end,    
    run_log(Name, F).

%% @spec dump(Name) -> ok
%% @doc Dumps the logfile with Name to the shell
dump(Name) ->
    run_log(Name, fun([Post]) -> ?INFO("~p",[Post]) end). 

repost(Post, Old, New, Deep) when Post#post.method == 'POST'->
    [Raw | _ ] = string:tokens(Post#post.path, "?"),
    P = hn_mochi:parse_ref(Post#post.site ++ Raw),
    case is_in_path(Deep, P, Old) of
        false -> ok;
        true  ->
            Url  = New#refX.site ++ Post#post.path,
            Type = "application/json", 
            Data = Post#post.body,
            http:request(post,{Url, [], Type, Data}, [], []),
            ok
    end;

repost(_, _, _, _) ->
    ok.

is_in_path(shallow, LogItem, Old) ->
    LogItem#refX.site == Old#refX.site andalso
        LogItem#refX.path == Old#refX.path;

is_in_path(deep, LogItem, Old) ->
    LogItem#refX.site == Old#refX.site andalso
        startswith(LogItem#refX.path, Old#refX.path).
    
startswith(_List1, []) ->    
    true;
startswith([], _List2) ->    
    false;
startswith([Head | R1], [Head | R2]) ->
    startswith(R1, R2);
startswith(_List1, _List2) ->
    false.

run_log(Name, Fun) ->
    
    Log = logfile(Name),
    
    case filelib:is_file(Log) of 
        false -> {error, no_file};
        true  ->
            disk_log:open([{name, Name}, {file, Log}]),
            walk(Name, Fun, start),
            disk_log:close(Name)
    end.
    

walk(Name, F, Cont) ->
    case disk_log:chunk(Name, Cont, 1) of
        eof           -> ok;
        {NCont, []}   -> walk(Name, F, NCont);
        {NCont, Data} -> 
            F(Data), 
            walk(Name, F, NCont)
    end.

logfile() ->
    logfile(?NAME).
logfile(Name) ->
    lists:concat([code:lib_dir(hypernumbers),"/log/",Name,".LOG"]).
