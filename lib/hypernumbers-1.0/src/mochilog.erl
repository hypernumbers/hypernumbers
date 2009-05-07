%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc Module to replay logs from a live server to a dev
%%% environment for debugging
-module(mochilog).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(NAME, "post_log").
-record(post, {time, site, path, method, body, peer, user, referer, browser}).

-export([log/4, start/0, stop/0, replay/3, replay/4, clear/0, dump/1,
        generate_mi/2 ]).

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
replay(Name, LogPath, NewSite) ->
    replay(Name, LogPath, NewSite, deep).

%% @spec replay(Name, Old, New, Deep) -> ok
%% @doc Name is the name of the log file to read from (must be 
%% stored in /lib/hypernumbers-1.0/log/), Old is the site to copy
%% all posts from, New is the new location to post them too. Deep
%% decides whether to copy subpages or not
replay(Name, Old, New, Deep) ->
    NRef = hn_mochi:parse_ref(New), 
    OPath = string:tokens(Old,"/"),
    F    = fun([Post]) -> repost(Post, OPath, NRef, Deep) end,    
    run_log(Name, F).

%% @spec generate_mi(Name, Path) -> ok
%% @doc Name is the name of the log file to read from (must be 
%% stored in /lib/hypernumbers-1.0/log/), Path is the path of 
%% the file to write mi to
generate_mi(Name, Path) ->
    {ok, File} = file:open(Path, [write]),
    io:format(File, "Date,Site,Path,Body,Method,IP,User,Referer,User-Agent~n",[]),
    run_log(Name, fun([Post]) -> do_mi(File, Post) end),
    file:close(File).

do_mi(File, Post) ->
    Date = dh_date:format("r", Post#post.time),
    #post{ site=Site, path=Path, body=Body, method=Mthd, peer=Peer, user=Usr,
           referer=Rfr, browser=UA} = Post,
    Str  = "~p,~p,~p,~p,~p,~p,~p,~p,~p~n",
    io:format(File, Str, [Date, Site, Path, btol(Body), atol(Mthd), Peer, Usr, Rfr, UA]).

atol(X) ->
    atom_to_list(X).

btol(X) when is_atom(X) ->
    X;
btol(X) ->
    binary_to_list(X).

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

is_in_path(shallow, LogItem, Path) ->
    LogItem#refX.path == Path;
is_in_path(deep, LogItem, Path) ->
    startswith(LogItem#refX.path, Path).
    
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
    case filelib:is_file(Log++".siz") of 
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
