%%% Platform-independent functions to work with the filesystem.
%%% <hasan@hypernumbers.com>
%%% <dale@hypernumbers.com>

-module(fileutil).
-export([cp/2,
         ls/1,
         mv/2,
         rm/1]).

-import(filename, [nativename/1]).

-define(OS,
        case os:type() of
            {unix, _}  -> unix;
            {win32, _} -> windows
        end).


%%%------------------%%%
%%% Public functions %%%
%%%------------------%%%

cp(Src, Dst) ->
    cp(Src, Dst, ?OS).


ls(Src) ->
    ls(Src, ?OS).


mv(Src, Dst) ->
    mv(Src, Dst, ?OS).


rm(Src) ->
    rm(Src, ?OS).


%%%-------------------%%%
%%% Private functions %%%
%%%-------------------%%%

cp(From, To, unix) ->
    os:cmd("cp " ++ From ++ " " ++ To);

cp(From, To, windows) ->
    os:cmd("copy " ++ nativename(From) ++ " " ++ nativename(To)).


ls(Src, unix) ->
    string:tokens(os:cmd("ls " ++ Src), "\n");

ls(Src, windows) ->
    string:tokens(os:cmd("dir " ++ nativename(Src)), "\r\n"). % UNTESTED!


mv(From, To, unix) ->
    os:cmd("mv " ++ From ++ " " ++ To);

mv(From, To, windows) ->
    os:cmd("move " ++ nativename(From) ++ " " ++ nativename(To)).


rm(File, unix) ->
    os:cmd("rm " ++ File);

rm(File, windows) ->
    os:cmd("del " ++ nativename(File)).
