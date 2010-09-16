%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This module really shouldn't be here - it is a scratch
%%%            debugging module (hence the name!).
%%% @private
-module(log).

-export([
         log/1
        ]).                

log(String) ->
    log(String, "../logs/dump.txt").

log(String, File) ->
    _Return=filelib:ensure_dir(File),
    
    case file:open(File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [String]),
	    file:close(Id);
	_ ->
	    error
    end.

