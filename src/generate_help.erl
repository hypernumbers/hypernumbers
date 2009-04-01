%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc        Generates the help file
%%%
%%% @end
%%% Created :  1 Apr 2009 by <gordon@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(generate_help).

-export([run/0]).

-include("working_fns.hrl").

run() ->
    Json = [json_util:jsonify(X) || X <- ?WORKING_FNS],
    Json2 = mochijson:encode({array, Json}),
    
    File = case os:type() of
               {win32,nt} -> "c:\\opt\\code\\trunk\\lib\\hypernumbers-1.0"++
                                 "\\priv\\docroot\\hypernumbers\\functions.json";
               _          -> "../lib/hypernumbers-1.0/priv/docroot/hypernumbers/"++
                                 "functions.json"
           end,
    
    _Return=filelib:ensure_dir(File),
    case file:open(File, [write]) of
	{ok, Id}  -> io:fwrite(Id, "~s~n", [Json2]),
                 file:close(Id);
        Error -> io:format("In generate_help: can't open file ~p with ~p~n",
                           [File, Error]),
                 error
    end.
