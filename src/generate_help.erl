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
-include("french_fns.hrl").
-include("german_fns.hrl").
-include("italian_fns.hrl").
-include("portuguese_fns.hrl").
-include("russian_fns.hrl").
-include("spanish_fns.hrl").

run() ->
    % first do the English Fns
    Json = [json_util:jsonify(X) || X <- ?WORKING_FNS],
    Json2 = mochijson:encode({array, Json}),
    write(Json2, "en_gb"),
    % now do Johnny Foreigner
    do_jf("fr", "french",     ?french_fns),
    do_jf("de", "german",     ?german_fns),
    do_jf("it", "italian",    ?italian_fns),
    do_jf("pt", "portuguese", ?portuguese_fns),
    % do_jf("ru", "russian",    ?russian_fns),
    do_jf("es", "spanish",    ?spanish_fns).

do_jf(Code, Lang, Fns) ->
    io:format("In do_jf for ~p~n", [Lang]),
    Fun = fun(#help{name = N} = Help) ->
                  {value, {_, _, NewN}} = lists:keysearch(N, 2, Fns),
                  NewHelp = Help#help{name = NewN, warning = "", text = ""},
                  json_util:jsonify(NewHelp)
          end,
    Json = [Fun(X) || X <- ?WORKING_FNS],
    Json2 = mochijson:encode({array, Json}),
    write(Json2, Code).

write(Str, Lang) ->
        File = case os:type() of
               {win32,nt} -> "c:\\opt\\code\\trunk\\lib\\hypernumbers-1.0"++
                                 "\\priv\\docroot\\hypernumbers\\fns_" ++
                                 Lang ++ ".json";
               _          -> "../lib/hypernumbers-1.0/priv/docroot/hypernumbers/"++
                                 "fns_" ++ Lang ++ ".json"
           end,
    
    _Return=filelib:ensure_dir(File),
    case file:open(File, [write]) of
	{ok, Id}  -> io:fwrite(Id, "~s~n", [Str]),
                 file:close(Id);
        Error -> io:format("In generate_help: can't open file ~p with ~p~n",
                           [File, Error]),
                 error
    end.
