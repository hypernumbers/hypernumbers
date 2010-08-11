%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This module really shouldn't be here - it is a scratch
%%%            debugging module (hence the name!).
%%% @private
-module(bits).
-export([
         init_memory_trace/0,
         mem_csv/0,
         basic_views/0,
        log/1]).                

basic_views() ->
    auth_srv2:add_view("http://localhost:9000", [], [everyone], "_g/core/spreadsheet"),
    auth_srv2:set_champion("http://localhost:9000", [], "_g/core/spreadsheet"),
    auth_srv2:add_view("http://localhost:9000", ["[**]"], [everyone], "_g/core/spreadsheet"),
    auth_srv2:set_champion("http://localhost:9000", ["[**]"], "_g/core/spreadsheet").    

init_memory_trace() ->
    File= case os:type() of
	      {win32,nt} -> exit('erk, not on windows...');
	      _          -> "../var/logs/memory.csv"
          end,
    io:format("File is ~p~n", [File]),
    ok = filelib:ensure_dir(File),
    % so shit its not true :(
    ok = file:delete(File),
    ok = filelib:ensure_dir(File),
    log(stringify([time, total, processes, processes_used, system,
                   atom, atom_used, binary, code, ets])).    

mem_csv() ->
    Mem = erlang:memory(),
    Vals = [V || {_K, V} <- Mem],
    io:format("Mem is ~p~nVals is ~p~n", [Mem, Vals]),
    Secs = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    File = "../logs/memory.csv",
    log(stringify([Secs | Vals]), File).

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

stringify(List) -> str1(List, []).

str1([], Acc) -> string:join(lists:reverse(Acc), ",");
str1([H | T], Acc) when is_atom(H) ->
    str1(T, ["\"" ++ atom_to_list(H) ++ "\"" | Acc]);
str1([H | T], Acc) when is_integer(H) ->
    str1(T, ["\"" ++ integer_to_list(H) ++ "\"" | Acc]).
