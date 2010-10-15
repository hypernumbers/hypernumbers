
%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This module really shouldn't be here - it is a scratch
%%%            debugging module (hence the name!).
%%% @private
-module(bits).

-export([
         expand_zone/0,
         delete_sites/1,
         dump_sites/0,
         init_memory_trace/0,
         mem_csv/0,
         basic_views/0,
         log/1,
         load_inline/0
        ]).                


-type now() :: {integer(),integer(),integer()}.
-type cellidx() :: pos_integer().
-type generator() :: fun(() -> string()). 
-type resource_addr() :: {string(), integer(), atom()}. %% (IP, Port, Node).

-record(zone, { label :: string(),
                min_size :: integer(),
                ideal_size :: integer(),
                pool :: gb_tree(),
                generator :: generator(),
                zone_id :: integer() }).

-record(refX,
        {
          site        = [],
          path        = [],
          obj         = null
         }).

expand_zone() ->

    Fun = fun() ->
                  [Zone] = mnesia:read(service_hns_zone, "tiny.hn"),
                  %io:format("Zone is ~p ~p~n",[Zone#zone.min_size, Zone#zone.ideal_size]),
                  NewZone = Zone#zone{min_size=250, ideal_size=500},
                  &io:format("NewZone is ~p~n",[NewZone]),
                  mnesia:write(service_hns_zone, NewZone, write)
          end,
    mnesia:activity(transaction, Fun).

delete_sites(File) ->
    {ok, Dev} = file:open(File, read),
    del_sites(Dev).

del_sites(Dev) ->
    case file:read_line(Dev) of
        {ok, Site} -> Site2 = string:strip(Site, right, 10),
                      ok = hn_setup:delete_site(Site2),
                      Msg = dh_date:format("d:M:Y (
D) h:m:s") ++
                          " - deleting " ++ Site2, 
                      Log = "../var/logs/sitedeletion.log",
                      log(Msg, Log),
                      del_sites(Dev);
        eof        -> ok
    end.

dump_sites() ->
    Fun = fun() -> mnesia:all_keys(core_site) end,
    List = mnesia:activity(transaction, Fun),
    [log(X, "../var/logs/coresite.dump") || X <- List].

load_inline() ->
    RefX1 = #refX{site="http://hypernumbers.dev:9000",
                 path = [],
                 obj = {cell, {1, 1}}},
    RefX2 = #refX{site="http://hypernumbers.dev:9000",
                 path = [],
                 obj = {cell, {1, 2}}},
    hn_db_api:write_attributes([{RefX1, [{"inline", "input"}]}]),
    hn_db_api:write_attributes([{RefX2, [{"inline", "input"}]}]).

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
