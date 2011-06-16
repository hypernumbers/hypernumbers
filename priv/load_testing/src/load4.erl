-module(load4).

-type cellidx() :: pos_integer().

-record(refX,
        {
          site        = [],
          type,
          path        = [],
          obj         = null
         }).

-record(local_obj,
        {
          idx,
          type,
          path,
          obj,
          revidx
         }).

-record(item, {idx, attrs}).

-record(relation,
        {
          cellidx                    :: cellidx(),
          children = ordsets:new()   :: ordsets:ordset(cellidx()),
          parents = ordsets:new()    :: ordsets:ordset(cellidx()),
          infparents = ordsets:new() :: ordsets:ordset(cellidx()),
          z_parents = ordsets:new()  :: ordsets:ordset(#refX{}),
          include = false
       }).

-define(cycles, 100).
-define(loading, 1000).
-define(site, "http://hypernumbers.dev:9000").
-define(log, "load4.log").

-export([
         load/1
        ]).

load(Type) -> load2(Type, ?cycles).

load2(_Type, 0) -> ok;
load2(Type, N) ->
    io:format("in cycle ~p loading ~p~n", [N, Type]),
    Timestamp = integer_to_list(util2:get_timestamp()),
    syslib:log(atom_to_list(Type) ++ ", " ++ Timestamp, ?log),
    add_load(Type),
    test(),
    load2(Type, N - 1).

add_load(Type) -> add2(Type, ?loading).

add2(_Type, 0)     ->
    ok;
add2(local_obj, N) ->
    Timestamp = integer_to_list(util2:get_timestamp()),
    Path =
    Rec = #local_obj{idx = "lo" ++ Timestamp, revidx = "rev" ++ Timestamp},
    write(local_obj, Rec),
    add2(local_obj, N - 1);
add2(item, N) ->
    Timestamp = integer_to_list(util2:get_timestamp()),
    Rec = #item{idx = "it" ++ Timestamp},
    write(item, Rec),
    add2(item, N - 1);
add2(relation, N) ->
    Timestamp = integer_to_list(util2:get_timestamp()),
    Rec = #relation{cellidx = "rel" ++ Timestamp},
    write(relation, Rec),
    add2(relation, N - 1).

test() -> ok.

write(Table, Record) ->
    Table2 = new_db_wu:trans(?site, Table),
    Fun = fun() ->
                  mnesia:write(Table2, Record, write)
          end,
    mnesia:activity(transaction, Fun).
