%%% @author    Gordon Guthrie 
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       functions for mnesia administration
%%%
%%% @end
%%% Created : 13 Dec 2009 by gordon@hypernumbers.com

-module(hn_db_admin).

-define(CACHEABLE_TABLES, [
                           "dirty_queue",
                           "dirty_zinf",
                           "form",
                           "item",
                           "group",
                           "local_objs",
                           "relation",
                           "styles"
                          ]).

-export([
         create_table/7,
         into_mem/1,
         outof_mem/1,
         backup/3,
         restore/2,
         dump_site_table/2,
         dump_core_table/2
        ]).

-spec dump_site_table(string(), string()) -> ok.
%% just dumps a table to the shell
dump_site_table(Site, Table) ->
    Record = list_to_atom(Table),
    Table2 = hn_db_wu:trans(Site, Record),
    dump2(Table2, Record).

-spec dump_core_table(string(), string()) -> ok.
%% just dumps a table to the shell
dump_core_table(Prefix, Record) ->
    Table = list_to_atom(Prefix ++ Record),
    dump2(Table, list_to_atom(Record)).

dump2(Table, Record) ->
    N = ms_util2:no_of_fields(Record),
    Spec =  list_to_tuple([Record| lists:duplicate(N, '_')]),
    Fun = fun() ->
                  Ret = mnesia:match_object(Table, Spec, write),
                  io:format("Dumping ~p~n~n~p~n", [Table, Ret]),
                  ok
          end,
    mnesia:activity(async_dirty, Fun),
    ok.

-spec restore(list(), list()) -> ok.
restore(Dir, Name) ->
    {atomic, _Tables} = mnesia:restore(Dir ++ Name,
                                     [{default_op, recreate_tables}]),
    ok.

-spec backup(list(), list(), list()) -> ok.
backup(Tables, Dir, Name) ->
    ChP = {name, Name},
    ChPDef = {min, Tables},

    % start by checkpointing
    {ok, ChPName, _} = mnesia:activate_checkpoint([ChP, ChPDef]),

    % do the backup
    ok = mnesia:backup_checkpoint(Name, filename:join(Dir, Name), []),

    % delete the checkpoint    
    ok = mnesia:deactivate_checkpoint(ChPName).    

-spec create_table(atom(), atom(),
                   any(),
                   disc_only_copies | disc_copies | ram_copies,
                   set | bag | ordered_set,
                   true | false,
                   [atom()]) -> ok. 
create_table(TblName, Rec, Fields, Storage, Type, Local, Indicies) ->
    R = mnesia:create_table(TblName, [{record_name, Rec},
                                      {attributes, Fields},
                                      {Storage, [node()]},
                                      {type, Type},
                                      {local_content, Local},
                                      {index, Indicies}]),
    case R of 
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> ok;
        {aborted, Reason}              -> throw(Reason)
    end.

-spec into_mem(list()) -> ok.
into_mem("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    [{atomic, ok} = chg_copy_type(Site, Port, X, disc_copies)
     || X <- ?CACHEABLE_TABLES],
    ok.

-spec outof_mem(list()) -> ok.
outof_mem("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    [{atomic, ok} = chg_copy_type(Site, Port, X, disc_only_copies)
     || X <- ?CACHEABLE_TABLES],
    ok.

chg_copy_type(Site, Port, Table, Mode) ->
    ActualTable = list_to_existing_atom(Site ++ "&"
                                        ++ Port ++ "&"
                                        ++ Table),
    mnesia:change_table_copy_type(ActualTable, node(), Mode).



