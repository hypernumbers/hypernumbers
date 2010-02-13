%%% @author    Gordon Guthrie 
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       functions for mnesia administration
%%%
%%% @end
%%% Created : 13 Dec 2009 by gordon@hypernumbers.com

-module(hn_db_admin).

-define(CACHEABLE_TABLES, [
                           "dirty_inc_hn_create",
                           "dirty_notify_back_in",
                           "dirty_notify_out",
                           "dirty_notify_back_out",
                           "item",
                           "local_objs",
                           "local_cell_link",
                           "relation",
                           "hn_user",
                           "remote_objs",
                           "remote_cell_link",
                           "incoming_hn",
                           "outgoing_hn",
                           "styles",
                           "style_counters",
                           "page_vsn",
                           "page_history"
                          ],


-export([
         create_table/6,
         into_mem/1,
         outof_mem/1,
         backup/3,
         restore/2
        ]).

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
    ok = mnesia:backup_checkpoint(Name, Dir ++ Name, []),

    % delete the checkpoint    
    ok = mnesia:deactivate_checkpoint(ChPName).    

-spec create_table(atom(), atom(),
                   any(),
                   disc_only_copies | disc_copies | ram_copies,
                   set | bag | ordered_set,
                   [atom()]) -> ok. 
create_table(TblName, Rec, Fields, Storage, Type, Indicies) ->
    R = mnesia:create_table(TblName, [{record_name, Rec},
                                      {attributes, Fields},
                                      {Storage, [node()]},
                                      {type, Type},
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

