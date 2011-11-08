%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       functions for mnesia administration
%%%
%%% @end
%%% Created : 13 Dec 2009 by gordon@hypernumbers.com

-module(hn_db_admin).

-include("spriki.hrl").

-define(CACHEABLE_TABLES, [
                           "api",
                           "dirty_for_zinf",
                           "dirty_queue",
                           "dirty_zinf",
                           "form",
                           "group",
                           "include",
                           "item",
                           "local_obj",
                           "del_local",
                           "relation",
                           "style",
                           "timer",
                           "user_fns"
                          ]).

-export([
         force_recalc/0,
         force_recalc/1,
         create_table/7,
         mem_only/1,
         mem_only/2,
         disc_only/1,
         disc_only/2,
         disc_and_mem/1,
         disc_and_mem/2,
         backup/3,
         restore/2,
         copy_site/2
        ]).

force_recalc() ->
    Sites = hn_setup:get_sites(),
    [force_recalc(X) || X <- Sites].

force_recalc(Site) ->
    Fun1 = fun(X, []) ->
                   case X of
                       #relation{children = [], parents = [],
                                 infparents = []} ->
                           ok;
                       #relation{cellidx = Idx, children = []} ->
                           new_db_api:mark_idx_dirty(Site, Idx);
                       _X ->
                           ok
                   end,
                   []
           end,
    Fun2 = fun() ->
                   Tbl = new_db_wu:trans(Site, relation),
                   mnesia:foldl(Fun1, [], Tbl)
           end,
    mnesia:activity(transaction, Fun2),
    ok.

-spec copy_site(string(), string()) -> ok.
copy_site(FromSite, ToSite) ->
    Tables = hn_setup:tables(),
    [copy(FromSite, ToSite, X) || {X, _, _, _, _} <- Tables],
    ok.

copy(FromSite, ToSite, Table) ->
    io:format("Copying ~p from ~p to ~p~n", [Table, FromSite, ToSite]),
    FromTable = new_db_wu:trans(FromSite, Table),
    ToTable = new_db_wu:trans(ToSite, Table),
    {atomic, ok} = mnesia:clear_table(ToTable),
    Fun1 = fun() ->
                   Fun2 = fun(X, Acc) ->
                                  ok = mnesia:write(ToTable, X, write),
                                  Acc
                          end,
                   mnesia:foldl(Fun2, [], FromTable)
           end,
    [] = mnesia:activity(transaction, Fun1),
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

-spec disc_and_mem(list()) -> ok.
disc_and_mem("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    [{atomic, ok} = chg_copy_type(Site, Port, X, disc_copies)
     || X <- ?CACHEABLE_TABLES],
    ok.

-spec disc_and_mem(list(), list()) -> ok.
disc_and_mem("http://" ++ SiteAndPort, Table) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    {atomic, ok} = chg_copy_type(Site, Port, Table, disc_copies),
    ok.

-spec disc_only(list()) -> ok.
disc_only("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    [{atomic, ok} = chg_copy_type(Site, Port, X, disc_only_copies)
     || X <- ?CACHEABLE_TABLES],
    ok.

-spec disc_only(list(), list()) -> ok.
disc_only("http://" ++ SiteAndPort, Table) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    {atomic, ok} = chg_copy_type(Site, Port, Table, disc_only_copies),
    ok.

-spec mem_only(list()) -> ok.
mem_only("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    [{atomic, ok} = chg_copy_type(Site, Port, X, ram_copies)
     || X <- ?CACHEABLE_TABLES],
    ok.

-spec mem_only(list(), list()) -> ok.
mem_only("http://" ++ SiteAndPort, Table) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    {atomic, ok} = chg_copy_type(Site, Port, Table, ram_copies),
    ok.

chg_copy_type(Site, Port, Table, Mode) ->
    ActualTable = list_to_existing_atom(Site ++ "&"
                                        ++ Port ++ "&"
                                        ++ Table),
    mnesia:change_table_copy_type(ActualTable, node(), Mode).



