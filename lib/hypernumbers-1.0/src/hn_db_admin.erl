%%% @author    Gordon Guthrie
%%% @copyright (C) 2009-2014, Hypernumbers Ltd
%%% @doc       functions for mnesia administration
%%%
%%% @end
%%% Created : 13 Dec 2009 by gordon@hypernumbers.com

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(hn_db_admin).

-include("spriki.hrl").

-export([
         force_recalc/0,
         force_recalc/1,
         create_table/7,
         all_sites_disc_only/0,
         disc_only/1,
         disc_only/2,
         mem_only/1,
         mem_only/2,
         all_sites_disc_and_mem/0,
         disc_and_mem/1,
         disc_and_mem/2,
         backup/2,
         restore/2
        ]).

force_recalc() ->
    Sites = hn_setup:get_sites(),
    [force_recalc(X) || X <- Sites].

force_recalc(Site) ->
    Fun1 = fun(X, []) ->
                   case X of
                       #relation{parents = [], infparents = [],
                                z_parents = []} ->
                           ok;
                       #relation{cellidx = Idx} ->
                           ok = new_db_api:mark_idx_dirty(Site, Idx)
                   end,
                   []
           end,
    Fun2 = fun() ->
                   Tbl = new_db_wu:trans(Site, relation),
                   mnesia:foldl(Fun1, [], Tbl)
           end,
    mnesia:activity(transaction, Fun2),
    ok.

-spec restore(list(), list()) -> ok.
restore(Dir, Name) ->
    ok = mnesia:install_fallback(Dir ++ Name, []).

-spec backup(list(), list()) -> ok.
backup(Dir, Name) ->

    % make the directory exist
    _Return = filelib:ensure_dir(Dir ++ Name),

    % do the backup
    ok = mnesia:backup(filename:join(Dir, Name)).

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

-spec all_sites_disc_and_mem() -> ok.
all_sites_disc_and_mem() ->
    Sites = hn_setup:get_sites(),
    [disc_and_mem(X) || X <- Sites],
    ok.

-spec disc_and_mem(list()) -> ok.
disc_and_mem("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Cacheable = [X || {X, _, _, _, _, C} <- hn_setup:tables(), C == cache],
    [ok = chg_copy_type(Site, Port, X, disc_copies)
     || X <- Cacheable],
    ok.

-spec disc_and_mem(list(), list()) -> ok.
disc_and_mem("http://" ++ SiteAndPort, Table) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    ok = chg_copy_type(Site, Port, Table, disc_copies),
    ok.

-spec all_sites_disc_only() -> ok.
all_sites_disc_only() ->
    Sites = hn_setup:get_sites(),
    [disc_only(X) || X <- Sites],
    ok.

-spec disc_only(list()) -> ok.
disc_only("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Cacheable = [X || {X, _, _, _, _, C} <- hn_setup:tables(), C == cache],
    [ok = chg_copy_type(Site, Port, X, disc_only_copies)
     || X <- Cacheable],
    ok.

-spec disc_only(list(), list()) -> ok.
disc_only("http://" ++ SiteAndPort, Table) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    ok = chg_copy_type(Site, Port, Table, disc_only_copies),
    ok.

-spec mem_only(list()) -> ok.
mem_only("http://" ++ SiteAndPort) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Cacheable = [X || {X, _, _, _, _, C} <- hn_setup:tables(), C == cache],
    [ok = chg_copy_type(Site, Port, X, ram_copies)
      || X <- Cacheable],
    ok.

-spec mem_only(list(), list()) -> ok.
mem_only("http://" ++ SiteAndPort, Table) ->
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    ok = chg_copy_type(Site, Port, Table, ram_copies),
    ok.

chg_copy_type(Site, Port, Table, Mode) ->
    ActualTable = list_to_existing_atom(Site ++ "&"
                                        ++ Port ++ "&"
                                        ++ atom_to_list(Table)),
    case mnesia:change_table_copy_type(ActualTable, node(), Mode) of
        {atomic, ok}                         -> ok;
        {aborted, {already_exists, _, _, _}} -> ok;
        {aborted, Reason}                    -> {error, Reason}
    end.




