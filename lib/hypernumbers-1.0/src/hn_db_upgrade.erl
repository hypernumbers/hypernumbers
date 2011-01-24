%%% @author Dale Harvey
%%% @copyright 2009 Hypernumbers Ltd
%%% @doc Upgrade db functions

-module(hn_db_upgrade).

-include("spriki.hrl").
-include("hypernumbers.hrl").
-include("keyvalues.hrl").

%% Upgrade functions that were applied at upgrade_REV
-export([
         upgrade_zinf_2011_01_17/0,
         upgrade_2011_01_07/0
         %% upgrade_1519/0,
         %% upgrade_1556/0,
         %% upgrade_1630/0,
         %% upgrade_1641/0,
         %% upgrade_1743_A/0,
         %% upgrade_1743_B/0,
         %% upgrade_1776/0
        ]).


% adds 2 new tables:
% * a dirty z and infinite relations table
% * a new table of writes from the zinf tree to determine if they
%   are 'proper' dirty
% adds a new infinite and z parents record to the relations table
%% Release Procedure
%% * load code on production target (DO NOT COMPILE)
%% * run 'hypernumbers_sup:suspend_mochi().' from the shell
%% * compile hn_db_upgrade in its ebin with the command
%%   - erlc -I ../include ../src/hn_db_upgrade.erl
%% * compile hn_db_api.erl and hn_db_wu.erl the same way
%% * get to shell
%% * load the new version of the upgrade, api and wu modules
%%   l(hn_db_upgrade)
%% * run 'hn_db_upgrade:upgrade_2011_01_17().' from the shell
%% * stop and then restart the server
upgrade_zinf_2011_01_17() ->
    % multi-site upgrade
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   % first add stuff to the relations table
                   Fun2 = fun({relation, Cellidx, Children, Parents, Include}) ->
                                  {relation, Cellidx, Children, Parents,
                                   ordsets:new(), ordsets:new(), Include}
                          end,
                   Tbl1 = hn_db_wu:trans(Site, relation),
                   io:format("Table ~p transformed~n", [Tbl1]),
                   Ret1 = mnesia:transform_table(Tbl1, Fun2,
                                                 [cellidx, children, parents,
                                                  infparents, z_parents, include]),
                   io:format("Ret is ~p~n", [Ret1]),
                   % now create the tables for zinf (and the kv store to put zinf in)
                   Tables = [
                             {dirty_zinf, record_info(fields, dirty_zinf)},
                             {dirty_for_zinf, record_info(fields, dirty_for_zinf)},
                             {kvstore, record_info(fields, kvstore)}
                             ],
                   [ok = make_table(Site, X, Y) || {X, Y} <- Tables],
                   ok = hn_db_api:write_kv(Site, ?zinf_tree, gb_trees:empty())
           end,
    lists:foreach(Fun1, Sites),
    ok.

make_table(Site, Record, RecordInfo) ->
    Tbl = hn_db_wu:trans(Site, Record),
    Ret = hn_db_admin:create_table(Tbl, Record, RecordInfo,
                                   disc_copies, set, false, []),
    io:format("~p creation status: ~p~n", [Tbl, Ret]),
    Ret.

%% add a new field to the relations table - can be done prior to the table changes being released to production
%% Release Procedure
%% * load code on production target (DO NOT COMPILE)
%% * get to shell
%% * run 'hypernumbers_sup:suspend_mochi().' from the shell
%% * run 'hn_db_upgrade:upgrade_2011_01_07().' from the shell
%% * from the command line rebuild the software
%% * run 'hypernumbers_sup:resume_mochi().' from the shell
%% the code you run must have the new field 'include' in the record definition
%% of 'relation' in spriki.hrl
upgrade_2011_01_07() ->
    % multi-site upgrade
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                 Fun2 = fun({relation, Cellidx, Children, Parents}) ->
                                {relation, Cellidx, Children, Parents, false}
                        end,
                   Tbl = hn_db_wu:trans(Site, relation),
                   io:format("Table is ~p~n", [Tbl]),
                   Ret = mnesia:transform_table(Tbl, Fun2,
                                                [cellidx, children,
                                                 parents, include]),
                   io:format("Ret is ~p~n", [Ret])
         end,
    lists:foreach(Fun1, Sites).
    
%% upgrade_1519() ->
%%     F = fun({hn_user, Name, Pass, Auth, Created}) ->
%%                 {hn_user, Name, Pass, Auth, Created, dict:new()}
%%         end,
%%     mnesia:transform_table(hn_user, F, record_info(fields, hn_user)).

%% upgrade_1556() ->
%%     F = fun({hn_user, Name, Pass, Auth, Created, _Dict}) ->
%%                 {hn_user, Name, Pass, Auth, Created, dict:new()}
%%         end,
%%     mnesia:transform_table(hn_user, F, record_info(fields, hn_user)).

%% upgrade_1630() ->
%%     % multi-site upgrade
%%     HostsInfo = hn_config:get(hosts),
%%     Sites = hn_util:get_hosts(HostsInfo),
%%     F3 = fun(X) ->
%%                  ok = mnesia:delete_object(X),
%%                  {Table, Idx, "dependency-tree", {xml, Value}} = X,
%%                  NewValue = upgrade_1630_1(Value, []),
%%                  NewRec = {Table, Idx, "__dependency-tree", NewValue},
%%                  mnesia:write(NewRec)
%%          end,                 
%%     F1 = fun(X) ->
%%                  F2 = fun() ->
%%                               H = hn_db_wu:trans(X, #item{key = "dependency-tree",
%%                                                           _ = '_'}),
%%                               Match = [{H, [], ['$_']}],
%%                               Recs = mnesia:select(hn_db_wu:trans(X, item), Match),
%%                               lists:foreach(F3, Recs)
%%                       end,
%%                  mnesia:activity(transaction, F2)
%%          end,
%%     lists:foreach(F1, Sites).

%% upgrade_1630_1([], Acc)      ->
%%     Acc;
%% upgrade_1630_1([H | T], Acc) ->
%%     {url, [{type, Type}], [Idx]} = H,
%%     upgrade_1630_1(T, [{Type, Idx} | Acc]).

%% upgrade_1641() ->
%%     HostsInfo = hn_config:get(hosts),
%%     Sites = hn_util:get_hosts(HostsInfo),
%%     Fun = fun(X) ->
%%                   NewName = hn_db_wu:trans(X, local_objs),
%%                   mnesia:add_table_index(NewName, idx)
%%           end,
%%     [Fun(X) || X <- Sites].

%% upgrade_1743_A() ->
%%     % multi-site upgrade
%%     HostsInfo = hn_config:get(hosts),
%%     Sites = hn_util:get_hosts(HostsInfo),
%%     F1 = fun(X) ->
%%                  NewName = hn_db_wu:trans(X, dirty_cell),
%%                  {atomic, ok} = mnesia:del_table_copy(NewName, node())
%%          end,                 
%%     [F1(X) || X <- Sites].

%% upgrade_1743_B() ->
%%     % multi-site upgrade
%%     HostsInfo = hn_config:get(hosts),
%%     Sites = hn_util:get_hosts(HostsInfo),
%%     F1 = fun(X) ->
%%                  NewName = hn_db_wu:trans(X, dirty_cell),
%%                  Attr = [{attributes, ms_util2:get_record_info(dirty_cell)},
%%                          {type, set}, {disc_copies, [node()]}],
%%                  {atomic, ok} = mnesia:create_table(NewName, Attr),
%%                  {atomic, ok} = mnesia:add_table_index(NewName, idx)
%%          end,                 
%%     [F1(X) || X <- Sites].

%% upgrade_1776() ->
%%     % multi-site upgrade
%%     HostsInfo = hn_config:get(hosts),
%%     Sites = hn_util:get_hosts(HostsInfo),
%%     F1 = fun(X) ->
%%                  F2 = fun(Y) ->
%%                               Y2 = hn_db_wu:trans_back(Y),
%%                               Rec = case Y2 of
%%                                         {item, Idx, "__dependency-tree", Val} -> 
%%                                             io:format("updating ~p~n", [Idx]),
%%                                             Val2 = lists:sort(hslists:uniq(Val)),
%%                                             {item, Idx, "__dependency-tree", Val2};
%%                                         _  ->
%%                                             Y2
%%                                     end,
%%                               hn_db_wu:trans(X, Rec)
%%                       end,
%%                  Table = hn_db_wu:trans(X, item),
%%                  mnesia:add_table_index(hn_db_wu:trans(X, local_cell_link), childidx),
%%                  mnesia:transform_table(Table, F2, record_info(fields, item))
%%          end,                 
%%     [F1(X) || X <- Sites].

