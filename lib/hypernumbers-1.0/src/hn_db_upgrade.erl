%%% @author Dale Harvey
%%% @copyright 2009 Hypernumbers Ltd
%%% @doc Upgrade db functions

-module(hn_db_upgrade).

-include("spriki.hrl").
-include("hypernumbers.hrl").
-include("keyvalues.hrl").

%% Upgrade functions that were applied at upgrade_REV
-export([
         make_include_table/0,
         make_log_table/0,
         fix_borked_binaries/0,
         unload_from_mem/0,
         reload_into_mem/0,
         upgrade_doubler_2011_03_14/0,
         %upgrade_item_2011_03_14/0,
         %upgrade_local_obj_2011_03_14/0,
         upgrade_auth_srv_2011_03_13/0,
         upgrade_loc_obj_2011_03_01/0,
         upgrade_row_col_2011_02_04/0,
         upgrade_local_obj_2011_01_26/0,
         upgrade_pages_2011_01_26/0,
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

make_include_table() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tables = [
                             {include, record_info(fields, include)}
                            ],
                   [ok = make_table(Site, X, Y, disc_copies)
                    || {X, Y} <- Tables],
                   ok
           end,
    lists:foreach(Fun1, Sites).

make_log_table() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tables = [
                             {logging, record_info(fields, logging)}
                            ],
                   [ok = make_table(Site, X, Y, disc_only_copies)
                    || {X, Y} <- Tables],
                   ok
           end,
    lists:foreach(Fun1, Sites).

fix_borked_binaries() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fun = fun({item, Idx, Attrs}) when is_binary(Attrs)->
                                 io:format("still binaries in item ~p~n", [Idx]),
                                 {item, Idx, binary_to_term(Attrs)};
                            ({item, Idx, Attrs}) ->
                                 {item, Idx, Attrs}
                         end,
                   Tbl = hn_db_wu:trans(Site, item),
                   Ret = mnesia:transform_table(Tbl, Fun,
                                                 [idx, attrs]),

                   io:format("Table ~p transformed: ~p~n", [Tbl, Ret])
           end,
    lists:foreach(Fun1, Sites),
    Fun2 = fun(Site) ->
                   % first add stuff to the relations table
                   Fun3 = fun({local_obj, Idx, Type, Path, Obj, RevIdx})
                             when is_binary(Path) andalso is_binary(RevIdx)->
                                  io:format("still binaries in local_obj ~p~n",
                                            [Idx]),
                                  {local_obj, Idx, Type, binary_to_term(Path),
                                   Obj, binary_to_term(RevIdx)};
                              ({local_obj, Idx, Type, Path, Obj, RevIdx}) ->
                                  ({local_obj, Idx, Type, Path, Obj, RevIdx})
                          end,
                   Tbl2 = hn_db_wu:trans(Site, local_obj),
                   Ret2 = mnesia:transform_table(Tbl2, Fun3,
                                                 [idx, type, path,
                                                  obj, revidx]),

                   io:format("Table ~p transformed: ~p~n", [Tbl2, Ret2])
           end,
    lists:foreach(Fun2, Sites),
    ok.

unload_from_mem() ->
    Sites = hn_setup:get_sites(),
    [ok = hn_db_admin:outof_mem(X) || X <- Sites].

reload_into_mem() ->
    Sites = hn_setup:get_sites(),
    [ok = hn_db_admin:into_mem(X) || X <- Sites].

% store paths and reverse indices in #local_obj as binaries not lists of strings
% and also compresses the attributes in items to a binary (big savings!)
% * git pull
% * > hypernumbers_sup:suspend_mochi().
% * ./hn quick
% * > hn_db_upgrade:upgrade_item_2011_03_14().
% * > hypernumbers_sup:resume_mochi().
upgrade_doubler_2011_03_14() ->
    upgrade_item_2011_03_14(),
    upgrade_local_obj_2011_03_14().

% run as a doubler!
upgrade_item_2011_03_14() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   % first add stuff to the relations table
                   Fun = fun({item, Idx, Attrs}) ->
                                  {item, Idx, term_to_binary(Attrs)}
                          end,
                   Tbl = hn_db_wu:trans(Site, item),
                   Ret = mnesia:transform_table(Tbl, Fun,
                                                 [idx, attrs]),

                   io:format("Table ~p transformed: ~p~n", [Tbl, Ret])
           end,
    lists:foreach(Fun1, Sites),
    ok.

% run as a doubler!
upgrade_local_obj_2011_03_14() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   % first add stuff to the relations table
                   Fun = fun({local_obj, Idx, Type, Path, Obj, RevIdx}) ->
                                  {local_obj, Idx, Type, term_to_binary(Path),
                                   Obj, term_to_binary(RevIdx)}
                          end,
                   Tbl = hn_db_wu:trans(Site, local_obj),
                   Ret = mnesia:transform_table(Tbl, Fun,
                                                 [idx, type, path,
                                                  obj, revidx]),

                   io:format("Table ~p transformed: ~p~n", [Tbl, Ret])
           end,
    lists:foreach(Fun1, Sites),
    ok.

% Move auth_srv from a dets table into the kv store
% * git pull
% * from the shell
%   > hypernumbers_sup:suspend_mochi().
% * ./hn quick
% * from the shell
%   > upgrade_auth_srv_2011_03_13().
%   > hypernumbers_sup:resume_mochi().
upgrade_auth_srv_2011_03_13() ->
    Sites = hn_setup:get_sites(),
    Fun = fun(Site) ->
                    % Load stuff out of the dets table
                    Table = hn_util:site_to_fs(Site),
                    Dir = filename:join([code:lib_dir(hypernumbers), "..", "..",
                           "var", "dets"]),
                    filelib:ensure_dir([Dir,"/"]),
                    {ok, _} = dets:open_file(Table, [{file, filename:join(Dir,Table)}]),
                    Tree = case dets:lookup(Table, "auth_tree") of
                                []                   -> gb_trees:empty();
                                [{"auth_tree", Val}] -> Val
                            end,
                    % save it into the kv table
                    ok = hn_db_api:write_kv(Site, ?auth_srv, Tree),
                    ok = dets:close(Table)
          end,
    lists:foreach(Fun, Sites),
    ok.


% speed increases for local_obj table
upgrade_loc_obj_2011_03_01() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   % first add stuff to the relations table
                   Fun2 = fun({local_obj, Idx, Type, Path, Obj}) ->
                                  RevIdx = hn_util:list_to_path(Path)
                                      ++ hn_util:obj_to_ref(Obj),
                                  {local_obj, Idx, Type, Path, Obj, RevIdx}
                          end,
                   Tbl1 = hn_db_wu:trans(Site, local_obj),
                   io:format("Table ~p transformed~n", [Tbl1]),
                   Ret1 = mnesia:transform_table(Tbl1, Fun2,
                                                 [idx, type, path,
                                                  obj, revidx]),
                   io:format("Ret is ~p~n", [Ret1]),
                   Ret2 = mnesia:add_table_index(Tbl1, revidx),
                   io:format("Ret2 is ~p~n", [Ret2])
           end,
    lists:foreach(Fun1, Sites),
    ok.

% back out the different row/col stuff
upgrade_row_col_2011_02_04() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   % first add stuff to the relations table
                   Fun2 = fun({local_obj, Idx, Url, Path, Obj}) ->
                                  NO = case Obj of
                                           {column, {range, {X1, zero, X2, inf}}} ->
                                               {column, {X1, X2}};
                                           {row, {range, {zero, Y1, inf, Y2}}} ->
                                               {row, {Y1, Y2}};
                                           Other -> Other
                                       end,
                                  {local_obj, Idx, Url, Path, NO}
                          end,
                   Tbl1 = hn_db_wu:trans(Site, local_obj),
                   io:format("Table ~p transformed~n", [Tbl1]),
                   Ret1 = mnesia:transform_table(Tbl1, Fun2, [idx, type, path, obj]),
                   io:format("Ret is ~p~n", [Ret1])
           end,
    lists:foreach(Fun1, Sites),
    ok.

% upgrade plan for local_obj ad upgrade_pages
% * git pull the new source (DO NOT RUN ANY BUILD SCRIPT)
% * get a shell
% * run 'hypernumbers_sup:suspend_mochi().'
% * compile hn_db_upgrade, hn_db_api and hn_db_wu from the command line in ebin/
%   - 'erlc  -I ../include../src/hn_db_upgrade.erl'
%   - then in the shell load them with 'l(hn_db_upgrade)' etc...
% run 'hn_db_upgrade:upgrade_pages_2011_01_26().'
% run 'hn_db_upgrade:upgrade_local_obj_2011_01_26().'
% stop the server
% run './hn lexer-parser'
% restart the server
% mebbies it will cause the server to fail with memory overflow - if it does
% just rerun the upgrade. The memory allocator doubles on each request so a restart
% resets it.

% adds a type field to local obj
upgrade_local_obj_2011_01_26() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   % first add stuff to the relations table
                   Fun2 = fun({local_obj, Idx, Path, Obj}) ->
                                  {local_obj, Idx, url, Path, Obj}
                          end,
                   Tbl1 = hn_db_wu:trans(Site, local_obj),
                   io:format("Table ~p transformed~n", [Tbl1]),
                   Ret1 = mnesia:transform_table(Tbl1, Fun2, [idx, type, path, obj]),
                   io:format("Ret is ~p~n", [Ret1])
           end,
    lists:foreach(Fun1, Sites),
    ok.

% populates the new pages server
upgrade_pages_2011_01_26() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fun2 = fun() ->
                                  Head = #local_obj{idx ='_', type = '_', path = '$1', obj= '_'},
                                  Guard = [],
                                  Match = ['$1'],
                                  Items = hslists:uniq(mnesia:dirty_select(hn_db_wu:trans(Site, local_obj), [{Head, Guard, Match}])),
                                  io:format("Items is ~p~n", [Items]),
                                  Items
                          end,
                   Ret = mnesia:activity(transaction, Fun2),
                   io:format("Ret is ~p~n", [Ret]),
                   io:format("Page is ~p~n", [?pages]),
                   ok = hn_db_api:write_kv(Site, ?pages, Ret)
           end,
    lists:foreach(Fun1, Sites),
    ok.

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
%% * stop the server
%% * run ./hn quick
%% * then restart the server
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
                   [ok = make_table(Site, X, Y, disc_copies) || {X, Y} <- Tables],
                   ok = hn_db_api:write_kv(Site, ?zinf_tree, gb_trees:empty())
           end,
    lists:foreach(Fun1, Sites),
    ok.

make_table(Site, Record, RecordInfo, Storage) ->
    Tbl = hn_db_wu:trans(Site, Record),
    Ret = hn_db_admin:create_table(Tbl, Record, RecordInfo,
                                   Storage, set, false, []),
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

