%%% @author Dale Harvey
%%% @copyright 2009 Hypernumbers Ltd
%%% @doc Upgrade db functions

-module(hn_db_upgrade).

-include("spriki.hrl").
-include("hypernumbers.hrl").

%% Upgrade functions that were applied at upgrade_REV
-export([
         upgrade_1519/0,
         upgrade_1556/0,
         upgrade_1630/0,
         upgrade_1641/0,
         upgrade_1743_A/0,
         upgrade_1743_B/0,
         upgrade_1776/0,
         upgrade_1817/0,
         upgrade_1825/0,
         upgrade_1838/0
        ]).


%% rejigs the magic style record in table styles
upgrade_1838() ->
    
    Fun1 = fun(X) -> {styles, RefX, Idx, M} = X,
                     
                     %% index of 'border-color' in the old version of the record
                     %% (index in a list sense ie including 'magic_style')
                     Index = 5, 
                     L = tuple_to_list(M),
                     {Start, [C | End]} = lists:split(Index, L),
                     [TC, RC, BC, LC] = 
                         case C of
                             []  -> [[], [], [], []];
                             [S] -> string:tokens(S, " ")
                         end,
                     %% Note the change of order to reflect the new layout
                     %% of magic_style - different to how CSS needed them
                     L2 = lists:append([Start, [RC, LC, TC, BC], End]),
                     {styles, RefX, Idx, list_to_tuple(L2)}
           end,
    
    Tbl = fun(Host, Port, Table) ->
                  list_to_atom(lists:concat([Host,"&",Port,"&",Table]))
          end,
    
    Fun2 = fun("http://"++Site) ->
                   [Host, Port] = string:tokens(Site, ":"),
                   Name = Tbl(Host, Port, styles),
                   Fields = ms_util2:get_record_info(styles), 
                   mnesia:transform_table(Name, Fun1, Fields)
           end,
    
    [Fun2(X) || X <- hn_util:get_hosts(hn_config:get(hosts))].
    
%% Takes out duplicate entries in dependancy tree
upgrade_1825() ->
    
    Item = fun({item, Id, "__dependency-tree", List}) ->
                   {item, idstr_to_int(Id),
                    "__dependency-tree", hslists:uniq(List)};
              ({item, Id6, Name6, List6}) ->
                   {item, idstr_to_int(Id6), Name6, List6}
           end,
    
    Obj = fun({local_objs, Path, Ref, Id}) ->
                  {local_objs, Path, Ref, idstr_to_int(Id)}
          end,
    
    Link = fun({local_cell_link, Id1, Id2}) ->
                   {local_cell_link, idstr_to_int(Id1),  idstr_to_int(Id2) };
              (Else) -> Else
           end,    

    Tbl = fun(Host, Port, Table) ->
                  list_to_atom(lists:concat([Host,"&",Port,"&",Table]))
          end,
    
    F = fun("http://"++Site) ->
                
                [Host, Port] = string:tokens(Site, ":"),
                
                ItemTbl = Tbl(Host, Port, item),
                transform_keys(ItemTbl, Item),

                LinkTbl = Tbl(Host, Port, local_cell_link),                
                transform_keys(LinkTbl, Link),

                ObjTbl = Tbl(Host, Port, local_objs),
                Fields = ms_util2:get_record_info(local_objs), 
                mnesia:transform_table(ObjTbl, Obj, Fields),
                
                ok
        end,

    [F(X) || X <- hn_util:get_hosts(hn_config:get(hosts))].

transform_keys(Table, Fun) ->
    F = fun(Record, _Acc) ->
                New = Fun(Record),
                mnesia:delete(Table, Record, write),
                mnesia:write(Table, New, write)
        end,
    mnesia:activity(transaction, fun mnesia:foldl/3, [F, ok, Table]).
                

idstr_to_int("Loc"++Int) ->
    list_to_integer(Int).

%% Changes the recordname of every record in mnesia from
%% Site&Port&RecordName to RecordName
upgrade_1817() ->
    Up = fun(Rec) ->
                 [Name | Rest] = tuple_to_list(Rec),
                 Tbl = strip_site(Name),
                 list_to_tuple([Tbl | Rest])
         end,
    F = fun(schema) -> ok;
           (Tbl) ->    
                Name = strip_site(Tbl),
                Fields = ms_util2:get_record_info(Name),                
                mnesia:transform_table(Tbl, Up, Fields, Name)
        end,
    [F(X) || X <- mnesia:system_info(tables)].

strip_site(Name) ->
    [_Site, _Port, Tbl] = string:tokens(atom_to_list(Name), "&"),
    list_to_atom(Tbl).

upgrade_1519() ->
    F = fun({hn_user, Name, Pass, Auth, Created}) ->
                {hn_user, Name, Pass, Auth, Created, dict:new()}
        end,
    mnesia:transform_table(hn_user, F, record_info(fields, hn_user)).

upgrade_1556() ->
    F = fun({hn_user, Name, Pass, Auth, Created, _Dict}) ->
                {hn_user, Name, Pass, Auth, Created, dict:new()}
        end,
    mnesia:transform_table(hn_user, F, record_info(fields, hn_user)).

upgrade_1630() ->
    % multi-site upgrade
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    F3 = fun(X) ->
                 ok = mnesia:delete_object(X),
                 {Table, Idx, "dependency-tree", {xml, Value}} = X,
                 NewValue = upgrade_1630_1(Value, []),
                 NewRec = {Table, Idx, "__dependency-tree", NewValue},
                 mnesia:write(NewRec)
         end,                 
    F1 = fun(X) ->
                 F2 = fun() ->
                              H = hn_db_wu:trans(X, #item{key = "dependency-tree",
                                                          _ = '_'}),
                              Match = [{H, [], ['$_']}],
                              Recs = mnesia:select(hn_db_wu:trans(X, item), Match),
                              lists:foreach(F3, Recs)
                      end,
                 mnesia:activity(transaction, F2)
         end,
    lists:foreach(F1, Sites).

upgrade_1630_1([], Acc)      ->
    Acc;
upgrade_1630_1([H | T], Acc) ->
    {url, [{type, Type}], [Idx]} = H,
    upgrade_1630_1(T, [{Type, Idx} | Acc]).

upgrade_1641() ->
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    Fun = fun(X) ->
                  NewName = hn_db_wu:trans(X, local_objs),
                  mnesia:add_table_index(NewName, idx)
          end,
    [Fun(X) || X <- Sites].

upgrade_1743_A() ->
    % multi-site upgrade
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    F1 = fun(X) ->
                 NewName = hn_db_wu:trans(X, dirty_cell),
                 {atomic, ok} = mnesia:del_table_copy(NewName, node())
         end,                 
    [F1(X) || X <- Sites].

upgrade_1743_B() ->
    % multi-site upgrade
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    F1 = fun(X) ->
                 NewName = hn_db_wu:trans(X, dirty_cell),
                 Attr = [{attributes, ms_util2:get_record_info(dirty_cell)},
                         {type, set}, {disc_copies, [node()]}],
                 {atomic, ok} = mnesia:create_table(NewName, Attr),
                 {atomic, ok} = mnesia:add_table_index(NewName, idx)
         end,                 
    [F1(X) || X <- Sites].

upgrade_1776() ->
    % multi-site upgrade
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    F1 = fun(X) ->
                 F2 = fun(Y) ->
                              Y2 = hn_db_wu:trans_back(Y),
                              Rec = case Y2 of
                                        {item, Idx, "__dependency-tree", Val} -> 
                                            io:format("updating ~p~n", [Idx]),
                                            Val2 = lists:sort(hslists:uniq(Val)),
                                            {item, Idx, "__dependency-tree", Val2};
                                        _  ->
                                            Y2
                                    end,
                              hn_db_wu:trans(X, Rec)
                      end,
                 Table = hn_db_wu:trans(X, item),
                 mnesia:add_table_index(hn_db_wu:trans(X, local_cell_link), childidx),
                 mnesia:transform_table(Table, F2, record_info(fields, item))
         end,                 
    [F1(X) || X <- Sites].

