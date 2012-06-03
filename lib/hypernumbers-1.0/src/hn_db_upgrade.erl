%%% @author Dale Harvey
%%% @copyright 2009 Hypernumbers Ltd
%%% @doc Upgrade db functions

-module(hn_db_upgrade).

-include("spriki.hrl").
-include("keyvalues.hrl").

%% Upgrade functions that were applied at upgrade_REV
-export([
         add_dirty_queue_cache_2012_06_03/0,
         rejig_phones/1,
         add_site_table_2011_05_12/0,
         upgrade_phone_records_2012_05_08/0,
         change_hns_record_table/0,
         add_siteonly_table_2012_03_05/0,
         write_twilio_kvs/3,
         write_twilio_spoof_kvs/0,
         write_twilio_use_kvs/0,
         write_twilio_dev_kvs/0,
         add_phone_table_2011_02_11/0,
         upgrade_relation_table_2012_01_03/0,
         recalc_includes/0,
         fix_page_srv_2011_11_23/0,
         upgrade_etl_2011_11_23/0,
         add_api_table_2011_11_03/0,
         show_ranges_2011_10_20/0,
         fix_up_row_col_revidxs_2011_10_20/0,
         do_z_parents_exist_2011_10_20/0,
         type_local_objs_2011_10_13/0,
         check_local_objs_2011_10_13/0,
         add_del_obj_table_2011_10_13/0,
         check_local_obj_consistency/0,
         look_for_borked_merges/0,
         look_for_borked_merges/1,
         make_revidx_2011_10_02/0,
         dump_borked_local_objs/0,
         fix_borked_local_objs/3,
         fix_borked_local_objs/2,
         fix_borked_local_objs/1,
         fix_borked_local_objs/0,
         clean_up_timer_table/0,
         add_api_table_2011_07_23/0,
         blip/0,
         unmigrate_page_srv_2011_07_11/0,
         migrate_page_srv_2011_07_09/0,
         add_user_fn_table_2011_06_30/0,
         bug_fix_for_row_cols_2011_06_25/0,
         add_path_index_to_logs_2011_05_26/0,
         force_sparkline_recalc_2011_05_15/0,
         bug_fix_dirty_for_zinf_2011_05_14/0,
         add_timer_table_2011_05_02/0,
         upgrade_dirty_zinf_2011_05_02/0,
         flash_tests/0,
         flash_dev/0,
         flash_devsrv/0,
         fix_zinf_local_obj_bug/0,
         add_include_index/0,
         make_include_table/0,
         make_log_table/0,
         fix_borked_binaries/0,
         unload_from_mem/0,
         reload_into_mem/0,
         upgrade_doubler_2011_03_14/0,
         % upgrade_item_2011_03_14/0,
         % upgrade_local_obj_2011_03_14/0,
         upgrade_auth_srv_2011_03_13/0,
         upgrade_loc_obj_2011_03_01/0,
         upgrade_row_col_2011_02_04/0,
         upgrade_local_obj_2011_01_26/0,
         upgrade_pages_2011_01_26/0,
         upgrade_zinf_2011_01_17/0,
         upgrade_2011_01_07/0
         % upgrade_1519/0,
         % upgrade_1556/0,
         % upgrade_1630/0,
         % upgrade_1641/0,
         % upgrade_1743_A/0,
         % upgrade_1743_B/0,
         % upgrade_1776/0
        ]).

add_dirty_queue_cache_2012_06_03() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   io:format("fixing dirty_queue_cache for ~p~n", [Site]),
                   OTbl = new_db_wu:trans(Site, dirty_q_cache),
                   mnesia:delete_table(OTbl),
                   NTbl = new_db_wu:trans(Site, dirty_queue_cache),
                   Fields = record_info(fields, dirty_queue_cache),
                   make_table(Site, NTbl, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

rejig_phones(Site) ->
    AccSid = "AC7a076e30da6d49119b335d3a6de43844",
    AuthTk = "9248c9a2a25f6914fad9c9fb5b30e69c",
    AppSid = "APfc041b9fd029441fba86f114ad4ca09d",
    AC = #twilio_account{account_sid     = AccSid,
                         auth_token      = AuthTk,
                         application_sid = AppSid,
                         site_phone_no   = "+441315101897",
                         type            = outbound},
    new_db_api:write_kv(Site, ?twilio, AC).

add_site_table_2011_05_12() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tables = mnesia:system_info(local_tables),
                   Tbl = new_db_wu:trans(Site, site),
                   case lists:member(Tbl, Tables) of
                       true  -> mnesia:delete_table(Site);
                       false -> ok
                   end,
                   Fields = record_info(fields, site),
                   make_table(Site, site, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

upgrade_phone_records_2012_05_08() ->
    Sites = hn_setup:get_sites(),
    F2 = fun(Site) ->
                 F1 = fun() ->
                              NewAccSid = "AC7a076e30da6d49119b335d3a6de43844",
                              NewAuthTk = "9248c9a2a25f6914fad9c9fb5b30e69c",
                              NewAppSid = "AP93d273f3cc624008805842376d561bed",
                              NewPhone = "+441315101897",
                              R = #twilio_account{account_sid = NewAccSid,
                                                  auth_token = NewAuthTk,
                                                  application_sid = NewAppSid,
                                                  site_phone_no = NewPhone,
                                                  type = outbound},
                              new_db_api:write_kv(Site, ?twilio, R)
                      end,
                 mnesia:activity(transaction, F1)
         end,
    lists:foreach(F2, Sites),
    ok.

write_twilio_kvs(Site, AppSID, PhoneNo)
  when is_list(PhoneNo)->
    AccSID = "AC7a076e30da6d49119b335d3a6de43844",
    AuthToken = "9248c9a2a25f6914fad9c9fb5b30e69c",
    AC = #twilio_account{account_sid     = AccSID,
                         auth_token      = AuthToken,
                         application_sid = AppSID,
                         site_phone_no   = PhoneNo},
    new_db_api:write_kv(Site, ?twilio, AC).

write_twilio_spoof_kvs() ->
    Site = "http://hypernumbers.dev:9000",
    AC = #twilio_account{account_sid     = "aaaa",
                         auth_token      = "bbbb",
                         application_sid = "cccc",
                         site_phone_no   = "+441315101875",
                         type            = full},
    new_db_api:write_kv(Site, ?twilio, AC).

write_twilio_dev_kvs() ->
    Site = "http://dev.hypernumbers.com:8080",
    AC = #twilio_account{account_sid     = "AC7a076e30da6d49119b335d3a6de43844",
                         auth_token      = "9248c9a2a25f6914fad9c9fb5b30e69c",
                         application_sid = "APe2c6b02daf974b699fb14591cc7bbd79",
                         site_phone_no   = "+441315101875",
                         type            = full},
    new_db_api:write_kv(Site, ?twilio, AC).

write_twilio_use_kvs() ->
    Site = "http://usability.hypernumbers.com:8080",
    AC = #twilio_account{account_sid     = "AC7a076e30da6d49119b335d3a6de43844",
                         auth_token      = "9248c9a2a25f6914fad9c9fb5b30e69c",
                         application_sid = "APf2b5e475549b404e8ff26ed1a9fb8bcb",
                         site_phone_no   = "+441315101883",
                         type            = fulll},
    new_db_api:write_kv(Site, ?twilio, AC).

change_hns_record_table() ->
    Fun = fun({record, Nm, Add, ZID, RID}) ->
                  {hns_record, Nm, Add, ZID, RID}
          end,
    Tbl = service_hns_record,
    Ret = mnesia:transform_table(Tbl, Fun, [name, address,
                                            zone_id, resource_id], hns_record),
    io:format("Ret is ~p~n", [Ret]),
    ok.

add_siteonly_table_2012_03_05() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tables = mnesia:system_info(local_tables),
                   Tbl = new_db_wu:trans(Site, siteonly),
                   case lists:member(Tbl, Tables) of
                       true  -> mnesia:delete_table(Site);
                       false -> ok
                   end,
                   Fields = record_info(fields, siteonly),
                   make_table(Site, siteonly, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

add_phone_table_2011_02_11() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tables = mnesia:system_info(local_tables),
                   Tbl = new_db_wu:trans(Site, phone),
                   case lists:member(Tbl, Tables) of
                       true  -> mnesia:delete_table(Site);
                       false -> ok
                   end,
                   Fields = record_info(fields, phone),
                   make_table(Site, phone, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

% * git pull
% * > hypernumbers_sup:suspend_mochi().
% * ./hn quick
% * > hn_db_upgrade:upgrade_relation_table_2012_01_03().
% * > hypernumbers_sup:resume_mochi().
upgrade_relation_table_2012_01_03() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fun = fun({relation, C, Ch, P, InfP, ZP, Inc}) ->
                                 O = ordsets:new(),
                                 {relation, C, Ch, P, InfP, ZP, O, O, Inc}
                         end,
                   Tbl = new_db_wu:trans(Site, relation),
                   io:format("Table ~p transformed~n", [Tbl]),
                   Ret1 = mnesia:transform_table(Tbl, Fun,
                                                 [cellidx, children, parents,
                                                  infparents, z_parents,
                                                  dyn_parents, dyn_infparents,
                                                  include]),
                   io:format("Ret is ~p~n", [Ret1])
           end,
    lists:foreach(Fun1, Sites),
    ok.

recalc_includes() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   io:format("~nForcing recalcs on includes in ~p~n", [Site]),
                   Tbl2 = new_db_wu:trans(Site, relation),
                   Fun2 = fun(X, N) ->
                                  case X#relation.include of
                                      false ->
                                          io:format("."),
                                          ok;
                                      true  ->
                                          io:format("*"),
                                          [new_db_api:mark_idx_dirty(Site, Y)
                                           || Y <- X#relation.children]
                                  end,
                                  if
                                      N > 80 ->
                                          io:format("n"),
                                          1;
                                      N =< 80 ->
                                          N
                                  end
                          end,
                   Fun3 = fun() ->
                                  mnesia:foldl(Fun2, 1, Tbl2)
                          end,
                   mnesia:activity(transaction, Fun3)
           end,
    lists:foreach(Fun1, Sites),
    ok.

fix_page_srv_2011_11_23() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   [{kvstore, ?pages, P}] = new_db_api:read_kv(Site, ?pages),
                   case P of
                       [] ->
                           P2 = dh_tree:new(),
                           ok = new_db_api:write_kv(Site, ?pages, P2),
                           io:format("Page server for ~p updated~n", [Site]);
                       _  -> ok
                   end
           end,
    lists:foreach(Fun1, Sites),
    ok.

upgrade_etl_2011_11_23() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Root = hn_util:etlroot(Site),
                   Files = filelib:wildcard(Root ++ "/*.map"),
                   io:format("Root is ~p Files is ~p~n", [Root, Files]),
                   [upgrade_etl(X) || X <- Files]

           end,
    lists:foreach(Fun1, Sites),
    ok.

upgrade_etl(File) ->
    {ok, Terms} = file:consult(File),
    Terms2 = upgrade_etl2(Terms, []),
    IoData = lists:flatten([io_lib:format("~p.~n", [X]) || X <- Terms2]),
    ok = file:write_file(File, IoData),
    ok.

upgrade_etl2([], Acc) ->
    lists:reverse(Acc);
upgrade_etl2([{head, Ty, Ft, Tp, Ov} | T], Acc) ->
    NewHead = #head{type = Ty, filetype = Ft, template = Tp, overwrite = Ov},
    upgrade_etl2(T, [NewHead | Acc]);
upgrade_etl2([H | T], Acc) ->
    upgrade_etl2(T, [H | Acc]).

add_api_table_2011_11_03() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fields = record_info(fields, api),
                   make_table(Site, api, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

show_ranges_2011_10_20() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   io:format("Checking site ~p~n", [Site]),
                   Tbl = new_db_wu:trans(Site, local_obj),
                   Fun2 = fun(X, []) ->
                                  #local_obj{obj = O, path = P} = X,
                                  case O of
                                      {range, _} ->
                                          Pa = hn_util:list_to_path(binary_to_term(P)),
                                          NO = hn_util:obj_to_ref(O),
                                          io:format("Range in ~p ~p~n", [X, Pa ++ NO]);
                                      _ ->
                                          ok
                                  end,
                                  []
                          end,
                   Fun3 = fun() ->
                                  mnesia:foldl(Fun2, [], Tbl)
                          end,
                   mnesia:activity(transaction, Fun3)
           end,
    lists:foreach(Fun1, Sites).

fix_up_row_col_revidxs_2011_10_20() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   io:format("Checking site ~p~n", [Site]),
                   Tbl = new_db_wu:trans(Site, local_obj),
                   Fun2 = fun(X, []) ->
                                  #local_obj{path = P, obj = O, revidx = R} = X,
                                  Pa = hn_util:list_to_path(binary_to_term(P)),
                                  NO = hn_util:obj_to_ref(O),
                                  R2 = binary_to_term(R),
                                  Rev2 = Pa ++ NO,
                                  case Rev2 of
                                      R2 ->
                                          ok;
                                      _ ->
                                          io:format("borked revidx ~p ~p for ~p~n",
                                                    [R2, Rev2, X]),
                                          Rec = X#local_obj{revidx = term_to_binary(Rev2)},
                                          mnesia:write(Tbl, Rec, write)
                                  end,
                                  []
                          end,
                   Fun3 = fun() ->
                                  mnesia:foldl(Fun2, [], Tbl)
                          end,
                   mnesia:activity(transaction, Fun3)
           end,
    lists:foreach(Fun1, Sites).

do_z_parents_exist_2011_10_20() ->
    Sites = hn_setup:get_sites(),
    F1 = fun(Site) ->
                 io:format("Checking z parents for ~p~n", [Site]),
                 Tbl = new_db_wu:trans(Site, relation),
                 F2 = fun(LO, Acc) ->
                              case LO of
                                  #relation{z_parents = []} ->
                                      ok;
                                  _ ->
                                      io:format("LO is ~p~n", [LO])
                              end,
                              Acc
                      end,
                 F3 = fun() ->
                              mnesia:foldl(F2, [], Tbl)
                      end,
                 mnesia:activity(transaction, F3)
         end,
    lists:foreach(F1, Sites).

type_local_objs_2011_10_13() ->
    Sites = hn_setup:get_sites(),
    Fun2 = fun(Site) ->
                   io:format("Checking site ~p~n", [Site]),
                   Tbl3 = new_db_wu:trans(Site, local_obj),
                   Fun1 = fun(X, []) ->
                                  #local_obj{type = T, path = P,
                                             obj = O} = X,
                                  Pa = binary_to_term(P),
                                  case T of
                                      undefined ->
                                          Type = type(Pa),
                                          io:format("undefined ~p ~p is ~p~n",
                                                    [Pa, O, Type]),
                                          Rec = X#local_obj{type = Type},
                                          mnesia:write(Tbl3, Rec, write);
                                      "url" ->
                                          io:format("string type url ~p ~p~n",
                                                    [Pa, O]),
                                          Rec = X#local_obj{type = url},
                                          mnesia:write(Tbl3, Rec, write);
                                      _ ->
                                          ok
                                  end,
                                  case {O, T} of
                                      {{Ref, _}, gurl} when Ref == row
                                                            orelse Ref == column ->
                                          ok;
                                      {{Ref, _}, _} when Ref == row
                                                         orelse Ref == column ->
                                          io:format("string type url ~p ~p was ~p~n",
                                                    [Pa, O, T]),
                                          Rec2 = X#local_obj{type = gurl},
                                          mnesia:write(Tbl3, Rec2, write);
                                      _ ->
                                          ok
                                  end,
                                  []
                          end,
                   Fun3 = fun() ->
                                  mnesia:foldl(Fun1, [], Tbl3)
                          end,
                   mnesia:activity(transaction, Fun3)
           end,
    lists:foreach(Fun2, Sites).

type([])               -> url;
type(["["++_Rest| _T]) -> gurl;
type([_H | T])         -> type(T).

check_local_objs_2011_10_13() ->
    Sites = hn_setup:get_sites(),
    Fun2 = fun(Site) ->
                   io:format("Checking site ~p~n", [Site]),
                   Tbl1 = new_db_wu:trans(Site, item),
                   Tbl2 = new_db_wu:trans(Site, relation),
                   Tbl3 = new_db_wu:trans(Site, local_obj),
                   Fun1 = fun(X, []) ->
                                  #local_obj{idx = Idx, type = T, path = P,
                                             obj = O} = X,
                                  case T of
                                      undefined ->
                                          Pa = binary_to_term(P),
                                          io:format("undefined ~p ~p~n", [Pa, O]);
                                      _ ->
                                          ok
                                  end,
                                  Ret1 = mnesia:read(Tbl1, Idx, read),
                                  Ret2 = mnesia:read(Tbl2, Idx, read),
                                  case {Ret1, Ret2} of
                                      {[], []} -> %mnesia:delete(Tbl3, Idx,
                                          %              write);
                                          P2 = binary_to_term(P),
                                          io:format("delete ~p ~p~n", [P2, O]);
                                      _ -> ok
                                  end,
                                  []
                          end,
                   Fun3 = fun() ->
                                  mnesia:foldl(Fun1, [], Tbl3)
                          end,
                   mnesia:activity(transaction, Fun3)
           end,
    lists:foreach(Fun2, Sites).

add_del_obj_table_2011_10_13() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fields = record_info(fields, del_local),
                   make_table(Site, del_local, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

check_local_obj_consistency() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(X, []) ->
                   #local_obj{path = P, obj = O, revidx = R} = X,
                   P2 = hn_util:list_to_path(binary_to_term(P)),
                   O2 = hn_util:obj_to_ref(O),
                   R2 = binary_to_term(R),
                   case P2 ++ O2 of
                       R2 -> ok;
                       _  -> io:format("Revidx is borked ~p ~p~n",
                                       [R2, P2 ++ O2])
                   end,
                   []
           end,
    Fun2 = fun(Site) ->
                   io:format("Checking site ~p~n", [Site]),
                   Tbl = new_db_wu:trans(Site, local_obj),
                   Fun3 = fun() ->
                                  mnesia:foldl(Fun1, [], Tbl)
                          end,
                   mnesia:activity(transaction, Fun3)
           end,
    lists:foreach(Fun2, Sites).

look_for_borked_merges() ->
    Sites = hn_setup:get_sites(),
    [look_for_borked_merges(X) || X <- Sites],
    ok.

look_for_borked_merges(Site) ->
    io:format("Checking site ~p for borked merges~n", [Site]),
    Tbl = new_db_wu:trans(Site, item),
    Fun1 = fun(Item, []) ->
                   Term = binary_to_term(Item#item.attrs),
                   case lists:keyfind("merge", 1, Term) of
                       false ->
                           ok;
                       {"merge", {struct, List}} ->
                           check_merges(List, Item#item.idx)
                   end,
                   []
           end,
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, [], Tbl)
           end,
    mnesia:activity(transaction, Fun2).

check_merges([], _Idx) -> ok;
check_merges([{Type, N} | T], Idx) when N < 0 ->
    io:format("Index ~p has negative index ~p for ~p~n", [Idx, N, Type]),
    check_merges(T, Idx);
check_merges([_H | T], Idx) ->
    check_merges(T, Idx).

make_revidx_2011_10_02() ->
    Sites = hn_setup:get_sites(),
    Fields = record_info(fields, revidx),
    Fun1 = fun(Site) ->
                   make_table(Site, revidx, Fields, disc_copies),
                   Tbl1 = new_db_wu:trans(Site, local_obj),
                   Tbl2 = new_db_wu:trans(Site, revidx),
                   Fun2 = fun(LO, []) ->
                                  #local_obj{idx = Idx, path = P, obj = O} = LO,
                                  P2 = binary_to_term(P),
                                  RevIdx = hn_util:list_to_path(P2)
                                      ++ hn_util:obj_to_ref(O),
                                  BRevIdx = term_to_binary(RevIdx),
                                  Rec = #revidx{revidx = BRevIdx, idx = Idx},
                                  mnesia:write(Tbl2, Rec, write),
                                  io:format("."),
                                  []
                          end,
                   Fun3 = fun() ->
                                  mnesia:foldl(Fun2, [], Tbl1)
                          end,
                   mnesia:activity(transaction, Fun3),
                   io:format("~n")
           end,
    lists:foreach(Fun1, Sites).

dump_borked_local_objs() ->
    Sites = hn_setup:get_sites(),
    fix2(Sites, [], report).

fix_borked_local_objs() ->
    Sites = hn_setup:get_sites(),
    fix2(Sites, quiet, report).

fix_borked_local_objs(Site) ->
    fix2([Site], quiet, report).

fix_borked_local_objs(Site, Verbosity) ->
    fix2([Site], Verbosity, report).

fix_borked_local_objs(Site, Verbosity, Fix) ->
    fix2([Site], Verbosity, Fix).

fix2(Sites, Verbosity, Fix) ->
    F = fun(Site) ->
                io:format("inspecting ~p~n", [Site]),
                Tbl = new_db_wu:trans(Site, local_obj),
                F2 = fun(LO, {N, Acc}) ->
                             #local_obj{path = P, obj = O, revidx = R} = LO,
                             P2 = binary_to_term(P),
                             Pattern = {local_obj, '_', '_', '_', '_', R},
                             case mnesia:index_match_object(Tbl, Pattern,
                                                            6, read) of
                                 [_I] ->
                                     {N, Acc};
                                 List ->
                                     write(Verbosity, "~p LO ~p ~p ~p "
                                           ++ "borked ~p~n",
                                           [N, Site, P2, O,
                                            length(List)]),
                                     NewAcc = add(P, O, List, Acc),
                                     {N + 1, NewAcc}
                             end
                     end,
                F3 = fun() ->
                             mnesia:foldl(F2, {0, []}, Tbl)
                     end,
                {NBorked, Borked} = mnesia:activity(async_dirty, F3),
                write(Verbosity, "~p borked local_objs for ~p~n",
                      [NBorked, Site]),
                Borked2 = transform(Borked, []),
                F4 = fun() ->
                             {DI, DX} = fix_up(Verbosity, Site, Borked2),
                             write(Verbosity, "DI is ~p~nDX is ~p~n", [DI, DX]),
                             case Fix of
                                 fix ->
                                     [new_db_api:mark_idx_dirty(Site, X)
                                      || X <- DI],
                                     new_db_wu:mark_these_dirtyD(DX, nil);
                                 _ ->
                                     ok
                             end
                     end,
                mnesia:activity(transaction, F4)
        end,
    lists:foreach(F, Sites),
    ok.

write(verbose, Msg, Data) -> io:format(Msg, Data);
write(_, _, _)            -> ok.

fix_up(Verb, Site, Borked) ->
    fix_item(Verb, Site, Borked),
    fix_logging(Verb, Site, Borked),
    fix_form(Verb, Site, Borked),
    fix_timer(Verb, Site, Borked),
    fix_include(Verb, Site, Borked),
    % returns a list of idxs to recalc
    {_, {DirtyIdxs, DirtyXRefXs}} = fix_relation(Verb, Site, Borked),
    {hslists:uniq(lists:flatten(DirtyIdxs)),
     hslists:uniq(lists:flatten(DirtyXRefXs))}.

fix_item(Verb, Site, Borked) ->
    Tbl = new_db_wu:trans(Site, item),
    Fun = fun(#item{idx = Idx}, Bkd) ->
                  case lists:keyfind(Idx, 1, Bkd) of
                      false -> ok;
                      Tuple -> write(Verb, "in item ~p (should delete)~n",
                                     [Tuple])
                  end,
                  Bkd
          end,
    mnesia:foldl(Fun, Borked, Tbl).

fix_relation(Verb, Site, Borked) ->
    Tbl = new_db_wu:trans(Site, relation),
    Fun = fun(Rel, {Bkd, {DIdx, DXRefX}}) ->
                  #relation{cellidx = Idx, children = C, parents = P,
                            infparents = Inf, z_parents = Z} = Rel,
                  NDs = case lists:keyfind(Idx, 1, Bkd) of
                            false -> Cks = [{parents, P},
                                            {infinite, Inf},
                                            {zs, Z}],
                                     D = case check_rels(Cks, Idx, Bkd, false) of
                                             true  -> [Idx | DIdx];
                                             false -> DIdx
                                         end,
                                     D2 = check_children(C, Site, Bkd, []),
                                     {lists:merge(DIdx, D),
                                      lists:merge(DXRefX, D2)};
                            Tuple ->
                                write(Verb, "in relation ~p  (should delete)~n",
                                      [Tuple]),
                                {DIdx, DXRefX}
                        end,
                  {Borked, NDs}
          end,
    mnesia:foldl(Fun, {Borked, {[], []}}, Tbl).

check_children([], _Site, _Bkd, Acc) -> Acc;
check_children([H | T], Site, Bkd, Acc) ->
    NewAcc = case lists:keyfind(H, 1, Bkd) of
                 false -> Acc;
                 _     -> [new_db_wu:idx_to_xrefXD(Site, H) | Acc]
             end,
    check_children(T, Site, Bkd, NewAcc).

check_rels([], _Idx, _Bkd, IsDirty) -> IsDirty;
check_rels([{Type, List} | T], Idx, Bkd, IsDirty) ->
    NewIsDirty = case check_rels2(List, Idx, Type, Bkd, false) of
                     false -> IsDirty;
                     true  -> true
                 end,
    check_rels(T, Idx, Bkd, NewIsDirty).

check_rels2([], _Idx, _Type, _Borked, IsDirty) -> IsDirty;
check_rels2([H | T], Idx, Type, Borked, IsDirty) ->
    NewIsDirty = case lists:keyfind(H, 1, Borked) of
                     false  -> IsDirty;
                     _Tuple -> io:format("Substitute ~p ~p for ~p~n",
                                         [Type, H, Idx]),
                               true
                 end,
    check_rels2(T, Idx, Type, Borked, NewIsDirty).

fix_logging(Verb, Site, Borked) ->
    Tbl = new_db_wu:trans(Site, logging),
    Fun = fun(#logging{idx = Idx}, Bkd) ->
                  case lists:keyfind(Idx, 1, Bkd) of
                      false -> ok;
                      Tuple -> write(Verb, "in logging ~p (should delete)~n",
                                     [Tuple])
                  end,
                  Bkd
          end,
    mnesia:foldl(Fun, Borked, Tbl).

fix_form(Verb, Site, Borked) ->
    Tbl = new_db_wu:trans(Site, form),
    Fun = fun(#form{key = Idx}, Bkd) ->
                  case lists:keyfind(Idx, 1, Bkd) of
                      false -> ok;
                      Tuple -> write(Verb, "in form ~p (should delete)~n",
                                     [Tuple])
                  end,
                  Bkd
          end,
    mnesia:foldl(Fun, Borked, Tbl).

fix_timer(Verb, Site, Borked) ->
    Tbl = new_db_wu:trans(Site, timer),
    Fun = fun(#timer{idx = Idx}, Bkd) ->
                  case lists:keyfind(Idx, 1, Bkd) of
                      false -> ok;
                      Tuple -> write(Verb, "in timer ~p (should delete)~n",
                                     [Tuple])
                  end,
                  Bkd
          end,
    mnesia:foldl(Fun, Borked, Tbl).

fix_include(Verb, Site, Borked) ->
    Tbl = new_db_wu:trans(Site, include),
    Fun = fun(#include{idx = Idx}, Bkd) ->
                  case lists:keyfind(Idx, 1, Bkd) of
                      false -> ok;
                      Tuple -> write(Verb, "in include ~p (should delete)~n",
                                     [Tuple])
                  end,
                  Bkd
          end,
    mnesia:foldl(Fun, Borked, Tbl).

transform([], Acc) -> Acc;
transform([H | T], Acc) ->
    {_, List} = H,
    Fun = fun(#local_obj{idx = I1}, #local_obj{idx = I2}) ->
                  if
                      I1 >  I2 -> true;
                      I1 =< I2 -> false
                  end
          end,
    L2 = lists:sort(Fun, List),
    [LO | Extras] = L2,
    NewAcc = lists:merge(zip(Extras, LO, []), Acc),
    transform(T, NewAcc).

zip([], _, Acc)       -> Acc;
zip([H | T], LO, Acc) ->
    #local_obj{idx = BorkedIdx} = H,
    #local_obj{idx = MasterIdx} = LO,
    zip(T, LO, [{BorkedIdx, MasterIdx} | Acc]).

add(Path, Obj, List, Acc) ->
    case lists:keymember({Path, Obj}, 1, Acc) of
        true  -> Acc;
        false -> [{{Path, Obj}, List} | Acc]
    end.

clean_up_timer_table() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(#timer{idx = Idx}, Acc) ->
                   [Idx | Acc]
           end,
    Fun2 = fun(Site) ->
                   io:format("Site is ~p~n", [Site]),
                   Fun3 = fun() ->
                                  Tbl = new_db_wu:trans(Site, timer),
                                  Idxs = mnesia:foldl(Fun1, [], Tbl),
                                  Tbl4 = new_db_wu:trans(Site, local_obj),
                                  Fun4 = fun(Idx) ->
                                                 case mnesia:read(Tbl4, Idx, read) of
                                                     [#local_obj{}] ->
                                                         ok;
                                                     [] ->
                                                         io:format("Idx ~p NOT found~n", [Idx]),
                                                         mnesia:delete(Tbl, Idx, write)
                                                 end
                                         end,
                                  [Fun4(X) || X <- Idxs],
                                  ok
                          end,
                   mnesia:activity(transaction, Fun3)
           end,
    lists:foreach(Fun2, Sites),
    ok.

add_api_table_2011_07_23() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fields = record_info(fields, api),
                   make_table(Site, api, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

blip() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   [{kvstore, ?pages, P}] = new_db_api:read_kv(Site, ?pages),
                   {ok, P2} = P,
                   ok = new_db_api:write_kv(Site, ?pages, P2),
                   io:format("Page server for ~p updated~n", [Site])
           end,
    lists:foreach(Fun1, Sites),
    ok.

unmigrate_page_srv_2011_07_11() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   [{kvstore, ?pages, P}] = new_db_api:read_kv(Site, ?pages),
                   {ok, P2} = dh_tree:flatlist(P),
                   ok = new_db_api:write_kv(Site, ?pages, P2),
                   io:format("Page server for ~p updated~n", [Site])
           end,
    lists:foreach(Fun1, Sites),
    ok.

migrate_page_srv_2011_07_09() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   [{kvstore, ?pages, P}] = new_db_api:read_kv(Site, ?pages),
                   P2 = dh_tree:create(P),
                   ok = new_db_api:write_kv(Site, ?pages, P2),
                   io:format("Page server for ~p updated~n", [Site])
           end,
    lists:foreach(Fun1, Sites),
    ok.

add_user_fn_table_2011_06_30() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fields = record_info(fields, user_fns),
                   make_table(Site, user_fns, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

bug_fix_for_row_cols_2011_06_25() ->
    Sites = hn_setup:get_sites(),
    F1 = fun(Site) ->
                 F2 = fun({local_obj, Idx, Type, Path, Obj, RevB}) ->
                              P = binary_to_term(Path),
                              Rev2 = hn_util:list_to_path(P)
                                  ++ hn_util:obj_to_ref(Obj),
                              Revidx = binary_to_term(RevB),
                              NewRevB = case Rev2 of
                                            Revidx ->
                                                RevB;
                                            O     ->
                                                io:format("Fixing ~p ~p ~p~n"
                                                          ++ "old revidx ~p~n",
                                                          [Site, P, Obj, O]),
                                                term_to_binary(Rev2)
                                        end,
                              {local_obj, Idx, Type, Path, Obj, NewRevB}
                      end,
                 Tbl1 = new_db_wu:trans(Site, local_obj),
                 Ret = mnesia:transform_table(Tbl1, F2, [idx, type, path,
                                                         obj, revidx]),
                 io:format("Ret is ~p~n", [Ret]),
                 io:format("Table ~p transformed~n", [Tbl1])
         end,
    lists:foreach(F1, Sites).

add_path_index_to_logs_2011_05_26() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tbl = new_db_wu:trans(Site, logging),
                   Ret = mnesia:add_table_index(Tbl, path),
                   io:format("Ret is ~p~n", [Ret])
           end,
    lists:foreach(Fun1, Sites).

% will populate the dirty queue - to retrigger calcs just write a cell
force_sparkline_recalc_2011_05_15() ->
    %Site = "http://hypernumbers.dev:9000",
    Site = "http://dla-piper.hypernumbers.com:80",
    Length = 6,
    Root = "buildings",
    Cell = "H15",
    Obj = hn_util:parse_ref(Cell),
    Pages = page_srv:get_pages(Site),
    Pages2 = filter_pages(Pages, Length, Root, []),
    mark_dirty(Site, Pages2, Obj),
    ok.

mark_dirty(_, [], _) -> ok;
mark_dirty(Site, [H | T], Obj) ->
    RefX = #refX{site = Site, type = url, path = H, obj = Obj},
    Fun  = fun() ->
                   case new_db_wu:refX_to_xrefXD(RefX) of
                       false -> ok;
                       XRefX ->
                           io:format("~p exists - marking dirty~n", [XRefX]),
                           new_db_wu:mark_these_dirtyD([XRefX], nil)
                   end
           end,
    mnesia:transaction(Fun),
    mark_dirty(Site, T, Obj).

filter_pages([], _Length, _Root, Acc) -> Acc;
filter_pages([[Root | _T1] = P | T2], Length, Root, Acc) ->
    io:format("Checking ~p against ~p~n", [P, Root]),
    NewAcc = if
                 length(P) ==  Length -> [P | Acc];
                 length(P) =/= Length -> Acc
             end,
    filter_pages(T2, Length, Root, NewAcc);
filter_pages([_H | T], Length, Root, Acc) ->
    filter_pages(T, Length, Root, Acc).

bug_fix_dirty_for_zinf_2011_05_14() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tbl = new_db_wu:trans(Site, dirty_for_zinf),
                   io:format("about to check ~p~n", [Tbl]),
                   Fun2 = fun(#dirty_for_zinf{dirty = #refX{}} = Rec) ->
                                  RefX = Rec#dirty_for_zinf.dirty,
                                  XRefX = new_db_wu:refX_to_xrefXD(RefX),
                                  NewRec = Rec#dirty_for_zinf{dirty = XRefX},
                                  io:format("found a refX ~p~n", [Rec]),
                                  io:format("NewRec is ~p~n", [NewRec]),
                                  NewRec;
                             (X) ->
                                  X
                          end,
                   mnesia:transform_table(Tbl, Fun2, [id, dirty])
           end,
    lists:foreach(Fun1, Sites),
    ok.

add_timer_table_2011_05_02() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fields = record_info(fields, timer),
                   make_table(Site, timer, Fields, disc_copies)
           end,
    lists:foreach(Fun1, Sites),
    ok.

upgrade_dirty_zinf_2011_05_02() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Fun = fun({dirty_zinf, Id, Type, Dirty, O, N}) ->
                                 {dirty_zinf, Id, Type, Dirty, O, N, false}
                         end,
                   Tbl = new_db_wu:trans(Site, dirty_zinf),
                   io:format("Table ~p transformed~n", [Tbl]),
                   Ret1 = mnesia:transform_table(Tbl, Fun,
                                                 [id, type, dirtycellidx,
                                                  old, new, processed]),
                   io:format("Ret is ~p~n", [Ret1])
           end,
    lists:foreach(Fun1, Sites),
    ok.

flash_tests() ->
    ok = hn_setup:delete_site("http://tests.hypernumbers.dev:9000"),
    ok = hn_setup:site("http://tests.hypernumbers.dev:9000", blank, []).

flash_dev() ->
    ok = hn_setup:delete_site("http://hypernumbers.dev:9000"),
    ok = hypernumbers_app:local_hypernumbers().

flash_devsrv() ->
    ok = hn_setup:delete_site("http://dev.hypernumbers.com:8080"),
    ok = hypernumbers_app:local_srv_hypernumbers(),
    ok = write_twilio_dev_kvs().

fix_zinf_local_obj_bug() ->
    Sites = hn_setup:get_sites(),
    Fun = fun(X, Acc) ->
                  NewAcc = case X#local_obj.obj of
                               {cell, {0, 0}} -> io:format("found ~p~n", [X]),
                                                 [X| Acc];
                               _              -> Acc
                           end,
                  NewAcc
          end,
    Fun1 = fun(Site) ->
                   Tbl = hn_db_wu:trans(Site, local_obj),
                   io:format("about to check ~p~n", [Tbl]),
                   Fun2 = fun() ->
                                  mnesia:foldl(Fun, [], Tbl)
                          end,
                   Recs = mnesia:activity(transaction, Fun2),
                   [begin
                        Fun3 = fun() ->
                                       mensia:delete_object(X)
                               end,
                        mnesia:activity(transaction, Fun3)
                    end || X <- Recs]
           end,
    lists:foreach(Fun1, Sites).

add_include_index() ->
    Sites = hn_setup:get_sites(),
    Fun1 = fun(Site) ->
                   Tbl = hn_db_wu:trans(Site, include),
                   Ret = mnesia:add_table_index(Tbl, path),
                   io:format("Ret is ~p~n", [Ret])
           end,
    lists:foreach(Fun1, Sites).

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
    Tbl = new_db_wu:trans(Site, Record),
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

