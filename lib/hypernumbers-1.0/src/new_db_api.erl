%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       new db work rewrites
%%%            old hn_db_api.erl poured in and rewritten
%%% @end
%%% Created :  5 Apr 2011 by gordon@hypernumbers.com

-module(new_db_api).

-include("spriki.hrl").
-include("hypernumbers.hrl").
-include("passport.hrl").
-include("keyvalues.hrl").
-include("syslib.hrl").
-include("errvals.hrl").

-export([
         write_user_to_cacheD/1,
         uid_to_emailD/1,
         email_to_uidD/1,
         write_commission_logD/1,
         read_commission_logD/1,
         get_unprocessed_commissionsD/0,
         run_zevalD/3,
         any_adminD/1,
         set_usersD/3,
         rem_userD/3,
         add_userD/3,
         get_a_users_groups/2,
         delete_groupD/2,
         create_groupD/2,
         is_memberD/3,
         get_all_groups/1,
         get_groups/1,
         rollback_dirty_cacheD/1,
         clear_dirty_cacheD/1,
         revert/3,
         clear_factory/1,
         make_factory/1,
         make_factory/2,
         mark_site_dirty/1,
         delete_siteonly/2,
         read_siteonly/2,
         write_siteonly/3,
         idx_to_xrefX/2,
         get_phone/1,
         get_phone/2,
         does_page_exist/1,
         delete_api/2,
         write_api/2,
         read_api/2,
         get_api_keys/1,
         delete_user_fn/2,
         write_user_fn/2,
         read_user_fn/2,
         mark_idx_dirty/2,
         read_timers/1,
         read_includes/1,
         write_kv/3,
         read_kv/2,
         delete_kv/2,
         kvs_exportD/1,
         read_pages/1,
         read_page_structure/1,
         read_intersect_ref/1,
         handle_form_post/3,
         append_row/3,
         read_ref/1,
         read_attribute/2,
         recalc/1,
         matching_forms/2,
         write_attributes/1,
         write_attributes/2,
         write_attributes/3,
         handle_dirty_cell/3,
         delete/2, delete/3,
         insert/2, insert/3,
         copy_n_paste/4,
         drag_n_drop/3,
         % cut_n_paste/3,
         clear/3,
         set_borders/5
        ]).

% fun for smoothing delete
-export([
         clear_by_rows/4,
         clear_by_rows/5,
         delete_by_rows/3,
         delete_by_rows/4
        ]).

% the write_activity fun is exposed for use in debugging only
% should not be called directly from outside this module
-export([
         write_activity_DEBUG/3
        ]).

%% will force a web site to recalc completes
-export([
         recalc_site_EMERGENCYD/1,
         clear_all_queues_EMERGENCYD/1
        ]).

% fns for logging
-export([
         get_logs/1
        ]).

-export([
         length_dirty_zinf_q/1,
         reset_dirty_zinfs/1,
         maybe_write_zinftree/3,
         wait_for_dirty/1,
         process_dirties_for_zinf/3,
         process_dirty_zinfs/4,
         load_dirty_since/2,
         handle_circref_cell/3
        ]).

-export([
         read_styles_IMPORT/1
        ]).

-define(wu, new_db_wu).
-define(sq_bra, 91).
-define(sq_ket, 93).
-define(E, error_logger:error_msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% API Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_user_to_cacheD(User) ->
    Fun = fun() ->
                  ok = mnesia:write(passport_cached, User, write)
          end,
    mnesia:activity(transaction, Fun).

uid_to_emailD(UID) ->
    Fun = fun() ->
                  mnesia:read(passport_cached, UID, read)
          end,
    case mnesia:activity(transaction, Fun) of
        [U] -> {ok, U#user.email};
        _   -> {error, not_in_cache}
    end.

email_to_uidD(Email) ->
    Fun = fun() ->
                  mnesia:index_read(passport_cached, Email, #user.email)
          end,
    case mnesia:activity(transaction, Fun) of
        [U] -> {ok, U#user.uid};
        _   -> {error, not_in_cache}
    end.

write_commission_logD(Commission) ->
    Table = commission,
    Fun = fun() ->
                  ok = mnesia:write(Table, Commission, write)
          end,
    mnesia:activity(transaction, Fun).

read_commission_logD(Uid) ->
    Table = commission,
    Fun = fun() ->
                  mnesia:index_read(Table, Uid, uid)
          end,
    mnesia:activity(transaction, Fun).

get_unprocessed_commissionsD() ->
    Table = commission,
    Fun = fun() ->
                  mnesia:index_read(Table, false, synched)
          end,
    mnesia:activity(transaction, Fun).

run_zevalD(Site, Path, Z) ->
    Z2 = string:strip(string:strip(Z, right, ?sq_ket), left, ?sq_bra),
    % this expression is not 'real' so we run it in a non-existent cell
    % {cell, {0, 0}} is 1 up and 1 left of the cell 'A1'
    {ok, Toks} = xfl_lexer:lex(Z2, {0, 0}),
    % need to set up the process dictionary
    Fun = fun() ->
                  try
                      muin:external_zeval(Site, Path, Toks)
                  catch
                      error:
                      Err  -> ?E("Zseg ~p on ~p and ~p failed: ~p~n",
                                 [Toks, Site, Path, Err]),
                              ?ERRVAL_VAL;
                      exit:
                      Exit -> ?E("Zseg ~p on ~p and ~p failed: ~p~n",
                                 [Toks, Site, Path, Exit]),
                              ?ERRVAL_VAL
                  end
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

any_adminD(Site) ->
    Tbl = ?wu:trans(Site, group),
    Fun = fun() ->
                  case mnesia:read(Tbl, "admin", read) of
                      [#group{members = M}] ->
                          case gb_sets:is_empty(M) of
                              false -> {uid, gb_sets:smallest(M)};
                              true  -> no_admin
                          end;
                      _ ->
                          no_group
                  end
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

set_usersD(Site, Users, GroupN) ->
    Tbl = ?wu:trans(Site, group),
    Fun = fun() ->
                  case mnesia:read(Tbl, GroupN, write) of
                      [G] ->
                          Members = gb_sets:from_list(Users),
                          G2 = G#group{members = Members},
                          mnesia:write(Tbl, G2, write),
                          ?wu:mark_site_dirtyD(Site);
                      _ ->
                          no_group
                  end
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

get_a_users_groups(Site, Uid) ->
    Fun1 = fun() ->
                  ?wu:groupsD(Site)
          end,
    {ok, Email} = passport:uid_to_email(Uid),
    Groups = mnesia:activity(transaction, Fun1),
    Fun2 = fun({X, List}, Acc) ->
                  case lists:member(Email, List) of
                      true  -> [X | Acc];
                      false -> Acc
                  end
          end,
    lists:foldl(Fun2, [], Groups).

rem_userD(Site, Uid, GroupN) ->
    Tbl = ?wu:trans(Site, group),
    Fun = fun() ->
                  case mnesia:read(Tbl, GroupN, write) of
                      [G] ->
                          Members = gb_sets:delete_any(Uid, G#group.members),
                          G2 = G#group{members = Members},
                          mnesia:write(Tbl, G2, write),
                          ?wu:mark_site_dirtyD(Site);
                      _ ->
                          no_group
                  end
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

add_userD(Site, Uid, GroupN) ->
    Tbl = ?wu:trans(Site, group),
    Fun = fun() ->
                  case mnesia:read(Tbl, GroupN, write) of
                      [G] ->
                          Members = gb_sets:add(Uid, G#group.members),
                          G2 = G#group{members=Members},
                          ok = mnesia:write(Tbl, G2, write),
                          ?wu:mark_site_dirtyD(Site);
                      _ ->
                          no_group
                  end
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

delete_groupD(Site, GroupN) ->
    Tbl = ?wu:trans(Site, group),
    Fun = fun() ->
                  mnesia:delete(Tbl, GroupN, write),
                  ?wu:mark_site_dirtyD(Site)
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

create_groupD(Site, GroupN) ->
    Tbl = ?wu:trans(Site, group),
    Fun = fun() ->
                  case mnesia:read(Tbl, GroupN, write) of
                      [] ->
                          Group = #group{name = GroupN},
                          mnesia:write(Tbl, Group, write),
                          ?wu:mark_site_dirtyD(Site);
                      _ ->
                          ok
                  end
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

is_memberD(Site, Uid, Groups) ->
    {ok, Email} = passport:uid_to_email(Uid),
    Tbl = ?wu:trans(Site, group),
    Fun = fun() ->
                  is_member1(Groups, Tbl, Email, Uid)
          end,
    mnesia:activity(transaction, Fun).

is_member1([], _Tbl, _Email, _Uid) -> false;
is_member1([GroupN | Rest], Tbl, Email, Uid) ->
    %% check if the Group is the Email first
    case GroupN of
        Email ->
            true;
        _ ->
            case mnesia:read(Tbl, GroupN) of
                [G] ->
                    case gb_sets:is_member(Uid, G#group.members) of
                        true  -> true;
                        false -> is_member1(Rest, Tbl, Email, Uid)
                    end;
                _ ->
                    is_member1(Rest, Tbl, Email, Uid)
            end
    end.

get_all_groups(Site) ->
    Fun = fun() ->
                  ?wu:all_groupsD(Site)
          end,
    mnesia:activity(transaction, Fun).

get_groups(Site) ->
    Fun = fun() ->
                  ?wu:groupsD(Site)
          end,
    mnesia:activity(transaction, Fun).

rollback_dirty_cacheD(Site) ->
    Tbl = ?wu:trans(Site, dirty_queue),
    TblC = ?wu:trans(Site, dirty_queue_cache),
    Fun1 = fun(#dirty_queue_cache{id = I, dirty = D, auth_req = A}) ->
                   #dirty_queue{id = I, dirty = D, auth_req = A}
           end,
    Fun2 = fun() ->
                   Recs = mnesia:match_object(TblC, #dirty_queue_cache{_='_'},
                                              write),
                   [ok = mnesia:write(Tbl, Fun1(X), write) || X <- Recs]
           end,
    mnesia:activity(transaction, Fun2),
    % can't be called within the transaction - but doens't matter
    {atomic, ok} = mnesia:clear_table(TblC),
    ok.

clear_dirty_cacheD(Site) ->
    Tbl = ?wu:trans(Site, dirty_queue_cache),
    {atomic, ok} = mnesia:clear_table(Tbl),
    ok.

revert(#refX{} = RefX, Revision, UID) ->
    Fun = fun() ->
                  ?wu:revertD(RefX, Revision, UID)
          end,
    write_activity(RefX, Fun, "quiet").

clear_factory(Site) ->
    Fun = fun() ->
                  ?wu:delete_kvD(Site, factory),
                  ?wu:mark_site_dirtyD(Site)
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

make_factory(Site) ->
    make_factory2(Site, all).

make_factory(Site, List) when is_list(List) ->
    make_factory2(Site, List).

make_factory2(Site, Terms) ->
    Fun = fun() ->
                  ?wu:write_kvD(Site, factory, Terms),
                  ?wu:mark_site_dirtyD(Site)
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

mark_site_dirty(Site) ->
    Fun = fun() ->
                  ?wu:mark_site_dirtyD(Site)
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

-spec wait_for_dirty(string()) -> ok.
wait_for_dirty(Site) ->
    case dbsrv:is_busy(Site) of
        true ->
            timer:sleep(100),
            wait_for_dirty(Site);
        false ->
            ok
    end.

mark_idx_dirty(Site, Idx) ->
    Fun1 = fun() ->
                   ?wu:idx_to_xrefXD(Site, Idx)
           end,
    XRefX = mnesia:activity(transaction, Fun1),
    Fun2 = fun() ->
                   ok = ?wu:mark_these_dirtyD([XRefX], nil)
           end,
    RefX = hn_util:xrefX_to_refX(XRefX),
    write_activity(RefX, Fun2, "quiet").

read_timers(Site) ->
    Fun = fun() ->
                  ?wu:read_timersD(Site)
          end,
    mnesia:activity(transaction, Fun).

read_includes(#refX{obj = {page, "/"}} = RefX) ->
    Fun = fun() ->
                  XRefX = ?wu:refX_to_xrefX_createD(RefX),
                  unpack_incs(?wu:read_incsD(XRefX))
          end,
    mnesia:activity(transaction, Fun).

-spec handle_circref_cell(string(), cellidx(), auth_srv:auth_spec()) -> ok.
handle_circref_cell(Site, Idx, Ar) ->
    Fun = fun() ->
                  Cell = ?wu:idx_to_xrefXD(Site, Idx),
                  _Dct = ?wu:write_attrs(Cell, [{"formula", "=#CIRCREF!"}], Ar),
                  ok
          end,
    mnesia:activity(transaction, Fun).

get_logs(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  ?wu:get_logsD(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% Loads new dirty information into the recalc graph.
-spec load_dirty_since(term(), atom()) -> {term(), [cellidx()]}.
load_dirty_since(Site, Since) ->
    F = fun() ->
                ?wu:load_dirty_sinceD(Site, Since)
        end,
    case mnesia:activity(transaction, F) of
        []  -> {Since, []};
        Ret -> {SinceL, DirtyLL} = lists:unzip(Ret),
               Since2 = lists:max(SinceL),
               DirtyL = lists:usort(lists:flatten(DirtyLL)),
               {Since2, DirtyL}
    end.

% this fn is used in subscribing/unsubscribe operations for dirty_zinf
length_dirty_zinf_q(Site) ->
    Fun  = fun() ->
                   ?wu:len_dirty_zinf_qD(Site)
           end,
    mnesia:activity(transaction, Fun).

%% this function marks all dirty zinfs as unprocessed
%% designed to be used for zinf_srv initing in case of
%% a hard failure AND NOT FOR ANY OTHER REASON
reset_dirty_zinfs(Site) ->
    Fun  = fun() ->
                   ?wu:reset_dirty_zinfsD(Site)
           end,
    mnesia:activity(transaction, Fun).

%% this function decides to maybe write the zinf tree
%% depending on the log queue
maybe_write_zinftree(Site, Tree, MaxSize) ->
    Tbl = ?wu:trans(Site, dirty_zinf),
    Size = mnesia:table_info(Tbl, size),
    if
        Size > MaxSize ->
            Fun  = fun() ->
                           ?wu:maybe_write_zinftreeD(Site, Tree)
                   end,
            mnesia:activity(transaction, Fun);
        Size =< MaxSize ->
            ok
    end.

process_dirty_zinfs(Site, Tree, AddFun, DelFun) ->
    Fun = fun() ->
                  ?wu:proc_dirty_zinfsD(Site, Tree, AddFun, DelFun)
          end,
    mnesia:activity(transaction, Fun).

process_dirties_for_zinf(Site, Tree, CheckFun) ->
    Fun = fun() ->
                  ?wu:proc_dirties_for_zinfD(Site, Tree, CheckFun)
          end,
    % using a spoof RefX because it is not going to the front end
    % (change that from a quiet report and watch it die!)
    RefX = #refX{site = Site},
    write_activity(RefX, Fun, "quiet"),
    ok.

idx_to_xrefX(Site, Idx) ->
    Fun = fun() ->
                  ?wu:idx_to_xrefXD(Site, Idx)
          end,
    mnesia:activity(transaction, Fun).

delete_siteonly(#refX{} = RefX, Type) ->
    Fun = fun() ->
                  ?wu:delete_siteonlyD(RefX, Type)
          end,
    mnesia:activity(transaction, Fun).

read_siteonly(#refX{} = RefX, Type) ->
    Fun = fun() ->
                  ?wu:read_siteonlyD(RefX, Type)
          end,
    mnesia:activity(transaction, Fun).

write_siteonly(#refX{} = RefX, Type, Payload) ->
    Fun = fun() ->
                  % this is a 'write once' record can only be
                  % one of each type per site
                  case ?wu:read_siteonly(RefX, Type) of
                      [] ->
                          ?wu:write_siteonlyD(RefX, Type, Payload);
                      [_R] ->
                          {error, exists}
                  end
          end,
    mnesia:activity(transaction, Fun).

get_phone(Site, Idx) ->
    Fun = fun() ->
                  ?wu:get_phone(Site, Idx)
          end,
    mnesia:activity(transaction, Fun).

get_phone(#refX{obj = {cell, _}} = RefX) ->
    Fun = fun() ->
                  ?wu:get_phone(RefX)
          end,
    mnesia:activity(transaction, Fun).

does_page_exist(#refX{obj = {page, "/"}} = RefX) ->
    Fun = fun() ->
                  ?wu:does_page_exist(RefX)
          end,
    mnesia:activity(transaction, Fun).

delete_api(Site, PublicKey) ->
    Fun = fun() ->
                  ?wu:delete_apiD(Site, PublicKey),
                  ?wu:mark_site_dirtyD(Site)
          end,
    mnesia:activity(transaction, Fun).

write_api(Site, #api{} = API) ->
    Fun = fun() ->
                  ?wu:write_apiD(Site, API),
                  ?wu:mark_site_dirtyD(Site)
          end,
    mnesia:activity(transaction, Fun).

get_api_keys(Site) ->
    Fun = fun() ->
                  ?wu:get_api_keysD(Site)
          end,
    mnesia:activity(transaction, Fun).

read_api(Site, PublicKey) ->
    Fun = fun() ->
                  ?wu:read_apiD(Site, PublicKey)
          end,
    mnesia:activity(transaction, Fun).

delete_user_fn(Site, FnName) ->
    Fun = fun() ->
                  ?wu:delete_user_fnD(Site, FnName)
          end,
    mnesia:activity(transaction, Fun).

write_user_fn(Site, #user_fns{} = Fn) ->
    Fun = fun() ->
                  ?wu:write_user_fnD(Site, Fn)
          end,
    mnesia:activity(transaction, Fun).

read_user_fn(Site, FnName) ->
    Fun = fun() ->
                  ?wu:read_user_fnD(Site, FnName)
          end,
    mnesia:activity(transaction, Fun).

kvs_exportD(Site) ->
    Fun = fun() ->
                  Tbl = new_db_wu:trans(Site, kvstore),
                  Keys = mnesia:all_keys(Tbl),
                  Fun2 = fun(X) ->
                                 [{kvstore, X, Val}] = mnesia:read(Tbl, X, read),
                                 {X, Val}
                         end,
                  [Fun2(X) || X <- Keys, X =/= auth_srv
                                     andalso X =/= pages
                                     andalso X =/= zinf_tree]
          end,
    mnesia:activity(transaction, Fun).

write_kv(Site, Key, Value) ->
    Fun = fun() ->
                  ?wu:write_kvD(Site, Key, Value)
          end,
    mnesia:activity(transaction, Fun).

read_kv(Site, Key) ->
    Fun = fun() ->
                  ?wu:read_kvD(Site, Key)
          end,
    mnesia:activity(transaction, Fun).

delete_kv(Site, Key) ->
    Fun = fun() ->
                  ?wu:delete_kvD(Site, Key)
          end,
    mnesia:activity(transaction, Fun).

read_pages(RefX) when is_record(RefX, refX) ->
    page_srv:get_flatpages(RefX#refX.site).

%% @doc reads pages
%% @todo fix up api
%% we are doing tree -> flat -> tree here
%% smellee! :(
read_page_structure(#refX{site = Site}) ->
    Pages = page_srv:get_flatpages(Site),
    filter_pages(Pages, dh_tree:new()).

-spec read_intersect_ref(#refX{}) -> [{#refX{}, [{string(), term()}]}].
read_intersect_ref(RefX) ->
    Fun = fun() ->
                  ?wu:read_ref(RefX, intersect)
          end,
    read_activity(RefX, Fun).

read_styles_IMPORT(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  ?wu:read_styles_IMPORTD(RefX)
          end,
    read_activity(RefX, Fun).

handle_form_post(#refX{site = S, path = P,
                       obj = {row, {1, 1}}} = RefX,
                 Array, PosterUid) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = page_srv:page_written(S, P),
                  % Labels from the results page
                  LabXs = ?wu:expand_ref(RefX),
                  OldLabs = [?wu:read_ref_field(X, "__rawvalue", read)
                             || X <- LabXs],
                  OldLabs2 = lists:flatten(OldLabs),
                  % if this is the first time that a form post is
                  % occurring then set the default view to 'table'
                  % basically if there ain't no lables its a first post
                  if
                      OldLabs2 ==  [] ->
                          auth_srv:set_champion(S, P, "table");
                      OldLabs2 =/= [] ->
                          ok
                  end,
                  Values = [ {"submitted", dh_date:format("d/m/Y h:i:s")} |
                             lists:reverse(lists:foldl(fun generate_labels/2,
                                                       [], Array)) ],
                  LastCol = get_last_col(OldLabs2),
                  {NewLabels, NVals} =
                      allocate_values(S, P, Values, OldLabs2, RefX, LastCol),
                  NLbls = [ {Lref, [{"formula", Val}]}
                            || {Lref, Val} <- NewLabels],
                  [begin
                       XRefX = ?wu:refX_to_xrefX_createD(X),
                       _Dict = ?wu:write_attrs(XRefX, A, PosterUid)
                   end || {X, A} <- NLbls],
                  Row = ?wu:get_last_row(RefX) + 1,
                  F = fun(X, Val) ->
                              Obj = {cell, {X, Row}},
                              RefX2 = #refX{site = S, type = url, path = P,
                                            obj = Obj},
                              XRefX2 = ?wu:refX_to_xrefX_createD(RefX2),
                              _Dict = ?wu:write_attrs(XRefX2,
                                                      [{"formula", Val}],
                                                      PosterUid),
                              ok = ?wu:mark_these_dirtyD([XRefX2], PosterUid)
                      end,
                  [F(X, V) || {#refX{site = S1, type = gurl, path = P1,
                                     obj = {column, {X, X}}}, V}
                                  <- NVals, S == S1, P == P1]
          end,

    write_activity(RefX, Fun, "write last").

append_row([], _PAr, _VAr) -> ok;
append_row(List, PAr, VAr) when is_list(List) ->

    % all the refX's in the list must have the same site/path/object type
    {RefX=#refX{site = S, path = P},_} = hd(List),
    Trans =
        fun() ->
                ok = init_front_end_notify(),
                ok = page_srv:page_written(S, P),
                Row = ?wu:get_last_row(RefX) + 1,
                F = fun(X, Val) ->
                            Obj = {cell, {X, Row}},
                            RefX2 = #refX{site = S, type = url, path = P,
                                          obj = Obj},
                            XRefX2 = ?wu:refX_to_xrefX_createD(RefX2),
                            _Dict = ?wu:write_attrs(XRefX2, [{"formula", Val}],
                                                    PAr),
                            ok = ?wu:mark_these_dirtyD([XRefX2], VAr)
                    end,
                [F(X, V) || {#refX{site = S1, type = gurl, path = P1,
                                   obj = {column, {X, X}}}, V}
                                <- List, S == S1, P == P1]
        end,

    write_activity(RefX, Trans, "write last").

-spec matching_forms(#refX{}, common | string()) -> [#form{}].
matching_forms(RefX, Transaction) ->
    Fun = fun() ->
                  ?wu:matching_formsD(RefX, Transaction)
          end,
    read_activity(RefX, Fun).

-spec set_borders(#refX{}, any(), any(), any(), any()) -> ok.
%% @doc  takes a range or cell reference and sets the borders
%% for the range according
%% to the borders parameter passed in
%% The borders attribute can be one of:
%% <ul>
%% <li>surround</li>
%% <li>inside</li>
%% <li>none</li>
%% <li>call</li>
%% <li>left</li>
%% <li>top</li>
%% <li>right</li>
%% <li>bottom</li>
%% </ul>
%% all borders except 'none' set the border in a postive fashion - they
%% don't toggle. So if a particular cell has a left border set then setting
%% its border 'left' means it will *still* have its left border set
%% by contrast 'none' tears all borders down.
%% Border_Color is a colour expressed as a hex string of format "#FF0000"
%% Border_Style can be one of

%% for a cell just switch it to a range
set_borders(#refX{obj = {cell, {X, Y}}} = RefX, Type, Border, Style, Color) ->
    set_borders(RefX#refX{obj = {range, {X, Y, X, Y}}},
                Type, Border, Style, Color);
%% now proper set borders
set_borders(#refX{obj = {range, _}} = RefX, "none",_Border,
            _Border_Style, _Border_Color) ->
    ok = set_borders2(RefX, "left",   [], [], []),
    ok = set_borders2(RefX, "right",  [], [], []),
    ok = set_borders2(RefX, "top",    [], [], []),
    ok = set_borders2(RefX, "bottom", [], [], []),
    ok;

set_borders(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, Where,
            Border, B_Style, B_Color)
  when Where == "left"
       orelse Where == "right"
       orelse Where == "top"
       orelse Where == "bottom" ->
    NewObj = case Where of
                 "left"   -> {range, {X1, Y1, X1, Y2}};
                 "right"  -> {range, {X2, Y1, X2, Y2}};
                 "top"    -> {range, {X1, Y1, X2, Y1}};
                 "bottom" -> {range, {X1, Y2, X1, Y2}}
             end,
    NewRefX = RefX#refX{obj = NewObj},
    ok = set_borders2(NewRefX, Where, Border, B_Style, B_Color);

set_borders(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX,
            Where, Border, B_Style, B_Color)
  when Where == "surround" ->
    Top    = RefX#refX{obj = {range, {X1, Y1, X2, Y1}}},
    Bottom = RefX#refX{obj = {range, {X1, Y2, X2, Y2}}},
    Left   = RefX#refX{obj = {range, {X1, Y1, X1, Y2}}},
    Right  = RefX#refX{obj = {range, {X2, Y1, X2, Y2}}},
    ok = set_borders2(Top,    "top",    Border, B_Style, B_Color),
    ok = set_borders2(Bottom, "bottom", Border, B_Style, B_Color),
    ok = set_borders2(Left,   "left",   Border, B_Style, B_Color),
    ok = set_borders2(Right,  "right",  Border, B_Style, B_Color),
    ok;

set_borders(#refX{obj = {range, _}} = RefX, Where, Border, B_Style, B_Color)
  when Where == "all" ->
    ok = set_borders2(RefX, "top",    Border, B_Style, B_Color),
    ok = set_borders2(RefX, "bottom", Border, B_Style, B_Color),
    ok = set_borders2(RefX, "left",   Border, B_Style, B_Color),
    ok = set_borders2(RefX, "right",  Border, B_Style, B_Color),
    ok;

%% there are a number of different function heads for 'inside'
%% 'inside' on a cell does nothing
set_borders(#refX{obj = {range, {X1, Y1, X1, Y1}}} = _RefX,
            Where, _Border, _B_Style, _B_Color)
  when Where == "inside" ->
    ok;
%% 'inside' a single column
set_borders(#refX{obj = {range, {X1, Y1, X1, Y2}}} = RefX,
            Where, Border, B_Style, B_Color)
  when Where == "inside" ->
    NewRefX = RefX#refX{obj = {range, {X1, Y1 + 1, X1, Y2}}},
    ok = set_borders2(NewRefX, "top", Border, B_Style, B_Color);
%% 'inside' a single row
set_borders(#refX{obj = {range, {X1, Y1, X2, Y1}}} = RefX,
            Where, Border, B_Style, B_Color)
  when Where == "inside" ->
    NewRefX = RefX#refX{obj = {range, {X1 + 1, Y1, X2, Y1}}},
    ok = set_borders2(NewRefX, "left", Border, B_Style, B_Color);
%% proper 'inside'
set_borders(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX,
            Where, Border, B_Style, B_Color)
  when Where == "inside" ->
    NewRefX1 = RefX#refX{obj = {range, {X1, Y1 + 1, X2, Y2}}},
    ok = set_borders2(NewRefX1, "top", Border, B_Style, B_Color),
    NewRefX2 = RefX#refX{obj = {range, {X1 + 1, Y1, X2, Y2}}},
    ok = set_borders2(NewRefX2, "left", Border, B_Style, B_Color).

set_borders2(#refX{site = S, path = P} = RefX, Where, Border,
             B_Style, B_Color) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = page_srv:page_written(S, P),
                  B   = "border-" ++ Where ++ "-width",
                  B_S = "border-" ++ Where ++ "-style",
                  B_C = "border-" ++ Where ++ "-color",
                  _ = ?wu:write_attrs(RefX, [{B,   Border}]),
                  _ = ?wu:write_attrs(RefX, [{B_S, B_Style}]),
                  _ = ?wu:write_attrs(RefX, [{B_C, B_Color}])
          end,
    write_activity(RefX, Fun, "set_borders2").

-spec read_ref(#refX{}) -> [{#refX{}, term()}].
read_ref(#refX{obj = {cell, _}} = RefX) ->
    Fun = fun() ->
                  ?wu:read_ref(RefX, inside, read)
          end,
    read_activity(RefX, Fun).

-spec read_attribute(#refX{}, string()) -> [{#refX{}, term()}].
read_attribute(RefX, Field) when is_record(RefX, refX) ->
    Fun = fun() ->
                  XRefX = ?wu:refX_to_xrefX_createD(RefX),
                  ?wu:read_ref_field(XRefX, Field, read)
          end,
    read_activity(RefX, Fun).

clear_all_queues_EMERGENCYD(Site) ->
    %% clear all the dirty queues
    io:format("Clearing all the dirty queues of ~p~n",
              [Site]),
    Tbl1 = new_db_wu:trans(Site, dirty_for_zinf),
    Tbl2 = new_db_wu:trans(Site, dirty_queue_cache),
    Tbl3 = new_db_wu:trans(Site, dirty_queue),
    Tbl4 = new_db_wu:trans(Site, dirty_zinf),
    {atomic, ok} = mnesia:clear_table(Tbl1),
    {atomic, ok} = mnesia:clear_table(Tbl2),
    {atomic, ok} = mnesia:clear_table(Tbl3),
    {atomic, ok} = mnesia:clear_table(Tbl4),
    ok.

recalc_site_EMERGENCYD(Site) ->
    Tbl = ?wu:trans(Site, item),
    Fun = fun() ->
                  Idxs = mnesia:all_keys(Tbl),
                  io:format("Marking ~p cells on ~p dirty~n",
                            [length(Idxs), Site]),
                  [ok = new_db_wu:mark_these_idxs_dirtyD([X], Site, nil)
                   || X <- Idxs],
                  ok
          end,
    %% use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet").

recalc(#refX{} = RefX) ->
    Fun = fun() ->
                  XRefX = ?wu:refX_to_xrefX_createD(RefX),
                  Refs = ?wu:read_ref(XRefX, inside),
                  Refs2 = [X || {X, L} <- Refs, L =/= []],
                  ok = ?wu:mark_these_dirtyD(Refs2, nil)
          end,
    write_activity(RefX, Fun, "refresh").

clear(#refX{obj = {page, "/"}} = RefX, Type, Ar) ->
    % page clear are too 'big' cause massive memory spikes
    % and dirty cell rushes and message queues to go through
    % the roof, so we don't do them atomically.
    %
    % Instead we clear the page row-by-row starting with the last
    % row.
    Fun1 = fun() ->
                   ?wu:get_last_row(RefX)
           end,
    NoOfRows = mnesia:activity(transaction, Fun1),
    ok = clear_by_rows(NoOfRows, 0, RefX, Type, Ar),
    % finally clear the page to get rid of rows and columns
    Fun2 = fun() ->
                   ok = init_front_end_notify(),
                   ok = ?wu:clear_rows_and_colsD(RefX, Ar)
           end,
    write_activity(RefX, Fun2, "clear");
clear(#refX{} = RefX, Type, Ar) ->
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                ok = ?wu:clear_cells(RefX, Type, Ar)
        end,
    write_activity(RefX, Fun, "clear").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% server side drag'n'drop                                                    %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc copies the formula and formats from a cell or range and
%% pastes them to the destination then deletes the original.
%%
%% (see also {@link new_db_api:drag_n_drop/2}
%% and {@link new_db_api:copy_n_paste/2} -
%% the difference between drag'n'drop and copy'n'paste or cut'n'paste is that
%% drag'n'drop increments)
%%
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li>
%% </ul>
%%
%% If a range is to be cut'n'pasted to a range then one of the following
%% criteria MUST be true:
%% <ul>
%% <li>the <b>from</b> range must be the same dimensions as the
%% <b>to</b> range</li>
%% <li>the <b>from</b> range must be one cell high and the same width as the
%% <b>to</b> range</li>
%% <li>the <b>from</b> must be one cell wide and the same height as the
%% <b>to</b> range</li>
%% </ul>
%%
%% @todo cut'n'paste a page
%% cut_n_paste(From, To, Ar) when
%%       is_record(From, refX), is_record(To, refX) ->
%%     Fun = fun() ->
%%                   ok = init_front_end_notify(),
%%                   ok = copy_n_paste2(From, To, all, Ar),
%%                   ok = clear(From, all, Ar)
%%           end,
%%     write_activity(From, Fun, "cut n paste").

%% @doc copies the formula and formats from a cell or range and
%% pastes them to the destination.
%%
%% (see also {@link new_db_api:drag_n_drop/2}
%% and {@link new_db_api:cut_n_paste/2} -
%% the difference between drag'n'drop and copy'n'paste or cut'n'paste is that
%% drag'n'drop increments)
%%
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul><li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li></ul>
%%
%% Also whole pages can be copy_n_pasted by making both From and To
%% page refX's
-spec copy_n_paste(#refX{}, #refX{}, all | style | value,
                   auth_srv:auth_spec()) -> ok.
copy_n_paste(From, #refX{site = ToS, path = ToP} = To, What, Ar) when
is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = page_srv:page_written(ToS, ToP),
                  ok = copy_n_paste2(From, To, What, Ar)
          end,
    write_activity(From, Fun, "copy n paste").

%% @doc takes the formula and formats from a cell and drag_n_drops
%% them over a destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
%%
%% (see also {@link new_db_api:cut_n_paste/2}
%% and {@link new_db_api:copy_n_paste/2} -
%% the difference between drag'n'drop and copy'n'paste or cut'n'paste is that
%% drag'n'drop increments)
%%
%% drag'n'drop has an interesting specification
%% (taken from Excel 2007 help)
%% currently excludes customer autofill
%%
%% <code>Initial selection       Extended series</code>
%%
%% <code>-----------------       ---------------</code>
%%
%% <code>1, 2, 3                 4, 5, 6,... </code>
%%
%% <code>9:00 10:00,             11:00, 12:00,... </code>
%%
%% <code>Mon Tue,                Wed, Thu,... </code>
%%
%% <code>Monday Tuesday,         Wednesday, Thursday,... </code>
%%
%% <code>Jan Feb,                Mars, Apr,... </code>
%%
%% <code>Jan, Apr                Jul, Oct, Jan,... </code>
%%
%% <code>Jan-07, Apr-07          Jul-07, Oct-07, Jan-08,... </code>
%%
%% <code>15-Jan, 15-Apr          15-Jul, 15-Oct,... </code>
%%
%% <code>2007, 2008              2009, 2010, 2011,... </code>
%%
%% <code>1-Jan, 1-Mar            1-May, 1-Jul, 1-Sep,... </code>
%%
%% <code>Qtr3                    Qtr4, Qtr1, Qtr2,... </code>
%%
%% <code>Q3                      Q4, Q1, Q2,... </code>
%%
%% <code>Quarter3                Quarter4, Quarter1, Quarter2,... </code>
%%
%% <code>text1, textA text2,     textA, text3, textA,... </code>
%%
%% <code>1st Period              2nd Period, 3rd Period,... </code>
%%
%% <code>Product 1               Product 2, Product 3,... </code>
%%
%% <code>1 Product               2 Product, 3 Product</code>
%%
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul><li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li></ul>
%%
%% If a range is to be drag'n'dropped to a range then
%% one of the following criteria MUST be true:
%% <ul><li>the <b>from</b> range must be the same dimensions as the
%% <b>to</b> range</li>
%% <li>the <b>from</b> range must be the same width as the
%% <b>to</b> range</li>
%% <li>the <b>from</b> must be the same height as the
%% <b>to</b> range</li></ul>
drag_n_drop(From, #refX{site = ToS, path = ToP} = To, Ar)
  when is_record(From, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = page_srv:page_written(ToS, ToP),
                  case is_valid_d_n_d(From, To) of
                      {ok, 'onto self', _Incr} -> ok;
                      {ok, single_cell, Incr} ->
                          copy_cell(From, To, Incr, all, Ar);
                      {ok, cell_to_range, Incr} ->
                          copy2(From, To, Incr, all, Ar)
                  end
          end,
    ok = write_activity(From, Fun, "drag n drop").

%% @todo This needs to check if it intercepts a shared formula
%% and if it does it should fail...
insert(#refX{obj = {column, _}} = RefX, Ar) ->
    move(RefX, insert, horizontal, Ar, false);
insert(#refX{obj = {row, _}} = RefX, Ar) ->
    move(RefX, insert, vertical, Ar, false);
insert(#refX{obj = R} = RefX, Ar)
  when R == cell orelse R == range ->
    move(RefX, insert, vertical, Ar, false).

%% The Type variable determines how the insert displaces the existing cases...
insert(#refX{obj = {R, _}} = RefX, Disp, Ar)
  when is_record(RefX, refX), (R == cell orelse R == range),
       (Disp == horizontal orelse Disp == vertical)->
    move(RefX, insert, Disp, Ar, false).

%% @doc deletes a column or a row or a page
%%
%% @todo this is all bollocks - should be row, column then cell/range as
%% per insert/2.
%% This needs to check if it intercepts a shared formula
%% and if it does it should fail...
-spec delete(#refX{}, auth_srv:auth_spec()) -> ok.
delete(#refX{obj = {R, _}} = RefX, Ar) when R == cell orelse R == range ->
    move(RefX, delete, vertical, Ar, false);
delete(#refX{obj = {column, _}} = RefX, Ar)  ->
    move(RefX, delete, horizontal, Ar, false);
delete(#refX{obj = {row, {_Min, _Max}}} = RefX, Ar) ->
    move(RefX, delete, vertical, Ar, false);
delete(#refX{site = S, path = P, obj = {page, _}} = RefX, Ar) ->
    % page deletes are too 'big' cause massive memory spikes
    % and dirty cell rushes and message queues to go through
    % the roof, so we don't do them atomically.
    %
    % Instead we delete the page row-by-row starting with the last
    % row.
    %
    % Need to do jiggery-pokery to make the logs look OK
    Fun1 = fun() ->
                   % log it as a page delete in this transaction
                   % bit of a cheat...
                   ?wu:log_page(RefX, 'page deleted', Ar),
                   ?wu:get_last_row(RefX)
           end,
    NoOfRows = mnesia:activity(transaction, Fun1),
    ok = delete_by_rows(NoOfRows, 0, RefX, Ar),
    % now do a 'normal' delete to clear off the rows and columns
    Fun2 = fun() ->
                   ReWr = ?wu:delete_cells(RefX, vertical, Ar),
                   ok = ?wu:mark_these_dirtyD(ReWr, Ar)
           end,
    % because we have cleared all the cells that have to be deleted
    % in advance we expect delete_cells to return an empty list
    ok = mnesia:activity(transaction, Fun2),
    ok = page_srv:page_deleted(S, P).

%% @doc deletes a reference.
%%
%% The <code>refX{}</code> can be one of a:
%% <ul>
%% <li>cell</li>
%% <li>row</li>
%% <li>column</li>
%% <li>range</li>
%% </ul>
%%
%% For all refs except those to a page this function deletes the cells
%% and closes up the rest of them. If Disp is Horizontal it moves
%% cells right-to-left to close the gap. If Disp is vertical is moves
%% cells bottom-to-top to close the gap
delete(#refX{obj = {R, _}} = RefX, Disp, Ar)
  when R == cell orelse R == range ->
    move(RefX, delete, Disp, Ar, false).

delete_by_rows(N, Min, RefX) -> delete_by_rows(N, Min, RefX, nil).

delete_by_rows(Min, Min, _, _) -> ok;
delete_by_rows(N, Min, #refX{site = _S} = RefX, Ar) ->
    NewRefX = RefX#refX{obj = {row, {N, N}}},
    move(NewRefX, delete, vertical, Ar, false),
    %syslib:limiter(S),
    delete_by_rows(N - 1, Min, RefX, Ar).

clear_by_rows(N, Min, RefX, Type) -> clear_by_rows(N, Min, RefX, Type, nil).

clear_by_rows(Min, Min, _, _, _) -> ok;
clear_by_rows(N, Min, #refX{site = _S} = RefX, Type, Ar) ->
    NewRefX = RefX#refX{obj = {row, {N, N}}},
    clear(NewRefX, Type, Ar),
    %syslib:limiter(S),
    clear_by_rows(N - 1, Min, RefX, Type,Ar).

-spec handle_dirty_cell(string(), cellidx(), auth_srv:auth_spec()) -> list().
handle_dirty_cell(Site, Idx, Ar) ->
    ok = init_front_end_notify(),
    Fun =
        fun() ->
                % check if the cell has been deleted
                case ?wu:has_cell_been_deletedD(Site, Idx) of
                    true ->
                        [];
                    false ->
                        Attrs = case ?wu:read_itemD(Site, Idx) of
                                    [#item{attrs = A}] -> binary_to_term(A);
                                    _                  -> orddict:new()
                                end,
                        Cell = ?wu:idx_to_xrefXD(Site, Idx),
                        case orddict:find("formula", Attrs) of
                            {ok, F} ->
                                _ = ?wu:write_attrs(Cell, [{"formula", F}], Ar);
                            _ ->
                                %% handle range caching
                                case orddict:find(range, Attrs) of
                                    {ok, _R} ->
                                        %% delete the range because its children
                                        %% will then refetch it
                                        ok = new_db_wu:delete_itemD(Cell);
                                    _ ->
                                        ok
                                end
                        end,
                        ok = handle_dirty_c2(Cell, Attrs, Ar),
                        % cells may have been written that now depend
                        % on this cell so it needs to report back
                        % dirty children
                        [Rel] = ?wu:read_relations(Cell, read),
                        Ch = Rel#relation.children,
                        ?wu:mark_these_idxs_dirtyD(Ch, Site, Ar)
                end
        end,
    ok = mnesia:activity(transaction, Fun),
    tell_front_end("handle dirty", #refX{}),
    ok.

handle_dirty_c2(Cell, Attrs, Ar) ->
    case orddict:find("input", Attrs) of
        {ok, I} ->
            % need to reset the dynamic select as if
            % it hasn't calculated yet so that the
            % recalc will 'fire' on writing
            case I of
                "inline" ->
                    % normal inlines don't have vals to recalcrecalc
                    ok;
                "inlinerich" ->
                    % rich inlines don't have vals to recalcrecalc
                    ok;
                "none" ->
                    % used to have an input but it's been cleared
                    ok;
                {"select", _} ->
                    % normal select's don't recalc their vals
                    ok;
                {"dynamic_select", S, _Vals} ->
                    NewI = [{"input", {"dynamic_select", S}}],
                    _Dict2 = ?wu:write_attrs(Cell, NewI, Ar)
            end,
            ok;
        _  ->
            ok
    end.

%% @spec write_attributes(RefX :: #refX{}, List) -> ok
%% List = [{Key, Value}]
%% Key = atom()
%% Value = term()
%% @doc writes out all the attributes in the list to the reference.
%%
%% The <code>refX{}</code> can be
%% one of:
%% <ul>
%% <li>a cell</li>
%% <li>a range</li>
%% </ul>

-spec write_attributes([{#refX{}, [tuple()]}]) -> ok.
write_attributes(List) ->
    write_attributes(List, nil, nil).

-spec write_attributes([{#refX{}, [tuple()]}], auth_srv:auth_spec()) -> ok.
write_attributes(List, Uid) ->
    write_attributes(List, Uid, nil).

write_attributes([], _PAr, _VAr) -> ok;
write_attributes(List, PAr, VAr) ->
    [ok = page_srv:page_written(S, P) ||
        {#refX{site = S, path = P}, _} <- List],
    Pgs = hslists:uniq([{S, P} || {#refX{site = S, path = P}, _} <- List]),
    [ok = page_srv:page_written(S, P) || {S, P} <- Pgs],
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  List2 = expand(List),
                  [ok = write_attributes1(XR, A2, PAr, VAr)
                   || {XR, A2} <- List2],
                  ok
          end,
    % assumes all refX's are for the same site and page, hmmm...
    {Ref, _} = hd(List),
    write_activity(Ref, Fun, "write attrs").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand(List) ->
    {R, A} = lists:unzip(List),
    expand2(R, A, []).

expand2([], [], Acc) -> Acc;
expand2([R | TR], [A | TA], Acc) ->
    R2 = ?wu:refXs_to_xrefXs_create([R]),
    NewAcc = case length(R2) of
                 1 -> [R3] = R2,
                      {R3, A};
                 N -> A2 = lists:duplicate(N, A),
                      lists:zip(R2, A2)
             end,
    expand2(TR, TA, lists:flatten([NewAcc | Acc])).

write_attributes1(#xrefX{} = XRefX, List, PAr, VAr) ->
    ?wu:write_attrs(XRefX, List, PAr),
    % first up do the usual 'dirty' stuff - this cell is dirty
    case lists:keymember("formula", 1, List) of
        true  -> ?wu:mark_these_dirtyD([XRefX], VAr);
        false -> ok
    end,
    % now do the include dirty stuff (ie this cell has had it's format updated)
    % so make any cells that use '=include(...)' on it redraw themselves
    case lists:keymember("__in_includeFn", 1, List) of
        true -> ?wu:mark_dirty_for_inclD([XRefX], VAr);
        false -> ok
    end.

init_front_end_notify() ->
    _Return = put('front_end_notify', []),
    ok.

-spec read_activity(#refX{}, fun()) -> any().
read_activity(#refX{site = _Site}, Op) ->
    Activity = fun() ->
                       mnesia:activity(transaction, Op)
               end,
    Activity().
%dbsrv:read_only_activity(Site, Activity).

% expose for debugging only
write_activity_DEBUG(RefX, Op, FrontEnd) ->
    write_activity(RefX, Op, FrontEnd).

-spec write_activity(#refX{}, fun(), string()) -> ok.
write_activity(#refX{site = Site} = RefX, Op, FrontEnd) ->
    % still keep the notification of the db server for the mo
    Activity = fun() ->
                       Ret = mnesia:activity(transaction, Op),
                       tell_front_end(FrontEnd, RefX),
                       Ret
               end,
    dbsrv:write_activity(Site, Activity).

tell_front_end("quiet", _RefX) ->
    ok;
tell_front_end(Type, #refX{path = P} = RefX)
  when Type == "move" orelse Type == "refresh" ->
    Notifications = get('front_end_notify'),
    % the move or refresh notifications are used when a page is changed
    % radically - they tell the front end to request the whole page
    % but if there is a formula on another page referring to a cell on a page
    % with, say an insert or delete, then that page has its formulae
    % rewritten/recalculated that page needs to get details notifications
    % so we pull them out of the process dictionary and fire 'em off here...
    Fun = fun(X) ->
                  case X of
                      {_, #xrefX{path = P}, _} -> false;
                      _                        -> true
                  end
          end,
    Extra = lists:filter(Fun, Notifications),
    Notifications = put('front_end_notify', Extra),
    remoting_reg:notify_refresh(RefX#refX.site, RefX#refX.path),
    % now run the extra notifications
    tell_front_end(extras, RefX);
tell_front_end(_FnName, _RefX) ->
    List = lists:reverse(get('front_end_notify')),
    Fun = fun({change, #xrefX{site = S, path = P, obj = {page, "/"}}, _Attrs}) ->
                  remoting_reg:notify_refresh(S, P);
             ({change, #xrefX{site = S, path = P, obj = O}, Attrs}) ->
                  remoting_reg:notify_change(S, P, O, Attrs);
             ({style, #xrefX{site = S, path = P}, Style}) ->
                  remoting_reg:notify_style(S, P, Style);
             ({delete_attrs, #xrefX{site = S, path = P, obj = O}, Attrs}) ->
                  remoting_reg:notify_delete_attrs(S, P, O, Attrs)
          end,
    [ok = Fun(X) || X <- List],
    ok.

move(RefX, Type, Disp, Ar, LogChange)
  when (Type == insert orelse Type == delete)
       andalso (Disp == vertical orelse Disp == horizontal) ->
    Fun = fun() ->
                  move_tr(RefX, Type, Disp, Ar, LogChange)
          end,
    write_activity(RefX, Fun, "move").

move_tr(RefX, Type, Disp, Ar, LogChange) ->
    ok = init_front_end_notify(),
    % if the Type is delete we first delete the original cells
    % when the move type is DELETE the cells that are moved
    % DO NOT include the cells described by the reference
    % but when the move type is INSERT the cells that are
    % move DO include the cells described by the reference
    % To make this work we shift the RefX up 1, left 1
    % before getting the cells to shift for INSERT
    % if this is a delete - we need to actually delete the cells
    case LogChange of
        true  -> ?wu:log_move(RefX, Type, Disp, Ar);
        false -> ok
    end,
    ReWr = do_delete(Type, RefX, Disp, Ar),
    ok = ?wu:shift_rows_and_columnsD(RefX, Type, Disp, Ar),
    MoreDirty = ?wu:shift_cellsD(RefX, Type, Disp, ReWr, Ar),
    %% TODO Mebbies remove - not sure if next line is needed?
    ok = ?wu:mark_these_dirtyD(MoreDirty, Ar).

do_delete(insert, _RefX, _Disp, _UId) ->
    [];
do_delete(delete, RefX, Disp, Uid) ->
    ?wu:delete_cells(RefX, Disp, Uid).

%% the last parameter returned is whether dates and integers should be
%% incremented this can only be true for a vertical or horizontal drag
%% (returning 'y' and 'x') or is otherwise false
%% cell to cell drag'n'drop
is_valid_d_n_d(#refX{obj = {cell, A}}, #refX{obj = {cell, A}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#refX{obj = {cell, {X, _Y1}}}, #refX{obj = {cell, {X, _Y2}}}) ->
    {ok, single_cell, vertical};
is_valid_d_n_d(#refX{obj = {cell, {_X1, Y}}}, #refX{obj = {cell, {_X2, Y}}}) ->
    {ok, single_cell, horizontal};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {cell, _}}) ->
    {ok, single_cell, false};
%% cell to range drag'n'drop
is_valid_d_n_d(#refX{obj = {cell, _}},
               #refX{obj = {range, {TX, _TY1, TX, _TY2}}}) ->
    {ok, cell_to_range, vertical};
is_valid_d_n_d(#refX{obj = {cell, _}},
               #refX{obj = {range, {_TX1, TY, _TX2, TY}}}) ->
    {ok, cell_to_range, horizontal};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range, false};
%% range to range drag'n'drop
is_valid_d_n_d(#refX{obj = {range, Range}}, #refX{obj = {range, Range}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#refX{obj = {range, {X, _FY1, X, _FY2}}},
               #refX{obj = {range, {X, _TY1, X, _TY2}}}) ->
    {ok, col_range_to_range};
is_valid_d_n_d(#refX{obj = {range, {_FX1, Y, _FX2, Y}}},
               #refX{obj = {range, {_TX1, Y, _TX2, Y}}}) ->
    {ok, row_range_to_range};
is_valid_d_n_d(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {error, "from range is invalid"};
is_valid_d_n_d(_, _) -> {error, "not valid either"}.

is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {cell, _}})  ->
    {ok, single_cell};
is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {cell, _}}) ->
    {ok, range_to_cell};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {ok, range_to_range}.

cell_to_range(#refX{obj = {cell, {X, Y}}} = RefX) ->
    RefX#refX{obj = {range, {X, Y, X, Y}}}.

-spec copy_cell(#refX{}, #refX{},
                false | horizontal | vertical,
                all | style | value,
                auth_srv:uid())
-> ok.
copy_cell(From = #refX{site = Site, path = Path}, To, Incr, What, Ar) ->
    case auth_srv:get_any_main_view(Site, Path, Ar) of
        {view, _} ->
            XFrom = ?wu:refX_to_xrefX_createD(From),
            XTo = ?wu:refX_to_xrefX_createD(To),
            ok = ?wu:copy_cell(XFrom, XTo, Incr, What, Ar),
            ok = ?wu:mark_these_dirtyD([XTo], Ar);
        denied ->
            % if they haven't got permssion we just silently ignore them
            ok
end.

copy_n_paste2(From, To, What, Ar) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    ->
            ok = copy_cell(From, To, false, What, Ar);
        {ok, cell_to_range} ->
            copy2(From, To, false, What, Ar);
        {ok, range_to_cell} ->
            To2 = cell_to_range(To),
            copy3(From, To2, false, What, Ar);
        {ok, range_to_range} ->
            copy3(From, To, false, What, Ar)
    end.

%% cell to range
copy2(From, To, Incr, What, Ar)
  when is_record(From, refX), is_record(To, refX) ->
    List = hn_util:range_to_list(To),
    [copy_cell(From, X, Incr, What, Ar) || X <- List],
    ok.

%% range to range
copy3(From, To, Incr, What, Ar)
  when is_record(From, refX), is_record(To, refX) ->
    % range to range copies are 'tiled'
    TileList = get_tiles(From, To),
    copy3a(From, TileList, Incr, What, Ar).

copy3a(_From, [], _Incr, _What, _Ar)   -> ok;
copy3a(From, [H | T], Incr, What, Ar) ->
    FromRange = hn_util:range_to_list(From),
    ToRange = hn_util:range_to_list(H),
    ok = copy3b(FromRange, ToRange, Incr, What, Ar),
    copy3a(From, T, Incr, What, Ar).

copy3b([], [], _Incr, _What, _Ar)             -> ok;
copy3b([FH | FT], [TH | TT], Incr, What, Ar) ->
    ok = copy_cell(FH, TH, Incr, What, Ar),
    copy3b(FT, TT, Incr, What, Ar).

get_tiles(#refX{obj = {range, {X1F, Y1F, X2F, Y2F}}},
          #refX{obj = {range, {X1T, Y1T, X2T, Y2T}}} = To) ->

    % this is a bit messy. Excel does the following things:
    % * if both ranges are congruent - 1 tile
    % * if the To range is smaller than the From range it writes the whole
    %   From range as a block into the range whose top left is the same
    %   as the To range - 1 tile
    % * if the To range is an exact multipe of the from range it
    %   tiles it
    % * if the To range is one column wide and a the height is a multiple
    %   of the From range then it tiles the From range vertically
    % * if the To range is one row high and the width is a multiple
    %   of the From range then it tiles the From range horizontally
    % * if the To range is not one of the above it writes the whole
    %   block into the range whose top left is the same as the To range
    %
    %   First up rectify the ranges
    {FX1, FY1, FX2, FY2} = hn_util:rectify_range(X1F, Y1F, X2F, Y2F),
    {TX1, TY1, TX2, TY2} = hn_util:rectify_range(X1T, Y1T, X2T, Y2T),
    FWidth  = FX2 - FX1 + 1,
    FHeight = FY2 - FY1 + 1,
    TWidth  = TX2 - TX1 + 1,
    THeight = TY2 - TY1 + 1,
    WidthMultiple = TWidth/FWidth - erlang:trunc(TWidth/FWidth),
    HeightMultiple = THeight/FHeight - erlang:trunc(THeight/FHeight),
    % if the to range is an exact multiple of the From range in both
    % dimensions then tile it, otherwise just copy the From range into
    % a range of the same size whose top left cell is that of the To range
    % capice?
    {WTile, HTile} = case {WidthMultiple, HeightMultiple} of
                         {0.0, 0.0} -> {erlang:trunc(TWidth/FWidth),
                                        erlang:trunc(THeight/FHeight)} ;
                         _          -> {1, 1}
                     end,
    get_tiles2(To, FWidth, FHeight, {WTile, HTile}).

get_tiles2(Ref, Width, Height, {WTile, HTile}) ->
    get_tiles2(Ref, Width, Height, {1, 1}, {WTile, HTile}, []).

%% has a special terminator for the single tile case
%% the algorith relies on you zigzagging over the body of the kirk:
%% * down the first column, increment the column, reset the row
%% * down the next column
%% * etc, etc,
%% which can't happen if you have 1 column and 1 row
get_tiles2(RefX, W, H, {WT, FT}, {WT, FT}, Acc) ->
    #refX{obj = {range, {X, Y, _, _}}} = RefX,
    SX = X + (WT - 1) * W,
    SY = Y + (FT - 1) * H,
    EX = SX + (W - 1),
    EY = SY + (H - 1),
    NewAcc = RefX#refX{obj = {range, {SX, SY, EX, EY}}},
    [NewAcc | Acc];
get_tiles2(RefX, W, H, {WT, M},  {WT, FT}, Acc) ->
    #refX{obj = {range, {X, Y, _, _}}} = RefX,
    SX = X + (WT - 1) * W,
    SY = Y + (M - 1) * H,
    EX = SX + (W - 1),
    EY = SY + (H - 1),
    NewAcc = RefX#refX{obj = {range, {SX, SY, EX, EY}}},
    get_tiles2(RefX, W, H, {1, M + 1}, {WT, FT}, [NewAcc | Acc]);
get_tiles2(RefX, W, H, {N, M},  {WT, FT}, Acc)  ->
    #refX{obj = {range, {X, Y, _, _}}} = RefX,
    SX = X + (N - 1) * W,
    SY = Y + (M - 1) * H,
    EX = SX + (W - 1),
    EY = SY + (H - 1),
    NewAcc = RefX#refX{obj = {range, {SX, SY, EX, EY}}},
    get_tiles2(RefX, W, H, {N + 1, M}, {WT, FT}, [NewAcc | Acc]).

get_last_col(Labels) ->
    case [X || {#xrefX{obj = {cell, {X, _}}}, _Val} <- Labels] of
        []   -> 0;
        List -> lists:max(List)
    end.

allocate({Label, Value}, {S, P, Labels, Index, Ref, NLabels, Refs}) ->
    % escape the values to prevent script injection etc, etc
    case lists:keyfind(Label, 2, Labels) of
        {#xrefX{obj = {cell, {X, _Y}}}, Label} ->
            % Label already exists
            {S, P, Labels, Index, Ref, NLabels,
             [{#refX{site = S, type = gurl, path = P,
                     obj = {column, {X, X}}}, Value} | Refs]};
        false  ->
            % Write new label
            X = Index + 1,
            {S, P, Labels, X, Ref,
             [{#refX{site = S, type = url, path = P,
                     obj = {cell, {X, 1}}}, Label} | NLabels],
             [{#refX{site = S, type = gurl, path = P,
                     obj = {column, {X, X}}}, Value} | Refs]}
    end.

allocate_values(S, P, Values, Labels, Ref, Index) ->
    {S, P, _Labels, _Index, _Ref, NLabels, Refs} =
        lists:foldl(fun allocate/2, {S, P, Labels, Index, Ref, [], []}, Values),
    {NLabels, Refs}.

generate_labels({struct,[{"label", []}, {"formula", Formula}]}, List) ->
    [{uniqify("default", List), Formula} | List];
generate_labels({struct,[{"label", Label}, {"formula", Formula}]}, List) ->
    [{uniqify(Label, List), Formula} | List].

-spec uniqify(string(), list()) -> string().
uniqify(Label, List) ->
    case lists:keyfind(Label, 1, List) of
        {Label, _Value} -> uniqify(Label, List, 2);
        false           -> Label
    end.

-spec uniqify(string(), list(), integer()) -> string().
uniqify(Label, List, Index) ->
    NLabel = Label ++ " " ++ integer_to_list(Index),
    case lists:keyfind(NLabel, 1, List) of
        {NLabel, _Value} -> uniqify(Label, List, Index+1);
        false            -> NLabel
    end.

filter_pages([], Tree) ->
    Tree;
filter_pages([Path | T], Tree) ->
    filter_pages(T, dh_tree:add(Path, Tree)).

unpack_incs(List) -> unpack_1(List, [], [], [], []).

unpack_1([], Js, Js_H, Js_r, CSS) ->
    {hslists:uniq(Js), hslists:uniq(Js_H),
     hslists:uniq(Js_r), hslists:uniq(CSS)};
unpack_1([H | T], Js, Js_H, Js_r, CSS) ->
    #include{js = J, js_head = Jh, js_reload = R, css = C} = H,
    NewJ = case J of
               [] -> Js;
               _  -> lists:append([J, Js])
           end,
    NewJs_H = case Jh of
                  [] -> Js_H;
                  _ -> lists:append([Jh, Js_H])
              end,
    NewR = case R of
               [] -> Js_r;
               _  -> lists:append([R, Js_r])
           end,
    NewC = case C of
               [] -> CSS;
               _  -> lists:append([C, CSS])
           end,
    unpack_1(T, NewJ, NewJs_H, NewR, NewC).
