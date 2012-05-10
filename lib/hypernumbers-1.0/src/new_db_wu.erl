%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       new db api (old hn_db_api poured in and rewritten)
%%%
%%% @end
%%% Created :  5 Apr 2011 by gordon@hypernumbers

-module(new_db_wu).

-include("syslib.hrl").
-include("keyvalues.hrl").
-include("spriki.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(to_refX, hn_util:refX_from_index).

-define(lt, list_to_tuple).
-define(lf, lists:flatten).
-define(dict, orddict:orddict()).
-define(TRANSFORMATIVE, true).
-define(NONTRANSFORMATIVE, false).

-export([
         delete_siteonlyD/2,
         read_siteonlyD/2,
         write_siteonlyD/3,
         get_phone/1,
         does_page_exist/1,
         has_cell_been_deletedD/2,
         proc_dirties_for_zinfD/3,
         proc_dirty_zinfsD/4,
         maybe_write_zinftreeD/2,
         len_dirty_zinf_qD/1,
         reset_dirty_zinfsD/1,
         read_timersD/1,
         has_forms/1,
         read_incsD/1,
         xrefX_to_rti/3,
         trans/2,
         get_prefix/1,
         delete_apiD/2,
         write_apiD/2,
         read_apiD/2,
         get_api_keysD/1,
         delete_user_fnD/2,
         write_user_fnD/2,
         read_user_fnD/2,
         write_kvD/3,
         read_kvD/2,
         delete_kvD/2,
         expand_ref/1,
         matching_formsD/2,
         refX_to_xrefXD/1,
         refXs_to_xrefXs_create/1,
         refX_to_xrefX_createD/1,
         write_attrs/2, write_attrs/3,
         read_ref/2, read_ref/3, read_ref_field/3,
         read_relations/2,
         read_relationsD/3,
         mark_users_and_groups_dirtyD/1,
         mark_these_dirtyD/2,
         mark_these_idxs_dirtyD/3,
         mark_dirty_for_inclD/2,
         idx_to_xrefXD/2,
         get_cell_for_muin/3,
         delete_cells/3,
         shift_cellsD/5,
         shift_rows_and_columnsD/4,
         clear_cells/2, clear_cells/3,
         copy_cell/5
        ]).

% fns for logging
-export([
         log_page/3,
         log_move/4,
         get_logsD/1
        ]).

-export([
         load_dirty_sinceD/2
        ]).

-export([
         read_styles_IMPORTD/1
        ]).

%% Structural Query Exports
-export([
         get_last_row/1,
         get_last_col/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% API Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_siteonlyD(#refX{site = S}, Type) ->
    Tbl = trans(S, siteonly),
    mnesia:delete(Tbl, Type, write).

read_siteonlyD(#refX{site = S}, Type) ->
    Tbl = trans(S, siteonly),
    mnesia:read(Tbl, Type, read).

write_siteonlyD(#refX{site = S} = RefX, Type, Payload) ->
    Tbl = trans(S, siteonly),
    XRefX = refX_to_xrefX_createD(RefX),
    Siteonly = #siteonly{type = Type, idx = XRefX#xrefX.idx, payload = Payload},
    mnesia:write(Tbl, Siteonly, write).

% see if there is a phone - might not be, mebbies the cell doesn't exist
get_phone(#refX{obj = {Type, _}} = RefX)
  when Type == cell orelse Type == range ->
    XRefXs = expand_ref(RefX),
    get_phone2(XRefXs, []).

get_phone2([], Acc) ->
    lists:flatten(lists:reverse(Acc));
get_phone2([XRefX | T], Acc) ->
    #xrefX{site = S, idx = Idx} = XRefX,
    Tbl = trans(S, phone),
    NewAcc = mnesia:read(Tbl, Idx, read),
    get_phone2(T, [NewAcc | Acc]).

does_page_exist(#refX{site = S, obj = {page , "/"}} = RefX) ->
    case read_objs(RefX, inside) of
        []   -> false;
        Recs -> do_items_existD(Recs, S)
    end.

do_items_existD([], _Site) -> false;
do_items_existD([#local_obj{idx = Idx, obj = {cell, _}} | T], Site) ->
    Tbl = trans(Site, item),
    case mnesia:read(Tbl, Idx, read) of
        []       -> do_items_existD(T, Site);
        [Items]  -> case Items of
                        [{"__in_includeFn", _}] ->
                            do_items_existD(T, Site);
                         _ ->
                            true
                    end
    end;
do_items_existD([_H | T], Site) ->
    do_items_existD(T, Site).

has_cell_been_deletedD(Site, Idx) ->
    Tbl = trans(Site, del_local),
    case mnesia:read(Tbl, Idx, read) of
        []   -> false;
        [_R] -> true
    end.

proc_dirties_for_zinfD(Site, Tree, CheckFun) ->
    Tbl = trans(Site, dirty_for_zinf),
    L = mnesia:dirty_match_object(Tbl, #dirty_for_zinf{_='_'}),
    case L of
        [] ->
            zinf_srv:subscribe_to_dirty_for_zinf(Site);
        _ ->
            L2 = shrink(L),
            Dirties = [CheckFun(Tree, X) || X <- L2],
            D1 = hslists:uniq(lists:flatten(Dirties)),
            % this construction takes the call to mark_these_idxs_dirtyD
            % out of cprof figures when you are profiling!
            ok = case D1 of
                     []   -> ok;
                     List -> mark_these_idxs_dirtyD(List, Site, nil)
                 end,
            [ok = mnesia:delete(Tbl, Id, write)
             || #dirty_for_zinf{id = Id} <- L],
            % now get the zinf server to check again
            ok = zinf_srv:check_zinfs(Site),
            ok
    end.

proc_dirty_zinfsD(Site, Tree, AddFun, DelFun) ->
    Tbl = trans(Site, dirty_zinf),
    Fun = fun(DirtyZinf, Tr) ->
                  #dirty_zinf{dirtycellidx = CI, old = OldP,
                              new = NewP} = DirtyZinf,
                  % expand the new and old parents
                  NewP2 = lists:zip(NewP, lists:duplicate(length(NewP), CI)),
                  OldP2 = lists:zip(OldP, lists:duplicate(length(OldP), CI)),
                  Add = ordsets:subtract(NewP2, OldP2),
                  Del = ordsets:subtract(OldP2, NewP2),
                  NewTree = lists:foldl(AddFun, Tr, Add),
                  NewTree2 = lists:foldl(DelFun, NewTree, Del),
                  NewTree2
          end,
    Spec = #dirty_zinf{_='_', processed = false},
    L = mnesia:match_object(Tbl, Spec, write),
    case L of
        [] ->
            zinf_srv:subscribe_to_dirty_zinf(Site),
            Tree;
        _ ->
            % need to apply the dirty zinfs in the order
            % they were added
            L2 = lists:sort(L),
            NewTree = lists:foldl(Fun, Tree, L2),
            [ok = mnesia:write(Tbl, X#dirty_zinf{processed = true},
                               write) || X  <- L],
            % now get the zinf server to check again
            ok = zinf_srv:process_zinfs(Site),
            NewTree
    end.

maybe_write_zinftreeD(Site, Tree) ->
    Tbl = trans(Site, dirty_zinf),
    ok = write_kvD(Site, ?zinf_tree, Tree),
    Spec = #dirty_zinf{_='_', processed = true},
    L = mnesia:match_object(Tbl, Spec, write),
    [ok = mnesia:delete(Tbl, Id, write)
     || #dirty_zinf{id = Id} <- L],
    ok.

len_dirty_zinf_qD(Site) ->
    Tbl = trans (Site, dirty_zinf),
    Spec = #dirty_zinf{_='_', processed = false},
    L = mnesia:match_object(Tbl, Spec, write),
    length(L).

reset_dirty_zinfsD(Site) ->
    Tbl = trans(Site, dirty_zinf),
    Spec = #dirty_zinf{_='_', processed = true},
    L = mnesia:match_object(Tbl, Spec, write),
    [ok = mnesia:write(Tbl, X#dirty_zinf{processed = false},
                       write) || X  <- L],
    ok.

read_timersD(Site) ->
    Tbl = trans(Site, timer),
    Spec = #timer{_ ='_'},
    mnesia:match_object(Tbl, Spec, read).

has_forms(#refX{} = RefX) ->
    XRefs = expand_ref(RefX),
    check2D(XRefs).

check2D([]) -> false;
check2D([#xrefX{site = S, idx = Idx} | T]) ->
    Table = trans(S, form),
    case mnesia:read(Table, Idx, read) of
        []   -> check2D(T);
        [_R] -> true
    end.

get_logsD(RefX = #refX{site = S, path = P}) when is_record(RefX, refX) ->
    XRefX = refX_to_xrefXD(RefX), % IGNORE
    Table = trans(S, logging),
    Logs1 = case XRefX of
                false -> make_blank(RefX, false);
                _     -> mnesia:read(Table, XRefX#xrefX.idx, read)
            end,
    Logs2 = mnesia:index_read(Table, hn_util:list_to_path(P), #logging.path),
    Logs3 = get_page_logs(Logs2),
    lists:merge(Logs1, Logs3).

load_dirty_sinceD(Since, QTbl) ->
    M = ets:fun2ms(fun(#dirty_queue{id = T, dirty = D})
                         when Since < T -> {T, D}
                   end),
    mnesia:select(QTbl, M, write).

delete_apiD(Site, PublicKey) ->
    Tbl = trans(Site, api),
    mnesia:delete(Tbl, PublicKey, write).

write_apiD(Site, API) ->
    Tbl = trans(Site, api),
    mnesia:write(Tbl, API, write).

read_apiD(Site, PublicKey) ->
    Tbl = trans(Site, api),
    mnesia:read(Tbl, PublicKey, read).

get_api_keysD(Site) ->
    Tbl = trans(Site, api),
    Pattern = mnesia:table_info(Tbl, wild_pattern),
    mnesia:match_object(Tbl, Pattern, read).

delete_user_fnD(Site, Fn) ->
    Tbl = trans(Site, user_fns),
    mnesia:delete(Tbl, Fn, write).

write_user_fnD(Site, Fn) ->
    Tbl = trans(Site, user_fns),
    mnesia:write(Tbl, Fn, write).

read_user_fnD(Site, Key) ->
    Tbl = trans(Site, user_fns),
    mnesia:read(Tbl, Key, read).

% clear_uniq needs to preserve the old record whilst making it not
% functional - need this for things like the function 'configure.email'
% where the fact that the email has been validated needs to be remembered
clear_uniq(Site, Key) ->
    [{kvstore, Key, {_Idx, Rec}}] = read_kvD(Site, Key),
    write_kvD(Site, Key, {deleted, Rec}).

write_kvD(Site, Key, Value) ->
    Tbl = trans(Site, kvstore),
    Rec = #kvstore{key = Key, value = Value},
    mnesia:write(Tbl, Rec, write).

read_kvD(Site, Key) ->
    Tbl = trans(Site, kvstore),
    mnesia:read(Tbl, Key, read).

delete_kvD(Site, Key) ->
    Tbl = trans(Site, kvstore),
    mnesia:delete(Tbl, Key, write).

read_styles_IMPORTD(#refX{site = Site}) ->
    Tbl = trans(Site, style),
    MS = ets:fun2ms(fun(X) -> X end),
    mnesia:select(Tbl, MS, write).

-spec get_last_row(#refX{}) -> integer().
get_last_row(#refX{site = S, path = P}) ->
    SelX = #refX{site = S, type = url, path = P, obj = {page, "/"}},
    Desc = lists:usort(fun ({A,_}, {B,_}) -> A > B end,
                       [{Y, LO} || LO = #local_obj{obj = {cell,{_,Y}}}
                                       <- read_objs(SelX, inside)]),
    largest_content(Desc, S).

-spec get_last_col(#refX{}) -> integer().
get_last_col(#refX{site = S, path = P}) ->
    SelX = #refX{site = S, type = url, path = P, obj = {page, "/"}},
    Desc = lists:usort(fun ({A,_}, {B,_}) -> A > B end,
                       [{X, LO} || LO = #local_obj{obj = {cell,{X,_}}}
                                       <- read_objs(SelX, inside)]),
    largest_content(Desc, S).

-spec matching_formsD(#refX{}, common | string()) -> [#form{}].
matching_formsD(#refX{site = Site, path = Path}, Trans) ->
    MS = [{#form{id = {Path, Trans, '_'}, _ = '_'}, [], ['$_']}],
    mnesia:select(trans(Site, form), MS).

%% %% @doc copys cells from a reference to a reference
-spec copy_cell(#refX{}, #refX{},
                false | horizontal | vertical,
                all | style | value, string()) -> ok.
copy_cell(From = #xrefX{obj = {cell, _}},
          To = #xrefX{obj = {cell, _}},
          Incr, value, Uid) ->
    SourceAttrs = case read_ref(From, inside, read) of
                      [{_, As}] -> As;
                      _         -> []
                  end,
    Attrs = case orddict:find("__rawvalue", SourceAttrs) of
                {ok, V} ->
                    orddict:store("formula", tconv:to_s(V), orddict:new());
                _ ->
                    orddict:new()
            end,
    copy_c2(From, To, Incr, Uid, Attrs);
copy_cell(From = #xrefX{obj = {cell, _}},
          To = #xrefX{obj = {cell, _}},
          _, style, Uid) ->
    SourceAttrs = case read_ref(From, inside, read) of
                      [{_, As}] -> As;
                      _         -> []
                  end,
    Op = fun(Attrs) -> {clean, copy_attributes(SourceAttrs, Attrs,
                                               ["style"])} end,
    apply_to_attrsD(To, Op, copy, Uid, ?NONTRANSFORMATIVE),
    ok;
copy_cell(From, To, Incr, all, Uid) ->
    Attrs = case read_ref(From, inside, read) of
                [{_, As}] -> As;
                _         -> []
            end,
    copy_c2(From, To, Incr, Uid, Attrs).

copy_c2(#xrefX{obj = {cell, {FX, FY}}},
        #xrefX{obj = {cell, {TX, TY}}} = To, Incr, Uid, Attrs) ->
    Formula = case orddict:find("formula", Attrs) of
                  {ok, V} -> superparser:process(V);
                  _       -> "" end,
    Formula2  =
        case Formula of
            {formula, F1} ->
                offset_formula(F1, {(TX - FX), (TY - FY)});
            [{Type, F1},  _A, _F] ->
                case Incr of
                    false  ->
                        case Type of
                            datetime ->
                                {datetime, D, T} = F1,
                                dh_date:format("d/m/Y", {D, T});
                            _ ->
                                tconv:to_s(F1)
                        end;
                    _Other -> %% Other can be a range of different values...
                        case Type of
                            int      ->
                                NewV = F1 + diff(FX, FY, TX, TY, Incr),
                                tconv:to_s(NewV);
                            datetime ->
                                {datetime, {Y, M , D}, T} = F1,
                                Date = calendar:date_to_gregorian_days(Y, M, D),
                                Date2 = Date + diff(FX, FY, TX, TY, Incr),
                                NewD = calendar:gregorian_days_to_date(Date2),
                                dh_date:format("d/m/Y", {NewD, T});
                            _ ->
                                tconv:to_s(F1)
                        end
                end;
            _ ->
                ""
        end,
    Attrs2 = copy_attributes(Attrs, orddict:new(), ["merge", "style"]),
    Attrs3 = orddict:store("formula", Formula2, Attrs2),
    write_attrs(To, Attrs3, Uid),
    ok.

%% @doc deletes the contents (formula/value) and the formats
%% of a cell (but doesn't delete the cell itself).
-spec clear_cells(#refX{}, auth_srv:uid()) -> ok.
clear_cells(RefX, Uid) -> clear_cells(RefX, contents, Uid).

-spec clear_cells(#refX{}, all | style | contents | tuple(), auth_srv:uid()) -> ok.
clear_cells(Ref, contents, Uid) ->
    do_clear_cells(Ref, [input | content_attrs()], clear, Uid);
clear_cells(Ref, all, Uid) ->
    do_clear_cells(Ref, ["style", "merge", "input" | content_attrs()], clear, Uid);
clear_cells(Ref, style, Uid) ->
    do_clear_cells(Ref, ["style", "merge"], ignore, Uid);
clear_cells(Ref, {attributes, DelAttrs}, Uid) ->
    do_clear_cells(Ref, DelAttrs, clear, Uid).

do_clear_cells(Ref, DelAttrs, Action, Uid) ->
    Op = fun(XRefX) ->
                 fun(Attrs) ->
                         case lists:keymember("formula", 1, Attrs) of
                             true ->
                                 % set relations to handle other cells
                                 ok = set_relationsD(XRefX, [], [], false),
                                 ok = clean_up(XRefX, Attrs),
                                 ok = mark_these_dirtyD([XRefX], nil);
                             false ->
                                 ok
                         end,
                         case is_dynamic_select(Attrs) of
                             true ->
                                 ok = clean_up_dynamic_select(XRefX);
                             false ->
                                 ok
                         end,
                         {clean, del_attributes(Attrs, DelAttrs)}
                 end
         end,
    XRefs = expand_ref(Ref),
    [apply_to_attrsD(X, Op(X), Action, Uid, ?NONTRANSFORMATIVE) || X <- XRefs],
    % now mark the refs dirty for zinfs
    [ok = mark_dirty_for_zinfD(X) || X <- XRefs],
    ok.

%% @doc takes a reference to a
%% <ul>
%% <li>page</li>
%% <li>row</li>
%% <li>column</li>
%% <li>range</li>
%% <li>cell</li>
%% </ul>
%% and then deletes all the cells including their indices in local_obj
%% and makes all cells that are their children throw a #ref! error
%% and deletes the links there the old cell was the child of another cell
%% @todo this is ineffiecient because it reads and then deletes each
%% record individually - if remoting_reg supported a {delete refX all}
%% type message it could be speeded up
-spec delete_cells(#refX{}, atom(), auth_srv:uid()) -> [#refX{}].
delete_cells(#refX{site = S} = DelX, Disp, Uid) ->
    case expand_ref(DelX) of
        % there may be no cells to delete, but there may be rows or
        % columns widths to delete...
        []     ->
            expunge_refsD(S, expand_to_rows_or_cols(DelX)),
            [];
        Cells  ->
            % update the children that point to the cell that is
            % being deleted by rewriting the formulae of all the
            % children cells replacing the reference to this cell
            % with #ref!
            % THESE NOW RETURN IDX's!
            LocalChildren = [get_childrenD(C) || C <- Cells],
            LocalChildren2 = hslists:uniq(lists:flatten(LocalChildren)),

            % sometimes a cell will have local children that are also
            % in the delete zone these need to be removed before we
            % do anything else...
            LocalChildren3 = lists:subtract(LocalChildren2, Cells),

            % now clean up includes, timers and forms.
            [ok = clean_up(X) || X <- Cells],

            % Rewrite formulas
            Status = [deref_formula(X, DelX, Disp, Uid) || X <- LocalChildren3],
            Fun = fun({dirty, _Ref}) -> true; ({clean, _Ref}) -> false end,
            Dirty = [X || {dirty, X} <- lists:filter(Fun, Status)],
            ok = mark_these_dirtyD(Dirty, nil),

            % fix relations table.
            [ok = delete_relationD(X) || X <- Cells],

            % mark 'em dirty for zinf as well
            [ok = mark_dirty_for_zinfD(X) || X <- Cells],

            % Delete the rows or columns and cells (and their indices)
            expunge_refsD(S, lists:append(expand_to_rows_or_cols(DelX), Cells)),
            LocalChildren3
    end.

%% This function takes a list of refX's (including ranges) and converts them
%% all to xrefX
-spec refXs_to_xrefXs_create([#refX{}]) -> [#xrefX{}].
refXs_to_xrefXs_create(List) when is_list(List) ->
    lists:flatten([refXs2(X) || X <- List]).

refXs2(#refX{obj = {range, _}} = RefX) ->
    [refXs2(X) || X <- hn_util:range_to_list(RefX)];
refXs2(RefX) -> refX_to_xrefX_createD(RefX).

%% refX_to_xrefX reads the index of an object AND RETURNS 'false'
%% IF IT DOESN'T EXIST
-spec refX_to_xrefXD(#refX{}) -> #xrefX{} | false.
refX_to_xrefXD(#refX{site = S, path = P, obj = O}) ->
    Table = trans(S, local_obj),
    RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
    Pattern = {local_obj, '_', '_', '_', '_', term_to_binary(RevIdx)},
    case mnesia:index_match_object(Table, Pattern, 6, read) of
        [I]  -> #xrefX{idx = I#local_obj.idx, site = S, path = P, obj = O};
        []   -> false;
        List -> %error_logger:error_msg("local_obj table screwed up for ~p:~n~p",
            %                    [RefX, List]),
            L2 = lists:reverse(lists:sort(List)),
            [I2 | _] = L2,
            #xrefX{idx = I2#local_obj.idx, site = S, path = P, obj = O}
    end.

-spec refX_to_xrefX_createD(#refX{}) -> #xrefX{}.
%% @doc refX_to_xrefX_create refX_to_xrefX_create gets the index of an object
%% AND CREATES IT IF IT DOESN'T EXIST
refX_to_xrefX_createD(#refX{site = S, type = Ty, path = P, obj = O} = RefX)
when Ty == url orelse Ty == gurl ->
    case refX_to_xrefXD(RefX) of
        false -> Idx = util2:get_timestamp(),
                 RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
                 Rec = #local_obj{path = term_to_binary(P), type = Ty, obj = O,
                                  idx = Idx, revidx = term_to_binary(RevIdx)},
                 ok = mnesia:write(trans(S, local_obj), Rec, write),
                 #xrefX{idx = Idx, site = S, path = P, obj = O};
        XrefX -> XrefX
    end.

-spec write_attrs(#xrefX{}, [{string(), term()}]) -> ?dict.
write_attrs(XRefX, NewAttrs) -> write_attrs(XRefX, NewAttrs, nil).

-spec write_attrs(#xrefX{}, [{string(), term()}], auth_srv:auth_spec()) -> ?dict.
write_attrs(#xrefX{site = S} = XRefX, NewAs, AReq)
  when is_record(XRefX, xrefX) ->
    Op = fun(Attrs) ->
                 Is_Formula  = lists:keymember("formula", 1, NewAs),
                 Has_Form    = orddict:is_key("__hasform", Attrs),
                 Has_Incs    = orddict:is_key("__hasincs", Attrs),
                 Has_Timer   = orddict:is_key("__hastimer", Attrs),
                 Has_Phone   = orddict:is_key("__hasphone", Attrs),
                 Is_In_IncFn = orddict:is_key("__in_includeFn", Attrs),
                 Has_Uniq    = orddict:is_key("__unique", Attrs),
                 Has_Users   = orddict:is_key("__users", Attrs),
                 Is_Dynamic  = is_dynamic_select(Attrs),
                 Attrs2 =
                     case Is_Formula of
                         true ->
                             A3 = case Has_Form of
                                      true ->
                                          unattach_formD(XRefX),
                                          orddict:erase("__hasform", Attrs);
                                      false  ->
                                          Attrs
                                  end,
                             A4 = case Has_Incs of
                                      true ->
                                          delete_incsD(XRefX),
                                          orddict:erase("__hasincs", A3);
                                      false  ->
                                          A3
                                  end,
                             A5 = case Has_Timer of
                                      true ->
                                          delete_timerD(XRefX),
                                          orddict:erase("__hastimer", A4);
                                      false ->
                                          A4
                                  end,
                             A6 = case Has_Phone of
                                      true ->
                                          unattach_phoneD(XRefX),
                                          orddict:erase("__hasphone", A5);
                                      false ->
                                          A5
                                  end,
                             A7 = case Has_Uniq of
                                      true ->
                                          Unique = orddict:fetch("__unique", A6),
                                          ok = clear_uniq(S, Unique),
                                          orddict:erase("__unique", A6);
                                      false ->
                                          A6
                                  end,
                             A8 = case Has_Users of
                                      true ->
                                          unattach_usersD(XRefX),
                                          orddict:erase("__users", A6);
                                      false ->
                                          A7
                                  end,
                             % erase the preview from the attributes
                             orddict:erase("preview", A8);
                         false ->
                             case Is_Dynamic of
                                 true ->
                                     ok = clean_up_dynamic_select(XRefX),
                                     orddict:erase("input", Attrs);
                                 false ->
                                     Attrs
                             end
                             end,
                 case Is_In_IncFn of
                     true  -> mark_children_dirtyD(XRefX, AReq);
                     false -> ok
                 end,
                 {clean, process_attrs(NewAs, XRefX, AReq, Attrs2)}
         end,
    apply_to_attrsD(XRefX, Op, write, AReq, ?NONTRANSFORMATIVE).

mark_users_and_groups_dirtyD(Site) ->
    Tbl = trans(Site, users_and_groups),
    Idxs = mnesia:all_keys(Tbl),
    mark_these_idxs_dirtyD(Idxs, Site, nil).

mark_children_dirtyD(#xrefX{site = S, idx = Idx}, AReq) ->
    Tbl = trans(S, relation),
    case mnesia:read(Tbl, Idx, read) of
        []    -> ok;
        [Rec] -> #relation{children = C} = Rec,
                 mark_these_idxs_dirtyD(C, S, AReq)
    end.

-spec mark_these_idxs_dirtyD(list(), atom(),auth_srv:auth_spec()) -> ok.
mark_these_idxs_dirtyD([], _Site, _) -> ok;
mark_these_idxs_dirtyD(Idxs, Site, AReq) ->
    Entry = #dirty_queue{dirty = Idxs, auth_req = AReq},
    mnesia:write(trans(Site, dirty_queue), Entry, write).

-spec mark_these_dirtyD([#xrefX{}], auth_srv:auth_spec()) -> ok.
mark_these_dirtyD([], _) -> ok;
mark_these_dirtyD(Refs = [#xrefX{site = Site}|_], AReq) ->
    Idxs = lists:flatten([C#xrefX.idx || R <- Refs, C <- expand_ref(R)]),
    case Idxs of
        [] -> ok;
        _  -> Entry = #dirty_queue{dirty = Idxs, auth_req = AReq},
              mnesia:write(trans(Site, dirty_queue), Entry, write)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec process_attrs([{string(), term()}], #xrefX{}, auth_srv:auth_spec(),
                    ?dict) -> ?dict.
process_attrs([], _XRefX, _AReq, Attrs) ->
    Attrs;
process_attrs([{"formula",Val} | Rest], XRefX, AReq, Attrs) ->
    Attrs2 = case superparser:process(Val) of
                 {formula, Fla} ->
                     write_formula1(XRefX, Fla, Val, AReq, Attrs);
                 [NVal, Align, Frmt] ->
                     write_formula2(XRefX, Val, NVal, Align, Frmt, Attrs)
             end,
    ok = mark_dirty_for_zinfD(XRefX),
    process_attrs(Rest, XRefX, AReq, Attrs2);
process_attrs([{"input", {"dynamic_select", S}} | Rest], XRefX, AReq, Attrs) ->
    {SelPath, Ref} = split_select(S),
    SelPath2 = case SelPath of
                   "/" -> "";
                   _   -> SelPath
               end,
    Path = muin_util:walk_path(XRefX#xrefX.path, SelPath2),
    % it is not possible to check the url client-side so we need to do
    % a try/catch here to handle user duff user input...
    {NA, Ps, InfPs} = parse_select(XRefX, {Path, Ref}, S),
    ok = set_dyn_relationsD(XRefX, Ps, InfPs),
    process_attrs([NA | Rest], XRefX, AReq, Attrs);
process_attrs([A = {Key, Val} | Rest], XRefX, AReq, Attrs) ->
    Attrs2 = case ms_util2:is_in_record(magic_style, Key) of
                 true  -> apply_style(XRefX, A, Attrs);
                 false -> orddict:store(Key, Val, Attrs)
             end,
    process_attrs(Rest, XRefX, AReq, Attrs2).

parse_select(XRefX, {Path, Ref}, S) ->
    try
        % this is what will blow up if it does...
        process_dyn(XRefX, {Path, Ref}, S)
    catch
        error: _Error ->
            {{"input", {"dynamic_select", S, ["#ERROR!"]}}, [], []};
        exit: _Error ->
            {{"input", {"dynamic_select", S, ["#ERROR!"]}}, [], []};
        throw: {errval, Err} ->
            {{"input", {"dynamic_select", S, [Err]}}, [], []}
    end.

process_dyn(#xrefX{obj = {cell, {X, Y}}} = XRefX, {Path, Ref}, Select) ->
    Z = hn_util:list_to_path(Path) ++ Ref,
    case muin:parse(Z, {X, Y}) of
        {ok, AST} ->
            Rti = xrefX_to_rti(XRefX, nil, false),
            Vals = muin:fetch_for_select(AST, Rti),
            Vals2 = lists:delete(blank, hslists:uniq(Vals)),
            Vals3 = [tconv:to_s(XX) || XX <- Vals2],
            % lists:delete don't mind if the value already exists...
            % sooo, get rid of any blanks
            Vals4 = lists:sort(Vals3),
            {_Errors, References} = get(retvals),
            FiniteRefs = [{XX, L} || {XX, _, L} <- References],
            InfiniteRefs = get(infinite),
            case get(circref) of
                true  ->
                    {{"input", {"dynamic_select", Select, ["#CIRCREF!"]}},
                      [], []};
                false ->
                    {{"input", {"dynamic_select", Select, Vals4}},
                     FiniteRefs, InfiniteRefs}
            end;
        _Err ->
            {{"input", {"dynamic_select", Select, ["#ERR!"]}}, [], []}
    end.

write_formula1(XRefX, Fla, Formula, AReq, Attrs) ->
    Rti = xrefX_to_rti(XRefX, AReq, false),
    case muin:run_formula(Fla, Rti) of
        % General error condition
        {error, {errval, Error}} ->
            write_error_attrs(Attrs, XRefX, Formula, Error);
        % a special return
        {ok, {Pcode, #spec_val{} = Res, Parents, InfParents,
              Recompile, CircRef}} ->
            Attrs2 = proc_special(Res, XRefX, Attrs),
            write_formula_attrs(Attrs2, XRefX, Formula, Pcode,
                                Res#spec_val.val,
                                {Parents, Res#spec_val.include}, InfParents,
                                Recompile, CircRef);
        {ok, {Pcode, {webcontrol, {Payload, {Title, Wd, Ht, Incs}}, Res},
              Parents, InfParents, Recompile, CircRef}} ->
            {Trans, Label} = Payload#form.id,
            Form = Payload#form{id = {XRefX#xrefX.path, Trans, Label}},
            ok = attach_formD(XRefX, Form),
            Attrs2 = orddict:store("__hasform", t, Attrs),
            Blank = #incs{},
            Attrs3 = case Incs of
                         Blank -> orddict:erase("__hasincs", Attrs2);
                         _     -> ok = update_incsD(XRefX, Incs),
                                  orddict:store("__hasincs", t, Attrs2)
                     end,
            Attrs4 = orddict:store("preview", {Title, Wd, Ht}, Attrs3),
            Attrs5 = handle_merge(Ht, Wd, Attrs4),
            write_formula_attrs(Attrs5, XRefX, Formula, Pcode, Res,
                                {Parents, false}, InfParents,
                                Recompile, CircRef);
        % the formula returns a phone control
        {ok, {Pcode, {phone, {PreV, Wd, Ht, Payload}, Res},
              Parents, InfParents, Recompile, CircRef}} ->
            ok = attach_phoneD(XRefX, Payload),
            Attrs2 = orddict:store("preview", {PreV, Wd, Ht}, Attrs),
            Attrs3 = orddict:store("__hasphone", t, Attrs2),
            Attrs4 = handle_merge(Ht, Wd, Attrs3),
            write_formula_attrs(Attrs4, XRefX, Formula, Pcode, Res,
                                {Parents, false}, InfParents,
                                Recompile, CircRef);
        % the formula returns a web-hingie that needs to be previewed
        {ok, {Pcode, {preview, {PreV, Wd, Ht, Incs}, Res}, Pars,
              InfPars, Recompile, CircRef}} ->
            Attrs2 = orddict:store("preview", {PreV, Wd, Ht}, Attrs),
            Blank = #incs{},
            Attrs3 = case Incs of
                         Blank -> orddict:erase("__hasincs", Attrs2);
                         _     -> ok = update_incsD(XRefX, Incs),
                                  orddict:store("__hasincs", t, Attrs2)
                     end,
            Attrs4 = handle_merge(Ht, Wd, Attrs3),
            write_formula_attrs(Attrs4, XRefX, Formula, Pcode, Res,
                                {Pars, false}, InfPars,
                                Recompile, CircRef);
        % special case for the include function (special dirty!)
        {ok, {Pcode, {include, {PreV, Wd, Ht, Incs}, Res}, Pars,
              InfPars, Recompile, CircRef}} ->
            Attrs2 = orddict:store("preview", {PreV, Wd, Ht}, Attrs),
            Blank = #incs{},
            Attrs3 = case Incs of
                         Blank -> orddict:erase("__hasincs", Attrs2);
                         _     -> ok = update_incsD(XRefX, Incs),
                                  orddict:store("__hasincs", t, Attrs2)
                     end,
            % with include you might need to bring incs through from
            % whatever is included so some jiggery might be required
            % on the pokey
            Attrs4 = handle_merge(Ht, Wd, Attrs3),
            Attrs5 = bring_through(Attrs4, XRefX, Pars),
            write_formula_attrs(Attrs5, XRefX, Formula, Pcode, Res,
                                {Pars, true}, InfPars,
                                Recompile, CircRef);
        % normal functions with a resize
        {ok, {Pcode, {resize, {Wd, Ht, Incs}, Res}, Parents,
              InfParents, Recompile, CircRef}} ->
            Blank = #incs{},
            Attrs2 = case Incs of
                         Blank -> orddict:erase("__hasincs", Attrs);
                         _     -> ok = update_incsD(XRefX, Incs),
                                  orddict:store("__hasincs", t, Attrs)
                     end,
            Attrs3 = handle_merge(Ht, Wd, Attrs2),
            write_formula_attrs(Attrs3, XRefX, Formula, Pcode, Res,
                                {Parents, false}, InfParents,
                                Recompile, CircRef);
        {ok, {Pcode, {timer, Spec, Res}, Parents, InfParents, Recompile,
              CircRef}} ->
            Attrs2 = orddict:store("__hastimer", t, Attrs),
            ok = update_timerD(XRefX, Spec),
            write_formula_attrs(Attrs2, XRefX, Formula, Pcode, Res,
                                {Parents, false}, InfParents,
                                Recompile, CircRef);
        % bog standard function!
        {ok, {Pcode, Res, Parents, InfParents, Recompile, CircRef}} ->
            % there might have been a preview before - nuke it!
            % mebbies there was incs, nuke 'em (WHY?)
            ok = update_incsD(XRefX, #incs{}),
            write_formula_attrs(Attrs, XRefX, Formula, Pcode, Res,
                                {Parents, false}, InfParents,
                                Recompile, CircRef)
    end.

proc_special(#spec_val{rawform = null} = SP, XRefX, Attrs) ->
    proc_sp1(SP, XRefX, Attrs);
proc_special(#spec_val{rawform = RF} = SP, XRefX, Attrs) ->
    #rawform{form = Form} = RF,
    ok = attach_formD(XRefX, Form),
    NewAttrs = orddict:store("__hasform", t, Attrs),
    proc_sp1(SP, XRefX, NewAttrs).

proc_sp1(#spec_val{sp_webcontrol = null} = SP, XRefX, Attrs) ->
    proc_sp2(SP, XRefX, Attrs);
proc_sp1(#spec_val{sp_webcontrol = WC} = SP, XRefX, Attrs) ->
    {Trans, Label} = WC#form.id,
    Form = WC#form{id = {XRefX#xrefX.path, Trans, Label}},
    ok = attach_formD(XRefX, Form),
    NewAttrs = orddict:store("__hasform", t, Attrs),
    proc_sp2(SP, XRefX, NewAttrs).

proc_sp2(#spec_val{sp_phone = null} = SP, XRefX, Attrs) ->
    proc_sp3(SP, XRefX, Attrs);
proc_sp2(#spec_val{sp_phone = Pl} = SP, XRefX, Attrs) ->
    ok = attach_phoneD(XRefX, Pl),
    NewAttrs = orddict:store("__hasphone", t, Attrs),
    proc_sp3(SP, XRefX, NewAttrs).

proc_sp3(#spec_val{preview = null} = SP, XRefX, Attrs) ->
    proc_sp4(SP, XRefX, Attrs);
proc_sp3(#spec_val{preview = Pr} = SP, XRefX, Attrs) ->
    #preview{title = Title, height = Ht, width = Wd} = Pr,
    NewAttrs = orddict:store("preview", {Title, Wd, Ht}, Attrs),
    NewAttrs2 = handle_merge(Ht, Wd, NewAttrs),
    proc_sp4(SP, XRefX, NewAttrs2).

proc_sp4(#spec_val{sp_incs = null} = SP, XRefX, Attrs) ->
    proc_sp5(SP, XRefX, Attrs);
proc_sp4(#spec_val{sp_incs = Incs} = SP, XRefX, Attrs) ->
    ok = update_incsD(XRefX, Incs),
    NewAttrs = orddict:store("__hasincs", t, Attrs),
    proc_sp5(SP, XRefX, NewAttrs).

proc_sp5(#spec_val{resize = null} = SP, XRefX, Attrs) ->
    proc_sp6(SP, XRefX, Attrs);
proc_sp5(#spec_val{resize = RSz} = SP, XRefX, Attrs) ->
    #resize{width = W, height = H} = RSz,
    NewAttrs = handle_merge(H, W, Attrs),
    proc_sp6(SP, XRefX, NewAttrs).

proc_sp6(#spec_val{sp_timer = null} = SP, XRefX, Attrs) ->
    proc_sp7(SP, XRefX, Attrs);
proc_sp6(#spec_val{sp_timer = _Tr} = SP, _XRefX, _Attrs) ->
    io:format("SP is ~p~n", [SP]),
    exit("fix me 7a").

proc_sp7(#spec_val{sp_users = null} = SP, XRefX, Attrs) ->
    proc_sp8(SP, XRefX, Attrs);
proc_sp7(#spec_val{sp_users = true} = SP, XRefX, Attrs) ->
    ok = attach_usersD(XRefX),
    NewAttrs = orddict:store("__users", t, Attrs),
    proc_sp8(SP, XRefX, NewAttrs).

proc_sp8(#spec_val{unique = null}, _XRefX, Attrs) ->
    Attrs;
proc_sp8(#spec_val{unique = Uq}, _XRefX, Attrs) ->
    orddict:store("__unique", Uq, Attrs).

handle_merge(1, 1, Attrs) -> orddict:erase("merge", Attrs);
handle_merge(Ht, Wd, Attrs)
  when Ht >= 1 andalso Wd >= 1 ->
    Merge = {struct, [{"right", Wd - 1}, {"down",  Ht - 1}]},
    orddict:store("merge", Merge, Attrs).

write_formula2(XRefX, OrigVal, {Type, Val},
               {"text-align", Align},
               {"format", Format}, Attrs) ->
    Formula = case Type of
                  quote    -> [$' | Val];
                  datetime -> OrigVal;
                  float    -> OrigVal;
                  int      -> OrigVal;
                  _        -> hn_util:text(Val)
              end,
    ok = set_relationsD(XRefX, [], [], false),
    Attrs2 = add_attributes(Attrs, [{"__default-align", Align},
                                    {"__rawvalue", Val},
                                    {"formula", Formula},
                                    {"__lastcalced", util2:get_timestamp()}]),
    % there might have been a preview before - nuke it!
    Attrs3 = orddict:erase("preview", Attrs2),
    Attrs4 = orddict:erase("__ast", Attrs3),
    Attrs5 = orddict:erase("__recompile", Attrs4),
    case Format of
        "null" -> Attrs5;
        _      -> orddict:store("format", Format, Attrs5)
    end.

write_error_attrs(Attrs, XRefX, Formula, Error) ->
    ok = set_relationsD(XRefX, [], [], false),
    add_attributes(Attrs, [{"formula", Formula},
                           {"__rawvalue", {errval, Error}},
                           {"__ast", []}]).

-spec set_dyn_relationsD(#xrefX{}, [#xrefX{}], [#xrefX{}]) -> ok.
set_dyn_relationsD(#xrefX{idx = CellIdx, site = Site}, DynParents,
                   DynInfParents) ->
    Tbl = trans(Site, relation),
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R;
              []  -> #relation{cellidx = CellIdx}
          end,
    Rel2 = set_dyn_parents(Tbl, Site, Rel, DynParents, DynInfParents),
    mnesia:write(Tbl, Rel2, write).

-spec set_relationsD(#xrefX{}, [#xrefX{}], [#xrefX{}], boolean()) -> ok.
set_relationsD(#xrefX{idx = CellIdx, site = Site}, FiniteParents,
              InfParents, IsIncl) ->
    Tbl = trans(Site, relation),
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R;
              []  -> #relation{cellidx = CellIdx}
          end,
    Rel2 = set_parents(Tbl, Site, Rel, FiniteParents, InfParents, IsIncl),
    case IsIncl of
        true  -> [mark_in_includeFn(Site, X) || X <- Rel2#relation.parents];
        false -> ok
    end,
    mnesia:write(Tbl, Rel2, write).

mark_in_includeFn(Site, Idx) ->
    XRefX = idx_to_xrefXD(Site, Idx),
    Op = fun(Attrs) ->
                 case orddict:find("__in_includeFn", Attrs) of
                     {ok, true}  ->
                         {clean, Attrs};
                     error ->
                         {clean, orddict:store("__in_includeFn", true, Attrs)}
                 end
         end,
    apply_to_attrsD(XRefX, Op, include, nil, ?NONTRANSFORMATIVE).

-spec set_dyn_parents(atom(), list(), #relation{}, [#xrefX{}],
                      [#xrefX{}]) -> #relation{}.
set_dyn_parents(Tbl, Site, Rel, DynParents, DynInfParents) ->
    #relation{cellidx = CellIdx, dyn_parents = CurDynParents,
              dyn_infparents = CurDynInfPars} = Rel,
    Fun = fun({local, #xrefX{idx = Idx} = XRefX}) ->
                  {Idx, XRefX}
          end,
    {PIdxs, _} = lists:unzip(lists:map(Fun, DynParents)),
    {InfPIdxs, InfPars} = lists:unzip(lists:map(Fun, DynInfParents)),
    NewParentIdxs = ordsets:from_list(PIdxs),
    LostParents = ordsets:subtract(CurDynParents, NewParentIdxs),
    [del_childD(P, CellIdx, Tbl) || P <- LostParents],
    [ok = add_childD(P, CellIdx, Tbl, false) || P <- NewParentIdxs],
    NewInfParIdxs = ordsets:from_list(InfPIdxs),
    XCurInfPars = [idx_to_xrefXD(Site, X) || X <- CurDynInfPars],
    case InfPars of
        XCurInfPars ->
            ok;
        _           ->
            ok = handle_infsD(CellIdx, Site, InfPars, XCurInfPars)
    end,
    Rel#relation{dyn_parents = NewParentIdxs, dyn_infparents = NewInfParIdxs}.

-spec set_parents(atom(), list(), #relation{}, [#xrefX{}],
                  [#xrefX{}], boolean()) -> #relation{}.
set_parents(Tbl,
            Site, Rel,
            Parents,
            InfParents,
            IsIncl) ->
    #relation{cellidx = CellIdx, parents = CurParents,
              infparents = CurInfPars} = Rel,
    Fun = fun({local, #xrefX{idx = Idx} = XRefX}) ->
                  {Idx, XRefX}
          end,
    {PIdxs, _} = lists:unzip(lists:map(Fun, Parents)),
    {InfPIdxs, InfPars} = lists:unzip(lists:map(Fun, InfParents)),
    NewParentIdxs = ordsets:from_list(PIdxs),
    LostParents = ordsets:subtract(CurParents, NewParentIdxs),
    [del_childD(P, CellIdx, Tbl) || P <- LostParents],
    [ok = add_childD(P, CellIdx, Tbl, IsIncl) || P <- NewParentIdxs],
    %Ret = read_relationsD(Site, CellIdx, write),
    %Cell = idx_to_xrefXD(Site, CellIdx),
    NewInfParIdxs = ordsets:from_list(InfPIdxs),
    XCurInfPars = [idx_to_xrefXD(Site, X) || X <- CurInfPars],
    case InfPars of
        XCurInfPars ->
            ok;
        _           ->
            ok = handle_infsD(CellIdx, Site, InfPars, XCurInfPars)
    end,
    Rel#relation{parents = NewParentIdxs, infparents = NewInfParIdxs}.

%% @doc Make a #muin_rti record out of an xrefX record and a flag that specifies
%% whether to run formula in an array context.
xrefX_to_rti(#xrefX{idx = Idx, site = S, path = P,
                    obj = {cell, {C, R}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P,
              col = C, row = R, idx = Idx,
              array_context = AC,
              auth_req = AR};
xrefX_to_rti(#xrefX{idx = Idx, site = S, path = P,
                    obj = {range, {C, R, _, _}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P,
              col = C, row = R, idx = Idx,
              array_context = AC,
              auth_req = AR}.

clean_up_dynamic_select(XRefX) ->
    ok = set_dyn_relationsD(XRefX, [], []).

delete_timerD(#xrefX{idx = Idx, site = S}) ->
    Tbl = trans(S, timer),
    mnesia:delete(Tbl, Idx, write).

delete_incsD(#xrefX{idx = Idx, site = S}) ->
    Tbl = trans(S, include),
    mnesia:delete(Tbl, Idx, write).

-spec unattach_usersD(#xrefX{}) -> ok.
unattach_usersD(#xrefX{site = Site, idx = Idx}) ->
    Tbl = trans(Site, users_and_groups),
    mnesia:delete(Tbl, Idx, write).

-spec unattach_phoneD(#xrefX{}) -> ok.
unattach_phoneD(#xrefX{site = Site, idx = Idx}) ->
    Tbl = trans(Site, phone),
    mnesia:delete(Tbl, Idx, write).

-spec unattach_formD(#xrefX{}) -> ok.
unattach_formD(#xrefX{site = Site, idx = Idx}) ->
    Tbl = trans(Site, form),
    mnesia:delete(Tbl, Idx, write).

%% Apply to attrs does the actual work of modifying a target ref. The
%% behaviour of the modification is determined by the passed in 'Op'
%% function. Upon completion of 'Op' the post_process function is
%% applied, which sets formats and styles as necessary.
%% the op function returns a tuple of {Status, Ref, Attrs} where
%% Status is either 'clean' or 'dirty'
%% some apply_to_attrsD operations are 'transformative' the cell
%% may have moved.
-spec apply_to_attrsD(#xrefX{}, fun((?dict) -> ?dict),
                                   atom(), auth_srv:uid(), boolean) -> ?dict.
apply_to_attrsD(#xrefX{idx = Idx, site = Site} = XRefX, Op, Action,
                Uid, Transformative) ->
    Table = trans(Site, item),
    Attrs = case mnesia:read(Table, Idx, write) of
                [#item{attrs = A}] -> binary_to_term(A);
                _                  -> orddict:new()
            end,
    {Status, Attrs2} = Op(Attrs),
    Attrs3 = post_process(XRefX, Attrs2),
    % the Op may have shifted the RefX that the Idx now points to, so look it
    % up again for the log
    NewXRefX = case Transformative of
                   true  -> idx_to_xrefXD(Site, Idx);
                   false -> XRefX
               end,
    ok = log_write(NewXRefX, Attrs, Attrs2, Action, Uid),
    Item = #item{idx = Idx, attrs = term_to_binary(Attrs3)},
    case deleted_attrs(Attrs, Attrs3) of
        []   -> ok;
        List -> tell_front_end_delete_attrs(XRefX, List)
    end,
    tell_front_end_change(XRefX, Attrs3),
    mnesia:write(Table, Item, write),
    {Status, Attrs3}.

-spec tell_front_end_delete_attrs(#xrefX{}, list()) -> ok.
tell_front_end_delete_attrs(XRefX, Attrs) ->
    Tuple = {delete_attrs, XRefX, Attrs},
    tell_front_end1(Tuple).

-spec tell_front_end_style(#xrefX{}, #style{}) -> ok.
tell_front_end_style(XRefX, Style) ->
    Tuple = {style, XRefX, Style},
    tell_front_end1(Tuple).

-spec tell_front_end_change(#xrefX{}, ?dict) -> ok.
tell_front_end_change(XRefX, Attrs) ->
    Tuple = {change, XRefX, Attrs},
    tell_front_end1(Tuple).

tell_front_end1(Tuple) ->
    List = get('front_end_notify'),
    put('front_end_notify', [Tuple | List]),
    ok.

idx_to_xrefXD(S, Idx) ->
    case mnesia:read(trans(S, local_obj), Idx, read) of
        [Rec] -> #local_obj{path = P, obj = O} = Rec,
                 #xrefX{idx = Idx, site = S,
                        path = binary_to_term(P), obj = O};
        []    -> {error, id_not_found, Idx}
    end.

%% converts a tablename into the site-specific tablename
trans(Site, TableName) when is_atom(TableName) ->
    Prefix = get_prefix(Site),
    list_to_atom(Prefix ++ "&" ++ atom_to_list(TableName)).
%% trans(Site, Record) when is_tuple(Record) ->
%%     NRec = trans(Site, element(1, Record)),
%%     setelement(1, Record, NRec).

get_prefix(R) when is_record(R, refX) ->
    get_prefix(R#refX.site);
get_prefix("http://"++Site) ->
    [case S of $: -> $&; S  -> S end
     || S <- Site].

mark_dirty_for_zinfD(#xrefX{site = S, obj = {cell, _}} = XRefX) ->
    Tbl = trans(S, dirty_for_zinf),
    ok = mnesia:write(Tbl, #dirty_for_zinf{dirty = XRefX}, write),
    %ok = zinf_srv:check_zinfs(S),
    ok;
% any other refs ignore 'em
mark_dirty_for_zinfD(XRefX) when is_record(XRefX, xrefX) -> ok.

%% this function is called when a new attribute is set for a style
-spec apply_style(#xrefX{}, {string(), term()}, ?dict) -> ?dict.
apply_style(XRefX, {Name, Val}, Attrs) ->
    NewSIdx = case orddict:find("style", Attrs) of
                  {ok, StyleIdx} -> based_styleD(XRefX, StyleIdx, Name, Val);
                  _              -> fresh_style(XRefX, Name, Val)
              end,
    orddict:store("style", NewSIdx, Attrs).

-spec fresh_style(#xrefX{}, string(), any()) -> integer().
fresh_style(#xrefX{site = Site} = Ref, Name, Val) ->
    FieldNo = ms_util2:get_index(magic_style, Name),
    Tbl = trans(Site, style),
    MStyle = setelement(FieldNo + 1, #magic_style{}, Val),
    store_styleD(Ref, Tbl, MStyle).

-spec based_styleD(#xrefX{}, integer(), string(), any()) -> integer().
based_styleD(#xrefX{site = Site} = XRefX, BaseIdx, Name, Val) ->
    Tbl = trans(Site, style),
    case mnesia:index_read(Tbl, BaseIdx, #style.idx) of
        [#style{magic_style = MStyle1}] ->
            FieldNo = ms_util2:get_index(magic_style, Name),
            MStyle2 = setelement(FieldNo + 1, MStyle1, Val),
            store_styleD(XRefX, Tbl, MStyle2);
        _ ->
            BaseIdx %% <- strange..
    end.

-spec attach_usersD(#xrefX{}) -> ok.
attach_usersD(#xrefX{idx = Idx, site = Site}) ->
    Tbl = trans(Site, users_and_groups),
    ok = mnesia:write(Tbl, #users_and_groups{idx = Idx}, write).

-spec attach_phoneD(#xrefX{}, #phone{}) -> ok.
attach_phoneD(#xrefX{idx = Idx, site = Site}, Phone) ->
    Tbl = trans(Site, phone),
    mnesia:write(Tbl, Phone#phone{idx = Idx}, write).

-spec attach_formD(#xrefX{}, #form{}) -> ok.
attach_formD(#xrefX{idx = Idx, site = Site}, Form) ->
    Tbl = trans(Site, form),
    mnesia:write(Tbl, Form#form{key = Idx}, write).

write_formula_attrs(Attrs, XRefX, Formula, Pcode, Res, {Parents, IsIncl},
                    InfParents, Recompile, CircRef) ->
    ok = set_relationsD(XRefX, Parents, InfParents, IsIncl),
    Align = default_align(Res),
    % if it is a circular reference it is going to be recalculated
    % so stop the recalc NEXT TIME by replacing the current
    % formula with the #CIRCREF! error - the next recalc will
    % sort out the final __ast, blah-blah...
    Formula2 = case CircRef of
                   true -> "#CIRCREF!";
                   _    -> Formula
               end,
    add_attributes(Attrs, [{"formula", Formula2},
                           {"__rawvalue", Res},
                           {"__ast", Pcode},
                           {"__default-align", Align},
                           {"__recompile", Recompile},
                           {"__lastcalced", util2:get_timestamp()}]).

update_timerD(#xrefX{idx = Idx, site = S}, Spec) ->
    Tbl = trans(S, timer),
    mnesia:write(Tbl, #timer{idx = Idx, spec = Spec}, write).

update_incsD(XRefX, Incs) when is_record(XRefX, xrefX)
                            andalso is_record(Incs, incs) ->
    #xrefX{idx = Idx, site = S, path = P} = XRefX,
    #incs{js = Js, js_reload = Js_reload, css = CSS} = Incs,
    Tbl = trans(S, include),
    Blank = #incs{},
    case {mnesia:read(Tbl, Idx, write), Incs} of
        {[], Blank}  -> ok;
        {Incs, Incs} -> ok;
        _            -> Inc = #include{idx = Idx, path = P, js = Js,
                                       js_reload = Js_reload, css = CSS},
                        mnesia:write(Tbl, Inc, write)
    end.

bring_through(Attrs, XRefX, Pars) ->
    Pars2 = [X || {Type, X} <- Pars, Type ==  local],
    Pars3 = remove_same_page(Pars2, XRefX, []),
    Incs = get_incs(Pars3, [], [], []),
    Blank = #incs{}, % no you can't just use #incs{} in the case statement, duh!
    case Incs of
        Blank -> Attrs;
        _     -> ok = update_incsD(XRefX, Incs),
                 orddict:store("__hasincs", t, Attrs)
    end.

remove_same_page([], _XRefX, Acc) ->
    Acc;
remove_same_page([H | T], #xrefX{path = P1} = XRefX, Acc) ->
    #xrefX{path = P2} = H,
    NewAcc = case P1 of
                 P2 -> Acc;
                 _  -> [H | Acc]
             end,
    remove_same_page(T, XRefX, NewAcc).

get_incs([], Js, Js_R, CSS) -> #incs{js = hslists:uniq(Js),
                                     js_reload = hslists:uniq(Js_R),
                                     css = hslists:uniq(CSS)};
get_incs([H | T], Js, Js_R, CSS) ->
    {NewJ, NewJs_R, NewC} = case read_incsD(H) of
        []   -> {Js, Js_R, CSS};
        Incs -> process_incs(Incs, [], [], [])
    end,
    get_incs(T, NewJ, NewJs_R, NewC).

read_incsD(#xrefX{site = S, path = P}) ->
    Table = trans(S, include),
    mnesia:index_read(Table, P, #include.path).

process_incs([], J, Js_R, CSS) -> {J, Js_R, CSS};
process_incs([H | T], Js, Js_R, CSS) ->
    NewJ = lists:append(Js, H#include.js),
    NewJs_R = lists:append(Js_R, H#include.js_reload),
    NewC = lists:append(CSS, H#include.css),
    process_incs(T, NewJ, NewJs_R, NewC).

add_attributes(D, []) -> D;
add_attributes(D, [{Key, Val}|T]) ->
    D2 = orddict:store(Key, Val, D),
    add_attributes(D2, T).

del_attributes(D, []) -> D;
del_attributes(D, [Key|T]) ->
    D2 = orddict:erase(Key, D),
    del_attributes(D2, T).

-spec del_childD(cellidx(), cellidx(), atom()) -> ok.
del_childD(CellIdx, Child, Tbl) ->
    case mnesia:read(Tbl, CellIdx, write) of
        [R] ->
            Children = ordsets:del_element(Child, R#relation.children),
            R2 = R#relation{children = Children},
            mnesia:write(Tbl, R2, write);
        _ ->
            ok
    end.

%% Adds a new child to a parent.
-spec add_childD(cellidx(), cellidx(), atom(), boolean()) -> ok.
add_childD(CellIdx, Child, Tbl, IsIncl) ->
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R#relation{include = IsIncl};
              []  -> #relation{cellidx = CellIdx, include = IsIncl}
          end,
    Children = ordsets:add_element(Child, Rel#relation.children),
    mnesia:write(Tbl, Rel#relation{children = Children}, write).

handle_infsD(_CellIdx, _Site, Inf, Inf) ->
    ok;
handle_infsD(CellIdx, Site, NewInfParents, OldInfParents) ->
    Tbl = trans(Site, dirty_zinf),
    Rec = #dirty_zinf{type = infinite, dirtycellidx = CellIdx,
                      old = OldInfParents, new = NewInfParents},
    ok = mnesia:write(Tbl, Rec, write),
    %ok = zinf_srv:process_zinfs(Site).
    ok.

%% Last chance to apply any default styles and formats.
-spec post_process(#xrefX{}, ?dict) -> ?dict.
post_process(XRefX, Attrs) ->
    Attrs2 = post_process_styles(XRefX, Attrs),
    case orddict:find("__rawvalue", Attrs2) of
        {ok, Raw} -> post_process_format(Raw, Attrs2);
        _         -> Attrs2
    end.

-spec post_process_styles(#xrefX{}, ?dict) -> ?dict.
post_process_styles(XRefX, Attrs) ->
    case orddict:find("style", Attrs) of
        {ok, _} -> Attrs;
        _       ->
            case orddict:find("__default-align", Attrs) of
                {ok, Align} -> apply_style(XRefX,{"text-align",Align},Attrs);
                _           -> Attrs
            end
    end.

post_process_format(Raw, Attrs) ->
    Format = case orddict:find("format", Attrs) of
                 {ok, F} -> F;
                 _       -> "General"
             end,
    case format:get_src(Format) of
        {erlang, {_Type, Output}} ->
            % Y'all hear, this is how America does color. I tell you what.
            case format:run_format(Raw, Output) of
                {Color, Val1} ->
                    Val2  = case Val1 of
                                {errval, ErrVal}     -> atom_to_list(ErrVal);
                                A when is_atom(A)    -> atom_to_list(A);
                                I when is_integer(I) -> integer_to_list(I);
                                Fl when is_float(Fl) -> float_to_list(Fl);
                                _ -> Val1
                            end,
                    add_attributes(Attrs,
                                   [{"value", Val2},
                                    {"overwrite-color", atom_to_list(Color)}]);
                _ ->
                    Attrs
            end;
        _ ->
            Attrs
    end.

log_write(_, _, _, _Action, nil) -> ok;
log_write(#xrefX{idx = Idx, site = S, path = P, obj = O}, Old,
          New, Action, Uid) ->
    {OldF, OldV} = extract(Old),
    {NewF, NewV} = extract(New),
    L = io_lib:format("old: formula ~p value ~p new: formula ~p value ~p",
                      [OldF, OldV, NewF, NewV]),
    L2 = term_to_binary(L),
    Log = #logging{idx = Idx, uid = Uid, action = Action, actiontype = "",
                   type = cell, path = hn_util:list_to_path(P),
                   obj = O, log = L2},
    write_logD(S, Log).

write_logD(Site, Log) ->
    Tbl = trans(Site, logging),
    mnesia:write(Tbl, Log, write).

deleted_attrs(Old, New) ->
    OldKeys = lists:sort(orddict:fetch_keys(Old)),
    NewKeys = lists:sort(orddict:fetch_keys(New)),
    del_a1(OldKeys, NewKeys, []).

del_a1([], _L, Acc)                 -> Acc;
del_a1([[$_, $_ | _H] | T], L, Acc) -> del_a1(T, L, Acc);
del_a1([H | T], L, Acc)             ->
    case lists:member(H, L) of
        true  -> del_a1(T, L, Acc);
        false -> del_a1(T, L, [H | Acc])
    end.

-spec get_cell_for_muin(#refX{}, [finite | infinite], list()) ->
    {any(), any(), any()}.
%% @doc this function is called by muin during recalculation and should
%%      not be used for any other purpose
%% takes a  #refX{} and not an xrefX{} because it is spat out of the compiler
%% and I don't know what the idx is, or if it exists yet
get_cell_for_muin(#refX{} = RefX, Type, ValType)
  when ValType == "value" orelse ValType == "__rawvalue" ->
    XRefX = refX_to_xrefX_createD(RefX),
    Attrs = case read_ref(XRefX, inside, write) of
                             [{XRefX, A}] -> A;
                             []           -> orddict:new()
            end,
    Value = case orddict:find(ValType, Attrs) of
                {ok, {datetime, _, [N]}} ->
                    muin_date:from_gregorian_seconds(N);
                {ok, V} ->
                    V;
                _ ->
                    blank
            end,
    {Value, [], [{local, Type, XRefX}]}.

read_relations(#xrefX{site = S, idx = Idx}, Lock) ->
    read_relationsD(S, Idx, Lock).

read_relationsD(Site, Idx, Lock) ->
    Tbl = trans(Site, relation),
    mnesia:read(Tbl, Idx, Lock).

read_ref(Ref, Relation) -> read_ref(Ref, Relation, read).

-spec read_ref([#xrefX{} | #refX{}], inside | intersect, read | write)
-> [{#xrefX{}, ?dict}].
read_ref(#refX{site = S} = RefX, Relation, Lock) ->
    read_attrs(S, read_objs(RefX, Relation), Lock);
read_ref(#xrefX{site = S} = XRefX, Relation, Lock) ->
    read_attrs(S, read_objs(XRefX, Relation), Lock).

-spec read_attrs(string(), [#local_obj{}], read|write)
-> [{#xrefX{}, ?dict}].
read_attrs(S, LocObjs, Lock) ->
    Tbl = trans(S, item),
    read_attrs_D(LocObjs, S, Tbl, Lock, []).

read_attrs_D([], _S, _Tbl, _Lock, Acc) ->
    lists:reverse(Acc);
read_attrs_D([LO|Tail], S, Tbl, Lock, Acc) ->
    Acc2 = case mnesia:read(Tbl, LO#local_obj.idx, Lock) of
               [#item{attrs = Attrs}] ->
                   [{lobj_to_xrefX(S, LO), binary_to_term(Attrs)} | Acc];
               []                   ->
                   Acc end,
    read_attrs_D(Tail, S, Tbl, Lock, Acc2).

-spec read_ref_field(#xrefX{}, string(), read|write)
-> [{#xrefX{}, term()}].
read_ref_field(XRefX, Field, Lock) ->
    Cells = read_ref(XRefX, inside, Lock),
    extract_field(Cells, Field, []).

-spec extract_field([{#xrefX{}, ?dict}], string(), [{#xrefX{}, term()}])
-> [{#xrefX{}, term()}].
extract_field([], _F, Acc) ->
    lists:reverse(Acc);
extract_field([{XRefX, Attrs}|T], Field, Acc) ->
    Acc2 = case orddict:find(Field, Attrs) of
               {ok, V} -> [{XRefX, V}|Acc];
               _       -> Acc end,
    extract_field(T, Field, Acc2).

-spec read_objs(#xrefX{} | #refX{}, inside | intersect | direct) ->
    [#local_obj{}] | [].
% if its a cell then just the local obj
read_objs(#refX{site = S, path = P, obj = {cell, _} = O}, inside) ->
    read_objsD(#refX{site = S, path = P, obj = {cell, _} = O}, inside);
% if it a xrefX then we already know the idx so just return it
read_objs(#xrefX{idx = I, path = P, obj = {cell, _} = O}, _Any) ->
    RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
    [#local_obj{idx = I, path = term_to_binary(P), obj = O, type = url,
                revidx = term_to_binary(RevIdx)}];
read_objs(#xrefX{site = S, path = P, obj = {page, "/"}}, inside) ->
    read_objsD(#refX{site = S, path = P, obj = {page, "/"}}, inside);
read_objs(#refX{site = S, path = P, obj = {page, "/"}}, inside) ->
    read_objsD(#refX{site = S, path = P, obj = {page, "/"}}, inside);
read_objs(#xrefX{site = S, path = P, obj = {column, {X1, X2}}}, inside) ->
    read_objsD(#refX{site = S, path = P, obj = {column, {X1, X2}}}, inside);
read_objs(#refX{site = S, path = P, obj = {column, {X1, X2}}}, inside) ->
    read_objsD(#refX{site = S, path = P, obj = {column, {X1, X2}}}, inside);
read_objs(#xrefX{site = S, path = P, obj = {row, {Y1, Y2}}}, inside) ->
    read_objs(#refX{site = S, path = P, obj = {row, {Y1, Y2}}}, inside);
read_objs(#refX{site = S, path = P, obj = {row, {Y1, Y2}}}, inside) ->
    read_objsD(#refX{site = S, path = P, obj = {row, {Y1, Y2}}}, inside);
read_objs(#xrefX{site = S, path = P,
                 obj = {range, {X1, Y1, X2, Y2}}}, inside) ->
    read_objsD(#refX{site = S, path = P,
                     obj = {range, {X1, Y1, X2, Y2}}}, inside);
read_objs(#refX{site = S, path = P,
                obj = {range, {X1, Y1, X2, Y2}}}, inside) ->
    read_objsD(#refX{site = S, path = P,
                     obj = {range, {X1, Y1, X2, Y2}}}, inside);
% if the intersect is a page then just read the page
read_objs(#xrefX{site = S, path = P, obj = {page, "/"}}, intersect) ->
    read_objs(#refX{site = S, path = P, obj = {page, "/"}}, intersect);
read_objs(#refX{site = S, path = P, obj = {page, "/"}}, intersect) ->
    read_objsD(#refX{site = S, path = P, obj = {page, "/"}}, intersect);
read_objs(#xrefX{site = S, path = P, obj = O}, intersect) ->
    read_objsD(#refX{site = S, path = P, obj = O}, intersect);
read_objs(#refX{site = Site} = Ref, intersect) ->
    read_objsD(#refX{site = Site} = Ref, intersect);
read_objs(#xrefX{site = S, path = P, obj = O}, direct) ->
    read_objsD(#refX{site = S, path = P, obj = O}, direct);
read_objs(#refX{site = S, path = P, obj = O}, direct) ->
    read_objsD(#refX{site = S, path = P, obj = O}, direct).

-spec read_objsD(#refX{} | #refX{}, inside | intersect | direct) ->
    [#local_obj{}] | [].
% if its a cell then just the local obj
read_objsD(#refX{site = S, path = P, obj = {cell, _} = O}, inside) ->
    Table = trans(S, local_obj),
    RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
    mnesia:index_read(Table, term_to_binary(RevIdx), #local_obj.revidx);
read_objsD(#refX{site = S, path = P, obj = {page, "/"}}, inside) ->
    Table = trans(S, local_obj),
    mnesia:index_read(Table, term_to_binary(P), #local_obj.path);
read_objsD(#refX{site = S, path = P, obj = {column, {X1, X2}}}, inside) ->
    Table = trans(S, local_obj),
    Page = mnesia:index_read(Table, term_to_binary(P), #local_obj.path),
    Fun = fun(#local_obj{obj = {cell, {MX, _MY}}}) ->
                  if
                      X1  =< MX, MX  =< X2 -> true;
                      true               -> false
                  end;
             (_LO) -> false
          end,
    lists:filter(Fun, Page);
read_objsD(#refX{site = S, path = P, obj = {row, {Y1, Y2}}}, inside) ->
    Table = trans(S, local_obj),
    Page = mnesia:index_read(Table, term_to_binary(P), #local_obj.path),
    Fun = fun(#local_obj{obj = {cell, {_MX, MY}}}) ->
                  if
                      Y1  =< MY, MY  =< Y2 -> true;
                      true                 -> false
                  end;
             (_LO) ->
                  false
          end,
    lists:filter(Fun, Page);
read_objsD(#refX{site = S, path = P, obj = {range, {X1, Y1, X2, Y2}}}, inside) ->
    Table = trans(S, local_obj),
    Page = mnesia:index_read(Table, term_to_binary(P), #local_obj.path),
    Fun = fun(#local_obj{obj = {cell, {MX, MY}}}) ->
                  if
                      X1  =< MX, MX  =< X2,
                      Y1  =< MY, MY  =< Y2 -> true;
                      true                 -> false
                  end;
             (_LO) -> false
          end,
    lists:filter(Fun, Page);
% if the intersect is a page then just read the page
read_objsD(#refX{site = S, path = P, obj = {page, "/"}}, intersect) ->
    Table = trans(S, local_obj),
    mnesia:index_read(Table, term_to_binary(P), #local_obj.path);
read_objsD(#refX{site = Site} = Ref, intersect) ->
    MS = objs_intersect_ref(Ref),
    mnesia:select(trans(Site, local_obj), MS);
read_objsD(#refX{site = S, path = P, obj = O}, direct) ->
    Table = trans(S, local_obj),
    RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
    mnesia:index_read(Table, term_to_binary(RevIdx), #local_obj.revidx).

%% Note that this is most useful when given cells, or ranges.
objs_intersect_ref(#refX{path = P, obj = {page, "/"}}) ->
    [{#local_obj{path = term_to_binary(P), _ = '_'}, [], ['$_']}];
objs_intersect_ref(#refX{path = P, obj = {range, {X1, Y1, X2, Y2}}}) ->
    PBin = term_to_binary(P),
    ets:fun2ms(fun(LO = #local_obj{path = MP, obj = {cell,{MX, MY}}})
                  when MP ==  PBin,
                       X1  =< MX, MX  =< X2,
                       Y1  =< MY, MY  =< Y2 -> LO;
                  (LO = #local_obj{path = MP, obj = {row,{MY, MY}}})
                  when MP ==  PBin,
                       Y1  =< MY, MY  =< Y2 -> LO;
                  (LO = #local_obj{path = MP, obj = {column, {MX, MX}}})
                  when MP ==  PBin,
                       X1  =< MX, MX  =< X2 -> LO;
                  (LO = #local_obj{path = MP, obj = {page, _}})
                  when MP ==  PBin -> LO
               end);
objs_intersect_ref(#refX{path = P, obj = {cell, {X, Y}}}) ->
    PBin = term_to_binary(P),
    ets:fun2ms(
      fun(LO = #local_obj{path = MP, obj = {cell, {MX, MY}}})
         when MP ==  PBin, MX ==  X, MY ==  Y -> LO;
         (LO = #local_obj{path = MP, obj = {column, {MX, MX}}})
         when MP ==  PBin, MX ==  X -> LO;
         (LO = #local_obj{path = MP, obj = {row, {MY, MY}}})
         when MP ==  PBin, MY ==  Y -> LO;
         (LO = #local_obj{path = MP, obj = {page, _}})
         when MP ==  PBin -> LO
      end);
objs_intersect_ref(#refX{path = P, obj = {column, {X1, X2}}}) ->
    PBin = term_to_binary(P),
    ets:fun2ms(fun(LO = #local_obj{path = MP, obj = {cell, {MX,_MY}}})
                  when MP ==  PBin,
                       X1  =< MX, MX  =< X2 -> LO;
                  (LO = #local_obj{path = MP, obj = {row,_}})
                  when MP ==  PBin -> LO;
                  (LO = #local_obj{path = MP, obj = {page, _}})
                  when MP ==  PBin -> LO
               end);
objs_intersect_ref(#refX{path = P, obj = {row, {R1, R2}}}) ->
    PBin = term_to_binary(P),
    ets:fun2ms(fun(LO = #local_obj{path = MP, obj = {cell,{_MX,MY}}})
                  when MP ==  PBin,
                       R1  =< MY, MY  =< R2 -> LO;
                  (LO = #local_obj{path = MP, obj = {column, _}})
                  when MP ==  PBin-> LO;
                  (LO = #local_obj{path = MP, obj = {page, _}})
                  when MP ==  PBin -> LO
               end).

extract(List) ->
    FT = lists:keyfind("formula",    1, List),
    VT = lists:keyfind("__rawvalue", 1, List),
    case {FT, VT} of
        {false, false}                      -> {"", ""};
        {{"formula", F}, false}             -> {F, ""};
        {{"formula", F}, {"__rawvalue", V}} -> {F, V}
    end.


-spec lobj_to_xrefX(string(), #local_obj{}) -> #xrefX{}.
lobj_to_xrefX(Site, #local_obj{idx = I, path = P, obj = O}) ->
    #xrefX{idx = I, site = Site, path = binary_to_term(P), obj = O}.

-spec store_styleD(#xrefX{}, atom(), #magic_style{}) -> integer().
store_styleD(XRefX, Tbl, MStyle) ->
    case mnesia:read(Tbl, MStyle, read) of
        [#style{idx = I}] ->
            I;
        _ ->
            I = util2:get_timestamp(),
            StyleRec = #style{magic_style = MStyle, idx = I},
            ok = tell_front_end_style(XRefX, StyleRec),
            ok = mnesia:write(Tbl, StyleRec, write),
            I
    end.

default_align(Res) when is_number(Res) -> "right";
default_align(Res) when is_list(Res)   -> "left";
default_align(_Res)                    -> "center".

expand_ref(#refX{site = S} = RefX) ->
    [lobj_to_xrefX(S, LO) || LO <- read_objs(RefX, inside)];
expand_ref(#xrefX{site = S} = XRefX) ->
    [lobj_to_xrefX(S, LO) || LO <- read_objs(XRefX, inside)].

expand_to_rows_or_cols(#refX{obj = {RC, {I, J}}} = Ref)
  when RC ==  row; RC ==  column ->
    expand_to_2(Ref, I, J, []);
expand_to_rows_or_cols(_) -> [].

expand_to_2(#refX{obj = {Type, _}} = Ref, I, I, A) ->
    [Ref#refX{obj = {Type, {I, I}}} | A];
expand_to_2(#refX{obj = {Type, _}} = Ref, I, J, A) ->
    expand_to_2(Ref, I + 1, J, [Ref#refX{obj = {Type, {I, I}}} | A]).

-spec expunge_refsD(string(), [#refX{}]) -> ok.
expunge_refsD(S, Refs) ->
    ItemT = trans(S, item),
    ObjT = trans(S, local_obj),
    RelT = trans(S, relation),
    DelT = trans(S, del_local),
    [begin
         mnesia:delete(ItemT, Idx, write),
         mnesia:delete(ObjT, Idx, write),
         mnesia:delete(RelT, Idx, write),
         Rec = #del_local{idx = Idx},
         mnesia:write(DelT, Rec, write),
         case O of
             {cell, _} -> unattach_formD(Ref);
             _         -> ok
         end
     end || Ref <- Refs,
            #local_obj{idx = Idx, obj = O} <- read_objs(Ref, direct)],
    ok.

-spec mark_dirty_for_inclD([#xrefX{}], auth_srv:auth_spec()) -> ok.
mark_dirty_for_inclD([], _) -> ok;
mark_dirty_for_inclD(Refs = [#xrefX{site = Site}|_], AReq) ->
    %% TODO check out this stuff
    %% not sure all the has include stuff is needed...
    Idxs = lists:flatten([C#xrefX.idx || R <- Refs, C <- expand_ref(R)]),
    Dirties = [Idx || Idx <- Idxs, has_includeD(Site, Idx)],
    case Dirties of
        [] -> ok;
        _  -> Entry = #dirty_queue{dirty = Dirties, auth_req = AReq},
              mnesia:write(trans(Site, dirty_queue), Entry, write)
    end.

-spec has_includeD(string(), cellidx()) -> boolean().
has_includeD(Site, CellIdx) ->
    Tbl = trans(Site, relation),
    case mnesia:read(Tbl, CellIdx, write) of
        []  -> false;
        [R] -> R#relation.include
    end.

log_page(#refX{site = S, path = P, obj = {page, "/"} = O}, Action, Uid) ->
    Log = #logging{idx = "", uid = Uid,
                   action = Action, actiontype = "",
                   type = page, path = hn_util:list_to_path(P),
                   obj = O, log = ""},
    write_logD(S, Log),
    ok.

log_move(#refX{site = S, path = P, obj = {Type, _} = O}, Action, Disp, Uid)
  when Type ==  row orelse Type ==  column orelse Type ==  range ->
    Log = #logging{idx = "", uid = Uid,
                   action = Action, actiontype = Disp,
                   type = page, path = hn_util:list_to_path(P),
                   obj = O, log = ""},
    write_logD(S, Log),
    ok.

shift_rows_and_columnsD(#refX{site = S, path = P, obj = {column, {X1, X2}}},
                       Type, horizontal, _Ar) ->
    Table = trans(S, local_obj),
    Objs = mnesia:index_read(Table, term_to_binary(P), #local_obj.path),
    Offset = get_offset(Type, X1, X2),
    Objs2 = shift_cols(Objs, X2, Offset, []),
    [mnesia:write(Table, Rec, write) || Rec <- Objs2],
    ok;
shift_rows_and_columnsD(#refX{site = S, path = P, obj = {row, {Y1, Y2}}},
                       Type, vertical, _Ar) ->
    Table = trans(S, local_obj),
    Objs = mnesia:index_read(Table, term_to_binary(P), #local_obj.path),
    Offset = get_offset(Type, Y1, Y2),
    Objs2 = shift_rows(Objs, Y2, Offset, []),
    [mnesia:write(Table, Rec, write) || Rec <- Objs2],
    ok;
%% all other shifts don't move rows and columns
shift_rows_and_columnsD(_, _, _, _) ->
    ok.

get_offset(delete, N1, N2) -> N1 - N2 - 1;
get_offset(insert, N1, N2) -> N2 - N1 + 1.

shift_rows([], _, _, Acc) -> Acc;
shift_rows([#local_obj{path = P, obj = {row, {MY1, MY2}}} = H | T],
           Y2, Offset, Acc)
  when MY1 >= Y2 ->
    NO = {row, {MY1 + Offset, MY2 + Offset}},
    NP = hn_util:list_to_path(binary_to_term(P)),
    NewRevIdx = term_to_binary(NP ++ hn_util:obj_to_ref(NO)),
    NewRow = H#local_obj{obj = NO, revidx = NewRevIdx},
    shift_rows(T, Y2, Offset, [NewRow | Acc]);
shift_rows([_H | T], Y2, Offset, Acc) ->
    shift_rows(T, Y2, Offset, Acc).

shift_cols([], _, _, Acc) -> Acc;
shift_cols([#local_obj{path = P, obj = {column, {MX1, MX2}}} = H | T],
           X2, Offset, Acc)
  when MX1 >= X2 ->
    NO = {column, {MX1 + Offset, MX2 + Offset}},
    NP = hn_util:list_to_path(binary_to_term(P)),
    NewRevIdx = term_to_binary(NP ++ hn_util:obj_to_ref(NO)),
    NewCol = H#local_obj{obj = NO, revidx = NewRevIdx},
    shift_cols(T, X2, Offset, [NewCol | Acc]);
shift_cols([_H | T], X2, Offset, Acc) ->
    shift_cols(T, X2, Offset, Acc).

shift_cellsD(#refX{site = Site, obj =  Obj} = From, Type, Disp, Rewritten, Uid)
  when (Type ==  insert orelse Type ==  delete) andalso
       (Disp ==  vertical orelse Disp ==  horizontal) ->
    {XOff, YOff} = hn_util:get_offset(Type, Disp, Obj),
    RefXSel = shift_pattern(From, Disp),
    % mark the refs dirty for zinfs to force recalc
    [ok = mark_dirty_for_zinfD(X) || X <- expand_ref(From)],
    case read_objs(RefXSel, inside) of
        []       ->
            [];
        ObjsList ->
            % We might need to rewrite the formulas and dynamic selectors of
            % all the child cells so lets collect them
            RefXList = [lobj_to_xrefX(Site, O) || O <- ObjsList],
            ChildCells = lists:flatten([get_childrenD(X) || X <- RefXList]),
            ChildCells2 = hslists:uniq(ChildCells),
            DedupedChildren = lists:subtract(ChildCells2, Rewritten),

            % Rewrite formulas that are affected
            Formulas = [F || X <- DedupedChildren,
                             F <- read_ref_field(X, "formula", write)],
            Acc = {{From, {XOff, YOff}, Uid}, []},
            {_, DirtyCh1} = lists:foldl(fun rewrite_formulae/2, Acc, Formulas),
            % now rewrite any dynamic selects that are affected
            DynSels = [F || X <- DedupedChildren,
                            F <- read_ref_field(X, "input", write)],
            {_, DirtyCh2} = lists:foldl(fun rewrite_dyn_select/2, Acc, DynSels),

            % Rewrite the local_obj entries by applying the shift offset.
            ObjTable = trans(Site, local_obj),
            [begin
                 mnesia:delete(ObjTable, Idx, write),
                 mnesia:write(ObjTable, shift_obj(LO, XOff, YOff), write)
             end || #local_obj{idx = Idx} = LO <- ObjsList],
            lists:merge(DirtyCh1, DirtyCh2)
    end.

rewrite_dyn_select({ChildRef, DynSel}, {{From, {XOff, YOff}, Uid}, Acc}) ->
  case DynSel of
      {"dynamic_select", DynFormula, _} ->
          {St, "="++DF2} = offset_fm_w_rng(ChildRef, "="++DynFormula, From,
                                     {XOff, YOff}),
          Op = fun(Attrs) ->
                       % don't worry about calculating the values for the
                       % dynamic select as we are going to make the cell
                       % recalc itself anyhoo
                       DynSel2 = {"dynamic_select", DF2, []},
                       {St, orddict:store("input", DynSel2, Attrs)}
               end,
          apply_to_attrsD(ChildRef, Op, rewrite, Uid,
                          ?TRANSFORMATIVE),
          % you need to switch the ref to an idx because later on
          % you are going to move the cells and you need to know
                                   % the ref of the shifted cell
          case St of
              clean -> {{From, {XOff, YOff}, Uid}, Acc};
              dirty -> {{From, {XOff, YOff}, Uid}, [ChildRef | Acc]}
          end;
      _ ->
          {{From, {XOff, YOff}, Uid}, Acc}
  end.

rewrite_formulae({ChildRef, F1}, {{From, {XOff, YOff}, Uid}, Acc}) ->
    {St, F2} = offset_fm_w_rng(ChildRef, F1, From, {XOff, YOff}),
    Op = fun(Attrs) ->
                 {St, orddict:store("formula", F2, Attrs)}
         end,
    apply_to_attrsD(ChildRef, Op, rewrite, Uid, ?TRANSFORMATIVE),
    % you need to switch the ref to an idx because later on
    % you are going to move the cells and you need to know
    % the ref of the shifted cell
    case St of
        clean -> {{From, {XOff, YOff}, Uid}, Acc};
        dirty -> {{From, {XOff, YOff}, Uid}, [ChildRef | Acc]}
    end.

shift_pattern(#refX{obj = {cell, {X, Y}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {X, Y, X, infinity}}};
shift_pattern(#refX{obj = {cell, {X, Y}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X, Y, infinity, Y}}};
shift_pattern(#refX{obj = {range, {X1, Y1, X2, _Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {X1, Y1, X2, infinity}}};
shift_pattern(#refX{obj = {range, {X1, Y1, _X2, Y2}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X1, Y1, infinity, Y2}}};
shift_pattern(#refX{obj = {row, {Y1, _Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {0, Y1, infinity, infinity}}};
shift_pattern(#refX{obj = {column, {X1, _X2}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X1, 0, infinity, infinity}}}.

shift_obj(#local_obj{path = P, obj = {cell, {X, Y}}} = LO, XOff, YOff) ->
    O2 = {cell, {X + XOff, Y + YOff}},
    RevIdx2 = hn_util:list_to_path(binary_to_term(P)) ++ hn_util:obj_to_ref(O2),
    LO#local_obj{obj = O2, revidx = term_to_binary(RevIdx2)};
shift_obj(#local_obj{path = P, obj = {column, {X1, X2}}} = LO, XOff, _YOff) ->
    O2 = {column, {X1 + XOff, X2 + XOff}},
    RevIdx2 = hn_util:list_to_path(binary_to_term(P)) ++ hn_util:obj_to_ref(O2),
    LO#local_obj{obj = O2, revidx = term_to_binary(RevIdx2)};
shift_obj(#local_obj{path = P, obj = {row, {Y1, Y2}}} = LO, _XOff, YOff) ->
    O2 = {row, {Y1 + YOff, Y2 + YOff}},
    RevIdx2 = hn_util:list_to_path(binary_to_term(P)) ++ hn_util:obj_to_ref(O2),
    LO#local_obj{obj = O2, revidx = term_to_binary(RevIdx2)};
shift_obj(LO, _, _) -> LO.

-spec get_childrenD(#xrefX{}) -> [cellidx()].
get_childrenD(#xrefX{site = Site, obj = {cell, _}} = XRefX) ->
    Table = trans(Site, relation),
    Idxs = case mnesia:read(Table, XRefX#xrefX.idx, read) of
               [R] -> R#relation.children;
               _   -> []
           end,
    [idx_to_xrefXD(Site, X) || X <- Idxs];
get_childrenD(#xrefX{obj = {Type, _}} = Ref)
  when (Type ==  row) orelse (Type ==  column) orelse
       (Type ==  range) orelse (Type ==  page) ->
        lists:flatten(expand_ref(Ref)).

-spec deref_formula(string(), #xrefX{}, atom(), auth_srv:uid()) ->
    {clean | dirty, #refX{}}.
deref_formula(XRefX, DelRef, Disp, Uid) ->
    Op = fun(Attrs) ->
                 {St2, A2} = case orddict:find("formula", Attrs) of
                                 {ok, F1} ->
                                     {St, F2} = deref(XRefX, F1, DelRef, Disp),
                                     {St, orddict:store("formula", F2, Attrs)};
                                 _ ->
                                     {clean, Attrs}
                             end,
                 case is_dynamic_select(Attrs) of
                     true ->
                         {ok, V} = orddict:find("input", Attrs),
                         {"dynamic_select", Dyn, _} = V,
                         case deref(XRefX, "=" ++ Dyn, DelRef, Disp) of
                             {clean, "=" ++ _NewDyn} ->
                                 {merge(clean, St2), A2};
                             {dirty, "=" ++ NewDyn} ->
                                 % this cell is going to be marked as dirty
                                 % and recalced, so what we do is not bother
                                 % getting the values for the dyn select
                                 % gonnae get them on the recalc anyhoo
                                 NInp = {"dynamic_select", NewDyn, []},
                                 {dirty, orddict:store("input", NInp, A2)}
                         end;
                     false ->
                         {St2, A2}
                 end
         end,
    {Status, _} = apply_to_attrsD(XRefX, Op, deref, Uid, ?NONTRANSFORMATIVE),
    {Status, XRefX}.

merge(clean, clean) -> clean;
merge(_, _)         -> dirty.

%% dereferences a formula
deref(XChildX, [$= |Formula], DeRefX, Disp)
  when is_record(DeRefX, refX) ->
    {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {1, 1}),
    NewToks = deref1(XChildX, Toks, DeRefX, Disp, []),
    hn_util:make_formula(clean, NewToks);
deref(_XChildX, Formula, _DeRefX, _Disp)
  when is_record(_DeRefX, refX) ->
    {clean, Formula}.

deref1(_XChildX, [], _DeRefX, _Disp, Acc) -> lists:reverse(Acc);
deref1(XChildX, [{rangeref, _, #rangeref{path = Path, text = Text}} = H | T],
       DeRefX, Disp, Acc) ->
    #xrefX{path = CPath} = XChildX,
    #refX{path = DPath, obj = Obj1} = DeRefX,
    PathCompare = muin_util:walk_path(CPath, Path),
    NewTok = case PathCompare of
                 DPath ->
                     % only deref the range if it is completely obliterated
                     % by the deletion
                     Range = muin_util:just_ref(Text),
                     Prefix = case muin_util:just_path(Text) of
                                  "/" -> [];
                                  Pre -> Pre
                              end,
                     case deref_overlap(Range, Obj1, Disp) of
                         {deref, "#REF!"} -> {deref, Prefix ++ "#REF!"};
                         {recalc, Str}    -> {recalc, Prefix ++ Str};
                         {formula, Str}   -> {formula, Prefix ++ Str}
                     end;
                 _ -> H
             end,
    deref1(XChildX, T, DeRefX, Disp, [NewTok | Acc]);
deref1(XChildX, [{cellref, _, #cellref{path = Path, text = Text}} = H | T],
       DeRefX, Disp, Acc) ->
    NewTok = deref2(XChildX, H, Text, Path, DeRefX, Disp),
    deref1(XChildX, T, DeRefX, Disp, [NewTok | Acc]);
deref1(XChildX, [H | T], DeRefX, Disp, Acc) ->
    deref1(XChildX, T, DeRefX, Disp, [H | Acc]).

%% sometimes Text has a prepended slash
deref2(XChildX, H, [$/|Text], Path, DeRefX, Disp) ->
    Deref2 = deref2(XChildX, H, Text, Path, DeRefX, Disp),
    case Deref2 of
        H              -> H;
        {deref,   Str} -> {deref,   "/" ++ Str};
        {recalc,  Str} -> {recalc,  "/" ++ Str};
        {formula, Str} -> {formula, "/" ++ Str}
    end;
%% special case for ambiguous parsing of division
%% this matches on cases like  = a1/b3
deref2(_XChildX, _H, Text, "/", DeRefX, Disp) ->
    #refX{obj = Obj1} = DeRefX,
    deref_overlap(Text, Obj1, Disp);
deref2(XChildX, H, Text, Path, DeRefX, Disp) ->
    #xrefX{path = CPath} = XChildX,
    #refX{path = DPath, obj = Obj1} = DeRefX,
    PathCompare = muin_util:walk_path(CPath, Path),
    case PathCompare of
        DPath -> case Path of
                     "./" -> {deref, "#REF!"};
                     _P   -> S1 = muin_util:just_path(Text),
                             S2 = muin_util:just_ref(Text),
                             case deref_overlap(S2, Obj1, Disp) of
                                 {deref, "#REF!"} -> {deref, S1 ++ "#REF!"};
                                 O                -> S1 ++ O
                             end
                 end;
        _Else -> H
    end.

%% if DelObj completely subsumes RewriteObj then the reference to RewriteObj should
%% be dereferenced (return 'deref')
%% %% if DelObj partially subsumes RewriteObj then the reference to RewriteObj should
%% be rewritten (return 'rewrite')
%% if there is partial or no overlap then return 'unchanged'
deref_overlap(Text, DelObj, Disp) ->
    RewriteObj = hn_util:parse_ref(Text),
    % the first thing we do is check each corner of RewriteObj to see if it is inside
    % DelObj. Depending on the pattern of corners we rewrite the formula
    % - if all 4 corners are in the delete area the range must be dereferenced
    % - if 2 corners are in the delete area the formula must be rewritten
    % - if 1 corner is in the delete are the range must be forced to recalc
    %   because there is no way to rewrite it...
    % - (if 3 corners are in the delete area then the laws of Euclidean
    %   geometry have broken down and the end times are probably upon us
    %   so I would flee for my life sinner!)
    %
    % BUT if all 4 corners are outside the delete area we need to check again:
    % - if the delete area is wholy inside the range then the range must be deferenced
    % - if the delete area transpierces the range then the range must be rewritten
    {X1,  Y1,   X2,  Y2}  = expand(DelObj),
    {XX1, YY1,  XX2, YY2} = expand(RewriteObj),
    IntTL = intersect(XX1, YY1, X1, Y1, X2, Y2),
    IntBL = intersect(XX1, YY2, X1, Y1, X2, Y2),
    IntTR = intersect(XX2, YY1, X1, Y1, X2, Y2),
    IntBR = intersect(XX2, YY2, X1, Y1, X2, Y2),
    case {IntTL, IntBL, IntTR, IntBR} of
        % all included - deref!
        {in,  in,  in,  in}  -> {deref, "#REF!"};
        % none included you need to recheck in case the delete area
        % is contained in, or transects the target area
        {out, out, out, out} -> recheck_overlay(Text, DelObj, RewriteObj, Disp);
        % one corner included - deref!
        {in,  out, out, out} -> {recalc, Text};
        {out, in,  out, out} -> {recalc, Text};
        {out, out, in,  out} -> {recalc, Text};
        {out, out, out, in}  -> {recalc, Text};
        % two corners included rewrite
        {in,  in,  out, out} -> rewrite(X1, X2, RewriteObj, Text, left, Disp); % left del
        {out, out, in,  in}  -> rewrite(X1, X2, RewriteObj, Text, right, Disp);
        {in,  out, in,  out} -> rewrite(Y1, Y2, RewriteObj, Text, top, Disp);  % top del
        {out, in,  out, in}  -> rewrite(Y1, Y2, RewriteObj, Text, bottom, Disp);
        % transects are column/row intersects
        {transect, transect, out, out} -> rewrite(X1, X2, RewriteObj, Text, left, Disp); % left del
        {out, out, transect, transect} -> rewrite(X1, RewriteObj, Text, right, Disp);
        {transect, out, transect, out} -> rewrite(Y1, Y2, RewriteObj, Text, top, Disp);  % top del
        {out, transect, out, transect} -> rewrite(Y1, RewriteObj, Text, bottom, Disp);
        {transect, transect, transect, transect} -> {deref, "#REF!"}
    end.

%% this first clause catches rows/columns where the deleting object is a cell/range
%% in none of these cases does the formula dereference...
intersect(A1, A2, X1, Y1, X2, Y2)
  when (is_atom(A1) orelse is_atom(A2)) andalso
       (is_integer(X1) andalso is_integer(Y1)
        andalso is_integer(X2) andalso is_integer(Y2)) ->
    out;
%% cols/rows never dereference
intersect(A1, Y1, X1, A2, X2, A3)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(Y1) andalso is_integer(X1) andalso is_integer(X2)) ->
    out;
%% rows/cols never deference
intersect(X1, A1, A2, Y1, A3, Y2)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(X1) andalso is_integer(Y1) andalso is_integer(Y2)) ->
    out;
%% page deletes always dereference
intersect(_XX1, _YY1, zero, zero, inf, inf) ->
    out;
%% this is a row-row comparison
intersect(Type, YY1, zero, Y1, inf, Y2)
  when ((Type ==  zero) orelse (Type ==  inf)) ->
    if
        (YY1 >=  Y1), (YY1  =< Y2) -> transect;
        true                     -> out
    end;
%% this is a col-col comparison
intersect(XX1, Type, X1, zero, X2, inf)
  when ((Type ==  zero) orelse (Type ==  inf)) ->
    if
        (XX1 >=  X1), (XX1  =< X2) -> transect;
        true                     -> out
    end;
intersect(XX1, YY1, X1, Y1, X2, Y2) ->
    if
        % check for cell/range intersections
        (XX1 >=  X1),   (XX1  =< X2), (YY1 >=  Y1),  (YY1  =< Y2) -> in;
        % order matters - first check for rows that are included
        (XX1 >=  X1),   (XX1  =< X2), (zero ==  Y1), (inf ==  Y2) -> in;
        (zero ==  X1),  (inf ==  X2), (YY1 >=  Y1),  (YY1  =< Y2) -> in;
        % now check for partial intersections
        (XX1 ==  zero), (YY1 >=  Y1), (YY1  =< Y2)               -> in;
        (XX1 ==  inf),  (YY1 >=  Y1), (YY1  =< Y2)               -> in;
        (YY1 ==  zero), (XX1 >=  X1), (XX1  =< X2)               -> in;
        (YY1 ==  inf),  (XX1 >=  X1), (XX1  =< X2)               -> in;
        true                                                  -> out
    end.

%% rewrite/6
rewrite(X1O, X2O, {range, _}, Text, left, horizontal)   -> % tested
    {XD1, _X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1O, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - (X2O - X1O + 1)), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(_X1O, X2O, {range, _}, Text, left, vertical)   -> % tested
    {XD1, _X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X2O + 1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, Y2, 0),
    {recalc, S};

rewrite(Y1O, _Y2O, {range, _}, Text, bottom, _Disp) -> % tested
    {XD1, X1, YD1, Y1, XD2, X2, YD2, _Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y1O - 1), 0),
    {recalc, S};

rewrite(X1O, _X20, {range, _}, Text, right, horizontal) ->
    {XD1, X1, YD1, Y1, XD2, _X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X1O - 1), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(X1O, _X20, {range, _}, Text, right, vertical) ->
    {XD1, X1, YD1, Y1, XD2, _X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X1O - 1), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(Y1O, Y2O, {range, _}, Text, top, vertical) ->
    {XD1, X1, YD1, _Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1O, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - (Y2O - Y1O + 1)), 0),
    {recalc, S};

rewrite(_Y1O, Y2O, {range, _}, Text, top, horizontal) ->
    {XD1, X1, YD1, _Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y2O + 1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, Y2, 0),
    {recalc, S};

rewrite(Y1O, Y2O, {range, _}, Text, middle_row, vertical) ->
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - (Y2O - Y1O + 1)), 0),
    {recalc, S};

rewrite(X1O, X2O, {range, _}, Text, middle_column, horizontal)  ->
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - (X2O - X1O + 1)), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(X1O, X2O, {column, _}, Text, left, _Disp)   ->
    {XD1, _X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1O) ++ ":" ++ make_col(XD2, (X2 - (X2O - X1O + 1))),
    {recalc, S};

rewrite(Y1O, Y2O, {row, _}, Text, top, _Disp)   ->
    {YD1, _Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1O) ++ ":" ++ make_row(YD2, (Y2 - (Y2O - Y1O + 1))),
    {recalc, S}.

%% rewrite/5
rewrite(XO, {column, _}, Text, right, _Disp)  ->
    {XD1, X1, XD2, _X2} = parse_cols(Text),
    S = make_col(XD1, X1) ++ ":" ++ make_col(XD2, (XO - 1)),
    {recalc, S};

rewrite(XO, {column, _}, Text, middle, _Disp)  ->
    {XD1, X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1) ++ ":" ++ make_col(XD2, (X2 - XO)),
    {recalc, S};

rewrite(YO, {row, _}, Text, bottom, _Disp) ->
    {YD1, Y1, YD2, _Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (YO - 1)),
    {recalc, S};

rewrite(YO, {row, _}, Text, middle, _Disp) ->
    {YD1, Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (Y2 - YO)),
    {recalc, S}.

%% page deletes always derefence
recheck_overlay(_Text, {page, "/"}, _Target, _Disp) ->
    {deref, "#REF!"};
%% cell targets that haven't matched so far ain't gonna
recheck_overlay(Text, DelX, {cell, {X1, Y1}}, _Disp) ->
    {XX1, YY1, XX2, YY2} = expand(DelX),
    recheck2(Text, X1, Y1, XX1, YY1, XX2, YY2);
%% cell deletes that haven't matched a row or column so far ain't gonna
recheck_overlay(Text, {cell, _}, {Type, _}, _Disp)
  when ((Type ==  row) orelse (Type ==  column)) ->
    {formula, Text};
% different behaviours with a cell/range depending on if the range is
% a single row or a single col range
% single row
recheck_overlay(Text, {cell, {X1, Y1}},
                {range, {XX1, YY1, XX2, YY1}}, vertical) ->
    recheck2(Text, X1, Y1, XX1, YY1, XX2, YY1);
% single row
recheck_overlay(Text, {cell, {X1, Y1}},
                {range, {XX1, YY1, XX2, YY1}}, horizontal) ->
    if
        (XX1 < X1), (XX2 > X1), (YY1 ==  Y1) ->
            C1 = make_cell(false, XX1, 0, false, YY1, 0),
            C2 = make_cell(false, XX2, -1, false, YY1,  0),
            {recalc,  C1 ++ ":" ++ C2};
        true                                ->
            {formula, Text}
    end;
% single col
recheck_overlay(Text, {cell, {X1, Y1}},
                {range, {XX1, YY1, XX1, YY2}}, vertical) ->
    if
        (XX1 ==  X1), (YY1 < Y1), (YY2 > Y1) ->
            C1 = make_cell(false, XX1, 0, false, YY1, 0),
            C2 = make_cell(false, XX1, 0, false, YY2,  -1),
            {recalc,  C1 ++ ":" ++ C2};
        true                                ->
            {formula, Text}
    end;
% single col
recheck_overlay(Text, {cell, {X1, Y1}},
                {range, {XX1, YY1, XX1, YY2}}, horizontal) ->
    recheck2(Text, X1, Y1, XX1, YY1, XX1, YY2);
recheck_overlay(Text, {cell, {X1, Y1}},
                {range, {XX1, YY1, XX2, YY2}}, _Disp) ->
    recheck2(Text, X1, Y1, XX1, YY1, XX2, YY2);
% cols/rows cols/range comparisons always fail
recheck_overlay(Text, {Type, _}, {column, _}, _Disp)
  when ((Type ==  row) orelse (Type ==  range)) ->
    {formula, Text};
%% rows/cols comparisons always fail
recheck_overlay(Text, {Type, _}, {row, _}, _Disp)
  when ((Type ==  column) orelse (Type ==  range)) ->
    {formula, Text};
%% check a row/row
recheck_overlay(Text, {row, {X1, X2}},
                {row, {XX1, XX2}} = Tgt, Disp) ->
    if
        (X1 >=  XX1), (X1  =< XX2), (X2 >=  XX1), (X2  =< XX2) ->
            rewrite((X2 - X1 + 1), Tgt, Text, middle, Disp);
        true ->
            {formula, Text}
    end;
%% check a col/col
recheck_overlay(Text, {column, {Y1, Y2}}, {column, {YY1, YY2}} = Tgt, Disp) ->
    if
        (Y1 >=  YY1), (Y1  =< YY2), (Y2 >=  YY1), (Y2  =< YY2) ->
            rewrite((Y2 - Y1 + 1), Tgt, Text, middle, Disp);
        true ->
            {formula, Text}
    end;
%% check range/range
recheck_overlay(Text, {range, {X1, Y1, X2, Y2}},
                {range, {XX1, YY1, XX2, YY2}} = R, horizontal) ->
    if
        (X1 >=  XX1), (X1  =< XX2), (X2 >=  XX1), (X2  =< XX2),
        (Y1  =< YY1), (Y2 >=  YY1), (Y2  =< YY2) ->
            rewrite(X1, X2, R, Text, middle_column, horizontal);
        (X1 >=  XX1), (X1  =< XX2), (X2 >=  XX1), (X2  =< XX2),
        (Y1 >=  YY1), (Y1  =< YY2), (Y2 >=  YY2) ->
            rewrite(X1, X2, R, Text, middle_column, horizontal);
        (X1 >=  XX1), (X1  =< XX2), (X2 >=  XX1), (X2  =< XX2),
        (Y1  =< YY1), (Y2 >=  YY1)                           ->
            rewrite(X1, X2, R, Text, middle_column, horizontal);
        ((X1 >=  XX1) andalso (X1  =< XX2))
        orelse ((X2 >=  XX1) andalso  (X2  =< XX2))
        orelse ((Y1 >=  YY1) andalso (Y1  =< YY2))
        orelse ((Y2 >=  YY1) andalso (Y2  =< YY2)) ->
            {recalc, Text};
        true                                               ->
            {formula, Text}
    end;
recheck_overlay(Text, {range, {X1, Y1, X2, Y2}},
                {range, {XX1, YY1, XX2, YY2}} = R, vertical) ->
    if
        (X1  =< XX1), (X2 >=  XX1), (X2  =< XX2),
        (Y1 >=  YY1), (Y1  =< YY2), (Y2 >=  YY1), (Y2  =< YY2) ->
            rewrite(Y1, Y2, R, Text, middle_row, vertical);
        (X1 >=  XX1), (X1  =< XX2), (X2 >=  XX2),
        (Y1 >=  YY1), (Y1  =< YY2), (Y2 >=  YY1), (Y2  =< YY2) ->
            rewrite(Y1, Y2, R, Text, middle_row, vertical);
        (X1  =< XX1), (X2 >=  XX2),
        (Y1 >=  YY1), (Y1  =< YY2), (Y2 >=  YY1), (Y2  =< YY2) ->
            rewrite(Y1, Y2, R, Text, middle_row, vertical);
        ((X1 >=  XX1) andalso (X1  =< XX2))
        orelse ((X2 >=  XX1) andalso (X2  =< XX2))
        orelse ((Y1 >=  YY1) andalso (Y1  =< YY2))
        orelse ((Y2 >=  YY1) andalso (Y2  =< YY2)) ->
            {recalc, Text};
        true                                               ->
            {formula, Text}
    end;
%% check range/column
recheck_overlay(Text, {column, {range, {X1, zero, X2, inf}}},
                {range, _} = Tgt, Disp) ->
    recheck_overlay(Text, {column, {X1, X2}}, Tgt, Disp);
recheck_overlay(Text, {column, {X1, X2}},
                {range, {XX1, _YY1, XX2, _YY2}} = Tgt, Disp) ->
    if
        (X1 >=  XX1), (X1  =< XX2), (X2 >=  XX1), (X2  =< XX2) ->
            rewrite(X1, X2, Tgt, Text, middle_column, Disp);
        true -> {formula, Text}
    end;
%% check range/row
recheck_overlay(Text, {row, {range, {zero, Y1, inf, Y2}}},
                {range, _} = Tgt, Disp) ->
    recheck_overlay(Text, {row, {Y1, Y2}}, Tgt, Disp);
recheck_overlay(Text, {row, {Y1, Y2}},
                {range, {_XX1, YY1, _XX2, YY2}} = Tgt, Disp) ->
    if
        (Y1 >=  YY1), (Y1  =< YY2), (Y2 >=  YY1), (Y2  =< YY2) ->
            rewrite(Y1, Y2, Tgt, Text, middle_row, Disp);
        true -> {formula, Text}
    end.

recheck2(Text, X1, Y1, XX1, YY1, XX2, YY2) ->
    case {lteq(XX1, X1), gteq(XX2, X1), lteq(YY1, Y1), gteq(YY2, Y1)} of
        {true, true, true, true} -> {recalc, Text};
        _                        -> {formula, Text}
    end.

gteq(N, zero) when is_integer(N) -> true;
gteq(zero, N) when is_integer(N) -> false;
gteq(N, inf) when is_integer(N)  -> false;
gteq(inf, N) when is_integer(N)  -> true;
gteq(inf, inf) -> true;
gteq(N1, N2) when is_integer(N1), is_integer(N2) -> (N1 >=  N2).

lteq(N, zero) when is_integer(N) -> false;
lteq(zero, N) when is_integer(N) -> true;
lteq(N, inf) when is_integer(N)  -> true;
lteq(inf, N) when is_integer(N)  -> false;
lteq(inf, inf) -> true;
lteq(N1, N2) when is_integer(N1), is_integer(N2) -> (N1  =< N2).

expand({cell, {X, Y}})                      -> {X, Y, X, Y};
expand({range, {X1, Y1, X2, Y2}})           -> {X1, Y1, X2, Y2};
expand({column, {range, {X1, Y1, X2, Y2}}}) -> {X1, Y1, X2, Y2};
expand({row, {range, {X1, Y1, X2, Y2}}})    -> {X1, Y1, X2, Y2};
expand({column, {X1, X2}})                  -> {X1, zero, X2, inf}; % short for infinity
expand({row, {Y1, Y2}})                     -> {zero, Y1, inf, Y2}; % short for infinity
expand({page, "/"})                         -> {zero, inf, zero, inf}.

-spec delete_relationD(#xrefX{}) -> ok.
delete_relationD(#xrefX{site = Site} = Cell) ->
    Tbl = trans(Site, relation),
    case mnesia:read(Tbl, Cell#xrefX.idx, write) of
        [R] ->
            % delete infinite relations and stuff
            OldInfPs = [#xrefX{} = idx_to_xrefXD(Site, X)
                        || X <- R#relation.infparents],
            OldDynInfPs = [#xrefX{} = idx_to_xrefXD(Site, X)
                        || X <- R#relation.dyn_infparents],
            OldPs = lists:merge(OldInfPs, OldDynInfPs),
            ok = handle_infsD(Cell#xrefX.idx, Site, [], OldPs),
            [del_childD(P, Cell#xrefX.idx, Tbl) ||
                P <- R#relation.parents],
            [del_childD(P, Cell#xrefX.idx, Tbl) ||
                P <- R#relation.dyn_parents],
            ok = mnesia:delete(Tbl, Cell#xrefX.idx, write);
        _ -> ok
    end.

offset_formula(Formula, {XO, YO}) ->
%% the xfl_lexer:lex takes a cell address to lex against
%% in this case {1, 1} is used because the results of this
%% are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> NewToks = d_n_d_c_n_p_offset(Toks, XO, YO),
                         {_St, NewFormula} = hn_util:make_formula(clean, NewToks),
                         NewFormula;
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                   "formula in offset_formula but "++
                                   "you do~n-~p~n", [Formula]),
                         " = " ++ Formula
    end.

%% different to offset_formula because it truncates ranges
offset_fm_w_rng(XCell, [$= |Formula], From, Offset) ->
    % the xfl_lexer:lex takes a cell address to lex against
    % in this case {1, 1} is used because the results of this
    % are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> {Status, NewToks} = offset_with_ranges(Toks, XCell,
                                                                From, Offset),
                         hn_util:make_formula(Status, NewToks);
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                   "formula in offset_fm_w_rng but "++
                                   "you do~n-~p~n", [Formula]),
                         {[], " = " ++ Formula}
    end.

offset_with_ranges(Toks, XCell, From, Offset) ->
    offset_with_ranges1(Toks, XCell, From, Offset, clean, []).

offset_with_ranges1([], _XCell, _From, _Offset, Status, Acc) ->
    {Status, lists:reverse(Acc)};
offset_with_ranges1([{rangeref, LineNo,
                      #rangeref{path = Path, text = Text} = H} | T],
                    XCell, #refX{path = FromPath} = From,
                    Offset, Status, Acc) ->
    #xrefX{path = CPath} = XCell,
    PathCompare = muin_util:walk_path(CPath, Path),
    Range = muin_util:just_ref(Text),
    Prefix = muin_util:just_path(Text),
    [Cell1|[Cell2]] = string:tokens(Range, ":"),
    {X1D, X1, Y1D, Y1} = parse_cell(Cell1),
    {X2D, X2, Y2D, Y2} = parse_cell(Cell2),
    {St, NewText} = case PathCompare of
                        FromPath -> make_new_range(Prefix, Cell1, Cell2,
                                                   {X1D, X1, Y1D, Y1},
                                                   {X2D, X2, Y2D, Y2},
                                                   From, Offset);
                        _        -> {clean, Text}
                    end,
    NewAcc = {rangeref, LineNo, H#rangeref{text = NewText}},
    NewStatus = case St of
                    dirty -> dirty;
                    clean -> Status
                end,
    offset_with_ranges1(T, XCell, From, Offset, NewStatus, [NewAcc | Acc]);
%% need to disambiuate division for formula like  = a1/b2
offset_with_ranges1([{cellref, LineNo,
                      C = #cellref{path = "/", text = [$/ |Tx]}} | T],
                    XCell, From, Offset, Status, Acc) ->
    List = [{'/', 1}, {cellref, LineNo, C#cellref{path = "./", text = Tx}} | T],
    offset_with_ranges1(List, XCell, From, Offset, Status, Acc);
offset_with_ranges1([{cellref, LineNo,
                      C = #cellref{path = Path, text = Text}} = H | T],
                    XCell, #refX{path = FromPath} = From,
                    {XO, YO} = Offset, Status, Acc) ->
    {XDollar, X, YDollar, Y} = parse_cell(muin_util:just_ref(Text)),
    case From#refX.obj of
        % If ever we apply two offsets at once, do it in two steps.
        % handle old and new rows/columns
        {range, {Left, Top, _Right, Bottom}}
        when (YO ==  0) and ((Left > X) or (Top > Y) or (Y > Bottom)) ->
            offset_with_ranges1(T, XCell, From, Offset, Status, [H | Acc]);
        {range, {Left, Top, Right, _Bottom}}
        when (XO ==  0) and ((Left > X) or (X > Right) or (Top > Y)) ->
            offset_with_ranges1(T, XCell, From, Offset, Status, [H | Acc]);
        {column, {range, {Left, zero, _Right, inf}}} when X < Left ->
            offset_with_ranges1(T, XCell, From, Offset, Status, [H | Acc]);
        {row,{range, {zero, Top, inf, _Bottom}}} when Y < Top ->
            offset_with_ranges1(T, XCell, From, Offset, Status, [H | Acc]);
        {column, {Left,_Right}} when X < Left ->
            offset_with_ranges1(T, XCell, From, Offset, Status, [H | Acc]);
        {row,{Top,_Bottom}} when Y < Top ->
            offset_with_ranges1(T, XCell, From, Offset, Status, [H | Acc]);
        _Else ->
            #xrefX{path = CPath} = XCell,
            Prefix = case muin_util:just_path(Text) of
                         "/"   -> "";
                         Other -> Other
                     end,
            PathCompare = muin_util:walk_path(CPath, Path),
            NewCell  =
                case PathCompare of
                    FromPath -> Prefix ++ make_cell(XDollar, X, XO,
                                                    YDollar, Y, YO);
                    _        -> Text
                end,
            NewAcc = {cellref, LineNo, C#cellref{text = NewCell}},
            offset_with_ranges1(T, XCell, From, {XO, YO}, Status, [NewAcc | Acc])
    end;
% all zcellrefs need to recalc
offset_with_ranges1([{zcellref, _, _, _} = H | T], XCell, From, Offset,
                    _Status, Acc) ->
    offset_with_ranges1(T, XCell, From, Offset, dirty, [H | Acc]);
offset_with_ranges1([H | T], XCell, From, Offset, Status, Acc) ->
    offset_with_ranges1(T, XCell, From, Offset, Status, [H | Acc]).

parse_cell(Cell) ->
    {XDollar, Rest} = is_fixed(Cell),
    Fun = fun(XX) ->
                  if XX < 97  -> false;
                     XX > 122 -> false;
                     true     -> true
                  end
          end,
    {XBits, YBits} = lists:partition(Fun,string:to_lower(Rest)),
    {YDollar, Y} = is_fixed(YBits),
    {XDollar, tconv:to_i(XBits), YDollar, list_to_integer(Y)}.

parse_range(Range) ->
    [Cell1, Cell2] = string:tokens(Range, ":"),
    {XD1, X1, YD1, Y1} = parse_cell(Cell1),
    {XD2, X2, YD2, Y2} = parse_cell(Cell2),
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2}.

parse_cols(Cols) ->
    [Col1, Col2] = string:tokens(Cols, ":"),
    {XD1, R1} = is_fixed(Col1),
    {XD2, R2} = is_fixed(Col2),
    {XD1, tconv:to_i(R1), XD2, tconv:to_i(R2)}.

parse_rows(Rows) ->
    [Row1, Row2] = string:tokens(Rows, ":"),
    {YD1, R1} = is_fixed(Row1),
    {YD2, R2} = is_fixed(Row2),
    {YD1, list_to_integer(R1), YD2, list_to_integer(R2)}.

is_fixed([$$|Rest]) -> {true, Rest};
is_fixed(List)      -> {false, List}.

%% make_cell makes a cell with dollars and stuff based on the offset
make_cell(false, X, XOffset, false, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, false, Y, YOffset) ->
    [$$] ++ tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(false, X, XOffset, true, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, true, Y, YOffset)  ->
    [$$] ++ tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset).

make_col(false, X) -> tconv:to_b26(X);
make_col(true,  X) -> [$$] ++ X.

make_row(false, Y) -> tconv:to_s(Y);
make_row(true,  Y) -> [$$] ++ tconv:to_s(Y).

diff( FX, _FY,  TX, _TY, horizontal) -> TX - FX;
diff(_FX,  FY, _TX,  TY, vertical)   -> TY - FY.

%% handle cells
make_new_range(Prefix, Cell1, Cell2,
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {cell, {X, Y}}} = _From, {XO, YO}) ->
    {St1, NC1}  =
        case {X1, Y1} of
            {X, Y} -> {dirty, make_cell(X1D, X1, XO, Y1D, Y1, YO)};
            _      -> {clean, Cell1}
        end,
    {St2, NC2}  =
        case {X2, Y2} of
            {X, Y} -> {dirty, make_cell(X2D, X2, XO, Y2D, Y2, YO)};
            _      -> {clean, Cell2}
        end,
    Ret = Prefix ++ NC1 ++ ":" ++ NC2,
    case {St1, St2} of
        {clean, clean} -> {clean, Ret};
        _              -> {dirty, Ret}
    end;
%% handle rows
make_new_range(Prefix, Cell1, Cell2,
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {row, {Top, _Bottom}}},
               {0 = _XOffset, YOffset}) ->
    {St1, NC1} = if Top  =< Y1 ->
                         {dirty, make_cell(X1D, X1, 0, Y1D, Y1, YOffset)};
                    true      ->
                         {clean, Cell1}
                 end,
    {St2, NC2} = if Top  =< Y2 ->
                         {dirty, make_cell(X2D, X2, 0, Y2D, Y2, YOffset)};
                    true      ->
                         {clean, Cell2}
                 end,
    Ret = Prefix ++ NC1 ++ ":" ++ NC2,
    case {St1, St2} of
        {clean, clean} -> {clean, Ret};
        _              -> {dirty, Ret}
    end;
%% handle columns
make_new_range(Prefix, Cell1, Cell2,
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {column, {Left, _Right}}},
               {XOffset, 0 = _YOffset}) ->
    {St1, NC1} = if Left  =< X1 ->
                         {dirty, make_cell(X1D, X1, XOffset, Y1D, Y1, 0)};
                    true       ->
                         {clean, Cell1}
                 end,
    {St2, NC2} = if Left  =< X2 ->
                         {dirty, make_cell(X2D, X2, XOffset, Y2D, Y2, 0)};
                    true       ->
                         {clean, Cell2}
                 end,
    Ret = Prefix ++ NC1 ++ ":" ++ NC2,
    case {St1, St2} of
        {clean, clean} -> {clean, Ret};
        _              -> {dirty, Ret}
    end;
%% handle ranges (horizontal rewrite)
make_new_range(Prefix, Cell1, Cell2,
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {range, {_XA, YA, _XB, YB}}},
               {XOffset, 0}) ->
    Status = if
                 (YA  =< Y1 andalso YB >=  Y2) ->
                     NC1 = make_cell(X1D, X1, XOffset, Y1D, Y1, 0),
                     NC2 = make_cell(X2D, X2, XOffset, Y2D, Y2, 0),
                     dirty;
                 (YA >  Y1 orelse  YB <  Y2) ->
                     NC1 = Cell1,
                     NC2 = Cell2,
                     clean
             end,
    {Status, Prefix ++ NC1 ++ ":" ++ NC2};
%% handle ranges (horizontal rewrite)
make_new_range(Prefix, Cell1, Cell2,
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {range, {XA, _YA, XB, _YB}}},
               {0, YOffset}) ->
    Status = if
                 (XA  =< X1 andalso XB >=  X2) ->
                     NC1 = make_cell(X1D, X1, 0, Y1D, Y1, YOffset),
                     NC2 = make_cell(X2D, X2, 0, Y2D, Y2, YOffset),
                     dirty;
                 (XA >  X1 orelse  XB <  X2) ->
                     NC1 = Cell1,
                     NC2 = Cell2,
                     clean
             end,
    {Status, Prefix ++ NC1 ++ ":" ++ NC2}.

content_attrs() ->
    [
     "formula",
     "value",
     "preview",
     "overwrite-color",
     "__hasincs",
     "__hasform",
     "__rawvalue",
     "__ast",
     "__recompile",
     "__shared",
     "__area",
     "__default-align",
     "__lastcalced",
     "__unique"
    ].

copy_attributes(_SD, TD, []) -> TD;
copy_attributes(SD, TD, [Key|T]) ->
    case orddict:find(Key, SD) of
        {ok, V} -> copy_attributes(SD, orddict:store(Key, V, TD), T);
        _ -> copy_attributes(SD, TD, T)
    end.

%% used in copy'n'paste, drag'n'drops etc...
d_n_d_c_n_p_offset(Toks, XOffset, YOffset) ->
    d_n_d_c_n_p_offset1(Toks, XOffset, YOffset, []).

d_n_d_c_n_p_offset1([], _XOffset, _YOffset, Acc) -> lists:reverse(Acc);
d_n_d_c_n_p_offset1([{zcellref, LineNo, ZPath, #cellref{text = Text} = H} | T],
                    XOffset, YOffset, Acc) ->
    NewZPath = d_n_d_zpath(ZPath, XOffset, YOffset),
    Prefix = make_zpath(NewZPath, []),
    Cell = muin_util:just_ref(Text),
    {XDollar, X, YDollar, Y} = parse_cell(Cell),
    NewCell = drag_n_drop_cell(XDollar, X, XOffset, YDollar, Y, YOffset),
    NewRef = {zcellref, LineNo, NewZPath, H#cellref{text = Prefix ++ NewCell}},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewRef | Acc]);
d_n_d_c_n_p_offset1([{cellref, LineNo, #cellref{text = Text} = H} | T],
                    XOffset, YOffset, Acc) ->
    Cell = muin_util:just_ref(Text),
    Prefix = case muin_util:just_path(Text) of
                 "/"   -> "";
                 Other -> Other
             end,
    {XDollar, X, YDollar, Y} = parse_cell(Cell),
    NewCell = drag_n_drop_cell(XDollar, X, XOffset, YDollar, Y, YOffset),
    NewRef = {cellref, LineNo, H#cellref{text = Prefix ++ NewCell}},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewRef | Acc]);
d_n_d_c_n_p_offset1([{rangeref, LineNo, #rangeref{text = Text} = H} | T],
                    XOffset, YOffset, Acc) ->
    Range = muin_util:just_ref(Text),
    Pf = case muin_util:just_path(Text) of
             "/"   -> "";
             Other -> Other
         end,
    [Cell1 | [Cell2]] = string:tokens(Range, ":"),
    {X1D, X1, Y1D, Y1} = parse_cell(Cell1),
    {X2D, X2, Y2D, Y2} = parse_cell(Cell2),
    NC1 = drag_n_drop_cell(X1D, X1, XOffset, Y1D, Y1, YOffset),
    NC2 = drag_n_drop_cell(X2D, X2, XOffset, Y2D, Y2, YOffset),
    NewText = if
                  NC1 == "#REF!" orelse NC2 == "#REF!" ->
                      "#REF!";
                  NC1  =/=  "#REF!" andalso NC2  =/=  "#REF!" ->
                      Pf ++ NC1 ++ ":" ++ NC2
              end,
    NewR = {rangeref, LineNo, H#rangeref{text = NewText}},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewR | Acc]);
d_n_d_c_n_p_offset1([H | T], XOffset, YOffset, Acc) ->
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [H | Acc]).

d_n_d_zpath({zpath, ZPath}, XOffset, YOffset) ->
    d_n_d_zseg(ZPath, XOffset, YOffset, []).

d_n_d_zseg([], _XO, _YO, Acc)               -> lists:reverse(Acc);
d_n_d_zseg([{seg, _} = H | T], XO, YO, Acc) -> d_n_d_zseg(T, XO, YO, [H | Acc]);
d_n_d_zseg([{zseg, List, _Text} | T], XO, YO, Acc) ->
    NewZSeg = d_n_d_c_n_p_offset1(List, XO, YO, []),
    {_, [$= | NewZTxt]} = hn_util:make_formula(clean, NewZSeg),
    NewZTxt2 = lists:concat(["[", NewZTxt, "]"]),
    d_n_d_zseg(T, XO, YO, [{zseg, NewZSeg, NewZTxt2} | Acc]).

make_zpath([], Acc)                   -> lists:flatten(lists:reverse(Acc));
make_zpath([{seg, Txt} | T], Acc)     -> make_zpath(T, [Txt | Acc]);
make_zpath([{zseg, _, Txt} | T], Acc) -> make_zpath(T, [Txt | Acc]).

%% drag'n'drop_cell drags and drops a cell ignoring offsets
%% if the row/column part is fixed with a dollar
drag_n_drop_cell(false, X, XOffset, false, Y, YOffset) ->
    if
        (X + XOffset) <  1 orelse  (Y + YOffset) <  1 -> "#REF!";
        (X + XOffset) >=  1 andalso (Y + YOffset) >=  1 ->
            tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset)
    end;
drag_n_drop_cell(true, X, _XOffset, false, Y, YOffset) ->
    if
        X <  1 orelse ( Y + YOffset) <  1 -> "#REF!";
        X >=  1 andalso (Y + YOffset) >=  1 ->
            [$$] ++ tconv:to_b26(X) ++ tconv:to_s(Y + YOffset)
    end;
drag_n_drop_cell(false, X, XOffset, true, Y, _YOffset) ->
    if
        (X + XOffset) <  1 orelse  Y <  1 -> "#REF!";
        (X + XOffset) >=  1 andalso Y >=  1 ->
            tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y)
    end;
drag_n_drop_cell(true, X, _XOffset, true, Y, _YOffset)  ->
    if
        X <  1 orelse  Y <  1 -> "#REF!";
        X >=  1 andalso Y >=  1 ->
            [$$] ++ tconv:to_b26(X) ++ [$$] ++ tconv:to_s(Y)
    end.

%% Working from the bottom of the form to the top, find the first
%% local object which has content, and return its corresponding
%% ROW/COL
-spec largest_content([{integer(), #local_obj{}}], string()) -> integer().
largest_content([], _S) -> 0;
largest_content([{K, LO} | T], S) ->
    case has_content(S, LO) of
        true -> K;
        false -> largest_content(T, S)
    end.

-spec has_content(string(), #local_obj{}) -> boolean().
has_content(S, LO) ->
    case extract_field(read_attrs(S, [LO], read), "formula", []) of
        [] -> false;
        [{_, []}] -> false;
        _ -> true
    end.

get_page_logs(Logs) -> get_page_l(Logs, []).

get_page_l([], Acc)                              -> Acc;
get_page_l([#logging{obj = {cell, _}} | T], Acc) -> get_page_l(T, Acc);
get_page_l([H | T], Acc)                         -> get_page_l(T, [H | Acc]).

make_blank(#refX{obj = O}, Idx) ->
    [#logging{idx = Idx, obj = O, log = term_to_binary("This cell is blank")}].

%% merge_incs([], Incs) ->
%%     Incs;
%% merge_incs([#include{js = JS1, js_reload = JSR1, css = CSS1} = Inc | T],
%%            #incs{js = JS2, js_reload = JSR2, css = CSS2}) ->

%%     NewIncs = Inc#include{js = hslists:uniq(lists:merge(JS1, JS2)),
%%                           js_reload = hslists:uniq(lists:merge(JSR1, JSR2)),
%%                           css = hslists:uniq(lists:merge(CSS1, CSS2))},
%%     merge_incs(T, NewIncs).

shrink([])   -> [];
shrink(List) -> List2 = lists:sort(List),
                shrink2(List2, []).

shrink2([#dirty_for_zinf{dirty = D}], Acc) -> [D | Acc];
% if they are the same xrefX drop one
shrink2([#dirty_for_zinf{dirty = D} = H,
         #dirty_for_zinf{dirty = D} | T], Acc) ->
    shrink2([H | T], Acc);
shrink2([#dirty_for_zinf{dirty = D} | T], Acc) -> shrink2(T, [D | Acc]).

%% get_zinfs(List, CI) -> get_z2(List, CI, []).

%% get_z2([], _CI, Acc) -> lists:sort(lists:flatten(Acc));
%% get_z2([H | T], CI, Acc) ->
%%     Zs = zinf_srv:expand_zrefs(H),
%%     Zs2 = lists:merge([H], Zs),
%%     NewAcc = [{X, CI} || X <- Zs2],
%%     get_z2(T, CI, [NewAcc | Acc]).

clean_up(#xrefX{site = S} = XRefX, Attrs) ->
    case lists:keymember("__hasform", 1, Attrs) of
        true  -> ok = unattach_formD(XRefX);
        false -> ok
    end,
    case lists:keymember("__hasincs", 1, Attrs) of
        true  -> ok = delete_incsD(XRefX);
        false -> ok
    end,
    case lists:keymember("__hastimer", 1, Attrs) of
        true  -> ok = delete_timerD(XRefX);
        false -> ok
    end,
    case lists:keymember("__hasphone", 1, Attrs) of
        true  -> ok = unattach_phoneD(XRefX);
        false -> ok
    end,
    case lists:keymember("__unique", 1, Attrs) of
        true  -> Unique = orddict:fetch("__unique", Attrs),
                 ok = delete_kvD(S, Unique);
        false -> ok
    end.

clean_up(#xrefX{} = XRefX) ->
    % trash any includes that might pertain
    % TODO this contains a lot of reads...
    ok = delete_incsD(XRefX),
    % trash any timers that might pertain
    % TODO this contains lot of reads
    ok = delete_timerD(XRefX),
    % trash any forms that might pertain
    % TODO this contains a lot of reads
    ok = unattach_formD(XRefX).

is_dynamic_select(Attrs) ->
    case orddict:find("input", Attrs) of
        {ok, Val} -> case Val of
                         {"dynamic_select", _, _} -> true;
                         _                        -> false
                     end;
        error     -> false
    end.

split_select(String) ->
    [Ref | Htap] = lists:reverse(string:tokens(String, "/")),
    Path = case lists:reverse(Htap) of
               [".." | Rest ] ->
                   ".." ++ hn_util:list_to_path(Rest);
               ["."  | Rest ] ->
                   "."  ++ hn_util:list_to_path(Rest);
               SimplePath ->
                   hn_util:list_to_path(SimplePath)
           end,
    {Path, Ref}.
