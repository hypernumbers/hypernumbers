%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       new db work unites
%%%            old hn_db_api.erl poured in and rewritten
%%% @end
%%% Created :  5 Apr 2011 by gordon@hypernumbers.com

-module(new_db_api).

-include("spriki.hrl").
-include("hypernumbers.hrl").
-include("keyvalues.hrl").
-include("syslib.hrl").

-export([
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

% the write_activty fun is exposed for use in debugging only
% should not be called directly from outside this module
-export([
         write_activity_DEBUG/4
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% API Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
revert(#refX{} = RefX, Revision, UID) ->
    Report = mnesia_mon:get_stamp("revert"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:revertD(RefX, Revision, UID)
          end,
    write_activity(RefX, Fun, "quiet", Report).

clear_factory(Site) ->
    Report = mnesia_mon:get_stamp("clear_factory"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:delete_kvD(Site, factory),
                  new_db_wu:mark_site_dirtyD(Site)
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet", Report).

make_factory(Site) ->
    make_factory2(Site, all).

make_factory(Site, List) when is_list(List) ->
    make_factory2(Site, List).

make_factory2(Site, Terms) ->
    Report = mnesia_mon:get_stamp("make_factory"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:write_kvD(Site, factory, Terms),
                  new_db_wu:mark_site_dirtyD(Site)
          end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet", Report).

mark_site_dirty(Site) ->
    Report = mnesia_mon:get_stamp("mark_site_dirty"),
    Fun = fun() ->
                   mnesia_mon:report(Report),
                   new_db_wu:mark_site_dirtyD(Site)
           end,
    % use a spoof RefX for write activity
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    write_activity(RefX, Fun, "quiet", Report).

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
    Report1 = mnesia_mon:get_stamp("mark_idx_dirty (1)"),
    Fun1 = fun() ->
                   mnesia_mon:report(Report1),
                   new_db_wu:idx_to_xrefXD(Site, Idx)
           end,
    XRefX = mnesia_mon:log_act(transaction, Fun1, Report1),
    Report2 = mnesia_mon:get_stamp("mark_idx_dirty (2)"),
    Fun2 = fun() ->
                   mnesia_mon:report(Report1),
                   ok = new_db_wu:mark_these_dirtyD([XRefX], nil)
           end,
    RefX = hn_util:xrefX_to_refX(XRefX),
    write_activity(RefX, Fun2, "quiet", Report2).

read_timers(Site) ->
    Report = mnesia_mon:get_stamp("read_timers"),
    Fun = fun() ->
                  new_db_wu:read_timersD(Site)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

read_includes(#refX{obj = {page, "/"}} = RefX) ->
    Report = mnesia_mon:get_stamp("read_includes"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  XRefX = new_db_wu:refX_to_xrefX_createD(RefX),
                  unpack_incs(new_db_wu:read_incsD(XRefX))
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

-spec handle_circref_cell(string(), cellidx(), auth_srv:auth_spec()) -> ok.
handle_circref_cell(Site, Idx, Ar) ->
    Report = mnesia_mon:get_stamp("handle_circref_cell"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  Cell = new_db_wu:idx_to_xrefXD(Site, Idx),
                  _Dict = new_db_wu:write_attrs(Cell,
                                                [{"formula", "=#CIRCREF!"}],
                                                Ar),
                  ok
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

get_logs(RefX) when is_record(RefX, refX) ->
    Report = mnesia_mon:get_stamp("get_logs"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:get_logsD(RefX)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

%% Loads new dirty information into the recalc graph.
-spec load_dirty_since(term(), atom()) -> {term(), [cellidx()]}.
load_dirty_since(Since, QTbl) ->
    Report = mnesia_mon:get_stamp("load_dirty_since"),
    F = fun() ->
                mnesia_mon:report(Report),
                new_db_wu:load_dirty_sinceD(Since, QTbl)
        end,
    case mnesia_mon:log_act(transaction, F, Report) of
        []  -> {Since, []};
        Ret -> {SinceL, DirtyLL} = lists:unzip(Ret),
               Since2 = lists:max(SinceL),
               DirtyL = lists:usort(lists:flatten(DirtyLL)),
               {Since2, DirtyL}
    end.

% this fn is used in subscribing/unsubscribe operations for dirty_zinf
length_dirty_zinf_q(Site) ->
    Report = mnesia_mon:get_stamp("length_dirty_zinf_q"),
    Fun  = fun() ->
                   mnesia_mon:report(Report),
                   new_db_wu:len_dirty_zinf_qD(Site)
           end,
    mnesia_mon:log_act(transaction, Fun, Report).

%% this function marks all dirty zinfs as unprocessed
%% designed to be used for zinf_srv initing in case of
%% a hard failure AND NOT FOR ANY OTHER REASON
reset_dirty_zinfs(Site) ->
    Report = mnesia_mon:get_stamp("reset_dirty_zinfs"),
    Fun  = fun() ->
                   mnesia_mon:report(Report),
                   new_db_wu:reset_dirty_zinfsD(Site)
           end,
    mnesia_mon:log_act(transaction, Fun, Report).

%% this function decides to maybe write the zinf tree
%% depending on the log queue
maybe_write_zinftree(Site, Tree, MaxSize) ->
    Report = mnesia_mon:get_stamp("maybe_write_zinftree"),
    Tbl = new_db_wu:trans(Site, dirty_zinf),
    Size = mnesia:table_info(Tbl, size),
    if
        Size > MaxSize ->
            Fun  = fun() ->
                           mnesia_mon:report(Report),
                           new_db_wu:maybe_write_zinftreeD(Site, Tree)
                   end,
            mnesia_mon:log_act(transaction, Fun, Report);
        Size =< MaxSize ->
            ok
    end.

process_dirty_zinfs(Site, Tree, AddFun, DelFun) ->
    Report = mnesia_mon:get_stamp("process_dirty_zinfs"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:proc_dirty_zinfsD(Site, Tree, AddFun, DelFun)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

process_dirties_for_zinf(Site, Tree, CheckFun) ->
    Report = mnesia_mon:get_stamp("process_dirties_for_zinf"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:proc_dirties_for_zinfD(Site, Tree, CheckFun)
          end,
    % using a spoof RefX because it is not going to the front end
    % (change that from a quiet report and watch it die!)
    RefX = #refX{site = Site},
    write_activity(RefX, Fun, "quiet", Report),
    ok.

idx_to_xrefX(Site, Idx) ->
    Report = mnesia_mon:get_stamp("get refX from idx"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:idx_to_xrefXD(Site, Idx)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

delete_siteonly(#refX{} = RefX, Type) ->
    Report = mnesia_mon:get_stamp("delete_siteonly"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:delete_siteonlyD(RefX, Type)
          end,
    mnesia_mon:oog_act(transaction, Fun, Report).

read_siteonly(#refX{} = RefX, Type) ->
    Report = mnesia_mon:get_stamp("read_siteonly"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:read_siteonlyD(RefX, Type)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

write_siteonly(#refX{} = RefX, Type, Payload) ->
    Report = mnesia_mon:get_stamp("write_siteonly"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  % this is a 'write once' record can only be
                  % one of each type per site
                  case new_db_wu:read_siteonly(RefX, Type) of
                      [] ->
                          new_db_wu:write_siteonlyD(RefX, Type, Payload);
                      [_R] ->
                          {error, exists}
                  end
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

get_phone(Site, Idx) ->
    Report = mnesia_mon:get_stamp("get_phone"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:get_phone(Site, Idx)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

get_phone(#refX{obj = {cell, _}} = RefX) ->
    Report = mnesia_mon:get_stamp("get_phone"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:get_phone(RefX)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

does_page_exist(#refX{obj = {page, "/"}} = RefX) ->
    Report = mnesia_mon:get_stamp("does_page_exists"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:does_page_exist(RefX)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

delete_api(Site, PublicKey) ->
    Report = mnesia_mon:get_stamp("delete_api"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:delete_apiD(Site, PublicKey)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

write_api(Site, #api{} = API) ->
    Report = mnesia_mon:get_stamp("write_api"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:write_apiD(Site, API)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

get_api_keys(Site) ->
    Report = mnesia_mon:get_stamp("get_api_keys"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:get_api_keysD(Site)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

read_api(Site, PublicKey) ->
    Report = mnesia_mon:get_stamp("read_api"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:read_apiD(Site, PublicKey)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

delete_user_fn(Site, FnName) ->
    Report = mnesia_mon:get_stamp("delete_user_fn"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:delete_user_fnD(Site, FnName)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

write_user_fn(Site, #user_fns{} = Fn) ->
    Report = mnesia_mon:get_stamp("write_user_fn"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:write_user_fnD(Site, Fn)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

read_user_fn(Site, FnName) ->
    Report = mnesia_mon:get_stamp("read_user_fn"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:read_user_fnD(Site, FnName)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

write_kv(Site, Key, Value) ->
    Report = mnesia_mon:get_stamp("write_kv"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:write_kvD(Site, Key, Value)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

read_kv(Site, Key) ->
    Report = mnesia_mon:get_stamp("read_kv"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:read_kvD(Site, Key)
          end,
    mnesia_mon:log_act(transaction, Fun, Report).

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
    Report = mnesia_mon:get_stamp("read_intersect_ref"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:read_ref(RefX, intersect)
          end,
    read_activity(RefX, Fun, Report).

read_styles_IMPORT(RefX) when is_record(RefX, refX) ->
    Report = mnesia_mon:get_stamp("read_styles_IMPORT"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:read_styles_IMPORTD(RefX)
          end,
    read_activity(RefX, Fun, Report).

handle_form_post(#refX{site = S, path = P,
                       obj = {row, {1, 1}}} = RefX,
                 Array, PosterUid) ->
    Report = mnesia_mon:get_stamp("handle_form_post"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  ok = init_front_end_notify(),
                  ok = page_srv:page_written(S, P),
                  % Labels from the results page
                  LabXs = new_db_wu:expand_ref(RefX),
                  OldLabs = [new_db_wu:read_ref_field(X, "__rawvalue", read)
                             || X <- LabXs],
                  OldLabs2 = lists:flatten(OldLabs),

                  Values = [ {"submitted", dh_date:format("d/m/Y h:i:s")} |
                             lists:reverse(lists:foldl(fun generate_labels/2, [],
                                                       Array)) ],
                  LastCol = get_last_col(OldLabs2),
                  {NewLabels, NVals} =
                      allocate_values(S, P, Values, OldLabs2, RefX, LastCol),
                  NLbls = [ {Lref, [{"formula", Val}]}
                            || {Lref, Val} <- NewLabels],
                  [begin
                       XRefX = new_db_wu:refX_to_xrefX_createD(X),
                       _Dict = new_db_wu:write_attrs(XRefX, A, PosterUid)
                   end || {X, A} <- NLbls],
                  Row = new_db_wu:get_last_row(RefX) + 1,
                  F = fun(X, Val) ->
                              Obj = {cell, {X, Row}},
                              RefX2 = #refX{site = S, type = url, path = P,
                                            obj = Obj},
                              XRefX2 = new_db_wu:refX_to_xrefX_createD(RefX2),
                              _Dict = new_db_wu:write_attrs(XRefX2,
                                                            [{"formula", Val}],
                                                            PosterUid),
                              ok = new_db_wu:mark_these_dirtyD([XRefX2], PosterUid)
                      end,
                  [F(X, V) || {#refX{site = S1, type = gurl, path = P1,
                                     obj = {column, {X, X}}}, V}
                                  <- NVals, S == S1, P == P1]
          end,

    write_activity(RefX, Fun, "write last", Report).

append_row([], _PAr, _VAr) -> ok;
append_row(List, PAr, VAr) when is_list(List) ->

    Report = mnesia_mon:get_stamp("append_row"),
    % all the refX's in the list must have the same site/path/object type
    {RefX=#refX{site = S, path = P},_} = hd(List),
    Trans =
        fun() ->
                mnesia_mon:report(Report),
                ok = init_front_end_notify(),
                ok = page_srv:page_written(S, P),
                Row = new_db_wu:get_last_row(RefX) + 1,
                F = fun(X, Val) ->
                            Obj = {cell, {X, Row}},
                            RefX2 = #refX{site = S, type = url, path = P,
                                          obj = Obj},
                            XRefX2 = new_db_wu:refX_to_xrefX_createD(RefX2),
                            _Dict = new_db_wu:write_attrs(XRefX2,
                                                          [{"formula", Val}],
                                                          PAr),
                            ok = new_db_wu:mark_these_dirtyD([XRefX2], VAr)
                    end,
                [F(X, V) || {#refX{site = S1, type = gurl, path = P1,
                                   obj = {column, {X, X}}}, V}
                                <- List, S == S1, P == P1]
        end,

    write_activity(RefX, Trans, "write last", Report).

-spec matching_forms(#refX{}, common | string()) -> [#form{}].
matching_forms(RefX, Transaction) ->
    Report = mnesia_mon:get_stamp("matching_forms"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:matching_formsD(RefX, Transaction)
          end,
    read_activity(RefX, Fun, Report).

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
    Report = mnesia_mon:get_stamp("set_border2"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  ok = init_front_end_notify(),
                  ok = page_srv:page_written(S, P),
                  B   = "border-" ++ Where ++ "-width",
                  B_S = "border-" ++ Where ++ "-style",
                  B_C = "border-" ++ Where ++ "-color",
                  _ = new_db_wu:write_attrs(RefX, [{B,   Border}]),
                  _ = new_db_wu:write_attrs(RefX, [{B_S, B_Style}]),
                  _ = new_db_wu:write_attrs(RefX, [{B_C, B_Color}])
          end,
    write_activity(RefX, Fun, "set_borders2", Report).

-spec read_ref(#refX{}) -> [{#refX{}, term()}].
read_ref(#refX{obj = {cell, _}} = RefX) ->
    Report = mnesia_mon:get_stamp("read_ref"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  new_db_wu:read_ref(RefX, inside, read)
          end,
    read_activity(RefX, Fun, Report).

-spec read_attribute(#refX{}, string()) -> [{#refX{}, term()}].
read_attribute(RefX, Field) when is_record(RefX, refX) ->
    Report = mnesia_mon:get_stamp("read_attribute"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  XRefX = new_db_wu:refX_to_xrefX_createD(RefX),
                  new_db_wu:read_ref_field(XRefX, Field, read)
          end,
    read_activity(RefX, Fun, Report).

recalc(#refX{} = RefX) ->
    Report = mnesia_mon:get_stamp("recalc"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  XRefX = new_db_wu:refX_to_xrefX_createD(RefX),
                  Refs = new_db_wu:read_ref(XRefX, inside),
                  Refs2 = [X || {X, L} <- Refs, L =/= []],
                  ok = new_db_wu:mark_these_dirtyD(Refs2, nil)
          end,
    write_activity(RefX, Fun, "refresh", Report).

clear(#refX{obj = {page, "/"}} = RefX, Type, Ar) ->
    % page clear are too 'big' cause massive memory spikes
    % and dirty cell rushes and message queues to go through
    % the roof, so we don't do them atomically.
    %
    % Instead we clear the page row-by-row starting with the last
    % row.
    Report1 = mnesia_mon:get_stamp("get no of rows for page clear"),
    Fun1 = fun() ->
                   new_db_wu:get_last_row(RefX)
           end,
    NoOfRows = mnesia_mon:log_act(transaction, Fun1, Report1),
    clear_by_rows(NoOfRows, 0, RefX, Type, Ar);
clear(#refX{} = RefX, Type, Ar) ->
    Report = mnesia_mon:get_stamp("clear"),
    Fun =
        fun() ->
                mnesia_mon:report(Report),
                ok = init_front_end_notify(),
                ok = new_db_wu:clear_cells(RefX, Type, Ar)
        end,
    write_activity(RefX, Fun, "clear", Report).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% server side drag'n'drop                                                    %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc copies the formula and formats from a cell or range and
%% pastes them to the destination then deletes the original.
%%
%% (see also {@link new_db_api:drag_n_drop/2} and {@link new_db_api:copy_n_paste/2} -
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
%% If a range is to be cut'n'pasted to a range then one of the following criteria MUST
%% true:
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
%% (see also {@link new_db_api:drag_n_drop/2} and {@link new_db_api:cut_n_paste/2} -
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
-spec copy_n_paste(#refX{}, #refX{}, all | style | value, auth_srv:auth_spec()) -> ok.
copy_n_paste(From, #refX{site = ToS, path = ToP} = To, What, Ar) when
is_record(From, refX), is_record(To, refX) ->
    Report = mnesia_mon:get_stamp("copy'n'paste"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  ok = init_front_end_notify(),
                  ok = page_srv:page_written(ToS, ToP),
                  ok = copy_n_paste2(From, To, What, Ar)
          end,
    write_activity(From, Fun, "copy n paste", Report).

%% @doc takes the formula and formats from a cell and drag_n_drops
%% them over a destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
%%
%% (see also {@link new_db_api:cut_n_paste/2} and {@link new_db_api:copy_n_paste/2} -
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
    Report = mnesia_mon:get_stamp("drag'n'drop"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
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
    ok = write_activity(From, Fun, "drag n drop", Report).

%% @todo This needs to check if it intercepts a shared formula
%% and if it does it should fail...
insert(#refX{obj = {column, _}} = RefX, Ar) ->
    move(RefX, insert, horizontal, Ar, "insert_col", false);
insert(#refX{obj = {row, _}} = RefX, Ar) ->
    move(RefX, insert, vertical, Ar, "insert_row", false);
insert(#refX{obj = R} = RefX, Ar)
  when R == cell orelse R == range ->
    move(RefX, insert, vertical, Ar, "insert cell/range", false).

%% The Type variable determines how the insert displaces the existing cases...
insert(#refX{obj = {R, _}} = RefX, Disp, Ar)
  when is_record(RefX, refX), (R == cell orelse R == range),
       (Disp == horizontal orelse Disp == vertical)->
    move(RefX, insert, Disp, Ar, "insert cell/range", false).

%% @doc deletes a column or a row or a page
%%
%% @todo this is all bollocks - should be row, column then cell/range as
%% per insert/2.
%% This needs to check if it intercepts a shared formula
%% and if it does it should fail...
-spec delete(#refX{}, auth_srv:auth_spec()) -> ok.
delete(#refX{obj = {R, _}} = RefX, Ar) when R == cell orelse R == range ->
    move(RefX, delete, vertical, Ar, "delete cell/range", false);
delete(#refX{obj = {column, _}} = RefX, Ar)  ->
    move(RefX, delete, horizontal, Ar, "delete row/col", false);
delete(#refX{obj = {row, {_Min, _Max}}} = RefX, Ar) ->
    move(RefX, delete, vertical, Ar, "delete", false);
delete(#refX{site = S, path = P, obj = {page, _}} = RefX, Ar) ->
    % page deletes are too 'big' cause massive memory spikes
    % and dirty cell rushes and message queues to go through
    % the roof, so we don't do them atomically.
    %
    % Instead we delete the page row-by-row starting with the last
    % row.
    %
    % Need to do jiggery-pokery to make the logs look OK
    Report1 = mnesia_mon:get_stamp("get no of rows for page delete"),
    Fun1 = fun() ->
                   % log it as a page delete in this transaction
                   % bit of a cheat...
                   new_db_wu:log_page(RefX, 'page deleted', Ar),
                   new_db_wu:get_last_row(RefX)
           end,
    NoOfRows = mnesia_mon:log_act(transaction, Fun1, Report1),
    ok = delete_by_rows(NoOfRows, 0, RefX, Ar),
    % now do a 'normal' delete to clear off the rows and columns
    Report2 = mnesia_mon:get_stamp("clear off rows and cols "
                                   ++ "in a page delete"),
    Fun2 = fun() ->
                   ReWr = new_db_wu:delete_cells(RefX, vertical, Ar),
                   ok = new_db_wu:mark_these_dirtyD(ReWr, Ar)
           end,
    % because we have cleared all the cells that have to be deleted
    % in advance we expect delete_cells to return an empty list
    ok = mnesia_mon:log_act(transaction, Fun2, Report2),
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
    move(RefX, delete, Disp, Ar, "delete", false).

delete_by_rows(N, Min, RefX) -> delete_by_rows(N, Min, RefX, nil).

delete_by_rows(Min, Min, _, _) -> ok;
delete_by_rows(N, Min, #refX{site = _S} = RefX, Ar) ->
    NewRefX = RefX#refX{obj = {row, {N, N}}},
    move(NewRefX, delete, vertical, Ar, "delete", false),
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
    Report = mnesia_mon:get_stamp("handle_dirty_cell: "
                                  ++ integer_to_list(Idx)),
    ok = init_front_end_notify(),
    Fun =
        fun() ->
                mnesia_mon:report(Report),
                % check if the cell has been deleted
                case new_db_wu:has_cell_been_deletedD(Site, Idx) of
                    true ->
                        [];
                    false ->
                        Cell = new_db_wu:idx_to_xrefXD(Site, Idx),
                        Attrs = case new_db_wu:read_ref(Cell, inside, write) of
                                    [{_, A}] -> A;
                                    _        -> orddict:new()
                                end,
                        case orddict:find("formula", Attrs) of
                            {ok, F} ->
                                _Dict1 = new_db_wu:write_attrs(Cell,
                                                               [{"formula", F}],
                                                               Ar),
                                % cells may have been written that now depend
                                % on this cell so it needs to report back
                                % dirty children
                                [Rels] = new_db_wu:read_relations(Cell, read),
                                Rels#relation.children;
                            _ ->
                                []
                        end,
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
                                    _ ->
                                        {"dynamic_select", S, _Vals} = I,
                                        NewI = [{"input", {"dynamic_select", S}}],
                                        _Dict2 = new_db_wu:write_attrs(Cell, NewI, Ar)
                                end,
                                [];
                            _  ->
                                []
                        end
                end
        end,
    NewDirties = mnesia_mon:log_act(transaction, Fun, Report),
    tell_front_end("handle dirty", #refX{}),
    NewDirties.

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
    Report = mnesia_mon:get_stamp("write_attributes"),
    Fun = fun() ->
                  mnesia_mon:report(Report),
                  ok = init_front_end_notify(),
                  List2 = expand(List),
                  [ok = write_attributes1(XR, A2, PAr, VAr)
                   || {XR, A2} <- List2],
                  ok
          end,
    % assumes all refX's are for the same site and page, hmmm...
    {Ref, _} = hd(List),
    write_activity(Ref, Fun, "write attrs", Report).

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
    R2 = new_db_wu:refXs_to_xrefXs_create([R]),
    NewAcc = case length(R2) of
                 1 -> [R3] = R2,
                      {R3, A};
                 N -> A2 = lists:duplicate(N, A),
                      lists:zip(R2, A2)
             end,
    expand2(TR, TA, lists:flatten([NewAcc | Acc])).

write_attributes1(#xrefX{} = XRefX, List, PAr, VAr) ->
    new_db_wu:write_attrs(XRefX, List, PAr),
    % first up do the usual 'dirty' stuff - this cell is dirty
    case lists:keymember("formula", 1, List) of
        true  -> new_db_wu:mark_these_dirtyD([XRefX], VAr);
        false -> ok
    end,
    % now do the include dirty stuff (ie this cell has had it's format updated)
    % so make any cells that use '=include(...)' on it redraw themselves
    case lists:keymember("__in_includeFn", 1, List) of
        true -> new_db_wu:mark_dirty_for_inclD([XRefX], VAr);
        false -> ok
    end.

init_front_end_notify() ->
    _Return = put('front_end_notify', []),
    ok.

-spec read_activity(#refX{}, fun(), list()) -> any().
read_activity(#refX{site = Site}, Op, Report) ->
    Activity = fun() -> mnesia_mon:log_act(transaction, Op, Report) end,
    dbsrv:read_only_activity(Site, Activity).

% expose for debugging only
write_activity_DEBUG(RefX, Op, FrontEnd, Report) ->
    write_activity(RefX, Op, FrontEnd, Report).

-spec write_activity(#refX{}, fun(), string(), list()) -> ok.
write_activity(#refX{site = Site} = RefX, Op, FrontEnd, Report) ->
    % still keep the notification of the db server for the mo
    Activity = fun() ->
                       Ret = mnesia_mon:log_act(transaction, Op, Report),
                       tell_front_end(FrontEnd, RefX),
                       Ret
               end,

    dbsrv:write_activity(Site, Activity).

tell_front_end("quiet", _RefX) ->
    ok;
tell_front_end(Type, #refX{path = P} = RefX)
  when Type == "move" orelse Type == "refresh" ->
    Notifications = get('front_end_notify'),
    % the move or refresh notifications are used when a page is changed radically
    % - they tell the front end to request the whole page
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
    Fun = fun({change, #xrefX{site=S, path=P, obj={page, "/"}}, _Attrs}) ->
                  remoting_reg:notify_refresh(S, P);
             ({change, #xrefX{site=S, path=P, obj=O}, Attrs}) ->
                  remoting_reg:notify_change(S, P, O, Attrs);
             ({style, #xrefX{site=S, path=P}, Style}) ->
                  remoting_reg:notify_style(S, P, Style);
             ({delete_attrs, #xrefX{site=S, path=P, obj=O}, Attrs}) ->
                  remoting_reg:notify_delete_attrs(S, P, O, Attrs)
          end,
    [ok = Fun(X) || X <- List],
    ok.

move(RefX, Type, Disp, Ar, Report, LogChange)
  when (Type == insert orelse Type == delete)
       andalso (Disp == vertical orelse Disp == horizontal) ->
    Report2 = mnesia_mon:get_stamp(Report),
    Fun = fun() ->
                  mnesia_mon:report(Report2),
                  move_tr(RefX, Type, Disp, Ar, LogChange)
          end,
    write_activity(RefX, Fun, "move", Report2).

move_tr(RefX, Type, Disp, Ar, LogChange) ->
    ok = init_front_end_notify(),
    % if the Type is delete we first delete the original cells
    _R = {insert, atom_to_list(Disp)},
    % when the move type is DELETE the cells that are moved
    % DO NOT include the cells described by the reference
    % but when the move type is INSERT the cells that are
    % move DO include the cells described by the reference
    % To make this work we shift the RefX up 1, left 1
    % before getting the cells to shift for INSERT
    % if this is a delete - we need to actually delete the cells
    case LogChange of
        true  -> new_db_wu:log_move(RefX, Type, Disp, Ar);
        false -> ok
    end,
    ReWr = do_delete(Type, RefX, Disp, Ar),
    ok = new_db_wu:shift_rows_and_columnsD(RefX, Type, Disp, Ar),
    MoreDirty = new_db_wu:shift_cellsD(RefX, Type, Disp, ReWr, Ar),
    ok = new_db_wu:mark_these_dirtyD(ReWr, Ar),
    ok = new_db_wu:mark_these_dirtyD(MoreDirty, Ar).

do_delete(insert, _RefX, _Disp, _UId) ->
    [];
do_delete(delete, RefX, Disp, Uid) ->
    new_db_wu:delete_cells(RefX, Disp, Uid).

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
               #refX{obj = {range, {X, _TY1, X, _TY2}}}) -> {ok, col_range_to_range};
is_valid_d_n_d(#refX{obj = {range, {_FX1, Y, _FX2, Y}}},
               #refX{obj = {range, {_TX1, Y, _TX2, Y}}}) -> {ok, row_range_to_range};
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
    case auth_srv:get_any_view(Site, Path, Ar) of
        {view, _} ->
            XFrom = new_db_wu:refX_to_xrefX_createD(From),
            XTo = new_db_wu:refX_to_xrefX_createD(To),
            ok = new_db_wu:copy_cell(XFrom, XTo, Incr, What, Ar),
            ok = new_db_wu:mark_these_dirtyD([XTo], Ar);
        _ ->
            throw(auth_error)
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

unpack_incs(List) -> unpack_1(List, [], [], []).

unpack_1([], Js, Js_r, CSS) -> {hslists:uniq(Js),
                                hslists:uniq(Js_r),
                                hslists:uniq(CSS)};
unpack_1([H | T], Js, Js_r, CSS) ->
    #include{js = J, js_reload = R, css = C} = H,
    NewJ = case J of
               [] -> Js;
               _  -> lists:append([J, Js])
           end,
    NewR = case R of
               [] -> Js_r;
               _  -> lists:append([R, Js_r])
           end,
    NewC = case C of
               [] -> CSS;
               _  -> lists:append([C, CSS])
           end,
    unpack_1(T, NewJ, NewR, NewC).
