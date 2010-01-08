%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       <h1>Overview</h1>
%%% 
%%%            This module provides the data access api.
%%%            Each function in this should call functions from
%%%            {@link hn_db_wu} which provides work units and it
%%%            should wrap them in an mnesia transaction.
%%%            
%%%            mnesia <em>MUST NOT</em> be called from any function in
%%%            this module.
%%%            
%%%            This module also handles the transaction management of
%%%            notification to the front-end. In order for this to work
%%%            each mnesia transaction construcuted here MUST begin with
%%%            a function call to initialise the notifications in the 
%%%            process dictionary using the function 'init_front_end_notify'
%%%            When the mnesia transaction returns the {@link hn_db_wu}
%%%            functions will have loaded the process dictionary with
%%%            the appropriate front end notifications which can then be
%%%            forwared using the function 'tell_front_end'
%%%            
%%%            Obviously this only needs to be done on functions that generate
%%%            notifications to the front-end (ie it is not required for any 
%%%            reads but also for some writes - like page version updates)
%%%            
%%%            Security operations (like checking if the right biccie
%%%            has been supplied for a remote update) <em>MUST</em> be
%%%            performed in these API functions using the 
%%%            {@link hn_db_wu:verify_biccie_in/2} and 
%%%            {@link hn_db_wu:verify_biccie_out/3} functions - they
%%%            <em>WILL NOT</em> be done in {@link hn_db_wu}.
%%%            
%%%            It makes extensive use of #refX{} records which can
%%%            exist in the the following flavours:
%%%            <ul>
%%%            <li>cell</li>
%%%            <li>range</li>
%%%            <li>column</li>
%%%            <li>row</li>
%%%            <li>page</li>
%%%            </ul>
%%%            These flavours are distingished by the obj attributes
%%%            which have the following forms:
%%%            <ul>
%%%            <li><code>{cell, {X, Y}}</code></li>
%%%            <li><code>{range, {X1, Y1, X2, Y2}}</code></li>
%%%            <li><code>{column, {X1, X2}}</code></li>
%%%            <li><code>{row, {Y1, Y2}}</code></li>
%%%            <li><code>{page, "/"}</code></li>
%%%            </ul>
%%%            
%%%            <h2>Gotcha's</h2>
%%%            
%%%            There is an event cycle artefact that relates to the use of the 
%%%            hypernumbers() function.
%%%            
%%%            When a hypernumber is used is a formula muin asks for the value
%%%            if the remote cell isn't used a hypernumber is setup AND THE 
%%%            NEW REMOTE LINK IS WRITTEN
%%%            
%%%            If the remote cell is <i>already used</i> by another cell the value
%%%            is and there is no remote link a remote like is also written as is
%%%            a notify_back message to add a matching remote link on the remote server
%%%            
%%%            If the remote cell is <i>already used</i> by another cell and the
%%%            remote link is already written, nothing happens...
%%%            
%%%            This is a bit messy, but the alternative is a race condition :(
%%%            
%%%            The remote site only gets a notification that a new cell is linking
%%%            in when the (internal) function {@link update_rem_parents} 
%%%            runs...
%%%            
%%%            <h2>Notes On Terminology</h2>
%%% 
%%%            Various terms that describe the relationships between
%%%            different cells are shown below:
%%%            <img src="./terminology.png" />
%%% 
%%% @TODO need to write a function to clear attributes 
%%% <code>clear_attributes(#refX{}, [Key1, Key2...])</code>
%%% @TODO should we have a subpages #refX egt {subpages, "/"}
%%% which would alllow you to specify operations like delete and copy
%%% on whole subtrees?
%%% @TODO in tell_front_end we gen_srv each change in one at a time, but
%%% we should bung 'em all in a oner...
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_api).

-include("spriki.hrl").
-include("hypernumbers.hrl").

-export([
         write_attributes/1,
         write_attributes/2,
         write_last/1,
         read_last/1,
         % write_permission/2,
         write_style_IMPORT/2,
         read_attributes/2,
         read/1,
         read_whole_page/1,
         read_inherited_list/2,
         read_inherited_value/3,
         read_styles/1,
         read_page_structure/1,
         read_pages/1,
         % update_style/2,
         recalculate/1,
         reformat/1,
         drag_n_drop/2,
         copy_n_paste/2,
         cut_n_paste/2,
         copy_style/2,
         insert/1,
         insert/2,
         delete/1,
         delete/2,
         clear/1,
         clear/2,
         % delete_permission/1,
         % delete_style/1,
         notify_back_create/2,
         read_incoming_hn/2,
         write_remote_link/3,
         notify_from_web/5,
         notify_back_from_web/4,
         handle_dirty_cell/2,
         %%shrink_dirty_cell/1,
         handle_dirty/2,
         set_borders/5,
         register_hn_from_web/4,
         check_page_vsn/2,
         initialise_remote_page_vsn/2,
         incr_remote_page_vsn/3,
         resync/2,
         write_formula_to_range/2,
         wait_for_dirty/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% API Interfaces                                                             %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
-spec set_borders(#refX{}, any(), any(), any(), any()) -> ok.
%% @doc  takes a range reference and sets the borders for the range according
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

set_borders2(RefX, Where, Border, B_Style, B_Color) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  B   = "border-" ++ Where ++ "-width",
                  B_S = "border-" ++ Where ++ "-style",
                  B_C = "border-" ++ Where ++ "-color",
                  ok = hn_db_wu:write_attr(RefX, {B,   Border}),
                  ok = hn_db_wu:write_attr(RefX, {B_S, B_Style}),
                  ok = hn_db_wu:write_attr(RefX, {B_C, B_Color})
          end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("set_borders2").

wait_for_dirty(Site) ->
    case mnesia:dirty_first(hn_db_wu:trans(Site, dirty_cell)) of
        '$end_of_table' ->
            ok;
        _Index ->
            timer:sleep(100),
            wait_for_dirty(Site) 
    end.


%% %% @todo write documentation for shrink_dirty_cell
%% shrink_dirty_cell(Site) ->
%%     mnesia:activity(sync_dirty, fun do_shrink_dirty_cell/1, [Site]).

%% do_shrink_dirty_cell(Site) ->
%%     List = hn_db_wu:read_all_dirty_cells(Site),
%%     Fun2 = fun(#dirty_cell{idx = Idx} = D, Acc) ->
%%                    L =  hn_db_wu:read_local_parents_idx(Site, Idx),
%%                    case L of
%%                        [] -> Acc;
%%                        _  -> [{D, L} | Acc]
%%                    end
%%            end,
%%     ParentsList = lists:foldl(Fun2, [], List),
%%     % now dedup the dirty list
%%     DeleteList = shrink(ParentsList, List),
%%     ok = hn_db_wu:delete_dirty_cells(Site, DeleteList).

%% @todo write documentation for write_formula_to_range
write_formula_to_range(RefX, _Formula) when is_record(RefX, refX) ->
    exit("write write_formula_to_range in hn_db_api!").
% write_formula_to_range(Formula, RefX = #refX{obj = 
%         {range, {TlCol, TlRow, BrCol, BrRow}}}) -> 
%    Rti = hn_db_wu:refX_to_rti(Ref, true), 
%    {formula, FormulaProcd} = superparser:process(Formula), 
%    {ok, {Pcode, Res, Parents, DepTree, Recompile}} = muin:run_formula(FormulaProcd, Rti), 
%    SetCell = fun({Col, Row}) -> 
%                      OffsetCol = Col - TlCol + 1, 
%                      OffsetRow = Row - TlRow + 1, 
%                      Value = case area_util:at(OffsetCol, OffsetRow, Res) of 
%                                  {ok, V}    -> V; 
%                                  {error, _} -> ?ERRVAL_NA 
%                              end, 
%                      ParentsXml = map(fun muin_link_to_simplexml/1, Parents), 
%                      DepTreeXml = map(fun muin_link_to_simplexml/1, DepTree), 
%                      Addr = Ref#ref{ref = {cell, {Col, Row}}}, 
%                      db_put(Addr, '__ast', Pcode), 
%                      db_put(Addr, '__recompile', Recompile), 
%                      db_put(Addr, '__shared', true), 
%                      db_put(Addr, '__area', {TlCol, TlRow, BrCol, BrRow}), 
%                      write_cell(Addr, Value, Formula, ParentsXml, DepTreeXml) 
%              end, 
%    Coords = muin_util:expand_cellrange(TlRow, BrRow, TlCol, BrCol), 
%    foreach(SetCell, Coords). 

%% @spec write_style_IMPORT(#refX{}, #styles{}) -> Index
%% @doc write_style will write a style record
%% It is intended to be used in the FILE IMPORT process only 
%% - normally the respresentation of styles in the magic style record
%% is designed to be hidden from the API
%% (that's for why it is a 'magic' style n'est pas?)
write_style_IMPORT(RefX, Style)
  when is_record(RefX, refX) andalso is_record(Style, magic_style) ->

    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = hn_db_wu:write_style_IMPORT(RefX, Style)
          end,
    
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("write_style_IMPORT").

%% @doc reads pages
%% @todo fix up api
read_page_structure(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  hn_db_wu:read_page_structure(RefX)
          end,
    mnesia:activity(transaction, Fun).

read_pages(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  hn_db_wu:read_pages(RefX)
          end,
    mnesia:activity(transaction, Fun).    

%% @spec initialise_remote_page_vsn(Site, Version) -> ok
%% @doc intialises the page version for a 'newly discovered' remote page.
%% This function is only called the first time that a remote pages makes a
%% request and the function {@link read_page_vsn_raw/2} returns 
%% <code>undefined</code>
initialise_remote_page_vsn(Site, Version) when is_record(Version, version) ->
    #version{page = Page, version = V} = Version,
    RefX = hn_util:url_to_refX(Page),
    Fun = fun() ->
                  ok = hn_db_wu:initialise_remote_page_vsn(Site, RefX, V)
          end,
    mnesia:activity(transaction, Fun).

%% @spec resync(Site, List) -> ok
%% @doc triggers a resync of the current site to the remote pages.
%% This function is called when version numbers are out of sync between local 
%% and remote pages and forces a resynch
%% @TODO write me, ya bas!
resync(_Site, #version{page = _Page, version = _Vsn}) ->
    ok.

%% @spec incr_remote_page_vsn(Site, Version::#version{}, Payload) -> 
%% {ok, NewVersion} | {error, pages_out_of_synch}
%% @doc increments the local storage of a page version number for a page on
%% a remote site if the new increment is one above the old one. If the increment
%% is greater than 1 returns an error which should trigger a resynch.
%% Incrementation of page versions for local pages should be done with 
%% {@link get_new_local_page_vsn/2}
%% @todo break up payload in hn_mochi.erl
incr_remote_page_vsn(Site, Version, Payload) when is_record(Version, version) ->
    Fun = fun() ->
                  hn_db_wu:incr_remote_page_vsn(Site, Version, Payload)
          end,
    mnesia:activity(transaction, Fun).

%% @spec check_page_vsn(Site, Version::#version{}) 
%% -> synched | unsynched | not_yet_synched
%% @doc checks the page verion numbers for a set of pages
check_page_vsn(Site, Version) when is_record(Version, version) ->
    F = fun() ->
                #version{page = Page, version = NewV} = Version,
                PageX = hn_util:url_to_refX(Page),
                OldV = hn_db_wu:read_page_vsn(Site, PageX),
                case {NewV, OldV} of
                    {Vsn, Vsn} -> 
                        synched;
                    {"undefined", OldV} ->
                        ok = hn_db_wu:initialise_remote_page_vsn(Site, PageX, 
                                                                 OldV),
                        synched;
                    {NewV, "undefined"} -> 
                        not_yet_synched;
                    {NewV, OldV} -> 
                        unsynched
                end
        end,
    mnesia:activity(transaction, F).

%% @spec register_hn_from_web(Parent::#refX{}, Child::#refX{}, Proxy, Biccie) -> 
%% list()
%% @doc handles the registration of a new hypernumber from the web server.
%% The Parent and Child references must both point to a cell.
%% This function returns a list with two tuples, one for the current value
%% and one for the current dependency tree
register_hn_from_web(Parent, Child, Proxy, Biccie)
  when is_record(Parent, refX), is_record(Child, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  hn_db_wu:write_remote_link(Parent, Child, outgoing),
                  hn_db_wu:register_out_hn(Parent, Child, Proxy, Biccie),
                  List = hn_db_wu:read_attrs(Parent, ["value", "dependency-tree"], read),
                  List2 = extract_kvs(List),
                  Site = Parent#refX.site,
                  Version = hn_db_wu:read_page_vsn(Site, Parent),
                  
                  % get the value (if there is one)
                  V = case lists:keysearch("value", 1, List2) of
                          false           -> "blank";
                          {_, {_, Value}} -> Value
                      end,

                  % get the dependency-tree (if there is one)
                  D = case lists:keysearch("dependency-tree", 1, List2) of
                          false             -> {xml, []};
                          {_, {_, DepTree}} -> DepTree
                      end,

                  % now package it up for the site that is registering
                  % the hypernumber
                  PPage = Parent#refX{obj = {page, "/"}},
                  PUrl = hn_util:refX_to_url(PPage),
                  Vsn = #version{page = PUrl, version = Version},
                  VsnJson = json_util:jsonify(Vsn),

                  Dep = simplexml:to_xml_string(D),
                  % now return a JSON ready structure...
                  {struct, [{"value",           V},
                            {"dependency-tree", Dep},
                            {"parent_vsn",      VsnJson}]}
          end,
    Str = mnesia:activity(transaction, Fun),
    ok = tell_front_end("register_hn_from_web"),
    Str.

%% @spec handle_dirty_cell(Site, Timestamp) -> ok
%% @doc handles a dirty cell.
%% Timestamp is the timestamp of the dirty cell - the actual
%% record itself may have been rewritten before it is processed.
%% If the target of the dirty cell has been deleted the dirty cell record
%% will have been written to have an index of 'deleted' and this function
%% will skip it
%% Silently fails if the cell is part of a shared range
%% @todo extend this to a dirty shared formula
%% @todo needs to be ran inside transaction from
%% other module, kinda ugly, fix
handle_dirty_cell(Site, Rec) ->
    
    ok   = init_front_end_notify(),  
    Cell = hn_db_wu:read_dirty_cell(Site, Rec),
    
    case hn_db_wu:read_attrs(Cell, ["__shared"], read) of
        [] ->
            %% THIS IS BAD, we shouldnt hand dirty cells not
            %% existing since they shouldnt be marked dirty
            case hn_db_wu:read_attrs(Cell, ["formula"], read) of
                [{C, KV}] -> hn_db_wu:write_attr(C, KV);
                []        -> ok
            end;
        _  ->
            ?INFO("TODO: handle_dirty_cell shared formula", [])
    end,
    
    ok = tell_front_end("handle dirty").


%% @spec handle_dirty(Site, Record) -> ok
%% Record = #dirty_notify_in{}
%% @doc handles a dirty.
%% The reference must be to a cell
%% @todo implement dirty ranges/functions/queries and stuff
%% %@TODO list the full set of possible change messages and how they should
handle_dirty(Site, Record) when is_record(Record, dirty_notify_in) ->
    Parent = Record#dirty_notify_in.parent,
    % read the incoming remote children and mark them dirty
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                Cells = hn_db_wu:read_remote_children(Site, Parent, incoming),
                Fun2 =
                    fun(X) ->
                            [{RefX, KV}] =
                                hn_db_wu:read_attrs(X, ["formula"], write),
                            hn_db_wu:write_attr(RefX, KV)
                    end,
                [ok = Fun2(X)  || X <- Cells],
                hn_db_wu:clear_dirty(Site, Record)
        end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("handle dirty");
handle_dirty(Site, Record) when is_record(Record, dirty_notify_out) ->
    ok = horiz_api:notify(Record),
    % now delete the dirty outgoing hypernumber
    mnesia:activity(transaction, fun hn_db_wu:clear_dirty/2, [Site, Record]);
handle_dirty(Site, Record) when is_record(Record, dirty_notify_back_in) ->
    ok = horiz_api:notify_back(Record),
    mnesia:activity(transaction, fun hn_db_wu:clear_dirty/2, [Site, Record]);
handle_dirty(_Site, Record)
  when is_record(Record, dirty_notify_back_out) ->
    #dirty_notify_back_out{parent = P, child = C, change = Type} = Record,
    Fun =
        fun() ->
                % hn_db_wu:clear_dirty doesnt exist, need to specify site
                % not sure if child or parent
                case Type of
                    "unregister" ->
                        ok = hn_db_wu:unregister_out_hn(P, C);
                    %ok = hn_db_wu:clear_dirty(Record);
                    "new child" ->
                        ok = hn_db_wu:write_remote_link(P, C, outgoing)
                        %ok = hn_db_wu:clear_dirty(Record)
                end
        end,
    mnesia:activity(transaction, Fun).

%% @spec notify_from_web(Parent::#refX{}, Child::#refX{}, Type, Payload, 
%% Biccie) -> ok
%% @doc handles a notify message from the web server.
%% The parent reference must be cell references.
%% If this value doesn't match the Biccie on record this update will be logged and
%% will silently fail
%% Version is the page version of the remote page. If this version is newer that the
%% page version stored here it will trigger a structural replay of page updates
%% @todo log the biccie failures
%% @todo this code assumes that the child will notify all dependency tree
%% changes consequent to the insert/deletes and that we don't need to care
%% about it - page syncs will 'take care of it' BE SURE THAT THEY DO!
notify_from_web(P, C, "new_value", Payload, Bic)
  when is_record(P, refX), is_record(C, refX) ->
    #refX{site = ChildSite} = C,
    {_, Value, {array, DepTree}} = json_util:json_to_payload(Payload),
    DepTree2 = convertdep(C, DepTree),
    F = fun() ->
                ok = init_front_end_notify(),
                case hn_db_wu:verify_biccie_in(ChildSite, P, Bic) of
                    true  -> hn_db_wu:update_inc_hn(P, C, Value, DepTree2, Bic);
                    false -> ok
                end
        end,
    ok = mnesia:activity(transaction, F),
    ok = tell_front_end("notify from web");
notify_from_web(P, C, "insert", Payload, Bic)
  when is_record(P, refX), is_record(C, refX) ->

    #refX{site = ChildSite} = C,

    F = fun() ->
                ok = init_front_end_notify(),
                case hn_db_wu:verify_biccie_in(ChildSite, P, Bic) of
                    true  -> notify_from_web2(P, C, Payload, Bic);
                    false -> ok
                end
        end,
    ok = mnesia:activity(transaction, F),
    ok = tell_front_end("notify from web").

notify_from_web2(Parent, Child, Payload, _Biccie) ->
    % this function is called when a insert/delete has been made on a remote page
    % this function does the following
    % * read all the #incoming_hn's that are from the page that has had
    %   the insert and then adjust them
    % * read all the cells that use the incoming_hn's, rewrite their formulae
    %   and resave them
    % * read all the remote_cell_link's that use those incoming_hn's and
    %   adjust them

    % do some housekeeping
    % first up unpack the payload
    {Type, Ref, Displacement} = json_util:json_to_payload(Payload),
    % now calculate the offset
    {XOff, YOff} = hn_util:get_offset(Type, Displacement, Ref),
    % make the Parent a page parent because multiple notifications
    % will have been suppresed...
    PageParent = Parent#refX{obj = {page, "/"}},
    #refX{site = ChildSite} = Child,
    % start by reading the incoming_hn's
    case hn_db_wu:read_incoming_hn(ChildSite, PageParent) of
        []   ->
            io:format("in hn_db_api:notify_from_web no incoming "++
                      "hypernumbers - should log this!~n"),
            ok;
        Inc_Hns ->
            % now shorten the list to the relevant ones
            % (ie those 'below' a vertical displacement or
            % to the 'right' of a horizontal displacement)

            Inc_Hns2 = shorten(Inc_Hns, Ref, Displacement),

            % now sort out the incoming_hn's
            Fun3 = fun(I) ->
                           #incoming_hn{site_and_parent = {_S, P}} = I,
                           NewP = shift(P, XOff, YOff),
                           ok = hn_db_wu:shift_inc_hns(I, NewP)
                   end,
            lists:foreach(Fun3, Inc_Hns2),

            % convert the short list of incoming_hn's to refXs
            Fun4 = fun(#incoming_hn{site_and_parent = {_S, P}}) ->
                           hn_util:refX_from_index(P)
                   end,
            HnsXs = lists:map(Fun4, Inc_Hns2),

            % Lets process this list of cells
            % Start by rewriting them

            % now get the cells that use these incoming hypernumbers
            % Swap this out, read all the children of an item and then
            % move that item, then onto the next item...
            Fun5 =
                fun(P) ->
                        case hn_db_wu:read_remote_children(P, incoming) of
                            [] ->
                                io:format("in hn_db_api:notify_from_web "++
                                          "no children of an incoming "++
                                          "hypernumber - should log "++
                                          "this!~n"),
                                ok;
                            Children ->
                                NewP = shift(P, XOff, YOff),
                                ok = hn_db_wu:shift_children(Children, P, NewP),
                                ok = hn_db_wu:shift_remote_links(parent, P,
                                                                 NewP, incoming)
                        end
                end,
            lists:foreach(Fun5, HnsXs),
            ok
    end.

%% @spec notify_back_from_web(Parent::#refX{}, Child::#refX{}, Biccie, Type) 
%% -> ok
%% @doc handles a notify_back message from the web server.
%% The parent and child references must be cell references.
notify_back_from_web(P, C, B, Type)
  when is_record(P, refX), is_record(C, refX) ->
    #refX{site = PSite} = P,
    Fun =
        fun() ->
                case hn_db_wu:verify_biccie_out(P, C, B) of
                    true -> Rec = #dirty_notify_back_out{child = C, parent = P,
                                                         change = Type},
                            hn_db_wu:mark_dirty(PSite, Rec);
                    _    -> ok
                end
        end,
    mnesia:activity(transaction, Fun).

%% @spec write_remote_link(Parent::#refX{}, Child::#refX{}, Type) -> ok
%% @doc writes a remote link
write_remote_link(Parent, Child, Type)
  when is_record(Parent, refX), is_record(Child, refX) ->
    Fun = fun() ->
                  hn_db_wu:write_remote_link(Parent, Child, Type)
          end,
    mnesia:activity(transaction, Fun).

%% @spec read_incoming_hn(Parent::#refX{}, Child::#refX{}) -> {Value, DepTree}
%% @doc gets the value of an incoming hypernumber
%% Parent is a reference to a remote cell only
%% Child is a reference to a local cell only
%% If the hypernumber requested hasn't been set up yet, this function will
%% trigger a creation process and return 'blank' for the moment... when the
%% hypernumber is set up the 'correct' value will come through as per normal...
read_incoming_hn(P, C) when is_record(P, refX), is_record(C, refX) ->
    #refX{site = CSite} = C,
    P2 = P#refX{obj = {page, "/"}},
    C2 = C#refX{obj = {page, "/"}},
    CUrl = hn_util:refX_to_url(C2),
    PUrl = hn_util:refX_to_url(P2),
    PVsn = #version{page = PUrl, version = hn_db_wu:read_page_vsn(CSite, P)},
    CVsn = #version{page = CUrl, version = hn_db_wu:read_page_vsn(CSite, C)},
    F = fun() ->
                case hn_db_wu:read_incoming_hn(CSite, P) of
                    [] ->
                        Rec = #dirty_inc_hn_create{parent = P, child = C,
                                                   parent_vsn = PVsn,
                                                   child_vsn = CVsn},
                        ok = hn_db_wu:mark_dirty(CSite, Rec),
                        % need to write a link
                        ok = hn_db_wu:write_remote_link(P, C, incoming),
                        {blank, []};
                    [Hn] ->
                        #incoming_hn{value = Val, biccie = B,
                                     'dependency-tree' = DepTree} = Hn,
                        % check if there is a remote cell
                        RPs = hn_db_wu:read_remote_parents(C, incoming),
                        ok =
                            case lists:keymember(P, 1, RPs) of
                                false ->
                                    R = #dirty_notify_back_in{parent = P, child = C,
                                                              change = "new child",
                                                              biccie = B,
                                                              parent_vsn = PVsn,
                                                              child_vsn = CVsn},
                                    hn_db_wu:mark_dirty(CSite, R);
                                true  -> ok
                            end,
                        {Val, DepTree}
                end
        end,
    mnesia:activity(transaction, F).

%% @spec notify_back_create(Site, Record::#dirty_inc_hn_create{}) -> ok
%% @doc sets up a hypernumbers
%% @todo expand the paradigm to include ranges, columns, rows references and 
%% queries as things that be remote parents.
%% @TODO handle verions
notify_back_create(Site, Record)
  when is_record(Record, dirty_inc_hn_create) ->

    #dirty_inc_hn_create{parent = P, child = C, parent_vsn = PVsn} = Record,
    _ParentPage = P#refX{obj = {page, "/"}},
    #refX{site = CSite} = C,

    Return = horiz_api:notify_back_create(Record),
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                case Return of % will have to add NewCVsn in line below...
                    {Value, DepT, Biccie, _NewPVsn} ->
                        ok = hn_db_wu:update_inc_hn(P, C, Value, DepT, Biccie);
                    {error, unsynced, PVsn} ->
                        resync(CSite, PVsn);
                    {error, permission_denied} ->
                        exit("need to fix permission handling in "++
                             "hn_db_api:notify_back_create")
                end,
                ok = hn_db_wu:clear_dirty(Site, Record)
        end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("notify back create").

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
write_attributes(RefX, List) ->
    mnesia:activity(transaction, fun write_attributes1/2, [RefX, List]),
    ok = tell_front_end("write attributes").

write_attributes(List) ->
    Fun = fun() ->
                  [ok = write_attributes1(RefX, L) || {RefX, L} <- List],
                  ok
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("write attributes").

%% @spec write_last(List) -> ok
%% List = [{#refX{}, Val}]
%% Val = [list() | float() | integer()]
%% @doc takes a list of references and appends either a column or row at the end of them
%% 
%% All of the references must be either a:
%% <ul>
%% <li>column</li>
%% <li>row</li>
%% </ul>
%% and they must also be on the same page.
%% 
%% If the List looks like:
%%   <code>[{#refX{obj = {column, {2, 2}}, "=3"}, 
%%   {refX{obj = {column, {4, 5}}, 0}]</code>
%% then the value of highest written row will be got the the following cells 
%% will be written:
%% <ul>
%% <li>{cell, {2, LastRow+1} with formula of "=3"</li>
%% <li>{cell, {4, LastRow+1} with formula of 0</li>
%% <li>{cell, {5, LastRow+1} with formula of 0</li>
%% </ul>
%% It will write the values to the last row/column as if they were
%% 'formula' attributes
%% Formulae can be inserted into row or column using <code>rc</code> notation
%%@end
%% write_last uses a match on the first element to enforce the fact that
%% all refs are on the same page - this won't work with an empty list, so
%% write a special clause for that case....
write_last([]) -> ok;
write_last(List) when is_list(List) ->
    % all the refX's in the list must have the same site/path/object type
    % so get those from the head of the list and enforce it by matching down
    % --> at FORCE ME!
    [{#refX{site = S, path = P, obj = O} = RefX, _} | _T] = List,
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                {LastColumnRefX, LastRowRefX} = hn_db_wu:get_last_refs(RefX),
                #refX{obj = {cell, {LastCol, _}}} = LastColumnRefX,
                #refX{obj = {cell, {_, LastRow}}} = LastRowRefX,
                % Add 1 to because we are adding data 'as the last row'
                % (or column) ie one more than the current last row/column
                % NOTE a column reference appends to the 'LAST ROW' not the
                % 'last column' (think about it!)
                {Type, {PosX, PosY}} = case O of
                                  {row, _}    -> Y = LastCol + 1, 
                                                 {row, {Y, Y}};
                                  {column, _} -> X = LastRow + 1,
                                                 {column, {X, X}}
                              end,

                % now convert the column or row references to cell references
                Fun1 =
                    fun({#refX{site = S1, path = P1, obj = {Type1, {IdxX, IdxY}}}, Val})  ->
                            % FORCE ME to match (see above)
                            S = S1,
                            P = P1,
                            Type = Type1,
                            Obj = case Type1 of
                                      row    -> {cell, {PosX, IdxY}};
                                      column -> {cell, {IdxX, PosY}}
                                  end,
                            RefX2 = #refX{site = S1, path = P1, obj = Obj},
                            hn_db_wu:write_attr(RefX2, {"formula", Val})
                    end,
                [Fun1(X) || X <- List]

        end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("write last"),
    ok.

%% @spec read_last(RefX) -> ok
%% @doc takes a list of references and appends either a column or row at the end of them
%% 
%% The reference must be either a:
%% <ul>
%% <li>column</li>
%% <li>row</li>
%% </ul>
read_last(#refX{obj = {R, _}} = RefX) when R == column orelse R == row ->

    Fun = fun() ->
                  Values = hn_db_wu:read_attrs(RefX, ["formula"], write),
                  biggest(Values, R)
          end,

    mnesia:activity(transaction, Fun).

%% @spec read_inherited_list(#refX{}, Attribute) -> {#refX{}, Val}
%% Attribute = string()
%% @doc Scans the tree and returns a list of value stored against
%%      Key 
read_inherited_list(RefX, Key) when is_record(RefX, refX) ->
    F = fun hn_db_wu:read_inherited_list/2,
    mnesia:activity(transaction, F, [RefX, Key]).

%% @spec read_inherited_value(#refX{}, Attibute, Default) -> {#refX{}, Val}
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at a given reference, if not found it returns the supplied
%%       default value
%%       
read_inherited_value(RefX, Key, Default) when is_record(RefX, refX) ->
    F = fun hn_db_wu:read_inherited/3,
    mnesia:activity(transaction, F, [RefX, Key, Default]).

%% @spec read_attributes(#refX{}, AttrList) -> {#refX{}, Val}
%% AttrList = [list()]
%% Val = term()
%% @doc Given a reference and the name of an attribute, returns the reference
%% and the value of that attribute.
%% 
%% The reference can point to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_attributes(RefX, AttrList) when is_record(RefX, refX), is_list(AttrList) ->
    Fun = fun hn_db_wu:read_attrs/3,
    mnesia:activity(transaction, Fun, [RefX, AttrList, read]).

%% @spec read_whole_page(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc read takes a page refererence and returns all the attributes under that
%% refernce including column, row, page, permission and user attributes
%% 
%% The <code>refX{}</code> must only be a page refX
read_whole_page(#refX{obj = {page, "/"}} = RefX) ->
    Fun = fun() ->    
              hn_db_wu:read_whole_page(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec read(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc read takes a refererence and returns the cell attributes.
%% 
%% The <code>refX{}</code> can be one of a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  hn_db_wu:read_cells(RefX, read)
          end,
    mnesia:activity(transaction, Fun).

%% @spec read_styles(#refX{}) -> [Style]
%% Style = #styles{}
%% @doc read_style gets the list of styles that pertain to a particular 
%% reference.
%% 
%% The <code>#refX{}</code> can point to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_styles(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  hn_db_wu:read_styles(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec recalculate(#refX{}) -> ok
%% @doc recalculates the cells refered to (and all cells that depend on them)
%% 
%% The <code>refX{}</code> can be to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
recalculate(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  Cells = hn_db_wu:read_attrs(RefX, ["formula"], write),
                  [ok = hn_db_wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("recalculate").

%% @spec reformat(#refX{}) -> ok
%% @doc reformats the all cells refered to
%% 
%% The <code>refX{}</code> can be to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
% <li>page</li>
%% </ul>
reformat(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  Cells = hn_db_wu:read_attrs(RefX, ["format"], read),
                  [ok = hn_db_wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("reformat").

%% @spec insert(RefX::#refX{}) -> ok
%% @doc inserts a single column or a row
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>row</li>
%% <li>column</li>
%% </ul>
%% 
%% @todo This needs to check if it intercepts a shared formula
%% and if it does it should fail...
insert(#refX{obj = {column, _}} = RefX)  ->
    move(RefX, insert, horizontal);
insert(#refX{obj = {row, _}} = RefX)  ->
    move(RefX, insert, vertical);
insert(#refX{obj = R} = RefX) when R == cell orelse R == range  ->
    move(RefX, insert, vertical).

%% @spec insert(RefX :: #refX{}, Type) -> ok 
%% Type = [horizontal | vertical]
%% @doc inserts a cell or range
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% </ul>
%% 
%% The Type variable determines how the insert displaces the existing cases...
insert(#refX{obj = {R, _}} = RefX, Disp)
  when is_record(RefX, refX), (R == cell orelse R == range),
       (Disp == horizontal orelse Disp == vertical)->
    move(RefX, insert, Disp).

%% @spec delete(Ref :: #refX{}) -> ok
%% @doc deletes a column or a row or a page
%% 
%% @todo this is all bollocks - should be row, column then cell/range as 
%% per insert/2.
%% This needs to check if it intercepts a shared formula
%% and if it does it should fail...
delete(#refX{obj = {R, _}} = RefX) when R == column orelse R == row ->
    Disp = case R of
               row    -> vertical;
               column -> horizontal
           end,
    move(RefX, delete, Disp);
delete(#refX{obj = {page, _}} = RefX) ->
    Fun1 = fun() ->
                   ok = init_front_end_notify(),
                   Status = hn_db_wu:delete_page(RefX),
                   %% now force all deferenced cells to recalculate
                   Fun2 = fun({dirty, X}) ->
                                  [{X, {"formula", F}}] = 
                                      hn_db_wu:read_attrs(X,["formula"], write),
                                  ok = hn_db_wu:write_attr(X, {"formula", F})
                          end,
                   [ok = Fun2(X) || X <- Status]
           end,
    mnesia:activity(transaction, Fun1),
    ok = tell_front_end("delete").

%% @spec delete(RefX :: #refX{}, Type) -> ok
%% Type = [horizontal | vertical]
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
delete(#refX{obj = {R, _}} = RefX, Disp)
  when R == cell orelse R == range orelse R == row orelse R == column ->
    move(RefX, delete, Disp).

move(RefX, Type, Disp)
  when (Type == insert orelse Type == delete)
       andalso (Disp == vertical orelse Disp == horizontal) ->
    ok = mnesia:activity(transaction, fun move_tr/3, [RefX, Type, Disp]),
    ok = tell_front_end("move", RefX).

do_delete(insert, _RefX) ->
    {[], []};
do_delete(delete, RefX) ->
    {hn_db_wu:delete_cells(RefX),
     hn_db_wu:read_local_children(RefX)}.

move_tr(#refX{site = Site, obj = Obj} = RefX, Type, Disp) ->

    ok = init_front_end_notify(),
    % if the Type is delete we first delete the original cells
    R = {insert, atom_to_list(Disp)},
    _NewVsn = hn_db_wu:get_new_local_page_vsn(RefX, R),
    % when the move type is DELETE the cells that are moved
    % DO NOT include the cells described by the reference
    % but when the move type is INSERT the cells that are
    % move DO include the cells described by the reference
    % To make this work we shift the RefX up 1, left 1 
    % before getting the cells to shift for INSERT
    % if this is a delete - we need to actually delete the cells
        
    {Status1, ReWr} = do_delete(Type, RefX),
    Status2 = hn_db_wu:shift_cells(RefX, Type, Disp, ReWr),
    case Obj of
        {row,    _} ->
            ok = hn_db_wu:delete_row_objs(RefX),
            ok = hn_db_wu:shift_row_objs(RefX, Type);
        {column, _} ->
            ok = hn_db_wu:delete_col_objs(RefX),
            ok = hn_db_wu:shift_col_objs(RefX, Type);
        _ ->
            ok
    end,
    
    % now notify all parents and children of all cells on
    % this page
    PageRef = RefX#refX{obj = {page, "/"}},
    
    % OK all our local stuff is sorted, now lets deal with the remote
    % children
    Change = {insert, Obj, Disp},
    % set the delay to zero
    ok = hn_db_wu:mark_notify_out_dirty(PageRef, Change, 0),
    
    Status = lists:flatten([Status1, Status2]),

    % finally deal with any cells returned from delete_cells that
    % are dirty - these need to be recalculated now that the link/local_objs
    % tables have been transformed
    Fun2 = fun(X) ->
                   case hn_db_wu:read_attrs(X, ["formula"], write) of
                       [{X, {"formula", F}}] ->
                           hn_db_wu:write_attr(X, {"formula", F});
                       _  ->
                           ok
                   end
           end,
    [ok = Fun2(X) || {dirty, X} <- Status],
    % Jobs a good'un, now for the remote parents
    % io:format("in hn_db_api:move do something with Parents...~n"),
    _Parents =  hn_db_wu:find_incoming_hn(Site, PageRef),
    %io:format("in hn_db_api:move Parents are ~p~n", [Parents]),
    ok.

%% @spec clear(#refX{}) -> ok
%% @doc same as <code>clear(refX{}, all)</code>.
clear(RefX) when is_record(RefX, refX) ->
    clear(RefX, all).

%% @spec clear(#refX{}, Type) -> ok
%% Type = [contents | style | all]
%% @doc clears the contents of the cell or range
%% (but doesn't delete the cell or range itself).
%% 
%% If <code>Type  = 'contents'</code> it clears:
%% <ul>
%% <li>formula</li>
%% <li>values</li>
%% </ul>
%% If <code>Type = 'style'</code> it clears the style.
%% If <code>Type = 'all'</code> it clears both style and content.
%% It doesn't clear other/user-defined attributes of the cell or range
%%
%% The <code>refX{}</code> can be to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
clear(RefX, Type) when is_record(RefX, refX) ->
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                hn_db_wu:clear_cells(RefX, Type)
        end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("clear").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% server side drag'n'drop                                                    %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec cut_n_paste(From :: #refX{}, To :: #refX{}) -> ok
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination then deletes the original.
%% 
%% (see also {@link hn_db_api:drag_n_drop/2} and {@link hn_db_api:copy_n_paste/2} - 
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
cut_n_paste(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To),
                  ok = hn_db_wu:clear_cells(From, all)
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("cut n paste").

%% @spec copy_n_paste(From :: #refX{}, To :: #refX{}) -> ok
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination.
%% 
%% (see also {@link hn_db_api:drag_n_drop/2} and {@link hn_db_api:cut_n_paste/2} - 
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
copy_n_paste(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To)
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("copy n paste").

%% @spec drag_n_drop(From :: #refX{}, To :: #refX{}) -> ok
%% @doc takes the formula and formats from a cell and drag_n_drops 
%% them over a destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
%% 
%% (see also {@link hn_db_api:cut_n_paste/2} and {@link hn_db_api:copy_n_paste/2} - 
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
drag_n_drop(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  case is_valid_d_n_d(From, To) of
                      {ok, single_cell, Incr}   ->
                          hn_db_wu:copy_cell(From, To, Incr);
                      {ok, 'onto self', _Incr}  -> ok;
                      {ok, cell_to_range, Incr} -> copy2(From, To, Incr)
                  end
          end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("drag n drop").

%% @spec(From::refX{}, To::refX{}) -> ok
%% @doc Copies the style applied to From and attaches it to To.
%%      From can only be a cell ref but To can be either a cell or range
%%      ref
%% @end
copy_style(#refX{obj = {cell, _}} = From, 
           #refX{obj = {Type, _}} = To)
  when Type == cell orelse Type == range ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  hn_db_wu:copy_style(From, To)
          end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("drag n drop").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Internal Functions                                                         %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
biggest(List, Type) ->
    StartAcc = #refX{obj = {cell, {0, 0}}},
    Fun = fun({Ref, _V}, Acc) ->
       
                  #refX{obj = {cell, {X1, Y1}}} = Ref,
                  #refX{obj = {cell, {X2, Y2}}} = Acc,
       
                  case Type of
                      column -> if
                                    Y1 >  Y2 -> Ref;
                                    Y1 =< Y2 -> Acc
                                end;
                      row    -> if
                                    X1 >  X2 -> Ref; 
                                    X1 =< X2 -> Acc
                                end
                  end
          end,
    lists:foldl(Fun, StartAcc, List).

write_attributes1(RefX, List) when is_record(RefX, refX), is_list(List) ->
    ok = init_front_end_notify(),
    [hn_db_wu:write_attr(RefX, X) || X <- List],
    ok = hn_db_wu:mark_children_dirty(RefX).

%% shrink(ParentsList, List) ->
%%     shrink(ParentsList, List, []).

%% shrink([], _List, Acc) ->
%%     Acc;
%% shrink([Dirty | T], List, Acc) ->
%%     DirtyParents = has_dirty_parent(List, Dirty),
%%     NewAcc = case DirtyParents of
%%                  false  -> Acc;
%%                  Dirty2 -> [Dirty2 | Acc]
%%              end,
%%     shrink(T, List, NewAcc).

%% has_dirty_parent(List, {Cell , PIdxList}) ->
%%     case has_dirty_parent2(List, PIdxList, false) of
%%         false -> false;
%%         true  -> Cell
%%     end.

%% %% One true is good enough!
%% has_dirty_parent2(_List, _P, true) -> true;
%% has_dirty_parent2(_List, [], false) -> false;
%% has_dirty_parent2(List, [#local_cell_link{parentidx = Idx} | T], false) -> 
%%     case lists:keymember(Idx, 2, List) of
%%         true  -> true;
%%         false -> has_dirty_parent2(List, T, false)
%%     end.

%% has_dirty_parent([], _Dirty)       -> false;
%% has_dirty_parent([H | T], Parent)  -> %io:format("H is ~p~n", [H]),
%%                                       {dirty_cell, Index, _} = H,
%%                                       {_Cell, Links} = Parent,
%%                                       %io:format("Links is ~p~n", [Links]),
%%                                       case lists:keymember(Index, 3, Links) of
%%                                           true  -> H;
%%                                           false -> has_dirty_parent(T, Parent)
%%                                       end.

init_front_end_notify() ->
    _Return = put('front_end_notify', []),
    ok.

tell_front_end("move", RefX) ->
    remoting_reg:notify_refresh(RefX#refX.site, RefX#refX.path).

tell_front_end(_FnName) ->
    List = lists:reverse(get('front_end_notify')),
    Fun = fun({{Key, "__"++_V}, _A, _B}) when is_record(Key, refX) -> ok;
             ({{Key, V}, A, change}) when is_record(Key, refX) -> 
                  #refX{site = S1, path = P1, obj = Rf} = Key,
                  remoting_reg:notify_change(S1, P1, Rf, V, A);
             ({{Key, V}, A, delete}) when is_record(Key, refX) -> 
                  #refX{site = S1, path = P1, obj = Rf} = Key,
                  remoting_reg:notify_delete(S1, P1, Rf, V, A);             
             ({{S, P}, A, B}) ->
                  remoting_reg:notify_style(S, P, A, B)
          end,
    [ok = Fun(X) || X <- List],
    ok.

shift(RefX, XOff, YOff) ->
    #refX{obj = {cell, {XF, YF}}} = RefX,
    % finally make the new RefX record
    XT = XF + XOff,
    YT = YF + YOff,
    RefX#refX{obj = {cell, {XT, YT}}}.

%% takes a list of incoming hypernumbers and an displacement around a reference
%% and then only returns those hypernumbers which need to be changed
shorten(List, {cell, {X, Y}}, Displacement) ->
    shorten(List, {range, {X, Y, X, Y}}, Displacement);
shorten(List, {range, {X1, Y1, X2, Y2}} = _Range, "vertical") ->
    {X1a, Y1a, X2a, _Y2a} = hn_util:rectify_range(X1, Y1, X2, Y2),
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {MyX, MyY}}} = Parent,
                  ((MyX >= X1a) and (MyX =< X2a) and (MyY > Y1a))
          end,
    lists:filter(Fun, List);
shorten(List, {range, {X1, Y1, X2, Y2}} = _Range, "horizontal") ->
    {X1a, Y1a, _X2a, Y2a} = hn_util:rectify_range(X1, Y1, X2, Y2),
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {MyX, MyY}}} = Parent,
                  ((MyY >= Y1a) and (MyY =< Y2a) and (MyX > X1a))
          end,
    lists:filter(Fun, List);
shorten(List, {column, {X1, _X2}}, "horizontal") ->
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {MyX, _MyY}}} = Parent,
                  (MyX > X1)
          end,
    lists:filter(Fun, List);
shorten(List, {row, {Y1, _Y2}}, "vertical") ->
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {_MyX, MyY}}} = Parent,
                  (MyY >= Y1)
          end,
    lists:filter(Fun, List).

%% converts the dependency tree from the 'wire format' to the 'database format'
convertdep(Child, DepTree) when is_record(Child, refX) ->
    #refX{site = Site} = Child,
    convertdep1(DepTree, Site, []).

convertdep1([], _Site, Acc)      -> Acc;
convertdep1([H | T ], Site, Acc) ->
    RefX = hn_util:parse_url(H),
    #refX{site = Site1} = RefX,
    case Site of
        Site1 -> convertdep1(T, Site, [{url, [{type, "local"}], [H]} | Acc]);
        _     -> convertdep1(T, Site, [{url, [{type, "remote"}],[H]} | Acc])
    end.

%% extracts the key value pairs
extract_kvs(List) -> extract_kvs1(List, []).

extract_kvs1([], Acc)             -> Acc;
extract_kvs1([{_R, KV} | T], Acc) -> extract_kvs1(T, [KV | Acc]).

%% this clause copies whole pages
copy_n_paste2(#refX{site = Site, obj = {page, "/"}} = From, 
              #refX{site = Site, path = NewPath, obj = {page, "/"}}) ->
    Cells = hn_db_wu:get_cells(From),
    Fun = fun(X) ->
                  ok = hn_db_wu:copy_cell(X, X#refX{path = NewPath}, false)
          end,
    [Fun(X) || X <- Cells],
    ok;
%% this clause copies bits of pages
copy_n_paste2(From, To) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    -> hn_db_wu:copy_cell(From, To, false);
        {ok, 'onto self'}    -> ok;
        {ok, cell_to_range}  -> copy2(From, To, false);
        {ok, range_to_cell}  -> To2 = cell_to_range(To),
                                copy3(From, To2, false);
        {ok, range_to_range} -> copy3(From, To, false)
    end.

cell_to_range(#refX{obj = {cell, {X, Y}}} = RefX) ->
    RefX#refX{obj = {range, {X, Y, X, Y}}}.      

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

%% cell to range
copy2(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    %%#refX{site = Site} = To,
    List = hn_util:range_to_list(To),
    lists:map(fun(X) -> hn_db_wu:copy_cell(From, X, Incr) end, List),
    %%ok = shrink_dirty_cell(Site),
    ok.

%% range to range
copy3(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    % range to range copies are 'tiled'
    TileList = get_tiles(From, To),
    copy3a(From, TileList, Incr).

copy3a(_From, [], _Incr)    -> ok;
copy3a(From, [H | T], Incr) -> FromRange = hn_util:range_to_list(From),
                               ToRange = hn_util:range_to_list(H),
                               ok = copy3b(FromRange, ToRange, Incr),
                               copy3a(From, T, Incr).

copy3b([], [], _Incr)              -> ok;
copy3b([FH | FT], [TH | TT], Incr) ->
    ok = hn_db_wu:copy_cell(FH, TH, Incr),
    copy3b(FT, TT, Incr).

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

is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {cell, _}})  ->
    {ok, single_cell};
is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {cell, _}}) ->
    {ok, range_to_cell};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {ok, range_to_range}.
