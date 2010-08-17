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


-export([write_styles_IMPORT/2,
         write_magic_style_IMPORT/2,
         read_styles_IMPORT/1
        ]).

-export([write_attributes/1,
         write_attributes/3,
         append_row/3,
         read_attribute/2,
         read_inside_ref/1,
         read_intersect_ref/1,
         read_styles/2,
         read_page_structure/1,
         read_pages/1,
         matching_forms/2,
         copy_n_paste/4,
         drag_n_drop/3,
         cut_n_paste/3,
         insert/2, insert/3,
         delete/2, delete/3,
         clear/3,
         %%set_borders/5,
         write_formula_to_range/2,
         force_recalc/1
        ]).

-export([wait_for_dirty/1,
         handle_dirty_cell/3,
         handle_circref_cell/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% API Interfaces                                                             %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

force_recalc(Site) ->
    RefX = #refX{site = Site, path = [], obj = {page, "/"}},
    Fun = fun() ->
                  Pages = hn_db_wu:read_pages(RefX),
                  [ok=mark_these_dirty([RefX#refX{path=X}], nil)||X<-Pages]
          end,
    mnesia:activity(transaction, Fun),
    ok.

%% @spec write_style_IMPORT(#refX{}, #styles{}) -> Index
%% @doc write_style will write a style record
%% It is intended to be used in the FILE IMPORT process only 
%% - normally the respresentation of styles in the magic style record
%% is designed to be hidden from the API
%% (that's for why it is a 'magic' style n'est pas?)
write_styles_IMPORT(RefX, Styles) 
  when is_record(RefX, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  [ok = hn_db_wu:write_style_IMPORT(RefX, S) || S <- Styles],
                  ok
          end,
    write_activity(RefX, Fun, "write_style_IMPORT").

write_magic_style_IMPORT(RefX, MStyle) 
  when is_record(RefX, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  hn_db_wu:write_magic_style_IMPORT(RefX, MStyle) 
          end,
    write_activity(RefX, Fun, "write_style_IMPORT").

read_styles_IMPORT(RefX) when is_record(RefX, refX) ->
    Fun = fun() -> hn_db_wu:read_styles_IMPORT(RefX) end, 
    read_activity(RefX, Fun).

%% -spec set_borders(#refX{}, any(), any(), any(), any()) -> ok.
%% %% @doc  takes a range reference and sets the borders for the range according
%% %% to the borders parameter passed in
%% %% The borders attribute can be one of:
%% %% <ul>
%% %% <li>surround</li>
%% %% <li>inside</li>
%% %% <li>none</li>
%% %% <li>call</li>
%% %% <li>left</li>
%% %% <li>top</li>
%% %% <li>right</li>
%% %% <li>bottom</li>
%% %% </ul>
%% %% all borders except 'none' set the border in a postive fashion - they
%% %% don't toggle. So if a particular cell has a left border set then setting
%% %% its border 'left' means it will *still* have its left border set
%% %% by contrast 'none' tears all borders down.
%% %% Border_Color is a colour expressed as a hex string of format "#FF0000"
%% %% Border_Style can be one of
%% set_borders(#refX{obj = {range, _}} = RefX, "none",_Border, 
%%             _Border_Style, _Border_Color) ->
%%     ok = set_borders2(RefX, "left",   [], [], []),
%%     ok = set_borders2(RefX, "right",  [], [], []),
%%     ok = set_borders2(RefX, "top",    [], [], []),
%%     ok = set_borders2(RefX, "bottom", [], [], []),
%%     ok;

%% set_borders(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, Where, 
%%             Border, B_Style, B_Color) 
%%   when Where == "left" 
%%        orelse Where == "right" 
%%        orelse Where == "top" 
%%        orelse Where == "bottom" ->
%%     NewObj = case Where of
%%                  "left"   -> {range, {X1, Y1, X1, Y2}};
%%                  "right"  -> {range, {X2, Y1, X2, Y2}};
%%                  "top"    -> {range, {X1, Y1, X2, Y1}};
%%                  "bottom" -> {range, {X1, Y2, X1, Y2}}
%%              end,
%%     NewRefX = RefX#refX{obj = NewObj},
%%     ok = set_borders2(NewRefX, Where, Border, B_Style, B_Color);
 
%% set_borders(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, 
%%             Where, Border, B_Style, B_Color) 
%%   when Where == "surround" ->
%%     Top    = RefX#refX{obj = {range, {X1, Y1, X2, Y1}}},
%%     Bottom = RefX#refX{obj = {range, {X1, Y2, X2, Y2}}},
%%     Left   = RefX#refX{obj = {range, {X1, Y1, X1, Y2}}},
%%     Right  = RefX#refX{obj = {range, {X2, Y1, X2, Y2}}},
%%     ok = set_borders2(Top,    "top",    Border, B_Style, B_Color),
%%     ok = set_borders2(Bottom, "bottom", Border, B_Style, B_Color),
%%     ok = set_borders2(Left,   "left",   Border, B_Style, B_Color),
%%     ok = set_borders2(Right,  "right",  Border, B_Style, B_Color),
%%     ok;

%% set_borders(#refX{obj = {range, _}} = RefX, Where, Border, B_Style, B_Color)
%%   when Where == "all" ->
%%     ok = set_borders2(RefX, "top",    Border, B_Style, B_Color),
%%     ok = set_borders2(RefX, "bottom", Border, B_Style, B_Color),
%%     ok = set_borders2(RefX, "left",   Border, B_Style, B_Color),
%%     ok = set_borders2(RefX, "right",  Border, B_Style, B_Color),
%%     ok;

%% %% there are a number of different function heads for 'inside'
%% %% 'inside' on a cell does nothing
%% set_borders(#refX{obj = {range, {X1, Y1, X1, Y1}}} = _RefX, 
%%             Where, _Border, _B_Style, _B_Color) 
%%   when Where == "inside" ->
%%     ok;
%% %% 'inside' a single column
%% set_borders(#refX{obj = {range, {X1, Y1, X1, Y2}}} = RefX, 
%%             Where, Border, B_Style, B_Color) 
%%   when Where == "inside" ->
%%     NewRefX = RefX#refX{obj = {range, {X1, Y1 + 1, X1, Y2}}},
%%     ok = set_borders2(NewRefX, "top", Border, B_Style, B_Color);
%% %% 'inside' a single row
%% set_borders(#refX{obj = {range, {X1, Y1, X2, Y1}}} = RefX, 
%%             Where, Border, B_Style, B_Color) 
%%   when Where == "inside" ->
%%     NewRefX = RefX#refX{obj = {range, {X1 + 1, Y1, X2, Y1}}},
%%     ok = set_borders2(NewRefX, "left", Border, B_Style, B_Color);
%% %% proper 'inside'
%% set_borders(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, 
%%             Where, Border, B_Style, B_Color) 
%%   when Where == "inside" ->
%%     NewRefX1 = RefX#refX{obj = {range, {X1, Y1 + 1, X2, Y2}}},
%%     ok = set_borders2(NewRefX1, "top", Border, B_Style, B_Color),
%%     NewRefX2 = RefX#refX{obj = {range, {X1 + 1, Y1, X2, Y2}}},
%%     ok = set_borders2(NewRefX2, "left", Border, B_Style, B_Color).

%% set_borders2(RefX, Where, Border, B_Style, B_Color) ->
%%     Fun = fun() ->
%%                   ok = init_front_end_notify(),
%%                   B   = "border-" ++ Where ++ "-width",
%%                   B_S = "border-" ++ Where ++ "-style",
%%                   B_C = "border-" ++ Where ++ "-color",
%%                   ok = hn_db_wu:write_attr(RefX, {B,   Border}),
%%                   ok = hn_db_wu:write_attr(RefX, {B_S, B_Style}),
%%                   ok = hn_db_wu:write_attr(RefX, {B_C, B_Color})
%%           end,
%%     write_activity(RefX, Fun, "set_borders2").

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



%% @doc reads pages
%% @todo fix up api
read_page_structure(RefX) when is_record(RefX, refX) ->
    read_activity(RefX, fun() -> hn_db_wu:read_page_structure(RefX) end).

read_pages(RefX) when is_record(RefX, refX) ->
    read_activity(RefX, fun() -> hn_db_wu:read_pages(RefX) end).    

-spec matching_forms(#refX{}, common | string()) -> [#form{}]. 
matching_forms(RefX, Transaction) ->
    read_activity(RefX, fun() -> 
                                hn_db_wu:matching_forms(RefX, Transaction) 
                        end).

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
write_attributes([], _PAr, _VAr) -> ok;
write_attributes(List, PAr, VAr) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  [ok = write_attributes1(RefX, L, PAr, VAr) 
                   || {RefX, L} <- List],
                  ok
          end,
    {Ref, _} = hd(List),
    write_activity(Ref, Fun, "quiet").


append_row([], _PAr, _VAr) -> ok;
append_row(List, PAr, VAr) when is_list(List) ->
    %% all the refX's in the list must have the same site/path/object type
    {RefX=#refX{site = S, path = P},_} = hd(List),
    Trans =
        fun() ->
                ok = init_front_end_notify(),
                Row = hn_db_wu:get_last_row(RefX) + 1,
                F = fun(X, Val) ->
                            Obj = {cell, {X, Row}},
                            RefX2 = #refX{site = S, path = P, obj = Obj},
                            hn_db_wu:write_attrs(RefX2, 
                                                 [{"formula", Val}], 
                                                 PAr),
                            mark_these_dirty([RefX2], VAr)
                    end,
                [F(X,V) || {#refX{site=S1, path=P1, obj={column,{X,X}}}, V} 
                               <- List, S == S1, P == P1]
        end,
    
    write_activity(RefX, Trans, "write last").

-spec read_attribute(#refX{}, string()) -> [{#refX{}, term()}].
read_attribute(RefX, Field) when is_record(RefX, refX) ->
    Fun = fun() -> hn_db_wu:read_ref_field(RefX, Field, read) end,
    read_activity(RefX, Fun).

-spec read_inside_ref(#refX{}) -> [{#refX{}, [{string(), term()}]}].
read_inside_ref(RefX) ->
    Fun = fun() -> hn_db_wu:read_ref(RefX, inside) end,
    read_activity(RefX, Fun).

-spec read_intersect_ref(#refX{}) -> [{#refX{}, [{string(), term()}]}].
read_intersect_ref(RefX) ->
    Fun = fun() -> hn_db_wu:read_ref(RefX, intersect) end,
    read_activity(RefX, Fun).

%% @doc read_style gets the list of styles that pertain to a particular 
-spec read_styles(#refX{}, [integer()]) -> #style{}. 
read_styles(RefX, Idxs) when is_record(RefX, refX) ->
    Fun = fun() -> hn_db_wu:read_styles(RefX, Idxs) end,
    read_activity(RefX, Fun).

%% @todo This needs to check if it intercepts a shared formula
%% and if it does it should fail...
insert(#refX{obj = {column, _}} = RefX, Ar) ->
    move(RefX, insert, horizontal, Ar);
insert(#refX{obj = {row, _}} = RefX, Ar)  ->
    move(RefX, insert, vertical, Ar);
insert(#refX{obj = R} = RefX, Ar) 
  when R == cell orelse R == range ->
    move(RefX, insert, vertical, Ar).

%% The Type variable determines how the insert displaces the existing cases...
insert(#refX{obj = {R, _}} = RefX, Disp, Ar)
  when is_record(RefX, refX), (R == cell orelse R == range),
       (Disp == horizontal orelse Disp == vertical)->
    move(RefX, insert, Disp, Ar).

%% @doc deletes a column or a row or a page
%% 
%% @todo this is all bollocks - should be row, column then cell/range as 
%% per insert/2.
%% This needs to check if it intercepts a shared formula
%% and if it does it should fail...
-spec delete(#refX{}, auth_srv:auth_req()) -> ok.
delete(#refX{obj = {R, _}} = RefX, Ar) when R == column orelse R == row ->
    Disp = case R of
               row    -> vertical;
               column -> horizontal
           end,
    move(RefX, delete, Disp, Ar);
delete(#refX{obj = {page, _}} = RefX, Ar) ->
    Fun1 = fun() ->
                   ok = init_front_end_notify(),
                   Dirty = hn_db_wu:delete_cells(RefX),
                   mark_these_dirty(Dirty, Ar)
           end,
    write_activity(RefX, Fun1, "delete").

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
  when R == cell orelse R == range orelse R == row orelse R == column ->
    move(RefX, delete, Disp, Ar).

move(RefX, Type, Disp, Ar)
  when (Type == insert orelse Type == delete)
       andalso (Disp == vertical orelse Disp == horizontal) ->
    Fun = fun() -> move_tr(RefX, Type, Disp, Ar) end,
    write_activity(RefX, Fun, "move").

move_tr(RefX, Type, Disp, Ar) ->
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
    ReWr = do_delete(Type, RefX),
    MoreDirty = hn_db_wu:shift_cells(RefX, Type, Disp, ReWr),
    mark_these_dirty(ReWr, Ar),
    mark_these_dirty(MoreDirty, Ar).        

do_delete(insert, _RefX) ->
    [];
do_delete(delete, RefX) ->
    hn_db_wu:delete_cells(RefX).

%% @doc clears the contents of the cell or range
%% (but doesn't delete the cell or range itself).
%% 
%% If <code>Type  = 'contents'</code> it clears:
%% <ul>
%% <li>formula</li>
%% <li>values</li>
%% </ul>
%% If <code>Type = 'style'</code> it clears the style.
%% If <code>Type = {'attributes', List}</code> it clears all the attributes
%% in the list
%% If <code>Type = 'all'</code> it clears style, content and all attributes.
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
clear(RefX, Type, Ar) when is_record(RefX, refX) ->
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                hn_db_wu:clear_cells(RefX, Type),
                mark_these_dirty([RefX], Ar)
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
cut_n_paste(From, To, Ar) when 
      is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To, all, Ar),
                  ok = clear(From, all, Ar)
          end,
    write_activity(From, Fun, "cut n paste").

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
-spec copy_n_paste(#refX{}, #refX{}, all | style | value, auth_srv:auth_req()) -> ok. 
copy_n_paste(From, To, What, Ar) when 
      is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To, What, Ar)
          end,
    write_activity(From, Fun, "copy n paste").

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
drag_n_drop(From, To, Ar) 
  when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  case is_valid_d_n_d(From, To) of
                      {ok, 'onto self', _Incr} -> ok;
                      {ok, single_cell, Incr} -> 
                          copy_cell(From, To, Incr, all, Ar);
                      {ok, cell_to_range, Incr} -> 
                          copy2(From, To, Incr, all, Ar)
                  end
          end,
    ok = write_activity(From, Fun, "drag n drop").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec wait_for_dirty(string()) -> ok. 
wait_for_dirty(Site) ->
    case dbsrv:is_busy(Site) of
        true ->
            timer:sleep(100),
            wait_for_dirty(Site);
        false ->
            ok
    end.

-spec handle_dirty_cell(string(), cellidx(), auth_srv:auth_req()) -> ok.
handle_dirty_cell(Site, Idx, Ar) ->
    ok = init_front_end_notify(),  
    Fun = fun() ->
                  Cell = hn_db_wu:idx_to_ref(Site, Idx),
                  Attrs = case hn_db_wu:read_ref(Cell, inside, write) of
                              [{_, A}] -> A;
                              _ -> orddict:new()
                          end,
                  case orddict:find("formula", Attrs) of
                      {ok, F} ->
                          hn_db_wu:write_attrs(Cell, [{"formula", F}], Ar);
                      _ ->
                          ok
                  end
          end,
    mnesia:activity(transaction, Fun),
    tell_front_end("handle dirty", #refX{}).

-spec handle_circref_cell(string(), cellidx(), auth_srv:auth_req()) -> ok.
handle_circref_cell(Site, Idx, Ar) ->
    Fun = fun() ->
                  Cell = hn_db_wu:idx_to_ref(Site, Idx),
                  hn_db_wu:write_attrs(Cell, 
                                       [{"formula", "=#CIRCREF!"}], 
                                       Ar),
                  ok
          end,
    mnesia:activity(transaction, Fun).
                      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Internal Functions                                                         %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_attributes1(#refX{obj = {range, _}}=Ref, AttrList, PAr, VAr) ->
    List = hn_util:range_to_list(Ref),
    [ok = write_attributes1(X, AttrList, PAr, VAr) || X <- List],
    ok;
write_attributes1(RefX, List, PAr, VAr) ->
    hn_db_wu:write_attrs(RefX, List, PAr),
    case lists:keymember("formula", 1, List) of
       true  -> ok = mark_these_dirty([RefX], VAr);
       false -> ok
    end.

-spec copy_cell(#refX{}, #refX{}, 
                false | horizontal | vertical,
                all | style | value,
                auth_srv:uid())
               -> ok.
copy_cell(From = #refX{site = Site, path = Path}, To, Incr, What, Ar) ->
    case auth_srv:get_any_view(Site, Path, Ar) of
        {view, _} ->
            hn_db_wu:copy_cell(From, To, Incr, What),
            mark_these_dirty([To], Ar);
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

is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {cell, _}})  ->
    {ok, single_cell};
is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {cell, _}}) ->
    {ok, range_to_cell};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {ok, range_to_range}.


-spec read_activity(#refX{}, fun()) -> any().
read_activity(#refX{site=Site}, Op) ->
    Activity = fun() -> mnesia:activity(transaction, Op) end,
    dbsrv:read_only_activity(Site, Activity).

-spec write_activity(#refX{}, fun(), string() | quiet) -> ok.
write_activity(Ref=#refX{site=Site}, Op, FrontEnd) ->
    Activity = fun() ->
                       Ret = mnesia:activity(transaction, Op),
                       tell_front_end(FrontEnd, Ref),
                       Ret
               end,
    dbsrv:write_activity(Site, Activity).

-spec mark_these_dirty([#refX{}], auth_srv:auth_req()) -> ok.
mark_these_dirty([], _) -> ok;
mark_these_dirty(Refs = [#refX{site = Site}|_], AReq) ->
    F = fun(C) -> case hn_db_wu:ref_to_idx(C) of
                      false -> []; 
                      Idx   -> Idx
                  end
        end,
    Idxs = lists:flatten([F(C) || R <- Refs, C <- hn_db_wu:expand_ref(R)]),
    Entry = #dirty_queue{dirty = Idxs, auth_req = AReq},
    mnesia:write(hn_db_wu:trans(Site, dirty_queue), Entry, write).

init_front_end_notify() ->
    _Return = put('front_end_notify', []),
    ok.

tell_front_end(quiet, _RefX) ->
    ok;
tell_front_end("move", RefX) ->
    remoting_reg:notify_refresh(RefX#refX.site, RefX#refX.path);
tell_front_end(_FnName, _refX) ->
    List = lists:reverse(get('front_end_notify')),
    Fun = fun({change, #refX{site=S, path=P, obj=O}, Attrs}) ->
                  remoting_reg:notify_change(S, P, O, Attrs);
             ({delete, #refX{site=S, path=P, obj=O}}) ->
                  remoting_reg:notify_delete(S, P, O);
             ({style, #refX{site=S, path=P}, Style}) ->
                  remoting_reg:notify_style(S, P, Style)
          end,
    [ok = Fun(X) || X <- List],
    ok.



