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
-include("auth.hrl").

-export([
         write_attributes/1,
         write_attributes/3,
         write_last/3,
         read_last/1,
         write_style_IMPORT/2,
         read_attribute/2,
         read_ref/1,
         read_inherited_list/2,
         read_inherited_value/3,
         read_styles/1,
         read_page_structure/1,
         read_pages/1,
         recalculate/1,
         reformat/1,
         drag_n_drop/3,
         copy_n_paste/3,
         cut_n_paste/3,
         copy_style/3,
         insert/2,
         insert/3,
         delete/2,
         delete/3,
         clear/3,
         handle_dirty_cell/3,
         set_borders/5,
         initialise_remote_page_vsn/2,
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
    case mnesia:dirty_first(hn_db_wu:trans(Site, dirty_queue)) of
        '$end_of_table' ->
            ok;
        _Index ->
            timer:sleep(100),
            wait_for_dirty(Site) 
    end.

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
    mnesia:activity(transaction, fun hn_db_wu:read_page_structure/1, [RefX]).

read_pages(RefX) when is_record(RefX, refX) ->
    mnesia:activity(transaction, fun hn_db_wu:read_pages/1, [RefX]).    

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

-spec handle_dirty_cell(string(), cellidx(), nil | uid()) -> ok. 
handle_dirty_cell(Site, Idx, Ar) ->
    ok = init_front_end_notify(),  
    Fun = fun() ->
                Cell = hn_db_wu:local_idx_to_refX(Site, Idx),
                case hn_db_wu:read_ref_field(Cell, "formula", write) of
                    [{Cell, F}] -> 
                        hn_db_wu:write_attrs(Cell, [{"formula", F}], Ar);
                    _ ->
                        ok
                end
        end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok = tell_front_end("handle dirty").

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

write_attributes(List) ->
    write_attributes(List, nil, nil).
write_attributes(List, PAr, VAr) ->
    Fun = fun() ->
                  [ok = write_attributes1(RefX, L, PAr, VAr) 
                   || {RefX, L} <- List],
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
write_last([], _PAr, _VAr) -> ok;
write_last(List, PAr, VAr) when is_list(List) ->
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
                            hn_db_wu:write_attr(RefX2, {"formula", Val}, PAr),
                            hn_db_wu:mark_children_dirty(RefX2, VAr)
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

-spec read_attribute(#refX{}, string()) -> [{#refX{}, term()}].
read_attribute(RefX, Field) when is_record(RefX, refX) ->
    Fun = fun() -> hn_db_wu:read_ref_field(RefX, Field, read) end,
    mnesia:activity(transaction, Fun).

-spec read_ref(#refX{}) -> [{#refX{}, [{string(), term()}]}].
read_ref(RefX) ->
    Fun = fun() -> hn_db_wu:read_ref(RefX) end,
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
insert(#refX{obj = {column, _}} = RefX, Ar) ->
    move(RefX, insert, horizontal, Ar);
insert(#refX{obj = {row, _}} = RefX, Ar)  ->
    move(RefX, insert, vertical, Ar);
insert(#refX{obj = R} = RefX, Ar) 
  when R == cell orelse R == range ->
    move(RefX, insert, vertical, Ar).

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
-spec delete(#refX{}, nil | uid()) -> ok.
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
                   hn_db_wu:mark_these_dirty(Dirty, Ar)
           end,
    mnesia:activity(transaction, Fun1),
    ok = tell_front_end("delete").

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
    ok = mnesia:activity(transaction, fun move_tr/4, [RefX, Type, Disp, Ar]),
    ok = tell_front_end("move", RefX).

move_tr(#refX{obj = Obj} = RefX, Type, Disp, Ar) ->

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
    hn_db_wu:mark_these_dirty(ReWr, Ar),
    hn_db_wu:mark_these_dirty(MoreDirty, Ar),

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
    % PageRef = RefX#refX{obj = {page, "/"}},
    
    % OK all our local stuff is sorted, now lets deal with the remote
    % children
    % Change = {insert, Obj, Disp},
    % set the delay to zero
    % ok = hn_db_wu:mark_notify_out_dirty(PageRef, Change, 0),
    
    %% Status = lists:flatten([Status1, Status2]),

    %% % finally deal with any cells returned from delete_cells that
    %% % are dirty - these need to be recalculated now that the link/local_objs
    %% % tables have been transformed
    %% Fun2 = fun(X) ->
    %%                case hn_db_wu:read_attrs(X, ["formula"], write) of
    %%                    [{X, {"formula", F}}] ->
    %%                        hn_db_wu:write_attr(X, {"formula", F});
    %%                    _  ->
    %%                        ok
    %%                end
    %%        end,
    %% [ok = Fun2(X) || {dirty, X} <- Status],
    % Jobs a good'un, now for the remote parents
    % io:format("in hn_db_api:move do something with Parents...~n"),
    %%_Parents =  hn_db_wu:find_incoming_hn(Site, PageRef),
    %io:format("in hn_db_api:move Parents are ~p~n", [Parents]),
    ok.

do_delete(insert, _RefX) ->
    [];
do_delete(delete, RefX) ->
    hn_db_wu:delete_cells(RefX).

%% @spec clear(#refX{}, Type) -> ok
%% Type = [contents | style | all | {attributes, List}]
%% @doc clears the contents of the cell or range
%% (but doesn't delete the cell or range itself).
%% 
%% If <code>Type  = 'contents'</code> it clears:
%% <ul>
%% <li>formula</li>
%% <li>values</li>
%% </ul>
%% If <code>Type = 'style'</code> it clears the style.
%% If >code>Type = {'attributes', List}</code> it clears all the attributes
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
                hn_db_wu:mark_children_dirty(RefX, Ar)
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
cut_n_paste(From, To, Ar) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To, Ar),
                  ok = clear(From, all, Ar)
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
copy_n_paste(From, To, Ar) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To, Ar)
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
drag_n_drop(From, To, Ar) 
  when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  case is_valid_d_n_d(From, To) of
                      {ok, 'onto self', _Incr} -> ok;
                      {ok, single_cell, Incr} -> 
                          copy_cell(From, To, Incr, Ar);
                      {ok, cell_to_range, Incr} -> 
                          copy2(From, To, Incr, Ar)
                  end
          end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("drag n drop").

%% @spec(From::refX{}, To::refX{}) -> ok
%% @doc Copies the style applied to From and attaches it to To.
%%      From can only be a cell ref but To can be either a cell or range
%%      ref
%% @end
copy_style(#refX{obj = {range, {X, Y, _, _}}} = From, To, Ar) ->
                  copy_style(From#refX{obj = {cell, {X, Y}}}, To, Ar);
copy_style(#refX{obj = {cell, _}} = From, 
           #refX{obj = {Type, _}} = To, Ar)
  when Type == cell orelse Type == range ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  hn_db_wu:copy_style(From, To, Ar)
          end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("copy style").

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

write_attributes1(#refX{obj = {range, _}}=Ref, AttrList, PAr, VAr) ->
    List = hn_util:range_to_list(Ref),
    [ok = write_attributes1(X, AttrList, PAr, VAr) || X <- List],
    ok;
write_attributes1(RefX, List, PAr, VAr) 
  when is_record(RefX, refX), is_list(List) ->
    ok = init_front_end_notify(),
    hn_db_wu:write_attrs(RefX, List, PAr),
    case lists:keymember("formula", 1, List) of
       true  -> ok = hn_db_wu:mark_children_dirty(RefX, VAr);
       false -> ok
    end.

-spec copy_cell(#refX{}, #refX{}, 
                false | horizontal | vertical,
                uid())
               -> ok.
copy_cell(From = #refX{site = Site, path = Path}, To, Incr, Ar) ->
    case auth_srv:get_any_view(Site, Path, Ar) of
        {view, _} ->
            hn_db_wu:copy_cell(From, To, Incr),
            hn_db_wu:mark_children_dirty(To, Ar);
        _ ->
            throw(auth_error)
    end.

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

%% this clause copies whole pages
copy_n_paste2(#refX{site = Site, obj = {page, "/"}} = From, 
              #refX{site = Site, path = NewPath, obj = {page, "/"}},
              Ar) ->
    Cells = hn_db_wu:get_cells(From),
    [ok = copy_cell(X, X#refX{path = NewPath}, false, Ar) 
     || X <- Cells],
    ok;
%% this clause copies bits of pages
copy_n_paste2(From, To, Ar) ->
    case is_valid_c_n_p(From, To) of
        {ok, 'onto self'}    -> ok;
        {ok, single_cell}    -> 
            ok = copy_cell(From, To, false, Ar);
        {ok, cell_to_range} -> 
            copy2(From, To, false, Ar);
        {ok, range_to_cell} -> 
            To2 = cell_to_range(To),
            copy3(From, To2, false, Ar);
        {ok, range_to_range} -> 
            copy3(From, To, false, Ar)
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
copy2(From, To, Incr, Ar) 
  when is_record(From, refX), is_record(To, refX) ->
    List = hn_util:range_to_list(To),
    [copy_cell(From, X, Incr, Ar) || X <- List],
    ok.

%% range to range
copy3(From, To, Incr, Ar) 
  when is_record(From, refX), is_record(To, refX) ->
    % range to range copies are 'tiled'
    TileList = get_tiles(From, To),
    copy3a(From, TileList, Incr, Ar).

copy3a(_From, [], _Incr, _Ar)   -> ok;
copy3a(From, [H | T], Incr, Ar) -> 
    FromRange = hn_util:range_to_list(From),
    ToRange = hn_util:range_to_list(H),
    ok = copy3b(FromRange, ToRange, Incr, Ar),
    copy3a(From, T, Incr, Ar).

copy3b([], [], _Incr, _Ar)             -> ok;
copy3b([FH | FT], [TH | TT], Incr, Ar) ->
    ok = copy_cell(FH, TH, Incr, Ar),
    copy3b(FT, TT, Incr, Ar).

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

