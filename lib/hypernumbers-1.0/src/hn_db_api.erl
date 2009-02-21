%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This module provides the data access api.
%%%            Each function in this should call functions from
%%%            {@link hn_db_wu} which provides work units and it
%%%            should wrap them in an mnesia transaction.
%%%            
%%%            mnesia MUST NOT be called from any function in
%%%            this module.
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
%%%            <li>{cell, {X, Y}}</li>
%%%            <li>{range, {X1, Y1, X2, Y2}}</li>
%%%            <li>{column, {X1, X2}}</li>
%%%            <li>{row, {Y1, Y2}}</li>
%%%            <li>{page, "/"}</li>
%%%            </ul>
%%% 
%%% @TODO should we have a subpages #refX egt {subpages, "/"}
%%% which would alllow you to specify operations like delete and copy
%%% on whole subtrees?
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_api).

-include("spriki.hrl").

-define(mn_ac,mnesia:activity).
-define(mn_sl,mnesia:select).
-define(copy, hn_db_wu:copy_cell).
-define(counter, mnesia:dirty_update_counter).

-export([
         write_attributes/2,
         write_last/1,
         % write_permission/2,
         % write_style/2,
         read_attributes/2,
         read/1,
         read_styles/1,
         % read_permissions/1,
         % update_style/2,
         recalculate/1,
         reformat/1,
         drag_n_drop/2,
         copy_n_paste/2,
         cut_n_paste/2,
         % copy_page/2,
         insert/1,
         insert/2,
         delete/1,
         delete/2,
         unregister_hypernumber/3,
         clear/1,
         clear/2,
         % delete_permission/1,
         % delete_style/1,
         notify_back/3,
         notify_hypernumber/1
        ]).

%%% Debugging interface
-export([copy_DEBUG/0,
         delete_cell_contents_DEBUG/1,
         clear_cells_DEBUG/1,
         insert_delete_DEBUG/0,
         delete_DEBUG/2,
         insert_DEBUG/2,
         clear_TEST/0]).

-export([get_tiles_DEBUG/0,
         read_styles_DEBUG/0,
         hn_DEBUG/0]). % Debugging

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% API Interfaces                                                             %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec nofity_hypernumber(Parent::#refX{}) -> {ok, ok}
%% @doc notify's any remote sites that a hypernumber has changed.
%% the reference must be for a cell
%% @todo generalise the references to row, column, range and page
notify_hypernumber(Parent) ->
    Fun = fun() ->
                  H = hn_db_wu:read_outgoing_hypernumbers(Parent),
                  [{Parent, {rawvalue, V}}] = hn_db_wu:read_attrs(Parent, [rawvalue]),
                  {H, V}
          end,
    {List, Value} = mnesia:activity(transaction, Fun),
    ParentIdx = hn_util:index_from_refX(Parent),
    ParentURL = hn_util:index_to_url(ParentIdx),
%    error_logger:error_msg("in hn_db_wu:notify_hypernumber *WARNING* "++
%                           "notify_hypernumber not using "++
%                           "version number - ie it aint working - yet :("),
    Fun2 = fun(X) ->
                   {Server, _} = X#outgoing_hn.index,
                   Version = tconv:to_s(X#outgoing_hn.version),
                   Actions = simplexml:to_xml_string(
                               {notify,[],[
                                           {biccie,     [], [X#outgoing_hn.biccie]},
                                           {parent, [], [ParentURL]},
                                           {type,       [], ["change"]},
                                           {value,      [], hn_util:to_xml(Value)},
                                           {version,    [], [Version]}
                                          ]}),
                   
                   hn_util:post(Server,Actions,"text/xml"),
                   ok
           end,
    [ok = Fun2(X) || X <- List],
    {ok, ok}.
           
%% @spec notify_back(ParentRefX::#refX{}, ChildRefX::#refX{}, Change) -> 
%% {ok, ok}
%% @doc notify's a change of a cell back to its remote hypernumber parent
%% <code>#refX{}</code> can be a cell only
%% @todo expand the paradigm to include ranges, columns, rows references and 
%% queries as things that be remote parents
notify_back(ParentRefX, ChildRefX, Change)
  when is_record(ChildRefX, refX), is_record(ParentRefX, refX) ->
    Fun1 = fun() ->
                  hn_db_wu:read_incoming_hypernumber(ParentRefX)
          end,
    [Hypernumber] = mnesia:activity(transaction, Fun1),
    ChildIdx = hn_util:refX_to_index(ChildRefX),
    ParentIdx = hn_util:refX_to_index(ParentRefX),
    #incoming_hn{biccie = Biccie} = Hypernumber,
    #refX{site = Server} = ParentRefX,
    ChildUrl=hn_util:index_to_url(ChildIdx),
    ParentUrl=hn_util:index_to_url(ParentIdx),
    Actions = simplexml:to_xml_string(
                {notify_back,[],[
                                 {biccie,      [],[Biccie]},
                                 {child_url,   [],[ChildUrl]},
                                 {parent_url,  [],[ParentUrl]},
                                 {type,        [],[Change]}
                                ]}),

    Fun2 = fun() ->
                  hn_db_wu:clear_dirty_notify_back(ParentRefX, ChildRefX, Change)
          end,
    mnesia:activity(transaction, Fun2).
                      
unregister_hypernumber(ParentUrl, ChildUrl, Biccie)
  when is_record(ParentUrl, refX), is_record(ChildUrl, refX) ->
    Fun = fun() ->
                  hn_db_wu:delete_outgoing_hn(ParentUrl, ChildUrl, Biccie)
          end,
    mnesia:activity(transaction, Fun).

%% @spec write_attributes(RefX :: #refX{}, List) -> {ok, ok}     
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
write_attributes(RefX, List) when is_record(RefX, refX), is_list(List) ->
    Fun = fun() ->
                  [{ok, ok} = hn_db_wu:write_attr(RefX, X) || X <- List]
          end,
    mnesia:activity(transaction, Fun).

%% @spec write_last(List) -> {ok, ok}
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
write_last(List) when is_list(List) ->
    [{#refX{site = S, path = P, obj = O} = RefX, _} | T] = List,
    Fun =
        fun() ->
                {LastColumnRefX, LastRowRefX} = hn_db_wu:get_last_refs(RefX),
                #refX{obj = {cell, {LastCol, _}}} = LastColumnRefX,
                #refX{obj = {cell, {_, LastRow}}} = LastRowRefX,
                % Add 1 to because we are adding data 'as the last row'
                % (or column) ie one more than the current last row/column
                {Type, Pos} = case O of
                                  {row, _}    -> Y = LastRow + 1, 
                                                 {row, {Y, Y}};
                                  {column, _} -> X = LastCol + 1,
                                                 {column, {X, X}}
                              end,
                % now convert the column or row references to cell references

                % The matches in the Fun ensure that all the cells are the same
                % page and are row or column references as appropriate 
                Fun1 =
                    fun({#refX{site = S, path = P, obj = {Type, Idx}}, Val})  ->
                            Obj = case Type of
                                      row    -> {cell, {Pos, Idx}};
                                      column -> {cell, {Idx, Pos}}
                                  end,
                            RefX2 = #refX{site = S, path = P, obj = Obj},
                            hn_db_wu:write_attr(RefX2, {formula, Val})
                    end,
                [Fun1(X) || X <- List]
        end,
    Return = mnesia:activity(transaction, Fun),
    {ok, ok}.

%% @spec read_attributes(#refX{}, AttrList) -> {#refX{}, Val}
%% AttrList = [atom()]
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
read_attributes(RefX, AttrList) when is_record(RefX, refX),
                                     is_list(AttrList) ->
    Fun = fun() ->
                  hn_db_wu:read_attrs(RefX, AttrList)
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
                  hn_db_wu:read_attrs(RefX)
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
                  hn_db_wu:read_style(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec recalculate(#refX{}) -> {ok, ok}
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
                  Cells = hn_db_wu:read_attrs(RefX, [formula]),
                  [hn_db_wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun).

%% @spec reformat(#refX{}) -> {ok, ok}
%% @doc reformats the cells refered to
%% 
%% The <code>refX{}</code> can be to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
reformat(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  Cells = hn_db_wu:read_attrs(RefX, [format]),
                  [hn_db_wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun).

%% @spec insert(RefX :: #refX{}) -> ok
%% @doc inserts a single column or a row
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>row</li>
%% <li>column</li>
%% </ul>
insert(#refX{obj = {column, {X1, X2}}} = RefX)  ->
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {insert, column}),
                  RefXs = hn_db_wu:get_refs_right(RefX),
                  % shift doesn't commute so it is up to us to sort
                  % the cells
                  RefXs2 = sort(RefXs, 'right-to-left'),
                  [hn_db_wu:shift_cell(F, offset(F, {0, 1})) || F <- RefXs2]
          end,
    mnesia:activity(transaction, Fun);
insert(#refX{obj = {row, {Y1, Y2}}} = RefX)  ->
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {insert, row}),
                  RefXs = hn_db_wu:get_refs_below(RefX),
                  % shift doesn't commute so it is up to us to sort
                  % the cells
                  RefXs2 = sort(RefXs, 'bottom-to-top'),
                  [hn_db_wu:shift_cell(F, offset(F, {1, 0})) || F <- RefXs2]
          end,
    mnesia:activity(transaction, Fun);
insert(#refX{obj = R} = RefX) when R == cell orelse R == range  ->
    insert(RefX, vertical).

%% @spec insert(RefX :: #refX{}, Type) -> ok 
%% Type = [horizontal | vertical]
%% @doc inserts a cell or range
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% </ul>
insert(#refX{obj = {R, _}} = RefX, Disp) when R == cell orelse R == range ->
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {insert, Disp}),
                  RefXs = hn_db_wu:get_refs_below(RefX),
                  % shift doesn't commute so it is up to us to sort
                  % the cells
                  RefXs2 = sort(RefXs, 'bottom-to-top'),
                  [hn_db_wu:shift_cell(F, offset(F, {0,1})) || F <- RefXs2]
          end,
    mnesia:activity(transaction, Fun).

%% @spec delete(Ref :: #refX{}) -> ok
%% @doc deletes a column or a row or a page
%% 
%% @todo this is all bollocks - should be row, column then cell/range as 
%% per insert/2
delete(#refX{obj = {R, _}} = RefX) when R == column orelse R == row ->
    Disp = case R of
               row    -> vertical;
               column -> horizontal
           end,
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {delete, Disp}),
                  hn_db_wu:shift_cell(RefX, Disp, delete)
          end,
    mnesia:activity(transaction, Fun);
delete(#refX{obj = {page, _}} = RefX) ->
    Fun = fun() ->
                  hn_db_wu:delete(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec delete(RefX :: #refX{}, Type) -> ok
%% Type = [contents | all | horizontal | vertical]
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
delete(#refX{obj = {R, _}} = RefX, horizontal) when R == cell orelse R == range ->
    Fun =
        fun() ->
                {ok, ok} = write_page_vsn(RefX, {delete, horizontal}),
                hn_db_wu:shift_cell(RefX, horizontal, delete)
        end,
    mnesia:activity(transaction, Fun);
delete(#refX{obj = {R, _}} = RefX, vertical) when R == cell orelse R == range ->
    Fun =
        fun() ->
                {ok, ok} = write_page_vsn(RefX, {delete, vertical}),
                hn_db_wu:shift_cell(RefX, vertical, delete)
        end,
    mnesia:activity(transaction, Fun).

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
                hn_db_wu:clear_cells(RefX, Type)
        end,
    mnesia:activity(transaction, Fun).

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
                  {ok, ok} = copy_n_paste2(From, To),
                  {ok, ok} = hn_db_wu:clear_cells(From, all)
          end,
    mnesia:activity(transaction, Fun).

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
%% @todo copy'n'paste a page
copy_n_paste(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  {ok, ok} = copy_n_paste2(From, To)
          end,
    mnesia:activity(transaction, Fun).

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
%% <code>Jan Feb,                Mar, Apr,... </code>
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
                  case is_valid_d_n_d(From, To) of
                      {ok, single_cell, Incr}   -> ?copy(From, To, Incr);
                      {ok, 'onto self', _Incr}  -> {ok, ok};
                      {ok, cell_to_range, Incr} -> copy2(From, To, Incr)
                  end
          end,
    mnesia:activity(transaction, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Internal Functions                                                         %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_page_vsn(RefX, Action) ->
    PageRefX = #refX{obj = {page, "/"}},
    NewVsn = ?counter(page_vsn_counters, PageRefX, 1),
    Record = #page_vsn{page_refX = PageRefX, action = Action,
                       action_refX = RefX, version = NewVsn},
    ok = mnesia:write(Record),
   {ok, ok}.

offset(#refX{obj = {cell, {X, Y}}} = RefX, {XO, YO}) ->
    RefX#refX{obj = {cell, {X + XO, Y + YO}}}.

sort(List, 'bottom-to-top') ->
    Fun = fun(#refX{obj = {cell, {_XA, YA}}} = A,
              #refX{obj = {cell, {_XB, YB}}} = B) ->
                  if
                      (YA - YB)  > 0 -> true;
                      (YA - YB) =< 0 -> false
                  end
          end,
    lists:sort(Fun, List);
sort(List, 'right-to-left') ->
    Fun = fun(#refX{obj = {cell, {XA, _YA}}} = A,
              #refX{obj = {cell, {XB, _YB}}} = B) ->
                  if
                      (XA - XB)  > 0 -> true;
                      (XA - XB) =< 0 -> false
                  end
          end,
    lists:sort(Fun, List).

copy_n_paste2(From, To) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    -> hn_db_wu:copy_cell(From, To, false);
        {ok, 'onto self'}    -> {ok, ok};
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
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, {TX, _TY1, TX, _TY2}}}) ->
    {ok, cell_to_range, vertical};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, {_TX1, TY, _TX2, TY}}}) ->
    {ok, cell_to_range, horizontal};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range, false};
%% range to range drag'n'drop
is_valid_d_n_d(#refX{obj = {range, Range}}, #refX{obj = {range, Range}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#refX{obj = {range, {FX, FY1, FX, FY2}}}, #refX{obj = {range, TRange}}) ->
    {_TX1, TY1, _TX2, TY2} = TRange,
    case ((TY2 - TY1) - (FY2 - FY1)) of
        0    -> {ok, col_range_to_range};
        true -> {error, "target range is not the same height as the source range"}
    end;
is_valid_d_n_d(#refX{obj = {range, {FX1, FY, FX2, FY}}}, #refX{obj = {range, TRange}}) ->
    {TX1, _TY1, TX2, _TY2} = TRange,
    case ((TX2 - TX1) - (FX2 - FX1)) of
        0    -> {ok, row_range_to_range};
        true -> {error, "target range is not the same width as the source range"}
    end;
is_valid_d_n_d(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {error, "from range is invalid"};
is_valid_d_n_d(_, _) -> {error, "not valid either"}.

%% cell to range
copy2(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    List = hn_util:range_to_list(To),
    lists:map(fun(X) -> ?copy(From, X, Incr) end, List),
    {ok, ok}.

%% range to range
copy3(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    % range to range copies are 'tiled'
    TileList = get_tiles(From, To),
    copy3a(From, TileList, Incr).

copy3a(_From, [], Incr)     -> {ok, ok};
copy3a(From, [H | T], Incr) -> FromRange = hn_util:range_to_list(From),
                               ToRange = hn_util:range_to_list(H),
                               {ok, ok} = copy3b(FromRange, ToRange, Incr),
                               copy3a(From, T, Incr).

copy3b([], [], _Incr) -> {ok, ok};
copy3b([FH | FT], [TH | TT], Incr) ->
    {ok, ok} = hn_db_wu:copy_cell(FH, TH, Incr),
    copy3b(FT, TT, Incr).

get_tiles(#refX{obj = {range, {X1F, Y1F, X2F, Y2F}}} = From,
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

%shift(RefX, Disp, _Type) when is_record(RefX, refX) ->
%    % single mnesia transaction
%    % 
%    % The process is:
%    % * get all the cells which will be displaced
%    %  * for each cell (in a transaction)
%    %    * delete
%    %    * rewrite it
%    %    * save it
%    % Need to do funny stuff with remote hypernumbers...

%    % io:format("in shift Ref is ~p Disp is ~p Type is ~p~n", [Ref, Disp, Type]),
%    #refX{site = Site, path = Path, obj = {Range, R}} = RefX,

%    % get the 'ShiftedCells'
%    RefX2 = RefX#refX{obj = {cell, {'$1', '$2'}},  auth  = '_'},
%    Head = ms_util:make_ms(hn_item, [{addr, RefX2}]),
%    % Cond selects the cells to be 'adjusted' and Offset 
%    % determines how they are to be 'adjusted'
%    {Cond, Offset}
%        = case Range of
%              row   ->
%                  A = [{'>', '$1', R}],
%                  B = {0, -1},
%                  {A, B};
%              col   ->
%                  A = [{'>', '$2', R}],
%                  B = {-1, 0},
%                  {A, B};
%              cell  ->
%                  {X, Y} = R,
%                  case Disp of
%                      horizontal ->
%                          A = [{'and', {'>',  '$1', Y}, {'==', '$2', X}}],
%                          B = {-1, 0},
%                          {A, B};
%                      vertical   ->
%                          A = [{'and', {'==', '$1', X}, {'>',  '$2', Y}}],
%                          B = {0, -1},
%                          {A, B}
%                  end;
%              range ->
%                  {X1, Y1, X2, Y2} = R,
%                  case Disp of
%                      horizontal ->
%                          A = [{'and', {'>=', '$1', Y1}, {'==', '$2', X1}}],
%                          B = [{'and', {'=<', '$1', Y2}, {'==', '$2', X2}}],
%                          C = {0, Y1 - Y2}, % should be negative!
%                          {[{'and', A, B}], C};
%                      vertical   ->
%                          A = [{'and', {'==', '$1', X1}, {'>=', '$2', Y1}}],
%                          B = [{'and', {'==', '$1', X2}, {'=<', '$2', Y2}}],
%                          C = {0, X1 - X2}, % should be negative!
%                          {[{'and', A, B}], C}                         
%                  end
%          end,
%    Body = ['$_'],
%    Fun1 = fun() -> ?mn_sl(hn_item, [{Head, Cond, Body}]) end,
%    ShiftedCells = ?mn_ac(transaction, Fun1),
%    % now shift the cells
%    Fun2 = fun() -> hn_db_wu:shift_cell(ShiftedCells, Offset) end,
%    {ok, ok} = ?mn_ac(transaction, Fun2),
%    {ok, ok}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Debugging interfaces                                                       %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
clear_TEST() ->
    Site = "http://127.0.0.1:9000",
    Path = ["test"],

    write_data("http://il_ballo.dev:9000",["data"]),
    write_data(["data2"]),

    write_value(Path, "integer below", {1, 2}, [bold]),
    write_value(Path, "1", {1, 3}, [{colour, "yellow"}]),

    write_value(Path, "local ref below", {1, 5}, [bold]),
    write_value(Path, "=../data2/b1", {1, 6}, [{colour, "yellow"}]),

    write_value(Path, "remote ref below", {1, 8}, [bold]),
    write_value(Path, "=hn(\"http://il_ballo.dev:9000/data/E1?hypernumber\")",
                {1, 9}, [{colour, "yellow"}]),

    make_thick(Path, 1),

    test_util:wait(100),

    % rewrite the same formula
    write_value(Path, "=hn(\"http://il_ballo.dev:9000/data/E1?hypernumber\")",
                 {1, 9}, [{colour, "yellow"}]),

    % clear the remote hyperlink
    % clear_cells(Site, Path, {1, 9}),
    
    % clear_cells_DEBUG(Site, Path),
    ok.

%% @hidden
insert_delete_DEBUG() ->
    insert_delete_DEBUG2("insert").
% insert_delete_DEBUG2("delete").

%% @hidden
delete_DEBUG(Ref, Type) -> delete(Ref, Type).

%% @hidden
insert_DEBUG(Ref, Type) -> insert(Ref, Type).

%% @hidden
copy_DEBUG() ->
    io:format("in copy_DEBUG going into drag'n'drop (2)~n"),
    copy_DEBUG2("drag_n_drop"),
    io:format("in copy_DEBUG going into copy'n'paste (2)~n"),
    copy_DEBUG2("copy_n_paste"),
    io:format("in copy_DEBUG going into cut'n'paste (2)~n"),
    copy_DEBUG2("cut_n_paste"),
    io:format("in copy_DEBUG going into copy'n'paste (3)~n"),
    copy_DEBUG3("copy_n_paste"),
    io:format("in copy_DEBUG going into cut'n'paste (3)~n"),
    copy_DEBUG3("cut_n_paste").

%% @hidden
delete_cell_contents_DEBUG(Site, Path) ->
    Target = #ref{site = Site, path = Path, ref = {range, {1, 1, 30, 30}}},
    clear(Target, contents).

%% @hidden
delete_cell_contents_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    io:format("In delete_cell_contents_DEBUG Path is ~p~n", [Path]),
    delete_cell_contents_DEBUG(Site, Path).

%% @hidden
clear_cells_DEBUG(Site, Path) ->
    Target = #refX{site = Site, path = Path, obj = {range, {1, 1, 30, 30}}},
    clear(Target, all).

%% @hidden
clear_cells_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    clear_cells_DEBUG(Site, Path).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Used In Debugging interfaces                                               %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
insert_delete_DEBUG2(FunName) ->

    Path = [FunName],

    io:format("Clearing the cells...~n"),

    clear_cells_DEBUG(Path),
    clear_cells_DEBUG([FunName | "data"]),
    clear_cells_DEBUG("http://il_ballo.dev:9000", [FunName, "data"]),

    test_util:wait(20),

    % now write out the data and perform the tests
    write_value(Path, FunName, {1, 1}, [bold, underline, center]),

    write_value(Path, "before: "++FunName, {3, 1}, [bold, underline, center]),
    colour(Path, {3, 1}, "grey"),

    % cell stuff first
    write_value(Path, "Cell "++FunName++" (down)", {1, 3},
                [bold, underline, center]),

    write_value(Path, "Links to Column A", {3, 3},
                [bold, underline, center]),
    
    write_value(Path, "=./data/A1", {1, 4}, []),

    write_value(Path, FunName++" a cell here", {1, 5}, []),
    write_value(Path, "=./data/B1", {1, 6}, []),
    write_value(Path, "=hn(\"http://il_ballo.dev:9000/insert/data/C1?hypernumber\")",
                {1, 7}, []),
    colour(Path, {1, 4}, "orange"),
    colour(Path, {1, 5}, "orange"),
    colour(Path, {1, 6}, "orange"),
    colour(Path, {1, 7}, "orange"),
    colour(Path, {1, 8}, "orange"),

    write_value(Path, "=A6", {3, 5}, []),
    colour(Path, {3, 5}, "yellow"),
    
    io:format("Insert a cell...~n"),

    % ensure that the dirty tables are loaded to make this test work...
    gen_server:cast(dirty_cell,         {setstate, passive}),
    gen_server:cast(dirty_incoming_hn,  {setstate, passive}),
    gen_server:cast(dirty_outgoing_hn,  {setstate, passive}),

    % Now set up the various bits of data that is used to mark the 
    % insert tests
    io:format("writing out data...~n"),
    write_data([FunName,"data"]),
    write_data("http://il_ballo.dev:9000",[FunName,"data"]),
        
    insert_delete(FunName, Path, {cell, {1, 5}}, vertical),

    _Return1=gen_server:cast(dirty_cell,        {setstate, active}),
    _Return2=gen_server:call(dirty_cell,        flush, infinity),
    _Return3=gen_server:cast(dirty_incoming_hn, {setstate, active}),
    _Return4=gen_server:call(dirty_incoming_hn, flush, infinity),
    _Return5=gen_server:cast(dirty_outgoing_hn, {setstate, active}),
    _Return6=gen_server:call(dirty_outgoing_hn, flush, infinity),
    
    %    write_value(Path, "Cell "++FunName++" (right)", {1, 9}, 
    %    [bold, underline, center]),
    %    write_value(Path, FunName++" a cell here", {1, 10}, []),
    %    write_value(Path, "=./data/A1", {2, 10}, []),
    %    colour(Path, {1, 10}, "orange"),
    %    colour(Path, {2, 10}, "orange"),

    make_thick(Path, 1),
    make_thick(Path, 3),

    %    write_value(Path, "Row "++FunName++" (right)", {1, 12}, 
    %    [bold, underline, center]),
    %    write_value(Path, FunName++" a row here", {1, 13}, []),
    %    write_value(Path, "=./data/A1", {1, 14}, []),
    %    colour(Path, {1, 13}, "orange"),
    %    colour(Path, {1, 14}, "orange"),

    %    make_thin(Path, 2),

    %    write_value(Path, FunName++" Column Here", {3, 3}, 
    %    [bold, underline, center]),

    %    make_thick(Path, 3),

    %    make_thin(Path, 4),

    %    make_thick(Path, 5),

    %    write_value(Path, "Range "++FunName++" (down)", {5, 14}, 
    %    [bold, underline, center]),
    %    write_value(Path, "=./data/A1", {5, 15}, []),
    %    write_value(Path, "=./data/B1", {5, 16}, []),
    %    write_value(Path, "=./data/C1", {5, 17}, []),
    %    write_value(Path, "=./data/D1", {5, 18}, []),
    %    colour(Path, {5, 15}, "orange"),
    %    colour(Path, {5, 16}, "orange"),
    %    colour(Path, {5, 17}, "orange"),
    %    colour(Path, {5, 18}, "orange"),

    %    write_value(Path, "=./data/E1", {6, 15}, []),
    %    write_value(Path, "=./data/F1", {6, 16}, []),
    %    write_value(Path, "=./data/G1", {6, 17}, []),
    %    write_value(Path, "=./data/H1", {6, 18}, []),
    %    colour(Path, {6, 15}, "orange"),
    %    colour(Path, {6, 16}, "yellow"),
    %    colour(Path, {6, 17}, "yellow"),
    %    colour(Path, {6, 18}, "orange"),

    %    write_value(Path, "=./data/I1", {7, 15}, []),
    %    write_value(Path, "=./data/J1", {7, 16}, []),
    %    write_value(Path, "=./data/K1", {7, 17}, []),
    %    write_value(Path, "=./data/L1", {7, 18}, []),
    %    colour(Path, {7, 15}, "orange"),
    %    colour(Path, {7, 16}, "yellow"),
    %    colour(Path, {7, 17}, "yellow"),
    %    colour(Path, {7, 18}, "orange"),

    %    write_value(Path, "=./data/M1", {8, 15}, []),
    %    write_value(Path, "=./data/N1", {8, 16}, []),
    %    write_value(Path, "=./data/O1", {8, 17}, []),
    %    write_value(Path, "=./data/P1", {8, 18}, []),
    %    colour(Path, {8, 15}, "orange"),
    %    colour(Path, {8, 16}, "orange"),
    %    colour(Path, {8, 17}, "orange"),
    %    colour(Path, {8, 18}, "orange"),

    %    make_thin(Path, 9),

    %    write_value(Path, "Range "++FunName++" (right)", {10, 14}, 
    %    [bold, underline, center]),
    %    write_value(Path, "A", {10, 15}, []),
    %    write_value(Path, "B", {10, 16}, []),
    %    write_value(Path, "C", {10, 17}, []),
    %    write_value(Path, "D", {10, 18}, []),
    %    colour(Path, {10, 15}, "orange"),
    %    colour(Path, {10, 16}, "orange"),
    %    colour(Path, {10, 17}, "orange"),
    %    colour(Path, {10, 18}, "orange"),

    %    write_value(Path, "E", {11, 15}, []),
    %    write_value(Path, "F", {11, 16}, []),
    %    write_value(Path, "G", {11, 17}, []),
    %    write_value(Path, "H", {11, 18}, []),
    %    colour(Path, {11, 15}, "orange"),
    %    colour(Path, {11, 16}, "yellow"),
    %    colour(Path, {11, 17}, "yellow"),
    %    colour(Path, {11, 18}, "orange"),

    %    write_value(Path, "I", {12, 15}, []),
    %    write_value(Path, "J", {12, 16}, []),
    %    write_value(Path, "K", {12, 17}, []),
    %    write_value(Path, "L", {12, 18}, []),
    %    colour(Path, {12, 15}, "orange"),
    %    colour(Path, {12, 16}, "yellow"),
    %    colour(Path, {12, 17}, "yellow"),
    %    colour(Path, {12, 18}, "orange"),

    %    write_value(Path, "M", {13, 15}, []),
    %    write_value(Path, "N", {13, 16}, []),
    %    write_value(Path, "O", {13, 17}, []),
    %    write_value(Path, "P", {13, 18}, []),
    %    colour(Path, {13, 15}, "orange"),
    %    colour(Path, {13, 16}, "orange"),
    %    colour(Path, {13, 17}, "orange"),
    %    colour(Path, {13, 18}, "orange"),

    %    make_thick(Path, 10),

    % Now do the inserts and deletes

    %    io:format("'bout to wait...~n"),
    %    test_util:wait(100),
    %    io:format("'done waitin...~n"),
    write_value(Path, "after: "++FunName, {3, 1}, [bold, underline, center]),
    colour(Path, {3, 1}, "red"),

    %    io:format("At the end...~n"),

    %    insert_delete(FunName, Path, {cell, {1, 5}}, vertical),
    % insert_delete(FunName, Path, {cell, {1, 5}}, horizontal),
    % insert_delete(FunName, Path, {row, {12, 12}}),
    % insert_delete(FunName, Path, {column, {3, 3}}),
    % insert_delete(FunName, Path, {range, {6, 16, 7, 18}}, vertical),
    % insert_delete(FunName, Path, {range, {11, 16, 13, 18}}, vertical),

    ok.

%% @hidden
copy_DEBUG3(FunName) ->

    Path = [FunName, "for_ranges"],

    clear_cells_DEBUG(Path),

    test_util:wait(100),

    write_value(Path, FunName++" - ranges", {1, 1}, [bold, underline]),

    make_high(Path, 1),

    write_value(Path, "From Range", {1, 2}, [bold]),
    write_value(Path, "a", {1, 3}, [{colour, "yellow"}]),
    write_value(Path, "b", {1, 4}, [{colour, "yellow"}]),
    write_value(Path, "c", {1, 5}, [{colour, "yellow"}]),
    write_value(Path, "d", {2, 3}, [{colour, "yellow"}]),
    write_value(Path, "e", {2, 4}, [{colour, "yellow"}]),
    write_value(Path, "f", {2, 5}, [{colour, "yellow"}]),
    write_value(Path, "g", {3, 3}, [{colour, "yellow"}]),
    write_value(Path, "h", {3, 4}, [{colour, "yellow"}]),
    write_value(Path, "i", {3, 5}, [{colour, "yellow"}]),

    write_value(Path, "To Range (the same size)", {1, 7}, [bold]),
    make_high(Path, 7),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {1, 3, 3, 5}},
                                     {range, {1, 8, 3, 10}}),

    write_value(Path, "From", {5, 2}, [bold]),

    write_value(Path, "aa", {5, 3}, [{colour, "yellow"}]),
    write_value(Path, "bb", {5, 4}, [{colour, "yellow"}]),
    write_value(Path, "cc", {5, 5}, [{colour, "yellow"}]),
    write_value(Path, "dd", {6, 3}, [{colour, "yellow"}]),
    write_value(Path, "ee", {6, 4}, [{colour, "yellow"}]),
    write_value(Path, "ff", {6, 5}, [{colour, "yellow"}]),
    write_value(Path, "gg", {7, 3}, [{colour, "yellow"}]),
    write_value(Path, "hh", {7, 4}, [{colour, "yellow"}]),
    write_value(Path, "ii", {7, 5}, [{colour, "yellow"}]),

    write_value(Path, "To Range (smaller)", {5, 7}, [bold]),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {5, 3, 7, 5}},
                                     {range, {5, 8, 6, 9}}),

    write_value(Path, "From", {9, 2}, [bold]),

    write_value(Path, "aaa", {9, 3}, [{colour, "yellow"}]),
    write_value(Path, "bbb", {9, 4}, [{colour, "yellow"}]),
    write_value(Path, "ccc", {9, 5}, [{colour, "yellow"}]),
    write_value(Path, "ddd", {10, 3}, [{colour, "yellow"}]),
    write_value(Path, "eee", {10, 4}, [{colour, "yellow"}]),
    write_value(Path, "fff", {10, 5}, [{colour, "yellow"}]),
    write_value(Path, "ggg", {11, 3}, [{colour, "yellow"}]),
    write_value(Path, "hhh", {11, 4}, [{colour, "yellow"}]),
    write_value(Path, "iii", {11, 5}, [{colour, "yellow"}]),

    write_value(Path, "To Cell", {9, 7}, [bold]),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {9, 3, 12, 5}},
                                     {cell, {9, 8}}),

    write_value(Path, "From Range", {1, 12}, [bold]),
    write_value(Path, "aaaa", {1, 13}, [{colour, "yellow"}]),
    write_value(Path, "bbbb", {1, 14}, [{colour, "yellow"}]),
    write_value(Path, "cccc", {2, 13}, [{colour, "yellow"}]),
    write_value(Path, "dddd", {2, 14}, [{colour, "yellow"}]),

    write_value(Path, "To Vertical Tiles", {1, 16}, [bold]),
    make_high(Path, 16),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {1,13, 2, 14}},
                                     {range, {1, 17, 2, 22}}),

    write_value(Path, "From Range", {4, 12}, [bold]),
    write_value(Path, "A", {4, 13}, [{colour, "yellow"}]),
    write_value(Path, "B", {4, 14}, [{colour, "yellow"}]),
    write_value(Path, "C", {5, 13}, [{colour, "yellow"}]),
    write_value(Path, "D", {5, 14}, [{colour, "yellow"}]),

    write_value(Path, "To Horizontal Tiles", {4, 16}, [bold]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {4, 13, 5, 14}},
                                     {range, {4, 17, 7, 18}}),

    write_value(Path, "From Range", {9, 12}, [bold]),
    write_value(Path, "AA", {9, 13}, [{colour, "yellow"}]),
    write_value(Path, "BB", {9, 14}, [{colour, "yellow"}]),
    write_value(Path, "CC", {10, 13}, [{colour, "yellow"}]),
    write_value(Path, "DD", {10, 14}, [{colour, "yellow"}]),

    write_value(Path, "To Tiles", {9, 16}, [bold]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {9, 13, 10, 14}},
                                     {range, {9, 17, 12, 20}}),

    write_value(Path, "From Range", {14, 12}, [bold]),
    write_value(Path, "AAA", {14, 13}, [{colour, "yellow"}]),
    write_value(Path, "BBB", {14, 14}, [{colour, "yellow"}]),
    write_value(Path, "CCC", {15, 13}, [{colour, "yellow"}]),
    write_value(Path, "DDD", {15, 14}, [{colour, "yellow"}]),

    write_value(Path, "Non-Tiling Range", {14, 16}, [bold]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {14, 13, 15, 14}},
                                     {range, {14, 17, 16, 16}}).

%% @hidden
copy_DEBUG2(FunName) ->

    Path = [FunName],

    clear_cells_DEBUG(Path),

    test_util:wait(100),

    write_value(Path, FunName++" - cell to cell", {1, 1}, [bold, underline]),

    % cell to cell drop down
    write_value(Path, "integer below", {1, 2}, [bold]),
    write_value(Path, "1", {1, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 3}},
                                     {cell, {1, 4}}),
    colour(Path, {1, 3}, "cyan"),

    write_value(Path, "float below", {1, 5}, [bold]),
    write_value(Path, "1.1", {1, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 6}},
                                     {cell, {1, 7}}),
    colour(Path, {1, 6}, "cyan"),

    write_value(Path, "string below", {1, 8}, [bold]),
    write_value(Path, "hey!", {1, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 9}},
                                     {cell, {1, 10}}),
    colour(Path, {1, 9}, "cyan"),

    write_value(Path, "date below", {1, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {1, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 12}},
                                     {cell, {1, 13}}),
    colour(Path, {1, 12}, "cyan"),

    write_value(Path, "boolean below", {1, 14}, [bold]),
    write_value(Path, "true", {1, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 15}},
                                     {cell, {1, 16}}),
    colour(Path, {1, 15}, "cyan"),

    make_thick(Path, 1),
    make_thick(Path, 2),
    make_high(Path, 1),

    % cell to cell across
    write_value(Path, "integer beside", {2, 2}, [bold]),
    write_value(Path, "1", {2, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 3}},
                                     {cell, {3, 3}}),
    colour(Path, {2, 3}, "cyan"),

    write_value(Path, "float beside", {2, 5}, [bold]),
    write_value(Path, "1.1", {2, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 6}},
                                     {cell, {3, 6}}),
    colour(Path, {2, 6}, "cyan"),

    write_value(Path, "string beside", {2, 8}, [bold]),
    write_value(Path, "hey!", {2, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 9}},
                                     {cell, {3, 9}}),
    colour(Path, {2, 9}, "cyan"),

    write_value(Path, "date beside", {2, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {2, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 12}},
                                     {cell, {3, 12}}),
    colour(Path, {2, 12}, "cyan"),

    write_value(Path, "boolean beside", {2, 14}, [bold]),
    write_value(Path, "true", {2, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 15}},
                                     {cell, {3, 15}}),
    colour(Path, {2, 15}, "cyan"),

    make_thin(Path, 4),

    write_value(Path, FunName++" - cell to down 'thin' range", {5, 1},
                [bold, underline]),

    % cell to range down
    write_value(Path, "integer below", {5, 2}, [bold]),
    write_value(Path, "1", {5, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {5, 3}},
                                     {range, {5, 4, 5, 5}}),
    colour(Path, {5, 3}, "cyan"),

    write_value(Path, "float below", {6, 5}, [bold]),
    write_value(Path, "1.1", {6, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {6, 6}},
                                     {range, {6, 7, 6, 8}}),
    colour(Path, {6, 6}, "cyan"),

    write_value(Path, "string below", {5, 8}, [bold]),
    write_value(Path, "hey!", {5, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {5, 9}},
                                     {range, {5, 10, 5, 11}}),
    colour(Path, {5, 9}, "cyan"),

    write_value(Path, "date below", {6, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {6, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {6, 12}},
                                     {range, {6, 13, 6, 14}}),
    colour(Path, {6, 12}, "cyan"),

    write_value(Path, "boolean below", {5, 14}, [bold]),
    write_value(Path, "true", {5, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {5, 15}},
                                     {range, {5, 16, 5, 17}}),
    colour(Path, {5, 15}, "cyan"),

    write_value(Path, FunName++" - cell to across 'thin' range", {7, 1},
                [bold, underline]),

    make_thick(Path, 5),
    make_thick(Path, 6),

    % cell to range down
    write_value(Path, "integer beside", {7, 2}, [bold]),
    write_value(Path, "1", {7, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 3}},
                                     {range, {7, 4, 8, 4}}),
    colour(Path, {7, 3}, "cyan"),

    write_value(Path, "float beside", {7, 5}, [bold]),
    write_value(Path, "1.1", {7, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 6}},
                                     {range, {7, 7, 8, 7}}),
    colour(Path, {7, 6}, "cyan"),

    write_value(Path, "string beside", {7, 8}, [bold]),
    write_value(Path, "hey!", {7, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 9}},
                                     {range, {7, 10, 8, 10}}),
    colour(Path, {7, 9}, "cyan"),

    write_value(Path, "date beside", {7, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {7, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 12}},
                                     {range, {7, 13, 8, 13}}),
    colour(Path, {7, 12}, "cyan"),

    write_value(Path, "boolean beside", {7, 14}, [bold]),
    write_value(Path, "true", {7, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 15}},
                                     {range, {7, 16,  8, 16}}),
    colour(Path, {7, 15}, "cyan"),

    make_thick(Path, 7),
    make_thick(Path, 8),

    make_thin(Path, 9),

    % cell to 'thick' ranges don't increment even if they are drag'n'drop
    write_value(Path, FunName++" - cell to 'thick' range", {10,1},
                [bold, underline]),

    write_value(Path, "integer", {10,2}, [bold]),
    write_value(Path, "1", {10,3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {10,3}},
                                     {range, {10,4, 11, 10}}),
    colour(Path, {10,3}, "cyan"),

    % same as above but arsey backwards range
    write_value(Path, "testing inverted range", {10,11}, [bold]),
    write_value(Path, "1", {10,12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {10,12}},
                                     {range, {10, 20, 11, 13}}),
    colour(Path, {10,3}, "cyan"),

    make_thick(Path, 10),

    make_thin(Path, 12),

    % set up formula data
    write_value(Path, "data for formula", {13,2}, [bold]),
    colour(Path, {13, 2}, "yellow"),
    write_value(Path, "1", {13, 3}, []),
    write_value(Path, "22", {13, 4}, []),
    write_value(Path, "333", {13, 5}, []),
    write_value(Path, "4444", {13, 6}, []),
    write_value(Path, "5555", {13, 7}, []),
    write_value(Path, "11111", {14, 3}, []),
    write_value(Path, "222222", {14, 4}, []),
    write_value(Path, "333333", {14, 5}, []),
    write_value(Path, "4444444", {14, 6}, []),
    write_value(Path, "55555555", {14, 7}, []),
    colour(Path, {13, 3}, "orange"),
    colour(Path, {13, 4}, "orange"),
    colour(Path, {13, 5}, "orange"),
    colour(Path, {13, 6}, "orange"),
    colour(Path, {13, 7}, "orange"),
    colour(Path, {14, 3}, "orange"),
    colour(Path, {14, 4}, "orange"),
    colour(Path, {14, 5}, "orange"),
    colour(Path, {14, 6}, "orange"),
    colour(Path, {14, 7}, "orange"),

    make_thick(Path, 13),

    make_thin(Path, 15),

    % some formula stuff
    write_value(Path, "formula below", {16, 2}, [bold]),
    write_value(Path, "=m3+n3", {16, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 3}},
                                     {range, {16, 4, 17, 6}}),
    colour(Path, {16, 3}, "cyan"),

    write_value(Path, "fix col formula below", {16, 7}, [bold]),
    write_value(Path, "=$m3+n3", {16, 8}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 8}},
                                     {range, {16, 9, 17, 11}}),
    colour(Path, {16, 8}, "cyan"),

    write_value(Path, "fix row formula below", {16, 12}, [bold]),
    write_value(Path, "=m$3+n3", {16, 13}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 13}},
                                     {range, {16, 14, 17, 16}}),
    colour(Path, {16, 13}, "cyan"),

    write_value(Path, "fix row and col formula below", {16,17}, [bold]),
    write_value(Path, "=$m$3+n3", {16, 18}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 18}},
                                     {range, {16, 19, 17, 21}}),
    colour(Path, {16, 18}, "cyan"),

    make_thick(Path, 16).

%% @hidden
hn_DEBUG() ->
    Path = ["hypernumbers"],
    Site1 = "http://il_ballo.dev:9000",
    Path1 = ["data"],
    clear_cells_DEBUG(Path),
    clear_cells_DEBUG(Site1, Path1),
    write_data(Site1, Path1),

    write_value(Path,"=hn(\"http://il_ballo.dev:9000/data/A1?hypernumber\")",
                {1, 1}, []).

%% @hidden
get_tiles_DEBUG() ->
    F1 = #refX{obj = {range, {1, 1, 2, 2}}},
    F2 = #refX{obj = {range, {1, 1, 3, 3}}},
    T1 = #refX{obj = {range, {1, 1, 3, 3}}},
    T2 = #refX{obj = {range, {1, 1, 6, 6}}},
    T3 = #refX{obj = {range, {1, 1, 3, 6}}},
    T4 = #refX{obj = {range, {1, 1, 6, 3}}},

    T5 = #refX{obj = {range, {1, 1, 1, 3}}},
    T6 = #refX{obj = {range, {1, 3, 3, 3}}},
    T7 = #refX{obj = {range, {2, 3, 4, 7}}},
    T8 = #refX{obj = {range, {1, 1, 1, 1}}},

    get_tiles(F1, T8),
    io:format("**********************************~n"),
    get_tiles(F2, T8),
    io:format("**********************************~n"),
    get_tiles(F2, T1),
    io:format("**********************************~n"),   
    get_tiles(F2, T2),
    io:format("**********************************~n"),   
    get_tiles(F2, T3),
    io:format("**********************************~n"),   
    get_tiles(F2, T4),  
    io:format("**********************************~n"),   
    get_tiles(F2, T7).    

%% @hidden
read_styles_DEBUG() ->
    Site = "http://blah.com",
    Path = ["some", "path"],
    write_value(Site, Path, "1", {1, 1}, []),
    write_value(Site, Path, "bob", {1, 2}, []),
    write_value(Site, Path, "true", {2, 1}, []),
    write_value(Site, Path, "1/1/09", {2, 2}, []),

    RefX1 = #refX{site = Site, path = Path, obj = {cell, {1, 1}}},
    RefX2 = #refX{site = Site, path = Path, obj = {cell, {1, 2}}},
    RefX3 = #refX{site = Site, path = Path, obj = {cell, {2, 1}}},
    RefX4 = #refX{site = Site, path = Path, obj = {cell, {2, 2}}},

    RefX5 = #refX{site = Site, path = Path, obj = {column, {2, 2}}},
    RefX6 = #refX{site = Site, path = Path, obj = {row, {2, 2}}},
    RefX7 = #refX{site = Site, path = Path, obj = {range,{1, 1, 2, 2}}},
    RefX8 = #refX{site = Site, path = Path, obj = {page, "/"}},

    List = [RefX1, RefX2, RefX3, RefX4, RefX5, RefX6, RefX7, RefX8],
    [io:format("read_styles returns ~p~n", [read_styles_DEBUG2(X)]) || X <- List].

%% @hidden
read_styles_DEBUG2(X) ->
    io:format("reading styles for ~p~n", [X]),
    Fun = fun() ->
                  hn_db_wu:read_styles(X)
          end,
    mnesia:activity(transaction, Fun).

insert_delete(Fun, Path, Target) ->
    Site = "http://127.0.0.1:9000",
    Ref = #refX{site = Site, path = Path, obj = Target},
    erlang:apply(?MODULE, list_to_atom(Fun), [Ref]).

insert_delete(Fun, Path, Target, Type) ->
    Site = "http://127.0.0.1:9000",
    Ref = #refX{site = Site, path = Path, obj= Target},
    erlang:apply(?MODULE, list_to_atom(Fun), [Ref, Type]).

cut_n_drag_n_copy_n_drop_n_paste(Fun, Path, From, To) ->
    Site = "http://127.0.0.1:9000",
    From1 = #refX{site =  Site, path = Path, obj = From},
    To1   = #refX{site =  Site, path = Path, obj = To},
    erlang:apply(?MODULE, list_to_atom(Fun), [From1, To1]).

%% choose the site to write to
write_value(Site, Path, Value, {X, Y}, Attributes) ->
    RefX = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    write_attributes(RefX, [{formula, Value}]),
    write_attr_DEBUG(RefX, Attributes).

%% just write to the default
write_value(Path, Value, {X, Y}, Attributes) ->
    Site = "http://127.0.0.1:9000",
    write_value(Site, Path, Value, {X, Y}, Attributes).

write_attr_DEBUG(_RefX, []) -> ok;
write_attr_DEBUG(RefX, [Attr | T]) ->
    Attr2 = case Attr of
                bold              -> {'font-weight', "bold"};
                underline         -> {'text-decoration', "underline"};
                center            -> {'font-align', "center"};
                {colour, Colour}  -> {'background-color', Colour};
                thin              -> {width, "30"};
                thick             -> {width, "200"}
            end,
    write_attributes(RefX, [Attr2]),
    write_attr_DEBUG(RefX, T).

colour(Path, {X, Y}, Colour) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    Val = {'background-color', Colour},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

make_high(Path, X) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {row, {X, X}}},
    Val = {height, "30"},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

make_thin(Path, X) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {column, {X, X}}},
    Val = {width, "30"},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

make_thick(Path, X) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {column, {X, X}}},
    Val = {width, "200"},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

write_data(Path) ->
    Site = "http://127.0.0.1:9000",
    write_data(Site, Path).

write_data(Site, Path) ->
    clear_cells_DEBUG(Site, Path),
    % writes out sample data for hyperlinks
    write_value(Site, Path, "1", {1, 1}, []),
    write_value(Site, Path, "2", {2, 1}, []),
    write_value(Site, Path, "3", {3, 1}, []),
    write_value(Site, Path, "4", {4, 1}, []),
    write_value(Site, Path, "5", {5, 1}, []),
    write_value(Site, Path, "6", {6, 1}, []),
    write_value(Site, Path, "7", {7, 1}, []),
    write_value(Site, Path, "8", {8, 1}, []),
    write_value(Site, Path, "9", {9, 1}, []).

clear_cells(Site, Path, {X, Y}) ->
    Target = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    clear(Target, all).    
