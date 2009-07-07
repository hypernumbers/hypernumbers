%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This is a util function for hn_db_api containing only functions
%%%            that MUST be called from within an mnesia transactions - these
%%%            functions can be considered <em>work units</em> from which
%%%            api transactions can be constructed (hence the name).
%%%            
%%% The module {@link hn_db_api} is the wrapper for calls to this
%%% module.
%%% 
%%% This module <em>PRESUMES THAT PERMISSIONS ARE ALL IN ORDER</em> - 
%%% for instance no function in this module will check if a biccie
%%% is valid - that <em>MUST BE DONE</em> in the api level functions.
%%% 
%%% This module generates two sorts of side-effects:
%%% * changes to the database
%%% * notifications to the fron./t-end
%%% 
%%% The changes to the database are done under mnesia transactions management
%%% (as you would expect). It is necessary for the notifications to the 
%%% front=end also to be under transaction management. This is achieved by 
%%% using the process dictionary in a specific way.
%%% 
%%% To notify the front-end under transaction management, a function in 
%%% this module will call the function 'tell_front_end' with the appropriate
%%% change - the transaction management is applied in the module 
%%% {@link hn_db_api} and is documented there.
%%% 
%%% <h3>Functional Categories</h3>
%%% 
%%% These functions fall into 3 types:
%%% <ul>
%%% <li>structural queries that returns #refs{}</li>
%%% <li>cell queries that operate on cells</li>
%%% <li>dirty management</li>
%%% </ul>
%%%  
%%% <h4>Structural Queries</h4>
%%% 
%%% Structural queries have the following characteristics:
%%% <ul>
%%% <li>they are all 'read' queries - they do not impact the
%%% structure of the database</li>
%%% <li>they all have the word ref in their function name</li>
%%% </ul>
%%% 
%%% The all return lists of #refX{}'s
%%% 
%%% <h4>Cell Queries</h4>
%%% 
%%% Cell queries come in 4 distinct flavours:
%%% <ul>
%%% <li>create/write</li>
%%% <li>read</li>
%%% <li>update</li>
%%% <li>delete</li>
%%% </ul>
%%% 
%%% The reads return lists of tuples containing #refX{} and 
%%% {Key, Value} pairs.
%%% 
%%% The others return ok
%%% 
%%% <h4>Dirty Management</h4>
%%% 
%%% A thing can be set dirty by call a function with a name like 
%%% <code>mark something dirty</code> and when a dirty thing has been
%%% processed it is cleared with a function like 
%%% <code>clear dirty something</code>.
%%% 
%%% (NB: not all functions with the word 'clear' in them pertain to dirty 
%%% management)
%%% 
%%% <h2>Under The Hood</h2>
%%% 
%%% This section looks at how all this stuff is implemented 
%%% 'under the hood'. The key reason for this is that 
%%% otherwise it is hard to understand how difficult things
%%% like inserting rows and columns are implemented.
%%% 
%%% This will look at two things:
%%% <ul>
%%% <li>attributes</li>
%%% <li>tables</li>
%%% </ul>
%%% These will particularly look at how data that contains
%%% information about the relationship between cells is
%%% stored.
%%% 
%%% <h3>Attributes</h3>
%%% 
%%% There are a number of attributes about a cell that are of
%%% considerable interest:<p />
%%% <img src="./attributes.png" />
%%% 
%%% Information is entered into a cell by way of a 
%%% <code>formula</code>. That <code>formula</code> can be in
%%% a 'shared formula' context (which will result in it
%%% having <code>__area</code> and <code>__shared</code>
%%% attributes).
%%% 
%%% By default a cell has a <code>format</code> and a 
%%% <code>style</code> attributes. (There will also be
%%% <code>permissions</code> attributes later on...)
%%% 
%%% The <code>formula</code>, <code>format</code> and
%%% <code>style</code> attributes are all used to calculate
%%% the remaining attributes <code>value</code>, 
%%% <code>rawvalue</code>, <code>overwrite-color</code> 
%%% and <code>__recompile</code>.
%%% 
%%% (Attributes starting with '__' are private in that they
%%% are never exposed to the front-end - they are server-side
%%% only.)
%%% 
%%% They key point is that if a cell is moved, any formula
%%% of <i>a child</i> of that cell needs to be be rewritten,
%%% which means that the <code>__ast</code>, 
%%% <code>parents</code> and <code>__dependency-tree</code>
%%% attributes need to be rewritten.
%%% 
%%% Then <i>in turn</i> the <code>formula</code> attributes
%%% of all the <i>grand-children</i> of the original cell 
%%% need to rewrite and so on and so forth.
%%%
%%% <h3>Tables</h3>
%%%
%%% The tables that information is stored in is shown below:<p />
%%% <img src="./tables.png" />
%%%
%%% Each of the following tables will now be discussed in some detail:
%%% <ol>
%%% <li>item and local_objs and remote_objs (FIX ME)</li>
%%% <li>local_cell_link</li>
%%% <li>remote_cell_link</li>
%%% <li>incomingn_hn</li>
%%% <li>outgoing_hn</li>
%%% <li>dirty_cell</li>
%%% <li>dirty_notify_out</li>
%%% <li>dirty_notify_in</li>
%%% <li>dirty_inc_hn_create</li>
%%% <li>dirty_notify_back_in</li>
%%% <li>dirty_notify_back_out</li>
%%% </ol>
%%% 
%%% <h4>1 item</h4>
%%% 
%%% contains all the atributes of the cell plus attributes of columns, rows, pages
%%% etc, etc - including stuff not documented here like permissions
%%% 
%%% <h4>2 local_cell_link</h4>
%%% 
%%% contains parent-child pig's ear links of cells stored in item
%%% 
%%% <h4>3 remote_cell_link</h4>
%%% 
%%% contains parent-child links that connect cells stored in item 
%%% <i>for this site</i> to cells on other sites. Becaause this physical 
%%% server can support many sites that 'remote' cell may be on this 
%%% machine - as a consequence the links are tagged with incoming/outgoing 
%%% types
%%% 
%%% <h4>4 incoming_hn</h4>
%%% 
%%% there is an entry in this table for each remote cell that is referenced
%%% by a cell on this site. It holds the current value of that remote cell
%%% and all the connection information required to authenticate updates
%%% to that cell
%%% 
%%% <h4>5 outgoing_hn</h4>
%%% 
%%% there is an entry in this table for each cell that is referenced by a
%%% remote site. It holds the connection information required to successfully
%%% update the remote sites
%%% 
%%% <h4>6 dirty_cell</h4>
%%% 
%%% contains a reference to a cell whose parents (local or remote) are dirty
%%% 
%%% <h4>7 dirty_notify_out</h4>
%%% 
%%% contains a reference to every <code>outgoing_hn</code> whose value has 
%%% changed and the new value. This is necesseary because notifying the remote
%%% cell of changes is a asychronous affair and the 'parent cell' needs to be able
%%% be operated on (deleted, moved, updated, etc, etc) while the notification of
%%% remote servers is ongoing
%%% 
%%% <h4>8 dirty_notify_in</h4>
%%% 
%%% contains a list of <code>incomging_hn</code>'s whose value has
%%% been changed by a notify message. The dirty_srv uses this to identify cells 
%%% marked as dirty
%%% 
%%% <h4>9 dirty_inc_hn_create</h4>
%%% 
%%% when a new hypernumber is to be created a entry is made to this table and the
%%% dirty_srv sets up the hypernumber and triggers dirty_notify_back_in when it
%%% is complete
%%% 
%%% <h4>10 dirty_notify_back_in</h4>
%%% 
%%% certain actions on a child hypernumber need to be notified back to the
%%% parent, for instance:
%%% <ul>
%%% <li>formula using a hypernumber has been deleted</li>
%%% <li>a new formula using an existing hypernumber has been created (ie
%%% the remote cell has new parents)</li>
%%% <li>a child has been moved by an insert or delete command</li>
%%% </ul>
%%% 
%%% <h4>11 dirty_notify_back_out</h4>
%%% 
%%% when a notification back is received from the child server the change
%%% is written to this table to be implemented
%%% 
%%% <h3>Structure Of Dirty Tables</h3>
%%% 
%%% The dirty_cell table lives in a world of its own and is not shown below, 
%%% but the relationship of the rest of the  dirty tables to each 
%%% other is shown below:<p />
%%% <img src="./update_cycles.png" />
%%% 
%%% <h2>Corner Cases</h2>
%%% 
%%% This section will now describe what happens under a complete set of
%%% corner cases which are listed below:
%%% <ol>
%%% <li>delete a stand-alone value</li>
%%% <li>delete a value in a cell that is referenced by another
%%% local cell</li>
%%% <li>delete a value in a cell that is referenced by another
%%% remote cell (or cells)</li>
%%% <li>delete a value that references another local cell</li>
%%% <li>delete a value that references a remote cell</li>
%%% <li>create a new value</li>
%%% <li>create a new formula referencing local cells</li>
%%% <li>create a new formula referencing a new hypernumber 
%%% (a remote cell not referenced by any other cell on its site></li>
%%% <li>create a new formula referencing an existing hypernumber 
%%% (a remote cell already referenced by another cell on its site></li>
%%% <li>change a value when the cell is referenced by another
%%% local cell</li>
%%% <li>change a value when the cell is referenced by another
%%% remote cell (or cells) (it is a hypernumber)</li>
%%% <li>move a cell that onlyhas a value</li>
%%% <li>move a cell that references another local cell</li>
%%% <li>move a cell that is referenced by another local cell</li>
%%% <li>move a cell that is referenced by a remote cell</li>
%%% <li>move a cell that references a remote cell</li>
%%% <li>copy a cell from one place to another</li>
%%% <li>delete a shared formula</li>
%%% <li>create a shared formula with:
%%%   <ul>
%%%   <li>local parents</li>
%%%   <li>remote parents</li>
%%%   <li>local children</li>
%%%   <li>remote children</li>
%%%   </ul>
%%% </li>
%%% </ol>
%%%  
%%% The actual implementation of the 'high-level' function MUST handle
%%% all of these edge cases (as well as the composite cases (ie changing
%%% a cell that is referenced by a local cell, a remote cell and whose formula
%%% uses local and remote cells, etc, etc...).
%%%  
%%% In addition the high-level actions (insert a column, delete a column etc)
%%% should handle structual attributes like column widths and stuff correctly
%%%  
%%% <h4>1 Delete A Stand-Alone Value</h4>
%%% 
%%% If the cell is not shared (ie has an <code>__shared</code> attribute
%%% then the following attributes are deleted:
%%% <code><ul>
%%% <li>formula</li>
%%% <li>__ast</li>
%%% <li>value</li>
%%% <li>rawvalue</li>
%%% <li>__recompile</li>
%%% </ul></code>
%%% 
%%% <h4>2 Delete A Value In A Cell That Is Referenced By Another
%%% Local Cell</h4>
%%% 
%%% As per <i>Delete A Stand-Alone Value</i> except that a record is 
%%% written to <code>dirty_cell</code>for each local child of the cell.
%%% The dirty server then tells the dirty cells to recalculate themselves
%%% 
%%% <h4>3 Delete A Value In A Cell That Is Referenced By Another
%%% Remote Cell (Or Cells)</h4>
%%% 
%%% As per <i>Delete A Stand-Alone Value</i> except that a record is 
%%% written to <code>dirty_notify_out</code> referencing the original
%%% cell (and not the remote children). This triggers a hypernumbers
%%% notification message to the remote server.
%%% 
%%% The remote server gets the notification message and updates the table
%%% <code>incoming_hn</code>. It then writes a <code>dirty_notify_in</code>
%%% record. The dirty server uses this message to write a 
%%% <code>dirty_cell</code> message for each cell that uses the changed hypernumber
%%% 
%%% <h4>4 Delete A Value That References Another Local Cell</h4>
%%% 
%%% The cell is deleted as per <i>1 Delete A Stand-Alone Value</i> and then
%%% the relevant record in <code>local_cell_link</code> is deleted.
%%% 
%%% <h4>5 Delete A Value That References A Remote Cell</h4>
%%% 
%%% The cell is deleted as per <i>1 Delete A Stand-Alone Value</i> and then
%%% the relevant record in <code>remote_cell_link</code> is deleted and the
%%% appropriate message is written to <code>dirty_notify_back_in</code>.
%%% 
%%% The remote server gets the notify_back message and uses this to delete
%%% the record from its <code>remote_cell_link</code> table. If it is the last
%%% reference to the a particular entry in <code>outgoing_hn</code> table then
%%% that entry is also deleted.
%%% 
%%% <h4>6 Create A New Value</h4>
%%% 
%%% <ul>
%%% <li>a record is written to item</li>
%%% <li>if the cell has local children a record is written to 
%%% <code>dirty_cell</code> for each of them</li>
%%% <li>if the cell has a remote child a record is written to 
%%% <code>dirty_notify_out</code></li>
%%% </ul>
%%% 
%%% The dirty_srv gets notified of each write and instructs the dirty
%%% children to recalculate themselves.
%%% 
%%% <h4>7 Create A New Formula Referencing Local Cells</h4>
%%% 
%%% As per <i>Create A New Value</i> except a new <code>local_cell_link
%%% </code> record is also written.
%%% 
%%% <h4>8 Create A New Formula Referencing A New Hypernumber</h4>
%%%
%%% <ul>
%%% <li>a record is written to <code>item</code></li>
%%% <li>if the cell has a local child a record is written to 
%%% <code>dirty_cell</code></li>
%%% <li>if the cell has a remote child a record is written to 
%%% <code>dirty_notify_out</code></li>
%%% <li>a new <code>remote_cell_link</code> of type <code>incoming</code> 
%%% is written</li>
%%% <li>the formula looks up the value of the hypernumber - there isn't
%%% one so it gets the value 'blank' back and a 
%%% <code>dirty_inc_hn_create</code> record is written. When the 
%%% dirty server has got the remote hypernumber it will writes its 
%%% value to the table <code>incoming_hn</code> and create a record
%%% in <code>dirty_notify_in</code></li>
%%% </ul>
%%% 
%%% The dirty_srv gets notified of each write and instructs the dirty
%%% children to recalculate themselves.
%%% 
%%% <h4>9 Create A New Formula Referencing An Existing Hypernumber</h4>
%%% 
%%% <ul>
%%% <li>a record is written to <code>item</code></li>
%%% <li>if the cell has a local child a record is written to 
%%% <code>dirty_cell</code></li>
%%% <li>if the cell has a remote child a record is written to 
%%% <code>dirty_notify_out</code></li>
%%% <li>a new <code>remote_cell_link</code> of type <code>incoming</code> 
%%% is written</li>
%%% <li>the formula looks up the value of the hypernumber - gets it - writes
%%% a <code>dirty_notify_back_in</code> record to notify the remote site that
%%% a new cell is using a particular hypernumber</li>
%%% </ul>
%%% 
%%% <h4>10 Change A Value When The Cell Is Referenced By Another
%%% Local Cell</h4>
%%% 
%%% Same as <i>6 Create A New Value</i>
%%% 
%%% <h4>11 Change A Value When The Cell Is Referenced By Another
%%% Remote Cell (Or Cells)</h4>
%%%
%%% Same as <i>6 Create A New Value</i>
%%%  
%%% <h4>12 Move A Cell That Only Has A Value</h4>
%%% 
%%% Same as <i>1 Delete A Stand-Alone Value</i> followed by
%%% <i>6 Create A New Value</i>
%%% 
%%% <h4>13 Move A Cell That References Another Local Cell</h4>
%%% 
%%% Same as <i>2 Delete A Value In A Cell That Is Referenced By Another
%%% Local Cell</i> followed by <i>6 Create A New Value</i>
%%% 
%%% <h4>14 Move A Cell That Is Referenced By Another Local Cell</h4>
%%% 
%%% <ul>
%%% <li>all the attributes (normal and user-defined) as well as permissions
%%% are copied from the old position which is then deleted...</li>
%%% <li>all <code>local_cell_links</code> where the moving cell is the child are 
%%% rewritten</li>
%%% <li>any cell that references the original has the <code>formula</code> and 
%%% is then marked as dirty (forcing it it rewrite itself). <em>This could be 
%%% done better by some sort of 'mark __dependency-tree dirty' 
%%% algorithm...</em></li>
%%% </ul>
%%% 
%%% <h4>15 Move A Cell That Is Referenced By A Remote Cell</h4>
%%% 
%%% <ul>
%%% <li>all the attributes (normal and user-defined) as well as permissions
%%% are copied from the old position which is then deleted...</li>
%%% <li>all <code>remote_cell_links</code> where the moving cell is the child are 
%%% rewritten</li>
%%% <li>a message is written to <code>dirty_notify_out</code> stating that
%%% the child has moved. The remote server processes this message and writes a 
%%% <code>dirty_notify_in</code> record. On processing the 
%%% <code>dirty_notify_in</code> record the dirty cell rewrites the formula
%%% on all the children of the changed cell and rewrites them triggering an 
%%% update of the dependency trees of all their children. (see <i>14 Move A Cell 
%%% That Is Referenced By Another Local Cell</i> for a caveat on this algorithm!</li>
%%% </ul>
%%% 
%%% <h4>16 Move A Cell That References A Remote Cell</h4>
%%% 
%%% <ul>
%%% <li>all the attributes (normal and user-defined) as well as permissions
%%% are copied from the old position which is then deleted...</li>
%%% <li>all <code>remote_cell_links</code> where the moving cell is the child are 
%%% rewritten</li>
%%% <li>a message is written to <code>dirty_notify_back_in</code> table. When the
%%% dirty server processes this it sends a message to the parent, which write a
%%% record to the <code>dirty_notify_back_out</code> table. On processing this record 
%%% remote server edits its <code>remote_cell_link</code> table.</li>
%%% </ul>
%%%
%%% <h4>17 Copy A Cell From One Place To Another</h4>
%%%  
%%% The attributes of the old cell are read and possibly the formula is rewritten
%%% (if it is a drag and drop or copy and paste and stuff) and the new formula
%%% is written to the new cell. This is just like a normal cell write.
%%%  
%%% <h4>18 Delete A Shared Formula</h4>
%%% 
%%% A shared formula delete is like a delete of all cells in a shared formula and
%%% is treated the same way.
%%% 
%%% <h4>19 Create A Shared Formula With All The Trimings</h4>
%%% 
%%% A shared formula create is the same a the creation of all the cells in the
%%% share formula (<em>with some malarky about array values that I don't understand 
%%% yet!</em>)
%%% 
%%% @TODO we need to add 'tell_front_end' messages for when we do stuff
%%%       like add new children to cells /delete existing children from cells 
%%%       so that the front-ends can update themselves...
%%%       parents/childre
%%% @TODO we use atoms for keys in {key, value} pairs of attributes
%%%       which is then used in atom_to_list for checking if they are private.
%%%       This is a memory leak! See also hn_yaws.erl
%%% @TODO there is the port bodge function - need to handle port correctly
%%% @TODO when a new style is written for a page it should notify the
%%%       viewing pages to update themselves or the style stuff won't work...
%%% @TODO the registration of a new hypernumber is not robust (what if the remote
%%%       server is not available at registration time? there is no retry 
%%%       function, etc etc)
%%% @TODO the whole 'dirty names' stuff needs to be added to the cycles described 
%%%       above so that when the value of a name changes the various functions 
%%%       using it recalculcate
%%% @TODO understand the whole shared formula stuff...
%%% @TODO there is no effective page versioning which is critical...
%%% @TODO read_remote_children/read_remote_parents both take either a 
%%%       <code>#refX{}</code> or a list of <code>#refX{}</code>'s
%%%       should probably extend this metaphor...
%%% @TODO the page version/sync code for page updates is a bit broken...
%%%       when we insert/delete on a page that has hypernumber children
%%%       on multiple pages on another site we only send ONE message to
%%%       that site with a child page reference. So only the page version
%%%       number of ONE of the child pages is checked before the update is
%%%       made - may need to rewrite versioning...
%%% @TODO the formula rewriting for insert/delete is icredibly inefficient
%%%       would be fixed by moving to using ranges in the local_link table
%%% @TODO REWRITE THE DOCUMENTATION TO TAKE INTO ACCOUNT ALL THE CHANGES!
%%% @TODO Remove the catch()'s around the superlexer for transformed formulae
%%%       only in there to provide a logging framework for diagnosis...
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_wu).

%% Cell Query Exports
-export([
         write_attr/2,       % tested
         read_cells/2,       % tested
         read_cells_raw/2,
         read_attrs/2,       % tested
         read_attrs/3,       % tested
         read_inherited/3,
         read_inherited_list/2,
         read_styles/1,
         read_incoming_hn/2,
         read_dirty_cell/2,
         read_all_dirty_cells/1,
         read_whole_page/1,
         find_incoming_hn/2,
         read_outgoing_hns/2,
         clear_cells/1,
         clear_cells/2,
         delete_page/1,
         delete_cells/1,
         delete_attrs/2,
         clear_dirty/2,
         clear_dirty_cell/2,
         delete_dirty_cells/2,
         shift_cells/4,
         shift_row_objs/2,
         shift_col_objs/2,
         delete_row_objs/1,
         delete_col_objs/1,
         shift_children/3,
         shift_remote_links/4,
         shift_inc_hns/2,
         copy_cell/3,
         copy_attrs/3,
         copy_style/2,
         get_cells/1,
         mark_cells_dirty/1,
         mark_dirty/2,
         mark_notify_out_dirty/2,            
         mark_notify_out_dirty/3,      
         update_inc_hn/5,
         does_remote_link_exist/3,
         write_remote_link/3,
         register_out_hn/4,
         unregister_out_hn/2,
         verify_biccie_in/3,
         verify_biccie_out/3,
         incr_remote_page_vsn/3,
         get_new_local_page_vsn/2,
         read_page_vsn/2,
         initialise_remote_page_vsn/3, 
         read_page_structure/1,
         get_cell_for_muin/1,
         get_local_item_index/1
        ]).

%% Database transformation functions
-export([
         trans/2,
         split_trans/1,
         trans_back/1
         ]).

%% Structural Query Exports
-export([
         get_last_refs/1
        ]).

%% These functions are exposed for the dirty_srv to use
-export([
         read_local_parents/1,
         read_local_parents_idx/2,
         read_local_children/1,
         read_remote_parents/2,
         read_remote_children/2
        ]).

-export([
         write_style_IMPORT/2
        ]).

-export([
         deref_overlap_TEST/0
        ]).

-define(to_xml_str, simplexml:to_xml_string).
-define(to_refX, hn_util:refX_from_index).

%% bit of tidying up for later on

-define(hn, {name, "HN"}).
-define(bra, {'('}).
-define(AND, andalso).
-define(OR, orelse).
-define(rootcellref, {cellref, _, _, "/", R}).
-define(cellref, {cellref, _, _, _, R}).
-define(cellref2, {cellref, _, _, _, R2}).
-define(rangeref, {rangeref, _, _, _, __, _, _, R}).
-define(namedexpr, {namedexpr, P, N}).

-define(lt, list_to_tuple).
-define(lf, lists:flatten).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Exported functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec delete_dirty_cells(Site, List) -> ok
%% @doc this function takes a list  of dirty cells and 
%% rewrites all of them to be deleted
delete_dirty_cells(Site, List) when is_list(List) ->
    [ok = delete_dirty_cell(trans(Site, dirty_cell), X) || X <- List],
    ok.

delete_dirty_cell(Table, Cell) when is_record(Cell, dirty_cell) ->
    mnesia:delete_object(Table, Cell, write);
delete_dirty_cell(Table, Cell) when is_record(Cell, refX) ->
    mnesia:delete(Table, get_local_item_index(Cell), write);
delete_dirty_cell(Table, Id) when is_list(Id) ->
    mnesia:delete(Table, Id, write).

-spec get_cell_for_muin(#refX{}) -> {any(), any(), any(), any()}.
%% @doc this function is called by muin during recalculation and should
%%      not be used for any other purpose
get_cell_for_muin(#refX{obj = {cell, {XX, YY}}} = RefX) ->
    #refX{site = Site, path = Path} = RefX,
    Value = case hn_db_wu:read_attrs(RefX, ["rawvalue"], read) of
                []            -> [];
                [{_, {_, V}}] -> V
            end, 

    DTree = case hn_db_wu:read_attrs(RefX, ["__dependency-tree"], read) of
                [{_, {_, Tree}}] -> Tree;
                []                     -> []
            end,

    Val = case Value of
              []                 -> blank;
              {datetime, _, [N]} -> muin_date:from_gregorian_seconds(N);
              Else               -> Else %% Strip other type tags.
          end,
    
    Dep = DTree ++ [{"local", get_local_item_index(RefX)}],
    {Val, Dep, [], [{"local", {Site, Path, XX, YY}}]}.

%% @hidden
%% @doc write_style_IMPORT is a wrapper for the internal function write_style
%% which should never be used except in file import
write_style_IMPORT(RefX, Style)
  when is_record(RefX, refX), is_record(Style, magic_style) ->
    NewIndex = write_style(RefX, Style),
    write_attr3(RefX, {"style", NewIndex}),
    ok.

%% @spec read_all_dirty_cells(Site) -> List
%% @doc reads the complete list of dirty cells
read_all_dirty_cells(Site) ->
    Table = trans(Site, dirty_cell),
    mnesia:match_object(Table, #dirty_cell{_ = '_'}, write).
    
-spec read_dirty_cell(any(), any()) -> #refX{}.
%% @doc reads a dirty_cell based on its timestamp
read_dirty_cell(Site, #dirty_cell{idx=IdX}) ->
    local_idx_to_refX(Site, IdX).
    
%% @spec shift_remote_links(Type1, OldRef::#refX{}, 
%% NewRef::#refX{}, Type1)  -> ok
%% Type1 = [parent | child]
%% Type2 = [incoming | outgoing]
%% @doc shifts remote links from the Old Reference to the New Reference. 
%% Both References must be a cell reference.
shift_remote_links(Type1, From, To, Type2)
  when is_record(From, refX), is_record(To, refX),
       ((Type1 == parent) orelse (Type1 == child)),
       ((Type2 == incoming) orelse (Type2 == outgoing)) ->
    #refX{site = Site} = From,
    Head = ms_util:make_ms(remote_cell_link, [{Type1, From}, {type, Type2}]),
    Match = {Head, [], ['$_']},
    Recs = mnesia:select(trans(Site, remote_cell_link), [Match], write),
    LinkedCells = Recs,
    shift_remote_links2(Site, LinkedCells, To).

%% @spec shift_inc_hns(Refs, NewParent::#refX{}) -> ok
%% Regs = [#refX{}] | #refX{}
%% @doc shifts an incoming hypernumber to the New Parent.
%% NewParent must be a reference to a cell.
shift_inc_hns(List, NewParent)
  when is_list(List), is_record(NewParent, refX) ->
    [ok = shift_inc_hns(X, NewParent) || X <- List],
    ok;
shift_inc_hns(#incoming_hn{site_and_parent = SP} = Inc_Hn, NewParent)
  when is_record(Inc_Hn, incoming_hn), is_record(NewParent, refX) ->
    {Site, _OldP} = SP,
    NewRec = Inc_Hn#incoming_hn{site_and_parent = {Site, NewParent}},
    ok = mnesia:delete_object(trans(Site, incoming_hn), Inc_Hn),
    ok = mnesia:write(trans(Site, incoming_hn), NewRec, write).

%% @spec shift_children(Children, OldParent::#refX{}, NewParent::#refX{}) -> ok
%% Children = [ [#refX{}] | #refX{} ]
%% @doc shift_children is called when a message comes in from a remote parent
%% saying that that parent has moved. The children of that remote hypernumber
%% then have their formulae rewritten to refer to the new location of the parent
%% OldParent and NewParent are the respective parents and must be cell references
%% Children can either be a cell reference or a list of cell references
shift_children(List, OldParent, NewParent)
  when is_list(List), is_record(OldParent, refX), is_record(NewParent, refX) ->
    [ok = shift_children(X, OldParent, NewParent) || X <- List],
    ok;
shift_children(Child, OldParent, NewParent)
  when is_record(Child, refX), is_record(OldParent, refX),
       is_record(NewParent, refX) ->
    OUrl = hn_util:refX_to_url(OldParent) ++ "?hypernumber",
    NUrl = hn_util:refX_to_url(NewParent) ++ "?hypernumber",
    % first read the child formula
    % but strip off the leading '=' sign
    [{Child, {"formula", [$= | Formula]}}] = read_attrs(Child, ["formula"], write),
    % just spoofing the lexing which is why we pass in the cell {1, 1}
    NewFormula = case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
                     {ok, Toks}    -> rewrite_hn_formula(Toks, OUrl, NUrl);
                     _Syntax_Error -> io:format("Not sure how you get an "++
                                                         "invalid formula is "++
                                                         "shift_children but "++
                                                         "you do~n-~p~n", [Formula]),
                                      % S2 = io_lib:format("~p", [Child]),
                                      % bits:log(S1 ++ " " ++ S2),
                                      Formula
                 end,
    % bits:log(io_lib:format("(2) Formula is ~p NewFormula is ~p~n",
    %                       [Formula, NewFormula])),
    write_attr(Child, {"formula", NewFormula}).

%% @spec initialise_remote_page_vsn(Site, Page::#refX{}, Version) -> ok
%% @doc intialises the page version for a 'newly discovered' remote page.
%% This function is only called the first time that a remote pages makes a
%% request and the function {@link read_page_vsn_raw/2} returns 
%% <code>undefined</code>
initialise_remote_page_vsn(Site, RefX, Version)
  when is_record(RefX, refX) ->
    % io:format("in initialise_remote_page_vsn~n"),
    Page = RefX#refX{obj = {page, "/"}},
    Record = #page_vsn{site_and_pg = {Site, Page}, version = Version},
    mnesia:write(trans(Site, page_vsn), Record, write).

%% @spec incr_remote_page_vsn(Site, Version::#version{}, Payload) -> 
%% [ok | {error, unsynched}]
%% NewVersion = integer()
%% @doc increments the local storage of a page version number for a page on
%% a remote site if the new increment is one above the old one. If the increment
%% is greater than 1 returns an error which should trigger a resynch.
%% Incrementation of page versions for local pages should be done with 
%% {@link get_new_local_page_vsn/2}
incr_remote_page_vsn(Site, #version{version = "undefined"} = V, Payload) ->
    % io:format("in incr_remote_page_vsn~n"),
    #version{page = Page} = V,
    PageX = hn_util:url_to_refX(Page),
    Record = #page_vsn{site_and_pg = {Site, PageX}, version = 1},
    ok = mnesia:write(trans(Site, page_vsn), Record, write),
    % now write a history record
    {Type, Ref, Displacement} = json_util:json_to_payload(Payload),
    ActionRefX = PageX#refX{obj = Ref},
    Record2 = #page_history{site_and_pg = {Site, PageX}, 
                            action = {Type, Displacement},
                            action_refX = ActionRefX, 
                            version = 1},
    ok = mnesia:write(trans(Site, page_history), Record2, write),
    synched;
incr_remote_page_vsn(Site, Version, Payload) when is_record(Version, version) ->
    #version{page = Page, version = NewVsn} = Version,
    PageX = hn_util:url_to_refX(Page),
    OldVsn = read_page_vsn(Site, PageX),
    case OldVsn + 1 of
        NewVsn -> Record = #page_vsn{site_and_pg = {Site, PageX}, version = NewVsn},
                  ok = mnesia:write(trans(Site, page_vsn), Record, write),
                  {Type, Ref, Displacement} = json_util:json_to_payload(Payload),
                  ActionRefX = PageX#refX{obj = Ref},
                  Record2 = #page_history{site_and_pg = {Site, PageX},
                                          action = {Type, Displacement},
                                          action_refX = ActionRefX, version = NewVsn},
                  ok = mnesia:write(trans(Site, page_history), Record2, write),
                  synched;
        _      -> unsynched
    end.

%% @spec(get_new_local_page_vsn(RefX :: #refX{}, Action) -> NewVsn
%% NewVsn = integer()
%% @doc writes an action to the page version table and gets the new
%% version number of the page for a page on the current server - doesn't affect
%% the page version information for pages on other servers which must be increased
%% using (@link incr_remote_page_vsn/3}.
%% RefX can be a cell, range, column, row or page reference. 
%% (the page_vns tables hold the page version for both local and remote sites)
get_new_local_page_vsn(#refX{site = Site} = RefX, Action) ->
    
    PageRefX = RefX#refX{obj = {page, "/"}},
    % first read the current page version number, increment it and overwrite it
    NewVsn = case mnesia:read(trans(Site, page_vsn), {Site, PageRefX}) of
                 []                       -> 1;
                 [#page_vsn{version = N}] -> N + 1
             end,
    Record1 = #page_vsn{site_and_pg = {Site, PageRefX}, version = NewVsn},
    ok = mnesia:write(trans(Site, page_vsn), Record1, write),
    % now write the history table
    Record2 = #page_history{site_and_pg = {Site, PageRefX},
                            action = Action,
                            action_refX = RefX, version = NewVsn},
    ok = mnesia:write(trans(Site, page_history), Record2, write),
    NewVsn.

%% @spec read_page_vsn(Site, RefX::#refX{}) -> Vsn
%% Vsn = integer()
%% @doc gets the current page number of a page. 
%% The variable Site is the protocol, domain name and port of the current site
%% (eg "http://sub.dom.tld:1234")
%% RefX is a reference to an object on the same Site and can be a cell, range,
%% column, row or page reference. 
%% (the page_vns tables hold the page version for both local and remote sites)
read_page_vsn(Site, RefX) when is_record(RefX, refX) ->
    PageRefX = RefX#refX{obj = {page, "/"}},
    case mnesia:read({trans(Site, page_vsn), {Site, PageRefX}}) of
        []                       -> "undefined";
        [#page_vsn{version = N}] -> N
    end.

%% @spec register_out_hn(Parent::#refX{}, Child::#refX{}, Proxy, Biccie) -> ok
%% @doc register_hypernumber registers a new hypernumber.
%% This function is *ONLY* called on the parent (or out) side of the relationship
register_out_hn(Parent, Child, Proxy, Biccie)
  when is_record(Parent, refX), is_record(Child, refX)->
    #refX{site = ParentSite} = Parent,
    #refX{site = ChildSite} = Child,
    Hn = #outgoing_hn{site_and_parent = {ParentSite, Parent},
                      biccie          = Biccie,
                      child_site      = ChildSite,
                      child_proxy     = Proxy},
    mnesia:write(trans(ParentSite, Hn)).

%% @spec does_remote_link_exist(Parent::#refX{}, Child::#refX{}, Type) -> 
%% [true | false]
%% Type = [incoming | outgoing]
%% @doc does_remote_link_exists checks if a remote link already exists
does_remote_link_exist(Parent, Child, Type)
  when is_record(Parent, refX), is_record(Child, refX),
             (Type =:= incoming orelse Type =:= outgoing) ->
    #refX{site = Site} = case Type of
                             incoming -> Child;
                             outgoing -> Parent
                         end,
        Head = #remote_cell_link{parent = Parent, child = Child, 
                                 type = Type, _ = '_'},
    Match = [{Head, [], ['$_']}],
    case mnesia:select(trans(Site, remote_cell_link), Match, read) of
        []     -> false;
        [_Rec] -> true
    end.

%% @spec verify_biccie_out(Parent::#refX{}, Child::#refX{}, Biccie) -> [true | false]
%% @doc verifies if a biccie provided for an outgoing hyperlink is valid. It strips out
%% the Child site from the Child <code>#refX{}</code>.
verify_biccie_out(Parent, Child, Biccie)
  when is_record(Parent, refX), is_record(Child, refX) ->
    #refX{site = ParentSite} = Parent,
    #refX{site = ChildSite} = Child,
    Match = #outgoing_hn{site_and_parent = {ParentSite, Parent},
                         child_site = ChildSite, _ = '_'},
    % because it is an outgoing hypernumber the site for the table is the parent site...
    Table = trans(ParentSite, outgoing_hn),
    [Hn] = mnesia:match_object(Table, Match, read),
    #outgoing_hn{biccie = Biccie2} = Hn,
    case Biccie of
        Biccie2 -> true;
        _       -> PUrl = hn_util:refX_to_url(Parent),
                   CUrl = hn_util:refX_to_url(Child),
                   bits:log("BICCIE MATCH FAIL £ " ++ pid_to_list(self()) ++
                            " £ "++ PUrl ++ " £ " ++ Biccie2 ++ " £ " ++ CUrl ++
                            " £ " ++ Biccie),
                   false
    end.

%% @spec verify_biccie_in(Site, Parent::#refX{}, Biccie) -> [true | false]
%% @doc verifies if a biccie provided for an incoming hyperlink is valid. 
%% Site is the local site. It strips out
%% the child site from the Child <code>#refX{}</code>.
verify_biccie_in(Site, Parent, Biccie) when is_record(Parent, refX) ->
    Match = #incoming_hn{site_and_parent = {Site, Parent}, _ = '_'},
    Table = trans(Site, outgoing_hn),
    List = mnesia:match_object(Table, Match, read),
    case List of
        []   -> false;
        [Hn] -> #incoming_hn{biccie = Biccie2} = Hn,
                case Biccie of
                    Biccie2 -> true;
                    _       -> false
                end
    end.

%% @spec mark_dirty(Site, Record) -> ok
%% Record = #dirty_notify_back_in{} | #dirty_notify_in{}
%% @doc writes a record to the appropriate dirty table
mark_dirty(Site, Record)
  when (is_record(Record, dirty_notify_back_in)
        orelse is_record(Record, dirty_notify_in)
        orelse is_record(Record, dirty_notify_back_out)
        orelse is_record(Record, dirty_inc_hn_create)) ->
    mnesia:write(trans(Site, Record)).

%% @spec write_remote_link(Parent::#refX{}, Child::#refX{}, Type) -> ok
%% @doc writes a remote link between the parent and the child.
%% Both parent and child references must be to a cell. The parent is on the
%% remote site and the child is on the local site
write_remote_link(P, C, Type)
  when is_record(P, refX), is_record(C, refX),
       (Type =:= incoming orelse Type =:= outgoing)->
    Rec = #remote_cell_link{parent = P, child = C, type = Type},
    #refX{site = Site} = case Type of
                             incoming -> C;
                             outgoing -> P
                         end,
    mnesia:write(trans(Site, remote_cell_link), Rec).

%% @spec update_inc_hn(Parent::#refX{}, Child::#refX{}, Val, 
%% DepTree, Biccie) -> ok
%% DepTree = list()
%% @doc update_inc_hn will try and update the incoming hypernumber with
%% a new value.
%% Both Parent and Child must be cell references
%% This function also triggers the child cells as dirty so they recalculate
update_inc_hn(Parent, Child, Val, DepTree, Biccie)
  when is_record(Parent, refX), is_record(Child, refX) ->
    #refX{site = ChildSite} = Child,
    Rec1 = #incoming_hn{site_and_parent = {ChildSite, Parent}, value = Val,
                        'dependency-tree' = DepTree, biccie = Biccie},
    ok = mnesia:write(trans(ChildSite, incoming_hn), Rec1, write),
    Rec2 = #dirty_notify_in{parent = Parent},
    ok = mark_dirty(ChildSite, Rec2).

%% @spec get_cells(RefX::#refX{}) -> [#refX{}]
%% @doc takes a reference and expands it to *populated* cell references.
%% The reference can be any of:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>row</li>
%% <li>column</li>
%% <li>page</li>
%% </ul>
%% and the return is a list of cell references
%%
%% If you want to expand a range to a list of cell references 
%% irrespective of wether or not they are populated then use
%% hn_util:range_to_list(refX{})
get_cells(#refX{obj = {cell, _}} = RefX) -> [RefX];
get_cells(#refX{site = S, obj = {range, _}} = RefX) ->
    MatchRef = make_range_match_ref(RefX),
    get_cells1(S, MatchRef);
get_cells(#refX{site = S, obj = {row, _}} = RefX) ->
    MatchRef = make_row_match_ref(RefX),
    get_cells1(S, MatchRef);
get_cells(#refX{site = S, obj = {column, _}} = RefX) ->
    MatchRef = make_col_match_ref(RefX),
    get_cells1(S, MatchRef);
get_cells(#refX{site = S, obj = {page, _}} = RefX) ->
    MatchRef = make_page_match_ref(RefX),
    get_cells1(S, MatchRef).

get_cells1(Site, MatchRef) ->
    Table = trans(Site, local_objs),
    List = mnesia:select(Table, [MatchRef]),
    Fun = fun(#local_objs{path = Path, obj = Obj}) ->
                  #refX{site = Site, path = Path, obj = Obj}
          end,
    [Fun(X) || X <- List].

%% @spec clear_dirty(Site, Record) -> ok
%% Record = #dirty_notify_back_in{} | #dirty_inc_hn_create{} | #dirty_notify_in{}
%% @doc clears a dirty record.
clear_dirty(Site, Rec) when (is_record(Rec, dirty_notify_in)
                       orelse is_record(Rec, dirty_notify_out)
                       orelse is_record(Rec, dirty_inc_hn_create)
                       orelse is_record(Rec, dirty_notify_back_in)
                       orelse is_record(Rec, dirty_notify_back_out)) ->
    Rec2 = trans(Site, Rec),
    mnesia:delete_object(Rec2).

-spec clear_dirty_cell(string(), #dirty_cell{}) -> ok.
%% @doc clears a dirty cell marker.
%% The reference must be to a cell
clear_dirty_cell(Site, Record) when is_record(Record, dirty_cell) ->
    mnesia:delete_object(trans(Site, dirty_cell), Record, write).

%% @spec get_refs_below(#refX{}) -> [#refX{}]
%% @doc gets all the refs equal to or below a given reference as well as
%% all cells that are children of cells below the refence
%% 
%% The reference passed in can
%% be one of the following, a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>row</li>
%% </ul>
get_refs_below(#refX{obj = {cell, {X, Y}}} = RefX) ->
    get_refs_below2(RefX, X, X, Y);
get_refs_below(#refX{site = S, path = P, obj = {row, {Y1, Y2}}} = RefX) ->
    % rectify the row range in case they are reversed...
    YY = ?COND(Y1 > Y2, Y1, Y2),
    Obj = {cell, {'_', '$1'}},
    % first get the cells
    Head1 = #local_objs{path = P, obj = Obj, _ = '_'},
    Cond = [{'>', '$1', YY}],
    Body = ['$_'],
    RefXs1 = local_objs_to_refXs(S, get_local_idxs(S, {Head1, Cond, Body})),
    % now get the local cells that are children of
    % cells below the refX
    PIdx = read_local_item_index(RefX),
    Head2 = #local_cell_link{parentidx = PIdx, _ = '_'},
    RefXs2 = get_local_links_refs(S, {Head2, [], Body}),
    RefXs = lists:append([RefXs1, RefXs2]),
    hslists:uniq(RefXs);
get_refs_below(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    % rectify the ranges in case they are reversed...
    YY = ?COND(Y1 > Y2, Y1, Y2),
    {XX1, XX2} = ?COND(X1 > X2, {X2, X1}, {X1, X2}),
    get_refs_below2(RefX, XX1, XX2, YY).

%% @spec get_refs_right(#refX{}) -> [#refX{}]
%% @doc gets all the refs to the equal to or to the right of a given reference.
%% 
%% The reference passed
%% in can be one of the following, a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% </ul>
get_refs_right(#refX{obj = {cell, {X, Y}}} = RefX) ->
    get_refs_right2(RefX, X, Y, Y);
get_refs_right(#refX{site = S, obj = {column, {X1, X2}}} = RefX) ->
    % rectify the row range in case they are reversed...
    XX = ?COND(X1 > X2, X1, X2),
    #refX{site = S, path = P} = RefX,
    Obj = {cell, {'$1', '_'}},
    Head1a = #local_objs{path = P, obj = Obj, _ = '_'},
    Cond = [{'>', '$1', XX}],
    Body = ['$_'],
    RefXs1 = local_objs_to_refXs(S, get_local_idxs(S, {Head1a, Cond, Body})),
    % now get the local pages that are children of
    %  cells below the refX
    PIdx = read_local_item_index(RefX),
    Head2 = #local_cell_link{parentidx = PIdx, _ = '_'},
    RefXs2 = get_local_links_refs(S, {Head2, [], Body}),
    RefXs = lists:append([RefXs1, RefXs2]),
    hslists:uniq(RefXs);
get_refs_right(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    % rectify the ranges in case they are reversed...
    XX = ?COND(X1 > X2, X1, X2),
    {YY1, YY2} = ?COND(Y1 > Y2, {Y2, Y1}, {Y1, Y2}),
    get_refs_right2(RefX, XX, YY1, YY2).

%% @spec get_last_refs(#refX{}) -> {LastColumnRef, LastRowRef}
%% LastColumnRef = #refX{}
%% LastColumnRef = #refX{}
%% @doc takes a reference and gets the value of the last populated row
%% 
%% The refX{} can refer to a:
%% <ul>
%% <li>page</li>
%% </ul>
%% @TODO this may have a race condition if two people try
%% and get the last row/column at the same time...
get_last_refs(#refX{site = S, path = P}) ->
    RefX2 = #refX{site = S, path = P, obj = {page, "/"}},
    Cells = read_cells(RefX2, read),
    Fun = fun({R, _}, {MaxRefX, MaxRefY}) ->
                  #refX{obj = {cell, {X, Y}}} = R,
                  #refX{obj = {cell, {MaxX, _}}} = MaxRefX,
                  #refX{obj = {cell, {_, MaxY}}} = MaxRefY,
                  NewX = ?COND(MaxX > X, MaxRefX, X),
                  NewY = ?COND(MaxY > Y, MaxRefY, Y),
                  {NewX, NewY}
          end,
    Zero = #refX{site = S, path = P, obj = {cell, {0, 0}}},
    lists:foldl(Fun, {Zero, Zero}, Cells).

%% @spec read_remote_parents(Ref, type) -> [#refX{}]
%% Ref = #refX{} | [#refX{}]
%% @doc this returns the remote parents of a reference.
%% 
%% The reference can be either a single <code>#refX{}</code> or a list
%% of them. (rows, columns, ranges and pages are disallowed either way).
%% 
%% This fn is called read_remote_parents:
%% <ul>
%% <li>if the type is <code>incoming</code> and the reference is to a remote cell it 
%% returns all the parents of that remote cell on this site</li>
%% <li>if the type is <code>outgoing</code> and the reference is to a remote cell it
%% returns all the parents of the local cell on the remote server</li>
%% </ul>
read_remote_parents(List, Type) when is_list(List) ->
    Return = [read_remote_parents(X, Type) || X <- List],
    lists:flatten(Return);
read_remote_parents(#refX{site = Site, obj = {cell, _}} = Child, Type)
  when Type =:= incoming; Type =:= outgoing ->
    Match = #remote_cell_link{child = Child, type = Type, _ = '_'},
    Table = trans(Site, remote_cell_link),
    Links = mnesia:match_object(Table, Match, read),
    get_remote_parents(Links).

%% @spec read_remote_children(Ref, Type) -> [#refX{}]
%% Ref = #refX{} | [#refX{}]
%% @doc this returns the remote children of a reference.
%% 
%% The reference can be either a single <code>#refX{}</code> be to a cell or a list
%% of them. (rows, columns, ranges and pages are disallowed either way).
%% 
%% This fn is called read_remote_children:
%% <ul>
%% <li>if the type is <code>incoming</code> and the reference is to a remote cell it 
%% returns all the children of that remote cell</li>
%% <li>if the type is <code>outgoing</code> and the reference is to a local cell it
%% returns all the children of the local cell on the remote server</li>
%% </ul>
read_remote_children(List, Type) when is_list(List) ->
    Return = [read_remote_children(X, Type) || X <- List],
    lists:flatten(Return);
read_remote_children(#refX{site = Site, obj = {cell, _}} = Parent, Type)
  when Type =:= incoming; Type =:= outgoing ->
    Match = #remote_cell_link{parent = Parent, type = Type, _ = '_'},
    Table = trans(Site, remote_cell_link),
    Links = mnesia:match_object(Table, Match, read),
    get_remote_children(Links).

%% @spec read_local_parents_idx(Site, Idx) -> IdxList
%% @doc this returns the local parents of a reference. Both the parameters
%% in and list out are Idx's
%% 
read_local_parents_idx(Site, CIdx)  ->
    Table = trans(Site, local_cell_link),
    mnesia:index_read(Table, CIdx, childidx).

%% @spec read_local_parents(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the local parents of a reference. The reference can only
%% be to a cell and not a range, column, row or page
%% 
%% This fn is called read_local_parents because it consists of all the
%% local links where the current RefX is the child
read_local_parents(#refX{site = Site} = Child)  ->
    case read_local_item_index(Child) of
        false -> [];
        CIdx  -> Table = trans(Site, local_cell_link),
                 Links = mnesia:index_read(Table, CIdx, childidx),
                 get_local_parents(Site, Links)
    end.

%% @spec read_local_children(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the local children of a reference. The reference can only
%% be to a 
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
%% 
%% This fn is called read_local_children because it consists of all the
%% local links where the current RefX is the parent
read_local_children(#refX{obj = {Type, _}} = Parent)
  when (Type == row) orelse (Type == column)
       orelse (Type == range) orelse (Type == page) ->
    Cells = get_cells(Parent),
    lists:flatten([read_local_children(X) || X <- Cells]);
read_local_children(#refX{site = Site, obj = {cell, _}} = Parent) ->
    case read_local_item_index(Parent) of
        false -> [];
        PIdx  -> Table = trans(Site, local_cell_link),
                 Links = mnesia:read(Table, PIdx, read),
                 get_local_children(Site, Links)
    end.

%% @spec write_attr(RefX :: #refX{}, {Key, Value}) -> ok
%% Key = atom()
%% Value = term()
%% @doc this function writes attributes to a cell or cells.
%% 
%% The refX{} can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
%% 
%% This function deals with style-able attributes of cells auto-magically.
%% 
%% If an attribute is saved against a cell which is one of the styled
%% attributes defined in the ref magic_styles in file 
%% <a href="../include/spriki.hrl">spriki.hrl</a>
%% it will be magically managed as a style
%% @end
%% This clause deals with a formula
write_attr(#refX{obj = {cell, _}} = RefX, {"formula", _} = Attr) ->
    % first check that the formula is not part of a shared array
    case read_attrs(RefX, ["__shared"], read) of
        [_X] -> throw({error, cant_change_part_of_array});
        []   -> write_attr2(RefX, Attr)
    end;
write_attr(#refX{obj = {cell, _}} = RefX, {"format", Format} = Attr) ->
    ok = write_attr3(RefX, Attr),
    % now reformat values (if they exist)
    case read_attrs(RefX, ["rawvalue"], read) of
        []                               -> ok;
        [{RefX, {"rawvalue", RawValue}}] -> 
            ok = process_format(RefX, Format, RawValue)
    end;
write_attr(#refX{obj = {cell, _}} = RefX, {"__dependency-tree", DTree}) ->
    write_attr3(RefX, {"__dependency-tree", DTree});
write_attr(#refX{obj = {cell, _}} = RefX, {Key, Val} = Attr) ->
    % NOTE the attribute 'overwrite-color' isn't in a magic style and shouldn't be
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> process_styles(RefX, Attr);
        false -> write_attr3(RefX, {Key, Val})
    end;
write_attr(#refX{obj = {range, _}} = RefX, Attr) ->
    List = hn_util:range_to_list(RefX),
    [ok = write_attr(X, Attr) || X <- List],
    ok;
%% for the rest just write 'em out
write_attr(RefX, {Key, Val}) when is_record(RefX, refX) ->
    write_attr3(RefX, {Key, Val}).

%% @spec read_whole_page(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc reads all the attributes of a cell or cells
%%
%% The reference can refer to a page only
read_whole_page(#refX{site = S, path = P, obj = {page, "/"}}) ->
    List = mnesia:read(trans(S, local_objs), P, read),
    Return = make_refXs(S, List, [], read),
    drop_private(Return).
    
-spec read_cells(#refX{}, write | read) -> [{#refX{}, {any(), any()}}].
%% Key = atom()
%% Value = term()
%% @doc reads all the attributes of a cell or cells
%%
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_cells(Ref, Lock) when (Lock == write orelse Lock == read) ->
    List = read_cells_raw(Ref, Lock),
    drop_private(List).

-spec read_cells_raw(#refX{}, write | read) -> [{#refX{}, {any(), any()}}].
%% Key = atom()
%% Value = term()
%% @doc reads all the attributes of a cell or cells
%% 
%% This is a raw read because it returns *ALL* the attributes
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_cells_raw(#refX{site = S, path = P, obj = {cell, _} = O}, Lock) 
  when (Lock == write orelse Lock == read) ->
    H = #local_objs{path = P, obj = O, _ = '_'},
    read_attrs1(S, {H, [], ['$_']}, [], Lock);
read_cells_raw(#refX{site = Site, obj = {range, _}} = RefX, Lock)
  when (Lock == write orelse Lock == read) ->
    Match = make_range_match_ref(RefX),
    read_attrs1(Site, Match, [], Lock);
read_cells_raw(#refX{site = Site, obj = {column, _}} = RefX, Lock)
  when (Lock == write orelse Lock == read) ->
    Match = make_col_match_ref(RefX),
    read_attrs1(Site, Match, [], Lock);
read_cells_raw(#refX{site = Site, obj = {row, _}} = RefX, Lock)
  when (Lock == write orelse Lock == read) ->
    Match = make_row_match_ref(RefX),
    read_attrs1(Site, Match, [], Lock);
read_cells_raw(#refX{site = Site, obj = {page, _}} = RefX, Lock)
  when (Lock == write orelse Lock == read) ->
    Match = make_page_match_ref(RefX),
    read_attrs1(Site, Match, [], Lock).

%% @spec read_inherited_list(#refX{}, Key) -> {ok, Value}
%% Key = atom()
%% Value = term()
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at a given reference, if not found it returns the supplied
%%       default value
%%       
%% @todo what are the ref types it supports? improve the documentation, etc, etc
read_inherited_list(RefX, Key) when is_record(RefX, refX)  ->
    Type = case RefX#refX.obj of
               null -> page;
               {T, _R} -> T
           end,
    get_item_list(Type, RefX, Key, []).

%% @spec read_inherited(#refX{}, Key, Default) -> {ok, Value}
%% Key = atom()
%% Value = term()
%% Default = term()
%% @doc  This function searches the tree for the occurences of a key
%%       and returns a list of them
read_inherited(RefX, Key, Default) when is_record(RefX, refX)  ->
    Type = case RefX#refX.obj of
               null    -> page;
               {T, _R} -> T
           end,
    case return_first(Type, RefX, Key) of
        {ok, Value} -> {ok, Value};
        nomatch     -> {ok, Default}
    end.

%% @spec read_attrs(#refX{}, Lock) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc reads all the attributes for a reference.
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_attrs(RefX, Lock) 
  when is_record(RefX, refX), 
       (Lock == write orelse Lock == read) ->
    read_attrs(RefX, [], Lock).

%% @spec read_attrs(#refX{}, AttrsList, Lock) -> [{#refX{}, {Key, Value}}]
%% AttrsList = [Key]
%% Key = atom()
%% Value = term()
%% @end
%% @doc reads the attributes specified in the AttrsList for a reference.
%% If the attribute list is blank returns all the attributes
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_attrs(#refX{obj = {cell, _}} = RefX, Attrs, Lock) 
  when is_list(Attrs), 
       (Lock == write orelse Lock == read) ->
    #refX{site = S, path = P, obj= Obj} = RefX,
    H = #local_objs{path  = P, obj = Obj, _ = '_'},
    read_attrs1(S, {H, [], ['$_']}, Attrs, Lock);
read_attrs(#refX{site = Site, obj = {range, _}} = RefX, Attrs, Lock)
  when is_list(Attrs), 
       (Lock == write orelse Lock == read) ->
    Match = make_range_match_ref(RefX),
    read_attrs1(Site, Match, Attrs, Lock);
read_attrs(#refX{site = Site, obj = {column, _}} = RefX, Attrs, Lock)
  when is_list(Attrs), 
       (Lock == write orelse Lock == read) ->
    Match = make_col_match_ref(RefX),
    read_attrs1(Site, Match, Attrs, Lock);
read_attrs(#refX{site = Site, obj = {row, _}} = RefX, Attrs, Lock)
  when is_list(Attrs), 
       (Lock == write orelse Lock == read) ->
    Match = make_row_match_ref(RefX),
    read_attrs1(Site, Match, Attrs, Lock);
read_attrs(#refX{site = S, obj = {page, _}} = RefX, Attrs, Lock)
  when is_list(Attrs), 
       (Lock == write orelse Lock == read) ->
    Match = make_page_match_ref(RefX),
    read_attrs1(S, Match, Attrs, Lock).

read_attrs1(Site, Match, AttrList, Lock) ->
    Table = trans(Site, local_objs),
    List = mnesia:select(Table, [Match], read),
    make_refXs(Site, List, AttrList, Lock).

%% A/B performance testing for a page read
make_refXs(Site, LocalObjs, Attrs, Lock) ->  
make_refXs1A(Site, LocalObjs, Attrs, Lock, []).
% make_refXs(Site, LocalObjs, Attrs, Local) -> 
%    make_refXs1B(Site, LocalObjs, Attrs, Lock).

make_refXs1A(Site, [], _AttrList, _Lock, Acc) ->
    case Acc of
        []     -> [];
        _Other -> make_refXs2A(Site, lists:flatten(Acc), [])
    end;
% if the attributes list is blank get them all
make_refXs1A(Site, [H | T], [], Lock, Acc) ->
    #local_objs{idx = Idx} = H,
    Table = trans(Site, item),
    Recs = mnesia:read(Table, Idx, Lock),
    make_refXs1A(Site, T, [], Lock, [{H, Recs} | Acc]);
make_refXs1A(Site, [H | T], AttrList, Lock, Acc) ->
    #local_objs{idx = Idx} = H,
    Table = trans(Site, item),
    Recs = mnesia:read(Table, Idx, Lock),
    Fun = fun(#item{key = K}) ->
                  lists:member(K, AttrList) 
          end,
    Attrs = lists:filter(Fun, Recs),
    NewAcc = case Attrs of
                 [] -> Acc;
                 _  -> [{H, Attrs} | Acc]
             end,
    make_refXs1A(Site, T, AttrList, Lock, NewAcc).

make_refXs2A(_Site, [], Acc) -> lists:flatten(Acc);
make_refXs2A(Site, [{LocalObj, Attrs} | T], Acc) ->
    NewAcc = make_refXs3A(Site, LocalObj, Attrs, []),
    make_refXs2A(Site, T, [NewAcc | Acc]).

make_refXs3A(_Site, _LocalObj, [], Acc) -> Acc;
make_refXs3A(Site, #local_objs{path = P, obj = O} = LocalObj, [H | T], Acc) ->
    #item{key = K, val = V} = H,
    NewAcc = {#refX{site = Site, path = P, obj = O}, {K,V}},
    make_refXs3A(Site, LocalObj, T, [NewAcc | Acc]).

-spec shift_cells(#refX{}, any(), any(), any()) -> any().
%% Status = list()
%% @doc shift_cells takes a range, row or column and shifts it by the offset.
%% The list of cells that are passed in as Rewritten are not to be rewritten
%% here
%% <em>NOTE</em> this function doesn't pass any messages on to parents or
%% children on other websites - that is done in the API layer by calling
%% {@link hn_db_wu:mark_dirty/1} with <code>dirty_notify_out</code> and 
%% <code>dirty_notify_back_in</code> records as appropriate
shift_cells(From, Type, Disp, Rewritten)
  when is_record(From, refX)
       andalso (Type == insert orelse Type == delete)
       andalso (Disp == vertical orelse Disp == horizontal) ->
    #refX{site = Site, obj = Obj} = From,
    {XO, YO} = hn_util:get_offset(Type, Disp, Obj),
    RefXList = case {Type, Disp} of
                   {insert, horizontal} ->
                       RefX2 = insert_shift(From, Disp),
                       get_refs_right(RefX2);
                   {insert, vertical}   ->
                       RefX2 = insert_shift(From, Disp),
                       get_refs_below(RefX2);
                   {delete, horizontal} ->
                       get_refs_right(From);
                   {delete, vertical}   ->
                       get_refs_below(From)
               end,
    case RefXList of
        [] -> [];
        _  ->
            [ok = shift_cells1(X, offset(X, {XO, YO})) || X <- RefXList],
            
            % now get the indexes of all the objs referred to
            IdxList = [read_local_item_index(X) || X <- RefXList],
            
            % now rewrite the formula of all the child cells
            %  - get the formulae
            %  - rewrite the formulae and save the cells
            H1 = #local_cell_link{childidx = '$1', parentidx = '$2', _ = '_'},
            C1 = make_clause(IdxList, '$2', 'orelse'),
            B1 = ['$1'],
            Table1 = trans(Site, local_cell_link),
            CIdxList = mnesia:select(Table1, [{H1, C1, B1}], write),
            ChildCells = [local_idx_to_refX(Site, X) || X <- CIdxList],
            ChildCells2 = hslists:uniq(ChildCells),
            DedupedChildren = lists:subtract(ChildCells2, Rewritten),
            
            Fun1 = fun(X) ->
                           read_attrs(X, ["formula"], write)
                   end,
            FormulaList1 = [Fun1(X) || X <- DedupedChildren],
            FormulaList2 = hslists:uniq(lists:flatten(FormulaList1)),

            Fun2 = fun({RefX, {"formula", F1}}, Acc) ->
                           {St, F2} = offset_fm_w_rng(RefX, F1, From, {XO, YO}),
                           % bits:log(io_lib:format("(3) F1 is ~p F2 is ~p~n",
                           % [F1, F2])),
                           % io:format("(3) F1 is ~p F2 is ~p~n",
                           %          [F1, F2]),
                           ok = write_attr3(RefX, {"formula", F2}),
                           case St of
                               clean  -> Acc;
                               dirty -> [{dirty, RefX} | Acc]
                           end
                   end,
            Status = lists:foldl(Fun2, [], FormulaList2),              
            % now shift the actual cells
            % - first up adjust the local_objs table 
            %   - read all the cell indices
            %   - adjust them all
            %   - delete the old ones
            %   - write the new ones
            H2 = #local_objs{idx = '$1', _ = '_'},
            C2 = make_or(IdxList, '$1'),
            B2 = ['$_'],
            Table2 = trans(Site, local_objs),
            Cells = mnesia:select(Table2, [{H2, C2, B2}], read),
            Fun3 = fun(#local_objs{obj = {cell, {X, Y}}} = Cell, Acc) ->
                           O2 = {cell, {X + XO, Y + YO}},
                           [Cell#local_objs{obj = O2} | Acc]
                   end,
            NewCells = lists:foldl(Fun3, [], Cells),
            ok = delete_recs_new(Site, Cells),
            [ok = mnesia:write(trans(Site, local_objs), X, write) 
             || X <- NewCells],
            % return the Status of dirty cells
            Status
    end.

shift_cells1(From, To) when is_record(From, refX), is_record(To, refX) ->
    ok = shift_dirty_notify_ins(From, To),
    ok = shift_remote_links(parent, From, To, outgoing),
    ok = shift_remote_links(child, From, To, incoming),
    ok.

-spec delete_col_objs(#refX{}) -> ok.
%% @doc deletes any col objects completely covered by the #refX{}
delete_col_objs(#refX{site = S, path = P, obj = {column, {X1, X2}}}) ->
    H = #local_objs{path = P, obj = {column, {'$1', '$2'}}, _ = '_'},
    C = [{'or', {'>=', '$1', X1}, {'>=', '$2', X2}}],
    B = ['$_'],
    M = [{H, C, B}],
    Table = trans(S, local_objs),
    Recs = mnesia:select(Table, M, write),
    ok = delete_recs_new(S, Recs).

-spec delete_row_objs(#refX{}) -> ok.
%% @doc deletes any row objects completely covered by the #refX{}
delete_row_objs(#refX{site = S, path = P, obj = {row, {Y1, Y2}}}) ->
    H = trans(S, #local_objs{path = P, obj = {row, {'$1', '$2'}}, _ = '_'}),
    C = [{'or', {'>=', '$1', Y1}, {'>=', '$2', Y2}}],
    B = ['$_'],
    M = [{H, C, B}],
    Table = trans(S, local_objs),
    Recs = mnesia:select(Table, M, write),
    ok = delete_recs(S, Recs).

-spec shift_col_objs(#refX{}, insert | delete) -> ok.
%% @doc shift_cols shifts cols left or right
shift_col_objs(#refX{site = S, path = P, obj = {column, {X1, X2}}} = Change,
               Type)
  when ((Type == insert) orelse (Type == delete)) ->
    XX = case Type of
             insert -> X2;
             delete -> X1
         end,
    H = trans(S, #local_objs{path = P, obj = {column, {'$1', '$2'}}, _ = '_'}),
    C = [{'or', {'>=', '$1', XX}, {'>=', '$2', XX}}],
    B = ['$_'],
    M = [{H, C, B}],
    Table = trans(S, local_objs),
    Recs = mnesia:select(Table, M, write),
    [ok = shift_col_objs1(X, Change, Type) || X <- Recs],
    ok.

shift_col_objs1(Shift, Change, Type) ->
    #local_objs{obj = {column, {Y1, Y2}}} = Shift,
    #refX{site = S, path = _P, obj = {column, {YY1, YY2}}} = Change,
    Offset = case Type of
                 insert -> YY2 - YY1 + 1;
                 delete -> -(YY2 - YY1 + 1)             end,
    New = Shift#local_objs{obj = {column, {Y1 + Offset, Y2 + Offset}}},
    ok = delete_recs_new(S, [Shift]),
    ok = mnesia:write(trans(S, local_objs), New, write).

-spec shift_row_objs(#refX{}, insert | delete) -> ok.
%% @doc shift_rows shifts rows up or down
shift_row_objs(#refX{site = S, path = P, obj = {row, {Y1, Y2}}} = Change, Type)
  when ((Type == insert) orelse (Type == delete)) ->
    YY = case Type of
             insert -> Y2;
             delete -> Y1
         end,
    H = #local_objs{path = P, obj = {row, {'$1', '$2'}}, _ = '_'},
    C = [{'or', {'>=', '$1', YY}, {'>=', '$2', YY}}],
    B = ['$_'],
    M = [{H, C, B}],
    Table = trans(S, local_objs),
    Recs = mnesia:select(Table, M, write),
    [ok = shift_row_objs1(X, Change, Type) || X <- Recs],
    ok.

shift_row_objs1(Shift, Change, Type) ->
    #local_objs{obj = {row, {X1, X2}}} = Shift,
    #refX{site = S, obj = {row, {XX1, XX2}}} = Change,
    Offset = case Type of
                 insert -> XX2 - XX1 + 1;
                 delete -> -(XX2 - XX1 + 1)
             end,
    New = Shift#local_objs{obj = {row, {X1 + Offset, X2 + Offset}}},
    ok = delete_recs_new(S, [Shift]),
    ok = mnesia:write(trans(S, local_objs),  New, write).

%% @spec read_styles(#refX{}) -> [Style]
%% Style = #styles{}
%% @doc returns a list of styles associated with a reference
%% 
%% The refX{} can refer to any of a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_styles(#refX{site = Site, obj = {page, _}} = RefX) ->
    Table = trans(Site, styles),
    mnesia:read(Table, RefX, read);
read_styles(#refX{site = Site} = RefX) when is_record(RefX, refX) ->
    % first get the style records to get the indexes
    CellList = read_attrs(RefX, ["style"], read),
    IndexList = hslists:uniq(extract_values(CellList)),
    % RefX is a cell/column/row/range ref - make it a page ref
    Ref2 = RefX#refX{obj = {page, "/"}},
    Match = #styles{refX = Ref2, index =  '$1', _ = '_'},
    Cond = make_or(IndexList, '$1'),
    Body = ['$_'],
    Table = trans(Site, styles),
    mnesia:select(Table, [{Match, Cond, Body}]).

%% @spec clear_cells(#refX{}) -> ok
%% @doc deletes the contents (formula/value) and the formats and attributes
%% of a cell (but doesn't delete the cell itself).
%% 
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
%%  
%% The same as clear_cells(RefX, contents).
clear_cells(RefX) when is_record(RefX, refX) ->
    clear_cells(RefX, contents).

%% @spec clear_cells(#refX{}, Type) -> ok 
%% Type = [contents | style | all]
%% @doc clears a cell or cells
%% 
%% The behaviour depends on the value of type
%% <ul>
%% <li><code>contents</code> - deletes the formula/value but not the attributes
%% or formats (or the cell itself)</li>
%% <li><code>all</code> - deletes the contents, formats, styles 
%% and attributes (but not the cell itself)</li>
%% </ul>
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
clear_cells(RefX, all) when is_record(RefX, refX)->
    ok = clear_cells(RefX, style),
    ok = clear_cells(RefX, contents);
clear_cells(RefX, style) when is_record(RefX, refX) ->
    List = read_attrs(RefX, ["style"], write),
    [delete_attrs(X, Key) || {X, {Key, _Val}} <- List],
    ok;
clear_cells(RefX, contents) when is_record(RefX, refX) ->
    
    % first up clear the list
    case read_attrs(RefX, write) of
        [] -> ok;
        List1 ->
            List2 = get_cells(RefX),
            % first set all the dirty cells that match to deleted
            ok = delete_dirty_cells(RefX#refX.site, List2),
            %[ok = mark_dirty_cells_deleted(X) || X <- List2],
            % now delete the links to the cells
            [ok = delete_parent_links(X) || X <- List2],
            % finally delete all the attributes
            List3 = get_content_attrs(List1),
            [delete_attrs(X, Key) || {X, {Key, _Val}} <- List3],
            [ok = mark_cells_dirty(X) || X <- List2],
            ok
    end.

%% @spec delete_page(RefX) -> Status
%% @doc takes a reference to a page, does delete_cells,
%% Then reads any existing local_objs and deletes any
%% row / column ones, along with any attributes set on them
%% Returns a list of dereferenced cells thatneed to be set dirty to recalculate
delete_page(#refX{site=Site, path=Path, obj = {page, "/"}} = RefX) ->
    
    Status = delete_cells(RefX),
    
    F = fun(#local_objs{obj = {X, _Y}, idx = Id} = Obj)
           when X == column; X == row ->
                ok = mnesia:delete(trans(Site, item), Id, write),
                mnesia:delete_object(trans(Site, local_objs), Obj, write);
           (_Else) ->
                ok
        end,

    Match = #local_objs{path = Path, _ = '_'},
    Objs  = mnesia:match_object(trans(Site, local_objs), Match, read),
    
    [ ok = F(X) || X <- Objs ],
    
    Status.

%% @spec delete_cells(RefX) -> Status
%% Status = list()
%% @doc takes a reference to a
%% <ul>
%% <li>page</li>
%% <li>row</li>
%% <li>column</li>
%% <li>range</li>
%% <li>cell</li>
%% </ul>
%% and then deletes all the cells including their indices in local_objs) 
%% and makes all cells that are their children throw a #ref! error
%% and deletes the links there the old cell was the child of another cell
%% @todo this is ineffiecient because it reads and then deletes each
%% record individually - if remoting_reg supported a {delete refX all}
%% type message it could be speeded up
delete_cells(#refX{site = S} = DelX) ->
    Cells = get_cells(DelX),
     case Cells of
        [] -> [];
        _ -> %% first delete any dirty cell references that point to these cells
             delete_dirty_cells(S, Cells),
             %[ok = mark_dirty_cells_deleted(X) || X <- Cells],
             %% update the children that point to the cell that is being deleted
             %% by rewriting the formulae of all the children cells replacing the 
             %% reference to this cell with #ref!
             LocalChildren = [read_local_children(X) || X <- Cells],
             LocalChildren2 = hslists:uniq(lists:flatten(LocalChildren)),
             %% sometimes a cell will have local children that are 
             %% also in the delete zone these need to be removed 
             %% before we do anything else...
             LocalChildren3 = lists:subtract(LocalChildren2, Cells),
             %% now split this list into two
             %% * a list of child/parents to delink
             %% * a list of children to deref
             %% first up deref the children
             Fun1 = fun(X, Acc) ->
                            Status = deref_child(X, DelX),
                            case Status of
                                []   -> Acc;
                                Recs -> [Recs | Acc]
                            end
                    end,
             Status = lists:flatten(lists:foldl(Fun1, [], LocalChildren3)),
           
             %% now delink all the cells that are being deleted
             Fun2 = fun(X) ->
                            Idx = get_local_item_index(X),
                            case mnesia:read(trans(S, local_cell_link), Idx, write) of
                                [] -> ok;
                                _  -> ok % io:format("~nCell IS a local parent~n")
                            end,
                            ok = mnesia:delete(trans(S, local_cell_link), Idx, write)
                    end,
             [ok = Fun2(X) || X <- Cells],
             
             %% now remove all the links where these cells were the children
             Fun3 = fun(X) ->
                            CIdx = read_local_item_index(X),
                            Table = trans(S, local_cell_link),
                            Recs = mnesia:index_read(Table, CIdx, childidx),
                            ok = delete_recs(S, Recs)
                    end,

             [ok = Fun3(X) || X <- Cells],
             
             %% get the index of all items to be deleted
             #refX{site = S} = DelX,
             H1 = #local_objs{path = '$1', obj = '$2', _ = '_'},
             C1 = make_del_cond(Cells),
             Table1 = trans(S, local_objs),
             Recs = mnesia:select(Table1, [{H1, C1, ['$_']}], write),
             Fun4 = fun(X) ->
                            #local_objs{idx = Idx} = X,
                            Idx
                    end,
             IdxList = [Fun4(X) || X <- Recs],
             
             % delete all items with that index
             Tb2 = trans(S, item),
             Fun5 = 
                 fun(X) -> 
                         L = mnesia:read(Tb2, X, write),
                         %% you need to notify the front end 
                         %% before you delete the object...
                         RefX = local_idx_to_refX(S, X),
                         [ok = tell_front_end(RefX, {K, V}, delete) ||
                             #item{key = K, val = V} <- L],
                         [ok = mnesia:delete_object(Tb2, XX, write) || XX <- L],
                         ok
                 end,

             [ok = Fun5(X) || X <- IdxList],
             % finally delete the index records themselves
             [ ok = mnesia:delete_object(trans(S, local_objs), X, write)
               || X <- Recs],
             % need to return any cells that need to recalculate after the move
             Status
    end.

make_del_cond([]) ->
    exit("make_del_cond can't take an empty list");
make_del_cond(List) ->
    make_del_cond1(List, []).

make_del_cond1([], Acc) when length(Acc) == 1 ->
    Acc;
make_del_cond1([], Acc) ->
    [list_to_tuple(lists:flatten(['or', Acc]))];
make_del_cond1([#refX{path = P, obj = O} | T], Acc) ->
    make_del_cond1(T, [{'and', {'=:=', '$1', {const, P}},
                        {'=:=', '$2', {const, O}}} | Acc]).

%% @spec delete_attrs(RefX :: #refX{}, Key) -> ok
%% Key = atom()
%% @doc deletes a named attribute from a
%% cell or cells (but doesn't delete the cells themselves)
delete_attrs(#refX{site = S} = RefX, Key) ->
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> delete_style_attr(RefX, Key);
        false -> Idx = read_local_item_index(RefX),
                 H = #item{idx = Idx, key = Key, _ = '_'},
                 M = [{H, [], ['$_']}],
                 Table = trans(S, item), 
                 [#item{val = Val} = Rec] = mnesia:select(Table, M),
                 ok = mnesia:delete_object(Table, Rec, write),
                 ok = tell_front_end(RefX, {Key, Val}, delete)
    end.

%% @spec delete_if_attrs(RefX :: #refX{}, Key) -> ok
%% Key = atom()
%% @doc deletes a named attribute from a
%% cell or cells if it exists (but doesn't delete the cells themselve)
%% ONLY USE THIS FOR ATTRIBUTES THAT MIGHT EXIST OTHERWISE USE delete_attrs
delete_if_attrs(#refX{site = S} = RefX, Key) ->
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> delete_style_attr(RefX, Key);
        false ->
            Idx = read_local_item_index(RefX),
            H = #item{idx = Idx, key = Key, _ = '_'},
            Tb = trans(S, item),
            case mnesia:select(Tb, [{H, [], ['$_']}]) of
                []                   -> ok;
                [#item{val = V} = R] -> ok = mnesia:delete_object(Tb, R, write),
                                        ok = tell_front_end(RefX, {Key, V}, delete)
            end
    end.

%% @spec copy_cell(From :: #refX{}, To ::#refX{}, Incr) -> ok
%% Incr = [false | horizonal | vertical]
%% @doc copys cells from a reference to a reference
copy_cell(#refX{obj = {cell, _}} = From, #refX{obj = {cell, _}} = To, Incr)
  when is_record(From, refX), is_record(To, refX) ->
    FromList = read_cells_raw(From, read),
    {Contents, FilteredList} = filter_for_drag_n_drop(FromList),
    Output = case Contents of
                 [Contents2] -> superparser:process(Contents2);
                 []          -> ""
             end,
    #refX{obj = {cell, {FX, FY}}} = From,
    #refX{obj = {cell, {TX, TY}}} = To,
    case Output of
        {formula, Formula} ->
            NewFormula = offset_formula(Formula, {(TX - FX), (TY - FY)}),
            ok = write_attr(To, {"formula", NewFormula});
        [{Type, V},  _A, _F] ->
            V2 = case Incr of
                     false  ->
                         tconv:to_s(V);
                     _Other -> % Other can be a range of different values...
                         case Type of
                             int      -> NewV = V + diff(FX, FY, TX, TY, Incr),
                                         tconv:to_s(NewV);
                             datetime -> {datetime, {Y, M, D}, T} = V,
                                         D2 = D + diff(FX, FY, TX, TY, Incr),
                                         tconv:to_s({datetime, {Y, M, D2}, T}); 
                             _        -> tconv:to_s(V)
                         end
                 end,
            ok = write_attr(To, {"formula", V2});
        []  ->
            ok = clear_cells(To, all)
    end,
    % You want to copy the attributes AFTER setting the value
    % because setting a value sets the default alignment and format
    % and if the source cell has been reformatted after the data was entered
    % you want to carry that forward.
    AttrList = get_attr_keys(FilteredList),
    ok = copy_attrs(From, To, AttrList),
    % now mark the To cell dirty...
    ok = mark_cells_dirty(To).

%% @spec copy_attrs(From :: #refX{}, To :: #refX{}, AttrList) -> ok
%% AttrList = [atom()]
%% @doc copies all the attributes of a cell to a new cell or cells.
%% All the listed attributes are copied from the From #refX{} to
%% the To refX{}. 
%% The From #refX{} must be a cell reference, but the To #refX{} can be either
%% a cell or a range
copy_attrs(_From, _To, []) -> ok;
%% this clause deals with a copy attrs on the same page
copy_attrs(#refX{path = P, obj = {cell, _}} = From, 
           #refX{path = P, obj = {cell, _}} = To, ["style" | T]) ->
    [{From, {"style", Value}}] = read_attrs(From, ["style"], read),
    ok = write_attr(To, {"style", Value}),
    copy_attrs(From, To, T);
%% this clause deals with copying attributes between different pages
copy_attrs(#refX{obj = {cell, _}} = From, 
           #refX{obj = {cell, _}} = To, ["style" | T]) ->
    [{styles, _, _Idx, MagicStyle}] = read_styles(From),
    Idx = write_style(To, MagicStyle),
    ok = write_attr(To, {"style", Idx}),
    copy_attrs(From, To, T);
copy_attrs(#refX{obj = {cell, _}} = From, 
           #refX{obj = {cell, _}} = To, [H | T]) ->
    [{From, {Key, Value}}] = read_attrs(From, [H], read),
    ok = write_attr(To, {Key, Value}),
    copy_attrs(From, To, T);
copy_attrs(#refX{obj = {cell, _}} = From, 
           #refX{obj = {range, _}} = To, Attrs) ->
    List = hn_util:range_to_list(To),
    [ok = copy_attrs(From, X, Attrs) || X <- List].

%% @spec(From::refX{}, To::refX{}) -> ok
%% @doc Copies the style applied to From and attaches it to To.
%%      From can only be a cell ref but To can be either a cell or range
%%      ref
%% @end
%% this clause is for 'on page' copies where both From and To are on
%% the same page - just the index is copied
copy_style(#refX{site = S, path = P, obj = {cell, _}} = From, 
           #refX{site = S, path = P, obj = {Type, _}} = To)
  when Type == cell orelse Type == range ->
    [{_, {"style", Idx}}] = read_attrs(From, ["style"], read),
    List = case Type of
               cell  -> [To];
               range -> hn_util:range_to_list(To)
           end,
    Fun = fun(X) ->
                  write_attr(X, {"style", Idx})
          end,
    [ok = Fun(X) || X <- List],
    ok;
%% this clause is for copying styles across different pages
copy_style(#refX{obj = {cell, _}} = From, 
           #refX{obj = {Type, _}} = To)
  when Type == cell orelse Type == range ->
    [{styles, _, _Idx, MagicStyle}] = read_styles(From),
    List = case Type of
               cell  -> [To];
               range -> hn_util:range_to_list(To)
           end,
    Fun = fun(X) ->
                  Idx = write_style(X, MagicStyle),
                  write_attr(X, {"style", Idx})
          end,
    [ok = Fun(X) || X <- List],
    ok.    

%% @spec read_incoming_hn(Site, Parent) -> #incoming_hn{} | []
%% Parent = [#refX{}]
%% @doc reads an incoming hypernumber from the reference to the parent cell
%% The reference can be either a single <code>#refX{}</code> be to a cell, page
%% or a list of them. (rows, columns and ranges are disallowed).
%% @todo extend the hypernumbers paradigm to include registering with a range,
%% column, row or query, etc, etc
read_incoming_hn(Site, Args) -> read_incoming_hn2(Site, Args).

read_incoming_hn2(Site, List) when is_list(List) ->
    Return = [read_incoming_hn2(Site, X) || X <- List], 
    lists:flatten(Return);
read_incoming_hn2(Site, #refX{obj = {page, _}} = RefX) ->
    MatchRef = make_page_match(Site, RefX, incoming_hn),
    read_incoming_hn3(Site, MatchRef);
read_incoming_hn2(Site, #refX{obj = {cell, _}} = Parent)
  when is_record(Parent, refX) ->
    Head = #incoming_hn{site_and_parent = {Site, Parent}, _ = '_'},
    read_incoming_hn3(Site, Head).

read_incoming_hn3(Site, Head) ->
    Table = trans(Site, incoming_hn),
    mnesia:select(Table, [{Head, [], ['$_']}], write).

%% @spec find_incoming_hn(Site, Parent) -> #incoming_hn{} | []
%% Parent = [#refX{}]
%% @doc finds an incoming hypernumber from a reference to one of its children
%% The reference can be either a single <code>#refX{}</code> be to a cell, a page
%%  or a list of them. (rows, columns and ranges are disallowed either way).
%% @todo extend the hypernumbers paradigm to include registering with a range,
%% column, row or query, etc, etc
find_incoming_hn(Site, List) when is_list(List) ->
    Return = [find_incoming_hn(Site, X) || X <- List], 
    lists:flatten(Return);
find_incoming_hn(Site, RefX) when is_record(RefX, refX) ->
    % first expand the list to cells
    List = get_cells(RefX),
    % now get all the remote cell links with a reference as a child...
    Parents = read_remote_parents(List, incoming),
    read_incoming_hn(Site, Parents).

%% @spec read_outgoing_hns(Site, Parent) -> [#outgoing_hn{}]
%% Parent = #refX{} | [#refX{}]
%% @doc reads the details of all outgoing hypernumbers from a particular cell.
%% The reference can be either a single <code>#refX{}</code> be to a cell, a page
%% or a list containing either
%% of them. (rows, columns and ranges are disallowed either way).
%% @todo extend the hypernumbers paradigm to include registering with a range,
%% column, row or query, etc, etc
read_outgoing_hns(Site, List) when is_list(List) ->
    Return = [read_outgoing_hns(Site, X) || X <- List], 
    lists:flatten(Return);
read_outgoing_hns(Site, #refX{obj = {page, _}} = RefX) ->
    MatchRef = make_page_match(Site, RefX, outgoing_hn),
    Table = trans(Site, outgoing_hn),
    mnesia:select(Table, [{MatchRef, [], ['$_']}]);
read_outgoing_hns(Site, #refX{obj = {cell, _}} = Parent)
  when is_record(Parent, refX) ->
    MatchRef = #outgoing_hn{site_and_parent = {Site, Parent}, _ = '_'},
    Table = trans(Site, outgoing_hn),
    mnesia:select(Table, [{MatchRef, [], ['$_']}]).

%% spec mark_cells_dirty(RefX::#refX{}) -> ok
%% @doc marks a set of cells as dirty - leading to them being
%% recalculated.
%% The reference can be to one of a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>row</li>
%% <li>column</li>
%% <li>page</li>
%% </ul>
%% @todo extend to include url/db functions as required...
mark_cells_dirty(#refX{site = Site, obj = {cell, _}} = RefX) ->
    % Make a list of cells hypernumbers and direct
    % cell links
    % first up local
    case read_local_children(RefX) of
        []            -> ok;
        LocalChildren ->
            % Now write the local children to dirty_cell
            Fun = fun(X) ->
                          %?INFO("Dirty ~p", [X]),
                          Dirty = #dirty_cell{idx = get_local_item_index(X)},
                          ok = mnesia:write(trans(Site, dirty_cell), Dirty, write)
                  end,
            _Return1 = lists:foreach(Fun, LocalChildren),
            ok
    end;
%% for any other refX just expand it to a set of refXs and
%% then set them all dirty
mark_cells_dirty(RefX) when is_record(RefX, refX) ->
    Cells = get_cells(RefX),
    [ok = mark_cells_dirty(X) || X <- Cells],
    ok.

%% @spec mark_notify_out_dirty(Parent::#refX{}, Change)  -> ok
%% Change = {new_value, Value, DepTree} | {insert, Obj, Disp} | {delete, Obj, Disp}
%% DepTree = list()
%% Delay = integer()
%% @doc marks a cell as dirty so that its remote children can be updated.
%% Updates called with this function and not 
%% <code>mark_notify_out_dirty/3</code> are marked with the default delay
%% (defined in spriki.hrl)
mark_notify_out_dirty(Parent, Change) when is_record(Parent, refX) ->
    mark_notify_out_dirty(Parent, Change, ?DELAY).

%% @spec mark_notify_out_dirty(Parent::#refX{}, Change, Delay)  -> ok
%% Change = {new_value, Value, DepTree} | {insert, Obj, Disp} | {delete, Obj, Disp}
%% DepTree = list()
%% Delay = integer()
%% @doc marks a cell as dirty so that its remote children can be updated
%% Delay is a time in milliseconds that this message should be delayed
%% @todo this contains a transaction, WTF?
mark_notify_out_dirty(#refX{site = Site} = P, {Type, _, _} = Change, Delay) ->
    % read the outgoing hypernumbers
    % one for each site where the Parent is used in a hypernumber
    List = read_outgoing_hns(Site, P),
    % now we need to get a list of all the actual children and the page versions
    % of their pages
    Fun2 =
        fun(X) ->
                #outgoing_hn{child_site = ChildSite} = X,
                Head = get_head(ChildSite, P, Type),
                Table = trans(Site, remote_cell_link),
                Children = mnesia:select(Table, [{Head , [], ['$_']}], read),
                ReturnList = get_pages_and_vsns(Site, Children),
                {X, ReturnList}
        end,
    ChildrenList = lists:map(Fun2, List),

    % always write the dirty outgoing hypernumber
    PVsn = read_page_vsn(Site, P),
    ParentPage = P#refX{obj = {page, "/"}},
    ParentUrl = hn_util:refX_to_url(ParentPage),
    case List of
        [] -> ok;
        _  -> Rec = #dirty_notify_out{parent = P, change = Change,
                                      outgoing = ChildrenList,
                                      parent_vsn = {version, ParentUrl, PVsn},
                                      delay = Delay},
              Rec2 = trans(Site, Rec),
              mnesia:write(Rec2)
    end.

%% @spec unregister_out_hn(Parent::#refX{}, Child::#refX{}) -> ok
%% @doc unregisters an outgoing hypernumber - if it is the last reference
%% to an outgoing hypernumber deletes the hypernumber as well
%% Both parent and child references must point to a cell. This function is
%% *ONLY* to be used on the parent (or out) side of the hypernumber
%% @todo this required a full table scan for an unregister
%% will get veeeerrry expensive if you have 100,000 children tracking a
%% number!
unregister_out_hn(P, C)
  when is_record(P, refX), is_record(C, refX) ->
    #refX{site = ParentSite} = P,
    #refX{site = ChildSite} = C,
    % first up delete the remote cell link
    Head = #remote_cell_link{parent = P, child = C, type = outgoing, _ = '_'},
    Table = trans(ParentSite, remote_cell_link),
    [RemCellRec] = mnesia:select(Table, [{Head, [], ['$_']}], write),
    ok = mnesia:delete_object(Table, RemCellRec),
    % now see if any other remote cell references match this site...
    % - if none do, delete the hypernumber from outgoing_hn
    % - if some do, do nothing...
    H3 = #refX{site = ChildSite, _ = '_'},
    H4 = #remote_cell_link{parent = P, child = H3, type = outgoing, _ = '_'},
    case mnesia:select(Table, [{H4, [], ['$_']}], read) of
        []  -> H6 = #outgoing_hn{site_and_parent = {ParentSite, P},
                                     child_site = ChildSite, _ = '_'},
               Table2 = trans(ParentSite, outgoing_hn),
               [Rec] = mnesia:select(Table2, [{H6, [], ['$_']}], read),
               mnesia:delete_object(Table2, Rec);
        _   -> ok
    end.

%% @spec read_page_structure(Ref) -> dh_tree()
%% @doc read the populated pages under the specified path
%% @todo fix up api
read_page_structure(#refX{site = Site, obj = {page, "/"}}) ->
    Items = mnesia:dirty_all_keys(trans(Site, local_objs)),
    filter_pages(Items, dh_tree:new()).

filter_pages([], Tree) ->
    Tree;
filter_pages([Path | T], Tree) ->
    filter_pages(T, dh_tree:add(Path, Tree)).

-spec get_local_item_index(#refX{}) -> any().
%% @doc get_local_item_index get_item_index gets the index of an object 
%% AND CREATES IT IF IT DOESN'T EXIST
get_local_item_index(#refX{site = S, path = P, obj = O} = RefX) ->
    case read_local_item_index(RefX) of
        false -> Idx = util2:get_timestamp(),
                 Rec = #local_objs{path = P, obj = O, idx = Idx},
                 ok = mnesia:write(trans(S, local_objs), Rec, write),
                 Idx;
        Idx   -> Idx        
    end.
            
% converts a tablename into the site-specific tablename
trans(Site, TableName) when is_atom(TableName) ->
    Prefix = get_prefix(Site),
    list_to_atom(Prefix ++ "&" ++ atom_to_list(TableName));
% converts a record into the site-specific record
trans(Site, Record) when is_tuple(Record) -> 
    OldName = element(1, Record),
    NewName = trans(Site, OldName),
    setelement(1, Record, NewName).

% splits a tablename into the site and record
split_trans(List) when is_list(List)->
    Fun = fun(X, Acc) ->
                  [split_trans(X) | Acc]
          end,
    lists:foldl(Fun, [], List);
split_trans(Record) when is_tuple(Record) ->
    OldName = element(1, Record),
    OldName2 = atom_to_list(OldName),
    [Site, Port, NewName] = string:tokens(OldName2, "&"),
    NewName2 = list_to_atom(NewName),
    {"http://" ++ Site ++ ":" ++ Port, NewName2, Record}.

trans_back([]) -> [];
trans_back(List) when is_list(List) ->
    Fun = fun(X, Acc) ->
                  [trans_back(X) | Acc]
          end,
    lists:foldl(Fun, [], List);
trans_back(Atom) when is_atom(Atom) ->
    [_Site, _Port, NewName] = string:tokens(atom_to_list(Atom), "&"),
    list_to_atom(NewName);
trans_back(Record) when is_tuple(Record)-> 
    OldName = element(1, Record),
    OldName2 = atom_to_list(OldName),
    [_Site, _Port, NewName] = string:tokens(OldName2, "&"),
    setelement(1, Record, list_to_atom(NewName)).

get_prefix(Site) ->
    [_Proto, Dom, Port] = string:tokens(Site, ":"),
    [$/, $/ | Dom2] = Dom,
    Dom2 ++ "&" ++ Port.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_recs(Site, List) when is_list(List) ->
    [ok = delete_recs1(Site, X) || X <- List],
    ok.

delete_recs1(Site, Rec) ->
    Table = trans(Site, element(1, Rec)),
    mnesia:delete_object(Table, Rec, write).

delete_recs_new(Site, List) when is_list(List) ->
    [ok = delete_recs_new1(Site, X) || X <- List],
    ok.

delete_recs_new1(Site, Rec) ->
    Table = trans(Site, element(1, Rec)),
    ok = mnesia:delete_object(Table, Rec, write).

%% mark_dirty_cells_deleted(#refX{site = S, obj = {cell, _}} = RefX) ->
%%     Idx = read_local_item_index(RefX),
%%     H = trans(S, #dirty_cell{idx = Idx, _ = '_'}),
%%     M = [{H, [], ['$_']}],
%%     case trans_back(mnesia:select(trans(S, dirty_cell), M)) of
%%         [] -> ok;
%%         [#dirty_cell{timestamp = T}] ->
%%             D = #dirty_cell{idx = deleted, timestamp = T},
%%             ok = mnesia:write(trans(S, D))
%%     end.

insert_shift(#refX{obj = {cell, {X, Y}}} = RefX, vertical) ->
    RefX#refX{obj = {cell, {X, Y - 1}}};
insert_shift(#refX{obj = {cell, {X, Y}}} = RefX, horizontal) ->
    RefX#refX{obj = {cell, {X - 1, Y}}};
insert_shift(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {X1, Y1 - 1, X2, Y2 - 1}}};
insert_shift(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X1 - 1, Y1, X2 - 1, Y2}}};
insert_shift(#refX{obj = {row, {Y1, _Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {row, {Y1-1, Y1-1}}};
insert_shift(#refX{obj = {column, {X1, _X2}}} = RefX, horizontal) ->
    RefX#refX{obj = {column, {X1-1, X1-1}}};
insert_shift(RefX, _Disp) -> RefX.

local_idx_to_refX(S, Idx) ->
    case mnesia:index_read(trans(S, local_objs), Idx, idx) of
        [Rec] -> #local_objs{path = P, obj = O} = Rec,
                 #refX{site = S, path = P, obj = O};
        []    -> {error, id_not_found, Idx}
    end.

%% @doc Make a #muin_rti record out of a ref record and a flag that specifies 
%% whether to run formula in an array context.
refX_to_rti(#refX{site = S, path = P, obj = {cell, {C, R}}}, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, col = C, row = R, array_context = AC};
refX_to_rti(#refX{site = S, path = P, obj = {range, {C, R, _, _}}}, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, col = C, row = R, array_context = AC}.

get_local_idxs(Site, Match) ->
    Table = trans(Site, local_objs),
    mnesia:select(Table, [Match]).

local_objs_to_refXs(Site, List) when is_list(List) ->
    Return = [local_objs_to_refXs(Site, X) || X <- List],
    lists:flatten(Return);
local_objs_to_refXs(Site, LocalObj) when is_record(LocalObj, local_objs) ->
    #local_objs{path = P, obj = O} = LocalObj,
    [#refX{site = Site, path = P, obj = O}].

%% read_item_index reads the index of an object AND RETURNS 'false'
%% IF IT DOESN'T EXIST
read_local_item_index(#refX{site = S, path = P, obj = Obj}) ->
    Table = trans(S, local_objs),
    case mnesia:read(Table, P, read) of
        []   -> false;
        Recs -> I1 = ms_util2:get_index(local_objs, obj) + 1,
                case lists:keysearch(Obj, I1, Recs) of
                    false      -> false;
                    {value, R} -> I2 = ms_util2:get_index(local_objs, idx) + 1,
                                  element(I2, R)
                end
    end.


get_head(Site, Parent, Type) when ((Type == insert) orelse (Type == delete)) ->
    H1 = ms_util:make_ms(refX, [{site, Site}, {obj, {cell, {'_', '_'}}}]),
    H2 = Parent#refX{obj = {cell, {'_', '_'}}},
    #remote_cell_link{parent = H2, child = H1, _ = '_'};
get_head(Site, Parent, Type) when (Type == new_value) ->
    H1 = ms_util:make_ms(refX, [{site, Site}]), 
    #remote_cell_link{parent = Parent, child = H1, _ = '_'}.

get_pages_and_vsns(Site, List) ->
    Fun = fun(#remote_cell_link{child = C}) ->
                  Page = C#refX{obj = {page, "/"}},
                  Vsn = read_page_vsn(Site, Page), 
                  Page2 = hn_util:refX_to_url(Page),
                  #version{page = Page2, version = Vsn}
          end,
    hslists:uniq(lists:map(Fun, List)).

%% just takes a tokenised formula and swaps the Old Url for the New Url anywhere
%% that it is used in a hypernumber, ie looks for 'HN(OldUrl'
rewrite_hn_formula(Toks, OUrl, NUrl) -> rwf1(Toks, OUrl, NUrl, []).

%% just swap out the old URL for the new one...
rwf1([], _O, _N, {_St, Acc})        -> make_formula(lists:reverse(Acc));
rwf1([?hn,?bra,{str,O}|T], O, N, A) -> rwf1(T, O, N, [{str,N},?bra,?hn|A]);
rwf1([H | T], O, N, A)              -> rwf1(T, O, N, [H | A]).      

%% REMEMBER this is a BAG table and not a set table so writes
%% do not delete!
%% Therefore check if a record with this key and this object index 
%% already exists:
%% * if it does delete it and then write the record
%% * if it don't then just write it.
%% will write out any raw attribute
write_attr3(#refX{site = Site} = RefX, {Key, Val}) ->
    Idx = get_local_item_index(RefX),
    Match = #item{idx = Idx, key = Key, _ = '_'},
    Rec   = Match#item{val = Val},
    Table = trans(Site, item),
    
    case mnesia:match_object(Table, Match, write) of
        [] ->
            ok = mnesia:write(Table, Rec, write);
        [OldRec] ->
            ok = mnesia:delete_object(Table, OldRec, write),
            ok = mnesia:write(Table, Rec, write)
    end,
    
    case Key of
        "__" ++ _ ->
            ok;
        Key ->
            ok = tell_front_end(RefX, {Key, Val}, change)
    end.

update_rem_parents(Child, OldParents, NewParents) when is_record(Child, refX) ->
    {Del, Write} = split_parents(OldParents, NewParents),
    % first delete all the records on the delete list
    % and unregister them (probably should be done in a gen server!)
    [ok = delete_remote_parents(X) || X <- Del],
    % now write all the records on the write list
    [ok = write_remote_link(X, Child, incoming) || X <- Write],
    ok.

%% This function is called on a local cell to inform all remote cells that it
%% used to reference as hypernumbers to no longer do so.
unregister_inc_hn(Parent, Child)
  when is_record(Child, refX), is_record(Parent, refX) ->
    #refX{site = ChildSite} = Child,
    Head = #incoming_hn{site_and_parent = {ChildSite, Parent}, _ = '_'},
    Table = trans(ChildSite, incoming_hn),
    [Hn] = mnesia:select(Table, [{Head, [], ['$_']}], read),
    #incoming_hn{biccie = Biccie} = Hn,
    Head3 = #remote_cell_link{parent =  Parent, type = incoming, _ = '_'},
    Table = trans(ChildSite, remote_cell_link),
    ok = case mnesia:select(Table, [{Head3, [], ['$_']}], read) of
             [] -> Table2 = trans(ChildSite, incoming_hn),
                   ok = mnesia:delete({Table2, Parent});
             _  -> ok % somebody else still wants it so don't unregister
         end,
    PPage = Parent#refX{obj = {page, "/"}},
    CPage = Child#refX{obj = {page, "/"}},
    PUrl = hn_util:refX_to_url(PPage),
    CUrl = hn_util:refX_to_url(CPage),
    PV = hn_db_wu:read_page_vsn(ChildSite, Parent),
    CV = hn_db_wu:read_page_vsn(ChildSite, Child),
    PVsn = #version{page = PUrl, version = PV},
    CVsn = #version{page = CUrl, version = CV},
    Rec = #dirty_notify_back_in{parent = Parent, child = Child,
                                change = "unregister",
                                biccie = Biccie, parent_vsn = PVsn,
                                child_vsn = CVsn},
    mark_dirty(ChildSite, Rec).

%get_refXs(List) -> get_refXs(List, []).

%get_refXs([], Acc)              -> hslists:uniq(Acc);
%get_refXs([{RefX, _} | T], Acc) -> get_refXs(T, [RefX | Acc]).

delete_parent_links(RefX) ->
    ok = delete_local_parents(RefX),
    ok = delete_remote_parents(RefX).

get_refs_below2(RefX, MinX, MaxX, Y) ->
    #refX{site = S, path = P} = RefX,
    Obj = {cell, {'$1', '$2'}},
    Head = #local_objs{path = P, obj = Obj, _ ='_'},
    Cond = case MinX of
               MaxX -> [{'and', {'>', '$2', Y}, {'==', '$1', MinX}}];
               _    -> [{'and', {'>', '$2', Y}, {'>', '$1', MinX},
                         {'=<', '$1', MaxX}}]
           end,
    Body = ['$_'],
    Idxs = get_local_idxs(S, {Head, Cond, Body}),
    RefXs1 = local_objs_to_refXs(S, Idxs),
    RefXs2 = get_local_links_refs(S, RefXs1),
    RefXs = lists:merge([RefXs1, RefXs2]),
    hslists:uniq(RefXs).    

get_refs_right2(RefX, X, MinY, MaxY) ->
    #refX{site = S, path = P} = RefX,
    Obj = {cell, {'$1', '$2'}},
    Head = #local_objs{path = P, obj = Obj, _ = '_'},
    Cond = case MinY of
               MaxY -> [{'and', {'>', '$1', X}, {'==', '$2', MinY}}];
               _    -> [{'and', {'>', '$1', X}, {'>', '$2', MinY},
                         {'=<', '$2', MaxY}}]
           end,
    Body = ['$_'],
    Idxs = get_local_idxs(S, {Head, Cond, Body}),
    RefXs1 = local_objs_to_refXs(S, Idxs),
    RefXs2 = get_local_links_refs(S, RefXs1),
    RefXs = lists:merge([RefXs1, RefXs2]),
    hslists:uniq(RefXs).

get_local_links_refs(Site, {Head, Cond, Body}) ->
    Head2 = trans(Site, Head),
    Table = trans(Site, local_cell_link),
    Return = trans_back(mnesia:select(Table, [{Head2, Cond, Body}])),
    % now tidy them up, get the relevant refX's and dedup them all...
    Fun = fun(#local_cell_link{parentidx = P}, Acc) ->
                  [local_idx_to_refX(Site, P) | Acc]
          end,
    Return1 = lists:foldl(Fun, [], Return),
    hslists:uniq(Return1).

make_page_match(Site, RefX, RecordName) ->
    #refX{site = S, path = P} = RefX,
    Match  = ms_util:make_ms(refX, [{site, S}, {path, P},
                                    {obj, {cell, {'$1', '$2'}}}]),
    ms_util:make_ms(RecordName, [{site_and_parent, {Site, Match}}]).

make_range_match_ref(RefX) ->
    #refX{path = P, obj = Range} = RefX,
    {range, {X1, Y1, X2, Y2}} = Range,
    {MinX, MaxX} = if
                       X1 >= X2 -> {X2, X1};
                       X1 <  X2 -> {X1, X2}
                   end,
    {MinY, MaxY} = if
                       Y1 >= Y2 -> {Y2, Y1};
                       Y1 <  Y2 -> {Y1, Y2}
                   end,
    Obj = {cell, {'$1', '$2'}},
    Match = #local_objs{path = P, obj = Obj, _ = '_'},
    % build a conditional for selecting cells
    % also need to build a cond for the attributes
    Cond = [{'and', {'>=', '$1', MinX }, {'=<', '$1', MaxX},
             {'>=', '$2', MinY}, {'=<', '$2', MaxY}}],
    Body = ['$_'],
    {Match, Cond, Body}.

make_col_match_ref(RefX) ->
    #refX{path = P, obj = Col} = RefX,
    {column, {X1, X2}} = Col,
    {MinX, MaxX} = if
                       X1 >= X2 -> {X2, X1};
                       X1 <  X2 -> {X1, X2}
                   end,
    Obj = {cell, {'$1', '_'}},
    Match = #local_objs{path = P, obj = Obj, _ = '_'},
    % build a conditional for selecting cells
    % also need to build a cond for the attributes
    Cond = [{'>=', '$1', MinX }, {'=<', '$1', MaxX}],
    Body = ['$_'],
    {Match, Cond, Body}.

make_row_match_ref(RefX) ->
    #refX{path = P, obj = Row} = RefX,
    {row, {Y1, Y2}} = Row,
    {MinY, MaxY} = if
                       Y1 >= Y2 -> {Y2, Y1};
                       Y1 <  Y2 -> {Y1, Y2}
                   end,
    Obj = {cell, {'_', '$1'}},
    Match = #local_objs{path = P, obj = Obj, _ = '_'},

    % build a conditional for selecting cells
    % also need to build a cond for the attributes
    Cond = [{'>=', '$1', MinY }, {'=<', '$1', MaxY}],
    Body = ['$_'],
    {Match, Cond, Body}.

make_page_match_ref(RefX) ->
    #refX{path = P} = RefX,
    Obj = {cell, {'_', '_'}},
    H = #local_objs{path = P, obj = Obj, _ = '_'},
    {H, [], ['$_']}.

get_attr_keys(List)  -> get_attr_keys(List, []).

get_attr_keys([], Acc)                       -> Acc;
get_attr_keys([{_RefX, {Key, _V}} | T], Acc) -> get_attr_keys(T, [Key | Acc]).

% make_cell makes a cell with dollars and stuff based on the offset
make_cell(false, X, XOffset, false, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, false, Y, YOffset) ->
    [$$] ++ tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(false, X, XOffset, true, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, true, Y, YOffset)  -> 
    [$$] ++ tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset).

% drag'n'drop_cell drags and drops a cell ignoring offsets
% if the row/column part is fixed with a dollar
drag_n_drop_cell(false, X, XOffset, false, Y, YOffset) ->
    if
        (X + XOffset) <  1 orelse  (Y + YOffset) <  1 -> "#REF!";
        (X + XOffset) >= 1 andalso (Y + YOffset) >= 1 ->
            tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset)
    end;
drag_n_drop_cell(true, X, _XOffset, false, Y, YOffset) ->
    if
        X <  1 orelse ( Y + YOffset) <  1 -> "#REF!";
        X >= 1 andalso (Y + YOffset) >= 1 ->
            [$$] ++ tconv:to_b26(X) ++ tconv:to_s(Y + YOffset)
    end;
drag_n_drop_cell(false, X, XOffset, true, Y, _YOffset) ->
    if
        (X + XOffset) <  1 orelse  Y <  1 -> "#REF!";
        (X + XOffset) >= 1 andalso Y >= 1 ->
            tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y)
    end;
drag_n_drop_cell(true, X, _XOffset, true, Y, _YOffset)  -> 
    if
        X <  1 orelse  Y <  1 -> "#REF!";
        X >= 1 andalso Y >= 1 ->
            [$$] ++ tconv:to_b26(X) ++ [$$] ++ tconv:to_s(Y)
    end.

make_col(false, X) -> tconv:to_b26(X);
make_col(true,  X) -> [$$] ++ X.

make_row(false, Y) -> tconv:to_s(Y);
make_row(true,  Y) -> [$$] ++ tconv:to_s(Y).
    

diff( FX, _FY,  TX, _TY, horizontal) -> TX - FX;
diff(_FX,  FY, _TX,  TY, vertical) -> TY - FY.

% make formula creates a new formula, but also returns a status.
% Status can be [clean | dirty]
% Formulae that return dirty should be marked dirty at recalc
% time as they will not recalc to the real value
% The function 'INDIRECT' is an example of such a function
make_formula(Toks) ->
    mk_f(Toks, {clean, []}).

%% this function needs to be extended...
mk_f([], {St, A})                         -> {St, "="++lists:flatten(lists:reverse(A))};
mk_f([{errval, '#REF!'} | T], {St, A})    -> mk_f(T, {St, ["#REF!" | A]});
mk_f([{deref, Text}     | T], {St, A})    -> mk_f(T, {St, [Text | A]});
% special infering of division
mk_f([?cellref, ?cellref2| T], {St, A})   -> mk_f(T, {St, [R2, "/", R | A]});
mk_f([{int, I}, ?cellref| T], {St, A})      -> mk_f(T, {St, [R, "/", integer_to_list(I) | A]});
mk_f([{float, F, _}, ?cellref| T], {St, A}) -> mk_f(T, {St, [R, "/", float_to_list(F) | A]});
mk_f([{')'}, ?cellref| T], {St, A})         -> mk_f(T, {St, [R, "/", ")" | A]});
%% order matters - now detecting 'root' cells
mk_f([?rootcellref       | T], {St, A})     -> mk_f(T, {St, ["/" ++ R | A]});
mk_f([?cellref           | T], {St, A})   -> mk_f(T, {St, [R | A]});
mk_f([?rangeref          | T], {St, A})   -> mk_f(T, {St, [R | A]});
mk_f([?namedexpr         | T], {St, A})   -> mk_f(T, {St, [P ++ N | A]});
mk_f([{bool, H}          | T], {St, A})   -> mk_f(T, {St, [atom_to_list(H) | A]});
mk_f([{atom, H}          | T], {St, A})   -> mk_f(T, {St, [atom_to_list(H) | A]});
mk_f([{int, I}           | T], {St, A})   -> mk_f(T, {St, [integer_to_list(I) | A]});
mk_f([{float, {F, _OrigStr}} | T], {St, A}) -> mk_f(T, {St, [float_to_list(F) | A]});
mk_f([{formula, S}       | T], {St, A})   -> mk_f(T, {St, [S | A]});
mk_f([{str, S}           | T], {St, A})   -> mk_f(T, {St, [$", S, $" | A]});
mk_f([{recalc, S}        | T], {_St, A})  -> mk_f(T, {dirty, [S | A]});
mk_f([{name, "INDIRECT"} | T], {_St, A})  -> mk_f(T, {dirty, ["INDIRECT" | A]});
mk_f([{name, S}          | T], {St, A})   -> mk_f(T, {St, [S | A]});
mk_f([{H}                | T], {St, A})   -> mk_f(T, {St, [atom_to_list(H) | A]}).

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

offset_with_ranges(Toks, Cell, From, Offset) ->
    offset_with_ranges1(Toks, Cell, From, Offset, []).

offset_with_ranges1([], _Cell, _From, _Offset, Acc) ->
    lists:reverse(Acc);
offset_with_ranges1([#rangeref{path = Path, text = Text} = H | T],
                    Cell, #refX{path = FromPath} = From, Offset, Acc) ->
    #refX{path = CPath} = Cell,
    PathCompare = muin_util:walk_path(CPath, Path),
    Range = muin_util:just_ref(Text),
    Prefix = case muin_util:just_path(Text) of
                 "/"     -> "";
                 Other   -> Other
             end,
    [Cell1|[Cell2]] = string:tokens(Range, ":"),
    {X1D, X1, Y1D, Y1} = parse_cell(Cell1),
    {X2D, X2, Y2D, Y2} = parse_cell(Cell2),
    NewText = case PathCompare of
                  FromPath -> make_new_range(Prefix, Cell1, Cell2,
                                             {X1D, X1, Y1D, Y1},
                                             {X2D, X2, Y2D, Y2},
                                             From, Offset);
                  _        -> Text
              end,
    NewAcc = H#rangeref{text = NewText},
    offset_with_ranges1(T, Cell, From, Offset, [NewAcc | Acc]);
offset_with_ranges1([#cellref{path = Path, text = Text} = H | T],
                    Cell, #refX{path = FromPath} = From, {XO, YO}, Acc) ->
    #refX{path = CPath} = Cell,
    Prefix = case muin_util:just_path(Text) of
                 "/"   -> "";
                 Other -> Other
             end,
    {XDollar, X, YDollar, Y} = parse_cell(muin_util:just_ref(Text)),
    PathCompare = muin_util:walk_path(CPath, Path),
    NewCell =
        case PathCompare of
            FromPath -> make_cell(XDollar, X, XO, YDollar, Y, YO);
            _        -> Text
        end,
    NewAcc = H#cellref{text = Prefix ++ NewCell},    
    offset_with_ranges1(T, Cell, From, {XO, YO}, [NewAcc | Acc]);
offset_with_ranges1([H | T], Cell, From, Offset, Acc) ->
    offset_with_ranges1(T, Cell, From, Offset, [H | Acc]).

make_new_range(Prefix, Cell1, Cell2, {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {cell, {X, Y}}} = _From, {XO, YO}) ->
    NC1 =
        case {X1, Y1} of
            {X, Y} -> make_cell(X1D, X1, XO, Y1D, Y1, YO);
            _      -> Cell1
        end,
    NC2 =
        case {X2, Y2} of
            {X, Y} -> make_cell(X2D, X2, XO, Y2D, Y2, YO);
            _      -> Cell2
        end,
    Prefix ++ NC1 ++ ":" ++ NC2;
% this clause is for a column delete
make_new_range(Prefix, Cell1, Cell2, {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {column, {XA, XB}}} = _From, {XO, 0 = _YO})
  when (XO < 0)->
    {NC1, NC2} = if
                     % if both columns are to the left of the range then
                     % offset both
                     (XA < X1) andalso (XB < X1) ->
                         C1 = make_cell(X1D, X1, XO, Y1D, Y1, 0),
                         C2 = make_cell(X2D, X2, XO, Y2D, Y2, 0),
                         {C1, C2};
                     % if both are to the right, then offset neither
                     (XA > X2) andalso (XB > X2) -> {Cell1, Cell2};
                     % if the delete clips the left hand side then set the left-hand
                     % of the range to the left most value of the clip
                     (XA =< X1) andalso (XB >= X1) andalso (XB =< X2) ->
                         C1 = make_cell(X1D, XA, 0,  Y1D, Y1, 0),
                         C2 = make_cell(X2D, X2, XO, Y2D, Y2, 0),
                         {C1, C2};
                     % if the delete clips the right hand site then set the right-hand
                     % of the range to the LEFT most value of the clip
                     (XA >= X1) andalso (XA =< X2) andalso (XB >= X2) ->
                         C2 = make_cell(X2D, XA, 0, Y2D, Y2, 0),
                         {Cell1, C2};
                     % if the delete intersects the range then offset the right
                     % hand only
                     (XA >= X1) andalso (XB =< X2) ->
                         C2 = make_cell(X2D, X2, XO, Y2D, Y2, 0),
                         {Cell1, C2}
                 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
% this clause is for a column insert
make_new_range(Prefix, Cell1, Cell2, {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {column, {XA, XB}}} = _From, {XO, 0 = _YO})
  when (XO > 0)->
    {NC1, NC2} = if
                     % if both columns are to the left of the range
                     % then offset them both
                     (XA < X1) andalso (XB < X1) ->
                         C1 = make_cell(X1D, X1, XO, Y1D, Y1, 0),
                         C2 = make_cell(X2D, X2, XO, Y2D, Y2, 0),
                         {C1, C2};
                     % if both columns are to the right of the range
                     % then offset neither
                     (XA > X2) andalso (XB > X2) -> {Cell1, Cell2};
                     % if the insert clips the left hand side then offset
                     % both sides
                     (XA =< X1) andalso (XB >= X1) andalso (XB =< X2) ->
                         C1 = make_cell(X1D, X1, XO, Y1D, Y1, 0),
                         C2 = make_cell(X2D, X2, XO, Y2D, Y2, 0),
                         {C1, C2};
                     % if the insert clips the right hand site then set the right-hand
                     % of the range to the offset of the right hand of the clip
                     (XA >= X1) andalso (XA =< X2) andalso (XB >= X2) ->
                         C2 = make_cell(X2D, XB, XO, Y2D, Y2, 0),
                         {Cell1, C2};
                     % if the delete intersects the range then offset the right
                     % hand only
                     (XA >= X1) andalso (XB =< X2) ->
                         C2 = make_cell(X2D, X2, XO, Y2D, Y2, 0),
                         {Cell1, C2}
                 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
% this clause is for a row delete
make_new_range(Prefix, Cell1, Cell2, {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {row, {YA, YB}}} = _From, {0 = _XO, YO})
  when (YO < 0) ->
    {NC1, NC2} = if
                     % if both rows are above the range then offset them both
                     (YA < Y1) andalso (YB < Y1) ->
                         C1 = make_cell(X1D, X1, 0, Y1D, Y1, YO),
                         C2 = make_cell(X2D, X2, 0, Y2D, Y2, YO),
                         {C1, C2};
                     % if both rows are below the range then offset neither
                     (YA > Y2) andalso (YB > Y2) -> {Cell1, Cell2};
                     % if the delete clips the top then set the top of the range
                     % to the top of the clip
                     (YA =< Y1) andalso (YB >= Y1) andalso (YB =< Y2) ->
                         C1 = make_cell(X1D, X1, 0, Y1D, YA, 0),
                         C2 = make_cell(X2D, X2, 0, Y2D, Y2, YO),
                         {C1, C2};
                     % if the delete clips the bottom then set the bottom of the 
                     % range to the top of the clip
                     (YA >= Y1) andalso (YA =< Y2) andalso (YB >= Y2) ->
                         C2 = make_cell(X2D, X2, 0, Y2D, YA, 0),
                         {Cell1, C2};
                     % if the delete intersects the range then offset the bottom
                     % of the range only
                     (YA >= Y1) andalso (YB =< Y2) ->
                         C2 = make_cell(X2D, X2, 0, Y2D, Y2, YO),
                         {Cell1, C2}
                 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
% this clause is for a row insert
make_new_range(Prefix, Cell1, Cell2, {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {row, {YA, YB}}} = _From, {0 = _XO, YO})
  when (YO > 0)->
    {NC1, NC2} = if
                     % if both rows are above the range offset them both
                     (YA < Y1) andalso (YB < Y1) ->
                         C1 = make_cell(X1D, X1, 0, Y1D, Y1, YO),
                         C2 = make_cell(X2D, X2, 0, Y2D, Y2, YO),
                         {C1, C2};
                     % if both rows are below the range offset neither
                     (YA > Y2) andalso (YB > Y2) -> {Cell1, Cell2};
                     % if the insert clips the top then offset the whole range
                     (YA =< Y1) andalso (YB >= Y1) andalso (YB =< Y2) ->
                         C1 = make_cell(X1D, X1, 0, Y1D, Y1, 0),
                         C2 = make_cell(X2D, X2, 0, Y2D, Y2, YO),
                         {C1, C2};
                     % if the insert clips the bottom then set the new bottom
                     % to the bottom of the clip
                     (YA >= Y1) andalso (YA =< Y2) andalso (YB >= Y2) ->
                         C2 = make_cell(X2D, X2, 0, Y2D, YB, 0),
                         {Cell1, C2};
                     % if the delete intersects the range then offset the bottom
                     % only
                     (YA >= Y1) andalso (YB =< Y2) ->
                         C2 = make_cell(X2D, X2, 0, Y2D, Y2, YO),
                         {Cell1, C2}
                 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
make_new_range(_Prefix, _Cell1, _Cell2, {_X1D, _X1, _Y1D, _Y1},
               {_X2D, _X2, _Y2D, _Y2},
               #refX{obj = {range, _}} = _From, {_XO, _YO}) ->
    exit("make_new_range for range not written yet!");
make_new_range(_Prefix, _Cell1, _Cell2, {_X1D, _X1, _Y1D, _Y1},
               {_X2D, _X2, _Y2D, _Y2},
               #refX{obj = {cell, _}} = _From, {_XO, _YO}) ->
    exit("make_new_range for not written yet!").        

offset(#refX{obj = {cell, {X, Y}}} = RefX, {XO, YO}) ->
    RefX#refX{obj = {cell, {X + XO, Y + YO}}}.

% used in copy'n'paste, drag'n'drops etc...
d_n_d_c_n_p_offset(Toks, XOffset, YOffset) ->
    d_n_d_c_n_p_offset1(Toks, XOffset, YOffset, []).

d_n_d_c_n_p_offset1([], _XOffset, _YOffset, Acc) -> lists:reverse(Acc);
d_n_d_c_n_p_offset1([#cellref{text = Text}
         = H | T], XOffset, YOffset, Acc) ->
    Cell = muin_util:just_ref(Text),
    Prefix = case muin_util:just_path(Text) of
                 "/"   -> "";
                 Other -> Other
             end,
    {XDollar, X, YDollar, Y} = parse_cell(Cell),
    NewCell = drag_n_drop_cell(XDollar, X, XOffset, YDollar, Y, YOffset),
    NewRef = H#cellref{text = Prefix ++ NewCell},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewRef | Acc]);
d_n_d_c_n_p_offset1([#rangeref{text = Text} = H | T], XOffset, YOffset, Acc) ->
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
                  NC1 ==  "#REF!" orelse  NC2 ==  "#REF!" ->
                      "#REF!";
                  NC1 =/= "#REF!" andalso NC2 =/= "#REF!" ->
                      Pf ++ NC1 ++ ":" ++ NC2
              end,
    NewR = H#rangeref{text = NewText},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewR | Acc]);
d_n_d_c_n_p_offset1([H | T], XOffset, YOffset, Acc) ->
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [H | Acc]).

filter_for_drag_n_drop(List) -> fl(List, [], []).

fl([], A, B)                                  -> {A, B};
fl([{_, {"value", _}}  | T], A, B)            -> fl(T, A, B);
fl([{_, {"rawvalue", _}}| T], A, B)           -> fl(T, A, B);
fl([{_, {"parents", _}} | T], A, B)           -> fl(T, A, B);
fl([{_, {"__dependency-tree", _}}| T], A, B)  -> fl(T, A, B);
fl([{_, {"__ast", _}} | T], A, B)             -> fl(T, A, B);
fl([{_, {"formula", V}}| T], A, B)            -> fl(T, [V | A], B);
fl([H | T], A, B)                             -> fl(T, A, [H | B]).

get_local_parents(Site, List) -> get_l_p(Site, List, []).

get_l_p(_Site, [], Acc) -> Acc;
get_l_p(Site, [#local_cell_link{parentidx = P} | T], Acc) ->
    get_l_p(Site, T, [local_idx_to_refX(Site, P) | Acc]).

get_local_children(Site, List) -> get_l_c(Site, List, []).

get_l_c(_Site, [], Acc) -> Acc;
get_l_c(Site, [#local_cell_link{childidx = C} | T], Acc) ->
    RefX = local_idx_to_refX(Site, C),
    get_l_c(Site, T, [RefX | Acc]).

get_remote_parents(List) -> get_r_p(List, []).

get_r_p([], Acc) -> Acc;
get_r_p([#remote_cell_link{parent = P} | T], Acc) ->
    get_r_p(T, [P | Acc]).

get_remote_children(List) -> get_r_c(List, []).

get_r_c([], Acc) -> Acc;

get_r_c([#remote_cell_link{child = C} | T], Acc) ->
    get_r_c(T, [C | Acc]).

delete_remote_parents(#refX{site = Site} = Child) ->
    Match = #remote_cell_link{child = Child, type = incoming, _ = '_'}, 
    Table = trans(Site, remote_cell_link),
    Parents = mnesia:match_object(Table, Match, read),
    % unregister the hypernumbers
    Fun = fun(X) ->
                  #remote_cell_link{parent = P, child = C, type = incoming} = X,
                  Rec = #remote_cell_link{parent = P, child = C,
                                          type = incoming},
                  ok = delete_recs_new(Site, [Rec]),
                  unregister_inc_hn(P, C)
          end,
    [ok = Fun(X) || X <- Parents],
    delete_recs(Site, Parents).

delete_local_parents(#refX{site = Site} = Child) ->
    CIdx = read_local_item_index(Child),
    Table = trans(Site, local_cell_link),
    Parents = mnesia:index_read(Table, CIdx, childidx),
    delete_recs(Site, Parents).

write_local_parents(#refX{site = Site} = Child, List) ->
    Fun = fun(P) ->
                  CIdx = get_local_item_index(Child),
                  PIdx = get_local_item_index(P),
                  NewRec = #local_cell_link{childidx = CIdx, parentidx = PIdx},
                  ok = mnesia:write(trans(Site, local_cell_link), NewRec, write)
          end,
    [Fun(X) || X <- List],
    ok.
            
get_content_attrs(List) -> get_content_attrs(List, []).

get_content_attrs([], Acc)      -> Acc;
get_content_attrs([H | T], Acc) ->
    {_, {Key, _V}} = H,
    case Key of
        "formula"             -> get_content_attrs(T, [H | Acc]);
        "rawvalue"            -> get_content_attrs(T, [H | Acc]);
        "value"               -> get_content_attrs(T, [H | Acc]);
        "overwrite-color"     -> get_content_attrs(T, [H | Acc]);
        "__ast"               -> get_content_attrs(T, [H | Acc]);
        "__recompile"         -> get_content_attrs(T, [H | Acc]);
        "__shared"            -> get_content_attrs(T, [H | Acc]);
        "__area"              -> get_content_attrs(T, [H | Acc]);
        "__dependency-tree"   -> get_content_attrs(T, [H | Acc]);
        "parents"             -> get_content_attrs(T, [H | Acc]);
        _                     -> get_content_attrs(T, Acc)
    end.

shift_remote_links2(_Site, [], _To) -> ok;
shift_remote_links2(Site, [H | T], To) ->
    % now read delete the old remote link
    NewLink = H#remote_cell_link{parent = To},
    ok = mnesia:delete_object(trans(Site, remote_cell_link), H),
    ok = mnesia:write(trans(Site, NewLink)),
    shift_remote_links2(Site, T, To).

deref_child(#refX{site = _S} = Child, DeRefX) ->
    [{Child, {"formula", Formula}}] = read_attrs(Child, ["formula"], write),
    {Status, NewFormula} = deref(Child, Formula, DeRefX),
    ok = write_attr(Child, {"formula", NewFormula}),
    
    case Status of
        dirty -> {dirty, Child};
        clean -> []
    end.
                

% dereferences a formula
deref(Child, [$=|Formula], DeRefX) when is_record(DeRefX, refX) ->
    {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {1, 1}),
    NewToks = deref1(Child, Toks, DeRefX, []),
    make_formula(NewToks).

deref1(_Child, [], _DeRefX, Acc) -> lists:reverse(Acc);
deref1(Child, [#rangeref{text = Text} | T], DeRefX, Acc) ->
    % only deref the range if it is completely obliterated by the deletion
    #refX{obj = Obj1} = DeRefX,
    Range = muin_util:just_ref(Text),
    Prefix = case muin_util:just_path(Text) of
                 "/" -> [];
                 Pre -> Pre
             end,
    Obj2 = hn_util:parse_ref(Range),
    NewTok = case deref_overlap(Range, Obj1, Obj2) of
                 {deref, "#REF!"} -> {deref, Prefix ++ "#REF!"};
                 {recalc, Str}    -> {recalc, Prefix ++ Str};
                 {formula, Str}   -> {formula, Prefix ++ Str}
                 % O              -> io:format("O is ~p~n", [O]),
                 %                   Prefix ++ O
             end,
    deref1(Child, T, DeRefX, [NewTok | Acc]);
deref1(Child, [#cellref{path = Path, text = Text} = H | T], DeRefX, Acc) ->
    NewTok = deref2(Child, H, Text, Path, DeRefX),
    deref1(Child, T, DeRefX, [NewTok | Acc]);
deref1(Child, [H | T], DeRefX, Acc) ->
    deref1(Child, T, DeRefX, [H | Acc]).

% sometimes Text has a prepended slash
deref2(Child, H, [$/|Text], Path, DeRefX) ->
    Deref2 = deref2(Child, H, Text, Path, DeRefX),
    case Deref2 of
        H              -> H;
        {deref,   Str} -> {deref,   "/" ++ Str};
        {recalc,  Str} -> {recalc,  "/" ++ Str};
        {formula, Str} -> {formula, "/" ++ Str}
    end;
% special case for ambiguous parsing of division
% this matches on cases like =a1/b3
deref2(_Child, _H, Text, "/", DeRefX) ->
    #refX{obj = Obj1} = DeRefX,
    Obj2 = hn_util:parse_ref(Text),
    deref_overlap(Text, Obj1, Obj2);
deref2(Child, H, Text, Path, DeRefX) ->
    #refX{path = CPath} = Child,
    #refX{path = DPath, obj = Obj1} = DeRefX,
    PathCompare = muin_util:walk_path(CPath, Path),
    case PathCompare of
        DPath -> case Path of
                     "./" -> {deref, "#REF!"};
                     _P   -> S1 = muin_util:just_path(Text),
                             S2 = muin_util:just_ref(Text),
                             Obj2 = hn_util:parse_ref(S2),
                             case deref_overlap(S2, Obj1, Obj2) of
                                 {deref, "#REF!"} -> {deref, S1 ++ "#REF!"};
                                 O                -> S1 ++ O
                             end
                 end;
        _Else -> H
    end.

% if Obj1 completely subsumes Obj2 then the reference to Obj2 should 
% be dereferenced (return 'deref')
% % if Obj1 partially subsumes Obj2 then the reference to Obj2 should
% be rewitten (return 'rewrite')
% if there is partial or no overlap then return 'unchanged'
deref_overlap(Text, Obj1, Obj2) ->
    % the first thing we do is check each corner of Objs2 to see if it is inside
    % Obj1. Depending on the pattern of corners we rewrite the formula
    % - if all 4 corners are in the delete area the range must be dereferenced
    % - if 2 corners are in the delete area the formula must be rewritten
    % - if 1 corner is in the delete are the range must be deferenced
    %   because there is no way to rewrite it...
    % - (if 3 corners are in the delete area then the laws of Euclidean
    %   geometry have broken down and the end times are probably upon us
    %   so I would flee for my life sinner!)
    % 
    % BUT if all 4 corners are outside the delete area we need to check again:
    % - if the delete area is wholy inside the range then the range must be deferenced
    % - if the delete area transpierces the range then the range must be rewritten
    {X1,  Y1,   X2,  Y2}  = expand(Obj1),
    {XX1, YY1,  XX2, YY2} = expand(Obj2),
    IntTL = intersect(XX1, YY1, X1, Y1, X2, Y2),
    IntBL = intersect(XX1, YY2, X1, Y1, X2, Y2),
    IntTR = intersect(XX2, YY1, X1, Y1, X2, Y2),
    IntBR = intersect(XX2, YY2, X1, Y1, X2, Y2),
    case {IntTL, IntBL, IntTR, IntBR} of
        % all included - deref!
        {in,  in,  in,  in}  -> {deref, "#REF!"};
        % none included you need to recheck in case the delete area
        % is contained in, or transects the target area
        {out, out, out, out} -> recheck_overlay(Text, Obj1, Obj2);
        % one corner included - deref!
        {in,  out, out, out} -> {deref, "#REF!"};
        {out, in,  out, out} -> {deref, "#REF!"};
        {out, out, in,  out} -> {deref, "#REF!"};
        {out, out, out, in}  -> {deref, "#REF!"};
        % two corners included rewrite
        {in,  in,  out, out} -> rewrite(X1, X2, Obj2, Text, left); % left del
        {out, out, in,  in}  -> rewrite(X1, Obj2, Text, right);
        {in,  out, in,  out} -> rewrite(Y1, Y2, Obj2, Text, top);  % top del
        {out, in,  out, in}  -> rewrite(Y1, Obj2, Text, bottom);
        % transects are column/row intersects
        {transect, transect, out, out} -> rewrite(X1, X2, Obj2, Text, left); % left del
        {out, out, transect, transect} -> rewrite(X1, Obj2, Text, right);
        {transect, out, transect, out} -> rewrite(Y1, Y2, Obj2, Text, top);  % top del
        {out, transect, out, transect} -> rewrite(Y1, Obj2, Text, bottom);
        {transect, transect, transect, transect} -> {deref, "#REF!"}
    end.

% this first clause catches rows/columns where the deleting object is a cell/range
% in none of these cases does the formula dereference...
intersect(A1, A2, X1, Y1, X2, Y2)
  when (is_atom(A1) orelse is_atom(A2)) andalso
       (is_integer(X1) andalso is_integer(Y1)
        andalso is_integer(X2) andalso is_integer(Y2)) -> out;
% cols/rows never dereference
intersect(A1, Y1, X1, A2, X2, A3)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(Y1) andalso is_integer(X1) andalso is_integer(X2)) -> out;
% rows/cols never deference
intersect(X1, A1, A2, Y1, A3, Y2)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(X1) andalso is_integer(Y1) andalso is_integer(Y2)) -> out;
% page deletes always dereference
intersect(_XX1, _YY1, zero, zero, inf, inf) ->
    out;
% this is a row-row comparison
intersect(Type, YY1, zero, Y1, inf, Y2)
  when ((Type == zero) orelse (Type == inf)) ->
    if
        (YY1 >= Y1), (YY1 =< Y2) -> transect;
        true                     -> out
    end;
% this is a col-col comparison
intersect(XX1, Type, X1, zero, X2, inf)
  when ((Type == zero) orelse (Type == inf)) ->
    if
        (XX1 >= X1), (XX1 =< X2) -> transect;
        true                     -> out
    end;
intersect(XX1, YY1, X1, Y1, X2, Y2) ->    
    if
        % check for cell/range intersections
        (XX1 >= X1),   (XX1 =< X2), (YY1 >= Y1),  (YY1 =< Y2) -> in;
        % order matters - first check for rows that are included
        (XX1 >= X1),   (XX1 =< X2), (zero == Y1), (inf == Y2) -> in;
        (zero == X1),  (inf == X2), (YY1 >= Y1),  (YY1 =< Y2) -> in;
        % now check for partial intersections
        (XX1 == zero), (YY1 >= Y1), (YY1 =< Y2)               -> in;
        (XX1 == inf),  (YY1 >= Y1), (YY1 =< Y2)               -> in;
        (YY1 == zero), (XX1 >= X1), (XX1 =< X2)               -> in;
        (YY1 == inf),  (XX1 >= X1), (XX1 =< X2)               -> in;
        true                                                  -> out
    end.         

% rewrite/5
rewrite(X1O, X2O, {range, _}, Text, left)   ->
    % io:format("in rewrite (1) Text is ~p~n", [Text]),
    {XD1, _X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1O, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - (X2O - X1O + 1)), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(X1O, X2O, {column, _}, Text, left)   ->
    {XD1, _X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1O) ++ ":" ++ make_col(XD2, (X2 - (X2O - X1O + 1))),
    {recalc, S};

rewrite(Y1O, Y2O, {range, _}, Text, top)   ->
    % io:format("in rewrite (3) Text is ~p~n", [Text]),
    {XD1, X1, YD1, _Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1O, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - (Y2O - Y1O + 1)), 0),
    {recalc, S};

rewrite(Y1O, Y2O, {row, _}, Text, top)   ->
    {YD1, _Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1O) ++ ":" ++ make_row(YD2, (Y2 - (Y2O - Y1O + 1))),
    {recalc, S}.

% rewrite/4
rewrite(XO, {range, _}, Text, right)  ->
    {XD1, X1, YD1, Y1, XD2, _X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (XO - 1), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(XO, {column, _}, Text, right)  ->
    {XD1, X1, XD2, _X2} = parse_cols(Text),
    S = make_col(XD1, X1) ++ ":" ++ make_col(XD2, (XO - 1)),
    {recalc, S};

rewrite(XO, {range, _}, Text, middle_column)  ->
    % io:format("in rewrite (7) Text is ~p~n", [Text]),
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - XO), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(XO, {column, _}, Text, middle)  ->
    {XD1, X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1) ++ ":" ++ make_col(XD2, (X2 - XO)),
    {recalc, S};

rewrite(YO, {range, _}, Text, bottom) ->
    % io:format("in rewrite (9) Text is ~p~n", [Text]),
    {XD1, X1, YD1, Y1, XD2, X2, YD2, _Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (YO - 1), 0),
    {recalc, S};

rewrite(YO, {row, _}, Text, bottom) ->
    {YD1, Y1, YD2, _Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (YO - 1)),
    {recalc, S};

rewrite(YO, {range, _}, Text, middle_row) ->
    % io:format("in rewrite (11) Text is ~p~n", [Text]),
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - YO), 0),
    {recalc, S};

rewrite(YO, {row, _}, Text, middle) ->
    {YD1, Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (Y2 - YO)),
    {recalc, S}.

% page deletes always derefence
recheck_overlay(_Text, {page, "/"}, _Target) -> {deref, "#REF!"};
% cell targets that have matched so far ain't gonna
recheck_overlay(Text, _DelX, {cell, _}) -> {formula, Text};
% cell deletes that haven't matched a row or column so far ain't gonna
recheck_overlay(Text, {cell, _}, {Type, _})
  when ((Type == row) orelse (Type == column)) -> {formula, Text};
% cols/rows cols/range comparisons always fail
recheck_overlay(Text, {Type, _}, {column, _})
  when ((Type == row) orelse (Type == range)) -> {formula, Text};
% rows/cols comparisons always fail
recheck_overlay(Text, {Type, _}, {row, _})
      when ((Type == column) orelse (Type == range)) -> {formula, Text};
% check a row/row
recheck_overlay(Text, {row, {X1, X2}}, {row, {XX1, XX2}} = Tgt) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2) ->
            rewrite((X2 - X1 + 1), Tgt, Text, middle);
        true ->
            {formula, Text}
    end;
% check a col/col
recheck_overlay(Text, {column, {Y1, Y2}}, {column, {YY1, YY2}} = Tgt) ->
    if
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) ->
            rewrite((Y2 - Y1 + 1), Tgt, Text, middle);
        true ->
            {formula, Text}
    end;
% check range/range
recheck_overlay(Text, {range, {X1, Y1, X2, Y2}}, {range, {XX1, YY1, XX2, YY2}}) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2),
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) -> {deref, "#REF!"};
        true                                               -> {formula, Text}
    end;
% check range/column
recheck_overlay(Text, {column, {X1, X2}}, {range, {XX1, _YY1, XX2, _YY2}} = Tgt) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2) ->
            rewrite((X2 - X1 + 1), Tgt, Text, middle_column);
        true -> {formula, Text}
    end;
% check range/row
recheck_overlay(Text, {row, {Y1, Y2}}, {range, {_XX1, YY1, _XX2, YY2}} = Tgt) ->
    if
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) ->
            rewrite((Y2 - Y1 + 1), Tgt, Text, middle_row);
        true -> {formula, Text}
    end.

expand({cell, {X, Y}})            -> {X, Y, X, Y};
expand({range, {X1, Y1, X2, Y2}}) -> {X1, Y1, X2, Y2};
expand({column, {X1, X2}})        -> {X1, zero, X2, inf}; % short for infinity
expand({row, {Y1, Y2}})           -> {zero, Y1, inf, Y2}; % short for infinity
expand({page, "/"})               -> {zero, inf, zero, inf}.

% different to offset_formula because it truncates ranges
offset_fm_w_rng(Cell, [$=|Formula], From, Offset) ->
    % the xfl_lexer:lex takes a cell address to lex against
    % in this case {1, 1} is used because the results of this
    % are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> NewToks = offset_with_ranges(Toks, Cell, From, Offset),
                         make_formula(NewToks);
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                   "formula is offset_fm_w_rng but "++
                                   "you do~n-~p~n", [Formula]),
                         % S2 = io_lib:format("~p", [Cell]),
                         % bits:log(S1 ++ " " ++ S2), 
                         {[], "=" ++ Formula}
    end;
offset_fm_w_rng(_Cell, Value, _From, _Offset) -> Value.

offset_formula(Formula, {XO, YO}) ->
    % the xfl_lexer:lex takes a cell address to lex against
    % in this case {1, 1} is used because the results of this
    % are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> NewToks = d_n_d_c_n_p_offset(Toks, XO, YO),
                         {_St, NewFormula} = make_formula(NewToks),
                         NewFormula;
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                   "formula is offset_formula but "++
                                   "you do~n-~p~n", [Formula]),
                         % bits:log(S1),
                         "=" ++ Formula
    end.

shift_dirty_notify_ins(#refX{site = Site} = From, To) ->
    case mnesia:wread({trans(Site, dirty_notify_in), From}) of
        []        -> ok;
        [DirtyHn] -> DirtyHn2 = trans_back(DirtyHn),
                     NewDirty = DirtyHn2#dirty_notify_in{parent = To},
            ok = mnesia:delete_object(trans(Site, DirtyHn)),
            ok = mnesia:write(trans(Site, NewDirty))
    end.

write_attr2(RefX, {"formula", Val}) ->
    case superparser:process(Val) of
        {formula, Fla}      -> write_formula1(RefX, Fla, Val);
        [NVal, Align, Frmt] -> write_formula2(RefX, Val, NVal, Align, Frmt)
    end.

write_formula1(RefX, Fla, Val) ->
    Rti = refX_to_rti(RefX, false),
    case muin:run_formula(Fla, Rti) of
        % TODO : Get rid of this, muin should return {error, Reason}?
        {ok, {_P, {error, error_in_formula}, _, _, _}} ->
            ?ERROR("invalid return from muin:run_formula ~p",[Val]),
            #refX{site = Site, path = Path, obj = R} = RefX,
            ok = remoting_reg:notify_error(Site, Path, R, error_in_formula,
                                           Val);
        {error, Error} ->
            #refX{site = Site, path = Path, obj = R} = RefX,
            ok = remoting_reg:notify_error(Site, Path, R,  Error, Val);
        {ok, {Pcode, Res, Deptree, Parents, Recompile}} ->
            Parxml = map(fun muin_link_to_simplexml/1, Parents),
            % Deptreexml = map(fun muin_link_to_simplexml/1, Deptree),
            ok = write_attr3(RefX, {"__ast", Pcode}),
            ok = write_attr3(RefX, {"__recompile", Recompile}),
            % write the default text align for the result
            ok = write_default_alignment(RefX, Res),
            write_cell(RefX, Res, Val, Parxml, Deptree)
    end.

write_formula2(RefX, OrigVal, {Type, Value}, {"text-align", Align}, Format) ->
    % now write out the actual cell
    Formula = case Type of
                  quote    -> [39 | Value];
                  datetime -> OrigVal;
                  float    -> OrigVal;
                  int      -> OrigVal;
                  _        -> hn_util:text(Value)
              end,
    ok = write_cell(RefX, Value, Formula, [], []),
    %% only write the default alignment if there is no style on this cell
    case read_attrs(RefX, ["style"], read) of
        [] -> ok = write_attr(RefX, {"text-align", Align});
        _  -> ok
    end,
    %?INFO("~p",[read_attrs(RefX, ["text-align"], read)]),
    % write out the format (if any)
    case Format of
        {"format", "null"} -> ok;
        {"format", F}      -> write_attr(RefX, {"format", F})
    end.

% if there is a style set for the cell don't write the default
% aignment
write_default_alignment(RefX, Res) ->
    case read_attrs(RefX, ["style"], read) of
        [] -> write_default_alignment1(RefX, Res);
        _  -> ok
    end.
    
write_default_alignment1(RefX, Res) when is_number(Res) ->
    write_attr(RefX, {"text-align" ,"right"});
write_default_alignment1(RefX, Res) when is_list(Res) ->
    write_attr(RefX, {"text-align" ,"left"});
%% this clause matches for booleans, dates and errors
write_default_alignment1(RefX, _Res)  ->
    write_attr(RefX, {"text-align" ,"center"}).

write_cell(RefX, Value, Formula, Parents, DepTree) when is_record(RefX, refX) ->
    
    % This function writes a cell out with all the trimings
    % 
    % The term 'old' refers to the values of these attributes for this
    % cell before this function is called.
    % 
    % The term 'new' refers to those values passed in as parameters to 
    % this function,
    % 
    % The order of work is this:
    % * overwrite the new formula
    % * overwrite the new raw value of the cell
    %   - the write_rawvalue function calls the format and applies
    %     it to the rawvalue to calculate the value and overwrite colour.
    %     It overwrite the new values of the following attributes:
    %     & rawvalue
    %     & value
    %     & format
    %     & 'overwrite-color's
    % * overwrites the new values of these attributes:
    %   - parents
    %   - '__dependency-tree'
    % * reads the old set of local links:
    %   - writes any local links that aren't already there
    %   - deletes any local links that are no longer there
    % * reads the old set of remote links:
    %   - writes any remote links that aren't already there
    %   - deletes any remote links that are no longer there
    % * marks this cell dirty
    {NewLocPs, NewRemotePs} = split_local_remote(Parents),

    % write the formula
    ok = write_attr3(RefX, {"formula", Formula}), 

    % now write the rawvalue, etc, etc
    ok = write_rawvalue(RefX, Value),

    % overwrite the parents and '__dependency-tree'
    Set = fun(X, {Key, []})  -> delete_if_attrs(X, Key);
             (X, {Key, Val}) -> write_attr(X, {Key, Val})
          end,

    Set(RefX, {"parents",           {xml, Parents}}),
    Set(RefX, {"__dependency-tree", DepTree}),

    % now do the local parents
    ok = delete_local_parents(RefX),
    ok = write_local_parents(RefX, NewLocPs),

    % now do the remote parents
    % this is a bit messier - if a cell is being updated to change a
    % formula I don't want to first delete the old link (and trigger
    % an UNREGISTRATION of the hypernumbers) and then REWRITE the
    % same remote link and trigger a REGISTRATION so I first read the links
    OldRemotePs = read_remote_parents(RefX, incoming),
    ok = update_rem_parents(RefX, OldRemotePs, NewRemotePs),

    % We need to know the calculcated value
    [{RefX, {"rawvalue", RawValue}}] = read_attrs(RefX, ["rawvalue"], read),
    ok = mark_cells_dirty(RefX),

    % mark this cell as a possible dirty hypernumber
    mark_notify_out_dirty(RefX, {new_value, RawValue, DepTree}).

split_parents(Old, New) -> split_parents1(lists:sort(Old),
                                          lists:sort(New), {[],[]}).

%% if we have run out of OldParents stick the rest of the News on the Write Acc
split_parents1([], New, {D, W}) ->
    {D, lists:merge([New, W])};
%% if NewParents have run out stick the rest of the Olds on the Delete Acc
split_parents1(Old, [], {D, W}) ->
    {lists:merge([Old, D]), W};
%% if the same record appears in Old and New neither delete nor write
split_parents1([H | T1], [H | T2], Acc) ->
    split_parents1(T1, T2, Acc);
%% for every unique old record - delete it
split_parents1([H | T], New, {D, W}) ->
    split_parents1(T, New, {[H | D], W}).

split_local_remote(List) -> split_local_remote1(List, {[], []}).

split_local_remote1([], Acc) -> Acc;
split_local_remote1([{_, [{_, "local"}], [Url]} | T], {A, B})  ->
    P2 = hn_util:url_to_refX(Url),
    split_local_remote1(T, {[P2 | A], B});
split_local_remote1([{_, [{_, "remote"}], [Url]} | T], {A, B}) ->
    P2 = hn_util:url_to_refX(Url),
    split_local_remote1(T, {A, [P2 | B]}).

write_rawvalue(RefX, Value) when is_record(RefX, refX) ->
    % first write the rawvalue
    ok = write_attr3(RefX, {"rawvalue", Value}),
    % now get the format that is to be applied
    % run the format and then stick the value into
    % the database
    {ok, Format} = read_inherited(RefX, "format", "General"),
    process_format(RefX, Format, Value).

process_format(RefX, Format, Value) when is_record(RefX, refX) ->
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {Color, V}} = format:run_format(Value, Output),
    % first write the formatted value
    ok = write_attr3(RefX, {"value", V}),
    % now write the overwrite colour that comes from the format
    ok = write_attr3(RefX, {"overwrite-color", atom_to_list(Color)}).

get_item_list(Type, RefX, Key, Acc) ->
    case traverse(Type, RefX, Key) of
        {last, []}                    -> {ok, Acc};
        {last, [#item{val = Val}]}    -> {ok,lists:append(Val, Acc)};
        {NType, NewRefX, []}          -> get_item_list(NType, NewRefX, Key, Acc);
        {NType, []}                   -> get_item_list(NType, RefX, Key, Acc);
        {NType, NewRefX, [#item{val = Val}]} ->
            get_item_list(NType, NewRefX, Key, lists:append(Val, Acc));
        {NType,[#item{val = Val}]}     -> 
            get_item_list(NType, RefX, Key, lists:append(Val, Acc))
    end.

return_first(Type, RefX, Key) ->
    case traverse(Type, RefX, Key) of
        {last, []}                           -> nomatch;
        {last, [#item{val = Val}]}           -> {ok, Val};
        {NType, []}                          -> return_first(NType, RefX, Key);
        {NType, NRefX, []}                   -> return_first(NType, NRefX, Key);
        {_NType, _NRefX, [#item{val = Val}]} -> {ok, Val};
        {_NType, [#item{val = Val}]}         -> {ok, Val}
    end.

traverse(cell, #refX{obj = {cell, _}} = RefX, Key) ->
    {range, match_ref(RefX, Key)};
traverse(range, #refX{obj = {range, _}} = RefX, Key) ->
    {page, match_ref(RefX, Key)};
traverse(range, #refX{obj = {cell, _}} = RefX, Key) ->
    V = case get_ranges(RefX#refX{obj = {page, "/"}}, Key) of
            []   -> [];
            List -> case filter_range(List, RefX) of
                        nomatch -> [];
                        Val     -> [Val]
                    end
        end,
    {row_col, V};
traverse(row_col, #refX{obj = {cell, {_X, Y}}} = RefX, Key) ->
    {column, match_ref(RefX#refX{obj = {row, {Y, Y}}}, Key)};
traverse(row, #refX{obj = {row, _}} = RefX, Key) ->
    {page, match_ref(RefX, Key)};
traverse(row, #refX{obj = {cell, {_X, Y}}} = RefX, Key) ->
    {page, match_ref(RefX#refX{obj = {row, {Y, Y}}}, Key)};
traverse(column, #refX{obj = {column, _}} = RefX, Key) ->
    {page, match_ref(RefX, Key)};
traverse(column, #refX{obj = {cell, {X, _Y}}} = RefX, Key) ->
    {page, match_ref(RefX#refX{obj= {column, {X, X}}}, Key)};
traverse(page, #refX{path=[]} = RefX, Key) ->
    {last, match_ref(RefX#refX{obj = {page,"/"}}, Key)};
traverse(page, RefX, Key) ->
    NewPath = hslists:init(RefX#refX.path),
    {page, RefX#refX{path = NewPath}, match_ref(RefX#refX{obj = {page, "/"}}, Key)}.

get_ranges(#refX{site = S, path = P, obj = {page, "/"}}, Key) ->
    Head = #local_objs{path = P, obj = {range, '_'}, idx = '$1'},
    List1 = mnesia:select(trans(S, local_objs), [{Head, [], ['$_']}]),
    List2 = [read_attrs(X, Key, read) || X <- List1],
    % now sort the results
    % now convert the list of local_objs into refX's
    Fun1 = fun({#local_objs{path = Path, obj = Obj, idx = Idx}, _KV}) ->
                   {#refX{site = S, path = Path, obj = Obj}, Idx}
           end,
    List3 = lists:keysort(2, lists:map(Fun1, List2)),
    Fun2 = fun(X, _Y) -> X end,
    [Fun2(X, Y) || {X, Y} <- List3].

filter_range([], _Cell)     -> nomatch;
filter_range([H | T], Cell) ->
    case hn_util:in_range(H#refX.obj, Cell#refX.obj) of
        true -> H;
        _    -> filter_range(T, Cell)
    end.

%% @doc Convert Parents and DependencyTree tuples as returned by 
%% Muin into SimpleXML.
muin_link_to_simplexml({Type, {S, P, X1, Y1}}) ->
    Url = hn_util:index_to_url({index, S, P, X1, Y1}),
    {url, [{type, Type}], [Url]}.

%% @doc Get the value of a named attribute, if it doesn't exist for address
%% check parent (cell -> range -> row -> column -> page -> root -> default)
match_ref(#refX{site = S} = RefX, Key) ->
    case read_local_item_index(RefX) of
        false -> [];
        Idx -> Table = trans(S, item),
               %% this function is only used in traverse to 
               %% recursively find the value of an attribute
               %% so it gets a read lock only..
               case mnesia:read(Table, Idx, read) of
                   []   -> [];
                   Recs -> IdxNo = ms_util2:get_index(item, key) + 1,
                           case lists:keysearch(Key, IdxNo, Recs) of
                               false           -> [];
                               {value, Return} -> [Return]
                           end
               end
    end.

make_or(Attrs, PlcHoldr)  -> make_clause(Attrs, PlcHoldr, 'or').
%make_and(Attrs, PlcHoldr) -> make_clause(Attrs, PlcHoldr, 'and').

make_clause(Attrs, PlcHoldr, Op) -> make_clause(Attrs, PlcHoldr, Op, []).

make_clause([], _, Op, Acc)      -> 
    case length(Acc) of
        0 -> [];   % no attributes get everything
        1 ->  Acc; % 1 attribute - no Op statement
        _ -> [list_to_tuple(lists:flatten([Op, Acc]))]
    end;
make_clause([H | T], PH, Op, A)  -> 
    make_clause(T, PH, Op, [{'==', PH, H} | A]).

delete_style_attr(#refX{site = S} = RefX, Key)  ->
    % this function works by overwriting the set style attribute in the
    % current style record with []
    [{RefX, {"style", Idx}}] = read_attrs(RefX, ["style"], write),
    PageRefX = RefX#refX{obj = {page, "/"}},
    Match = #styles{refX = PageRefX, index = Idx, _ = '_'},
    Table = trans(S, styles),
    [CurrentStyle] = mnesia:match_object(Table, Match, read),
    NewStyleIdx = get_style(RefX, CurrentStyle, Key, []),
    write_attr3(RefX, {"style", NewStyleIdx}).    

%% this function is called when a new attribute is set for a style
process_styles(RefX, {Name, Val}) when is_record(RefX, refX) ->
    
    NewSIdx = case read_attrs(RefX, ["style"], read) of 
                  []                       -> get_style(RefX, Name, Val);
                  [{RefX, {"style", Idx}}] -> get_style(RefX, Idx, Name, Val) 
              end,
    write_attr3(RefX, {"style", NewSIdx}).    

get_style(RefX, Name, Val) ->
    NoOfFields = ms_util2:no_of_fields(magic_style), 
    Index = ms_util2:get_index(magic_style, Name), 
    Style = make_tuple(magic_style, NoOfFields, Index, Val), 
    % Now write the style 
    write_style(RefX, Style). 

%% edits a style
get_style(#refX{site = Site} = RefX, StIdx, Name, Val) ->
    PageRefX = RefX#refX{obj = {page, "/"}},
    Match = #styles{refX = PageRefX, index = StIdx, _ = '_'},
    Table = trans(Site, styles),
    Return = mnesia:match_object(Table, Match, read),
    [#styles{magic_style = CurrentStyle}] = Return, 
    Index = ms_util2:get_index(magic_style, Name), 
    Style2 = tuple_to_list(CurrentStyle), 
    {Start, [_H | End]} = lists:split(Index, Style2), 
    NewStyle = list_to_tuple(lists:append([Start, [Val], End])), 
    write_style(RefX, NewStyle). 

%% write_style will write a style if it doesn't exist and then 
%% return an index pointing to it 
%% If the style already exists it just returns the index 
write_style(#refX{site = Site} = RefX, Style) ->
    % Ref is a cell ref, need a page ref
    Ref2 = RefX#refX{obj = {page, "/"}},
    Match = #styles{refX = Ref2, magic_style = Style, _ = '_'},
    Table = trans(Site, styles),
    case mnesia:match_object(Table, Match, read) of 
        []                          -> write_style2(RefX, Style); 
        [#styles{index = NewIndex}] -> NewIndex
    end. 

write_style2(#refX{site = Site} = RefX, Style) ->
    % Ref is a cell reference - a page reference is needed
    Ref2 = RefX#refX{obj = {page, "/"}},
    NewIndex = mnesia:dirty_update_counter(trans(Site, style_counters), Ref2, 1), 
    ok = tell_front_end(Ref2, NewIndex, Style),
    Rec = #styles{refX = Ref2, index = NewIndex, magic_style = Style},
    ok = mnesia:write(trans(Site, styles), Rec, write),
    NewIndex. 

-spec tell_front_end(#refX{}, {any(), any()}, insert | delete) -> ok.
%% Type = [change | delete]
%% @doc calls the remoting server and tells is that something has changed
%% names like '__name' are not notified to front-end
tell_front_end(RefX, {Key, Val}, Type) when is_record(RefX, refX) ->
    Ref = {RefX, Key}, 
    Tuple = {Ref, Val, Type},
    tell_front_end1(Ref, Tuple);
tell_front_end(#refX{site=Site, path=Path}, Index, Style)
  when is_record(Style, magic_style) ->
    Key = {Site, Path},
    Tuple = {Key, Index, Style},
    tell_front_end1(Key, Tuple).

tell_front_end1(Key, Tuple) ->
    List = get('front_end_notify'),
    case lists:keymember(Key, 1, List) of
        true  -> put('front_end_notify', lists:keyreplace(Key, 1, List, Tuple));
        false -> put('front_end_notify', [Tuple | List])
    end,
    ok.

make_tuple(Style, Counter, Index, Val) -> 
    make_tuple1(Style, Counter, Index, Val, []). 

make_tuple1(S, 0, _I, _V, Acc) -> list_to_tuple([S|Acc]); 
make_tuple1(S, I, I, V, Acc )  -> make_tuple1(S, I -1 , I, V, [V | Acc]); 
make_tuple1(S, C, I, V, Acc)   -> make_tuple1(S, C - 1, I, V, [[] | Acc]).

drop_private(List) -> drop_private(List, []).

drop_private([], Acc) -> Acc;
drop_private([H | T], Acc) ->
    {_, {Name, _}} = H,
    case Name of
        "__"++_ -> drop_private(T, Acc);
        _       -> drop_private(T, [H | Acc])
    end.

extract_values(List) -> extract_values1(List, []).

extract_values1([], Acc)                  -> Acc;
extract_values1([{_R, {_K, V}}| T], Acc)  -> extract_values1(T, [V | Acc]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
% Debugging interface                                                          %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deref_overlap_TEST() ->
    io:format("~n~n~n~n~n~n"),
    Tests =[
            % cells match
            {"A1",    {cell, {1, 1}},        {cell, {1, 1}},        {deref, "#REF!"}},
            % cells don't match
            {"A1",    {cell, {2, 1}},        {cell, {1, 1}},        {formula, "A1"}},
            % cell in a range
            {"A1",    {cell, {1, 1}},        {range, {1, 1, 2, 3}}, {deref, "#REF!"}},
            % cell not in a range
            {"A1",    {cell, {4, 4}},        {range, {1, 1, 2, 3}}, {formula, "A1"}},

            % ranges match
            {"A1:B2", {range, {1, 1, 2, 2}}, {range, {1, 1, 2, 2}}, {deref, "#REF!"}},
            % ranges don't overlap
            {"A1:B2", {range, {1, 1, 2, 2}}, {range, {3, 3, 4, 4}}, {formula, "A1:B2"}},
            % target range inside delete range
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 5, 5}}, {deref, "#REF!"}},
            % delete range inside target range
            {"B2:E5", {range, {2, 2, 5, 5}}, {range, {3, 3, 4, 4}}, {deref, "#REF!"}},
            % delete range clips top-left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 2, 2}}, {deref, "#REF!"}},
            % delete range clips bottom-right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {3, 3, 5, 5}}, {deref, "#REF!"}},
            % delete range clips top-right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {4, 1, 5, 2}}, {deref, "#REF!"}},
            % delete range clips bottom-left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 4, 2, 5}}, {deref, "#REF!"}},
            % delete range slices left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 2, 9}}, {recalc, "A2:B4"}},
            % delete range slices top
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 7, 2}}, {recalc, "B1:D2"}},
            % delete range slices bottom
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 4, 5, 9}}, {recalc, "B2:D3"}},
            % delete range slices right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {4, 1, 5, 9}}, {recalc, "B2:C4"}},

            % cell in a column
            {"C5",    {cell, {3, 5}},        {column, {3, 4}},      {deref, "#REF!"}},
            % cell not in a column
            {"C5",    {cell, {3, 5}},        {column, {6, 7}},      {formula, "C5"}},
            % range in a column (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {3, 5}},      {deref, "#REF!"}},
            % range in a column (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {2, 6}},      {deref, "#REF!"}},
            % delete columns slices left (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {3, 3}},      {recalc, "C5:D8"}},
            % delete columns slices left (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {2, 3}},      {recalc, "B5:C8"}},
            % delete columns slices right (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {5, 5}},      {recalc, "C5:D8"}},
            % delete columns slices right (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {5, 6}},      {recalc, "C5:D8"}},
            % delete column slices middle
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {4, 4}},      {recalc, "C5:D8"}},
            
            
            % cell in a row
            {"C5",    {cell, {3, 5}},        {row, {4, 6}},         {deref, "#REF!"}},
            % cell not in a row
            {"C5",    {cell, {3, 5}},        {row, {6, 7}},         {formula, "C5"}},
            % range in a row (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {5, 8}},         {deref, "#REF!"}},
            % range in a row (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {4, 9}},         {deref, "#REF!"}},
            % delete row slices top (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {5, 5}},         {recalc, "C5:D7"}},
            % delete row slices top (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {4, 5}},         {recalc, "C4:D6"}},
            % delete row slices bottom (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {8, 8}},         {recalc, "C5:D7"}},
            % delete row slices bottom (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {8, 9}},         {recalc, "C5:D7"}},
            % delete row slices middle
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {6, 7}},         {recalc, "C5:D6"}},
            
            % columns can't be derefed by cell deletes
            {"C:F",   {column, {3, 6}},      {cell, {2, 3}},        {formula, "C:F"}},
            % columns can't be derefed by range deletes
            {"C:F",   {column, {3, 6}},      {range, {2, 3, 4, 5}}, {formula, "C:F"}},
            % columns can't be derefed by row deletes
            {"C:F",   {column, {3, 6}},      {row, {2, 3}},         {formula, "C:F"}},
            % column inside a column delete (1)
            {"C:F",   {column, {3, 6}},      {column, {2, 7}},      {deref, "#REF!"}},
            % column inside a column delete (2)
            {"C:F",   {column, {3, 6}},      {column, {3, 6}},      {deref, "#REF!"}},
            % column not inside a column delete
            {"C:F",   {column, {3, 6}},      {column, {8, 9}},      {formula, "C:F"}},
            % column delete slices left (1)
            {"C:F",   {column, {3, 6}},      {column, {3, 3}},      {recalc, "C:E"}},
            % column delete slices left (2)
            {"C:F",   {column, {3, 6}},      {column, {2, 4}},      {recalc, "B:C"}},
            % column delete slices right (1)
            {"C:F",   {column, {3, 6}},      {column, {6, 6}},      {recalc, "C:E"}},
            % column delete slices right (2)
            {"C:F",   {column, {3, 6}},      {column, {5, 7}},      {recalc, "C:D"}},
            % column delete slices middles
            {"C:F",   {column, {3, 6}},      {column, {5, 5}},      {recalc, "C:E"}},
            
            % rows can't be derefed by cell deletes
            {"3:6",   {row, {3, 6}},         {cell, {2, 3}},        {formula, "3:6"}},
            % rows can't be derefed by range deletes
            {"3:6",   {row, {3, 6}},         {range, {2, 3, 4, 5}}, {formula, "3:6"}},
            % rows can't be derefed by column deletes
            {"3:6",   {row, {3, 6}},         {column, {2, 3}},      {formula, "3:6"}},
            % row inside a row delete (1)
            {"3:6",   {row, {3, 6}},         {row, {3, 6}},         {deref, "#REF!"}},
            % row inside a row delete (2)
            {"3:6",   {row, {3, 6}},         {row, {2, 7}},         {deref, "#REF!"}},
            % row not inside a row delete
            {"3:6",   {row, {3, 6}},         {row, {8, 9}},         {formula, "3:6"}},
            % row slices top (1)
            {"3:6",   {row, {3, 6}},         {row, {3, 3}},         {recalc, "3:5"}},
            % row slices top (2)
            {"3:6",   {row, {3, 6}},         {row, {2, 3}},         {recalc, "2:4"}},
            % row slices bottom (1)
            {"3:6",   {row, {3, 6}},         {row, {6, 6}},         {recalc, "3:5"}},
            % row slices bottom (2)
            {"3:6",   {row, {3, 6}},         {row, {6, 7}},         {recalc, "3:5"}},
            % row slices middle
            {"3:6",   {row, {3, 6}},         {row, {4, 4}},         {recalc, "3:5"}}
           ],
    [test_ov(X) || X <- Tests].
            
test_ov({Text, Cell, DelX, Return}) ->
    Return1 = deref_overlap(Text, DelX, Cell),
    case Return of
        Return1 -> ok; % io:format("P ~p~n", [Text]);
        _       -> io:format("Fail: ~p : ~p : ~p : ~p - ~p~n",
                             [Text, Cell, DelX, Return, Return1])
    end.
                   
             
