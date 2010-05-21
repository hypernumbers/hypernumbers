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
%%% * notifications to the front-end
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
         write_attrs/2,
         write_attrs/3,
         read_styles/1,
         local_idx_to_refX/2,
         read_ref/1, read_ref/2, read_ref_field/3,
         clear_cells/1,
         clear_cells/2,
         delete_cells/1,
         shift_cells/4,
         shift_row_objs/2,
         shift_col_objs/2,
         delete_row_objs/1,
         delete_col_objs/1,
         get_local_children/1,
         %%shift_children/3,
         copy_cell/3,
         copy_style/3,
         mark_children_dirty/2,
         mark_these_dirty/2,
         read_page_structure/1,
         read_pages/1,
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
         %%get_last_refs/1
        ]).

-export([
         write_style_IMPORT/2
        ]).

-export([
         deref_overlap_TEST/0
        ]).

-define(to_xml_str, simplexml:to_xml_string).
-define(to_refX, hn_util:refX_from_index).

-define(lt, list_to_tuple).
-define(lf, lists:flatten).
-define(dict, orddict:orddict()).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").
-include("auth.hrl").

-include_lib("stdlib/include/ms_transform.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Exported functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_cell_for_muin(#refX{}) -> {any(), any(), any(), any()}.
%% @doc this function is called by muin during recalculation and should
%%      not be used for any other purpose
get_cell_for_muin(#refX{obj = {cell, {XX, YY}}} = RefX) ->
    #refX{site = Site, path = Path} = RefX,
    Attrs = case read_ref(RefX) of
                [{_, A}] -> A;
                _        -> orddict:new()
            end,
    Value = case orddict:find("rawvalue", Attrs) of
                {ok, {datetime, _, [N]}} -> 
                    muin_date:from_gregorian_seconds(N);
                {ok, V} -> 
                    V;
                _ -> 
                    blank
            end,
    DTree = case orddict:find("__dependency-tree", Attrs) of
                {ok, D} -> D;
                _       -> []
            end,
    Dep = DTree ++ [{"local", get_local_item_index(RefX)}],
    {Value, Dep, [], [{"local", {Site, Path, XX, YY}}]}.

%% @hidden
%% @doc write_style_IMPORT is a wrapper for the internal function write_style
%% which should never be used except in file import
write_style_IMPORT(RefX, Style)
  when is_record(RefX, refX), is_record(Style, magic_style) ->
    NewIndex = write_style(RefX, Style),
    write_attrs(RefX, [{"style", NewIndex}]),
    ok.

%% %% @spec shift_children(Children, OldParent::#refX{},
%% %% NewParent::#refX{}) -> ok Children = [ [#refX{}] | #refX{} ] @doc
%% %% shift_children is called when a message comes in from a remote
%% %% parent saying that that parent has moved. The children of that
%% %% remote hypernumber then have their formulae rewritten to refer to
%% %% the new location of the parent OldParent and NewParent are the
%% %% respective parents and must be cell references % Children can
%% %% either be a cell reference or a list of cell references
%% shift_children(List, OldParent, NewParent)
%%   when is_list(List), is_record(OldParent, refX), is_record(NewParent, refX) ->
%%     [ok = shift_children(X, OldParent, NewParent) || X <- List],
%%     ok;
%% shift_children(Child, OldParent, NewParent)
%%   when is_record(Child, refX), is_record(OldParent, refX),
%%        is_record(NewParent, refX) ->
%%     OUrl = hn_util:refX_to_url(OldParent) ++ "?hypernumber",
%%     NUrl = hn_util:refX_to_url(NewParent) ++ "?hypernumber",
%%     %% first read the child formula
%%     %% but strip off the leading '=' sign
%%     [{Child, {"formula", [$= | Formula]}}] = read_attrs(Child, ["formula"], write),
%%     %% %% just spoofing the lexing which is why we pass in the cell {1, 1}
%%     %% NewFormula = case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
%%     %%                  {ok, Toks}    -> rewrite_hn_formula(Toks, OUrl, NUrl);
%%     %%                  _Syntax_Error -> io:format("Not sure how you get an "++
%%     %%                                             "invalid formula is "++
%%     %%                                             "shift_children but "++
%%     %%                                             "you do~n-~p~n", [Formula]),

%%     %%                                   Formula
%%     %%              end,
%%     write_attrs(Child, [{"formula", NewFormula}]).

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
%% get_last_refs(#refX{site = S, path = P}) ->
%%     RefX2 = #refX{site = S, path = P, obj = {page, "/"}},
%%     Cells = read_ref(RefX2),
%%     Fun = fun({R, V}, {MaxRefX, MaxRefY}) ->
%%                   {NewX, NewY} =
%%                       case V of
%%                           {"formula", _} ->
%%                               #refX{obj = {cell, {X, Y}}} = R,
%%                               #refX{obj = {cell, {MaxX, _}}} = MaxRefX,
%%                               #refX{obj = {cell, {_, MaxY}}} = MaxRefY,
%%                               NX = ?COND(MaxX > X, MaxX, X),
%%                               NY = ?COND(MaxY > Y, MaxY, Y),
%%                               {NX, NY};
%%                           _ ->
%%                               #refX{obj = {cell, {MaxX, _}}} = MaxRefX,
%%                               #refX{obj = {cell, {_, MaxY}}} = MaxRefY,
%%                               {MaxX, MaxY}
%%                       end,
%%                   {#refX{site = S, path = P, obj = {cell, {NewX, 0}}},
%%                    #refX{site = S, path = P, obj = {cell, {0, NewY}}}}
%%           end,
%%     Zero = #refX{site = S, path = P, obj = {cell, {0, 0}}},
%%     lists:foldl(Fun, {Zero, Zero}, Cells).

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

-spec write_attrs(#refX{}, [{string(), term()}]) -> ok. 
write_attrs(Ref, NewAttrs) -> write_attrs(Ref, NewAttrs, nil).

-spec write_attrs(#refX{}, [{string(), term()}], auth_req()) -> ok.
write_attrs(Ref, NewAs, AReq) ->
    Op = fun(Attrs) -> process_attrs(NewAs, Ref, AReq, Attrs) end,
    apply_to_attrs(Ref, Op).

-spec process_attrs([{string(), term()}], #refX{}, auth_req(), ?dict) 
                   -> ?dict.
process_attrs([], _Ref, _AReq, Attrs) ->
    Attrs;
process_attrs([{"formula",Val}|Rest], Ref, AReq, Attrs) ->
    Attrs2 = 
        case superparser:process(Val) of
            {formula, Fla} -> 
                write_formula1(Ref, Fla, Val, AReq, Attrs);
            [NVal, Align, Frmt] -> 
                write_formula2(Ref, Val, NVal, Align, Frmt, Attrs)
        end,
    process_attrs(Rest, Ref, AReq, Attrs2);
process_attrs([A={Key,Val}|Rest], Ref, AReq, Attrs) ->
    Attrs2  = case ms_util2:is_in_record(magic_style, Key) of 
                  true  -> apply_style(Ref, A, Attrs);
                  false -> orddict:store(Key, Val, Attrs)
              end,
    process_attrs(Rest, Ref, AReq, Attrs2).

expand_ref(#refX{site=S}=Ref) -> 
    [lobj_to_ref(S, LO) || LO <- objs_inside_ref(Ref)].

read_ref(Ref) -> read_ref(Ref, read).
-spec read_ref(#refX{}, read | write) -> [{#refX{}, ?dict}].
read_ref(#refX{site=S}=Ref, Lock) ->
    read_attrs(S, objs_inside_ref(Ref), Lock).

-spec read_ref_field(#refX{}, string(), read|write) 
                    -> [{#refX{}, term()}].
read_ref_field(Ref, Field, Lock) ->
    Cells = read_ref(Ref, Lock),
    extract_field(Cells, Field, []).
    
-spec extract_field([{#refX{}, ?dict}], string(), [{#refX{}, term()}])
                   -> [{#refX{}, term()}].
extract_field([], _F, Acc) ->
    lists:reverse(Acc);
extract_field([{Ref, Attrs}|T], Field, Acc) ->
    Acc2 = case orddict:find(Field, Attrs) of
               {ok, V} -> [{Ref, V}|Acc]; 
               _       -> Acc end,
    extract_field(T, Field, Acc2).    

-spec objs_inside_ref(#refX{}) -> [#local_objs{}]. 
objs_inside_ref(#refX{site = S, path = P, obj = {page, "/"}}) ->
    MS = ets:fun2ms(fun(LO=#local_objs{path=MP}) when MP == P -> LO end),
    mnesia:select(trans(S, local_objs), MS);
objs_inside_ref(#refX{site = S, path = P, obj = {column, {X1,X2}}}) ->
    MS = ets:fun2ms(fun(LO=#local_objs{path=MP, obj={cell,{MX,_MY}}}) 
                          when MP == P,
                               X1 =< MX, MX =< X2 -> LO
                    end),
    mnesia:select(trans(S, local_objs), MS);
objs_inside_ref(#refX{site = S, path = P, obj = {row, {R1,R2}}}) ->
    MS = ets:fun2ms(fun(LO=#local_objs{path=MP, obj={cell,{_MX,MY}}}) 
                          when MP == P,
                               R1 =< MY, MY =< R2 -> LO
                    end),
    mnesia:select(trans(S, local_objs), MS);
objs_inside_ref(#refX{site = S, path = P, obj = {range, {0,Y1,infinity,Y2}}}) ->
    MS = ets:fun2ms(fun(LO=#local_objs{path=MP, obj={cell,{MX,MY}}}) 
                          when MP == P,
                               0 =< MX, MX =< infinity, 
                               Y1 =< MY, MY =< Y2 -> LO;
                       (LO=#local_objs{path=MP, obj={row,{MY,MY}}})
                          when MP == P,
                               Y1 =< MY, MY =< Y2 -> LO
                    end),
    mnesia:select(trans(S, local_objs), MS);
objs_inside_ref(#refX{site = S, path = P, obj = {range, {X1,0,X2,infinity}}}) ->
    MS = ets:fun2ms(fun(LO=#local_objs{path=MP, obj={cell,{MX,MY}}}) 
                          when MP == P,
                               X1 =< MX, MX =< X2, 
                               0 =< MY, MY =< infinity -> LO; 
                       (LO=#local_objs{path=MP, obj={column,{MX,MX}}})
                          when MP == P,
                               X1 =< MX, MX =< X2 -> LO
                    end),
    mnesia:select(trans(S, local_objs), MS);
objs_inside_ref(#refX{site = S, path = P, obj = {range, {X1,Y1,X2,Y2}}}) ->
    MS = ets:fun2ms(fun(LO=#local_objs{path=MP, obj={cell,{MX,MY}}}) 
                          when MP == P,
                               X1 =< MX, MX =< X2, 
                               Y1 =< MY, MY =< Y2 -> LO
                    end),
    mnesia:select(trans(S, local_objs), MS);
objs_inside_ref(#refX{site = S, path = P, obj = {cell, {X,Y}}}) ->
    MS = ets:fun2ms(
           fun(LO=#local_objs{path=MP, obj={cell,{MX,MY}}})
                 when MP == P, MX == X, MY == Y -> LO
           end),
    mnesia:select(trans(S, local_objs), MS).

-spec read_attrs(string(), [#local_objs{}], read|write)
                -> [{#refX{}, ?dict}].
read_attrs(S, LocObjs, Lock) ->
    Tbl = trans(S, item),
    read_attrs_(LocObjs, S, Tbl, Lock, []).
read_attrs_([], _S, _Tbl, _Lock, Acc) ->
    lists:reverse(Acc);
read_attrs_([LO|Tail], S, Tbl, Lock, Acc) ->
    Acc2 = case mnesia:read(Tbl, LO#local_objs.idx, Lock) of
               [#item{attrs=Attrs}] -> [{lobj_to_ref(S, LO), Attrs} | Acc];
               []                   -> Acc end,
    read_attrs_(Tail, S, Tbl, Lock, Acc2).

-spec lobj_to_ref(string(), #local_objs{}) -> #refX{}.
lobj_to_ref(Site, #local_objs{path=P, obj=O}) ->
    #refX{site=Site, path=P, obj=O}.

-spec expunge_refs(string(), [#refX{}]) -> ok. 
expunge_refs(S, Refs) ->
    ItemT = trans(S, item),
    ObjT = trans(S, local_objs),
    [begin
         mnesia:delete(ItemT, Idx, write),
         mnesia:delete_object(ObjT, LO, write)
     end || Ref <- Refs,
            #local_objs{idx=Idx}=LO <- objs_inside_ref(Ref)],
    ok.

-spec apply_to_attrs(#refX{}, fun((?dict) -> ?dict)) -> ok.
apply_to_attrs(#refX{site=Site}=Ref, Op) ->
    Table = trans(Site, item), 
    Idx = get_local_item_index(Ref),
    Attrs = case mnesia:read(Table, Idx, write) of
                [#item{attrs=A}] -> A;
                _                -> orddict:new()
            end,
    Attrs2 = Op(Attrs),
    Attrs3 = post_process(Ref, Attrs2),
    Item = #item{idx = Idx, attrs = Attrs3},
    tell_front_end_change(Ref, Attrs3),
    mnesia:write(Table, Item, write).

%% Last chance to apply any default styles and formats. 
-spec post_process(#refX{}, ?dict) -> ?dict. 
post_process(Ref, Attrs) ->
    Attrs2 = post_process_styles(Ref, Attrs),
    case orddict:find("rawvalue", Attrs2) of
        {ok, Raw} -> post_process_format(Raw, Attrs2);
        _         -> Attrs2
    end.                       
    
post_process_styles(Ref, Attrs) -> 
    case orddict:find("style", Attrs) of
        {ok, _} -> Attrs;
        _       ->
            case orddict:find("__default-align", Attrs) of
                {ok, Align} -> apply_style(Ref,{"text-align",Align},Attrs);
                _           -> Attrs
            end
    end.

post_process_format(Raw, Attrs) ->
    Format = case orddict:find("format", Attrs) of
                 {ok, F} -> F;
                 _       -> "General"
             end,
    {erlang, {_Type, Output}} = format:get_src(Format),
    % Y'all hear, this is how America does color. I tell you what.
    {ok, {Color, Value}} = format:run_format(Raw, Output),
    add_attributes(Attrs, [{"value", Value},
                           {"overwrite-color", atom_to_list(Color)}]).
                        
%% @spec read_inherited_list(#refX{}, Key) -> {ok, Value}
%% Key = atom()
%% Value = term()
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at a given reference, if not found it returns the supplied
%%       default value
%%       
%% %% @todo what are the ref types it supports? improve the documentation, etc, etc
%% read_inherited_list(RefX, Key) when is_record(RefX, refX)  ->
%%     Type = case RefX#refX.obj of
%%                null    -> page;
%%                {T, _R} -> T
%%            end,
%%     get_item_list(Type, RefX, Key, []).

%% @spec read_inherited(#refX{}, Key, Default) -> {ok, Value}
%% Key = atom()
%% Value = term()
%% Default = term()
%% @doc  This function searches the tree for the occurences of a key
%%       and returns a list of them
%% read_inherited(RefX, Key, Default) when is_record(RefX, refX)  ->
%%     Type = case RefX#refX.obj of
%%                null    -> page;
%%                {T, _R} -> T
%%            end,
%%     case return_first(Type, RefX, Key) of
%%         {ok, Value} -> {ok, Value};
%%         nomatch     -> {ok, Default}
%%     end.

%% Status = list()
%% @doc shift_cells takes a range, row or column and shifts it by the offset.
%% The list of cells that are passed in as Rewritten are not to be rewritten
%% here
%% <em>NOTE</em> this function doesn't pass any messages on to parents or
%% children on other websites - that is done in the API layer by calling
%% {@link hn_db_wu:mark_dirty/1} with <code>dirty_notify_out</code> and 
%% <code>dirty_notify_back_in</code> records as appropriate
shift_cells(#refX{site=Site, obj= Obj}=From, Type, Disp, Rewritten)
  when (Type == insert orelse Type == delete) andalso 
       (Disp == vertical orelse Disp == horizontal) ->
    {XOff, YOff} = hn_util:get_offset(Type, Disp, Obj),
    RefXSel = shift_pattern(From, Disp),
    case objs_inside_ref(RefXSel) of
        [] -> [];
        ObjsList ->
            %% %% Rewrite the formulas of all the child cells
            RefXList = [lobj_to_ref(Site, O) || O <- ObjsList],
            ChildCells = lists:flatten([get_local_children(X) || X <- RefXList]),
            ChildCells2 = hslists:uniq(ChildCells),
            DedupedChildren = lists:subtract(ChildCells2, Rewritten),
            Formulas = [F || X <- DedupedChildren,
                             F <- read_ref_field(X, "formula", write)],
            Fun = fun({ChildRef, F1}, Acc) ->
                          {St, F2} = offset_fm_w_rng(ChildRef, F1, From, {XOff, YOff}),
                          Op = fun(Attrs) -> orddict:store("formula", F2, Attrs) end,
                          ok = apply_to_attrs(ChildRef, Op),
                          case St of
                              clean -> Acc;
                              dirty -> [ChildRef | Acc]
                          end
                  end,
            DirtyChildren = lists:foldl(Fun, [], Formulas),
            
            %% Rewrite the local_objs entries by applying the shift offset.
            ObjTable = trans(Site, local_objs),
            [begin 
                 mnesia:delete_object(ObjTable, LO, write),
                 mnesia:write(ObjTable, shift_obj(LO, XOff, YOff), write)
             end || LO <- ObjsList],
            DirtyChildren
    end.

shift_obj(#local_objs{obj = {cell, {X, Y}}}=LO, XOff, YOff) ->
    O2 = {cell, {X + XOff, Y + YOff}},
    LO#local_objs{obj = O2};
shift_obj(#local_objs{obj = {column, {X1, X2}}}=LO, XOff, _YOff) ->
    O2 = {column, {X1 + XOff, X2 + XOff}},
    LO#local_objs{obj = O2};
shift_obj(#local_objs{obj = {row, {Y1, Y2}}}=LO, _XOff, YOff) ->
    O2 = {row, {Y1 + YOff, Y2 + YOff}},
    LO#local_objs{obj = O2};
shift_obj(LO, _, _) -> LO. 


-spec delete_col_objs(#refX{}) -> ok.
%% @doc deletes any col objects completely covered by the #refX{}
delete_col_objs(#refX{site = S, path = P, obj = {column, {X1, X2}}}) ->
    H = #local_objs{path = P, obj = {column, {'$1', '$2'}}, _ = '_'},
    C = [{'and', {'>=', '$1', X1}, {'=<', '$2', X2}}],
    B = ['$_'],
    M = [{H, C, B}],
    Table = trans(S, local_objs),
    Recs = mnesia:select(Table, M, write),
    ok = delete_recs(S, Recs).

-spec delete_row_objs(#refX{}) -> ok.
%% @doc deletes any row objects completely covered by the #refX{}
delete_row_objs(#refX{site = S, path = P, obj = {row, {Y1, Y2}}}) ->
    H = #local_objs{path = P, obj = {row, {'$1', '$2'}}, _ = '_'},
    C = [{'and', {'>=', '$1', Y1}, {'=<', '$2', Y2}}],
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
    ok = delete_recs(S, [Shift]),
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
    ok = delete_recs(S, [Shift]),
    ok = mnesia:write(trans(S, local_objs), New, write).

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
read_styles(#refX{site = Site} = RefX) ->
    %% first get the style records to get the indexes
    Cells = read_ref_field(RefX, "style", read),
    IndexList = hslists:uniq([V || {_, V} <- Cells]),
    %% RefX is a cell/column/row/range ref - make it a page ref
    Ref2 = RefX#refX{obj = {page, "/"}},
    Match = #styles{refX = Ref2, index =  '$1', _ = '_'},
    Cond = make_or(IndexList, '$1'),
    Body = ['$_'],
    Table = trans(Site, styles),
    mnesia:select(Table, [{Match, Cond, Body}]).

%% @doc deletes the contents (formula/value) and the formats
%% of a cell (but doesn't delete the cell itself).
-spec clear_cells(#refX{}) -> ok.
clear_cells(RefX) -> clear_cells(RefX, contents).

-spec clear_cells(#refX{}, all | style | contents) -> ok. 
clear_cells(Ref, all) ->
    Op = fun(_) -> orddict:new() end,
    [ok = apply_to_attrs(X, Op) || X <- expand_ref(Ref)], ok;
clear_cells(Ref, style) ->
    Op = fun(Attrs) -> orddict:erase("style", Attrs) end,
    [ok = apply_to_attrs(X, Op) || X <- expand_ref(Ref)], ok;
clear_cells(Ref, contents) ->
    Op = fun(Attrs) ->
                 del_attributes(Attrs, ["formula",
                                        "rawvalue",          
                                        "value",             
                                        "overwrite-color",
                                        "__ast",             
                                        "__recompile",       
                                        "__shared",          
                                        "__area",            
                                        "__dependency-tree",
                                        "parents"])           
         end,
    [ok = apply_to_attrs(X, Op) || X <- expand_ref(Ref)], ok.

%% @doc takes a reference to a
%% <ul>
%% <li>page</li>
%% <li>row</li>
%% <li>column</li>
%% <li>range</li>
%% <li>cell</li>
%% </ul>
%% and then deletes all the cells including their indices in local_objs
%% and makes all cells that are their children throw a #ref! error
%% and deletes the links there the old cell was the child of another cell
%% @todo this is ineffiecient because it reads and then deletes each
%% record individually - if remoting_reg supported a {delete refX all}
%% type message it could be speeded up
-spec delete_cells(#refX{}) -> [#refX{}].
delete_cells(#refX{site = S} = DelX) ->
    case expand_ref(DelX) of
        [] -> [];
        Cells  ->
            %% update the children that point to the cell that is
            %% being deleted by rewriting the formulae of all the
            %% children cells replacing the reference to this cell
            %% with #ref!
            LocalChildren = [get_local_children(C) || C <- Cells],
            LocalChildren2 = hslists:uniq(lists:flatten(LocalChildren)),
            
            %% sometimes a cell will have local children that are also
            %% in the delete zone these need to be removed before we
            %% do anything else...
            LocalChildren3 = lists:subtract(LocalChildren2, Cells),
            
            %% Rewrite formulas
            [deref_formula(X, DelX) || X <- LocalChildren3],

            %% fix relations table.
            [ok = delete_local_relation(X) || X <- Cells],

            %% Delete the cells (and their indicices)
            expunge_refs(S, Cells),
            LocalChildren3
    end.

-spec deref_formula(#refX{}, #refX{}) -> ok. 
deref_formula(Ref, DelRef) ->
    Op = fun(Attrs) -> 
                 case orddict:find("formula", Attrs) of
                     {ok, F1} -> 
                         {_Status, F2} = deref(Ref, F1, DelRef),
                         orddict:store("formula", F2, Attrs);
                     _ ->
                         Attrs
                 end
         end,
    apply_to_attrs(Ref, Op).

%% %% @doc copys cells from a reference to a reference
%% -spec copy_cell(#refX{}, #refX{}, false | horizontal | vertical) -> ok.
copy_cell(#refX{obj = {cell, {FX,FY}}} = From, 
          #refX{obj = {cell, {TX,TY}}} = To, 
          Incr)
  when is_record(From, refX), is_record(To, refX) ->
    Formula = case read_ref_field(From, "formula", read) of
                  [{_, V}] -> superparser:process(V); 
                  _        -> ""
              end,
    F2 = case Formula of
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
    write_attrs(To, [{"formula", F2}]).

%% @spec(From::refX{}, To::refX{}) -> ok
%% @doc Copies the style applied to From and attaches it to To.
%%      From can only be a cell ref but To can be either a cell or range
%%      ref
%% @end
%% this clause is for 'on page' copies where both From and To are on
%% the same page - just the index is copied
copy_style(#refX{site = S, path = P, obj = {cell, _}} = From, 
           #refX{site = S, path = P, obj = {Type, _}} = To, 
           _AReq)
  when Type == cell orelse Type == range ->
    case read_ref_field(From, "style", read) of
        [{_, Idx}] ->
            List = case Type of
                       cell  -> [To];
                       range -> hn_util:range_to_list(To)
                   end,
            Op = fun(Attrs) -> orddict:store("style", Idx, Attrs) end,
            [ok = apply_to_attrs(X, Op) || X <- List],
            ok;
        _ ->
            ok
    end;
%% this clause is for copying styles across different pages
copy_style(#refX{obj = {cell, _}} = From, 
           #refX{obj = {Type, _}} = To, _AReq)
  when Type == cell orelse Type == range ->
    case read_styles(From) of
        [] -> ok;
        [{styles, _, _Idx, MagicStyle}] ->
            List = case Type of
                       cell  -> [To];
                       range -> hn_util:range_to_list(To)
                   end,
            Fun = fun(X) ->
                          Idx = write_style(X, MagicStyle),
                          fun(Attrs) -> 
                                  orddict:store("style", Idx, Attrs) 
                          end
                  end,
            [ok = Fun(X) || X <- List],
            ok
    end.

-spec mark_these_dirty([#refX{}], nil | uid()) -> ok.
mark_these_dirty([], _) -> ok;
mark_these_dirty(Refs = [#refX{site = Site}|_], AReq) ->
    F = fun(C) -> case read_local_item_index(C) of
                      false -> []; 
                      Idx   -> Idx
                  end
        end,
    Tbl = trans(Site, relation),
    Idxs = lists:flatten([F(C) || R <- Refs, C <- expand_ref(R)]),
    Q = insert_work_queue(Idxs, Tbl, 1, hn_workq:new(AReq)),
    Entry = #dirty_queue{id = hn_workq:id(Q), queue = Q},
    ok = mnesia:write(trans(Site, dirty_queue), Entry, write).
    
-spec mark_children_dirty(#refX{}, nil | uid()) -> ok.
mark_children_dirty(#refX{site = Site}=RefX, AReq) ->
    Tbl = trans(Site, relation),
    Children = get_local_children_idxs(RefX),
    Q = insert_work_queue(Children, Tbl, 1, hn_workq:new(AReq)),
    case hn_workq:is_empty(Q) of
        true  -> ok;
        false -> Entry = #dirty_queue{id = hn_workq:id(Q), queue = Q},
                 ok = mnesia:write(trans(Site, dirty_queue), Entry, write)
    end.

%% Recursively walk child relation, adding entries into the work
%% queue.  We maintain an invariant that children must have a higher
%% priority than their parents, forcing them to be calculated after
%% their parent(s). This algorithm could naively visit the same
%% children multiple times, if there is no unique path from N1 ~~>
%% N2. An attempt is made to stop walking a bad path asap.
%% see:needs_elem(...).
-spec insert_work_queue([cellidx()], 
                         atom(), 
                         integer(), 
                         hn_workq:work_queue())
                        -> hn_workq:work_queue(). 
insert_work_queue([], _Tbl, _Priority, Q) ->
    Q;
insert_work_queue([Idx|Rest], Tbl, Priority, Q) ->
    Qnext = 
        case mnesia:read(Tbl, Idx) of
            [R] -> 
                case hn_workq:needs_elem(Idx, Priority, Q) of
                    true ->
                        Children = ordsets:to_list(R#relation.children),
                        Q2 = insert_work_queue(Children, 
                                               Tbl, 
                                               Priority + 1,
                                               Q),
                        hn_workq:add(Idx, Priority, Q2);
                    false -> Q
                end;
            _ -> Q
        end,
    insert_work_queue(Rest, Tbl, Priority, Qnext).

%% @spec mark_notify_out_dirty(Parent::#refX{}, Change)  -> ok
%% Change = {new_value, Value, DepTree} | {insert, Obj, Disp} | {delete, Obj, Disp}
%% DepTree = list()
%% Delay = integer()
%% @doc marks a cell as dirty so that its remote children can be updated.
%% Updates called with this function and not 
%% <code>mark_notify_out_dirty/3</code> are marked with the default delay
%% (defined in spriki.hrl)
%% mark_notify_out_dirty(Parent, Change) when is_record(Parent, refX) ->
%%     mark_notify_out_dirty(Parent, Change, ?DELAY).

%% @spec mark_notify_out_dirty(Parent::#refX{}, Change, Delay)  -> ok
%% Change = {new_value, Value, DepTree} | {insert, Obj, Disp} | {delete, Obj, Disp}
%% DepTree = list()
%% Delay = integer()
%% @doc marks a cell as dirty so that its remote children can be updated
%% Delay is a time in milliseconds that this message should be delayed
%% @todo this contains a transaction, WTF?
%% mark_notify_out_dirty(#refX{site = Site} = P, {Type, _, _} = Change, Delay) ->
%%     %% read the outgoing hypernumbers
%%     %% one for each site where the Parent is used in a hypernumber
%%     List = read_outgoing_hns(Site, P),
%%     %% now we need to get a list of all the actual children and the page versions
%%     %% of their pages
%%     Fun2 =
%%         fun(X) ->
%%                 #outgoing_hn{child_site = ChildSite} = X,
%%                 Head = get_head(ChildSite, P, Type),
%%                 Table = trans(Site, remote_cell_link),
%%                 Children = mnesia:select(Table, [{Head , [], ['$_']}], read),
%%                 ReturnList = get_pages_and_vsns(Site, Children),
%%                 {X, ReturnList}
%%         end,
%%     ChildrenList = lists:map(Fun2, List),

%%     %% always write the dirty outgoing hypernumber
%%     PVsn = read_page_vsn(Site, P),
%%     ParentPage = P#refX{obj = {page, "/"}},
%%     ParentUrl = hn_util:refX_to_url(ParentPage),
%%     case List of
%%         [] -> ok;
%%         _  -> Rec = #dirty_notify_out{parent = P, change = Change,
%%                                       outgoing = ChildrenList,
%%                                       parent_vsn = {version, ParentUrl, PVsn},
%%                                       delay = Delay},
%%               Tbl = trans(Site, element(1,Rec)),
%%               mnesia:write(Tbl, Rec, write)
%%     end.

%% @spec unregister_out_hn(Parent::#refX{}, Child::#refX{}) -> ok
%% @doc unregisters an outgoing hypernumber - if it is the last reference
%% to an outgoing hypernumber deletes the hypernumber as well
%% Both parent and child references must point to a cell. This function is
%% *ONLY* to be used on the parent (or out) side of the hypernumber
%% @todo this required a full table scan for an unregister
%% will get veeeerrry expensive if you have 100,000 children tracking a
%% number!
%% unregister_out_hn(P, C)
%%   when is_record(P, refX), is_record(C, refX) ->
%%     #refX{site = ParentSite} = P,
%%     #refX{site = ChildSite} = C,
%%     %% first up delete the remote cell link
%%     Head = #remote_cell_link{parent = P, child = C, type = outgoing, _ = '_'},
%%     Table = trans(ParentSite, remote_cell_link),
%%     [RemCellRec] = mnesia:select(Table, [{Head, [], ['$_']}], write),
%%     ok = mnesia:delete_object(Table, RemCellRec, write),
%%     %% now see if any other remote cell references match this site...
%%     %% - if none do, delete the hypernumber from outgoing_hn
%%     %% - if some do, do nothing...
%%     H3 = #refX{site = ChildSite, _ = '_'},
%%     H4 = #remote_cell_link{parent = P, child = H3, type = outgoing, _ = '_'},
%%     case mnesia:select(Table, [{H4, [], ['$_']}], read) of
%%         []  -> H6 = #outgoing_hn{site_and_parent = {ParentSite, P},
%%                                  child_site = ChildSite, _ = '_'},
%%                Table2 = trans(ParentSite, outgoing_hn),
%%                [Rec] = mnesia:select(Table2, [{H6, [], ['$_']}], read),
%%                mnesia:delete_object(Table2, Rec, write);
%%         _   -> ok
%%     end.

%% @spec read_page_structure(Ref) -> dh_tree()
%% @doc read the populated pages under the specified path
%% @todo fix up api
read_page_structure(#refX{site = Site, obj = {page, "/"}}) ->
    Items = mnesia:dirty_all_keys(trans(Site, local_objs)),
    filter_pages(Items, dh_tree:new()).

read_pages(#refX{site = Site, obj = {page, "/"}}) ->
    mnesia:all_keys(trans(Site, local_objs)).

filter_pages([], Tree) ->
    Tree;
filter_pages([Path | T], Tree) ->
    filter_pages(T, dh_tree:add(Path, Tree)).

-spec get_local_item_index(#refX{}) -> pos_integer().
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

%% converts a tablename into the site-specific tablename
trans(Site, TableName) when is_atom(TableName) ->
    Prefix = get_prefix(Site),
    list_to_atom(Prefix ++ "&" ++ atom_to_list(TableName));
trans(Site, Record) when is_tuple(Record) -> 
    NRec = trans(Site, element(1, Record)),
    setelement(1, Record, NRec).


%%% splits a tablename into the site and record
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

get_prefix(R) when is_record(R, refX) ->
    get_prefix(R#refX.site);
get_prefix("http://"++Site) ->
    [case S of $: -> $&; S  -> S end 
     || S <- Site].

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

shift_pattern(#refX{obj = {cell, {_X, Y}}} = RefX, vertical) ->
    RefX#refX{obj = {row, {Y, infinity}}};
shift_pattern(#refX{obj = {cell, {X, _Y}}} = RefX, horizontal) ->
    RefX#refX{obj = {column, {X, infinity}}};
shift_pattern(#refX{obj = {range, {X1, Y1, X2, _Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {X1, Y1, X2, infinity}}};
shift_pattern(#refX{obj = {range, {X1, Y1, _X2, Y2}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X1, Y1, infinity, Y2}}};
shift_pattern(#refX{obj = {row, {Y1, _Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {row, {Y1, infinity}}};
shift_pattern(#refX{obj = {column, {X1, _X2}}} = RefX, horizontal) ->
    RefX#refX{obj = {column, {X1, infinity}}};
shift_pattern(RefX, _Disp) -> RefX.

local_idx_to_refX(S, Idx) ->
    case mnesia:index_read(trans(S, local_objs), Idx, idx) of
        [Rec] -> #local_objs{path = P, obj = O} = Rec,
                 #refX{site = S, path = P, obj = O};
        []    -> {error, id_not_found, Idx}
    end.

%% @doc Make a #muin_rti record out of a ref record and a flag that specifies 
%% whether to run formula in an array context.
refX_to_rti(#refX{site = S, path = P, obj = {cell, {C, R}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, 
              col = C, row = R, 
              array_context = AC,
              auth_req = AR};
refX_to_rti(#refX{site = S, path = P, obj = {range, {C, R, _, _}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, 
              col = C, row = R, 
              array_context = AC,
              auth_req = AR}.

%% read_item_index reads the index of an object AND RETURNS 'false'
%% IF IT DOESN'T EXIST
-spec read_local_item_index(#refX{}) -> pos_integer() | false. 
read_local_item_index(#refX{site = S, path = P, obj = Obj}) ->
    Table = trans(S, local_objs),
    MS = ets:fun2ms(fun(#local_objs{path=MP, obj=MObj, idx=I}) when 
                              MP == P, MObj == Obj -> I
                    end),
    case mnesia:select(Table, MS, read) of
        [I] -> I;
        _   -> false
    end.

%% make_cell makes a cell with dollars and stuff based on the offset
make_cell(false, X, XOffset, false, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, false, Y, YOffset) ->
    [$$] ++ tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(false, X, XOffset, true, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, true, Y, YOffset)  -> 
    [$$] ++ tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset).

%% drag'n'drop_cell drags and drops a cell ignoring offsets
%% if the row/column part is fixed with a dollar
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
diff(_FX,  FY, _TX,  TY, vertical)   -> TY - FY.

%% make formula creates a new formula, but also returns a status.
%% Status can be [clean | dirty]
%% Formulae that return dirty should be marked dirty at recalc
%% time as they will not recalc to the real value
%% The function 'INDIRECT' is an example of such a function
make_formula(Toks) ->
    mk_f(Toks, {clean, []}).

%% this function needs to be extended...
mk_f([], {St, A}) -> 
    {St, "="++lists:flatten(lists:reverse(A))};

mk_f([{errval, _, '#REF!'} | T], {St, A}) -> 
    mk_f(T, {St, ["#REF!" | A]});

mk_f([{deref, _, Text} | T], {St, A}) -> 
    mk_f(T, {St, [Text | A]});

%% special infering of division
mk_f([{cellref, _, C1}, {cellref, _, C2} | T], {St, A}) -> 
    mk_f(T, {St, [C2#cellref.text, "/", C1#cellref.text | A]});

mk_f([{int, _, I}, {cellref,_,C} | T], {St, A}) -> 
    mk_f(T, {St, [C#cellref.text, "/", integer_to_list(I) | A]});

mk_f([{float, _, {F, _}}, {cellref,_,C} | T], {St, A}) -> 
    mk_f(T, {St, [C#cellref.text, "/", float_to_list(F) | A]});

mk_f([{')',_}, {cellref,_,C} | T], {St, A}) ->
    mk_f(T, {St, [C#cellref.text, "/", ")" | A]});

%% order matters - now detecting 'root' cells
mk_f([{cellref, _, #cellref{path="/", text=Text}} | T], {St, A}) -> 
    mk_f(T, {St, ["/" ++ Text | A]});

mk_f([{cellref, _, C} | T], {St, A}) ->
    mk_f(T, {St, [C#cellref.text | A]});

mk_f([{rangeref, _, R} | T], {St, A}) ->
    mk_f(T, {St, [R#rangeref.text | A]});

mk_f([{namedexpr, _, N} | T], {St, A}) ->
    mk_f(T, {St, [N#namedexpr.path ++ N#namedexpr.text | A]});

mk_f([{bool, _, H} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]});

mk_f([{atom, _, H} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]});

mk_f([{int, _, I} | T], {St, A}) ->
    mk_f(T, {St, [integer_to_list(I) | A]});

mk_f([{float, _, {F, _OrigStr}} | T], {St, A}) ->
    mk_f(T, {St, [float_to_list(F) | A]});

mk_f([{formula, _, S} | T], {St, A}) ->
    mk_f(T, {St, [S | A]});

mk_f([{str, _, S} | T], {St, A}) ->
    mk_f(T, {St, [$", S, $" | A]});

mk_f([{recalc, _, S} | T], {_St, A}) ->
    mk_f(T, {dirty, [S | A]});

mk_f([{name, _, "INDIRECT"} | T], {_St, A}) ->
    mk_f(T, {dirty, ["INDIRECT" | A]});

mk_f([{name, _, "SUM"} | T], {_St, A}) ->
    mk_f(T, {dirty, ["SUM" | A]});

mk_f([{name, _, "CELL"} | T], {_St, A}) ->
    mk_f(T, {dirty, ["CELL" | A]});

mk_f([{name, _, S} | T], {St, A}) ->
    mk_f(T, {St, [S | A]});

mk_f([{H, _} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]}).

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
offset_with_ranges1([{rangeref, LineNo,
                      #rangeref{path = Path, text = Text}=H} | T],
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
    NewAcc = {rangeref, LineNo, H#rangeref{text = NewText}},
    offset_with_ranges1(T, Cell, From, Offset, [NewAcc | Acc]);
offset_with_ranges1([{cellref, LineNo,
                      C=#cellref{path = Path, text = Text}}=H | T],
                    Cell, #refX{path = FromPath} = From,
                    {XO, YO}=Offset, Acc) ->
    {XDollar, X, YDollar, Y} = parse_cell(muin_util:just_ref(Text)),
    case From#refX.obj of
        {column,{Left,_Right}} when X < Left ->
            offset_with_ranges1(T, Cell, From, Offset, [H | Acc]);
        {row,{Top,_Bottom}} when Y < Top ->
            offset_with_ranges1(T, Cell, From, Offset, [H | Acc]);
        _Else ->
            #refX{path = CPath} = Cell,
            Prefix = case muin_util:just_path(Text) of
                         "/"   -> "";
                         Other -> Other
                     end,

            PathCompare = muin_util:walk_path(CPath, Path),
            NewCell =
                case PathCompare of
                    FromPath -> make_cell(XDollar, X, XO, YDollar, Y, YO);
                    _        -> Text
                end,
            NewAcc = {cellref, LineNo, C#cellref{text = Prefix ++ NewCell}},    
            offset_with_ranges1(T, Cell, From, {XO, YO}, [NewAcc | Acc])
    end;
offset_with_ranges1([H | T], Cell, From, Offset, Acc) ->
    offset_with_ranges1(T, Cell, From, Offset, [H | Acc]).

%% handle cells
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
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
%% handle rows
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {row, {Top, _Bottom}}}, 
               {0=_XOffset, YOffset}) ->
    NC1 = if Top =< Y1 -> make_cell(X1D, X1, 0, Y1D, Y1, YOffset);
             true      -> Cell1 end,
    NC2 = if Top =< Y2 -> make_cell(X2D, X2, 0, Y2D, Y2, YOffset); 
             true      -> Cell2 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
%% handle columns
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {column, {Left, _Right}}}, 
               {XOffset, 0=_YOffset}) ->
    NC1 = if Left =< X1 -> make_cell(X1D, X1, XOffset, Y1D, Y1, 0);
             true       -> Cell1 end,
    NC2 = if Left =< X2 -> make_cell(X2D, X2, XOffset, Y2D, Y2, 0); 
             true       -> Cell2 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
%% handle ranges (horizontal rewrite)
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {range, {_XA, YA, _XB, YB}}}, 
               {XOffset, 0}) ->
    if
        (YA =< Y1 andalso YB >= Y2) ->
            NC1 = make_cell(X1D, X1, XOffset, Y1D, Y1, 0),
            NC2 = make_cell(X2D, X2, XOffset, Y2D, Y2, 0);
        (YA >  Y1 orelse  YB <  Y2) ->
            NC1 = Cell1,
            NC2 = Cell2
    end,
    Prefix ++ NC1 ++ ":" ++ NC2;
%% handle ranges (horizontal rewrite)
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {range, {XA, _YA, XB, _YB}}}, 
               {0, YOffset}) ->
    if
        (XA =< X1 andalso XB >= X2) ->
            NC1 = make_cell(X1D, X1, 0, Y1D, Y1, YOffset),
            NC2 = make_cell(X2D, X2, 0, Y2D, Y2, YOffset);
        (XA >  X1 orelse  XB <  X2) ->
            NC1 = Cell1,
            NC2 = Cell2
    end,
    Prefix ++ NC1 ++ ":" ++ NC2.

%% used in copy'n'paste, drag'n'drops etc...
d_n_d_c_n_p_offset(Toks, XOffset, YOffset) ->
    d_n_d_c_n_p_offset1(Toks, XOffset, YOffset, []).

d_n_d_c_n_p_offset1([], _XOffset, _YOffset, Acc) -> lists:reverse(Acc);
d_n_d_c_n_p_offset1([{cellref, LineNo, #cellref{text = Text}= H} | T], 
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
d_n_d_c_n_p_offset1([{rangeref, LineNo, #rangeref{text = Text}=H} | T], XOffset, YOffset, Acc) ->
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
    NewR = {rangeref, LineNo, H#rangeref{text = NewText}},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewR | Acc]);
d_n_d_c_n_p_offset1([H | T], XOffset, YOffset, Acc) ->
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [H | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Relations 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -spec get_local_parents(#refX{}) -> [#refX{}].
%% get_local_parents(#refX{site = Site} = X) ->
%%     ParentIdxs = get_local_rel_idxs(X, parents),
%%     [local_idx_to_refX(Site, C) || C <- ParentIdxs].

-spec get_local_children(#refX{}) -> [#refX{}].
get_local_children(#refX{site = Site} = X) ->
    ChildIdxs = get_local_rel_idxs(X, children),
    [local_idx_to_refX(Site, C) || C <- ChildIdxs].

-spec get_local_children_idxs(#refX{}) -> [cellidx()]. 
get_local_children_idxs(Ref) -> get_local_rel_idxs(Ref, children).

-spec get_local_rel_idxs(#refX{}, children|parents) -> [cellidx()]. 
get_local_rel_idxs(#refX{site = Site, obj = {cell, _}} = Ref, Relation) ->
    case read_local_item_index(Ref) of
        false -> 
            [];
        Idx -> 
            Table = trans(Site, relation),
            case mnesia:read(Table, Idx, read) of
                [R] -> case Relation of
                           children -> R#relation.children;
                           parents  -> R#relation.parents
                       end;
                _   -> []
            end
    end;
get_local_rel_idxs(#refX{obj = {Type, _}} = Ref, Relation) 
  when (Type == row) orelse (Type == column) orelse 
       (Type == range) orelse (Type == page) ->
    lists:flatten([get_local_rel_idxs(X, Relation)
                   || X=#refX{obj={cell,_}} <- expand_ref(Ref)]).

-spec delete_local_relation(#refX{}) -> ok.
delete_local_relation(#refX{site = Site} = Cell) ->
    case read_local_item_index(Cell) of
        false -> ok;
        CellIdx ->
            Tbl = trans(Site, relation),
            case mnesia:read(Tbl, CellIdx, write) of
                [R] ->
                    [del_local_child(P, CellIdx, Tbl) || 
                        P <- R#relation.parents],
                    ok = mnesia:delete(Tbl, CellIdx, write);
                _ -> ok
            end
    end.

-spec del_local_child(cellidx(), cellidx(), atom()) -> ok.
del_local_child(CellIdx, Child, Tbl) ->
     case mnesia:read(Tbl, CellIdx, write) of
         [R] ->
             Children = ordsets:del_element(Child, R#relation.children),
             R2 = R#relation{children = Children},
             mnesia:write(Tbl, R2, write);
         _ ->
             ok
     end.

-spec set_local_relations(#refX{}, [#refX{}]) -> ok.
set_local_relations(#refX{site = Site} = Cell, Parents) ->
    Tbl = trans(Site, relation),
    CellIdx = get_local_item_index(Cell),
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R; 
              []  -> #relation{cellidx = CellIdx}
          end,
    Rel2 = set_local_parents(Tbl, Rel, Parents),
    mnesia:write(Tbl, Rel2, write).

-spec set_local_parents(atom(), #relation{}, [#refX{}]) -> #relation{}. 
set_local_parents(Tbl, 
                  Rel = #relation{cellidx = CellIdx, 
                                  parents = CurParents},
                  Parents) ->
    ParentIdxs = ordsets:from_list([get_local_item_index(P) || P <- Parents]),
    LostParents = ordsets:subtract(CurParents, ParentIdxs),
    [del_local_child(P, CellIdx, Tbl) || P <- LostParents],
    [ok = add_local_child(P, CellIdx, Tbl) || P <- ParentIdxs],
    Rel#relation{parents = ParentIdxs}.

 %% Adds a new child to a parent.
-spec add_local_child(cellidx(), cellidx(), atom()) -> ok.
add_local_child(CellIdx, Child, Tbl) ->
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R;
              []  -> #relation{cellidx = CellIdx}
          end,
    Children = ordsets:add_element(Child, Rel#relation.children),
    mnesia:write(Tbl, Rel#relation{children = Children}, write).

%% get_attrs(List, AttrList) -> get_attrs1(List, AttrList, []).

%% get_attrs1([], _AttrList, Acc) -> Acc;
%% get_attrs1([{_, {K, _V}} = H | T], AttrList, Acc) ->
%%     NewAcc = case lists:member(K, AttrList) of
%%                  true  -> [H | Acc];
%%                  false -> Acc
%%              end,
%%     get_attrs1(T, AttrList, NewAcc).

%% dereferences a formula
deref(Child, [$=|Formula], DeRefX) when is_record(DeRefX, refX) ->
    {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {1, 1}),
    NewToks = deref1(Child, Toks, DeRefX, []),
    make_formula(NewToks).

deref1(_Child, [], _DeRefX, Acc) -> lists:reverse(Acc);
deref1(Child, [{rangeref, _, #rangeref{text = Text}} | T], DeRefX, Acc) ->
    %% only deref the range if it is completely obliterated by the deletion
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
             end,
    deref1(Child, T, DeRefX, [NewTok | Acc]);
deref1(Child, [{cellref, _, #cellref{path = Path, text = Text}}=H | T], 
       DeRefX, Acc) ->
    NewTok = deref2(Child, H, Text, Path, DeRefX),
    deref1(Child, T, DeRefX, [NewTok | Acc]);
deref1(Child, [H | T], DeRefX, Acc) ->
    deref1(Child, T, DeRefX, [H | Acc]).

%% sometimes Text has a prepended slash
deref2(Child, H, [$/|Text], Path, DeRefX) ->
    Deref2 = deref2(Child, H, Text, Path, DeRefX),
    case Deref2 of
        H              -> H;
        {deref,   Str} -> {deref,   "/" ++ Str};
        {recalc,  Str} -> {recalc,  "/" ++ Str};
        {formula, Str} -> {formula, "/" ++ Str}
    end;
%% special case for ambiguous parsing of division
%% this matches on cases like =a1/b3
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

%% if Obj1 completely subsumes Obj2 then the reference to Obj2 should 
%% be dereferenced (return 'deref')
%% %% if Obj1 partially subsumes Obj2 then the reference to Obj2 should
%% be rewitten (return 'rewrite')
%% if there is partial or no overlap then return 'unchanged'
deref_overlap(Text, Obj1, Obj2) ->
    %% the first thing we do is check each corner of Objs2 to see if it is inside
    %% Obj1. Depending on the pattern of corners we rewrite the formula
    %% - if all 4 corners are in the delete area the range must be dereferenced
    %% - if 2 corners are in the delete area the formula must be rewritten
    %% - if 1 corner is in the delete are the range must be deferenced
    %%   because there is no way to rewrite it...
    %% - (if 3 corners are in the delete area then the laws of Euclidean
    %%   geometry have broken down and the end times are probably upon us
    %%   so I would flee for my life sinner!)
    %% 
    %% BUT if all 4 corners are outside the delete area we need to check again:
    %% - if the delete area is wholy inside the range then the range must be deferenced
    %% - if the delete area transpierces the range then the range must be rewritten
    {X1,  Y1,   X2,  Y2}  = expand(Obj1),
    {XX1, YY1,  XX2, YY2} = expand(Obj2),
    IntTL = intersect(XX1, YY1, X1, Y1, X2, Y2),
    IntBL = intersect(XX1, YY2, X1, Y1, X2, Y2),
    IntTR = intersect(XX2, YY1, X1, Y1, X2, Y2),
    IntBR = intersect(XX2, YY2, X1, Y1, X2, Y2),
    case {IntTL, IntBL, IntTR, IntBR} of
        %% all included - deref!
        {in,  in,  in,  in}  -> {deref, "#REF!"};
        %% none included you need to recheck in case the delete area
        %% is contained in, or transects the target area
        {out, out, out, out} -> recheck_overlay(Text, Obj1, Obj2);
        %% one corner included - deref!
        {in,  out, out, out} -> {deref, "#REF!"};
        {out, in,  out, out} -> {deref, "#REF!"};
        {out, out, in,  out} -> {deref, "#REF!"};
        {out, out, out, in}  -> {deref, "#REF!"};
        %% two corners included rewrite
        {in,  in,  out, out} -> rewrite(X1, X2, Obj2, Text, left); %% left del
        {out, out, in,  in}  -> rewrite(X1, Obj2, Text, right);
        {in,  out, in,  out} -> rewrite(Y1, Y2, Obj2, Text, top);  %% top del
        {out, in,  out, in}  -> rewrite(Y1, Obj2, Text, bottom);
        %% transects are column/row intersects
        {transect, transect, out, out} -> rewrite(X1, X2, Obj2, Text, left); %% left del
        {out, out, transect, transect} -> rewrite(X1, Obj2, Text, right);
        {transect, out, transect, out} -> rewrite(Y1, Y2, Obj2, Text, top);  %% top del
        {out, transect, out, transect} -> rewrite(Y1, Obj2, Text, bottom);
        {transect, transect, transect, transect} -> {deref, "#REF!"}
    end.

%% this first clause catches rows/columns where the deleting object is a cell/range
%% in none of these cases does the formula dereference...
intersect(A1, A2, X1, Y1, X2, Y2)
  when (is_atom(A1) orelse is_atom(A2)) andalso
       (is_integer(X1) andalso is_integer(Y1)
        andalso is_integer(X2) andalso is_integer(Y2)) -> out;
%% cols/rows never dereference
intersect(A1, Y1, X1, A2, X2, A3)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(Y1) andalso is_integer(X1) andalso is_integer(X2)) -> out;
%% rows/cols never deference
intersect(X1, A1, A2, Y1, A3, Y2)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(X1) andalso is_integer(Y1) andalso is_integer(Y2)) -> out;
%% page deletes always dereference
intersect(_XX1, _YY1, zero, zero, inf, inf) ->
    out;
%% this is a row-row comparison
intersect(Type, YY1, zero, Y1, inf, Y2)
  when ((Type == zero) orelse (Type == inf)) ->
    if
        (YY1 >= Y1), (YY1 =< Y2) -> transect;
        true                     -> out
    end;
%% this is a col-col comparison
intersect(XX1, Type, X1, zero, X2, inf)
  when ((Type == zero) orelse (Type == inf)) ->
    if
        (XX1 >= X1), (XX1 =< X2) -> transect;
        true                     -> out
    end;
intersect(XX1, YY1, X1, Y1, X2, Y2) ->    
    if
        %% check for cell/range intersections
        (xx1 >= X1),   (XX1 =< X2), (YY1 >= Y1),  (YY1 =< Y2) -> in;
        %% order matters - first check for rows that are included
        (XX1 >= X1),   (XX1 =< X2), (zero == Y1), (inf == Y2) -> in;
        (zero == X1),  (inf == X2), (YY1 >= Y1),  (YY1 =< Y2) -> in;
        %% now check for partial intersections
        (XX1 == zero), (YY1 >= Y1), (YY1 =< Y2)               -> in;
        (XX1 == inf),  (YY1 >= Y1), (YY1 =< Y2)               -> in;
        (YY1 == zero), (XX1 >= X1), (XX1 =< X2)               -> in;
        (YY1 == inf),  (XX1 >= X1), (XX1 =< X2)               -> in;
        true                                                  -> out
    end.         

%% rewrite/5
rewrite(X1O, X2O, {range, _}, Text, left)   ->
    {XD1, _X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1O, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - (X2O - X1O + 1)), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(X1O, X2O, {column, _}, Text, left)   ->
    {XD1, _X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1O) ++ ":" ++ make_col(XD2, (X2 - (X2O - X1O + 1))),
    {recalc, S};

rewrite(Y1O, Y2O, {range, _}, Text, top)   ->
    {XD1, X1, YD1, _Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1O, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - (Y2O - Y1O + 1)), 0),
    {recalc, S};

rewrite(Y1O, Y2O, {row, _}, Text, top)   ->
    {YD1, _Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1O) ++ ":" ++ make_row(YD2, (Y2 - (Y2O - Y1O + 1))),
    {recalc, S}.

%% rewrite/4
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
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - XO), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(XO, {column, _}, Text, middle)  ->
    {XD1, X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1) ++ ":" ++ make_col(XD2, (X2 - XO)),
    {recalc, S};

rewrite(YO, {range, _}, Text, bottom) ->
    {XD1, X1, YD1, Y1, XD2, X2, YD2, _Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (YO - 1), 0),
    {recalc, S};

rewrite(YO, {row, _}, Text, bottom) ->
    {YD1, Y1, YD2, _Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (YO - 1)),
    {recalc, S};

rewrite(YO, {range, _}, Text, middle_row) ->
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - YO), 0),
    {recalc, S};

rewrite(YO, {row, _}, Text, middle) ->
    {YD1, Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (Y2 - YO)),
    {recalc, S}.

%% page deletes always derefence
recheck_overlay(_Text, {page, "/"}, _Target) -> {deref, "#REF!"};
%% cell targets that have matched so far ain't gonna
recheck_overlay(Text, _DelX, {cell, _}) -> {formula, Text};
%% cell deletes that haven't matched a row or column so far ain't gonna
recheck_overlay(Text, {cell, _}, {Type, _})
  when ((Type == row) orelse (Type == column)) -> {formula, Text};
%% cols/rows cols/range comparisons always fail
recheck_overlay(Text, {Type, _}, {column, _})
  when ((Type == row) orelse (Type == range)) -> {formula, Text};
%% rows/cols comparisons always fail
recheck_overlay(Text, {Type, _}, {row, _})
  when ((Type == column) orelse (Type == range)) -> {formula, Text};
%% check a row/row
recheck_overlay(Text, {row, {X1, X2}}, {row, {XX1, XX2}} = Tgt) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2) ->
            rewrite((X2 - X1 + 1), Tgt, Text, middle);
        true ->
            {formula, Text}
    end;
%% check a col/col
recheck_overlay(Text, {column, {Y1, Y2}}, {column, {YY1, YY2}} = Tgt) ->
    if
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) ->
            rewrite((Y2 - Y1 + 1), Tgt, Text, middle);
        true ->
            {formula, Text}
    end;
%% check range/range
recheck_overlay(Text, {range, {X1, Y1, X2, Y2}}, {range, {XX1, YY1, XX2, YY2}}) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2),
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) -> {deref, "#REF!"};
        true                                               -> {formula, Text}
    end;
%% check range/column
recheck_overlay(Text, {column, {X1, X2}}, {range, {XX1, _YY1, XX2, _YY2}} = Tgt) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2) ->
            rewrite((X2 - X1 + 1), Tgt, Text, middle_column);
        true -> {formula, Text}
    end;
%% check range/row
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

%% different to offset_formula because it truncates ranges
offset_fm_w_rng(Cell, [$=|Formula], From, Offset) ->
    %% the xfl_lexer:lex takes a cell address to lex against
    %% in this case {1, 1} is used because the results of this
    %% are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> NewToks = offset_with_ranges(Toks, Cell, From, Offset),
                         make_formula(NewToks);
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                   "formula in offset_fm_w_rng but "++
                                   "you do~n-~p~n", [Formula]),
                         {[], "=" ++ Formula}
    end;
offset_fm_w_rng(_Cell, Value, _From, _Offset) -> Value.

offset_formula(Formula, {XO, YO}) ->
    %% the xfl_lexer:lex takes a cell address to lex against
    %% in this case {1, 1} is used because the results of this
    %% are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> NewToks = d_n_d_c_n_p_offset(Toks, XO, YO),
                         {_St, NewFormula} = make_formula(NewToks),
                         NewFormula;
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                   "formula in offset_formula but "++
                                   "you do~n-~p~n", [Formula]),
                         "=" ++ Formula
    end.

write_formula1(Ref, Fla, Formula, AReq, Attrs) ->
    Rti = refX_to_rti(Ref, AReq, false),
    case muin:run_formula(Fla, Rti) of
        %% TODO : Get rid of this, muin should return {error, Reason}?
        {ok, {_P, {error, error_in_formula}, _, _, _}} ->
            ?ERROR("invalid return from muin:run_formula ~p",[Formula]),
            #refX{site = Site, path = Path, obj = R} = Ref,
            ok = remoting_reg:notify_error(Site,Path,R,error_in_formula,Formula),
            Attrs;
        {error, Error} ->
            #refX{site = Site, path = Path, obj = R} = Ref,
            ok = remoting_reg:notify_error(Site, Path, R,  Error, Formula),
            Attrs;
        {ok, {Pcode, Res, Deptree, Parents, Recompile}} ->
            Parxml = map(fun muin_link_to_simplexml/1, Parents),
            {NewLocPs, _NewRemotePs} = split_local_remote(Parxml),
            ok = set_local_relations(Ref, NewLocPs),
            Align = default_align(Res),
            add_attributes(Attrs, [{"parents", {xml, Parxml}},
                                   {"formula", Formula},
                                   {"rawvalue", Res},
                                   {"__ast", Pcode},
                                   {"__dependency-tree", Deptree},
                                   {"__default-align", Align},
                                   {"__recompile", Recompile}])
    end.

default_align(Res) when is_number(Res) -> "right";
default_align(Res) when is_list(Res)   -> "left";
default_align(_Res)                    -> "center".

write_formula2(Ref, OrigVal, {Type, Val},
               {"text-align", Align}, 
               {"format", Format}, Attrs) ->
    Formula = case Type of
                  quote    -> [$' | Val];
                  datetime -> OrigVal;
                  float    -> OrigVal;
                  int      -> OrigVal;
                  _        -> hn_util:text(Val) end,
    Parxml = map(fun muin_link_to_simplexml/1, []),
    {NewLocPs, _NewRemotePs} = split_local_remote([]),
    ok = set_local_relations(Ref, NewLocPs),
    Attrs2 = add_attributes(Attrs, [{"__dependency-tree", []},
                                    {"__default-align", Align},
                                    {"parents", {xml, Parxml}},
                                    {"rawvalue", Val},
                                    {"formula", Formula}]),
    case Format of
        "null" -> Attrs2;
        _      -> orddict:store("format", Format, Attrs2)
    end.

%% split_parents(Old, New) -> split_parents1(lists:sort(Old),
%%                                           lists:sort(New), {[],[]}).

%% %% if we have run out of OldParents stick the rest of the News on the Write Acc
%% split_parents1([], New, {D, W}) ->
%%     {D, lists:merge([New, W])};
%% %% if NewParents have run out stick the rest of the Olds on the Delete Acc
%% split_parents1(Old, [], {D, W}) ->
%%     {lists:merge([Old, D]), W};
%% %% if the same record appears in Old and New neither delete nor write
%% split_parents1([H | T1], [H | T2], Acc) ->
%%     split_parents1(T1, T2, Acc);
%% %% for every unique old record - delete it
%% split_parents1([H | T], New, {D, W}) ->
%%     split_parents1(T, New, {[H | D], W}).

split_local_remote(List) -> split_local_remote1(List, {[], []}).

split_local_remote1([], Acc) -> Acc;
split_local_remote1([{_, [{_, "local"}], [Url]} | T], {A, B})  ->
    P2 = hn_util:url_to_refX(Url),
    split_local_remote1(T, {[P2 | A], B});
split_local_remote1([{_, [{_, "remote"}], [Url]} | T], {A, B}) ->
    P2 = hn_util:url_to_refX(Url),
    split_local_remote1(T, {A, [P2 | B]}).

%% get_item_list(Type, RefX, Key, Acc) ->
%%     case traverse(Type, RefX, Key) of
%%         {last, []}                    -> {ok, Acc};
%%         {last, [#item{val = Val}]}    -> {ok,lists:append(Val, Acc)};
%%         {NType, NewRefX, []}          -> get_item_list(NType, NewRefX, Key, Acc);
%%         {NType, []}                   -> get_item_list(NType, RefX, Key, Acc);
%%         {NType, NewRefX, [#item{val = Val}]} ->
%%             get_item_list(NType, NewRefX, Key, lists:append(Val, Acc));
%%         {NType,[#item{val = Val}]}     -> 
%%             get_item_list(NType, RefX, Key, lists:append(Val, Acc))
%%     end.

%% return_first(Type, RefX, Key) ->
%%     case traverse(Type, RefX, Key) of
%%         {last, []}                           -> nomatch;
%%         {last, [#item{val = Val}]}           -> {ok, Val};
%%         {NType, []}                          -> return_first(NType, RefX, Key);
%%         {NType, NRefX, []}                   -> return_first(NType, NRefX, Key);
%%         {_NType, _NRefX, [#item{val = Val}]} -> {ok, Val};
%%         {_NType, [#item{val = Val}]}         -> {ok, Val}
%%     end.

add_attributes(D, []) -> D;
add_attributes(D, [{Key, Val}|T]) ->
    D2 = orddict:store(Key, Val, D),
    add_attributes(D2, T).

del_attributes(D, []) -> D; 
del_attributes(D, [Key|T]) ->
    D2 = orddict:erase(Key, D),
    del_attributes(D2, T).

%% traverse(cell, #refX{obj = {cell, _}} = RefX, Key) ->
%%     {range, match_ref(RefX, Key)};
%% traverse(range, #refX{obj = {range, _}} = RefX, Key) ->
%%     {page, match_ref(RefX, Key)};
%% traverse(range, #refX{obj = {cell, _}} = RefX, Key) ->
%%     V = case get_ranges(RefX#refX{obj = {page, "/"}}, Key) of
%%             []   -> [];
%%             List -> case filter_range(List, RefX) of
%%                         nomatch -> [];
%%                         Val     -> [Val]
%%                     end
%%         end,
%%     {row_col, V};
%% traverse(row_col, #refX{obj = {cell, {_X, Y}}} = RefX, Key) ->
%%     {column, match_ref(RefX#refX{obj = {row, {Y, Y}}}, Key)};
%% traverse(row, #refX{obj = {row, _}} = RefX, Key) ->
%%     {page, match_ref(RefX, Key)};
%% traverse(row, #refX{obj = {cell, {_X, Y}}} = RefX, Key) ->
%%     {page, match_ref(RefX#refX{obj = {row, {Y, Y}}}, Key)};
%% traverse(column, #refX{obj = {column, _}} = RefX, Key) ->
%%     {page, match_ref(RefX, Key)};
%% traverse(column, #refX{obj = {cell, {X, _Y}}} = RefX, Key) ->
%%     {page, match_ref(RefX#refX{obj= {column, {X, X}}}, Key)};
%% traverse(page, #refX{path=[]} = RefX, Key) ->
%%     {last, match_ref(RefX#refX{obj = {page,"/"}}, Key)};
%% traverse(page, RefX, Key) ->
%%     NewPath = hslists:init(RefX#refX.path),
%%     {page, RefX#refX{path = NewPath}, match_ref(RefX#refX{obj = {page, "/"}}, Key)}.

%% get_ranges(#refX{site = S, path = P, obj = {page, "/"}}, Key) ->
%%     Head = #local_objs{path = P, obj = {range, '_'}, idx = '$1'},
%%     List1 = mnesia:select(trans(S, local_objs), [{Head, [], ['$_']}]),
%%     List2 = [read_ref(X, Key, read) || X <- List1],
%%     %% now sort the results
%%     %% now convert the list of local_objs into refX's
%%     Fun1 = fun({#local_objs{path = Path, obj = Obj, idx = Idx}, _KV}) ->
%%                    {#refX{site = S, path = Path, obj = Obj}, Idx}
%%            end,
%%     List3 = lists:keysort(2, lists:map(Fun1, List2)),
%%     Fun2 = fun(X, _Y) -> X end,
%%     [Fun2(X, Y) || {X, Y} <- List3].

%% filter_range([], _Cell)     -> nomatch;
%% filter_range([H | T], Cell) ->
%%     case hn_util:in_range(H#refX.obj, Cell#refX.obj) of
%%         true -> H;
%%         _    -> filter_range(T, Cell)
%%     end.

%% @doc Convert Parents and DependencyTree tuples as returned by 
%% Muin into SimpleXML.
muin_link_to_simplexml({Type, {S, P, X1, Y1}}) ->
    Url = hn_util:index_to_url({index, S, P, X1, Y1}),
    {url, [{type, Type}], [Url]}.

%% %% @doc Get the value of a named attribute, if it doesn't exist for address
%% %% check parent (cell -> range -> row -> column -> page -> root -> default)
%% match_ref(#refX{site = S} = RefX, Key) ->
%%     case read_local_item_index(RefX) of
%%         false -> [];
%%         Idx   -> Table = trans(S, item),
%%                  % this function is only used in traverse to 
%%                  % recursively find the value of an attribute
%%                  % so it gets a read lock only..
%%                  case mnesia:read(Table, Idx, read) of
%%                      []   -> [];
%%                      Recs -> IdxNo = ms_util2:get_index(item, key) + 1,
%%                              case lists:keysearch(Key, IdxNo, Recs) of
%%                                  false           -> [];
%%                                  {value, Return} -> [Return]
%%                              end
%%                  end
%%     end.

make_or(Attrs, PlcHoldr)  -> make_clause(Attrs, PlcHoldr, 'or').
%%make_and(Attrs, PlcHoldr) -> make_clause(Attrs, PlcHoldr, 'and').

make_clause(Attrs, PlcHoldr, Op) -> make_clause(Attrs, PlcHoldr, Op, []).

make_clause([], _, Op, Acc)      -> 
    case length(Acc) of
        0 -> [];   % no attributes get everything
        1 ->  Acc; % 1 attribute - no Op statement
        _ -> [list_to_tuple(lists:flatten([Op, Acc]))]
    end;
make_clause([H | T], PH, Op, A)  -> 
    make_clause(T, PH, Op, [{'==', PH, H} | A]).

%% delete_style_attr(#refX{site = S} = RefX, Key)  ->
%%     %% this function works by overwriting the set style attribute in the
%%     %% current style record with []
%%     [{RefX, {"style", Idx}}] = read_attrs(RefX, ["style"], write),
%%     PageRefX = RefX#refX{obj = {page, "/"}},
%%     Match = #styles{refX = PageRefX, index = Idx, _ = '_'},
%%     Table = trans(S, styles),
%%     [CurrentStyle] = mnesia:index_match_object(Table, Match, 1, read),
%%     NewStyleIdx = get_style(RefX, CurrentStyle, Key, []),
%%     write_attr3(RefX, {"style", NewStyleIdx}).


%% this function is called when a new attribute is set for a style
-spec apply_style(#refX{}, {string(), term()}, ?dict) -> ?dict.
apply_style(Ref, {Name, Val}, Attrs) ->
    NewSIdx = case orddict:find("style", Attrs) of
                  {ok, Idx} -> edit_style(Ref, Idx, Name, Val);
                  _         -> new_style(Ref, Name, Val)
              end,
    orddict:store("style", NewSIdx, Attrs).

new_style(Ref, Name, Val) ->
    NoOfFields = ms_util2:no_of_fields(magic_style), 
    Index = ms_util2:get_index(magic_style, Name), 
    Style = make_tuple(magic_style, NoOfFields, Index, Val), 
    %% Now write the style 
    write_style(Ref, Style). 

edit_style(#refX{site = Site} = RefX, StIdx, Name, Val) ->
    PageRefX = RefX#refX{obj = {page, "/"}},
    Match = #styles{refX = PageRefX, index = StIdx, _ = '_'},
    Table = trans(Site, styles),
    Return = mnesia:index_match_object(Table, Match, 1, read),
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
    %% Ref is a cell ref, need a page ref
    Ref2 = RefX#refX{obj = {page, "/"}},
    Match = #styles{refX = Ref2, magic_style = Style, _ = '_'},
    Table = trans(Site, styles),
    case mnesia:index_match_object(Table, Match, 1, read) of 
        []                          -> write_style2(RefX, Style); 
        [#styles{index = NewIndex}] -> NewIndex
    end. 

write_style2(#refX{site = Site} = RefX, Style) ->
    %% Ref is a cell reference - a page reference is needed
    Ref2 = RefX#refX{obj = {page, "/"}},
    NewIndex = mnesia:dirty_update_counter(trans(Site, style_counters), Ref2, 1), 
    ok = tell_front_end_style(Ref2, NewIndex, Style),
    Rec = #styles{refX = Ref2, index = NewIndex, magic_style = Style},
    ok = mnesia:write(trans(Site, styles), Rec, write),
    NewIndex. 


tell_front_end_style(Ref, NewIndex, Style) ->
    Tuple = {style, Ref, NewIndex, Style},
    tell_front_end1(Tuple).
tell_front_end_change(Ref, Attrs) ->
    Tuple = {change, Ref, Attrs},
    tell_front_end1(Tuple).

tell_front_end1(Tuple) ->
    List = get('front_end_notify'),
    put('front_end_notify', [Tuple | List]),
    ok.

make_tuple(Style, Counter, Index, Val) -> 
    make_tuple1(Style, Counter, Index, Val, []). 
make_tuple1(S, 0, _I, _V, Acc) -> list_to_tuple([S|Acc]); 
make_tuple1(S, I, I, V, Acc )  -> make_tuple1(S, I -1 , I, V, [V | Acc]); 
make_tuple1(S, C, I, V, Acc)   -> make_tuple1(S, C - 1, I, V, [[] | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                              %
%% Debugging interface                                                          %
%%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deref_overlap_TEST() ->
    io:format("~n~n~n~n~n~n"),
    Tests =[
            %% cells match
            {"A1",    {cell, {1, 1}},        {cell, {1, 1}},        {deref, "#REF!"}},
            %% cells don't match
            {"A1",    {cell, {2, 1}},        {cell, {1, 1}},        {formula, "A1"}},
            %% cell in a range
            {"A1",    {cell, {1, 1}},        {range, {1, 1, 2, 3}}, {deref, "#REF!"}},
            %% cell not in a range
            {"A1",    {cell, {4, 4}},        {range, {1, 1, 2, 3}}, {formula, "A1"}},

            %% ranges match
            {"A1:B2", {range, {1, 1, 2, 2}}, {range, {1, 1, 2, 2}}, {deref, "#REF!"}},
            %% ranges don't overlap
            {"A1:B2", {range, {1, 1, 2, 2}}, {range, {3, 3, 4, 4}}, {formula, "A1:B2"}},
            %% target range inside delete range
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 5, 5}}, {deref, "#REF!"}},
            %% delete range inside target range
            {"B2:E5", {range, {2, 2, 5, 5}}, {range, {3, 3, 4, 4}}, {deref, "#REF!"}},
            %% delete range clips top-left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 2, 2}}, {deref, "#REF!"}},
            %% delete range clips bottom-right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {3, 3, 5, 5}}, {deref, "#REF!"}},
            %% delete range clips top-right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {4, 1, 5, 2}}, {deref, "#REF!"}},
            %% delete range clips bottom-left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 4, 2, 5}}, {deref, "#REF!"}},
            %% delete range slices left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 2, 9}}, {recalc, "A2:B4"}},
            %% delete range slices top
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 7, 2}}, {recalc, "B1:Dl2"}},
            %% delete range slices bottom
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 4, 5, 9}}, {recalc, "B2:D3"}},
            %% delete range slices right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {4, 1, 5, 9}}, {recalc, "B2:C4"}},

            %% cell in a column
            {"C5",    {cell, {3, 5}},        {column, {3, 4}},      {deref, "#REF!"}},
            %% cell not in a column
            {"C5",    {cell, {3, 5}},        {column, {6, 7}},      {formula, "C5"}},
            %% range in a column (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {3, 5}},      {deref, "#REF!"}},
            %% range in a column (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {2, 6}},      {deref, "#REF!"}},
            %% delete columns slices left (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {3, 3}},      {recalc, "C5:D8"}},
            %% delete columns slices left (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {2, 3}},      {recalc, "B5:C8"}},
            %% delete columns slices right (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {5, 5}},      {recalc, "C5:D8"}},
            %% delete columns slices right (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {5, 6}},      {recalc, "C5:D8"}},
            %% delete column slices middle
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {4, 4}},      {recalc, "C5:D8"}},


            %% cell in a row
            {"C5",    {cell, {3, 5}},        {row, {4, 6}},         {deref, "#REF!"}},
            %% cell not in a row
            {"C5",    {cell, {3, 5}},        {row, {6, 7}},         {formula, "C5"}},
            %% range in a row (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {5, 8}},         {deref, "#REF!"}},
            %% range in a row (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {4, 9}},         {deref, "#REF!"}},
            %% delete row slices top (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {5, 5}},         {recalc, "C5:D7"}},
            %% delete row slices top (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {4, 5}},         {recalc, "C4:D6"}},
            %% delete row slices bottom (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {8, 8}},         {recalc, "C5:D7"}},
            %% delete row slices bottom (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {8, 9}},         {recalc, "C5:D7"}},
            %% delete row slices middle
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {6, 7}},         {recalc, "C5:D6"}},

            %% columns can't be derefed by cell deletes
            {"C:F",   {column, {3, 6}},      {cell, {2, 3}},        {formula, "C:F"}},
            %% columns can't be derefed by range deletes
            {"C:F",   {column, {3, 6}},      {range, {2, 3, 4, 5}}, {formula, "C:F"}},
            %% columns can't be derefed by row deletes
            {"C:F",   {column, {3, 6}},      {row, {2, 3}},         {formula, "C:F"}},
            %% column inside a column delete (1)
            {"C:F",   {column, {3, 6}},      {column, {2, 7}},      {deref, "#REF!"}},
            %% column inside a column delete (2)
            {"C:F",   {column, {3, 6}},      {column, {3, 6}},      {deref, "#REF!"}},
            %% column not inside a column delete
            {"C:F",   {column, {3, 6}},      {column, {8, 9}},      {formula, "C:F"}},
            %% column delete slices left (1)
            {"C:F",   {column, {3, 6}},      {column, {3, 3}},      {recalc, "C:E"}},
            %% column delete slices left (2)
            {"C:F",   {column, {3, 6}},      {column, {2, 4}},      {recalc, "B:C"}},
            %% column delete slices right (1)
            {"C:F",   {column, {3, 6}},      {column, {6, 6}},      {recalc, "C:E"}},
            %% column delete slices right (2)
            {"C:F",   {column, {3, 6}},      {column, {5, 7}},      {recalc, "C:D"}},
            %% column delete slices middles
            {"C:F",   {column, {3, 6}},      {column, {5, 5}},      {recalc, "C:E"}},

            %% rows can't be derefed by cell deletes
            {"3:6",   {row, {3, 6}},         {cell, {2, 3}},        {formula, "3:6"}},
            %% rows can't be derefed by range deletes
            {"3:6",   {row, {3, 6}},         {range, {2, 3, 4, 5}}, {formula, "3:6"}},
            %% rows can't be derefed by column deletes
            {"3:6",   {row, {3, 6}},         {column, {2, 3}},      {formula, "3:6"}},
            %% row inside a row delete (1)
            {"3:6",   {row, {3, 6}},         {row, {3, 6}},         {deref, "#REF!"}},
            %% row inside a row delete (2)
            {"3:6",   {row, {3, 6}},         {row, {2, 7}},         {deref, "#REF!"}},
            %% row not inside a row delete
            {"3:6",   {row, {3, 6}},         {row, {8, 9}},         {formula, "3:6"}},
            %% row slices top (1)
            {"3:6",   {row, {3, 6}},         {row, {3, 3}},         {recalc, "3:5"}},
            %% row slices top (2)
            {"3:6",   {row, {3, 6}},         {row, {2, 3}},         {recalc, "2:4"}},
            %% row slices bottom (1)
            {"3:6",   {row, {3, 6}},         {row, {6, 6}},         {recalc, "3:5"}},
            %% row slices bottom (2)
            {"3:6",   {row, {3, 6}},         {row, {6, 7}},         {recalc, "3:5"}},
            %% row slices middle
            {"3:6",   {row, {3, 6}},         {row, {4, 4}},         {recalc, "3:5"}}
           ],
    [test_ov(X) || X <- Tests].

test_ov({Text, Cell, DelX, Return}) ->
    Return1 = deref_overlap(Text, DelX, Cell),
    case Return of
        Return1 -> ok; 
        _       -> io:format("Fail: ~p : ~p : ~p : ~p - ~p~n",
                             [Text, Cell, DelX, Return, Return1])
    end.
