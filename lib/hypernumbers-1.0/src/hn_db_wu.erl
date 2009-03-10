%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This is a util function for hn_db_api containing only functions
%%%            that MUST be called from within an mnesia transactions.
%%% 
%%% The module {@link hn_db_api} is the wrapper for calls to this
%%% function.
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
%%% The others return {ok, ok}
%%% 
%%% <h4>Dirty Management</h4>
%%% 
%%% A thing can be set dirty by call a function with a name like 
%%% <code>mark somethineg dirty</code> and when a dirty thing has been
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
%%% <code>parents</code> and <code>dependency-tree</code>
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
%%% <li>hn_item</li>
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
%%% <h4>1 hn_item</h4>
%%% 
%%% contains all the atributes of the cell plus attributes of columns, rows, pages
%%% etc, etc - including stuff not documented here like permissions
%%% 
%%% <h4>2 local_cell_link</h4>
%%% 
%%% contains parent-child pig's ear links of cells stored in hn_item
%%% 
%%% <h4>3 remote_cell_link</h4>
%%% 
%%% contains parent-child links that connect cells stored in hn_item 
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
%%% <li>move a cell that only has a value</li>
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
%%% <li>a record is written to hn_item</li>
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
%%% <li>a record is written to <code>hn_item</code></li>
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
%%% <li>a record is written to <code>hn_item</code></li>
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
%%% done better by some sort of 'mark dependency-tree dirty' 
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
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_wu).

%% Cell Query Exports
-export([write_attr/2,       % tested
         read_cells/1,       % tested
         read_cells_raw/1,
         read_attrs/1,       % tested
         read_attrs/2,       % tested
         read_inherited/3,
         read_styles/1,
         read_incoming_hn/1,
         read_outgoing_hns/1,
         clear_cells/1,
         clear_cells/2,
         delete_attrs/2,
         delete_outgoing_hn/3,
         clear_dirty_cell/1,
         clear_dirty_inc_hn_create/2,
         clear_dirty_notify_in/1,
         clear_dirty_notify_out/1,
         clear_dirty_notify_back_in/3,
         clear_dirty_notify_back_out/3,
         shift_cell/2,
         copy_cell/3,
         copy_attrs/3,
         get_cells/1,
         mark_cells_dirty/1,
         mark_inc_create_dirty/2,
         mark_notify_in_dirty/1,
         mark_notify_out_dirty/3,
         mark_notify_back_in_dirty/3,
         mark_notify_back_out_dirty/3,
         update_inc_hn/5,
         does_remote_link_exist/3,
         write_remote_link/3,
         verify_biccie/3]).

%% Structural Query Exports
-export([get_last_refs/1,
         get_refs_below/1,
         get_refs_right/1]).

%% These functions are exposed for the dirty_srv to use
-export([read_local_parents/1,
         read_local_children/1,
         read_remote_parents/2,
         read_remote_children/2]).

%% Debugging
-export([dump/0]).
-export([dump/2]).

-define(to_xml_str, simplexml:to_xml_string).
-define(to_refX, hn_util:refX_from_index).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Exported functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
does_remote_link_exist(Parent, Child, Type)
  when is_record(Parent, refX), is_record(Child, refX) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    ChildIdx = hn_util:index_from_refX(Child),
    Head = ms_util:make_ms(remote_cell_link, [{parent, ParentIdx},
                                              {child, ChildIdx},
                                              {type, Type}]),
    Match = [{Head, [], ['$_']}],
    case mnesia:dirty_select(remote_cell_link, Match) of
        []                                          -> false;
        [Rec] when is_record(Rec, remote_cell_link) -> true
    end.

verify_biccie(Parent, Child, Biccie)
  when is_record(Parent, refX), is_record(Child, refX) ->
    #refX{site = ChildSite} = Child,
    PIdx = hn_util:index_from_refX(Parent),
    Match = ms_util:make_ms(outgoing_hn, [{parent, PIdx},
                                          {child_site, ChildSite}]),
    [Hn] = mnesia:match_object(outgoing_hn, Match, read),
    #outgoing_hn{biccie = Biccie2} = Hn,
    case Biccie of
        Biccie2 -> true;
        _       -> false
    end.

%% @spec mark_notify_back_in_dirty(Parent::#refX{}, Child::#refX{}, Msg) -> 
%% {ok, ok}
%% @doc marks a child of a remote hypernumber as dirty so that the remote 
%% (parent) site is to be notified that the link has been created.
%% Both Child and Parent must be a cell reference.
%% The message can be one of:
%% <ul>
%% <li>unregister</li>
%% <li>new child</li>
%% </ul>
mark_notify_back_in_dirty(Parent, Child, Msg)
  when is_record(Parent, refX), is_record(Child, refX) ->
    P = hn_util:index_from_refX(Parent),
    C = hn_util:index_from_refX(Child),
    Rec = #dirty_notify_back_in{parent = P, child = C, change = Msg},
    Match = ms_util:make_ms(dirty_notify_back_in, [{parent, P}, {child, C},
                                                  {change, Msg}]),
    case mnesia:match_object(Match) of
        [] -> ok = mnesia:write(Rec);
        _  -> ok
    end,
    {ok, ok}.

%% @spec mark_notify_back_out_dirty(Parent::#refX{}, Child::#refX{}, Msg) -> 
%% {ok, ok}
%% @doc marks the parent of a hypernumber as dirty because a remote site
%% has sent a message about changes to a child of that cell.
%% Both Child and Parent must be a cell referece.
%% The message can be one of:
%% <ul>
%% <li>unregister</li>
%% <li>new_child</li>
%% </ul>
mark_notify_back_out_dirty(Parent, Child, Msg)
  when is_record(Parent, refX), is_record(Child, refX) ->
    P = hn_util:index_from_refX(Parent),
    C = hn_util:index_from_refX(Child),
    Rec = #dirty_notify_back_out{child = C, parent = P, change = Msg},
    Match = ms_util:make_ms(dirty_notify_back_out, [{parent, P}, {child, C},
                                                    {change, Msg}]),
    case mnesia:match_object(Match) of
        [] -> ok = mnesia:write(Rec);
        _  -> ok
    end,
    {ok, ok}.

%% @spec write_remote_link(Parent::#refX{}, Child::#refX{}, Type) -> {ok, ok}
%% @doc writes a remote link between the parent and the child.
%% Both parent and child references must be to a cell. The parent is on the
%% remote site and the child is on the local site
write_remote_link(Parent, Child, Type)
  when is_record(Parent, refX), is_record(Child, refX),
       (Type =:= incoming orelse Type =:= outgoing)->
    P = hn_util:index_from_refX(Parent),
    C = hn_util:index_from_refX(Child),
    Rec = #remote_cell_link{parent = P, child = C, type = Type},
    ok = mnesia:write(Rec),
    {ok, ok}.

%% @spec update_inc_hn(Parent::#refX{}, Val, DepTree, Biccie, Version) -> {ok, ok}
%% DepTree = list()
%% Version = integer()
%% @doc update_inc_hn will try and update the incoming hypernumber with
%% a new value.
%% If the page versions of the originating page are out of synch with what we
%% have this will trigger a page version update and ensure that they are resynched
%% (<em>This is critical for dealing with server downtime</em>)
%% This function also triggers the child cells as dirty so they recalculate
%% @todo put code in to deal with page versions! (^^ lying in the docos above)
update_inc_hn(Parent, Val, DepTree, Biccie, Version)
  when is_record(Parent, refX), is_list(DepTree) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    Rec = #incoming_hn{remote = ParentIdx, value = Val, 
                       'dependency-tree' = DepTree, biccie = Biccie,
                       version = Version},
    ok = mnesia:write(Rec),
    {ok, ok} = mark_notify_in_dirty(Parent),
    {ok, ok}.

%% @spec mark_inc_create_dirty(Parent::#refX{}, Child::#refX{}) -> {ok, ok}
%% @doc marks a hypernmber (a remote reference) as not yet created.
%% The reference is to a cell only
mark_inc_create_dirty(Parent, Child) when is_record(Parent, refX),
                                          is_record(Child, refX)->
    ParentIdx = hn_util:index_from_refX(Parent),
    ChildIdx = hn_util:index_from_refX(Child),
    Rec = #dirty_inc_hn_create{parent = ParentIdx, child = ChildIdx},
    ok = mnesia:write(Rec),
    {ok, ok}.

%% @spec get_cells(RefX::#refX{}) -> [#refX{}]
%% @doc takes a reference and expands it to cell references.
%% The reference can be any of:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>row</li>
%% <li>column</li>
%% <li>page</li>
%% </ul>
%% and the return is a list of cell references
get_cells(#refX{obj = {cell, _}} = RefX) -> [RefX];
get_cells(#refX{obj = {range, _}} = RefX) ->
    MatchRef = make_range_match(RefX, []),
    get_cells1(MatchRef);
get_cells(#refX{obj = {row, _}} = RefX) ->
    MatchRef = make_row_match(RefX, []),
    get_cells1(MatchRef);
get_cells(#refX{obj = {column, _}} = RefX) ->
    MatchRef = make_column_match(RefX, []),
    get_cells1(MatchRef);
get_cells(#refX{obj = {page, _}} = RefX) ->
    MatchRef = make_page_match(RefX, []),
    get_cells1(MatchRef).

get_cells1(MatchRef) ->
    % - get all attributes
    % - strip out their refX's and
    % - return a unique list of them...
    List = mnesia:select(hn_item, [MatchRef]),
    get_refXs(List).

%% @spec clear_dirty_cell(RefX::#refX{}) -> {ok, ok}
%% @doc clears a dirty cell marker.
%% The reference must be to a cell
clear_dirty_cell(#refX{obj = {cell, _}} = RefX) ->
    Index = hn_util:index_from_refX(RefX),
    mnesia:delete({dirty_cell, Index}),
    {ok, ok}.

%% @spec clear_dirty_inc_hn_create(Parent::#refX{}, Child::#refX{}) -> {ok, ok}
%% @doc clears a marked dirty_inc_hn_create
clear_dirty_inc_hn_create(Parent, Child)
  when is_record(Parent, refX), is_record(Child, refX) ->
    PIdx = hn_util:index_from_refX(Parent),
    CIdx = hn_util:index_from_refX(Child),
    Match = ms_util:make_ms(dirty_inc_hn_create, [{parent, PIdx},
                                                  {child, CIdx}]),
    Dirty = mnesia:match_object(dirty_inc_hn_create, Match, read),
    {ok, ok} = delete_recs(Dirty).

%% @spec clear_dirty_notify_in(Parent::#refX{}) -> {ok, ok}
%% @doc clears a dirty notification.
%% The parent reference must point to a cell
%% @todo extend the reference to include rows, columns, ranges, etc, etc
clear_dirty_notify_in(Parent) when is_record(Parent, refX) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    ok = mnesia:delete({dirty_notify_in, ParentIdx}),
    {ok, ok}.

%% @spec clear_dirty_notify_out(Parent::#refX{}) -> {ok, ok}
%% @doc clears a dirty notification.
%% The parent reference must point to a cell
%% @todo extend the reference to include rows, columns, ranges, etc, etc
clear_dirty_notify_out(Parent) when is_record(Parent, refX) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    ok = mnesia:delete({dirty_notify_out, Parent}),
    {ok, ok}.

%% @spec clear_dirty_notify_back_in(Parent::#refX{}, Child::#refX{}, Change) -> 
%% {ok, ok}
%% @doc clears a dirty notify back_in.
%% Both the parent and the child references must point to a cell
%% @todo extend the references to include rows, columns, ranges, etc, etc
clear_dirty_notify_back_in(Parent, Child, Change)
  when is_record(Parent, refX), is_record(Child, refX) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    ChildIdx = hn_util:index_from_refX(Child),
    Head = ms_util:make_ms(dirty_notify_back_in, [{parent, ParentIdx},
                                                   {child, ChildIdx},
                                                   {change, Change}]),
    [Record] = mnesia:select(dirty_notify_back_in, [{Head, [], ['$_']}]),
    ok = mnesia:delete_object(Record),
    {ok, ok}.

%% @spec clear_dirty_notify_back_out(Parent::#refX{}, Child::#refX{}, Change) -> 
%% {ok, ok}
%% @doc clears a dirty notify back out.
%% Both the parent and the child references must point to a cell
%% @todo extend the references to include rows, columns, ranges, etc, etc
clear_dirty_notify_back_out(Parent, Child, Change)
  when is_record(Parent, refX), is_record(Child, refX) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    ChildIdx = hn_util:index_from_refX(Child),
    Head = ms_util:make_ms(dirty_notify_back_out, [{parent, ParentIdx},
                                                   {child, ChildIdx},
                                                   {change, Change}]),
    [Record] = mnesia:select(dirty_notify_back_out, [{Head, [], ['$_']}]),
    ok = mnesia:delete_object(Record),
    {ok, ok}.

%% @spec delete_outgoing_hn(Parent::#refX{}, Child::#refX{}, Biccie) -> {ok, ok}
%% @doc deletes an outgoing hypernumber if the biccie quoted is correct.
%% 
%% The parent and the child references both point to a cell.
%% The biccie is the biccie which was originally given when the
%%  hypernumber was set up.
%% 
%% @todo at the moment the <code>refX{}</code> can only point to a cell
%% should be a more general reference (including eventually a query reference)
delete_outgoing_hn(Parent, Child, Biccie) ->
    ParentIndex = hn_util:index_from_refX(Parent),
    ChildIndex = hn_util:index_from_refX(Child),
    #index{site = ChildSite} = ChildIndex,
    Head = ms_util:make_ms(outgoing_hn, [{parent, ParentIndex},
                                         {child_site, ChildSite}, {biccie, Biccie}]),
    Hypernumbers = mnesia:select(outgoing_hn, [{Head, [], ['$_']}]),
    delete_recs(Hypernumbers).

%% @spec get_refs_below(#ref{}) -> [#ref{}]
%% @doc gets all the refs below a given reference.
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
get_refs_below(#refX{obj = {row, {Y1, Y2}}} = RefX) ->
    % rectify the row range in case they are reversed...
    YY = ?COND(Y1 > Y2, Y1, Y2),
    #refX{site = S, path = P} = RefX,
    Obj = {cell, {'_', '$1'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, Obj}]),
    Cond = [{'>', '$1', YY}],
    Body = ['$_'],
    get_match_refs([{MatchRef, Cond, Body}]);
get_refs_below(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    % rectify the ranges in case they are reversed...
    YY = ?COND(Y1 > Y2, Y1, Y2),
    {XX1, XX2} = ?COND(X1 > X2, {X2, X1}, {X1, X2}),
    get_refs_below2(RefX, XX1, XX2, YY).

%% @spec get_refs_right(#ref{}) -> [#ref{}]
%% @doc gets all the refs to the right of a given reference. 
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
get_refs_right(#refX{obj = {column, {X1, X2}}} = RefX) ->
    % rectify the row range in case they are reversed...
    XX = ?COND(X1 > X2, X1, X2),
    #refX{site = S, path = P} = RefX,
    Obj = {cell, {'$1', '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, Obj}]),
    Cond = [{'>', '$1', XX}],
    Body = ['$_'],
    get_match_refs([{MatchRef, Cond, Body}]);
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
    Cells = read_cells(RefX2),
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

%% @spec read_remote_parents(RefX :: #refX{}, type) -> [#refX{}]
%% @doc this returns the remote parents of a reference.
%% 
%% The reference can only be to a cell and not a range, column, row or page
%% 
%% This fn is called read_remote_parents:
%% <ul>
%% <li>if the type is <code>incoming</code> and the reference is to a remote cell it 
%% returns all the parents of that remote cell on this site</li>
%% <li>if the type is <code>outgoing</code> and the reference is to a remote cell it
%% returns all the parents of the local cell on the remote server</li>
%% </ul>
read_remote_parents(#refX{obj = {cell, _}} = RefX, Type)
  when Type =:= incoming; Type =:= outgoing ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(remote_cell_link, [{child, Index}, {type, Type}]),
    Links = mnesia:match_object(remote_cell_link, Match, read),
    get_remote_parents(Links).

%% @spec read_remote_children(RefX :: #refX{}, Type) -> [#refX{}]
%% @doc this returns the remote children of a reference.
%% 
%% The reference can only be to a cell and not a range, column, row or page
%% 
%% This fn is called read_remote_children:
%% <ul>
%% <li>if the type is <code>incoming</code> and the reference is to a remote cell it 
%% returns all the children of that remote cell</li>
%% <li>if the type is <code>outgoing</code> and the reference is to a local cell it
%% returns all the children of the local cell on the remote server</li>
%% </ul>
read_remote_children(#refX{obj = {cell, _}} = RefX, Type)
  when Type =:= incoming; Type =:= outgoing ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(remote_cell_link, [{parent, Index},
                                               {type, Type}]),
    Links = mnesia:match_object(remote_cell_link, Match, read),
    get_remote_children(Links).

%% @spec read_local_parents(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the local parents of a reference. The reference can only
%% be to a cell and not a range, column, row or page
%% 
%% This fn is called read_local_parents because it consists of all the
%% local links where the current RefX is the child
read_local_parents(#refX{obj = {cell, _}} = RefX)  ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(local_cell_link, [{child, Index}]),
    Links = mnesia:match_object(local_cell_link, Match, read),
    get_local_parents(Links).

%% @spec read_local_children(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the local children of a reference. The reference can only
%% be to a cell and not a range, column, row or page
%% 
%% This fn is called read_local_children because it consists of all the
%% local links where the current RefX is the parent
read_local_children(#refX{obj = {cell, _}} = RefX) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(local_cell_link, [{parent, Index}]),
    Links = mnesia:match_object(local_cell_link, Match, read),
    get_local_children(Links).

%% @spec write_attr(RefX :: #refX{}, {Key, Value}) -> {ok, ok}
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
write_attr(#refX{obj = {cell, _}} = RefX, {formula, _} = Attr) ->
    % first check that the formula is not part of a shared array
    case read_attrs(RefX, ['__shared']) of
        [_X] -> throw({error, cant_change_part_of_array});
        []   -> write_attr2(RefX, Attr)
    end;
write_attr(#refX{obj = {cell, _}} = RefX, {Key, Val} = Attr) ->
    % NOTE the attribute 'overwrite-color' isn't in a magic style and shouldn't be
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> process_styles(RefX, Attr);
        false -> write_attr3(RefX, {Key, Val})
    end, 
    {ok, ok};
write_attr(#refX{obj = {range, _}} = RefX, Attr) ->
    Ref = hn_util:refX_to_ref(RefX, "not needed"),
    List = hn_util:range_to_list(Ref),
    List2 = [hn_util:ref_to_refX(X, "not needed") || X <- List],
    lists:flatten([write_attr(X, Attr) || {X, _Discard} <- List2]);
%% for the rest just write'em out
write_attr(RefX, {Key, Val}) when is_record(RefX, refX) ->
    Ref = hn_util:refX_to_ref(RefX, Key),
    Record = #hn_item{addr = Ref, val = Val},
    ok = mnesia:write(Record),
    spawn(fun() -> tell_front_end(Record, change) end),
    {ok, ok}.

%% @spec read_cells(#refX{}) -> [{#refX{}, {Key, Value}}]
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
read_cells(Ref) ->
    List = read_cells_raw(Ref),
    drop_private(List).

%% @spec read_cells_raw(#refX{}) -> [{#refX{}, {Key, Value}}]
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
read_cells_raw(#refX{obj = {cell, _}} = RefX) ->
    #refX{site = S, path = P, obj= R} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    hn_util:from_hn_item(mnesia:match_object(hn_item, Match, read));
read_cells_raw(#refX{obj = {range, _}} = RefX) ->
    MatchRef = make_range_match(RefX, []),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_cells_raw(#refX{obj = {column, _}} = RefX) ->
    MatchRef = make_column_match(RefX, []),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_cells_raw(#refX{obj = {row, _}} = RefX) ->
    MatchRef = make_row_match(RefX, []),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_cells_raw(#refX{obj = {page, _}} = RefX) ->
    MatchRef = make_page_match(RefX, []),    
    hn_util:from_hn_item(mnesia:match_object(hn_item, MatchRef, read)).

%% @spec read_inherited(#refX{}, Key, Default) -> {ok, Value}
%% Key = atom()
%% Value = term()
%% Default = term()
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at a given reference, if not found it returns the supplied
%%       default value
%%       
%% @todo what are the ref types it supports? improve the documentation, etc, etc
read_inherited(RefX, Key, Default) when is_record(RefX, refX)  ->
    Ref = hn_util:refX_to_ref(RefX, Key),
    case return_first(cell, Ref) of
        {ok, Value} -> {ok, Value};
        nomatch     -> {ok, Default}
    end.

%% @spec read_attrs(#refX{}) -> [{#refX{}, {Key, Value}}]
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
read_attrs(RefX) when is_record(RefX, refX) ->
    read_attrs(RefX, []).

%% @spec read_attrs(#refX{}, AttrsList) -> [{#refX{}, {Key, Value}}]
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
read_attrs(#refX{obj = {range, _}} = RefX, Attrs) when is_list(Attrs) ->
    
    MatchRef = make_range_match(RefX, Attrs),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));

read_attrs(#refX{obj = {column, _}} = RefX, Attrs) when is_list(Attrs) ->

    MatchRef = make_column_match(RefX, Attrs),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));

read_attrs(#refX{obj = {row, _}} = RefX, Attrs) when is_list(Attrs) ->

    MatchRef = make_row_match(RefX, Attrs),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));

read_attrs(#refX{obj = {cell, _}} = RefX, Attrs) when is_list(Attrs) ->

    #refX{site = S, path = P, obj= R} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P},
                                     {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, Attrs);

read_attrs(#refX{obj = {page, _}} = RefX, Attrs) when is_list(Attrs) ->
    #refX{site = S, path = P} = RefX,
    R = {cell, {'_', '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P},
                                     {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, Attrs).

%% @spec shift_cell(RefX :: #refX{}, Offset :: {X :: integer(), 
%% Y :: integer()}) -> {ok, ok}
%% @doc shift_cells takes a cell and shifts it by the offset
%% 
%% Provided that offset is valid of course - ie the cell can't be 
%% made negative (mebbies it should be).
shift_cell(From, To) when is_record(From, refX), is_record(To, refX) ->
    % the order is IMPORTANT (I think) GG :(
    io:format("in shift_cell~n-From is ~p~n-To is ~p~n", [From, To]),
    {ok, ok} = shift_dirty_cells(From, To),
    {ok, ok} = shift_outgoing_hns(From, To),
    {ok, ok} = shift_incoming_hns(From, To),
    {ok, ok} = shift_dirty_notify_ins(From, To),
    % {ok, ok} = shift_dirty_notify_outs(From, To),
    {ok, ok} = shift_cell2(From, To),
    {ok, ok} = shift_local_links(From, To),
    % dump(To, "and now here..."),
    {ok, ok}.

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
read_styles(#refX{obj = {page, _}} = RefX) ->
    Ref = hn_util:refX_to_ref(RefX, style),
    Match = ms_util:make_ms(styles, [{ref, Ref}]),
    mnesia:match_object(Match);
read_styles(RefX) when is_record(RefX, refX) ->
    % first get the style records to get the indexes
    CellList = read_attrs(RefX, [style]),
    IndexList = hslists:uniq(extract_values(CellList)),
    Ref = hn_util:refX_to_ref(RefX, style),
    % Ref is a cell/column/row/range ref - make it a page ref
    Ref2 = Ref#ref{ref = {page, "/"}},
    Match = ms_util:make_ms(styles, [{ref, Ref2}, {index, '$1'}]),
    Cond = make_or(IndexList, '$1'),
    Body = ['$_'],
    mnesia:select(styles, [{Match, Cond, Body}]).

%% @spec clear_cells(#refX{}) -> {ok, ok}
%% @doc deletes the contents (formula/value) and the formats and attributes
%% of a cell (but doesn't delete the cell itself)
%%  
%% The same as clear_cells(RefX, contents).
clear_cells(RefX) when is_record(RefX, refX) ->
    clear_cells(RefX, contents).

%% @spec clear_cells(#refX{}, Type) -> {ok, ok} 
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
    clear_cells(RefX, style),
    clear_cells(RefX, contents);
clear_cells(RefX, style) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX, [style]),
    List2 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
             || {X, {Key, Val}} <- List1],
    delete_recs(List2);    
clear_cells(RefX, contents) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX),
    case List1 of
        [] -> {ok, ok};
        _  -> List2 = get_refXs(List1),
              [{ok, ok} = delete_links(X) || X <- List2],
              % now delete all the attributes
              List3 = get_content_attrs(List1),
              List4 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
                       || {X, {Key, Val}} <- List3],
              {ok, ok} = delete_recs(List4),
              [{ok, ok} = mark_cells_dirty(X) || X <- List2],
              {ok, ok}
    end.

%% @spec delete_attrs(RefX :: #refX{}, Key) -> {ok, ok}
%% Key = atom()
%% @doc deletes a named attribute from a
%% cell or cells (but doesn't delete the cells themselve)
delete_attrs(RefX, Key) ->
    Ref = hn_util:refX_to_ref(RefX, Key),
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> delete_style_attr(RefX, Key);
        false -> ok  = mnesia:delete({hn_item, Ref}),
                 Record = #hn_item{addr = Ref, val = "not important"},
                 spawn(fun() -> tell_front_end(Record, delete) end),
                 {ok, ok}
    end.

%% @spec copy_cell(From :: #refX{}, To ::#refX{}, Incr) -> {ok, ok}
%% Incr = [false | horizonal | vertical]
%% @doc copys cells from a reference to a reference
copy_cell(#refX{obj = {cell, _}} = From, #refX{obj = {cell, _}} = To, Incr)
  when is_record(From, refX), is_record(To, refX) ->
    FromList = read_cells_raw(From),
    {Contents, FilteredList} = filter_for_drag_n_drop(FromList),
    Output = case Contents of
                 [Contents2] -> superparser:process(Contents2);
                 []          -> ""
             end,
    #refX{obj = {cell, {FX, FY}}} = From,
    #refX{obj = {cell, {TX, TY}}} = To,
    case Output of
        {formula, Formula} ->
            {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {FX, FY}),
            NewToks = offset(Toks, (TX - FX), (TY - FY)),
            NewFormula = make_formula(NewToks),
            {ok, ok} = write_attr(To, {formula, NewFormula});
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
            {ok, ok} = write_attr(To, {formula, V2});
        []  ->
            {ok, ok} = clear_cells(To, all)
    end,
    % You want to copy the attributes AFTER setting the value
    % because setting a value sets the default alignment and format
    % and if the source cell has been reformatted after the data was entered
    % you want to carry that forward.
    AttrList = get_attr_keys(FilteredList),
    {ok, ok} = copy_attrs(From, To, AttrList),
    {ok, ok}.

%% @spec copy_attrs(From :: #refX{}, To :: #refX{}, AttrList) -> {ok, ok}
%% AttrList = [atom()]
%% @doc copies all the attributes of a cell to a new cell or cells.
%% All the listed attributes are copied from the From #ref{} to
%% the To ref{}. 
%% The From #ref{} must be a cell reference, but the To #ref{} can be either
%% a cell or a range
copy_attrs(_From, _To, []) -> {ok, ok};
copy_attrs(#refX{obj = {cell, _}} = From, #refX{obj = {cell, _}} = To, [H | T]) ->
    [{From, {Key, Value}}] = read_attrs(From, [H]),
    {ok, ok} = write_attr(To, {Key, Value}),
    copy_attrs(From, To, T);
copy_attrs(#refX{obj = {cell, _}} = From, #refX{obj = {range, _}} = To, Attrs) ->
    Ref = hn_util:refX_to_ref(To, "not needed"),
    List = hn_util:range_to_list(Ref),
    [copy_attrs(From, X, Attrs) || X <- List].

%% @spec read_incoming_hn(Parent::#refX{}) -> #incoming_hn{} | []
%% @doc reads an incoming hypernumber.
%% The <code>#refX{}</code> must refer to a cell
%% @todo extend the hypernumbers paradigm to include registering with a range,
%% column, row or query, etc, etc
read_incoming_hn(Parent) when is_record(Parent, refX) ->
    ParentIdx = hn_util:refX_to_index(Parent),
    Head = ms_util:make_ms(incoming_hn, [{remote, ParentIdx}]),
    Return = mnesia:select(incoming_hn, [{Head, [], ['$_']}]),
    case Return of
        []   -> [];
        [Hn] -> Hn
    end.
        
%% @spec read_outgoing_hns(Parent::#refX{}) -> [#outgoing_hn{}]
%% @doc reads the details of all outgoing hypernumbers from a particular cell.
%% The <code>#refX{}</code> must refer to a cell
%% @todo extend the hypernumbers paradigm to include registering with a range,
%% column, row or query, etc, etc
%% shouldn't this just take the 
read_outgoing_hns(Parent) when is_record(Parent, refX) ->
    ParentIdx = hn_util:refX_to_index(Parent),
    Head = ms_util:make_ms(outgoing_hn, [{parent, ParentIdx}]),
    mnesia:select(outgoing_hn, [{Head, [], ['$_']}]).

%% spec mark_cells_dirty(RefX::#refX{}) -> {ok, ok}
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
mark_cells_dirty(#refX{obj = {cell, _}} = RefX) ->
    % Make a list of cells hypernumbers + direct
    % cell links, and check for any wildcard * on the path

    % first up local
    LocalChildren = read_local_children(RefX),
    % now wildcards (must be fixed!)
    Index = hn_util:index_from_refX(RefX),
    NIndex = Index#index{path=lists:reverse(Index#index.path)},
    Queries = dyn_parents(NIndex,[],[]),

    LocalChildren2 = lists:append(LocalChildren, Queries),

    % Now write the local children to dirty_cell
    Fun = fun(X) ->
                  XIdx = hn_util:refX_to_index(X),
                  Match = ms_util:make_ms(dirty_cell, [{index, XIdx}]),
                  % only write the dirty cell if 
                  % it doesn't already exist
                  case mnesia:match_object(Match) of
                      [] -> ok = mnesia:write(#dirty_cell{index = XIdx});
                      _  -> ok
                  end
          end,
    _Return1 = lists:foreach(Fun, LocalChildren2),
    {ok, ok};
mark_cells_dirty(RefX) when is_record(RefX, refX) ->
    Cells = get_cells(RefX),
    [{ok, ok} = mark_cells_dirty(X) || X <- Cells],
    {ok, ok}.

%% @spec mark_notify_in_dirty(RefX::#refX{})  -> {ok, ok}
%% @doc marks a remote parent as dirty
mark_notify_in_dirty(RefX) when is_record(RefX, refX)  ->
    ParentIdx = hn_util:refX_to_index(RefX),
    % only write the dirty_notify_in if it doesn't already exist
    Match = ms_util:make_ms(dirty_notify_in, [{parent, ParentIdx}]),
    case mnesia:match_object(Match) of
        [] -> ok = mnesia:write(#dirty_notify_in{parent = ParentIdx});
        _  -> ok
    end,
    {ok, ok}.

%% @spec mark_notify_out_dirty(RefX::#refX{}, Value, DepTree)  -> {ok, ok}
%% DepTree = list()
%% @doc marks a cell as dirty so that it's remote children can be updated
mark_notify_out_dirty(RefX, Val, DepTree) ->
    % read the outgoing hypernumber
    Fun = fun() ->
                  read_outgoing_hns(RefX)
          end,
    List = mnesia:activity(transaction, Fun),
    % always write the dirty outgoing hypernumber
    PIdx = hn_util:index_from_refX(RefX),
    case List of
        [] -> ok;
        _  -> ok = mnesia:write(#dirty_notify_out{parent = PIdx, outgoing = List,
                                                  value = Val,
                                                  'dependency-tree' = DepTree})
    end,
    {ok, ok}.


%% @spec unregister_out_hn(Parent::#refX{}, Child::#refX{}, Biccie) -> {ok, ok}
%% @doc unregisters an outgoing hypernumber.
%% Both parent and child references must point to a cell
%% @todo this required a full table scan for an unregister
%% will get veeeerrry expensive if you have 100,000 children tracking a
%% number!
unregister_out_hn(P, C, B)
  when is_record(P, refX), is_record(C, refX) ->
    case verify_biccie(P, C, B) of
        true -> PIdx = hn_util:index_from_refX(P),
                #refX{site = CS} = C,
                M1 = ms_util:make_ms(index, [{site, CS}]),
                M2 = ms_util:make_ms(remote_cell_link, [{parent, PIdx},
                                                        {child, M1}]),
                case mnesia:select_object(M2) of
                    [] -> M3 = ms_util:make_ms(outgoing_hn,
                                               [{parent, PIdx},
                                                {child_site, CS}]),
                          [R] = mnesia:select(outgoing_hn, [{M3, [], ['S_']}]),
                          ok = mnesia:delete_object(R),
                          {ok, ok};
                    _   -> {ok, ok}
                end;
        false  ->
            {ok, ok}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% will write out any raw attribute
write_attr3(RefX, {Key, Value}) ->
    Ref = hn_util:refX_to_ref(RefX, Key),
    % notify any registered front ends
    Record = #hn_item{addr = Ref, val = Value},
    mnesia:write(Record),
    spawn(fun() -> tell_front_end(Record, change) end),
    {ok, ok}.

update_rem_parents(Child, OldParents, NewParents) when is_record(Child, refX) ->
    {Del, Write} = split_parents(OldParents, NewParents),
    % first delete all the records on the delete list
    % and unregister them (probably should be done in a gen server!)
    Fun1 = fun(X) ->
                   {ok, ok} = delete_remote_parents(X)
           end,
    [{ok, ok} = Fun1(X) || X <- Del],
    % now write all the records on the write list
    Fun2 = fun(X) ->
                   
                   {ok, ok} = write_remote_link(X, Child, incoming)
           end,
    [{ok, ok} = Fun2(X) || X <- Write],
    {ok, ok}.

%% This function is called on a local cell to inform all remote cells that it
%% used to reference as hypernumbers to no longer do so.
%% 
%% It looks to see if there are any other local cells also consuming that hypernumber
%%   - if not then delete the hypernumber in table 'incoming_hn' 
%%     and inform the remote source that we no longer need updates
unregister_inc_hn(Parent, Child)
  when is_record(Child, refX), is_record(Parent, refX) ->
    ParentIdx = hn_util:refX_to_index(Parent),
    Head = ms_util:make_ms(remote_cell_link, [{parent, ParentIdx},
                                              {type, incoming}]),
    case mnesia:select(remote_cell_link, [{Head, [], ['$_']}]) of
        [] -> Msg = "unregister",
              {ok, ok} = mark_notify_back_in_dirty(Parent, Child, Msg); 
        _  -> {ok, ok} % somebody else still wants it so don't unregister
    end.

get_refXs(List) -> get_refXs(List, []).

get_refXs([], Acc)      -> hslists:uniq(Acc);
get_refXs([{RefX, _} | T], Acc) -> get_refXs(T, [RefX | Acc]).

delete_links(RefX) ->
    {ok, ok} = delete_local_parents(RefX),
    {ok, ok} = delete_remote_parents(RefX),
    {ok, ok}.

get_refs_below2(RefX, MinX, MaxX, Y) ->
    #refX{site = S, path = P} = RefX,
    Obj = {cell, {'$1', '$2'}},
    Match = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, Obj}]),
    MatchRef = ms_util:make_ms(hn_item, [{addr, Match}, {val, '_'}]),
    Cond = case MinX of
               MaxX -> [{'and', {'>', '$2', Y}, {'==', '$1', MinX}}];
               _    -> [{'and', {'>', '$2', Y}, {'>=', '$1', MinX},
                         {'=<', '$1', MaxX}}]
           end,
    Body = ['$_'],
    get_match_refs({MatchRef, Cond, Body}).

get_refs_right2(RefX, X, MinY, MaxY) ->
    #refX{site = S, path = P} = RefX,
    Obj = {cell, {'$1', '$2'}},
    Match = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, Obj}]),
    MatchRef = ms_util:make_ms(hn_item, [{addr, Match}, {val, '_'}]),
    Cond = case MinY of
               MaxY -> [{'and', {'>', '$1', X}, {'==', '$2', MinY}}];
               _    -> [{'and', {'>', '$1', X}, {'>=', '$2', MinY},
                         {'=<', '$2', MaxY}}]
           end,
    Body = ['$_'],
    get_match_refs({MatchRef, Cond, Body}).

get_match_refs(MatchRef) ->
    Return = hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef])),
    % now extract the RefX's
    Fun = fun({X, {_Key, _Value}}, Acc) -> [X | Acc] end,
    Return1 = lists:foldl(Fun, [], Return),
    hslists:uniq(Return1).

make_range_match(RefX, AttrList) ->
    #refX{site = S, path = P, obj = Range, auth = A} = RefX,
    {range, {X1, Y1, X2, Y2}} = Range,
    {MinX, MaxX} = if
                       X1 >= X2 -> {X2, X1};
                       X1 <  X2 -> {X1, X2}
                   end,
    {MinY, MaxY} = if
                       Y1 >= Y2 -> {Y2, Y1};
                       Y1 <  Y2 -> {Y1, Y2}
                   end,
    Ref = {cell, {'$1', '$2'}},
    Match = case AttrList of
                [] -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , Ref}]);
                _  -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , Ref}, {name, '$3'}])
            end,
    Match2 = ms_util:make_ms(hn_item, [{addr, Match}, {val, '_'}]),
    % build a conditional for selecting cells
    % also need to build a cond for the attributes
    Ret = make_or(AttrList, '$3'),
    Cond = case Ret of
               [] -> [{'and', {'>=', '$1', MinX }, {'=<', '$1', MaxX},
                       {'>=', '$2', MinY}, {'=<', '$2', MaxY}}];
               [X] -> [{'and', {'and' , {'>=', '$1', MinX }, {'=<', '$1', MaxX},
                                {'>=', '$2', MinY}, {'=<', '$2', MaxY}}, X}]
           end,
    Body = ['$_'],
    {Match2, Cond, Body}.

make_column_match(RefX, AttrList) ->
    #refX{site = S, path = P, obj = Col, auth = A} = RefX,
    {column, {X1, X2}} = Col,
    {MinX, MaxX} = if
                       X1 >= X2 -> {X2, X1};
                       X1 <  X2 -> {X1, X2}
                   end,
    Ref = {cell, {'$1', '_'}},
    Match = case AttrList of
                [] -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , Ref}]);
                _  -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , Ref}, {name, '$3'}])
            end,
    Match2 = ms_util:make_ms(hn_item, [{addr, Match}, {val, '_'}]),
    % build a conditional for selecting cells
    % also need to build a cond for the attributes
    Ret = make_or(AttrList, '$3'),
    Cond = case Ret of
               [] -> [{'>=', '$1', MinX }, {'=<', '$1', MaxX}];
               [C] -> [{'and', {'>=', '$1', MinX }, {'=<', '$1', MaxX}, C}]
           end,
    Body = ['$_'],
    {Match2, Cond, Body}.

make_row_match(RefX, AttrList) ->
    #refX{site = S, path = P, obj = Row, auth = A} = RefX,
    {row, {Y1, Y2}} = Row,
    {MinY, MaxY} = if
                       Y1 >= Y2 -> {Y2, Y1};
                       Y1 <  Y2 -> {Y1, Y2}
                   end,
    Ref = {cell, {'_', '$1'}},
    Match = case AttrList of
                [] -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , Ref}]);
                _  -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , Ref}, {name, '$3'}])
            end,
    Match2 = ms_util:make_ms(hn_item, [{addr, Match}, {val, '_'}]),
    % build a conditional for selecting cells
    % also need to build a cond for the attributes
    Ret = make_or(AttrList, '$3'),
    Cond = case Ret of
               [] -> [{'>=', '$1', MinY }, {'=<', '$1', MaxY}];
               [C] -> [{'and', {'>=', '$1', MinY }, {'=<', '$1', MaxY}, C}]
           end,
    Body = ['$_'],
    {Match2, Cond, Body}.

make_page_match(RefX, AttrList) ->
    #refX{site = S, path = P, auth = A} = RefX,
    Match = case AttrList of
                [] -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , {cell, {'_', '_'}}}]);
                _  -> ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A},
                                            {ref , {cell, {'_', '_'}}}, {name, '$3'}])
            end,
    Match = ms_util:make_ms(hn_item, [{addr, Match}]).

get_attr_keys(List)  -> get_attr_keys(List, []).

get_attr_keys([], Acc)                       -> Acc;
get_attr_keys([{_RefX, {Key, _V}} | T], Acc) -> get_attr_keys(T, [Key | Acc]).

make_cell(false, X, XOffset, false, Y, YOffset) ->
    tconv:to_b26(X + XOffset)++tconv:to_s(Y + YOffset);
make_cell(true, X, _XOffset, false, Y, YOffset) ->
    [$$]++tconv:to_b26(X)++tconv:to_s(Y + YOffset);
make_cell(false, X, XOffset, true, Y, _YOffset) ->
    tconv:to_b26(X + XOffset)++[$$]++tconv:to_s(Y);
make_cell(true, X, _XOffset, true, Y, _YOffset)  -> 
    [$$]++tconv:to_b26(X)++[$$]++tconv:to_s(Y).

diff( FX, _FY,  TX, _TY, horizontal) -> TX - FX;
diff(_FX,  FY, _TX,  TY, vertical) -> TY - FY.

make_formula(Toks) -> mk_f(Toks, []).

mk_f([], Acc)                        -> "="++lists:flatten(lists:reverse(Acc));
mk_f([{ref, _, _, _, Ref} | T], Acc) -> mk_f(T, [Ref | Acc]);
mk_f([{atom, H} | T], Acc)           -> mk_f(T, [H | Acc]);
mk_f([{H} | T], Acc)                 -> mk_f(T, [atom_to_list(H) | Acc]).

parse_cell(Ref) ->
    {XDollar, Rest} = case Ref of
                          [$$ | T1] -> {true, T1};
                          _         -> {false, Ref}
                      end,
    Fun = fun(XX) ->
                  if XX < 97  -> false;
                     XX > 122 -> false;
                     true     -> true
                  end
          end,
    {XBits, YBits} = lists:partition(Fun,string:to_lower(Rest)),
    {YDollar, Y} = case YBits of
                       [$$ | T2] -> {true, T2};
                       _         -> {false, YBits}
                   end,
    {XDollar, tconv:to_i(XBits), YDollar, list_to_integer(Y)}.

offset(Toks, XOffset, YOffset) -> offset(Toks, XOffset, YOffset, []).

offset([], _XOffset, _YOffset, Acc) -> lists:reverse(Acc);
offset([{ref, Col, Row, Path, Cell} | T], XOffset, YOffset, Acc) ->
    {XDollar, X, YDollar, Y} = parse_cell(Cell),
    NewCell = make_cell(XDollar, X, XOffset, YDollar, Y, YOffset),
    NewRef = {ref, Col, Row, Path, NewCell},
    offset(T, XOffset, YOffset, [NewRef | Acc]);
offset([H | T], XOffset, YOffset, Acc) ->
    offset(T, XOffset, YOffset, [H | Acc]).

filter_for_drag_n_drop(List) -> fl(List, [], []).

fl([], A, B)                                -> {A, B};
fl([{_, {value, _}}  | T], A, B)            -> fl(T, A, B);
fl([{_, {rawvalue, _}}| T], A, B)           -> fl(T, A, B);
fl([{_, {parents, _}} | T], A, B)           -> fl(T, A, B);
fl([{_, {'dependency-tree', _}}| T], A, B)  -> fl(T, A, B);
fl([{_, {'__ast', _}} | T], A, B)           -> fl(T, A, B);
fl([{_, {formula, V}}| T], A, B)            -> fl(T, [V | A], B);
fl([H | T], A, B)                           -> fl(T, A, [H | B]).
    
dyn_parents(_Index = #index{path=[]},Results, _Acc) ->
    Results;
dyn_parents(Index = #index{path=[_H]},Results, Acc) ->
    Path = ["*"|Acc],
    lists:append(Results,get_par(Index,Path));
dyn_parents(Index = #index{path=[H|T]},Results,Acc) ->
    Path = lists:append(lists:reverse(T),["*"|Acc]),
    NResults = lists:append(Results,get_par(Index,Path)),
    dyn_parents(Index#index{path=T},NResults,[H|Acc]).

get_par(Index,Path) ->
    % This used to be an ets read of mnesia - so I suspect
    % it will be a lot slower - but hey!
    El = {local_cell_link,Index#index{path=Path},'_'},
    mnesia:match_object(El).

get_local_parents(List) -> get_l_p(List, []).

get_l_p([], Acc) -> Acc;
get_l_p([#local_cell_link{parent = P} | T], Acc) ->
    get_l_p(T, [?to_refX(P) | Acc]).

get_local_children(List) -> get_l_c(List, []).

get_l_c([], Acc) -> Acc;
get_l_c([#local_cell_link{child = C} | T], Acc) ->
    get_l_c(T, [?to_refX(C) | Acc]).

get_remote_parents(List) -> get_r_p(List, []).

get_r_p([], Acc) -> Acc;
get_r_p([#remote_cell_link{parent = P} | T], Acc) ->
    get_r_p(T, [?to_refX(P) | Acc]).

get_remote_children(List) -> get_r_c(List, []).

get_r_c([], Acc) -> Acc;
get_r_c([#remote_cell_link{child = C} | T], Acc) ->
    get_r_c(T, [?to_refX(C) | Acc]).

delete_remote_parents(RefX) when is_record(RefX, refX) ->
    Index = hn_util:index_from_refX(RefX),
    Match = ms_util:make_ms(remote_cell_link, [{child, Index}, {type, incoming}]), 
    Parents = mnesia:match_object(remote_cell_link, Match, read),
    % unregister the hypernumbers
    Fun = fun(X) ->
                  #remote_cell_link{parent = P, child = C, type = incoming} = X,
                  Rec = #remote_cell_link{parent = P, child = C,
                                          type = incoming},
                  {ok, ok} = delete_recs([Rec]),
                  Parent = hn_util:refX_from_index(P),
                  Child = hn_util:refX_from_index(C),
                  unregister_inc_hn(Parent, Child)
          end,
    [{ok, ok} = Fun(X) || X <- Parents],
    delete_recs(Parents).

delete_local_parents(RefX)  when is_record(RefX, refX)->
    Index = hn_util:index_from_refX(RefX),
    Match = ms_util:make_ms(local_cell_link, [{child, Index}]),
    Parents = mnesia:match_object(local_cell_link, Match, read),
    delete_recs(Parents).

write_local_parents(Child, List) ->
    Child2 = hn_util:index_from_refX(Child),
    Fun = fun(C, P) ->
                  Index = hn_util:index_from_refX(P),
                  mnesia:write(#local_cell_link{child = C, parent = Index})
          end,
    [ok = Fun(Child2, X) || X <- List],
    {ok, ok}.

delete_recs([]) ->
    {ok, ok};
delete_recs([H | T]) when is_record(H, hn_item) ->
    ok = mnesia:delete_object(H),
    spawn(fun() -> tell_front_end(H, delete) end),
    delete_recs(T);
delete_recs([H | T]) ->
    ok = mnesia:delete_object(H),
    delete_recs(T).

get_content_attrs(List) -> get_content_attrs(List, []).

get_content_attrs([], Acc)      -> Acc;
get_content_attrs([H | T], Acc) ->
    {_, {Key, _V}} = H,
    case Key of
        formula            -> get_content_attrs(T, [H | Acc]);
        rawvalue           -> get_content_attrs(T, [H | Acc]);
        value              -> get_content_attrs(T, [H | Acc]);
        'overwrite-color'  -> get_content_attrs(T, [H | Acc]);
        '__ast'            -> get_content_attrs(T, [H | Acc]);
        '__recompile'      -> get_content_attrs(T, [H | Acc]);
        '__shared'         -> get_content_attrs(T, [H | Acc]);
        '__area'           -> get_content_attrs(T, [H | Acc]);
        'dependency-tree'  -> get_content_attrs(T, [H | Acc]);
        parents            -> get_content_attrs(T, [H | Acc]);
        _                  -> get_content_attrs(T, Acc)
    end.

read_attrs2(MatchRef, AttrList) ->
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    Cond = make_or(AttrList, '$1'),
    Body = ['$_'],
    hn_util:from_hn_item(mnesia:select(hn_item, [{Match, Cond, Body}])).

shift_cell2(From, To) ->
    % Rewrite the shifted cell
    AttrList = read_cells_raw(From),
    Fun1 = fun({_RefX, {Key, Val}}) ->
                  write_attr3(To, {Key, Val})
          end,
    [Fun1(X) || X <- AttrList],
    % now delete the originals
    Fun2 = fun(X) ->
                   io:format("in shift_cell2 deleting ~p~n", [X]),
                   {RefX, {K, V}} = X,
                   Ref = hn_util:refX_to_ref(RefX, {K, V}),
                   mnesia:delete({hn_item, Ref})
           end,
    [Fun2(X) || X <- AttrList],
    {ok, ok}.

shift_local_links(From, To) ->
    % Now rewrite the cells that link to this cell
    Offset = get_offset(From, To),
    Index = hn_util:refX_to_index(From),
    Head = ms_util:make_ms(local_cell_link, [{parent, Index}]),
    LinkedCells = mnesia:select(local_cell_link, [{Head, [], ['$_']}]),
    shift_local_links2(LinkedCells, Offset).

shift_local_links2([], _Offset) -> {ok, ok};
shift_local_links2([#local_cell_link{child = C} | T], Offset) ->
    % now read the child
    ChildRefX = hn_util:refX_from_index(C),
    [{Child, {formula, Formula}}] = read_attrs(ChildRefX, [formula]),
    NewFormula = shift_formula(Formula, Offset),
    io:format("in shift_local_links2 rewriting the formula for ~p~n", [Child]),
    {ok, ok} = write_attr(Child, {formula, NewFormula}),
    shift_local_links2(T, Offset).

shift_formula([$=|Formula], {XO, YO}) ->
    % the xfl_lexer:lex takes a cell address to lex against
    % in this case {1, 1} is used because the results of this
    % are not actually going to be used here (ie {1, 1} is a dummy!)
    {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {1, 1}),
    NewToks = offset(Toks, XO, YO),
    make_formula(NewToks).

shift_dirty_cells(From, To) ->
    ToIdx = hn_util:index_from_refX(To),
    case  mnesia:read({dirty_cell, From}) of
        []          -> {ok, ok};
        [DirtyCell] -> NewDirty = DirtyCell#dirty_cell{index = ToIdx},
                       mnesia:delete(DirtyCell),
                       mnesia:write(NewDirty),
                       {ok, ok}
    end.

shift_dirty_notify_ins(From, To) ->
    FromIdx = hn_util:index_from_refX(From),
    ToIdx   = hn_util:index_from_refX(To),
    case mnesia:read({dirty_notify_in, FromIdx}) of
        []        -> {ok, ok};
        [DirtyHn] -> io:format("in hn_db_wu:shift_dirty_notify_ins "++
                               "this has got to be wrong too...~n"),
                     NewDirty = DirtyHn#dirty_notify_in{parent = ToIdx},
                     mnesia:delete(DirtyHn),
                     mnesia:write(NewDirty),
                     {ok, ok}
    end.

shift_outgoing_hns(_From, _To) ->
    io:format("in hn_db_wu:shift_outgoing_hn write me ya wank!~n"),
    {ok, ok}.

%% changes the hypernumber description and also lets the remote party know that
%% the hypernumber has changed...
shift_incoming_hns(From, To) ->
    FromIdx = hn_util:index_from_refX(From),
    case mnesia:read({incoming_hn, {'_', FromIdx}}) of
        [] -> {ok, ok};
        L  -> FromIdx = hn_util:refX_to_index(From),
              FromURL = hn_util:index_to_url(FromIdx),
              ToIdx = hn_util:refX_to_index(To),
              ToURL = hn_util:index_to_url(ToIdx),
              Msg = {"move", {"from", FromURL}, {"to", ToURL}},
              Fun = fun(X) ->
                            {ok, ok} = mark_notify_back_in_dirty(From, X, Msg)
                    end,
              [ok = Fun(X) || X <- L],
              {ok, ok}
    end.

get_offset(#refX{obj = {cell, {FX, FY}}}, #refX{obj = {cell, {TX, TY}}}) ->
    {TX - FX, TY - FY}.

write_attr2(RefX, {formula, Val}) ->
    case superparser:process(Val) of
        {formula, Fla}        -> write_formula1(RefX, Val, Fla);
        [NewVal, Align, Frmt] -> write_formula2(RefX, Val, NewVal, Align, Frmt)
    end.

write_formula1(RefX, Val, Fla) ->
    Ref = hn_util:refX_to_ref(RefX, Val),
    Rti = ref_to_rti(Ref, false),
    case muin:run_formula(Fla, Rti) of
        {error, Error} ->
            io:format("in hn_db_wu:write_formula1 Fla is ~p Rti is ~p Error is ~p~n",
                      [Fla, Rti, Error]),
            % @TODO, notify clients
            {ok, ok};       
        {ok, {Pcode, Res, Deptree, Parents, Recompile}} ->
            Parxml = map(fun muin_link_to_simplexml/1, Parents),
            Deptreexml = map(fun muin_link_to_simplexml/1, Deptree),
            {ok, ok} = write_pcode(RefX, Pcode),
            {ok, ok} = write_recompile(RefX, Recompile),
            % write the default text align for the result
            {ok, ok} = write_default_alignment(RefX, Res),
            write_cell(RefX, Res, "=" ++ Fla, Parxml, Deptreexml)
    end.

write_formula2(RefX, OrigVal, {Type, Value}, {'text-align', Align}, Format) ->
    {ok, ok} = write_attr(RefX, {'text-align', Align}),
    % write out the format (if any)
    case Format of
        {format, "null"} -> ok;
        {format, F}      -> write_attr(RefX, {format,F})
    end,
    % now write out the actual cell
    Formula = case Type of
                  quote    -> [39 | Value];
                  datetime -> OrigVal;
                  _        -> hn_util:text(Value)
              end,
    write_cell(RefX, Value, Formula, [], []).

write_pcode(_RefX, nil)  -> {ok, ok};
write_pcode(RefX, Pcode) -> Ref = hn_util:refX_to_ref(RefX, '__ast'),
                            Record = #hn_item{addr = Ref, val = Pcode},
                            ok = mnesia:write(Record),
                            {ok, ok}.

write_recompile(RefX, true)    ->
    Ref = hn_util:refX_to_ref(RefX, '__recompile'),
    Record = #hn_item{addr = Ref, val = true},
    ok = mnesia:write(Record),
    {ok, ok};
write_recompile(_RefX,_Recomp) -> {ok, ok}.

write_default_alignment(RefX, Res) when is_number(Res) ->
    write_attr(RefX, {'text-align' ,"right"});
write_default_alignment(RefX, Res) when is_list(Res) ->
    write_attr(RefX, {'text-align' ,"left"});
%% this clause matches for booleans, dates and errors
write_default_alignment(RefX, _Res)  ->
    write_attr(RefX, {'text-align' ,"center"}).

write_cell(RefX, Value, Formula, Parents, DepTree) ->
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
    %     & 'overwrite-color'
    % * overwrites the new values of these attributes:
    %   - parents
    %   - 'dependency-tree'
    % * reads the old set of local links:
    %   - writes any local links that aren't already there
    %   - deletes any local links that are no longer there
    % * reads the old set of remote links:
    %   - writes any remote links that aren't already there
    %   - deletes any remote links that are no longer there
    % * marks this cell dirty

    Ref = hn_util:refX_to_ref(RefX, 'formula'),
    {NewLocPs, NewRemotePs} = split_local_remote(Parents),

    % write the formula
    Record = #hn_item{addr = Ref, val = Formula},
    ok = mnesia:write(Record),
    spawn(fun() -> tell_front_end(Record, change) end),

    % now write the rawvalue, etc, etc
    {ok, ok} = write_rawvalue(RefX, Value),

    % overwrite the parents and 'dependency-tree'
    Set = fun(X, {Key, {xml,[]}}) -> delete_attrs(X, Key);
             (X, {Key, Val})      -> write_attr(X, {Key, Val})
          end,

    Set(RefX, {'parents',         {xml, Parents}}),
    Set(RefX, {'dependency-tree', {xml, DepTree}}),

    % now do the local parents
    {ok, ok} = delete_local_parents(RefX),
    {ok, ok} = write_local_parents(RefX, NewLocPs),

    % now do the remote parents
    % this is a bit messier - if a cell is being updated to change a
    % formula I don't want to first delete the old link (and trigger
    % an UNREGISTRATION of the hypernumbers) and then REWRITE the
    % same remote link and trigger a REGISTRATION so I first read the links
    OldRemotePs = read_remote_parents(RefX, incoming),
    {ok, ok} = update_rem_parents(RefX, OldRemotePs, NewRemotePs),

    % We need to know the calculcated value
    [{RefX, {rawvalue, RawValue}}] = read_attrs(RefX, [rawvalue]),
    {ok, ok} = mark_cells_dirty(RefX),

    % mark this cell as a possible dirty hypernumber
    % This takes a value and the dependency tree as it is asyncronous...
    {ok, ok} = mark_notify_out_dirty(RefX, RawValue, DepTree),
    {ok, ok}.

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
split_local_remote1([{_, [{_, "local"}], _} = P | T], {A, B})  ->
    P2 = hn_util:url_to_refX(P),
    split_local_remote1(T, {[P2 | A], B});
split_local_remote1([{_, [{_, "remote"}], _} = P | T], {A, B}) ->
    P2 = hn_util:url_to_refX(P),
    split_local_remote1(T, {A, [P2 | B]}).

write_rawvalue(RefX, Value) ->
    % first write the rawvalue
    Ref = hn_util:refX_to_ref(RefX, rawvalue),
    Record1 = #hn_item{addr = Ref, val = Value},
    ok = mnesia:write(Record1),
    spawn(fun() -> tell_front_end(Record1, change) end),
    % now get the format that is to be applied
    % run the format and then stick the value into
    % the database
    {ok, Format} = read_inherited(RefX, format, "General"),
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {Color, V}}=format:run_format(Value,Output),
    Ref1 = hn_util:refX_to_ref(RefX, value),
    Record2 = #hn_item{addr = Ref1, val = V},
    ok = mnesia:write(Record2),
    spawn(fun() -> tell_front_end(Record2, change) end),
    % now write the overwrite colour that comes from the format
    Ref2 = hn_util:refX_to_ref(RefX, 'overwrite-color'),
    Record3 = #hn_item{addr = Ref2, val = atom_to_list(Color)},
    ok = mnesia:write(Record3),
    spawn(fun() -> tell_front_end(Record3, change) end),
    {ok, ok}.

return_first(RefType, Addr) ->
    case traverse(RefType, Addr) of
        {last, []}                              -> nomatch;
        {last, [#hn_item{val = Val}]}           -> {ok, Val};
        {Ref, []}                               -> return_first(Ref, Addr);
        {Ref, NewAddr, []}                      -> return_first(Ref, NewAddr);
        {_Ref, _NewAddr, [#hn_item{val = Val}]} -> {ok, Val};
        {_Ref, [#hn_item{val = Val}]}           -> {ok, Val}
    end.

traverse(cell, Addr = #ref{ref = {cell, _}}) ->
    {range, match_ref(Addr)};

traverse(range, Addr = #ref{ref = {range, _}}) ->
    {page, match_ref(Addr)};
traverse(range, Addr = #ref{ref = {cell, _}}) ->
    V = case match_ref(Addr#ref{ref = {range, '_'}}) of
            [] -> [];
            List ->
                case filter_range(List, Addr#ref.ref) of
                    nomatch -> [];
                    Val     -> [Val]
                end
        end,
    {row_col, V};

traverse(row_col, Addr = #ref{ref = {cell, {_X, Y}}}) ->
    {column, match_ref(Addr#ref{ref = {row, {Y, Y}}})};

traverse(row, Addr = #ref{ref = {row, _}}) ->
    {page, match_ref(Addr)};
traverse(row, Addr = #ref{ref = {cell, {_X, Y}}}) ->
    {page, match_ref(Addr#ref{ref = {row, {Y, Y}}})};

traverse(column, Addr = #ref{ref = {column, _}}) ->
    {page, match_ref(Addr)};
traverse(column, Addr = #ref{ref = {cell, {X, _Y}}}) ->
    {page, match_ref(Addr#ref{ref= {column, {X, X}}})};

traverse(page, Addr = #ref{path=[]}) ->
    {last, match_ref(Addr#ref{ref = {page,"/"}})};
traverse(page, Addr) ->
    NewPath = hslists:init(Addr#ref.path),
    {page, Addr#ref{path = NewPath}, match_ref(Addr#ref{ref = {page, "/"}})}.

%% @doc Make a #muin_rti record out of a ref record and a flag that specifies 
%% whether to run formula in an array context.
ref_to_rti(#ref{site = S, path = P, ref= {cell, {C, R}}}, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, col = C, row = R, array_context = AC};
ref_to_rti(#refX{site = S, path = P, obj= {range, {C, R, _, _}}}, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, col = C, row = R, array_context = AC}.

%% @doc Convert Parents and DependencyTree tuples as returned by 
%% Muin into SimpleXML.
muin_link_to_simplexml({Type, {S, P, X1, Y1}}) ->
    Url = hn_util:index_to_url({index, S, P, X1, Y1}),
    {url, [{type, Type}], [Url]}.

%% @doc Get the value of a named attribute, if it doesnt exist for address
%% check parent (cell -> range -> row -> column -> page -> root -> default)
match_ref(Ptn) ->
    mnesia:match_object(hn_item, #hn_item{addr = Ptn, _='_'}, read).

filter_range([], _Cell)   ->
    nomatch;
filter_range([H | T], Cell) ->
    case hn_util:in_range((H#hn_item.addr), #ref.ref, Cell) of
        true -> H;
        _    -> filter_range(T, Cell)
    end.

make_or(AttrList, PlaceHolder) -> make_or(AttrList, PlaceHolder, []).

make_or([], _, Acc) -> case length(Acc) of
                           0 -> []; % no attributes get everything
                           1 ->  Acc; % 1 attribute - no 'or' statement
                           _ -> [list_to_tuple(lists:flatten(['or', Acc]))]
                       end;
make_or([H | T], PH, Acc) -> make_or(T, PH, [{'==', H, PH} | Acc]).

delete_style_attr(RefX, Key) ->
    Ref = hn_util:refX_to_ref(RefX, style),
    Match = ms_util:make_ms(hn_item, [{addr, Ref}]),
    CurrentStyle = mnesia:match_object(hn_item, Match, read),
    NewStyleIdx = get_style(RefX, CurrentStyle, Key, []),
    write_style_idx(RefX, NewStyleIdx).    

%% this function is called when a new attribute is set for a style
process_styles(RefX, {Name, Val}) ->
    % First up read the current style 
    Ref = hn_util:refX_to_ref(RefX, Name),
    Match = ms_util:make_ms(hn_item, [{addr, Ref}]),
    CurrentStyle = mnesia:match_object(hn_item, Match, read),
    NewStyleIdx = case CurrentStyle of 
                      []      -> get_style(RefX, Name, Val); 
                      [Style] -> get_style(RefX, Style, Name, Val) 
                  end,
    write_style_idx(RefX, NewStyleIdx).

write_style_idx(RefX, NewStyleIdx) when is_record(RefX, refX) ->
    Ref2 = hn_util:refX_to_ref(RefX, style),
    Record = #hn_item{addr = Ref2, val = NewStyleIdx},
    mnesia:write(Record),
    spawn(fun() -> tell_front_end(Record, change) end),
    {ok, ok}.

get_style(RefX, Name, Val) ->
    NoOfFields = ms_util2:no_of_fields(magic_style), 
    Index = ms_util2:get_index(magic_style, Name), 
    Style = make_tuple(magic_style, NoOfFields, Index, Val), 
    % Now write the style 
    write_style(RefX, Style). 

% edits a style
get_style(RefX, Style, Name, Val) -> 
    % use the index of the style to read the style
    Ref = hn_util:refX_to_ref(RefX, style), 
    #hn_item{addr = Ref, val = StIdx} = Style, 
    Match = #styles{ref = Ref#ref{ref = {page, "/"}}, index = StIdx, _ = '_'}, 
    Return = mnesia:match_object(styles, Match, read),
    [#styles{magic_style = CurrentStyle}] = Return, 
    Index = ms_util2:get_index(magic_style, Name), 
    Style2 = tuple_to_list(CurrentStyle), 
    {Start, [_H | End]} = lists:split(Index, Style2), 
    NewStyle = list_to_tuple(lists:append([Start, [Val], End])), 
    write_style(RefX, NewStyle). 

%% write_style will write a style if it doesn't exist and then 
%% return an index pointing to it 
%% If the style already exists it just returns the index 
write_style(RefX, Style) ->
    Ref = hn_util:refX_to_ref(RefX, style),
    % Ref is a cell ref, need a page ref
    Ref2 = Ref#ref{ref = {page, "/"}},
    Match = #styles{ref = Ref2, magic_style = Style, _ = '_'}, 
    case mnesia:match_object(styles, Match, read) of 
        []              -> write_style2(RefX, Style); 
        [ExistingStyle] -> #styles{index = NewIndex} = ExistingStyle, 
                           NewIndex 
    end. 

write_style2(RefX, Style) ->
    Ref = hn_util:refX_to_ref(RefX, style),
    % Ref is a cell reference - a page reference is needed
    Ref2 = Ref#ref{ref = {page, "/"}},
    NewIndex = mnesia:dirty_update_counter(style_counters, Ref2, 1), 
    % should spawn a notification that there is a new style
    io:format("in hn_db_wu:write_style2- needs to be fixed!~n"),
    mnesia:write(#styles{ref = Ref2, index = NewIndex, magic_style = Style}),
    NewIndex. 

%% @spec tell_front_end(Item, Type) -> ok
%% Type = [change | delete]
%% @doc calls the remoting server and tells is that something has changed
tell_front_end(Item, Type) when is_record(Item, hn_item) ->
    #hn_item{addr = #ref{site = S, path = P, ref = R, name = N}} = Item,
    case atom_to_list(N) of
        [$_, $_|_R]  -> ok; % names like '__name' are not notified to front-end
        _Other       ->
            Msg = case Type of
                      change -> MsgXml=hn_util:item_to_xml(Item),
                                ?FORMAT("change ~s",   [?to_xml_str(MsgXml)]);
                      delete -> ?FORMAT("delete ~p ~p",[N,hn_util:ref_to_str(R)])
                  end,
            ok = gen_server:call(remoting_reg, {change, S, P, Msg},?TIMEOUT)
    end.

make_tuple(Style, Counter, Index, Val) -> 
    make_tuple1(Style, Counter, Index, Val, []). 

make_tuple1(S, 0, _I, _V, Acc) -> list_to_tuple([S|Acc]); 
make_tuple1(S, I, I, V, Acc )  -> make_tuple1(S, I -1 , I, V, [V | Acc]); 
make_tuple1(S, C, I, V, Acc)   -> make_tuple1(S, C - 1, I, V, [[] | Acc]).

drop_private(List) -> drop_private(List, []).

drop_private([], Acc) -> Acc;
drop_private([H | T], Acc) ->
    {_, {Name, _}} = H,
    case atom_to_list(Name) of
        "__"++_ -> drop_private(T, Acc);
        _       -> drop_private(T, [H | Acc])
    end.

extract_values(List) -> extract_values1(List, []).

extract_values1([], Acc) -> Acc;
extract_values1([{_R, {_K, V}}| T], Acc)  -> extract_values1(T, [V | Acc]).

%% @hidden
dump() ->
    RefX = #refX{site = "http://127.0.0.1:9000", path = ["insert", "data"],
                 obj = {cell, {1, 1}}},
    Attrs = read_attrs(RefX, [rawvalue]),
    io:format("in dump Attrs are ~p~n", [Attrs]),
    {ok, ok}.

%% @hidden
dump(RefX, Msg) ->
    Ref = hn_util:refX_to_ref(RefX, '_'),
    Match = ms_util:make_ms(hn_item, [{addr, Ref}]),
    List = mnesia:match_object(Match),
    io:format("********************************* ~p~n", [Msg]),
    [io:format("~p~n", [X]) || X <- List],
    io:format("********************************* ends...~n").
