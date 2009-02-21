%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This is a util function for hn_db_api containing only functions
%%%            that MUST be called from within an mnesia transactions.
%%%            
%%%            The module {@link hn_db_api} is the wrapper for calls to this
%%%            function.
%%%            
%%%            <h3>Functional Categories</h3>
%%%            
%%%            These functions fall into 2 types:
%%%            <ul>
%%%            <li>structural queries that returns #refs{}</li>
%%%            <li>cell queries that operate on cells</li>
%%%            </ul>
%%%            
%%%            <h4>Structural Queries</h4>
%%%            
%%%            Structural queries have the following characteristics:
%%%            <ul>
%%%            <li>they are all 'read' queries - they do not impact the
%%%            structure of the database</li>
%%%            <li>they all have the word ref in their function name</li>
%%%            </ul>
%%%            
%%%            The all return lists of #refX{}'s
%%%            
%%%            <h4>Cell Queries</h4>
%%%            
%%%            Cell queries come in 4 distinct flavours:
%%%            <ul>
%%%            <li>create/write</li>
%%%            <li>read</li>
%%%            <li>update</li>
%%%            <li>delete</li>
%%%            </ul>
%%%            
%%%            The reads return lists of tuples containing #refX{} and 
%%%            {Key, Value} pairs.
%%%            
%%%            The others return {ok, ok}
%%% 
%%% @ TODO we use atoms for keys in {key, value} pairs of attributes
%%% which is then used in atom_to_list for checking if they are private.
%%% This is a memory leak! See also hn_yaws.erl
%%% 
%%% Also there is the port bodge function - need to handle port correctly
%%%
%%% And also when a new style is written for a page it should notify the
%%% viewing pages to update themselves or the style stuff won't work...
%%% 
%%% And the registration of a new hypernumber is not robust (what if the remote
%%% server is not available at registration time? there is no retry function, etc
%%% etc)
%%%
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
         read_incoming_hypernumber/1,
         read_outgoing_hypernumbers/1,
         clear_cells/1,
         clear_cells/2,
         delete_attrs/2,
         delete_outgoing_hn/3,
         clear_dirty_notify_back/3,
         clear_dirty_notify/1,
         shift_cell/2,
         copy_cell/3,
         copy_attrs/3]).

%% Structural Query Exports
-export([get_last_refs/1,
         get_refs_below/1,
         get_refs_right/1]).

%% These functions are exposed for the dirty_srv to use
-export([read_local_parents/1,
         read_local_children/1,
         read_remote_parents/1,
         read_remote_children/1]).

%% Debugging
-export([dump/0]).

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
%% @spec clear_dirty_notify(Parent::#refX{}) -> {ok, ok}
%% @doc clears a dirty notification.
%% The parent reference must point to a cell
%% @todo extend the reference to include rows, columns, ranges, etc, etc
clear_dirty_notify(Parent) when is_record(Parent, refX) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    ok = mnesia:delete_object(dirty_outgoing_hn, ParentIdx),
    {ok, ok}.

%% @spec clear_dirty_notify_back(Parent::#refX{}, Child::#refX{}, Change) -> {ok, ok}
%% @doc clears a dirty notify back.
%% Both the parent and the child references must point to a cell
%% @todo extend the references to include rows, columns, ranges, etc, etc
clear_dirty_notify_back(Parent, Child, Change)
  when is_record(Parent, refX), is_record(Child, refX) ->
    ParentIdx = hn_util:index_from_refX(Parent),
    ChildIdx = hn_util:index_from_refX(Child),
    Head = ms_util:make_ms(dirty_notify_back, [{parent, ParentIdx},
                                               {child, ChildIdx},
                                               {change, Change}]),
    [Record] = mnesia:select(dirty_notify_back, [{Head, [], ['$_']}]),
    ok = mnesia:delete_object(Record),
    {ok, ok}.

%% @spec delete_outgoing_hn(#refX{}, Url, Biccie) -> {ok, ok}
%% @doc deletes an outgoing hypernumber if the biccie quoted is correct.
%% 
%% The Url is the Url of the cell on the remote site which used to use
%% this hypernumber (but no longer does). The biccie is the biccie which was 
%% originally given when the hypernumber was set up.
%% 
%% @todo at the moment the <code>refX{}</code> can only point to a cell
%% should be a more general reference (including eventually a query reference)
delete_outgoing_hn(Parent, Child, Biccie) ->
    ParentIndex = hn_util:index_from_refX(Parent),
    ChildIndex = hn_util:index_from_refX(Child),
    Url = hn_util:index_to_url(ChildIndex),
    Head = ms_util:make_ms(outgoing_hn, [{index, {'_', ParentIndex}},
                                         {url, Url}, {biccie, Biccie}]),
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
    {XX1, XX2} = ?COND(X1 > X1, {X2, X1}, {X1, X2}),
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
    {YY1, YY2} = ?COND(Y1 > Y1, {Y2, Y1}, {Y1, Y2}),
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
get_last_refs(#refX{site = S, path = P} = RefX) ->
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

%% @spec read_remote_parents(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the remote parents of a reference.
%% 
%% The reference can only be to a cell and not a range, column, row or page
%% 
%% This fn is called read_remote_parents because it consists of all the
%% remote links where the current RefX is the child
read_remote_parents(#refX{obj = {cell, _}} = RefX) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(remote_cell_link, [{child, Index}, {type, incoming}]),
    Links = mnesia:match_object(remote_cell_link, Match, read),
    get_remote_parents(Links).

%% @spec read_remote_children(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the remote children of a reference.
%% 
%% The reference can only be to a cell and not a range, column, row or page
%% 
%% This fn is called read_remote_children because it consists of all the
%% remote links where the current RefX is the parent
read_remote_children(#refX{obj = {cell, _}} = RefX) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(remote_cell_link, [{parent, Index},
                                               {type, incoming}]),
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
        false -> Ref = hn_util:refX_to_ref(RefX, Key),
                 % notify any registered front ends
                 Record = #hn_item{addr = Ref, val = Val},
                 mnesia:write(Record),
                 spawn(fun() -> tell_front_end(Record, change) end)
    end, 
    {ok, ok};
write_attr(#refX{obj = {range, _}} = RefX, Attr) ->
    Ref = hn_util:refX_to_ref(RefX, "not needed"),
    List = hn_util:range_to_list(Ref),
    List2 = [hn_util:ref_to_refX(X, "not needed") || X <- List],
    lists:flatten([write_attr(X, Attr) || {X, _Discard} <- List2]);
%% for the rest just write'em out
write_attr(RefX, {Key, Val} = Attr) when is_record(RefX, refX) ->
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
%% This is a raw read because it returns *ALL* the attributes
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
read_cells_raw(#refX{obj = {column, {X1, X2}}} = RefX) ->
    MatchRef = make_column_match(RefX, []),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_cells_raw(#refX{obj = {row, {Y1, Y2}}} = RefX) ->
    MatchRef = make_row_match(RefX, []),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_cells_raw(#refX{obj = {page, _}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P},
                                     {ref, {cell, {'_', '_'}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    hn_util:from_hn_item(mnesia:match_object(hn_item, Match, read)).

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
read_attrs(#refX{obj = {range, _}} = RefX, AttrList)
  when is_list(AttrList) ->
    MatchRef = make_range_match(RefX, AttrList),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_attrs(#refX{obj = {column, {X1, X2}}} = RefX, AttrList)
  when is_list(AttrList) ->
    MatchRef = make_column_match(RefX, AttrList),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_attrs(#refX{obj = {row, {Y1, Y2}}} = RefX, AttrList)
  when is_list(AttrList) ->
    MatchRef = make_row_match(RefX, AttrList),
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_attrs(#refX{obj = {cell, _}} = RefX, AttrList)
  when is_list(AttrList) ->
    #refX{site = S, path = P, obj= R} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P},
                                     {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList);
read_attrs(#refX{obj = {page, _}} = RefX, AttrList)
  when is_list(AttrList) ->
    #refX{site = S, path = P} = RefX,
    R = {cell, {'_', '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P},
                                     {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList).

%% @spec shift_cell(RefX :: #refX{}, Offset :: {X :: integer(), 
%% Y :: integer()}) -> {ok, ok}
%% @doc shift_cells takes a cell and shifts it by the offset
%% 
%% Provided that offset is valid of course - ie the cell can't be 
%% made negative (mebbies it should be).
shift_cell(From, To) when is_record(From, refX), is_record(To, refX) ->
    {ok, ok} = shift_cell2(From, To),
    {ok, ok} = shift_links(From, To),
    {ok, ok} = shift_dirty_cells(From, To),
    {ok, ok} = shift_hypernumbers(From, To),
    {ok, ok} = shift_dirty_incoming_hns(From, To),
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
%%  
%% @TODO make proper clears - need to remove links and stuff
%% and also notify remote hypernumbers that the value has cleared
clear_cells(RefX, all) when is_record(RefX, refX)->
    % first get all attributes to be deleted
    List1 = read_attrs(RefX),
    % now get a list of the cells from the attribute list
    % and delete all their links
    List2 = get_refXs(List1),
    [{ok, ok} = delete_links(X) || X <- List2],
    % now delete all the attributes
    List3 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
             || {X, {Key, Val}} <- List1],
    delete_recs(List3);
clear_cells(RefX, style) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX, style),
    List2 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
             || {X, {Key, Val}} <- List1],
    delete_recs(List2);    
clear_cells(RefX, contents) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX),
    % now get a list of the cells from the attribute list
    % and delete all their links
    List2 = get_refXs(List1),
    [{ok, ok} = delete_links(X) || X <- List2],
    % now delete all the attributes
    List3 = get_content_attrs(List1),
    List4 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
             || {X, {Key, Val}} <- List3],
    delete_recs(List4).

%% @spec delete_attrs(RefX :: #refX{}, Key) -> {ok, ok}
%% Key = atom()
%% @doc deletes a named attribute from a
%% cell or cells (but doesn't delete the cells themselve)
delete_attrs(RefX, Key) ->
    Ref = hn_util:refX_to_ref(RefX, Key),
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> delete_style_attr(RefX, Key);
        false -> mnesia:delete({hn_item, Ref}),
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
                     false ->
                         tconv:to_s(V);
                     Other -> % Other can be a range of different values...
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
copy_attrs(#refX{obj = {cell, _}} = From, #refX{obj = {range, _}} = To, [H | T]) ->
    Ref = hn_util:refX_to_ref(To, "not needed"),
    List = hn_util:range_to_list(Ref),
    [copy_attrs(From, X, T) || X <- List].

%% @spec read_incoming_hypernumber(Parent::#refX{}) -> [#incoming_hn{}]
%% @doc reads an incoming hypernumber.
%% The <code>#refX{}</code> must refer to a cell
%% @todo extend the hypernumbers paradigm to include registering with a range,
%% column, row or query, etc, etc
read_incoming_hypernumber(Parent) when is_record(Parent, refX) ->
    ParentIdx = hn_util:refX_to_index(Parent),
    Head = ms_util:make_ms(incoming_hn, [{remote, ParentIdx}]),
    mnesia:select(incoming_hn, [{Head, [], ['$_']}]).

%% @spec read_outgoing_hypernumbers(Parent::#refX{}) -> [#outgoing_hn{}]
%% @doc reads the details of all outgoing hypernumbers from a particular cell.
%% The <code>#refX{}</code> must refer to a cell
%% @todo extend the hypernumbers paradigm to include registering with a range,
%% column, row or query, etc, etc
%% shouldn't this just take the 
read_outgoing_hypernumbers(Parent) when is_record(Parent, refX) ->
    ParentIdx = hn_util:refX_to_index(Parent),
    Head = ms_util:make_ms(outgoing_hn, [{index, {'_',ParentIdx}}]),
    mnesia:select(outgoing_hn, [{Head, [], ['$_']}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_rem_parents(RefX, OldParents, NewParents) when is_record(RefX, refX) ->
    {Del, Write} = split_parents(OldParents, NewParents),
    % first delete all the records on the delete list
    % and unregister them (probably should be done in a gen server!)
    Fun1 = fun(X) ->
                   delete_remote_parents(RefX)
          end,
    [{ok, ok} = Fun1(X) || X <- Del],
    % now write all the records on the write list
    Fun2 = fun(X) ->
                   P = hn_util:index_from_refX(X),
                   C = hn_util:index_from_refX(RefX),
                   Rec = #remote_cell_link{parent = P, child = C,
                                           type = incoming},
                   ok = mnesia:write(Rec)
           end,
    [ok = Fun2(X) || X <- Write],
    {ok, ok}.

%% This function is called on a local cell to inform all remote cells that it
%% used to reference as hypernumbers to no longer do so.
%% 
%% It looks to see if there are any other local cells also consuming that hypernumber
%%   - if not then delete the hypernumber in table 'incoming_hn' 
%%     and inform the remote source that we no longer need updates
unregister_hypernumber(Loc, Rem)
  when is_record(Loc, refX), is_record(Rem, refX) ->
    LocIdx = hn_util:refX_to_index(Loc),
    RemIdx = hn_util:refX_to_index(Rem),
    Head = ms_util:make_ms(remote_cell_link, [{parent, RemIdx},
                                               {type, incoming}]),
    case mnesia:select(remote_cell_link, [{Head, [], ['$_']}]) of
        [] -> Msg = "unregister",
              mark_notify_back_dirty(Loc, Rem, Msg); 
        _  -> {ok, ok} % somebody else still wants it so don't unregister
    end.

mark_dirty_outgoing_hn(RefX) ->
    Idx = hn_util:index_from_refX(RefX),
    ok = mnesia:write(#dirty_outgoing_hn{index=Idx}).

mark_notify_back_dirty(Local, Remote, Msg) ->
    LocIdx = hn_util:refX_to_index(Local),
    RemIdx = hn_util:refX_to_index(Remote),
    Rec = #dirty_notify_back{child = LocIdx, parent = RemIdx, change = Msg},
     ok = mnesia:write(Rec),
    {ok, ok}.

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
fl([{_, {'dependancy-tree', _}}| T], A, B)  -> fl(T, A, B);
fl([{_, {'__ast', _}} | T], A, B)           -> fl(T, A, B);
fl([{_, {formula, V}}| T], A, B)            -> fl(T, [V | A], B);
fl([H | T], A, B)                           -> fl(T, A, [H | B]).

mark_cell_dirty(Index) ->
    % Make a list of cells hypernumbers + direct
    % cell links, and check for any wildcard * on the path

    % first up local
    RefX = hn_util:refX_from_index(Index),
    LocalChildren = read_local_children(RefX),
    % now wildcards (must be fixed!)
    NIndex = Index#index{path=lists:reverse(Index#index.path)},
    Queries = dyn_parents(NIndex,[],[]),

    LocalChildren2 = lists:append(LocalChildren, Queries),
        
    % Now write the local children to dirty_cell
    Fun = fun(X) ->
                  % only write the dirty cell if 
                  % it doesn't already exist
                  XIdx = hn_util:refX_to_index(X),
                  Match = ms_util:make_ms(dirty_cell, [{index, XIdx}]),
                  case mnesia:match_object(Match) of
                      [] -> mnesia:write(#dirty_cell{index = XIdx}) ;
                      _  -> ok
                  end
          end,
    _Return1 = lists:foreach(Fun, LocalChildren2),

    % mark this cell as a possible dirty hypernumber
    ok = mark_dirty_outgoing_hn(hn_util:refX_from_index(Index)),
    ok.

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
                  Remote = hn_util:refX_from_index(P),
                  Local = hn_util:refX_from_index(C),
                  unregister_hypernumber(Local, Remote)
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

%write_remote_parents(Child, List) ->
%    io:format("in hn_db_wu:write_remote_parents~n-Child is ~p~n-List is ~p~n",
%              [Child, List]),
%    % this function does a few things
%    % * it writes the table 'remote_cell_link' to indicate that there is a
%    %   remote parent
%    %   NOTE: muin will have already have called the function that registers
%    %   and gets the hypernumber
%    Child2 = hn_util:index_from_refX(Child),
%    Fun = fun(C, P) ->
%                  I = hn_util:index_from_refX(P),
%                  T = incoming,
%                  Record = #remote_cell_link{child = C, parent = I, type = T},
%                  mnesia:write(Record)
%          end,
%    [ok = Fun(Child2, X) || X <- List],
%    {ok, ok}.

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
        formula           -> get_content_attrs(T, [H | Acc]);
        rawvalue          -> get_content_attrs(T, [H | Acc]);
        value             -> get_content_attrs(T, [H | Acc]);
        '__ast'           -> get_content_attrs(T, [H | Acc]);
        '__recompile'     -> get_content_attrs(T, [H | Acc]);
        '__shared'        -> get_content_attrs(T, [H | Acc]);
        '__area'          -> get_content_attrs(T, [H | Acc]);
        'dependency-tree' -> get_content_attrs(T, [H | Acc]);
        parents           -> get_content_attrs(T, [H | Acc]);
        _                 -> get_content_attrs(T, Acc)
    end.

read_attrs2(MatchRef, AttrList) ->
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    Cond = make_or(AttrList, '$1'),
    Body = ['$_'],
    hn_util:from_hn_item(mnesia:select(hn_item, [{Match, Cond, Body}])).

shift_cell2(From, To) ->
    % Rewrite the shifted cell
    case read_attrs(From, [formula]) of
        []                          -> {ok, ok} = clear_cells(From, all);
        [{From, {formula, Value}} ] -> #refX{obj = CT} = To,
                                       To2 = From#refX{obj = CT},
                                       {ok, ok} = write_attr(To2, {formula, Value}),
                                       % Now delete the old one...
                                       {ok, ok} = clear_cells(From, all)
    end.

shift_links(From, To) ->
    % Now rewrite the cells that link to this cell
    Offset = get_offset(From, To),
    Index = hn_util:refX_to_index(From),
    Head = ms_util:make_ms(local_cell_link, [{child, Index}]),
    LinkedCells = mnesia:select(local_cell_link, [{Head, [], ['$_']}]),
    shift_links2(LinkedCells, Offset).

shift_dirty_cells(From, To) ->
    ToIdx = hn_util:index_from_refX(To),
    case  mnesia:read({dirty_cell, From}) of
        []          -> {ok, ok};
        [DirtyCell] -> NewDirty = DirtyCell#dirty_cell{index = ToIdx},
                       mnesia:delete({dirty_cell, DirtyCell}),
                       mnesia:write(NewDirty),
                       {ok, ok}
    end.

shift_dirty_incoming_hns(From, To) ->
    ToIdx = hn_util:index_from_refX(To),
    case  mnesia:read({dirty_incoming_hn, From}) of
        []          -> {ok, ok};
        [DirtyHn] -> NewDirty = DirtyHn#dirty_cell{index = ToIdx},
                       mnesia:delete({dirty_incoming_hn, DirtyHn}),
                       mnesia:write(NewDirty),
                       {ok, ok}
    end.

shift_links2([], _Offset) -> {ok, ok};
shift_links2([#local_cell_link{child = Child, parent = Parent} | T], Offset) ->
    io:format("******************************~n"),
    io:format("*                            *~n"),
    io:format("* Write shift_links2         *~n"),
    io:format("*                            *~n"),
    io:format("******************************~n"),
    shift_links2(T, Offset).

%% @TODO write shift_hypernumbers
shift_hypernumbers(_Ref, _Offset) ->
    io:format("******************************~n"),
    io:format("*                            *~n"),
    io:format("* Write shift_hypernumbers   *~n"),
    io:format("*                            *~n"),
    io:format("******************************~n"),
    {ok, ok}.

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
            io:format("in hn_db_wu:write_formula1 Error is ~p~n", [Error]),
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
    %   - 'dependancy-tree'
    % * reads the old set of local links:
    %   - writes any local links that aren't already there
    %   - deletes any local links that are no longer there
    % * reads the old set of remote links:
    %   - writes any remote links that aren't already there
    %   - deletes any remote links that are no longer there
    % * marks this cell dirty

    Index = to_index(RefX),
    Ref = hn_util:refX_to_ref(RefX, 'formula'),
    {NewLocPs, NewRemotePs} = split_local_remote(Parents),

    % write the formula
    Record = #hn_item{addr = Ref, val = Formula},
    ok = mnesia:write(Record),
    spawn(fun() -> tell_front_end(Record, change) end),

    % now write the rawvalue, etc, etc
    {ok, ok} = write_rawvalue(RefX, Value),

    % overwrite the parents and 'dependancy-tree'
    Set = fun(X, {Key, {xml,[]}}) -> delete_attrs(X, Key);
             (X, {Key, Val})      -> write_attr(X, {Key, Val})
          end,

    Set(RefX, {'parents',         {xml, Parents}}),
    Set(RefX, {'dependancy-tree', {xml, DepTree}}),

    % now do the local parents
    {ok, ok} = delete_local_parents(RefX),
    {ok, ok} = write_local_parents(RefX, NewLocPs),

    % now do the remote parents
    % this is a bit messier - if a cell is being updated to change a
    % formula I don't want to first delete the old link (and trigger
    % an UNREGISTRATION of the hypernumbers) and then REWRITE the
    % same remote linke and trigger a REGISTRATION so I first read the links
    OldRemotePs = read_remote_parents(RefX),
    {ok, ok} = update_rem_parents(RefX, OldRemotePs, NewRemotePs),

    % @TODO remove hn_db thangs
    mark_cell_dirty(Index),

    {ok, ok}.

split_parents(Old, New) -> sp_ps(lists:sort(Old), lists:sort(New), {[],[]}).

%% if we have run out of OldParents stick the rest of the News on the Write Acc
sp_ps([], New, {D, W}) -> {D, lists:merge([New, W])};
%% if we have run out of NewParents stick the rest of the Olds on the Delete Acc
sp_ps(Old, [], {D, W}) -> {lists:merge([Old, D]), W};
%% if the same record appears in Old and New neither delete nor write
sp_ps([H | T1], [H | T2], Acc) -> sp_ps(T1, T2, Acc);
%% for every unique old record - delete it
sp_ps([H | T], New, {D, W}) -> sp_ps(T, New, {[H | D], W}).

split_local_remote(List) -> sp_lr(List, {[], []}).

sp_lr([], Acc) -> Acc;
sp_lr([{_, [{_, "local"}], _} = P | T], {A, B})  -> P2 = hn_util:url_to_refX(P),
                                                      sp_lr(T, {[P2 | A], B});
sp_lr([{_, [{_, "remote"}], _} = P | T], {A, B}) -> P2 = hn_util:url_to_refX(P),
                                                      sp_lr(T, {A, [P2 | B]}).

write_rawvalue(RefX, Value) ->
    Ref = hn_util:refX_to_ref(RefX, rawvalue),
    Record1 = #hn_item{addr = Ref, val = Value},
    mnesia:write(Record1),
    spawn(fun() -> tell_front_end(Record1, change) end),
    {ok, Format} = read_inherited(RefX, format, "General"),
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {Color, V}}=format:run_format(Value,Output),
    Ref1 = hn_util:refX_to_ref(RefX, value),
    Record2 = #hn_item{addr = Ref1, val = V},
    mnesia:write(Record2),
    spawn(fun() -> tell_front_end(Record2, change) end),
    Ref2 = hn_util:refX_to_ref(RefX, 'overwrite-color'),
    Record3 = #hn_item{addr = Ref2, val = atom_to_list(Color)},
    mnesia:write(Record3),
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

%% @doc Convert Parents and DependencyTree tuples as returned by Muin into SimpleXML.
muin_link_to_simplexml({Type, {S, P, X1, Y1}}) ->
    Url = hn_util:index_to_url({index, S, P, X1, Y1}),
    {url, [{type, Type}], [Url]}.

to_index(#refX{site = Site, path = Path, obj = {cell, {X, Y}}}) ->
    #index{site = Site, path = Path, column = X, row = Y}.

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

make_or([], _, Acc)       -> case length(Acc) of
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
        [$_, $_|_R]  -> ok; % names of form '__name' are not notified to front-end
        _Other       ->
            Msg = case Type of
                      change -> MsgXml=hn_util:item_to_xml(Item),
                                ?FORMAT("change ~s",   [?to_xml_str(MsgXml)]);
                      delete -> ?FORMAT("delete ~p ~p",[N,hn_util:ref_to_str(R)])
                  end,
            ok = gen_server:call(remoting_reg, {change, S, P, Msg},?TIMEOUT)
    end.

make_tuple(Style, Counter, Index, Val) -> 
    make_tuple(Style, Counter, Index, Val, []). 

make_tuple(S, 0, _I, _V, Acc)     -> list_to_tuple([S|Acc]); 
make_tuple(S, I, I, V, Acc )      -> make_tuple(S, I -1 , I, V, [V | Acc]); 
make_tuple(S, Counter, I, V, Acc) -> make_tuple(S, Counter - 1, I, V, [[] | Acc]).

drop_private(List) -> drop_private(List, []).

drop_private([], Acc) -> Acc;
drop_private([H | T], Acc) ->
    {_, {Name, _}} = H,
    case atom_to_list(Name) of
        "__"++_ -> drop_private(T, Acc);
        _       -> drop_private(T, [H | Acc])
    end.

extract_values(List) -> extract_values2(List, []).

extract_values2([], Acc) -> Acc;
extract_values2([{_R, {_K, V}}| T], Acc)  -> extract_values2(T, [V | Acc]).

%% @hidden
dump() ->
    RefX = #refX{site = "http://127.0.0.1:9000", path = ["insert", "data"],
                 obj = {cell, {1, 1}}},
    Attrs = read_attrs(RefX, [rawvalue]),
    io:format("in dump Attrs are ~p~n", [Attrs]),
    {ok, ok}.


