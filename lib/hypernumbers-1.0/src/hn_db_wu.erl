%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This is a util function for hn_db containing only functions
%%%            that MUST be called from within an mnesia transaction
%%%            
%%%            Functions fall into two categories:
%%%            <ul>
%%%            <li>those that are commutative operations</li>
%%%            <li>those that are non-commutative</li>
%%%            </ul>
%%%            
%%%            Commmutative operations have results that are 
%%%            of the order in which they are applied.
%%%            
%%%            For instance <code>delete cell a1</code> followed
%%%            by <code>delete cell a2</code> is the same as
%%%            <code>delete cell a2</code> followed
%%%            by <code>delete cell a1</code>. <code>delete</code>
%%%            is communatative.
%%%            
%%%            Contrast this with <code>shift cell a2 to a1</code>
%%%            followed by <code>shift cell a3 to a2</code>. This
%%%            does not commute.
%%%            
%%%            Commutative functions can be executed 2 ways in this
%%%            module:
%%%            <ul>
%%%            <li>function_cell(Args, ...) - operates on a cell</li>
%%%            <li>function_list(Args, ...) - operators on a list of cells</li>
%%%            </ul>
%%%            
%%%            By contrast non-commutative functions are only exposed as:
%%%            <ul>
%%%            <li>function_cell(Args, ...)</li>
%%%            </ul>
%%% 
%%% @ TODO we use atoms for keys in {key, value} pairs of attributes
%%% which is then used in atom_to_list for checking if they are private.
%%% This is a memory leak! See also hn_yaws.erl

%%%
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_wu).

-export([write_attr/2,       % tested
         read_cells/1,       % tested
         read_cells_raw/1,
         read_attrs/1,       % tested
         read_attrs/2,       % tested
         read_inherited/3,
         delete_cell/1,
         delete_cell/2,
         delete_attrs/2,
         shift_cell/2,
         copy_attributes/2]).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Exported functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec write_attr(RefX :: #refX{}, Attr) -> {ok, ok}
%% @doc This function writes attributes to a cell or a range.
%% The second paramater can either be a single attribute as 
%% a tuple {key, value} or a list of tuples like that
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% </ul>
%% 
%% This function deals with style-able attributes auto-magically
%% 
%% If an attribute is saved which is one of the styled attributes defined in
%% the ref magic_styles in file <a href="../include/spriki.hrl">spriki.hrl</a>
%% it will be magically managed as a style
%% 
%% @todo the range clause here expands the range and then searches
%% for all cells - should actually generate a match object bounded by
%% the range and then search with that for efficiency/speed
%% @end
%% 
%% This clause deals with a formula
write_attr(#refX{obj = {cell, _}} = RefX, {formula, _} = Attr) ->
    % first check that the formula is not part of a shared array
    % io:format("in write_attr (2) RefX is ~p Attr is ~p~n", [RefX, Attr]),
    {_Key, Val} = Attr,
    case read_attrs(RefX, ['__shared']) of
        [_X] -> throw({error, cant_change_part_of_array});
        []   -> write_attr2(RefX, Val)
    end;
write_attr(#refX{obj = {cell, _}} = RefX, {Key, Val} = Attr) ->
    % io:format("in write_attr (3) RefX is ~p Attr is ~p~n", [RefX, Attr]),
    % NOTE the attribute 'overwrite-color' isn't in a magic style and shouldn't be
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> process_styles(RefX, Attr);
        false -> Ref = refX_to_ref(RefX, key),
                 mnesia:write(#hn_item{addr = Ref, val = Val}) 
    end, 
    {ok, ok};
write_attr(#refX{obj = {range, _}} = RefX, Attr) ->
    % io:format("in write_attr (4) RefX is ~p Attr is ~p~n", [RefX, Attr]),
    Ref = refX_to_ref(RefX, "not needed"),
    List = hn_util:range_to_list(Ref),
    List2 = [ref_to_refX(X, "not needed") || X <- List],
    % io:format("in read_attrs List is ~p~nList2 is ~p~n", [List, List2]),
    lists:flatten([write_attr(X, Attr) || {X, _Discard} <- List2]).

%% @spec read_cells(Refs) -> [Cells :: #hn_itemX{}]
%% @doc reads all the attributes of a cell or cells given a ref to them.
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

%% @spec read_cells_raw(Refs) -> [Cells :: #hn_itemX{}]
%% @doc reads all the attributes of a cell or cells given a ref to them.
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
    % io:format("in hn_db_wu:read_cells (1) MatchRef is ~p~n", [MatchRef]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    % io:format("in hn_db_wu:read_cells (1) Match is ~p~n", [Match]),
    mnesia:match_object(hn_item, Match, read);
read_cells_raw(#refX{obj = {range, _}} = RefX) ->
    Range = hn_util:list_from_range(RefX),
    % io:format("in hn_db_wu:read_cells (2) Range is ~p~n", [Range]),
    % now just get all the cells
    lists:flatten([read_cells_raw(X) || X <- Range]);
read_cells_raw(#refX{obj = {column, X}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {X, '_'}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    % io:format("in hn_db_wu:read_cells (3) Match is ~p~n", [Match]),
    mnesia:match_object(hn_item, Match, read);
read_cells_raw(#refX{obj = {row, Y}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {'_', Y}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    % io:format("in hn_db_wu:read_cells (4) Match is ~p~n", [Match]),
    mnesia:match_object(hn_item, Match, read);
read_cells_raw(#refX{obj = {page, _}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {'_', '_'}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    % io:format("in hn_db_wu:read_cells (5) Match is ~p~n", [Match]),
    mnesia:match_object(hn_item, Match, read).                 


%% @spec read_inherited(RefX, Key, Default) -> {ok, Value}
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at RefX, if not found return default
%%       
%% @todo what are the ref types it supports? improve the documentation, etc, etc
read_inherited(RefX, Key, Default) when is_record(RefX, refX)  ->
    % io:format("in read_inherited RefX is ~p Key is ~p Default is ~p~n",
    %          [RefX, Key, Default]),
    Ref = refX_to_ref(RefX, Key),
    case return_first(cell, Ref) of
        {ok, Format} -> {ok, Format};
        nomatch      -> {ok, Default}
    end.

%% @spec read_attrs(Ref :: #refX{}) -> [Attrs :: #hn_itemX{}]
%% @doc Reads all the attributes for a reference.
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_attrs(RefX) ->
    read_attrs(RefX, []).

%% @spec read_attrs(Ref :: #refX{}, AttrsList) -> [#refX{}]
%% AttrsList = [#hn_itemX{}]
%% @doc Reads the attributes specified in the AttrsList for a reference.
%% If the attribute list is blank returns all the attributes
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
%% @todo the range clause here expands the range and then searches
%% for all cells - should actually generate a match object bounded by
%% the range and then search with that for efficiency/speed
read_attrs(#refX{obj = {range, _}} = RefX, AttrList) when is_list(AttrList) ->
    % io:format("in read_attrs (1) RefX is ~p AttrList is ~p~n", [RefX, AttrList]),
    Ref = refX_to_ref(RefX, "not needed"),
    List = hn_util:range_to_list(Ref),
    List2 = [ref_to_refX(X, "not needed") || X <- List],
    % io:format("in read_attrs List is ~p~nList2 is ~p~n", [List, List2]),
    lists:flatten([read_attrs(X, AttrList) || {X, _Discard} <- List2]);
read_attrs(#refX{obj = {column, X}} = RefX, AttrList) when is_list(AttrList) ->
    % io:format("in read_attrs (2) RefX is ~p AttrList is ~p~n", [RefX, AttrList]),
    #refX{site = S, path = P} = RefX,
    R = {cell, {X, '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef,AttrList);
read_attrs(#refX{obj = {row, Y}} = RefX, AttrList) when is_list(AttrList) ->
    % io:format("in read_attrs (3) RefX is ~p AttrList is ~p~n", [RefX, AttrList]),
    #refX{site = S, path = P} = RefX,
    R = {cell, {'_', Y}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList);
read_attrs(#refX{obj = {cell, _}} = RefX, AttrList) when is_list(AttrList) ->
    io:format("in read_attrs (4) RefX is ~p AttrList is ~p~n", [RefX, AttrList]),
    #refX{site = S, path = P, obj= R} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    io:format("in read_attrs (4) MatchRef is ~p~n", [MatchRef]),
    read_attrs2(MatchRef, AttrList);
read_attrs(#refX{obj = {page, _}} = RefX, AttrList) when is_list(AttrList) ->
    io:format("in read_attrs (5) RefX is ~p AttrList is ~p~n", [RefX, AttrList]),
    #refX{site = S, path = P} = RefX,
    R = {cell, {'_', '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList).

%% @spec shift_cell(RefX :: #hn_itemX{}, Offset :: {X :: integer(), 
%% Y :: integer()}) -> {ok, ok}
%% @doc shift_cell takes a cell and shifts it by the offset - provided
%% that offset is valid off course (ie the cell can't be made negative - mebbies
%% it should be).
%% 
%% @todo shift this from an hn_item to a ref calling parameter
shift_cell(RefX, {Key, _Value}) when Key == parents orelse Key == rawvalue orelse Key == value orelse Key == 'dependancy-tree' orelse Key == '__ast' ->
    io:format("in shift_cell (1) RefX is ~p~n", [RefX]),
    % do nothing
    {ok, ok};
shift_cell(RefX, Offset) ->
    io:format("in shift_cell (2) RefX  is ~p~n", [RefX]),
    {ok, ok} = shift_cell(RefX, Offset),
    {ok, ok} = shift_links(RefX, Offset),
    {ok, ok} = shift_hypernumbers(RefX, Offset),
    {ok, ok}.

%% @spec delete_cell(RefX :: #refX{}) -> {ok, ok}
%% @doc deletes the contents (formula/value) and the formats and attributes
%% of a cell (but doesn't delete the cell itself)
%%  
%% The same as delete_cell(RefX, contents).
delete_cell(RefX) ->
    % io:format("in delete_cell RefX is ~p~n", [RefX]),
    delete_cell(RefX, contents).

%% @spec delete_cell(Ref :: #refX{}, Type) -> {ok, ok} 
%% Type = [contents | all]
%% @doc deletes a cell - behaviour depends on the value of type
%% <ul>
%% <li><code>contents</code> - deletes the formula/value but not the attributes
%% or formats (or the cell itself)</li>
%% <li><code>all</code> - deletes the contents, formats, styles 
%% and attributes (but not the cell itself)</li>
%% </ul>
%%  
%% @TODO put in stuff about formats, styles or attributes only - not sure where...
delete_cell(RefX, all) ->
    List = read_attrs(RefX),
    delete_recs(List);
delete_cell(RefX, contents) ->
    List = read_attrs(RefX),
    List2 = get_content_attrs(List),
    delete_recs(List2).

%% @spec delete_attrs(RefX :: #refX{}, Key) -> {ok, ok}
%% @doc deletes the attribute with key Key from a
%% of a cell (but doesn't delete the cell itself)
delete_attrs(RefX, Key) ->
    Ref = refX_to_ref(RefX, Key),
    mnesia:delete({hn_item, Ref}).

%% @spec copy_attributes(List, To :: #refX{}) -> {ok, ok}
%% List = [#refX{}]
%% @doc fix me up...
%% @todo split out into copy_attributes and copy_attributes_list
%% prolly rename me
%% take out the call to hn_main, blah, blah...
%% fix the calling parameters
copy_attributes([],_To)                -> {ok, ok};
copy_attributes([{_, Ref, V} | T], To) -> % #ref{name = Name} = Ref,
                                          % wtf?
                                          % To#ref{name = Name},
                                          hn_main:set_attribute(Ref, V),
                                          copy_attributes(T, To).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_local_children(RefX) ->
    % Ooh! dodgy double transform, eek!
    Index = hn_util:ref_to_index(refX_to_ref(RefX, child)),
    MatchRef = ms_util:make_ms(local_cell_link, [{child, Index}]), 
    List = mnesia:match_object(local_cell_link, MatchRef, read),
    delete_recs(List).
    
delete_recs([])      -> {ok, ok};
delete_recs([H | T]) -> mnesia:delete_object(H),
                         delete_recs(T).

get_content_attrs(List) -> get_content_attrs(List, []).

get_content_attrs([], Acc) -> Acc;
get_content_attrs([H | T], Acc) ->
    #hn_item{addr = #ref{name = Name}} = H,
    case Name of
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
    % io:format("In read_attrs2 Match is ~p~n", [Match]),
    Cond = make_or(AttrList, '$1'),
    Body = ['$_'],
    % io:format("in read_attrs2 Match is ~p Cond is ~p Body is ~p~n",
    %          [Match, Cond, Body]),
    Return = mnesia:select(hn_item, [{Match, Cond, Body}]),
    % io:format("in read_attrs2 Return is ~p~n", [Return]),
    Return.

shift_cell2(Addr, Offset) ->
    % Rewrite the shifted cell
    {XO, YO} = Offset,
    #hn_item{addr = Ref, val = Val} = Addr,
    #ref{ref = {cell, {X, Y}}} = Ref,
    Ref2 = Ref#ref{ref = {cell, {X + XO, Y + YO}}},
    % now write it
    ok = hn_main:set_attribute(Ref2, Val),
    {ok, ok}.

shift_links(Addr, Offset) ->
    % Now rewrite the cells that link to this cell
    % {XO, YO} = Offset,
    #hn_item{addr = Ref} = Addr,
    % #ref{ref = {cell, {X, Y}}} = Ref,
    Index = hn_util:ref_to_index(Ref),
    Head = ms_util:make_ms(local_cell_link, [{parent, Index}]),
    LinkedCells = mnesia:select(local_cell_link, [{Head, [], ['$_']}]),
    io:format("LinkedCells are ~p~n", [LinkedCells]),
    shift_links2(LinkedCells, Offset).

shift_links2([], _Offset) -> {ok, ok};
shift_links2([#local_cell_link{child = Child, parent = Parent} | T], Offset) ->
    io:format("in shift_links2 Child is ~p~nParent is ~p~n", [Child, Parent]),
    shift_links2(T, Offset).

%% @TODO write shift_hypernumbers
shift_hypernumbers(_Ref, _Offset) ->
    io:format("hn_db_wu:shift_hypernumbers needs to be written!~n"),
    {ok, ok}.

write_attr2(RefX, Val) ->
    % io:format("in write_attr2 RefX is ~p Val is ~p~n", [RefX, Val]),
    case superparser:process(Val) of
        {formula, Fla}        -> write_formula1(RefX, Val, Fla);
        [NewVal, Align, Frmt] -> write_formula2(RefX, Val, NewVal, Align, Frmt)
    end.

write_formula1(RefX, Val, Fla) ->
    % io:format("in write_formula1 RefX is ~p Val is ~p Fla is ~p~n", [RefX, Val, Fla]),
    Ref = refX_to_ref(RefX, Val),
    Rti = ref_to_rti(Ref, false),
    case muin:run_formula(Fla, Rti) of
        {error, _Error} -> 
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
    % io:format("in write_formula2 RefX is ~p OrigVal is ~p Value is ~p "++
    %          "Align is ~p Format is ~p~n",[RefX, OrigVal, Value, Align, Format]),
    % @TODO fix me - write out the alignment
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
write_pcode(RefX, Pcode) -> Ref = refX_to_ref(RefX, '__ast'),
                            Item = #hn_item{addr = Ref, val = Pcode},
                            ok = mnesia:write(Item),
                            {ok, ok}.

refX_to_ref(RefX, Name) ->
    % io:format("in refX_to_ref RefX is ~p Name is ~p~n", [RefX, Name]),
    #refX{site = S, path = P, obj = R, auth = A} = RefX,
    #ref{site = S, path = P, ref = R, name = Name, auth = A}.

ref_to_refX(Ref, Val) ->
    % io:format("in ref_to_ref Ref is ~p Val is ~p~n", [Ref, Val]),
    #ref{site = S, path = P, ref = R, name = Key, auth = A} = Ref,
    RefX = #refX{site = S, path = P, obj = R, auth = A},
    {RefX, {Key, Val}}.

write_recompile(RefX, true)    ->
    Ref = refX_to_ref(RefX, '__recompile'),
    Item = #hn_item{addr = Ref, val = true},
    ok = mnesia:write(Item),
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
    io:format("in write_cell RefX is ~p Value is ~p Formula is ~p Parents is ~p "++
              "DepTree is ~p~n", [RefX, Value, Formula, Parents, DepTree]),
    Index = to_index(RefX),
    Ref = refX_to_ref(RefX, 'formula'),
    Item = #hn_item{addr = Ref, val = Formula},
    mnesia:write(Item),
    io:format("in write_cell RefX is ~p Value is ~p~n", [RefX, Value]),
    {ok, ok} = write_rawvalue(RefX, Value),

    % Delete attribute if empty, else store
    Set = fun(X, {Key, {xml,[]}}) -> delete_attrs(X, Key);
             (X, {Key, Val})      -> write_attr(X, {Key, Val})
          end,

    io:format("Got to 1~n"),
    Set(RefX, {'parents',         {xml, Parents}}),
    Set(RefX, {'dependancy-tree', {xml, DepTree}}),
    % Delete the references

    io:format("Got to 2~n"),

    delete_local_children(RefX),
    % @TODO fix up
    % delete_remote_chldren(RefX),
    % 
    % probably to be cleaned up, go through the remote parents
    % to this cell, if they dont exist within the list of new
    % parents, delete it (and unregister)
    lists:foreach(
      fun(X) when is_record(X, remote_cell_link) ->
              Url  = hn_util:index_to_url(X#remote_cell_link.parent),
              case lists:member({url, [{type, "remote"}], [Url]}, Parents) of
                  false -> hn_db:del_remote_link(X); % @TODO remove hn_db thangs

                  true  -> ok
              end;
         (_) -> ok
      end,
      hn_db:read_remote_links(Index, child, incoming)), % @TODO remove hn_db thangs

    % Writes all the parent links 
    lists:map( 
      fun({url, [{type, Type}], [Url]}) ->
              {ok,#ref{site = Site, path = Path, ref = {cell, {X, Y}}}}
                  = hn_util:parse_url(Url),
              Parent = {index, Site, string:to_lower(Path), X, Y},
              case Type of % @TODO remove hn_db thangs
                  "local"  -> hn_db:write_local_link(Parent, Index);
                  "remote" -> hn_db:write_remote_link(Parent, Index, incoming)
              end,
              ok
      end,
      Parents),

    % @TODO remove hn_db thangs
    hn_db:mark_dirty(Index,cell),    
    {ok, ok}.

write_rawvalue(RefX, Value) ->
    % io:format("in write_rawvalue RefX is ~p Value is ~p~n", [RefX, Value]),
    Ref = refX_to_ref(RefX, rawvalue),
    mnesia:write(#hn_item{addr = Ref, val = Value}),
    {ok, Format} = read_inherited(RefX, format, "General"),
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {Color, V}}=format:run_format(Value,Output),
    Ref1 = refX_to_ref(RefX, value),
    mnesia:write(#hn_item{addr = Ref1, val = V}),
    Ref2 = refX_to_ref(RefX, 'overwrite-color'),
    mnesia:write(#hn_item{addr = Ref2, val = atom_to_list(Color)}),
    {ok, ok}.

return_first(RefType, Addr) ->
    % io:format("in return_first RefType is ~p Addr is ~p~n", [RefType, Addr]),
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
    {column, match_ref(Addr#ref{ref = {row, Y}})};

traverse(row, Addr = #ref{ref = {row, _}}) ->
    {page, match_ref(Addr)};
traverse(row, Addr = #ref{ref = {cell, {_X, Y}}}) ->
    {page, match_ref(Addr#ref{ref = {row, Y}})};

traverse(column, Addr = #ref{ref = {column, _}}) ->
    {page, match_ref(Addr)};
traverse(column, Addr = #ref{ref = {cell, {X, _Y}}}) ->
    {page, match_ref(Addr#ref{ref= {column, X}})};

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
                                 0 -> Acc; % no attributes get everything
                                 1 -> Acc; % 1 attribute - no 'or' statement
                                 _ -> [list_to_tuple(['or', Acc])]
                             end;
make_or([H | T], PH, Acc) -> make_or(T, PH, [{'==', H, PH} | Acc]).

process_styles(RefX, {Name, Val}) ->
    % io:format("in process_styles RefX is ~p Name is ~p Val is ~p~n",
    %          [RefX, Name, Val]),
    % First up read the current style 
    Ref = refX_to_ref(RefX, Name),
    Match = #hn_item{addr = Ref, _ = '_'}, 
    CurrentStyle = mnesia:match_object(hn_item, Match, read),
    NewStyleIdx = case CurrentStyle of 
                      []      -> get_style(RefX, Name, Val); 
                      [Style] -> get_style(RefX, Style, Name, Val) 
                  end,
    Ref2 = refX_to_ref(RefX, style),
    Item = #hn_item{addr = Ref2, val = NewStyleIdx},
    % io:format("in process_styles Item is ~p~n", [Item]),
    mnesia:write(Item),

    spawn(fun() -> notify_remote(Item) end), 
    {ok, ok}.

get_style(RefX, Name, Val) ->
    % io:format("in get_style RefX is ~p Name is ~p Val is ~p~n",
    %          [RefX, Name, Val]),
    NoOfFields = ms_util2:no_of_fields(magic_style), 
    Index = ms_util2:get_index(magic_style, Name), 
    Style = make_tuple(magic_style, NoOfFields, Index, Val), 
    % Now write the style 
    write_style(RefX, Style). 

get_style(RefX, Style, Name, Val) -> 
    % use the index of the style to read the style
    Ref = refX_to_ref(RefX, style), 
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
    % io:format("in write_style RefX is ~p Style is ~p~n", [RefX, Style]),
    Ref = refX_to_ref(RefX, style),
    Match = #styles{ref = Ref, magic_style = Style, _ = '_'}, 
    case mnesia:match_object(styles, Match, read) of 
        []              -> write_style2(RefX, Style); 
        [ExistingStyle] -> #styles{index = NewIndex} = ExistingStyle, 
                           NewIndex 
    end. 

write_style2(RefX, Style) ->
    % io:format("in write_style2 RefX is ~p Style is ~p~n", [RefX, Style]),
    Ref = refX_to_ref(RefX, style),
    NewIndex = mnesia:dirty_update_counter(style_counters, Ref, 1), 
    mnesia:write(#styles{ref = Ref, index = NewIndex, magic_style = Style}),
    NewIndex. 

%% @spec notify_remote(Item) -> ok
%% @doc  Adds an attribute to a reference addressed by Ref
notify_remote(Item = #hn_item{addr = #ref{site = Site, path = Path}}) ->
    Name = 'need to pass a name into notify_remote',
    case atom_to_list(Name) of
        [$_, $_|_R]  -> ok; % names of form '__name' are not notified to front-end
        _Other       -> MsgXml=hn_util:item_to_xml(Item),
                        Msg = ?FORMAT("change ~s", [simplexml:to_xml_string(MsgXml)]),
                        gen_server:call(remoting_reg, {change, Site, Path, Msg},?TIMEOUT),
                        ok
    end.

make_tuple(Style, Counter, Index, Val) -> 
    make_tuple(Style, Counter, Index, Val, []). 

make_tuple(S, 0, _I, _V, Acc)     -> list_to_tuple([S|Acc]); 
make_tuple(S, I, I, V, Acc )      -> make_tuple(S, I -1 , I, V, [V | Acc]); 
make_tuple(S, Counter, I, V, Acc) -> make_tuple(S, Counter - 1, I, V, [[] | Acc]).

drop_private(List) -> drop_private(List, []).

drop_private([], Acc) -> Acc;
drop_private([H | T], Acc) ->
    #hn_item{addr = Addr} = H,
    #ref{name = Name} = Addr,
    case atom_to_list(Name) of
        "__"++_ -> drop_private(T, Acc);
        _       -> drop_private(T, [H | Acc])
    end.
            
