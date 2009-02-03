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
%%% Also there is the port bodge function - need to handle port correctly
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
         clear_cells/1,
         clear_cells/2,
         delete_attrs/2,
         shift_cells/2,
         copy_attributes/2,
         get_last/1]).

%% These functions are exposed for the dirty_srv to use
-export([read_local_parents/1,
         read_remote_parents/1]).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Exported functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec get_last(#refX{}) -> {LastColumn, LastRow}
%% LastRow = integer()
%% @doc takes a refX{} and gets the value of the last populated row
%% The refX{} can be refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>row</li>
%% <li>column</li>
%% <li>page</li>
%% </ul>
%% @TODO this may have a race condition if two people try
%% and get the last row/column at the same time...
get_last(#refX{site = S, path = P} = RefX) ->
    io:format("In get_last RefX is ~p~n", [RefX]),
    RefX2 = #refX{site = S, path = P, obj = {page, "/"}},
    Cells = read_cells(RefX2),
    Fun = fun({RefX, _}, {MaxX, MaxY}) ->
                  #refX{obj = {cell, {X, Y}}} = RefX,
                  NewX = ?COND(MaxX > X, MaxX, X),
                  NewY = ?COND(MaxY > Y, MaxY, Y),
                  {NewX, NewY}
          end,                  
    lists:foldl(Fun, {0, 0}, Cells).
    
%% @spec read_remote_parents(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the remote parents of a reference. The reference can only
%% be to a cell and not a range, column, row or page
%% 
%% This fn is called read_remote_parents because it consists of all the
%% remote links where the current RefX is the child
read_remote_parents(#refX{obj = {cell, _}} = RefX) when is_record(RefX, refX) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(remote_cell_link, [{child, Index}]),
    Links = mnesia:match_object(remote_cell_link, Match, read),
    Return = get_parents(Links),
    % io:format("in read_remote_parents Links is ~p~nReturn is ~p~n",
    %          [Links, Return]),
    Return.

%% @spec read_local_parents(RefX :: #refX{}) -> [#refX{}]
%% @doc this returns the local parents of a reference. The reference can only
%% be to a cell and not a range, column, row or page
%% 
%% This fn is called read_local_parents because it consists of all the
%% local links where the current RefX is the child
read_local_parents(#refX{obj = {cell, _}} = RefX) when is_record(RefX, refX) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}} = RefX,
    Index = #index{site = S, path = P, column = X, row = Y},
    Match = ms_util:make_ms(local_cell_link, [{child, Index}]),
    Links = mnesia:match_object(local_cell_link, Match, read),
    get_parents(Links).

%% @spec write_attr(RefX :: #refX{}, Attr) -> {ok, ok}
%% Attr = [atom()]
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
    io:format("In hn_db_wu:write_attr RefX is ~p Attr is ~p~n", [RefX, Attr]),
    % first check that the formula is not part of a shared array
    {_Key, Val} = Attr,
    case read_attrs(RefX, ['__shared']) of
        [_X] -> throw({error, cant_change_part_of_array});
        []   -> write_attr2(RefX, Val)
    end;
write_attr(#refX{obj = {cell, _}} = RefX, {Key, Val} = Attr) ->
    % NOTE the attribute 'overwrite-color' isn't in a magic style and shouldn't be
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> process_styles(RefX, Attr);
        false -> Ref = refX_to_ref(RefX, key),
                 mnesia:write(#hn_item{addr = Ref, val = Val}) 
    end, 
    {ok, ok};
write_attr(#refX{obj = {range, _}} = RefX, Attr) ->
    Ref = refX_to_ref(RefX, "not needed"),
    List = hn_util:range_to_list(Ref),
    List2 = [ref_to_refX(X, "not needed") || X <- List],
    lists:flatten([write_attr(X, Attr) || {X, _Discard} <- List2]).

%% @spec read_cells(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
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

%% @spec read_cells_raw(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
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
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    from_hn_item(mnesia:match_object(hn_item, Match, read));
read_cells_raw(#refX{obj = {range, _}} = RefX) ->
    Range = hn_util:list_from_range(RefX),
    % now just get all the cells
    lists:flatten([read_cells_raw(X) || X <- Range]);
read_cells_raw(#refX{obj = {column, X}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {X, '_'}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    from_hn_item(mnesia:match_object(hn_item, Match, read));
read_cells_raw(#refX{obj = {row, Y}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {'_', Y}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    from_hn_item(mnesia:match_object(hn_item, Match, read));
read_cells_raw(#refX{obj = {page, _}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {'_', '_'}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    from_hn_item(mnesia:match_object(hn_item, Match, read)).                 


%% @spec read_inherited(#refX{}, Key, Default) -> {ok, Value}
%% Key = atom()
%% Value = term()
%% Default = term()
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at RefX, if not found return default
%%       
%% @todo what are the ref types it supports? improve the documentation, etc, etc
read_inherited(RefX, Key, Default) when is_record(RefX, refX)  ->
    Ref = refX_to_ref(RefX, Key),
    case return_first(cell, Ref) of
        {ok, Value} -> {ok, Value};
        nomatch     -> {ok, Default}
    end.

%% @spec read_attrs(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc Reads all the attributes for a reference.
%% The reference can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_attrs(RefX) when is_record(RefX, refX) ->
    io:format("in hn_db_wu:read_attrs/1 RefX is ~p~n", [RefX]),
    read_attrs(RefX, []).

%% @spec read_attrs(#refX{}, AttrsList) -> [{#refX{}, {Key, Value}}]
%% AttrsList = [Key]
%% Key = atom()
%% Value = term()
%% @end
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
    io:format("in hn_db_wu:read_attrs/2 for range~n"),
    #refX{site = S, path = P, obj = {range, {X1, Y1, X2, Y2}}, auth = A} = RefX,
    {MinX, MaxX} = if
                       X1 >= X2 -> {X2, X1};
                       X1 <  X2 -> {X1, X2}
                   end,
    {MinY, MaxY} = if
                       Y1 >= Y2 -> {Y2, Y1};
                       Y1 <  Y2 -> {Y1, Y2}
                   end,
    Ref = {cell, {'$1', '$2'}},
    Match = ms_util:make_ms(ref, [{site, S}, {path , P}, {auth, A}, {ref , Ref}]),
    Match2 = ms_util:make_ms(hn_item, [{addr, Match}, {val, '_'}]),
    Cond = [{'and' , {'>=', '$1', MinX }, {'=<', '$1', MaxX},
             {'>=', '$2', MinY}, {'=<', '$2', MaxY}}],
    Body = ['$_'],
    io:format("in hn_db_wu:read_attrs~n-Match is ~p~n-Match2 is ~p~n"++
             "Cond is ~p~nBody is ~p~n", [Match, Match2, Cond, Body]),
    from_hn_item(mnesia:select(hn_item, [{Match2, Cond, Body}]));
read_attrs(#refX{obj = {column, X}} = RefX, AttrList) when is_list(AttrList) ->
    #refX{site = S, path = P} = RefX,
    R = {cell, {X, '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef,AttrList);
read_attrs(#refX{obj = {row, Y}} = RefX, AttrList) when is_list(AttrList) ->
    #refX{site = S, path = P} = RefX,
    R = {cell, {'_', Y}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList);
read_attrs(#refX{obj = {cell, _}} = RefX, AttrList) when is_list(AttrList) ->
    #refX{site = S, path = P, obj= R} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList);
read_attrs(#refX{obj = {page, _}} = RefX, AttrList) when is_list(AttrList) ->
    #refX{site = S, path = P} = RefX,
    R = {cell, {'_', '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList).

%% @spec shift_cells(RefX :: #refX{}, Offset :: {X :: integer(), 
%% Y :: integer()}) -> {ok, ok}
%% @doc shift_cells takes a cell and shifts it by the offset - provided
%% that offset is valid of course - ie the cell can't be made negative - mebbies
%% it should be).
%% 
%% @todo shift this from an hn_item to a ref calling parameter
shift_cells(RefX, {Key, _Value}) when Key == parents orelse Key == rawvalue orelse Key == value orelse Key == 'dependancy-tree' orelse Key == '__ast' ->
    io:format("in shift_cells (1) RefX is ~p~n", [RefX]),
    % do nothing
    {ok, ok};
shift_cells(RefX, Offset) ->
    io:format("in shift_cells (2) RefX  is ~p~n", [RefX]),
    {ok, ok} = shift_cells(RefX, Offset),
    {ok, ok} = shift_links(RefX, Offset),
    {ok, ok} = shift_hypernumbers(RefX, Offset),
    {ok, ok}.

%% @spec clear_cells(#refX{}) -> {ok, ok}
%% @doc deletes the contents (formula/value) and the formats and attributes
%% of a cell (but doesn't delete the cell itself)
%%  
%% The same as clear_cells(RefX, contents).
clear_cells(RefX) when is_record(RefX, refX) ->
    io:format("in clear_cells RefX is ~p~n", [RefX]),
    clear_cells(RefX, contents).

%% @spec clear_cells(#refX{}, Type) -> {ok, ok} 
%% Type = [contents | style | all]
%% @doc clears a cell or cells - behaviour depends on the value of type
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
%% @TODO put in stuff about formats, styles or attributes only - not sure where...
%% Also this is a naive page delete - iterates over all cells on a page and
%% deletes them
clear_cells(RefX, all) when is_record(RefX, refX)->
    List1 = read_attrs(RefX),
    List2 = [#hn_item{addr = refX_to_ref(X, Key), val = Val} || {X, {Key, Val}} <- List1],
    delete_recs(List2);
clear_cells(RefX, style) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX, style),
    io:format("in clear_cells for style List1 is ~p~n", [List1]),
    List2 = [#hn_item{addr = refX_to_ref(X, Key), val = Val} || {X, {Key, Val}} <- List1],
    delete_recs(List2);    
clear_cells(RefX, contents) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX),
    List2 = get_content_attrs(List1),
    List3 = [#hn_item{addr = refX_to_ref(X, Key), val = Val} || {X, {Key, Val}} <- List2],
    delete_recs(List3).

%% @spec delete_attrs(RefX :: #refX{}, Key) -> {ok, ok}
%% Key = atom()
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
%%% These functions convert to and from #refX, #ref, #hn_item                %%%
%%% and #indexrecords                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

refX_to_ref(RefX, Name) ->
    #refX{site = S, path = P, obj = R, auth = A} = RefX,
    #ref{site = S, path = P, ref = R, name = Name, auth = A}.

ref_to_refX(Ref, Val) ->
    #ref{site = S, path = P, ref = R, name = Key, auth = A} = Ref,
    RefX = #refX{site = S, path = P, obj = R, auth = A},
    {RefX, {Key, Val}}.

from_hn_item(List) -> from_hn_item(List, []).

from_hn_item([], Acc)      -> Acc;
from_hn_item([H | T], Acc) -> #hn_item{addr = Ref, val = V} = H, 
                              NewAcc = ref_to_refX(Ref, V),
                              from_hn_item(T, [NewAcc | Acc]).

refX_from_index(#index{site = S, path = P, column = X, row = Y}) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}}.

index_from_refX(#refX{site = S, path = P, obj = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

index_from_ref(#ref{site = S, path = P, ref = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mark_cell_dirty(Index) ->
    % Make a list of cells hypernumbers + direct
    % cell links, and check for any wildcard * on the path

    % first up local
    RefX = refX_from_index(Index),
    Local = read_local_parents(RefX),
    % now wildcards (must be fixed!)
    NIndex = Index#index{path=lists:reverse(Index#index.path)},
    Queries = dyn_parents(NIndex,[],[]),

    Local2 = lists:append(Local, Queries),
    
    % now read hypernumbers
    Remote = read_remote_parents(RefX),

    % Now write the local children to dirty_cell
    Fun = fun(X) -> 
                  % only write the dirty cell if 
                  % it doesnt already exist
                  Match = ms_util:make_ms(dirty_cell, [{index, X}]),
                  Match2 = #dirty_cell{index = X, _= '_'},
                  case mnesia:match_object(Match) of
                      [] -> mnesia:write(#dirty_cell{index = X}) ;
                      _  -> ok
                  end
          end,
    _Return1 = lists:foreach(Fun, Local2),
    % Now write notify the remote children that they are dirty
    % get the new value first
    Val = read_attrs(RefX, [rawvalue]),
    Notify = fun(X) ->
                     notify_remote_change(X, Val)
             end,        
    _Return2 = lists:foreach(Notify, Remote),
    ok.

notify_remote_change(Hn,Value) ->
    {Server,Cell} = Hn#outgoing_hn.index,
    Version = hn_util:text(Hn#outgoing_hn.version + 1),
    error_logger:error_msg("in hn_db:notify_remote_change *WARNING* "++
                           "notify remote change not using "++
                           "version number ~p - ie it aint working - yet :(",
                           [Version]),

    Actions = simplexml:to_xml_string(
                {notify,[],[
                            {biccie,      [],[Hn#outgoing_hn.biccie]},
                            {cell,        [],[hn_util:index_to_url(Cell)]},
                            {type,        [],["change"]},
                            {value,       [],hn_util:to_xml(Value)},
                            {version,     [],["1"]}
                           ]}),

    hn_util:post(Server,Actions,"text/xml"),
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
    % it will be a lot slower - buy hey!
    El = {local_cell_link,Index#index{path=Path},'_'},
    mnesia:match_object(El).

get_parents(List) -> get_p(List, []).

get_p([], Acc) -> Acc;
get_p([#local_cell_link{parent = P} | T], Acc) -> get_p(T, [P | Acc]).

delete_remote_parents(List) ->
    io:format("write delete_remote_parents~n"),
    {ok, ok}.

delete_local_parents(List) ->
    io:format("In delete_local_parents List is ~p~n", [List]),
    % Ooh! dodgy double transform, eek!
    % Index = hn_util:ref_to_index(refX_to_ref(RefX, child)),
    % MatchRef = ms_util:make_ms(local_cell_link, [{child, Index}]), 
    % List = mnesia:match_object(local_cell_link, MatchRef, read),
    delete_recs(List).

write_local_parents(Child, List) ->
    Child2 = index_from_refX(Child),
    Fun = fun(C, P) ->
                  {url, _, [Url]} = P,
                  {ok, Ref} = hn_util:parse_url(Url),
                  #ref{site = S} = Ref,
                  S2 = port_bodge(S),
                  Ref2 = Ref#ref{site = S2},
                  Index = index_from_ref(Ref2),
                  mnesia:write(#local_cell_link{child = C, parent = Index})
          end,
    [ok = Fun(Child2, X) || X <- List]. 

write_remote_parents(Child, List) ->
    Child2 = index_from_refX(Child),
    Fun = fun(C, P) ->
                  {url, _, [Url]} = P,
                  {ok, Ref} = hn_util:parse_url(Url),
                  #ref{site = S} = Ref,
                  S2 = port_bodge(S),
                  Ref2 = Ref#ref{site = S2},
                  I = index_from_ref(Ref2),
                  T = incoming,
                  Record = #remote_cell_link{child = C, parent = I, type = T},
                  mnesia:write(Record)
          end,
    [ok = Fun(Child2, X) || X <- List]. 

port_bodge(String) -> [Proto, Site | _T] = string:tokens(String, ":"),
                      Proto ++ ":" ++Site.

delete_recs([])      -> {ok, ok};
delete_recs([H | T]) -> ok = mnesia:delete_object(H),
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
    from_hn_item(mnesia:select(hn_item, [{Match, Cond, Body}])).

shift_cells2(Addr, Offset) ->
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
    case superparser:process(Val) of
        {formula, Fla}        -> write_formula1(RefX, Val, Fla);
        [NewVal, Align, Frmt] -> write_formula2(RefX, Val, NewVal, Align, Frmt)
    end.

write_formula1(RefX, Val, Fla) ->
    Ref = refX_to_ref(RefX, Val),
    Rti = ref_to_rti(Ref, false),
    case muin:run_formula(Fla, Rti) of
        {error, Error} ->
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
    % io:format("in write_cell~n-RefX is ~p~nValue is ~p~nFormula is ~p~n"++
    %          "Parents is ~p~nDepTree is ~p~n", [RefX, Value, Formula,
    %                                             Parents, DepTree]),
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
    Ref = refX_to_ref(RefX, 'formula'),
    {NewLocPs, NewRemotePs} = split_local_remote(Parents),

    % write the formula
    Item = #hn_item{addr = Ref, val = Formula},
    ok = mnesia:write(Item),

    % now write the rawvalue, etc, etc
    {ok, ok} = write_rawvalue(RefX, Value),

    % overwrite the parents and 'dependancy-tree'
    Set = fun(X, {Key, {xml,[]}}) -> delete_attrs(X, Key);
             (X, {Key, Val})      -> write_attr(X, {Key, Val})
          end,

    Set(RefX, {'parents',         {xml, Parents}}),
    Set(RefX, {'dependancy-tree', {xml, DepTree}}),

    % now do the local parents
    OldLocPs = read_local_parents(RefX),
    {DelLocPs, WriteLocPs} = split_parents(OldLocPs, NewLocPs),
    delete_local_parents(DelLocPs),
    write_local_parents(RefX, WriteLocPs),

    % now do the remote parents
    OldRemotePs = read_remote_parents(RefX),
    % io:format("OldRemotePs is ~p~n", [OldRemotePs]),
    {DelRemotePs, WriteRemotePs} = split_parents(OldRemotePs, NewRemotePs),
    % io:format("DelRemotesPs are ~p~n WriteRemotePs are ~p~n",
    %          [DelRemotePs, WriteRemotePs]),
    delete_remote_parents(DelRemotePs),
    write_remote_parents(RefX, WriteRemotePs),


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
sp_lr([{_, [{_, "local"}], _} = P | T], {A1, A2})  -> sp_lr(T, {[P | A1], A2});
sp_lr([{_, [{_, "remote"}], _} = P | T], {A1, A2}) -> sp_lr(T, {A1, [P | A2]}).

write_rawvalue(RefX, Value) ->
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
    mnesia:write(Item),

    spawn(fun() -> notify_remote(Item) end), 
    {ok, ok}.

get_style(RefX, Name, Val) ->
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
    Ref = refX_to_ref(RefX, style),
    Match = #styles{ref = Ref, magic_style = Style, _ = '_'}, 
    case mnesia:match_object(styles, Match, read) of 
        []              -> write_style2(RefX, Style); 
        [ExistingStyle] -> #styles{index = NewIndex} = ExistingStyle, 
                           NewIndex 
    end. 

write_style2(RefX, Style) ->
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
    {_, {Name, _}} = H,
    case atom_to_list(Name) of
        "__"++_ -> drop_private(T, Acc);
        _       -> drop_private(T, [H | Acc])
    end.

