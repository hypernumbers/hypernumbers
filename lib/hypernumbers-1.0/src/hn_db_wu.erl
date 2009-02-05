%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This is a util function for hn_db containing only functions
%%%            that MUST be called from within an mnesia transactions
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
%%% And also when a new style is written for a page it should notify the
%%% viewing pages to update themselves or the style stuff won't work...
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
         read_styles/1,
         clear_cells/1,
         clear_cells/2,
         delete_attrs/2,
         shift_cell/2,
         copy_cell/3,
         copy_attrs/3,
         get_last/1]).

%% These functions are exposed for the dirty_srv to use
-export([read_local_parents/1,
         read_remote_parents/1]).

-define(to_xml_str, simplexml:to_xml_string).

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
    Fun = fun({R, _}, {MaxX, MaxY}) ->
                  #refX{obj = {cell, {X, Y}}} = R,
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

%% @spec write_attr(RefX :: #refX{}, {Key, Value}) -> {ok, ok}
%% Key = atom()
%% Value = term()
%% @doc This function writes attributes EITHER to a cell or a range of cells OR
%% to a column, row or page
%% If it writes to a range it will be expanded to write to all the cells in the
%% range
%% 
%% This function deals with style-able attributes of cells auto-magically.
%% 
%% If an attribute is saved against a cell which is one of the styled
%% attributes defined in the ref magic_styles in file 
%% <a href="../include/spriki.hrl">spriki.hrl</a>
%% it will be magically managed as a style
%% ~end
%% This clause deals with a formula
write_attr(#refX{obj = {cell, _}} = RefX, {formula, _} = Attr) ->
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
        false -> Ref = hn_util:refX_to_ref(RefX, Key),
                 % notify any registered front ends
                 Record = #hn_item{addr = Ref, val = Val},
                 mnesia:write(Record),
                 spawn(fun() -> notify_remote(Record, change) end)
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
    spawn(fun() -> notify_remote(Record, change) end),
    {ok, ok}.

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
    hn_util:from_hn_item(mnesia:match_object(hn_item, Match, read));
read_cells_raw(#refX{obj = {range, _}} = RefX) ->
    Range = hn_util:list_from_range(RefX),
    % now just get all the cells
    lists:flatten([read_cells_raw(X) || X <- Range]);
read_cells_raw(#refX{obj = {column, X}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {X, '_'}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    hn_util:from_hn_item(mnesia:match_object(hn_item, Match, read));
read_cells_raw(#refX{obj = {row, Y}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {'_', Y}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    hn_util:from_hn_item(mnesia:match_object(hn_item, Match, read));
read_cells_raw(#refX{obj = {page, _}} = RefX) ->
    #refX{site = S, path = P} = RefX,
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, {cell, {'_', '_'}}}]),
    Match = ms_util:make_ms(hn_item, [{addr, MatchRef}]),
    hn_util:from_hn_item(mnesia:match_object(hn_item, Match, read)).                 

%% @spec read_inherited(#refX{}, Key, Default) -> {ok, Value}
%% Key = atom()
%% Value = term()
%% Default = term()
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at RefX, if not found return default
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
read_attrs(#refX{obj = {range, _}} = RefX, AttrList) when is_list(AttrList) ->
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
                [] -> [{'and' , {'>=', '$1', MinX }, {'=<', '$1', MaxX},
                        {'>=', '$2', MinY}, {'=<', '$2', MaxY}}];
                [X] -> [{'and', {'and' , {'>=', '$1', MinX }, {'=<', '$1', MaxX},
                                 {'>=', '$2', MinY}, {'=<', '$2', MaxY}}, X}]
            end,
    Body = ['$_'],
    MatchRef = {Match2, Cond, Body},

    %% because this function calculates its own conditional don't go
    %% to read_attrs2/2
    hn_util:from_hn_item(mnesia:select(hn_item, [MatchRef]));
read_attrs(#refX{obj = {column, X}} = RefX, AttrList) when is_list(AttrList) ->
    #refX{site = S, path = P} = RefX,
    R = {cell, {X, '_'}},
    MatchRef = ms_util:make_ms(ref, [{site, S}, {path, P}, {ref, R}, {name, '$1'}]),
    read_attrs2(MatchRef, AttrList);
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

%% @spec shift_cell(RefX :: #refX{}, Offset :: {X :: integer(), 
%% Y :: integer()}) -> {ok, ok}
%% @doc shift_cells takes a cell and shifts it by the offset - provided
%% that offset is valid of course - ie the cell can't be made negative - mebbies
%% it should be).
%% 
%% @todo shift this from an hn_item to a ref calling parameter
shift_cell(From, To) when is_record(From, refX), is_record(To, refX) ->
    io:format("in shift_cell (2) From is ~p To  is ~p~n", [From, To]),
    {ok, ok} = shift_cell2(From, To),
    {ok, ok} = shift_links(From, To),
    {ok, ok} = shift_hypernumbers(From, To),
    {ok, ok}.

%% @spec read_styles(#refX{}) -> [Style]
%% Style = #styles{}
%% @doc returns a list of styles associated with the ref which
%% can refer to any of a:
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
    List2 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
             || {X, {Key, Val}} <- List1],
    delete_recs(List2);
clear_cells(RefX, style) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX, style),
    List2 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
             || {X, {Key, Val}} <- List1],
    delete_recs(List2);    
clear_cells(RefX, contents) when is_record(RefX, refX) ->
    List1 = read_attrs(RefX),
    List2 = get_content_attrs(List1),
    List3 = [#hn_item{addr = hn_util:refX_to_ref(X, Key), val = Val}
             || {X, {Key, Val}} <- List2],
    delete_recs(List3).

%% @spec delete_attrs(RefX :: #refX{}, Key) -> {ok, ok}
%% Key = atom()
%% @doc deletes the attribute with key Key from a
%% of a cell (but doesn't delete the cell itself)
delete_attrs(RefX, Key) ->
    Ref = hn_util:refX_to_ref(RefX, Key),
    case ms_util2:is_in_record(magic_style, Key) of 
        true  -> delete_style_attr(RefX, Key);
        false -> mnesia:delete({hn_item, Ref}),
                 Record = #hn_item{addr = Ref, val = "not important"},
                 spawn(fun() -> notify_remote(Record, delete) end),
                 {ok, ok}
    end.

%% @spec copy_cell(From :: #refX{}, To ::#refX{}, Incr) -> {ok, ok}
%% Incr = [false | horizonal | vertical]
%% @doc copys cells from a ref to a ref
copy_cell(#refX{obj = {cell, _}} = From, #refX{obj = {cell, _}} = To, Incr)
  when is_record(From, refX), is_record(To, refX) ->
    % io:format("in hn_db_wu:copy_cells From is ~p To is ~p Incr is ~p~n",
    %           [From, To, Incr]),
    FromList = read_cells_raw(From),
    % io:format("in hn_db_wu:copy_cells FromList is ~p~n", [FromList]),
    {Contents, FilteredList} = filter_for_drag_n_drop(FromList),
    % io:format("in hn_db_wu:copy_cells Contents is ~p FilteredList is ~p~n",
    %           [Contents, FilteredList]),
    Output = case Contents of
                 [Contents2] -> superparser:process(Contents2);
                 []          -> ""
             end,
    #refX{obj = {cell, {FX, FY}}} = From,
    #refX{obj = {cell, {TX, TY}}} = To,
    % io:format("in hn_db_wu:copy_cells Output is ~p~n", [Output]),
    case Output of
        {formula, Formula} ->
            {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {FX, FY}),
            NewToks = offset(Toks, (TX - FX), (TY - FY)),
            NewFormula = make_formula(NewToks),
            % io:format("in hn_db_wu:copy_cells~nFormula is ~p~n-NewFormula is ~p~n",
            %          [Formula, NewFormula]),
            {ok, ok} = write_attr(To, {formula, NewFormula});
        [{Type, V},  _A, _F] ->
            % io:format("in hn_db_wu:copy_cells Type is ~p V is ~p~n",
            %           [Type, V]),
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
            % io:format("in hn_db_wu:copy_cells V2 is ~p~n", [V2]),
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

%% @spec copy_attrs({From :: #refX{}, To :: #refX{}, AttrList) -> {ok, ok}
%% AttrList = [atom()]
%% @doc This function copies all the attributes listed from the From #ref{} to
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
                  % Match2 = #dirty_cell{index = X, _= '_'}, ?????
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
    case List of
        [] -> {ok, ok};
        _  -> io:format("write delete_remote_parents in hn_db_wu.erl~n"),
              {ok, ok}
    end.

delete_local_parents([]) -> {ok, ok};
delete_local_parents([H | T]) -> 
    Match = ms_util:make_ms(local_cell_link, [{child, H}]), 
    Parents = mnesia:match_object(local_cell_link, Match, read),
    delete_recs(Parents),
    delete_local_parents(T).

write_local_parents(Child, List) ->
    Child2 = hn_util:index_from_refX(Child),
    Fun = fun(C, P) ->
                  {url, _, [Url]} = P,
                  {ok, Ref} = hn_util:parse_url(Url),
                  #ref{site = S} = Ref,
                  S2 = port_bodge(S),
                  Ref2 = Ref#ref{site = S2},
                  Index = hn_util:index_from_ref(Ref2),
                  mnesia:write(#local_cell_link{child = C, parent = Index})
          end,
    [ok = Fun(Child2, X) || X <- List]. 

write_remote_parents(Child, List) ->
    Child2 = hn_util:index_from_refX(Child),
    Fun = fun(C, P) ->
                  {url, _, [Url]} = P,
                  {ok, Ref} = hn_util:parse_url(Url),
                  #ref{site = S} = Ref,
                  S2 = port_bodge(S),
                  Ref2 = Ref#ref{site = S2},
                  I = hn_util:index_from_ref(Ref2),
                  T = incoming,
                  Record = #remote_cell_link{child = C, parent = I, type = T},
                  mnesia:write(Record)
          end,
    [ok = Fun(Child2, X) || X <- List]. 

port_bodge(String) -> [Proto, Site | _T] = string:tokens(String, ":"),
                      Proto ++ ":" ++Site.

delete_recs([]) ->
    {ok, ok};
delete_recs([H | T]) when is_record(H, hn_item) ->
    ok = mnesia:delete_object(H),
    spawn(fun() -> notify_remote(H, delete) end),
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
    Offset = {3, 4},
    Addr = 2,
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
    Ref = hn_util:refX_to_ref(RefX, Val),
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
    Ref = hn_util:refX_to_ref(RefX, 'formula'),
    {NewLocPs, NewRemotePs} = split_local_remote(Parents),

    % write the formula

    Record = #hn_item{addr = Ref, val = Formula},
    ok = mnesia:write(Record),
    spawn(fun() -> notify_remote(Record, change) end),

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
    Ref = hn_util:refX_to_ref(RefX, rawvalue),
    Record1 = #hn_item{addr = Ref, val = Value},
    mnesia:write(Record1),
    spawn(fun() -> notify_remote(Record1, change) end),
    {ok, Format} = read_inherited(RefX, format, "General"),
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {Color, V}}=format:run_format(Value,Output),
    Ref1 = hn_util:refX_to_ref(RefX, value),
    Record2 = #hn_item{addr = Ref1, val = V},
    mnesia:write(Record2),
    spawn(fun() -> notify_remote(Record2, change) end),
    Ref2 = hn_util:refX_to_ref(RefX, 'overwrite-color'),
    Record3 = #hn_item{addr = Ref2, val = atom_to_list(Color)},
    mnesia:write(Record3),
    spawn(fun() -> notify_remote(Record3, change) end),
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
    io:format("in delete_style_attr~n-CurrentStyle is ~p~n-NewStyleIdx is ~p~n",
              [CurrentStyle, NewStyleIdx]),
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
    spawn(fun() -> notify_remote(Record, change) end),
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
    %% should spawn a notification that there is a new style
    io:format("in hn_db_wu:write_style2- needs to be fixed!~n"),
    mnesia:write(#styles{ref = Ref2, index = NewIndex, magic_style = Style}),
    NewIndex. 

%% @spec notify_remote(Item, Type) -> ok
%% Type = [change | delete]
%% @doc  Adds an attribute to a reference addressed by Ref
notify_remote(Item, Type) when is_record(Item, hn_item) ->
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

