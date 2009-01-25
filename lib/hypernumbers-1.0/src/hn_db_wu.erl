%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This is a util function for hn_db containing only functions
%%%            that MUST be called from within an mnesia transaction
%%%
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_wu).

-export([shift_cell/2,
         shift_list/2,
         copy_attributes/2,
         delete_cell/1,
         delete_cell/2,
         delete_list/1,
         delete_list/2]).

-include("spriki.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Exported funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec shift_cell(Addr :: #hn_item{}, Offset :: {X :: integer(), 
%% Y :: integer()}) -> {ok, ok}
%% @doc shift_cell takes a cell and shifts it by the offset - provided
%% that offset is valid off course (ie the cell can't be made negative - mebbies
%% it should be).
%% 
%% Why isn't that a #ref{}?
shift_cell(#hn_item{addr = #ref{name = Attr}} = Ref, _) when 
Attr == parents orelse Attr == rawvalue orelse Attr == value
orelse Attr == 'dependancy-tree' orelse Attr == '__ast' ->
    % do nothing
    {ok, ok};
shift_cell(#hn_item{addr = #ref{name = formula}} = Ref, Offset) ->
    io:format("in shift_cell Ref  is ~p~n", [Ref]),
    {ok, ok} = shift_cell(Ref, Offset),
    {ok, ok} = shift_links(Ref, Offset),
    {ok, ok} = shift_hypernumbers(Ref, Offset),
    {ok, ok};
shift_cell(Ref, Offset) ->
    shift_cell2(Ref, Offset).

%% @spec shift_list(List, 
%% Offset :: {X :: integer(), Y :: integer()}) 
%% -> {ok, ok} List = [#hn_item{}]
%% @doc shift takes a list of cells and shifts them by the offset (which
%% must be valid).
%% 
%% The list is processed singly and therefore the cells being offset can be 
%% on different pages
%% 
%% Why isn't that a #ref{}?
shift_list(List, Offset) -> [{ok, ok} = shift_cell(X, Offset) || X <- List].

%%  @spec delete_cell(Ref :: #ref{}) -> {ok, ok}
%%  @doc deletes the contents (formula/value) and the formats and attributes
%%  of a cell (but doesn't delete the cell itself)
%%  
%%  The same as delete_cell(Ref, contents).
delete_cell(Ref) -> delete_cell(Ref, contents).

%%  @spec delete_list(List) -> {ok, ok} List = [#ref{}]
%%  @doc deletes the contents (formula/value) and the formats and attributes
%%  of a cell (but doesn't delete the cell itself)
%%  
%%  The same as delete_list(List, contents).
delete_list(List) -> delete_list(List, contents).

%%  @spec delete_cell(Ref :: #ref{}, Type) -> {ok, ok} 
%%  Type = [contents | all]
%%  @doc deletes a cell - behaviour depends on the value of type
%%  <ul><li><code>contents</code> - deletes the formula/value but not the attributes
%%  or formats (or the cell itself)</li>
%%  <li><code>all</code> - deletes the contents, formats, styles 
%%  and attributes (but not the cell itself)</li></ul>
%%  
%%  TODO - put in stuff about formats, styles or attributes only - not sure where...
delete_cell(Ref, contents) ->
      Data = [{formula, [], []},
            {rawvalue, [],[]},
            {value, [], []},
            {'__ast', [], []},
            {'__recompile', [], []},
            {'__shared', [], []},
            {'__area', [], []},
            {'dependency-tree', [], []},
            {parents, [], []}],
    lists:map
      (
      fun({Attr,[],[]}) ->
              % TODO fix the call to hn_db:remove
              hn_db:remove_item(Ref#ref{name=Attr})
      end,
      Data
     );
delete_cell(Ref, all) ->
    io:format("write hn_db_wu:delete_cell~n"),
    {ok, ok}.

%%  @spec delete_list(List, Type) -> {ok, ok} 
%%  List = [#ref{}]
%%  Type = [contents | all]
%%  @doc Applies delete_cell to the list
delete_list(List, Type) -> [{ok, ok} = delete_cell(X, Type) || X <- List].

%% @spec copy_attributes(List, To :: #ref{}) -> {ok, ok}
%% List = [#ref{}]
%% @doc fix me up...
%% split out into copy_attributes and copy_attributes_list
%% prolly rename me
%% take out the call to hn_main, blah, blah...
copy_attributes([],_To)                -> {ok, ok};
copy_attributes([{_, Ref, V} | T], To) -> #ref{name = Name} = Ref,
                                          Addr=To#ref{name = Name},
                                          hn_main:set_attribute(Addr, V),
                                          copy_attributes(T, To).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shift_cell2(Ref, Offset) ->
    % Rewrite the shifted cell
    {XO, YO} = Offset,
    #hn_item{addr = Addr, val = Val} = Ref,
    #ref{ref = {cell, {X, Y}}} = Addr,
    Addr2 = Addr#ref{ref = {cell, {X + XO, Y + YO}}},
    % now write it
    ok = hn_main:set_attribute(Addr2, Val),
    {ok, ok}.

shift_links(Ref, Offset) ->
    % Now rewrite the cells that link to this cell
    {XO, YO} = Offset,
    #hn_item{addr = Addr, val = Val} = Ref,
    #ref{ref = {cell, {X, Y}}} = Addr,
    Index = hn_util:ref_to_index(Addr),
    Head = ms_util:make_ms(local_cell_link, [{parent, Index}]),
    LinkedCells = mnesia:select(local_cell_link, [{Head, [], ['$_']}]),
    io:format("LinkedCells are ~p~n", [LinkedCells]),
    shift_links2(LinkedCells, Offset).

shift_links2([], _Offset) -> {ok, ok};
shift_links2([#local_cell_link{child = Child, parent = Parent} | T], Offset) ->
    io:format("in shift_links2 Child is ~p~nParent is ~p~n", [Child, Parent]),
    shift_links2(T, Offset).

%% TODO
shift_hypernumbers(Ref, Offset) ->
    io:format("hn_db_util:shift_hypernumbers needs to be written!~n"),
    {ok, ok}.
