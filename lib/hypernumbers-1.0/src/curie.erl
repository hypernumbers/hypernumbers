%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  3 Jun 2011 by gordon@hypernumbers.com

-module(curie).

-include("spriki.hrl").

-compile(export_all).
%-export([
%         build_fun/4
%        ]).


%%curie:build_fun("http://hypernumbers.dev:9000", ["function"], "b8", ["b1", "b2", "b3"]).
build_fun(Site, Path, ReturnRef, ParamList) ->
	RetRef = #refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(ReturnRef)},
    [{_, AST}] = new_db_api:read_attribute(RetRef, "__ast"),
    io:format("---------Initial AST is~n~p~n", [AST]),
    %params in upper case, easier to compare
    ParamListUpper = lists:map(fun string:to_upper/1, ParamList),
	%no cells from different workbooks are allowed
    case check_off_page(AST, Path) of
        invalid -> {error, offpageref};
        valid   -> build_fun2(RetRef, AST, ParamListUpper, Site, Path)
    end.

build_fun2(RetRef, AST, ParamList, Site, Path) ->
	NewAST = walk_zero(AST, RetRef, ParamList, [], Site, Path),
	
    io:format("---------Final AST is~n"),
    NewAST.


walk_zero(AST, RetRef, ParamList, [], Site, Path)	-> walk(AST, RetRef, ParamList, [], Site, Path, RetRef).


walk([], _, _, Acc,_ ,_, _) ->
	lists:reverse(Acc);
walk([{cellref, _OffsetX, _OffsetY, _Path, Cell} = H | T], Ref, Params, Acc, Site, Path, _FinalRetRef) ->
	case contains(Params, Cell) of
		true	->
			walk(T, Ref, Params, [H | Acc], Site, Path, _FinalRetRef);
		false	->
			walk([walk_helper_cellref(H, Ref, Params, Site, Path) | T], walk_new_ret_ref(H, Site), Params, Acc, Site, Path, _FinalRetRef)
	end;
%TODO
%	-translate range to list of cell names ("b1:b3" = ["b1","b2","b3"])
%	-update function contains, so it can take two lists as argument and 
%	 return true if list1 is a sublist of list2
%	-then walk through the list of cells
%	-!!!!!!remember to remove underscores from arguments
walk([{rangeref, finite, _, {{offset, StartX}, {offset, StartY}}, {{offset, StopX}, {offset, StopY}}, _, _, RangeString} = H | T], Ref, Params, Acc, Site, Path, FinalRetRef)	->
	io:format("Hello rangeref~n"),
	%(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX)
	
	%get return cell coordinates:
	%Ref is {refX,"http://hypernumbers.dev:9000",undefined,"./",{cell,{2,6}}}

	
	
	io:format("Ref is ~p~nFinalRetRef is ~p~n", [Ref, FinalRetRef]),
	

	io:format("Ref is ~p~n", [Ref]),
	walk_helper_rangeref(Ref, FinalRetRef,hn_util:range_to_list(#refX{site = Site, path = Path,obj = {range, {StartX, StartY, StopX, StopY}}})),
	
	
	walk(T, Ref, Params, [H | Acc], Site, Path, FinalRetRef);
	
walk([H | T], Ref, Params, Acc, Site, Path, _FinalRetRef)	when is_list(H)	->
	walk(T, Ref, Params, [walk(H, Ref, Params, [], Site, Path, _FinalRetRef) | Acc], Site, Path, _FinalRetRef);
walk([H | T], Ref, Params, Acc, Site, Path, _FinalRetRef) ->
	io:format("-skipped ~p~n", [H]),
	walk(T, Ref, Params, [H | Acc], Site, Path, _FinalRetRef).


%used when AST contains a reference to a cell not in the input range,
walk_new_ret_ref(NewReference, Site)	->
	{cellref, _OffsetX, _OffsetY, Path, Cell} = NewReference,
	%calculate new refX
	NewRetRef = #refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(Cell)}.
                   
walk_helper_rangeref(CurrentRef, FinalRetRef, List)	->
	io:format("walk_helper_rangeref:~nCurrentRef is ~p~nFinalRetRef is ~p~nList is ~p~n", [CurrentRef, FinalRetRef, List]),
	{_, _, _, _, {cell, {ReturnX, ReturnY}}} = FinalRetRef,
	{_, _, _, _, {cell, {CurrentX, CurrentY}}} = CurrentRef,
	DeltaX = CurrentX - ReturnX, 
	DeltaY = CurrentY - ReturnY,
	io:format("DeltaX is ~p~nDeltaY is ~p~n", [DeltaX, DeltaY]),
	io:format("walk_helper_rangeref END:~n"),
	update_rangeref_offset(DeltaX, DeltaY, List, ReturnX, ReturnY, []).

update_rangeref_offset(DeltaX, DeltaY, [{refX, _, _, _, {cell,{X,Y}}} = H | T], ReturnX, ReturnY, Acc)	->a. 





%if in an AST there is a reference to a cell from outside of the input range this function is called,
%it takes the cell's reference and retreives an AST which the cell holds
walk_helper_cellref(NewReference, Ref, Params, Site, Path)	->
	%get the cell name and store it in Cell
	{cellref, {offset, OffsetX}, {offset, OffsetY}, _Path, Cell} = NewReference,
	%calculate new refX
	NewRetRef = #refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(Cell)},
    case contains(Params, Cell) of
		true	-> 
			NewReference;
		false	->
			[{_, AST}] = new_db_api:read_attribute(NewRetRef, "__ast"),
			case check_off_page(AST, Path) of
				invalid -> {error, offpageref};
				valid   -> 
					update_cellref_offset(AST, OffsetX, OffsetY, [])
			end
	end.
	

update_cellref_offset([], _, _, Acc)	-> 
	NewList = lists:reverse(Acc),
	NewList;
update_cellref_offset([H | T], X, Y, Acc) when is_list(H)	->
	update_cellref_offset(T, X, Y, [ update_cellref_offset(H , X, Y, []) | Acc]);
update_cellref_offset([{cellref, {offset, OldX}, {offset, OldY}, _A, _B} | T], X, Y, Acc)	->
	update_cellref_offset(T, X, Y, [{cellref, {offset, OldX + X}, {offset, OldY + Y}, _A, _B} | Acc]);
update_cellref_offset([H | T], X, Y , Acc)	-> 	update_cellref_offset(T, X, Y , [H | Acc]).


check_off_page([], _Path) -> valid;
check_off_page([{cellref, _, _, P, _} | T], Path) ->
		case muin_util:walk_path(Path, P) of
        Path -> check_off_page(T, Path);
        _    -> invalid
    end;
check_off_page([_H | T], Path) ->
	check_off_page(T, Path).


%return true if list contains Element, false otherwise
contains([], _Element)	->
	false;
contains([Element | _T], Element)	->
	true;
contains([_H | T], Element)	->
	contains(T, Element).
