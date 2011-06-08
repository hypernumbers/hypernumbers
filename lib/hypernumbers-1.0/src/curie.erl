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
	NewAST = walk(AST, RetRef, ParamList, [], Site, Path),
	
    io:format("---------Final AST is~n"),
    NewAST.

walk([], _, _, Acc,_ ,_) ->
	lists:reverse(Acc);
walk([{cellref, _OffsetX, _OffsetY, _Path, Cell} = H | T], Ref, Params, Acc, Site, Path) ->
	case contains(Params, Cell) of
		true	->
			walk(T, Ref, Params, [H | Acc], Site, Path);
		false	->
			walk([walk_helper(H, Ref, Params, Site, Path) | T], Ref, Params, Acc, Site, Path)
	end;
%TODO
%	-translate range to list of cell names ("b1:b3" = ["b1","b2","b3"])
%	-update function contains, so it can take two lists as argument and 
%	 return true if list1 is a sublist of list2
%	-then walk through the list of cells
%	-!!!!!!remember to remove underscores from arguments
walk([{rangeref, finite, _, {{offset, StartX}, {offset, StartY}}, {{offset, StopX}, {offset, StopY}}, _, _, RangeString} = H | _T], _Ref, _Params, _Acc, _Site, _Path)	->
	io:format("Hello rangeref~n"),
	%(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX)
	List = hn_util:range_to_list(#refX{obj = {range, {StartX, StartY, StopX, StopY}}}),
	io:format("TODO:~nH is ~p~nRangeString is ~p~nList is ~p~n", [H, RangeString, List]);
	
walk([H | T], Ref, Params, Acc, Site, Path)	when is_list(H)	->
	walk(T, Ref, Params, [walk(H, Ref, Params, [], Site, Path) | Acc], Site, Path);
walk([H | T], Ref, Params, Acc, Site, Path) ->
	walk(T, Ref, Params, [H | Acc], Site, Path).


%if in an AST there is a reference to a cell from outside of the input range this function is called,
%it takes the cell's reference and retreives an AST which the cell holds
walk_helper(NewReference, _Ref, Params, Site, Path)	->
	{cellref, _OffsetX, _OffsetY, _Path, Cell} = NewReference,
	%calculate new 
	
	NewRetRef = #refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(Cell)},
    io:format("NewRetRef is ~p~n", [NewRetRef]),
	case contains(Params, Cell) of
		true	-> 
			NewReference;
		false	->
			[{_, AST2}] = new_db_api:read_attribute(NewRetRef, "__ast"),
			case check_off_page(AST2, Path) of
				invalid -> {error, offpageref};
				valid   -> AST2
			end
	end.

check_off_page([], _Path) -> valid;
check_off_page([{cellref, _, _, P, _} | T], Path) ->
		case muin_util:walk_path(Path, P) of
        Path -> check_off_page(T, Path);
        _    -> invalid
    end;
check_off_page([_H | T], Path) ->
	check_off_page(T, Path).


%return true if list contains Element, false otherwie
contains([], _Element)	->
	false;
contains([Element | _T], Element)	->
	true;
contains([_H | T], Element)	->
	contains(T, Element).
