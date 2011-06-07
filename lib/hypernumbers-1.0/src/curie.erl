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
	io:format("---------build_fun---------~n"),
    io:format("RetRef is ~p~n", [RetRef]),
    io:format("---------build_fun---------~n"),
    [{_, AST}] = new_db_api:read_attribute(RetRef, "__ast"),
    %params in upper case, easier to compare
    ParamListUpper = lists:map(fun string:to_upper/1, ParamList),
	%no cells from different workbooks are allowed
    case check_off_page(AST, Path) of
        invalid -> {error, offpageref};
        valid   -> build_fun2(RetRef, AST, ParamListUpper, Site, Path)
    end.

build_fun2(RetRef, AST, ParamList, Site, Path) ->
	io:format("---------build_fun2---------~n"),
    io:format("ReturnRef is ~p~n AST is ~p~nParamList is ~p~n",
              [RetRef, AST, ParamList]),
    NewAST = walk(AST, RetRef,ParamList, [], Site, Path),
    io:format("NewAST is ~p~n", [NewAST]),
    io:format("---------build_fun2---------~n"),
    ok.

%TODO
%	Do you have to check off page reference for each cell that is in the AST?
%	If so, find the place where to place call to check_off_page,
%	to find the answer see how muin_util:walk_path works.
walk([], _, _, Acc,_ ,_) -> lists:reverse(Acc),
	io:format("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ACC is ~p~n", [Acc]);
walk([{cellref, {offset, X}, {offset, Y}, _, _} = H | T], Ref, Params, Acc, Site, Path) ->
	io:format("---------walk cellref---------~n"),
	walk([walk_helper(H, Ref, Params, Site, Path) | T], Ref, Params, Acc, Site, Path);
	
	
%TODO
%	-translate range to list of cell names ("b1:b3" = ["b1","b2","b3"])
%	-update function contains, so it can take two lists as argument and 
%	 return true if list1 is a sublist of list2
%	-then walk through the list of cells
walk([{rangeref, finite, _, {{offset, StartX}, {offset, StartY}}, {{offset, StopX}, {offset, StopY}}, _, _, RangeString}], Ref, Params, Acc, Site, Path)	->
	io:format("TODO: Rangeref is ~p~n", [RangeString]);
walk([H | T], Ref, Params, Acc, Site, Path)	when is_list(H)	->
	io:format("walk, head is a list: ~p~n", [H]),
	walk(T, Ref, Params, [walk(H, Ref, Params, [], Site, Path) | Acc], Site, Path);
walk([H | T], Ref, Params, Acc, Site, Path) ->
	io:format("skipped: ~p~n",[H]),
    walk(T, Ref, Params, [H | Acc], Site, Path).


%if in a AST there is a reference to a cell from outside the range this function is called,
%it takes the cell's reference and retreives a AST which the cell holds
walk_helper(NewReference, Ref, Params, Site, Path)	->
	io:format("---------walk helper---------~n"),
	{cellref, {offset, X}, {offset, Y}, _, _} = NewReference,
	#refX{obj = {cell, {OldX, OldY}}} = Ref,
	NewX = OldX + X,
	NewY = OldY + Y,
    NewObj = hn_util:obj_to_ref({cell, {NewX, NewY}}),
	io:format("NewObj is ~p~n", [NewObj]),
	NewRetRef = #refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(NewObj)},
    io:format("NewRetRef is ~p~n", [NewRetRef]),
    case contains(Params, NewObj) of
		true	-> 
			io:format("Cell is in input range: ~p~n",[NewReference]),
			NewReference;
		
		
		false	-> 
			io:format("--------------walk_helper false--------------~n"),
			[{_, AST2}] = new_db_api:read_attribute(NewRetRef, "__ast"),
			io:format("AST2 is ~p~n", [AST2]),
			AST2
	end,
			io:format("++++++++++++++++++++++++walk helper end~n").

check_off_page([], _Path) -> valid;
check_off_page([{cellref, _, _, P, _} | T], Path) ->
    case muin_util:walk_path(Path, P) of
        Path -> check_off_page(T, Path);
        _    -> invalid
    end;
check_off_page([_H | T], Path) ->
    check_off_page(T, Path).


%return true if list contains Element, false otherwie
contains([], Element)	->
	false;
contains([Element | T], Element)	->
	true;
contains([_H | T], Element)	->
	contains(T, Element).
