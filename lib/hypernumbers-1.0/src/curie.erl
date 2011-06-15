%%% @author    Gordon Guthrie
%%% @author    Jakub Chlanda
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%		Implements famoust paper "A User-Centred Approach to Functions in Excel"
%%%		by Simon Peyton Jones, Alan Blackwell and Margaret Burnett
%%% @end
%%% Created :  15 Jun 2011

-module(curie).

-include("spriki.hrl").

%-compile(export_all).
-export([
         build_fun/4,
         get_cells_ast/3
        ]).



%%TODO

%in muin:run_code(Pcode, #muin_rti{site=Site, path=Path,
                          %~ col=Col,   row=Row,
                          %~ array_context=AryCtx,
                          %~ auth_req=AuthReq})
%~ Pcode is ['/',
          %~ {cellref,{offset,0},{offset,-3},"./","B7"},
          %~ {cellref,{offset,0},{offset,-2},"./","B8"}]
%~ #muin_rti is {muin_rti,"http://hypernumbers.dev:9000", ["function"], 2,11,false,nil}




%%curie:build_fun("http://hypernumbers.dev:9000", ["function"], "b8", ["b1", "b2", "b3"]).
build_fun(Site, Path, ReturnRef, ParamList) ->

	My_AST = get_cells_ast(ReturnRef, Site, Path),
	case is_list(My_AST) of
		false	-> 
				io:format("~n---------Initial AST is~n~p~n~n", [My_AST]),
			    io:format("~n---------Final AST is~n"),
			    My_AST;
		true	->
				case My_AST of
					[]		-> {error, no_ast_in_final_result};
					_AST	->
						RetRef = #refX{site = Site, path = Path,
									   obj = hn_util:parse_ref(ReturnRef)},
						io:format("~n---------Initial AST is~n~p~n~n", [_AST]),
						%params in upper case, easier to compare
						ParamListUpper = lists:map(fun string:to_upper/1, ParamList),
						%no cells from different workbooks are allowed
						%check return cell
						case check_off_page(_AST, Path) of
							invalid -> {error, off_page_reference};
							valid   ->
								case check_input_off_reference(ParamList, Site, Path) of
										invalid -> {error, off_page_reference};
										valid   -> build_fun2(RetRef, _AST, ParamListUpper, Site, Path)
								end
						end
			   end
	end.
	

build_fun2(RetRef, AST, ParamList, Site, Path) ->
	io:format("~n---------Final AST is~n"),
	walk(AST, RetRef, ParamList, [], Site, Path).
	
    
%main function of the program, takes AST and walks down it. 
%returns new AST which only contains references to input cells.
walk(AST, RetRef, ParamList, [], Site, Path)	-> walk2(AST, RetRef, ParamList, [], Site, Path, RetRef).


%added final result cell to make global offset calculation possible
walk2([], _, _, Acc,_ ,_, _) ->
	lists:reverse(Acc);
walk2([{cellref, _OffsetX, _OffsetY, _Path, Cell} = H | T], Ref, Params, Acc, Site, Path, _FinalRetRef) ->
	case contains(Params, Cell) of
		true	->
			walk2(T, Ref, Params, [H | Acc], Site, Path, _FinalRetRef);
		false	->
			walk2([walk_helper_cellref(H, Ref, Params, Site, Path) | T], walk_new_ret_ref(H, Site), Params, Acc, Site, Path, _FinalRetRef)
	end;
%Translates range to list of cell names ("b1:b3" = ["b1","b2","b3"])
walk2([{rangeref, finite, _, {{offset, StartX}, {offset, StartY}}, {{offset, StopX}, {offset, StopY}}, _, _, _RangeString} | T], Ref, Params, Acc, Site, Path, FinalRetRef)	->
	walk2([walk_helper_rangeref(Ref, FinalRetRef,hn_util:range_to_list(#refX{site = Site, path = Path,obj = {range, {StartX, StartY, StopX, StopY}}})) |T], Ref, Params, Acc, Site, Path, FinalRetRef);	
walk2([H | T], Ref, Params, Acc, Site, Path, _FinalRetRef)	when is_list(H)	->
	walk2(T, Ref, Params, [walk2(H, Ref, Params, [], Site, Path, _FinalRetRef) | Acc], Site, Path, _FinalRetRef);
walk2([H | T], Ref, Params, Acc, Site, Path, _FinalRetRef) ->
	walk2(T, Ref, Params, [H | Acc], Site, Path, _FinalRetRef).


%used when AST contains a reference to a cell not in the input range,
%walk_new_ret_ref(NewReference, Site)
walk_new_ret_ref({cellref, _OffsetX, _OffsetY, Path, Cell}, Site)	->
	%calculate new refX
	#refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(Cell)}.
   
                
walk_helper_rangeref(CurrentRef, FinalRetRef, List)	->
	{_, _, _, _, {cell, {ReturnX, ReturnY}}} = FinalRetRef,
	{_, _, _, _, {cell, {CurrentX, CurrentY}}} = CurrentRef,
	DeltaX = CurrentX - ReturnX, 
	DeltaY = CurrentY - ReturnY,
	update_rangeref_offset(FinalRetRef, DeltaX, DeltaY, List, ReturnX, ReturnY, []).


%keep track of the offset
update_rangeref_offset(FinalRetRef, DeltaX, DeltaY, [{refX, _A, _B, _C, {cell,{X,Y}}} | T], ReturnX, ReturnY, Acc)	->
	update_rangeref_offset(FinalRetRef, DeltaX, DeltaY, T, ReturnX, ReturnY, [refX_to_cellref(FinalRetRef, {refX, _A, _B, _C, {cell,{X + DeltaX,Y + DeltaY}}}) |Acc]);
update_rangeref_offset(_FinalRetRef, _DeltaX, _DeltaY, [], _ReturnX, _ReturnY, Acc)	->	
	Acc.


%if in an AST there is a reference to a cell from outside of the input range this function is called,
%it takes the cell's reference and retreives an AST which the cell holds
walk_helper_cellref(NewReference, _Ref, Params, Site, Path)	->
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
				invalid -> {error, off_page_reference};
				valid   -> 
					update_cellref_offset(AST, OffsetX, OffsetY, [])
			end
	end.


%get cell's name from its coordinates
%{refX,"http://hypernumbers.dev:9000", undefined, ["function"], {cell,{0,-5}}}
%{cellref,{offset,0},{offset,-7},"./","B1"},	
%refX_to_cellref(FinalRetRef, CurrentCell)
refX_to_cellref({refX, _, _, _, {cell, {FinalX, FinalY}}}, {refX, _, _, _, {cell, {X, Y}}})	->
		{cellref, {offset, X}, {offset, Y}, "./", hn_util:obj_to_ref({cell, {X + FinalX, Y + FinalY}})}.


%when hopping from cell to cell (going down the AST) offset has to be updated,
%to make sure it always points to the final result cell
update_cellref_offset([], _, _, Acc)	-> 
	lists:reverse(Acc);
update_cellref_offset([H | T], X, Y, Acc) when is_list(H)	->
	update_cellref_offset(T, X, Y, [ update_cellref_offset(H , X, Y, []) | Acc]);
update_cellref_offset([{cellref, {offset, OldX}, {offset, OldY}, _A, _B} | T], X, Y, Acc)	->
	update_cellref_offset(T, X, Y, [{cellref, {offset, OldX + X}, {offset, OldY + Y}, _A, _B} | Acc]);
update_cellref_offset([H | T], X, Y , Acc)	-> 	update_cellref_offset(T, X, Y , [H | Acc]).


%check if a cell points away from a specified worksheet
%check_off_page({cellref, {offset,-1}, {offset,-57}, "/page1/","/page1/a1"}, ["function"])
check_off_page([], _Path) -> valid;
check_off_page([{cellref, _, _, P, _} | T], Path) ->
	case muin_util:walk_path(Path, P) of
        Path -> 
			check_off_page(T, Path);
        _    -> 
			invalid
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
	

%check if specified input list contains off page references
check_input_off_reference([], _Site, _Path) -> valid;
check_input_off_reference([H | T], Site, Path) ->
    case check_off_page([get_cells_ast(H, Site, Path)], Path) of
        invalid -> invalid;
        valid -> check_input_off_reference(T, Site, Path)
    end.


%return cell's abstract syntax tree (not a tuple but just a list)
get_cells_ast(H, Site, Path)	->
	Cell = #refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(H)},
    List = new_db_api:read_ref(Cell),
    case List of 
		[]	-> [];
		_List	->
			[{_address, Properties}] = _List,
			Tuple = lists:keyfind("__ast", 1, Properties),
			case Tuple of 
				false			-> [];
				{"__ast", AST}	-> AST;
				[]				-> []
			end
	end.
