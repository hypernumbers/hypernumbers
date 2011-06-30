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
-include("hypernumbers.hrl").
-include("hn_mochi.hrl").

-compile(export_all).
%~ -export([
         %~ build_fun/4,
         %~ get_cells_ast/3
        %~ ]).



%%TODO
	%~ fix get_pages_json
	%~ start working on updating defined functions
%%TODO

create_user_fn(Site, Page, Name, Description, OutputValue, ListOfParameters, ListOfParameterNames, ListOfParameterDescriptions)	->
	AST = build_fun(Site, Page, OutputValue, ListOfParameters),
	JSON = get_page_s_json(Site, Page, OutputValue),
	JSON_for_WIZARD = make_json_for_fn_wizard(Name, Description, ListOfParameterNames, ListOfParameterDescriptions),
	io:format("----------------------~ncreate_user_fn~n----------------------~nAST is:~n~p~nJSON is:~n~p~nJSON_for_WIZARD is:~n~p~n----------------------~n", [AST, JSON, JSON_for_WIZARD]).

%	make_json_for_fn_wizard("user.normalise", "normalised average", ["factor", "range"], ["normalisation factor", "range of cells to get average from"]).
make_json_for_fn_wizard(Name, Description, ListOfParameterNames, ListOfParameterDescriptions)	->
	Parameters = get_parameters(ListOfParameterNames, ListOfParameterDescriptions, []),
	Entry = 
			{struct,
					[
						{"fn",Name},
						{"category","User Defined"},
						{"desc", Description},
						{"experimental",false},
						{"includable",true},
						{"inexcel",true},
						{"resize",false},
						{"wizardready",true},
						{"link", "REQUIRES A LINK TO DOC PAGE OR HAVE TO GENERATE IT AUTOMATICALLY"},
						{"args", {array, Parameters}}
					]
			},
	mochijson:encode(Entry).

get_parameters([], [], Parameters)	-> 
	lists:reverse(Parameters);
	
get_parameters([H | T], [H2 | T2], Parameters)	-> 
	Parameter = {struct, [{"name", H}, {"desc", H2}, {"type", "finite"}]},
	get_parameters(T, T2, [Parameter | Parameters]).
		

%~ get_page_s_json("http://hypernumbers.dev:9000", ["page1"], "j10").
get_page_s_json(Site, Page, Cell)	->
	Ref = #refX{site = Site, path = Page, obj = hn_util:parse_ref(Cell)},
	io:format("Ref is: ~p~n", [Ref]),
	Encoder = mochijson:encoder([{input_encoding, utf8}]),
	io:format("Encoder is: ~p~n", [Encoder]),
	Encoded_Page = Encoder(hn_mochi:page_attributes(Ref#refX{path = Page}, #env{})),
	io:format("Encoded_Page is: ~p~n", [Encoded_Page]),
	EtfDest = Page,
	io:format("EtfDest is: ~p~n", [EtfDest]),
	
	%~ dump_page(EtfDest, Encoder, Ref, Path)
	Page_s_json = hn_archive:dump_page(EtfDest, Encoder, Ref, Encoded_Page),
	io:format("Page's json is: ~p~n", [Page_s_json]),
	Page_s_json.
	
	



%%curie:build_fun("http://hypernumbers.dev:9000", ["function"], "b8", ["b1", "b2", "b3"]).
build_fun(Site, Page, OutputValue, ListOfParameters) ->

	My_AST = get_cells_ast(OutputValue, Site, Page),
	case is_list(My_AST) of
		false	-> 
				io:format("~n---------Initial AST is~n~p~n~n", [My_AST]),
			    io:format("~n---------Final AST is~n"),
			    My_AST;
		true	->
				case My_AST of
					[]		-> {error, no_ast_in_final_result};
					_My_AST	->
						RetRef = #refX{site = Site, path = Page,
									   obj = hn_util:parse_ref(OutputValue)},
						io:format("~n---------Initial AST is~n~p~n~n", [_My_AST]),
						%params in upper case, easier to compare
						ParamListUpper = lists:map(fun string:to_upper/1, ListOfParameters),
						%no cells from different workbooks are allowed
						%check return cell
						case check_off_page(_My_AST, Page) of
							invalid -> {error, off_page_reference};
							valid   ->
								case check_input_off_reference(ListOfParameters, Site, Page) of
										invalid -> {error, off_page_reference};
										valid   -> build_fun2(RetRef, _My_AST, ParamListUpper, Site, Page)
								end
						end
			   end
	end.
	

build_fun2(RetRef, AST, ListOfParameters, Site, Page) ->
	io:format("~n---------Final AST is~n"),
	walk(AST, RetRef, ListOfParameters, [], Site, Page).
	
    
%main function of the program, takes AST and walks down it. 
%returns new AST which only contains references to input cells.
walk(AST, RetRef, ListOfParameters, [], Site, Page)	-> walk2(AST, RetRef, ListOfParameters, [], Site, Page, RetRef).


%added final result cell to make global offset calculation possible
walk2([], _, _, Acc,_ ,_, _) ->
	lists:reverse(Acc);
walk2([{cellref, _OffsetX, _OffsetY, _Page, Cell} = H | T], Ref, Params, Acc, Site, Page, _FinalRetRef) ->
	case contains(Params, Cell) of
		true	->
			walk2(T, Ref, Params, [H | Acc], Site, Page, _FinalRetRef);
		false	->
			walk2([walk_helper_cellref(H, Ref, Params, Site, Page) | T], walk_new_ret_ref(H, Site), Params, Acc, Site, Page, _FinalRetRef)
	end;
%Translates range to list of cell names ("b1:b3" = ["b1","b2","b3"])
walk2([{rangeref, finite, _, {{offset, StartX}, {offset, StartY}}, {{offset, StopX}, {offset, StopY}}, _, _, _RangeString} | T], Ref, Params, Acc, Site, Page, FinalRetRef)	->
	walk2([walk_helper_rangeref(Ref, FinalRetRef,hn_util:range_to_list(#refX{site = Site, path = Page,obj = {range, {StartX, StartY, StopX, StopY}}})) |T], Ref, Params, Acc, Site, Page, FinalRetRef);	
walk2([H | T], Ref, Params, Acc, Site, Page, _FinalRetRef)	when is_list(H)	->
	walk2(T, Ref, Params, [walk2(H, Ref, Params, [], Site, Page, _FinalRetRef) | Acc], Site, Page, _FinalRetRef);
walk2([H | T], Ref, Params, Acc, Site, Page, _FinalRetRef) ->
	walk2(T, Ref, Params, [H | Acc], Site, Page, _FinalRetRef).


%used when AST contains a reference to a cell not in the input range,
%walk_new_ret_ref(NewReference, Site)
walk_new_ret_ref({cellref, _OffsetX, _OffsetY, Page, Cell}, Site)	->
	%calculate new refX
	#refX{site = Site, path = Page,
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
walk_helper_cellref(NewReference, _Ref, Params, Site, Page)	->
	%get the cell name and store it in Cell
	{cellref, {offset, OffsetX}, {offset, OffsetY}, _Page, Cell} = NewReference,
	%calculate new refX
	NewRetRef = #refX{site = Site, path = Page,
                   obj = hn_util:parse_ref(Cell)},
    case contains(Params, Cell) of
		true	-> 
			NewReference;
		false	->
			[{_, AST}] = new_db_api:read_attribute(NewRetRef, "__ast"),
			case check_off_page(AST, Page) of
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
check_off_page([], _Page) -> valid;
check_off_page([{cellref, _, _, P, _} | T], Page) ->
	case muin_util:walk_path(Page, P) of
        Page -> 
			check_off_page(T, Page);
        _    -> 
			invalid
    end;
check_off_page([_H | T], Page) ->
	check_off_page(T, Page).


%return true if list contains Element, false otherwise
contains([], _Element)	->
	false;
contains([Element | _T], Element)	->
	true;
contains([_H | T], Element)	->
	contains(T, Element).
	

%check if specified input list contains off page references
check_input_off_reference([], _Site, _Page) -> valid;
check_input_off_reference([H | T], Site, Page) ->
    case check_off_page([get_cells_ast(H, Site, Page)], Page) of
        invalid -> invalid;
        valid -> check_input_off_reference(T, Site, Page)
    end.


%return cell's abstract syntax tree (not a tuple but just a list)
get_cells_ast(H, Site, Page)	->
	Cell = #refX{site = Site, path = Page,
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
