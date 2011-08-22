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
-include("hn_mochi.hrl").

-compile(export_all).
%~ -export([
      %~ 
        %~ ]).

%%TODO
	%~ update cellref goes crazy if in cell there is =2. remember to change it when geting rid of cellref in a AST, just swap it for 2
%%TODO

create_user_fn(Site, Function_Name, Page, Function_Description, Output_Value, Parameters_Array)	->
	{array, Parameters} = Parameters_Array,
	{ListOfParameterNames, ListOfParameterDescriptions, ListOfParameterValues} = get_parameters_data(Parameters, [], [], []),
	Page_Array = re:split(Page, "/", [{return,list}]),
	Page_Array2 = refine_string_list(Page_Array, []),
	%~ io:format("Site is: ~p~nPage is: ~p~nFunction_Name is: ~p~nFunction_Description is: ~p~nFunction_Output_Value is: ~p~nListOfParameterNames is: ~p~nListOfParameterDescriptions is: ~p~nListOfParameterValues is: ~p~n", [Site, Page_Array2, Function_Name, Function_Description, Function_Output_Value, ListOfParameterNames, ListOfParameterDescriptions, ListOfParameterValues]),
	
	create_user_fn2(Site, Page_Array2, Function_Name, Function_Description, Output_Value, ListOfParameterNames, ListOfParameterDescriptions, ListOfParameterValues).
	
	
get_parameters_data([], Names, Descriptions, Values)	->
	{lists:reverse(Names), lists:reverse(Descriptions), lists:reverse(Values)};

get_parameters_data([H | T], Names, Descriptions, Values)	->
	{struct, Args} = H,
	Nm	= kfind("name", Args),
	Dsc	= kfind("description", Args),
	Val	= kfind("value", Args),
	get_parameters_data(T, [Nm | Names], [Dsc | Descriptions], [Val | Values]).
	
%~ curie:create_user_fn2("http://hypernumbers.dev:9000", ["page1"],"b1", "b2", "b8", ["b5", "b6"], ["c5", "c6"], ["f5", "f6"]).
create_user_fn2(Site, Page, Name, Description, OutputValue, ListOfParameterNames, ListOfParameterDescriptions, ListOfParameterValues)	->
    FUNCTION_NAME = get_cell_s_value(Name, Site, Page),
    AST = build_fun(Site, Page, OutputValue, ListOfParameterValues),
	case AST of
		{error, Message}	-> 	io:format("error, ~p~n", [Message]),
								{error, Message};
		_					->
			PAGE_S_JSON = get_page_s_json(Site, Page),
			WIZARD_TEMPLATE = make_template_for_fn_wizard(Site, Page, Name,
														  Description,
														  ListOfParameterNames,
														  ListOfParameterDescriptions),
			WIZARD_S_JSON = lists:flatten(mochijson:encode(WIZARD_TEMPLATE)),
			DB_Entry = #user_fns{name = FUNCTION_NAME, ast = AST, pagejson = PAGE_S_JSON, wizardjson = WIZARD_S_JSON},
			new_db_api:write_user_fn(Site, DB_Entry),
			io:format("ok~n~p~n", [DB_Entry])
	end,
	{ok, "create_user_fn"}.
			
%~ curie:make_template_for_fn_wizard("http://hypernumbers.dev:9000", ["page1"],"b1", "b2", ["b5", "b6"], ["c5", "c6"]).
%~ curie:make_template_for_fn_wizard("http://hypernumbers.dev:9000", ["page2"],"b1", "b2", ["b5"], ["c5"]).
make_template_for_fn_wizard(Site, Page, Name, Description, ListOfParameterNames, ListOfParameterDescriptions)	->
	Parameters = get_parameters(Site, Page, ListOfParameterNames, ListOfParameterDescriptions, []),
	{array,
			[{struct,
					[
						{"fn",get_cell_s_value(Name, Site, Page)},
						{"category","User Defined"},
						{"desc", get_cell_s_value(Description, Site, Page)},
						{"experimental",false},
						{"includable",true},
						{"inexcel",true},
						{"resize",false},
						{"wizardready",true},
						{"link", "REQUIRES A LINK TO DOC PAGE OR HAVE TO GENERATE IT AUTOMATICALLY"},
						{"args", {array, Parameters}}
					]
			}]
	}.


get_parameters(_Site, _Page, [], [], Parameters)	->
	lists:reverse(Parameters);


get_parameters(Site, Page, [H | T], [H2 | T2], Parameters)	->
	Parameter = {struct, [{"name", get_cell_s_value(H, Site, Page)}, {"desc", get_cell_s_value(H2, Site, Page)}, {"type", "finite"}]},
	get_parameters(Site, Page, T, T2, [Parameter | Parameters]).


%~ curie:get_page_s_json("http://hypernumbers.dev:9000", ["page1"]).
%~ curie:get_page_s_json("http://hypernumbers.dev:9000", ["page2"]).
get_page_s_json(Site, Path) ->
    Ref = hn_util:url_to_refX(Site),
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    Page = Encoder(hn_mochi:page_attributes(Ref#refX{path = Path}, #env{})),
    io_lib:format("~s", [lists:flatten(Page)]).


read_user_fn(Site, Name)	->
	Result = new_db_api:read_user_fn(Site, Name),
	io:format("Read User Fn, Result is: ~p~n", [Result]),
	{ok, "read_user_fn"}.
	
delete_user_fn(Site, Name)	->
	new_db_api:delete_user_fn(Site, Name),
	{ok, "delete_user_fn"}.

%~ curie:build_fun("http://hypernumbers.dev:9000", ["function"], "b8", ["b1", "b2", "b3"]).
build_fun(Site, Page, OutputValue, ListOfParameters) ->

	My_AST = get_cell_s_ast(OutputValue, Site, Page),
	case is_list(My_AST) of
		false	->
				 My_AST;
		true	->
				case My_AST of
					[]		-> {error, no_ast_in_final_result};
					_My_AST	->
						RetRef = #refX{site = Site, path = Page,
									   obj = hn_util:parse_ref(OutputValue)},
						%params in upper case, easier to compare
						ParamListUpper = lists:map(fun string:to_upper/1, ListOfParameters),
						%no cells from different workbooks are allowed
						%check return cell
						case check_off_page(_My_AST, Page) of
							invalid -> {error, off_page_reference};
							valid   ->
								case check_input_off_reference(ListOfParameters, Site, Page) of
										invalid ->	{error, off_page_reference};
										valid   ->	
													AST = build_fun2(RetRef, _My_AST, ParamListUpper, Site, Page),
													case check_for_not_in_param_error(AST) of
														{error, Message}	->	{error, Message};
														_					->	AST
													end
								end
						end
			   end
	end.


build_fun2(RetRef, AST, ListOfParameters, Site, Page) ->
	walk(AST, RetRef, ListOfParameters, [], Site, Page).


%main function of the program, takes AST and walks down it.
%returns new AST which only contains references to input cells.
walk(AST, RetRef, ListOfParameters, [], Site, Page)	-> walk2(AST, RetRef, ListOfParameters, [], Site, Page, RetRef).


%added final result cell to make global offset calculation possible
walk2([], _, _, Acc,_ ,_, _) ->
	lists:reverse(Acc);
walk2([{cellref, _OffsetX, _OffsetY, _Page, Cell} = H | T], Ref, Params, Acc, Site, Page, _FinalRetRef) ->
	case contains(Params, string:to_upper(Cell)) of
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
    case contains(Params, Cell) of
		true	->
			NewReference;
		false	->
			AST = get_cell_s_ast(Cell, Site, Page),
			case check_off_page(AST, Page) of
				invalid -> {error, off_page_reference};
				valid   ->
					case update_cellref_offset(AST, OffsetX, OffsetY, []) of
						{error, Message}	-> {error, Message};
						Succes				-> Succes
					end
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
	case Acc of
		[]	-> {error, cell_not_in_param};
		_	-> lists:reverse(Acc)
	end;
update_cellref_offset([H | T], X, Y, Acc) when is_list(H)	->
	update_cellref_offset(T, X, Y, [ update_cellref_offset(H , X, Y, []) | Acc]);
update_cellref_offset([{cellref, {offset, OldX}, {offset, OldY}, _A, _B} | T], X, Y, Acc)	->
	update_cellref_offset(T, X, Y, [{cellref, {offset, OldX + X}, {offset, OldY + Y}, _A, _B} | Acc]);
update_cellref_offset([H | T], X, Y , Acc)	-> 	update_cellref_offset(T, X, Y , [H | Acc]);
update_cellref_offset({cellref, {offset, OldX}, {offset, OldY}, _A, _B}, X, Y, _Acc)	->
	{cellref, {offset, OldX + X}, {offset, OldY + Y}, _A, _B}.



%check if a cell points away from a specified worksheet
%check_off_page({cellref, {offset,-1}, {offset,-57}, "/page1/","/page1/a1"}, ["function"])
check_off_page([], _Page) -> valid;
check_off_page(Number, _Page) when not(is_list(Number))	-> valid;
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
    case check_off_page([get_cell_s_ast(H, Site, Page)], Page) of
        invalid -> invalid;
        valid -> check_input_off_reference(T, Site, Page)
    end.


%return cell's abstract syntax tree (not a tuple but just a list)
get_cell_s_ast(H, Site, Page)	->
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


%get_cell_s_value("a2", "http://hypernumbers.dev:9000", ["page1"]).
get_cell_s_value(Cell, Site, Page)	->
	Ref = #refX{site = Site, path = Page,
                   obj = hn_util:parse_ref(Cell)},
	List = new_db_api:read_ref(Ref),
    case List of
		[]	-> [];
		_List	->
			[{_address, Properties}] = _List,
			Tuple = lists:keyfind("value", 1, Properties),
			case Tuple of
				false			-> [];
				{"value", Value}	-> Value;
				[]				-> []
			end
	end.
	
	
%curie:read_cell_s_attributes("a1", "http://hypernumbers.dev:9000", ["page3"]).
read_cell_s_attributes(Cell, Site, Page)	->
	Ref = #refX{site = Site, path = Page,
                   obj = hn_util:parse_ref(Cell)},
    Attributes = new_db_api:read_ref(Ref),
	io:format("Attributes are: ~p~n", [Attributes]).

%if string starts with / regexp:split/2 wuould return [] as its representation,
%this module gets rid of empty lists in a result list.
refine_string_list([H | T], Result)	->
	case H of
		[]	-> refine_string_list(T, Result);
		_	-> refine_string_list(T, [H | Result])
	end;
refine_string_list([], Result)	->
	lists:reverse(Result).

-spec kfind(string(), list()) -> any().
kfind(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.

check_for_not_in_param_error([])						-> ok;
check_for_not_in_param_error([{error, Message} | _T])	-> {error, Message};
check_for_not_in_param_error({error, Message})			-> {error, Message};
check_for_not_in_param_error([H | T]) when is_list(H)	-> check_for_not_in_param_error([check_for_not_in_param_error(H) | T]);
check_for_not_in_param_error([_H | T])					-> check_for_not_in_param_error(T).
	
