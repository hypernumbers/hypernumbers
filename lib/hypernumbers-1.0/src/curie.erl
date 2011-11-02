%%% @author    Gordon Guthrie
%%% @author    Jakub Chlanda
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%		Implements famous paper "A User-Centred Approach to Functions in Excel"
%%%		by Simon Peyton Jones, Alan Blackwell and Margaret Burnett
%%% @end
%%% Created :  15 Jun 2011

-module(curie).

-include("spriki.hrl").
-include("hn_mochi.hrl").
-include("muin_records.hrl").
-export([
         create_user_fn/6
        ]).
%% TODO
%% update cellref goes crazy if in cell there is =2. remember to change it
%% when geting rid of cellref in a AST, just swap it for 2
%% TODO

create_user_fn(Site, FunName, Page, FunDesc, OutputVal, ParamsArr)	->
    {array, Params} = ParamsArr,
    {LParamNames, LParamDescs, LParamVals} = get_param_data(Params, [], [], []),
    Page_Array = re:split(Page, "/", [{return,list}]),
    Page_Array2 = refine_string_list(Page_Array, []),
    create_user_fn2(Site, Page_Array2, FunName, FunDesc, OutputVal,
                    LParamNames, LParamDescs, LParamVals).

get_param_data([], Names, Descriptions, Values)	->
    {lists:reverse(Names), lists:reverse(Descriptions), lists:reverse(Values)};
get_param_data([H | T], Names, Descriptions, Values)	->
    {struct, Args} = H,
    Nm	= kfind("name", Args),
    Dsc	= kfind("description", Args),
    Val	= kfind("value", Args),
    get_param_data(T, [Nm | Names], [Dsc | Descriptions], [Val | Values]).

% curie:create_user_fn2("http://hypernumbers.dev:9000", ["page1"],
% "b1", "b2", "b8", ["b5", "b6"], ["c5", "c6"], ["f5", "f6"]).
create_user_fn2(Site, Page, Name, Description, OutputValue, LParamNames,
                LParamDescs, LParamVals)	->
    FnName = get_cell_value(Name, Site, Page),
    AST = build_fun(Site, Page, OutputValue, LParamVals),
    case AST of
        {error, Message} ->
            io:format("error, ~p~n", [Message]),
            ok;
        _	 ->
            PageJSON = get_page_json(Site, Page),
            WizTemplate = make_template(Site, Page, Name, Description,
                                        LParamNames, LParamDescs),
            WizJSON = lists:flatten(mochijson:encode(WizTemplate)),
            DB_Entry = #user_fns{name = FnName, ast = AST,
                                 pagejson = PageJSON, wizardjson = WizJSON},
            new_db_api:write_user_fn(Site, DB_Entry)
        end,
    {ok, "create_user_fn"}.

% curie:make_template("http://hypernumbers.dev:9000", ["page1"],
% "b1", "b2", ["b5", "b6"], ["c5", "c6"]).
% curie:make_template("http://hypernumbers.dev:9000", ["page2"],
% "b1", "b2", ["b5"], ["c5"]).
make_template(Site, Page, Name, Description, LParamNames, LParamDescs)	->
    Params = get_parameters(Site, Page, LParamNames, LParamDescs, []),
    {array,
     [{struct,
       [
        {"fn",get_cell_value(Name, Site, Page)},
        {"category","User Defined"},
        {"desc", get_cell_value(Description, Site, Page)},
        {"experimental",false},
        {"includable",true},
        {"inexcel",true},
        {"resize",false},
        {"wizardready",true},
        {"link", "REQUIRES A LINK TO DOC PAGE OR HAVE TO GENERATE "
         ++ "IT AUTOMATICALLY"},
        {"args", {array, Params}}
       ]
			}]
    }.

get_parameters(_Site, _Page, [], [], Params)	->
    lists:reverse(Params);
get_parameters(Site, Page, [H | T], [H2 | T2], Params)	->
    Parameter = {struct, [{"name", get_cell_value(H, Site, Page)},
                          {"desc", get_cell_value(H2, Site, Page)},
                          {"type", "finite"}]},
    get_parameters(Site, Page, T, T2, [Parameter | Params]).

% curie:get_page_json("http://hypernumbers.dev:9000", ["page1"]).
% curie:get_page_json("http://hypernumbers.dev:9000", ["page2"]).
get_page_json(Site, Path) ->
    Ref = hn_util:url_to_refX(Site),
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    Page = Encoder(hn_mochi:page_attributes(Ref#refX{path = Path}, #env{})),
    io_lib:format("~s", [lists:flatten(Page)]).

read_user_fn(Site, Name)	->
    Result = new_db_api:read_user_fn(Site, Name),
    case Result of
        []	->	{error, no_entry_in_DB};
        _	  ->	{ok, Result}
    end.

delete_user_fn(Site, Name)	->
    new_db_api:delete_user_fn(Site, Name),
    {ok, "delete_user_fn"}.

% curie:build_fun("http://hypernumbers.dev:9000", ["function"],
% "b8", ["b1", "b2", "b3"]).
build_fun(Site, Page, OutputVal, ListOfParams) ->
    My_AST = get_cell_ast(OutputVal, Site, Page),
    case is_list(My_AST) of
        false	->
            My_AST;
        true	->
            case My_AST of
                []		-> {error, no_ast_in_final_result};
                _My_AST	->
                    RetRef = #refX{site = Site, type = url, path = Page,
                                   obj = hn_util:parse_ref(OutputVal)},
                    % params in upper case, easier to compare
                    UParamList = lists:map(fun string:to_upper/1, ListOfParams),
                    % no cells from different workbooks are allowed
                    % check return cell
                    case check_off_page(_My_AST, Page) of
                        invalid -> {error, off_page_reference};
                        valid   ->
                            case chk_inp_off_ref(ListOfParams, Site, Page) of
                                invalid ->	{error, off_page_reference};
                                valid   ->
                                    AST = build_fun2(RetRef, _My_AST,
                                                     UParamList, Site, Page),
                                    case check_for_not_in_param_error(AST) of
                                        {error, Msg} ->	{error, Msg};
                                        _					   ->	AST
                                    end
                            end
                    end
            end
    end.

build_fun2(RetRef, AST, ListOfParams, Site, Page) ->
    walk(AST, RetRef, ListOfParams, [], Site, Page).

% main function of the program, takes AST and walks down it.
% returns new AST which only contains references to input cells.
walk(AST, RetRef, ListOfParams, [], Site, Page)	->
    walk2(AST, RetRef, ListOfParams, [], Site, Page, RetRef).

% added final result cell to make global offset calculation possible
walk2([], _, _, Acc,_ ,_, _) ->
    lists:reverse(Acc);
walk2([#cellref{text = Cell} = H | T], Ref, Params, Acc, Site,
      Pg, _FinalRetRef) ->
    case contains(Params, string:to_upper(Cell)) of
        true	->
            walk2(T, Ref, Params, [H | Acc], Site, Pg, _FinalRetRef);
        false	->
            walk2([walk_helper_cellref(H, Ref, Params, Site, Pg) | T],
                  walk_new_ret_ref(H, Site), Params, Acc, Site, Pg,
                  _FinalRetRef)
    end;
% Translates range to list of cell names ("b1:b3" = ["b1","b2","b3"])
walk2([#rangeref{type = finite, tl = {{offset, StartX}, {offset, StartY}},
                 br = {{offset, StopX}, {offset, StopY}}} | T],
      Ref, Params, Acc, Site, P, FinalRetRef)	->
    Obj = {range, {StartX, StartY, StopX, StopY}},
    RangeRef = hn_util:range_to_list(#refX{site = Site, path = P, obj = Obj}),
    CellRefs = walk_helper_rangeref(Ref, FinalRetRef, RangeRef),
    Merged_AST = lists:merge(CellRefs, T),
    walk2(Merged_AST, Ref, Params, Acc, Site, P, FinalRetRef);
walk2([H | T], Ref, Params, Acc, Site, P, _FinalRetRef)	when is_list(H)	->
    NewAcc = [walk2(H, Ref, Params, [], Site, P, _FinalRetRef) | Acc],
    walk2(T, Ref, Params, NewAcc, Site, P, _FinalRetRef);
walk2([H | T], Ref, Params, Acc, Site, P, _FinalRetRef) ->
    walk2(T, Ref, Params, [H | Acc], Site, P, _FinalRetRef).

% used when AST contains a reference to a cell not in the input range,
% walk_new_ret_ref(NewReference, Site)
walk_new_ret_ref(#cellref{path = Path, text = Cell}, Site)	->
    % calculate new refX
    #refX{site = Site, type = url, path = Path, obj = hn_util:parse_ref(Cell)}.

% FIXUP - think these are RefX's coming in
walk_helper_rangeref(CurRef, FinalRetRef, List)	->
    {_, _, _, _, {cell, {RetX, RetY}}} = FinalRetRef,
    {_, _, _, _, {cell, {CurX, CurY}}} = CurRef,
    DeltaX = CurX - RetX,
    DeltaY = CurY - RetY,
    update_rangeref_offset(FinalRetRef, DeltaX, DeltaY, List, RetX, RetY, []).

% keep track of the offset
update_rangeref_offset(FinalRetRef, DeltaX, DeltaY,
                       [#refX{obj = {cell,{X,Y}}} = RefX | T],
                       RetX, RetY, Acc)	->
    NewRefX = RefX#refX{obj = {cell, {X + DeltaX, Y + DeltaY}}},
    NewAcc = [refX_to_cellref(FinalRetRef, NewRefX) | Acc],
    update_rangeref_offset(FinalRetRef, DeltaX, DeltaY, T, RetX, RetY, NewAcc);
update_rangeref_offset(_FinalRetRef, _DeltaX, _DeltaY, [], _RetX, _RetY, Acc)	->
    Acc.

% if in an AST there is a reference to a cell from outside of the input
% range this function is called,
% it takes the cell's reference and retreives an AST which the cell holds
walk_helper_cellref(NewRef, _Ref, Params, Site, Path)	->
    % get the cell name and store it in Cell
    #cellref{col = {offset, OffsetX},row = {offset, OffsetY},
             text = Cell} = NewRef,
    case contains(Params, Cell) of
        true	->
            NewRef;
        false	->
            AST = get_cell_ast(Cell, Site, Path),
            case check_off_page(AST, Path) of
                invalid -> {error, off_page_reference};
                valid   ->
                    case update_cellref_offset(AST, OffsetX, OffsetY, []) of
                        {error, Msg} -> {error, Msg};
                        Success      -> Success
                    end
            end
    end.

% get cell's name from its coordinates
% {refX,"http://hypernumbers.dev:9000", undefined, ["function"],
% {cell, {0, -5}}} {cellref, {offset,0}, {offset,-7}, "./", "B1"},
% refX_to_cellref(FinalRetRef, CurrentCell)
refX_to_cellref(#refX{obj = {cell, {FinalX, FinalY}}},
                #refX{obj = {cell, {X, Y}}})	->
    Ref = hn_util:obj_to_ref({cell, {X + FinalX, Y + FinalY}}),
		#cellref{col = {offset, X}, row = {offset, Y}, path = "./", text = Ref}.

% when hopping from cell to cell (going down the AST) offset has to be updated,
% to make sure it always points to the final result cell
update_cellref_offset([], _, _, Acc)	->
    case Acc of
        [] -> {error, cell_not_in_param};
        _	 -> lists:reverse(Acc)
    end;
update_cellref_offset([H | T], X, Y, Acc) when is_list(H)	->
    NewAcc = [ update_cellref_offset(H , X, Y, []) | Acc],
    update_cellref_offset(T, X, Y, NewAcc);
update_cellref_offset([#cellref{col = {offset, OldX},
                                row = {offset, OldY}} = Ref | T],
                       X, Y, Acc)	->
    NewAcc = [Ref#cellref{col = {offset, OldX + X},
                          row = {offset, OldY + Y}} | Acc],
    update_cellref_offset(T, X, Y, NewAcc);
update_cellref_offset([H | T], X, Y , Acc)	->
    update_cellref_offset(T, X, Y , [H | Acc]);
update_cellref_offset(#cellref{col = {offset, OldX},
                               row = {offset, OldY}} = Ref,
                      X, Y, _Acc)	->
    Ref#cellref{col = {offset, OldX + X}, row = {offset, OldY + Y}}.

% check if a cell points away from a specified worksheet
% check_off_page({cellref, {offset, -1}, {offset, -57}, "/page1/",
% "/page1/a1"}, ["function"])
check_off_page([], _Path) ->
    valid;
check_off_page(Number, _Path) when not(is_list(Number))	->
    valid;
check_off_page([{cellref, _, _, P, _} | T], Path) ->
    case muin_util:walk_path(Path, P) of
        Path -> check_off_page(T, Path);
        _    -> invalid
    end;
check_off_page([_H | T], Path) ->
    check_off_page(T, Path).

% return true if list contains Element, false otherwise
contains([], _Element)	->
    false;
contains([Element | _T], Element)	->
    true;
contains([_H | T], Element)	->
    contains(T, Element).

% check if specified input list contains off page references
chk_inp_off_ref([], _Site, _Path) ->
    valid;
chk_inp_off_ref([H | T], Site, Path) ->
    case check_off_page([get_cell_ast(H, Site, Path)], Path) of
        invalid -> invalid;
        valid -> chk_inp_off_ref(T, Site, Path)
    end.

% return cell's abstract syntax tree (not a tuple but just a list)
get_cell_ast(H, Site, Path)	->
    Cell = #refX{site = Site, type = url, path = Path,
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

% get_cell_value("a2", "http://hypernumbers.dev:9000", ["page1"]).
get_cell_value(Cell, Site, Path)	->
    Ref = #refX{site = Site, type = url, path = Path,
                obj = hn_util:parse_ref(Cell)},
    List = new_db_api:read_ref(Ref),
    case List of
        []	->
            [];
        _List	->
            [{_address, Properties}] = _List,
            Tuple = lists:keyfind("value", 1, Properties),
            case Tuple of
                false	            -> [];
                {"value", Value}	-> Value;
                []			          -> []
            end
    end.

%curie:read_cell_attrs("a1", "http://hypernumbers.dev:9000", ["page3"]).
read_cell_attrs(Cell, Site, Path)	->
    Ref = #refX{site = Site, type = url, path = Path,
                obj = hn_util:parse_ref(Cell)},
    Attributes = new_db_api:read_ref(Ref),
    io:format("Attributes are: ~p~n", [Attributes]).

% if string starts with / regexp:split/2 wuould return [] as its representation,
% this module gets rid of empty lists in a result list.
refine_string_list([H | T], Result)	->
    case H of
        []	-> refine_string_list(T, Result);
        _	  -> refine_string_list(T, [H | Result])
    end;
refine_string_list([], Result)	->
    lists:reverse(Result).

-spec kfind(string(), list()) -> any().
kfind(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.

check_for_not_in_param_error([]) ->
    ok;
check_for_not_in_param_error([{error, Msg} | _T])	->
    {error, Msg};
check_for_not_in_param_error({error, Msg}) ->
    {error, Msg};
check_for_not_in_param_error([H | T]) when is_list(H)	->
    check_for_not_in_param_error([check_for_not_in_param_error(H) | T]);
check_for_not_in_param_error([_H | T]) ->
    check_for_not_in_param_error(T).
