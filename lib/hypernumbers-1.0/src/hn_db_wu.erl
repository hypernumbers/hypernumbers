%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This is a util function for hn_db_api containing only functions
%%%            that MUST be called from within an mnesia transactions - these
%%%            functions can be considered <em>work units</em> from which
%%%            api transactions can be constructed (hence the name).
%%%            
%%% The module {@link hn_db_api} is the wrapper for calls to this
%%% module.
%%% 
%%% This module <em>PRESUMES THAT PERMISSIONS ARE ALL IN ORDER</em> - 
%%% for instance no function in this module will check if a biccie
%%% is valid - that <em>MUST BE DONE</em> in the api level functions.
%%% 
%%% This module generates two sorts of side-effects:
%%% * changes to the database
%%% * notifications to the front-end
%%% 
%%% The changes to the database are done under mnesia transactions management
%%% (as you would expect). It is necessary for the notifications to the 
%%% front=end also to be under transaction management. This is achieved by 
%%% using the process dictionary in a specific way.
%%% 
%%% To notify the front-end under transaction management, a function in 
%%% this module will call the function 'tell_front_end' with the appropriate
%%% change - the transaction management is applied in the module 
%%% {@link hn_db_api} and is documented there.
%%% 
-module(hn_db_wu).

%% Cell Query Exports
-export([
         get_cell_for_muin/1,
         write_attrs/2, write_attrs/3,
         read_styles/2,
         matching_forms/2,
         read_ref/2, read_ref/3, read_ref_field/3,
         expand_ref/1,
         clear_cells/1, clear_cells/2,
         delete_cells/1,
         shift_cells/4,
         get_children/1,
         get_children_idxs/1,
         copy_cell/4,
         read_page_structure/1,
         read_pages/1,
         idx_to_ref/2,
         ref_to_idx/1,
         ref_to_idx_create/1
        ]).

%% Database transformation functions
-export([
         trans/2,
         split_trans/1,
         trans_back/1
        ]).

%% Structural Query Exports
-export([
         get_last_row/1,
         get_last_col/1
        ]).

-export([
         write_style_IMPORT/2,
         write_magic_style_IMPORT/2,
         read_styles_IMPORT/1
        ]).

-export([
         deref_overlap_TEST/0
        ]).

-define(to_xml_str, simplexml:to_xml_string).
-define(to_refX, hn_util:refX_from_index).

-define(lt, list_to_tuple).
-define(lf, lists:flatten).
-define(dict, orddict:orddict()).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-include_lib("stdlib/include/ms_transform.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Exported functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_cell_for_muin(#refX{}) -> {any(), any(), any()}.
%% @doc this function is called by muin during recalculation and should
%%      not be used for any other purpose
get_cell_for_muin(#refX{obj = {cell, {XX, YY}}} = RefX) ->
    #refX{site = Site, path = Path} = RefX,
    Attrs = case read_ref(RefX, inside) of
                [{_, A}] -> A;
                _        -> orddict:new()
            end,
    Value = case orddict:find("__rawvalue", Attrs) of
                {ok, {datetime, _, [N]}} -> 
                    muin_date:from_gregorian_seconds(N);
                {ok, V} -> 
                    V;
                _ -> 
                    blank
            end,
    {Value, [], [{"local", {Site, Path, XX, YY}}]}.

write_style_IMPORT(#refX{site=Site}, Style) ->
    Tbl = trans(Site, style),
    mnesia:write(Tbl, Style, write).

-spec write_magic_style_IMPORT(#refX{}, #magic_style{}) -> integer(). 
write_magic_style_IMPORT(Ref=#refX{site=Site}, MagicStyle) ->
    Tbl = trans(Site, style),
    store_style(Ref, Tbl, MagicStyle).

read_styles_IMPORT(#refX{site=Site}) ->
    Tbl = trans(Site, style),
    MS = ets:fun2ms(fun(X) -> X end),
    mnesia:select(Tbl, MS, read).

-spec get_last_row(#refX{}) -> integer(). 
get_last_row(#refX{site=S, path=P}) -> 
    SelX = #refX{site=S, path=P, obj={page, "/"}},
    Desc = lists:usort(fun ({A,_}, {B,_}) -> A > B end, 
                       [{Y, LO} || LO=#local_obj{obj={cell,{_,Y}}}
                                       <- read_objs(SelX, inside)]),
    largest_content(Desc, S).

-spec get_last_col(#refX{}) -> integer(). 
get_last_col(#refX{site=S, path=P}) -> 
    SelX = #refX{site=S, path=P, obj={page, "/"}},
    Desc = lists:usort(fun ({A,_}, {B,_}) -> A > B end, 
                       [{X, LO} || LO=#local_obj{obj={cell,{X,_}}} 
                                       <- read_objs(SelX, inside)]),
    largest_content(Desc, S).

%% Working from the bottom of the form to the top, find the first
%% local object which has content, and return its corresponding
%% ROW/COL
-spec largest_content([{integer(), #local_obj{}}], string()) -> integer(). 
largest_content([], _S) -> 0;
largest_content([{K, LO} | T], S) ->
    case has_content(S, LO) of
        true -> K; 
        false -> largest_content(T, S)
    end.

-spec has_content(string(), #local_obj{}) -> boolean().
has_content(S, LO) ->
    case extract_field(read_attrs(S, [LO], read), "formula", []) of
        [] -> false; 
        [{_, []}] -> false; 
        _ -> true
    end.
    
%% Key = atom()
%% Value = term()
%% @doc this function writes attributes to a cell or cells.
%% 
%% The refX{} can refer to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
%% 
%% This function deals with style-able attributes of cells auto-magically.
%% 
%% If an attribute is saved against a cell which is one of the styled
%% attributes defined in the ref magic_styles in file 
%% <a href="../include/spriki.hrl">spriki.hrl</a>
%% it will be magically managed as a style
%% @end
%% This clause deals with a formula

-spec write_attrs(#refX{}, [{string(), term()}]) -> ?dict.
write_attrs(Ref, NewAttrs) -> write_attrs(Ref, NewAttrs, nil).

-spec write_attrs(#refX{}, [{string(), term()}], auth_srv:auth_req()) 
                 -> ?dict.
write_attrs(Ref, NewAs, AReq) ->
    Op = fun(Attrs) -> 
                 Attrs2 = case orddict:is_key("__hasform", Attrs) of
                              true ->
                                  unattach_form(Ref, ref_to_idx(Ref)),
                                  orddict:erase("__hasform", Attrs);
                               false ->
                                  Attrs
                          end,
                 process_attrs(NewAs, Ref, AReq, Attrs2) end,
    apply_to_attrs(Ref, Op).

-spec process_attrs([{string(), term()}], #refX{}, auth_srv:auth_req(), ?dict) 
                   -> ?dict.
process_attrs([], _Ref, _AReq, Attrs) ->
    Attrs;
process_attrs([{"formula",Val}|Rest], Ref, AReq, Attrs) ->
    Attrs2 = 
        case superparser:process(Val) of
            {formula, Fla} -> 
                write_formula1(Ref, Fla, Val, AReq, Attrs);
            [NVal, Align, Frmt] -> 
                write_formula2(Ref, Val, NVal, Align, Frmt, Attrs)
        end,
    process_attrs(Rest, Ref, AReq, Attrs2);
process_attrs([A={Key,Val}|Rest], Ref, AReq, Attrs) ->
    Attrs2  = case ms_util2:is_in_record(magic_style, Key) of 
                  true  -> apply_style(Ref, A, Attrs);
                  false -> orddict:store(Key, Val, Attrs)
              end,
    process_attrs(Rest, Ref, AReq, Attrs2).

expand_to_rows_or_cols(#refX{obj={RC, {I, J}}}=Ref) when RC == row; RC == column ->
    expand_to_2(Ref, I, J, []);
expand_to_rows_or_cols(_) -> [].

expand_to_2(#refX{obj={Type, _}}=Ref, I, I, Acc) ->
    [Ref#refX{obj={Type, {I, I}}} | Acc];
expand_to_2(#refX{obj={Type, _}}=Ref, I, J, Acc) ->
    expand_to_2(Ref, I+1, J, [Ref#refX{obj={Type, {I, I}}} | Acc]).

expand_ref(#refX{site=S}=Ref) -> 
    [lobj_to_ref(S, LO) || LO <- read_objs(Ref, inside)].

read_ref(Ref, Relation) -> read_ref(Ref, Relation, read).
-spec read_ref(#refX{}, inside | intersect, read | write) 
              -> [{#refX{}, ?dict}].
read_ref(#refX{site=S}=Ref, Relation, Lock) ->
    read_attrs(S, read_objs(Ref, Relation), Lock).

-spec read_ref_field(#refX{}, string(), read|write) 
                    -> [{#refX{}, term()}].
read_ref_field(Ref, Field, Lock) ->
    Cells = read_ref(Ref, inside, Lock),
    extract_field(Cells, Field, []).
    
-spec extract_field([{#refX{}, ?dict}], string(), [{#refX{}, term()}])
                   -> [{#refX{}, term()}].
extract_field([], _F, Acc) ->
    lists:reverse(Acc);
extract_field([{Ref, Attrs}|T], Field, Acc) ->
    Acc2 = case orddict:find(Field, Attrs) of
               {ok, V} -> [{Ref, V}|Acc]; 
               _       -> Acc end,
    extract_field(T, Field, Acc2).    

-spec read_attrs(string(), [#local_obj{}], read|write)
                -> [{#refX{}, ?dict}].
read_attrs(S, LocObjs, Lock) ->
    Tbl = trans(S, item),
    read_attrs_(LocObjs, S, Tbl, Lock, []).
read_attrs_([], _S, _Tbl, _Lock, Acc) ->
    lists:reverse(Acc);
read_attrs_([LO|Tail], S, Tbl, Lock, Acc) ->
    Acc2 = case mnesia:read(Tbl, LO#local_obj.idx, Lock) of
               [#item{attrs=Attrs}] -> [{lobj_to_ref(S, LO), Attrs} | Acc];
               []                   -> Acc end,
    read_attrs_(Tail, S, Tbl, Lock, Acc2).

-spec lobj_to_ref(string(), #local_obj{}) -> #refX{}.
lobj_to_ref(Site, #local_obj{path=P, obj=O}) ->
    #refX{site=Site, path=P, obj=O}.

-spec expunge_refs(string(), [#refX{}]) -> ok. 
expunge_refs(S, Refs) ->
    ItemT = trans(S, item),
    ObjT = trans(S, local_obj),
    [begin
         mnesia:delete(ItemT, Idx, write),
         mnesia:delete_object(ObjT, LO, write),
         unattach_form(Ref, Idx)
     end || Ref <- Refs,
            #local_obj{idx=Idx}=LO <- read_objs(Ref, direct)],
    ok.

%% Apply to attrs does the actual work of modifying a target ref. The
%% behaviour of the modification is determined by the passed in 'Op'
%% function. Upon completion of 'Op' the post_process function is
%% applied, which sets formats and styles as necessary.
-spec apply_to_attrs(#refX{}, fun((?dict) -> ?dict)) -> ?dict.
apply_to_attrs(#refX{site=Site}=Ref, Op) ->
    Table = trans(Site, item), 
    Idx = ref_to_idx_create(Ref),
    Attrs = case mnesia:read(Table, Idx, write) of
                [#item{attrs=A}] -> A;
                _                -> orddict:new()
            end,
    Attrs2 = Op(Attrs),
    Attrs3 = post_process(Ref, Attrs2),
    Item = #item{idx = Idx, attrs = Attrs3},
    tell_front_end_change(Ref, Attrs3),
    mnesia:write(Table, Item, write),
    Attrs3.

%% Last chance to apply any default styles and formats. 
-spec post_process(#refX{}, ?dict) -> ?dict. 
post_process(Ref, Attrs) ->
    Attrs2 = post_process_styles(Ref, Attrs),
    case orddict:find("__rawvalue", Attrs2) of
        {ok, Raw} -> post_process_format(Raw, Attrs2);
        _         -> Attrs2
    end.                       
    
-spec post_process_styles(#refX{}, ?dict) -> ?dict.
post_process_styles(Ref, Attrs) -> 
    case orddict:find("style", Attrs) of
        {ok, _} -> Attrs;
        _       ->
            case orddict:find("__default-align", Attrs) of
                {ok, Align} -> apply_style(Ref,{"text-align",Align},Attrs);
                _           -> Attrs
            end
    end.

post_process_format(Raw, Attrs) ->
    Format = case orddict:find("format", Attrs) of
                 {ok, F} -> F;
                 _       -> "General"
             end,
    case format:get_src(Format) of
        {erlang, {_Type, Output}} ->
            %% Y'all hear, this is how America does color. I tell you what.
            case format:run_format(Raw, Output) of
                {ok, {Color, Val1}} ->
                    Val2  = case Val1 of
                                {errval, ErrVal} -> atom_to_list(ErrVal);
                                A when is_atom(A) -> atom_to_list(A);
                                I when is_integer(I) -> integer_to_list(I);
                                Fl when is_float(Fl) -> float_to_list(Fl);
                                _ -> Val1
                            end,
                    add_attributes(Attrs, [{"value", Val2},
                                           {"overwrite-color", atom_to_list(Color)}]);
                _ ->
                    Attrs
            end;
        _ ->
            Attrs
    end.

shift_cells(#refX{site=Site, obj= Obj}=From, Type, Disp, Rewritten)
  when (Type == insert orelse Type == delete) andalso 
       (Disp == vertical orelse Disp == horizontal) ->
    {XOff, YOff} = hn_util:get_offset(Type, Disp, Obj),
    RefXSel = shift_pattern(From, Disp),
    case read_objs(RefXSel, inside) of
        [] -> [];
        ObjsList ->
            %% %% Rewrite the formulas of all the child cells
            RefXList = [lobj_to_ref(Site, O) || O <- ObjsList],
            ChildCells = lists:flatten([get_children(X) || X <- RefXList]),
            ChildCells2 = hslists:uniq(ChildCells),
            DedupedChildren = lists:subtract(ChildCells2, Rewritten),
            Formulas = [F || X <- DedupedChildren,
                             F <- read_ref_field(X, "formula", write)],
            Fun = fun({ChildRef, F1}, Acc) ->
                          {St, F2} = offset_fm_w_rng(ChildRef, F1, From, {XOff, YOff}),
                          Op = fun(Attrs) -> orddict:store("formula", F2, Attrs) end,
                          apply_to_attrs(ChildRef, Op),
                          case St of
                              clean -> Acc;
                              dirty -> [ChildRef | Acc]
                          end
                  end,
            DirtyChildren = lists:foldl(Fun, [], Formulas),
            
            %% Rewrite the local_obj entries by applying the shift offset.
            ObjTable = trans(Site, local_obj),
            [begin 
                 mnesia:delete_object(ObjTable, LO, write),
                 mnesia:write(ObjTable, shift_obj(LO, XOff, YOff), write)
             end || LO <- ObjsList],
            DirtyChildren
    end.

shift_obj(#local_obj{obj = {cell, {X, Y}}}=LO, XOff, YOff) ->
    O2 = {cell, {X + XOff, Y + YOff}},
    LO#local_obj{obj = O2};
shift_obj(#local_obj{obj = {column, {X1, X2}}}=LO, XOff, _YOff) ->
    O2 = {column, {X1 + XOff, X2 + XOff}},
    LO#local_obj{obj = O2};
shift_obj(#local_obj{obj = {row, {Y1, Y2}}}=LO, _XOff, YOff) ->
    O2 = {row, {Y1 + YOff, Y2 + YOff}},
    LO#local_obj{obj = O2};
shift_obj(LO, _, _) -> LO. 

-spec read_styles(#refX{}, [cellidx()]) -> [#style{}].
read_styles(#refX{site = Site}, Idxs) ->
    Table = trans(Site, style),
    [S || I <- Idxs,
          S <- mnesia:index_read(Table, I, #style.idx)].

%% @doc deletes the contents (formula/value) and the formats
%% of a cell (but doesn't delete the cell itself).
-spec clear_cells(#refX{}) -> ok.
clear_cells(RefX) -> clear_cells(RefX, contents).

-spec clear_cells(#refX{}, all | style | contents) -> ok. 
clear_cells(Ref, contents) ->
    do_clear_cells(Ref, content_attrs());
clear_cells(Ref, all) ->
    do_clear_cells(Ref, ["style", "merge" | content_attrs()]);
clear_cells(Ref, style) ->
    do_clear_cells(Ref, ["style", "merge"]);
clear_cells(Ref, {attributes, DelAttrs}) ->
    do_clear_cells(Ref, DelAttrs).    

do_clear_cells(Ref, DelAttrs) ->
    Op = fun(RX) ->
                 fun(Attrs) ->
                         case lists:keymember("formula", 1, Attrs) of
                             true -> 
                                 set_relations(RX, []),
                                 unattach_form(RX, ref_to_idx(RX));
                             false -> ok
                         end,
                         del_attributes(Attrs, DelAttrs)
                 end
         end,
    [apply_to_attrs(X, Op(X)) || X <- expand_ref(Ref)], 
    ok.

content_attrs() ->
    ["formula",
     "value",             
     "overwrite-color",
     "__rawvalue",          
     "__ast",             
     "__recompile",       
     "__shared",          
     "__area",           
     "__default-align"]. 

%% @doc takes a reference to a
%% <ul>
%% <li>page</li>
%% <li>row</li>
%% <li>column</li>
%% <li>range</li>
%% <li>cell</li>
%% </ul>
%% and then deletes all the cells including their indices in local_obj
%% and makes all cells that are their children throw a #ref! error
%% and deletes the links there the old cell was the child of another cell
%% @todo this is ineffiecient because it reads and then deletes each
%% record individually - if remoting_reg supported a {delete refX all}
%% type message it could be speeded up
-spec delete_cells(#refX{}) -> [#refX{}].
delete_cells(#refX{site = S} = DelX) ->
    case expand_ref(DelX) of
        %% there may be no cells to delete, but there may be rows or
        %% columns widths to delete...
        []     ->
            expunge_refs(S, expand_to_rows_or_cols(DelX)),
            [];
        Cells  ->
            %% update the children that point to the cell that is
            %% being deleted by rewriting the formulae of all the
            %% children cells replacing the reference to this cell
            %% with #ref!
            LocalChildren = [get_children(C) || C <- Cells],
            LocalChildren2 = hslists:uniq(lists:flatten(LocalChildren)),

            %% sometimes a cell will have local children that are also
            %% in the delete zone these need to be removed before we
            %% do anything else...
            LocalChildren3 = lists:subtract(LocalChildren2, Cells),

            %% Rewrite formulas
            [deref_formula(X, DelX) || X <- LocalChildren3],

            %% fix relations table.
            [ok = delete_relation(X) || X <- Cells],

            %% Delete the rows or columns and cells (and their indicices)
            expunge_refs(S, lists:append(expand_to_rows_or_cols(DelX), Cells)),
            LocalChildren3
    end.

-spec deref_formula(#refX{}, #refX{}) -> ok.
deref_formula(Ref, DelRef) ->
    Op = fun(Attrs) -> 
                 case orddict:find("formula", Attrs) of
                     {ok, F1} -> 
                         {_Status, F2} = deref(Ref, F1, DelRef),
                         %% TODO if the status is dirty force a recalculation
                         %% (for Tom McNulty)
                         orddict:store("formula", F2, Attrs);
                     _ ->
                         Attrs
                 end
         end,
    apply_to_attrs(Ref, Op),
    ok.

%% %% @doc copys cells from a reference to a reference
-spec copy_cell(#refX{}, #refX{}, 
                false | horizontal | vertical,
                all | style | value) -> ok.
copy_cell(From=#refX{obj={cell, _}}, 
          To=#refX{obj={cell, _}},
          _, value) ->
    SourceAttrs = case read_ref(From, inside, read) of
                      [{_, As}] -> As;
                      _         -> []
                  end,
    Op = fun(Attrs) -> copy_attributes(SourceAttrs, Attrs, ["value",
                                                            "formula",
                                                            "__rawvalue"]) 
         end,
    apply_to_attrs(To, Op),
    ok;
copy_cell(From=#refX{obj={cell, _}}, 
          To=#refX{obj={cell, _}},
          _, style) ->
    SourceAttrs = case read_ref(From, inside, read) of
                      [{_, As}] -> As;
                      _         -> []
                  end,
    Op = fun(Attrs) -> copy_attributes(SourceAttrs, Attrs, ["style"]) end,
    apply_to_attrs(To, Op),
    ok;
copy_cell(#refX{obj = {cell, {FX,FY}}} = From, 
          #refX{obj = {cell, {TX,TY}}} = To, 
          Incr, all) ->
    Attrs = case read_ref(From, inside, read) of
                [{_, As}] -> As;
                _         -> []
            end,
    Formula = case orddict:find("formula", Attrs) of
                  {ok, V} -> superparser:process(V); 
                  _       -> "" end,
    Formula2 = 
        case Formula of
            {formula, F1} ->
                offset_formula(F1, {(TX - FX), (TY - FY)});
            [{Type, F1},  _A, _F] ->
                case Incr of
                    false  ->
                        case Type of
                            datetime ->
                                {datetime, D, T} = F1,
                                dh_date:format("d/m/Y", {D, T});
                            _ ->
                                tconv:to_s(F1)
                        end;
                    _Other -> %% Other can be a range of different values...
                        case Type of
                            int      ->
                                NewV = F1 + diff(FX, FY, TX, TY, Incr),
                                tconv:to_s(NewV);
                            datetime ->
                                {datetime, {Y, M , D}, T} = F1,
                                Date = calendar:date_to_gregorian_days(Y, M, D),
                                Date2 = Date + diff(FX, FY, TX, TY, Incr),
                                NewD = calendar:gregorian_days_to_date(Date2),
                                dh_date:format("d/m/Y", {NewD, T});
                            _ ->
                                tconv:to_s(F1)
                        end
                end;
            _ -> 
                ""
        end,
    Attrs2 = copy_attributes(Attrs, orddict:new(), ["merge", "style"]),
    Attrs3 = orddict:store("formula", Formula2, Attrs2),
    write_attrs(To, Attrs3),
    ok.

%% @spec read_page_structure(Ref) -> dh_tree()
%% @doc read the populated pages under the specified path
%% @todo fix up api
read_page_structure(#refX{site = Site}) ->
    MS = ets:fun2ms(fun(#local_obj{path=P}) -> P end),
    Items = mnesia:dirty_select(trans(Site, local_obj), MS),
    filter_pages(Items, dh_tree:new()).

read_pages(#refX{site = Site}) ->
    MS = ets:fun2ms(fun(#local_obj{path=P}) -> P end),
    lists:usort(mnesia:select(trans(Site, local_obj), MS, read)).
    
filter_pages([], Tree) ->
    Tree;
filter_pages([Path | T], Tree) ->
    filter_pages(T, dh_tree:add(Path, Tree)).

-spec ref_to_idx_create(#refX{}) -> pos_integer().
%% @doc ref_to_idx_create ref_to_idx_create gets the index of an object 
%% AND CREATES IT IF IT DOESN'T EXIST
ref_to_idx_create(#refX{site = S, path = P, obj = O} = RefX) ->
    case ref_to_idx(RefX) of
        false -> Idx = util2:get_timestamp(),
                 Rec = #local_obj{path = P, obj = O, idx = Idx},
                 ok = mnesia:write(trans(S, local_obj), Rec, write),
                 Idx;
        Idx   -> Idx        
    end.

%% converts a tablename into the site-specific tablename
trans(Site, TableName) when is_atom(TableName) ->
    Prefix = get_prefix(Site),
    list_to_atom(Prefix ++ "&" ++ atom_to_list(TableName));
trans(Site, Record) when is_tuple(Record) -> 
    NRec = trans(Site, element(1, Record)),
    setelement(1, Record, NRec).


%%% splits a tablename into the site and record
split_trans(List) when is_list(List)->
    Fun = fun(X, Acc) ->
                  [split_trans(X) | Acc]
          end,
    lists:foldl(Fun, [], List);
split_trans(Record) when is_tuple(Record) ->
    OldName = element(1, Record),
    OldName2 = atom_to_list(OldName),
    [Site, Port, NewName] = string:tokens(OldName2, "&"),
    NewName2 = list_to_atom(NewName),
    {"http://" ++ Site ++ ":" ++ Port, NewName2, Record}.

trans_back([]) -> [];
trans_back(List) when is_list(List) ->
    Fun = fun(X, Acc) ->
                  [trans_back(X) | Acc]
          end,
    lists:foldl(Fun, [], List);
trans_back(Atom) when is_atom(Atom) ->
    [_Site, _Port, NewName] = string:tokens(atom_to_list(Atom), "&"),
    list_to_atom(NewName);
trans_back(Record) when is_tuple(Record)-> 
    OldName = element(1, Record),
    OldName2 = atom_to_list(OldName),
    [_Site, _Port, NewName] = string:tokens(OldName2, "&"),
    setelement(1, Record, list_to_atom(NewName)).

get_prefix(R) when is_record(R, refX) ->
    get_prefix(R#refX.site);
get_prefix("http://"++Site) ->
    [case S of $: -> $&; S  -> S end 
     || S <- Site].

shift_pattern(#refX{obj = {cell, {X, Y}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {X, Y, X, infinity}}};
shift_pattern(#refX{obj = {cell, {X, Y}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X, Y, infinity, Y}}};
shift_pattern(#refX{obj = {range, {X1, Y1, X2, _Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {X1, Y1, X2, infinity}}};
shift_pattern(#refX{obj = {range, {X1, Y1, _X2, Y2}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X1, Y1, infinity, Y2}}};
shift_pattern(#refX{obj = {row, {Y1, _Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {0, Y1, infinity, infinity}}};
shift_pattern(#refX{obj = {column, {X1, _X2}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X1, 0, infinity, infinity}}}.

idx_to_ref(S, Idx) ->
    case mnesia:read(trans(S, local_obj), Idx, read) of
        [Rec] -> #local_obj{path = P, obj = O} = Rec,
                 #refX{site = S, path = P, obj = O};
        []    -> {error, id_not_found, Idx}
    end.

%% @doc Make a #muin_rti record out of a ref record and a flag that specifies 
%% whether to run formula in an array context.
refX_to_rti(#refX{site = S, path = P, obj = {cell, {C, R}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, 
              col = C, row = R, 
              array_context = AC,
              auth_req = AR};
refX_to_rti(#refX{site = S, path = P, obj = {range, {C, R, _, _}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P, 
              col = C, row = R, 
              array_context = AC,
              auth_req = AR}.

%% ref_to_idx reads the index of an object AND RETURNS 'false'
%% IF IT DOESN'T EXIST
-spec ref_to_idx(#refX{}) -> pos_integer() | false. 
ref_to_idx(#refX{site = S, path = P, obj = Obj}) ->
    Table = trans(S, local_obj),
    MS = [{#local_obj{path=P, obj=Obj, idx = '$1', _='_'}, [], ['$1']}],
    case mnesia:select(Table, MS, read) of
        [I] -> I;
        _   -> false
    end.

%% make_cell makes a cell with dollars and stuff based on the offset
make_cell(false, X, XOffset, false, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, false, Y, YOffset) ->
    [$$] ++ tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset);
make_cell(false, X, XOffset, true, Y, YOffset) ->
    tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, true, Y, YOffset)  -> 
    [$$] ++ tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y + YOffset).

%% drag'n'drop_cell drags and drops a cell ignoring offsets
%% if the row/column part is fixed with a dollar
drag_n_drop_cell(false, X, XOffset, false, Y, YOffset) ->
    if
        (X + XOffset) <  1 orelse  (Y + YOffset) <  1 -> "#REF!";
        (X + XOffset) >= 1 andalso (Y + YOffset) >= 1 ->
            tconv:to_b26(X + XOffset) ++ tconv:to_s(Y + YOffset)
    end;
drag_n_drop_cell(true, X, _XOffset, false, Y, YOffset) ->
    if
        X <  1 orelse ( Y + YOffset) <  1 -> "#REF!";
        X >= 1 andalso (Y + YOffset) >= 1 ->
            [$$] ++ tconv:to_b26(X) ++ tconv:to_s(Y + YOffset)
    end;
drag_n_drop_cell(false, X, XOffset, true, Y, _YOffset) ->
    if
        (X + XOffset) <  1 orelse  Y <  1 -> "#REF!";
        (X + XOffset) >= 1 andalso Y >= 1 ->
            tconv:to_b26(X + XOffset) ++ [$$] ++ tconv:to_s(Y)
    end;
drag_n_drop_cell(true, X, _XOffset, true, Y, _YOffset)  -> 
    if
        X <  1 orelse  Y <  1 -> "#REF!";
        X >= 1 andalso Y >= 1 ->
            [$$] ++ tconv:to_b26(X) ++ [$$] ++ tconv:to_s(Y)
    end.

make_col(false, X) -> tconv:to_b26(X);
make_col(true,  X) -> [$$] ++ X.

make_row(false, Y) -> tconv:to_s(Y);
make_row(true,  Y) -> [$$] ++ tconv:to_s(Y).


diff( FX, _FY,  TX, _TY, horizontal) -> TX - FX;
diff(_FX,  FY, _TX,  TY, vertical)   -> TY - FY.

%% make formula creates a new formula, but also returns a status.
%% Status can be [clean | dirty]
%% Formulae that return dirty should be marked dirty at recalc
%% time as they will not recalc to the real value
%% The function 'INDIRECT' is an example of such a function
make_formula(Toks) ->
    mk_f(Toks, {clean, []}).

%% this function needs to be extended...
mk_f([], {St, A}) ->
    {St, "="++lists:flatten(lists:reverse(A))};

mk_f([{errval, _, '#REF!'} | T], {St, A}) -> 
    mk_f(T, {St, ["#REF!" | A]});

mk_f([{deref, Text} | T], {_St, A}) ->
    mk_f(T, {dirty, [Text | A]});

%% special infering of division
mk_f([{cellref, _, C1}, {cellref, _, C2} | T], {St, A}) -> 
    mk_f(T, {St, [C2#cellref.text, "/", C1#cellref.text | A]});

mk_f([{int, _, I}, {cellref,_,C} | T], {St, A}) -> 
    mk_f(T, {St, [C#cellref.text, "/", integer_to_list(I) | A]});

mk_f([{float, _, {F, _}}, {cellref,_,C} | T], {St, A}) -> 
    mk_f(T, {St, [C#cellref.text, "/", float_to_list(F) | A]});

mk_f([{')',_}, {cellref,_,C} | T], {St, A}) ->
    mk_f(T, {St, [C#cellref.text, "/", ")" | A]});

%% order matters - now detecting 'root' cells
mk_f([{cellref, _, #cellref{path="/", text=Text}} | T], {St, A}) -> 
    mk_f(T, {St, ["/" ++ Text | A]});

mk_f([{cellref, _, C} | T], {St, A}) ->
    mk_f(T, {St, [C#cellref.text | A]});

mk_f([{rangeref, _, R} | T], {St, A}) ->
    mk_f(T, {St, [R#rangeref.text | A]});

mk_f([{namedexpr, _, N} | T], {St, A}) ->
    mk_f(T, {St, [N#namedexpr.path ++ N#namedexpr.text | A]});

mk_f([{bool, _, H} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]});

mk_f([{atom, _, H} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]});

mk_f([{int, _, I} | T], {St, A}) ->
    mk_f(T, {St, [integer_to_list(I) | A]});

mk_f([{float, _, {F, _OrigStr}} | T], {St, A}) ->
    mk_f(T, {St, [float_to_list(F) | A]});

mk_f([{formula, _, S} | T], {St, A}) ->
    mk_f(T, {St, [S | A]});

mk_f([{str, _, S} | T], {St, A}) ->
    mk_f(T, {St, [$", S, $" | A]});

mk_f([{recalc, S} | T], {_St, A}) ->
    mk_f(T, {dirty, [S | A]});

mk_f([{name, _, "INDIRECT"} | T], {_St, A}) ->
    mk_f(T, {dirty, ["INDIRECT" | A]});

mk_f([{name, _, "SUM"} | T], {_St, A}) ->
    mk_f(T, {dirty, ["SUM" | A]});

mk_f([{name, _, "CELL"} | T], {_St, A}) ->
    mk_f(T, {dirty, ["CELL" | A]});

mk_f([{name, _, S} | T], {St, A}) ->
    mk_f(T, {St, [S | A]});

mk_f([{H, _} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]}).

parse_cell(Cell) ->
    {XDollar, Rest} = is_fixed(Cell),
    Fun = fun(XX) ->
                  if XX < 97  -> false;
                     XX > 122 -> false;
                     true     -> true
                  end
          end,
    {XBits, YBits} = lists:partition(Fun,string:to_lower(Rest)),
    {YDollar, Y} = is_fixed(YBits),
    {XDollar, tconv:to_i(XBits), YDollar, list_to_integer(Y)}.

parse_range(Range) ->
    [Cell1, Cell2] = string:tokens(Range, ":"),
    {XD1, X1, YD1, Y1} = parse_cell(Cell1),
    {XD2, X2, YD2, Y2} = parse_cell(Cell2),
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2}.

parse_cols(Cols) ->
    [Col1, Col2] = string:tokens(Cols, ":"),
    {XD1, R1} = is_fixed(Col1),
    {XD2, R2} = is_fixed(Col2),
    {XD1, tconv:to_i(R1), XD2, tconv:to_i(R2)}.

parse_rows(Rows) ->
    [Row1, Row2] = string:tokens(Rows, ":"),
    {YD1, R1} = is_fixed(Row1),
    {YD2, R2} = is_fixed(Row2),
    {YD1, list_to_integer(R1), YD2, list_to_integer(R2)}.

is_fixed([$$|Rest]) -> {true, Rest};
is_fixed(List)      -> {false, List}.

offset_with_ranges(Toks, Cell, From, Offset) ->
    offset_with_ranges1(Toks, Cell, From, Offset, []).

offset_with_ranges1([], _Cell, _From, _Offset, Acc) ->
    lists:reverse(Acc);
offset_with_ranges1([{rangeref, LineNo,
                      #rangeref{path = Path, text = Text}=H} | T],
                    Cell, #refX{path = FromPath} = From, Offset, Acc) ->
    #refX{path = CPath} = Cell,
    PathCompare = muin_util:walk_path(CPath, Path),
    Range = muin_util:just_ref(Text),
    Prefix = case muin_util:just_path(Text) of
                 "/"     -> "";
                 Other   -> Other
             end,
    [Cell1|[Cell2]] = string:tokens(Range, ":"),
    {X1D, X1, Y1D, Y1} = parse_cell(Cell1),
    {X2D, X2, Y2D, Y2} = parse_cell(Cell2),
    NewText = case PathCompare of
                  FromPath -> make_new_range(Prefix, Cell1, Cell2,
                                             {X1D, X1, Y1D, Y1},
                                             {X2D, X2, Y2D, Y2},
                                             From, Offset);
                  _        -> Text
              end,
    NewAcc = {rangeref, LineNo, H#rangeref{text = NewText}},
    offset_with_ranges1(T, Cell, From, Offset, [NewAcc | Acc]);
offset_with_ranges1([{cellref, LineNo,
                      C=#cellref{path = Path, text = Text}}=H | T],
                    Cell, #refX{path = FromPath} = From,
                    {XO, YO}=Offset, Acc) ->
    {XDollar, X, YDollar, Y} = parse_cell(muin_util:just_ref(Text)),
    case From#refX.obj of
        %% If ever we apply two offsets at once, do it in two steps.
        {range, {Left, Top, _Right, Bottom}}
          when (YO == 0) and ((Left > X) or (Top > Y) or (Y > Bottom)) ->
            offset_with_ranges1(T, Cell, From, Offset, [H | Acc]);
        {range, {Left, Top, Right, _Bottom}}
          when (XO == 0) and ((Left > X) or (X > Right) or (Top > Y)) ->
            offset_with_ranges1(T, Cell, From, Offset, [H | Acc]);
        {column,{Left,_Right}} when X < Left ->
            offset_with_ranges1(T, Cell, From, Offset, [H | Acc]);
        {row,{Top,_Bottom}} when Y < Top ->
            offset_with_ranges1(T, Cell, From, Offset, [H | Acc]);
        _Else ->
            #refX{path = CPath} = Cell,
            Prefix = case muin_util:just_path(Text) of
                         "/"   -> "";
                         Other -> Other
                     end,

            PathCompare = muin_util:walk_path(CPath, Path),
            NewCell =
                case PathCompare of
                    FromPath -> make_cell(XDollar, X, XO, YDollar, Y, YO);
                    _        -> Text
                end,
            NewAcc = {cellref, LineNo, C#cellref{text = Prefix ++ NewCell}},    
            offset_with_ranges1(T, Cell, From, {XO, YO}, [NewAcc | Acc])
    end;
offset_with_ranges1([H | T], Cell, From, Offset, Acc) ->
    offset_with_ranges1(T, Cell, From, Offset, [H | Acc]).

%% handle cells
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {cell, {X, Y}}} = _From, {XO, YO}) ->
    NC1 =
        case {X1, Y1} of
            {X, Y} -> make_cell(X1D, X1, XO, Y1D, Y1, YO);
            _      -> Cell1
        end,
    NC2 =
        case {X2, Y2} of
            {X, Y} -> make_cell(X2D, X2, XO, Y2D, Y2, YO);
            _      -> Cell2
        end,
    Prefix ++ NC1 ++ ":" ++ NC2;
%% handle rows
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {row, {Top, _Bottom}}}, 
               {0=_XOffset, YOffset}) ->
    NC1 = if Top =< Y1 -> make_cell(X1D, X1, 0, Y1D, Y1, YOffset);
             true      -> Cell1 end,
    NC2 = if Top =< Y2 -> make_cell(X2D, X2, 0, Y2D, Y2, YOffset); 
             true      -> Cell2 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
%% handle columns
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {column, {Left, _Right}}}, 
               {XOffset, 0=_YOffset}) ->
    NC1 = if Left =< X1 -> make_cell(X1D, X1, XOffset, Y1D, Y1, 0);
             true       -> Cell1 end,
    NC2 = if Left =< X2 -> make_cell(X2D, X2, XOffset, Y2D, Y2, 0); 
             true       -> Cell2 end,
    Prefix ++ NC1 ++ ":" ++ NC2;
%% handle ranges (horizontal rewrite)
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {range, {_XA, YA, _XB, YB}}}, 
               {XOffset, 0}) ->
    if
        (YA =< Y1 andalso YB >= Y2) ->
            NC1 = make_cell(X1D, X1, XOffset, Y1D, Y1, 0),
            NC2 = make_cell(X2D, X2, XOffset, Y2D, Y2, 0);
        (YA >  Y1 orelse  YB <  Y2) ->
            NC1 = Cell1,
            NC2 = Cell2
    end,
    Prefix ++ NC1 ++ ":" ++ NC2;
%% handle ranges (horizontal rewrite)
make_new_range(Prefix, Cell1, Cell2, 
               {X1D, X1, Y1D, Y1},
               {X2D, X2, Y2D, Y2},
               #refX{obj = {range, {XA, _YA, XB, _YB}}}, 
               {0, YOffset}) ->
    if
        (XA =< X1 andalso XB >= X2) ->
            NC1 = make_cell(X1D, X1, 0, Y1D, Y1, YOffset),
            NC2 = make_cell(X2D, X2, 0, Y2D, Y2, YOffset);
        (XA >  X1 orelse  XB <  X2) ->
            NC1 = Cell1,
            NC2 = Cell2
    end,
    Prefix ++ NC1 ++ ":" ++ NC2.

%% used in copy'n'paste, drag'n'drops etc...
d_n_d_c_n_p_offset(Toks, XOffset, YOffset) ->
    d_n_d_c_n_p_offset1(Toks, XOffset, YOffset, []).

d_n_d_c_n_p_offset1([], _XOffset, _YOffset, Acc) -> lists:reverse(Acc);
d_n_d_c_n_p_offset1([{cellref, LineNo, #cellref{text = Text}= H} | T], 
                    XOffset, YOffset, Acc) ->
    Cell = muin_util:just_ref(Text),
    Prefix = case muin_util:just_path(Text) of
                 "/"   -> "";
                 Other -> Other
             end,
    {XDollar, X, YDollar, Y} = parse_cell(Cell),
    NewCell = drag_n_drop_cell(XDollar, X, XOffset, YDollar, Y, YOffset),
    NewRef = {cellref, LineNo, H#cellref{text = Prefix ++ NewCell}},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewRef | Acc]);
d_n_d_c_n_p_offset1([{rangeref, LineNo, #rangeref{text = Text}=H} | T], XOffset, YOffset, Acc) ->
    Range = muin_util:just_ref(Text),
    Pf = case muin_util:just_path(Text) of
             "/"   -> "";
             Other -> Other
         end,
    [Cell1 | [Cell2]] = string:tokens(Range, ":"),
    {X1D, X1, Y1D, Y1} = parse_cell(Cell1),
    {X2D, X2, Y2D, Y2} = parse_cell(Cell2),
    NC1 = drag_n_drop_cell(X1D, X1, XOffset, Y1D, Y1, YOffset),
    NC2 = drag_n_drop_cell(X2D, X2, XOffset, Y2D, Y2, YOffset),
    NewText = if
                  NC1 ==  "#REF!" orelse  NC2 ==  "#REF!" ->
                      "#REF!";
                  NC1 =/= "#REF!" andalso NC2 =/= "#REF!" ->
                      Pf ++ NC1 ++ ":" ++ NC2
              end,
    NewR = {rangeref, LineNo, H#rangeref{text = NewText}},
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [NewR | Acc]);
d_n_d_c_n_p_offset1([H | T], XOffset, YOffset, Acc) ->
    d_n_d_c_n_p_offset1(T, XOffset, YOffset, [H | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Relations 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -spec get_local_parents(#refX{}) -> [#refX{}].
%% get_local_parents(#refX{site = Site} = X) ->
%%     ParentIdxs = get_local_rel_idxs(X, parents),
%%     [idx_to_ref(Site, C) || C <- ParentIdxs].

-spec get_children(#refX{}) -> [#refX{}].
get_children(#refX{site = Site} = X) ->
    ChildIdxs = get_children_idxs(X),
    [idx_to_ref(Site, C) || C <- ChildIdxs].

-spec get_children_idxs(#refX{}) -> [cellidx()]. 
get_children_idxs(#refX{site = Site, obj = {cell, _}} = Ref) ->
    case ref_to_idx(Ref) of
        false -> 
            [];
        Idx -> 
            Table = trans(Site, relation),
            case mnesia:read(Table, Idx, read) of
                [R] -> R#relation.children;
                _   -> []
            end
    end;
get_children_idxs(#refX{obj = {Type, _}} = Ref) 
  when (Type == row) orelse (Type == column) orelse 
       (Type == range) orelse (Type == page) ->
    lists:flatten([get_children_idxs(X) 
                   || X=#refX{obj={cell,_}} <- expand_ref(Ref)]).

-spec delete_relation(#refX{}) -> ok.
delete_relation(#refX{site = Site} = Cell) ->
    case ref_to_idx(Cell) of
        false -> ok;
        CellIdx ->
            Tbl = trans(Site, relation),
            case mnesia:read(Tbl, CellIdx, write) of
                [R] ->
                    [del_child(P, CellIdx, Tbl) || 
                        P <- R#relation.parents],
                    ok = mnesia:delete(Tbl, CellIdx, write);
                _ -> ok
            end
    end.

-spec del_child(cellidx(), cellidx(), atom()) -> ok.
del_child(CellIdx, Child, Tbl) ->
    case mnesia:read(Tbl, CellIdx, write) of
        [R] ->
            Children = ordsets:del_element(Child, R#relation.children),
            R2 = R#relation{children = Children},
            mnesia:write(Tbl, R2, write);
        _ ->
            ok
    end.

-spec set_relations(#refX{}, [#refX{}]) -> ok.
set_relations(#refX{site = Site} = Cell, Parents) ->
    Tbl = trans(Site, relation),
    CellIdx = ref_to_idx_create(Cell),
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R; 
              []  -> #relation{cellidx = CellIdx}
          end,
    Rel2 = set_parents(Tbl, Rel, Parents),
    mnesia:write(Tbl, Rel2, write).

-spec set_parents(atom(), #relation{}, [#refX{}]) -> #relation{}. 
set_parents(Tbl, 
                  Rel = #relation{cellidx = CellIdx, 
                                  parents = CurParents},
                  Parents) ->
    ParentIdxs = ordsets:from_list([ref_to_idx_create(P) || P <- Parents]),
    LostParents = ordsets:subtract(CurParents, ParentIdxs),
    [del_child(P, CellIdx, Tbl) || P <- LostParents],
    [ok = add_child(P, CellIdx, Tbl) || P <- ParentIdxs],
    Rel#relation{parents = ParentIdxs}.

%% Adds a new child to a parent.
-spec add_child(cellidx(), cellidx(), atom()) -> ok.
add_child(CellIdx, Child, Tbl) ->
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R;
              []  -> #relation{cellidx = CellIdx}
          end,
    Children = ordsets:add_element(Child, Rel#relation.children),
    mnesia:write(Tbl, Rel#relation{children = Children}, write).

%% dereferences a formula
deref(Child, [$=|Formula], DeRefX) when is_record(DeRefX, refX) ->
    {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {1, 1}),
    NewToks = deref1(Child, Toks, DeRefX, []),
    make_formula(NewToks).

deref1(_Child, [], _DeRefX, Acc) -> lists:reverse(Acc);
deref1(Child, [{rangeref, _, #rangeref{text = Text}} | T], DeRefX, Acc) ->
    %% only deref the range if it is completely obliterated by the deletion
    #refX{obj = Obj1} = DeRefX,
    Range = muin_util:just_ref(Text),
    Prefix = case muin_util:just_path(Text) of
                 "/" -> [];
                 Pre -> Pre
             end,
    Obj2 = hn_util:parse_ref(Range),
    NewTok = case deref_overlap(Range, Obj1, Obj2) of
                 {deref, "#REF!"} -> {deref, Prefix ++ "#REF!"};
                 {recalc, Str}    -> {recalc, Prefix ++ Str};
                 {formula, Str}   -> {formula, Prefix ++ Str}
             end,
    deref1(Child, T, DeRefX, [NewTok | Acc]);
deref1(Child, [{cellref, _, #cellref{path = Path, text = Text}}=H | T], 
       DeRefX, Acc) ->
    NewTok = deref2(Child, H, Text, Path, DeRefX),
    deref1(Child, T, DeRefX, [NewTok | Acc]);
deref1(Child, [H | T], DeRefX, Acc) ->
    deref1(Child, T, DeRefX, [H | Acc]).

%% sometimes Text has a prepended slash
deref2(Child, H, [$/|Text], Path, DeRefX) ->
    Deref2 = deref2(Child, H, Text, Path, DeRefX),
    case Deref2 of
        H              -> H;
        {deref,   Str} -> {deref,   "/" ++ Str};
        {recalc,  Str} -> {recalc,  "/" ++ Str};
        {formula, Str} -> {formula, "/" ++ Str}
    end;
%% special case for ambiguous parsing of division
%% this matches on cases like =a1/b3
deref2(_Child, _H, Text, "/", DeRefX) ->
    #refX{obj = Obj1} = DeRefX,
    Obj2 = hn_util:parse_ref(Text),
    deref_overlap(Text, Obj1, Obj2);
deref2(Child, H, Text, Path, DeRefX) ->
    #refX{path = CPath} = Child,
    #refX{path = DPath, obj = Obj1} = DeRefX,
    PathCompare = muin_util:walk_path(CPath, Path),
    case PathCompare of
        DPath -> case Path of
                     "./" -> {deref, "#REF!"};
                     _P   -> S1 = muin_util:just_path(Text),
                             S2 = muin_util:just_ref(Text),
                             Obj2 = hn_util:parse_ref(S2),
                             case deref_overlap(S2, Obj1, Obj2) of
                                 {deref, "#REF!"} -> {deref, S1 ++ "#REF!"};
                                 O                -> S1 ++ O
                             end
                 end;
        _Else -> H
    end.

%% if Obj1 completely subsumes Obj2 then the reference to Obj2 should 
%% be dereferenced (return 'deref')
%% %% if Obj1 partially subsumes Obj2 then the reference to Obj2 should
%% be rewitten (return 'rewrite')
%% if there is partial or no overlap then return 'unchanged'
deref_overlap(Text, Obj1, Obj2) ->
    %% the first thing we do is check each corner of Objs2 to see if it is inside
    %% Obj1. Depending on the pattern of corners we rewrite the formula
    %% - if all 4 corners are in the delete area the range must be dereferenced
    %% - if 2 corners are in the delete area the formula must be rewritten
    %% - if 1 corner is in the delete are the range must be deferenced
    %%   because there is no way to rewrite it...
    %% - (if 3 corners are in the delete area then the laws of Euclidean
    %%   geometry have broken down and the end times are probably upon us
    %%   so I would flee for my life sinner!)
    %% 
    %% BUT if all 4 corners are outside the delete area we need to check again:
    %% - if the delete area is wholy inside the range then the range must be deferenced
    %% - if the delete area transpierces the range then the range must be rewritten
    {X1,  Y1,   X2,  Y2}  = expand(Obj1),
    {XX1, YY1,  XX2, YY2} = expand(Obj2),
    IntTL = intersect(XX1, YY1, X1, Y1, X2, Y2),
    IntBL = intersect(XX1, YY2, X1, Y1, X2, Y2),
    IntTR = intersect(XX2, YY1, X1, Y1, X2, Y2),
    IntBR = intersect(XX2, YY2, X1, Y1, X2, Y2),
    case {IntTL, IntBL, IntTR, IntBR} of
        %% all included - deref!
        {in,  in,  in,  in}  -> {deref, "#REF!"};
        %% none included you need to recheck in case the delete area
        %% is contained in, or transects the target area
        {out, out, out, out} -> recheck_overlay(Text, Obj1, Obj2);
        %% one corner included - deref!
        {in,  out, out, out} -> {deref, "#REF!"};
        {out, in,  out, out} -> {deref, "#REF!"};
        {out, out, in,  out} -> {deref, "#REF!"};
        {out, out, out, in}  -> {deref, "#REF!"};
        %% two corners included rewrite
        {in,  in,  out, out} -> rewrite(X1, X2, Obj2, Text, left); %% left del
        {out, out, in,  in}  -> rewrite(X1, Obj2, Text, right);
        {in,  out, in,  out} -> rewrite(Y1, Y2, Obj2, Text, top);  %% top del
        {out, in,  out, in}  -> rewrite(Y1, Obj2, Text, bottom);
        %% transects are column/row intersects
        {transect, transect, out, out} -> rewrite(X1, X2, Obj2, Text, left); %% left del
        {out, out, transect, transect} -> rewrite(X1, Obj2, Text, right);
        {transect, out, transect, out} -> rewrite(Y1, Y2, Obj2, Text, top);  %% top del
        {out, transect, out, transect} -> rewrite(Y1, Obj2, Text, bottom);
        {transect, transect, transect, transect} -> {deref, "#REF!"}
    end.

%% this first clause catches rows/columns where the deleting object is a cell/range
%% in none of these cases does the formula dereference...
intersect(A1, A2, X1, Y1, X2, Y2)
  when (is_atom(A1) orelse is_atom(A2)) andalso
       (is_integer(X1) andalso is_integer(Y1)
        andalso is_integer(X2) andalso is_integer(Y2)) -> out;
%% cols/rows never dereference
intersect(A1, Y1, X1, A2, X2, A3)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(Y1) andalso is_integer(X1) andalso is_integer(X2)) -> out;
%% rows/cols never deference
intersect(X1, A1, A2, Y1, A3, Y2)
  when (is_atom(A1) andalso is_atom(A2) andalso is_atom(A3))
       andalso (is_integer(X1) andalso is_integer(Y1) andalso is_integer(Y2)) -> out;
%% page deletes always dereference
intersect(_XX1, _YY1, zero, zero, inf, inf) ->
    out;
%% this is a row-row comparison
intersect(Type, YY1, zero, Y1, inf, Y2)
  when ((Type == zero) orelse (Type == inf)) ->
    if
        (YY1 >= Y1), (YY1 =< Y2) -> transect;
        true                     -> out
    end;
%% this is a col-col comparison
intersect(XX1, Type, X1, zero, X2, inf)
  when ((Type == zero) orelse (Type == inf)) ->
    if
        (XX1 >= X1), (XX1 =< X2) -> transect;
        true                     -> out
    end;
intersect(XX1, YY1, X1, Y1, X2, Y2) ->    
    if
        %% check for cell/range intersections
        (xx1 >= X1),   (XX1 =< X2), (YY1 >= Y1),  (YY1 =< Y2) -> in;
        %% order matters - first check for rows that are included
        (XX1 >= X1),   (XX1 =< X2), (zero == Y1), (inf == Y2) -> in;
        (zero == X1),  (inf == X2), (YY1 >= Y1),  (YY1 =< Y2) -> in;
        %% now check for partial intersections
        (XX1 == zero), (YY1 >= Y1), (YY1 =< Y2)               -> in;
        (XX1 == inf),  (YY1 >= Y1), (YY1 =< Y2)               -> in;
        (YY1 == zero), (XX1 >= X1), (XX1 =< X2)               -> in;
        (YY1 == inf),  (XX1 >= X1), (XX1 =< X2)               -> in;
        true                                                  -> out
    end.         

%% rewrite/5
rewrite(X1O, X2O, {range, _}, Text, left)   ->
    {XD1, _X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1O, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - (X2O - X1O + 1)), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(X1O, X2O, {column, _}, Text, left)   ->
    {XD1, _X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1O) ++ ":" ++ make_col(XD2, (X2 - (X2O - X1O + 1))),
    {recalc, S};

rewrite(Y1O, Y2O, {range, _}, Text, top)   ->
    {XD1, X1, YD1, _Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1O, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - (Y2O - Y1O + 1)), 0),
    {recalc, S};

rewrite(Y1O, Y2O, {row, _}, Text, top)   ->
    {YD1, _Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1O) ++ ":" ++ make_row(YD2, (Y2 - (Y2O - Y1O + 1))),
    {recalc, S}.

%% rewrite/4
rewrite(XO, {range, _}, Text, right)  ->
    {XD1, X1, YD1, Y1, XD2, _X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (XO - 1), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(XO, {column, _}, Text, right)  ->
    {XD1, X1, XD2, _X2} = parse_cols(Text),
    S = make_col(XD1, X1) ++ ":" ++ make_col(XD2, (XO - 1)),
    {recalc, S};

rewrite(XO, {range, _}, Text, middle_column)  ->
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, (X2 - XO), 0, YD2, Y2, 0),
    {recalc, S};

rewrite(XO, {column, _}, Text, middle)  ->
    {XD1, X1, XD2, X2} = parse_cols(Text),
    S = make_col(XD1, X1) ++ ":" ++ make_col(XD2, (X2 - XO)),
    {recalc, S};

rewrite(YO, {range, _}, Text, bottom) ->
    {XD1, X1, YD1, Y1, XD2, X2, YD2, _Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (YO - 1), 0),
    {recalc, S};

rewrite(YO, {row, _}, Text, bottom) ->
    {YD1, Y1, YD2, _Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (YO - 1)),
    {recalc, S};

rewrite(YO, {range, _}, Text, middle_row) ->
    {XD1, X1, YD1, Y1, XD2, X2, YD2, Y2} = parse_range(Text),
    S = make_cell(XD1, X1, 0, YD1, Y1, 0) ++ ":" ++
        make_cell(XD2, X2, 0, YD2, (Y2 - YO), 0),
    {recalc, S};

rewrite(YO, {row, _}, Text, middle) ->
    {YD1, Y1, YD2, Y2} = parse_rows(Text),
    S = make_row(YD1, Y1) ++ ":" ++ make_row(YD2, (Y2 - YO)),
    {recalc, S}.

%% page deletes always derefence
recheck_overlay(_Text, {page, "/"}, _Target) -> {deref, "#REF!"};
%% cell targets that have matched so far ain't gonna
recheck_overlay(Text, _DelX, {cell, _}) -> {formula, Text};
%% cell deletes that haven't matched a row or column so far ain't gonna
recheck_overlay(Text, {cell, _}, {Type, _})
  when ((Type == row) orelse (Type == column)) -> {formula, Text};
%% cols/rows cols/range comparisons always fail
recheck_overlay(Text, {Type, _}, {column, _})
  when ((Type == row) orelse (Type == range)) -> {formula, Text};
%% rows/cols comparisons always fail
recheck_overlay(Text, {Type, _}, {row, _})
  when ((Type == column) orelse (Type == range)) -> {formula, Text};
%% check a row/row
recheck_overlay(Text, {row, {X1, X2}}, {row, {XX1, XX2}} = Tgt) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2) ->
            rewrite((X2 - X1 + 1), Tgt, Text, middle);
        true ->
            {formula, Text}
    end;
%% check a col/col
recheck_overlay(Text, {column, {Y1, Y2}}, {column, {YY1, YY2}} = Tgt) ->
    if
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) ->
            rewrite((Y2 - Y1 + 1), Tgt, Text, middle);
        true ->
            {formula, Text}
    end;
%% check range/range
recheck_overlay(Text, {range, {X1, Y1, X2, Y2}}, {range, {XX1, YY1, XX2, YY2}}) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2),
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) -> {deref, "#REF!"};
        true                                               -> {formula, Text}
    end;
%% check range/column
recheck_overlay(Text, {column, {X1, X2}}, {range, {XX1, _YY1, XX2, _YY2}} = Tgt) ->
    if
        (X1 >= XX1), (X1 =< XX2), (X2 >= XX1), (X2 =< XX2) ->
            rewrite((X2 - X1 + 1), Tgt, Text, middle_column);
        true -> {formula, Text}
    end;
%% check range/row
recheck_overlay(Text, {row, {Y1, Y2}}, {range, {_XX1, YY1, _XX2, YY2}} = Tgt) ->
    if
        (Y1 >= YY1), (Y1 =< YY2), (Y2 >= YY1), (Y2 =< YY2) ->
            rewrite((Y2 - Y1 + 1), Tgt, Text, middle_row);
        true -> {formula, Text}
    end.

expand({cell, {X, Y}})            -> {X, Y, X, Y};
expand({range, {X1, Y1, X2, Y2}}) -> {X1, Y1, X2, Y2};
expand({column, {X1, X2}})        -> {X1, zero, X2, inf}; % short for infinity
expand({row, {Y1, Y2}})           -> {zero, Y1, inf, Y2}; % short for infinity
expand({page, "/"})               -> {zero, inf, zero, inf}.

%% different to offset_formula because it truncates ranges
offset_fm_w_rng(Cell, [$=|Formula], From, Offset) ->
    %% the xfl_lexer:lex takes a cell address to lex against
    %% in this case {1, 1} is used because the results of this
    %% are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> NewToks = offset_with_ranges(Toks, Cell, From, Offset),
                         make_formula(NewToks);
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                       "formula in offset_fm_w_rng but "++
                                       "you do~n-~p~n", [Formula]),
                         {[], "=" ++ Formula}
    end;
offset_fm_w_rng(_Cell, Value, _From, _Offset) -> Value.

offset_formula(Formula, {XO, YO}) ->
    %% the xfl_lexer:lex takes a cell address to lex against
    %% in this case {1, 1} is used because the results of this
    %% are not actually going to be used here (ie {1, 1} is a dummy!)
    case catch(xfl_lexer:lex(super_util:upcase(Formula), {1, 1})) of
        {ok, Toks}    -> NewToks = d_n_d_c_n_p_offset(Toks, XO, YO),
                         {_St, NewFormula} = make_formula(NewToks),
                         NewFormula;
        _Syntax_Error -> io:format("Not sure how you get an invalid "++
                                       "formula in offset_formula but "++
                                       "you do~n-~p~n", [Formula]),
                         "=" ++ Formula
    end.

write_formula1(Ref, Fla, Formula, AReq, Attrs) ->
    Rti = refX_to_rti(Ref, AReq, false),
    case muin:run_formula(Fla, Rti) of
        %% TODO : Get rid of this, muin should return {error, Reason}?
        {ok, {_P, {error, error_in_formula}, _, _}} ->
            log:log("error_in_formula "++Formula),
            write_error_attrs(Ref, Formula, error_in_formula);
        {error, Error} ->
            log:log("error "++Formula++" - "++io_lib:format("~p", [Error])),
            write_error_attrs(Ref, Formula, Error);        
        {ok, {Pcode, {rawform, RawF, Html}, Parents, Recompile}} ->
            {Trans, Label} = RawF#form.id,
            Form = RawF#form{id={Ref#refX.path, Trans, Label}}, 
            ok = attach_form(Ref, Form),
            Attrs2 = orddict:store("__hasform", t, Attrs),
            write_formula_attrs(Attrs2, Ref, Formula, Pcode, Html, 
                                Parents, Recompile);
        {ok, {Pcode, Res, Parents, Recompile}} ->
            log:log("normal "++Formula++" - "++io_lib:format("~p", [Res])),
            write_formula_attrs(Attrs, Ref, Formula, Pcode, Res, 
                                Parents, Recompile)
    end.

write_formula_attrs(Attrs, Ref, Formula, Pcode, Res, Parents, Recompile) ->
    Parxml = map(fun muin_link_to_simplexml/1, Parents),
    {NewLocPs, _NewRemotePs} = split_local_remote(Parxml),
    ok = set_relations(Ref, NewLocPs),
    Align = default_align(Res),
    add_attributes(Attrs, [{"formula", Formula},
                           {"__rawvalue", Res},
                           {"__ast", Pcode},
                           {"__default-align", Align},
                           {"__recompile", Recompile}]).

write_error_attrs(Ref, Formula, error_in_formula) ->
    write_error_attrs(Ref, Formula, '#ERROR!');
write_error_attrs(Ref, Formula, Error) ->
    ok = set_relations(Ref, []),
    add_attributes(orddict:new(), [{"formula", Formula},
                                   {"__rawvalue", {errval, Error}},
                                   {"__ast", []}]).

default_align(Res) when is_number(Res) -> "right";
default_align(Res) when is_list(Res)   -> "left";
default_align(_Res)                    -> "center".

write_formula2(Ref, OrigVal, {Type, Val},
               {"text-align", Align}, 
               {"format", Format}, Attrs) ->
    Formula = case Type of
                  quote    -> [$' | Val];
                  datetime -> OrigVal;
                  float    -> OrigVal;
                  int      -> OrigVal;
                  _        -> hn_util:text(Val) end,
    {NewLocPs, _NewRemotePs} = split_local_remote([]),
    ok = set_relations(Ref, NewLocPs),
    Attrs2 = add_attributes(Attrs, [{"__default-align", Align},
                                    {"__rawvalue", Val},
                                    {"formula", Formula}]),
    case Format of
        "null" -> Attrs2;
        _      -> orddict:store("format", Format, Attrs2)
    end.

split_local_remote(List) -> split_local_remote1(List, {[], []}).

split_local_remote1([], Acc) -> Acc;
split_local_remote1([{_, [{_, "local"}], [Url]} | T], {A, B})  ->
    P2 = hn_util:url_to_refX(Url),
    split_local_remote1(T, {[P2 | A], B});
split_local_remote1([{_, [{_, "remote"}], [Url]} | T], {A, B}) ->
    P2 = hn_util:url_to_refX(Url),
    split_local_remote1(T, {A, [P2 | B]}).

add_attributes(D, []) -> D;
add_attributes(D, [{Key, Val}|T]) ->
    D2 = orddict:store(Key, Val, D),
    add_attributes(D2, T).

del_attributes(D, []) -> D; 
del_attributes(D, [Key|T]) ->
    D2 = orddict:erase(Key, D),
    del_attributes(D2, T).

copy_attributes(_SD, TD, []) -> TD;
copy_attributes(SD, TD, [Key|T]) ->
    case orddict:find(Key, SD) of
        {ok, V} -> copy_attributes(SD, orddict:store(Key, V, TD), T);
        _ -> copy_attributes(SD, TD, T)
    end.

-spec matching_forms(#refX{}, common | string()) -> [#form{}].
matching_forms(#refX{site=Site, path=Path}, Trans) -> 
    MS = [{#form{id = {Path, Trans, '_'}, _='_'}, [], ['$_']}],
    mnesia:select(trans(Site, form), MS).

-spec attach_form(#refX{}, #form{}) -> ok. 
attach_form(Ref=#refX{site=Site}, Form) ->
    Tbl = trans(Site, form),
    Idx = ref_to_idx_create(Ref),
    mnesia:write(Tbl, Form#form{key = Idx}, write).

-spec unattach_form(#refX{}, cellidx()) -> ok. 
unattach_form(#refX{site=Site}, Key) ->
    Tbl = trans(Site, form),
    mnesia:delete(Tbl, Key, write).
                 
%% @doc Convert Parents and DependencyTree tuples as returned by 
%% Muin into SimpleXML.
muin_link_to_simplexml({Type, {S, P, X1, Y1}}) ->
    Url = hn_util:index_to_url({index, S, P, X1, Y1}),
    {url, [{type, Type}], [Url]}.

%% this function is called when a new attribute is set for a style
-spec apply_style(#refX{}, {string(), term()}, ?dict) -> ?dict.
apply_style(Ref, {Name, Val}, Attrs) ->
    NewSIdx = case orddict:find("style", Attrs) of
                  {ok, Idx} -> based_style(Ref, Idx, Name, Val);
                  _         -> fresh_style(Ref, Name, Val)
              end,
    orddict:store("style", NewSIdx, Attrs).

-spec fresh_style(#refX{}, string(), any()) -> integer(). 
fresh_style(#refX{site=Site}=Ref, Name, Val) ->
    FieldNo = ms_util2:get_index(magic_style, Name),
    Tbl = trans(Site, style),
    MStyle = setelement(FieldNo + 1, #magic_style{}, Val),
    store_style(Ref, Tbl, MStyle).

-spec based_style(#refX{}, integer(), string(), any()) -> integer().
based_style(#refX{site=Site}=Ref, BaseIdx, Name, Val) ->
    Tbl = trans(Site, style),
    case mnesia:index_read(Tbl, BaseIdx, #style.idx) of
        [#style{magic_style = MStyle1}] ->
            FieldNo = ms_util2:get_index(magic_style, Name),
            MStyle2 = setelement(FieldNo + 1, MStyle1, Val),
            store_style(Ref, Tbl, MStyle2);
        _ ->
            BaseIdx %% <- strange..
    end.

-spec store_style(#refX{}, atom(), #magic_style{}) -> integer(). 
store_style(Ref, Tbl, MStyle) ->
    case mnesia:read(Tbl, MStyle, read) of
        [#style{idx = I}] -> 
            I; 
        _ ->
            I = util2:get_timestamp(),
            StyleRec = #style{magic_style = MStyle, idx = I},
            ok = tell_front_end_style(Ref, StyleRec),
            ok = mnesia:write(Tbl, StyleRec, write),
            I
    end.    

-spec tell_front_end_style(#refX{}, #style{}) -> ok. 
tell_front_end_style(Ref, Style) ->
    Tuple = {style, Ref, Style},
    tell_front_end1(Tuple).

-spec tell_front_end_change(#refX{}, ?dict) -> ok. 
tell_front_end_change(Ref, Attrs) ->
    Tuple = {change, Ref, Attrs},
    tell_front_end1(Tuple).

tell_front_end1(Tuple) ->
    List = get('front_end_notify'),
    put('front_end_notify', [Tuple | List]),
    ok.

-spec read_objs(#refX{}, inside | intersect | direct) -> [#local_obj{}]. 
read_objs(#refX{site=Site}=Ref, inside) ->
    MS = objs_inside_ref(Ref),
    mnesia:select(trans(Site, local_obj), MS);
read_objs(#refX{site=Site}=Ref, intersect) ->
    MS = objs_intersect_ref(Ref),
    mnesia:select(trans(Site, local_obj), MS);
read_objs(#refX{site=Site, path=P, obj = O}, direct) ->
    MS = [{#local_obj{path=P, obj = O, _='_'}, [], ['$_']}],
    mnesia:select(trans(Site, local_obj), MS).                            

objs_inside_ref(#refX{path = P, obj = {page, "/"}}) ->
    [{#local_obj{path = P, _='_'}, [], ['$_']}];
objs_inside_ref(#refX{path = P, obj = {column, {X1,X2}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{MX,_MY}}}) 
                     when MP == P,
                          X1 =< MX, MX =< X2 -> LO
               end);
objs_inside_ref(#refX{path = P, obj = {row, {R1,R2}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{_MX,MY}}}) 
                     when MP == P,
                          R1 =< MY, MY =< R2 -> LO
               end);
objs_inside_ref(#refX{path = P, obj = {range, {0,Y1,infinity,Y2}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{MX,MY}}}) 
                     when MP == P,
                          0 =< MX, MX =< infinity, 
                          Y1 =< MY, MY =< Y2 -> LO;
                  (LO=#local_obj{path=MP, obj={row,{MY,MY}}})
                     when MP == P,
                          Y1 =< MY, MY =< Y2 -> LO
               end);
objs_inside_ref(#refX{path = P, obj = {range, {X1,0,X2,infinity}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{MX,MY}}}) 
                     when MP == P,
                          X1 =< MX, MX =< X2, 
                          0 =< MY, MY =< infinity -> LO; 
                  (LO=#local_obj{path=MP, obj={column,{MX,MX}}})
                     when MP == P,
                          X1 =< MX, MX =< X2 -> LO
               end);
objs_inside_ref(#refX{path = P, obj = {range, {X1,Y1,X2,Y2}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{MX,MY}}}) 
                     when MP == P,
                          X1 =< MX, MX =< X2, 
                          Y1 =< MY, MY =< Y2 -> LO
               end);
objs_inside_ref(#refX{path = P, obj = O = {cell, _}}) ->
    [{#local_obj{path = P, obj = O, _='_'}, [], ['$_']}].

%% Note that this is most useful when given cells, or ranges. 
objs_intersect_ref(#refX{path = P, obj = {page, "/"}}) ->
    [{#local_obj{path=P, _='_'}, [], ['$_']}];
objs_intersect_ref(#refX{path = P, obj = {range, {X1,Y1,X2,Y2}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{MX,MY}}}) 
                     when MP == P,
                          X1 =< MX, MX =< X2,
                          Y1 =< MY, MY =< Y2 -> LO;
                  (LO=#local_obj{path=MP, obj={row,{MY,MY}}})
                     when MP == P,
                          Y1 =< MY, MY =< Y2 -> LO;
                  (LO=#local_obj{path=MP, obj={column, {MX,MX}}})
                     when MP == P,
                          X1 =< MX, MX =< X2 -> LO;
                  (LO=#local_obj{path=MP, obj={page, _}}) 
                     when MP == P -> LO
               end);
objs_intersect_ref(#refX{path = P, obj = {cell, {X,Y}}}) ->
    ets:fun2ms(
      fun(LO=#local_obj{path=MP, obj={cell,{MX,MY}}})
            when MP == P, MX == X, MY == Y -> LO;
         (LO=#local_obj{path=MP, obj={column,{MX,MX}}}) 
            when MP == P, MX == X -> LO; 
         (LO=#local_obj{path=MP, obj={row,{MY,MY}}}) 
            when MP == P, MY == Y -> LO;
         (LO=#local_obj{path=MP, obj={page, _}}) 
            when MP == P -> LO
      end);
objs_intersect_ref(#refX{path = P, obj = {column, {X1,X2}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{MX,_MY}}}) 
                     when MP == P,
                          X1 =< MX, MX =< X2 -> LO;
                  (LO=#local_obj{path=MP, obj={row,_}})
                     when MP == P -> LO;
                  (LO=#local_obj{path=MP, obj={page, _}}) 
                     when MP == P -> LO
               end);
objs_intersect_ref(#refX{path = P, obj = {row, {R1,R2}}}) ->
    ets:fun2ms(fun(LO=#local_obj{path=MP, obj={cell,{_MX,MY}}}) 
                     when MP == P,
                          R1 =< MY, MY =< R2 -> LO;
                  (LO=#local_obj{path=MP, obj={column, _}})
                     when MP == P-> LO;
                  (LO=#local_obj{path=MP, obj={page, _}}) 
                     when MP == P -> LO
               end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                              %
%% Debugging interface                                                          %
%%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deref_overlap_TEST() ->
    io:format("~n~n~n~n~n~n"),
    Tests =[
            %% cells match
            {"A1",    {cell, {1, 1}},        {cell, {1, 1}},        {deref, "#REF!"}},
            %% cells don't match
            {"A1",    {cell, {2, 1}},        {cell, {1, 1}},        {formula, "A1"}},
            %% cell in a range
            {"A1",    {cell, {1, 1}},        {range, {1, 1, 2, 3}}, {deref, "#REF!"}},
            %% cell not in a range
            {"A1",    {cell, {4, 4}},        {range, {1, 1, 2, 3}}, {formula, "A1"}},

            %% ranges match
            {"A1:B2", {range, {1, 1, 2, 2}}, {range, {1, 1, 2, 2}}, {deref, "#REF!"}},
            %% ranges don't overlap
            {"A1:B2", {range, {1, 1, 2, 2}}, {range, {3, 3, 4, 4}}, {formula, "A1:B2"}},
            %% target range inside delete range
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 5, 5}}, {deref, "#REF!"}},
            %% delete range inside target range
            {"B2:E5", {range, {2, 2, 5, 5}}, {range, {3, 3, 4, 4}}, {deref, "#REF!"}},
            %% delete range clips top-left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 2, 2}}, {deref, "#REF!"}},
            %% delete range clips bottom-right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {3, 3, 5, 5}}, {deref, "#REF!"}},
            %% delete range clips top-right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {4, 1, 5, 2}}, {deref, "#REF!"}},
            %% delete range clips bottom-left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 4, 2, 5}}, {deref, "#REF!"}},
            %% delete range slices left
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 2, 9}}, {recalc, "A2:B4"}},
            %% delete range slices top
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 1, 7, 2}}, {recalc, "B1:Dl2"}},
            %% delete range slices bottom
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {1, 4, 5, 9}}, {recalc, "B2:D3"}},
            %% delete range slices right
            {"B2:D4", {range, {2, 2, 4, 4}}, {range, {4, 1, 5, 9}}, {recalc, "B2:C4"}},

            %% cell in a column
            {"C5",    {cell, {3, 5}},        {column, {3, 4}},      {deref, "#REF!"}},
            %% cell not in a column
            {"C5",    {cell, {3, 5}},        {column, {6, 7}},      {formula, "C5"}},
            %% range in a column (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {3, 5}},      {deref, "#REF!"}},
            %% range in a column (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {2, 6}},      {deref, "#REF!"}},
            %% delete columns slices left (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {3, 3}},      {recalc, "C5:D8"}},
            %% delete columns slices left (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {2, 3}},      {recalc, "B5:C8"}},
            %% delete columns slices right (1)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {5, 5}},      {recalc, "C5:D8"}},
            %% delete columns slices right (2)
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {5, 6}},      {recalc, "C5:D8"}},
            %% delete column slices middle
            {"C5:E8", {range, {3, 5, 5, 8}}, {column, {4, 4}},      {recalc, "C5:D8"}},


            %% cell in a row
            {"C5",    {cell, {3, 5}},        {row, {4, 6}},         {deref, "#REF!"}},
            %% cell not in a row
            {"C5",    {cell, {3, 5}},        {row, {6, 7}},         {formula, "C5"}},
            %% range in a row (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {5, 8}},         {deref, "#REF!"}},
            %% range in a row (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {4, 9}},         {deref, "#REF!"}},
            %% delete row slices top (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {5, 5}},         {recalc, "C5:D7"}},
            %% delete row slices top (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {4, 5}},         {recalc, "C4:D6"}},
            %% delete row slices bottom (1)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {8, 8}},         {recalc, "C5:D7"}},
            %% delete row slices bottom (2)
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {8, 9}},         {recalc, "C5:D7"}},
            %% delete row slices middle
            {"C5:D8", {range, {3, 5, 4, 8}}, {row, {6, 7}},         {recalc, "C5:D6"}},

            %% columns can't be derefed by cell deletes
            {"C:F",   {column, {3, 6}},      {cell, {2, 3}},        {formula, "C:F"}},
            %% columns can't be derefed by range deletes
            {"C:F",   {column, {3, 6}},      {range, {2, 3, 4, 5}}, {formula, "C:F"}},
            %% columns can't be derefed by row deletes
            {"C:F",   {column, {3, 6}},      {row, {2, 3}},         {formula, "C:F"}},
            %% column inside a column delete (1)
            {"C:F",   {column, {3, 6}},      {column, {2, 7}},      {deref, "#REF!"}},
            %% column inside a column delete (2)
            {"C:F",   {column, {3, 6}},      {column, {3, 6}},      {deref, "#REF!"}},
            %% column not inside a column delete
            {"C:F",   {column, {3, 6}},      {column, {8, 9}},      {formula, "C:F"}},
            %% column delete slices left (1)
            {"C:F",   {column, {3, 6}},      {column, {3, 3}},      {recalc, "C:E"}},
            %% column delete slices left (2)
            {"C:F",   {column, {3, 6}},      {column, {2, 4}},      {recalc, "B:C"}},
            %% column delete slices right (1)
            {"C:F",   {column, {3, 6}},      {column, {6, 6}},      {recalc, "C:E"}},
            %% column delete slices right (2)
            {"C:F",   {column, {3, 6}},      {column, {5, 7}},      {recalc, "C:D"}},
            %% column delete slices middles
            {"C:F",   {column, {3, 6}},      {column, {5, 5}},      {recalc, "C:E"}},

            %% rows can't be derefed by cell deletes
            {"3:6",   {row, {3, 6}},         {cell, {2, 3}},        {formula, "3:6"}},
            %% rows can't be derefed by range deletes
            {"3:6",   {row, {3, 6}},         {range, {2, 3, 4, 5}}, {formula, "3:6"}},
            %% rows can't be derefed by column deletes
            {"3:6",   {row, {3, 6}},         {column, {2, 3}},      {formula, "3:6"}},
            %% row inside a row delete (1)
            {"3:6",   {row, {3, 6}},         {row, {3, 6}},         {deref, "#REF!"}},
            %% row inside a row delete (2)
            {"3:6",   {row, {3, 6}},         {row, {2, 7}},         {deref, "#REF!"}},
            %% row not inside a row delete
            {"3:6",   {row, {3, 6}},         {row, {8, 9}},         {formula, "3:6"}},
            %% row slices top (1)
            {"3:6",   {row, {3, 6}},         {row, {3, 3}},         {recalc, "3:5"}},
            %% row slices top (2)
            {"3:6",   {row, {3, 6}},         {row, {2, 3}},         {recalc, "2:4"}},
            %% row slices bottom (1)
            {"3:6",   {row, {3, 6}},         {row, {6, 6}},         {recalc, "3:5"}},
            %% row slices bottom (2)
            {"3:6",   {row, {3, 6}},         {row, {6, 7}},         {recalc, "3:5"}},
            %% row slices middle
            {"3:6",   {row, {3, 6}},         {row, {4, 4}},         {recalc, "3:5"}}
           ],
    [test_ov(X) || X <- Tests].

test_ov({Text, Cell, DelX, Return}) ->
    Return1 = deref_overlap(Text, DelX, Cell),
    case Return of
        Return1 -> ok; 
        _       -> io:format("Fail: ~p : ~p : ~p : ~p - ~p~n",
                             [Text, Cell, DelX, Return, Return1])
    end.
