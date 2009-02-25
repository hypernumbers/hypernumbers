%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @private
-module(hn_main).

-include("hypernumbers.hrl").
-include("spriki.hrl").
-include("regexp.hrl").
-include("handy_macros.hrl").
-include("errvals.hrl").
-include("muin_records.hrl").

-export([recalc/1,
         set_attribute/2,
         set_cell/2,
         get_cell_info/4,
         write_cell/5,
         get_hypernumber/9,
         copy_pages_below/2,
         formula_to_range/2,
         attributes_to_range/2,
         get_pages_under/1,
         value_to_cell/2]).

%% @spec set_attribute(Ref, Val) -> ok
%% @doc set an attribute on a reference, if the attribute name
%%      is formula / format, then processed
set_attribute(Ref = #ref{name=formula},Val) -> 
    set_cell(Ref,Val);
set_attribute(Ref = #ref{name=format}, Val) ->
    hn_db:write_item(Ref,Val),
    F = fun(X,[]) ->
                case hn_db:get_item_val(X#ref{name=rawvalue}) of
                    []    -> ok;
                    Value -> set_cell_rawvalue(X,Value)
                end
        end,
    apply_range(Ref,F,[]);
set_attribute(Ref,Val) ->
    hn_db:write_item(Ref,Val).

%% @spec set_cell(Addr, Val) -> ok
%% @doc process_formula
%% @todo this is a bug isn't it - hn_db:get_item will never
%% return 'true', innit?
set_cell(Addr, Val) ->
    case hn_db:get_item_val(Addr#ref{name = '__shared'}) of
        true -> throw({error, cant_change_part_of_array});
        _    -> value_to_cell(Addr, Val)
    end.

value_to_cell(Addr, Val) ->

    case superparser:process(Val) of
        {formula, Fla} ->
            Rti = ref_to_rti(Addr, false),
            case muin:run_formula(Fla, Rti) of
                {error, _Error} ->
                    % @TODO, notify clients
                    ok;       
                {ok, {Pcode, Res, Deptree, Parents, Recompile}} ->
                    Parxml = map(fun muin_link_to_simplexml/1, Parents),
                    Deptreexml = map(fun muin_link_to_simplexml/1, Deptree),

                    ?IF(Pcode =/= nil,     db_put(Addr, '__ast', Pcode)),
                    ?IF(Recompile == true, db_put(Addr, '__recompile', true)),
                    % write the default text align for the result
                    if
                        is_number(Res) ->
                            hn_db:write_item(Addr#ref{name='text-align'}, "right");
                        is_list(Res) -> 
                            hn_db:write_item(Addr#ref{name='text-align'}, "left");
                        true ->
                            hn_db:write_item(Addr#ref{name='text-align'}, "center")
                        end,
                    write_cell(Addr, Res, "=" ++ Fla, Parxml, Deptreexml)
            end;            
        [{Type, Value}, {'text-align', Align}, Format] ->
            % write out the alignment
            hn_db:write_item(Addr#ref{name='text-align'},Align),
            % write out the format (if any)
            case Format of
                {format, "null"} -> ok;
                {format, F}      -> hn_db:write_item(Addr#ref{name=format},F)
            end,
            % now write out the actual cell
            Formula = case Type of
                          quote    -> [39 | Value];
                          datetime -> Val;
                          _        -> hn_util:text(Value)
                      end,
            write_cell(Addr, Value, Formula, [], [])
       end.

%% @doc Process a formula in array mode.
formula_to_range(Formula, Ref = #ref{ref = {range, {TlCol, TlRow, BrCol, BrRow}}}) ->
    Rti = ref_to_rti(Ref, true),
    {formula, FormulaProcd} = superparser:process(Formula),
    {ok, {Pcode, Res, Parents, DepTree, Recompile}} = muin:run_formula(FormulaProcd, Rti),
    SetCell = fun({Col, Row}) ->
                      OffsetCol = Col - TlCol + 1,
                      OffsetRow = Row - TlRow + 1,
                      Value = case area_util:at(OffsetCol, OffsetRow, Res) of
                                  {ok, V}    -> V;
                                  {error, _} -> ?ERRVAL_NA
                              end,
                      ParentsXml = map(fun muin_link_to_simplexml/1, Parents),
                      DepTreeXml = map(fun muin_link_to_simplexml/1, DepTree),
                      Addr = Ref#ref{ref = {cell, {Col, Row}}},
                      db_put(Addr, '__ast', Pcode),
                      db_put(Addr, '__recompile', Recompile),
                      db_put(Addr, '__shared', true),
                      db_put(Addr, '__area', {TlCol, TlRow, BrCol, BrRow}),
                      write_cell(Addr, Value, Formula, ParentsXml, DepTreeXml)
              end,

    Coords = muin_util:expand_cellrange(TlRow, BrRow, TlCol, BrCol),
    foreach(SetCell, Coords).

%% @doc Apply list of attributes posted to a range.
attributes_to_range(Data, Ref = #ref{ref = {range,{Y1, X1, Y2, X2}}}) ->
    F = fun(X,Y,Z) ->
                case lists:nth(Z,Data) of
                    {_Name,[],[]} ->
                        ok;
                    {Name,[],[Val]} ->
                        NewRef = Ref#ref{ref={cell,{Y,X}},name=Name},
                        hn_main:set_attribute(NewRef,Val)
                end
        end,

    [[F(X,Y,((X-X1)*(Y2-Y1+1))+(Y-Y1)+1) 
      || Y <- lists:seq(Y1,Y2)] 
     || X <- lists:seq(X1,X2)].

%%%-----------------------------------------------------------------
%%% Function    : write_cell()
%%% Types       : 
%%% Description : writes a cell to the db, marks it
%%%               dirty (trigger recalculations) and update
%%%               references
%%%-----------------------------------------------------------------    
write_cell(Addr, Value, Formula, Parents, DepTree) ->

    Index = to_index(Addr),

    hn_db:write_item(Addr#ref{name=formula},Formula),
    set_cell_rawvalue(Addr,Value),

    % Delete attribute if empty, else store
    Set = fun(Ref,{xml,[]}) -> hn_db:remove_item(Ref);
             (Ref,Val)      -> hn_db:write_item(Ref,Val)
          end,

    Set(Addr#ref{name=parents},{xml,Parents}),
    Set(Addr#ref{name='dependancy-tree'},{xml,DepTree}),
    % Delete the references

    hn_db:del_links(Index,child),
    % probably to be cleaned up, go through the remote parents
    % to this cell, if they dont exist within the list of new
    % parents, delete it (and unregister)
    lists:foreach(
      fun(X) when is_record(X,remote_cell_link) ->
              Url  = hn_util:index_to_url(X#remote_cell_link.parent),
              case lists:member({url,[{type,"remote"}],[Url]},Parents) of
                  false -> 
                      hn_db:del_remote_link(X);
                  true  -> 
                      ok
              end;
         (_) -> ok
      end,
      hn_db:read_remote_links(Index,child,incoming)),

    % Writes all the parent links 
    lists:map( 
      fun({url,[{type,Type}],[Url]}) ->
              {ok,#ref{site=Site,path=Path,ref={cell,{X,Y}}}} = hn_util:parse_url(Url),
              Parent = {index,Site,string:to_lower(Path),X,Y},
              case Type of
                  "local"  -> 
                      hn_db:write_local_link(Parent,Index);
                  "remote" -> 
                      hn_db:write_remote_link(Parent,Index,incoming)
              end,
              ok
      end,
      Parents),

    %#index{row=Row,column=Col}=Index,
    hn_db:mark_dirty(Index,Value,cell),    
    ok.

set_cell_rawvalue(Addr,Value) ->
    hn_db:write_item(Addr#ref{name=rawvalue},Value),
    {ok,Format} = hn_db:get_item_inherited(Addr#ref{name=format}, "General"),
    {erlang,{_Type,Output}} = format:get_src(Format),
    {ok,{Color,V}}=format:run_format(Value,Output),
    hn_db:write_item(Addr#ref{name=value},V),
    hn_db:write_item(Addr#ref{name='overwrite-color'},atom_to_list(Color)),
    ok.

%%%-----------------------------------------------------------------
%%% Function    : apply_range/2
%%% Types       : 
%%% Description : Apply a function to a range of cells, the 
%%%               function called must take the address as the
%%%               first arg
%%%-----------------------------------------------------------------
apply_range(Addr,Fun,Args) ->
    case Addr#ref.ref of
        {range,{X1,Y1,X2,Y2}} ->
            lists:foreach( fun(X) -> 
                                   lists:foreach( fun(Y) ->
                                                          apply(Fun,[Addr#ref{ref={cell,{X,Y}}},Args])
                                                  end,lists:seq(Y1,Y2))
                           end,lists:seq(X1,X2));
        {cell,{_X,_Y}} ->
            apply(Fun,[Addr,Args])
%% @TODO : Add row / col / page?
    end.

%%%-----------------------------------------------------------------
%%% Function    : get_cell_info/2
%%% Types       : 
%%% Description : Provides the information required by the
%%%               formula parser about a cell, ie its direct
%%%               parents/ dependancy tree, and value
%%%-----------------------------------------------------------------
get_cell_info(Site, TmpPath, X, Y) ->

    Path = lists:filter(fun(Z) -> not(Z == $/) end, TmpPath),   
    Ref = #ref{site=string:to_lower(Site),path=Path,ref={cell,{X,Y}}},
    Value   = hn_db:get_item_val(Ref#ref{name=rawvalue}),

    DepTree = case hn_db:get_item_val(Ref#ref{name='dependancy-tree'}) of
                  {xml,Tree} -> Tree;
                  []         -> []
              end,

    Val = case Value of
              []                 -> blank;
              {datetime, _, [N]} -> muin_date:from_gregorian_seconds(N);
              Else               -> Else %% Strip other type tags.
          end,

    F = fun({url,[{type,Type}],[Url]}) -> 
                {ok,#ref{site=S,path=P,ref={cell,{X1,Y1}}}} = hn_util:parse_url(Url),
                P2 = string:tokens(P,"/"),
                {Type,{S,P2,X1,Y1}}
        end,

    Dep = lists:map(F,DepTree) ++ [{"local",{Site,Path,X,Y}}],
    {Val,Dep,[],[{"local",{Site,Path,X,Y}}]}.

%%%-----------------------------------------------------------------
%%% Function    : get_hypernumber/lots
%%% Types       : 
%%% Description : 
%%%-----------------------------------------------------------------
get_hypernumber(TSite,TPath,TX,TY,URL,FSite,FPath,FX,FY) ->
    
    NewTPath = lists:filter(fun(X) -> not(X == $/) end, TPath),
    NewFPath = lists:filter(fun(X) -> not(X == $/) end, FPath),

    To = #index{site=FSite,path=NewFPath,column=FX,row=FY},

    Fr = #index{site=TSite,path=NewTPath,column=TX,row=TY},

    case hn_db:get_hn(URL,Fr,To) of

        {error,permission_denied} ->
            {{errval,'#AUTH'},[],[],[]};

        #incoming_hn{value=Val,deptree=T} ->
            F = fun({url,[{type,Type}],[Url]}) ->

                        {ok,#ref{site=S,path=P,ref={cell,{X,Y}}}} = hn_util:parse_url(Url),
                        {Type,{S,P,X,Y}}
                end,

            Dep = lists:map(F,T) ++ [{"remote",{FSite,NewFPath,FX,FY}}],            
            {Val,Dep,[],[{"remote",{FSite,NewFPath,FX,FY}}]}
    end.

%%%-----------------------------------------------------------------
%%% Function    : recalc/1
%%% Types       : 
%%% Description : called when a (parent)dependancy changes, pull 
%%%               the abstract syntax tree of a cell and 
%%%               recalculate its value
%%%-----------------------------------------------------------------
recalc(Index) ->
    Addr = index_to_ref(Index),
    case hn_db:get_item_val(Addr#ref{name = '__shared'}) of
        true -> recalc_array(Index);
        _    -> recalc_cell(Index)
    end.

recalc_array(Index) ->
    Addr = index_to_ref(Index),
    {TlCol, TlRow, BrCol, BrRow} = hn_db:get_item_val(Addr#ref{name = '__area'}),
    Formula = hn_db:get_item_val(Addr#ref{name = formula}),
    Target = Addr#ref{ref = {range, {TlCol, TlRow, BrCol, BrRow}}},
    formula_to_range(Formula, Target),
    ok.

recalc_cell(Index) ->
    Addr = index_to_ref(Index),
    case hn_db:get_item_val(Addr#ref{name = '__recompile'}) of
        true ->
            set_cell(Addr, hn_db:get_item_val(Addr#ref{name = formula}));
        _ ->
            Pcode = hn_db:get_item_val(Addr#ref{name = '__ast'}),
            Rti = ref_to_rti(Addr, false),
            % @TODO Save Dependancy tree
            case muin:run_code(Pcode, Rti) of
                {ok, {_, Val, _, _, _}}  -> 
                    set_cell_rawvalue(Addr,Val),
                    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    % Logging code                                      %
                    % #index{row=Row,column=Col}=Index,                 %
                    % bits:log("Row,"++integer_to_list(Row)++",Col,"++  %
                    %         integer_to_list(Col)), 5                  %
                    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    hn_db:mark_dirty(Index, Val, cell);
                {error, _Reason} ->
                    ok
            end
    end,
    ok.

copy_pages_below(From = #ref{path=Root},To) ->

    Under     = get_pages_under(Root),
    ToFilter = filter_instance(From,Under,[]),
    Tree     = dh_tree:create(Under),
    NTree    = delete_map(Tree,ToFilter),
    {ok,NPages}   = dh_tree:flatlist(NTree),

    FromPages = [From#ref{path=lists:append([Root,X])} || X <- NPages],
    ToPages   = [To++string:join(X,"/")++"/" || X <- NPages],

    hn_main:set_attribute(From#ref{path=string:tokens(To,"/"),
                                   name=instance},hn_util:list_to_path(Root)),

    copy_pages(FromPages,ToPages).

delete_map(Tree,[]) -> 
    Tree;
delete_map(Tree,[H|T]) ->
    delete_map(dh_tree:erase(H,Tree),T).

filter_instance(_Ref,[],Acc) -> 
    Acc;
filter_instance(Ref,[H|T],Acc) ->
    case is_instance(Ref#ref{path=Ref#ref.path++H}) of
        true  -> filter_instance(Ref,T,[H|Acc]);
        false -> filter_instance(Ref,T,Acc)
    end.

is_instance(Ref = #ref{}) ->
    case hn_db:get_item_val(Ref#ref{name=instance}) of
        []     -> false;
        _Else  -> true
    end.

copy_pages([],[]) -> ok;
copy_pages([H1|T1],[H2|T2]) ->
    Attr = hn_yaws:get_page_attributes(H1),
    misc_util:do_import(H1#ref.site++H2,Attr),
    copy_pages(T1,T2).

%%%-----------------------------------------------------------------
%%% Helper Functions
%%%-----------------------------------------------------------------
db_put(_Addr,_Name,[]) -> ok;
db_put(Addr,Name,Value) ->
    hn_db:write_item(Addr#ref{name=Name},Value).

to_index(#ref{site=Site,path=Path,ref={cell,{X,Y}}}) ->
    #index{site=Site,path=Path,column=X,row=Y}.

%% @doc #index{} -> #ref{}
index_to_ref(_I = #index{site = Site, path = Path, column = Col, row = Row}) ->
    #ref{site = Site, path = Path, ref = {cell, {Col, Row}}}.

%% @doc Make a #muin_rti record out of a ref record and a flag that specifies 
%% whether to run formula in an array context.
ref_to_rti(#ref{site = Site, path = Path, ref = {cell, {Col, Row}}}, ArrayContext) when is_boolean(ArrayContext) ->
    #muin_rti{site = Site, path = Path, col = Col, row = Row, array_context = ArrayContext};
ref_to_rti(#ref{site = Site, path = Path, ref = {range, {Col, Row, _, _}}}, ArrayContext) when is_boolean(ArrayContext) ->
    #muin_rti{site = Site, path = Path, col = Col, row = Row, array_context = ArrayContext}.

%% @doc Convert Parents and DependencyTree tuples as returned by Muin into SimpleXML.
muin_link_to_simplexml({Type, {S, P, X1, Y1}}) ->
    Url = hn_util:index_to_url({index, S, P, X1, Y1}),
    {url, [{type, Type}], [Url]}.

%% There is a reason we use this instead of get_pages_under in muin
%% that one has a bug in it and will be rewritten anyway when we do queries properly
get_pages_under(Under) ->
    UnderClause=muin:make_bits(lists:reverse(Under),'$1'),
    % The head clause matches all subpages of the UnderClause
    Ref=ms_util:make_ms(ref,[{path,UnderClause},{name,rawvalue},
                             {ref,{cell,{'$2','$3'}}}]),
    Head=ms_util:make_ms(hn_item,[{addr,Ref},{val,'$4'}]),
    Guard = "",
    Body  = ['$1'],
    Spec=[{Head,Guard,Body}],
    _List=muin:unique(Spec)++[].
