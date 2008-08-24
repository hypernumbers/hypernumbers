%%%-----------------------------------------------------------------------------
%%% File        : hn_main.erl
%%% Author      : Dale Harvey <dale@hypernumbers.com>
%%% Description : Handles main hypernumbers logic
%%%-----------------------------------------------------------------------------

-module(hn_main).

-include("spriki.hrl").
-include("regexp.hrl").
-include("handy_macros.hrl").

-export([
    recalc/1,
    set_attribute/2, 
    set_cell/2,
    get_cell_info/4,
    write_cell/5,
    get_hypernumber/9 
    ]).

%%%-----------------------------------------------------------------
%%% Exported Functions
%%%-----------------------------------------------------------------
%%%-----------------------------------------------------------------
%%% Function    : set_attribute/2
%%% Types       : 
%%% Description : Set an attribute on a reference, the reference
%%%               can be a row / column / page / cell, and the 
%%%               attribute is a name / value pairs, if the name
%%%               is 'formula',then the value is processed as the 
%%%               formula of a cell
%%%-----------------------------------------------------------------
set_attribute(Ref,Val) when Ref#ref.name == formula -> 
    set_cell(Ref,Val);
set_attribute(Ref,Val) when Ref#ref.name == format ->
    hn_db:write_item(Ref,Val),
    F = fun(X,[]) ->
                case hn_db:get_item_val(X#ref{name=rawvalue}) of
                    [] -> 
			ok;
                    Value -> 
			set_cell_rawvalue(X,Value)
                end
        end,
    apply_range(Ref,F,[]);
set_attribute(Ref,Val) -> 
    hn_db:write_item(Ref,Val).

%%%-----------------------------------------------------------------
%%% Function    : set_cell/2
%%% Types       : 
%%% Description : process the string input to a cell
%%%-----------------------------------------------------------------
set_cell(Addr, Val) ->
    case superparser:process(Val) of
        {formula, Fla} ->
            case muin:run_formula(Fla, Addr) of
                {error,_Error} -> ok;       
                {Pcode, Res, Parents, Deptree, Recompile} ->
                    %% Convert stuff to SimpleXML.
                    F = fun({Type, {S, P, X1, Y1}}) ->
        			Url = hn_util:index_to_url({index, S, P, X1, Y1}),
        			{url, [{type, Type}], [Url]}
        		end,

                    Parxml = map(F, Parents),
                    Deptreexml = map(F, Deptree),

                    ?IF(Pcode =/= nil,     db_put(Addr, "__ast", Pcode)),
                    ?IF(Recompile == true, db_put(Addr, "__recompile", true)),
                    write_cell(Addr, Res, "=" ++ Fla, Parxml, Deptreexml)
            end;
            
        {_Type, Value} ->
            write_cell(Addr, Value, hn_util:text(Value), [], [])
    end.
    
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
        
    %% Delete attribute if empty, else store
    Set = fun(Ref,Val) ->
		  case Val of
		      {xml,[]} -> 
			  hn_db:remove_item(Ref);
		      _ -> 
			  hn_db:write_item(Ref,Val)
		  end
    end,
           
    Set(Addr#ref{name=parents},{xml,Parents}),
    Set(Addr#ref{name='dependancy-tree'},{xml,DepTree}),
    
    %% Delete the references
    hn_db:del_links(Index,child),

    %% probably to be cleaned up, go through the remote parents
    %% to this cell, if they dont exist within the list of new
    %% parents, delete it (and unregister)
    lists:foreach(
      fun(X) when is_record(X,remote_cell_link) ->
	      Url  = hn_util:index_to_url(X#remote_cell_link.parent),
	      case lists:member({url,[{type,"remote"}],[Url]},Parents) of
		  false -> hn_db:del_remote_link(X);
		  true  -> ok
	      end;
	 (_) -> ok
      end,
      hn_db:read_remote_links(Index,child,incoming)),
    
    %% Writes all the parent links 
    lists:map( 
      fun({url,[{type,Type}],[Url]}) ->
	      #page{site=Site,path=Path,ref={cell,{X,Y}}} = hn_util:parse_url(Url),
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
    
    hn_db:mark_dirty(Index,cell),    
    ok.
    
set_cell_rawvalue(Addr,Value) ->
    hn_db:write_item(Addr#ref{name=rawvalue},Value),
    {ok,Format} = hn_db:get_item_inherited(Addr#ref{name=format}, "General"),
    {erlang,{_Type,Output}} = format:get_src(Format),
    {ok,{Color,V}}=format:run_format(Value,Output),
    hn_db:write_item(Addr#ref{name=value},V),
    hn_db:write_item(Addr#ref{name=color},atom_to_list(Color)),
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
    %% TODO : Add row / col / page?
    end.
    
%%%-----------------------------------------------------------------
%%% Function    : get_cell_info/2
%%% Types       : 
%%% Description : Provides the information required by the
%%%               formula parser about a cell, ie its direct
%%%               parents/ dependancy tree, and value
%%%-----------------------------------------------------------------
get_cell_info(Site, TmpPath, X, Y) ->

    Path = lists:filter(fun(Z) -> ?COND(Z==47,false,true) end,TmpPath),   
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
                #page{site=S,path=P,ref={cell,{X1,Y1}}} = hn_util:parse_url(Url),
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

    NewTPath = lists:filter(fun(X) -> ?COND(X==47,false,true) end,TPath),   
    NewFPath = lists:filter(fun(X) -> ?COND(X==47,false,true) end,FPath),   

    To = #index{site=FSite,path=NewFPath,column=FX,row=FY},
    Fr = #index{site=TSite,path=NewTPath,column=TX,row=TY},

    case hn_db:get_hn(URL,Fr,To) of
        {error,permission_denied} ->
            {{errval,'#AUTH'},[],[],[]};
        
        #incoming_hn{value=Val,deptree=T} ->
            F = fun({url,[{type,Type}],[Url]}) ->
                        #page{site=S,path=P,ref={cell,{X,Y}}} = hn_util:parse_url(Url),
                        P2 = string:tokens(P,"/"),
                        {Type,{S,P2,X,Y}}
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
    #index{site=Site, path=Path, column=X, row=Y} = Index,
    Addr = #ref{site=Site, path=Path, ref={cell, {X, Y}}},

    %% Muin flags cells to force full recompile if parents may change.
    case hn_db:get_item_val(Addr#ref{name="__recompile"}) of
        true ->
            set_cell(Addr,hn_db:get_item_val(Addr#ref{name=formula}));
        _ ->
            Pcode = hn_db:get_item_val(Addr#ref{name="__ast"}),
            Val = case muin:run_code(Pcode, Addr) of
                      {ok, {V, _, _, _, _}} ->                V;
                      {error, Reason} when is_atom(Reason) -> Reason
                  end,
            set_cell_rawvalue(Addr,Val)
    end,
    
    hn_db:mark_dirty(Index, cell),
    ok.

%%%-----------------------------------------------------------------
%%% Helper Functions
%%%-----------------------------------------------------------------
db_put(_Addr,_Name,[]) -> ok;
db_put(Addr,Name,Value) ->
    hn_db:write_item(Addr#ref{name=Name},Value).
    
to_index(#ref{site=Site,path=Path,ref={cell,{X,Y}}}) ->
    #index{site=Site,path=Path,column=X,row=Y}.
