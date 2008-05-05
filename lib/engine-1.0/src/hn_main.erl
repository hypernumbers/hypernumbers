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
set_attribute(R,Val) when R#ref.name == formula -> set_cell(R,Val);
set_attribute(Ref,Val) -> hn_db:write_item(Ref,Val).        

%%%-----------------------------------------------------------------
%%% Function    : set_cell/2
%%% Types       : 
%%% Description : process the string input to a cell
%%%-----------------------------------------------------------------
set_cell(Addr, Val) ->

    #ref{site=Site,path=Path,ref={cell,{X,Y}}} = Addr,

    case superparser:process(Val) of
    %% Formula
    {formula, Formula} ->  
        case muin:compile(Formula, {X, Y}) of
        {ok, Ast} ->
            case muin:run(Ast,[{site,Site},{path,Path},{x,X},{y,Y}]) of
            {ok, {Value, DepTree, _, Parents}} ->
               %% Store syntax tree
                db_put(Addr,"__ast",Ast),
                           
                %% Transform parents and deptree from tuple list to
                %% simplexml
                F = fun({Type,{S,P,X1,Y1}}) ->
                    Url = hn_util:index_to_url({index,S,P,X1,Y1}),
                    {url,[{type,Type}],[Url]}
                end,
                           
                NPar = lists:map(F,Parents),
                NDep = lists:map(F,DepTree),
                            
                write_cell(Addr, [hn_util:val_to_xml(Value)],
                    "="++Formula, NPar, NDep);
            
            {error, Reason} when is_atom(Reason) ->
                write_cell(Addr,[{error,[],[Reason]}],"="++Formula,[],[])
            end;
                
        {error, error_in_formula} ->
            write_cell(Addr,[{string,[],["Invalid Formula"]}],"="++Formula,[],[])
        end;
        
    {number, N} when is_integer(N) ->  
        write_cell(Addr, [{integer,[],[N]}], integer_to_list(N), [], []);
    {number, N} when is_float(N) ->  
        write_cell(Addr, [{float,[],[N]}], float_to_list(N), [], []);
    
    {string, S} ->  write_cell(Addr, [{string, [],[S]}], S, [], []);
    {bool, B} ->    write_cell(Addr, [{boolean,[],[B]}],atom_to_list(B), [], []);
    {error, E} ->   write_cell(Addr, E, E, [], [])
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
    hn_db:write_item(Addr#ref{name=value},Value),
        
    %% Delete attribute if empty, else store
    Set = fun(Ref,Val) ->
        case Val of
        [] -> hn_db:remove_item(Ref);
        _  -> hn_db:write_item(Ref,Val)
        end
    end,
           
    %% These arent written if empty
    Set(Addr#ref{name=parents},Parents),
    Set(Addr#ref{name='dependancy-tree'},DepTree),
        
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
            Parent = {index,Site,Path,X,Y},
            case Type of
            "local"  -> hn_db:write_local_link(Parent,Index);
            "remote" -> hn_db:write_remote_link(Parent,Index,incoming)
            end,
            ok
        end,
        Parents),

    hn_db:mark_dirty(Index,cell),    
    ok.
       
%%%-----------------------------------------------------------------
%%% Function    : get_cell_info/2
%%% Types       : 
%%% Description : Provides the information required by the
%%%               formula parser about a cell, ie its direct
%%%               parents/ dependancy tree, and value
%%%-----------------------------------------------------------------
get_cell_info(Site,Path,X,Y) ->

    Ref = #ref{site=Site,path=Path,ref={cell,{X,Y}}},
    
    Value   = get_val(hn_db:get_item(Ref#ref{name=value})),
    DepTree = get_val(hn_db:get_item(Ref#ref{name='dependancy-tree'})),   

    Val = case Value of
        []          -> blank;
        [{_,_,[V]}] -> V       %% Strip type info, might want in parser
    end,
       
    F = fun({url,[{type,Type}],[Url]}) ->
        #page{site=S,path=P,ref={cell,{X1,Y1}}} = hn_util:parse_url(Url),
        {Type,{S,P,X1,Y1}}
    end,
    
    Dep = lists:map(F,DepTree) ++ [{"local",{Site,Path,X,Y}}],
    
    {Val,Dep,[],[{"local",{Site,Path,X,Y}}]}.
       
%%%-----------------------------------------------------------------
%%% Function    : recalc/1
%%% Types       : 
%%% Description : called when a (parent)dependancy changes, pull 
%%%               the abstract syntax tree of a cell and 
%%%               recalculate its value
%%%-----------------------------------------------------------------
recalc(Index) ->
    #index{site=Site,path=Path,column=X,row=Y} = Index,
    Addr = #ref{ site=Site, path=Path, ref={cell,{X,Y}}},
    Ast  = hn_db:get_item_val(Addr#ref{name="__ast"}),
    
    V = case muin:run(Ast,[{site,Site},{path,Path},{x,X},{y,Y}]) of
        {ok,{Val, _, _, _}} -> [hn_util:val_to_xml(Val)];
        {error, Reason}     -> [{error,[],[Reason]}]
    end,
    
    hn_db:write_item(Addr#ref{name=value},V),
    hn_db:mark_dirty(Index,cell),
    ok.

%%%-----------------------------------------------------------------
%%% Function    : get_hypernumber/lots
%%% Types       : 
%%% Description : 
%%%-----------------------------------------------------------------
get_hypernumber(TSite,TPath,TX,TY,URL,FSite,FPath,FX,FY)->

    To = #index{site=FSite,path=FPath,column=FX,row=FY},
    Fr = #index{site=TSite,path=TPath,column=TX,row=TY},

    #incoming_hn{value=V,deptree=T} = hn_db:get_hn(URL,Fr,To),

    F = fun({url,[{type,Type}],[Url]}) ->
        #page{site=S,path=P,ref={cell,{X,Y}}} = hn_util:parse_url(Url),
        {Type,{S,P,X,Y}}
    end,
    
    Dep = lists:map(F,T) ++ [{"remote",{FSite,FPath,FX,FY}}],
    
    {V,Dep,[],[{"remote",{FSite,FPath,FX,FY}}]}.

%%%-----------------------------------------------------------------
%%% Helper Functions
%%%-----------------------------------------------------------------
get_val([]) -> [];
get_val([#hn_item{val=Value}]) -> Value.

db_put(_Addr,_Name,[]) -> ok;
db_put(Addr,Name,Value) ->
    hn_db:write_item(Addr#ref{name=Name},Value).
    
to_index(#ref{site=Site,path=Path,ref={cell,{X,Y}}}) ->
    #index{site=Site,path=Path,column=X,row=Y}.
            
