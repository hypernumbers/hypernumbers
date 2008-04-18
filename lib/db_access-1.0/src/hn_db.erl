%%%-------------------------------------------------------------------
%%% File        : hn_db.erl
%%% Author      : Dale Harvey <dale@hypernumbers.com>
%%% Description : Handles the main database operations
%%%-------------------------------------------------------------------
-module(hn_db).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("spriki.hrl").
-include("handy_macros.hrl").

%%%-----------------------------------------------------------------
%%% Exported Functions
%%%-----------------------------------------------------------------
-export([
    %% hn_item
    add_item/2,     get_item/1,     get_item_val/1,
    %% local_cell_link
  	read_links/2,   del_links/2,    write_local_link/2,
  	read_remote_links/3, 
  	write_remote_link/3,
  	del_remote_link/1,
    %% hypernumbers
    register_hn/5,  update_hn/4,    get_hn/3,
  	%% dirty tables
    dirty_refs_changed/2, get_first_dirty/1, mark_dirty/2
    ]).

%%-------------------------------------------------------------------
%% Table : hn_item
%% Def   : record( hn_item, { addr = #attr_addr{}, val = [] }).
%% Desc  : Stores items of information against an address, which 
%%         can be a row / cell / column /range or page.
%%         The value should be an int / string or the simplexml
%%         format
%%-------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function    : add_item/2
%%
%% Description : Adds an attribute to a reference addressed by Ref
%%--------------------------------------------------------------------
add_item(Addr,Val) when is_record(Addr,ref) ->

    Fun = fun() ->
        mnesia:write(#hn_item{addr = Addr, val = Val})
    end,
    {atomic, ok} = mnesia:transaction(Fun),
    
    %% Send a change notification to the remoting server, which 
    %% notifies web clients 
    V = hn_util:text(Val),
    case io_lib:deep_char_list(V) of
    false -> ok;
    true  ->
    
        #ref{site=Site,path=Path,ref={Type,Ref},name=Name} = Addr,
        
        Msg = io_lib:format("change ~s ~s ~s ~s",
            [hn_util:text(Type),
             hn_util:ref_to_str({Type,Ref}),
             hn_util:text(Name),
             hn_util:text(V)]),
            
        gen_server:call(remoting_reg,{change,Site,Path,Msg},?TIMEOUT)
        
    end,
            
    ok.
    
%%--------------------------------------------------------------------
%% Function    : get_item/1
%% 
%% Description : Returns the list of attributes that are contained
%%               within the range specified by Ref, ie if Ref refers
%%               to a cell, all atributes referring to that cell, if
%%               ref is a page, all cells / row / columns / ranges
%%               in that page are returned
%%--------------------------------------------------------------------    
get_item(#ref{site=Site,path=Path,ref=Ref,name=Name}) ->
    
    {atomic, List} = mnesia:transaction(fun() ->
    
        %% If Name is defined, match it
        N = ?COND(Name == undef,'_',Name),

        Attr  = #ref{site=Site, path=Path, name=N, _ = '_'},
        Match = #hn_item{addr = Attr, _ = '_'},
        List  = mnesia:match_object(hn_item,Match,read),
        
        lists:filter
        (
            fun(#hn_item{addr=#ref{ref=ItemRef}}) ->
            
                case {Ref, ItemRef} of
                
                {{page,_},_}            -> true; %% All Attr on that Page
                {X,X}                   -> true; %% Same Ref
                {{row,Y},{cell,_,Y}}    -> true; %% Cell on same row
                {{column,X},{cell,X,_}} -> true; %% Cell on same col
                
                {{range,{_,Y1,_,Y2}},{row,Y}}
                    when Y > Y1 andalso Y < Y2 -> true; 
                {{range,{X1,_,X2,_}},{column,X}}
                    when X > X1 andalso X < X2 -> true;
                    
                {{range,{X1,Y1,X2,Y2}},{cell,X,Y}}
                    when Y > Y1 andalso Y < Y2 andalso
                         X > X1 andalso X < X2 -> true;
                         
                _ -> false
                end
            end,
            List
        )
    end),
    
    List.
    
%%--------------------------------------------------------------------
%% Function    : get_item_val/2
%% 
%% Description : Use this to get the value of an item when only 
%%               one item will be returned (there can only be 
%%               one value per name specified)
%%--------------------------------------------------------------------   
get_item_val(Addr) ->
    case get_item(Addr) of
    [] -> [];
    [#hn_item{val=Value}] -> Value
    end.
      
%%-------------------------------------------------------------------
%% Table : local_cell_link
%% Def   : record(local_cell_link, {parent = #index{}, child = #index{}}).
%% Desc  : Creates a link between 2 cells, the child is using the
%%         parents value in a calculation
%%-------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function    : write_local_link/2
%%
%% Description : 
%%--------------------------------------------------------------------
write_local_link(Parent,Child) ->

    {atomic , ok} = mnesia:transaction(fun()-> 
        mnesia:write(#local_cell_link{ parent=Parent, child=Child })
    end),  
	ok.	

%%--------------------------------------------------------------------
%% Function    : read_links/2
%%
%% Description : 
%%--------------------------------------------------------------------
read_links(Index, Relation) ->
    
    Obj = ?COND(Relation == child,
        {local_cell_link,'_',Index},
        {local_cell_link,Index,'_'}),
    
    {atomic, List} = mnesia:transaction(fun() ->
        mnesia:match_object(Obj)
    end),
    List.

%%--------------------------------------------------------------------
%% Function    : del_links/1
%% 
%% Description : Delete links between a cell
%%--------------------------------------------------------------------
del_links(Index, Relation) ->

    Obj = ?COND(Relation == child,
        {local_cell_link,'_',Index},
        {local_cell_link,Index,'_'}),
    
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:dirty_delete_object(Obj)
    end),
    ok.
    
    
del_remote_link(Obj) ->

    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:dirty_delete_object(Obj)
    end),
    
    %% TODO: Unregister
    
    ok.


%%--------------------------------------------------------------------
%% Function    : write_local_link/2
%%
%% Description : 
%%--------------------------------------------------------------------
write_remote_link(Parent,Child,Type) ->

    {atomic , {ok,Link}} = mnesia:transaction(fun()-> 
    
        Link = #remote_cell_link{parent=Parent,child=Child,type=Type},
        case mnesia:match_object(Link) of
        [] ->
            mnesia:write(Link),
            [Hn] = mnesia:read({incoming_hn,Parent}),
            {ok,{write_link,Hn}};
        _->
            {ok,link_exists}
        end
    end),
    
    case Link of
    {write_link,Hn} ->

        Url   = hn_util:index_to_url(Child),
        Proxy = Child#index.site ++ Child#index.path,
        Actions = simplexml:to_xml_string(
        {register,[],[
            {biccie,[],[Hn#incoming_hn.biccie]},
            {proxy, [],[Proxy]},
            {url,   [],[Url]}
        ]}),
        
        PUrl = hn_util:index_to_url(Parent),
        hn_util:post(PUrl++"?hypernumber",Actions,"text/xml"),
        ok;
        
    _-> ok
    end.

%%--------------------------------------------------------------------
%% Function    : read_links/2
%%
%% Description : 
%%--------------------------------------------------------------------
read_remote_links(Index, Relation,Type) ->
    
    Obj = ?COND(Relation == child,
        {remote_cell_link,'_',Index,Type},
        {remote_cell_link,Index,'_',Type}),
    
    {atomic, List} = mnesia:transaction(fun() ->
        mnesia:match_object(Obj)
    end),
    List.		
	
%%-------------------------------------------------------------------
%% Table : dirty_cell , dirty_hypernumbers
%% Def   : 
%% Desc  : 
%%-------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function    : dirty_refs_changed/2
%%
%% Description : This is called from dirty_srv when a number changes
%%--------------------------------------------------------------------
%% This is called when a cell changes, update other cells using
%% it and outgoing hypernumbers
dirty_refs_changed(dirty_cell, Ref) ->
    
    Val = get_item_val((to_ref(Ref))#ref{name=value}),
    
    {atomic, {ok,Outgoing}} = mnesia:transaction(fun() ->
            
        %% Update local cells
        lists:foreach(
            fun({local_cell_link, _, RefTo}) ->
                hn_main:recalc(RefTo)
            end,
            read_links(Ref,parent) 
        ),
        
        mnesia:delete({dirty_cell, Ref}),
        
        %% Make a list of hypernumbers listening
        %% to this cell, (update outside transaction)
        Links = mnesia:match_object(#remote_cell_link{
            parent=Ref,type=outgoing,_='_'}),
        {ok,list_hn(Links,[])}
    end),
        
    %Update Remote Hypernumbers
    lists:foreach(
        fun(Cell) ->
            notify_remote_change(Cell,Val)
        end,
        Outgoing),
    
    ok;

%% This is called when a remote hypernumber changes
dirty_refs_changed(dirty_hypernumber, Ref) ->

    {atomic, ok} = mnesia:transaction(fun() ->

        Links = mnesia:match_object(#remote_cell_link{
            parent=Ref,type=incoming,_='_'}),

        lists:foreach(
            fun(To) ->    
                Cell = To#remote_cell_link.child,
 
                hn_main:recalc(Cell)
            end,
            Links
        ),       
        mnesia:delete({dirty_hypernumber, Ref})
    end),
    ok.   

%%--------------------------------------------------------------------
%% Function    : get_first_dirty/1
%%
%% Description : reads the first value out of the dirty table
%%--------------------------------------------------------------------
get_first_dirty(Table)->
    {atomic, List} = mnesia:transaction(
        fun()-> 
            case mnesia:dirty_first(Table) of
            '$end_of_table' -> [];
            Key -> mnesia:dirty_read(Table,Key)
	        end
	    end),   
    List.

%%--------------------------------------------------------------------
%% Function    : mark_dirty/2
%%
%% Description : Sets a cell or hypernumber to be 'dirty', this 
%%               means cells that use its value needs to be 
%%               recalculated (this triggers the recalc)
%%--------------------------------------------------------------------
mark_dirty(Index,Type) ->

    Obj = case Type of
    cell ->         #dirty_cell{index=Index};
    hypernumber ->  #dirty_hypernumber{index=Index}
    end,

    {atomic, _Okay} = mnesia:transaction(fun() ->
        mnesia:write(Obj)
    end),
    ok.

%%-------------------------------------------------------------------
%% Table : incoming_hn , outgoing_hn
%% Def   : 
%% Desc  : These tables store hypernumbers, incoming stores the
%%         value of a remote number, the value is changed on notify
%%         and cells use the value from here. outgoing stores a 
%%         list of cells that need to be notified when cells on
%%         this server change
%%-------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function    : update_hn/5
%%
%% Description : updates the value of a hypernumber and then triggers
%%               the recalc, this is ran when an external site 
%%               notifies us their number changed
%%--------------------------------------------------------------------
update_hn(From,Bic,Val,_Version)->

    {atomic, ok} = mnesia:transaction(fun() ->

        Index = hn_util:page_to_index(hn_util:parse_url(From)),
        Rec   = #incoming_hn{ remote = Index, biccie = Bic, _='_'},
        [Obj] = mnesia:match_object(Rec),

        V = ?COND(hn_util:is_numeric(Val) == true,
            util2:make_num(Val),util2:make_text(Val)),

        mnesia:write(Obj#incoming_hn{value=V}),
        mark_dirty(Index,hypernumber),

        ok
        
    end),
    
    ok.

%%--------------------------------------------------------------------
%% Function    : get_hn/3
%%
%% Description : This reads the hypernumber table and returns the
%%               current value of a hypernumber. if it doesnt 
%%               exist, make a http call to the site to 
%%               fetch its value, a 'biccie' is created which
%%               must be quoted in registrations etc
%%--------------------------------------------------------------------
get_hn(Url,From,To)->

    {atomic, List} = mnesia:transaction(fun() ->

        case mnesia:read({incoming_hn,To}) of

        [Hn] -> Hn;
        
		[]->	  
            XML = hn_util:req(Url++"?hypernumber"),
            %% TODO: Handle remote server being down
            {hypernumber,[],[
                {value,[],              [Val]},
                {'dependancy-tree',[],  Tree}]
            } = simplexml:from_xml_string(XML),
            
            V = ?COND(hn_util:is_numeric(Val) == true,
                util2:make_num(Val),util2:make_text(Val)),

            HNumber = #incoming_hn{
                value   = V,
                deptree = Tree,
                remote  = To,
                biccie  = util2:get_biccie()},
                
            mnesia:write(HNumber),   
            HNumber
        end
    end),
    List.
    
%%--------------------------------------------------------------------
%% Function    : register_hn/3
%%
%% Description : Register to recieve updates to from a hypernumber
%%               on a remote server when its value changes
%%--------------------------------------------------------------------
register_hn(To,From,Bic,Proxy,Url) -> 

    {atomic , _Ok} = mnesia:transaction(fun()-> 

        mnesia:write(#remote_cell_link{
            parent=To,
            child=From,
            type=outgoing }),
    
        mnesia:write(#outgoing_hn{
            index  = {Proxy,To},
            biccie = Bic,
            url    = Url}),
        ok
    end),  
	ok.
	
%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
notify_remote_change(Hn,Value) ->
    
    {Server,Cell} = Hn#outgoing_hn.index,
    Version = hn_util:text(Hn#outgoing_hn.version + 1),

    Actions = simplexml:to_xml_string(
        {notify,[],[
            {biccie,      [],[Hn#outgoing_hn.biccie]},
            {cell,        [],[hn_util:index_to_url(Cell)]},
            {type,        [],["change"]},
            {value,       [],[hn_util:text(Value)]},
            {version,     [],["1"]}
        ]}),
 
    hn_util:post(Server,Actions,"text/xml"),
    ok.
    
to_ref(#index{site=Site,path=Path,column=X,row=Y}) ->
    #ref{site=Site,path=Path,ref={cell,{X,Y}}}.
    
%% Given a list of remote cells, return a list of 
%% related outgoing_hn's
list_hn([],List) -> List;    
list_hn([H|T],List) ->
    Cell = H#remote_cell_link.parent,
    [Hn] = mnesia:match_object(#outgoing_hn{index={'_',Cell},_='_'}),    
    list_hn(T,hn_util:add_uniq(List,Hn)).    
    
