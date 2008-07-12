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
	 write_item/2,  
	 get_item/1,
	 get_item_val/1,  
	 get_item_inherited/2,
	 remove_item/1,
	 get_ref_from_name/1,
	 %% local_cell_link
	 read_links/2,   
	 del_links/2,
	 write_local_link/2,
	 read_remote_links/3, 
	 write_remote_link/3,
	 del_remote_link/1,
	 %% hypernumbers
	 register_hn/5, 
	 update_hn/4,
	 get_hn/3,
	 %% dirty tables
	 dirty_refs_changed/2,
	 mark_dirty/2
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
%% Function    : write_item/2
%%
%% Description : Adds an attribute to a reference addressed by Ref
%%--------------------------------------------------------------------
write_item(Addr,Val) when is_record(Addr,ref) ->

    Fun = fun() ->
        mnesia:write(#hn_item{addr = Addr, val = Val})
    end,
    {atomic, ok} = mnesia:transaction(Fun),

    %% Send a change notification to the remoting server, which 
    %% notifies web clients     
    spawn( fun() ->
        case Addr#ref.name of
        "__"++_ -> false;
        _ ->
            Msg = io_lib:format("change ~s",
                [simplexml:to_xml_string(
                    hn_util:item_to_xml(#hn_item{addr = Addr, val = Val})
                )]),
                
            gen_server:call(remoting_reg,{change,
                Addr#ref.site,Addr#ref.path,Msg},?TIMEOUT)        
        end
    end),
    
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
match_address(Pattern) ->
    {atomic, List} = mnesia:transaction(fun() ->
        mnesia:match_object(hn_item,#hn_item{addr=Pattern,_='_'},read)    
    end),
    List.
  
get_item(#ref{site=Site,path=Path,ref=Ref,name=Name}) ->
    
    {atomic, List} = mnesia:transaction(fun() ->
    
        %% If Name is defined, match it
        N = ?COND(Name == undef,'_',Name),
                
        Attr = case Ref of
        {cell,{X,Y}} -> #ref{site=Site, path=Path, name=N, ref={cell,{X,Y}}};
        _ ->            #ref{site=Site, path=Path, name=N, _ = '_'}
        end,
        Match = #hn_item{addr = Attr, _ = '_'},
        
        mnesia:match_object(hn_item,Match,read)
        
    end),

    case Ref of
    {cell,_} -> List;
    {page,_} -> List;
    _ -> 
        %% If a request for row / column or range, need to include all
        %% items contained within it
        lists:filter
        (
            fun(#hn_item{addr=#ref{ref=ItemRef}}) ->
                case {Ref, ItemRef} of
                {X,X}                     -> true; %% Same Ref
                {{row,Y},{cell,{_,Y}}}    -> true; %% Cell on same row
                {{column,X},{cell,{X,_}}} -> true; %% Cell on same col
                
                {{range,{_,Y1,_,Y2}},{row,Y}}
                    when Y > Y1 andalso Y < Y2 -> true; 
                {{range,{X1,_,X2,_}},{column,X}}
                    when X > X1 andalso X < X2 -> true;
                    
                {{range,{X1,Y1,X2,Y2}},{cell,{X,Y}}}
                    when Y >= Y1 andalso Y =< Y2 andalso
                            X >= X1 andalso X =< X2 -> true;
                            
                _ -> false
                end
            end, 
            List)
    end.
 
%% @doc Get the value of a named attribute, if it doesnt exist for address
%% check parent (cell -> range -> row -> column -> page -> root -> default)
get_item_inherited(Addr,Default) ->

    Return = case Addr#ref.ref of
    {cell,_}  -> get_cell_inh(Addr);
    {range,_} -> get_range_inh(Addr);
    {row,_}   -> get_row_inh(Addr);
    {col,_}   -> get_col_inh(Addr);
    {page,_}  -> get_page_inh(Addr)
    end,
    
    case Return of 
    {ok,Format} -> {ok,Format};
    nomatch     -> {ok,Default}
    end.

get_page_inh(Addr) ->
    get_page_inhx(Addr#ref{path=lists:reverse(Addr#ref.path)}).
    
get_page_inhx(#ref{site=Site,path=[],name=Name}) ->
    case match_address(#ref{site=Site,path=[],ref={page,"/"},name=Name}) of
    [] -> nomatch;
    [#hn_item{val=Val}] -> {ok,Val}
    end;
    
get_page_inhx(#ref{site=Site,path=[H|T],name=Name}) ->
    Path = lists:reverse([H|T]),
    match_stuff(
        #ref{site=Site,path=T,name=Name},
        #ref{site=Site,path=Path,ref={page,"/"},name=Name},
        fun get_page_inh/1).

get_col_inh(Addr) ->
    case Addr#ref.ref of 
    {cell,{_,Y}} -> match_stuff(Addr,Addr#ref{ref={col,Y}},fun get_page_inh/1);
    _            -> get_page_inh(Addr)
    end.

get_row_inh(Addr) ->
    case Addr#ref.ref of 
    {cell,{X,_}} -> match_stuff(Addr,Addr#ref{ref={row,X}},fun get_page_inh/1);
    _            -> get_page_inh(Addr)
    end.

get_cell_inh(Addr) ->
    match_stuff(Addr,Addr,fun get_range_inh/1).
    
get_range_inh(Addr) ->
    case match_address(Addr#ref{ref={range,'_'}}) of
    [] -> get_row_inh(Addr);
    List ->
        case filter_range(List,Addr#ref.ref) of
        nomatch -> get_row_inh(Addr);
        Val     -> {ok,Val}
        end
    end.

match_stuff(Addr,NewAddr,Fun) ->
    case match_address(NewAddr) of 
    [] -> Fun(Addr);
    [#hn_item{val=Val}] -> {ok,Val}
    end.

filter_range([],_Cell)   -> nomatch;  
filter_range([H|T],Cell) ->
    case hn_util:in_range((H#hn_item.addr)#ref.ref,Cell) of
    true -> H#hn_item.val;
    _    -> filter_range(T,Cell)
    end.
        
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
    
get_ref_from_name(Name) ->
    {atomic, [Item]} = mnesia:transaction(fun() ->  
        Match = #hn_item{addr=#ref{name=name, _ = '_'}, val = Name},
        mnesia:match_object(hn_item,Match,read)   
    end),
    Item#hn_item.addr. 
      
%%--------------------------------------------------------------------
%% Function    : get_item/1
%% 
%% Description : Returns the list of attributes that are contained
%%               within the range specified by Ref, ie if Ref refers
%%               to a cell, all atributes referring to that cell, if
%%               ref is a page, all cells / row / columns / ranges
%%               in that page are returned
%%--------------------------------------------------------------------    
remove_item(#ref{site=Site,path=Path,ref=Ref,name=Name}) ->
    
    {atomic, _Okay} = mnesia:transaction(fun() ->
    
        %% If Name is defined, match it
        N = ?COND(Name == undef,'_',Name),
        Attr  = #ref{site=Site, path=Path,ref=Ref, name=N, _ = '_'},
        Match = #hn_item{addr = Attr, _ = '_'},
        
        lists:map(
            fun(X) -> mnesia:delete_object(X) end,
            mnesia:match_object(hn_item,Match,read))
    end),
    ok.
      
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
    
%%--------------------------------------------------------------------
%% Function    : del_remote_link/1
%% 
%% Description : Delete a link between a local and remote cell, 
%%      send an unregister message so the child doesnt recieve
%%      any more updates to changes, and delete incomning record
%%--------------------------------------------------------------------
del_remote_link(Obj) when Obj#remote_cell_link.type == outgoing ->
    {atomic, ok} = mnesia:transaction(fun() ->
        Me = Obj#remote_cell_link.parent,
        mnesia:dirty_delete_object(Obj),
        case mnesia:match_object(#remote_cell_link{parent=Me,
            type=outgoing,_='_'}) of
        [] -> 
            [Hn] = mnesia:match_object(#outgoing_hn{index={'_',Me},_='_'}),
            mnesia:delete_object(Hn);
        _  -> ok
        end
    end),
    ok;

del_remote_link(Obj) when Obj#remote_cell_link.type == incoming ->

    {atomic, Hn} = mnesia:transaction(fun() ->
    
        Remote = Obj#remote_cell_link.parent,    
    
        %% Remove the relevant child attribute
        ParentRef = #ref{
            site=Remote#index.site,
            path=Remote#index.path,
            ref= {cell,{Remote#index.column,Remote#index.row}},
            name = children},
            
        ChildUrl = hn_util:index_to_url(Obj#remote_cell_link.child),
            
        Children = lists:filter( 
            fun(X) ->
                ?COND(X == {url,[{type,"remote"}],[ChildUrl]} , false, true)
            end,
            get_item_val(ParentRef)),
                    
        case Children of 
        [] -> hn_db:remove_item(ParentRef);
        _  -> hn_db:write_item(ParentRef,Children)
        end,
    
        mnesia:dirty_delete_object(Obj),
        [Hn] = mnesia:match_object(#incoming_hn{remote=Remote,_='_'}),

        case mnesia:match_object(#remote_cell_link{parent=Remote,
            type=incoming,_='_'}) of
        [] -> mnesia:delete_object(Hn);
        _  -> ok
        end,
        Hn
    end),
    
    Url   = hn_util:index_to_url(Obj#remote_cell_link.parent),
    Child = hn_util:index_to_url(Obj#remote_cell_link.child),
    Actions = simplexml:to_xml_string(
        {unregister,[],[
            {biccie,[],[Hn#incoming_hn.biccie]},
            {url,   [],[Child]}
        ]}),

    hn_util:post(Url++"?hypernumber",Actions,"text/xml"),
    
    ok.


%%--------------------------------------------------------------------
%% Function    : write_remote_link/2
%%
%% Description : Establishes a link between here and a remote cell
%%               if the link doesnt already exist, register with the
%%               remote cell to recieve updates
%%--------------------------------------------------------------------
write_remote_link(Parent,Child,Type) ->

    {atomic , {ok,Link}} = mnesia:transaction(fun()-> 
    
         ParentRef = #ref{
            site=Parent#index.site,
            path=Parent#index.path,
            ref= {cell,{Parent#index.column,Parent#index.row}},
            name = children},
            
        Children = [{url,[{type,"remote"}],[hn_util:index_to_url(Child)]}
            | get_item_val(ParentRef)],
            
        hn_db:write_item(ParentRef,{xml,Children}),   
    
        Link = #remote_cell_link{parent=Parent,child=Child,type=Type},
        case mnesia:match_object(Link) of
        [] ->
            mnesia:write(Link),
            [Hn] = mnesia:read({incoming_hn,Parent}),
            {ok,{register,Hn}};
        _->
            {ok,link_exists}
        end
    end),
    
    case Link of
    {register,Hn} ->

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
        
    {atomic, {ok,Remote,Local}} = mnesia:transaction(fun() ->

        %% Make a list of hypernumbers listening
        %% to this cell, (update outside transaction)
        Links = mnesia:match_object(#remote_cell_link{
            parent=Ref,type=outgoing,_='_'}),
        {ok,list_hn(Links,[]),read_links(Ref,parent)}
    end),

    Val = get_item_val((to_ref(Ref))#ref{name=rawvalue}),

    %% Update local cells
    lists:foreach(
        fun({local_cell_link, _, RefTo}) ->
            hn_main:recalc(RefTo)
        end,
        Local
    ),
    
    %Update Remote Hypernumbers
    lists:foreach(
        fun(Cell) ->
            notify_remote_change(Cell,Val)
        end,
        Remote),
             
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
            Links),       
        mnesia:delete({dirty_hypernumber, Ref})
    end),
    ok.   

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

        mnesia:write(Obj#incoming_hn{value=hn_util:xml_to_val(Val)}),
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
get_hn(Url,_From,To)->

    {atomic, List} = mnesia:transaction(fun() ->

        case mnesia:read({incoming_hn,To}) of

        [Hn] -> Hn;
        
		[]->	
            XML = hn_util:req(Url),
            
             %% TODO: Handle remote server being down            
            {hypernumber,[],[
                {value,[],              [Val]},
                {'dependancy-tree',[],  Tree}]
            } = simplexml:from_xml_string(XML),
            
            HNumber = #incoming_hn{
                value   = hn_util:xml_to_val(Val),
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
    error_logger:error_msg("in hn_db:notify_remote_change *WARNING* notify remote change not using "++
	      "version number ~p - ie it aint working - yet :(",[Version]),

    Actions = simplexml:to_xml_string(
        {notify,[],[
            {biccie,      [],[Hn#outgoing_hn.biccie]},
            {cell,        [],[hn_util:index_to_url(Cell)]},
            {type,        [],["change"]},
            {value,       [],hn_util:to_xml(Value)},
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
    

