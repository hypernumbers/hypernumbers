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

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
    %% hn_item
    add_item/2,     get_item/1,     get_item_val/1,
    %% link_cell
  	write_link/2,   read_links/2,    del_links/2,
  	%% dirty tables
    dirty_refs_changed/2,
	get_first_dirty/1,
	mark_dirty/2,
  
    register_hn/5,
    update_hn/4,
	get_hypnum/3
    ]).

%%%-----------------------------------------------------------------
%%% Exported Functions
%%%-----------------------------------------------------------------

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
%% Table : link_cell
%% Def   : record(link_cell, {parent = #index{}, child = #index{}}).
%% Desc  : Creates a link between 2 cells, the child is using the
%%         parents value in a calculation
%%-------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function    : write_link/2
%%
%% Description : 
%%--------------------------------------------------------------------
write_link(Parent,Child) ->

    {atomic , ok} = mnesia:transaction(fun()-> 
        mnesia:write(#link_cell{ parent=Parent, child=Child })
    end),  
	ok.	

%%--------------------------------------------------------------------
%% Function    : read_links/2
%%
%% Description : 
%%--------------------------------------------------------------------
read_links(Index, Relation) ->
    
    Obj = ?COND(Relation == child,
        {link_cell,'_',Index},
        {link_cell,Index,'_'}),
    
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
        {link_cell,'_',Index},
        {link_cell,Index,'_'}),
    
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:dirty_delete_object(Obj)
    end),
    ok.

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
    
    {atomic, ok} = mnesia:transaction(fun() ->
    
        %% Update Remote Hypernumbers
        lists:foreach(
            fun(Hn) ->                    
                notify_remote_change(Hn)
            end,
            mnesia:match_object(#outgoing_hn{local=Ref,_='_'})
        ),
        
        io:format("ref ~p~n",[Ref]),
        io:format("refs ~p~n",[read_links(Ref,parent)]),
        %% Update local cells
        lists:foreach(
            fun({link_cell, _, RefTo}) ->
                hn_main:recalc(RefTo)
            end,
            read_links(Ref,parent) 
        ),
        
        mnesia:delete({dirty_cell, Ref})
        
    end),
    
    ok;

%% This is called when a remote hypernumber changes
dirty_refs_changed(dirty_hypernumbers, Ref) ->

    {atomic, List} = mnesia:transaction(fun() ->
    
        [Obj] = mnesia:match_object(
            #incoming_hn{remote=Ref, _='_'}),       
        
        lists:foreach(
            fun(To) ->                          
                hn_main:recalc(To)
            end,
            Obj#incoming_hn.cells
        ),       
        mnesia:delete({dirty_hypernumbers, Ref})
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

    Rec = ?COND(Type == cell,
        #dirty_cell{index=Index},
        #dirty_hypernumbers{index=Index}),

    {atomic, _Okay} = mnesia:transaction(fun() ->
        mnesia:write(Rec)
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
%% Function    : update_hypnum/5
%%
%% Description : updates the value of a hypernumber and then triggers
%%               the recalc
%%--------------------------------------------------------------------
update_hn(From,Bic,Val,Version)->

    {atomic, List} = mnesia:transaction(fun() ->
    
        Index = hn_util:page_to_index(hn_util:parse_url(From)),
        Rec   = #incoming_hn{ remote = Index, biccie = Bic, _='_'},
 
        [Obj] = mnesia:match_object(Rec),
        
        mnesia:write(Obj#incoming_hn{value=Val}),
        mark_dirty(Index,hypernumber)
        
    end),    

    ok.

%%--------------------------------------------------------------------
%% Function    : get_hypnum/9
%%
%% Description : This reads the hypernumber table and returns the
%%               current value of a hypernumber or an empty record
%%                if one doesn't exist
%%
%%               It creates a new record for the hypernumber and an
%%               into the refs table and then populates the
%%               value by making an external call
%%
%%               Having got a value it then writes it to the
%%               table and marks it a dirty reference
%%
%%--------------------------------------------------------------------
get_hypnum(Url,From,To)->

    {atomic, List} = mnesia:transaction(fun() ->

        case mnesia:read({incoming_hn,To}) of

        %% Remote number has already been fetched, add self to
        %% list of listeners
        [Hn] ->
            Cells = Hn#incoming_hn.cells,
            New   = Hn#incoming_hn{cells = hn_util:add_uniq(Cells,From)},
            mnesia:write(New),
            New;
        
		[]->
            Bic     = util2:get_biccie(),
            PUrl    = hn_util:index_to_url(From), 
            Actions = simplexml:to_xml_string(
                {register,[],[
                    {biccie,[],[Bic]},
                    {proxy, [],[PUrl]},
                    {url,   [],[PUrl]}
                ]}),
            
            Tmp = get_hn_record(Url,Actions),
            HNumber = Tmp#incoming_hn{
                remote  = To,
                cells   = [From],
                biccie  = Bic},

            mnesia:write(HNumber),
            HNumber
        end
    end),
    
    List.

get_hn_record(Url,Actions) ->

    XML = hn_util:post(Url++"?hypernumber",Actions,"text/xml"),

    {hypernumber,[],[
        {value,[],              [Val]},
        {'dependancy-tree',[],  Tree}]
    } = simplexml:from_xml_string(XML),

    V = ?COND(hn_util:is_numeric(Val) == true,
        util2:make_num(Val),
        util2:make_text(Val)),

    #incoming_hn{value= V, deptree=Tree}.
    

register_hn(To,From,Bic,Proxy,Url) -> 
    {atomic , ok} = mnesia:transaction(fun()-> 
        mnesia:write(#outgoing_hn{ 
            remote = From, 
            local  = To,
            biccie = Bic,
            url    = Url,
            proxy  = Proxy })
    end),  
	ok.	    
        
%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
notify_remote_change(Hn) ->

    {atomic, List} = mnesia:transaction( fun() ->
    
        #outgoing_hn{
            local  = From,
            remote = To, 
            biccie = Bic,
            url    = Url,
            proxy  = Proxy } = Hn,
            
        Value = get_item_val((to_ref(From))#ref{name=value}),
            
        Actions = simplexml:to_xml_string(
            {notify,[],[
                {biccie,      [],[Bic]},
                {notifyurl,   [],[hn_util:index_to_url(From)]},
                {registerurl, [],[Url]},
                {type,        [],["change"]},
                {value,       [],[hn_util:text(Value)]},
                {version,     [],["1"]}
            ]
        }),
    
        hn_util:post(Proxy,Actions,"text/xml")

    end),
    
    List.
    
to_ref(#index{site=Site,path=Path,column=X,row=Y}) ->
    #ref{site=Site,path=Path,ref={cell,{X,Y}}}.
    
