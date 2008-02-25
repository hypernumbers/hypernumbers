%%%-------------------------------------------------------------------
%%% File        :     db.erl
%%% Author      :     Gordon Guthrie
%%% Description :
%%%
%%% Created     : 12 Nov 2006 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------
-module(db).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("spriki.hrl").
-include("handy_macros.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export([
	 update_hypnum/1,
	 trigger_recalcs/2,
	 get_first_dirty/1,
	 get_hypnum/3,
	 write_ref/4,
	 read_spriki/4,
	 read_row/3,
     read_site/2,
	 read_column/3,
	 read_spriki_and_bindings/4,
     read_pages/1,
	 read_range/3,
	 write/8,
	 del_spriki/4,
	 read_ref/2,
 	 read_link_to/1,
 	 read_link_from/1,
	 get_websheet/1,
	 save_websheet/1,
	 delete_websheet/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Public User Functions                                                    %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_websheet(Page) ->
    Fun=fun() -> mnesia:delete_object(#websheet{page = Page,_='_'}) end,
    case mnesia:transaction(Fun) of
        {atomic,ok} -> ok;
        Else        -> Else
    end.

get_websheet(_Page) ->
	{ok,#websheet{}}.
	
save_websheet(Record) ->
    case delete_websheet(Record#websheet.page) of
        {error,Reason}  -> {error,Reason};
        ok ->
            Fun = fun() -> mnesia:write(Record) end,
            case mnesia:transaction(Fun) of
                {aborted, Reason} -> {error, Reason};
                {atomic, ok}      -> ok
            end
    end.

%%--------------------------------------------------------------------
%% Function    : update_hypnum/5
%%
%% Description : updates the value of a hypernumber and then triggers
%%               a dirty_ref for it
%%
%%--------------------------------------------------------------------
update_hypnum(HNumber)->

    call(fun()-> 
        mnesia:write(HNumber),
		mnesia:write(#dirty_hypernumbers{timestamp=util2:get_timestamp(),
            index=HNumber#hypernumbers.ref_from})
	end).


%% Gets called from dirty_srv:process() whenever a cell is updated.
trigger_recalcs(Type, Ref) ->
    %% Update the value of hypernumbers from the changed cell.
    List = call(fun() ->
                        mnesia:match_object(hypernumbers,
                                            #hypernumbers{
                                              ref_from = Ref, _ = '_'},
                                            read)
                end),

    lists:foreach(fun(Num) -> 
                          [Rec] = read_spriki(Num#hypernumbers.ref_from),
                          update_hypnum(Num#hypernumbers{value=Rec#spriki.value})
                  end,
                  List),

    %% Trigger updates on local cells that depended on the changed cell.
    lists:foreach(fun({ref, _, RefTo, _, DetTo}) ->
                          trigger_update(Type, Ref, RefTo, DetTo)
                  end,
                  read_ref(from, Ref)), % Dependent refs, may be empty. 
    
    %% And delete the dirty ref.
    call(fun() ->
                 mnesia:delete({Type, Ref})
         end).

%%--------------------------------------------------------------------
%% Function    : get_first_dirty/1
%%
%% Description : reads the first value out of the dirty table
%%
%%--------------------------------------------------------------------
get_first_dirty(Table)->
    call(fun()-> 
        case mnesia:dirty_first(Table) of
        '$end_of_table' -> [];
        Key -> mnesia:dirty_read(Table,Key)
	    end
	end).

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
%%               table and marks it a dirhttp://mail.google.com/mail/ty reference
%%
%%--------------------------------------------------------------------
get_hypnum(Url,To,From)->

    call(fun() ->

        case mnesia:read({hypernumbers,From}) of

		[]->
            Bic     = util2:get_biccie(),
            PUrl    = hn_util:index_to_url(To), 
            Actions = simplexml:to_xml_string(
                {register,[],[
                    {biccie,[],[Bic]},
                    {proxy, [],[PUrl]},
                    {url,   [],[PUrl]}
                ]}),
            
            HNumber  = get_hn_record(Url,Actions),
            NHNumber = HNumber#hypernumbers{ref_from=From},

            mnesia:write(NHNumber),

            %% Now the ref table will already exist if both
            %% the sites are being served from the same server
            Ref = #ref{ref_from=From,ref_to=To,details_from=
                #details_from{proxy_URL=PUrl,biccie=Bic}},

			{atomic,List} = mnesia:transaction((fun() ->
				mnesia:match_object(ref, #ref{ref_from=From,
					ref_to = To, _='_'}, read) end)),

            case List of

            [] ->  
                mnesia:write(Ref);
            [R] -> 
                mnesia:delete_object(ref,R,write),
                mnesia:write(Ref#ref{details_to=R#ref.details_to})
            end,

            NHNumber;

        [HyperNumber] -> 
            HyperNumber
            
        end
    end).

get_hn_record(Url,Actions) ->

    XML  = hn_util:post(Url++"?hypernumber",Actions,"text/xml"),
    SXML = simplexml:from_xml_string(XML),

    {cell,[],[{value,[],[Val]},
        {references,[],Ref},
        {reftree,[],Tree},
        {errors,[],Errors}]} = SXML,

    V = case hn_util:is_numeric(Val) of
        true -> util2:make_num(Val);
        _    -> util2:make_text(Val)
    end,

    #hypernumbers{
        value= V, reftree=Tree,
        errors=Errors,refs=Ref}.
        
%%--------------------------------------------------------------------
%% Function    : write_ref
%%
%% Description : this function writes an entry in the ref_to table
%%               and creates a corresponding entry in the ref table
%%               entry from the dirty_ref table
%%
%%--------------------------------------------------------------------
write_ref(From,To,Details_from,Details_to)->
    Fun = fun()-> 
        mnesia:write(
            #ref{ ref_from=From, ref_to=To,
                  details_from=Details_from,
				  details_to=Details_to })
	  end,

    case mnesia:transaction(Fun) of
	{aborted, Reason} -> {error, Reason};
	{atomic , _Okay}  -> ok;
	Other3            -> {error,Other3}
    end.

%%--------------------------------------------------------------------
%% Function    : read_link_to/2
%%
%% Description : this function takes a site/page as arguments and
%%               returns the pages that this page links to
%%
%%               In future it will bring out full qualified links
%%               which will be post-processed in the spriki for
%%               the gui
%%
%%--------------------------------------------------------------------
read_link_to(_Page)-> 	[].
read_link_from(_Page)->	[].

%%--------------------------------------------------------------------
%% Function    : read_range/4
%%
%% Description : this function reads ranges from a single page
%%               In order to work the first marker point must be
%%               'nearer' the origin than the second
%%               ie X2 must be >= X1 and Y2 must be >= Y1
%%
%%--------------------------------------------------------------------
read_range(Site,Path,{X1,Y1,X2,Y2}) ->

	F = fun(Ind, Acc) -> 
		case Ind#spriki.index of
			#index{site=Site,path=Path,row = Y, column=X} when
				X >= X1,Y >= Y1, X =< X2,Y =< Y2 ->
				[Ind | Acc];
			_ -> 
				Acc
		end
	end,
    
    call(fun() -> mnesia:foldl(F, [], spriki) end).

%%--------------------------------------------------------------------
%% Function    : read_ref/2
%%
%% Description : ref is a two way table it joins cells that refer to
%%               each other together (it is a pigs ear on the table
%%               spriki effectively)
%%
%%               references are fully qualified domain names and can
%%               be used to refer to both local and remote cells
%%--------------------------------------------------------------------

%% Reads the pigs ear up (go from me to who refers to me).
read_ref(from, Index) ->
    call(fun() ->
                 mnesia:read({ref, Index})
         end);

%% Reads the pigs ear down (go from me to who refers to me).
read_ref(to, Index) ->
    call(fun() ->
                 mnesia:index_read(ref, Index, #ref.ref_to)
         end);

%% Finds a particular ref that connects two particular cells (both local
%% and remote).
read_ref(From, To) ->
    call(fun() ->
                 mnesia:select(ref, [{#ref{ref_from = From,
                                           ref_to = To,
                                           _ = '_'},
                                      [],
                                      ['$_']}])
         end).

%%--------------------------------------------------------------------
%% Function    : read_spriki/4
%%
%% Description :
%%--------------------------------------------------------------------
read_spriki(Index) ->
    call(fun() -> mnesia:read({spriki,Index}) end).

read_spriki(Site,Path,X,Y) ->
    call(fun() ->
        Index = #index{site=Site,path=Path,column=X,row=Y},
        mnesia:read({spriki,Index})
    end).

%%--------------------------------------------------------------------
%% Function    : read_row/3
%%
%% Description :
%%--------------------------------------------------------------------
read_row(Site,Path,Y) ->
    Fun = fun() ->
        Index= #index{site=Site,path=Path,row=Y, column='_'},
        mnesia:match_object({spriki,Index,'_','_','_','_','_'})
	  end,

    case mnesia:transaction(Fun) of
    {aborted, Reason} -> {error, Reason};
    {atomic,  List} -> 
        lists:sort(
            fun(A,B) -> 
                (A#spriki.index)#index.column < 
                (B#spriki.index)#index.column
            end,List)
    end.

%%--------------------------------------------------------------------
%% Function    : read_site/2
%%
%% Description :
%%--------------------------------------------------------------------
read_site(Site,Path) ->
    call(fun() ->
        Index= #index{site=Site,path=Path,row='_', column='_'},
        mnesia:match_object({spriki,Index,'_','_','_','_','_'})
      end).

%%--------------------------------------------------------------------
%% Function    : read_column/4
%%
%% Description :
%%--------------------------------------------------------------------
read_column(Site,Path,X) ->
    Fun = fun() ->
        Index= #index{site=Site,path=Path,row='_', column=X},
        mnesia:match_object({spriki,Index,'_','_','_','_','_'})
	  end,

    case mnesia:transaction(Fun) of
    {aborted, Reason} -> {error, Reason};
    {atomic,  List} -> 
        lists:sort(
            fun(A,B) -> 
                (A#spriki.index)#index.row < 
                (B#spriki.index)#index.row 
            end,List)
    end.


%%--------------------------------------------------------------------
%% Function    : read_spriki_and_bindings/4
%%
%% Description :
%%--------------------------------------------------------------------
read_spriki_and_bindings(Site,Path,X,Y) ->
    call(fun() ->
		  Index = #index{site=Site,path=Path,column=X,row=Y}, 
		  {mnesia:read({spriki,Index}),
           mnesia:read({bindings,Index})}
	end).

%%--------------------------------------------------------------------
%% Function    : read_websheets_from_page
%%
%% Description :
%%--------------------------------------------------------------------
read_pages(Site) ->
	call(fun() ->
        mnesia:match_object(spriki, {spriki,{index,Site,'_','_','_'},
            '_','_','_','_','_'}, read)
    end).

%%--------------------------------------------------------------------
%% Function    : write/8
%%
%% this function writes a cell to the table spriki
%% and all its variable bindings to the table bindings
%%--------------------------------------------------------------------
write(Site,Path,X,Y,Value,Type,Bind,Status)->

    F = fun() ->

        Index = #index{site=Site,path=Path,column=X,row=Y},

        mnesia:write(#spriki{index=Index,value=Value,
            val_type=Type,status=Status}),

        mnesia:write(#dirty_refs{timestamp=util2:get_timestamp(),
            index=Index}),
    
        case {Status#status.refs,Bind} of

        %% No References or Bindings
        {[],[]} -> ok;
        
        _ ->
            lists:map(

                fun({FSite,FPath,FX,FY}) ->

                    FIndex=#index{site=FSite,path=FPath,column=FX,row=FY},

                    %% read the refs
                    {DetFrom,DetTo} = 
                    case mnesia:match_object({ref,FIndex,Index,'_','_'}) of
                    []  -> {#details_from{},#details_to{}};
                    [R3]->
                        mnesia:delete_object(R3),
                        {R3#ref.details_from,R3#ref.details_to}
                    end,

                    mnesia:write(#ref{ref_from=FIndex,ref_to=Index,
                        details_from=DetFrom,details_to=DetTo})

                end,Status#status.refs),

            %% finish up with the bindings
            mnesia:delete({bindings,Index}),

            lists:map(
                fun({Site3,Path3,Type3,Nm,Val}) ->
                    mnesia:write(#bindings{index=Index,page=#page{site=Site3,
                        path=Path3},type=Type3,varname=Nm,value=Val})
                end,Bind)

        end

    end,

    case mnesia:transaction(F) of

	{aborted, Reason} -> {error, Reason};
	{atomic,  _Okay}  ->

	    %% notify the registration server of the write
        Page = #page{site=Site,path=Path,ref={cell,{X,Y}}},
	    gen_server:call(remoting_reg,{change,Page,Value},?TIMEOUT),
	    ok
    end.

%%--------------------------------------------------------------------
%% Function    : del_spriki/4
%%
%% Description :
%%--------------------------------------------------------------------
del_spriki(Site,Path,X,Y) ->
    Sel = fun() ->
        MatchHead = #spriki{index='$1',value='$5',status='$6'},
        Guard = {'==','$1',#index{site=Site,path=Path,column=X,row=Y}},
        mnesia:select(spriki,[{MatchHead,[Guard],['$5','$6']}],read)
    end,

    case mnesia:transaction(Sel) of
    {aborted, Reason}   -> {error, Reason};
    {atomic, [Return]}  ->
        call(fun() -> mnesia:delete({spriki,Return}) end)
    end.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% Wrapper around mnesia:transaction().
call(Fun) ->
    case mnesia:transaction(Fun) of
        {aborted, Reason} -> {error, Reason};
        {atomic,  List}   -> List
    end.


trigger_update(dirty_refs, From, To, Details) ->
    ?COND(From#index.site == To#index.site,
          spriki:recalc(To),
          remote_recalc(Details, From));

trigger_update(dirty_hypernumbers, _From, To, _Details) ->
    spriki:recalc(To).

remote_recalc(Details,From) ->

    call(fun() ->
    
        [#spriki{value=Value}] = mnesia:read({spriki,From}),   
        #details_to{proxy_URL=Proxy,reg_URL=Reg,biccie=Bic} = Details,

        Actions = simplexml:to_xml_string(
            {notify,[],[
                {biccie,      [],[Bic]},
                {notifyurl,   [],[hn_util:index_to_url(From)]},
                {registerurl, [],[Reg]},
                {type,        [],["change"]},
                {value,       [],[hn_util:text(Value)]},
                {version,     [],["1"]}
            ]
        }),
    
        hn_util:post(Proxy,Actions,"text/xml")

    end).
