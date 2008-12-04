%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hn_db).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-define(mn_tr,mnesia:transaction).
-define(mn_ac,mnesia:activity).

%% record_info isnt available at runtime
-define(create(Name,Type,Storage),
        fun() ->
                Attr = [{attributes, record_info(fields, Name)},
                        {type,Type},{Storage, [node()]}],
                {atomic,ok} = mnesia:create_table(Name, Attr)
        end()).

-export([create/0, write_item/2, get_item/1, get_item_val/1, 
         get_item_inherited/2, get_item_list/1, remove_item/1,
         read_links/2, del_links/2, write_local_link/2,
         read_remote_links/3, write_remote_link/3,del_remote_link/1,
         register_hn/5, update_hn/4, get_hn/3, cell_changed/1,
         mark_dirty/2, write_template/4, read_template/1, 
         get_templates/0, get_ref_from_name/1, hn_changed/1]).
          
%% @spec create() -> ok
%% @doc  Creates the database for hypernumbers
create()->
    %% Seems sensible to keep this restricted
    %% to disc_copies for now
    Storage = disc_copies,
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    mnesia:start(),
    ?create(hn_item,           set,Storage),
    ?create(remote_cell_link,  bag,Storage),
    ?create(local_cell_link,   bag,Storage),
    ?create(hn_user,           set,Storage),
    ?create(dirty_cell,        set,Storage),
    ?create(dirty_hypernumber, set,Storage),
    ?create(incoming_hn,       set,Storage),
    ?create(outgoing_hn,       set,Storage),
    ?create(template,          set,Storage),
    hn_users:create("admin","admin"),
    hn_users:create("user","user"),
    ok.

%% @spec write_item(Addr,Val) -> ok
%% @doc  Adds an attribute to a reference addressed by Ref
write_item(Addr,Val) when is_record(Addr,ref) ->
    Item = #hn_item{addr=Addr, val=Val},
    Fun  = fun()-> mnesia:write(Item) end,
    ok = ?mn_ac(async_dirty,Fun),
    spawn(fun() -> notify_remote(Item) end),
    ok.

%% @spec get_item(Ref) -> ListofItems
%% @doc  Returns the list of attributes that are contained within 
%%       the range specified by Ref
get_item(#ref{site=Site,path=Path,ref=Ref,name=Name}) ->

    F = fun() ->
                NName = ?COND(Name == undef,'_',Name),
                NAddr = ?COND(element(1,Ref)== cell, Ref, _='_'),
                NRef  = #ref{site=Site, path=Path, name=NName, ref=NAddr},
                Match = #hn_item{addr = NRef, _ = '_'},
                mnesia:match_object(hn_item,Match,read)
        end,
    List = ?mn_ac(ets,F),
    case Ref of
        {cell,_} -> List;
        {page,_} -> List;
        _ ->
            Fun = fun(#hn_item{addr=#ref{ref=Item}}) -> 
                          filter_cell(Ref,Item) 
                  end,
            lists:filter(Fun,List)
    end.

%% @spec get_item_val(Ref) -> Value
%% @doc  return only the value stored at Ref
get_item_val(Addr) ->
    case get_item(Addr) of
        []                    -> [];
        [#hn_item{val=Value}] -> Value
    end.

%% @spec get_ref_from_name(Name) -> RefList
%% @doc  Returns list of with name set to Name
get_ref_from_name(Name) ->
    Fun = fun() ->
                  Match = #hn_item{addr=#ref{name=name, _ = '_'}, val = Name},
                  mnesia:match_object(hn_item,Match,read)
          end,
    Items = ?mn_ac(ets,Fun),
    [X#hn_item.addr || X <- Items].

%% @spec get_item_inherited(Ref) -> {ok,Value}
%% @doc  searches the tree for the first occurence of a value
%%       stored at Ref, if not found return default
get_item_inherited(Addr = #ref{ref={RefType,_}}, Default) ->
    case return_first(RefType,Addr) of
        {ok,Format} -> {ok,Format};
        nomatch     -> {ok,Default}
    end.

%% @spec get_item_list(Ref) -> List
%% @doc  searches the tree for items matching Ref's name
get_item_list(Addr = #ref{ref={RefType,_}}) ->
    get_item_list(RefType,Addr,[]).

%% @spec remove_item(Ref) -> ok
%% @doc  Removes item addressed by Ref
remove_item(#ref{site=Site,path=Path,ref=Ref,name=Name}) ->    
    F = fun() ->
                %% If Name is defined, match it
                N = ?COND(Name == undef,'_',Name),
                Attr  = #ref{site=Site, path=Path,ref=Ref, name=N, _ = '_'},
                Match = #hn_item{addr = Attr, _ = '_'},
                ?INFO("Attr ~p",[Attr]),
                Fun   = fun(X) -> notify_remove(X),mnesia:delete_object(X) end,
                ?INFO("remove ~p",[mnesia:match_object(hn_item,Match,read)]),
                lists:map(Fun,mnesia:match_object(hn_item,Match,read))
        end,
    _Okay = ?mn_ac(transaction,F),
    ok.

%% @spec write_local_link(Parent,Child) -> ok
%% @doc  Sets a link between a parent and a child on a local server
write_local_link(Parent,Child) ->
    F = fun()->
                mnesia:write(#local_cell_link{parent=Parent,child=Child})
        end,
    ok = ?mn_ac(async_dirty,F),
    ok.

%% @spec read_links(Cell,Relation) -> Links
%% @doc  Reads the links to or from a cell
read_links(Index, Relation) ->

    Obj = ?COND(Relation == child,
                {local_cell_link,'_',Index},
                {local_cell_link,Index,'_'}),

    F = fun() -> mnesia:match_object(Obj) end,
    List = ?mn_ac(ets,F),

    List.

%% @spec del_links(Cell,Relation) -> ok
%% @doc  Deletes the links to or from a cell
del_links(Index, Relation) ->

    Obj = ?COND(Relation == child,
                {local_cell_link,'_',Index},
                {local_cell_link,Index,'_'}),
    
    F = fun() -> 
                lists:foreach(
                  fun mnesia:delete_object/1, 
                  mnesia:match_object(Obj))
        end,

    mnesia:activity(transaction,F).

%% @spec del_remote_link(CellLink) -> ok
%% @doc  Delete a link between a local and remote cell, send an 
%%       unregister message so the child doesnt recieve any more 
%%       updates
del_remote_link(Obj=#remote_cell_link{type=outgoing}) ->
    F = fun() ->
                mnesia:dirty_delete_object(Obj),
                
                Me       = Obj#remote_cell_link.parent,
                Outgoing = #remote_cell_link{parent=Me,type=outgoing,_='_'},
                
                case mnesia:match_object(Outgoing) of
                    [] ->
                        Out  = #outgoing_hn{index={'_',Me},_='_'},
                        [Hn] = mnesia:match_object(Out),
                        mnesia:delete_object(Hn);
                    _  -> 
                        ok
                end
        end,
    ok = ?mn_ac(transaction,F),
    ok;

del_remote_link(Obj = #remote_cell_link{type=incoming}) ->

    ChildUrl = hn_util:index_to_url(Obj#remote_cell_link.child),
    
    F = fun() ->
                Remote = Obj#remote_cell_link.parent,
                %% Remove the relevant child attribute
                ParentRef = #ref{
                  site=Remote#index.site,
                  path=Remote#index.path,
                  name=children,
                  ref= {cell,{Remote#index.column,
                              Remote#index.row}}},
                
                Fun = fun(X) ->
                              ?COND(X == {url,[{type,"remote"}],[ChildUrl]}, 
                                    false, true)
                      end,
                
                Children = lists:filter(Fun,get_item_val(ParentRef)),                
                
                case Children of
                    [] -> hn_db:remove_item(ParentRef);
                    _  -> hn_db:write_item(ParentRef,Children)
                end,
                
                mnesia:dirty_delete_object(Obj),
                [Hn] = mnesia:match_object(#incoming_hn{remote=Remote,_='_'}),
                
                Link = #remote_cell_link{parent=Remote,type=incoming,_='_'},
                case mnesia:match_object(Link) of
                    [] -> mnesia:delete_object(Hn);
                    _  -> ok
                end,
                Hn
        end,
    
    Hn = ?mn_ac(transaction,F),
    
    %% Unregister
    Url     = hn_util:index_to_url(Obj#remote_cell_link.parent),
    Actions = simplexml:to_xml_string(
                {unregister,[],[
                                {biccie,[],[Hn#incoming_hn.biccie]},
                                {url,   [],[ChildUrl]}
                               ]}),    
    hn_util:post(Url++"?hypernumber",Actions,"text/xml"),
    
    ok.

%% @spec write_remote_link(Parent,Child,Type) -> ok
%% @doc  Establishes a link between here and a remote cell if the 
%%       link doesnt already exist, register with the remote cell 
%%       to recieve updates
write_remote_link(Parent,Child,Type) ->
    F = fun()->
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
        end,

    {ok,Link} = ?mn_ac(transaction,F),

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

%% @spec read_remote_links(Index,Relation,Type) -> Links
%% @doc  Returns a list of links to/from Index
read_remote_links(Index, Relation,Type) ->
    Obj = ?COND(Relation == child,
                {remote_cell_link,'_',Index,Type},
                {remote_cell_link,Index,'_',Type}),
    F = fun() -> mnesia:match_object(Obj) end,
    List = ?mn_ac(ets,F),
    List.

%% @spec cell_changed(Cell) -> ok
%% @doc  called from dirty_srv when a number changes
cell_changed(Cell) ->
    %% Make a list of cells hypernumbers + direct
    %% cell links, and check for any wildcard * on the path
    F = fun() ->
                %% Read dynamic links "/page/*/a1"
                NIndex = Cell#index{path=lists:reverse(Cell#index.path)},
                Queries = dyn_parents(NIndex,[],[]),
                Direct = read_links(Cell,parent),
                Rem = #remote_cell_link{parent=Cell,
                                        type=outgoing,_='_'},
                Links = mnesia:match_object(Rem),
                {ok,list_hn(Links,[]),lists:append(Direct,Queries)}
        end,
    {ok,Remote,Local} = ?mn_ac(transaction,F),
    
    Val = get_item_val((to_ref(Cell))#ref{name=rawvalue}),
    
    Recalc = fun(#local_cell_link{child=To}) -> hn_main:recalc(To) end,
    Notify = fun(X) -> notify_remote_change(X,Val) end,    

    lists:foreach(Notify,Remote),
    lists:foreach(Recalc,Local),
    ok.

%% @spec hn_changed(Cell) -> ok
%% @doc  called from dirty_srv when a number changes
hn_changed(Cell) ->
    F = fun() ->
                Link = #remote_cell_link{
                  parent=Cell,type=incoming,_='_'},
                
                X = fun(#remote_cell_link{child=Child}) ->
                            hn_main:recalc(Child)
                    end,
                
                lists:foreach(X,mnesia:match_object(Link)),
                ok
        end,
    ok = ?mn_ac(transaction,F),
    ok.

%% @spec mark_dirty(Type,Cell) -> ok
%% @doc  Marks a cell dirty (triggers recalculation)
mark_dirty(Index,Type) ->
    Obj = case Type of
              cell ->         #dirty_cell{index=Index};
              hypernumber ->  #dirty_hypernumber{index=Index}
          end,

    F = fun() -> mnesia:write(Obj) end,
    ok = ?mn_ac(async_dirty,F),
    ok.

%% @spec update_hn(From,Bic,Val,Version) -> ok
%% @doc  Recieve an update to the value of a hypernumber
update_hn(From,Bic,Val,_Version)->
    F = fun() ->
                {ok,ParsedFrom}=hn_util:parse_url(From),
                Index = hn_util:ref_to_index(ParsedFrom),
                Rec   = #incoming_hn{ remote = Index, biccie = Bic, _='_'},
                [Obj] = mnesia:match_object(Rec),
                
                mnesia:write(Obj#incoming_hn{value=hn_util:xml_to_val(Val)}),
                mark_dirty(Index,hypernumber)
        end,
    ok = ?mn_ac(transaction,F),
    ok.

%% @spec get_hn(Url,From,To) -> ok
%% @doc  Get the value of a hypernumber, from local table 
%%       or remote site
get_hn(Url,_From,To)->
    F = fun() -> do_get_hn(Url,To) end,
    List = ?mn_ac(transaction,F),
    List.

do_get_hn(Url,To)->
    case mnesia:read({incoming_hn,To}) of
        [Hn] ->
            Hn;
        []->
            case http:request(get,{Url,[]},[],[]) of
                {ok,{{_V,200,_R},_H,Xml}} ->
                    
                    {hypernumber,[],[
                                     {value,[],              [Val]},
                                     {'dependancy-tree',[],  Tree}]
                    } = simplexml:from_xml_string(Xml),
                    
                    HNumber = #incoming_hn{
                      value   = hn_util:xml_to_val(Val),
                      deptree = Tree,
                      remote  = To,
                      biccie  = util2:get_biccie()},
                    
                    mnesia:write(HNumber),
                    HNumber;
                
                {ok,{{_V,503,_R},_H,_Body}} ->
                    {error,permission_denied}
            end
    end.

%% @spec register_hn(To,From,Bic,Proxy,Url) -> ok
%% @doc  Recieve registration for a hypernumber
register_hn(To,From,Bic,Proxy,Url) ->
    F = fun()->
                Link = #remote_cell_link{
                  parent=To,
                  child=From,
                  type=outgoing},
                
                Hn = #outgoing_hn{
                  index  = {Proxy,To},
                  biccie = Bic,
                  url    = Url},
                
                mnesia:write(Link),
                mnesia:write(Hn),
                ok
        end,
    _Ok = ?mn_ac(transaction,F),
	ok.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
get_par(Index,Path) ->
    El = {local_cell_link,Index#index{path=Path},'_'},
    F = fun() -> mnesia:match_object(El) end,
    List = ?mn_ac(ets,F),
    List.

dyn_parents(_Index = #index{path=[]},Results, _Acc) ->
    Results;
dyn_parents(Index = #index{path=[_H]},Results, Acc) ->
    Path = ["*"|Acc],
    lists:append(Results,get_par(Index,Path));
dyn_parents(Index = #index{path=[H|T]},Results,Acc) ->
    Path = lists:append(lists:reverse(T),["*"|Acc]),
    NResults = lists:append(Results,get_par(Index,Path)),
    dyn_parents(Index#index{path=T},NResults,[H|Acc]).

%% @spec notify_remote(Item) -> ok
%% @doc  Adds an attribute to a reference addressed by Ref
notify_remote(#hn_item{addr=#ref{name="__"++_}}) ->
    ok;
notify_remote(Item=#hn_item{addr=#ref{site=Site,path=Path}}) ->
    MsgXml=hn_util:item_to_xml(Item),
    Msg = ?FORMAT("change ~s",[simplexml:to_xml_string(MsgXml)]),
    gen_server:call(remoting_reg,{change,Site,Path,Msg},?TIMEOUT),
    ok.

%% @spec filter_cell(Range,Ref) -> true | false
%% @doc  Returns true for Refs that are inside Range
filter_cell(X,X) -> true;
filter_cell({row,Y},{cell,{_,Y}}) -> true;
filter_cell({column,X},{cell,{X,_}}) -> true;
filter_cell({range,{_,Y1,_,Y2}},{row,Y}) 
  when Y > Y1; Y < Y2 -> true;
filter_cell({range,{X1,_,X2,_}},{column,X}) 
  when X > X1; X < X2 -> true;
filter_cell({range,{X1,Y1,X2,Y2}},{cell,{X,Y}})
  when Y >= Y1; Y =< Y2 andalso X >= X1; X =< X2 -> true;
filter_cell(_,_) -> false.

notify_remote_change(Hn,Value) ->

    {Server,Cell} = Hn#outgoing_hn.index,
    Version = hn_util:text(Hn#outgoing_hn.version + 1),
    error_logger:error_msg("in hn_db:notify_remote_change *WARNING* "++
                           "notify remote change not using "++
                           "version number ~p - ie it aint working - yet :(",
                           [Version]),

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

write_template(Name,TemplatePath,Gui,Form) ->
    CompiledPath=hn_templates:make_path(TemplatePath),
    Template=#template{name=Name,temp_path=CompiledPath,
                       gui=Gui,form=Form},
    Fun  = fun() ->
		   mnesia:write(Template)
	   end,
    ok = ?mn_ac(async_dirty,Fun),
    {ok,ok}.

read_template(Name) ->
    Fun = fun() -> mnesia:read({template,Name}) end,
    [Template] = ?mn_ac(ets,Fun),
    {ok,Template}.

get_templates() ->
    Match=ms_util:make_ms(template,[]),
    Fun = fun() -> mnesia:match_object(Match) end,
    Templates = ?mn_ac(ets,Fun),
    {ok,Templates}.


%% @doc Get the value of a named attribute, if it doesnt exist for address
%% check parent (cell -> range -> row -> column -> page -> root -> default)
match_ref(Ptn) ->
    F = fun() ->
                mnesia:match_object(hn_item,#hn_item{addr=Ptn,_='_'},read)
        end,
    List = ?mn_ac(ets,F),
    List.

get_item_list(RefType,Addr,Acc) ->
    case traverse(RefType,Addr) of
        {last,[]} ->
            {ok,Acc};
        {last,[#hn_item{val=Val}]} ->
            {ok,lists:append([Val,Acc])};
        {Ref,NewAddr,[]} ->
            get_item_list(Ref,NewAddr,Acc);
        {Ref,[]} ->
            get_item_list(Ref,Addr,Acc);
        {Ref,NewAddr,[#hn_item{val=Val}]} ->
            get_item_list(Ref,NewAddr,lists:append([Val,Acc]));
        {Ref,[#hn_item{val=Val}]} ->
            get_item_list(Ref,Addr,lists:append([Val,Acc]))
    end.

return_first(RefType,Addr) ->
    case traverse(RefType,Addr) of
        {last,[]} ->
            nomatch;
        {last,[#hn_item{val=Val}]} ->
            {ok,Val};
        {Ref,[]} ->
            return_first(Ref,Addr);
        {Ref,NewAddr,[]} ->
            return_first(Ref,NewAddr);
        {_Ref,_NewAddr,[#hn_item{val=Val}]} ->
            {ok,Val};
        {_Ref,[#hn_item{val=Val}]} ->
            {ok,Val}
    end.

traverse(cell,Addr = #ref{ref={cell,_}}) ->
    {range, match_ref(Addr)};

traverse(range,Addr = #ref{ref={range,_}}) ->
    {page, match_ref(Addr)};
traverse(range,Addr = #ref{ref={cell,_}}) ->
    V = case match_ref(Addr#ref{ref={range,'_'}}) of
            [] -> [];
            List ->
                case filter_range(List,Addr#ref.ref) of
                    nomatch -> [];
                    Val -> [Val]
                end
        end,
    {row_col, V};

traverse(row_col,Addr = #ref{ref={cell,{_X,Y}}}) ->
    {column, match_ref(Addr#ref{ref={row,Y}})};

traverse(row,Addr = #ref{ref={row,_}}) ->
    {page, match_ref(Addr)};
traverse(row,Addr = #ref{ref={cell,{_X,Y}}}) ->
    {page, match_ref(Addr#ref{ref={row,Y}})};

traverse(column,Addr = #ref{ref={column,_}}) ->
    {page, match_ref(Addr)};
traverse(column,Addr = #ref{ref={cell,{X,_Y}}}) ->
    {page, match_ref(Addr#ref{ref={column,X}})};

traverse(page,Addr = #ref{path=[]}) ->
    {last,match_ref(Addr#ref{ref={page,"/"}})};
traverse(page,Addr) ->
    NewPath = hslists:init(Addr#ref.path),
    {page,Addr#ref{path=NewPath},match_ref(Addr#ref{ref={page,"/"}})}.

filter_range([],_Cell)   ->
    nomatch;
filter_range([H|T],Cell) ->
    case hn_util:in_range((H#hn_item.addr)#ref.ref,Cell) of
        true -> H;
        _    -> filter_range(T,Cell)
    end.

%% @spec notify_remove(Item) -> ok
%% @doc  Sends message to notification server to tell 
%%       clients of changed value
notify_remove(#hn_item{addr=#ref{site=Site,path=Path,ref=Ref,name=Name}}) ->
    Msg = ?FORMAT("delete ~p ~p",[Name,hn_util:ref_to_str(Ref)]),
    gen_server:call(remoting_reg,{change,Site,Path,Msg},?TIMEOUT),
    ok.
