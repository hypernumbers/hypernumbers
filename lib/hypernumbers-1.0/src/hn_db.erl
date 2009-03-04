%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hn_db).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-define(mn_ac,mnesia:activity).

%% record_info isnt available at runtime
-define(create(Name,Type,Storage),
        fun() ->
                Attr = [{attributes, record_info(fields, Name)},
                        {type,Type},{Storage, [node()]}],
                {atomic,ok} = mnesia:create_table(Name, Attr)
        end()).

-export([create/0,
         write_item/2,
         get_item/1,
         get_item_val/1, 
         get_item_inherited/2,
         get_item_list/1,
         remove_item/1,
         read_links/2,
         del_links/2,
         write_local_link/2,
         read_remote_links/3,
         write_remote_link/3,
         del_remote_link/1,
         register_hn/5,
         update_hn/4,
         get_hn/3,
         cell_changed/1,
         mark_dirty/3,
         write_template/4,
         read_template/1, 
         get_templates/0,
         get_ref_from_name/1,
         hn_changed/1]).

%% API for file import only 
%% writes a whole style in a oner - not on a per attribute basis 
-export([write_style_IMPORT/2]). 

%% @spec create() -> ok
%% @doc  Creates the database for hypernumbers
create()->
    % Seems sensible to keep this restricted
    % to disc_copies for now
    Storage = disc_copies,
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    mnesia:start(),
    ?create(hn_item,               set, Storage),
    ?create(remote_cell_link,      bag, Storage),
    ?create(local_cell_link,       bag, Storage),
    ?create(hn_user,               set, Storage),
    ?create(dirty_cell,            set, Storage),
    ?create(dirty_notify_in,       set, Storage),
    ?create(dirty_incoming_create, set, Storage),
    ?create(dirty_notify_back_in,  set, Storage),
    ?create(dirty_notify_out,      set, Storage),
    ?create(dirty_notify_back_out, set, Storage),
    ?create(incoming_hn,           set, Storage),
    ?create(outgoing_hn,           set, Storage),
    ?create(template,              set, Storage),
    ?create(styles,                bag, Storage), 
    ?create(style_counters,        set, Storage),
    ?create(page_vsn,              bag, Storage),
    ?create(page_vsn_counters,     set, Storage),
    hn_users:create("admin","admin"),
    hn_users:create("user","user"),
    ok.

%% @spec write_item(Addr,Val) -> ok
%% @doc  Adds an attribute to a reference addressed by Ref
write_item(Addr,Val) when is_record(Addr,ref) ->
    #ref{ref = Ref} = Addr,
    case Ref of
        null -> exit("crashing with null Ref from write item");
        _    -> ok
    end,
    Item = #hn_item{addr=Addr, val=Val}, 
    #ref{name=Name} = Addr, 
    % NOTE the attribute 'overwrite-color' isn't in this case statement 
    % it would APPEAR to be a CSS style, but it actually isn't 
    %  
    case ms_util2:is_in_record(magic_style, Name) of 
        true  -> process_styles(Addr#ref{name = magic_style}, {Name, Val}); 
        false -> Fun  = 
                     fun()-> mnesia:write(Item) 
                     end, 
                 ok = ?mn_ac(async_dirty,Fun), 
                 spawn(fun() -> notify_remote(Item) end) 
    end, 
    ok. 

%% @spec process_styles(Addr, Val) -> ok 
%% @doc takes the attribute setting and then updates the style that pertains 
%% to the cell and possibly the style table itself 
process_styles(Addr, {Name, Val}) -> 
    % First up read the current style 
    Fun1 = fun() -> 
                   Match = #hn_item{addr = Addr, _ = '_'}, 
                   mnesia:match_object(hn_item, Match, read) 
           end, 
    CurrentStyle = mnesia:activity(async_dirty, Fun1), 
    NewStyleIdx = case CurrentStyle of 
                      []      -> get_style(Addr, Name, Val); 
                      [Style] -> get_style(Addr, Style, Name, Val) 
                  end, 
    Item = #hn_item{addr = Addr#ref{name = style}, val = NewStyleIdx}, 
    
    Fun2  = 
        fun()-> 
                mnesia:write(Item) 
        end, 
    ok = ?mn_ac(async_dirty,Fun2), 
    
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
    List = mnesia:activity(transaction,F),
    case Ref of
        {cell,_} -> List;
        {page,_} -> List;
        _        -> Fun = fun(#hn_item{addr=#ref{ref=Item}}) -> 
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

%% @spec get_item_inherited(Ref, Default) -> {ok, Value}
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
                % If Name is defined, match it
                N = ?COND(Name == undef,'_',Name),
                Attr  = #ref{site=Site, path=Path,ref=Ref, name=N, _ = '_'},
                Match = #hn_item{addr = Attr, _ = '_'},
                Fun   = fun(X) -> notify_remove(X),mnesia:delete_object(X) end,
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
%%       unregister message so the child doesnt receive any more 
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
                % Remove the relevant child attribute
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

    % Unregister
    Url     = hn_util:index_to_url(Obj#remote_cell_link.parent),
    Actions = simplexml:to_xml_string(
                {unregister,[],[
                                {biccie,[],[Hn#incoming_hn.biccie]},
                                {child_url,   [],[ChildUrl]}
                               ]}),    
    hn_util:post(Url++"?hypernumber",Actions,"text/xml"),

    ok.

%% @spec write_remote_link(Parent,Child,Type) -> ok
%% @doc  Establishes a link between here and a remote cell if the 
%%       link doesnt already exist, register with the remote cell 
%%       to receive updates
write_remote_link(Parent,Child,Type) ->
    F = fun()->
                ParentRef = #ref{
                  site=Parent#index.site,
                  path=Parent#index.path,
                  ref = {cell,{Parent#index.column,Parent#index.row}},
                  name = children},
                
                Children = [{url,[{type,"remote"}],[hn_util:index_to_url(Child)]}
                            | get_item_val(ParentRef)],
                
                hn_db:write_item(ParentRef,{xml,Children}),
                Match = ms_util:make_ms(remote_cell_link,[{parent, Parent},
                                                          {child, Child},
                                                          {type, Type}]),
                case mnesia:match_object(Match) of 
                    [] ->
                        Link = #remote_cell_link{parent = Parent,
                                                 child = Child, type =  Type},
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
            Proxy = Child#index.site ++"/"++ string:join(Child#index.path,"/")++"/",
            Actions = simplexml:to_xml_string(
                        {register,[],[
                                      {biccie,    [],[Hn#incoming_hn.biccie]},
                                      {proxy,     [],[Proxy]},
                                      {child_url, [],[Url]}
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
    hn_main:recalc(Cell),
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

%% @spec mark_dirty(Type,Value,Cell) -> ok
%% @doc  Marks a cell dirty (triggers recalculation)
%mark_dirty(Index,Value,cell) ->
%    % Make a list of direct cell links, and check for 
%    % any wildcard * on the path
%    Fun1 = fun() ->
%                   % First read dynamic links "/page/*/a1"
%                   NIndex = Index#index{path=lists:reverse(Index#index.path)},
%                   Queries = dyn_parents(NIndex,[],[]),
%                   % Second read direct links
%                   Direct = read_links(Index,parent),
%                   {ok,lists:append(Direct,Queries)}
%           end,
%    {ok,Local} = ?mn_ac(transaction,Fun1),
%    % Now write the local children to dirty_cell
%    Fun2 = fun(#local_cell_link{child=To}) -> 
%                   F = fun() -> 
%                               % only write the dirty cell if 
%                               % it doesnt already exist
%                               Match=#dirty_cell{index=To,_='_'},
%                               case mnesia:match_object(Match) of
%                                   [] -> ok = mnesia:write(#dirty_cell{index=To}) ;
%                                   _  -> ok
%                               end
%                       end,
%                   ?mn_ac(transaction,F)
%           end,
%    _Return1=lists:foreach(Fun2,Local),
%    RefX = hn_util:refX_from_index(Index),
%    Fun3 = fun() ->
%                  {ok, ok} = hn_db_wu:mark_dirty_outgoing_hn_DEPRECATED(RefX, Value)
%          end,
%    {ok, ok} = mnesia:activity(transaction, Fun3),
%    ok;
mark_dirty(Index, _Value, hypernumber) ->
    RefX = hn_util:refX_from_index(Index),
    io:format("in hn_db:mark_dirty  for RefX of ~p AINT THIS OUTGOING!!!~n",
              [RefX]),
    Fun3 = fun() ->
                  {ok, ok} = hn_db_wu:mark_dirty_notify_in_DEPRECATED(RefX)
          end,
    {ok, ok} = mnesia:activity(transaction, Fun3),
    ok.

%% @spec update_hn(From,Bic,Val,Version) -> ok
%% @doc  Receive an update to the value of a hypernumber
update_hn(From,Bic,Val,_Version)->

    F = fun() ->
                {ok,ParsedFrom}=hn_util:parse_url(From),
                Index = hn_util:ref_to_index(ParsedFrom),
                Rec   = #incoming_hn{remote = Index, biccie = Bic, _='_'},
                [Obj] = mnesia:match_object(Rec),
                Rec2 = Obj#incoming_hn{value=hn_util:xml_to_val(Val)},
                io:format("in hn_db:update_hn Rec2 is ~p~n", [Rec2]),
                ok = mnesia:write(Rec2),
                ok = mark_dirty(Index,Val,hypernumber)
        end,
    ok = ?mn_ac(transaction,F),
    ok.

%% @spec get_hn(Url,From,To) -> ok
%% @doc  Get the value of a hypernumber, from local table 
%%       or remote site
%% @TODO - this needs to be robistified - retry if there is server failure etc
%% Also need to properly set up the proxy for use in this - should come from the 
%% config file...
get_hn(Url, From, To)->
    F = fun() -> do_get_hn(Url, From, To) end,
    ?mn_ac(transaction,F).

do_get_hn(Url, From, To)->
    case mnesia:read({incoming_hn,To}) of
        [Hn] ->
            Hn;
        []->
            io:format("in hn_db:do_get_hn "++
                      "this ain't me babe! this should just mark the table "++
                      "incoming_hn dirty and return blank to the function so "++
                      "that the dirty_srv does the work...~n"),
            Biccie = util2:get_biccie(),
            #index{site = S, path = P} = From,
            Proxy = S ++"/"++ string:join(P,"/")++"/",
            FromUrl = hn_util:index_to_url(From),
            Actions = simplexml:to_xml_string(
                        {register,[],[
                                      {biccie,     [], [Biccie]},
                                      {proxy,      [], [Proxy]},
                                      {child_url, [], [FromUrl]}
                                     ]}),

            case  http:request(post,{Url,[],"text/xml",Actions},[],[]) of
                {ok,{{_V,200,_R},_H,Xml}} ->
                    {hypernumber,[],[
                                     {value,[],              [Val]},
                                     {'dependency-tree',[],  Tree}]
                    } = simplexml:from_xml_string(Xml),

                    HNumber = #incoming_hn{
                      value             = hn_util:xml_to_val(Val),
                      'dependency-tree' = Tree,
                      remote            = To,
                      biccie            = Biccie},
                    io:format("in hn_db:do_get_hn HNumber is ~p~n", [HNumber]),
                    mnesia:write(HNumber),
                    HNumber;

                {ok,{{_V,503,_R},_H,_Body}} ->
                    {error,permission_denied}
            end
    end.

%% @spec register_hn(To,From,Bic,Proxy,Url) -> Val
%% Val = list()
%% @doc  Receive registration for a hypernumber
register_hn(To, From, Bic, Proxy, Url) ->
    % io:format("in hn_db:register_hn~n-To is ~p~n-From is ~p~n", [To, From]),
    F = fun()->
                Link = #remote_cell_link{
                  parent = From,
                  child  = To,
                  type = outgoing},
                % io:format("In hn_db:register_hn Link is ~p~n", [Link]),
                Hn = #outgoing_hn{
                  index     = {Proxy,To},
                  biccie    = Bic,
                  child_url = Url},

                ok = mnesia:write(Link),
                ok = mnesia:write(Hn),
                #index{site = S, path = P, column = X, row = Y} = To,
                Ref = #ref{site = S, path = P, ref = {cell, {X, Y}}, name = formula},
                Value = case get_item_val(Ref) of
                          [] -> [{blank,[],[]}];
                          Tmp -> hn_util:to_xml(Tmp)
                      end,
                DepTree = case hn_db:get_item_val(Ref#ref{name='dependency-tree'}) of
                              {xml,Tree} -> Tree;
                              []         -> []
                          end,
                {ok,{hypernumber,[],[{value,[], Value},
                                     {'dependency-tree',[], DepTree}]}}
        end,
    Val = ?mn_ac(transaction,F),
	Val.

%get_par(Index,Path) ->
%    El = {local_cell_link,Index#index{path=Path},'_'},
%    F = fun() -> mnesia:match_object(El) end,
%    List = ?mn_ac(ets,F),
%    List.

%dyn_parents(_Index = #index{path=[]},Results, _Acc) ->
%    Results;
%dyn_parents(Index = #index{path=[_H]},Results, Acc) ->
%    Path = ["*"|Acc],
%    lists:append(Results,get_par(Index,Path));
%dyn_parents(Index = #index{path=[H|T]},Results,Acc) ->
%    Path = lists:append(lists:reverse(T),["*"|Acc]),
%    NResults = lists:append(Results,get_par(Index,Path)),
%    dyn_parents(Index#index{path=T},NResults,[H|Acc]).

%% @spec notify_remote(Item) -> ok
%% @doc  Adds an attribute to a reference addressed by Ref
notify_remote(Item=#hn_item{addr=#ref{site=Site,path=Path,name=Name}}) ->
    case atom_to_list(Name) of
        [$_, $_|_R]  -> ok; % names of form '__name' are not notified to front-end
        _Other       -> MsgXml=hn_util:item_to_xml(Item),
                        Msg = ?FORMAT("change ~s",[simplexml:to_xml_string(MsgXml)]),
                        gen_server:call(remoting_reg,{change,Site,Path,Msg},?TIMEOUT),
                        ok
    end.

%% @spec filter_cell(Range,Ref) -> true | false
%% @doc  Returns true for Refs that are inside Range
filter_cell(X,X) -> true;
filter_cell({row,Y},{cell,{_,Y}}) -> true;
filter_cell({column,X},{cell,{X,_}}) -> true;
filter_cell({range,{_,Y1,_,Y2}},{row,Y}) 
  when Y > Y1; Y < Y2 -> true;
filter_cell({range,{X1,_,X2,_}},{column,X}) 
  when X > X1; X < X2 -> true;
filter_cell({range,Range},{cell,Cell}) -> 
    hn_util:in_range({range,Range},{cell,Cell});
filter_cell(_,_) -> false.

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

get_style(Addr, Name, Val) -> 
    NoOfFields = ms_util2:no_of_fields(magic_style), 
    Index = ms_util2:get_index(magic_style, Name), 
    Style = make_tuple(magic_style, NoOfFields, Index, Val), 
    % Now write the style 
    write_style(Addr, Style). 

get_style(Addr, Style, Name, Val) -> 
    % use the index of the style to read the style 
    #hn_item{addr = Ref, val = StyleIdx} = Style, 
    Fun = fun() -> 
                  Match = #styles{ref = Ref#ref{ref = {page, "/"}}, 
                                  index = StyleIdx, _ = '_'}, 
                  mnesia:match_object(styles, Match, read) 
          end, 
    Return = mnesia:activity(async_dirty, Fun), 
    [#styles{magic_style = CurrentStyle}] = Return, 
    Index = ms_util2:get_index(magic_style, Name), 
    Style2 = tuple_to_list(CurrentStyle), 
    {Start, [_H | End]} = lists:split(Index, Style2), 
    NewStyle = list_to_tuple(lists:append([Start, [Val], End])), 
    write_style(Addr, NewStyle). 

%% write_style will write a style if it doesn't exist and then 
%% return an index pointing to it 
%% If the style already exists it just returns the index 
write_style(Addr, Style) ->
    Ref = Addr#ref{ref = {page, "/"}, name = style, auth = []}, 
    Fun1 = fun() -> 
                   Match = #styles{ref = Ref, magic_style = Style, _ = '_'}, 
                   mnesia:match_object(styles, Match, read) 
           end, 
    case mnesia:activity(async_dirty, Fun1) of 
        []              -> write_style2(Addr, Style); 
        [ExistingStyle] -> #styles{index = NewIndex} = ExistingStyle, 
                           NewIndex 
    end. 

write_style2(Addr, Style) -> 
    Ref = Addr#ref{ref = {page, "/"}, name = style, auth = []}, 
    NewIndex = mnesia:dirty_update_counter(style_counters, Ref, 1), 
    Fun = fun() -> 
                  mnesia:write(#styles{ref = Ref, index = NewIndex,
                                       magic_style = Style}) 
          end, 
    ok = ?mn_ac(async_dirty, Fun), 
    NewIndex. 

make_tuple(Style, Counter, Index, Val) -> 
    make_tuple(Style, Counter, Index, Val, []). 

make_tuple(S, 0, _I, _V, Acc)     -> list_to_tuple([S|Acc]); 
make_tuple(S, I, I, V, Acc )      -> make_tuple(S, I -1 , I, V, [V | Acc]); 
make_tuple(S, Counter, I, V, Acc) -> make_tuple(S, Counter - 1, I, V, [[] | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%                                                                            %% 
%% write_style_IMPORT for file import API                                     %% 
%%                                                                            %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
write_style_IMPORT(Addr, Style) -> 
    % first write the Style
    StyleIndex = write_style(Addr, Style),
    % now write the style index for the address
    Ref = Addr#ref{name = style},
    Item = #hn_item{addr = Ref, val = StyleIndex},
    Fun  =
        fun()->
                mnesia:write(Item)
        end,
    ok = ?mn_ac(async_dirty,Fun).
