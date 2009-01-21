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
         mark_dirty/2,
         write_template/4,
         read_template/1, 
         get_templates/0,
         get_ref_from_name/1,
         hn_changed/1,
         delete_cells/1,
         clear_cells/1]).

%% API for file import only
%% writes a whole style in a oner - not on a per attribute basis
-export([write_style_IMPORT/2]).

%% server-side drag'n'drop
-export([drag_n_drop/2,
         copy_n_paste/2,
         cut_n_paste/2]).

%%% Debugging interface
-export([copy_DEBUG/0,
         delete_cells_DEBUG/1,
         clear_cells_DEBUG/1]).

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
    ?create(hn_item,           set, Storage),
    ?create(remote_cell_link,  bag, Storage),
    ?create(local_cell_link,   bag, Storage),
    ?create(hn_user,           set, Storage),
    ?create(dirty_cell,        set, Storage),
    ?create(dirty_hypernumber, set, Storage),
    ?create(incoming_hn,       set, Storage),
    ?create(outgoing_hn,       set, Storage),
    ?create(template,          set, Storage),
    ?create(styles,            bag, Storage),
    ?create(style_counters,    set, Storage),
    hn_users:create("admin","admin"),
    hn_users:create("user","user"),
    ok.

%% @spec write_item(Addr,Val) -> ok
%% @doc  Adds an attribute to a reference addressed by Ref
write_item(Addr,Val) when is_record(Addr,ref) ->
    Item = #hn_item{addr=Addr, val=Val},
    #ref{name=Name} = Addr,
    % NOTE the attribute 'overwrite-color' isn't in this case statement
    % it would APPEAR to be a CSS style, but it actually isn't
    % 
    case ms_util2:is_in_record(style, Name) of
        true  -> process_styles(Addr#ref{name = style}, {Name, Val});
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
    Ref = Addr#ref{ref = {page, "/"}},

    spawn(fun() -> notify_remote(Item) end),
    ok.

get_style(Addr, Name, Val) ->
    NoOfFields = ms_util2:no_of_fields(style),
    Index = ms_util2:get_index(style, Name),
    Style = make_tuple(style, NoOfFields, Index, Val),
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
    [#styles{style = CurrentStyle}] = Return,
    Index = ms_util2:get_index(style, Name),
    Style2 = tuple_to_list(CurrentStyle),
    {Start, [_H | End]} = lists:split(Index, Style2),
    NewStyle = list_to_tuple(lists:append([Start, [Val], End])),
    write_style(Addr, NewStyle).

%% write_style will write a style if it doesn't exist and then
%% return an index pointing to it
%% 
%% If the style already exists it just returns the index
write_style(Addr, Style) ->
    Ref = Addr#ref{ref = {page, "/"}, name = style, auth = []},
    Fun1 = fun() ->
                   Match = #styles{ref = Ref, style = Style, _ = '_'},
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
                  mnesia:write(#styles{ref = Ref, index = NewIndex, style = Style})
          end,
    ok = ?mn_ac(async_dirty, Fun),
    NewIndex.

make_tuple(Style, Counter, Index, Val) ->
    make_tuple(Style, Counter, Index, Val, []).

make_tuple(S, 0, _I, _V, Acc)     -> list_to_tuple([S|Acc]);
make_tuple(S, I, I, V, Acc )      -> make_tuple(S, I -1 , I, V, [V | Acc]);
make_tuple(S, Counter, I, V, Acc) -> make_tuple(S, Counter - 1, I, V, [[] | Acc]).

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
%%       to receive updates
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
            Proxy = Child#index.site ++"/"++ string:join(Child#index.path,"/")++"/",
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

%% @spec mark_dirty(Type,Cell) -> ok
%% @doc  Marks a cell dirty (triggers recalculation)
mark_dirty(Index,cell) ->
    % Make a list of cells hypernumbers + direct
    % cell links, and check for any wildcard * on the path
    Fun1 = fun() ->
                   % First read dynamic links "/page/*/a1"
                   NIndex = Index#index{path=lists:reverse(Index#index.path)},
                   Queries = dyn_parents(NIndex,[],[]),
                   % Second read direct links
                   Direct = read_links(Index,parent),
                   % Last get hypernumbers that are children
                   Rem = #remote_cell_link{parent=Index,
                                           type=outgoing,_='_'},
                   Links = mnesia:match_object(Rem),
                   RemReturn=list_hn(Links,[]),
                   {ok,RemReturn,lists:append(Direct,Queries)}
           end,
    {ok,Remote,Local} = ?mn_ac(transaction,Fun1),
    % Now write the local children to dirty_cell
    Fun2 = fun(#local_cell_link{child=To}) -> 
                   F = fun() -> 
%% only write the dirty cell if 
%% it doesnt already exist
                               Match=#dirty_cell{index=To,_='_'},
                               case mnesia:match_object(Match) of
                                   [] -> mnesia:write(#dirty_cell{index=To}) ;
                                   _  -> ok
                               end
                       end,
                   ?mn_ac(transaction,F)
           end,
    _Return1=lists:foreach(Fun2,Local),
    % Now write notify the remote children that they are dirty
    % get the new value first
    Val = get_item_val((to_ref(Index))#ref{name=rawvalue}),
    Notify = fun(X) -> notify_remote_change(X,Val) end,        
    _Return2=lists:foreach(Notify,Remote),
    ok;
mark_dirty(Index,hypernumber) ->
    Fun1 = fun() ->
                   Rem = #remote_cell_link{parent=Index,
                                           type=incoming,_='_'},
                   Links = mnesia:match_object(Rem),
                   {ok,Links}
           end,
    {ok,Local} = ?mn_ac(transaction,Fun1),
    Fun2 = fun(X) ->
                   #remote_cell_link{child=Index2}=X,
                   Fun3 = fun() -> mnesia:write(#dirty_cell{index=Index2}) end,
                   ?mn_ac(transaction,Fun3)
           end,
    _Return1=lists:foreach(Fun2,Local),
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
                ok = mark_dirty(Index,hypernumber)
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
%% @doc  Receive registration for a hypernumber
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


%% @spec delete_cells(#ref) -> ok
%% @doc deletes the value of a cell (but not any formatting information)
delete_cells(Ref = #ref{ref = {range, {X, Y, X, Y}}}) ->
    delete_cells(Ref =#ref{ref = {cell, {X, Y}}});
delete_cells(Ref = #ref{ref = {range, {X1, Y1, X2, Y2}}}) ->
    List = range_to_list(Ref, X1, Y1, X2, Y2),
    lists:map(fun(X) -> delete_cells(X) end, List);
delete_cells(Ref = #ref{ref = {cell, {X, Y}}}) ->
    Data = [{formula, [], []},
            {rawvalue, [],[]},
            {value, [], []},
            {'__ast', [], []},
            {'__recompile', [], []},
            {'__shared', [], []},
            {'__area', [], []},
            {'dependency-tree', [], []},
            {parents, [], []}],
    lists:map
      (
      fun({Attr,[],[]}) ->
              hn_db:remove_item(Ref#ref{name=Attr})
      end,
      Data
     ).

%% @spec clear_cells(#ref) -> {ok, ok}
%% @doc clears the values and all the formats of a cell
clear_cells(Ref = #ref{ref = {range, {X, Y, X, Y}}}) ->
    clear_cells(Ref =#ref{ref = {cell, {X, Y}}});
clear_cells(Ref = #ref{ref = {range, {X1, Y1, X2, Y2}}}) ->
    List = range_to_list(Ref, X1, Y1, X2, Y2),
    lists:map(fun(X) -> clear_cells(X) end, List);
clear_cells(Ref = #ref{ref = {cell, {X, Y}}}) ->
    remove_item(Ref#ref{name = undef}).

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
rectify_range(X1, Y1, X2, Y2) ->
    % in case the range is passed in arsey-backwards
    {X1a, X2a} = if
                     X1 < X2 -> {X1, X2};
                     true    -> {X2, X1}
                 end,
    {Y1a, Y2a} = if
                     Y1 < Y2 -> {Y1, Y2};
                     true    -> {Y2, Y1}
                 end,
    {X1a, Y1a, X2a, Y2a}.

range_to_list(Ref, X1, Y1, X2, Y2) ->
    {X1a, Y1a, X2a, Y2a} = rectify_range(X1, Y1, X2, Y2),
    range_to_list(Ref, X1a, X1a, Y1a, X2a, Y2a, []).

range_to_list(Ref, Reset, X, Y, X, Y, Acc) -> [Ref#ref{ref = {cell, {X, Y}}} | Acc];
range_to_list(Ref, Reset, X2, Y1, X2, Y2, Acc) ->
    range_to_list(Ref, Reset, Reset, Y1+1, X2, Y2,
                  [Ref#ref{ref = {cell, {X2, Y1}}} | Acc]);
range_to_list(Ref, Reset, X1, Y1, X2, Y2, Acc) ->
    range_to_list(Ref, Reset, X1 + 1, Y1, X2, Y2,
                  [Ref#ref{ref = {cell, {X1, Y1}}} | Acc]).

copy_attributes([],To)       -> {ok, ok};
copy_attributes([{_, Ref, V} | T], To) ->
    #ref{name = Name} = Ref,
    Addr=To#ref{name = Name},
    hn_main:set_attribute(Addr, V),
    copy_attributes(T, To).

%% the last parameter returned is whether dates and integers should be incremented
%% this can only be true for a vertical or horizontal drag (returning 'y' and 'x')
%% or is otherwise false
%% cell to cell drag'n'drop
is_valid_d_n_d(#ref{ref = {cell, A}}, #ref{ref = {cell, A}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#ref{ref = {cell, {X, Y1}}}, #ref{ref = {cell, {X, Y2}}}) ->
    {ok, single_cell, y};
is_valid_d_n_d(#ref{ref = {cell, {X1, Y}}}, #ref{ref = {cell, {X2, Y}}}) ->
    {ok, single_cell, x};
is_valid_d_n_d(#ref{ref = {cell, _}}, #ref{ref = {cell, _}}) ->
    {ok, single_cell, false};
%% cell to range drag'n'drop
is_valid_d_n_d(#ref{ref = {cell, {FX, FY}}}, #ref{ref = {range, {TX, TY1, TX, TY2}}}) ->
    {ok, cell_to_range, y};
is_valid_d_n_d(#ref{ref = {cell, {FX, FY}}}, #ref{ref = {range, {TX1, TY, TX2, TY}}}) ->
    {ok, cell_to_range, x};
is_valid_d_n_d(#ref{ref = {cell, {FX, FY}}}, #ref{ref = {range, {TX1, TY1, TX2, TY2}}}) ->
    {ok, cell_to_range, false};
%% range to range drag'n'drop
is_valid_d_n_d(#ref{ref = {range, Range}}, #ref{ref = {range, Range}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#ref{ref = {range, {FX, FY1, FX, FY2}}}, #ref{ref = {range, TRange}}) ->
    {TX1, TY1, TX2, TY2} = TRange,
    case ((TY2 - TY1) - (FY2 - FY1)) of
        0    -> {ok, col_range_to_range};
        true -> {error, "target range is not the same height as the source range"}
    end;
is_valid_d_n_d(#ref{ref = {range, {FX1, FY, FX2, FY}}}, #ref{ref = {range, TRange}}) ->
    {TX1, TY1, TX2, TY2} = TRange,
    case ((TX2 - TX1) - (FX2 - FX1)) of
        0    -> {ok, row_range_to_range};
        true -> {error, "target range is not the same width as the source range"}
    end;
is_valid_d_n_d(#ref{ref = {range, FRange}}, #ref{ref = {range, TRange}}) ->
    {error, "from range is invalid"};
is_valid_d_n_d(_, _) -> {error, "not valid either"}.

filter_for_drag_n_drop(List) -> fl(List, [], []).

fl([], A, B)                                                 -> {A, B};
fl([{_, {ref, _, _, _, value, _}, _}| T], A, B)              -> fl(T, A, B);
fl([{_, {ref, _, _, _, rawvalue, _}, _}| T], A, B)           -> fl(T, A, B);
fl([{_, {ref, _, _, _, parents, _}, _}| T], A, B)            -> fl(T, A, B);
fl([{_, {ref, _, _, _, 'dependancy-tree', _}, _}| T], A, B)  -> fl(T, A, B);
fl([{_, {ref, _, _, _, '__ast', _}, _}| T], A, B)            -> fl(T, A, B);
fl([{_, {ref, _, _, _, formula, _}, F}| T], A, B)            -> fl(T, [F | A], B);
fl([H | T], A, B)                                            -> fl(T, A, [H | B]).

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
notify_remote(Item=#hn_item{addr=#ref{site=Site,path=Path,name=Name}}) ->
    case atom_to_list(Name) of
        [$_, $_|_R] -> ok; % names of form '__name' are not notified to front-end
        Other       -> MsgXml=hn_util:item_to_xml(Item),
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
    Return=list_hn(T,hn_util:add_uniq(List,Hn)),
    Return.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% server side drag'n'drop                                                    %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec cut_n_paste(From, To) -> ok;
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination  then deletes the original
%% (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
cut_n_paste(From, To) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    -> copy_cut_drag_n_paste_drop(From, To, false);
        {ok, 'onto self'}    -> {ok, ok};
        {ok, cell_to_range}  -> copy_cut_drag_n_paste_drop2(From, To, false);
        {ok, range_to_range} -> exit("erk!")
    end,
    delete_cells(From).

%% @spec copy_n_paste(From, To) -> ok;
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
copy_n_paste(From, To) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    -> copy_cut_drag_n_paste_drop(From, To, false);
        {ok, 'onto self'}    -> {ok, ok};
        {ok, cell_to_range}  -> copy_cut_drag_n_paste_drop2(From, To, false);
        {ok, range_to_range} -> exit("erk!")
    end.

is_valid_c_n_p(From, From)                                    -> {ok, 'onto self'};
is_valid_c_n_p(#ref{ref = {cell, _}}, #ref{ref = {cell, _}})  -> {ok, single_cell};
is_valid_c_n_p(#ref{ref = {cell, _}}, #ref{ref = {range, _}}) -> {ok, cell_to_range};
is_valid_c_n_p(#ref{ref = {range, {FX1, FY1, FX2, FY2}}}, #ref{ref = {range, {TX1, TY1, TX2, TY2}}}) ->
    % two ranges for copy'n'paste (and cut'n'paste) must be the same shape
    X = TX2 - TX1,
    Y = TY2 - TY1,
    case {FX2 - FX1, FY2 - FY1} of
        {X, Y} -> {ok, range_to_range};
        _Other -> exit(invalid_range)
    end.

%% drag'n'drop has an interesting specification
%% (taken from Excel 2007 help)
%% currently excludes customer autofil

%% Initial selection   Extended series 
%% -----------------   ---------------
%% 1, 2, 3             4, 5, 6,... 
%% 9:00 10:00,         11:00, 12:00,... 
%% Mon Tue,            Wed, Thu,... 
%% Monday Tuesday,     Wednesday, Thursday,... 
%% Jan Feb,            Mar, Apr,... 
%% Jan, Apr            Jul, Oct, Jan,... 
%% Jan-07, Apr-07      Jul-07, Oct-07, Jan-08,... 
%% 15-Jan, 15-Apr      15-Jul, 15-Oct,... 
%% 2007, 2008          2009, 2010, 2011,... 
%% 1-Jan, 1-Mar        1-May, 1-Jul, 1-Sep,... 
%% Qtr3                Qtr4, Qtr1, Qtr2,... 
%% Q3                  Q4, Q1, Q2,... 
%% Quarter3            Quarter4, Quarter1, Quarter2,... 
%% text1, textA text2, textA, text3, textA,... 
%% 1st Period          2nd Period, 3rd Period,... 
%% Product 1           Product 2, Product 3,... 
%% 1 Product           2 Product, 3 Product

%% @spec drag_n_drop(From, To) -> ok;
%% @doc takes the formula and formats from a cell and drag_n_drops 
%% them over a destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
drag_n_drop(From, To) ->
    case is_valid_d_n_d(From, To) of
        {ok, single_cell, Incr}   -> copy_cut_drag_n_paste_drop(From, To, Incr);
        {ok, 'onto self', _Incr}  -> {ok, ok};
        {ok, cell_to_range, Incr} -> copy_cut_drag_n_paste_drop2(From, To, Incr)
    end.

copy_cut_drag_n_paste_drop(From, To, Incr) ->
    FromList = get_item(From),
    {Contents, FilteredList} = filter_for_drag_n_drop(FromList),
    Output = case Contents of
                 [Contents2] -> superparser:process(Contents2);
                 []          -> ""
             end,
    #ref{ref = {cell, {FX, FY}}} = From,
    #ref{ref = {cell, {TX, TY}}} = To,
    case Output of
        {formula, Formula} ->
            {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {FX, FY}),
            NewToks = offset(Toks, (TX - FX), (TY - FY)),
            NewFormula = make_formula(NewToks),
            hn_main:set_cell(To#ref{name=formula}, NewFormula);
        [{Type, V},  _A, _F] ->
            V2 = case Incr of
                     false ->
                         tconv:to_s(V);
                     _     -> 
                         case Type of
                             int      -> NewV = V + diff(FX, FY, TX, TY, Incr),
                                         tconv:to_s(NewV);
                             datetime -> {datetime, {Y, M, D}, T} = V,
                                         D2 = D + diff(FX, FY, TX, TY, Incr),
                                         tconv:to_s({datetime, {Y, M, D2}, T}); 
                             _        -> tconv:to_s(V)
                         end
                 end,
            hn_main:set_cell(To#ref{name=formula}, V2);
        []  -> delete_cells(To)
    end,
    % You want to copy the attributes AFTER setting the value
    % because setting a value sets the default alignment and format
    % and if the source cell has been reformatted after the data was entered
    % you want to carry that forward.
    {ok, ok} = copy_attributes(FilteredList, To),
    ok.

copy_cut_drag_n_paste_drop2(From, To, Incr) ->
    #ref{ref = {range, {X1, Y1, X2, Y2}}} = To,
    List = range_to_list(To, X1, Y1, X2, Y2),
    lists:map(fun(X) -> copy_cut_drag_n_paste_drop(From, X, Incr) end, List).

offset(Toks, XOffset, YOffset) -> offset(Toks, XOffset, YOffset, []).

offset([], _XOffset, _YOffset, Acc) -> lists:reverse(Acc);
offset([{ref, Col, Row, Path, Cell} | T], XOffset, YOffset, Acc) ->
    {XDollar, X, YDollar, Y} = parse_cell(Cell),
    NewCell = make_cell(XDollar, X, XOffset, YDollar, Y, YOffset),
    NewRef = {ref, Col, Row, Path, NewCell},
    offset(T, XOffset, YOffset, [NewRef | Acc]);
offset([H | T], XOffset, YOffset, Acc) ->
    offset(T, XOffset, YOffset, [H | Acc]).

parse_cell(Ref) ->
    {XDollar, Rest} = case Ref of
                          [$$ | T1] -> {true, T1};
                          _         -> {false, Ref}
                      end,
    Fun = fun(XX) ->
                  if XX < 97  -> false;
                     XX > 122 -> false;
                     true     -> true
                  end
          end,
    {XBits, YBits} = lists:partition(Fun,string:to_lower(Rest)),
    {YDollar, Y} = case YBits of
                       [$$ | T2] -> {true, T2};
                       _         -> {false, YBits}
                   end,
    {XDollar, tconv:to_i(XBits), YDollar, list_to_integer(Y)}.

make_cell(false, X, XOffset, false, Y, YOffset) ->
    tconv:to_b26(X + XOffset)++tconv:to_s(Y + YOffset);
make_cell(true, X, XOffset, false, Y, YOffset) ->
    [$$]++tconv:to_b26(X)++tconv:to_s(Y + YOffset);
make_cell(false, X, XOffset, true, Y, YOffset) ->
    tconv:to_b26(X + XOffset)++[$$]++tconv:to_s(Y);
make_cell(true, X, XOffset, true, Y, YOffset)  -> 
    [$$]++tconv:to_b26(X)++[$$]++tconv:to_s(Y).

make_formula(Toks) -> mk_f(Toks, []).

mk_f([], Acc)                        -> "="++lists:flatten(lists:reverse(Acc));
mk_f([{ref, _, _, _, Ref} | T], Acc) -> mk_f(T, [Ref | Acc]);
mk_f([{atom, H} | T], Acc)           -> mk_f(T, [H | Acc]);
mk_f([{H} | T], Acc)                 -> mk_f(T, [atom_to_list(H) | Acc]).

diff(FX, FY, TX, TY, x) -> TX - FX;
diff(FX, FY, TX, TY, y) -> TY - FY.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Debugging interfaces                                                       %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
copy_DEBUG() ->
    copy_DEBUG2("drag_n_drop"),
    copy_DEBUG2("copy_n_paste"),
    copy_DEBUG2("cut_n_paste").

copy_DEBUG2(FunName) ->
    delete_cells_DEBUG(FunName),
    write_value(FunName, FunName++" - cell to cell", {1, 1}, [bold, underline]),

    % cell to cell drop down
    write_value(FunName, "integer below", {1, 2}, [bold]),
    write_value(FunName, "1", {1, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 3}}, {cell, {1, 4}}),
    colour(FunName, {1, 3}, "cyan"),

    write_value(FunName, "float below", {1, 5}, [bold]),
    write_value(FunName, "1.1", {1, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 6}}, {cell, {1, 7}}),
    colour(FunName, {1, 6}, "cyan"),

    write_value(FunName, "string below", {1, 8}, [bold]),
    write_value(FunName, "hey!", {1, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 9}}, {cell, {1, 10}}),
    colour(FunName, {1, 9}, "cyan"),

    write_value(FunName, "date below", {1, 11}, [bold]),
    write_value(FunName, "1/2/3 4:5:6", {1, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 12}}, {cell, {1, 13}}),
    colour(FunName, {1, 12}, "cyan"),

    write_value(FunName, "boolean below", {1, 14}, [bold]),
    write_value(FunName, "true", {1, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 15}}, {cell, {1, 16}}),
    colour(FunName, {1, 15}, "cyan"),

    % cell to cell across
    write_value(FunName, "integer beside", {2, 2}, [bold]),
    write_value(FunName, "1", {2, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 3}}, {cell, {3, 3}}),
    colour(FunName, {2, 3}, "cyan"),

    write_value(FunName, "float beside", {2, 5}, [bold]),
    write_value(FunName, "1.1", {2, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 6}}, {cell, {3, 6}}),
    colour(FunName, {2, 6}, "cyan"),

    write_value(FunName, "string beside", {2, 8}, [bold]),
    write_value(FunName, "hey!", {2, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 9}}, {cell, {3, 9}}),
    colour(FunName, {2, 9}, "cyan"),

    write_value(FunName, "date beside", {2, 11}, [bold]),
    write_value(FunName, "1/2/3 4:5:6", {2, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 12}}, {cell, {3, 12}}),
    colour(FunName, {2, 12}, "cyan"),

    write_value(FunName, "boolean beside", {2, 14}, [bold]),
    write_value(FunName, "true", {2, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 15}}, {cell, {3, 15}}),
    colour(FunName, {2, 15}, "cyan"),

    make_thin(FunName, 4),

    write_value(FunName, "Drag'n'Drop - cell to down 'thin' range", {5, 1}, [bold, underline]),
    % cell to range down
    write_value(FunName, "integer below", {5, 2}, [bold]),
    write_value(FunName, "1", {5, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 3}}, {range, {5, 4, 5, 5}}),
    colour(FunName, {5, 3}, "cyan"),

    write_value(FunName, "float below", {6, 5}, [bold]),
    write_value(FunName, "1.1", {6, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {6, 6}}, {range, {6, 7, 6, 8}}),
    colour(FunName, {6, 6}, "cyan"),

    write_value(FunName, "string below", {5, 8}, [bold]),
    write_value(FunName, "hey!", {5, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 9}}, {range, {5, 10, 5, 11}}),
    colour(FunName, {5, 9}, "cyan"),

    write_value(FunName, "date below", {6, 11}, [bold]),
    write_value(FunName, "1/2/3 4:5:6", {6, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {6, 12}}, {range, {6, 13, 6, 14}}),
    colour(FunName, {6, 12}, "cyan"),

    write_value(FunName, "boolean below", {5, 14}, [bold]),
    write_value(FunName, "true", {5, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 15}}, {range, {5, 16, 5, 17}}),
    colour(FunName, {5, 15}, "cyan"),

    write_value(FunName, "Drag'n'Drop - cell to across 'thin' range", {7, 1}, [bold, underline]),
    % cell to range down
    write_value(FunName, "integer beside", {7, 2}, [bold]),
    write_value(FunName, "1", {7, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 3}}, {range, {7, 4, 8, 4}}),
    colour(FunName, {7, 3}, "cyan"),

    write_value(FunName, "float beside", {7, 5}, [bold]),
    write_value(FunName, "1.1", {7, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 6}}, {range, {7, 7, 8, 7}}),
    colour(FunName, {7, 6}, "cyan"),

    write_value(FunName, "string beside", {7, 8}, [bold]),
    write_value(FunName, "hey!", {7, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 9}}, {range, {7, 10, 8, 10}}),
    colour(FunName, {7, 9}, "cyan"),

    write_value(FunName, "date beside", {7, 11}, [bold]),
    write_value(FunName, "1/2/3 4:5:6", {7, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 12}}, {range, {7, 13, 8, 13}}),
    colour(FunName, {7, 12}, "cyan"),

    write_value(FunName, "boolean beside", {7, 14}, [bold]),
    write_value(FunName, "true", {7, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 15}}, {range, {7, 16,  8, 16}}),
    colour(FunName, {7, 15}, "cyan"),

    make_thin(FunName, 9),

    % cell to 'thick' ranges don't increment even if they are drag'n'drop
    write_value(FunName, "Drag'n'Drop - cell to 'thick' range", {10,1}, [bold, underline]),

    write_value(FunName, "integer", {10,2}, [bold]),
    write_value(FunName, "1", {10,3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {10,3}}, {range, {10,4, 11, 10}}),
    colour(FunName, {10,3}, "cyan"),

    % same as above but arsey backwards range
    write_value(FunName, "testing inverted range", {10,11}, [bold]),
    write_value(FunName, "1", {10,12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {10,12}}, {range, {10, 20, 11, 13}}),
    colour(FunName, {10,3}, "cyan"),

    make_thin(FunName, 12),

    % set up formula data
    write_value(FunName, "data for formula", {13,2}, [bold]),
    colour(FunName, {13, 2}, "yellow"),
    write_value(FunName, "1", {13, 3}, []),
    write_value(FunName, "22", {13, 4}, []),
    write_value(FunName, "333", {13, 5}, []),
    write_value(FunName, "4444", {13, 6}, []),
    write_value(FunName, "5555", {13, 7}, []),
    write_value(FunName, "11111", {14, 3}, []),
    write_value(FunName, "222222", {14, 4}, []),
    write_value(FunName, "333333", {14, 5}, []),
    write_value(FunName, "4444444", {14, 6}, []),
    write_value(FunName, "55555555", {14, 7}, []),
    colour(FunName, {13, 3}, "orange"),
    colour(FunName, {13, 4}, "orange"),
    colour(FunName, {13, 5}, "orange"),
    colour(FunName, {13, 6}, "orange"),
    colour(FunName, {13, 7}, "orange"),
    colour(FunName, {14, 3}, "orange"),
    colour(FunName, {14, 4}, "orange"),
    colour(FunName, {14, 5}, "orange"),
    colour(FunName, {14, 6}, "orange"),
    colour(FunName, {14, 7}, "orange"),

    make_thin(FunName, 15),

    % some formula stuff
    write_value(FunName, "formula below", {16, 2}, [bold]),
    write_value(FunName, "=m3+n3", {16, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 3}}, {range, {16, 4, 17, 6}}),
    colour(FunName, {16, 3}, "cyan"),

    write_value(FunName, "fix col formula below", {16, 7}, [bold]),
    write_value(FunName, "=$m3+n3", {16, 8}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 8}}, {range, {16, 9, 17, 11}}),
    colour(FunName, {16, 8}, "cyan"),

    write_value(FunName, "fix row formula below", {16, 12}, [bold]),
    write_value(FunName, "=m$3+n3", {16, 13}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 13}}, {range, {16, 14, 17, 16}}),
    colour(FunName, {16, 13}, "cyan"),

    write_value(FunName, "fix row and col formula below", {16,17}, [bold]),
    write_value(FunName, "=$m$3+n3", {16, 18}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 18}}, {range, {16, 19, 17, 21}}),
    colour(FunName, {16, 18}, "cyan").

%    io:format("in drag_n_drop should go through (4)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 8}}, {cell, {2, 8}}),
%    io:format("in drag_n_drop should go through (5)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 9}}, {cell, {2, 10}}),    
%    io:format("in drag_n_drop should go through (6)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 1}}, {range, {2, 2, 3, 6}}).
%    io:format("in drag_n_drop should go through (7)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {4, 1}}, {range, {4, 2, 4, 6}}),
%    io:format("in drag_n_drop should go through (8)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 1}}, {range, {6, 1, 8, 1}}).

cut_n_drag_n_copy_n_drop_n_paste(FunName, From, To) ->
    Site = "http://127.0.0.1:9000",
    From1 = #ref{site =  Site, path = [FunName], ref = From},
    To1 = #ref{site =  Site, path = [FunName], ref = To},
    erlang:apply(?MODULE, list_to_atom(FunName), [From1, To1]).

write_value(Path, Value, {X, Y}, Attributes) ->
    Site = "http://127.0.0.1:9000",
    Cell = #ref{site = Site, path = [Path], ref = {cell, {X, Y}}},
    hn_main:set_cell(Cell#ref{name=formula}, Value),
    write_attributes(Attributes, {X, Y}, Cell).

write_attributes([], _, _) -> ok;
write_attributes([Attr | T], {X, Y}, Cell) ->
    {Name, V} = case Attr of
                    bold              -> {'font-weight', "bold"};
                    underline         -> {'text-decoration', "underline"};
                    {colour, Colour}  -> {'background-color', Colour};
                    thin              -> {width, 30}
                end,
    Addr = Cell#ref{name=Name},
    hn_main:set_attribute(Addr, V),
    write_attributes(T, {X, Y}, Cell).

colour(Path, {X, Y}, Colour) ->
    Site = "http://127.0.0.1:9000",
    Cell = #ref{site = Site, path = [Path], ref = {cell, {X, Y}}},
    write_attributes([{colour, Colour}], {X, Y}, Cell).

make_thin(Path, X) ->
    Site = "http://127.0.0.1:9000",
    Ref = #ref{site = Site, path = [Path], ref = {column, X}, name = width},
    hn_main:set_attribute(Ref, "20").

delete_cells_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    Target = #ref{site = Site, path = [Path], ref = {range, {1, 1, 30, 30}}},
    delete_cells(Target).

clear_cells_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    Target = #ref{site = Site, path = [Path], ref = {range, {1, 1, 30, 30}}},
    clear_cells(Target).


