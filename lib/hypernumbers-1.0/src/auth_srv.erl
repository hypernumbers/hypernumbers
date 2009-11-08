%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc
%%% 
%%% == Overview ==
%%% A gen server for managing how pages are displayed and to whom.   
%%%
%%% The auth_srv holds a variety of information about how URL's are
%%% handled in the hypernumbers server:
%%% <ul>
%%% <li>valid paths</li>
%%% <li>security</li>
%%% <li>default pages</li>
%%% <li>pages that can be viewed</li>
%%% <li>(generation of sequential urls)</li>
%%% </ul>
%%% Some of these controls are stored against users and groups and some are not.
%%% Each will be discussed separately here. 
%%%
%%% == Users And Groups ==
%%%
%%% Each user of the system can belong to a number of groups. There
%%% are 2 special users and groups:
%%% <ul>
%%% <li>*</li>
%%% <li>anonymous (the atom anonymous not the string anonymous)</li>
%%% </ul>
%%% The user '*' matches all users and user <code>anonymous</code> matches
%%% unlogged in users only. Ditto for groups.
%%% Because permissions, pages views etc are keyed against users
%%% <em>and</em> groups there have to be rules about how the controls
%%% compose when many of them pertain. These will be discussed in the
%%% various sub-sections discussing the particular controls.
%%% == Paths ==
%%% === Overview ===
%%% Paths form the backbone of the system. A path is simply that part
%%% of the URL to the right of the port and terminating with the final
%%% slash:<br /><br />
%%% <code>http://example.com:1234/this/is/the/path/but?this=is not</code><br /><br />
%%% has a path of <code>["this", "is", "the", "path"]</code><br />
%%% === 'Normal' Paths ===
%%% A 'normal' path can contain standard latin letters and numbers only.
%%%
%%% Other characters are reserved for use in programmatic path segments
%%% which are delimited by square brackets:<br />
%%% <code>http://example.com:1234/this/is/a/[programmatic segement]/</code><br />
%%% === Programmatic Paths ===
%%% Only 2 programatic paths are implemented so far:
%%% <ul>
%%% <li>"[*]"  - the parsimonious wild card - matches a single
%%% path segement</li>
%%% <li>"[**]" - the multiple wild card - matches multiple path segments</li>
%%% </ul>
%%% The multiple wild card will only match the smallest segment if it is used
%%% interstitially. Consider the following match segment:<br />
%%% <code>["some", "**", "wild"]</code><br />
%%% were the following path to be matched against it:<br />
%%% <code>["some", "path", "to", "a", "wild"]</code><br />
%%% would match, but:<br />
%%% <code>["some", "path", "to", "a", "wild", "or", "another", "wild"]</code><br />
%%% would not.
%%% == Cascade - General Principles ==
%%% Controls can cascade down path trees, or between users and groups, and the
%%% general principle of cascade is that in the event of conflict the more
%%% specific one wins:
%%% <ul>
%%% <li>a user control beats a group control</li>
%%% <li>all user/group controls on a path are exhausted before looking for
%%% a less specific path</li>
%%% <li>a fully specified path beats one with a parsimonious wild card</li>
%%% <li>a parsimonious wild card beats a multiple wild card</li>
%%% <li>a wild card further down the path tree beats one higher up
%%% (even if the lower is multiple and the higher is a singe wild card)</li>
%%% </ul>
%%% == Controls ==
%%% === Introduction ===
%%% There are 3 controls implemented:
%%% <ul>
%%% <li><code>acl</code>'s - access control lists</li>
%%% <li><code>default views</code> - specify the default view for a path</li>
%%% <li><code>views</code> - determines what views are served to whom</li>
%%% </ul>
%%% === ACL's ===
%%% Access control lists specify what resources a user can get access to.
%%% They grant permissions. The current permission set is:
%%% <ul>
%%% <li><code>read</code></li>
%%% <li><code>write</code></li>
%%% </ul>
%%% The permissions set will be extended later.
%%% A permissions set is bound to a user or group and a path. Permissions
%%% are <strong>additive</strong>. Consider the following set of permssions
%%% on a particular path:<br />
%%% <code>{user, "Alice"}, [read]<br />
%%% {group, "restauranteuses"}, [read, write]<br /></code>
%%% A user with the profile <code>{Alice, ["cryptographer", "admin"]</code>
%%% would have permissions of <code>[read]</code> whereas a user with the
%%% profile of <code>{Alice, ["restauranteuses", "admin"]</code> would
%%% have permissions of <code>[read, write]</code>. A user of profile
%%% <code>{Bobbie, ["restauranteuses", "admin"]</code> would have permissions
%%% of <code>write</code>.
%%% === Defaults Views ===
%%% Defaults views are bound to particular paths and determine what
%%% view is served by the ungarnished url (ie one that terminated in a '/').
%%% The default view is identical for all users and groups. If there is
%%% no default view for a particular path the spreadsheet view
%%% <code>_global/spreadsheet</code> is served.
%%% Default views cascade down from wild cards. Consider the following paths:<br />
%%% <code>["a", "[**]"]</code><br />
%%% <code>["a", "b", "[**]"]</code><br />
%%% <code>["a", "b", "[*]"]</code><br />
%%% <code>["a", "b", "c"]</code><br />
%%% If there was no default view at <code>["a", "b", "c"]</code> then
%%% the default at <code>["a", "b", "[*]"]</code> would be used. If none
%%% there then the default at <code>["a", "b", "[**]"]</code>, and if not,
%%% then that at <code>["a", "[**]"]</code>
%%% Default views can be overridded by the override view specified in the
%%% view control.
%%% === Views ===
%%% Views are the list of all possible files that can be served to a user
%%% on a particular path. The files are specified relative to a root
%%% and can be of the form <code>_global/<em>filename</em></code> or
%%% <code><em>username</em>/<em>filename</em></code>
%%% across the piece...
%%% Views are bound to a <code>user</code> or a <code>group</code> and
%%% contain 2 parts:
%%% <ul>
%%% <li>an overriding view</li>
%%% <li>a list of views</li>
%%% </ul>
%%% There are two special views:
%%% <ul>
%%% <li>* - this matches all views defined in the user's view directory
%%% ie views of the form <code><em>username</em>/<em>viewname</em></code></li>
%%% <li>** - this matches all views in the <code>_global</code> directory
%%% and <strong>all</strong> user views</li>
%%% </ul>
%%% A user who is part of many groups can therefore have more than
%%% one 'overriding view' on a particular page. The overrides resolve
%%% in the following order:
%%% <ul>
%%% <li>the override bound to the username takes prority</li>
%%% <li>only if no overrideis bound to a username do the
%%% per group overrides come into play. If more than one group
%%% matches then <em>an</em> override from them will be applied
%%% - which one is undefined</li>
%%% </ul>
%%% This per user/group override takes priority over to the main
%%% default view and allows it to be overriden on a per case basis.
%%% Views cascade. Consider the following paths and the views bound to
%%% them (they are associated with the same users):<br />
%%% <code>{[], ["first", "second"]}</code> bound to
%%% <code>["a", "[**]"]</code><br />
%%% <code>{[]. ["third"]}</code> bound to <code>["a", "b", "c"]</code><br />
%%% The views that the user can see at <code>["a", "b", "c"]</code> is
%%% <code>["first", "second", "third"]</code> and the default view
%%% is undefined.
%%% Override views cascade in a similar manner - user to group at the same
%%% path and then up to the next less specific path, through the users and
%%% groups there, etc, etc.
%%% @todo extend paths from latin to unicode paths
%%% @todo implement path restrictions - and make sure they occur
%%% @end
%%% Created :  7 Oct 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------

-module(auth_srv).

-behaviour(gen_server).

-include("auth.hrl").

-include_lib("eunit/include/eunit.hrl").

%% API 
-export([start_link/0]).

-export([
         check_get_page/3,
         check_get_page/4,
         get_views/3,
         can_read/3,
         can_write/3,
         can_execute/3,
         add_perm/4,
         add_perm/6,
         add_views/4,
         add_default/4,
         remove_perm/4,
         remove_views/4,
         remove_default/4,
         get_as_json/2,
         get_groups/1
        ]).

-export([clear_all_perms_DEBUG/1]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TABLE, "auth_srv").
-define(KEY, "auth_tree").
-define(INDEX, "hypernumbers/index").

-record(state, {trees = [], file = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, [[Dir]]} = init:get_argument(dets_dir),
    Trees = load_trees(Dir, ?TABLE),
    {ok, #state{trees = Trees, file = ?TABLE}}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    #state{trees = Tr, file = File} = State,
    Return1 =
        case Request of
            {check_get_page, Host, AS, P} ->
                {Host, check_get_page1(get(Host, Tr), AS, P), false};
            {check_get_page, Host, AS, P, Gui} ->
                {Host, check_get_page1(get(Host, Tr), AS, P, Gui), false};
            {get_views, Host, AS, P} ->
                {Host, get_views1(get(Host, Tr), AS, P), false};
            {can_read, Host, AS, P} ->
                {Host, can_read1(get(Host, Tr), AS, P), false};
            {can_write, Host, AS, P} ->
                {Host, can_write1(get(Host, Tr), AS, P), false};
            {can_execute, Host, AS, TS} ->
                {Host, can_execute1(get(Host, Tr), AS, TS), false};
            {add_perm, Host, AL, Pg, Pm, Df, Gs} ->
                {Host, add_perm1(get(Host, Tr), AL, Pg, Pm, Df, Gs), true};
            {add_views, Host, AL, Pg, Gs} ->
                {Host, add_views1(get(Host, Tr), AL, Pg, Gs), true};
            {add_default, Host, AL, Pg, Df} ->
                {Host, add_default1(get(Host, Tr), AL, Pg, Df), true};
            {rem_perm, Host, AL, Pg, Pm} ->
                {Host, remove_perm1(get(Host, Tr), AL, Pg, Pm), true};
            {remove_views, Host, AL, Pg, Gs} ->
                {Host, remove_views1(get(Host, Tr), AL, Pg, Gs), true};
            {remove_def, Host, AL, Pg, Df} ->
                {Host, remove_default1(get(Host, Tr), AL, Pg, Df), true};
            {get_as_json, Host, Pg} ->
                {Host, get_as_json1(get(Host, Tr), Pg), false};
            {get_groups, Host} ->
                {Host, get_groups1(get(Host, Tr)), false};
            {clear_all_perms, Host} ->
                {Host, gb_trees:empty(), true}
        end,
      {Reply, NewTr} =
        case Return1 of
            {Host2, Return2, Write} ->
                case Write of
                    true  -> {ok, update_trees(File, Host2, Return2, Tr)};
                    false -> {Return2, Tr}
                end;
            Other               -> {ok, Other}
        end,
    {reply, Reply, State#state{trees = NewTr}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Public API
%%%===================================================================
start(Hosts) ->
    gen_server:call(auth_srv, {start, Hosts}).

check_get_page(Host, AuthSpec, Page) -> 
    gen_server:call(auth_srv, {check_get_page, Host, AuthSpec, Page}).

check_get_page(Host, AuthSpec, Page, Gui) ->
    gen_server:call(auth_srv, {check_get_page, Host, AuthSpec, Page, Gui}).

get_views(Host, AuthSpec, Page) ->
    gen_server:call(auth_srv, {get_views, Host, AuthSpec, Page}).

can_read(Host, AuthSpec, Page) ->
    gen_server:call(auth_srv, {can_read, Host, AuthSpec, Page}).

can_write(Host, AuthSpec, Page) ->
    gen_server:call(auth_srv, {can_write, Host, AuthSpec, Page}).

can_execute(Host, AuthSpec, Trans_signature) ->
    gen_server:call(auth_srv, {can_execute, Host, AuthSpec, Trans_signature}).

add_perm(Host, AuthList, Page, Perm) ->
    gen_server:call(auth_srv, {add_perm, Host, AuthList, Page, Perm,
                               ?INDEX, [?INDEX]}).
    
add_perm(Host, AuthList, Page, Perm, Def, Views) ->
    gen_server:call(auth_srv, {add_perm, Host, AuthList, Page, Perm,
                               Def, Views}).

add_views(Host, AuthList, Page, Views) ->
    gen_server:call(auth_srv, {add_views, Host, AuthList, Page, Views}).

add_default(Host, AuthList, Page, Gui) ->
    gen_server:call(auth_srv, {add_default, Host, AuthList, Page, Gui}).

remove_perm(Host, AuthList, Page, Perm) ->
    gen_server:call(auth_srv, {rem_perm, Host, AuthList, Page, Perm}).

remove_views(Host, AuthList, Page, Views) ->
    gen_server:call(auth_srv, {rem_views, Host, AuthList, Page, Views}).

remove_default(Host, AuthList, Page, Gui) ->
    gen_server:call(auth_srv, {rem_default, Host, AuthList, Page, Gui}).

get_as_json(Host, Page) ->
    gen_server:call(auth_srv, {get_as_json, Host, Page}).

get_groups(Host) ->
    gen_server:call(auth_srv, {get_groups, Host}).

clear_all_perms_DEBUG(Host) ->
    gen_server:call(auth_srv, {clear_all_perms, Host}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_get_page1(Tree, {User, Groups}, Page) ->
Fun = fun(X) -> case get_control(X, User, Groups) of
                        none     -> {return, '404'};
                        no_match -> {return, '401'};
                        Ctl      -> #control{perms = P} = Ctl,
                                    case has_perm(P, read) of
                                        true  -> #control{def_view = G} = Ctl,
                                                 {html, G}; 
                                        false -> {return, '401'}
                                    end
                    end
          end,
    check_get(Tree, Page, Fun).

check_get_page1(Tree, {User, Groups}, Page, View) -> 
    Fun = fun(X) -> case get_control(X, User, Groups) of
                        none     -> {return, '404'};
                        no_match -> {return, '401'};
                        Ctl      -> #control{views = Vws} = Ctl,
                                    case contains(View, Vws) of
                                        true  -> {html, View}; 
                                        false -> {return, '401'}
                                    end
                    end
          end,
    check_get(Tree, Page, Fun).
    
get_views1(Tree, {User, Groups}, Page) ->
    Fun = fun(X) -> case get_control(X, User, Groups) of
                        none -> {return, '404'};
                        Ctl  -> #control{perms = P, views = Gs} = Ctl,
                                case has_perm(P, read) of
                                    true  -> Gs; 
                                    false -> []
                                end
                    end
          end,
    check_get(Tree, Page, Fun).

can_read1(Tree, {User, Groups}, Page) ->
    Fun = fun(X) ->
                  case get_control(X, User, Groups) of
                      none     -> false;
                      no_match -> false;
                      Ctl      -> #control{perms = P} = Ctl,
                                  has_perm(P, read)
                  end
          end,
    check_get(Tree, Page, Fun).

can_write1(Tree, {User, Groups}, Page) -> 
    Fun = fun(X) ->
                  case get_control(X, User, Groups) of
                      none     -> false;
                      no_match -> false;
                      Ctl      -> #control{perms = P} = Ctl,
                                  has_perm(P, write)
                  end
          end,
    check_get(Tree, Page, Fun).

can_execute1(_Tree, _AuthSpec, _Trans_signature) -> {erk, not_written}.

add_perm1(Tree, AuthList, Page, Perms, Def, Views) ->
    Fun = fun(#control{def_view = D1, views = Vw1, perms = P1} = _Old,
              #control{def_view = D2, views = Vw2, perms = P2} = _New) ->
                  P3 = hslists:dedup([P1, P2]),
                  % push both the old and new defaults onto the list of views
                  Vw3 = hslists:dedup([[D1], [D2], Vw1, Vw2]),
                  #control{def_view = D2, views = Vw3, perms = P3}
          end,
    add_to_control(Tree, AuthList, Page, Perms, Def, Views, Fun).


add_views1(Tree, AuthList, Page, GuiList) ->
    Fun = fun(#control{def_view = D1,  views = Vw1, perms = P1} = _Old,
              #control{def_view = _D2, views = Vw2, perms = _P2} = _New) ->
                  Vw3 = hslists:dedup([Vw1, Vw2]),
                  #control{def_view = D1, views = Vw3, perms = P1}
          end,
    add_to_control(Tree, AuthList, Page, [], [], GuiList, Fun).

add_default1(Tree, AuthList, Page, Default) ->
    Fun = fun(#control{def_view = D1, views = Vw1, perms = P1} = _Old,
              #control{def_view = D2, views = Vw2, perms = _P2} = _New) ->
                  Vw3 = hslists:dedup([[D1], Vw1, Vw2]),
                  #control{def_view = D2, views = Vw3, perms = P1}
          end,
    add_to_control(Tree, AuthList, Page, [], Default, [], Fun).

remove_perm1(Tree, AuthList, Page, Perms) ->
    Fun = fun({K, #control{perms = P1} = Ctl}) ->
            P2 = lists:subtract(P1, Perms),
            case P2 of
                [] -> empty;
                _O -> {K, Ctl#control{perms = P2}}
            end
          end,
    remove_from_control(Tree, AuthList, Page, Fun).    

remove_views1(Tree, AuthList, Page, Views) -> 
    Fun = fun({K, #control{def_view = D1, views = Vw1, perms = P1}}) ->
                  {NewDef, NewViews} =
                      case contains(D1, Views) of
                          true  -> NewG = del(Vw1, Views),
                                   NewD = new_def(NewG, D1),
                                   {NewD, NewG};
                          false -> {D1, del(Vw1, Views)}
                      end,
                  {K, #control{def_view = new_def(NewViews, NewDef),
                               views = NewViews, perms = P1}}
          end,
    remove_from_control(Tree, AuthList, Page, Fun).

remove_default1(Tree, AuthList, Page, Default) ->
    Fun = fun({K, #control{def_view = D1, views = Vw1, perms = P1}}) ->
                  case Default of
                      % gotta have at least one gui 'index'
                      % if you have a control
                      D1 -> {K, #control{def_view = new_def(Vw1, D1),
                                         views = Vw1, perms = P1}};
                      _O -> {K, #control{def_view = D1,  views = Vw1,
                                         perms = P1}}
                  end
          end,
    remove_from_control(Tree, AuthList, Page, Fun).

get_as_json1(Tree, Page) ->
    Fun = fun(X) ->
                  make_json(X, [])
          end,
    check_get(Tree, Page, Fun).

get_groups1(_Tree) -> {erk, not_written}.

update_trees(File, Host, NewTree, Trees) ->
    NewVal = {Host, NewTree},
    NewTrees = case lists:keysearch(Host, 1, Trees) of
                   false -> [NewVal | Trees];
                   _     -> lists:keyreplace(Host, 1, Trees, NewVal)
               end,
    ok = dets:insert(File, {?KEY, NewTrees}),
    NewTrees.

get(Host, Trees) ->
    case lists:keyfind(Host, 1, Trees) of
        false          -> gb_trees:empty();
        {_Host, Other} -> Other
    end.            

%%
%% Tree Helper functions
%%
remove_from_control(Tree, AuthList, [], Fun) ->
    case gb_trees:lookup(controls, Tree) of
        none       -> Tree;
        {value, V} -> NewCtls = remove(AuthList, V, Fun), 
                      gb_trees:enter(controls, NewCtls, Tree)
    end;
remove_from_control(Tree, AuthList, [H | T], Fun) ->
    case gb_trees:lookup(H, Tree) of
        none       -> Tree;
        {value, V} -> NewVal = remove_from_control(V, AuthList, T, Fun),
                      gb_trees:enter(H, NewVal, Tree)
    end.

add_to_control(Tree, AuthList, [], Perms, Def, Views, Fun) ->
    Controls = make_controls(AuthList, Perms, Def, Views),
    case gb_trees:lookup(controls, Tree) of
        none       -> gb_trees:insert(controls, Controls, Tree);
        {value, V} -> NewControls = merge(V, Controls, Fun),
                      gb_trees:enter(controls, NewControls, Tree)
    end;
add_to_control(Tree, AuthList, [H | T], Perms, Def, Views, Fun) ->
    case gb_trees:lookup(H, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = add_to_control(Empty, AuthList, T, Perms, Def, Views, Fun),
                      gb_trees:insert(H, NewVal, Tree);
        {value, V} -> NewVal = add_to_control(V, AuthList, T, Perms, Def, Views, Fun),
                      gb_trees:enter(H, NewVal, Tree)
    end.

%%
%% General Helper functions
%%
make_json(Tree, Seg) ->
    List = gb_trees:to_list(Tree),
    {array, [make_json1(K, V, Seg) || {K, V}  <- List]}.

make_json1(controls, V, Seg) ->
    {struct, [{"seg", Seg},
              {"controls", json_control(V)}]};
make_json1(K, V, _Seg) when is_list(K) ->
    make_json(V, K).


json_control(List) -> json_control1(List, []).

json_control1([], Acc) -> {array, lists:reverse(Acc)};
json_control1([{{Type, Name}, C} | T], Acc) ->
    NewCtl = {struct, [{"def_view", C#control.def_view},
                       {"views", {array, C#control.views}},
                       {"perms", {array, C#control.perms}}]},
    NewAcc = {struct, [{"type", atom_to_list(Type)},
                       {"name", Name},
                       {"control", NewCtl}]},
    json_control1(T, [NewAcc | Acc]).

contains(_Element, [])            -> false;
contains(Element, [Element | _T]) -> true;
contains(Element, [_H | T])       -> contains(Element, T).

load_trees(Dir, Table) ->
    {ok, _} = dets:open_file(Table, [{file, Dir ++ Table}]),
    % if the value of auth_tree is an empty list,
    % create an empty tree and fire it in..
    case dets:lookup(Table, ?KEY) of
        []            -> [];            
        [{?KEY, Val}] -> Val
    end.

del(List1, List2) -> case lists:subtract(List1, List2) of
                         [] -> [?INDEX];
                         L  -> L
                     end.                              

new_def([], _Default)           -> ?INDEX;
new_def([Default | T], Default) -> new_def(T, Default);
new_def([H | _T], _Default)     -> H.

check_get(Tree, [], Fun) ->
    Fun(Tree);
check_get(Tree, [H | T], Fun) ->
    case gb_trees:lookup(H, Tree) of
        none        -> check_programmatic(Tree, T, Fun);
        {value , V} -> check_get(V, T, Fun)
    end.          

check_programmatic(Tree, List, Fun) ->
    case gb_trees:lookup("[*]", Tree) of
        none        -> case gb_trees:lookup("[**]", Tree) of
                           none        -> {return, '404'};
                           {value, V1} -> Fun(V1)
                       end;
        {value, V2} -> check_get(V2, List, Fun)
    end.

make_controls(AuthList, Perms, Def, Views) ->
    make_c1(AuthList, Perms, Def, Views, []).

make_c1([], _Perms, _Def, _Views, Acc)   -> Acc;
make_c1([H | T], Perms, Def, Views, Acc) ->
    Ctl = #control{perms = Perms, def_view = Def, views = Views},
    make_c1(T, Perms, Def, Views, [{H, Ctl} | Acc]).

get_control(Tree, User, Groups) ->
    case gb_trees:lookup(controls, Tree) of
        none          -> none;
        {value, Ctls} ->
            case lists:keyfind({user, User}, 1, Ctls) of
                false     -> case lists:keyfind({user, "*"}, 1, Ctls) of
                                 false     -> get_control2(Groups, Ctls);
                                 {_P, Ctl} -> Ctl
                             end;
                {_P, Ctl} -> Ctl
            end
    end.

get_control2([], _Controls)     -> no_match;
get_control2([H | T], Controls) ->
    case lists:keyfind({group, H}, 1, Controls) of
        false     -> case lists:keyfind({group, "*"}, 1, Controls) of
                         false     -> get_control2(T, Controls);
                         {_P, Ctl} -> Ctl
                     end;
        {_P, Ctl} -> Ctl
    end.

has_perm([], _Perm)         -> false;
has_perm([Perm | _T], Perm) -> true;
has_perm([_H | T], Perm)    -> has_perm(T, Perm).

remove([], V, _Fun)     -> V;
remove([H | T], V, Fun) ->
    case lists:keyfind(H, 1, V) of
        false -> remove(T, V, Fun);
        Ctl   -> case Fun(Ctl) of
                     empty    -> lists:keydelete(H, 1, V);
                     NewTuple -> lists:keyreplace(H, 1, V, NewTuple)
                 end
    end.            

merge(List1, List2, Fun) -> merge1(List1, List2, Fun, []).

merge1([], List, _Fun, Acc)          -> lists:merge(lists:sort(List), lists:sort(Acc));
merge1([{K, V} | T], List, Fun, Acc) ->
    case lists:keyfind(K, 1, List) of
        false   -> merge1(T, List, Fun, [{K, V} | Acc]);
        {K, V2} -> List2 = lists:keydelete(K, 1, List),
                   NewCtl = Fun(V, V2),
                   merge1(T, List2, Fun, [{K, NewCtl} | Acc])
    end.
        
%%%===================================================================
%%% Debugging interface
%%%===================================================================

debug() ->
    Start = dets:lookup(?TABLE, ?KEY),
    io:format("Start is ~p~n", [Start]),
    P = ["a", "b", "c", "d"],
    auth_srv:add_perm([{user, "User"}, {group, "Group"}],
                      P, [read, write],
                      "index", ["index"]),
    io:format("got to here in debug...~n"),
    auth_srv:add_default([{user, "User"}], P, "supervisor"),
    io:format("now got to here in debug...~n"),
    Ret = dets:lookup(?TABLE, ?KEY),
    io:format("Ret is ~p~n", [Ret]),
    ok.    

%%%===================================================================
%%% EUnit Tests
%%%===================================================================
%% the root is a special case - check it carefully
testX0() ->
    P = [],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% check the empty path
testX0a() ->
    P = [],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

testX1() ->
    P = [],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

testX1a() ->
    P = [],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"User", "Fail"}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

testX1aa() ->
    P = [],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"User", "Fail"}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

testX() ->
    P1 = [],
    P2 = ["a", "b", "c"],
    P3 = ["[**]"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "Bob"}, {group, "Group"}], P2,
                     [read, write], "index", ["index"]),
    Tree3= add_perm1(Tree2, [{user, "Bobby"}, {group, "Gentry"}], P3,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree3, {"User", "Fail"}, P1),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% Pre-basic test - set no permissions but test against them
test0() ->
    P = ["a", "b", "c", "d"],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% Basic tests - set 1 permission and check against it...

test1() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test1a() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"User", "Fail"}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test1b() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"Fail", "Fail"}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

test2() ->
    P = ["a", "b", "c", "d"],
    P2 = ["fail"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test3() ->
    P = ["a", "b", "c", "d"],
    P2 = ["a", "b"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test4() ->
    P = ["a", "b", "c", "d"],
    P2 = ["a", "b", "c", "e"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test5() ->
    P = ["a", "b", "c", "d"],
    P2 = ["a", "b", "c", "d", "e"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% set the same permission twice
test5a() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% Now set 2 permissions and check against them

test6() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["1", "2", "3", "4"],
    Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                      [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                      [read, write], "index", ["index"]),
    P2 = ["a", "b", "c", "d", "e"],
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test7() ->
    P1a = ["a", "b", "c"],
    P1b = ["a"],
    Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                      [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                      [read, write], "special", ["index", "special"]),
    P2 = ["a", "b", "c", "d", "e"],
    Ret1 = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    Ret2 = check_get_page1(Tree2, {"gordon", ["Group"]}, P1a),
    Ret3 = check_get_page1(Tree2, {"User", ["Group"]}, P1b),
    io:format("Tree1 is ~p~nTree2 is ~p~n", [Tree1, Tree2]),
    io:format("Ret1 is ~p~nRet2 is ~p~nRet3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{return, '404'}, {html, "index"}, {html, "special"}}).

test8() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["a", "b", "c", "d", "x"],
    Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                      [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                      [read, write], "index", ["index"]),
    P2 = ["a", "b", "c", "d", "e"],
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test9() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["a", "b", "c", "d", "x"],
    Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                      [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                      [read, write], "index", ["index"]),
    P2 = ["a", "b", "c", "d"],
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test10() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["a", "b", "c", "d", "x"],
    Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                      [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                      [read, write], "index", ["index"]),
    P2 = ["a", "b", "c", "d", "x"],
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% set a permission then fail with a different user/group

test11() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "New"}, {group, "Group"}], P,
                      [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test12() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "New"}, {group, "Group"}], P,
                      [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test13() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "New"}, {group, "Group"}], P,
                      [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree2, {"bob", ["GroupFail"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

%% check can_read

test14() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                      [read, write], "index", ["index"]),
    Ret = can_read1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == true).

test15() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                      [read, write], "index", ["index"]),
    Ret = can_read1(Tree2, {"bob", ["GroupFail"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test16() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                      [write], "index", ["index"]),
    Ret = can_read1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

%% check can_write

test17() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                      [read, write], "index", ["index"]),
    Ret = can_write1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == true).

test18() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                      [read, write], "index", ["index"]),
    Ret = can_write1(Tree2, {"bob", ["GroupFail"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test19() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                      [read], "index", ["index"]),
    Ret = can_write1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

%% Now remove permissions
test20() ->
    P = ["a"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}], P, [read], "index", ["index"]),
    Tree3 = remove_perm1(Tree2, [{user, "gordon"}], P, [read]),
    io:format("Tree is ~p~nTree2 is ~p~nTree3 is ~p~n", [Tree, Tree2, Tree3]),
    (Tree == Tree3).

test21() ->
    P = ["a"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "User"}], P, [write], "index", ["index"]),
    Tree3 = remove_perm1(Tree2, [{user, "User"}], P, [write]),
    io:format("Tree is ~p~nTree3 is ~p~n", [Tree, Tree3]),
    (Tree == Tree3).

%% add permissions, defaults and views
test22() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read, {trans, 123}, {trans, 456}], "default", ["default"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

test23() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

test24() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
    Tree2 = add_perm1(Tree, [{group, "Group"}], P,
                      [read, {trans, xxx}, {trans, yyy}], "supervisor",
                      ["index", "default", "supervisor"]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

test25() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "Old"}], P,
                     [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
    Tree2 = add_perm1(Tree, [{group, "Group"}], P,
                      [read, {trans, xxx}, {trans, yyy}], "supervisor",
                      ["index", "default", "supervisor"]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "supervisor"}).

test26() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "Old"}], P,
                     [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
    Tree2 = add_perm1(Tree, [{group, "Group"}], P,
                      [read, {trans, xxx}, {trans, yyy}], "supervisor",
                      ["index", "default", "supervisor"]),
    Tree3 = add_perm1(Tree2, [{group, "Subordinate"}], P,
                      [read, {trans, ab12}, {trans, bc23}], "subordinate",
                      ["index", "default", "subordinate"]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "supervisor"}).

test27() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
    Tree2 = add_perm1(Tree, [{user, "User"}], P,
                      [read, {trans, xxx}, {trans, yyy}], "supervisor",
                      ["index", "default", "supervisor"]),
    Tree3 = add_perm1(Tree2, [{user, "User"}], P,
                      [read, {trans, ab12}, {trans, bc23}], "subordinate",
                      ["index", "default", "subordinate"]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "subordinate"}).

test28() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "default", ["index", "default"]),
    Tree2 = add_perm1(Tree, [{user, "User"}], P,
                      [read], "supervisor",
                      ["supervisor"]),
    Tree3 = add_perm1(Tree2, [{user, "User"}], P,
                      [read], "subordinate",
                      []),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "subordinate"}).

%% add views alone
test29() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "default", ["index", "default"]),
    Tree2 = add_views1(Tree, [{user, "User"}], P,
                      ["supervisor"]),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = get_views1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["default", "index", "supervisor"]).

%% add a default or two
test30() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "default", ["index", "default"]),
    Tree2 = add_default1(Tree, [{user, "User"}], P, "supervisor"),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "supervisor"}).

test31() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "default", ["index", "default"]),
    Tree2 = add_default1(Tree, [{group, "Group"}], P, "supervisor"),
    Tree3 = add_default1(Tree2, [{user, "User"}], P, "blah-blah"),
    io:format("Tree3 is ~p~n", [Tree3]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "blah-blah"}).

%% remove a gui
test32() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "default", ["index", "default"]),
    io:format("Tree is ~p~n", [Tree]),
    Tree2 = add_perm1(Tree, [{user, "Bob"}], P,
                     [read], "default2", ["index", "default2"]),
    io:format("Tree2 is ~p~n", [Tree2]),
    Tree3 = remove_views1(Tree2, [{user, "User"}, {group, "Group"}], P, ["default"]),
    io:format("Tree3 is ~p~n", [Tree3]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, ?INDEX}).

test33() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "default", ["index", "default", "bob"]),
    Tree2 = add_perm1(Tree, [{user, "Bob"}], P,
                     [read], "default2", ["index", "default2"]),
    Tree3 = remove_views1(Tree2, [{user, "User"}, {group, "Group"}], P,
                         ["default", "index", "bob", "jim"]),
    io:format("Tree3 is ~p~n", [Tree3]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, ?INDEX}).

%% remove a default
test34() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "default", ["index", "default"]),
    Tree2 = remove_default1(Tree,  [{user, "User"}, {group, "Group"}], P, "default"),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test35() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "index", ["index", "default"]),
    Tree2 = remove_default1(Tree, [{user, "User"}, {group, "Group"}], P, "index"),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

test36() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
                     [read], "index", ["index"]),
    Tree2 = remove_default1(Tree, [{user, "User"}, {group, "Group"}], P, "index"),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, ?INDEX}).

%% now check if a page is acceptable
test37() ->
    P1 = ["a", "b"],
    P2 = ["a"],
    P3 = ["does", "not", "exist"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
                     [read], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "User2"}], P2,
                      [read], "default", ["index", "default", "bingo"]),
    % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
    Tree3 = add_perm1(Tree2, [{user, "User"}], P1,
                      [read], "index", ["index", "default", "special"]),
    Ret1 = check_get_page1(Tree3, {"User", ["RandomGroup"]}, P1, "special"),
    % io:format("Tree is ~p~nTree2 is ~p~nTree3 is ~p~n",
    %           [Tree, Tree2, Tree3]),
    Ret2 = check_get_page1(Tree3, {"User", ["Group"]}, P1, "epic fail"),
    Ret3 = check_get_page1(Tree3, {"User", ["Group"]}, P3),
    io:format("Ret1 is ~p~nRet2 is ~p~nRet3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "special"}, {return, '401'}, {return, '404'}}).

%% now do the old wild card stuff
test38() ->
    P1 = ["a", "[*]", "c"],
    P2 = ["a", "b", "c"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
                     [read], "index", ["index"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P2),
    % io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test39() ->
    P1 = ["a", "[**]"],
    P2 = ["a", "b", "x", "y"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
                     [read], "index", ["index"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P2),
    % io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% specific (ie [*]) overrides the general (ie [**])
test40() ->
    P1 = ["a", "[**]"],
    P2 = ["a", "[*]", "x"],
    P3 = ["a", "b", "x"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
                     [read], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "User"}], P2,
                     [read], "special", ["index", "special"]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P3),
    % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "special"}).

test41() ->
    P1 = ["a", "[**]"],
    P2 = ["a", "[*]", "x"],
    P3 = ["a", "b", "x"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
                     [read], "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "User"}], P2,
                     [read], "special", ["index", "special"]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P3),
    % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "special"}).

%% test wild card user and group names
test42() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "*"}, {group, "Group"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["No Match"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test43() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "*"}], P,
                     [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["No Match"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test44() ->
    P = ["[*]"],
    Tree = add_perm1(gb_trees:empty(), [{user, "*"}, {group, "*"}], P, [read],
                     "index", ["index"]),
    Ret = check_get_page1(Tree, {"junk", ["no way", "no how", "no soon"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% Insert a wild permission twice
test45() ->
    P = ["[*]"],
    Tree = add_perm1(gb_trees:empty(), [{user, "*"}, {group, "*"}], P, [read],
                     "index", ["index"]),
    Tree2 = add_perm1(Tree, [{user, "*"}, {group, "*"}], P, [read],
                     "index", ["index"]),
    Ret = check_get_page1(Tree2, {"junk", ["no way", "no how", "no soon"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%%
%% regression tests

test1001() ->
    
    auth_srv:add_perm("http://127.0.0.1:9000",
                      [{user, "*"}, {group, "*"}], ["[**]"],[read, write],
                      "default", ["default"]),

    auth_srv:add_views("http://127.0.0.1:9000", [{user, "dale"}],
                       ["u", "dale", "[**]"], ["test"]),
    
    {html, "default"} =
        auth_srv:check_get_page("http://127.0.0.1:9000",
                                {"dale", []}, ["u", "dale", "test"]).


test1002() ->

    auth_srv:clear_all_perms_DEBUG("http://127.0.0.1:9000"),

    hn_auth:init_permissions("http://127.0.0.1:9000"),

    % Permission to view this page is added in hn_auth
    {html, "_global/spreadsheet"} =
        auth_srv:check_get_page("http://127.0.0.1:9000",
                                {"dale", ["dev"]}, [], "_global/spreadsheet").


unit_test_() -> 
    [
     % tests for the root page []
     ?_assert(testX0()),
     ?_assert(testX0a()),
     ?_assert(testX1()),
     ?_assert(testX1a()),
     ?_assert(testX1aa()),
     ?_assert(testX()),
     % tests for other pages
     ?_assert(test0()),
     ?_assert(test1()),
     ?_assert(test1a()),
     ?_assert(test1b()),
     ?_assert(test2()),
     ?_assert(test3()),
     ?_assert(test4()),
     ?_assert(test5()),
     ?_assert(test5a()),
     ?_assert(test6()),
     ?_assert(test7()),
     ?_assert(test8()),
     ?_assert(test9()),
     ?_assert(test10()),
     ?_assert(test11()),
     ?_assert(test12()),
     ?_assert(test13()),
     ?_assert(test14()),
     ?_assert(test15()),
     ?_assert(test16()),
     ?_assert(test17()),
     ?_assert(test18()),
     ?_assert(test19()),
     ?_assert(test20()),
     ?_assert(test21()),
     ?_assert(test22()),
     ?_assert(test23()),
     ?_assert(test24()),
     ?_assert(test25()),
     ?_assert(test26()),
     ?_assert(test27()),
     ?_assert(test28()),
     ?_assert(test29()),
     ?_assert(test30()),
     ?_assert(test31()),
     ?_assert(test32()),
     ?_assert(test33()),
     ?_assert(test34()),  
     ?_assert(test35()),
     ?_assert(test36()),
     ?_assert(test37()),
     ?_assert(test38()),
     ?_assert(test39()),
     ?_assert(test40()),
     ?_assert(test41()),
     ?_assert(test42()),
     ?_assert(test43()),
     ?_assert(test44()),
     ?_assert(test45())
  ].
