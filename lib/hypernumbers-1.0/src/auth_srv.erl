%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc
%%% 
%%% == Overview ==
%%% A gen server for managing how pages are displayed and to whom.   
%%% The auth_srv holds a variety of information about how URL's are
%%% handled in the hypernumbers server:
%%% <ul>
%%% <li>valid paths</li>
%%% <li>security</li>
%%% <li>default pages</li>
%%% <li>pages that can be viewed</li>
%%% <li>(generation of sequential urls <strong>not built yet</strong>)</li>
%%% </ul>
%%% Some of these controls are stored against users and groups and some are not.
%%% Each will be discussed separately here. 
%%%
%%% == Users And Groups ==
%%%
%%% Each user of the system can belong to a number of groups. There
%%% are 3 special users and groups:
%%% <ul>
%%% <li>*</li>
%%% <li>anonymous (the atom anonymous not the string anonymous)</li>
%%% <li>admin (group only)</li>
%%% </ul>
%%% The user '*' matches all users and user <code>anonymous</code> matches
%%% unlogged in users only. Ditto for groups.
%%% The group "admin" is the superuser and can see any page
%%% or view and has read and write access to all pages that exist
%%% Because permissions, pages views etc are keyed against users
%%% <em>and</em> groups there have to be rules about how the controls
%%% compose when many of them pertain. These will be discussed in the
%%% various sub-sections discussing the particular controls.
%%% == Paths ==
%%% === Overview ===
%%% Paths form the backbone of the system. A path is simply that part
%%% of the URL to the right of the port and terminating with the final
%%% slash:<br /><br />
%%% <code>http://example.com:1234/this/is/the/path/but?this=is not
%%%  </code><br /><br />
%%% has a path of <code>["this", "is", "the", "path"]</code><br />
%%% === 'Normal' Paths ===
%%% A 'normal' path can contain standard latin letters and numbers only.
%%% <strong>(Not implemented yet!)</strong>
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
%%% The multiple wild card should only be used as a terminal segment.
%%% Constructions like ["a", "b", "[**]"] are acceptable
%%% Constructions like ["a", "[**]", "b"] are not
%%% == Controls ==
%%% === Introduction ===
%%% There are 3 controls implemented:
%%% <ul>
%%% <li><code>acl</code>'s - access control lists</li>
%%% <li><code>default views</code> - specify the default view for a path</li>
%%% <li><code>views</code> - determines which views are served to whom</li>
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
%%% {group, "restauranteuses"}, [write]<br /></code>
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
%%% Default views cascade down from wild cards.
%%% Consider the following paths:<br />
%%% <code>["a", "[**]"]</code><br />
%%% <code>["a", "b", "[**]"]</code><br />
%%% <code>["a", "b", "[*]"]</code><br />
%%% <code>["a", "b", "c"]</code><br />
%%% If there was no default view at <code>["a", "b", "c"]</code> then
%%% the default at <code>["a", "b", "[*]"]</code> would be used. If none
%%% there then the default at <code>["a", "b", "[**]"]</code>, and if not,
%%% then that at <code>["a", "[**]"]</code>
%%% Default views can be overridden by the override view specified in the
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
%%% <li>only if no override is bound to a username do the
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
%%% If there is no default view for a path and no override views, but there
%%% are some normal views then then one of them will be set. Which one is
%%% not specified - but if there are user views it will be one of them
%%% before group views come into play.
%%% @todo extend paths from latin to unicode paths
%%% @todo implement path restrictions - and make sure they occur
%%% @end
%%% Created :  7 Oct 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------

-module(auth_srv).

-behaviour(gen_server).

-include("auth.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).

%% API 
-export([start_link/0]).

-export([
         check_get_page/3,
         check_get_page/4,
         get_views/3,
         can_read/3,
         can_write/3,
         can_execute/3,
         add_controls/6,
         add_perm/4,
         add_views/5,
         add_default/3,
         remove_perm/4,
         remove_views/4,
         remove_default/3,
         get_as_json/2,
         pretty_print/3,
         get_groups/1
        ]).

-export([
         clear_all_perms_DEBUG/1,
         permissions_DEBUG/3
        ]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TABLE, "auth_srv").
-define(KEY, "auth_tree").
-define(INDEX, "hypernumbers/index").
-define(SPREADSHEET, "_global/spreadsheet").

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
    {ok, Dir} = application:get_env(hypernumbers, dets_dir),
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
            {add_controls, Host, AL, Pg, Pm, Or, Vs} ->
                {Host, add_controls1(get(Host, Tr), AL, Pg, Pm, Or, Vs), true};
            {add_perm, Host, AL, Pg, Pm} ->
                {Host, add_perm1(get(Host, Tr), AL, Pg, Pm), true};
            {add_views, Host, AL, Pg, Or, Vs} ->
                {Host, add_views1(get(Host, Tr), AL, Pg, Or, Vs), true};
            {add_default, Host, Pg, Df} ->
                {Host, add_default1(get(Host, Tr), Pg, Df), true};
            {rem_perm, Host, AL, Pg, Pm} ->
                {Host, remove_perm1(get(Host, Tr), AL, Pg, Pm), true};
            {rem_views, Host, AL, Pg, Vs} ->
                {Host, remove_views1(get(Host, Tr), AL, Pg, Vs), true};
            {rem_def, Host, Pg, Df} ->
                {Host, remove_default1(get(Host, Tr), Pg, Df), true};
            {get_as_json, Host, Pg} ->
                {Host, get_as_json1(get(Host, Tr), Pg), false};
            {pretty_print, Host, Pg, Type} ->
                {Host, pretty_print1(get(Host, Tr), Host, Pg, Type), false};
            {get_groups, Host} ->
                {Host, get_groups1(get(Host, Tr)), false};
            {clear_all_perms, Host} ->
                {Host, gb_trees:empty(), true};
            {permissions_debug, Host, AS, P} ->
                {Host, permissions_debug1(get(Host, Tr), AS, P), false}
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

add_controls(Host, AuthList, Page, Perm, Override, Views) ->
    gen_server:call(auth_srv, {add_controls, Host, AuthList, Page, Perm,
                               Override, Views}).

add_perm(Host, AuthList, Page, Perm) ->
    gen_server:call(auth_srv, {add_perm, Host, AuthList, Page, Perm}).

add_views(Host, AuthList, Page, Override, Views) ->
    gen_server:call(auth_srv, {add_views, Host, AuthList, Page, Override, Views}).

add_default(Host, Page, Gui) ->
    gen_server:call(auth_srv, {add_default, Host, Page, Gui}).

remove_perm(Host, AuthList, Page, Perm) ->
    gen_server:call(auth_srv, {rem_perm, Host, AuthList, Page, Perm}).

remove_views(Host, AuthList, Page, Views) ->
    gen_server:call(auth_srv, {rem_views, Host, AuthList, Page, Views}).

remove_default(Host, Page, Gui) ->
    gen_server:call(auth_srv, {rem_default, Host, Page, Gui}).

get_as_json(Host, Page) ->
    gen_server:call(auth_srv, {get_as_json, Host, Page}).

pretty_print(Host, Page, Type) ->
    gen_server:call(auth_srv, {pretty_print, Host, Page, Type}).

get_groups(Host) ->
    gen_server:call(auth_srv, {get_groups, Host}).

clear_all_perms_DEBUG(Host) ->
    gen_server:call(auth_srv, {clear_all_perms, Host}).

permissions_DEBUG(Host, AuthSpec, Page) -> 
    gen_server:call(auth_srv, {permissions_debug, Host, AuthSpec, Page}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
permissions_debug1(Tree, {U, Gs}, Page) ->
    Fun =
        fun(X) ->
                Ret1 = io_lib:fwrite("User is ~p~n and Groups are ~p~n",
                                     [U, Gs]),
                Ret2 = io_lib:fwrite("ACL's are ~p~n", [X#controls.acl]),
                Ret3 = io_lib:fwrite("Default is ~p~n", [X#controls.default]),
                Ret4 = io_lib:fwrite("Views are ~p~n", [X#controls.views]),
                "<html><head></head><body><br />" ++ Ret1
                    ++ "<br /><br />" ++ Ret2
                    ++ "<br /><br />" ++ Ret3
                    ++ "<br /><br />" ++ Ret4
                    ++ "</body></html>"
        end,
    check_get(Tree, Page, Fun).    

check_get_page1(Tree, {U, Gs}, Page) ->
    % first see if the user has permission to see the page
    % then see what page they should be getting
     Fun =
         fun(X) ->
                 io:format("X is ~p~nU is ~p~nGs is ~p~n", [X, U, Gs]),
                 case get_ret_code(X#controls.acl, U, Gs, read) of
                     {return, '404'} -> io:format("exiting here...~n"),
                                        {return, '404'};
                     {return, '401'} -> {return, '401'};
                     _Other          ->
                         case get_override(X#controls.views, U, Gs) of
                             []     -> case X#controls.default of
                                           []  -> get_random_view(X#controls.views, U, Gs);
                                           Def -> {html, Def}
                                       end;
                            Other2 -> {html, Other2}
                         end
                 end
         end,
    check_get(Tree, Page, Fun).

check_get_page1(Tree, {User, Groups}, Page, View) -> 
    % first see if the user has permission to see the page
    % then see what view they should be getting
     Fun =
         fun(X) ->
                 case get_ret_code(X#controls.acl, User, Groups, read) of
                     {return, '404'} -> {return, '404'}; 
                     {return, '401'} -> {return, '401'};
                     _Other          ->
                         case is_admin(Groups) of
                             true  -> {html, View};
                             false ->
                                 Default = X#controls.default,
                                 Views   = X#controls.views,
                                 AllViews = get_all_views(Default, Views, User, Groups),
                                 case contains(View, AllViews) of
                                     true  -> {html, View};
                                     false -> case has_wild(AllViews) of
                                                  global -> {html, View};
                                                  local  -> case is_local(User, View) of
                                                                true  -> {html, View};
                                                                false -> {return, '401'}
                                                            end;
                                                  none   -> {return, '404'}
                                              end
                                 end
                         end
                 end
         end,
    check_get(Tree, Page, Fun).

get_views1(Tree, {User, Groups}, Page) ->
    Fun = fun(X) ->
                  C = #controls{},
                  case X of
                      C -> [];
                      _ -> case is_admin(Groups) of
                               true  -> ["**"];
                               false -> Default = X#controls.default,
                                        Views   = X#controls.views,
                                        get_all_views(Default, Views, User, Groups)
                           end
                  end
          end,
    check_get(Tree, Page, Fun).

can_read1(Tree, {User, Groups}, Page) ->
     Fun = fun(X) ->
                   case get_ret_code(X#controls.acl, User, Groups, read) of
                       {return, '200'} -> true;
                       {return, '404'} -> false;
                       {return, '401'} -> false
                   end
           end,
    check_get(Tree, Page, Fun).

can_write1(Tree, {User, Groups}, Page) -> 
    Fun = fun(X) ->
                  get_ret_code(X#controls.acl, User, Groups, write)
          end,
    check_get(Tree, Page, Fun).

can_execute1(_Tree, _AuthSpec, _Trans_signature) -> {erk, not_written}.

add_controls1(Tree, AuthList, Page, Perms, Override, Views) ->
    Tree2 = add_perm1(Tree, AuthList, Page, Perms),
    add_views1(Tree2, AuthList, Page, Override, Views).

add_perm1(Tree, AuthList, Page, Perms) ->
    Fun = fun(Old, New) ->
                  hslists:dedup([Old, New])
          end,
    Controls = make_controls(AuthList, Perms),
    add_to_control(Tree, {acl, Controls}, Page, Fun).

add_views1(Tree, AuthList, Page, Override, Views) ->
    Fun = fun(#views{override = O1, views = V1} = _Old,
              #views{override = O2, views = V2} = _New) ->
                  {O3, V3} = case {O1, O2} of
                                 {[], []} -> {[], hslists:dedup([V1, V2])};
                                 {[], _}  -> {O2, hslists:dedup([[O2], V1, V2])};
                                 {_, []}  -> {O1, hslists:dedup([[O1], V1, V2])};
                                 {_, _}   -> {O2, hslists:dedup([[O1], [O2], V1, V2])}
                             end,
                  #views{override = O3, views = V3}
          end,
    Controls = make_controls(AuthList, #views{override = Override, views = Views}),
    add_to_control(Tree, {views, Controls}, Page, Fun).

add_default1(Tree, Page, Default) ->
    Fun = fun(_Old, _New) ->
                  io:format("The function to merge defaults never runs :( " ++
                            "shonky design, monkey boy!~n"),
                  exit('erk!')
          end,
    add_to_control(Tree, {default, Default}, Page, Fun).

remove_perm1(Tree, AuthList, Page, Perms) ->
    Fun = fun({K, V}) ->
                  case lists:subtract(V, Perms) of
                      [] -> empty;
                      V2 -> {K, V2}
                  end
          end,
    remove_from_control(Tree, {acl, AuthList}, Page, Fun).    

remove_views1(Tree, AuthList, Page, {Override, Views}) -> 
    Fun = fun({K, OldView}) ->
                  NewViews= lists:subtract(OldView#views.views, Views),
                  Ret = case OldView#views.override of
                            []       -> {K, OldView#views{views = NewViews}};
                            Override -> {K, OldView#views{override = [],
                                                          views = NewViews}};
                            _        -> {K, OldView#views{views = NewViews}}
                        end,
                  % if the whole views thing is now blank, return empty...
                  case Ret of
                      {K, #views{override = [], views = []}} -> empty;
                      Other                                  -> Other
                  end
          end,
    remove_from_control(Tree, {views, AuthList}, Page, Fun).

remove_default1(Tree, Page, Default) ->
    Fun = fun(_X) ->
                  io:format("The function to merge defaults never runs :( " ++
                            "shonky design, monkey boy!~n"),
                  exit('erk!')
          end,
    remove_from_control(Tree, {default, Default}, Page, Fun).

get_as_json1(Tree, Page) ->
    Fun = fun(X) ->
                  make_json(X, [])
          end,
    get_for_pp(Tree, Page, Fun).

pretty_print1(Tree, Host, Page, Type) ->
    Fun = fun(X) ->
                  make_prettyprint(X, Host, Type)
          end,
    get_for_pp(Tree, Page, Fun).

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
    case keyfind(Host, Trees) of
        [] -> gb_trees:empty();
        T  -> T
    end.
            
%%
%% Tree Helper functions
%%
remove_from_control(Tree, {Type, Control}, [], Fun) ->
    case gb_trees:lookup(Type, Tree) of
        none       -> Tree;
        {value, V} -> NewCtls = remove(Type, Control, V, Fun), 
                      gb_trees:enter(Type, NewCtls, Tree)
    end;
remove_from_control(Tree, {Type, Control}, [H | T], Fun) ->
    case gb_trees:lookup(H, Tree) of
        none       -> Tree;
        {value, V} -> NewVal = remove_from_control(V, {Type, Control}, T, Fun),
                      gb_trees:enter(H, NewVal, Tree)
    end.

add_to_control(Tree, {Type, Controls}, [], Fun) ->
    case gb_trees:lookup(Type, Tree) of
        none       -> gb_trees:insert(Type, Controls, Tree);
        {value, V} -> NewControls = case Type of
                                        default -> Controls;
                                        acl     -> merge(V, Controls, Fun);
                                        views   -> merge(V, Controls, Fun)
                                    end,
                      gb_trees:enter(Type, NewControls, Tree)
    end;
add_to_control(Tree, {Type, Controls}, [H | T], Fun) ->
%% force [**] to be a terminal segment only
    case {H, T} of
        {"[**]", []} -> ok;
        {"[**]", _X} -> exit("non-terminal [**] segment in add_to_congtrol");
        _            -> ok
    end,
    case gb_trees:lookup(H, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = add_to_control(Empty, {Type, Controls}, T, Fun),
                      gb_trees:insert(H, NewVal, Tree);
        {value, V} -> NewVal = add_to_control(V, {Type, Controls}, T, Fun),
                      gb_trees:enter(H, NewVal, Tree)
    end.

%%
%% General Helper functions
%%
has_wild(Views) ->
    case contains("**", Views) of
        true  -> global;
        false -> case contains("*", Views) of
                     true  -> local;
                     false -> none
                 end
    end.

is_local(User, View) ->
    case re:run(View, "^" ++ User ++ "/") of
        {match, _} -> true;                       
        nomatch    -> false
    end.

remove_empty(List) -> remove_e(List, []).

remove_e([], Acc)       -> Acc;
remove_e([[] | T], Acc) -> remove_e(T, Acc);
remove_e([H | T], Acc)  -> remove_e(T, [H | Acc]).                     

keyfind(K, L) ->
    case lists:keyfind(K, 1, L) of
        false  -> [];
        {K, V} -> V
    end.

get_random_view(Views, User, Groups) ->
    case lists:keyfind({user, User}, 1, Views) of
        false   -> get_random_v2(Views, Groups, is_admin(Groups));
        {_K, V} -> case V#views.views of
                       [] -> get_random_v2(Views, Groups, is_admin(Groups));
                       _  -> {html, hd(V#views.views)}
                   end
    end.

%% the 3rd parameter is whether or not the group is "admin"
get_random_v2(Views, [], false)                -> get_random_v3(Views);
get_random_v2([], [], true)                    -> {html, ?SPREADSHEET};
get_random_v2([{_, View} | _T], [], true) ->
    case View#views.override of
        []       -> case View#views.views of
                        []        -> {html, ?SPREADSHEET};
                        [V | _T1] -> {html, V}
                    end;
        Override -> {html, Override}
    end;
get_random_v2(Views, [H | T], IsAdmin)         ->
    case lists:keyfind({group, H}, 1, Views) of
        false   -> get_random_v2(Views, T, IsAdmin);
        {_K, V} -> case V#views.views of
                       [] -> case IsAdmin of
                                 true  -> {html, ?SPREADSHEET};
                                 false -> {return, '404'}
                             end;
                       _  -> get_first(V#views.views)
                   end
    end.            

get_random_v3([])                   -> {return, '404'};
get_random_v3([{{_, "*"}, V} | _T]) -> get_first(V#views.views);
get_random_v3([_H | T])             -> get_random_v3(T).

%% get first is only called if there are *some* views, so
%% if the only views are '**' or '*' you get a spreadsheet
get_first([])         -> {html, ?SPREADSHEET};
get_first(["**" | T]) -> get_first(T);
get_first(["*" | T])  -> get_first(T);
get_first([H | _T])   -> {html, H}.

get_ret_code([], _User, _Groups, read)  -> {return, '404'};
get_ret_code([], _User, _Groups, write) -> false;
get_ret_code(Acl, User, Groups, Type)   -> get_ret_2(Acl, User, Groups, Type).

get_ret_2([], _User, Groups, read)     -> case is_admin(Groups) of
                                              false -> {return, '401'};
                                              true  -> {return, '200'}
                                          end;
get_ret_2([], _User, Groups, write)    -> case is_admin(Groups) of
                                              false -> false;
                                              true  -> true
                                          end;
get_ret_2(Acls, User, Groups, Type)    ->
    Acl1 = lists:flatten([keyfind({user, X}, Acls) || X <- [User, "*"]]),
    Acl2 = get_ret_3(Acls, Groups, []),
    case contains(Type, hslists:dedup([Acl1, Acl2])) of
        true  -> case Type of
                     read  -> {return, '200'};
                     write -> true
                 end;
        false -> case Type of
                     read  -> case is_admin(Groups) of
                                  false -> {return, '401'};
                                  true  -> {return, '200'}
                              end;
                     write -> case is_admin(Groups) of
                                  false -> false;
                                  true  -> true
                              end
                 end
    end.

get_ret_3(Acls, [], Acc)      -> NewAcc = keyfind({group, "*"}, Acls),
                                 lists:flatten(hslists:dedup([NewAcc, Acc]));
get_ret_3(Acls, [H | T], Acc) ->
    NewAcc = case lists:keyfind({group, H}, 1, Acls) of
                 false   -> Acc;
                 {_K, V} -> [V | Acc]
             end,
    get_ret_3(Acls, T, NewAcc).

is_admin([])             -> false;
is_admin(["admin" | _T]) -> true;
is_admin([_H | T])       -> is_admin(T).

%% we will first check the users, then if there are no matches
%% we will check the groups
%% if there are still no matches we will check for the user "*"
%% and if there are still no matches we will check for the group "*",
%% than, and only then, do we give up :)
get_override(Views, User, Gs) ->
    case get_o1(Views, User) of
        [] -> case get_o2(Views, Gs) of
                  [] -> case get_o1(Views, "*") of
                            [] -> case get_o2(Views, ["*"]) of
                                      [] -> [];
                                      O1 -> O1
                                  end;
                            O2 -> O2
                        end;
                  O3 -> O3
              end;
        O4 -> O4
    end.

get_o1(Views, User) ->
    case keyfind({user, User}, Views) of
        [] -> [];
        V  -> V#views.override
    end.            

get_o2(_Views, [])     -> [];
get_o2(Views, [H | T]) -> case lists:keyfind({group, H}, 1, Views) of
                              false  -> get_o2(Views, T);
                              {_, V} -> V#views.override
                          end.

make_prettyprint(Tree, Host, html) -> Body = make_pp(Tree, html, [], "", []),
                                "<html><head><title>Permissions Tree</title></head><body><code>"
                                          "<bold>Permissions for " ++ Host ++
                                          "</bold><br />" ++
                                          Body ++ "</code></body></html>";
make_prettyprint(Tree, Host, text) -> "Permissions for " ++ Host ++ "~n" ++
                                          make_pp(Tree, text, [], "", []).

make_pp(Tree, Type, Seg, Prefix, Acc) ->
    LineEnd = case Type of
                  html -> "<br />";
                  text -> "~n"
              end,
    List = gb_trees:to_list(Tree),
    {Controls, Paths} = split(List),
    Seg2 = case Seg of
               [] -> "/";
               _  -> "/" ++ Seg ++ "/"
           end,
    NewPrefix = case {length(Paths), Type} of
                    {0, text} -> lists:append(Prefix, "  ");
                    {1, text} -> lists:append(Prefix, "  ");
                    {_, text} -> lists:append(Prefix, " |");
                    {0, html} -> lists:append(Prefix, "&nbsp;&nbsp;");
                    {1, html} -> lists:append(Prefix, "&nbsp;&nbsp;");
                    {_, html} -> lists:append(Prefix, "&nbsp;|")
                end,
    C = pp(Controls, Prefix, Type, []),
    make_pp2(Paths, Type, NewPrefix,
             [C, LineEnd, Seg2, "-> ", Prefix, LineEnd, Prefix | Acc]).

make_pp2([], text, _Prefix, Acc) -> lists:flatten(lists:reverse(Acc));
make_pp2([], html, _Prefix, Acc) -> lists:flatten(lists:reverse(Acc));
make_pp2([{K, V} | T], Type, Prefix, Acc) ->
    NewAcc = make_pp(V, Type, K, Prefix, []),
    make_pp2(T, Type, Prefix, [NewAcc | Acc]).

pp([], Prefix, html, [])       -> Prefix ++ "&nbsp;&nbsp;&nbsp;(no controls)" ++ "<br />";
pp([], Prefix, text, [])       -> Prefix ++ "   (no controls)" ++ "~n";
pp([], _Prefix, _Type, Acc)    -> lists:reverse(Acc);
pp([H | T], Prefix, Type, Acc) -> pp(T, Prefix, Type, [pp_c(H, Prefix, Type, [])| Acc]).


pp_c({acl, []}, Pf, html, A) -> Pf ++ "&nbsp;&nbsp;&nbsp;<b>ACL is:</b> " ++ " <br />" ++ A;
pp_c({acl, []}, Pf, text, A) -> Pf ++ "   ACL is: " ++ "~n" ++ A;
pp_c({views, []}, Pf, html, A) -> Pf ++ "&nbsp;&nbsp;&nbsp;<b>Views</b> " ++ " <br />" ++ A;
pp_c({views, []}, Pf, text, A) -> Pf ++ "   Views " ++ "~n" ++ A;
pp_c({default, D}, Pf, html, []) -> Pf ++ "&nbsp;&nbsp;&nbsp;<b>Default View is:</b> "
                                        ++ D ++" <br />";
pp_c({default, D}, Pf, text, []) -> Pf ++ "   Default View is: " ++ D ++ "~n";
pp_c({acl, [{{Ty, N}, L} | T]}, Pf, html, A) ->
    NewA = Pf ++ "&nbsp;&nbsp;&nbsp;- " ++ atom_to_list(Ty) ++ ": " ++ N
        ++ "&nbsp;&nbsp;-> " ++ pp_l(L, []) ++ "<br />",
    pp_c({acl, T}, Pf, html, [NewA | A]);
pp_c({acl, [{{Ty, N}, L} | T]}, Pf, text, A) ->
    NewA = Pf ++ "   - " ++ atom_to_list(Ty) ++ ": " ++ N
        ++ "  -> " ++ pp_l(L, []) ++ "~n",
    pp_c({acl, T}, Pf, text, [NewA | A]);
pp_c({views, [{{Ty, N}, #views{override = O, views = V}} | T]}, Pf, html, A) ->
    NewO = case O of
               [] -> "(none)";
               _  -> O
           end,
    NewA = Pf ++ "&nbsp;&nbsp;&nbsp;- " ++ atom_to_list(Ty) ++ ": " ++ N ++ "<br />" ++
        Pf ++ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-> override default is: " ++ NewO ++ "<br />" ++
        Pf ++ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-> views are:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ pp_l(V, []) ++ "<br />",
    pp_c({views, T}, Pf, html, [NewA | A]);
pp_c({views, [{{Ty, N}, #views{override = O, views = V}} | T]}, Pf, text, A) ->
    NewO = case O of
               [] -> "(none)";
               _  -> O
           end,
    NewA = Pf ++ "   - " ++ atom_to_list(Ty) ++ ": " ++ N ++ "~n" ++
        Pf ++ "     -> override default is: " ++ NewO ++ "~n" ++
        Pf ++ "     -> views are:           " ++ pp_l(V, []) ++ "~n",
    pp_c({views, T}, Pf, text, [NewA | A]).

pp_l([], [])                       -> ""; % blank list is just blank!
pp_l([], [_H | Acc])               -> lists:flatten(lists:reverse(Acc));
pp_l([H | T], Acc) when is_atom(H) -> pp_l(T, [", ", atom_to_list(H) | Acc]);
pp_l([H | T], Acc) when is_list(H) -> pp_l(T, [", ", H | Acc]).

split(L) -> sp1(L, [], []).

sp1([], Controls, Paths)      -> {lists:sort(Controls), lists:sort(Paths)};
sp1([H | T], Controls, Paths) ->
    case element(1, H) of
        X when is_atom(X) -> sp1(T, [H | Controls], Paths);
        X when is_list(X) -> sp1(T, Controls, [H | Paths])
    end.

make_json(Tree, Seg) ->
    List = gb_trees:to_list(Tree),
    {array, [make_json1(K, V, Seg) || {K, V}  <- List]}.

make_json1(default, V, _Seg)           -> {struct, [{"default", V}]};
make_json1(acl, V, _Seg)               -> {struct, [{"acl", json_control(V)}]};
make_json1(views, V, _Seg)             -> {struct, [{"views", json_control(V)}]};
make_json1(K, V, _Seg) when is_list(K) -> {K, make_json(V, K)}.

json_control(List) -> json_control1(List, []).

json_control1([], Acc) -> {array, lists:reverse(Acc)};
json_control1([{{Type, Name}, C} | T], Acc) when is_list(C)  ->
    NewAcc = {struct, [{Type, Name}, {array, C}]},
    json_control1(T, [NewAcc | Acc]);
json_control1([{{Type, Name}, C} | T], Acc) when is_tuple(C)  ->
    NewAcc = {struct, [{Type, Name}, {struct, [{"override", C#views.override},
                                               {array, C#views.views}]}]},
    json_control1(T, [NewAcc | Acc]).

contains(_Element, [])            -> false;
contains(Element, [Element | _T]) -> true;
contains(Element, [_H | T])       -> contains(Element, T).

load_trees(Dir, Table) ->
    {ok, _} = dets:open_file(Table, [{file, filename:join(Dir,Table)}]),
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
    Fun(make_controls(Tree));
check_get(Tree, [H | T], Fun) ->
    case gb_trees:lookup(H, Tree) of
        none        -> check_programmatic(Tree, T, Fun);
        {value , V} -> check_get(V, T, Fun)
    end.         

check_programmatic(Tree, List, Fun) ->
    case gb_trees:lookup("[*]", Tree) of
        none        -> case gb_trees:lookup("[**]", Tree) of
                           none        -> Fun(#controls{});
                           {value, V1} -> Fun(make_controls(V1))
                       end;
	        {value, V2} -> check_get(V2, List, Fun)
    end.

make_controls(Tree) ->
    A1 = case gb_trees:lookup(acl, Tree) of
             none       -> [];
             {value, A} -> A
         end,
    D1 = case gb_trees:lookup(default, Tree) of
             none       -> [];
             {value, D} -> D
         end,
    V1 = case gb_trees:lookup(views, Tree) of
             none       -> [];
             {value, V} -> V
         end,
    #controls{acl = A1, default = D1, views = V1}.

get_for_pp(Tree, [], Fun) ->
    Fun(Tree);
get_for_pp(Tree, [H | T], Fun) ->
    case gb_trees:lookup(H, Tree) of
        none        -> check_programmatic(Tree, T, Fun);
        {value , V} -> get_for_pp(V, T, Fun)
    end.          

make_controls(AuthList, Control) -> make_c1(AuthList, Control, []).

make_c1([], _Control, Acc)     -> Acc;
make_c1([H | T], Control, Acc) -> make_c1(T, Control, [{H, Control} | Acc]).

get_all_views(Default, Views, User, Groups) ->
    User2 = ["*" | [User]],
    Groups2 = ["*" | Groups],
    NewViews1 = get_all_v2(Views, user, User2, []),
    NewViews2 = get_all_v2(Views, group, Groups2, []),
    remove_empty(hslists:dedup([[Default], NewViews1, NewViews2])).

get_all_v2(_Control, _Type, [], Acc)      -> Acc;
get_all_v2(Control, Type, [H | T], Acc)  ->
    NewAcc = case keyfind({Type, H}, Control) of
                 [] -> [];
                 V  -> case V#views.override of
                           [] -> V#views.views;
                           _  -> hslists:dedup([[V#views.override], V#views.views])
                       end
             end,
    case NewAcc of
        [] -> get_all_v2(Control, Type, T, Acc);
        _  -> get_all_v2(Control, Type, T, hslists:dedup([NewAcc, Acc]))
    end.

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

remove(_Type, [], V, _Fun)     -> V;
remove(Type, [H | T], V, Fun)  ->
    NewV = case lists:keyfind(H, 1, V) of
               false -> V;
               Ctl   -> case Fun(Ctl) of
                            empty    -> lists:keydelete(H, 1, V);
                            NewTuple -> lists:keyreplace(H, 1, V, NewTuple)
                        end
           end,
    remove(Type, T, NewV, Fun).

merge(List1, List2, Fun) -> merge1(List1, List2, Fun, []).

merge1([], List, _Fun, Acc)          -> lists:merge(lists:sort(List),
                                                    lists:sort(Acc));
merge1([{K, V} | T], List, Fun, Acc) ->
    case lists:keyfind(K, 1, List) of
        false   -> merge1(T, List, Fun, [{K, V} | Acc]);
        {K, V2} -> List2 = lists:keydelete(K, 1, List),
                   NewCtl = Fun(V, V2),
                   merge1(T, List2, Fun, [{K, NewCtl} | Acc])
    end.

%%%===================================================================
%%% EUnit Tests
%%%===================================================================
% the root is a special case - check it carefully

%% check the empty path
test01() ->
    P = [],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% add a permission but no views
test02() ->
    P = [],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                     P, [read, write]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% add a control
test03() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"User", ["Fail"]}, P),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override"}).

test04() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"Fail", ["FailHarder"]}, P),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

%% add a control with a view but no default view
test05() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"User", ["Fail"]}, P),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

%% add a control with a view and an override view but no default view
test06() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"User", ["Fail"]}, P),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override"}).

%% add a control with a view, an override view and a default view
test07() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"User", ["Fail"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override"}).

%% add a control with a view, no override view and a default view
test08() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"User", ["Fail"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

%% a user with no groups
test09() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"User", []}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

%% test multiple users and groups
test010() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(),
                         [{user, "User"}, {user, "User2"},
                                            {group, "Group"}, {group, "Group2"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"Fail", ["Fail", "Group2"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

%% test a single wild card user
test011() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "*"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"User", ["Fail"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override"}).

%% test a single wild card group
test012() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "King"}, {group, "*"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"User", ["Fail"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override"}).

%% test that a user wild overcomes group wild
test013() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "*"}, {group, "Group"}],
                         P, [read, write],
                         "override1", ["a view", "another view"]),
    Tree2 = add_controls1(Tree, [{user, "King"}, {group, "*"}], P, [read, write],
                          "override2", ["a view", "another view"]),
    Tree3 = add_default1(Tree2, P, "default"),
    get_as_json1(Tree3, []),
    Ret = check_get_page1(Tree3, {"Fail", ["FailAgain"]}, P),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override1"}).

%% test that a user match comes in before a pair of wilds
test014() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "hey!", ["a view", "another view"]),
    Tree2 = add_controls1(Tree, [{user, "*"}, {group, "Group"}],
                          P, [read, write],
                          "override1", ["a view", "another view"]),
    Tree3 = add_controls1(Tree2, [{user, "King"}, {group, "*"}],
                          P, [read, write],
                          "override2", ["a view", "another view"]),
    Tree4 = add_default1(Tree3, P, "default"),
    Ret = check_get_page1(Tree4, {"User", ["FailAgain"]}, P),
    PP = pretty_print1(Tree4, "test", [], text),
    get_as_json1(Tree4, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "hey!"}).

%% test that a group match comes in before a pair of wilds
test015() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "hey!", ["a view", "another view"]),
    Tree2 = add_controls1(Tree, [{user, "*"}], P, [read, write],
                          "override1", ["a view", "another view"]),
    Tree3 = add_controls1(Tree2, [{group, "*"}], P, [read, write],
                          "override2", ["a view", "another view"]),
    Tree4 = add_default1(Tree3, P, "default"),
    get_as_json1(Tree4, []),
    Ret = check_get_page1(Tree4, {"Fail", ["Group"]}, P),
    PP = pretty_print1(Tree4, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "hey!"}).

%% the user path matchs comes in before a pair of wilds, but there is
%% no default set
test016() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "", ["a view", "another view"]),
    Tree2 = add_controls1(Tree, [{user, "*"}, {group, "Group"}],
                          P, [read, write],
                          "override1", ["a view", "another view"]),
    Tree3 = add_controls1(Tree2, [{user, "King"}, {group, "*"}],
                          P, [read, write],
                          "override2", ["a view", "another view"]),
    Tree4 = add_default1(Tree3, P, "default"),
    Ret = check_get_page1(Tree4, {"User", ["FailAgain"]}, P),
    PP = pretty_print1(Tree4, "test", [], text),
    get_as_json1(Tree4, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override1"}).

%% the group path matchs comes in before a pair of wilds, but there is
%% no default set
test017() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "", ["a view", "another view"]),
    Tree2 = add_controls1(Tree, [{user, "*"}], P, [read, write],
                          "override1", ["a view", "another view"]),
    Tree3 = add_controls1(Tree2, [{group, "Group"}], P, [read, write],
                          "override2", ["a view", "another view"]),
    Tree4 = add_default1(Tree3, P, "default"),
    Ret = check_get_page1(Tree4, {"Fail", ["Group"]}, P),
    PP = pretty_print1(Tree4, "test", [], text),
    get_as_json1(Tree4, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override2"}).

test018() ->
    P = [],
    Tree = remove_perm1(gb_trees:empty(), [{user, "Me"}, {groups, "Us"}],
                        P, [read]),
    get_as_json1(Tree, []),
    (Tree == gb_trees:empty()).

test019() ->
    P = [],
    Tree = remove_views1(gb_trees:empty(), [{user, "Me"}, {groups, "Us"}], P,
                         {"big", ["rock", "candy", "mountain"]}),
    get_as_json1(Tree, []),
    (Tree == gb_trees:empty()).

test020() ->
    P = [],
    Tree = remove_default1(gb_trees:empty(), P, "banjo"),
    get_as_json1(Tree, []),
    (Tree == gb_trees:empty()).

%% test the admin access
testA() ->
        P = [],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["admin"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

testB() ->
    P = [],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                     P, [read, write]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["admin"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "_global/spreadsheet"}).

testC() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"benjamin", ["admin"]}, P),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override"}).

testD() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"Benjamin", ["admin"]}, P),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).
    
testE() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"Benjamin", ["admin"]}, P),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override"}).

testF() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         "override", ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"Benjamin", ["admin"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).    

testG() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"Benjamin", ["admin"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

testH() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"Benjaimin", ["admin"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

testI() ->
    P = [],
    Tree = add_controls1(gb_trees:empty(), [{group, "Group"}],
                         P, [read, write],
                         [], ["a view", "another view"]),
    Tree2 = add_default1(Tree, P, "default"),
    Ret = check_get_page1(Tree2, {"Benjaimin", ["admin"]}, P),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

    
%% add some extra controls
test10() ->
    P1 = [],
    P2 = ["a", "b", "c"],
    P3 = ["[**]"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1,[read, write],
                         "index", ["first", "second"]),
    Tree2 = add_controls1(Tree, [{user, "Bob"}, {group, "Group"}],
                          P2,[read, write],
                          "override", ["third", "fourth"]),
    Tree3= add_controls1(Tree2, [{user, "Bobby"}, {group, "Gentry"}],
                         P3,[read, write],
                         [], ["fifth"]),
    get_as_json1(Tree3, []),
    Ret = check_get_page1(Tree3, {"User", "Fail"}, P1),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% Pre-basic test - set no permissions but test against them
test11() ->
    P = ["a", "b", "c", "d"],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% Basic tests - set 1 permission and check against it...

test12() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write],
                         "index", ["other", "one"]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test13() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write],
                         "index", ["hey!"]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"User", "Fail"}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test14() ->
    P = ["a", "b", "c", "d"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"Fail", "Fail"}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

test15() ->
    P = ["a", "b", "c", "d"],
    P2 = ["fail"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test16() ->
    P = ["a", "b", "c", "d"],
    P2 = ["a", "b"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test17() ->
    P = ["a", "b", "c", "d"],
    P2 = ["a", "b", "c", "e"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write]),
    io:format(pretty_print1(Tree, "test", [], text), []),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test18() ->
    P = ["a", "b", "c", "d"],
    P2 = ["a", "b", "c", "d", "e"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test18a() ->
    P = [],
    P2 = ["a", "b", "c", "d", "e"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% set the same permission twice
test19() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P, [read, write],
                         "index", ["hey!"]),
    Tree2 = add_controls1(Tree, [{user, "User"}, {group, "Group"}], P, [read, write],
                          "index", ["hey!"]),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% Now set 2 permissions and check against them

test19a() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["1", "2", "3", "4"],
    Tree1 = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                          [read, write], "banana", ["index"]),
    Tree2 = add_controls1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    P2 = ["a", "b", "c", "d", "e"],
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test19b() ->
    P1a = ["a", "b", "c"],
    P1b = ["a"],
    P2 = ["a", "b", "c", "d", "e"],
    Tree1 = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                          [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                          [read, write], "special", ["index", "special"]),
    get_as_json1(Tree2, []),
    Ret1 = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    Ret2 = check_get_page1(Tree2, {"gordon", ["Group"]}, P1a),
    Ret3 = check_get_page1(Tree2, {"User", ["Group"]}, P1b),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Ret1 is ~p~nRet2 is ~p~nRet3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{return, '404'}, {html, "index"}, {html, "special"}}).

test19c() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["a", "b", "c", "d", "x"],
    Tree1 = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                          [read, write], "lychee", ["index"]),
    Tree2 = add_controls1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                          [read, write], "apricot", ["index"]),
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P1b),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "apricot"}).

test19d() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["a", "b", "c", "d", "x"],
    Tree1 = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                          [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                          [read, write], "index", ["index"]),
    P2 = ["a", "b", "c", "d"],
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test19e() ->
    P1a = ["a", "b", "c", "d"],
    P1b = ["a", "b", "c", "d", "x"],
    Tree1 = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
                          [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    P2 = ["a", "b", "c", "d", "x"],
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

%% set a permission then fail with a different user/group

test19f() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "New"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test19g() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "New"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "index"}).

test19h() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "New"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"bob", ["GroupFail"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

%% check multiple paths

test20() ->
    P1 = ["[**]"],
    P2 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1, [read, write],
                         "bob", ["other", "one"]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "bob"}).

test21() ->
    P1 = ["[**]"],
    P2 = ["a", "b", "c"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1, [read, write],
                         "bob", ["other", "one"]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "bob"}).

test22() ->
    P1 = ["[*]", "b"],
    P2 = ["a", "b"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1, [read, write],
                         "bob", ["other", "one"]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "bob"}).

test23() ->
    P1 = ["a", "[*]", "c"],
    P2 = ["a", "b", "c"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1, [read, write],
                         "bob", ["other", "one"]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "bob"}).


%% check wild resolution order
test24() ->
    P1 = ["[**]"],
    P2 = ["a", "[*]", "c"],
    P3 = ["a", "b", "c"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "override0", ["a view", "another view"]),
    Tree2 = add_controls1(Tree, [{user, "*"}], P2, [read, write],
                          "override1", ["a view", "another view"]),
    Tree3 = add_controls1(Tree2, [{user, "User"}], P3, [read, write],
                          "override2", ["a view", "another view"]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P3),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    get_as_json1(Tree3, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "override2"}).

test25() ->
    P1 = ["[**]"],
    P2 = ["a", "[*]", "c"],
    P3 = ["a", "b", "c"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "override0", ["a view", "another view"]),
    Tree2 = add_controls1(Tree, [{user, "*"}], P2, [read, write],
                          "override1", ["a view", "another view"]),
    Tree3 = add_controls1(Tree2, [{user, "User"}], P3, [read, write],
                          "", ["a view", "another view"]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P3),
    PP = pretty_print1(Tree3, "test", [], text),
    get_as_json1(Tree3, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

%% check wild resolution order
test26() ->
    P1 = ["a", "[*]"],
    P2 = ["a", "b", "c", "d", "e"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "", ["a view", "another view"]),
    Tree2 = add_default1(Tree, P1, "default"),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P2),
    PP = pretty_print1(Tree2, "test", [], text),
    get_as_json1(Tree2, []),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% check can_read

test30() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_read1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == true).

test31() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    Ret = can_read1(Tree2, {"bob", ["GroupFail"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == false).

test32() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_read1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test33() ->
    P1 = ["a"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P1,
                          [read, write], "index", ["index"]),
    Ret = can_read1(Tree2, {"bob", ["Group"]}, P2),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test34() ->
    P1 = ["a"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P1,
                          [read, write], "index", ["index"]),
    Ret = can_read1(Tree2, {"User", ["None"]}, P2),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test35() ->
    P1 = ["a"],
    P2 = ["a", "b"],
    P3 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "User"}], P2,
                          [read], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_read1(Tree2, {"User", ["None"]}, P3),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test30A() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_read1(Tree2, {"bob", ["admin"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == true).
   
test30B() ->
    P1 = ["a"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P1,
                          [read, write], "index", ["index"]),
    Ret = can_read1(Tree2, {"bob", ["admin"]}, P2),
    get_as_json1(Tree2, []),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

%% check can_write

test40() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    Ret = can_write1(Tree2, {"bob", ["Group"]}, P),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == true).

test41() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    Ret = can_write1(Tree2, {"bob", ["GroupFail"]}, P),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test42() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [read], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [read], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_write1(Tree2, {"bob", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test43() ->
    P1 = ["a"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P1,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_write1(Tree2, {"bob", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test44() ->
    P1 = ["a"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Fail"}],
                         P1,
                         [read], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "User"}, {group, "Fail Again"}], P1,
                          [write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_write1(Tree2, {"User", []}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test45() ->
    P1 = ["a"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Fail"}],
                         P1,
                         [read], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "User"}, {group, "Fail Again"}], P1,
                          [write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_write1(Tree2, {"Bollocks", []}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

test40A() ->
     P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P,
                          [read, write], "index", ["index"]),
    Ret = can_write1(Tree2, {"bob", ["admin"]}, P),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == true).

test40B() ->
    P1 = ["a"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1,
                         [read, write], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "gordon"}, {group, "Group"}], P1,
                          [read, write], "index", ["index"]),
    get_as_json1(Tree2, []),
    Ret = can_write1(Tree2, {"bob", ["admin"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

%% Now remove permissions
test50() ->
    P = ["a"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                     P, [write]),
    Tree2 = add_perm1(Tree, [{user, "gordon"}], P, [read]),
    Tree3 = remove_perm1(Tree2, [{user, "gordon"}], P, [read]),
    get_as_json1(Tree3, []),
    io:format("Tree is ~p~nTree2 is ~p~nTree3 is ~p~n", [Tree, Tree2, Tree3]),
    (Tree == Tree3).

test51() ->
    P = ["a"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
                     [read]),
    Tree2 = add_perm1(Tree, [{user, "User"}], P, [write]),
    Tree3 = remove_perm1(Tree2, [{user, "User"}], P, [write]),
    get_as_json1(Tree3, []),
    io:format("Tree is ~p~nTree3 is ~p~n", [Tree, Tree3]),
    (Tree == Tree3).

test52() ->
    P = ["a"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                     P, [read, write]),
    Tree2 = add_perm1(Tree, [{user, "Bob"}, {group, "Admin"}, {group, "Help"}],
                      P, [read, write]),
    Tree3 = add_perm1(Tree2, [{user, "User"}], P, [write]),
    Tree4 = remove_perm1(Tree3, [{user, "User"}, {group, "Group"}], P, [write]),
    get_as_json1(Tree4, []),
    Ret = can_write1(Tree4, {"User", []}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == false).

%% add permissions, defaults and views
test60() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read, {trans, 123}, {trans, 456}],
                         "default", ["default"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P),
    get_as_json1(Tree, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

test61() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read, {trans, 123}, {trans, 456}],
                         "default", ["index", "default"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P),
    get_as_json1(Tree, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

test62() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read, {trans, 123}, {trans, 456}],
                         "default", ["index", "default"]),
    Tree2 = add_controls1(Tree, [{group, "Group"}], P,
                          [read, {trans, xxx}, {trans, yyy}], "supervisor",
                          ["index", "default", "supervisor"]),
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "default"}).

test63() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "Old"}], P,
                         [read, {trans, 123}, {trans, 456}],
                         "default", ["index", "default"]),
    Tree2 = add_controls1(Tree, [{group, "Group"}], P,
                          [read, {trans, xxx}, {trans, yyy}], "supervisor",
                          ["index", "default", "supervisor"]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "supervisor"}).

test64() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "Old"}], P,
                         [read, {trans, 123}, {trans, 456}],
                         "default", ["index", "default"]),
    Tree2 = add_controls1(Tree, [{group, "Group"}], P,
                          [read, {trans, xxx}, {trans, yyy}], "supervisor",
                          ["index", "default", "supervisor"]),
    Tree3 = add_controls1(Tree2, [{group, "Subordinate"}], P,
                          [read, {trans, ab12}, {trans, bc23}], "subordinate",
                          ["index", "default", "subordinate"]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    get_as_json1(Tree3, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "supervisor"}).

test65() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read, {trans, 123}, {trans, 456}],
                         "default", ["index", "default"]),
    Tree2 = add_controls1(Tree, [{user, "User"}], P,
                          [read, {trans, xxx}, {trans, yyy}], "supervisor",
                          ["index", "default", "supervisor"]),
    Tree3 = add_controls1(Tree2, [{user, "User"}], P,
                          [read, {trans, ab12}, {trans, bc23}], "subordinate",
                          ["index", "default", "subordinate"]),
    get_as_json1(Tree3, []),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "subordinate"}).

test66() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    Tree2 = add_controls1(Tree, [{user, "User"}], P,
                          [read], "supervisor",
                          ["supervisor"]),
    Tree3 = add_controls1(Tree2, [{user, "User"}], P,
                          [read], "subordinate",
                          []),
    get_as_json1(Tree3, []),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "subordinate"}).

%% add views alone
test70() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    Tree2 = add_views1(Tree, [{user, "User"}], P, [],
                       ["supervisor"]),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = get_views1(Tree2, {"User", ["Group"]}, P),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["supervisor", "index", "default"]).

test71() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    Tree2 = add_views1(Tree, [{user, "User"}], P, [],
                       ["supervisor"]),
    Tree3 = add_default1(Tree2, P, "and more..."),
    io:format("Tree2 is ~p~n", [Tree3]),
    Ret = get_views1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree3, []),
    (Ret == ["supervisor", "index", "default", "and more..."]).

test72() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    Tree2 = add_views1(Tree, [{user, "User"}], P, "supervisor",
                       ["supervisor"]),
    Tree3 = add_default1(Tree2, P, "and more..."),
    io:format("Tree2 is ~p~n", [Tree3]),
    Ret = get_views1(Tree3, {"User", ["Group"]}, P),
    get_as_json1(Tree3, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["supervisor", "index", "default", "and more..."]).

test73() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    Tree2 = add_views1(Tree, [{user, "User"}], P, "supervisor",
                       ["supervisor"]),
    Tree3 = add_default1(Tree2, P, "and more..."),
    io:format("Tree2 is ~p~n", [Tree3]),
    Ret = get_views1(Tree3, {"User", ["Group"]}, P),
    get_as_json1(Tree3, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["supervisor", "index", "default", "and more..."]).

test74() ->
    P1 = ["a", "b"],
    P2 = ["a", "b", "c"],
    P3= ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "default", ["index", "default"]),
    Tree2 = add_default1(Tree, P2, "and more..."),
    Tree3 = add_views1(Tree2, [{user, "User"}], P3, "supervisor",
                       ["supervisor"]),
    Ret = get_views1(Tree3, {"User", ["Group"]}, P3),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    get_as_json1(Tree3, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["supervisor"]).

test75() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    io:format("Tree is ~p~n", [Tree]),
    Tree2 = remove_views1(Tree, [{user, "User"}], P,
                          {"default", []}),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = get_views1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == ["index", "default"]).

test76() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    io:format("Tree is ~p~n", [Tree]),
    Tree2 = remove_views1(Tree, [{user, "User"}], P,
                          {"default", ["default"]}),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = get_views1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == ["index"]).

test77() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index"]),
    io:format("Tree is ~p~n", [Tree]),
    Tree2 = remove_views1(Tree, [{user, "User"}], P,
                          {"default", []}),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = get_views1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == ["index"]).

test78() ->
    P = ["a", "b", "c", "d"],
    P2 = ["x", "y", "z"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "nodule"]),
    io:format("Tree is ~p~n", [Tree]),
    Tree2 = remove_views1(Tree, [{user, "User"}], P,
                          {"frisky", ["pinky", "perky"]}),
    Tree3 = remove_views1(Tree2, [{user, "User"}], P2,
                          {"frisky", ["pinky", "perky"]}),
    io:format("Tree3 is ~p~n", [Tree3]),
    Ret = get_views1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree3, []),
    (Ret == ["nodule", "index", "default"]).

%% now add many views to many groups etc...
test79() ->
    P = ["a", "b", "c", "d"],
    P1 = ["a"],
    P2 = ["a", "b", "c"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "nodule"]),
    Tree2 = add_controls1(Tree, [{group, "Group1"}], P,
                          [read], "", ["andy", "bob"]),
    Tree3 = add_controls1(Tree2, [{group, "Group2"}], P,
                          [read], "charlie", ["dave", "eddie"]),
    Tree4 = add_default1(Tree3, P1, "x-ray"),
    Tree5 = add_default1(Tree4, P2, "zebra"),
    Ret = get_views1(Tree5, {"User", ["Group1", "Group2"]}, P),
    io:format(pretty_print1(Tree5, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree5, []),
    (Ret == ["nodule", "index", "eddie", "default", "dave", "charlie",
             "bob", "andy"]).

test70A() ->
    P = ["a", "b", "c", "d"],
    P1 = ["a"],
    P2 = ["a", "b", "c"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "nodule"]),
    Tree2 = add_controls1(Tree, [{group, "Group1"}], P,
                          [read], "", ["andy", "bob"]),
    Tree3 = add_controls1(Tree2, [{group, "Group2"}], P,
                          [read], "charlie", ["dave", "eddie"]),
    Tree4 = add_default1(Tree3, P1, "x-ray"),
    Tree5 = add_default1(Tree4, P2, "zebra"),
    Ret = get_views1(Tree5, {"Benjamin", ["admin"]}, P),
    io:format(pretty_print1(Tree5, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree5, []),
    (Ret == ["**"]).

test70B() ->
    P1 = ["a", "b"],
    P2 = ["a", "b", "c"],
    P3= ["a", "b", "c", "d"],
    P4 = ["a", "b", "c", "d", "e"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "default", ["index", "default"]),
    Tree2 = add_default1(Tree, P2, "and more..."),
    Tree3 = add_views1(Tree2, [{user, "User"}], P3, "supervisor",
                       ["supervisor"]),
    Ret = get_views1(Tree3, {"Benjamin", ["admin"]}, P4),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    get_as_json1(Tree3, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == []). 

%% add a default or two
test80() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "", ["index", "default"]),
    Tree2 = add_default1(Tree, P, "supervisor"),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "supervisor"}).

test81() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "", ["index", "default"]),
    Tree2 = add_default1(Tree, P, "supervisor"),
    Tree3 = add_default1(Tree2, P, "blah-blah"),
    io:format("Tree3 is ~p~n", [Tree3]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree3, []),
    (Ret == {html, "blah-blah"}).

test82() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "", ["index", "default"]),
    Tree2 = add_default1(Tree, P, "supervisor"),
    io:format("Tree2 is ~p~n", [Tree2]),
    Ret = get_views1(Tree2, {"User", ["Group"]}, P),
    get_as_json1(Tree2, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["supervisor", "index", "default"]).

test83() ->
    P1 = ["a", "b"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "", ["index", "default"]),
    Tree2 = add_default1(Tree, P1, "supervisor"),
    Tree3 = add_default1(Tree2, P2, "oh, yeah!"),
    Tree4 = add_controls1(Tree3, [{user, "User"}], P2,
                          [read], "", ["hey!", "ho!"]),
    io:format(pretty_print1(Tree4, "test", [], text)),
    Ret = get_views1(Tree4, {"User", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree4, []),
    (Ret == ["oh, yeah!", "ho!", "hey!"]).

test84() ->
    P1 = ["a", "b"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "", ["index", "default"]),
    Tree2 = add_default1(Tree, P1, "supervisor"),
    Tree3 = add_default1(Tree2, P1, "supervisoronnie"),
    Tree4 = add_default1(Tree3, P2, "oh, yeah!"),
    Tree5 = add_default1(Tree4, P2, "oh, yeaheroonie!"),
    Tree6 = add_controls1(Tree5, [{user, "User"}], P2,
                          [read], "", ["hey!", "ho!"]),
    get_as_json1(Tree6, []),
    io:format(pretty_print1(Tree6, "test", [], text)),
    Ret = get_views1(Tree6, {"User", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["oh, yeaheroonie!", "ho!", "hey!"]).

test80A() ->
    P1 = ["a", "b"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "", ["index", "default"]),
    Tree2 = add_default1(Tree, P1, "supervisor"),
    Tree3 = add_default1(Tree2, P1, "supervisoronnie"),
    Tree4 = add_default1(Tree3, P2, "oh, yeah!"),
    Tree5 = add_default1(Tree4, P2, "oh, yeaheroonie!"),
    Tree6 = add_controls1(Tree5, [{user, "User"}], P2,
                          [read], "", ["hey!", "ho!"]),
    get_as_json1(Tree6, []),
    io:format(pretty_print1(Tree6, "test", [], text)),
    Ret = get_views1(Tree6, {"bob", ["admin"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["**"]).

%% remove a gui
test90() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default"]),
    Tree2 = add_controls1(Tree, [{user, "Bob"}], P,
                          [read], "default2", ["index", "default2"]),
    Tree3 = remove_views1(Tree2, [{user, "User"}, {group, "Group"}],
                          P, {"default", ["default"]}),
    io:format("Tree3 is ~p~n", [Tree3]),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format(pretty_print1(Tree3, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree3, []),
    (Ret == {html, "index"}).

test91() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default", "bob"]),
    Tree2 = add_controls1(Tree, [{user, "Bob"}], P,
                          [read], "default2", ["index", "default2"]),
    Tree3 = remove_views1(Tree2, [{user, "User"}, {group, "Group"}], P,
                          {[], ["default", "index", "bob", "jim"]}),
    io:format("Tree3 is ~p~n", [Tree3]),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree3, []),
    (Ret == {html, "default"}).

test92() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "default", ["index", "default", "bob"]),
    Tree2 = add_controls1(Tree, [{user, "Bob"}], P,
                          [read], "default2", ["index", "default2"]),
    Tree3 = remove_views1(Tree2, [{user, "User"}, {group, "Group"}], P,
                          {"default", ["default", "index", "bob", "jim"]}),
    io:format("Tree3 is ~p~n", [Tree3]),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree3, []),
    (Ret == {return, '404'}).

%% remove a default
test100() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P,
                         [read], "", ["index", "default"]),
    Tree2 = add_default1(Tree, P, "default"),
    Tree3 = remove_default1(Tree2, P, "default"),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree3, []),
    (Ret == {html, "default"}).

test101() ->
    P1 = ["a", "b"],
    P2 = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P2,
                         [read], "", ["index", "ya bas!"]),
    Tree2 = add_default1(Tree, P1, "Dom"),
    Tree3 = add_default1(Tree2, P2, "Sub"),
    Tree4 = remove_default1(Tree3, P2, "Sub"),
    PP = pretty_print1(Tree4, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree4, {"User", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree4, []),
    (Ret == {html, "Sub"}).

test102() ->
    P1 = ["a", "b"],
    P2 = ["a", "b", "c", "d"],
    P3 = ["x", "y", "z"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P2,
                         [read], "", ["index", "ya bas!"]),
    Tree2 = add_default1(Tree, P1, "Dom"),
    Tree3 = add_default1(Tree2, P2, "Sub"),
    Tree4 = remove_default1(Tree3, P2, "Sub"),
    Tree5 = remove_default1(Tree4, P3, "Banjo"),
    PP = pretty_print1(Tree5, "test", [], text),
    io:format(PP),
    get_as_json1(Tree5, []),
    Ret = check_get_page1(Tree5, {"User", ["Group"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "Sub"}).

%% now check if a page is acceptable
test110() ->
    P1 = ["a", "b"],
    P2 = ["a"],
    P3 = ["does", "not", "exist"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "User2"}], P2,
                          [read], "default", ["index", "default", "bingo"]),
    Tree3 = add_controls1(Tree2, [{user, "User"}], P1,
                          [read], "index", ["index", "default", "special"]),
    Ret1 = check_get_page1(Tree3, {"User", ["RandomGroup"]}, P1),
    Ret2 = check_get_page1(Tree3, {"User", ["Group"]}, P2, "epic fail"),
    Ret3 = check_get_page1(Tree3, {"User", ["Group"]}, P3),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    get_as_json1(Tree3, []),
    io:format("Ret1 is ~p~nRet2 is ~p~nRet3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "index"}, {return, '401'}, {return, '404'}}).

% write 2 metric fuckloads more tests for acceptable pages and stuff...

%% now do the old wild card stuff
test111() ->
    P1 = ["a", "[*]", "c"],
    P2 = ["a", "b", "c"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "index", ["index"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P2),
    % io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree, []),
    (Ret == {html, "index"}).

test112() ->
    P1 = ["a", "[**]"],
    P2 = ["a", "b", "x", "y"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "index", ["index"]),
    Ret = check_get_page1(Tree, {"User", ["Group"]}, P2),
    % io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree, []),
    (Ret == {html, "index"}).

%% specific (ie [*]) overrides the general (ie [**])
test113() ->
    P1 = ["a", "[**]"],
    P2 = ["a", "[*]", "x"],
    P3 = ["a", "b", "x"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "User"}], P2,
                          [read], "special", ["index", "special"]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P3),
    % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "special"}).

test114() ->
    P1 = ["a", "[**]"],
    P2 = ["a", "[*]", "x"],
    P3 = ["a", "b", "x"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "User"}], P2,
                          [read], "special", ["index", "special"]),
    Ret = check_get_page1(Tree2, {"User", ["Group"]}, P3),
    % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "special"}).

%% test wild card user and group names
test115() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "*"}, {group, "Group"}], P,
                         [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["No Match"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree, []),
    (Ret == {html, "index"}).

test116() ->
    P = ["a", "b", "c", "d"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "*"}], P,
                         [read, write], "index", ["index"]),
    Ret = check_get_page1(Tree, {"gordon", ["No Match"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree, []),
    (Ret == {html, "index"}).

test117() ->
    P = ["[*]"],
    Tree = add_controls1(gb_trees:empty(), [{user, "*"}, {group, "*"}],
                         P, [read],
                         "index", ["index"]),
    Ret = check_get_page1(Tree, {"junk", ["no way", "no how", "no soon"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree, []),
    (Ret == {html, "index"}).

%% Insert a wild permission twice
test118() ->
    P = ["[*]"],
    Tree = add_controls1(gb_trees:empty(), [{user, "*"}, {group, "*"}],
                         P, [read],
                         "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "*"}, {group, "*"}], P, [read],
                          "index", ["index"]),
    Ret = check_get_page1(Tree2, {"junk", ["no way", "no how", "no soon"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "index"}).


%% check_get_page with a page name...
test120() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["brick"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "default"),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "default"}).

test121() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["brick"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "brick"),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "brick"}).

test122() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["brick"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "bonkette"),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "bonkette"}).

test123() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["brick"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "beezer"),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {return, '404'}).

test124() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["**"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "beezer"),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "beezer"}).

test125() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["*"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "beezer"),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {return, '401'}).

test126() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["*"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "User/beezer"),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree2, []),
    (Ret == {html, "User/beezer"}).

%% global wild beats local wild
test127() ->
    P1 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1, [read, write],
                         "bonkette", ["*", "**"]),
    Tree2 = add_default1(Tree, P1, "default"),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    get_as_json1(Tree2, []),
    Ret = check_get_page1(Tree2, {"User", ["nothing", "in", "particular"]},
                          P1, "beezer"),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "beezer"}).

%% wild on wild...
test128() ->
    P1 = [],
    P2 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1, [read, write], "over1", ["view1"]),
    Tree2 = add_controls1(Tree, [{user, "*"}, {group, "*"}], P1, [read, write],
                          "over2", ["view2"]),
    Tree3 = add_default1(Tree2, P1, "default1"),
    Tree4 = add_controls1(Tree3, [{user, "User"}, {group, "Another"}], P2,
                          [read, write], "over3", ["view3"]),
    Ret = get_views1(Tree4, {"User", ["Group", "Another"]}, P2),
    io:format(pretty_print1(Tree4, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree4, []),
    (Ret == ["view3", "over3"]).

test129() ->
    P1 = ["a"],
    P2 = ["a", "b"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                         P1, [read, write], "over1", ["view1"]),
    Tree2 = add_controls1(Tree, [{user, "*"}, {group, "*"}], P1, [read, write],
                          "over2", ["view2"]),
    Tree3 = add_default1(Tree2, P1, "default1"),
    Tree4 = add_controls1(Tree3, [{user, "User"}, {group, "Another"}], P2,
                          [read, write], "over3", ["view3"]),
    Ret = get_views1(Tree4, {"User", ["Group", "Another"]}, P2),
    io:format(pretty_print1(Tree4, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    get_as_json1(Tree4, []),
    (Ret == ["view3", "over3"]).

test120A() ->
    P1 = ["a", "b"],
    P2 = ["a"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}], P1,
                         [read], "index", ["index"]),
    Tree2 = add_controls1(Tree, [{user, "User2"}], P2,
                          [read], "default", ["index", "default", "bingo"]),
    Tree3 = add_controls1(Tree2, [{user, "User"}], P1,
                          [read], "index", ["index", "default", "special"]),
    Ret = check_get_page1(Tree3, {"Benjamin", ["admin"]}, P2, "epic fail"),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    get_as_json1(Tree3, []),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "epic fail"}).

%% bug fixes
test200() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "gordon/junk"}, true, true}).

test201() ->
    P1 = ["u", "gordon", "[**]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "gordon/junk"}, true, true}).
 
test202() ->
    P1 = ["u", "gordon", "[**]"],
    P2 = ["u", "gordon", "blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "gordon/junk"}, true, true}).

test203() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{return, '404'}, false, false}).

test204() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["**", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "gordon/junk"}, true, true}).

test204a() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "gordon/junk"}, true, true}).

test204b() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "_global/pagebuilder"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "_global/pagebuilder"}, true, true}).

test205() ->
    P1 = ["u", "gordon", "[**]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["**", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "gordon/junk"}, true, true}).

test206() ->
    P1 = ["u", "gordon", "[**]"],
    P2 = ["u", "gordon", "blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["**", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{html, "gordon/junk"}, true, true}).

test207() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["**", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret1 = check_get_page1(Tree, {"gordon", []}, P2, "gordon/junk"),
    Ret2 = can_read1(Tree, {"gordon", []}, P2),
    Ret3 = can_write1(Tree, {"gordon", []}, P2),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret1 is ~p Ret2 is ~p Ret3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {{return, '404'}, false, false}).

test208() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret = check_get_page1(Tree, {"gordon", []}, P2, "_global/junk"),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

test209() ->
    P1 = ["u", "gordon", "[**]"],
    P2 = ["u", "gordon", "blahblah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret = check_get_page1(Tree, {"gordon", []}, P2, "_global/junk"),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

test210() ->
    P1 = ["u", "gordon", "[**]"],
    P2 = ["u", "gordon", "blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret = check_get_page1(Tree, {"gordon", []}, P2, "_global/junk"),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '401'}).

test211() ->
    P1 = ["u", "gordon", "[*]"],
    P2 = ["u", "gordon", "blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Ret = check_get_page1(Tree, {"gordon", []}, P2, "_global/junk"),
    io:format(pretty_print1(Tree, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

test212() ->
    P1 = ["u", "gordon", "[**]"],
    P2 = ["u", "gordon", "blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "gordon"}], P1,
                         [read, write], "",
                         ["_global/spreadsheet", "_global/pagebuilder"]),
    Tree1 = add_views1(Tree, [{user, "gordon"}], P1, "", ["**"]),
    Ret = check_get_page1(Tree1, {"gordon", []}, P2, "gordon/junk"),
    io:format(pretty_print1(Tree1, "test", [], text)),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "gordon/junk"}).

test220() ->
    P1 = [],
    P2 = ["u", "jonpuleston"],
    P3 = ["u", "jonpuleston", "[**]"],
    P4 = ["u", "stevie"],
    P5 = ["u", "stevie", "[**]"],
    P6 = ["u", "[**]"],
    P7 = ["u", "jonpuleston", "blah", "bleh"],
    Tree = add_controls1(gb_trees:empty(), [{user, "*"}, {group, "*"}],
                         P1, [read],
                        "_global/home", ["_global/home"]),
    Tree2 = add_controls1(Tree, [{user, "jonpuleston"}], P2, [read],
                          "_global/userhome", ["_global/userhome"]),
    Tree3 = add_controls1(Tree2, [{user, "jonpuleston"}], P3, [read, write],
                          "_global/spreadsheet",
                          ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Tree4 = add_controls1(Tree3, [{user, "stevie"}], P4, [read],
                          "_global/userhome", ["_global/userhome"]),
    Tree5 = add_controls1(Tree4, [{user, "stevei"}], P5, [read, write],
                          "_global/spreadsheet",
                          ["*", "_global/spreadsheet", "_global/pagebuilder"]),
    Tree6 = add_controls1(Tree5, [{group, "dev"}], P6, [read],
                          "_global/spreadsheet", ["_global/spreadsheet"]),
    io:format(auth_srv:pretty_print1(Tree6, "test", [], text)),
    Ret1 = can_read1(Tree6, {"stevie", ["dev"]}, P7),
    Ret2 = check_get_page1(Tree6, {"stevie", ["dev"]}, P7),
    Ret3 = check_get_page1(Tree6, {"stevie", ["dev"]}, P7,
                           "_global/spreadsheet"),
    io:format("Ret1 is ~p~nRet2 is ~p~nRet3 is ~p~n", [Ret1, Ret2, Ret3]),
    ({Ret1, Ret2, Ret3} == {false,
                            {return, '401'},
                            {return, '401'}}).

test221() ->
    P = [],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                     P, [read]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["admin"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "_global/spreadsheet"}).

test222() ->
    P1 = ["_site", "[*]"],
    P2 = ["_site", "static_data"],
    Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                     P1, [read]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["admin"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "_global/spreadsheet"}).

test223() ->
    P1 = ["_site", "[*]"],
    P2 = ["_site", "static_data"],
    Tree = add_controls1(gb_trees:empty(), [{user, "User"}, {group, "Group"}],
                     P1, [read], "", []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    get_as_json1(Tree, []),
    Ret = check_get_page1(Tree, {"gordon", ["admin"]}, P2),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "_global/spreadsheet"}).

test224() ->
    P1 = [],
    P2 = ["[**]"],
    P3 = ["blah", "blah"],
    Tree = add_controls1(gb_trees:empty(), [{user, "*"}],
                     P1, [read, write], "", ["**", "_global/spreadsheet"]),
    Tree2 = add_controls1(Tree, [{user, "*"}],
                     P2, [read, write], "", ["**", "_global/spreadsheet"]),
    io:format(pretty_print1(Tree2, "test", [], text)),
    Ret = check_get_page1(Tree2, {"anonymous", []}, P3),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "_global/spreadsheet"}).
    

%% Numbered tests deal with normal users
%% Lettered tests deal with the 'admin' group
unit_test_() -> 
    [
     % tests for the root page []
     ?_assert(test01()),
     ?_assert(test02()),
     ?_assert(test03()),
     ?_assert(test04()),
     ?_assert(test05()),
     ?_assert(test06()),
     ?_assert(test07()),
     ?_assert(test08()),
     ?_assert(test09()),
     ?_assert(test010()),
     ?_assert(test011()),
     ?_assert(test012()),
     ?_assert(test013()),
     ?_assert(test014()),
     ?_assert(test015()),
     ?_assert(test016()),
     ?_assert(test017()),
     ?_assert(test018()),
     ?_assert(test019()),
     ?_assert(test020()),
     ?_assert(testA()),
     ?_assert(testB()),
     ?_assert(testC()),
     ?_assert(testD()),     
     ?_assert(testE()),
     ?_assert(testF()),
     ?_assert(testG()),
     ?_assert(testH()),     
     ?_assert(testI()),
     ?_assert(test10()),
     ?_assert(test11()),
     ?_assert(test12()),
     ?_assert(test13()),
     ?_assert(test14()),
     ?_assert(test15()),
     ?_assert(test16()),
     ?_assert(test17()),
     ?_assert(test18()),
     ?_assert(test18a()),
     ?_assert(test19()),
     ?_assert(test19a()),
     ?_assert(test19b()),
     ?_assert(test19c()),
     ?_assert(test19d()),
     ?_assert(test19e()),
     ?_assert(test19f()),
     ?_assert(test19g()),
     ?_assert(test19h()),
     ?_assert(test20()),
     ?_assert(test22()),
     ?_assert(test23()),
     ?_assert(test24()),
     ?_assert(test25()),
     ?_assert(test26()),
     ?_assert(test30()),
     ?_assert(test31()),
     ?_assert(test32()),
     ?_assert(test33()),
     ?_assert(test34()),
     ?_assert(test35()),
     ?_assert(test30A()),
     ?_assert(test30B()),     
     ?_assert(test40()),
     ?_assert(test41()),
     ?_assert(test42()),
     ?_assert(test43()),
     ?_assert(test44()),
     ?_assert(test45()),
     ?_assert(test40A()),
     ?_assert(test40B()),     
     ?_assert(test50()),
     ?_assert(test51()),
     ?_assert(test52()),
     ?_assert(test60()),
     ?_assert(test61()),
     ?_assert(test62()),
     ?_assert(test63()),
     ?_assert(test64()),
     ?_assert(test65()),
     ?_assert(test66()),
     ?_assert(test70()),
     ?_assert(test71()),
     ?_assert(test72()),
     ?_assert(test73()),
     ?_assert(test74()),
     ?_assert(test75()),
     ?_assert(test76()),
     ?_assert(test77()),
     ?_assert(test78()),
     ?_assert(test79()),
     ?_assert(test70A()),
     ?_assert(test70B()),  
     ?_assert(test80()),
     ?_assert(test81()),
     ?_assert(test82()),
     ?_assert(test83()),
     ?_assert(test84()),
     ?_assert(test80A()),
     ?_assert(test90()),
     ?_assert(test91()),
     ?_assert(test92()),
     ?_assert(test100()),
     ?_assert(test101()),
     ?_assert(test102()),
     ?_assert(test110()),
     ?_assert(test111()),
     ?_assert(test112()),
     ?_assert(test113()),
     ?_assert(test114()),
     ?_assert(test115()),
     ?_assert(test116()),
     ?_assert(test117()),
     ?_assert(test118()),
     ?_assert(test120()),
     ?_assert(test121()),
     ?_assert(test122()),
     ?_assert(test123()),
     ?_assert(test124()),
     ?_assert(test125()),
     ?_assert(test126()),
     ?_assert(test127()),
     ?_assert(test128()),
     ?_assert(test129()),
     ?_assert(test120A()),
     ?_assert(test200()),
     ?_assert(test201()),
     ?_assert(test202()),
     ?_assert(test203()),
     ?_assert(test204()),
     ?_assert(test204a()),
     ?_assert(test204b()),
     ?_assert(test205()),
     ?_assert(test206()),
     ?_assert(test207()),
     ?_assert(test208()),
     ?_assert(test209()),
     ?_assert(test210()),
     ?_assert(test211()),
     ?_assert(test212()),
     ?_assert(test220()),
     ?_assert(test221()),
     ?_assert(test222()),
     ?_assert(test223()),
     ?_assert(test224())
    ].

debug() ->
    P1 = ["a", "b", "c"],
    P2 = ["x", "y", "z"],
    Tree = add_default1(gb_trees:empty(), P1, "default"),
    Tree2 = add_default1(Tree, P1, "new default"),
    Tree3 = add_controls1(Tree2, [{user, "User"}, {groups, "Admin"}],
                          P1, [read, write],
                          "overide", ["index", "another"]),
    Tree4 = add_controls1(Tree3, [{user, "User2"}, {groups, "Admin"}], P2, [read],
                          "overideagain", ["index", "another one"]),
    get_as_json1(Tree4, []).


