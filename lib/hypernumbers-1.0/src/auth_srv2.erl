%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       Authorisation Server
%%% 
%%% @end
%%% Created :  7 Oct 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------

-module(auth_srv2).

-behaviour(gen_server).

-include("auth2.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).

%% API 
-export([start_link/0]).

-export([
         check_get_page/3,
         check_get_page/4,
         get_views/3,
         add_views/3,
         add_users_and_groups/4,
         set_champion/3,
         set_challenger/3,
         remove_views/4,
         remove_champion/2,
         remove_challenger/2,
         get_as_json/2,
         pretty_print/3,
         dump_script/1
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
            {add_views, Host, Pg, Vs} ->
                {Host, add_views1(get(Host, Tr), Pg, Vs), true};
            {add_users_and_groups, Host, Pg, V, AL} ->
                {Host, add_users_and_groups1(get(Host, Tr), Pg, V, AL), true};
            {set_champion, Host, Pg, Df} ->
                {Host, set_default1(get(Host, Tr), Pg, Df, champion), true};
            {set_challenger, Host, Pg, Df} ->
                {Host, set_default1(get(Host, Tr), Pg, Df, challenger), true};
            {rem_views, Host, Pg, Vs} ->
                {Host, remove_views1(get(Host, Tr), Pg, Vs), true};
            {rem_champion, Host, Pg} ->
                {Host, remove_default1(get(Host, Tr), Pg, champion), true};
            {rem_challenger, Host, Pg} ->
                {Host, remove_default1(get(Host, Tr), Pg, challenger), true};
            {get_as_json, Host, Pg} ->
                {Host, get_as_json1(get(Host, Tr), Pg), false};
            {pretty_print, Host, Pg, Type} ->
                {Host, pretty_print1(get(Host, Tr), Host, Pg, Type), false};
            {dump_script, Host} ->
                {Host, dump_script1(get(Host, Tr)), false};
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
check_get_page(Host, AuthSpec, Page) -> 
    gen_server:call(auth_srv, {check_get_page, Host, AuthSpec, Page}).

check_get_page(Host, AuthSpec, Page, Gui) ->
    gen_server:call(auth_srv, {check_get_page, Host, AuthSpec, Page, Gui}).

get_views(Host, AuthSpec, Page) ->
    gen_server:call(auth_srv, {get_views, Host, AuthSpec, Page}).

add_views(Host, Page, Views) ->
    gen_server:call(auth_srv, {add_views, Host, Page, Views}).

add_users_and_groups(Host, Page, Views, AuthList) ->
    gen_server:call(auth_srv, {add_users_and_groups, Host, Page, Views, AuthList}).

set_champion(Host, Page, Gui) ->
    gen_server:call(auth_srv, {set_champion, Host, Page, Gui}).

set_challenger(Host, Page, Gui) ->
    gen_server:call(auth_srv, {set_challenger, Host, Page, Gui}).

remove_views(Host, AuthList, Page, Views) ->
    gen_server:call(auth_srv, {rem_views, Host, AuthList, Page, Views}).

remove_champion(Host, Page) ->
    gen_server:call(auth_srv, {rem_champion, Host, Page}).

remove_challenger(Host, Page) ->
    gen_server:call(auth_srv, {rem_challenger, Host, Page}).

get_as_json(Host, Page) ->
    gen_server:call(auth_srv, {get_as_json, Host, Page}).

pretty_print(Host, Page, Type) ->
    gen_server:call(auth_srv, {pretty_print, Host, Page, Type}).

dump_script(Host) ->
    gen_server:call(auth_srv, {dump_script, Host}).

clear_all_perms_DEBUG(Host) ->
    gen_server:call(auth_srv, {clear_all_perms, Host}).

permissions_DEBUG(Host, AuthSpec, Page) -> 
    gen_server:call(auth_srv, {permissions_debug, Host, AuthSpec, Page}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_controls(List) -> make_c2(List, []).

make_c2([], Acc)      -> Acc;
make_c2([H | T], Acc) -> make_c2(T, [#control{view = H} | Acc]).

dump_script1(Tree) -> dump_s1(Tree, [], []).

permissions_debug1(Tree, {_U, _Gs}, Page) ->
    Fun =
        fun(_X) ->
                ok
        end,
    check_get(Tree, Page, Fun).    

check_get_page1(Tree, {User, Groups}, Page) ->
    % first see if the user has permission to see the page
    % then see what page they should be getting
     Fun =
         fun(X) ->
                 get_page(X, User, Groups, champion)
         end,
    check_get(Tree, Page, Fun).

check_get_page1(Tree, {_User, _Groups}, Page, _View) -> 
    % first see if the user has permission to see the page
    % then see what view they should be getting
     Fun =
         fun(_X) ->
                 ok
         end,
    check_get(Tree, Page, Fun).

get_views1(Tree, {_User, _Groups}, Page) ->
    Fun = fun(_X) ->
                  ok
          end,
    check_get(Tree, Page, Fun).

add_views1(Tree, [], Views)   ->
    case gb_trees:lookup(controls, Tree) of
        none       -> gb_trees:insert(controls, make_controls(Views), Tree);
        {value, V} -> {controls, Vs} = V,
                      NewVs = lists:merge(lists:sort(make_controls(Views)),
                                          lists:sort(Vs)),
                      gb_trees:enter(controls, NewVs, Tree)
    end;
add_views1(Tree, [H | T], Views) ->
    ok = force_terminal([H | T], "add_views1"),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = add_views1(Empty, T, Views),
                      gb_trees:insert({seg, H}, NewVal, Tree);
        {value, V} -> NewVal = add_views1(V, T, Views),
                      gb_trees:enter({seg, H}, NewVal, Tree)
    end.

add_users_and_groups1(Tree, [], View, UAndGs)   ->
    case gb_trees:lookup(controls, Tree) of
        none       -> Ctrl = [#control{view = View}],
                      NewCtrls = add_us_and_gps(Ctrl, View, UAndGs),
                      gb_trees:insert(controls, NewCtrls, Tree);
        {value, V} -> NewCtrls = add_us_and_gps(V, View, UAndGs),
                      gb_trees:enter(controls, NewCtrls, Tree)
    end;
add_users_and_groups1(Tree, [H | T], View, UAndGs) ->
    ok = force_terminal([H | T], "add_users_and_groups1"),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = add_users_and_groups1(Empty, T, View, UAndGs),
                      gb_trees:insert({seg, H}, NewVal, Tree);
        {value, V} -> NewVal = add_users_and_groups1(V, T, View, UAndGs),
                      gb_trees:enter({seg, H}, NewVal, Tree)
    end.


set_default1(Tree, [], Default, Type)   ->
    case gb_trees:lookup(controls, Tree) of
        none       -> Ctrl = [#control{view = Default}],
                      NewCtrls = set_def(Ctrl, Default, Type),
                      gb_trees:insert(controls, NewCtrls, Tree);
        {value, V} -> NewCtls = set_def(V, Default, Type),
                      gb_trees:enter(controls, NewCtls, Tree)
    end;                      
set_default1(Tree, [H | T], Default, Type) ->
    ok = force_terminal([H | T], "set_default1"),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = set_default1(Empty, T, Default, Type),
                      gb_trees:insert({seg, H}, NewVal, Tree);
        {value, V} -> NewVal = set_default1(V, T, Default, Type),
                      gb_trees:enter({seg, H}, NewVal, Tree)
    end.


remove_views1(Tree, [], Views) ->
    case gb_trees:lookup(controls, Tree) of
        none       -> Tree;
        {value, V} -> NewCtls = remove_views(V, Views),
                      gb_trees:enter(controls, NewCtls, Tree)
    end;                          
remove_views1(Tree, [H | T], Views) -> 
    ok = force_terminal([H | T], "remove_views"),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Tree;
        {value, V} -> NewViews = remove_views1(V, T, Views),
                      gb_trees:enter({seg, H}, NewViews, Tree)
    end.

remove_default1(Tree, [], Type) ->
    io:format("Tree is ~p~n-Type is ~p~n", [Tree, Type]),
    case gb_trees:lookup(controls, Tree) of
        none       -> Tree;
        {value, V} -> NewCtls = unset_default(V, Type),
                      gb_trees:enter(controls, NewCtls, Tree)
    end;                          
remove_default1(Tree, [H | T], Type) ->
    io:format("Tree is ~p~n-Page is ~p Type is ~p~n", [Tree, [H | T], Type]),
    ok = force_terminal([H | T], "remove_views"),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Tree;
        {value, V} -> NewViews = remove_default1(V, T, Type),
                      gb_trees:enter({seg, H}, NewViews, Tree)
    end.
   
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

%%
%% Internal Functions
%%

remove_views(Ctrl, [])      -> Ctrl;
remove_views(Ctrl, [H | T]) ->
    NewCtrl = lists:keydelete(H, #control.view, Ctrl),
    remove_views(NewCtrl, T).

force_terminal([H | T], Msg) ->
%% force [**] to be a terminal segment only
    case {H, T} of
        {"[**]", []} -> ok;
        {"[**]", _X} -> exit("non-terminal [**] segment in " ++ Msg);
        _            -> ok
    end.
    
set_def(Views, Default, Type) ->
    NewViews = unset_default(Views, type),
    set_def2(NewViews, Default, Type).

set_def2(Views, Default, Type) ->
    case lists:keysearch(Default, #control.view, Views) of
        false         -> NewView = #control{view = Default, default = Type},
                         [NewView, Views];
        {value, View} -> lists:keyreplace(View#control.view,
                                          #control.view, Views,
                                          View#control{default = Type})
    end.

unset_default(Views, Type) ->
    case lists:keysearch(Type, #control.default, Views) of
        false         -> Views;
        {value, View} -> lists:keyreplace(View#control.view,
                                          #control.view, Views,
                                          View#control{default = false})
    end.
                     

add_us_and_gps(Views, _View, [])                -> io:format("exiting here with ~p~n", [Views]),
                                                   Views;
add_us_and_gps(Views, View, [{Type, Role} | T]) ->
    case lists:keysearch(View, #control.view, Views) of
        false        -> Ctrl = [#control{view = View}],
                        NewCtrls = add_us_and_gps(Ctrl, View, [{Type, Role}]),
                        NewViews = lists:merge(lists:sort(NewCtrls), lists:sort(Views));
        {value, Ctl} ->
            NewV = case Type of
                       user  -> #control{users = U} = Ctl,
                                NewU = lists:merge(lists:sort(U), [Role]),
                                Ctl#control{users = NewU}; 
                       group -> #control{groups = G} = Ctl,
                                NewG = lists:merge(lists:sort(G), [Role]),
                                Ctl#control{groups = NewG}
                   end,
            NewViews  = lists:keyreplace(View, #control.view, Views, NewV)
    end,
    add_us_and_gps(NewViews, View, T).

get_page([], _User, _Groups, _Type) -> io:format("return from get_page (1) ~n"),
                                      {return, '404'};
get_page(List, User, Groups, Type)  ->
    case lists:keyfind(Type, #control.default, List) of
        #control{view = F}-> io:format("return from get_page (2) ~n"),
                             {html, F};
        false              ->
            case get_page2(List, User) of
                {html, F2} -> io:format("return from get_page (2) ~n"),
                              {html, F2};
                false      -> io:format("return from get_page (3) ~n"),
                              get_page3(List, Groups)
            end
    end.

get_page2([], _User)     -> io:format("return from get_page2 (1) ~n"),
                            false;
get_page2([H | T], User) ->
    case lists:member(User, H#control.users) of
        true  -> io:format("return from get_page2 (2) ~n"),
                 {html, H#control.view};
        false -> io:format("return from get_page2 (3) ~n"),
                 get_page2(T, User)
    end.

get_page3([], _Groups)     -> io:format("return from get_page3 (1) ~n"),
                              {return, '503'};
get_page3([H | T], Groups) ->
    Fun = fun(X) ->
                  io:format("X is ~p~n", [X]),
                  lists:member(X, H#control.groups)
          end,
    case lists:any(Fun, Groups) of
        true  -> io:format("return from get_page3 (2) ~n"),
                 {html, H#control.view};
        false -> io:format("return from get_page3 (3) ~n"),
                 get_page3(T, Groups)
    end.
            
dump_s1(Tree, Path, Acc) ->
    List = gb_trees:to_list(Tree),
    dump_s2(List, Path, Acc).

dump_s2([], _Path, Acc)                  -> lists:flatten(lists:reverse(Acc));
dump_s2([{acl, ACL} | T], Path, Acc)     -> NewAcc = write_perms(Path, ACL),
                                            dump_s2(T, Path, [NewAcc | Acc]);
dump_s2([{views, Views} | T], Path, Acc) -> NewAcc = write_views(Path, Views),
                                            dump_s2(T, Path, [NewAcc | Acc]);
dump_s2([{default, Def} | T], Path, Acc) -> NewAcc = write_default(Path, Def),
                                            dump_s2(T, Path, [NewAcc | Acc]);
dump_s2([H | T], Path, Acc)              -> NewAcc = dump_s3(H, Path, Acc),
                                            dump_s2(T, Path, lists:flatten([NewAcc | Acc])).

dump_s3([], _Path, Acc)        -> Acc;
dump_s3({Path1, H}, Path, Acc) -> dump_s1(H, [Path1 | Path], Acc).

write_perms(Path, Perms) -> write_p1(Path, Perms, []).

write_p1(_Path, [], Acc)                  -> Acc;
write_p1(Path, [{U_or_G, Perm} | T], Acc) -> NewAcc = {perm, [
                                                              {list, [U_or_G]},
                                                              {page, lists:reverse(Path)},
                                                              {perms, Perm}
                                                             ]},
                                             write_p1(Path, T, [NewAcc | Acc]).

write_views(Path, Views) -> write_v1(Path, Views, []).

write_v1(_Path, [], Acc)               -> Acc;
write_v1(Path, [{_U_or_G, _V} | T], Acc) ->
    NewAcc = something,
    write_v1(Path, T, [NewAcc | Acc]).

write_default(Path, Def) -> {default, [
                                       {page, lists:reverse(Path)},
                                       {default, Def}
                                      ]}.

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
% remove_from_control(Tree, {Type, Control}, [], Fun) ->
%     case gb_trees:lookup(Type, Tree) of
%         none       -> Tree;
%         {value, V} -> NewCtls = remove(Type, Control, V, Fun), 
%                       gb_trees:enter(Type, NewCtls, Tree)
%     end;
% remove_from_control(Tree, {Type, Control}, [H | T], Fun) ->
%     case gb_trees:lookup(H, Tree) of
%         none       -> Tree;
%         {value, V} -> NewVal = remove_from_control(V, {Type, Control}, T, Fun),
%                       gb_trees:enter(H, NewVal, Tree)
%     end.

%%
%% General Helper functions
%%
% has_wild(Views) ->
%     case contains("**", Views) of
%         true  -> global;
%         false -> case contains("*", Views) of
%                      true  -> local;
%                      false -> none
%                  end
%     end.

% is_local(User, View) ->
%     case re:run(View, "^" ++ User ++ "/") of
%         {match, _} -> true;                       
%         nomatch    -> false
%     end.

% remove_empty(List) -> remove_e(List, []).

% remove_e([], Acc)       -> Acc;
% remove_e([[] | T], Acc) -> remove_e(T, Acc);
% remove_e([H | T], Acc)  -> remove_e(T, [H | Acc]).                     

keyfind(K, L) ->
    case lists:keyfind(K, 1, L) of
        false  -> [];
        {K, V} -> V
    end.

% is_admin([])             -> false;
% is_admin(["admin" | _T]) -> true;
% is_admin([_H | T])       -> is_admin(T).

make_prettyprint(Tree, Host, html) -> Body = make_pp(Tree, html, [], "", []),
                                "<html><head><title>Permissions Tree</title></head><body><code>"
                                          "<bold>Permissions for " ++ Host ++
                                          "</bold><br />" ++
                                          Body ++ "</code></body></html>";
make_prettyprint(Tree, Host, text) -> "Permissions for " ++ Host ++ "~n" ++
                                          make_pp(Tree, text, [], "", []).

make_pp(_Tree, _Type, _Seg, _Prefix, _Acc) -> "fix me up!~n".

make_json(Tree, Seg) ->
    List = gb_trees:to_list(Tree),
    {array, [make_json1(K, V, Seg) || {K, V}  <- List]}.

make_json1(_Type, _V, _Seg)              -> "fix me ya bozo".
% make_json1(_K, _V, _Seg) when is_list(_K) -> "fix me as well".

load_trees(Dir, Table) ->
    {ok, _} = dets:open_file(Table, [{file, filename:join(Dir,Table)}]),
    % if the value of auth_tree is an empty list,
    % create an empty tree and fire it in..
    case dets:lookup(Table, ?KEY) of
        []            -> [];            
        [{?KEY, Val}] -> Val
    end.

check_get(Tree, [], Fun) ->
    Fun(get_controls(Tree));
check_get(Tree, [H | T], Fun) ->
    case gb_trees:lookup({seg, H}, Tree) of
        none        -> check_programmatic(Tree, T, Fun);
        {value , V} -> check_get(V, T, Fun)
    end.         

check_programmatic(Tree, List, Fun) ->
    case gb_trees:lookup({seg, "[*]"}, Tree) of
        none        -> case gb_trees:lookup({seg, "[**]"}, Tree) of
                           none        -> Fun([]);
                           {value, V1} -> Fun(get_controls(V1))
                       end;
	        {value, V2} -> check_get(V2, List, Fun)
    end.

get_controls(Tree) ->
    case gb_trees:lookup(controls, Tree) of
        none       -> [];
        {value, V} -> V
    end.

get_for_pp(Tree, [], Fun) ->
    Fun(Tree);
get_for_pp(Tree, [H | T], Fun) ->
    case gb_trees:lookup(H, Tree) of
        none        -> check_programmatic(Tree, T, Fun);
        {value , V} -> get_for_pp(V, T, Fun)
    end.

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
%% check_get_page (general)
testA1() ->
    P = [],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% add views
testA2() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"User", ["Fail"]}, P),
    get_as_json1(Tree, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).

%% Users and groups
testA3() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"User", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).
    
testA4() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA4a() ->
    P = [],
    Tree = add_users_and_groups1(gb_trees:empty(), P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA5() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"Fail", ["admin"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

%% set default/champion
testA6() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "another view", champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testA7() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "a view", champion),
    Tree3 = set_default1(Tree2, P, "another view", champion),
    Ret = check_get_page1(Tree3, {"Fail", ["admin"]}, P),
    get_as_json1(Tree3, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testA7a() ->
    P = [],
    Tree = set_default1(gb_trees:empty(), P, "a view", champion),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "another view", champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree2, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

%% remove views
testA8() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["another view"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA9() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["another view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA10() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["a view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

%% remove defaults/champion
testA11() ->
    P = [],
    Tree = set_default1(gb_trees:empty(), P, "another view", champion),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = remove_default1(Tree1, P, champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree2, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).


% test the non-empty path
testB1() ->
    P = ["hip", "hop"],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

testB2() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    io:format("Tree is ~p~n", [Tree]),
    Ret = check_get_page1(Tree, {"User", ["Fail"]}, P),
    get_as_json1(Tree, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).

testB3() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"User", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).
    
testB4() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB4a() ->
    P = ["blah", "bloh"],
    Tree = add_users_and_groups1(gb_trees:empty(), P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB5() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"Fail", ["admin"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB6() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "another view", champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testB7() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "a view", champion),
    Tree3 = set_default1(Tree2, P, "another view", champion),
    Ret = check_get_page1(Tree3, {"Fail", ["admin"]}, P),
    get_as_json1(Tree3, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testB7a() ->
    P = ["blah", "bloh"],
    Tree = set_default1(gb_trees:empty(), P, "a view", champion),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "another view", champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree2, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testB8() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["another view"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB9() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["another view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB10() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["a view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P),
    get_as_json1(Tree1, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testB11() ->
    P = ["bing", "bong"],
    Tree = set_default1(gb_trees:empty(), P, "another view", champion),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = remove_default1(Tree1, P, champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree2, []),
    % PP = pretty_print1(Tree, "test", [], text),
    % io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

%% Numbered tests deal with normal users
%% Lettered tests deal with the 'admin' group
unit_test_() -> 
    [
     % tests for the root page []
     ?_assert(testA1()),
     ?_assert(testA2()),
     ?_assert(testA3()),
     ?_assert(testA4()),
     ?_assert(testA4a()),
     ?_assert(testA5()),
     ?_assert(testA6()),
     ?_assert(testA7()),
     ?_assert(testA7a()),
     ?_assert(testA8()),
     ?_assert(testA9()),
     ?_assert(testA10()),
     ?_assert(testA11()),
     ?_assert(testB1()),
     ?_assert(testB2()),
     ?_assert(testB3()),
     ?_assert(testB4()),
     ?_assert(testB4a()),
     ?_assert(testB5()),
     ?_assert(testB6()),
     ?_assert(testB7()),
     ?_assert(testB7a()),
     ?_assert(testB8()),
     ?_assert(testB9()),
     ?_assert(testB10()),
     ?_assert(testB11())
    ].
