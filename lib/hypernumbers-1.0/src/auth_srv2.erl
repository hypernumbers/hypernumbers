%%% @copyright (C) 2009, Hypernumbers Ltd
%% TODO: Add in support for changing perms on the fly.
%% Add, Replace, etc. Will be motivated by admin panel design.

-module(auth_srv2).

-behaviour(gen_server).

-include("auth2.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).

%% API 
-export([start_link/1]).

-export([
         check_get_view/3,
         check_particular_view/4,
         check_get_challenger/3,
         get_views/3,
         get_any_view/3,
         add_view/4,
         set_champion/3,
         set_challenger/3,
         remove_views/4,
         delete_site/1,
         get_as_json/2,
         dump_script/1,
         load_script/2
        ]).

-export([
         clear_all_perms_DEBUG/1,
         demo_DEBUG/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, "auth_srv2").
-define(KEY, "auth_tree").
-define(INDEX, "hypernumbers/index").
-define(SPREADSHEET, "_global/spreadsheet").
-define(EMPTY_TREE, {0,nil}).

-record(state, {site :: string(),
                table :: string(),
                trees :: gb_tree()}).

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
-spec start_link(string()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:start_link({local, Id}, ?MODULE, [Site], []).

-spec check_get_view(string(), [string()], auth_req()) 
                    -> {view, string()} | not_found | denied.
check_get_view(Site, Path, AuthReq) -> 
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {check_get_view, Path, AuthReq}).

-spec check_get_challenger(string(), [string()], auth_req()) 
                          -> {view, string()} | not_found | denied.
check_get_challenger(Site, Path, AuthReq) -> 
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {check_get_challenger, Path, AuthReq}).

-spec check_particular_view(string(), [string()], auth_req(), string())
                           -> {view, string()} | not_found | denied.
check_particular_view(Site, Path, AuthReq, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {check_particular_view, Path, AuthReq, View}).

-spec get_any_view(string(), [string()], auth_req()) 
                  -> {view, string()} | not_found | denied.
get_any_view(Site, Path, AuthReq) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {get_any_view, Path, AuthReq}).

-spec get_views(string(), [string()], auth_req()) -> [string()]. 
get_views(Site, Path, AuthReq) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {get_views, Path, AuthReq}).

-spec add_view(string(), [string()], auth_spec(), string()) -> ok.
add_view(Site, Path, AuthSpec, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {add_view, Path, AuthSpec, View}).

-spec set_champion(string(), [string()], string()) -> ok. 
set_champion(Site, Path, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {set_champion, Path, View}).

-spec set_challenger(string(), [string()], string()) -> ok. 
set_challenger(Site, Path, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {set_challenger, Path, View}).

-spec remove_views(string(), [string()], auth_spec(), [string()]) -> ok. 
remove_views(Site, Path, AuthSpec, Views) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {rem_views, Path, AuthSpec, Views}).

get_as_json(Site, Path) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {get_as_json, Path}).

delete_site(Site) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, delete_site).

clear_all_perms_DEBUG(Site) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, clear_all_perms).

dump_script(Site) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, dump_script).

load_script(Site, Terms) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {load_script, Terms}).

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
init([Site]) ->
    Table = hn_util:site_to_fs(Site),
    {ok, Dir} = application:get_env(hypernumbers, dets_dir),
    Trees = load_trees(Dir, Table),
    {ok, #state{site = Site,
                table = Table,
                trees = Trees}}.

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
    #state{trees = Tr, table = Table, site = Site} = State,
    Return1 =
        case Request of
            {check_get_view, P, AR} ->
                {Site, check_get_view1(tree(Site, Tr), P, AR, champion), false};
            {check_get_challenger, P, AR} ->
                {Site, check_get_view1(tree(Site, Tr), P, AR, challenger),
                 false};
            {check_particular_view, P, AR, V} ->
                {Site, check_particular_view1(tree(Site, Tr), P, AR, V), false};
            {get_views, P, AR} ->
                {Site, get_views1(tree(Site, Tr), P, AR), false};
            {get_any_view, P, AR} ->
                {Site, check_get_view1(tree(Site, Tr), P, AR, any), false};
            {add_view, Pg, AS, V} ->
                {Site, add_view1(tree(Site, Tr), Pg, AS, V), true};
            {set_champion, Pg, Df} ->
                {Site, set_default(tree(Site, Tr), Pg, Df, champion), true};
            {set_challenger, Pg, Df} ->
                {Site, set_default(tree(Site, Tr), Pg, Df, challenger), true};
            {rem_views, P, Vs} ->
                {Site, remove_views1(tree(Site, Tr), P, Vs), true};
            {get_as_json, P} ->
                {Site, get_as_json1(tree(Site, Tr), P), false};
            delete_site ->
                {Site, delete};
            clear_all_perms ->
                {Site, gb_trees:empty(), true};
            dump_script ->
                {Site, dump_script1(tree(Site, Tr)), false};
            {load_script, Terms} ->
                {Site, load_script1(Terms), true}
            end,
    {Reply, NewTr} =
        case Return1 of
            {Site2, Return2, true} ->
                {ok, save_trees(Table, Site2, Return2, Tr)};
            {_Site2, Return2, false} ->
                {Return2, Tr};
            {Site2, delete} ->
                {ok, del_site_tree(Table, Site2, Tr)}
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
%%% Internal functions
%%%===================================================================

check_get_view1(Tree, Path, {User, Groups}, Type) ->
    Fun = fun(C) -> get_view(C, User, Groups, Type) end,
    run_ctl(Tree, Path, Fun).

check_particular_view1(Tree, Path, {User, Groups}, Type) ->
    Fun = fun(C) -> get_particular_view(C, User, Groups, Type) end,
    run_ctl(Tree, Path, Fun).

get_views1(Tree, Path, {User, Groups}) ->
    Fun = fun(#control{views = Views}) ->
                  KVs = gb_trees:to_list(Views),
                  [K || {K,_}=X <- KVs, 
                        get_role_view([X], User, Groups) /= none]
          end,
    run_ctl(Tree, Path, Fun).

add_view1(Tree, Path, UAndGs, V) ->
    Fun = fun(C) ->
                  CurViews = C#control.views,
                  View1 = case gb_trees:lookup(V, CurViews) of
                             none -> #view{};
                             {value, Val} -> Val end,
                  View2 = add_us_and_gps(View1, UAndGs),
                  NewViews = gb_trees:enter(V, View2, CurViews),
                  C#control{views = NewViews}
          end,
    alter_tree(Tree, Path, Fun).

%% Set default allows to change a champion or challenger.
set_default(Tree, Path, V, Type) ->
    Fun = fun(C) -> set_default2(Type, V, C) end,
    alter_tree(Tree, Path, Fun).

set_default2(champion, V, C) -> C#control{champion = V};
set_default2(challenger, V, C) -> C#control{challenger = V}.

remove_views1(Tree, Path, DelViews) ->
    Fun = fun(C) ->
                  Views2 = lists:foldl(fun(V, Acc) -> 
                                               gb_trees:delete_any(V, Acc)
                                       end,
                                       C#control.views,
                                       DelViews),
                  rem_champion(
                    DelViews, C#control.champion, 
                    rem_challenger(
                      DelViews, C#control.challenger,
                      C#control{views = Views2}
                     ))
          end,
    alter_tree(Tree, Path, Fun).

rem_champion([], _Champ, C) -> C; 
rem_champion([Champ | _Vs], Champ, C) -> C#control{champion = []};
rem_champion([_V | Vs], Champ, C) -> rem_champion(Vs, Champ, C).

rem_challenger([], _Chal, C) -> C; 
rem_challenger([Chal | _Vs], Chal, C) -> C#control{challenger = []};
rem_challenger([_V | Vs], Chal, C) -> rem_challenger(Vs, Chal, C).

add_us_and_gps(V, []) -> V;
add_us_and_gps(V, [everyone | Rest]) ->
    V2 = V#view{everyone = true},
    add_us_and_gps(V2, Rest);
add_us_and_gps(V, [{user, User} | Rest]) ->
    V2 = V#view{users = gb_sets:add(User, V#view.users)},
    add_us_and_gps(V2, Rest);
add_us_and_gps(V, [{group, Group} | Rest]) ->
    V2 = V#view{groups = gb_sets:add(Group, V#view.groups)},
    add_us_and_gps(V2, Rest).

%% When called with type 'champion' or 'challenger' the requested type
%% of view is returned providing the necessary credentials are met.
%% For requests of type 'any', the first view which can be satisifed by
%% the user's credentials will be returned.
-spec get_view(#control{}, 
               string(), [string()],
               champion | challenger | any)
              -> {view, string()} | not_found | denied.
get_view(#control{views = ?EMPTY_TREE}, _U, _G, _T) ->
    not_found;
get_view(C, User, Groups, any) ->
    KVs = gb_trees:to_list(C#control.views),
    case get_role_view(KVs, User, Groups) of 
        none -> denied; 
        V -> {view, V}
    end;
get_view(#control{champion = [], challenger = []}, _U, _G, _T) ->
    not_found;
get_view(#control{champion = V}=C, User, Groups, champion) ->
    View = gb_trees:get(V, C#control.views),
    case get_role_view([{V,View}], User, Groups) of
        V -> {view, V}; 
        _ -> denied
    end;
get_view(#control{challenger = V}=C, User, Groups, challenger) when V /= [] ->
    View = gb_trees:get(V, C#control.views),
    case get_role_view([{V,View}], User, Groups) of
        V -> {view, V}; 
        _ -> denied
    end.

-spec get_particular_view(#control{}, 
                          string(), [string()],
                          string())
                         -> {view, string()} | not_found | denied.
get_particular_view(C, User, Groups, V) ->
    %% we don't know this actually exists
    case gb_trees:lookup(V, C#control.views) of
        none -> denied;
        {value, View} -> 
            case get_role_view([{V,View}], User, Groups) of
                none -> denied;
                V -> {view, V}
            end
    end.

-spec get_role_view([{string(), #view{}}], string(), [string()]) 
                   -> none | string().
get_role_view(KVs, User, Groups) ->
    case get_user_view(KVs, User) of
        none -> get_group_view(KVs, Groups);
        V -> V
    end.

get_user_view([], _User) -> none; 
get_user_view([{V, #view{everyone = true}} | _Rest], _User) ->
    V;
get_user_view([{V, #view{users = Users}} | Rest], User) ->
    case gb_sets:is_member(User, Users) of
        true  -> V; 
        false -> get_user_view(Rest, User)
    end. 

get_group_view([], _CheckGroups) -> none;
get_group_view([{V, #view{groups = Groups}} | Rest], CheckGroups) ->
    F = fun(G) -> gb_sets:is_member(G, Groups) end,
    case lists:any(F, CheckGroups) of
        true -> V;
        false -> get_group_view(Rest, CheckGroups)
    end.

tree(Site, Trees) -> 
    case gb_trees:lookup(Site, Trees) of
        none -> gb_trees:empty();
        {value, V} -> V
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tree Manipulations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec load_trees(string(), string()) -> gb_tree().
load_trees(Dir, Table) ->
    {ok, _} = dets:open_file(Table, [{file, filename:join(Dir,Table)}]),
    %% if the value of auth_tree is an empty list,
    %% create an empty tree and fire it in..
    case dets:lookup(Table, ?KEY) of
        []            -> gb_trees:empty();
        [{?KEY, Val}] -> Val
    end.

-spec save_trees(string(), string(), gb_tree(), gb_tree()) -> gb_tree(). 
save_trees(Table, Site, NewTree, Trees) ->
    NewTrees = gb_trees:enter(Site, NewTree, Trees),
    ok = dets:insert(Table, {?KEY, NewTrees}),
    NewTrees.

-spec del_site_tree(string(), string(), gb_tree()) -> gb_tree().
del_site_tree(Table, Site, Trees) ->
    NewTrees = gb_trees:delete_any(Site, Trees),
    ok = dets:insert(Table, {?KEY, NewTrees}),
    NewTrees.
                                            
-spec alter_tree(gb_tree(), 
                 [string()], 
                 fun((#control{}) -> #control{})) -> gb_tree().
alter_tree(Tree, [], Fun) ->
    case gb_trees:lookup(control, Tree) of
        none       -> NewCtrls = Fun(#control{}),
                      gb_trees:insert(control, NewCtrls, Tree);
        {value, V} -> NewVs = Fun(V),
                      gb_trees:enter(control, NewVs, Tree)
    end;
alter_tree(Tree, [H | T], Fun) ->
    ok = force_terminal([H | T]),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = alter_tree(Empty, T, Fun),
                      gb_trees:insert({seg, H}, NewVal, Tree);
        {value, V} -> NewVal = alter_tree(V, T, Fun),
                      gb_trees:enter({seg, H}, NewVal, Tree)
    end.

force_terminal([H | T]) ->
     %% force [**] to be a terminal segment only
     case {H, T} of
         {"[**]", []} -> ok;
         {"[**]", _X} -> exit("non-terminal [**] segment");
         _            -> ok
     end.

-spec run_ctl(gb_tree(), [string()], fun((#control{}) -> X)) -> X.
run_ctl(Tree, Path, Fun) ->
    CtlNorm = seek_ctl_normal(Path, Tree),
    CtlWild = seek_ctl_wild(Path, Tree, none),
    Fun(merge_ctl(CtlNorm, CtlWild)).
                
-spec seek_ctl_normal([string()], gb_tree()) -> none | #control{}. 
seek_ctl_normal([], Tree) ->
    get_control(Tree);
seek_ctl_normal([W="[*]" | Rest], Tree) ->
    case gb_trees:lookup({seg, W}, Tree) of
        none -> none;
        {value, Tree2} -> seek_ctl_normal(Rest, Tree2)
    end;
seek_ctl_normal([H | Rest], Tree) ->
    case gb_trees:lookup({seg, H}, Tree) of
        none -> seek_ctl_normal(["[*]" | Rest], Tree);
        {value, Tree2} -> seek_ctl_normal(Rest, Tree2)
    end.

-spec seek_ctl_wild([string()], gb_tree(), none | #control{}) 
                   -> none | #control{}. 
seek_ctl_wild([], _Tree, AccCtl) ->
    AccCtl;
seek_ctl_wild([H | Rest], Tree, AccCtl) ->
    case gb_trees:lookup({seg, "[**]"}, Tree) of
        none -> 
            case gb_trees:lookup({seg, H}, Tree) of
                none -> AccCtl;
                {value, Tree2} -> seek_ctl_wild(Rest, Tree2, AccCtl)
            end;
        {value, TreeWild} -> 
            AccCtl2 = merge_ctl(get_control(TreeWild), AccCtl),
            case gb_trees:lookup({seg, H}, Tree) of
                none -> AccCtl2; 
                {value, Tree2} -> seek_ctl_wild(Rest, Tree2, AccCtl2)
            end
    end.

%% Left side is favored during merges. When merging normal and wild
%% controls, the normal should be on the left. Futhermore, when
%% munching down the wild tree, deeper controls should be left
%% favored.
-spec merge_ctl(none | #control{}, none | #control{}) -> #control{}. 
merge_ctl(none, none) -> #control{}; 
merge_ctl(C, none) -> C;
merge_ctl(none, C) -> C;
merge_ctl(C1, C2) ->
    Champion = merge_left(C1#control.champion, C2#control.champion),
    Challenger = merge_left(C1#control.challenger, C2#control.challenger),
    Views = merge_left_views(C1#control.views, C2#control.views),
    #control{champion = Champion,
             challenger = Challenger,
             views = Views}.

merge_left_views(ViewsL, ViewsR) ->
    KVs = gb_trees:to_list(ViewsR),
    lists:foldl(fun add_to_views/2, ViewsL, KVs).

add_to_views({V, NewView}, Views) ->
    case gb_trees:lookup(V, Views) of
        none ->
            gb_trees:insert(V, NewView, Views);
        {value, CurView} ->
            Everyone = CurView#view.everyone,
            Users = gb_sets:union(CurView#view.users, NewView#view.users),
            Groups = gb_sets:union(CurView#view.groups, NewView#view.groups),
            Merged = #view{everyone = Everyone,
                           users = Users,
                           groups = Groups},
            gb_trees:enter(V, Merged, Views)
    end.
                
merge_left([], C) -> C; 
merge_left(C, _) -> C.
    
-spec get_control(gb_tree()) -> none | #control{}.
get_control(Tree) ->
    case gb_trees:lookup(control, Tree) of
        none       -> none;
        {value, V} -> V
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pretty Print
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LB(X), list_to_binary(X)).
-spec get_as_json1(gb_tree(), [string()]) -> {struct, any()}.
get_as_json1(Tree, Path) ->
    run_ctl(Tree, Path, fun ctl_to_json/1).

ctl_to_json(C) ->
    ViewIter = gb_trees:iterator(C#control.views),
    Views = {struct, view_to_json(gb_trees:next(ViewIter))},
    {struct, [{champion, ?LB(C#control.champion)},
              {challenger, ?LB(C#control.challenger)},
              {views, Views}]}.

view_to_json(none) -> [];
view_to_json({V, View, Iter}) ->
    Users = [?LB(U) || U <- gb_sets:to_list(View#view.users)],
    Groups = [?LB(G) || G <- gb_sets:to_list(View#view.groups)],
    S = {?LB(V), {struct, [{everyone, View#view.everyone},
                           {users, Users},
                           {groups, Groups}]}},
    [S | view_to_json(gb_trees:next(Iter))].


-spec load_script1(list()) -> gb_tree().
load_script1(Terms) ->
    Tree = gb_trees:empty(),
    exec_script_terms(Terms, Tree).

-define(lget(Key, List), (element(2, lists:keyfind(Key, 1, List)))).
exec_script_terms([], Tree) ->
    Tree;
exec_script_terms([{add_view, C}|Rest], Tree) ->
    Tree2 = add_view1(Tree, ?lget(path, C), ?lget(perms, C), ?lget(view, C)),
    exec_script_terms(Rest, Tree2);
exec_script_terms([{champion, C}|Rest], Tree) ->
    Tree2 = set_default(Tree, ?lget(path, C), ?lget(view, C), champion),
    exec_script_terms(Rest, Tree2);
exec_script_terms([{challenger, C}|Rest], Tree) ->
    Tree2 = set_default(Tree, ?lget(path, C), ?lget(view, C), challenger),
    exec_script_terms(Rest, Tree2).

-spec dump_script(gb_tree()) -> [any()]. 
dump_script1(Tree) ->
    Iter = gb_trees:iterator(Tree),
    List = lists:flatten(dump_tree(gb_trees:next(Iter), [])),
    make_script_terms(List, []).

make_script_terms([], Acc) -> 
    FirstLine = io_lib:format("~s~n",["%%-*-erlang-*-"]),
    lists:flatten([FirstLine | lists:reverse(Acc)]);
make_script_terms([H | T], Acc) ->
    NewAcc = lists:flatten(io_lib:format("~p.~n", [H])),
    make_script_terms(T, [NewAcc | Acc]).

dump_tree(none, _Path) -> [];
dump_tree({{seg, S}, Tree2, Iter}, Path) ->
    SubIter = gb_trees:iterator(Tree2),
    SubTree = dump_tree(gb_trees:next(SubIter), Path ++ [S]),
    [SubTree | dump_tree(gb_trees:next(Iter), Path)];
dump_tree({control, C, Iter}, Path) ->
    CD = dump_control(C, Path),
    [CD | dump_tree(gb_trees:next(Iter), Path)].

dump_control(C, Path) ->
    Views = dump_views(gb_trees:to_list(C#control.views), Path),
    Champion = case C#control.champion of
                   [] -> [];
                   Chmp -> {champion, [{path, Path},
                                       {view, Chmp}]}
               end,
    Challenger = case C#control.challenger of
                     [] -> [];
                     Chal -> {challenger, [{path, Path},
                                           {view, Chal}]}
                 end,
    [Views, Champion, Challenger].

dump_views([], _) -> [];
dump_views([{V, View} | Rest], Path) ->
    Users = [{user, U} || U <- gb_sets:to_list(View#view.users)],
    Groups = [{group, G} || G <- gb_sets:to_list(View#view.groups)],
    Perms = Users ++ Groups ++ case View#view.everyone of
                                   true -> [everyone];
                                   false -> []
                               end,
    AddView = {add_view, [{path, Path},
                          {perms, Perms},
                          {view, V}]},
    [AddView | dump_views(Rest, Path)].


%%%===================================================================
%%% Demo to show off how the permissions are stored...
%%%===================================================================
demo_DEBUG() ->
    P1 = ["hip"],
    P2 = ["hip", "[*]"],
    P3 = ["dont", "stop"],
    Tree1 = add_view1(gb_trees:empty(), P1, [{user, "gordon"}, {group, "admin"}],
                                  "a view"),
    Tree2 = add_view1(Tree1, P1, [{user, "betty"}, {group, "admin"}],
                                  "another view"),
    Tree3 = add_view1(Tree2, P1, [{group, "himbos"}],
                                  "a third view"),
    Tree4 = add_view1(Tree3, P1, [{user, "jamie"}, {user, "annie"}],
                                  "a fourth view"),
    Tree4a = set_default(Tree4, P1, "another view", champion),
    Tree4b = set_default(Tree4a, P1, "a view", challenger),
    Tree5 = add_view1(Tree4b, P2, [{group, "admin"}],
                                  "*"),
    Tree6 = add_view1(Tree5, P2, [{user, "gordon"}, {group, "admin"}],
                                  "_g/blah"),
    Tree7 = add_view1(Tree6, P2, [{user, "gordon"}, {group, "admin"}],
                                  "_u/stevie/bleh"),
    Tree8 = add_view1(Tree7, P2, [{user, "gordon"}, {group, "admin"}],
                                  "whatever"),
    Tree9 = add_view1(Tree8, P3, [{user, "stevie"}],
                                  "a view"),
    Tree10 = add_view1(Tree9, P3, [{user, "dale"}],
                                   "another view"),
    Tree11 = add_view1(Tree10, P3, [{group, "user"}, {group, "admin"}],
                                   "yeah, yeah!"),
    Tree12 = add_view1(Tree11, P3, [{group, "admin"}],
                                   "*"),
    dump_script1(Tree12).

%%%===================================================================
%% %%% EUnit Tests
%% %%%===================================================================
%% %% the root is a special case - check it carefully

%% check the empty path
%% check_get_view (general)
testA1(P) ->
    Ret = check_get_view1(gb_trees:empty(), P, {"gordon", ["Group"]}, champion),
    ?assertEqual(not_found, Ret).

%% add views
testA2(P) ->
    UGs = [{user, "gordon"}, {group, "admin"}],
    Tree1 = add_view1(gb_trees:empty(), P, UGs, "a view"),
    Tree2 = add_view1(Tree1, P, UGs, "another view"),
    Tree3 = set_default(Tree2, P, "a view", champion),
    Ret = check_get_view1(Tree3, P, {"User", ["Fail"]}, champion),
    ?assertEqual(denied, Ret).

%% Users and groups
testA3(P) ->
    UGs = [{user, "gordon"}, {group, "admin"}],
    Tree1 = add_view1(gb_trees:empty(), P, UGs, "a view"),
    Tree2 = add_view1(Tree1, P, UGs, "another view"),
    Tree3 = set_default(Tree2, P, "a view", champion),
    Ret = check_get_view1(Tree3, P, {"gordon", ["Fail"]}, champion),
    ?assertEqual({view, "a view"}, Ret).

%% Test everyone
testA4(P) ->
    UGs = [{user, "gordon"}, {group, "admin"}, everyone],
    Tree1 = add_view1(gb_trees:empty(), P, UGs, "my view"),
    Tree2 = set_default(Tree1, P, "my view", champion),
    Ret = check_particular_view1(Tree2, P, {"nowhereman", []}, "my view"),
    ?assertEqual({view, "my view"}, Ret).

%% Get any view
testA5(P) ->
    UGs = [{user, "gordon"}, {group, "admin"}],
    Tree1 = add_view1(gb_trees:empty(), P, UGs, "my view"),
    Tree2 = set_default(Tree1, P, "my view", champion),
    Ret = check_get_view1(Tree2, P, {"gordon", []}, any),
    ?assertEqual({view, "my view"}, Ret).

testA7(P) ->
    Tree = add_view1(gb_trees:empty(), P, 
                     [{user, "gordon"}, {group, "admin"}],
                     "a view"),
    Tree1 = add_view1(Tree, P, 
                      [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree2 = set_default(Tree1, P, "a view", champion),
    Tree3 = set_default(Tree2, P, "another view", champion),
    Ret = check_get_view1(Tree3, P, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

%% remove views
testA8(P) ->
    Tree1 = add_view1(gb_trees:empty(), P, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P, 
                      [{user, "gordon"}, {group, "bleh"}],
                      "another view"),
    Tree3 = set_default(Tree2, P, "another view", champion),
    Tree4 = remove_views1(Tree3, P, ["another view"]),
    Ret = check_get_view1(Tree4, P, {"gordon", ["Fail"]}, champion),
    %% no permission to see 'champion'... it's gone.
    ?assertEqual(not_found, Ret). 

testA10(P) ->
    Tree = add_view1(gb_trees:empty(), P, 
                     [{user, "gordon"}, {group, "admin"}],
                     "some view"),
    Tree1 = add_view1(Tree, P, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P, 
                      [{user, "gordon"}, {group, "bleh"}],
                      "another view"),
    Tree3 = remove_views1(Tree2, P, ["some view", "a view"]),
    Tree4 = set_default(Tree3, P, "another view", champion),
    Ret = check_get_view1(Tree4, P, {"gordon", ["Fail"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

%% get the challenger
testA12(P) ->
    Tree1 = add_view1(gb_trees:empty(), P, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = set_default(Tree1, P, "a view", challenger),
    Ret = check_get_view1(Tree2, P, {"Fail", ["admin"]}, challenger),
    ?assertEqual({view, "a view"}, Ret).

%% get all views available to a user
testA14(P) ->
    Tree1 = add_view1(gb_trees:empty(), P, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Ret = get_views1(Tree1, P, {"Fail", ["admin"]}),
    ?assertEqual(["a view"], Ret).

%% get all views available to a user
testA15(P) ->
    Tree1 = add_view1(gb_trees:empty(), P, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Ret = get_views1(Tree2, P, {"Fail", ["admin"]}),
    ?assertEqual(["a fourth view", "a view"], lists:sort(Ret)).

%% check a particular view
testA16(P) ->
    Tree1 = add_view1(gb_trees:empty(), P, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Ret = check_particular_view1(Tree4, P, {"Fail", ["admin"]},
                          "a third view"),
    ?assertEqual(Ret, {view, "a third view"}).

testC() ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree1 = add_view1(gb_trees:empty(), P1, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P1, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P1, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P1, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = add_view1(Tree4, P2, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree6 = add_view1(Tree5, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree7 = add_view1(Tree6, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree8 = add_view1(Tree7, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree9 = add_view1(Tree8, P3, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree10 = add_view1(Tree9, P3, [{user, "gordon"}, {group, "admin"}],
                       "another view"),
    Tree11 = add_view1(Tree10, P3, [{user, "gordon"}, {group, "admin"}],
                       "a third view"),
    Tree12 = add_view1(Tree11, P3, [{user, "gordon"}, {group, "admin"}],
                       "a fourth view"),
    Ret = check_particular_view1(Tree12, P2, {"Fail", ["admin"]},
                          "a third view"),
    ?assertEqual({view, "a third view"}, Ret).

%% test wild cards
testD1() ->
    P1 = ["hip", "hop"],
    P2 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = set_default(Tree4, P2, "another view", champion),
    Ret = check_get_view1(Tree5, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

testD2() ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = set_default(Tree4, P2, "another view", champion),
    Ret = check_get_view1(Tree5, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

testD3() ->
    P1 = ["hip", "hop"],
    P2 = ["[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Ret = check_get_view1(Tree4, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual(not_found, Ret).

% specific overrides the general
testD4() ->
    P1 = ["hip", "hop"],
    P2 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = add_view1(Tree4, P1, [{user, "gordon"}, {group, "admin"}],
                      "blurgh"),
    Tree6 = set_default(Tree5, P1, "blurgh", champion),
    Ret = check_get_view1(Tree6, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "blurgh"}, Ret).

testD5() ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = add_view1(Tree4, P1, [{user, "gordon"}, {group, "admin"}],
                      "blurgh"),
    Tree6 = set_default(Tree5, P1, "blurgh", champion),
    Ret = check_get_view1(Tree6, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "blurgh"}, Ret).

testD6() ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    P3 = ["hip", "[**]"],
    P4 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = add_view1(Tree4, P1, [{user, "gordon"}, {group, "admin"}],
                      "blurgh"),
    Tree6 = add_view1(Tree5, P3, [{user, "gordon"}, {group, "admin"}],
                      "boodle"),
    Tree7 = add_view1(Tree6, P4, [{user, "gordon"}, {group, "admin"}],
                      "banjo"),
    Tree8 = set_default(Tree7, P1, "blurgh", champion),
    Ret = check_get_view1(Tree8, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "blurgh"}, Ret).

testD7() ->
    P1 = ["[**]"],
    P2 = ["u", "dale", "sheet"],
    Tree1 = add_view1(gb_trees:empty(), P1, [{user, "dale"}], 
                      "i/love/spreadsheets"),
    Tree2 = add_view1(Tree1, P2, [{user, "dale"}], "other view"),
    Ret = check_particular_view1(Tree2, P2, {"dale", []}, 
                                 "i/love/spreadsheets"),
    ?assertEqual({view, "i/love/spreadsheets"}, Ret).


%% Test multiple wild cards
testD8() ->
    P1 = ["[**]"],
    P2 = ["u", "dale", "sheet", "[**]"],
    P3 = ["u", "dale", "sheet", "new"],
    Tree1 = add_view1(gb_trees:empty(), P1, [{user, "dale"}], "global_stuff"),
    Tree2 = add_view1(Tree1, P2, [{user, "dale"}], "i/love/spreadsheets"),
    Tree3 = set_default(Tree2, P1, "global_stuff", champion),
    Tree4 = set_default(Tree3, P2, "i/love/spreadsheets", champion),
    Ret = get_views1(Tree4, P3, {"dale", []}),
    ?assertEqual(["global_stuff", "i/love/spreadsheets"], lists:sort(Ret)).


%% Test dump / restore
testE1() ->
    Tree = gb_trees:empty(),
    Iter1 = gb_trees:iterator(Tree),
    Terms = lists:flatten(dump_tree(gb_trees:next(Iter1), [])),
    %% now see if round trips work.
    Tree = load_script1(Terms),
    Iter2 = gb_trees:iterator(Tree),
    Terms = lists:flatten(dump_tree(gb_trees:next(Iter2), [])).

%% Test dump / restore, on complex input
testE2() ->
    %% To comapre trees, we have to tweak the insertion order,
    %% or this test can easily fail due to different balancing.
    P1 = ["[**]"],
    P2 = ["hip", "[*]"],
    P3 = ["hip", "hop"],
    Tree1 = add_view1(gb_trees:empty(), P1, 
                      [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "banjo"),
    Tree3 = add_view1(Tree2, P3, [{user, "gordon"}, {group, "admin"}],
                      "bingo"),
    FinalTree = set_default(Tree3, P1, "a view", champion),

    % Test starts here 
    Iter1 = gb_trees:iterator(FinalTree),
    Terms = lists:flatten(dump_tree(gb_trees:next(Iter1), [])),
    %% now see if round trips work.
    FinalTree = load_script1(Terms),
    Iter2 = gb_trees:iterator(FinalTree),
    Terms = lists:flatten(dump_tree(gb_trees:next(Iter2), [])).

unit_test_() -> 
    SeriesA = [fun testA1/1,
               fun testA2/1,
               fun testA3/1,
               fun testA4/1,
               fun testA5/1,
               fun testA7/1,
               fun testA8/1,
               fun testA10/1,
               fun testA12/1,
               fun testA14/1, 
               fun testA15/1,
               fun testA16/1],

    SeriesC = [fun testC/0], 

    SeriesD = [fun testD1/0,
               fun testD2/0,
               fun testD3/0,
               fun testD4/0,
               fun testD5/0,
               fun testD6/0,
               fun testD7/0,
               fun testD8/0 ],

    SeriesE = [fun testE1/0,
               fun testE2/0],

    [{with, [], SeriesA},
     {with, ["some", "longer", "path"], SeriesA},
     SeriesC,
     SeriesD,
     SeriesE
    ].
