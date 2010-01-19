%%% @copyright (C) 2009, Hypernumbers Ltd
%% TODO: Add in support for changing perms on the fly.
%% Add, Replace, etc. Will be motivated by admin panel design.

-module(auth_srv2).

-behaviour(gen_server).

-include("auth2.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).

%% API 
-export([start_link/0]).

-export([
         check_get_view/3,
         check_particular_view/4,
         check_get_challenger/3,
         get_views/3,
         add_view/4,
         set_champion/3,
         set_challenger/3,
         remove_views/4
         %% get_as_json/2,
         %% dump_script/1
        ]).

-export([
         clear_all_perms_DEBUG/1,
         demo_DEBUG/0
         %% permissions_DEBUG/3,
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, "auth_srv2").
-define(KEY, "auth_tree").
-define(INDEX, "hypernumbers/index").
-define(SPREADSHEET, "_global/spreadsheet").
-define(EMPTY_TREE, {0,nil}).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec check_get_view(string(), [string()], auth_req()) -> string(). 
check_get_view(Site, Path, AuthReq) -> 
    gen_server:call(?MODULE, {check_get_view, Site, Path, AuthReq}).

-spec check_get_challenger(string(), [string()], auth_req()) -> string(). 
check_get_challenger(Site, Path, AuthReq) -> 
    gen_server:call(?MODULE, {check_get_challenger, Site, Path, AuthReq}).

-spec check_particular_view(string(), [string()], auth_req(), string())
                           -> string(). 
check_particular_view(Site, Path, AuthReq, View) ->
    gen_server:call(?MODULE, {check_particular_view, Site, Path,
                              AuthReq, View}).

-spec get_views(string(), [string()], auth_req()) -> [string()]. 
get_views(Site, Path, AuthReq) ->
    gen_server:call(?MODULE, {get_views, Site, Path, AuthReq}).

-spec add_view(string(), [string()], auth_spec(), string()) -> string(). 
add_view(Site, Path, AuthSpec, View) ->
    gen_server:call(?MODULE, {add_view, Site, Path, AuthSpec, View}).

-spec set_champion(string(), [string()], string()) -> ok. 
set_champion(Site, Path, View) ->
    gen_server:call(?MODULE, {set_champion, Site, Path, View}).

-spec set_challenger(string(), [string()], string()) -> ok. 
set_challenger(Site, Path, View) ->
    gen_server:call(?MODULE, {set_challenger, Site, Path, View}).

-spec remove_views(string(), [string()], auth_spec(), [string()]) -> ok. 
remove_views(Site, Path, AuthSpec, Views) ->
    gen_server:call(?MODULE, {rem_views, Site, Path, AuthSpec, Views}).

%% get_as_json(Site, Path) ->
%%     gen_server:call(?MODULE, {get_as_json, Site, Path}).

%% pretty_print(Site, Path, Type) ->
%%     gen_server:call(?MODULE, {pretty_print, Site, Path, Type}).

%% dump_script(Site) ->
%%     gen_server:call(?MODULE, {dump_script, Site}).

clear_all_perms_DEBUG(Site) ->
    gen_server:call(?MODULE, {clear_all_perms, Site}).

%% permissions_DEBUG(Site, AuthSpec, Path) -> 
%%     gen_server:call(?MODULE, {permissions_debug, Site, AuthSpec, Path}).

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
            {check_get_view, Site, P, AR} ->
                {Site, check_get_view1(tree(Site, Tr), P, AR, champion), false};
            {check_get_challenger, Site, P, AR} ->
                {Site, check_get_view1(tree(Site, Tr), P, AR, challenger),
                 false};
            {check_particular_view, Site, P, AR, V} ->
                {Site, check_particular_view1(tree(Site, Tr), P, AR, V), false};
            {get_views, Site, P, AR} ->
                {Site, get_views1(tree(Site, Tr), P, AR), false};
            {add_view, Site, Pg, AS, V} ->
                {Site, add_view1(tree(Site, Tr), Pg, AS, V), true};
            {set_champion, Site, Pg, Df} ->
                {Site, set_default(tree(Site, Tr), Pg, Df, champion), true};
            {set_challenger, Site, Pg, Df} ->
                {Site, set_default(tree(Site, Tr), Pg, Df, challenger), true};
            {rem_views, Site, Pg, Vs} ->
                {Site, remove_views1(tree(Site, Tr), Pg, Vs), true};
            {clear_all_perms, Site} ->
                {Site, gb_trees:empty(), true}
            %% {get_as_json, Site, Pg} ->
            %%     {Site, get_as_json1(tree(Site, Tr), Pg), false};
            %% {pretty_print, Site, Pg, Type} ->
            %%     {Site, pretty_print1(tree(Site, Tr), Site, Pg, Type), false};
            %% {dump_script, Site} ->
            %%     {Site, dump_script1(tree(Site, Tr)), false};
                %% {permissions_debug, Site, AR, P} ->
            %%     {Site, permissions_debug1(tree(Site, Tr), AR, P), false}
            end,
    {Reply, NewTr} =
        case Return1 of
            {Site2, Return2, true} ->
                {ok, save_trees(File, Site2, Return2, Tr)};
            {_Site2, Return2, false} ->
                {Return2, Tr}
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

%% Set default allows to change a champion or challenger.  The new
%% candidate MUST already exist, unless we're nullifying a
%% champion/challenger
set_default(Tree, Path, [], Type) ->
    Fun = fun(C) -> set_default2(Type, [], C) end,
    alter_tree(Tree, Path, Fun);
set_default(Tree, Path, V, Type) ->
    Fun = fun(C=#control{views = Views}) ->  
                  case gb_trees:lookup(V, Views) of
                      none -> C; 
                      _ -> set_default2(Type, V, C)
                  end
          end,
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
              -> {html, string()} | {return, integer()}.
get_view(#control{views = ?EMPTY_TREE}, _U, _G, _T) ->
    {return, 404};
get_view(#control{champion = [], challenger = []}, _U, _G, _T) ->
    {return, 404};
get_view(#control{champion = V}=C, User, Groups, champion) ->
    View = gb_trees:get(V, C#control.views),
    case get_role_view([{V,View}], User, Groups) of
        V -> {html, V}; 
        _ -> {return, 401}
    end;
get_view(#control{challenger = V}=C, User, Groups, challenger) when V /= [] ->
    View = gb_trees:get(V, C#control.views),
    case get_role_view([{V,View}], User, Groups) of
        V -> {html, V}; 
        _ -> {return, 401}
    end;
get_view(C, User, Groups, any) ->
    KVs = gb_trees:to_list(C#control.views),
    case get_role_view(KVs, User, Groups) of 
        none -> {return, 401}; 
        V -> {html, V}
    end.

-spec get_particular_view(#control{}, 
                          string(), [string()],
                          string())
                         -> {html, string()} | {return, integer()}.
get_particular_view(C, User, Groups, V) ->
    %% we don't know this actually exists
    case gb_trees:lookup(V, C#control.views) of
        none -> {return, 401};
        {value, View} -> 
            case get_role_view([{V,View}], User, Groups) of
                none -> {return, 401};
                V -> {html, V}
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
save_trees(File, Site, NewTree, Trees) ->
    NewTrees = gb_trees:enter(Site, NewTree, Trees),
    ok = dets:insert(File, {?KEY, NewTrees}),
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
run_ctl(Tree, [], Fun) ->
    Fun(get_control(Tree));
run_ctl(Tree, [W="[*]" | T], Fun) ->
    case gb_trees:lookup({seg, W}, Tree) of
        none -> run_ctl(Tree, ["[**]" | T], Fun);
        {value, V} -> run_ctl(V, T, Fun)
    end;
run_ctl(Tree, [W="[**]" | _T], Fun) ->
    case gb_trees:lookup({seg, W}, Tree) of
        none -> Fun(get_control(leaf));
        {value, V} -> Fun(get_control(V))
    end;
run_ctl(Tree, [H | T], Fun) ->
    case gb_trees:lookup({seg, H}, Tree) of
        none -> run_ctl(Tree, ["[*]" | T], Fun);
        {value, V} -> run_ctl(V, T, Fun)
    end.

-spec get_control(leaf | gb_tree()) -> #control{}.
get_control(leaf) -> #control{};
get_control(Tree) ->
    case gb_trees:lookup(control, Tree) of
        none       -> #control{};
        {value, V} -> V
    end.

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
    %%PP = pretty_print1(Tree12, "test", [], text),
    io:format("~p~n", [Tree12]).

%%%===================================================================
%% %%% EUnit Tests
%% %%%===================================================================
%% %% the root is a special case - check it carefully

%% check the empty path
%% check_get_view (general)
testA1(P) ->
    Ret = check_get_view1(gb_trees:empty(), P, {"gordon", ["Group"]}, champion),
    ?assertEqual({return, 404}, Ret).

%% add views
testA2(P) ->
    UGs = [{user, "gordon"}, {group, "admin"}],
    Tree1 = add_view1(gb_trees:empty(), P, UGs, "a view"),
    Tree2 = add_view1(Tree1, P, UGs, "another view"),
    Tree3 = set_default(Tree2, P, "a view", champion),
    Ret = check_get_view1(Tree3, P, {"User", ["Fail"]}, champion),
    ?assertEqual({return, 401}, Ret).

%% Users and groups
testA3(P) ->
    UGs = [{user, "gordon"}, {group, "admin"}],
    Tree1 = add_view1(gb_trees:empty(), P, UGs, "a view"),
    Tree2 = add_view1(Tree1, P, UGs, "another view"),
    Tree3 = set_default(Tree2, P, "a view", champion),
    Ret = check_get_view1(Tree3, P, {"gordon", ["Fail"]}, champion),
    ?assertEqual({html, "a view"}, Ret).

%% Test everyone
testA4(P) ->
    UGs = [{user, "gordon"}, {group, "admin"}, everyone],
    Tree1 = add_view1(gb_trees:empty(), P, UGs, "my view"),
    Tree2 = set_default(Tree1, P, "my view", champion),
    Ret = check_particular_view1(Tree2, P, {"nowhereman", []}, "my view"),
    ?assertEqual({html, "my view"}, Ret).

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
    ?assertEqual({html, "another view"}, Ret).

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
    ?assertEqual({return, 404}, Ret). 

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
    ?assertEqual({html, "another view"}, Ret).

%% get the challenger
testA12(P) ->
    Tree1 = add_view1(gb_trees:empty(), P, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = set_default(Tree1, P, "a view", challenger),
    Ret = check_get_view1(Tree2, P, {"Fail", ["admin"]}, challenger),
    ?assertEqual({html, "a view"}, Ret).

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
    Tree1 = add_view1(gb_trees:empty(), P, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Ret = check_particular_view1(Tree4, P, {"Fail", ["admin"]},
                          "a third view"),
    ?assertEqual(Ret, {html, "a third view"}).

testC() ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree1 = add_view1(gb_trees:empty(), P1, [{user, "gordon"}, {group, "admin"}],
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
    ?assertEqual({html, "a third view"}, Ret).

%% test wild cards
testD1() ->
    P1 = ["hip", "hop"],
    P2 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = set_default(Tree4, P2, "another view", champion),
    Ret = check_get_view1(Tree5, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({html, "another view"}, Ret).

testD2() ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Tree5 = set_default(Tree4, P2, "another view", champion),
    Ret = check_get_view1(Tree5, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({html, "another view"}, Ret).

testD3() ->
    P1 = ["hip", "hop"],
    P2 = ["[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, [{user, "gordon"}, {group, "admin"}],
                      "a view"),
    Tree2 = add_view1(Tree1, P2, [{user, "gordon"}, {group, "admin"}],
                      "another view"),
    Tree3 = add_view1(Tree2, P2, [{user, "gordon"}, {group, "admin"}],
                      "a third view"),
    Tree4 = add_view1(Tree3, P2, [{user, "gordon"}, {group, "admin"}],
                      "a fourth view"),
    Ret = check_get_view1(Tree4, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({return, 404}, Ret).

% specific overrides the general
testD4() ->
    P1 = ["hip", "hop"],
    P2 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, [{user, "gordon"}, {group, "admin"}],
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
    ?assertEqual({html, "blurgh"}, Ret).

testD5() ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, [{user, "gordon"}, {group, "admin"}],
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
    ?assertEqual({html, "blurgh"}, Ret).

testD6() ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    P3 = ["hip", "[**]"],
    P4 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, [{user, "gordon"}, {group, "admin"}],
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
    ?assertEqual(Ret, {html, "blurgh"}).

testE1() ->
    P = [],
    Tree = add_view1(gb_trees:empty(), P, [{user, "gordon"}, {group, "admin"}],
                     "*"),
    Tree1 = add_view1(Tree, P, [{user, "gordon"}, {group, "admin"}],
                      "blah"),
    Tree2 = add_view1(Tree1, P, [{user, "gordon"}, {group, "admin"}],
                      "blerg"),
    Ret = get_views1(Tree2, P, {"Fail", ["admin"]}),
    ?assertEqual(["blerg", "blah", "*"], Ret).

testE2() ->
    P = [],
    Tree = add_view1(gb_trees:empty(), P, [{user, "gordon"}, {group, "admin"}],
                     "*"),
    Tree1 = add_view1(Tree, P, [{user, "gordon"}, {group, "admin"}],
                      "blah"),
    Tree2 = add_view1(Tree1, P, [{user, "gordon"}, {group, "admin"}],
                      "blerg"),
    Ret = check_particular_view1(Tree2, P, {"Fail", ["admin"]},
                          "random chops, tonto"),
    ?assertEqual({html, "random chops, tonto"}, Ret).


unit_test_() -> 
    SeriesA = [fun testA1/1,
               fun testA2/1,
               fun testA3/1,
               fun testA4/1,
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
               fun testD6/0 ],

    _SeriesE = [fun testE1/0,
               fun testE2/0],

    [{with, [], SeriesA},
     {with, ["some", "longer", "path"], SeriesA},
     SeriesC,
     SeriesD
     %%SeriesE
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pretty Print
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% dump_s1(Tree, Path, Acc) ->
%%     List = gb_trees:to_list(Tree),
%%     dump_s2(List, Path, Acc).

%% dump_s2([], _Path, Acc)                 -> lists:flatten(lists:reverse(Acc));
%% dump_s2([{control, C} | T], Path, Acc) -> NewAcc = write_control(Path, C),
%%                                            dump_s2(T, Path, [NewAcc | Acc]);
%% dump_s2([H | T], Path, Acc)             ->
%%     NewAcc = dump_s3(H, Path, Acc),
%%     dump_s2(T, Path, lists:flatten([NewAcc | Acc])).

%% dump_s3([], _Path, Acc)               -> Acc;
%% dump_s3({{seg, Path1}, H}, Path, Acc) -> dump_s1(H, [Path1 | Path], Acc).

%% write_control(Path, C) -> write_c(Path, C, []).

%% write_c(_Path, [], Acc)     -> lists:reverse(Acc);
%% write_c(Path, [H | T], Acc) ->
%%     #control{view = V, default = D, users = U, groups = G} = H,
%%     NewAcc1 = {add_view,
%%                [{path, lists:reverse(Path)},
%%                 {view, [V]}]},
%%     NewAcc2 = {add_users_and_groups,
%%                [{path, lists:reverse(Path)},
%%                 {view, V},
%%                 {users, U},
%%                 {groups, G}]},
%%     case D of
%%         false    -> write_c(Path, T, [NewAcc2, NewAcc1 | Acc]);
%%         champion -> NewAcc3 = {set_champion,
%%                                [{path, lists:reverse(Path)},
%%                                 {view, V}]},
%%                     write_c(Path, T, [NewAcc3, NewAcc2, NewAcc1 | Acc]);
%%         challenger -> NewAcc3 = {set_challenger,
%%                                  [{path, lists:reverse(Path)},
%%                                   {view, V}]},
%%                       write_c(Path, T, [NewAcc3, NewAcc2, NewAcc1 | Acc])
%%     end.


%% make_prettyprint(Tree, Site, html) ->
%%     Body = make_pp(Tree, html, [], "", []),
%%     "<html><head><title>Permissions Tree</title></head><body><code>"
%%         "<bold>Permissions for " ++ Site ++
%%         "</bold><br />" ++
%%         Body ++ "</code></body></html>";
%% make_prettyprint(Tree, Site, text) ->
%%     "Permissions for " ++ Site ++ "~n" ++
%%         make_pp(Tree, text, [], "", []).

%% make_pp(Tree, Type, Seg, Prefix, Acc) ->
%%     LineEnd = case Type of
%%                   html -> "<br />";
%%                   text -> "~n"
%%               end,
%%     List = gb_trees:to_list(Tree),
%%     {Control, Paths} = split(List),
%%     Seg2 = case Seg of
%%                [] -> "/";
%%                _  -> "/" ++ Seg ++ "/"
%%            end,
%%     NewPrefix = case {length(Paths), Type} of
%%                     {0, text} -> lists:append(Prefix, "  ");
%%                     {1, text} -> lists:append(Prefix, "  ");
%%                     {_, text} -> lists:append(Prefix, " |");
%%                     {0, html} -> lists:append(Prefix, "&nbsp;&nbsp;");
%%                     {1, html} -> lists:append(Prefix, "&nbsp;&nbsp;");
%%                     {_, html} -> lists:append(Prefix, "&nbsp;|")
%%                 end,
%%     C = pp(Control, Prefix, Type, []),
%%     make_pp2(Paths, Type, NewPrefix,
%%              [C, LineEnd, Seg2, "-> ", Prefix, LineEnd, Prefix | Acc]).

%% make_pp2([], text, _Prefix, Acc) -> lists:flatten(lists:reverse(Acc));
%% make_pp2([], html, _Prefix, Acc) -> lists:flatten(lists:reverse(Acc));
%% make_pp2([{{seg, K}, V} | T], Type, Prefix, Acc) ->
%%     NewAcc = make_pp(V, Type, K, Prefix, []),
%%     make_pp2(T, Type, Prefix, [NewAcc | Acc]).

%% pp([], Prefix, html, [])       -> Prefix ++ "&nbsp;&nbsp;&nbsp;(no control)"
%%                                       ++ "<br />";
%% pp([], Prefix, text, [])       -> Prefix ++ "   (no control)" ++ "~n";
%% pp([], _Prefix, _Type, Acc)    -> lists:reverse(Acc);
%% pp([{control, H} | T], Prefix, Type, Acc) ->
%%     pp(T, Prefix, Type, [pp_c(H, Prefix, Type, [])| Acc]).


%% pp_c([], _Prefix, _Type, Acc)    -> Acc;
%% pp_c([H | T], Prefix, html, Acc) ->
%%     #control{view = V, default = D, users = U, groups = G} = H,
%%     NewAcc =
%%         case D of
%%             false ->
%%                 Prefix ++ "&nbsp;&nbsp;&nbsp;view: " ++ V ++ "<br />"
%%                     ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;users&nbsp; : "
%%                     ++ make_list(U, []) ++ "<br />"
%%                     ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;groups : "
%%                     ++ make_list(G, []) ++ "<br />";
%%             _ ->
%%                 Prefix ++ "&nbsp;&nbsp;&nbsp;view: " ++ V ++ " ("
%%                     ++ make_string(D) ++ ")<br />"
%%                     ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;users&nbsp; : "
%%                     ++ make_list(U, []) ++ "<br />"
%%                     ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;groups : "
%%                     ++ make_list(G, []) ++ "<br />"
%%         end,
%%     pp_c(T, Prefix, html, [NewAcc | Acc]);
%% pp_c([H | T], Prefix, text, Acc) ->
%%     #control{view = V, default = D, users = U, groups = G} = H,
%%     NewAcc =
%%         case D of
%%             false ->
%%                 Prefix ++ "   view: " ++ V ++ "~n"
%%                     ++ Prefix ++ "    users  : " ++ make_list(U, []) ++ "~n"
%%                     ++ Prefix ++ "    groups : " ++ make_list(G, []) ++ "~n";
%%             _ ->
%%                 Prefix ++ "   view: " ++ V ++ " ("++ make_string(D) ++ ")~n"
%%                     ++ Prefix ++ "    users  : " ++ make_list(U, []) ++ "~n"
%%                     ++ Prefix ++ "    groups : " ++ make_list(G, []) ++ "~n"
%%         end,
%%     pp_c(T, Prefix, text, [NewAcc | Acc]).

%% make_string(champion)          -> "champion/default view";
%% make_string(challenger)        -> "challenger";
%% make_string(X) when is_list(X) -> X.

%% make_list([], Acc)      -> string:strip(string:strip(Acc, right), right, $,);
%% make_list([H | T], Acc) -> make_list(T, H ++ ", " ++ Acc).

%% pp_l([], [])                       -> ""; % blank list is just blank!
%% pp_l([], [_H | Acc])               -> lists:flatten(lists:reverse(Acc));
%% pp_l([H | T], Acc) when is_atom(H) -> pp_l(T, [", ", atom_to_list(H) | Acc]);
%% pp_l([H | T], Acc) when is_list(H) -> pp_l(T, [", ", H | Acc]).

%% split(L) -> sp1(L, [], []).

%% sp1([], Control, Paths)      -> {lists:sort(Control), lists:sort(Paths)};
%% sp1([H | T], Control, Paths) ->
%%     case element(1, H) of
%%         control  -> sp1(T, [H | Control], Paths);
%%         {seg, _S} -> sp1(T, Control, [H | Paths])
%%     end.

%% make_json(Tree) ->
%%     List = gb_trees:to_list(Tree),
%%     {array, [make_json1(K, V) || {K, V}  <- List]}.

%% make_json1({seg, Seg}, V) ->
%%     {struct, [{{path, Seg}, make_json(V)}]};
%% make_json1(control, Ctrls) ->
%%     {struct, [{control, {array, make_json2(Ctrls, [])}}]}.

%% make_json2([], Acc)      -> Acc;
%% make_json2([H | T], Acc) ->
%%     #control{view = V, default = D, users = U, groups = G} = H,
%%     NewAcc = {struct, [{view, V},
%%                        {default, D},
%%                        {users, {array, U}},
%%                        {groups, {array, G}}]},
%%     make_json2(T, [NewAcc | Acc]).

%% get_for_pp(Tree, [], Fun) ->
%%     Fun(Tree);
%% get_for_pp(Tree, [H | T], Fun) ->
%%     case gb_trees:lookup(H, Tree) of
%%         none        -> check_programmatic(Tree, T, Fun);
%%         {value , V} -> get_for_pp(V, T, Fun)
%%     end.

%% dump_script1(Tree) -> dump_s1(Tree, [], []).

%% permissions_debug1(Tree, {_U, _Gs}, Path) ->
%%     Fun =
%%         fun(_X) ->
%%                 ok
%%         end,
%%     run_ctl(Tree, Path, Fun).

%% get_as_json1(Tree, Path) ->
%%     Fun = fun(X) ->
%%                   make_json(X)
%%           end,
%%     get_for_pp(Tree, Path, Fun).

%% pretty_print1(Tree, Site, Path, Type) ->
%%     Fun = fun(X) ->
%%                   make_prettyprint(X, Site, Type)
%%           end,
%%     get_for_pp(Tree, Path, Fun).

