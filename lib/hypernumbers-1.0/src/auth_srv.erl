%%% @copyright (C) 2009, Hypernumbers Ltd.

-module(auth_srv).

-behaviour(gen_server).

-include("auth.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API 
-export([start_link/1, stop/1]).

-export([
         check_get_view/3,
         check_particular_view/4,
         check_get_challenger/3,
         get_views/3,
         add_view/4,
         set_view/4,
         set_champion/3,
         set_challenger/3,
         remove_views/3,
         get_as_json/2,
         dump_script/1,
         load_script/2
        ]).

-export([
         clear_all_perms_DEBUG/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SPACE, 32).
-define(KEY, "auth_tree").
-define(SPREADSHEET, "_global/spreadsheet").
-define(EMPTY_TREE, {0,nil}).

-record(control, {champion = [] :: string(),
                  challenger = [] :: string(),
                  views = gb_trees:empty() }).

-record(view, {everyone = false :: true | false,
               groups = gb_sets:empty() :: gb_set() }).

-record(state, {site :: string(),
                table :: string(),
                tree :: gb_tree() }).

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

stop(Site) ->
    Id = hn_util:site_to_atom(Site, "auth"),
    gen_server:cast(Id, stop).

-spec check_get_view(string(), [string()], uid()) 
                    -> {view, string()} | not_found | denied.
check_get_view(Site, Path, Uid) -> 
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {check_get_view, Path, Uid}).

-spec check_get_challenger(string(), [string()], uid()) 
                          -> {view, string()} | not_found | denied.
check_get_challenger(Site, Path, Uid) -> 
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {check_get_challenger, Path, Uid}).

-spec check_particular_view(string(), [string()], uid(), string())
                           -> {view, string()} | not_found | denied.
check_particular_view(Site, Path, Uid, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {check_particular_view, Path, Uid, View}).

-spec get_views(string(), [string()], uid()) -> [string()]. 
get_views(Site, Path, Uid) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {get_views, Path, Uid}).

-spec add_view(string(), [string()], auth_spec(), string()) -> ok.
add_view(Site, Path, AuthSpec, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {add_view, Path, AuthSpec, View}).

%% A non-additive way of setting views.
-spec set_view(string(), [string()], auth_spec(), string()) -> ok.
set_view(Site, Path, AuthSpec, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {set_view, Path, AuthSpec, View}).    

-spec set_champion(string(), [string()], string()) -> ok. 
set_champion(Site, Path, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {set_champion, Path, View}).

-spec set_challenger(string(), [string()], string()) -> ok. 
set_challenger(Site, Path, View) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {set_challenger, Path, View}).

-spec remove_views(string(), [string()], [string()]) -> ok. 
remove_views(Site, Path, Views) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {rem_views, Path, Views}).

-spec get_as_json(string(), [string()]) -> any(). 
get_as_json(Site, Path) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, {get_as_json, Path}).

-spec clear_all_perms_DEBUG(string()) -> ok. 
clear_all_perms_DEBUG(Site) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, clear_all_perms).

-spec dump_script(string()) -> iodata().
dump_script(Site) ->
    Id = hn_util:site_to_atom(Site, "_auth"),
    gen_server:call(Id, dump_script).

-spec load_script(string(), [any()]) -> ok. 
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
    Dir = filename:join([code:lib_dir(hypernumbers), "..", "..",
                         "var", "dets"]),
    Tree = load_tree(Dir, Table),
    {ok, #state{site = Site,
                table = Table,
                tree = Tree}}.

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
handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(Request, _From, State) ->
    #state{tree = Tr, table = Table, site = Site} = State,
    Return1 = case Request of
                  {check_get_view, P, U} ->
                      {check_get_view1(Site, Tr, P, U, champion), false};
                  {check_get_challenger, P, U} ->
                      {check_get_view1(Site, Tr, P, U, challenger),
                       false};
                  {check_particular_view, P, U, V} ->
                      {check_particular_view1(Site, Tr, P, U, V), false};
                  {get_views, P, U} ->
                      {get_views1(Site, Tr, P, U), false};
                  {add_view, Pg, AS, V} ->
                      {add_view1(Tr, Pg, AS, V), true};
                  {set_view, Pg, AS, V} ->
                      {set_view1(Tr, Pg, AS, V), true};
                  {set_champion, Pg, Df} ->
                      {set_default(Tr, Pg, Df, champion), true};
                  {set_challenger, Pg, Df} ->
                      {set_default(Tr, Pg, Df, challenger), true};
                  {rem_views, P, Vs} ->
                      {remove_views1(Tr, P, Vs), true};
                  {get_as_json, P} ->
                      {get_as_json1(Tr, P), false};
                  clear_all_perms ->
                      {gb_trees:empty(), true};
                  dump_script ->
                      {dump_script1(Tr), false};
                  {load_script, Terms} ->
                      {load_script1(Terms), true}
              end,
    case Return1 of
        {NewTree, true} ->
            ok = save_tree(Table, NewTree),
            {reply, ok, State#state{tree = NewTree}};
        {Reply, _} ->
            {reply, Reply, State}
    end.

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
terminate(_Reason, #state{table = Tbl}) ->
    dets:close(Tbl).

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

check_get_view1(Site, Tree, Path, Uid, Type) ->
    Fun = fun(C) -> get_view(C, Site, Uid, Type) end,
    run_ctl(Tree, Path, Fun).

check_particular_view1(Site, Tree, Path, Uid, Type) ->
    Fun = fun(C) -> get_particular_view(C, Site, Uid, Type) end,
    run_ctl(Tree, Path, Fun).

get_views1(Site, Tree, Path, Uid) ->
    Fun = fun(#control{views = Views}) ->
                  KVs = gb_trees:to_list(Views),
                  [K || {K,V} <- KVs, can_view(Site, Uid, V)]
          end,
    run_ctl(Tree, Path, Fun).

add_view1(Tree, Path, AuthSpec, V) ->
    Fun = fun(C) ->
                  CurViews = C#control.views,
                  View1 = case gb_trees:lookup(V, CurViews) of
                             none -> #view{};
                             {value, Val} -> Val end,
                  View2 = apply_authspec(View1, AuthSpec),
                  NewViews = gb_trees:enter(V, View2, CurViews),
                  C#control{views = NewViews}
          end,
    alter_tree(Tree, Path, Fun).

set_view1(Tree, Path, AuthSpec, V) ->
    Fun = fun(C) ->
                  CurViews = C#control.views,
                  View = apply_authspec(#view{}, AuthSpec),
                  NewViews = gb_trees:enter(V, View, CurViews),
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
                  C#control{views = Views2}
          end,
    alter_tree(Tree, Path, Fun).

-spec apply_authspec(#view{}, auth_spec()) -> #view{}. 
apply_authspec(V, []) -> V;
apply_authspec(V, [everyone | Rest]) ->
    V2 = V#view{everyone = true},
    apply_authspec(V2, Rest);
apply_authspec(V, [Group | Rest]) ->
    V2 = V#view{groups = gb_sets:add(Group, V#view.groups)},
    apply_authspec(V2, Rest).

%% When called with type 'champion' or 'challenger' the requested type
%% of view is returned providing the necessary credentials are met.
%% For requests of type 'any', the first view which can be satisifed by
%% the user's credentials will be returned.
-spec get_view(#control{}, string(), uid(), champion | challenger)
              -> {view, string()} | not_found | denied.
get_view(#control{views = ?EMPTY_TREE}, _S, _U, _T) ->
    not_found;
get_view(#control{champion = [], challenger = []}, _S, _U, _T) ->
    not_found;
get_view(#control{champion = V}=C, Site, Uid, champion) ->
    View = gb_trees:get(V, C#control.views),
    case can_view(Site, Uid, View) of
        true -> {view, V};
        false -> denied
    end;
get_view(#control{challenger = V}=C, Site, Uid, challenger) when V /= [] ->
    View = gb_trees:get(V, C#control.views),
    case can_view(Site, Uid, View) of
        true -> {view, V};
        false -> denied
    end.

-spec get_particular_view(#control{}, string(), uid(), string())
                         -> {view, string()} | not_found | denied.
get_particular_view(C, Site, Uid, V) ->
    case gb_trees:lookup(V, C#control.views) of
        none -> not_found;
        {value, View} -> 
            case can_view(Site, Uid, View) of
                true -> {view, V};
                false -> denied
            end
    end.

-spec can_view(string(), uid(), #view{}) -> boolean().
can_view(_Site, _Uid, #view{everyone = true}) -> 
    true; 
can_view(Site, Uid, #view{groups = Groups}) -> 
    hn_groups:is_member(Uid, Site, Groups).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tree Manipulations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec load_tree(string(), string()) -> gb_tree().
load_tree(Dir, Table) ->
    filelib:ensure_dir([Dir,"/"]),
    {ok, _} = dets:open_file(Table, [{file, filename:join(Dir,Table)}]),
    %% if the value of auth_tree is an empty list,
    %% create an empty tree and fire it in..
    case dets:lookup(Table, ?KEY) of
        []            -> gb_trees:empty();
        [{?KEY, Val}] -> Val
    end.

%% Todo: This really shouldn't be Dets.
-spec save_tree(string(), gb_tree()) -> ok.
save_tree(Table, NewTree) ->
    ok = dets:insert(Table, {?KEY, NewTree}).

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
            Groups = gb_sets:union(CurView#view.groups, NewView#view.groups),
            Merged = #view{everyone = Everyone,
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
    {struct, [{"champion", C#control.champion},
              {"challenger", {array, C#control.challenger}},
              {"views", Views}]}.

view_to_json(none) -> [];
view_to_json({V, View, Iter}) ->
    Groups = [G || G <- gb_sets:to_list(View#view.groups)],
    S = {V, {struct, [{"everyone", View#view.everyone},
                           {"groups", {array, Groups}}]}},
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
exec_script_terms([{set_champion, C}|Rest], Tree) ->
    Tree2 = set_default(Tree, ?lget(path, C), ?lget(view, C), champion),
    exec_script_terms(Rest, Tree2);
exec_script_terms([{set_challenger, C}|Rest], Tree) ->
    Tree2 = set_default(Tree, ?lget(path, C), ?lget(view, C), challenger),
    exec_script_terms(Rest, Tree2).

-spec dump_script1(gb_tree()) -> [any()].
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
                   Chmp -> {set_champion, [{path, Path},
                                           {view, Chmp}]}
               end,
    Challenger = case C#control.challenger of
                     [] -> [];
                     Chal -> {set_challenger, [{path, Path},
                                               {view, Chal}]}
                 end,
    [Views, Champion, Challenger].

dump_views([], _) -> [];
dump_views([{V, View} | Rest], Path) ->
    Groups = [{group, G} || G <- gb_sets:to_list(View#view.groups)],
    Perms = Groups ++ case View#view.everyone of
                          true -> [everyone];
                          false -> []
                      end,
    AddView = {add_view, [{path, Path},
                          {perms, Perms},
                          {view, V}]},
    [AddView | dump_views(Rest, Path)].

%%%===================================================================
%% %%% EUnit Tests
%% %%%===================================================================
%% %% the root is a special case - check it carefully

%% check the empty path
%% check_get_view (general)
testA1({S, P}) ->
    Ret = check_get_view1(S, gb_trees:empty(), P, {"gordon", ["Group"]}, champion),
    ?assertEqual(not_found, Ret).

%% add views
testA2({S, P}) ->
    AuthSpec = ["admin"],
    Tree1 = add_view1(gb_trees:empty(), P, AuthSpec, "a view"),
    Tree2 = add_view1(Tree1, P, AuthSpec, "another view"),
    Tree3 = set_default(Tree2, P, "a view", champion),
    Ret = check_get_view1(S, Tree3, P, {"User", ["Fail"]}, champion),
    ?assertEqual(denied, Ret).

%% Users and groups
testA3({S, P}) ->
    AuthSpec = ["admin"],
    Tree1 = add_view1(gb_trees:empty(), P, AuthSpec, "a view"),
    Tree2 = add_view1(Tree1, P, AuthSpec, "another view"),
    Tree3 = set_default(Tree2, P, "a view", champion),
    Ret = check_get_view1(S, Tree3, P, {"gordon", ["Fail"]}, champion),
    ?assertEqual({view, "a view"}, Ret).

%% Test everyone
testA4({S, P}) ->
    AuthSpec = ["admin", everyone],
    Tree1 = add_view1(gb_trees:empty(), P, AuthSpec, "my view"),
    Tree2 = set_default(Tree1, P, "my view", champion),
    Ret = check_particular_view1(S, Tree2, P, {"nowhereman", []}, "my view"),
    ?assertEqual({view, "my view"}, Ret).

%% Get any view
testA5({S, P}) ->
    AuthSpec = ["admin"],
    Tree1 = add_view1(gb_trees:empty(), P, AuthSpec, "my view"),
    Tree2 = set_default(Tree1, P, "my view", champion),
    Ret = check_get_view1(S, Tree2, P, {"gordon", []}, any),
    ?assertEqual({view, "my view"}, Ret).

testA7({S, P}) ->
    Tree = add_view1(gb_trees:empty(), P, ["admin"], "a view"),
    Tree1 = add_view1(Tree, P, ["admin"], "another view"),
    Tree2 = set_default(Tree1, P, "a view", champion),
    Tree3 = set_default(Tree2, P, "another view", champion),
    Ret = check_get_view1(S, Tree3, P, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

%% remove views
testA8({S, P}) ->
    Tree1 = add_view1(gb_trees:empty(), P, 
                      ["admin"],
                      "a view"),
    Tree2 = add_view1(Tree1, P, 
                      [{user, "gordon"}, {group, "bleh"}],
                      "another view"),
    Tree3 = set_default(Tree2, P, "another view", champion),
    Tree4 = remove_views1(Tree3, P, ["another view"]),
    Ret = check_get_view1(S, Tree4, P, {"gordon", ["Fail"]}, champion),
    %% no permission to see 'champion'... it's gone.
    ?assertEqual(not_found, Ret). 

%% remove views by replacing
testA9({S, P}) ->
    Tree1 = add_view1(gb_trees:empty(), P, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P, 
                      [{user, "gordon"}, {group, "bleh"}],
                      "another view"),
    Tree3 = set_view1(Tree2, P,
                      [{user, "usurper"}, {group, "god"}],
                      "another view"),
    Tree4 = set_default(Tree3, P, "another view", champion),
    Ret1 = check_get_view1(S, Tree4, P, {"gordon", ["Fail"]}, champion),
    Ret2 = check_get_view1(S, Tree4, P, {"usurper", ["Success"]}, champion),
    ?assertEqual(denied, Ret1),
    ?assertEqual({view, "another view"}, Ret2). 


testA10({S, P}) ->
    Tree = add_view1(gb_trees:empty(), P, ["admin"], "some view"),
    Tree1 = add_view1(Tree, P, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P, [{user, "gordon"}, {group, "bleh"}], 
                      "another view"),
    Tree3 = remove_views1(Tree2, P, ["some view", "a view"]),
    Tree4 = set_default(Tree3, P, "another view", champion),
    Ret = check_get_view1(S, Tree4, P, {"gordon", ["Fail"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

%% get the challenger
testA12({S, P}) ->
    Tree1 = add_view1(gb_trees:empty(), P, ["admin"],
                      "a view"),
    Tree2 = set_default(Tree1, P, "a view", challenger),
    Ret = check_get_view1(S, Tree2, P, {"Fail", ["admin"]}, challenger),
    ?assertEqual({view, "a view"}, Ret).

%% get all views available to a user
testA14({S, P}) ->
    Tree1 = add_view1(gb_trees:empty(), P, ["admin"], "a view"),
    Ret = get_views1(S, Tree1, P, {"Fail", ["admin"]}),
    ?assertEqual(["a view"], Ret).

%% get all views available to a user
testA15({S, P}) ->
    Tree1 = add_view1(gb_trees:empty(), P, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P, ["admin"], "a fourth view"),
    Ret = get_views1(S, Tree2, P, {"Fail", ["admin"]}),
    ?assertEqual(["a fourth view", "a view"], lists:sort(Ret)).

%% check a particular view
testA16({S, P}) ->
    Tree1 = add_view1(gb_trees:empty(), P, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P, ["admin"], "a fourth view"),
    Ret = check_particular_view1(S, Tree4, P, {"Fail", ["admin"]},
                          "a third view"),
    ?assertEqual(Ret, {view, "a third view"}).

testC(S) ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree1 = add_view1(gb_trees:empty(), P1, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P1, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P1, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P1, ["admin"], "a fourth view"),
    Tree5 = add_view1(Tree4, P2, ["admin"], "a view"),
    Tree6 = add_view1(Tree5, P2, ["admin"], "another view"),
    Tree7 = add_view1(Tree6, P2, ["admin"], "a third view"),
    Tree8 = add_view1(Tree7, P2, ["admin"], "a fourth view"),
    Tree9 = add_view1(Tree8, P3, ["admin"], "a view"),
    Tree10 = add_view1(Tree9, P3, ["admin"], "another view"),
    Tree11 = add_view1(Tree10, P3, ["admin"], "a third view"),
    Tree12 = add_view1(Tree11, P3, ["admin"], "a fourth view"),
    Ret = check_particular_view1(S, Tree12, P2, {"Fail", ["admin"]},
                          "a third view"),
    ?assertEqual({view, "a third view"}, Ret).

%% test wild cards
testD1(S) ->
    P1 = ["hip", "hop"],
    P2 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P2, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P2, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree5 = set_default(Tree4, P2, "another view", champion),
    Ret = check_get_view1(S, Tree5, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

testD2(S) ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P2, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P2, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree5 = set_default(Tree4, P2, "another view", champion),
    Ret = check_get_view1(S, Tree5, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "another view"}, Ret).

testD3(S) ->
    P1 = ["hip", "hop"],
    P2 = ["[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P2, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P2, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Ret = check_get_view1(S, Tree4, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual(not_found, Ret).

% specific overrides the general
testD4(S) ->
    P1 = ["hip", "hop"],
    P2 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P2, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P2, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree5 = add_view1(Tree4, P1, ["admin"], "blurgh"),
    Tree6 = set_default(Tree5, P1, "blurgh", champion),
    Ret = check_get_view1(S, Tree6, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "blurgh"}, Ret).

testD5(S) ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    Tree1 = add_view1(gb_trees:empty(), P2, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P2, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P2, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree5 = add_view1(Tree4, P1, ["admin"], "blurgh"),
    Tree6 = set_default(Tree5, P1, "blurgh", champion),
    Ret = check_get_view1(S, Tree6, P1, {"Fail", ["admin"]}, champion),
    ?assertEqual({view, "blurgh"}, Ret).

testD6(S) ->
    P1 = ["hip", "hop"],
    P2 = ["hip", "[*]"],
    P3 = ["hip", "[**]"],
    P4 = ["[**]"],
    Tree1 = add_view1(gb_trees:empty(), P2, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P2, ["admin"], "another view"),
    Tree3 = add_view1(Tree2, P2, ["admin"], "a third view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree4 = add_view1(Tree3, P2, ["admin"], "a fourth view"),
    Tree5 = add_view1(Tree4, P1, ["admin"], "blurgh"),
    Tree6 = add_view1(Tree5, P3, ["admin"], "boodle"),
    Tree7 = add_view1(Tree6, P4, ["admin"], "banjo"),
    Tree8 = set_default(Tree7, P1, "blurgh", champion),
    Ret = check_get_view1(S, Tree8, P1, ["admin"], champion),
    ?assertEqual({view, "blurgh"}, Ret).

testD7(S) ->
    P1 = ["[**]"],
    P2 = ["u", "dale", "sheet"],
    Tree1 = add_view1(gb_trees:empty(), P1, [{user, "dale"}], 
                      "i/love/spreadsheets"),
    Tree2 = add_view1(Tree1, P2, [{user, "dale"}], "other view"),
    Ret = check_particular_view1(S, Tree2, P2, {"dale", []}, 
                                 "i/love/spreadsheets"),
    ?assertEqual({view, "i/love/spreadsheets"}, Ret).


%% Test multiple wild cards
testD8(S) ->
    P1 = ["[**]"],
    P2 = ["u", "dale", "sheet", "[**]"],
    P3 = ["u", "dale", "sheet", "new"],
    Tree1 = add_view1(gb_trees:empty(), P1, [{user, "dale"}], "global_stuff"),
    Tree2 = add_view1(Tree1, P2, [{user, "dale"}], "i/love/spreadsheets"),
    Tree3 = set_default(Tree2, P1, "global_stuff", champion),
    Tree4 = set_default(Tree3, P2, "i/love/spreadsheets", champion),
    Ret = get_views1(S, Tree4, P3, {"dale", []}),
    ?assertEqual(["global_stuff", "i/love/spreadsheets"], lists:sort(Ret)).


%% Test dump / restore
testE1(_S) ->
    Tree = gb_trees:empty(),
    Iter1 = gb_trees:iterator(Tree),
    Terms = lists:flatten(dump_tree(gb_trees:next(Iter1), [])),
    %% now see if round trips work.
    Tree = load_script1(Terms),
    Iter2 = gb_trees:iterator(Tree),
    Terms = lists:flatten(dump_tree(gb_trees:next(Iter2), [])).

%% Test dump / restore, on complex input
testE2(_S) ->
    %% To comapre trees, we have to tweak the insertion order,
    %% or this test can easily fail due to different balancing.
    P1 = ["[**]"],
    P2 = ["hip", "[*]"],
    P3 = ["hip", "hop"],
    Tree1 = add_view1(gb_trees:empty(), P1, ["admin"], "a view"),
    Tree2 = add_view1(Tree1, P2, ["admin"], "banjo"),
    Tree3 = add_view1(Tree2, P3, ["admin"], "bingo"),
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
               fun testA9/1,
               fun testA10/1,
               fun testA12/1,
               fun testA14/1, 
               fun testA15/1,
               fun testA16/1],

    SeriesC = [fun testC/1], 

    SeriesD = [fun testD1/1,
               fun testD2/1,
               fun testD3/1,
               fun testD4/1,
               fun testD5/1,
               fun testD6/1,
               fun testD7/1,
               fun testD8/1 ],

    SeriesE = [fun testE1/1,
               fun testE2/1],

    [{with, {"testsite", []}, SeriesA},
     {with, {"testsite", ["some", "longer", "path"]}, SeriesA},
     {with, "testsite", SeriesC},
     {with, "testsite", SeriesD},
     SeriesE
    ].
