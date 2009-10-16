%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       this gen server handles:
%%%            * valid paths
%%%            * security
%%%            * default pages
%%%            * generation of sequential urls
%%%
%%% @end
%%% Created :  7 Oct 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------
-module(auth_srv).

%-behaviour(gen_server).

%-include("auth.hrl").

%% -include_lib("eunit/include/eunit.hrl").

%% %% API
%% -export([start_link/0]).

%% -export([check_get_page/2,
%%          check_get_page/3,
%%          check_get_page_attr/2,
%%          get_views/2,
%%          can_read/2,
%%          can_write/2,
%%          can_execute/2,
%%          add_perm/3,
%%          add_perm/5,
%%          add_views/3,
%%          add_default/3,
%%          remove_perm/3,
%%          remove_views/3,
%%          remove_default/3,
%%          get_groups/0]).

%% -compile(export_all).

%% %% gen_server callbacks
%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%%          terminate/2, code_change/3]).

%% -define(SERVER, ?MODULE). 

%% -record(state, {tree = []}).

%% %%%===================================================================
%% %%% API
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts the server
%% %%
%% %% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% %% @end
%% %%--------------------------------------------------------------------
%% start_link() ->
%%     gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% %%%===================================================================
%% %%% gen_server callbacks
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Initiates the server
%% %%
%% %% @spec init(Args) -> {ok, State} |
%% %%                     {ok, State, Timeout} |
%% %%                     ignore |
%% %%                     {stop, Reason}
%% %% @end
%% %%--------------------------------------------------------------------
%% init([]) ->
%%     Tree = gb_trees:empty(),
%%     {ok, #state{tree = Tree}}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling call messages
%% %%
%% %% @spec handle_call(Request, From, State) ->
%% %%                                   {reply, Reply, State} |
%% %%                                   {reply, Reply, State, Timeout} |
%% %%                                   {noreply, State} |
%% %%                                   {noreply, State, Timeout} |
%% %%                                   {stop, Reason, Reply, State} |
%% %%                                   {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_call(Request, _From, #state{tree = Tr}) ->
%%     NewTr =
%%         case Request of
%%             {check_get_page, AS, P}          -> check_get_page1(Tr, AS, P);
%%             {check_get_page, AS, P, Gui}     -> check_get_page1(Tr, AS, P, Gui);
%%             {check_get_page_attr, AS, P}     -> check_get_page_attr1(Tr, AS, P);
%%             {get_views, AS, P}                -> get_views1(Tr, AS, P);
%%             {can_read, AS, P}                -> can_read1(Tr, AS, P);
%%             {can_write, AS, P}               -> can_write1(Tr, AS, P);
%%             {can_execute, AS, TS}            -> can_execute1(Tr, AS, TS);
%%             {add_perm, AL, Pg, Perm, Df, Gs} -> add_perm1(Tr, AL, Pg, Perm, Df, Gs);
%%             {add_views, AL, Pg, Gs}           -> add_views1(Tr, AL, Pg, Gs);
%%             {add_default, AL, Pg, Df}        -> add_default1(Tr, AL, Pg, Df);
%%             {rem_perm, AL, Pg, Perm}         -> remove_perm1(Tr, AL, Pg, Perm);
%%             {remove_views, AL, Pg, Gs}        -> remove_views1(Tr, AL, Pg, Gs);
%%             {remove_def, AL, Pg, Df}         -> remove_default1(Tr, AL, Pg, Df);
%%             {get_groups}                     -> get_groups1(Tr)
%%     end,
%%     Reply = ok,
%%     {reply, Reply, #state{tree = NewTr}}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling cast messages
%% %%
%% %% @spec handle_cast(Msg, State) -> {noreply, State} |
%% %%                                  {noreply, State, Timeout} |
%% %%                                  {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_cast(_Msg, State) ->
%%     {noreply, State}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling all non call/cast messages
%% %%
%% %% @spec handle_info(Info, State) -> {noreply, State} |
%% %%                                   {noreply, State, Timeout} |
%% %%                                   {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_info(_Info, State) ->
%%     {noreply, State}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% This function is called by a gen_server when it is about to
%% %% terminate. It should be the opposite of Module:init/1 and do any
%% %% necessary cleaning up. When it returns, the gen_server terminates
%% %% with Reason. The return value is ignored.
%% %%
%% %% @spec terminate(Reason, State) -> void()
%% %% @end
%% %%--------------------------------------------------------------------
%% terminate(_Reason, _State) ->
%%     ok.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Convert process state when code is changed
%% %%
%% %% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% %% @end
%% %%--------------------------------------------------------------------
%% code_change(_OldVsn, State, _Extra) ->
%%     {ok, State}.

%% %%%===================================================================
%% %%% Public API
%% %%%===================================================================
%% check_get_page(AuthSpec, Page) -> 
%%     gen_server:call(auth_srv, {check_get_page, AuthSpec, Page}).

%% check_get_page(AuthSpec, Page, Gui) ->
%%     gen_server:call(auth_srv, {check_get_page, AuthSpec, Page, Gui}).

%% check_get_page_attr(AuthSpec, Page) ->
%%     gen_server:call(auth_srv, {check_get_page_attr, AuthSpec, Page}).

%% get_views(AuthSpec, Page) ->
%%     gen_server:call(auth_srv, {get_views, AuthSpec, Page}).

%% can_read(AuthSpec, Page) ->
%%     gen_server:call(auth_srv, {can_read, AuthSpec, Page}).

%% can_write(AuthSpec, Page) ->
%%     gen_server:call(auth_srv, {can_write, AuthSpec, Page}).

%% can_execute(AuthSpec, Trans_signature) ->
%%     gen_server:call(auth_srv, {can_execute, AuthSpec, Trans_signature}).

%% add_perm(AuthList, Page, Perm) ->
%%     gen_server:call(auth_srv, {add_perm, AuthList, Page, Perm, "index", ["index"]}).
    
%% add_perm(AuthList, Page, Perm, Def, Views) ->
%%     gen_server:call(auth_srv, {add_perm, AuthList, Page, Perm, Def, Views}).

%% add_views(AuthList, Page, Views) ->
%%     gen_server:call(auth_srv, {add_views, AuthList, Page, Views}).

%% add_default(AuthList, Page, Gui) ->
%%     gen_server:call(auth_srv, {add_perm, AuthList, Page, Gui}).

%% remove_perm(AuthList, Page, Perm) ->
%%     gen_server:call(auth_srv, {rem_perm, AuthList, Page, Perm}).

%% remove_views(AuthList, Page, Views) ->
%%     gen_server:call(auth_src, {rem_views, AuthList, Page, Views}).

%% remove_default(AuthList, Page, Gui) ->
%%     gen_server:call(auth_src, {rem_default, AuthList, Page, Gui}).

%% get_groups() ->
%%     gen_server:call(auth_srv, {get_groups}).

%% %%%===================================================================
%% %%% Internal functions
%% %%%===================================================================
%% check_get_page1(Tree, {User, Groups}, Page) ->
%%     Fun = fun(X) -> case get_control(X, User, Groups) of
%%                         none -> {return, '403'};
%%                         Ctl  -> #control{perms = P} = Ctl,
%%                                 case has_perm(P, read) of
%%                                     true  -> #control{def_view = G} = Ctl,
%%                                              {html, G}; 
%%                                     false -> {return, '403'}
%%                                 end
%%                     end
%%           end,
%%     check_get(Tree, Page, Fun).

%% check_get_page1(Tree, {User, Groups}, Page, View) -> 
%%     Fun = fun(X) -> case get_control(X, User, Groups) of
%%                         none -> {return, '403'};
%%                         Ctl  -> #control{views = Vws} = Ctl,
%%                                 case contains(View, Vws) of
%%                                     true  -> {html, View}; 
%%                                     false -> {return, '403'}
%%                                 end
%%                     end
%%           end,
%%     check_get(Tree, Page, Fun).

%% check_get_page_attr1(Tree, _AuthSpec, Page) ->
%%     Fun = fun(_X) ->
%%                  exit(erk, not_written_yet)
%%           end,
%%     check_get(Tree, Page, Fun).
    
%% get_views1(Tree, {User, Groups}, Page) ->
%%     Fun = fun(X) -> case get_control(X, User, Groups) of
%%                         none -> {return, '403'};
%%                         Ctl  -> #control{perms = P, views = Gs} = Ctl,
%%                                 case has_perm(P, read) of
%%                                     true  -> Gs; 
%%                                     false -> []
%%                                 end
%%                     end
%%           end,
%%     check_get(Tree, Page, Fun).

%% can_read1(Tree, {User, Groups}, Page) ->
%%     Fun = fun(X) ->
%%                   case get_control(X, User, Groups) of
%%                       none -> false;
%%                       Ctl  -> #control{perms = P} = Ctl,
%%                               has_perm(P, read)
%%                   end
%%           end,
%%     check_get(Tree, Page, Fun).

%% can_write1(Tree, {User, Groups}, Page) -> 
%%     Fun = fun(X) ->
%%                   case get_control(X, User, Groups) of
%%                       none -> false;
%%                       Ctl  -> #control{perms = P} = Ctl,
%%                               has_perm(P, write)
%%                   end
%%           end,
%%     check_get(Tree, Page, Fun).

%% can_execute1(_Tree, _AuthSpec, _Trans_signature) -> {erk, not_written}.

%% add_perm1(Tree, AuthList, Page, Perms, Def, Views) ->
%%     Fun = fun(#control{def_view = D1,  views = Vw1, perms = P1} = _Old,
%%               #control{def_view = D2, views = Vw2, perms = P2} = _New) ->
%%                   P3 = dedup([P1, P2]),
%%                   % push both the old and new defaults onto the list of views
%%                   Vw3 = dedup([[D1], [D2], Vw1, Vw2]),
%%                   #control{def_view = D2, views = Vw3, perms = P3}
%%           end,
%%     add_to_control(Tree, AuthList, Page, Perms, Def, Views, Fun).


%% add_views1(Tree, AuthList, Page, GuiList) ->
%%     Fun = fun(#control{def_view = D1,  views = Vw1, perms = P1} = _Old,
%%               #control{def_view = _D2, views = Vw2, perms = _P2} = _New) ->
%%                   Vw3 = dedup([Vw1, Vw2]),
%%                   #control{def_view = D1, views = Vw3, perms = P1}
%%           end,
%%     add_to_control(Tree, AuthList, Page, [], [], GuiList, Fun).

%% add_default1(Tree, AuthList, Page, Default) ->
%%     Fun = fun(#control{def_view = D1,  views = Vw1, perms = P1} = _Old,
%%               #control{def_view = D2, views = Vw2, perms = _P2} = _New) ->
%%                   Vw3 = dedup([[D1], Vw1, Vw2]),
%%                   #control{def_view = D2, views = Vw3, perms = P1}
%%           end,
%%     add_to_control(Tree, AuthList, Page, [], Default, [], Fun).

%% remove_perm1(Tree, AuthList, Page, Perms) ->
%%     Fun = fun({K, #control{perms = P1} = Ctl}) ->
%%             P2 = lists:subtract(P1, Perms),
%%             case P2 of
%%                 [] -> empty;
%%                 _O -> {K, Ctl#control{perms = P2}}
%%             end
%%           end,
%%     remove_from_control(Tree, AuthList, Page, Fun).    

%% remove_views1(Tree, AuthList, Page, Views) -> 
%%     Fun = fun({K, #control{def_view = D1,  views = Vw1, perms = P1}}) ->
%%                   {NewDef, NewViews} =
%%                       case contains(D1, Views) of
%%                           true  -> NewG = del(Vw1, Views),
%%                                    NewD = new_def(NewG, D1),
%%                                    {NewD, NewG};
%%                           false -> {D1, del(Vw1, Views)}
%%                       end,
%%                   {K, #control{def_view = new_def(NewViews, NewDef), views = NewViews, perms = P1}}
%%           end,
%%     remove_from_control(Tree, AuthList, Page, Fun).

%% remove_default1(Tree, AuthList, Page, Default) ->
%%     Fun = fun({K, #control{def_view = D1,  views = Vw1, perms = P1}}) ->
%%                   case Default of % gotta have at least one gui 'index' if you have a control
%%                       D1 -> {K, #control{def_view = new_def(Vw1, D1),  views = Vw1, perms = P1}};
%%                       _O -> {K, #control{def_view = D1,  views = Vw1, perms = P1}}
%%                   end
%%           end,
%%     remove_from_control(Tree, AuthList, Page, Fun).

%% get_groups1(_Tree) -> {erk, not_written}.

%% %%
%% %% Helper functions
%% %%
%% contains(_Element, [])            -> false;
%% contains(Element, [Element | _T]) -> true;
%% contains(Element, [_H | T])       -> contains(Element, T).

%% del(List1, List2) -> case lists:subtract(List1, List2) of
%%                          [] -> ["index"];
%%                          L  -> L
%%                      end.                              

%% new_def([], _Default)           -> "index";
%% new_def([Default | T], Default) -> new_def(T, Default);
%% new_def([H | _T], _Default)     -> H.

%% remove_from_control(Tree, AuthList, [], Fun) ->
%%     case gb_trees:lookup(controls, Tree) of
%%         none       -> Tree;
%%         {value, V} -> NewCtls = remove(AuthList, V, Fun), 
%%                       gb_trees:enter(controls, NewCtls, Tree)
%%     end;
%% remove_from_control(Tree, AuthList, [H | T], Fun) ->
%%     case gb_trees:lookup(H, Tree) of
%%         none       -> Tree;
%%         {value, V} -> NewVal = remove_from_control(V, AuthList, T, Fun),
%%                       gb_trees:enter(H, NewVal, Tree)
%%     end.

%% add_to_control(Tree, AuthList, [], Perms, Def, Views, Fun) ->
%%     Controls = make_controls(AuthList, Perms, Def, Views),
%%     case gb_trees:lookup(controls, Tree) of
%%         none       -> gb_trees:insert(controls, Controls, Tree);
%%         {value, V} -> NewControls = merge(V, Controls, Fun),
%%                       gb_trees:enter(controls, NewControls, Tree)
%%     end;
%% add_to_control(Tree, AuthList, [H | T], Perms, Def, Views, Fun) ->
%%     case gb_trees:lookup(H, Tree) of
%%         none       -> Empty = gb_trees:empty(),
%%                       NewVal = add_to_control(Empty, AuthList, T, Perms, Def, Views, Fun),
%%                       gb_trees:insert(H, NewVal, Tree);
%%         {value, V} -> NewVal = add_to_control(V, AuthList, T, Perms, Def, Views, Fun),
%%                       gb_trees:enter(H, NewVal, Tree)
%%     end.

%% check_get(Tree, [], Fun) ->
%%     Fun(Tree);
%% check_get(Tree, [H | T], Fun) ->
%%     case gb_trees:lookup(H, Tree) of
%%         none        -> check_programmatic(Tree, T, Fun);
%%         {value , V} -> check_get(V, T, Fun)
%%     end.          

%% check_programmatic(Tree, List, Fun) ->
%%     case gb_trees:lookup("[*]", Tree) of
%%         none        -> case gb_trees:lookup("[**]", Tree) of
%%                            none        -> {return, '404'};
%%                            {value, V1} -> Fun(V1)
%%                        end;
%%         {value, V2} -> check_get(V2, List, Fun)
%%     end.

%% make_controls(AuthList, Perms, Def, Views) ->
%%     make_c1(AuthList, Perms, Def, Views, []).

%% make_c1([], _Perms, _Def, _Views, Acc)   -> Acc;
%% make_c1([H | T], Perms, Def, Views, Acc) -> Ctl = #control{perms = Perms,
%%                                                           def_view = Def,
%%                                                           views = Views},
%%                                            make_c1(T, Perms, Def, Views, [{H, Ctl} | Acc]).

%% get_control(Tree, User, Groups) ->
%%     case gb_trees:lookup(controls, Tree) of
%%         none          -> none;
%%         {value, Ctls} ->
%%             case lists:keyfind({user, User}, 1, Ctls) of
%%                 false     -> get_control2(Groups, Ctls);
%%                 {_P, Ctl} -> Ctl
%%             end
%%     end.

%% get_control2([], _Controls)     -> none;
%% get_control2([H | T], Controls) ->
%%     case lists:keyfind({group, H}, 1, Controls) of
%%         false     -> get_control2(T, Controls);
%%         {_P, Ctl} -> Ctl
%%     end.

%% has_perm([], _Perm)         -> false;
%% has_perm([Perm | _T], Perm) -> true;
%% has_perm([_H | T], Perm)    -> has_perm(T, Perm).

%% remove([], V, _Fun)     -> V;
%% remove([H | T], V, Fun) ->
%%     case lists:keyfind(H, 1, V) of
%%         false -> remove(T, V, Fun);
%%         Ctl   -> case Fun(Ctl) of
%%                      empty    -> lists:keydelete(H, 1, V);
%%                      NewTuple -> lists:keyreplace(H, 1, V, NewTuple)
%%                  end
%%     end.            

%% merge(List1, List2, Fun) -> merge1(List1, List2, Fun, []).

%% merge1([], List, _Fun, Acc)          -> lists:merge(lists:sort(List), lists:sort(Acc));
%% merge1([{K, V} | T], List, Fun, Acc) ->
%%     case lists:keyfind(K, 1, List) of
%%         false   -> merge1(T, List, Fun, [{K, V} | Acc]);
%%         {K, V2} -> List2 = lists:keydelete(K, 1, List),
%%                    NewCtl = Fun(V, V2),
%%                    merge1(T, List2, Fun, [{K, NewCtl} | Acc])
%%     end.
        
%% dedup(List) -> dedup1(lists:merge([lists:sort(X) || X <- List]), []).

%% dedup1([], Acc)         -> lists:reverse(Acc);
%% dedup1([A, A | T], Acc) -> dedup1([A | T], Acc);
%% dedup1([H | T], Acc)    -> dedup1(T, [H | Acc]).
                             
%% %%%===================================================================
%% %%% EUnit Tests
%% %%%===================================================================

%% %% Basic tests - set 1 permission and check against it...

%% test1() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test1a() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"User", "Fail"}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test1b() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"Fail", "Fail"}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '403'}).

%% test2() ->
%%     P = ["a", "b", "c", "d"],
%%     P2 = ["fail"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '404'}).

%% test3() ->
%%     P = ["a", "b", "c", "d"],
%%     P2 = ["a", "b"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '403'}).

%% test4() ->
%%     P = ["a", "b", "c", "d"],
%%     P2 = ["a", "b", "c", "e"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '404'}).

%% test5() ->
%%     P = ["a", "b", "c", "d"],
%%     P2 = ["a", "b", "c", "d", "e"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '404'}).

%% %% Now set 2 permissions and check against them

%% test6() ->
%%     P1a = ["a", "b", "c", "d"],
%%     P1b = ["1", "2", "3", "4"],
%%     Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
%%                       [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
%%                       [read, write], "index", ["index"]),
%%     P2 = ["a", "b", "c", "d", "e"],
%%     Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '404'}).

%% test7() ->
%%     P1a = ["a", "b", "c"],
%%     P1b = ["a"],
%%     Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
%%                       [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
%%                       [read, write], "special", ["index", "special"]),
%%     P2 = ["a", "b", "c", "d", "e"],
%%     Ret1 = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
%%     Ret2 = check_get_page1(Tree2, {"gordon", ["Group"]}, P1a),
%%     Ret3 = check_get_page1(Tree2, {"User", ["Group"]}, P1b),
%%     io:format("Tree1 is ~p~nTree2 is ~p~n", [Tree1, Tree2]),
%%     io:format("Ret1 is ~p~nRet2 is ~p~nRet3 is ~p~n", [Ret1, Ret2, Ret3]),
%%     ({Ret1, Ret2, Ret3} == {{return, '404'}, {html, "index"}, {html, "special"}}).

%% test8() ->
%%     P1a = ["a", "b", "c", "d"],
%%     P1b = ["a", "b", "c", "d", "x"],
%%     Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
%%                       [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
%%                       [read, write], "index", ["index"]),
%%     P2 = ["a", "b", "c", "d", "e"],
%%     Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '404'}).

%% test9() ->
%%     P1a = ["a", "b", "c", "d"],
%%     P1b = ["a", "b", "c", "d", "x"],
%%     Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
%%                       [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
%%                       [read, write], "index", ["index"]),
%%     P2 = ["a", "b", "c", "d"],
%%     Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test10() ->
%%     P1a = ["a", "b", "c", "d"],
%%     P1b = ["a", "b", "c", "d", "x"],
%%     Tree1 = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P1a,
%%                       [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree1, [{user, "User"}, {group, "Group"}], P1b,
%%                       [read, write], "index", ["index"]),
%%     P2 = ["a", "b", "c", "d", "x"],
%%     Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P2),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% %% set a permission then fail with a different user/group

%% test11() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "New"}, {group, "Group"}], P,
%%                       [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree2, {"gordon", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test12() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "New"}, {group, "Group"}], P,
%%                       [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree2, {"bob", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test13() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "New"}, {group, "Group"}], P,
%%                       [read, write], "index", ["index"]),
%%     Ret = check_get_page1(Tree2, {"bob", ["GroupFail"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '403'}).

%% %% check can_read

%% test14() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
%%                       [read, write], "index", ["index"]),
%%     Ret = can_read1(Tree2, {"bob", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == true).

%% test15() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
%%                       [read, write], "index", ["index"]),
%%     Ret = can_read1(Tree2, {"bob", ["GroupFail"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == false).

%% test16() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
%%                       [write], "index", ["index"]),
%%     Ret = can_read1(Tree2, {"bob", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == false).

%% %% check can_write

%% test17() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
%%                       [read, write], "index", ["index"]),
%%     Ret = can_write1(Tree2, {"bob", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == true).

%% test18() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read, write], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
%%                       [read, write], "index", ["index"]),
%%     Ret = can_write1(Tree2, {"bob", ["GroupFail"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == false).

%% test19() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "gordon"}, {group, "Group"}], P,
%%                       [read], "index", ["index"]),
%%     Ret = can_write1(Tree2, {"bob", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == false).

%% %% Now remove permissions
%% test20() ->
%%     P = ["a"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "gordon"}], P, [read], "index", ["index"]),
%%     Tree3 = remove_perm1(Tree2, [{user, "gordon"}], P, [read]),
%%     io:format("Tree is ~p~nTree2 is ~p~nTree3 is ~p~n", [Tree, Tree2, Tree3]),
%%     (Tree == Tree3).

%% test21() ->
%%     P = ["a"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}, {group, "Group"}], P,
%%                      [read], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "User"}], P, [write], "index", ["index"]),
%%     Tree3 = remove_perm1(Tree2, [{user, "User"}], P, [write]),
%%     io:format("Tree is ~p~nTree3 is ~p~n", [Tree, Tree3]),
%%     (Tree == Tree3).

%% %% add permissions, defaults and views
%% test22() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read, {trans, 123}, {trans, 456}], "default", ["default"]),
%%     Ret = check_get_page1(Tree, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "default"}).

%% test23() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
%%     Ret = check_get_page1(Tree, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "default"}).

%% test24() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
%%     Tree2 = add_perm1(Tree, [{group, "Group"}], P,
%%                       [read, {trans, xxx}, {trans, yyy}], "supervisor",
%%                       ["index", "default", "supervisor"]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "default"}).

%% test25() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "Old"}], P,
%%                      [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
%%     Tree2 = add_perm1(Tree, [{group, "Group"}], P,
%%                       [read, {trans, xxx}, {trans, yyy}], "supervisor",
%%                       ["index", "default", "supervisor"]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "supervisor"}).

%% test26() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "Old"}], P,
%%                      [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
%%     Tree2 = add_perm1(Tree, [{group, "Group"}], P,
%%                       [read, {trans, xxx}, {trans, yyy}], "supervisor",
%%                       ["index", "default", "supervisor"]),
%%     Tree3 = add_perm1(Tree2, [{group, "Subordinate"}], P,
%%                       [read, {trans, ab12}, {trans, bc23}], "subordinate",
%%                       ["index", "default", "subordinate"]),
%%     Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "supervisor"}).

%% test27() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read, {trans, 123}, {trans, 456}], "default", ["index", "default"]),
%%     Tree2 = add_perm1(Tree, [{user, "User"}], P,
%%                       [read, {trans, xxx}, {trans, yyy}], "supervisor",
%%                       ["index", "default", "supervisor"]),
%%     Tree3 = add_perm1(Tree2, [{user, "User"}], P,
%%                       [read, {trans, ab12}, {trans, bc23}], "subordinate",
%%                       ["index", "default", "subordinate"]),
%%     Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "subordinate"}).

%% test28() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "default", ["index", "default"]),
%%     Tree2 = add_perm1(Tree, [{user, "User"}], P,
%%                       [read], "supervisor",
%%                       ["supervisor"]),
%%     Tree3 = add_perm1(Tree2, [{user, "User"}], P,
%%                       [read], "subordinate",
%%                       []),
%%     Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "subordinate"}).

%% %% add views alone
%% test29() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "default", ["index", "default"]),
%%     Tree2 = add_views1(Tree, [{user, "User"}], P,
%%                       ["supervisor"]),
%%     io:format("Tree2 is ~p~n", [Tree2]),
%%     Ret = get_views1(Tree2, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == ["default", "index", "supervisor"]).

%% %% add a default or two
%% test30() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "default", ["index", "default"]),
%%     Tree2 = add_default1(Tree, [{user, "User"}], P, "supervisor"),
%%     io:format("Tree2 is ~p~n", [Tree2]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "supervisor"}).

%% test31() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "default", ["index", "default"]),
%%     Tree2 = add_default1(Tree, [{group, "Group"}], P, "supervisor"),
%%     Tree3 = add_default1(Tree2, [{user, "User"}], P, "blah-blah"),
%%     io:format("Tree3 is ~p~n", [Tree3]),
%%     Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "blah-blah"}).

%% %% remove a gui
%% test32() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "default", ["index", "default"]),
%%     io:format("Tree is ~p~n", [Tree]),
%%     Tree2 = add_perm1(Tree, [{user, "Bob"}], P,
%%                      [read], "default2", ["index", "default2"]),
%%     io:format("Tree2 is ~p~n", [Tree2]),
%%     Tree3 = remove_views1(Tree2, [{user, "User"}, {group, "Group"}], P, ["default"]),
%%     io:format("Tree3 is ~p~n", [Tree3]),
%%     Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test33() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "default", ["index", "default", "bob"]),
%%     Tree2 = add_perm1(Tree, [{user, "Bob"}], P,
%%                      [read], "default2", ["index", "default2"]),
%%     Tree3 = remove_views1(Tree2, [{user, "User"}, {group, "Group"}], P,
%%                          ["default", "index", "bob", "jim"]),
%%     io:format("Tree3 is ~p~n", [Tree3]),
%%     Ret = check_get_page1(Tree3, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% %% remove a default
%% test34() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "default", ["index", "default"]),
%%     Tree2 = remove_default1(Tree,  [{user, "User"}, {group, "Group"}], P, "default"),
%%     io:format("Tree2 is ~p~n", [Tree2]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test35() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "index", ["index", "default"]),
%%     Tree2 = remove_default1(Tree, [{user, "User"}, {group, "Group"}], P, "index"),
%%     io:format("Tree2 is ~p~n", [Tree2]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "default"}).

%% test36() ->
%%     P = ["a", "b", "c", "d"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P,
%%                      [read], "index", ["index"]),
%%     Tree2 = remove_default1(Tree, [{user, "User"}, {group, "Group"}], P, "index"),
%%     io:format("Tree2 is ~p~n", [Tree2]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% %% now check if a page is acceptable
%% test37() ->
%%     P1 = ["a", "b"],
%%     P2 = ["a"],
%%     P3 = ["does", "not", "exist"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
%%                      [read], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "User2"}], P2,
%%                       [read], "default", ["index", "default", "bingo"]),
%%     % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
%%     Tree3 = add_perm1(Tree2, [{user, "User"}], P1,
%%                       [read], "index", ["index", "default", "special"]),
%%     Ret1 = check_get_page1(Tree3, {"User", ["RandomGroup"]}, P1, "special"),
%%     % io:format("Tree is ~p~nTree2 is ~p~nTree3 is ~p~n",
%%     %           [Tree, Tree2, Tree3]),
%%     Ret2 = check_get_page1(Tree3, {"User", ["Group"]}, P1, "epic fail"),
%%     Ret3 = check_get_page1(Tree3, {"User", ["Group"]}, P3),
%%     io:format("Ret1 is ~p~nRet2 is ~p~nRet3 is ~p~n", [Ret1, Ret2, Ret3]),
%%     ({Ret1, Ret2, Ret3} == {{html, "special"}, {return, '403'}, {return, '404'}}).

%% %% now do the old wild card stuff
%% test38() ->
%%     P1 = ["a", "[*]", "c"],
%%     P2 = ["a", "b", "c"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
%%                      [read], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"User", ["Group"]}, P2),
%%     % io:format("Tree is ~p~n", [Tree]),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% test39() ->
%%     P1 = ["a", "[**]"],
%%     P2 = ["a", "b", "x", "y"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
%%                      [read], "index", ["index"]),
%%     Ret = check_get_page1(Tree, {"User", ["Group"]}, P2),
%%     % io:format("Tree is ~p~n", [Tree]),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "index"}).

%% %% specific (ie [*]) overrides the general (ie [**])
%% test40() ->
%%     P1 = ["a", "[**]"],
%%     P2 = ["a", "[*]", "x"],
%%     P3 = ["a", "b", "x"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
%%                      [read], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "User"}], P2,
%%                      [read], "special", ["index", "special"]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P3),
%%     % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {html, "special"}).

%% test41() ->
%%     P1 = ["a", "[**]"],
%%     P2 = ["a", "[*]", "x"],
%%     P3 = ["a", "b", "x", "y"],
%%     Tree = add_perm1(gb_trees:empty(), [{user, "User"}], P1,
%%                      [read], "index", ["index"]),
%%     Tree2 = add_perm1(Tree, [{user, "User"}], P2,
%%                      [read], "special", ["index", "special"]),
%%     Ret = check_get_page1(Tree2, {"User", ["Group"]}, P3),
%%     % io:format("Tree is ~p~nTree2 is ~p~n", [Tree, Tree2]),
%%     io:format("Ret is ~p~n", [Ret]),
%%     (Ret == {return, '404'}).

%% unit_test_() -> 
%%     [
%%      ?_assert(test1()),
%%      ?_assert(test1a()),
%%      ?_assert(test1b()),
%%      ?_assert(test2()),
%%      ?_assert(test3()),
%%      ?_assert(test4()),
%%      ?_assert(test5()),
%%      ?_assert(test6()),
%%      ?_assert(test7()),
%%      ?_assert(test8()),
%%      ?_assert(test9()),
%%      ?_assert(test10()),
%%      ?_assert(test11()),
%%      ?_assert(test12()),
%%      ?_assert(test13()),
%%      ?_assert(test14()),
%%      ?_assert(test15()),
%%      ?_assert(test16()),
%%      ?_assert(test17()),
%%      ?_assert(test18()),
%%      ?_assert(test19()),
%%      ?_assert(test20()),
%%      ?_assert(test21()),
%%      ?_assert(test22()),
%%      ?_assert(test23()),
%%      ?_assert(test24()),
%%      ?_assert(test25()),
%%      ?_assert(test26()),
%%      ?_assert(test27()),
%%      ?_assert(test28()),
%%      ?_assert(test29()),
%%      ?_assert(test30()),
%%      ?_assert(test31()),
%%      ?_assert(test32()),
%%      ?_assert(test33()),
%%      ?_assert(test34()),  
%%      ?_assert(test35()),
%%      ?_assert(test36()),
%%      ?_assert(test37()),
%%      ?_assert(test38()),
%%      ?_assert(test39()),
%%      ?_assert(test40()),
%%      ?_assert(test41())
%%   ].
