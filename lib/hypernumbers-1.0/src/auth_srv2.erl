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
         check_get_page/3,        % tests
         check_particular_page/4, % tests
         check_get_challenger/3,  % tests
         get_views/3,             % tests
         add_views/3,             % tests
         add_users_and_groups/4,  % tests
         set_champion/3,          % tests
         set_challenger/3,        % tests
         remove_views/4,          % tests
         remove_champion/2,       % tests
         remove_challenger/2,     % tests
         get_as_json/2,           % tests
         pretty_print/3,          % tests
         dump_script/1            %
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
                {Host, check_get_page1(get(Host, Tr), AS, P, champion), false};
            {check_get_challenger, Host, AS, P} ->
                {Host, check_get_page1(get(Host, Tr), AS, P, challenger),
                 false};
            {check_particular_page, Host, AS, P, Gui} ->
                {Host, check_particular_page1(get(Host, Tr), AS, P, Gui),
                 false};
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

check_get_challenger(Host, AuthSpec, Page) -> 
    gen_server:call(auth_srv, {check_get_challenger, Host, AuthSpec, Page}).

check_particular_page(Host, AuthSpec, Page, Gui) ->
    gen_server:call(auth_srv, {check_particular_page, Host, AuthSpec,
                               Page, Gui}).

get_views(Host, AuthSpec, Page) ->
    gen_server:call(auth_srv, {get_views, Host, AuthSpec, Page}).

add_views(Host, Page, Views) ->
    gen_server:call(auth_srv, {add_views, Host, Page, Views}).

add_users_and_groups(Host, Page, Views, AuthList) ->
    gen_server:call(auth_srv, {add_users_and_groups, Host, Page,
                               Views, AuthList}).

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

check_get_page1(Tree, {User, Groups}, Page, Type)
  when Type == champion orelse Type == challenger->
    % first see if the user has permission to see the page
    % then see what page they should be getting
     Fun =
         fun(X) ->
                 get_page(X, User, Groups, Type)
         end,
    check_get(Tree, Page, Fun).

check_particular_page1(Tree, {User, Groups}, Page, View) ->
    % first see if the user has permission to see the page
    % then see what view they should be getting
    Fun = fun(X) ->
                  case lists:keymember(View, 2, X) of
                      false -> {return, '404'};
                      true  -> case can_read(lists:keyfind(View, 2, X),
                                             User, Groups) of
                                   false -> {return, '503'};
                                   true  -> {html, View}
                               end
                  end
          end,
    check_get(Tree, Page, Fun).

get_views1(Tree, {User, Groups}, Page) ->
    Fun = fun(X) ->
                  collect_views(X, User, Groups)
          end,
    check_get(Tree, Page, Fun).


add_views1(Tree, Path, Views) ->
    Fun = fun(Vs, X) ->
                  case Vs of
                      [] -> make_controls(X);
                      _  -> lists:merge(lists:sort(make_controls(X)),
                                           lists:sort(Vs))
                  end
          end,
    add_to_tree(Tree, Path, Views, Fun).

add_users_and_groups1(Tree, Path, View, UAndGs)   ->
    Fun = fun(Views, {V, UGs}) ->
                  Vs = case Views of
                           [] -> [#control{view = View}];
                           _  -> Views
                       end,
                     add_us_and_gps(Vs, V, UGs)
             end,
    add_to_tree(Tree, Path, {View, UAndGs}, Fun).
              
set_default1(Tree, Path, Default, Type)   ->
    Fun = fun(Views, {Def, Ty}) ->
                  Vs = case Views of
                           [] -> [#control{view = Def}];
                           _  -> Views
                       end,
                  set_def(Vs, Def, Ty)
          end,
    add_to_tree(Tree, Path, {Default, Type}, Fun).
                          
remove_views1(Tree, [], Views) ->
    case gb_trees:lookup(controls, Tree) of
        none       -> Tree;
        {value, V} -> NewCtls = remove_views(V, Views),
                      gb_trees:enter(controls, NewCtls, Tree)
    end;                          
remove_views1(Tree, [H | T], Views) -> 
    % ok = force_terminal([H | T], "remove_views"),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Tree;
        {value, V} -> NewViews = remove_views1(V, T, Views),
                      gb_trees:enter({seg, H}, NewViews, Tree)
    end.

remove_default1(Tree, [], Type) ->
    case gb_trees:lookup(controls, Tree) of
        none       -> Tree;
        {value, V} -> NewCtls = unset_default(V, Type),
                      gb_trees:enter(controls, NewCtls, Tree)
    end;                          
remove_default1(Tree, [H | T], Type) ->
    % ok = force_terminal([H | T], "remove_views"),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Tree;
        {value, V} -> NewViews = remove_default1(V, T, Type),
                      gb_trees:enter({seg, H}, NewViews, Tree)
    end.
   
get_as_json1(Tree, Page) ->
    Fun = fun(X) ->
                  make_json(X)
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

collect_views(Ctrls, User, Groups) -> collect_v1(Ctrls, User, Groups, []).

collect_v1([], _User, _Groups, Acc)    -> Acc;
collect_v1([H | T], User, Groups, Acc) ->
    NewAcc = case can_read(H, User, Groups) of
                 true  -> #control{view = V} = H,
                          [V | Acc];
                 false -> Acc
             end,
    collect_v1(T, User, Groups, NewAcc).
            

can_read(Control, User, Groups) ->
    #control{users = Users, groups = Groups2} = Control,
    case lists:member(User, Users) of
        true  -> true;
        false -> can_read2(Groups, Groups2)
    end .    

can_read2([], _Groups2) -> false;
can_read2([H | T], Groups2) ->
    case lists:member(H, Groups2) of
        true  -> true;
        false -> can_read2(T, Groups2)
    end.

add_to_tree(Tree, [], Params, Fun) ->
    case gb_trees:lookup(controls, Tree) of
        none       -> NewCtrls = Fun([], Params),
                      gb_trees:insert(controls, NewCtrls, Tree);
        {value, V} -> NewVs = Fun(V, Params),
                      gb_trees:enter(controls, NewVs, Tree)
    end;
add_to_tree(Tree, [H | T], Params, Fun) ->
    ok = force_terminal([H | T]),
    case gb_trees:lookup({seg, H}, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = add_to_tree(Empty, T, Params, Fun),
                      gb_trees:insert({seg, H}, NewVal, Tree);
        {value, V} -> NewVal = add_to_tree(V, T, Params, Fun),
                      gb_trees:enter({seg, H}, NewVal, Tree)
    end.
   

remove_views(Ctrl, [])      -> Ctrl;
remove_views(Ctrl, [H | T]) ->
    NewCtrl = lists:keydelete(H, #control.view, Ctrl),
    remove_views(NewCtrl, T).

force_terminal([H | T]) ->
%% force [**] to be a terminal segment only
    case {H, T} of
        {"[**]", []} -> ok;
        {"[**]", _X} -> exit("non-terminal [**] segment");
        _            -> ok
    end.
    
set_def(Views, Default, Type) ->
    NewViews = unset_default(Views, type),
    set_def2(NewViews, Default, Type).

set_def2(Views, Default, Type) ->
    case lists:keysearch(Default, #control.view, Views) of
        false         -> NewView = #control{view = Default, default = Type},
                         [NewView | Views];
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
                     

add_us_and_gps(Views, _View, [])                -> Views;
add_us_and_gps(Views, View, [{Type, Role} | T]) ->
    case lists:keysearch(View, #control.view, Views) of
        false        -> Ctrl = [#control{view = View}],
                        NewCtrls = add_us_and_gps(Ctrl, View, [{Type, Role}]),
                        NewViews = lists:merge(lists:sort(NewCtrls),
                                               lists:sort(Views));
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
    io:format("H is ~p T is ~p User is ~p~n", [H, T, User]),
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

dump_s2([], _Path, Acc)                 -> lists:flatten(lists:reverse(Acc));
dump_s2([{controls, C} | T], Path, Acc) -> NewAcc = write_controls(Path, C),
                                           dump_s2(T, Path, [NewAcc | Acc]);
dump_s2([H | T], Path, Acc)             ->
    NewAcc = dump_s3(H, Path, Acc),
    dump_s2(T, Path, lists:flatten([NewAcc | Acc])).

dump_s3([], _Path, Acc)               -> Acc;
dump_s3({{seg, Path1}, H}, Path, Acc) -> dump_s1(H, [Path1 | Path], Acc).

write_controls(Path, C) -> write_c(Path, C, []).

write_c(_Path, [], Acc)     -> lists:reverse(Acc);
write_c(Path, [H | T], Acc) ->
    #control{view = V, default = D, users = U, groups = G} = H,
    NewAcc1 = {add_view,
               [{path, lists:reverse(Path)},
                {view, [V]}]},
    NewAcc2 = {add_users_and_groups,
               [{path, lists:reverse(Path)},
                {view, V},
                {users, U},
                {groups, G}]},
    case D of
        false    -> write_c(Path, T, [NewAcc2, NewAcc1 | Acc]);
        champion -> NewAcc3 = {set_champion,
                               [{path, lists:reverse(Path)},
                                 {view, V}]},
                    write_c(Path, T, [NewAcc3, NewAcc2, NewAcc1 | Acc]);
        challenger -> NewAcc3 = {set_challenger,
                                 [{path, lists:reverse(Path)},
                                  {view, V}]},
                      write_c(Path, T, [NewAcc3, NewAcc2, NewAcc1 | Acc])
    end.

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

keyfind(K, L) ->
    case lists:keyfind(K, 1, L) of
        false  -> [];
        {K, V} -> V
    end.

make_prettyprint(Tree, Host, html) ->
    Body = make_pp(Tree, html, [], "", []),
    "<html><head><title>Permissions Tree</title></head><body><code>"
        "<bold>Permissions for " ++ Host ++
        "</bold><br />" ++
        Body ++ "</code></body></html>";
make_prettyprint(Tree, Host, text) ->
    "Permissions for " ++ Host ++ "~n" ++
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
make_pp2([{{seg, K}, V} | T], Type, Prefix, Acc) ->
    NewAcc = make_pp(V, Type, K, Prefix, []),
    make_pp2(T, Type, Prefix, [NewAcc | Acc]).

pp([], Prefix, html, [])       -> Prefix ++ "&nbsp;&nbsp;&nbsp;(no controls)"
                                      ++ "<br />";
pp([], Prefix, text, [])       -> Prefix ++ "   (no controls)" ++ "~n";
pp([], _Prefix, _Type, Acc)    -> lists:reverse(Acc);
pp([{controls, H} | T], Prefix, Type, Acc) ->
    pp(T, Prefix, Type, [pp_c(H, Prefix, Type, [])| Acc]).


pp_c([], _Prefix, _Type, Acc)    -> Acc;
pp_c([H | T], Prefix, html, Acc) ->
    #control{view = V, default = D, users = U, groups = G} = H,
    NewAcc =
        case D of
            false ->
                Prefix ++ "&nbsp;&nbsp;&nbsp;view: " ++ V ++ "<br />"
                    ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;users&nbsp; : "
                    ++ make_list(U, []) ++ "<br />"
                    ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;groups : "
                    ++ make_list(G, []) ++ "<br />";
            _ ->
                Prefix ++ "&nbsp;&nbsp;&nbsp;view: " ++ V ++ " ("
                    ++ make_string(D) ++ ")<br />"
                    ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;users&nbsp; : "
                    ++ make_list(U, []) ++ "<br />"
                    ++ Prefix ++ "&nbsp;&nbsp;&nbsp;&nbsp;groups : "
                    ++ make_list(G, []) ++ "<br />"
        end,
    pp_c(T, Prefix, html, [NewAcc | Acc]);
pp_c([H | T], Prefix, text, Acc) ->
    #control{view = V, default = D, users = U, groups = G} = H,
    NewAcc =
        case D of
            false ->
                Prefix ++ "   view: " ++ V ++ "~n"
                    ++ Prefix ++ "    users  : " ++ make_list(U, []) ++ "~n"
                    ++ Prefix ++ "    groups : " ++ make_list(G, []) ++ "~n";
            _ ->
                Prefix ++ "   view: " ++ V ++ " ("++ make_string(D) ++ ")~n"
                    ++ Prefix ++ "    users  : " ++ make_list(U, []) ++ "~n"
                    ++ Prefix ++ "    groups : " ++ make_list(G, []) ++ "~n"
        end,
    pp_c(T, Prefix, text, [NewAcc | Acc]).

make_string(champion)          -> "champion/default page";
make_string(challenger)        -> "challenger";
make_string(X) when is_list(X) -> X.

make_list([], Acc)      -> string:strip(string:strip(Acc, right), right, $,);
make_list([H | T], Acc) -> make_list(T, H ++ ", " ++ Acc).

pp_l([], [])                       -> ""; % blank list is just blank!
pp_l([], [_H | Acc])               -> lists:flatten(lists:reverse(Acc));
pp_l([H | T], Acc) when is_atom(H) -> pp_l(T, [", ", atom_to_list(H) | Acc]);
pp_l([H | T], Acc) when is_list(H) -> pp_l(T, [", ", H | Acc]).

split(L) -> sp1(L, [], []).

sp1([], Controls, Paths)      -> {lists:sort(Controls), lists:sort(Paths)};
sp1([H | T], Controls, Paths) ->
    case element(1, H) of
        controls  -> sp1(T, [H | Controls], Paths);
        {seg, _S} -> sp1(T, Controls, [H | Paths])
    end.

make_json(Tree) ->
    List = gb_trees:to_list(Tree),
    {array, [make_json1(K, V) || {K, V}  <- List]}.

make_json1({seg, Seg}, V) ->
    {struct, [{{path, Seg}, make_json(V)}]};
make_json1(controls, Ctrls) ->
    {struct, [{controls, {array, make_json2(Ctrls, [])}}]}.

make_json2([], Acc)      -> Acc;
make_json2([H | T], Acc) ->
    #control{view = V, default = D, users = U, groups = G} = H,
    NewAcc = {struct, [{view, V},
                       {default, D},
                       {users, {array, U}},
                       {groups, {array, G}}]},
    make_json2(T, [NewAcc | Acc]).

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
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]}, P, champion),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

%% add views
testA2() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Ret = check_get_page1(Tree, {"User", ["Fail"]}, P, champion),
    Json = get_as_json1(Tree, []),
    io:format("Json is ~p~n", [Json]),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).

%% Users and groups
testA3() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"User", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).
    
testA4() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA4a() ->
    P = [],
    Tree = add_users_and_groups1(gb_trees:empty(), P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA5() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
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
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
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
    Ret = check_get_page1(Tree3, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree3, []),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testA7a() ->
    P = [],
    Tree = set_default1(gb_trees:empty(), P, "a view", champion),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "another view", champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
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
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA9() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P,
                      ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["another view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testA10() ->
    P = [],
    Tree = add_views1(gb_trees:empty(), P,
                      ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["a view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
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
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

%% get the challenger
testA12() ->
    P = [],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, challenger),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

%% remove defaults/challenger
testA13() ->
    P = [],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = remove_default1(Tree2, P, challenger),
    Ret = check_get_page1(Tree3, {"Fail", ["admin"]}, P, challenger),
    get_as_json1(Tree3, []),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

%% get all views available to a user
testA14() ->
    P = [],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = get_views1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["a view"]).

%% get all views available to a user
testA15() ->
    P = [],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = add_users_and_groups1(Tree2, P, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = get_views1(Tree3, {"Fail", ["admin"]}, P),
    get_as_json1(Tree3, []),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["a view", "a fourth view"]).

% check a particular page
testA16() ->
    P = [],
    Tree1 = add_users_and_groups1(gb_trees:empty(), P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = add_users_and_groups1(Tree2, P, "a third view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree4 = add_users_and_groups1(Tree3, P, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_particular_page1(Tree4, {"Fail", ["admin"]},
                                 P, "a third view"),
    Json = get_as_json1(Tree4, []),
    io:format("Json is ~p~n", [Json]),
    PP = pretty_print1(Tree4, "test", [], text),
    io:format(PP),
    io:format("Tree4 is ~p~n", [Tree4]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a third view"}).


% test the non-empty path
testB1() ->
    P = ["hip", "hop"],
    Ret = check_get_page1(gb_trees:empty(), {"gordon", ["Group"]},
                          P, champion),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '404'}).

testB2() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    io:format("Tree is ~p~n", [Tree]),
    Ret = check_get_page1(Tree, {"User", ["Fail"]}, P, champion),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).

testB3() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"User", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).
    
testB4() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB4a() ->
    P = ["blah", "bloh"],
    Tree = add_users_and_groups1(gb_trees:empty(), P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree, []),
    PP = pretty_print1(Tree, "test", [], text),
    io:format(PP),
    io:format("Tree is ~p~n", [Tree]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB5() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree1, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree1 is ~p~n", [Tree1]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB6() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "another view", champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
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
    Ret = check_get_page1(Tree3, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree3, []),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testB7a() ->
    P = ["blah", "bloh"],
    Tree = set_default1(gb_trees:empty(), P, "a view", champion),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P, "another view", champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
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
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB9() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P, 
                      ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["another view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree1, []),
    PP = pretty_print1(Tree1, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testB10() ->
    P = ["blah", "bloh"],
    Tree = add_views1(gb_trees:empty(), P,
                      ["a view", "another view", "a third"]),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P, ["a view", "a third"]),
    Ret = check_get_page1(Tree3, {"gordon", ["Fail"]}, P, champion),
    get_as_json1(Tree3, []),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testB11() ->
    P = ["bing", "bong"],
    Tree = set_default1(gb_trees:empty(), P, "another view", champion),
    Tree1 = add_users_and_groups1(Tree, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = remove_default1(Tree1, P, champion),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

%% get the challenger
testB12() ->
    P = ["ecky", "thump"],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree2, {"Fail", ["admin"]}, P, challenger),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testB13() ->
    P = ["badda", "bing"],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = remove_default1(Tree2, P, challenger),
    Ret = check_get_page1(Tree3, {"Fail", ["admin"]}, P, champion),
    get_as_json1(Tree3, []),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a third view"}).

%% get all views available to a user
testB14() ->
    P = ["bingo", "masters", "breakout"],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = get_views1(Tree2, {"Fail", ["admin"]}, P),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Tree2 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["a view"]).

%% get all views available to a user
testB15() ->
    P = ["bingo", "masters", "breakout"],
    Tree = set_default1(gb_trees:empty(), P, "another view", challenger),
    Tree1 = set_default1(Tree, P, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = add_users_and_groups1(Tree2, P, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = get_views1(Tree3, {"Fail", ["admin"]}, P),
    get_as_json1(Tree3, []),
    PP = pretty_print1(Tree3, "test", [], text),
    io:format(PP),
    io:format("Tree3 is ~p~n", [Tree3]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["a view", "a fourth view"]).

% check a particular page
testB16() ->
    P = ["bingo", "masters", "breakout"],
    Tree1 = add_users_and_groups1(gb_trees:empty(), P, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P, "another view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = add_users_and_groups1(Tree2, P, "a third view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree4 = add_users_and_groups1(Tree3, P, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_particular_page1(Tree4, {"Fail", ["admin"]},
                                 P, "a third view"),
    Json = get_as_json1(Tree4, []),
    Script = dump_script1(Tree4),
    io:format("Script is ~p~n", [Script]),
    io:format("Json is ~p~n", [Json]),
    PP = pretty_print1(Tree4, "test", [], text),
    io:format(PP),
    io:format("Tree4 is ~p~n", [Tree4]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a third view"}).

%% test multiple pages
testC2() ->
    P1 = ["hip", "hop"],
    P2 = ["dont", "stop"],
    Tree = add_views1(gb_trees:empty(), P1, ["a view", "another view"]),
    Tree2 = add_views1(Tree, P2, ["ignore", "me"]),
    io:format("Tree is ~p~n", [Tree2]),
    Ret = check_get_page1(Tree2, {"User", ["Fail"]}, P1, champion),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {return, '503'}).

testC4() ->
    P1 = ["hip", "hop"],
    P2 = ["hip"],
    Tree = add_views1(gb_trees:empty(), P1, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P1, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P2, "a view",
                                  [{user, "fail"}, {group, "fail"}]),
    Ret = check_get_page1(Tree2, {"gordon", ["Fail"]}, P1, champion),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree2, "test", [], text),
    io:format(PP),
    io:format("Tree1 is ~p~n", [Tree2]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testC6() ->
    P1 = ["hip", "hop"],
    P2 = ["hip"],
    Tree = add_views1(gb_trees:empty(), P1, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P1, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = set_default1(Tree1, P1, "another view", champion),
    Tree3 = add_users_and_groups1(Tree2, P2, "blurgh",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree4 = set_default1(Tree3, P2, "blah-ha-ha", champion),
    Ret = check_get_page1(Tree4, {"Fail", ["admin"]}, P1, champion),
    get_as_json1(Tree4, []),
    PP = pretty_print1(Tree4, "test", [], text),
    io:format(PP),
    io:format("Tree4 is ~p~n", [Tree4]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testC8() ->
    P1 = ["hip", "hop"],
    P2 = ["hip"],
    Tree = add_views1(gb_trees:empty(), P1, ["a view", "another view"]),
    Tree1 = add_users_and_groups1(Tree, P1, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P1, "another view",
                                  [{user, "gordon"}, {group, "bleh"}]),
    Tree3 = remove_views1(Tree2, P1, ["another view"]),
    Tree4 = add_users_and_groups1(Tree3, P2, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree5 = add_users_and_groups1(Tree4, P2, "another view",
                                 [{user, "gordon"}, {group, "bleh"}]),
    Tree6 = remove_views1(Tree5, P2, ["another view"]),
    Ret = check_get_page1(Tree6, {"gordon", ["Fail"]}, P1, champion),
    get_as_json1(Tree6, []),
    PP = pretty_print1(Tree6, "test", [], text),
    io:format(PP),
    io:format("Tree6 is ~p~n", [Tree6]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a view"}).

testC11() ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree = set_default1(gb_trees:empty(), P1, "another view", champion),
    Tree1 = add_users_and_groups1(Tree, P1, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = remove_default1(Tree1, P1, champion),
    Tree3 = set_default1(Tree2, P1, "another view", champion),
    Tree4 = add_users_and_groups1(Tree3, P2, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree5 = remove_default1(Tree4, P2, champion),
    Tree6 = add_users_and_groups1(Tree5, P3, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_get_page1(Tree6, {"Fail", ["admin"]}, P1, champion),
    get_as_json1(Tree6, []),
    PP = pretty_print1(Tree6, "test", [], text),
    io:format(PP),
    io:format("Tree6 is ~p~n", [Tree6]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testC12() ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree = set_default1(gb_trees:empty(), P1, "another view", challenger),
    Tree1 = set_default1(Tree, P1, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P1, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = set_default1(Tree2, P2, "bleg", challenger),
    Tree4 = set_default1(Tree3, P2, "blog", champion),
    Tree5 = add_users_and_groups1(Tree4, P2, "a view",
                                  [{user, "hmm"}, {group, "heh"}]),
    Tree6 = set_default1(Tree5, P3, "scooby", challenger),
    Tree7 = set_default1(Tree6, P3, "do", champion),
    Tree8 = add_users_and_groups1(Tree7, P3, "boo-hoo",
                                  [{user, "annabel"}, {group, "typist"}]),
    Ret = check_get_page1(Tree8, {"Fail", ["admin"]}, P1, challenger),
    get_as_json1(Tree2, []),
    PP = pretty_print1(Tree8, "test", [], text),
    io:format(PP),
    io:format("Tree8 is ~p~n", [Tree8]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "another view"}).

testC13() ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree = set_default1(gb_trees:empty(), P1, "another view", challenger),
    Tree1 = set_default1(Tree, P1, "a third view", champion),
    Tree2 = add_users_and_groups1(Tree1, P1, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = remove_default1(Tree2, P1, challenger),

    Tree4 = set_default1(Tree3, P2, "another view", challenger),
    Tree5 = set_default1(Tree4, P2, "a third view", champion),
    Tree6 = add_users_and_groups1(Tree5, P2, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree7 = remove_default1(Tree6, P2, challenger),

    Tree8 = set_default1(Tree7, P3, "another view", challenger),
    Tree9 = set_default1(Tree8, P3, "a third view", champion),
    Tree10 = add_users_and_groups1(Tree9, P3, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree11 = remove_default1(Tree10, P3, challenger),

    Ret = check_get_page1(Tree11, {"Fail", ["admin"]}, P1, champion),
    get_as_json1(Tree11, []),
    PP = pretty_print1(Tree11, "test", [], text),
    io:format(PP),
    io:format("Tree11 is ~p~n", [Tree11]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a third view"}).

testC15() ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree = set_default1(gb_trees:empty(), P1, "snorkel", challenger),
    Tree1 = set_default1(Tree, P1, "blanket", champion),
    Tree2 = add_users_and_groups1(Tree1, P1, "bingo",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = add_users_and_groups1(Tree2, P1, "bimbo",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree4 = set_default1(Tree3, P2, "another view", challenger),
    Tree5 = set_default1(Tree4, P2, "a third view", champion),
    Tree6 = add_users_and_groups1(Tree5, P2, "a view",
                                 [{user, "gordon"}, {group, "admin"}]),
    Tree7 = add_users_and_groups1(Tree6, P2, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree8 = set_default1(Tree7, P3, "another view", challenger),
    Tree9 = set_default1(Tree8, P3, "a third view", champion),
    Tree10 = add_users_and_groups1(Tree9, P3, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree11 = add_users_and_groups1(Tree10, P3, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),

    Ret = get_views1(Tree11, {"Fail", ["admin"]}, P2),
    get_as_json1(Tree11, []),
    PP = pretty_print1(Tree11, "test", [], text),
    io:format(PP),
    io:format("Tree11 is ~p~n", [Tree11]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == ["a view", "a fourth view"]).

testC16() ->
    P1 = ["hip"],
    P2 = ["hip", "hop"],
    P3 = ["dont", "stop"],
    Tree1 = add_users_and_groups1(gb_trees:empty(), P1, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree2 = add_users_and_groups1(Tree1, P1, "another view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree3 = add_users_and_groups1(Tree2, P1, "a third view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree4 = add_users_and_groups1(Tree3, P1, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree5 = add_users_and_groups1(Tree4, P2, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree6 = add_users_and_groups1(Tree5, P2, "another view",
                                 [{user, "gordon"}, {group, "admin"}]),
    Tree7 = add_users_and_groups1(Tree6, P2, "a third view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree8 = add_users_and_groups1(Tree7, P2, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree9 = add_users_and_groups1(Tree8, P3, "a view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree10 = add_users_and_groups1(Tree9, P3, "another view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree11 = add_users_and_groups1(Tree10, P3, "a third view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Tree12 = add_users_and_groups1(Tree11, P3, "a fourth view",
                                  [{user, "gordon"}, {group, "admin"}]),
    Ret = check_particular_page1(Tree12, {"Fail", ["admin"]},
                                 P2, "a third view"),
    Json = get_as_json1(Tree12, []),
    Script = dump_script1(Tree12),
    io:format("Script is ~p~n", [Script]),
    io:format("Json is ~p~n", [Json]),
    PP = pretty_print1(Tree12, "test", [], text),
    io:format(PP),
    io:format("Tree12 is ~p~n", [Tree12]),
    io:format("Ret is ~p~n", [Ret]),
    (Ret == {html, "a third view"}).


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
     ?_assert(testA12()),
     ?_assert(testA13()),
     ?_assert(testA14()), 
     ?_assert(testA15()),
     ?_assert(testA16()),
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
     ?_assert(testB11()),
     ?_assert(testB12()),
     ?_assert(testB13()),
     ?_assert(testB14()),
     ?_assert(testB15()),
     ?_assert(testB16()),
     ?_assert(testC2()),
     ?_assert(testC4()),
     ?_assert(testC6()),
     ?_assert(testC8()),
     ?_assert(testC11()),
     ?_assert(testC12()),
     ?_assert(testC13()),
     ?_assert(testC15()),
     ?_assert(testC16())
    ].
