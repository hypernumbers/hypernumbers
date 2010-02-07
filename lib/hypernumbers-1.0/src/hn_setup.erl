%%% It is expected that all system, and module support tables are
%%% inplace prior to calling functions in this module.
-module(hn_setup).

-export([
         startup/0,
         site/3,
         delete_site/1,
         update/0, update/1,
         update/3,
         add_user/2,
         get_sites/0,
         resave_views/0
        ]).

-include("spriki.hrl").

-spec get_sites() -> list().
get_sites() ->
    Fun = fun() ->
                  mnesia:all_keys(core_site)
          end,
    mnesia:activity(transaction, Fun).

%% Startup current sites. 
-spec startup() -> ok. 
startup() ->
    F = fun(#core_site{site = Site}, _Acc) ->
                ok = launch_site(Site)
        end,
    Trans = fun() -> mnesia:foldl(F, nil, core_site) end,
    {atomic, _} = mnesia:transaction(Trans),
    ok.


%% Setup a new site from scratch
-spec site(string(), atom(), [{atom(), any()}]) -> ok.
site(Site, Type, Opts) when is_list(Site), is_atom(Type) ->
    error_logger:info_msg("Setting up: ~p as ~p~n", [Site, Type]),
    All = [templates, json, permissions, script, user_permissions, users],
    ok  = create_site(Site, Type),
    ok  = update(Site, Type, Opts, All).


%% Delete a site
%% TODO : stop any supervisors if they are running
-spec delete_site(string()) -> ok.
delete_site(Site) ->
    Dest = code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:parse_site(Site) ++ "/",

    auth_srv2:clear_all_perms_DEBUG(Site),
    [ {atomic, ok}  = mnesia:delete_table(hn_db_wu:trans(Site, Table))
      || {Table, _F, _T, _I} <- tables()],

    ok = hn_util:delete_directory(Dest),
    ok = mnesia:dirty_delete(core_site, Site).

%% Update all existing sites with default options
-spec update() -> ok. 
update() ->
    update([], [templates]).


-spec update(list()) -> ok. 
update(Opts) ->
    update([], Opts).


%% Update all sites
-spec update(list(), list()) -> ok. 
update(Opaque, Opts) ->
    F = fun(#core_site{site=Site, type=Type}, _Acc) ->
                update(Site, Type, Opaque, Opts)
        end,
    Trans = fun() -> mnesia:foldl(F, nil, core_site) end,
    {atomic, _} = mnesia:transaction(Trans),
    ok.


-spec update(string(), list(), list()) -> ok.
update(Site, Opaque, Opts) ->
    update(Site, get_type_by_site(Site), Opaque, Opts).


-spec update(string(), atom(), list(), list()) -> ok.
update(Site, Type, Opaque, Opts) ->
    [ ok = setup(Site, Type, Opaque, X) || X <- Opts ],
    ok.


-spec add_user(#refX{}, #hn_user{}) -> ok.
add_user(Site, User) ->
    Script = [sitedir(Site),"/","user.permissions.script"],
    case filelib:is_file(Script) of
        true  ->
            {ok, Terms} = file:consult(Script),
            [ add_u(Site, User, Term) || Term <- Terms, is_term(Term)],
            ok;
        false ->
            ok
    end.


-spec setup(string(), atom(), list(), atom()) -> ok.
setup(Site, Type, _Opts, templates) ->
    ok = filelib:ensure_dir(sitedir(Site)),
    % first copy over the standard type
    ok = hn_util:recursive_copy(coreinstalldir(), sitedir(Site)),
    ok = hn_util:recursive_copy(moddir(Type), sitedir(Site));
setup(Site, Type, _Opts, json) ->
    ok = import_json(Site, moddir(Type));
setup(Site, _Type, _Opts, permissions) ->
    Fun = fun(T) -> run_perms(T, Site) end,
    ok  = run([sitedir(Site),"/","permissions.script"], Fun);
setup(Site, _Type, Opts, script) ->
    Fun = fun(T) -> run_script(T, Site, Opts) end, 
    ok = run([sitedir(Site),"/","setup.script"], Fun);
setup(Site, _Type, Opts, users) ->
    Fun = fun(T) -> run_users(T, Site, Opts) end,
    ok  = run([sitedir(Site),"/","users.script"], Fun);
setup(Site, _Type, _Opts, user_permissions) ->    
    Users = mnesia:dirty_match_object(hn_db_wu:trans(Site, hn_user),
                                      #hn_user{_='_'}),
                 [ add_user(Site, User) || User <- Users],
    ok.

-spec coreinstalldir() -> string().
coreinstalldir() ->
    code:priv_dir(sitemods) ++ "/core_install".

-spec moddir(atom()) -> string(). 
moddir(Type) ->
    code:priv_dir(sitemods) ++ "/site_types/" ++ atom_to_list(Type).

-spec sitedir(atom()) -> string(). 
sitedir(Site) ->
    code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:parse_site(Site) ++ "/".


-spec launch_site(string()) -> ok. 
launch_site(Site) ->
    ok = dirty_srv:start(Site).


-spec resave_views() -> ok.
resave_views() ->

    ViewsPath = "/../../var/sites/*/docroot/views/*/*/*.tpl",

    [ resave_view(X)
      || X <- filelib:wildcard(code:lib_dir(hypernumbers) ++ ViewsPath) ],

    ok.

-spec resave_view(string()) -> ok.
resave_view(Path) ->
    ok.


-spec create_site(string(), atom()) -> ok.
create_site(Site, Type)->
%% Seems sensible to keep this restricted
%% to disc_copies for now
    Storage = disc_copies,
    [ok = hn_db_admin:create_table(hn_db_wu:trans(Site, N),
                                   N, F, Storage, T, I)
     || {N,F,T,I} <- tables()],
    Trans = fun() ->
                    ok = mnesia:write(#core_site{site = Site, type = Type}),
                    launch_site(Site)
            end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.


-define(TBL(N, T, I), {N, record_info(fields, N), T, I}).
tables() ->
    [ ?TBL(dirty_queue,           ordered_set, []),
      ?TBL(dirty_notify_in,       set,     	   []),         
      ?TBL(dirty_inc_hn_create,   set,    	   []),         
      ?TBL(dirty_notify_back_in,  set,    	   []),         
      ?TBL(dirty_notify_out,      set,    	   []),         
      ?TBL(dirty_notify_back_out, set,    	   []),         
      ?TBL(item,                  bag,    	   [key]),      
      ?TBL(local_objs,            bag,    	   [obj,idx]), 
      ?TBL(local_cell_link,       bag,    	   [childidx]), 
      ?TBL(relation,              set,    	   []),
      ?TBL(hn_user,               set,    	   []),         
      ?TBL(remote_objs,           set,    	   []),         
      ?TBL(remote_cell_link,      bag,    	   []),         
      ?TBL(incoming_hn,           set,    	   []),         
      ?TBL(outgoing_hn,           set,    	   []),         
      ?TBL(styles,                bag,    	   []),         
      ?TBL(style_counters,        set,    	   []),         
      ?TBL(page_vsn,              set,    	   []),         
      ?TBL(page_history,          bag,    	   []) ].


%% Import a set of json files into the live spreadsheet
import_json(Site, Dir) ->
    Files = filelib:wildcard(Dir++"/data/*.json"),
    JsonFiles = lists:filter(fun is_path/1, Files),
    [ ok = hn_import:json_file(Site ++ create_path_from_name(Json), Json)
      || Json <- JsonFiles],
    ok.

is_path(L) ->
    Tokens = string:tokens(L, "/"),
    File = hd(lists:reverse(Tokens)),
    case string:tokens(File, ".") of
        ["path" | _T] -> true;
        _Other        -> false
    end.

create_path_from_name(Name) ->
    [ "path" | Rest ]
        = string:tokens(filename:basename(Name, ".json"), "."),
    hn_util:list_to_path(Rest).


run(Script, Fun) ->
    case filelib:is_file(Script) of
        true  ->
            {ok, Terms} = file:consult(Script),
            [ok = Fun(Term) || Term<-Terms, is_term(Term)],
            ok;
        false ->
            ok
    end.

is_term([37 | _T1]) -> false;
is_term("\n")       -> false;
is_term(_)          -> true.

run_users({{user,Usr}, {group,Grp}, {email,_Mail}, {password,Pass}},
          Site, Opts) ->
    ok = hn_users:create(Site, resolve_user(Usr, Opts),
                         Grp,  resolve_password(Pass, Opts)).

resolve_user('$user', Opts) ->
    case pget(user, Opts, undefined) of
        undefined -> throw(no_user);
        U         -> U
    end;
resolve_user(User, _Opts) ->
    User.

resolve_password('$password', Opts) ->
    case pget(password, Opts, undefined) of
        undefined -> throw(no_pass);
        P         -> P
    end;
resolve_password(Password, _Opts) ->
    Password.

run_perms({add_view, C}, Site) ->
    auth_srv2:add_view(Site, lget(path, C), lget(perms, C), lget(view, C));
run_perms({champion, C}, Site) ->
    auth_srv2:set_champion(Site, lget(path, C), lget(view, C));
run_perms({challenger, C}, Site) ->
    auth_srv2:set_challenger(Site, lget(path, C), lget(view, C)).

run_script({Path, '$email'}, Site, Opts) ->
    run_script2(Path, Site, pget(email, Opts));
run_script({Path, '$user'}, Site, Opts) ->
    run_script2(Path, Site, pget(user, Opts));
run_script({Path, '$site'}, Site, _Opts) ->
    run_script2(Path, Site, Site);
run_script({Path, '$subdomain'}, Site, Opts) ->
    run_script2(Path, Site, pget(subdomain, Opts));
run_script({Path, '$expiry'}, Site, _Opts) ->
    {Date, _Time} = calendar:now_to_datetime(now()),
    NewDays = calendar:date_to_gregorian_days(Date) + 31,
    NewDate = calendar:gregorian_days_to_date(NewDays),
    Expr = "This site will expire on "
        ++ dh_date:format("D d M Y", {NewDate, {0, 0, 0}}),
    run_script2(Path, Site, Expr);
run_script({Path, '$password'}, Site, Opts) ->
    run_script2(Path, Site, pget(password, Opts));
run_script({Path, Expr}, Site, _Opts) ->
    run_script2(Path, Site, Expr).

run_script2(Path, Site, Expr) ->
    RefX = hn_util:parse_url(Site++Path), 
    hn_db_api:write_attributes([{RefX, [{"formula", Expr}]}]).


pget(Key, List) ->
    pget(Key, List, "").
pget(Key, List, Default) ->
    proplists:get_value(Key, List, Default).

lget(Key, List) ->
    element(2, lists:keyfind(Key, 1, List)).

replace(Key, Val, Key) ->
    Val;
replace(Key, Val, Rep) when is_list(Rep) ->
    [ replace(Key, Val, X) || X <- Rep ];
replace(Key, Val, Rep) when is_tuple(Rep) ->
    list_to_tuple( replace(Key, Val, tuple_to_list(Rep) ));
replace(_Key, _Val, Else) ->
    Else.

add_u(Site, User, {champion, C}) ->
    UserName = hn_users:name(User),
    Path     = replace('$user', UserName, lget(path, C)),
    auth_srv2:set_champion(Site, Path, lget(view, C));

add_u(Site, User, {add_view, C}) -> 
    UserName = hn_users:name(User),
    Perms    = replace('$user', UserName, lget(perms, C)),
    Path     = replace('$user', UserName, lget(path, C)),
    auth_srv2:add_view(Site, Path, Perms, lget(view, C));

add_u(Site, User, {import, P}) ->
    UserName = hn_users:name(User),
    P2 = replace('$user', "$user", lget(path, P)),
    Path = replace('$user', UserName, lget(path, P)),
    FileName = re:replace(hn_util:path_to_json_path(P2), "^path.",
                          "template.", [{return, list}]),
    Dest = code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:parse_site(Site) ++ "/data/",
    Url = Site ++ hn_util:list_to_path(Path),
    ok = hn_import:json_file(Url, Dest ++ FileName).

-spec get_type_by_site(list()) -> atom().
get_type_by_site(Site) ->
    [#core_site{type=Type}] = mnesia:dirty_read(core_site, Site),
    Type.
