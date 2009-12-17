%%% It is expected that all system, and module support tables are
%%% inplace prior to calling functions in this module.
-module(hn_setup).

-export([
         startup/0,
         site/3,
         delete_site/1,
         update/0, update/1,
         update/3,
         add_user/2
        ]).

-include("spriki.hrl").


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
    All = [json, templates, permissions, script, user_permissions, users],
    ok  = create_site(Site, Type),
    ok  = update(Site, Type, Opts, All).


%% Delete a site
%% TODO : stop any supervisors if they are running
-spec delete_site(string()) -> ok.
delete_site(Host) ->

    Dest = code:lib_dir(hypernumbers) ++ "/../../var/docroot/"
        ++ hn_util:parse_site(Host) ++ "/",
    
    auth_srv:clear_all_perms_DEBUG(Host),
    [ {atomic, ok}  = mnesia:delete_table(hn_db_wu:trans(Host, Table))
      || {Table, _F, _T, _I} <- tables()],
    
    ok = hn_util:delete_directory(Dest),
    ok = mnesia:dirty_delete(core_site, Host).



  
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
    Type   = get_type_by_site(Site),
    Script = [moddir(Type),"/","user.permissions.script"],
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
    Dest = code:lib_dir(hypernumbers) ++ "/../../var/docroot/"
        ++ hn_util:parse_site(Site) ++ "/",
    ok = filelib:ensure_dir(Dest),
    ok = hn_util:recursive_copy(moddir(Type)++"/docroot", Dest);

setup(Site, Type, _Opts, json) ->
    ok = import_json(Site, moddir(Type));
setup(Site, Type, _Opts, permissions) ->
    Fun = fun(T) -> run_perms(T, Site) end,
    ok  = run([moddir(Type),"/","permissions.script"], Fun);
setup(Site, Type, Opts, script) ->
    Fun = fun(T) -> run_script(T, Site, Opts) end, 
    ok = run([moddir(Type),"/","setup.script"], Fun);
setup(Site, Type, Opts, users) ->
    Fun = fun(T) -> run_users(T, Site, Opts) end,
    ok  = run([moddir(Type),"/","users.script"], Fun);
setup(Site, _Type, _Opts, user_permissions) ->    
    Users = mnesia:dirty_match_object(hn_db_wu:trans(Site, hn_user),
                                      #hn_user{_='_'}),
    [ add_user(Site, User) || User <- Users],
    ok.

-spec moddir(atom()) -> string(). 
moddir(Type) ->
    code:priv_dir(sitemods) 
        ++ "/" ++ atom_to_list(Type).

-spec launch_site(string()) -> ok. 
launch_site(Site) ->
    ok = dirty_srv:start(Site).

-define(RIF(R), record_info(fields, R)).
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

tables() ->
    [{dirty_cell,            ?RIF(dirty_cell),            set, []},      
     {dirty_notify_in,       ?RIF(dirty_notify_in),       set, []},         
     {dirty_inc_hn_create,   ?RIF(dirty_inc_hn_create),   set, []},         
     {dirty_notify_back_in,  ?RIF(dirty_notify_back_in),  set, []},         
     {dirty_notify_out,      ?RIF(dirty_notify_out),      set, []},         
     {dirty_notify_back_out, ?RIF(dirty_notify_back_out), set, []},         
     {item,                  ?RIF(item),                  bag, [key]},      
     {local_objs,            ?RIF(local_objs),            bag, [obj,idx]}, 
     {local_cell_link,       ?RIF(local_cell_link),       bag, [childidx]}, 
     {hn_user,               ?RIF(hn_user),               set, []},         
     {remote_objs,           ?RIF(remote_objs),           set, []},         
     {remote_cell_link,      ?RIF(remote_cell_link),      bag, []},         
     {incoming_hn,           ?RIF(incoming_hn),           set, []},         
     {outgoing_hn,           ?RIF(outgoing_hn),           set, []},         
     {styles,                ?RIF(styles),                bag, []},         
     {style_counters,        ?RIF(style_counters),        set, []},         
     {page_vsn,              ?RIF(page_vsn),              set, []},         
     {page_history,          ?RIF(page_history),          bag, []}].
    

%% Import a set of json files into the live spreadsheet
import_json(Site, Dir) ->
    [ ok = hn_import:json_file(Site ++ create_path_from_name(Json), Json)
      || Json <- filelib:wildcard(Dir++"/data/*.json")],
    ok.

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

run_perms({perms_and_views, C}, Site) ->
    auth_srv:add_perms_and_views(Site,  lget(list, C),
                                 lget(path, C),     lget(perms, C),
                                 lget(override, C), lget(views, C));

run_perms({perm, P}, Site) ->
    auth_srv:add_perm(Site, 
                      lget(list, P), lget(path, P), lget(perms, P));

run_perms({views, V}, Site) ->
    auth_srv:add_views(Site,
                       lget(list, V),     lget(path, V),
                       lget(override, V), lget(views, V));

run_perms({default, D}, Site)  ->
    auth_srv:add_default(Site, lget(path, D), lget(default, D)).

run_script({Path, '$email'}, Site, Opts) ->
    run_script2(Path, Site, pget(email, Opts));
run_script({Path, '$user'}, Site, Opts) ->
    run_script2(Path, Site, pget(user, Opts));
run_script({Path, '$site'}, Site, _Opts) ->
    run_script2(Path, Site, Site);
run_script({Path, '$domain'}, Site, _Opts) ->
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

add_u(Site, User, {perms_and_views,
                   [{list,     Auth},  {path,     Path},
                    {perms,    Perms}, {override, Def},
                    {views,    Views}]}) ->
    auth_srv:add_perms_and_views(Site,
                                 replace('$user', hn_users:name(User), Auth),
                                 replace('$user', hn_users:name(User), Path),
                                 Perms, Def, Views).


-spec get_type_by_site(list()) -> atom().
get_type_by_site(Site) ->
    [#core_site{type=Type}] = mnesia:dirty_read(core_site, Site),
    Type.

