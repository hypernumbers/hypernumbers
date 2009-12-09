%%% It is expected that all system, and module support tables are
%%% inplace prior to calling functions in this module.
-module(hn_setup).

-export([ site/3,
          startup/0,
          update/3, update/4,
          add_user/2
        ]).

-include("spriki.hrl").

-spec site(string(), atom(), [{atom(), any()}]) -> ok. 
site(Site, Type, Opts) when is_list(Site), is_atom(Type) ->

    error_logger:info_msg("Setting up: ~p as ~p~n", [Site, Type]),
    
    ok = create_site(Site, Type),

    ok = setup(Site, Type, Opts, json),
    ok = setup(Site, Type, Opts, templates),
    ok = setup(Site, Type, Opts, permissions),
    ok = setup(Site, Type, Opts, script),
    ok = setup(Site, Type, Opts, user_permissions),
    ok = setup(Site, Type, Opts, users),

    ok.

-spec setup(string(), atom(), list(), atom()) -> ok.
setup(Site, Type, _Opts, templates) ->
    Dest = code:priv_dir(hypernumbers) ++ "/docroot/views/"
        ++ hn_util:parse_site(Site) ++ "/",
    ok = hn_util:recursive_copy(moddir(Type)++"/../../_global",
                                Dest++"_global"),
    ok = hn_util:recursive_copy(moddir(Type)++"/viewtemplates", Dest);
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

-spec update(string(), atom(), list()) -> ok. 
update(Site, Type, ToUpdate) ->
    update(Site, Type, [], ToUpdate).
-spec update(string(), atom(), list(), list()) -> ok. 
update(Site, Type, Opts, ToUpdate) ->
    [ ok = setup(Site, Type, Opts, X) || X <- ToUpdate ],
    ok.

-spec moddir(atom()) -> string(). 
moddir(Type) ->
    code:priv_dir(sitemods) 
        ++ "/site_templates/sites/" ++ atom_to_list(Type).

-spec startup() -> ok. 
%% Startup current sites. 
startup() ->
    F = fun(#core_site{site = Site}, Acc) ->
                ok = launch_site(Site),
                Acc
        end,
    Trans = fun() -> mnesia:foldl(F, nil, core_site) end,
    {atomic, _} = mnesia:transaction(Trans),
    ok.

-spec launch_site(string()) -> ok. 
launch_site(Site) ->
    ok = dirty_srv:start(Site).


-define(RIF(R), record_info(fields, R)).
-spec create_site(string(), atom()) -> ok.
create_site(Site, Type)->
    %% Seems sensible to keep this restricted
    %% to disc_copies for now
    Storage = disc_only_copies,
    Opts = 
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
         {page_history,          ?RIF(page_history),          bag, []}],        

    [ok = hn_db_api:create_table(hn_db_wu:trans(Site, N), 
                                 N, F, Storage, T, I)
     || {N,F,T,I} <- Opts],
    Trans = fun() ->
                    ok = mnesia:write(#core_site{site = Site, type = Type}),
                    launch_site(Site)
            end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.


%% Import a set of json files into the live spreadsheet
import_json(Site, Dir) ->
    [ ok = hn_import:json_file(Site ++ create_path_from_name(Json), Json)
      || Json <- filelib:wildcard(Dir++"/*.json")],
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
is_term("\n") -> false;
is_term(_) -> true.

run_users({{user,Usr}, {group,Grp}, {email,_Mail}, {password,Pass}},
          Site, Opts) ->
    ok = hn_users:create(Site,
                         get_user(Usr, Opts),
                         Grp,
                         get_password(Pass, Opts)).


get_user('$user', Opts) ->
    case pget(user, Opts, undefined) of
        undefined -> throw(no_user);
        U -> U
    end;
get_user(User, _Opts) -> User.
 
%% get_email('$email', Opts) -> pget(email, Opts);
%% get_email(EMail, _Opts)     -> EMail.
 
get_password('$password', Opts) ->
    case pget(password, Opts, undefined) of
        undefined -> throw(no_pass);
        P -> P
    end;
get_password(Password, _Opts) -> Password.

run_perms({control, C}, Site) ->
    auth_srv:add_controls(Site,  lget(list, C),
                          lget(page, C),     lget(perms, C),
                          lget(override, C), lget(views, C));

run_perms({perm, P}, Site) ->
    auth_srv:add_perm(Site, 
                      lget(list, P), lget(page, P), lget(perms, P));

run_perms({views, V}, Site) ->
    auth_srv:add_views(Site,
                       lget(list, V),     lget(page, V),
                       lget(override, V), lget(views, V));

run_perms({default, D}, Site)  ->
    auth_srv:add_default(Site,
                         lget(page, D), lget(default, D)).

run_script({Path, '$email'}, Site, Opts) ->
    run_script2(Path, Site, pget(email, Opts));
run_script({Path, '$username'}, Site, Opts) ->
    run_script2(Path, Site, pget(username, Opts));
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

add_u(Site, User, {control,
                   [{list,     Auth},
                    {page ,    Page},
                    {perms,    Perms},
                    {override, Def},
                    {views,    Views}
                   ]}) ->
    
    auth_srv:add_controls(Site,
                          replace("$user", hn_users:name(User), Auth),
                          replace("$user", hn_users:name(User), Page),
                          Perms, Def, Views).

%-spec(#refX{}, #hn_user{}).
add_user(Site, User) ->
    [{core_site, Site, Type}] = mnesia:dirty_read(core_site, Site),
    Script = [moddir(Type),"/","user.permissions.script"],
    case filelib:is_file(Script) of
        true  ->
            {ok, Terms} = file:consult(Script),
            [ add_u(Site, User, Term) || Term<-Terms, is_term(Term)],
            ok;
        false ->
            ok
    end.
