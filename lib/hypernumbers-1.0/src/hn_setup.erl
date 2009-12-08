-module(hn_setup).

-include("password.hrl").
-include("spriki.hrl").

-export([ new_site/2, new_site/3 ]).

new_site(Host, Type) ->
    new_site(Host, Type, [{host, Host}]).

new_site(Host, Type, Opts) ->

    Dir = code:lib_dir(hypernumbers) ++ "/../../priv/site_templates/sites/"
        ++ Type,
    
    % create the database tables
    ok = hn_db_api:create_db(Host),

    % copy the appropriate sets of pages into the database tables
    ok = import_json(Host, Dir),

    % copy over the view templates
    Dest = code:lib_dir(hypernumbers) ++ "/priv/docroot/views/"
        ++ hn_util:parse_site(Host) ++ "/",
    ok = hn_util:recursive_copy(Dir ++ "/../../_global", Dest++"_global"),
    ok = hn_util:recursive_copy(Dir ++ "/viewtemplates", Dest),

    % create the appropriate permissions binding the appropriate tpl’s
    % to the right paths
    ok = run(Dir++"/"++"permissions.script", Opts, fun run_perms/2),

    % push any custom info into the pages (eg username)
    ok = run(Dir++"/"++"setup.script", Opts, fun run_script/2),
    
    % create the new user(s)
    ok = run(Dir++"/"++"users.script", Opts, fun run_users/2),
    
    ok.

%% Import a set of json files into the live spreadsheet
import_json(Host, Dir) ->
    [ ok = hn_import:json_file(Host ++ create_path_from_name(Json), Json)
      || Json <- filelib:wildcard(Dir++"/*.json")],
    ok.
    
create_path_from_name(Name) ->
    [ "path" | Rest ]
        = string:tokens(filename:basename(Name, ".json"), "."),
    hn_util:list_to_path(Rest).

run(Script, Details, Fun) ->
    case filelib:is_file(Script) of
        true  ->
            {ok, Terms} = file:consult(Script),
            [ok = Fun(Term, Details) || Term<-Terms, is_term(Term)],
            ok;
        false ->
            ok
    end.

is_term([37 | _T1]) -> false;
is_term("\n") -> false;
is_term(_) -> true.

run_users({{user,Usr}, {group,Grp}, {email,_Mail}, {password,Pass}}, Opts) ->
    ok = hn_users:create(pget(host, Opts),
                         get_user(Usr, Opts),
                         Grp,
                         get_password(Pass, Opts)).

get_user('$user', Opts) -> pget(username, Opts);
get_user(User, _Dt)     -> User.

%% get_email('$email', Opts) -> pget(email, Opts);
%% get_email(EMail, _Dt)     -> EMail.

get_password('$password', _Dt) -> get_password();
get_password(Password, _Dt)    -> Password.

run_perms({control, C}, Opts) ->
    auth_srv:add_controls(pget(host, Opts),  lget(list, C),
                          lget(page, C),     lget(perms, C),
                          lget(override, C), lget(views, C));

run_perms({perm, P}, Opts) ->
    auth_srv:add_perm(pget(host, Opts), lget(list, P),
                      lget(page, P),    lget(perms, P));

run_perms({views, V}, Opts)    ->
    auth_srv:add_views(pget(host, Opts),
                       lget(list, V),     lget(page, V),
                       lget(override, V), lget(views, V));

run_perms({default, D}, Opts)  ->
    auth_srv:add_default(pget(host, Opts),
                         lget(page, D), lget(default, D)).

run_script({Path, '$email'}, Opts) ->
    run_script2(Path, pget(email, Opts), Opts);
run_script({Path, '$username'}, Opts) ->
    run_script2(Path, pget(username, Opts), Opts);
run_script({Path, '$site'}, Opts) ->
    #refX{site=Site} = hn_util:parse_url(pget(host, Opts)),
    run_script2(Path, Site, Opts);
run_script({Path, '$subdomain'}, Opts) ->
    run_script2(Path, pget(subdomain, Opts), Opts);
run_script({Path, '$expiry'}, Opts) ->
    {Date, _Time} = calendar:now_to_datetime(now()),
    NewDays = calendar:date_to_gregorian_days(Date) + 31,
    NewDate = calendar:gregorian_days_to_date(NewDays),
    Expr = "This site will expire on "
        ++ dh_date:format("D d M Y", {NewDate, {0, 0, 0}}),
    run_script2(Path, Expr, Opts);
run_script({Path, '$password'}, Opts) ->
    run_script2(Path, pget(password, Opts), Opts);
run_script({Path, Expr}, D) ->
    run_script2(Path, Expr, D).

run_script2(Path, Expr, Opts) ->
    RefX = hn_util:parse_url(pget(host, Opts)++Path), 
    hn_db_api:write_attributes([{RefX, [{"formula", Expr}]}]).

get_password() -> 
    N1 = gen_server:call(random_srv, {random, int, ?NO_OF_WORDS}),
    N2 = gen_server:call(random_srv, {random, int, ?NO_OF_WORDS}),
    N3 = gen_server:call(random_srv, {random, int, 99}),
    {value, {N1, W1}} = lists:keysearch(N1, 1, ?WORDS),
    {value, {N2, W2}} = lists:keysearch(N2, 1, ?WORDS),
    W1 ++ "!" ++ W2 ++ integer_to_list(N3).

pget(Key, List) ->
    proplists:get_value(Key, List, undefined).

lget(Key, List) ->
    element(2, lists:keyfind(Key, 1, List)).
