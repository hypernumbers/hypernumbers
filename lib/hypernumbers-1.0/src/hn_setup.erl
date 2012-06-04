-module(hn_setup).

-export([
         copy_site/2,
         site/3,
         site/4,
         delete_site/1,
         update/0,
         update/1,
         update/3,
         site_exists/1,
         get_sites/0,
         get_site_type/1,
         create_path_from_name/2,
         is_path/1,
         tables/0
        ]).

-export([
         import_json_DEBUG/2
        ]).

-include("spriki.hrl").
-include("hypernumbers.hrl").
-include("keyvalues.hrl").

%% copies a site from one domain to another
-spec copy_site(string(), string()) ->
    ok | {error, from_site_exists} | {error, to_site_doesnt_exist}.
copy_site(From, To) ->
    case hn_setup:site_exists(To) of
        true ->
            {error, to_site_exists};
        false ->
            case hn_setup:site_exists(From) of
                false ->
                    {error, from_site_doesnt_exist};
                true ->
                    site(To, blank, []),
                    ok = sitemaster_sup:delete_site(From),
                    ok = sitemaster_sup:delete_site(To),
                    copy_tables(From, To),
                    ok = sitemaster_sup:add_site(From),
                    ok = sitemaster_sup:add_site(To)
            end
    end.

copy_tables(From, To) ->
    Tables = [X || {X, _, _, _, _, _} <- tables()],
    ok = copy_t2(Tables, From, To).

copy_t2([], _, _) -> ok;
copy_t2([H | T], From, To) ->
    io:format("~nCopying ~p from ~p to ~p~n", [H, From, To]),
    TblFrom = new_db_wu:trans(From, H),
    TblTo = new_db_wu:trans(To, H),
    Fun1 = fun() ->
                   Fun2 = fun(X, N) ->
                                  io:format("."),
                                  case N rem 80 of
                                      0 -> io:format("~n");
                                      _ -> ok
                                  end,
                                  mnesia:write(TblTo, X, write),
                                  N + 1
                          end,
                   mnesia:foldl(Fun2, 1, TblFrom)
           end,
    mnesia:activity(transaction, Fun1),
    copy_t2(T, From, To).

%% Setup a new site from scratch
-spec site(string(), atom(), [{atom(), any()}]) -> ok | exists.
site(Site, Type, Opts) when is_list(Site), is_atom(Type) ->
    site(Site, Type, Opts, [corefiles, sitefiles, json, groups,
                            permissions, script]).

-spec site(string(), atom(), [{atom(), any()}], [atom()]) ->
    {error, site_exists} | {initial_view, string()} .
site(Site, Type, Opts, ToLoad) when is_list(Site), is_atom(Type) ->
    case hn_setup:site_exists(Site) of
        true ->
            {error, site_exists};
        false ->
            error_logger:info_msg("Setting up: ~p as ~p~n", [Site, Type]),
            ok = create_site_tables(Site, Type),
            ok = create_blank_z_and_infs(Site),
            ok = create_blank_pages(Site),
            ok = init_telephony(Site),
            ok = create_userfiles(Site),
            ok = sitemaster_sup:add_site(Site),
            ok = update(Site, Type, Opts, ToLoad),
            get_initial_params(Site)
    end.

%% Delete a site
%% Todo: Does not currently remove DNS entries.
-spec delete_site(string()) -> ok.
delete_site(Site) ->
    Dest = code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:site_to_fs(Site) ++ "/",
    ok = sitemaster_sup:delete_site(Site),
    ok = hn_util:delete_directory(Dest),
    ok = mnesia:activity(transaction,
                         fun mnesia:delete/3,
                         [core_site, Site, write]),
    [ {atomic, ok} = mnesia:delete_table(new_db_wu:trans(Site, Table))
      || {Table, _F, _T, _I, _S, _C} <- tables()],
    [_Proto, [$/, $/ | Domain], _URL] = string:tokens(Site, ":"),
    [TLD, Dom | Subs] = lists:reverse(string:tokens(Domain, ".")),
    ZoneL = Dom ++ "." ++ TLD,
    Name = string:join(lists:reverse(Subs), "."),
    ok = hns:unlink_resource(ZoneL, Name).

%% Update all existing sites with default options
-spec update() -> ok.
update() ->
    update([], [corefiles]).

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
    update(Site, get_site_type(Site), Opaque, Opts).

-spec update(string(), atom(), list(), list()) -> ok.
update(Site, Type, Opaque, Opts) ->
    [ ok = setup(Site, Type, Opaque, X) || X <- Opts ],
    ok.

%% Quick and dirty test to see if a site exists
-spec site_exists(string()) -> boolean().
site_exists(Site) ->
    case mnesia:dirty_read(core_site, Site) of
        [_] -> true;
        []  -> false
    end.

-spec get_sites() -> [string()].
get_sites() ->
    mnesia:activity(transaction, fun mnesia:all_keys/1, [core_site]).

-spec get_site_type(string()) -> atom().
get_site_type(Site) ->
    [#core_site{type=Type}] = mnesia:dirty_read(core_site, Site),
    Type.

-spec setup(string(), atom(), list(), atom()) -> ok.
setup(Site, _Type, _Opts, corefiles) ->
    ok = filelib:ensure_dir(sitedir(Site)),
    % first copy over the standard type
    ok = hn_util:recursive_copy(coreinstalldir(), sitedir(Site));
setup(Site, Type, _Opts, sitefiles) ->
    ok = filelib:ensure_dir(sitedir(Site)),
    % first copy over the standard type
    ok = hn_util:recursive_copy(moddir(Type), sitedir(Site));
setup(Site, _Type, _Opts, batch) ->
    ok = batch_import(Site);
setup(Site, Type, _Opts, json) ->
    ok = import_json(Site, moddir(Type));
setup(Site, _Type, Opts, groups) ->
    case file:consult([sitedir(Site),"/","groups.script"]) of
        {ok, Terms} -> Terms2 = [group_transform(T, Opts) || T <- Terms],
                       ok = hn_groups:load_script(Site, Terms2);
        % file doesn't exist is an OK error - it is a valid
        % condition so don't crash - other file opening errors
        % should fail
        {error, enoent} -> ok
    end;
setup(Site, _Type, _Opts, permissions) ->
    case file:consult([sitedir(Site),"/","permissions.script"]) of
        {ok, Terms} -> ok = auth_srv:load_script(Site, Terms);
        % file doesn't exist is an OK error - it is a valid
        % condition so don't crash - other file opening errors
        % should fail
        {error, enoent} -> ok
    end;
setup(Site, _Type, Opts, script) ->
    case file:consult([sitedir(Site),"/","setup.script"]) of
        {ok, Terms} -> [ok = run_script(T, Site, Opts) || T <- Terms],
                       ok;
        {error, enoent} -> ok
    end.

get_initial_params(Site) ->
    case file:consult([sitedir(Site), "/", "provision.script"]) of
         {ok, [{initial_view, I}]} -> {initial_view, I};
         {error, enoent}           -> {initial_view, []}
     end.

%% -spec resave_views() -> ok.
%% resave_views() ->
%%     ViewsPath = "/../../var/sites/*/docroot/views/*/*/*.meta",
%%     [ resave_view(X)
%%       || X <- filelib:wildcard(code:lib_dir(hypernumbers) ++ ViewsPath) ],
%%     ok.

%% -spec resave_view(string()) -> ok.
%% resave_view(Path) ->
%%     [FileName, User, Type, "views", "docroot", Site | _ ]
%%         = lists:reverse(string:tokens(Path, "/")),
%%     [Domain, Port] = string:tokens(Site, "&"),
%%     NSite          = "http://"++Domain++":"++Port,
%%     ViewName     = [Type, "/", User, "/", filename:basename(FileName, ".meta")],
%%     {ok, [Data]} = file:consult(Path),
%%     ok           = hn_mochi:save_view(NSite, ViewName, Data).

-spec create_blank_pages(string()) -> ok.
create_blank_pages(Site) ->
    Key = ?pages,
    Value = dh_tree:new(),
    new_db_api:write_kv(Site, Key, Value).

-spec create_blank_z_and_infs(string()) -> ok.
create_blank_z_and_infs(Site) ->
    Key = ?zinf_tree,
    Value = gb_trees:empty(),
    new_db_api:write_kv(Site, Key, Value).

-spec create_site_tables(string(), atom()) -> ok.
create_site_tables(Site, Type)->
    %% Seems sensible to keep this restricted
    %% to disc_copies for now
    [ok = hn_db_admin:create_table(new_db_wu:trans(Site, N),
                                   N, F, S, T, true, I)
     || {N, F, T, I, S, _} <- tables()],
    Trans = fun() ->
                    mnesia:write(#core_site{site = Site, type = Type})
            end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

-define(TBL(N, T, I, S, C), {N, record_info(fields, N), T, I, S, C}).
tables() ->
    % setup local obj indices
    [
     ?TBL(api,               set, [],                  disc_copies,      cache),
     ?TBL(kvstore,           set, [],                  disc_only_copies, nocache),
     ?TBL(dirty_for_zinf,    set, [],                  disc_copies,      cache),
     ?TBL(dirty_zinf,        set, [],                  disc_copies,      cache),
     ?TBL(dirty_queue,       set, [],                  disc_copies,      cache),
     ?TBL(dirty_queue_cache, set, [],                  disc_copies,      cache),
     ?TBL(item,              set, [],                  disc_copies,      cache),
     ?TBL(local_obj,         set, [obj, path, revidx], disc_copies,      cache),
     ?TBL(del_local,         set, [],                  disc_copies,      cache),
     ?TBL(relation,          set, [],                  disc_copies,      cache),
     ?TBL(group,             set, [],                  disc_copies,      cache),
     ?TBL(style,             set, [idx],               disc_copies,      cache),
     ?TBL(form,              set, [id],                disc_copies,      cache),
     ?TBL(phone,             set, [],                  disc_copies,      cache),
     ?TBL(logging,           bag, [path],              disc_only_copies, nocache),
     ?TBL(include,           set, [path],              disc_copies,      cache),
     ?TBL(timer,             set, [],                  disc_copies,      cache),
     ?TBL(site,              set, [],                  disc_copies,      cache),
     ?TBL(siteonly,          set, [],                  disc_copies,      cache),
     ?TBL(user_fns,          set, [],                  disc_copies,      cache)
    ].

%% Import files on a batch basis
batch_import(Site) ->
    Files = code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:site_to_fs(Site) ++ "/import/*.csv",
    OverWild    = filelib:wildcard(Files ++ ".replace"),
    AppendWild  = filelib:wildcard(Files ++ ".append"),
    OverFiles   = lists:filter(fun is_path/1, OverWild),
    AppendFiles = lists:filter(fun is_path/1, AppendWild),
    [ ok = hn_import:csv_file(Site ++ create_path_from_name(X, ".csv.replace"), X) || X <- OverFiles],
    [ ok = hn_import:csv_append(Site ++ create_path_from_name(X, ".csv.append"), X) || X <- AppendFiles],
    ok.

import_json_DEBUG(Site, Dir) ->
    import_json(Site, Dir).

%% Import a set of json files into the live spreadsheet
import_json(Site, Dir) ->
    Files = filelib:wildcard(Dir++"/data/*.json"),
    JsonFiles = lists:filter(fun is_path/1, Files),
    [ ok = hn_import:json_file(Site ++ create_path_from_name(Json, ".json"),
                               Json)
      || Json <- JsonFiles],
    ok.

is_path(L) ->
    Tokens = string:tokens(L, "/"),
    File = hd(lists:reverse(Tokens)),
    case string:tokens(File, ".") of
        ["path" | _T] -> true;
        _Other        -> false
    end.

create_path_from_name(Name, FileType) ->
    [ "path" | Rest ]
        = string:tokens(filename:basename(Name, FileType), "."),
    hn_util:list_to_path(Rest).

-spec group_transform(tuple(), [tuple()]) -> [tuple()].
group_transform({add_user, Terms}, Opts) ->
    {add_user, substitute('$creator', pget(creator, Opts), Terms)};
group_transform(X, _Opts) ->
    X.

-spec run_script(tuple(), string(), [tuple()]) -> ok.
run_script({Path, '$email'}, Site, Opts) ->
    write_cell(Path, Site, pget(email, Opts));
run_script({Path, '$name'}, Site, Opts) ->
    write_cell(Path, Site, pget(name, Opts));
run_script({Path, '$site'}, Site, _Opts) ->
    write_cell(Path, Site, Site);
run_script({Path, Var}, Site, _Opts) when is_atom(Var) ->
    Expr = lists:flatten(io_lib:format("no binding for '~s'", [Var])),
    write_cell(Path, Site, Expr);
run_script({Path, Expr}, Site, _Opts) ->
    write_cell(Path, Site, Expr).

write_cell(Path, Site, Expr) ->
    RefX = hn_util:url_to_refX(Site++Path),
    new_db_api:write_attributes([{RefX, [{"formula", Expr}]}]).

substitute(Var, Val, Var) ->
    Val;
substitute(Var, Val, Terms) when is_list(Terms) andalso
                                 not(is_integer(hd(Terms))) ->
    [substitute(Var, Val, T) || T <- Terms];
substitute(Var, Val, Term) when is_tuple(Term) ->
    list_to_tuple(substitute(Var, Val, tuple_to_list(Term)));
substitute(_Var, _Val, Term) ->
    Term.

%% add_u(Site, User, {champion, C}) ->
%%     UserName = hn_users:name(User),
%%     Path     = replace('$user', UserName, pget(path, C)),
%%     auth_srv:set_champion(Site, Path, pget(view, C));

%% add_u(Site, User, {add_view, C}) ->
%%     UserName = hn_users:name(User),
%%     Perms    = replace('$user', UserName, pget(perms, C)),
%%     Path     = replace('$user', UserName, pget(path, C)),
%%     auth_srv:add_view(Site, Path, Perms, pget(view, C));

%% add_u(Site, User, {import, P}) ->
%%     UserName = hn_users:name(User),
%%     P2 = replace('$user', "$user", pget(path, P)),
%%     Path = replace('$user', UserName, pget(path, P)),
%%     FileName = re:replace(hn_util:path_to_json_path(P2), "^path.",
%%                           "template.", [{return, list}]),
%%     Dest = code:lib_dir(hypernumbers) ++ "/../../var/sites/"
%%         ++ hn_util:site_to_fs(Site) ++ "/data/",
%%     Url = Site ++ hn_util:list_to_path(Path),
%%     ok = hn_import:json_file(Url, Dest ++ FileName).

-spec coreinstalldir() -> string().
coreinstalldir() ->
    code:priv_dir(hypernumbers) ++ "/core_install".

-spec moddir(atom()) -> string().
moddir(Type) ->
    code:priv_dir(hypernumbers) ++ "/site_types/" ++ atom_to_list(Type).

-spec sitedir(string()) -> string().
sitedir(Site) ->
    code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:site_to_fs(Site) ++ "/".

pget(Key, List) ->
    pget(Key, List, "").
pget(Key, List, Default) ->
    proplists:get_value(Key, List, Default).

init_telephony(Site) ->
    AccSid = "AC7a076e30da6d49119b335d3a6de43844",
    AuthTk = "9248c9a2a25f6914fad9c9fb5b30e69c",
    AppSid = "AP93d273f3cc624008805842376d561bed",
    AC = #twilio_account{account_sid     = AccSid,
                         auth_token      = AuthTk,
                         application_sid = AppSid,
                         site_phone_no   = "+441315101897",
                         type            = outbound},
    new_db_api:write_kv(Site, ?twilio, AC).

create_userfiles(Site) ->
    filelib:ensure_dir(hn_util:userfilesroot(Site) ++ "force").
