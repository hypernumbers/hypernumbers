%%%------------------------------------------------------------------
%%% @author     Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       code to manage backup and restore etc as well
%%%            as 'grabbing' sites and applications
%%%
%%% @end
%%% Created : 18 Dec 2009 by Gordon Guthrie gordon@hypernumbers.com
%%%
%%%-------------------------------------------------------------------
-module(hn_archive).

-include("spriki.hrl").
-include("hypernumbers.hrl").
-include("hn_mochi.hrl").

-export([
         export/1,
         export/2,
         import/1,
         import/2,
         export_as_sitetype/2,
         export_as_sitetype/3,
         write_backup/0,
         convert_external_backup/2,
         restore_external_backup/2,
         restore_local_backup/1,
         restore_site/2,
         restore_site/3
         ]).

-define(join, filename:join).
-define(ext, ".mnesia.backup").
-define(site, "site").
-define(site_dirs, ["logs", "mochilog", "runerl_log"]).

-spec restore_external_backup(list(), atom()) -> ok.
restore_external_backup(Name, FromNode) ->
    ok = convert_external_backup(Name, FromNode),
    ok = restore_local_backup(Name).

convert_external_backup(Name, FromNode) ->
    ERoot = external_backup_dir(Name),
    IRoot = backup_dir(Name),
    ToNode = node(),
    {Sites, _DB} = unpack(ERoot),
    io:format("~n***********Conversion Starts****************************~n"),
    io:format("Converting backup ~p~n- from ~p~n-   to ~p~n- at   ~p ~p~n",
              [Name, FromNode, ToNode, date(), time()]),

    Source = ERoot ++ "db/" ++ Name ++ ?ext,
    Target = IRoot ++ "db/" ++ Name ++ ?ext,
    filelib:ensure_dir(Target),
    {ok, switched} = hn_db_convert:change_node_name(FromNode, ToNode,
                                                    Source, Target),
    io:format("Backup ~p converted from ~p to ~p~n",
              [Name, FromNode, ToNode]),
    io:format("Backup copied from var/external_backups/ to var/backups/~n"),

    % now copy over the collateral
    [ok = copy_external_collateral(Name, X) || X <- Sites],

    % finally copy over the server stuff
    ok = copy_external_server(Name),

    % and then simply restore the new local backup
    io:format("***********Conversion Ends*******************************~n"),
    ok.

copy_external_server(Name) ->
    Src = external_backup_dir(Name),
    Dest = backup_dir(Name),
    % restore the site dirs (mostly logs)
    [ok = hn_util:recursive_copy(Src ++ X, Dest ++ X) || X <- ?site_dirs],

    % now restore the sys.config
    {ok, _} = file:copy(Src ++ "sys.config." ++ Name,
                        Dest ++ "sys.config." ++ Name),
    io:format("Server for ~p copied over~n", [Name]),
    ok.

copy_external_collateral(Name, Site) ->
    ERoot = external_backup_dir(Name),
    IRoot = backup_dir(Name),
    Site2 = hn_util:site_to_fs(Site),
    % copy collateral across
    hn_util:recursive_copy(ERoot ++ Site2 ++ "/",
                           IRoot ++ Site2 ++ "/"),
    io:format("Collateral for ~p copied over from external backup ~p~n",
              [Site, Name]),
  ok.

restore_site(Name, Site) ->
    restore_site(Name, Site, Site).

restore_site(Name, ToSite, FromSite) ->
    {Sites, DB} = unpack(backup_dir(Name)),
    io:format("Sites is ~p~n", [Sites]),
    io:format("~n***********Restoration Starts***************************~n"),
    io:format("Starting restore of ~p as ~p~n- from ~p~n- at ~p ~p~n",
              [FromSite, ToSite, Name, date(), time()]),
    case hn_setup:site_exists(ToSite) of
        false -> hn_setup:site(ToSite, blank, []);
        true  -> ok
    end,
    case lists:member(FromSite, Sites) of
        false ->
            {error, from_site_not_in_backup};
        true  ->
            Backup = DB ++ Name ++ ?ext,
            {ok, restored} = hn_db_convert:restore_site(Backup, FromSite, ToSite),
            io:format("Restoration finished~n- at ~p ~p~n", [date(), time()]),
            io:format("~n***********Restoration Ends*****************************~n"),
            ok
    end.

-spec restore_local_backup(list()) -> ok.
restore_local_backup(Name) ->
    {Sites, DB} = unpack(backup_dir(Name)),
    io:format("~n***********Restoration Starts***************************~n"),
    io:format("Starting restore of ~p~n- at ~p ~p~n", [Name, date(), time()]),

    % stop everything
    application:stop(hypernumbers),
    application:stop(mnesia),

    % now restore the database
    hn_db_admin:restore(DB, Name ++ ?ext),
    io:format("Database restored from ~p~n", [Name]),

    % now restore the collateral
    [restore_collateral(Name, X) || X <- Sites],
    io:format("Finishing restore of ~p~n- at ~p ~p~n", [Name, date(), time()]),

    % finally restore server stuff
    ok = restore_server(Name),
    io:format("Server restored from ~p~n", [Name]),

    % restart everything
    application:start(mnesia),
    application:start(hypernumbers),
    io:format("***********Restoration Ends*****************************~n"),
    ok.

-spec restore_collateral(list(), list()) -> ok.
restore_collateral(Name, Site) ->
    io:format("Restoring ~p from ~p~n- at ~p ~p~n",
              [Site, Name, date(), time()]),
    Dir = backup_dir(Name),
    Src = Dir ++ hn_util:site_to_fs(Site) ++ "/",

    Dest = existing_site_dir(Site),

    % first delete the old version
    hn_util:delete_directory(Dest),
    filelib:ensure_dir(Dest ++ "junk.txt"),

    % then copy the new versions
    copy_(Src, Site),
    io:format("~p restored...~n", [Site]),
    ok.

unpack(Dir) ->
    [Name | Rid ] = lists:reverse(string:tokens(Dir, "/")),
    Dir2 = "/" ++ string:join(lists:reverse(Rid), "/") ++ "/",
    % now get a list of all the sites in the backup
    Files = filelib:wildcard(Dir2 ++ Name ++ "/*"),
    Dirs = [X || X <- Files, filelib:is_dir(X) == true],

    % get the db
    DB = Dir2 ++ Name ++ "/" ++ "db",
    SiteDirs = [Dir2 ++ Name ++ "/" ++ X || X <- ?site_dirs],
    Fun1 = fun(X, Acc) ->
                  lists:delete(X, Acc)
          end,
    Dirs2 = lists:foldl(Fun1, Dirs, [DB | SiteDirs]),

    % get the sites from the dirs
    Fun2 = fun(X, Acc) ->
                  [Site | _Rest] = lists:reverse(string:tokens(X, "/")),
                  [hn_util:site_from_fs(Site) | Acc]
          end,
    Sites = lists:foldl(Fun2, [], Dirs2),
    {Sites, DB ++ "/"}.

% writes a backup
-spec write_backup() -> ok.
write_backup() ->
    io:format("~n***********Backup Starts********************************~n"),
    Node = hn_util:node_to_fs(node()),
    Name = Node ++ "." ++ dh_date:format("Y_m_d_H_i_s"),
    Sites = hn_setup:get_sites(),

    io:format("Backing up ~p to ~p~n", [node(), Name]),

    % backup the sites collateral
    [ok = backup_collateral(Name, X) || X <- Sites],

    % now backup other server stuff
    ok = backup_server(Name),

    Dir = backup_dir(Name),

    % before starting on the db take it out of memory
    [ok = hn_db_admin:disc_only(X) || X <- Sites],

    backup_db(Dir ++ "/db/", Name),

    % now restore the database to memory (mebbies)
    {ok, Cache} = application:get_env(hypernumbers, site_cache_mode),
    site_to_memory(Sites, Cache),
    io:format("~n***********Backup Ends**********************************~n"),
    Name.

restore_server(Name) ->
    Src = backup_dir(Name),
    Dest = existing_server_root(),

    % delete the existing server stuff
    [ok = hn_util:delete_directory(Dest ++ X) || X <- ?site_dirs],
    % now make the directories again, lol!
    [filelib:ensure_dir(Dest ++ X) || X <- ?site_dirs],

    % restore the site dirs (mostly logs)
    [ok = hn_util:recursive_copy(Src ++ X, Dest ++ X) || X <- ?site_dirs],

    % now restore the sys.config
    {ok, _} = file:copy(Src ++ "sys.config." ++ Name,
                        Dest ++ "sys.config." ++ Name),
    io:format("NOTE: sys.config restored as sys.config." ++ Name ++ "~n"
              ++ "      Needs manual rename and restart (if appropriate)~n"),
    ok.

backup_server(Name) ->
    Src = existing_server_root(),
    Dest = backup_dir(Name),

    % back up the site dirs (mostly logs)
    [ok = hn_util:recursive_copy(Src ++ X, Dest ++ X) || X <- ?site_dirs],

    % now backup the sys.config
    {ok, _} = file:copy(Src ++ "sys.config", Dest ++ "sys.config." ++ Name),
    io:format("Server backed up to ~p~n", [Name]),
    ok.

site_to_memory([], _Cache) ->
    ok;
site_to_memory([Site | T], Cache) ->
    case {Cache, dbsrv:is_busy(Site)} of
        {true, true} ->
            % if it should be cached but if its busy reload into memory
            ok = hn_db_admin:disc_and_mem(Site);
        {true, false} ->
            % don't reload
            ok;
        {false, _} ->
            % if it shouldn't be cached reload into memory
            ok = hn_db_admin:disc_and_mem(Site)
    end,
    site_to_memory(T, Cache).

backup_collateral(Name, Site) ->
    % setup the backup
    io:format("Starting backup of ~p~n- to ~p~n- at ~p ~p~n",
              [Site, Name, date(), time()]),
    Dir = backup_dir(Name) ++ hn_util:site_to_fs(Site) ++ "/",

    % make sure the dir exists
    filelib:ensure_dir(Dir ++ "/junk.txt"),

    % now backup the site
    io:format("about to backup views~n"),
    ok = dump_folder(Site, Dir, "views"),
    io:format("about to backup docroot~n"),
    ok = dump_folder(Site, Dir, "docroot"),
    io:format("about to backup docroot~n"),
    ok = dump_folder(Site, Dir, "docroot"),
    io:format("about to backup templates~n"),
    ok = dump_folder(Site, Dir, "templates"),
    io:format("about to backup etl files~n"),
    ok = dump_folder(Site, Dir, "etl"),
    io:format("backup of the collateral of ~p completed~n", [Site]),

    % finish up
    io:format("Finishing backup of ~p~n- to ~p~n- at ~p ~p~n",
              [Site, Name, date(), time()]),
    ok.

backup_db(Dir, Name) ->
    Tables = mnesia:system_info(tables),
    ok = hn_db_admin:backup(Tables, Dir, Name ++ ?ext),
    io:format("Database backed up to ~p~n", [Name]),
    ok.

%% Exports a site as a sitemod that can be loaded by a factory
-spec export_as_sitetype(list(), atom()) -> ok.
export_as_sitetype(Site, NewType) ->
    export_as_sitetype(Site, NewType, "admin").

export_as_sitetype(Site, NewType, open) ->
    export_as_sitetype(Site, NewType, "admin");
export_as_sitetype(Site, NewType, Group) when is_list(Group) ->
    SiteTypes = ?join([code:priv_dir(hypernumbers), "site_types"]),
    Dest = ?join([SiteTypes, NewType]),
    ok = export_site(Dest, Site),
    ok = hn_util:recursive_copy(?join([Dest,"etf"]), ?join([Dest, "data"])),

    %% Rename files
    file:rename(?join([Dest, "groups.export"]),
                ?join([Dest, "groups.script"])),
    ok = post_process_groups(?join([Dest, "groups.script"]), Group),
    file:rename(?join([Dest, "permissions.export"]),
                ?join([Dest, "permissions.script"])),

    %% Delete backup-centric artifacts.
    io:format("about to clean up and delete views, docroots and etf folders~n"),
    [hn_util:delete_directory(?join([SiteTypes, NewType, ?join(Dir)]))
      || Dir <- [["etf"], ["views"], ["docroot"]]],
    ok.

post_process_groups(File, Group) ->
    {ok, Groups} = file:consult(File),
    StrippedGroups = [X || X <- Groups, element(1, X) =/= add_user],
    NewGroups = lists:append(StrippedGroups, [{add_user, [{uid, '$creator'},
                                                          {group, Group}]}]),
    NewG2 = lists:flatten([io_lib:format("~p.~n", [X]) || X <- NewGroups]),
    ok = file:write_file(File, NewG2).

%% Export sites to the given directory.
export(Dest) ->
    export_services(?join([Dest, "services"])),
    export(Dest, hn_setup:get_sites()).

%% Export the provided subset of sites to the destination directory.
%% Sites are expected in leading protocol form.
export(Dest, Sites) ->
    [ ok = export_site(?join([Dest, hn_util:site_to_fs(S)]), S)
      || S <- Sites, hn_setup:site_exists(S)],
    ok.

export_services(Dest) ->
    filelib:ensure_dir([Dest,"/"]),
    ok = dump_users(Dest).

export_site(Dest, Site) ->
    filelib:ensure_dir([Dest,"/"]),
    io:format("about to dump the etf~n"),
    ok = dump_etf(Site, Dest),
    io:format("about to dump groups~n"),
    ok = dump_groups(Site, Dest),
    io:format("about to dump perms~n"),
    ok = dump_perms(Site, Dest),
    io:format("about to dump views~n"),
    ok = dump_folder(Site, Dest, "views"),
    io:format("about to dump docroot~n"),
    ok = dump_folder(Site, Dest, "docroot"),
    io:format("about to dump templates~n"),
    ok = dump_folder(Site, Dest, "templates"),
    io:format("about to dump etl files~n"),
    ok = dump_folder(Site, Dest, "etl"),
    io:format("export site completed~n"),
    ok.

dump_users(Dest) ->
    Users = passport:dump_script(),
    ok = file:write_file(?join(Dest, "users.export"), Users).

dump_etf(Site, SiteDest) ->
    EtfDest = ?join(SiteDest, "etf"),
    filelib:ensure_dir([EtfDest,"/"]),
    Ref = hn_util:url_to_refX(Site),
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    [ok = dump_page(EtfDest, Encoder, Ref, Path)
     || Path <- page_srv:get_flatpages(Site)],
    ok.

dump_page(EtfDest, Encoder, Ref, Path) ->
    io:format("Dumping ~p~n", [Path]),
    FileName = ?join(EtfDest, hn_util:path_to_json_path(Path)),
    Page = Encoder(hn_mochi:page_attrs_for_export(Ref#refX{path = Path}, #env{})),
    Data = io_lib:format("~s", [lists:flatten(Page)]),
    file:write_file(FileName, Data).

dump_groups(Site, SiteDest) ->
    Groups = hn_groups:dump_script(Site),
    ok = file:write_file(?join(SiteDest, "groups.export"), Groups).

dump_perms(Site, SiteDest) ->
    Perms = auth_srv:dump_script(Site),
    ok = file:write_file(?join(SiteDest, "permissions.export"),
                         Perms).

dump_folder(Site, SiteDest, Folder) ->
    Dest = ?join([SiteDest, Folder]),
    SiteFs   = hn_util:site_to_fs(Site),
    Source   = ?join([code:priv_dir(hypernumbers), "../../../", "var", "sites",
                      SiteFs, Folder]),
    % sometimes the folder might not exists, so ensure it
    % add the junk file name 'cos you are only ensuring the directories exist...
    ok = filelib:ensure_dir(Source ++ "/junk.txt"),
    ok = filelib:ensure_dir(Dest ++ "/junk.txt"),
    hn_util:recursive_copy(Source, Dest).

%% Import all sites found in the given directory.
import(Src) ->
    ServSrc = ?join(Src, "services"),
    case filelib:is_dir(ServSrc) of
        true -> import_services(ServSrc);
        false -> ok
    end,
    Sites = [hn_util:site_from_fs(F) ||
                F <- filelib:wildcard("*", Src),
                hd(F) /= ".",
                F /= "services",
                filelib:is_dir(?join(Src,F))],
    import(Src, Sites).
%% Import the subset of sites found within the source directory.
%% Sites are expected in leading protocol form.
import(Src, Sites) ->
    [ok = import_site(Src, S) || S <- Sites],
    ok.

import_services(ServSrc) ->
    ok = load_users(ServSrc).

import_site(Src, Site) ->
    SiteSrc = ?join([Src, hn_util:site_to_fs(Site)]),
    hn_setup:site(Site, blank, [], [corefiles, sitefiles]),
    io:format("Importing ~p~n", [Site]),
    copy_(SiteSrc, Site),
    import_(SiteSrc, Site).

copy_(SiteSrc, Site) ->
    ok = copy_etf(Site, SiteSrc),
    io:format("etf copied...~n"),
    ok = copy2(Site, SiteSrc, "views"),
    io:format("views copied...~n"),
    ok = copy2(Site, SiteSrc, "docroot"),
    io:format("docroot copied...~n"),
    ok = copy2(Site, SiteSrc, "templates"),
    io:format("templates copied...~n"),
    ok = copy2(Site, SiteSrc, "etl"),
    io:format("maps copied...~n").

import_(SiteSrc, Site) ->
    ok = load_groups(Site, SiteSrc),
    io:format("groups loaded...~n"),
    ok = load_perms(Site, SiteSrc),
    io:format("perms loaded...~n"),
    ok.

load_users(Src) ->
    {ok, UserTs} = file:consult(?join(Src, "users.export")),
    ok = passport:load_script(UserTs).

-define(create, hn_setup:create_path_from_name).
copy_etf(Site, SiteSrc) ->
    Files = filelib:wildcard(?join([SiteSrc, "etf"])++"/*.json"),
    [ok = hn_import:json_file(Site ++ ?create(Json, ".json"))
      || Json <- Files, hn_setup:is_path(Json) ],
    ok.

load_groups(Site, SiteSrc) ->
    {ok, GroupTs} = file:consult(?join(SiteSrc, "groups.export")),
    ok = hn_groups:load_script(Site, GroupTs).

copy2(Site, SiteSrc, Type) ->
    SiteFs = hn_util:site_to_fs(Site),
    Source = ?join([SiteSrc, Type]),
    Dest   = ?join(["var", "sites", SiteFs, Type]),
    % sometimes the source and dest files don't exist
    filelib:ensure_dir(Source ++ "/junk.file"),
    filelib:ensure_dir(Dest ++ "/junk.file"),
    hn_util:recursive_copy(Source, Dest).

load_perms(Site, SiteSrc) ->
    {ok, Perms} = file:consult(?join(SiteSrc, "permissions.export")),
    auth_srv:load_script(Site, Perms).

external_backup_dir(Name) ->
    Root = code:lib_dir(hypernumbers),
    Root ++ "/../../var/external_backups/" ++ Name ++ "/".

backup_dir(Name) ->
    Root = code:lib_dir(hypernumbers),
    Root ++ "/../../var/backups/" ++ Name ++ "/".

existing_site_dir(Site) ->
    Root = code:lib_dir(hypernumbers),
    Root ++ "/../../var/sites/" ++  hn_util:site_to_fs(Site) ++ "/".

existing_server_root() ->
    Root = code:lib_dir(hypernumbers),
    Root ++ "/../../var/".

