%%%-------------------------------------------------------------------
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
         write_backup/1,
         write_backup/2,
         restore_backup/1,
         restore_backup/2
         ]).

-define(join, filename:join).

-spec restore_backup(list()) -> ok.
restore_backup(Dir) ->
    [Name | Rid ] = lists:reverse(string:tokens(Dir, "/")),
    Dir2 = "/" ++ string:join(lists:reverse(Rid), "/") ++ "/",
    io:format("Restoring ~p from ~p~n", [Name, Dir2]),
    % now get a list of all the sites in the backup
    Files = filelib:wildcard(Dir2 ++ Name ++ "/*"),
    Dirs = [X || X <- Files, filelib:is_dir(X) == true],

    % got to handle services differently (if they exist)
    Services = Dir2 ++ Name ++ "/" ++ "services",
    Dirs2 = case lists:member(Services, Dirs) of
                true  -> restore_services(Services ++ "/", Name),
                         lists:delete(Services, Dirs);
                false -> Dirs
            end,

    % get the sites from the dirs
    Fun = fun(X, Acc) ->
                  [Site | _Rest] = lists:reverse(string:tokens(X, "/")),
                  [hn_util:site_from_fs(Site) | Acc]
          end,
    Sites = lists:foldl(Fun, [], Dirs2),
    [restore_backup(Dir, X) || X <- Sites],
    ok.

-spec restore_backup(list(), list()) -> ok.
restore_backup(Dir, Site) ->
    [Name | _Rid ] = lists:reverse(string:tokens(Dir, "/")),
    case hn_setup:site_exists(Site) of
        false -> hn_setup:site(Site, blank, []);
        true  -> ok
    end,
    RestFile = Dir ++ "/" ++ hn_util:site_to_fs(Site) ++ "/",
    ok = hn_db_admin:restore(RestFile, Name),
    io:format("Database restored for ~p~n", [Site]),
    restore_site(Dir ++ "/" ++ hn_util:site_to_fs(Site) ++ "/", Site).

restore_services(Services, Name) ->
    hn_db_admin:restore(Services, Name).

restore_site(Src, Site) ->
    import_(Src, Site),
    io:format("~p restored...~n", [Site]),
    ok.

% writes a backup
-spec write_backup(list()) -> ok.
write_backup(Name) ->
    Sites = hn_setup:get_sites(),
    [ok = write_backup(Name, X) || X <- Sites],
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/backups/" ++ Name ++ "/",
    backup_services(Dir ++ "/services/", Name).

write_backup(Name, Site) ->
    % setup the backup
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/backups/"
        ++ Name ++ "/" ++ hn_util:site_to_fs(Site) ++ "/",
    Tables = [new_db_wu:trans(Site, X) ||
                 {X, _, _, _, _, _} <- hn_setup:tables()],

    % drop the db to disc only first
    ok = hn_db_admin:disc_only(Site),

    % now write the mnesia backup
    ok = hn_db_admin:backup(Tables, Dir, Name),

    % now restore it to memory (mebbies)
    {ok, Cache} = application:get_env(hypernumbers, site_cache_mode),
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

    % now backup the site
    backup_site(Dir, Site).

backup_site(Dest, Site) ->
    io:format("about to backup groups~n"),
    ok = dump_groups(Site, Dest),
    io:format("about to backup perms~n"),
    ok = dump_perms(Site, Dest),
    io:format("about to backup views~n"),
    ok = dump_folder(Site, Dest, "views"),
    io:format("about to backup docroot~n"),
    ok = dump_folder(Site, Dest, "docroot"),
    io:format("about to backup templates~n"),
    ok = dump_folder(Site, Dest, "templates"),
    io:format("about to backup etl files~n"),
    ok = dump_folder(Site, Dest, "etl"),
    io:format("backup of ~p completed~n", [Site]),
    ok.

backup_services(Dir, Name) ->
    PossibleTables = [
                      core_site,
                      'service_hns_record',
                      'service_hns_resource',
                      'service_hns_zone',
                      'service_passport_user'
                     ],
    Tables = mnesia:system_info(tables),
    ServiceTables = [X || X <- Tables,
                          lists:member(X, PossibleTables) == true],
    ok = hn_db_admin:backup(ServiceTables, Dir, Name),
    io:format("Services backed up~n"),
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
    [ hn_util:delete_directory( ?join([SiteTypes, NewType, ?join(Dir)]) )
      || Dir <- [["etf"], ["views"], ["docroot"]] ],
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
    import_(SiteSrc, Site).

import_(SiteSrc, Site) ->
    ok = load_etf(Site, SiteSrc),
    io:format("etf loaded for ~p~n", [Site]),
    ok = load_groups(Site, SiteSrc),
    io:format("groups loaded for ~p~n", [Site]),
    ok = load_views(Site, SiteSrc),
    io:format("views loaded for ~p~n", [Site]),
    ok = load_templates(Site, SiteSrc),
    io:format("templates loaded for ~p~n", [Site]),
    ok = load_maps(Site, SiteSrc),
    io:format("maps loaded for ~p~n", [Site]),
    ok = load_perms(Site, SiteSrc).

load_users(Src) ->
    {ok, UserTs} = file:consult(?join(Src, "users.export")),
    ok = passport:load_script(UserTs).

-define(create, hn_setup:create_path_from_name).
load_etf(Site, SiteSrc) ->
    Files = filelib:wildcard(?join([SiteSrc, "etf"])++"/*.json"),
    [ok = hn_import:json_file(Site ++ ?create(Json, ".json"))
      || Json <- Files, hn_setup:is_path(Json) ],
    ok.

load_groups(Site, SiteSrc) ->
    {ok, GroupTs} = file:consult(?join(SiteSrc, "groups.export")),
    ok = hn_groups:load_script(Site, GroupTs).

load_views(Site, SiteSrc) ->
    load_(Site, SiteSrc, "views").

load_templates(Site, SiteSrc) ->
    load_(Site, SiteSrc, "templates").

load_maps(Site, SiteSrc) ->
    load_(Site, SiteSrc, "etl").

load_(Site, SiteSrc, Type) ->
    SiteFs = hn_util:site_to_fs(Site),
    Source = ?join([SiteSrc, Type]),
    Dest   = ?join(["var", "sites", SiteFs, Type]),
    hn_util:recursive_copy(Source, Dest).

load_perms(Site, SiteSrc) ->
    {ok, Perms} = file:consult(?join(SiteSrc, "permissions.export")),
    auth_srv:load_script(Site, Perms).

