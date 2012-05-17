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

-export([export/1, export/2,
         import/1, import/2,
         export_as_sitetype/2,
         export_as_sitetype/3
         ]).

-define(join, filename:join).

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
    ok = load_etf(Site, SiteSrc),
    ok = load_groups(Site, SiteSrc),
    ok = load_views(Site, SiteSrc),
    ok = load_perms(Site, SiteSrc).

load_users(Src) ->
    {ok, UserTs} = file:consult(?join(Src, "users.export")),
    ok = passport:load_script(UserTs).

load_etf(Site, SiteSrc) ->
    Files = filelib:wildcard(?join([SiteSrc, "etf"])++"/*.json"),
    [ ok = hn_import:json_file(Site ++ hn_setup:create_path_from_name(Json, ".json"),
                               Json)
      || Json <- Files, hn_setup:is_path(Json) ],
    ok.

load_groups(Site, SiteSrc) ->
    {ok, GroupTs} = file:consult(?join(SiteSrc, "groups.export")),
    ok = hn_groups:load_script(Site, GroupTs).

load_views(Site, SiteSrc) ->
    SiteFs = hn_util:site_to_fs(Site),
    Source = ?join([SiteSrc, "views"]),
    Dest   = ?join(["var", "sites", SiteFs, "views"]),
    hn_util:recursive_copy(Source, Dest).

load_perms(Site, SiteSrc) ->
    {ok, Perms} = file:consult(?join(SiteSrc, "permissions.export")),
    auth_srv:load_script(Site, Perms).

