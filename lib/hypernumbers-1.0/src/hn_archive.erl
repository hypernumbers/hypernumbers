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
         export_as_sitetype/2]).

%% Exports a site as a sitemod that can be loaded by a factory
-spec export_as_sitetype(list(), atom()) -> ok.
export_as_sitetype(Site, NewType) ->
    SiteTypes = join([code:priv_dir(hypernumbers), "site_types"]),
    Dest = join([SiteTypes, NewType]),
    ok = export_site(Dest, Site),
    ok = hn_util:recursive_copy(join([Dest,"etf"]), join([Dest, "data"])),

    %% Rename files
    file:rename(join([Dest, "groups.export"]), 
                join([Dest, "groups.script"])),
    file:rename(join([Dest, "permissions.export"]), 
                join([Dest, "permissions.script"])),

    %% Delete backup-centric artifacts.
    [ hn_util:delete_directory( join([SiteTypes, NewType, join(Dir)]) )
      || Dir <- [["etf"], ["views", "_g", "core"]] ],
    ok.
    
%% Export sites to the given directory.
export(Dest) ->
    export_services(join([Dest, "services"])),
    export(Dest, hn_setup:get_sites()).

%% Export the provided subset of sites to the destination directory.
%% Sites are expected in leading protocol form.
export(Dest, Sites) ->
    [ ok = export_site(join([Dest, hn_util:site_to_fs(S)]), S)
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
    ok = dump_views(Site, Dest),
    io:format("export site completed~n"),
    ok.

dump_users(Dest) ->
    Users = passport:dump_script(),
    ok = file:write_file(filename:join(Dest, "users.export"), Users).

dump_etf(Site, SiteDest) -> 
    EtfDest = filename:join(SiteDest, "etf"),
    filelib:ensure_dir([EtfDest,"/"]),  
    Ref = hn_util:parse_url(Site),
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    [ok = dump_page(EtfDest, Encoder, Ref, Path) 
     || Path <- hn_db_api:read_pages(Ref)],
    ok.

dump_page(EtfDest, Encoder, Ref, Path) ->
    FileName = filename:join(EtfDest, hn_util:path_to_json_path(Path)),
    Page = Encoder(hn_mochi:page_attributes(Ref#refX{path = Path}, #env{})),
    Data = io_lib:format("~s", [lists:flatten(Page)]),
    file:write_file(FileName, Data).

dump_groups(Site, SiteDest) ->
    Groups = hn_groups:dump_script(Site),
    ok = file:write_file(filename:join(SiteDest, "groups.export"), Groups).

dump_perms(Site, SiteDest) ->
    Perms = auth_srv:dump_script(Site),
    ok = file:write_file(filename:join(SiteDest, "permissions.export"), 
                         Perms).

dump_views(Site, SiteDest) ->
    ViewDest = join([SiteDest, "views"]),
    SiteFs   = hn_util:site_to_fs(Site),
    Source   = join([code:priv_dir(hypernumbers), "../../../", "var", "sites", SiteFs, "views"]),
    hn_util:recursive_copy(Source, ViewDest).

%% Import all sites found in the given directory.
import(Src) ->
    ServSrc = filename:join(Src, "services"),
    case filelib:is_dir(ServSrc) of
        true -> import_services(ServSrc);
        false -> ok
    end,
    Sites = [hn_util:site_from_fs(F) || 
                F <- filelib:wildcard("*", Src),
                hd(F) /= ".",
                F /= "services",
                filelib:is_dir(filename:join(Src,F))],
    import(Src, Sites).
%% Import the subset of sites found within the source directory.
%% Sites are expected in leading protocol form.
import(Src, Sites) ->
    [ok = import_site(Src, S) || S <- Sites],
    ok.

import_services(ServSrc) ->
    ok = load_users(ServSrc).

import_site(Src, Site) ->       
    SiteSrc = join([Src, hn_util:site_to_fs(Site)]),
    hn_setup:site(Site, blank, [], [corefiles, sitefiles]),
    ok = load_etf(Site, SiteSrc),
    ok = load_groups(Site, SiteSrc),
    ok = load_views(Site, SiteSrc),
    ok = load_perms(Site, SiteSrc).

load_users(Src) ->
    {ok, UserTs} = file:consult(filename:join(Src, "users.export")),
    ok = passport:load_script(UserTs).

load_etf(Site, SiteSrc) ->    
    Files = filelib:wildcard(join([SiteSrc, "etf"])++"/*.json"),
    [ ok = hn_import:json_file(Site ++ hn_setup:create_path_from_name(Json, ".json"),
                               Json)
      || Json <- Files, hn_setup:is_path(Json) ],    
    ok.

load_groups(Site, SiteSrc) ->
    {ok, GroupTs} = file:consult(filename:join(SiteSrc, "groups.export")),
    ok = hn_groups:load_script(Site, GroupTs).

load_views(Site, SiteSrc) ->
    SiteFs = hn_util:site_to_fs(Site),
    Source = join([SiteSrc, "views"]),
    Dest   = join(["var", "sites", SiteFs, "views"]),
    hn_util:recursive_copy(Source, Dest).

load_perms(Site, SiteSrc) ->
    {ok, Perms} = file:consult(filename:join(SiteSrc, "permissions.export")),
    auth_srv:load_script(Site, Perms).

join(FileName) ->
    filename:join(FileName).
