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
-module(server_mgt).

-include("spriki.hrl").

-export([export/1, export/2,
         import/1, import/2,
         export_as_sitemod/2 ]).

%% Auxillary functions.
%-export([backup, move, delete, etc...]).

%% Exports a site as a sitemod that can be loaded by a factory
-spec export_as_sitemod(atom(), list()) -> ok.
export_as_sitemod(NewType, Site) ->
    
    SiteType = join([code:lib_dir(sitemods), "priv", "site_types"]),
    OldType  = hn_setup:get_site_type(Site),

    % TODO, this might be ok, but need support for exporting
    % and importing users (dont copy if the same file)
    (OldType =/= NewType) andalso
        file:copy(join([SiteType, OldType, "users.script"]),
                  join([SiteType, NewType, "users.script"])),
    
    ok = export_site(join([SiteType, NewType]), Site),
    ok = hn_util:recursive_copy(join([SiteType, NewType,"etf"]),
                                join([SiteType, NewType, "data"])),
    
    % Delete backup things
    [ file:delete( join([SiteType, NewType,  File]) )
      || File <- ["type", "mnesia.backup"] ],
    [ hn_util:delete_directory( join([SiteType, NewType, join(Dir)]) )
      || Dir <- [["etf"], ["views", "_g", "core"]] ],
    ok. 
    
%% Export sites to the given directory.
export(Dest) ->
    Sites = hn_setup:get_sites(),
    export(Dest, Sites).
%% Export the provided subset of sites to the destination directory.
%% Sites are expected in leading protocol form.
export(Dest, Sites) ->
    [ ok = export_site(join([Dest, hn_util:site_to_fs(S)]), S)
      || S <- Sites, hn_setup:site_exists(S)],
    ok.

export_site(Dest, Site) ->
    filelib:ensure_dir([Dest,"/"]),
    ok = dump_type(Site, Dest),
    ok = dump_etf(Site, Dest),
    ok = dump_mnesia(Site, Dest),
    ok = dump_perms(Site, Dest),
    ok = dump_views(Site, Dest).

dump_type(Site, SiteDest) ->
    Type = hn_setup:get_site_type(Site),
    {ok, F} = file:open(join([SiteDest, "type"]), [write]),
    io:format(F, "~p.", [Type]),
    file:close(F).

dump_etf(Site, SiteDest) -> 
    EtfDest = filename:join(SiteDest, "etf"),
    filelib:ensure_dir([EtfDest,"/"]),  
    Ref = hn_util:parse_url(Site),
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    Paths = hn_db_api:read_pages(Ref),
    Pages = [Encoder(hn_mochi:page_attributes(Ref#refX{path = P}, anonymous))
             || P <- Paths],
    [ok = file:write_file(
            filename:join(EtfDest, hn_util:path_to_json_path(Path)), 
            io_lib:format("~s", [lists:flatten(Page)])) || 
        {Path, Page} <- lists:zip(Paths, Pages)],
    ok.

dump_mnesia(Site, SiteDest) -> 
    SiteFs = hn_util:site_to_fs(Site),
    Tables = [T || T <- mnesia:system_info(tables),
                   lists:prefix(SiteFs, atom_to_list(T))],
    ok = hn_db_admin:backup(Tables, SiteDest, "mnesia.backup").

dump_perms(Site, SiteDest) ->
    Perms = auth_srv2:dump_script(Site),
    ok = file:write_file(filename:join(SiteDest, "permissions.script"), 
                         Perms).

dump_views(Site, SiteDest) -> 
    ViewDest = join([SiteDest, "views"]),
    SiteFs   = hn_util:site_to_fs(Site),
    Source   = join(["var", "sites", SiteFs, "views"]),
    hn_util:recursive_copy(Source, ViewDest).


%% Import all sites found in the given directory.
import(Src) ->
    Sites = [hn_util:site_from_fs(F) || 
                F <- filelib:wildcard("*", Src),
                hd(F) /= ".",
                filelib:is_dir(filename:join(Src,F))],
    import(Src, Sites).
%% Import the subset of sites found within the source directory.
%% Sites are expected in leading protocol form.
import(Src, Sites) ->
    [ok = import_site(Src, S) || S <- Sites],
    ok.
import_site(Src, Site) ->
    SiteSrc = join([Src, hn_util:site_to_fs(Site)]),
    Type    = load_type(SiteSrc),
    ok = hn_setup:site(Site, Type, []),
    ok = load_etf(Site, SiteSrc),
    ok = load_views(Site, SiteSrc),
    ok = load_perms(Site, SiteSrc).

load_type(SiteSrc) ->
    {ok, [Type]} = file:consult(join([SiteSrc, "type"])),
    Type.

load_etf(Site, SiteSrc) ->
    EtfSrc = join([SiteSrc, "etf"]),
    Jsons  = filelib:wildcard("path.*.json", EtfSrc),
    Paths  = [hn_util:list_to_path(
                string:tokens(
                  filename:basename(P, ".json"), 
                  "."
                 )) || "path."++P <- Jsons],
    [ok = hn_import:json_file(Site++Path, join([EtfSrc, Json])) ||
        {Json, Path} <- lists:zip(Jsons, Paths)],
    ok.
        
load_views(Site, SiteSrc) ->
    SiteFs = hn_util:site_to_fs(Site),
    Source = join([SiteSrc, "views"]),
    Dest   = join(["var", "sites", SiteFs, "views"]),
    hn_util:recursive_copy(Source, Dest).

load_perms(Site, SiteSrc) ->
    {ok, Perms} = file:consult(filename:join(SiteSrc, "permissions.script")),
    auth_srv2:load_script(Site, Perms).

join(FileName) ->
    filename:join(FileName).
%% -spec grab_as_site_type(list(), list()) -> ok.
%% grab_as_site_type(URL, SiteType) ->
%%     {GrabPrefix, GrabDir} = parse_url_for_grab(URL),
%%     ok = grab_site1(URL, GrabPrefix, GrabDir),
%%     SiteTypePrefix = code:priv_dir(sitemods) ++ "/site_types/",
%%     ToDir = SiteTypePrefix ++ SiteType,
%%     ok = case filelib:is_dir(ToDir) of
%%              true  -> hn_util:delete_directory(ToDir);
%%              false -> ok
%%          end,
%%     ok = filelib:ensure_dir(ToDir),
%%     ok = hn_util:recursive_copy(GrabDir, ToDir).

%% -spec grab_site(list()) -> ok.
%% grab_site(URL) ->
%%     {Prefix, Dir} = parse_url_for_grab(URL),
%%     grab_site1(URL, Prefix, Dir).

%% grab_site1(URL, Prefix, Dir) ->
%%     ok = case filelib:is_dir(Dir) of
%%              true  -> hn_util:delete_directory(Dir);
%%              false -> ok
%%          end,
%%     ok = filelib:ensure_dir(Dir),
%%     ok = grab_pages(URL, Dir),
%%     ok = get_views(ignored, [URL], Prefix, grab),
%%     ok = grab_auth_srv(URL, Dir).

%% -spec restore_srv(list()) -> ok.
%% restore_srv(Timestamp) ->
%%     Dir = filename:join([code:lib_dir(hypernumbers), "..", "..", 
%%                          "var/backup/ALL", Timestamp]),
%%     {ok, Files} = file:list_dir(Dir),
%%     restore_srv2(Dir, Timestamp, Files).

%% restore_srv2(_Dir, _Timestamp, [])     -> ok;
%% restore_srv2(Dir, Timestamp, [H | T]) ->
%%     ok = case filelib:is_dir(Dir ++ H) of
%%              true  -> [Site, Port] = string:tokens(H, "&"),
%%                       restore_site2(Site, Port, "ALL", Timestamp);
%%              false -> ok
%%          end,
%%     restore_srv2(Dir, Timestamp, T).
                              

%% -spec restore_site(list(), list()) -> ok.
%% restore_site(Site, Timestamp) ->
%%     "http://" ++ HP = Site,
%%     [Host, Port] = string:tokens(HP, ":"),
%%     Prefix = code:lib_dir(hypernumbers) ++ "/../../var/backup/",
%%     Type = "ALL",
%%     Dir = filename:join([Prefix, Type, Timestamp]),
%%     ok = restore_mnesia(Dir),
%%     ok = restore_views(Dir, Site ++ "&" ++ Port),
%%     ok = restore_auth_srv(Dir, Site ++ "&" ++ Port).

%% -spec backup_srv() -> ok.
%% backup_srv() ->
%%     Timestamp = integer_to_list(util2:get_timestamp()),
%%     Type = "ALL",
%%     Tables = mnesia:system_info(tables),
%%     Sites = hn_setup:get_sites(),
%%     ok = backup_mnesia(Timestamp, Tables, Type),
%%     ok = get_views(Timestamp, Sites, Type, backup),
%%     [ok = backup_auth_srv(Timestamp, S, Type) || S <- Sites],
%%     {ok, Type, Timestamp}.

%% -spec backup_site(list()) -> ok.
%% backup_site(URL) ->
%%     "http://" ++ SiteAndPort = URL,
%%     Timestamp = integer_to_list(util2:get_timestamp()),
%%     [Site, Port] = string:tokens(SiteAndPort, ":"),
%%     Prefix = Site ++ "&" ++ Port,

%%     ok = backup_mnesia(Timestamp, SiteTables, Prefix),
%%     ok = get_views(Timestamp, [URL], Prefix, backup),
%%     ok = backup_auth_srv(Timestamp, [URL], Prefix),
%%     {ok, Prefix, Timestamp}.


%% %%
%% %% Internal Functions
%% %%
%% parse_url_for_grab(URL) ->
%%     "http://" ++ SiteAndPort = URL,
%%     [Site, Port] = string:tokens(SiteAndPort, ":"),
%%     Prefix = Site ++ "&" ++ Port,
%%     {Prefix, code:lib_dir(hypernumbers) ++ "/../../var/" ++ "grab/"
%%         ++ Site ++ "&" ++ Port ++ "/"}.

%% restore_auth_srv(Dir, Site) ->
%%     File = [Dir, Site, "/auth_srv/auth_srv.dump"],
%%     {ok, Terms} = file:consult(File),
%%     auth_srv2:load_script(Site, Terms).

%% restore_views(Dir, Site) ->
%%     From = Dir ++ Site ++ "/views/",
%%     To = code:lib_dir(hypernumbers) ++ "/../../var/docroot/" ++ Site ++ "/",
%%     % first delete the existing one
%%     ok = case filelib:is_dir(To) of
%%              true  -> hn_util:delete_directory(To);
%%              false -> ok
%%          end,
%%     ok = hn_util:recursive_copy(From, To).
    
%% restore_mnesia(Dir) ->
%%     hn_db_admin:restore(Dir, "mnesia.backup").

%% backup_auth_srv(Timestamp, "http://" ++ HP = Site, Type) ->
%%     [Host, Port] = string:tokens(HP, ":"),
%%     Prefix = code:lib_dir(hypernumbers) ++ "/../../var/",
%%     Dir = filename:join([Prefix, "backup", Type, Timestamp,
%%                          Host ++ "&" ++ Port, "auth_srv"]),
%%     Perms = auth_srv2:dump_script(Site),
%%     ok = filelib:ensure_dir(Dir),
%%     ok = file:write_file([Dir, "/auth_srv.dump"], Perms).

%% grab_pages(URL, Dir) ->
%%     RefX = hn_util:url_to_refX(URL),
%%     Pages = hn_db_api:read_pages(RefX),
%%     filelib:ensure_dir(Dir ++ "/data/"),
%%     grab_pages2(RefX, Pages, Dir ++ "/data/").

%% grab_pages2(_RefX, [], _Dir)    -> ok;
%% grab_pages2(RefX, [H | T], Dir) ->
%%     Page = hn_mochi:page_attributes(RefX#refX{path = H}, anonymous),
%%     Page2 = (mochijson:encoder([{input_encoding, utf8}]))(Page),
%%     Name = hn_util:path_to_json_path(H),
%%     Pg = io_lib:fwrite("~s", [lists:flatten(Page2)]),
%%     ok = file:write_file(Dir ++ Name, Pg),
%%     grab_pages2(RefX, T, Dir).

%% grab_auth_srv(URL, Dir) ->
%%     Perms = auth_srv2:dump_script(URL),
%%     ok = filelib:ensure_dir(Dir),
%%     ok = file:write_file(Dir ++ permissions.script, Perms).

%% get_views(_Timestamp, [], _Type, _Kind)                -> ok;
%% get_views(Timestamp, ["http://" ++ H | T], Type, Kind) ->
%%     [Site, Port] = string:tokens(H, ":"),
%%     Prefix = code:lib_dir(hypernumbers) ++ "/../../var/",
%%     From = Prefix ++ "sites/" ++ Site ++ "&" ++ Port ++ "/docroot/",
%%     To = case Kind of
%%              backup -> Prefix ++ "backup/" ++ Type ++ "/" ++ Timestamp ++ "/"
%%                            ++ Site ++ "&" ++ Port ++ "/views/";
%%              grab   -> Prefix ++ "grab/" ++ Site ++ "&" ++ Port
%%                            ++ "/docroot/"
%%          end,
%%     ok = filelib:ensure_dir(To),
%%     ok = hn_util:recursive_copy(From, To),
%%     get_views(Timestamp, T, Type, Kind).

%% backup_mnesia(Timestamp, Tables, Type) ->
%%     Dir = code:lib_dir(hypernumbers) ++ "/../../var/backup/"
%%         ++ Type ++ "/" ++ Timestamp ++ "/",
%%     ok = filelib:ensure_dir(Dir),
%%     Name = "mnesia.backup",
%%     ok = hn_db_admin:backup(Tables, Dir, Name).


