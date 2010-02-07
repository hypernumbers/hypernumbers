%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie 
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       code to manage backup and restore etc as well
%%%            as 'grabbing' sites and applications
%%%
%%% @end
%%% Created : 18 Dec 2009 by Gordon Guthrie gordon@hypernumbers.com
%%%
%%% TODO: Work with new auth server2
%%%-------------------------------------------------------------------
-module(server_mgt).

-include("spriki.hrl").

-export([
         grab_site/1,
         backup_srv/0,
         backup_site/1,
         restore_srv/1,
         restore_site/2
        ]).

%% save_site(Site) ->
    
%%     ok.
    

-spec grab_site(list()) -> ok.
grab_site(URL) ->
    "http://" ++ SiteAndPort = URL,
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Prefix = Site ++ "&" ++ Port,
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/" ++ "grab/"
        ++ Site ++ "&" ++ Port ++ "/",
    
    ok = case filelib:is_dir(Dir) of
             true  -> hn_util:delete_directory(Dir);
             false -> ok
         end,
    ok = filelib:ensure_dir(Dir),
    ok = grab_pages(URL, Dir),
    ok = get_views(ignored, [URL], Prefix, grab),
    ok = grab_auth_srv(URL, Dir),
    ok.

-spec restore_srv(list()) -> ok.
restore_srv(Timestamp) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/backup/ALL/"
        ++ Timestamp ++"/",
    {ok, Files} = file:list_dir(Dir),
    restore_srv2(Dir, Timestamp, Files).

restore_srv2(_Dir, _Timestamp, [])     -> ok;
restore_srv2(Dir, Timestamp, [H | T]) ->
    ok = case filelib:is_dir(Dir ++ H) of
             true  -> [Site, Port] = string:tokens(H, "&"),
                      restore_site2(Site, Port, "ALL", Timestamp);
             false -> ok
         end,
    restore_srv2(Dir, Timestamp, T).
                              

-spec restore_site(list(), list()) -> ok.
restore_site(URL, Timestamp) ->
    "http://" ++ SiteAndPort = URL,
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    restore_site2(Site, Port, SiteAndPort, Timestamp).

restore_site2(Site, Port, Type, Timestamp) ->
    Prefix = code:lib_dir(hypernumbers) ++ "/../../var/backup/",
    Dir = Prefix ++ Type ++ "/" ++ Timestamp ++ "/",
    ok = restore_mnesia(Dir),
    ok = restore_views(Dir, Site ++ "&" ++ Port),
    ok = restore_auth_srv(Dir, Site ++ "&" ++ Port).

-spec backup_srv() -> ok.
backup_srv() ->
    Timestamp = integer_to_list(util2:get_timestamp()),
    Type = "ALL",
    Tables = mnesia:system_info(tables),
    Sites = hn_setup:get_sites(),
    ok = backup_mnesia(Timestamp, Tables, Type),
    ok = get_views(Timestamp, Sites, Type, backup),
    ok = backup_auth_srv(Timestamp, Sites, Type),
    {ok, Type, Timestamp}.

-spec backup_site(list()) -> ok.
backup_site(URL) ->
    "http://" ++ SiteAndPort = URL,
    Timestamp = integer_to_list(util2:get_timestamp()),
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Prefix = Site ++ "&" ++ Port,
    Tables = mnesia:system_info(tables),
    SiteTables = get_site_tables(Prefix, Tables),
    ok = backup_mnesia(Timestamp, SiteTables, Prefix),
    ok = get_views(Timestamp, [URL], Prefix, backup),
    ok = backup_auth_srv(Timestamp, [URL], Prefix),
    {ok, Prefix, Timestamp}.


%%
%% Internal Functions
%%
restore_auth_srv(Dir, Site) ->
    File = Dir ++ Site ++ "/auth_srv/auth_srv.dump",
    {ok, [Tree]} = file:consult(File),
    auth_srv:restore("http://" ++ Site, Tree).

restore_views(Dir, Site) ->
    From = Dir ++ Site ++ "/views/",
    To = code:lib_dir(hypernumbers) ++ "/../../var/docroot/" ++ Site ++ "/",
    % first delete the existing one
    ok = case filelib:is_dir(To) of
             true  -> hn_util:delete_directory(To);
             false -> ok
         end,
    ok = hn_util:recursive_copy(From, To).
    
restore_mnesia(Dir) ->
    hn_db_admin:restore(Dir, "mnesia.backup").

backup_auth_srv(_Timestamp, [], _Type)                       -> ok;
backup_auth_srv(Timestamp, ["http://" ++ H = Url | T], Type) ->
    [Site, Port] = string:tokens(H, ":"),
    Prefix = code:lib_dir(hypernumbers) ++ "/../../var/",
    To = Prefix ++ "backup/" ++ Type ++ "/" ++ Timestamp ++ "/"
        ++ Site ++ "&" ++ Port ++ "/auth_srv/auth_srv.dump",
    ok = auth_srv:backup(Url, To),
    backup_auth_srv(Timestamp, T, Type).

grab_pages(URL, Dir) ->
    RefX = hn_util:url_to_refX(URL),
    Pages = hn_db_api:read_pages(RefX),
    filelib:ensure_dir(Dir ++ "/data/"),
    grab_pages2(RefX, Pages, Dir ++ "/data/").

grab_pages2(_RefX, [], _Dir)    -> ok;
grab_pages2(RefX, [H | T], Dir) ->
    Page = hn_mochi:page_attributes(RefX#refX{path = H}, anonymous),
    Page2 = (mochijson:encoder([{input_encoding, utf8}]))(Page),
    Name = hn_util:path_to_json_path(H),
    Pg = io_lib:fwrite("~s", [lists:flatten(Page2)]),
    ok = file:write_file(Dir ++ Name, Pg),
    grab_pages2(RefX, T, Dir).

grab_auth_srv(URL, Dir) ->
    Perms = auth_srv2:dump_script(URL),
    ok = filelib:ensure_dir(Dir),
    ok = file:write_file(Dir ++ permissions.script, Perms).

get_views(_Timestamp, [], _Type, _Kind)                -> ok;
get_views(Timestamp, ["http://" ++ H | T], Type, Kind) ->
    [Site, Port] = string:tokens(H, ":"),
    Prefix = code:lib_dir(hypernumbers) ++ "/../../var/",
    From = Prefix ++ "sites/" ++ Site ++ "&" ++ Port ++ "/docroot/",
    To = case Kind of
             backup -> Prefix ++ "backup/" ++ Type ++ "/" ++ Timestamp ++ "/"
                           ++ Site ++ "&" ++ Port ++ "/views/";
             grab   -> Prefix ++ "grab/" ++ Site ++ "&" ++ Port
                           ++ "/docroot/"
         end,
    ok = filelib:ensure_dir(To),
    ok = hn_util:recursive_copy(From, To),
    get_views(Timestamp, T, Type, Kind).

backup_mnesia(Timestamp, Tables, Type) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/backup/"
        ++ Type ++ "/" ++ Timestamp ++ "/",
    ok = filelib:ensure_dir(Dir),
    Name = "mnesia.backup",
    ok = hn_db_admin:backup(Tables, Dir, Name).

get_site_tables(Prefix, Tables) -> get_st2(Prefix, Tables, []).

get_st2(_Prefix, [], Acc)     -> Acc;
get_st2(Prefix, [H | T], Acc) ->
    case lists:prefix(Prefix, atom_to_list(H)) of
        true  -> get_st2(Prefix, T, [H | Acc]);
        false -> get_st2(Prefix, T, Acc)
    end.

    
