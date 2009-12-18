%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie 
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       code to manage backup and restore etc as well
%%%            as 'grabbing' sites and applications
%%%
%%% @end
%%% Created : 18 Dec 2009 by Gordon Guthrie gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(server_mgt).

-export([
         backup_srv/0,
         backup_site/1
        ]).

-spec backup_srv() -> ok.
backup_srv() ->
    Timestamp = integer_to_list(util2:get_timestamp()),
    Type = "ALL",
    Tables = mnesia:system_info(tables),
    backup_mnesia(Timestamp, Tables, Type).

-spec backup_site(list()) -> ok.
backup_site("http://" ++ SiteAndPort) ->
    Timestamp = integer_to_list(util2:get_timestamp()),
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Prefix = Site ++ "&" ++ Port,
    Tables = mnesia:system_info(tables),
    SiteTables = get_site_tables(Prefix, Tables),
    backup_mnesia(Timestamp, SiteTables, Prefix).

%%
%% Internal Functions
%%

backup_mnesia(Timestamp, Tables, Type) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/backup/"
        ++ Type ++ "/" ++ Timestamp ++ "/",
    ok = filelib:ensure_dir(Dir),
    Name = "mnesia_" ++ Type ++ "_" ++ Timestamp,
    ok = hn_db_admin:backup(Tables, Dir, Name).

get_site_tables(Prefix, Tables) -> get_st2(Prefix, Tables, []).

get_st2(_Prefix, [], Acc)     -> Acc;
get_st2(Prefix, [H | T], Acc) ->
    case lists:prefix(Prefix, atom_to_list(H)) of
        true  -> get_st2(Prefix, T, [H | Acc]);
        false -> get_st2(Prefix, T, Acc)
    end.
    
