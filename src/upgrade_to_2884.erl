%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie 
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       code to manage backup and restore etc as well
%%%            as 'grabbing' sites and applications
%%%
%%% @end
%%% Created : 18 Dec 2009 by Gordon Guthrie gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(upgrade_to_2884).

-include("../lib/hypernumbers-1.0/include/spriki.hrl").

-export([
         grab_site/1,
         import_site/1
        ]).

import_site(URL) ->
    "http://" ++ SiteAndPort = URL,
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/" ++ "grab/"
        ++ Site ++ "&" ++ Port ++ "/",
    io:format("ayeee!~n"),
    [ ok = hn_import:json_file("http://" ++ Site ++ ":" ++ Port
        ++ create_path_from_name(Json), Json)
      || Json <- filelib:wildcard(Dir++"/data/*.json")],
    ok.

-spec grab_site(list()) -> ok.
grab_site(URL) ->
    "http://" ++ SiteAndPort = URL,
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/" ++ "grab/"
        ++ Site ++ "&" ++ Port ++ "/data/",
    ok = case filelib:is_dir(Dir) of
             true  -> hn_util:delete_directory(Dir);
             false -> ok
         end,
    ok = filelib:ensure_dir(Dir),
    ok = grab_pages(URL, Dir),
    ok.


%%
%% Internal Functions
%%

create_path_from_name(Name) ->
    [ "path" | Rest ] = string:tokens(filename:basename(Name, ".json"), "."),
    hn_util:list_to_path(Rest).

grab_pages(URL, Dir) ->
    RefX = hn_util:url_to_refX(URL),
    Pages = hn_db_api:read_pages(RefX),
    grab_pages2(RefX, Pages, Dir).

grab_pages2(_RefX, [], _Dir)    -> ok;
grab_pages2(RefX, [H | T], Dir) ->
    Page = hn_mochi:page_attributes(RefX#refX{path = H}, anonymous),
    Page2 = (mochijson:encoder([{input_encoding, utf8}]))(Page),
    Name = hn_util:path_to_json_path(H),
    Pg = io_lib:fwrite("~p.~n", [Page2]),
    ok = file:write_file(Dir ++ Name, Pg),
    grab_pages2(RefX, T, Dir).
    
