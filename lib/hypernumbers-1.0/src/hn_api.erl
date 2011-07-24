%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       this module manages the public/private key pairs
%%%            and associated permissions for the api
%%%
%%% @end
%%% Created : 24 Jul 2011 by <gordon@hypernumbers.com>

-module(hn_api).

-export([
         get_all_keys/1,
         get_new_pair/0,
         add_key/2,
         get_key/2,
         delete_key/2
         ]).

get_all_keys(Site) -> new_db_api:get_api_keys(Site).

get_new_pair() -> {Public, Private} = hmac_api_lib:get_api_keypair(),
                  [{"public", Public}, {"private", Private}].

add_key(Site, Json) ->
    API = make_api_record(Json),
    new_db_api:write_api(Site, API).

get_key(Site, PublicKey) ->
    [Rec] = new_db_api:read_api(Site, PublicKey),
    make_api_json(Rec).

delete_key(Site, PublicKey) -> new_db_api:delete_api(Site, PublicKey).

make_api_record(_Json) ->
    "erk".

make_api_json(_Api) ->
    "banjo".


