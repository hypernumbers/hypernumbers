%%%-------------------------------------------------------------------
%%% File        : load_db.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description :
%%%
%%% Created     : 12 Nov 2006 by Gordon Guthrie <gordon@psyduck.local>
%%%-------------------------------------------------------------------
-module(load_db).

-include("spriki.hrl").

-export([create_db/0,create_db/1]).

create_db()->
    create_db(transient).

create_db(Type)->

    Storage = case Type of
        persistent -> disc_only_copies;
        transient  -> ram_copies
    end,
    
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    
    mnesia:start(),
    
    {atomic,ok} = mnesia:create_table(hn_item,
        [{Storage, [node()]},{type,set},
         {attributes, record_info(fields, hn_item)}]),
    
    {atomic,ok} = mnesia:create_table(link_cell,
        [{Storage, [node()]},{type,bag},
	 {attributes, record_info(fields, link_cell)}]),
    
    {atomic,ok} = mnesia:create_table(users,
        [{Storage, [node()]},{type,bag},
	 {attributes, record_info(fields, users)}]),
         
    {atomic,ok} = mnesia:create_table(websheet,
        [{Storage, [node()]},{type,bag},
	 {attributes, record_info(fields, websheet)}]),
         
    {atomic,ok} = mnesia:create_table(dirty_cell,
    	[{Storage, [node()]},{type,set},
    	 {attributes, record_info(fields, dirty_cell)}]),
         
    {atomic,ok} = mnesia:create_table(dirty_hypernumbers,
    	[{Storage, [node()]},{type,set},
         {attributes, record_info(fields, dirty_hypernumbers)}]),
         
    {atomic,ok} = mnesia:create_table(incoming_hn,
        [{Storage, [node()]},{type,set},
         {attributes, record_info(fields, incoming_hn)}]),
         
    {atomic,ok} = mnesia:create_table(outgoing_hn,
        [{Storage, [node()]},{type,bag},
         {attributes, record_info(fields, outgoing_hn)}]),
                    

    users:create("admin","admin",superuser),
    users:create("user","user"),
    
    ok.
