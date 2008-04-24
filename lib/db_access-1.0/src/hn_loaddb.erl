%%%-----------------------------------------------------------------------------
%%% File        : hn_loaddb.erl
%%% Author      : Dale Harvey <dale@hypernumbers.com>
%%% Description : Sets up database
%%%-----------------------------------------------------------------------------
-module(hn_loaddb).

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
       
    {atomic,ok} = mnesia:create_table(remote_cell_link,
        [{Storage, [node()]},{type,bag},
	 {attributes, record_info(fields, remote_cell_link)}]),
    
    {atomic,ok} = mnesia:create_table(local_cell_link,
        [{Storage, [node()]},{type,bag},
	 {attributes, record_info(fields, local_cell_link)}]),
    
    {atomic,ok} = mnesia:create_table(users,
        [{Storage, [node()]},{type,bag},
	 {attributes, record_info(fields, users)}]),
         
    {atomic,ok} = mnesia:create_table(websheet,
        [{Storage, [node()]},{type,bag},
	 {attributes, record_info(fields, websheet)}]),
         
    {atomic,ok} = mnesia:create_table(dirty_cell,
    	[{Storage, [node()]},{type,set},
    	 {attributes, record_info(fields, dirty_cell)}]),
         
    {atomic,ok} = mnesia:create_table(dirty_hypernumber,
    	[{Storage, [node()]},{type,set},
         {attributes, record_info(fields, dirty_hypernumber)}]),
         
    {atomic,ok} = mnesia:create_table(incoming_hn,
        [{Storage, [node()]},{type,set},
         {attributes, record_info(fields, incoming_hn)}]),

    {atomic,ok} = mnesia:create_table(outgoing_hn,
        [{Storage, [node()]},{type,set},
         {attributes, record_info(fields, outgoing_hn)}]),
    
    ok.
