%%%-----------------------------------------------------------------------------
%%% File        : hn_loaddb.erl
%%% Author      : Dale Harvey <dale@hypernumbers.com>
%%% Description : Sets up database
%%%-----------------------------------------------------------------------------
-module(hn_loaddb).

-include("spriki.hrl").

-export([create_db/0,create_db/1]).

-define(create(Name,Type,Storage), 
    {atomic,ok} = mnesia:create_table(Name,
        [{attributes, record_info(fields, Name)},
            {type,Type},{Storage, [node()]}])).
            

create_db()->
    create_db(ram_copies).
    
create_db(Storage)->
    
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    mnesia:start(),
    
    ?create(hn_item,set,Storage),
    ?create(remote_cell_link,bag,Storage),
    ?create(local_cell_link,bag,Storage),
    ?create(hn_user,set,Storage),
    ?create(dirty_cell,set,Storage),
    ?create(dirty_hypernumber,set,Storage),
    ?create(incoming_hn,set,Storage),
    ?create(outgoing_hn,set,Storage),
    ?create(template,set,Storage),
    
    Ref1 = #ref{site = "http://127.0.0.1:9000",
               path = [],
               ref  = {page,"/"}},

    Ref2 = #ref{site = "http://claims.hypernumbers.dev:9000",
               path = [],
               ref  = {page,"/"}},
    Ref3 = #ref{site = "http://accounts.hypernumbers.dev:9000",
               path = [],
               ref  = {page,"/"}},

    users:create("admin","admin"),
    users:create("user","user"),
    Fun = fun(Ref) ->
                  hn_main:set_attribute(Ref#ref{name="__permissions"},
                                        [{user,anonymous,admin}]),
                  
                  hn_main:set_attribute(Ref#ref{name="__groups"},
                                        [{owner,[{user,"admin"}]}])
          end,
    lists:map(Fun,[Ref1,Ref2,Ref3]),
    gen_server:cast(dirty_cell, stop),
    gen_server:cast(dirty_hypernumber, stop),
    
    ok.
