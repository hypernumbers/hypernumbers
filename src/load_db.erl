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
    Type_attr= case Type of
		   persistent -> disc_only_copies;
		   transient  -> ram_copies
	       end,
    Result0a=mnesia:delete_schema([node()]),
    io:format("Mnesia schema deleted ~p~n",[Result0a]),
    Result0b=mnesia:create_schema([node()]),
    io:format("Mnesia schema created ~p~n",[Result0b]),
    mnesia:start(),
    Result1=mnesia:create_table(spriki,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, spriki)},
				 {type,set}]),
    Result2=mnesia:create_table(ref,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, ref)},
				 {type,bag}]),
    Result3=mnesia:create_table(bindings,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, bindings)},
				 {type,bag}]),
    Result4=mnesia:add_table_index(ref,ref_to),
    Result5=mnesia:add_table_index(bindings,page),
    Result6=mnesia:create_table(users,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, users)},
				 {type,bag}]),
    Result7=mnesia:create_table(websheet,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, websheet)},
				 {type,bag}]),
    Result8=mnesia:create_table(dirty_refs,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, dirty_refs)},
				 {type,set}]),
    Result9=mnesia:create_table(dirty_hypernumbers,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, dirty_hypernumbers)},
				 {type,set}]),
    Result10=mnesia:create_table(hypernumbers,
				[{Type_attr, [node()]},
				 {attributes, record_info(fields, hypernumbers)},
				 {type,set}]),

    io:fwrite("Create spriki table                 : ~p~n",[Result1]),
    io:fwrite("Create ref table                    : ~p~n",[Result2]),
    io:fwrite("Create bindings table               : ~p~n",[Result3]),
    io:fwrite("Create ref_to index on table ref    : ~p~n",[Result4]),
    io:fwrite("Create page index on table bindings : ~p~n",[Result5]),
    io:fwrite("Create users table                  : ~p~n",[Result6]),
    io:fwrite("Create websheets table              : ~p~n",[Result7]),
    io:fwrite("Create dirty_refs table             : ~p~n",[Result8]),
    io:fwrite("Create dirty_hypernumbers table     : ~p~n",[Result9]),
    io:fwrite("Create hypernumbers table           : ~p~n",[Result10]),
    users:create("admin","admin",superuser),
    users:create("user","user"),
    io:format("Two users created~n* u:admin p:admin (superuser)~n"++
	      "* u:user  p:user~n").