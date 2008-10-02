%%%-----------------------------------------------------------------------------
%%% File        : engine.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : the calculation engine
%%%
%%% Created     : 15 Oct 2007 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(hypernumbers_app).
-behaviour(application).

-include("yaws.hrl").
-include("spriki.hrl").


-export([start/2, stop/1, reset/0 ]).

start(_Type, _Args) ->

    {ok,[[Log]]}  = init:get_argument(hn_log),
    {ok,Hosts}    = get_hosts_conf(),

    case mnesia:table_info(schema, storage_type) of
        ram_copies -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ -> 
            ok
    end,
    
    case is_clean_startup() of
	true ->
	    hn_loaddb:create_db(disc_copies),
	    set_def_permissions(Hosts);
	false ->
	    ok
    end,

    SConfs = lists:map(fun(X) -> create_sconf(X) end,Hosts),    
    DefaultGC = yaws_config:make_default_gconf(false, "id"),
    GC = DefaultGC#gconf{logdir=Log},    

    application:set_env(yaws, embedded, true),
    application:start(yaws),    
    ok = yaws_api:setconf(GC, [SConfs]),

    case hypernumbers_sup:start_link() of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->     
            Error
    end.

reset() ->
    {ok,Hosts} = get_hosts_conf(),
    set_def_permissions(Hosts).

%%------------------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%------------------------------------------------------------------------------
stop(_State) -> ok.

create_sconf({IP,Port,_Domains}) ->
    #sconf{port = Port,
           appmods=[{"/",hn_yaws}],
           listen = IP,
           docroot = code:lib_dir(hypernumbers)++"/priv/docroot"}.
    
is_clean_startup() ->
    case mnesia:system_info(tables) of
	[schema] -> true;
	_ ->
	    Me = node(),
	    case mnesia:table_info(schema,cookie) of
		{_,Me} -> false;
		_      -> true
	    end
    end.

get_hosts_conf() ->
    {ok,[[Path]]} = init:get_argument(hn_config),	
    {ok,Config} = file:consult(Path), 
    {value,{hosts,HostList}} = lists:keysearch(hosts,1,Config),
    {ok,HostList}.

set_def_permissions([]) -> 
    ok;
set_def_permissions([{{I1,I2,I3,I4},Port,Domains}|T])->
    
    Url = lists:concat(["http://",I1,".",I2,".",I3,".",I4,":",Port]),

    F = fun(X) ->
		lists:concat(["http://"++X++":"++integer_to_list(Port)])
	end,
							     
    NDomains = [Url|lists:map(F,Domains)],

    set_perms(NDomains),
    set_def_permissions(T).

set_perms([]) -> ok;
set_perms([Domains|T]) ->
    
    Ref = #ref{site = Domains,
               path = [],
               ref  = {page,"/"}},
    
    hn_main:set_attribute(Ref#ref{name="__permissions"},
			  [{user,anonymous,admin}]),
    
    hn_main:set_attribute(Ref#ref{name="__groups"},
			  [{owner,[{user,"admin"}]}]),
    
    set_perms(T).
