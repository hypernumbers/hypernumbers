%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hypernumbers_app).
-behaviour(application).

-include("yaws.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").

-export([start/2, stop/1, clean_start/0 ]).
-export([set_def_perms/0]).
%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
start(_Type, _Args) ->
    
    %% TODO this test is buggy - the schema is always on disc 
    %%      but invididual tables can be:
    %%      * ram_copies
    %%      * disc_copies
    %%      * disc_only_copies
    case mnesia:table_info(schema, storage_type) of
        ram_copies -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ -> 
            ok
    end,
        
    case hypernumbers_sup:start_link() of
        {ok, Pid} -> 
    
            {ok,[[Path]]} = init:get_argument(hn_config),	
            {ok,Config} = file:consult(Path), 
            gen_server:call(hn_config,{set_conf,Config}),

            case is_fresh_startup() of
                true  -> clean_start();
                false -> ok
            end,
                        
            ok = start_yaws(),
            ok = start_dirty_subscribe(),
            {ok, Pid};

        Error -> 
            Error
    end.

%% @spec stop(State) -> ok
%% @doc  Application Callback
stop(_State) -> 
    ok.

%% @spec is_fresh_startup() -> true | false
%% @doc  does database already exist + 
%%       is readable on disk
is_fresh_startup() ->
    case mnesia:system_info(tables) of
        [schema] -> true;
        _ ->
            Me = node(),
            case mnesia:table_info(schema,cookie) of
                {_,Me} -> false;
                _      -> true
            end
    end.

%% @spec clean_start() -> ok
%% @doc  delete/create existing database and set up
%%       initial permissions
clean_start() ->
    %% Probably not a nice way to do this, 
    %% Before everything is restarted the msg queues
    %% for these needs to be emptied
    Kill = fun(undefined) -> ok;
              (Pid)       -> exit(Pid,clean_start)
           end,
    lists:map(fun(X) -> Kill(whereis(X)) end,
              [dirty_cell,dirty_hypernumbers]),

    ok = hn_db:create(),
    set_def_perms(),
    ok = start_dirty_subscribe(),
    ok.

%% @spec start_yaws() -> ok
%% @doc  Start yaws in embedded mode
start_yaws() ->
    
    {ok,[[Log]]}  = init:get_argument(hn_log),
    Hosts   = gen_server:call(hn_config,{get,hosts}),

    SConfs = lists:map(fun create_sconf/1,Hosts),    
    GC     = yaws_config:make_default_gconf(false, "id"),
    
    application:set_env(yaws, embedded, true),
    application:start(yaws),    
    ok = yaws_api:setconf(GC#gconf{logdir=Log}, [SConfs]),
    ok.

%% @spec start_dirty_subscribe() -> ok
%% @doc  Make dirty_x gen_srv's subscribe to mnesia
start_dirty_subscribe() ->
    gen_server:cast(dirty_cell,        subscribe),
    gen_server:cast(dirty_hypernumber, subscribe),
    ok.

%% @spec create_sconf(Details) -> SConf
%% @doc  Create SConf record for yaws setup
create_sconf({IP,Port,_Domains}) ->
    #sconf{port = Port,
           appmods=[{"/",hn_yaws}],
           listen = IP,
           docroot = code:lib_dir(hypernumbers)++"/priv/docroot"}.

%% @spec set_def_perms() -> ok
%% @doc  Set the default permissions on each domain
set_def_perms() ->
    Hosts = gen_server:call(hn_config,{get,hosts}),
    set_def_perms(Hosts).

set_def_perms([]) -> ok;
set_def_perms([{{I1,I2,I3,I4},Port,Domains}|T])->
    
    Url = lists:concat(["http://",I1,".",I2,".",I3,".",I4,":",Port]),
    
    F = fun(X) ->
                lists:concat(["http://"++X++":"++integer_to_list(Port)])
	end,

    lists:map(fun set_perms/1, [Url|lists:map(F,Domains)]),
    set_def_perms(T).

%% @spec set_perms(Domains) -> Return
%% @doc  Supervisor call back
set_perms(Domains) ->
    
    Ref = #ref{site = Domains,
               path = [],
               ref  = {page,"/"}},
    
    hn_main:set_attribute(Ref#ref{name='__permissions'},
                          [{user,anonymous,admin}]),
    
    hn_main:set_attribute(Ref#ref{name='__groups'},
                          [{owner,[{user,"admin"}]}]),

    ok.
