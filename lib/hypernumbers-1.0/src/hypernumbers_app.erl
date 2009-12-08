%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hypernumbers_app).
-behaviour(application).

-export([start/2, stop/1, hup/0, clean_start_DEBUG/0 ]).

%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
start(_Type, _Args) ->

    case mnesia:table_info(schema, storage_type) of
        ram_copies -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ -> 
            ok
    end,

    {ok, Pid}     = hypernumbers_sup:start_link(),
    {ok,[[Path]]} = init:get_argument(hn_config),	
    hn_config:read_conf(Path), 

    ok = case is_fresh_startup() of
             true             -> fresh_start();
             {exists, Tables} -> mnesia:wait_for_tables(Tables, 1000000)
         end,

    [ok   = dirty_srv:start(X, hosts()) || X <- dirty_tables()],
    ok    = load_muin_modules(),
    ok    = start_mochiweb(),
        
    {ok, Pid}.

% these need to be loaded for exported_function() to work
load_muin_modules() ->
    [ {module, Module} = code:ensure_loaded(Module)
      || Module <- muin:get_modules() ],
    ok.

hup() ->
    hn_config:hup(),
    [ok = dirty_srv:start(X, hosts()) || X <- dirty_tables()].

%% @spec stop(State) -> ok
%% @doc  Application Callback
stop(_State) ->
    mochilog:stop(),
    ok.

%% @spec is_fresh_startup() -> true | false
%% @doc  does database already exist + 
%%       is readable on disk
is_fresh_startup() ->
    case mnesia:system_info(tables) of
        [schema] ->
            true;
        Tbls ->
            Me = node(),
            case mnesia:table_info(schema, cookie) of
                {_,Me} -> {exists, Tbls};
                _      -> true
            end
    end.

%% @spec clean_start_DEBUG() -> ok
%% @doc  delete/create existing database and set up
%%       initial permissions
clean_start_DEBUG() ->
    [ok = dirty_srv:stop(X) || X <- dirty_tables()],
    ok = fresh_start(),
    [ok = dirty_srv:start(X, hosts()) || X <- dirty_tables()].

fresh_start() ->

    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    F = fun({Host, Type, Opts}) ->
                auth_srv:clear_all_perms_DEBUG(Host),
                hn_setup:new_site(Host, Type, [{host, Host} | Opts])
        end,

    [ F(X) || X<-hn_config:get(hosts) ],
    
    ok.

%% @spec start_mochiweb() -> ok
%% @doc  Start mochiweb http servers
%% @todo this server will accept a connection to any
%% domain name on the ip address, wtf?
start_mochiweb() ->
    mochilog:start(),
    [ start_instance(X) || X <- hn_config:get(ips)],
    ok.

start_instance({IP, Port}) ->
    
    Opts = [{port, Port},
            {name, inet_parse:ntoa(IP)++":"++integer_to_list(Port)},
            {ip,   inet_parse:ntoa(IP)}, 
            {loop, {hn_mochi, req}}],
    
    {ok, _Pid} = mochiweb_http:start(Opts).

hosts() ->
    [ Host || {Host, _Type, _Opts} <- hn_config:get(hosts) ].

dirty_tables() ->
    [ dirty_cell,
      dirty_notify_in,
      dirty_notify_back_in,
      dirty_inc_hn_create,
      dirty_notify_out,
      dirty_notify_back_out ].
