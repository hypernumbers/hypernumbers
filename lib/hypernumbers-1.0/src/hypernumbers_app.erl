%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hypernumbers_app).
-behaviour(application).

-export([start/2, stop/1]).


%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
start(_Type, _Args) ->
    ok = init_systables(),
    ok = load_muin_modules(),
    ok = mochilog:start(),
    {ok, Pid} = hypernumbers_sup:start_link(),
    ok = start_mochiweb(),
    {ok, Pid}.

%% @spec stop(State) -> ok
%% @doc  Application Callback
stop(_State) ->
    ok = mochilog:stop().

% these need to be loaded for exported_function() to work
load_muin_modules() ->
    [ {module, Module} = code:ensure_loaded(Module)
      || Module <- muin:get_modules() ],
    ok.

init_systables() -> 
    ensure_schema(),
    hn_db_api:create_systables().

ensure_schema() ->
    case mnesia:system_info(tables) of
        [schema] ->
            build_schema();
        Tables ->
            mnesia:wait_for_tables(Tables, infinity)
    end.

build_schema() ->
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start().

%% @spec start_mochiweb() -> ok
%% @doc  Start mochiweb http servers
%% @todo this server will accept a connection to any
%% domain name on the ip address, wtf?
start_mochiweb() ->
    {ok, Hs} = application:get_env(hypernumbers, hosts),
    [ start_instance(H) || H <- Hs],
    ok.

start_instance({IP, Port}) ->
    Opts = [{port, Port}, 
            {ip,   inet_parse:ntoa(IP)}, 
            {loop, {hn_mochi, req}}],
    mochiweb_http:start(Opts).
