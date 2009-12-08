%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hypernumbers_app).
-behaviour(application).
-export([start/2, stop/1]).

-include("spriki.hrl").
-include("tiny.hrl").


%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
start(_Type, _Args) ->
    ok = init_tables(),
    ok = load_muin_modules(),
    ok = mochilog:start(),
    {ok, Pid} = hypernumbers_sup:start_link(),
    ok = start_mochiweb(),
    ok = bootstrap_sites(),
    ok = hn_setup:startup(),
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

init_tables() -> 
    ensure_schema(),
    Storage = disc_only_copies,

    %% Core system tables -- required to operate system
    CoreTbls = [ {core_site, record_info(fields, core_site), set, []}],

    %% SiteMod tables 
    ModTbls = [ {tiny_sub, record_info(fields, tiny_sub), set, []}],
    
    [ok = hn_db_api:create_table(N, N, F, Storage, T, I) 
     || {N,F,T,I} <- CoreTbls ++ ModTbls],
    ok.

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
    StrIp = inet_parse:ntoa(IP),
    Opts = [{port, Port}, 
            {ip,   StrIp},
            {name, StrIp ++ integer_to_list(Port)},
            {loop, {hn_mochi, req}}],
    {ok, _Pid} = mochiweb_http:start(Opts),
    ok.

bootstrap_sites() ->
    case application:get_env(hypernumbers, bootstrapped) of
        {ok, Sites} ->
            [ok = hn_setup:site(S, T, O) || {S, T, O} <- Sites],
            ok;
        _Else ->
            ok
    end.

