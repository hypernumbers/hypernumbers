%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hypernumbers_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("hypernumbers.hrl").
-include("spriki.hrl").

%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
start(_Type, _Args) ->
    ok = ensure_dirs(),
    ok = init_tables(),
    ok = load_muin_modules(),
    {ok, Pid} = hypernumbers_sup:start_link(),
    ok = case application:get_env(hypernumbers, environment) of
             {ok,development} -> dev_tasks();
             {ok,production}  -> production_tasks()
         end,
    ok = mochilog:start(),
    ok = start_mochiweb(),
    {ok, Pid}.

%% @spec stop(State) -> ok
%% @doc  Application Callback
stop(_State) ->
    ok = mochilog:stop().

ensure_dirs() ->
    Dirs = [application:get_env(hypernumbers, dets_dir),
            application:get_env(mnesia, dir),
            application:get_env(sasl, error_logger_mf_dir)],
    [ok = filelib:ensure_dir(D++"/") || {ok, D} <- Dirs],
    ok.

% these need to be loaded for exported_function() to work
load_muin_modules() ->
    [ {module, Module} = code:ensure_loaded(Module)
      || Module <- muin:get_modules() ],
    ok.

init_tables() -> 
    ok = ensure_schema(),
    Storage = disc_only_copies,

    %% Core system tables -- required to operate system
    CoreTbls = [ {core_site, record_info(fields, core_site), set, []}],
    
    [ok = hn_db_admin:create_table(N, N, F, Storage, T, true, I) 
     || {N,F,T,I} <- CoreTbls],
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
    ok = application:start(mnesia).

%% @spec start_mochiweb() -> ok
%% @doc  Start mochiweb http servers
start_mochiweb() ->
    {ok, Hs} = application:get_env(hypernumbers, hosts),
    [ok = start_mochi_instance(H) || H <- Hs],
    ok.

start_mochi_instance({IP, Port}) ->
    StrIp = inet_parse:ntoa(IP),
    Opts = [{port, Port}, 
            {ip,   StrIp},
            {name, StrIp ++ "&" ++ integer_to_list(Port)},
            {loop, {hn_mochi, handle}}],
    {ok, _Pid} = mochiweb_http:start(Opts),
    ok.

dev_tasks() ->
    create_dev_zone(),
    local_hypernumbers().

create_dev_zone() ->
    Gen = fun() -> [crypto:rand_uniform($a, $z+1)] end,
    ok = hns:set_resource("127.0.0.1", 9000, node(), 1),
    hns:create_zone(?DEV_ZONE, 1, 26, Gen),
    ok.

local_hypernumbers() ->
    {ok, _, Uid} = passport:get_or_create_user("test@hypernumbers.com"),
    hn_setup:site("http://hypernumbers.dev:9000", blank, [{creator, Uid}]),
    passport:validate_uid(Uid),
    passport:set_password(Uid, "secure").

production_tasks() ->
    connect_peers().

connect_peers() ->
    case application:get_env(hypernumbers, super_peers) of
        {ok, [H|_Tail]} ->
            net_adm:ping(H),
            ok;
        _Else ->
            ok
    end.
