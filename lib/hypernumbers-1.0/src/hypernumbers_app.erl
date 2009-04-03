%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hypernumbers_app).
-behaviour(application).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-export([start/2, stop/1, clean_start/0 ]).
-export([set_def_perms/0]).

%% @spec start(Type,Args) -> {ok,Pid} | Error
%% @doc  Application callback
    %% @TODO this test is buggy - the schema is always on disc 
    %%      but invididual tables can be:
    %%      <ul>
    %%      <li>ram_copies</li>
    %%      <li>disc_copies</li>
    %%      <li>disc_only_copies</li>
    %%      </ul>
start(_Type, _Args) ->

    case mnesia:table_info(schema, storage_type) of
        ram_copies -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ -> 
            ok
    end,
        
    {ok, Pid} = hypernumbers_sup:start_link(),
    {ok,[[Path]]} = init:get_argument(hn_config),	
    hn_config:read_conf(Path),
    
    case is_fresh_startup() of
        true  -> clean_start();
        false -> ok
    end,
    
    ok = start_mochiweb(),
    ok = start_dirty_subscribe(),
    {ok, Pid}.

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
    % Probably not a nice way to do this, 
    % Before everything is restarted the msg queues
    % for these needs to be emptied
    Kill = fun(undefined) -> ok;
              (Pid)       -> exit(Pid,clean_start)
           end,
    lists:map(fun(X) -> Kill(whereis(X)) end,
              [dirty_cell, dirty_notify_in, dirty_inc_hn_create,
               dirty_notify_back_in, dirty_notify_out,
               dirty_notify_back_out]),

    ok = hn_db_api:create_db(),
    set_def_perms(),
    ok = start_dirty_subscribe(),
    ok.

%% @spec start_mochiweb() -> ok
%% @doc  Start mochiweb http server
start_mochiweb() ->
    [{IP, Port, _Hosts}] = hn_config:get(hosts),
    Opts = [{port, Port}, {ip, inet_parse:ntoa(IP)}, {loop, {hn_mochi, req}}],
    mochiweb_http:start(Opts),
    ok.

%% @spec start_dirty_subscribe() -> ok
%% @doc  Make dirty_x gen_srv's subscribe to mnesia
start_dirty_subscribe() ->
    ok = gen_server:cast(dirty_cell,            subscribe),
    ok = gen_server:cast(dirty_notify_in,       subscribe),
    ok = gen_server:cast(dirty_inc_hn_create,   subscribe),
    ok = gen_server:cast(dirty_notify_back_in,  subscribe),
    ok = gen_server:cast(dirty_notify_out,      subscribe),
    ok = gen_server:cast(dirty_notify_back_out, subscribe),
    ok.

%% @spec set_def_perms() -> ok
%% @doc  Set the default permissions on each domain
set_def_perms() ->
    
    [{IP, Port, Domains}] = hn_config:get(hosts),
    PStr = integer_to_list(Port),
    Url  = "http://"++inet_parse:ntoa(IP)++":"++PStr,
    
    Permissions = {"__permissions", hn_config:get(permissions)},
    Groups      = {"__groups",      hn_config:get(groups)},

    F = fun(X) -> lists:concat(["http://",X, ":", PStr]) end,
    
    lists:foreach(fun(X) -> set_perms(X, Groups, Permissions) end, 
                  [Url | lists:map(F, Domains)]), 
    ok.

%% @spec set_perms(Domains) -> Return
%% @doc  Supervisor call back
set_perms(Domain, Groups, Permissions) ->
    
    Ref = #refX{site = Domain,
               path = [],
               obj  = {page,"/"}},
 
    hn_db_api:write_attributes(Ref, [Groups, Permissions]),
    
    ok.
