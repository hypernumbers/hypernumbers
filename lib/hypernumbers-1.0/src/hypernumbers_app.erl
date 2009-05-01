%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hypernumbers_app).
-behaviour(application).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(cast, gen_server:cast).

-export([start/2, stop/1, clean_start/0, hup/0 ]).

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

    % HostsInfo = hn_config:get(hosts),
    % Sites = hn_util:get_hosts(HostsInfo),
    
    ok = start_mochiweb(),
    [ok = dirty_subscriber:monitor(X) || X <- ?dirties],
    {ok, Pid}.

hup() ->
    hn_config:hup(),
    write_permissions().

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
        [schema] -> true;
        _ ->
            Me = node(),
            case mnesia:table_info(schema, cookie) of
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
              (Pid)       -> exit(Pid, clean_start)
           end,
    lists:map(fun(X) -> Kill(whereis(X)) end,
              [dirty_cell, dirty_notify_in, dirty_inc_hn_create,
               dirty_notify_back_in, dirty_notify_out,
               dirty_notify_back_out]),

    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),

    [ok = hn_db_api:create_db(X) || X <- Sites],
    [ok = dirty_subscriber:monitor(X) || X <- ?dirties],
    ok = write_permissions(),
    ok.

%% @spec start_mochiweb() -> ok
%% @doc  Start mochiweb http servers
%% @todo this server will accept a connection to any
%% domain name on the ip address, wtf?
start_mochiweb() ->

    List = hn_config:get(hosts),
    % need to compress the list in case it contains duplicate ip address/port
    % combos
    List2 = compress(List),
    Fun = fun(X) ->
                  {IP, Port} = X,
          
                      Opts = [{port, Port}, 
                              {ip,   inet_parse:ntoa(IP)}, 
                              {loop, {hn_mochi, req}}],
                  
                  mochilog:start(),
                  mochiweb_http:start(Opts)
          end,
    [Fun(X) || X <- List2],
    ok.

compress(List) -> cmp1(List, []).

cmp1([], Acc)                       -> Acc;
cmp1([{IP, Port, _Hosts} | T], Acc) -> case lists:member({IP, Port}, Acc) of
                                           true  -> cmp1(T, Acc);
                                           false -> cmp1(T, [{IP, Port} | Acc])
                                       end.

%% @spec dirty_subscribe() -> ok
%% @doc  Make dirty_x gen_srv's (un)subscribe to mnesia
%dirty_subscribe(Type) ->
    
%    ok = ?cast(dirty_cell,            {Type, dirty_cell}),
%    ok = ?cast(dirty_notify_in,       {Type, dirty_notify_in}),
%    ok = ?cast(dirty_inc_hn_create,   {Type, dirty_inc_hn_create}),
%    ok = ?cast(dirty_notify_back_in,  {Type, dirty_notify_back_in}),
%    ok = ?cast(dirty_notify_out,      {Type, dirty_notify_out}),
%    ok = ?cast(dirty_notify_back_out, {Type, dirty_notify_back_out}).

%% @spec write_permissions() -> ok
%% @doc  Set the default permissions on each domain
write_permissions() ->
    write_permissions("__permissions", hn_config:get(permissions)),
    write_permissions("__groups",      hn_config:get(groups)).

write_permissions(_Name, []) -> 
    ok;
write_permissions(Name, [{Domain, Value} | T]) ->
    Ref = hn_mochi:parse_ref(Domain),
    #refX{site = Site} = Ref,
    Site2 = trim(Site),
    % now do some process dictionary magic
    _OldPD = put(sitename, Site2),
    hn_db_api:write_attributes(Ref, [{Name, Value}]),
    write_permissions(Name, T).

trim("http://"++Site) -> Site.
