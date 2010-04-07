-module(hns).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_zone/4, delete_zone/1, purge_zone/2, topup_zone/1,
         link_resource/1, unlink_resource/2,
         set_resource/4, delete_resource/3,
         resource_diagnostics/0, zone_diagnostics/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(API_KEY, 
        "C7ozR9fh4apHmfEwkWS7FrItSHDqGq9J3UTxm9JQrEEHnka3qA7wSHJYJM1kHVfs").
-define(DEV_ZONE_ID, -1).

-include("hypernumbers.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-type generator() :: fun(() -> string()). 
-type resource_addr() :: {string(), integer(), atom()}. %% (IP, Port, Node).

%%% Database Tables
-record(zone, { label :: string(),
                min_size :: integer(),
                ideal_size :: integer(),
                pool :: gb_tree(),
                generator :: generator(),
                zone_id :: integer() }).

-record(record, { name, % :: {string(), string()},
                  address,% :: resource_addr(),
                  %% Following two used for linode.
                  zone_id,% :: integer(),
                  resource_id }).% :: integer()}).

-record(resource, { address = [] :: resource_addr(),
                    weight = 0 :: integer() }).

%%% Internal State
-record(routing, { table = [], upper_limit = 0}).

-record(state, {routing = #routing{}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

    
-define(TRIES, 5).
%% Returns a name be mapped to one of the available resources, by
%% means of weighted-random draw.
-spec link_resource(string()) -> underflow | 
                                 no_zone |
                                 {ok,{string(), resource_addr()}}.
link_resource(Zone) ->
    Msg = {link_resource, Zone},
    do_link_resource(?TRIES, Msg).
do_link_resource(0, _Msg) ->
    underflow;
do_link_resource(Tries, Msg) ->
    case gen_server:call({global, ?MODULE}, Msg) of
        no_zone -> no_zone;
        underflow ->  do_link_resource(Tries-1, Msg);
        R -> {ok, R}
    end.    

-spec unlink_resource(string(), string()) -> ok.
unlink_resource(ZoneL, Name) ->
    gen_server:call({global, ?MODULE}, {unlink_resource, ZoneL, Name}).
                             
%% Create a zone responsible for the dispensation of mappings from
%% names to resources, backed by DNS. Since requests for new mappings
%% are non-linear, and DNS propegation is not instant, these mappings
%% are buffred in pools. Minsize is a lower watermark which should
%% trigger a pool topup to the IdealSize. The Generator function
%% should yield a distribution of names suitable for the size of the
%% target domain, as otherwise buffer underflows shall occur. The
%% Generator need not have memory, and as such, uniquness is not a
%% hard but a soft condition. (duplicates are disgarded).
-spec create_zone(string(), integer(), integer(), generator()) -> ok. 
create_zone(Zone, MinSize, IdealSize, Generator) when 
      MinSize > 0, IdealSize > 0 ->
    true = MinSize =< IdealSize,
    Msg = {create_zone, Zone, MinSize, IdealSize, Generator},
    gen_server:call({global, ?MODULE}, Msg, infinity).

%% Delete the current zone, and unlink any pooled resource mappings
-spec delete_zone(string()) -> ok. 
delete_zone(Zone) ->
    gen_server:call({global, ?MODULE}, {delete_zone, Zone}, infinity).

%% _CAUTION_ Purging will _IRREVOCABLY REMOVE_ all resource mappings
%% associated with this zone!
-spec purge_zone(string(), atom()) -> ok.
purge_zone(Zone, i_really_know_what_i_am_doing) ->
    delete_zone(Zone),
    gen_server:call({global, ?MODULE}, {purge, Zone}, infinity).

%% Tops up the zone's pooled resource mappings to the ideal size. This
%% should usually only be called internally.
-spec topup_zone(string()) -> ok.
topup_zone(Zone) ->
    gen_server:cast({global, ?MODULE}, {topup_zone, Zone}).

%% Adds, (or adjusts existing) resources to system. A resource is
%% describe by an address (IP, Node combination); and a weighted
%% importance with respect to all other resources.
-spec set_resource(string(), integer(), atom(), integer()) -> ok. 
set_resource(IP, Port, Node, Weight) when 
      is_integer(Weight), Weight >= 0 ->
    gen_server:call({global, ?MODULE}, 
                    {set_resource, {IP, Port, Node}, Weight}).

%% Removes a resource from future consideration for
%% mappings.TODO:Should pre-mapped names to this resource be pruned???
-spec delete_resource(string(), integer(), atom()) -> ok | not_found.
delete_resource(IP, Port, Node) ->
    gen_server:call({global, ?MODULE}, {delete_resource, {IP, Port, Node}}).

%% Useful for listing active resources, and testing random distribution.
-spec resource_diagnostics() -> ok.
resource_diagnostics() ->
    S = gen_server:call({global, ?MODULE}, resource_diagnostics),
    io:format("~s", [S]).

%% Prints infomation in existing zones. 
-spec zone_diagnostics() -> ok.
zone_diagnostics() ->
    S = gen_server:call({global, ?MODULE}, zone_diagnostics),
    io:format("~s", [S]).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ok = hn_db_admin:create_table(service_hns_resource, 
                                  resource, 
                                  record_info(fields, resource),
                                  disc_copies,
                                  set,
                                  false,
                                  []),
    ok = hn_db_admin:create_table(service_hns_zone,
                                  zone,
                                  record_info(fields, zone),
                                  disc_copies,
                                  set,
                                  false,
                                  []),
    ok = hn_db_admin:create_table(service_hns_record,
                                  record,
                                  record_info(fields, record),
                                  disc_copies,
                                  set,
                                  false,
                                  []),
    Routing = build_routing(all_resources()),
    {ok, #state{routing = Routing}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({create_zone, ZoneL, MinSize, IdealSize, Generator}, _From, S) ->
    case mnesia:activity(async_dirty, fun mnesia:read/2, 
                         [service_hns_zone, ZoneL]) of
        [_] -> 
            {reply, already_exists, S};
        _ ->
            ZoneId = zone_id_LINODE(ZoneL),
            Pool = allocate_names(IdealSize, S#state.routing, Generator, 
                                  ZoneL, ZoneId, gb_trees:empty()),
            Entry = #zone{label = ZoneL,
                          zone_id = ZoneId,
                          min_size = MinSize,
                          ideal_size = IdealSize,
                          pool = Pool,
                          generator = Generator},
            ok = mnesia:activity(async_dirty, fun mnesia:write/3,
                                 [service_hns_zone, Entry, write]),
            {reply, ok, S}
    end;

handle_call({delete_zone, ZoneL}, _From, S) ->
    case mnesia:activity(async_dirty, fun mnesia:read/2,
                         [service_hns_zone, ZoneL]) of
        [#zone{pool = A, zone_id=ZoneId}] ->
            %% Deallocated mappings in pool. 
            [delete_dns_LINODE(ZoneId, Rid) || {_,Rid} <- gb_trees:values(A)],
            ok = mnesia:activity(async_dirty, fun mnesia:delete/3,
                                 [service_hns_zone, ZoneL, write]);
        _ -> ok end,
    {reply, ok, S};

handle_call({purge, ZoneL}, _From, S) ->
    MS = ets:fun2ms(fun(#record{name = {Z, _}}=R) 
                          when ZoneL == Z -> R 
                    end),
    DelF = fun() ->
                   Recs = mnesia:select(service_hns_record, MS, write),
                   [mnesia:delete_object(service_hns_record, R, write) || R <- Recs],
                   Recs
           end,
    Recs = mnesia:activity(async_dirty, DelF),
    [delete_dns_LINODE(ZID, RID) || #record{zone_id = ZID, 
                                            resource_id = RID} <- Recs],
    {reply, ok, S};

handle_call({link_resource, ZoneL}, _From, S) ->
    Val = mnesia:activity(async_dirty, 
                          fun mnesia:read/2,
                          [service_hns_zone, ZoneL]),
    Ret = case Val of 
              [Z] ->
                  case gb_trees:size(Z#zone.pool) of
                      0 -> 
                          underflow;
                      _ -> 
                          {Name, Address, RId, Z2} = get_mapping(Z),
                          Rec = #record{name = {Z2#zone.label, Name},
                                        address = Address,
                                        zone_id = Z2#zone.zone_id,
                                        resource_id = RId},
                          F = fun() ->
                                      ok = mnesia:write(service_hns_zone, 
                                                        Z2, 
                                                        write),
                                      ok = mnesia:write(service_hns_record, 
                                                        Rec, 
                                                        write)
                              end,
                          ok = mnesia:activity(async_dirty, F),
                          {Name ++ "." ++ Z2#zone.label, Address}
                  end;
              _ ->
                  no_zone
          end,
    {reply, Ret, S};

handle_call({unlink_resource, ZoneL, Name}, _From, S) ->
    Val = mnesia:activity(async_dirty,
                          fun mnesia:read/2,
                          [service_hns_record, {ZoneL, Name}]),
    case Val of 
        [#record{zone_id = ZID, resource_id = RID}] -> 
            delete_dns_LINODE(ZID, RID),
            mnesia:activity(async_dirty,
                            fun mnesia:delete/3,
                            [service_hns_record, Name, write]);
        _ ->
            ok
    end,
    {reply, ok, S};

handle_call({set_resource, Address, Weight}, _From, S) ->
    Entry = #resource{address=Address, weight=Weight},
    ok = mnesia:activity(async_dirty, 
                         fun mnesia:write/3, 
                         [service_hns_resource, Entry, write]),
    Routing = build_routing(all_resources()),
    {reply, ok, S#state{routing = Routing}};

handle_call({delete_resource, Address}, _From, S) ->
    ok = mnesia:activity(async_dirty,
                         fun mnesia:delete/3, 
                         [service_hns_resource, Address, write]),
    Routing = build_routing(all_resources()),
    {reply, ok, S#state{routing = Routing}};

handle_call(resource_diagnostics, _From, S=#state{routing=R}) ->
    Runs = 50000,
    Results = repeat_select(Runs, R, dict:new()),
    Format = "Name: ~-20s Wgt: ~5b Expect: ~5.3f Actual: ~5.3f~n",
    F = fun(#resource{address=Address, weight=Weight}, Acc) ->
                Expected = Weight / R#routing.upper_limit,
                Actual = case dict:find(Address, Results) of
                             {ok, V} -> V / Runs; 
                             _       -> 0.0 end,
                Name = lists:flatten(io_lib:format("~p", [Address])),
                L = io_lib:format(Format, [Name, Weight, Expected, Actual]),
                [L | Acc]
        end,
    Stats = lists:foldl(F, [], all_resources()),
    Title = io_lib:format("After ~b runs:~n---~n", [Runs]),
    Reply = lists:flatten([Title | Stats]),
    {reply, Reply, S};

handle_call(zone_diagnostics, _From, S) ->
    Format = "Zone: ~s~n"
        "   Names like: '~p'~n"
        "   Linode Id : ~b~n"
        "   Pool  size: ~b~n"
        "   Ideal size: ~b~n"
        "   Min   size: ~b~n"
        "-------------------------~n",
    F = fun(Z) ->
                io_lib:format(
                  Format, [ Z#zone.label, 
                            (Z#zone.generator)() ++ "." ++ Z#zone.label,
                            Z#zone.zone_id,
                            gb_trees:size(Z#zone.pool),
                            Z#zone.ideal_size, 
                            Z#zone.min_size ])
        end,
    Reply = lists:flatten([F(Z) || Z <- all_zones()]),
    {reply, Reply, S};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({topup_zone, ZName}, State) ->
    case mnesia:activity(async_dirty, fun mnesia:read/2, 
                         [service_hns_zone, ZName]) of
        [Z=#zone{ideal_size=Ideal}] ->
            case gb_trees:size(Z#zone.pool) of 
                S when S < Ideal ->
                    Pool2 = allocate_names(Ideal - S, 
                                           State#state.routing, 
                                           Z#zone.generator,
                                           Z#zone.label,
                                           Z#zone.zone_id,
                                           Z#zone.pool),
                    Z2 = Z#zone{pool=Pool2},
                    mnesia:activity(async_dirty, fun mnesia:write/3,
                                    [service_hns_zone, Z2, write]);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Database Helpers
%%%===================================================================

-spec all_resources() -> [#resource{}]. 
all_resources() ->
    F = fun() ->
                mnesia:select(
                  service_hns_resource, 
                  ets:fun2ms(fun(X) -> X end),
                  read)
        end,
    mnesia:activity(async_dirty, F).

-spec all_zones() -> [#zone{}]. 
all_zones() ->
    F = fun() ->
                mnesia:select(
                  service_hns_zone, 
                  ets:fun2ms(fun(X) -> X end),
                  read)
        end,
    mnesia:activity(async_dirty, F).

%%%===================================================================
%%% Name Allocation
%%%===================================================================

%% Assumes pool is non-empty.
-spec get_mapping(#zone{}) -> {string(), resource_addr(), integer(), #zone{}}.
get_mapping(Z=#zone{label=Zone, pool=Pool, min_size=Min}) ->
    {Name, {Address, ResourceId}, Pool2} = 
        case gb_trees:size(Pool) of
            N when N =< Min -> 
                spawn(?MODULE, topup_zone, [Zone]),
                gb_trees:take_smallest(Pool);
            _ -> 
                gb_trees:take_smallest(Pool)
        end,
    {Name, Address, ResourceId, Z#zone{pool = Pool2}}.

-spec allocate_names(integer(), #routing{}, generator(), 
                     string(), integer(), gb_tree())
                    -> gb_tree().
allocate_names(X, _Routing, _Generator, _ZoneL, _ZoneId, Pool) when X =< 0 -> 
    Pool;
allocate_names(N, Routing, Generator, ZoneL, ZoneId, Pool) ->
    Pool2 = allocate_name(Routing, Generator, ZoneL, ZoneId, Pool),
    allocate_names(N-1, Routing, Generator, ZoneL, ZoneId, Pool2).

-spec allocate_name(#routing{}, generator(), string(), 
                    integer(), gb_tree())
                   -> gb_tree().
allocate_name(Routing, Generator, ZoneL, ZoneId, Pool) ->
    Name = Generator(),
    case record_exists({ZoneL, Name}) of
        true  -> 
            Pool;
        false -> 
            Address={IP, _Port, _Node} = pick_resource(Routing),
            ResourceID = create_dns_LINODE(Name, IP, ZoneId),
            gb_trees:enter(Name, {Address, ResourceID}, Pool)
    end.

-spec record_exists({string(), string()}) -> boolean(). 
record_exists(Key) ->
    case mnesia:activity(async_dirty, fun mnesia:read/2,
                         [service_hns_record, Key]) of
        [_]   -> true; 
        _Else -> false
    end.

-spec zone_id_LINODE(string()) -> integer().
zone_id_LINODE(?DEV_ZONE) -> ?DEV_ZONE_ID;
zone_id_LINODE(Zone) ->
    Struct = linode_api([{"api_action", "domain.list"}]),
    {array, Domains} = get_data(Struct),
    seek_zone_id(Domains, Zone).

-spec create_dns_LINODE(string(), string(), integer()) -> integer(). 
create_dns_LINODE(_Name, _Address, ?DEV_ZONE_ID) -> 
    crypto:rand_uniform(0, 1000);
create_dns_LINODE(Name, Address, ZoneId) ->
    ZoneIdStr = integer_to_list(ZoneId),
    Struct = linode_api([{"api_action", "domain.resource.create"},
                        {"DomainID", ZoneIdStr},
                        {"Type", "A"},
                        {"Name", Name},
                        {"Target", Address}]),
    {struct,[{"ResourceID",ResourceID}]} = get_data(Struct),
    ResourceID.
    
-spec delete_dns_LINODE(integer(), integer()) -> any(). 
delete_dns_LINODE(?DEV_ZONE_ID, _Reason) -> ok;
delete_dns_LINODE(ZoneId, ResourceId) ->
    ZoneIdStr = integer_to_list(ZoneId),
    ResourceIdStr = integer_to_list(ResourceId),
    linode_api([{"api_action", "domain.resource.delete"},
                {"DomainID", ZoneIdStr},
                {"ResourceID", ResourceIdStr}]).

get_data({struct,Attrs}) ->
    proplists:get_value("DATA", Attrs).

-spec seek_zone_id([{struct, list()}], string()) -> integer(). 
seek_zone_id([], _Zone) ->
    throw("Cannot find zone id");
seek_zone_id([{struct, Attrs} | Rest], Zone) ->
    case proplists:get_value("DOMAIN", Attrs) of
        Zone  -> proplists:get_value("DOMAINID",  Attrs);
        _Else -> seek_zone_id(Rest, Zone)
    end.
    
-spec linode_api([{string(), string()}]) -> {struct, any()}.
linode_api(Cmds0) ->
    Cmds = [{"api_key", ?API_KEY} | Cmds0],
    Qry = [$?|lists:flatten(string:join([[K,"=",V] || {K,V} <- Cmds], "&"))],
    Url = "https://api.linode.com/" ++ Qry,
    Hdrs = [{"Host", "api.linode.com"}, {"Accept", "application/json"}],
    {ok,{{"HTTP/1.1",200,"OK"},_, Json}} = httpc:request(get, {Url, Hdrs}, [], []),
    mochijson:decode(Json).

%%%===================================================================
%%% Resource selection 
%%%===================================================================

-spec pick_resource(#routing{}) -> resource_addr().
pick_resource(#routing{table = Tbl, upper_limit = Limit}) ->
    Val = crypto:rand_uniform(0, Limit),
    do_pick_resource(Tbl, Val).

do_pick_resource([], _) -> throw(no_resources);
do_pick_resource([{Range, Resource} | _], Val) when Val < Range -> Resource;
do_pick_resource([_ | Rest], Val) -> do_pick_resource(Rest, Val).

-spec build_routing([#resource{}]) -> #routing{}. 
build_routing(Resources) ->
    {Tbl, Upper} = lists:foldl(fun build_routing/2, {[], 0}, Resources),
    #routing{upper_limit = Upper,
             table = lists:reverse(Tbl)}.

-spec build_routing(#resource{}, X) -> X when is_subtype(X, {list(), integer()}).
build_routing(#resource{address=Key, weight=Val}, {Tbl, Count}) ->
    Count2 = Count + Val,
    Tbl2 = [{Count2, Key} | Tbl],
    {Tbl2, Count2}.

repeat_select(0, _, Results) ->
    Results;
repeat_select(N, S, Results) ->
    Srv = pick_resource(S),
    Results2 = dict:update_counter(Srv, 1, Results),
    repeat_select(N-1, S, Results2).    

-spec simple_test_() -> no_return(). 
simple_test_() ->
    Resources = [#resource{address = {"127.0.0.1", 80, alpha}, 
                           weight = 5},
                 #resource{address = {"10.10.10.10", 80, beta},
                           weight = 0},
                 #resource{address = {"192.168.1.1", 80, gamma}, 
                           weight = 0}],
    R = build_routing(Resources),
    ?_assertEqual({"127.0.0.1",80,alpha}, pick_resource(R)).

-spec count_test_() -> no_return(). 
count_test_() ->
    Resources = [#resource{address = {"127.0.0.1", 80, alpha}, 
                           weight = 5},
                 #resource{address = {"10.10.10.10", 80, beta},
                           weight = 20},
                 #resource{address = {"192.168.1.1", 80, gamma}, 
                           weight = 5}],
    R = build_routing(Resources),
    ?_assertEqual(30, R#routing.upper_limit).
