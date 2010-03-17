-module(hns).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_zone/4,
         delete_zone/1,
         request_name/1,
         topup_zone/1,
         set_server/2, set_server/3,
         delete_server/1,
         %% Diagostic Functions
         server_diagnostics/0,
         zone_diagnostics/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-type generator() :: fun(() -> string()). 

%%% Database Tables
-record(zone, { name :: string(),
                min_size :: integer(),
                ideal_size :: integer(),
                available :: gb_tree(),
                generator }).

-record(record, { name :: string(),
                  address :: string()}).

-record(server, { address = [] :: string(),
                  weight = 0 :: integer(),
                  alias = [] :: string()}).

%%% Internal State
-record(routing, {table = [],
                  upper_limit = 0}).

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
-spec request_name(string()) -> name_underflow | no_zone | {ok,string()}.
request_name(Zone) ->
    Msg = {request_name, Zone},
    do_request_name(?TRIES, Msg).
do_request_name(0, _Msg) ->
    name_underflow;
do_request_name(Tries, Msg) ->
    case gen_server:call({global, ?MODULE}, Msg) of
        no_zone -> no_zone;
        name_underflow ->  do_request_name(Tries-1, Msg);
        Name -> {ok, Name}
    end.    

-spec create_zone(string(), integer(), integer(), generator()) -> ok. 
create_zone(Zone, MinSize, IdealSize, Generator) when 
      MinSize > 0, IdealSize > 0 ->
    true = MinSize =< IdealSize,
    Msg = {create_zone, Zone, MinSize, IdealSize, Generator},
    gen_server:call({global, ?MODULE}, Msg).

-spec delete_zone(string()) -> ok. 
delete_zone(Zone) ->
    gen_server:call({global, ?MODULE}, {delete_zone, Zone}).

-spec topup_zone(string()) -> ok.
topup_zone(Zone) ->
    gen_server:cast({global, ?MODULE}, {topup_zone, Zone}).

%% Use this to add, or adjust the weighting of physical servers in the
%% cluster.
-spec set_server(string(), integer()) -> ok. 
set_server(Server, Weight) ->
    set_server(Server, Weight, []).
-spec set_server(string(), integer(), string()) -> ok. 
set_server(Server, Weight, Alias) when 
      is_integer(Weight), Weight >= 0 ->
    gen_server:call({global, ?MODULE}, {set_server, Server, Weight, Alias}).

-spec delete_server(atom() | string()) -> ok | not_found.
delete_server(Server) ->
    gen_server:call({global, ?MODULE}, {delete_server, Server}).

-spec server_diagnostics() -> ok.
server_diagnostics() ->
    S = gen_server:call({global, ?MODULE}, server_diagnostics),
    io:format("~s", [S]).

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
    ok = hn_db_admin:create_table(core_hns_server, 
                                  server, 
                                  record_info(fields, server),
                                  disc_copies,
                                  set,
                                  false,
                                  []),
    ok = hn_db_admin:create_table(core_hns_zone,
                                  zone,
                                  record_info(fields, zone),
                                  disc_copies,
                                  set,
                                  false,
                                  []),
    ok = hn_db_admin:create_table(core_hns_record,
                                  record,
                                  record_info(fields, record),
                                  disc_copies,
                                  set,
                                  false,
                                  []),
    Routing = build_routing(all_servers()),
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

handle_call({create_zone, Zone, MinSize, IdealSize, Generator}, _From, S) ->
    case mnesia:activity(async_dirty, fun mnesia:read/2, 
                         [core_hns_zone, Zone]) of
        [_] -> 
            {reply, already_exists, S};
        _ ->
            Available = allocate_names(IdealSize, S#state.routing, 
                                       Generator, 
                                       gb_trees:empty()),
            Entry = #zone{name = Zone,
                          min_size = MinSize,
                          ideal_size = IdealSize,
                          available = Available,
                          generator = Generator},
            ok = mnesia:activity(async_dirty, fun mnesia:write/3,
                                 [core_hns_zone, Entry, write]),
            {reply, ok, S}
    end;

handle_call({delete_zone, Zone}, _From, S) ->
    case mnesia:activity(async_dirty, fun mnesia:read/2,
                         [core_hns_zone, Zone]) of
        [#zone{available = A}] ->
            %% Deallocated names in pool.
            deallocate_names(gb_trees:values(A)),
            ok = mnesia:activity(async_dirty, fun mnesia:delete/3,
                                 [core_hns_zone, Zone, write]);
        _ ->
            ok
    end,
    {reply, ok, S};

handle_call({request_name, ZName}, _From, S) ->
    Val = mnesia:activity(async_dirty, 
                          fun mnesia:read/2,
                          [core_hns_zone, ZName]),
    Ret = case Val of 
              [Z] ->
                  case gb_trees:size(Z#zone.available) of
                      0 -> 
                          name_underflow;
                      _ -> 
                          {Name, Address, Z2} = get_name(Z),
                          Rec = #record{name=Name, address=Address},
                          F = fun() ->
                                      ok = mnesia:write(core_hns_zone, 
                                                        Z2, 
                                                        write),
                                      ok = mnesia:write(core_hns_record, 
                                                        Rec, 
                                                        write)
                              end,
                          ok = mnesia:activity(async_dirty, F),
                          Name
                  end;
              _ ->
                  no_zone
          end,
    {reply, Ret, S};

handle_call({set_server, Address, Weight, Alias}, _From, S) ->
    Entry = #server{address=Address, weight=Weight, alias=Alias},
    ok = mnesia:activity(async_dirty, 
                         fun mnesia:write/3, 
                         [core_hns_server, Entry, write]),
    Routing = build_routing(all_servers()),
    {reply, ok, S#state{routing = Routing}};

handle_call({delete_server, Address}, _From, S) ->
    ok = mnesia:activity(async_dirty,
                         fun mnesia:delete/3, 
                         [core_hns_server, Address, write]),
    Routing = build_routing(all_servers()),
    {reply, ok, S#state{routing = Routing}};

handle_call(server_diagnostics, _From, S=#state{routing=R}) ->
    Runs = 50000,
    Results = repeat_select(Runs, R, dict:new()),
    Format = "Name: ~-20s Wgt: ~5b Expect: ~5.3f Actual: ~5.3f~n",
    F = fun(#server{address=Address, weight=Weight, alias=Alias}, Acc) ->
                Expected = Weight / R#routing.upper_limit,
                Actual = case dict:find(Address, Results) of
                             {ok, V} -> V / Runs; 
                             _       -> 0.0 end,
                Name = if Alias == [] -> Address;
                          true        -> [Alias, " ", Address] end,
                L = io_lib:format(Format, [Name, Weight, Expected, Actual]),
                [L | Acc]
        end,
    Stats = lists:foldl(F, [], all_servers()),
    Title = io_lib:format("After ~b runs:~n---~n", [Runs]),
    Reply = lists:flatten([Title | Stats]),
    {reply, Reply, S};

handle_call(zone_diagnostics, _From, S) ->
    Format = "Zone: ~s~n"
        "   Names like: '~p'~n"
        "   Pool  size: ~b~n"
        "   Ideal size: ~b~n"
        "   Min   size: ~b~n"
        "-------------------------~n",
    F = fun(Z) ->
                io_lib:format(Format, [Z#zone.name, 
                                       (Z#zone.generator)(), 
                                       gb_trees:size(Z#zone.available),
                                       Z#zone.ideal_size, 
                                       Z#zone.min_size])
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
                         [core_hns_zone, ZName]) of
        [Z=#zone{available=Avail, ideal_size=Ideal, generator=Gen}] ->
            case gb_trees:size(Avail) of 
                S when S < Ideal ->
                    Avail2 = allocate_names(Ideal - S, 
                                            State#state.routing, 
                                            Gen, 
                                            Avail),
                    Z2 = Z#zone{available=Avail2},
                    mnesia:activity(async_dirty, fun mnesia:write/3,
                                    [core_hns_zone, Z2, write]);
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

-spec all_servers() -> [#server{}]. 
all_servers() ->
    F = fun() ->
                mnesia:select(
                  core_hns_server, 
                  ets:fun2ms(fun(X) -> X end),
                  read)
        end,
    mnesia:activity(async_dirty, F).

-spec all_zones() -> [#zone{}]. 
all_zones() ->
    F = fun() ->
                mnesia:select(
                  core_hns_zone, 
                  ets:fun2ms(fun(X) -> X end),
                  read)
        end,
    mnesia:activity(async_dirty, F).

%%%===================================================================
%%% Name Allocation
%%%===================================================================

%% Assumes available is non-empty.
-spec get_name(#zone{}) -> {string(), string(), #zone{}}.
get_name(Z=#zone{name=ZName, available=Available, min_size=Min}) ->
    {Name, Address, Available2} = 
        case gb_trees:size(Available) of
            N when N =< Min -> 
                spawn(?MODULE, topup_zone, [ZName]),
                gb_trees:take_smallest(Available);
            _ -> 
                gb_trees:take_smallest(Available)
        end,
    {Name, Address, Z#zone{available = Available2}}.

-spec allocate_names(integer(), #routing{}, generator(), gb_tree()) 
                    -> gb_tree().
allocate_names(0, _Routing, _Generator, Pool) -> 
    Pool;
allocate_names(N, Routing, Generator, Pool) ->
    Pool2 = allocate_name(Routing, Generator, Pool),
    allocate_names(N-1, Routing, Generator, Pool2).

-spec allocate_name(#routing{}, generator(), gb_tree()) -> gb_tree().
allocate_name(Routing, Generator, Pool) ->
    Name = Generator(),
    case name_exists(Name) of
        true  -> 
            Pool;
        false -> 
            Address = pick_server(Routing),
            ok = create_dns_entry_LINODE(Name, Address),
            gb_trees:enter(Name, Address, Pool)
    end.

-spec deallocate_names([string()]) -> ok. 
deallocate_names(Names) ->
    [ok = delete_dns_entry_LINODE(N) || N <- Names],
    ok.

-spec name_exists(string()) -> boolean(). 
name_exists(Name) ->
    case mnesia:activity(async_dirty, fun mnesia:read/2,
                         [core_hns_record, Name]) of
        [_]   -> true; 
        _Else -> false
    end.

create_dns_entry_LINODE(Name, Address) ->
    error_logger:info_msg("DNS create: ~p -> ~p~n", [Name, Address]),
    ok.

delete_dns_entry_LINODE(Site) ->
    error_logger:info_msg("DNS delete: ~p -> ~p~n", [Site]),
    ok.

%%%===================================================================
%%% Server selection 
%%%===================================================================

-spec pick_server(#routing{}) -> string().
pick_server(#routing{table = Tbl, upper_limit = Limit}) ->
    Val = crypto:rand_uniform(0, Limit),
    do_pick_server(Tbl, Val).

do_pick_server([], _) -> throw(no_servers);
do_pick_server([{Range, Server} | _], Val) when Val < Range -> Server;
do_pick_server([_ | Rest], Val) -> do_pick_server(Rest, Val).

-spec build_routing([#server{}]) -> #routing{}. 
build_routing(Servers) ->
    {Tbl, Upper} = lists:foldl(fun build_routing/2, {[], 0}, Servers),
    #routing{upper_limit = Upper,
             table = lists:reverse(Tbl)}.

-spec build_routing(#server{}, X) -> X when is_subtype(X, {list(), integer()}).
build_routing(#server{address=Key, weight=Val}, {Tbl, Count}) ->
    Count2 = Count + Val,
    Tbl2 = [{Count2, Key} | Tbl],
    {Tbl2, Count2}.

repeat_select(0, _, Results) ->
    Results;
repeat_select(N, S, Results) ->
    Srv = pick_server(S),
    Results2 = dict:update_counter(Srv, 1, Results),
    repeat_select(N-1, S, Results2).    

simple_test_() ->
    Servers = [#server{address = "alpha", weight = 5},
               #server{address = "beta", weight = 0},
               #server{address = "gamma", weight = 0}],
    R = build_routing(Servers),
    ?_assertEqual("alpha", pick_server(R)).

count_test_() ->
    Servers = [#server{address = "alpha", weight = 5},
               #server{address = "beta", weight = 20},
               #server{address = "gamma", weight = 5}],
    R = build_routing(Servers),
    ?_assertEqual(30, R#routing.upper_limit).
