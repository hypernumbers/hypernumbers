-module(hn_routing).

%% Initial routing server for provisioning.


-behaviour(gen_server).

%% API
-export([start_link/0,
         set_server/2,
         delete_server/1,
         pick_server/0,
         diagnostics/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {mappings = dict:new(),
                %% Items below depend on mappings,
                upper_limit = 0,
                routing_tbl = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set_server(atom() | string(), integer()) -> ok. 
set_server(Server, Weight) when 
      is_integer(Weight), Weight >= 0 ->
    gen_server:call(?MODULE, {set_server, Server, Weight}).

delete_server(Server) ->
    gen_server:call(?MODULE, {delete_server, Server}).

-spec pick_server() -> ok.
pick_server() ->
    gen_server:call(?MODULE, pick_server).

-spec diagnostics() -> ok.
diagnostics() ->
    S = gen_server:call(?MODULE, diagnostics),
    io:format("~s", [S]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({set_server, Server, Weight}, _From, S) ->
    Mappings2 = dict:store(Server, Weight, S#state.mappings),
    {reply, ok, update_state(S#state{mappings = Mappings2})};

handle_call({delete_server, Server}, _From, S) ->
    Mappings2 = dict:erase(Server, S#state.mappings),
    {reply, ok, update_state(S#state{mappings = Mappings2})};

handle_call(pick_server, _From, S) ->
    Reply = pick_server(S),
    {reply, Reply, S};

handle_call(diagnostics, _From, S) ->
    Runs = 100000,
    Results = repeat_select(Runs, S, dict:new()),
    Format = "Name: ~-12s Wgt: ~5b Expect: ~5.3f Actual: ~5.3f~n",
    F = fun(Server, Weight, Acc) ->
                Expected = Weight / S#state.upper_limit,
                Actual = case dict:find(Server, Results) of
                             {ok, V} -> V / Runs; 
                             _ -> 0.0
                         end,
                L = io_lib:format(Format, [Server, Weight, Expected, Actual]),
                [L | Acc]
        end,
    Stats = dict:fold(F, [], S#state.mappings),
    Title = io_lib:format("After ~b runs:~n---~n", [Runs]),
    Reply = lists:flatten([Title | Stats]),
    {reply, Reply, S};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec pick_server(#state{}) -> no_server | atom() | string().
pick_server(#state{routing_tbl = Tbl, upper_limit = Limit}) ->
    Val = crypto:rand_uniform(0, Limit),
    do_pick_server(Tbl, Val).

do_pick_server([], _) -> no_server;
do_pick_server([{Range, Server} | _], Val) when Val < Range -> Server;
do_pick_server([_ | Rest], Val) -> do_pick_server(Rest, Val).

-spec update_state(#state{}) -> #state{}. 
update_state(S) ->
    {Tbl, Upper} = dict:fold(fun build_routing/3, 
                             {[], 0}, 
                             S#state.mappings),
    S#state{upper_limit = Upper,
            routing_tbl = lists:reverse(Tbl)}.

-spec build_routing(atom() | string(), integer(), {list(), integer()}) 
                   -> {list(), integer()}.
build_routing(_Key, 0, Acc) -> Acc;
build_routing(Key, Val, {Tbl, Count}) ->
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
    Mappings = [{alpha, 5},
                {beta, 0},
                {gamma, 0}],
    S = update_state(#state{mappings = dict:from_list(Mappings)}),
    ?_assertEqual(alpha, pick_server(S)).

count_test_() ->
    Mappings = [{alpha, 5},
                {beta, 20},
                {gamma, 5}],
    S = update_state(#state{mappings = dict:from_list(Mappings)}),
    ?_assertEqual(30, S#state.upper_limit).
