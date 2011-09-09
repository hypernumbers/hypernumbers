 %%%-------------------------------------------------------------------
 %%% @author    Gordon Guthrie
 %%% @copyright (C) 2011, Hypernumbers ltd
 %%% @doc       This gen_server handles the remoting register for each
 %%%            individual page
 %%%
 %%% @end
 %%% Created :  5 Sep 2011 by gordon@hypernumbers.com
 %%%-------------------------------------------------------------------
 -module(remoting_srv).

 -behaviour(gen_server).

 -record(state, {site = [], path = [], updates = [], waiting = []}).

 %% API
 -export([
          start_link/2,
          timestamp/0
         ]).

 %% gen_server callbacks
 -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

 -define(SERVER, ?MODULE).

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
 start_link(Site, Path) ->
     gen_server:start_link(?MODULE, [Site, Path], []).

 %%%===================================================================
 %%% gen_server callbacks
 %%%===================================================================

 %%--------------------------------------------------------------------
 %% @private
 %% @doc
 %% Initiates the server
 %%
 %% @spec init(Args) -> {ok, State} |
 %%                     {ok, State, Timeout} |
 %%                     ignore |
 %%                     {stop, Reason}
 %% @end
 %%--------------------------------------------------------------------
 init([Site, Path]) ->
    % stash the site and path the server refers to
    % makes life easier on the old debugging front...
    {ok, #state{site = Site, path = Path}}.

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
handle_cast({MsgType, Site, Path, Msg}, State)
  when MsgType == msg orelse MsgType == sitemsg ->
    Packet = {MsgType, Site, Path, Msg, timestamp()},
    {noreply, forward_to_client(Packet, State)};
handle_cast({fetch, _Site, Path, Time, Pid}, State) ->
    {noreply, fetch_queued(Path, Time, Pid, State)}.

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
%%% Internal functions
%%%===================================================================
%% @doc  Send an update to the comet server to forward to client
send_to_server(Server, Time, Msgs) ->
    Server ! {msg, {struct, [{"time", Time},
                             {"msgs", {array, lists:reverse(Msgs)}}]}}.

%% @doc  When an update is received, automatically send the update to
%%       any clients waiting on the same page
forward_to_client({_, _, _, Contents, Time} = Msg, State) ->
    #state{updates = U, waiting = W} = State,
    [send_to_server(X, Time, [Contents]) || X <- W],
    NU = expire_updates([Msg | U]),
    % clear off the waiting...
    #state{updates = NU, waiting = []}.

%% @doc  When a client requests data, send any message that are older
%%       than the time the client reports (and on the same site)
fetch_queued(_Path, Time, Pid, #state{updates = U, waiting = W} = State) ->
    NU = expire_updates(U),
    NW = case has_newer(Time, NU) of
             []   ->
                 [Pid | W];
             Msgs ->
                 Contents = [X || {msg, _, _, X, _} <- Msgs],
                 send_to_server(Pid, timestamp(), Contents),
                 lists:delete(Pid, W)
         end,
    State#state{updates = NU, waiting = purge(NW)}.

timestamp() ->
    microsecs(erlang:now()).

microsecs({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

%% Expires any messages older than one minute
expire_updates(Old) ->
    MinuteAgo = timestamp() - 60000000,
    [ Msg || Msg = {msg, _Site, _Path, _Msg, Time} <- Old,
             Time > MinuteAgo].

has_newer(Time, List) ->
    Fun = fun({msg, _, _, _, Time1}) ->
                  Time1 > Time
          end,
    lists:filter(Fun, List).

purge(PIDs) -> p2(PIDs, []).

p2([], Acc)      -> Acc;
p2([H | T], Acc) -> NewAcc = case is_process_alive(H) of
                                 true  -> [H | Acc];
                                 false -> Acc
                             end,
                    p2(T, NewAcc).

%%
%% Tests
%%
%% -include_lib("eunit/include/eunit.hrl").
%% -define(SITE, "http://example.com:1234").

%% unit_test_() ->
%%     %% linked pid will die when test ends
%%     Setup = fun() -> remoting_reg:start_link(?SITE) end,
%%     Cleanup = fun(_) -> ok end,
%%     {setup, Setup, Cleanup,
%%      [fun test_basic_update/0,
%%       fun test_multiple_update/0,
%%       fun test_basic_waiting/0,
%%       fun test_multiple_waiting/0]}.

%% run_changes() ->
%%     remoting_reg:notify_change(?SITE, [], {cell, {1,1}}, [{"value", 99}]),
%%     remoting_reg:notify_change(?SITE, ["test"], {cell, {1,1}}, [{"value", 88}]),
%%     remoting_reg:notify_change(?SITE, [], {cell, {1,1}}, [{"value", 77}]).

%% get_changes(Path, Time) ->
%%     remoting_reg:request_update(?SITE, Path, Time, self()),
%%     receive
%%         {msg, {struct, [{"time", TStamp}, {"msgs", {array, Msgs}}]}} ->
%%             {TStamp, Msgs}
%%     after
%%         1000 -> {null, []}
%%     end.

%% test_basic_update() ->
%%     Time = timestamp(),
%%     run_changes(),
%%     {_, Msgs} = get_changes([[]], Time),
%%     ?assertEqual(2, length(Msgs)).

%% test_multiple_update() ->
%%     Time = timestamp(),
%%     run_changes(),
%%     {_, Msgs} = get_changes([[], ["test"]], Time),
%%     ?assertEqual(3, length(Msgs)).

%% test_basic_waiting() ->
%%     spawn( fun() -> timer:sleep(100), run_changes() end ),
%%     {Time, Changes1} = get_changes([[]], timestamp()),
%%     {_, Changes2} = get_changes([[]], Time),
%%     ?assertEqual(2, length(Changes1) + length(Changes2)).

%% test_multiple_waiting() ->
%%     spawn( fun() -> timer:sleep(100), run_changes() end ),
%%     {Time, Changes1} = get_changes([[], ["test"]], timestamp()),
%%     {_, Changes2} = get_changes([[], ["test"]], Time),
%%     ?assertEqual(3, length(Changes1) + length(Changes2)).
