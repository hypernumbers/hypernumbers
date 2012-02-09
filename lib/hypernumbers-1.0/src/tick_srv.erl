%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       The tick server provides tick functionality
%%%
%%% @end
%%% Created :  2 May 2011 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(tick_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([tick/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-define(ZERO, 0). % makes debugging easier

-record(state, {site, timerref}).

%%%===================================================================
%%% API
%%%===================================================================
tick(Site) ->
    Id = hn_util:site_to_atom(Site, "_tick"),
    gen_server:cast({global, Id}, tick).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(list()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Site) ->
    case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting tick_srv for ~p~n", [Site]);
       _Other     -> ok
    end,
    Id = hn_util:site_to_atom(Site, "_tick"),
    gen_server:start_link({global, Id}, ?MODULE, [Site], []).

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
init([Site]) ->
    TRef = setup_tick(Site),
    {ok, #state{site = Site, timerref = TRef}}.

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
handle_cast(_Msg, #state{site = Site} = State) ->
    Timers = new_db_api:read_timers(Site),
    {{Y, M, DoM}, {H, _M, _S}} = calendar:now_to_datetime(now()),
    Day = calendar:day_of_the_week({Y, M, DoM}),
    LeapYear = calendar:is_leap_year(Y),
    ok = run(Site, {LeapYear, M, DoM, Day, H}, Timers),
    TRef = setup_tick(Site),
    {noreply, State#state{timerref = TRef}}.

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
terminate(_Reason, #state{timerref = TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
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
setup_tick(Site) ->
    {_D, {H, M, S}} = calendar:now_to_datetime(now()),
    NowTime = calendar:time_to_seconds({H, M, S}),
    TickTime = calendar:time_to_seconds({H + 1, 0, 0}),
    TimeDiff = (TickTime - NowTime) * 1000,
    {ok, TRef}  = timer:apply_after(TimeDiff, tick_srv, tick, [Site]),
    TRef.

run(_Site, _Params, []) -> ok;
run(Site, Params, [H | T]) ->
    case run2(Params, H) of
        true  -> {timer, Idx, _} = H,
                 new_db_api:mark_idx_dirty(Site, Idx);
        false -> ok
    end,
    run(Site, Params, T).

% ?ZERO is a macro to make it easy to test without having to wait
% until midnight!
run2({_LeapY, _M, _DoM, _Day, _Hour}, {timer, _, hourly}) ->
    true;
run2({_LeapY, _M, _DoM, _Day, ?ZERO}, {timer, _, daily}) ->
    true;
run2({_LeapY, _M, _DoM, Day,  ?ZERO}, {timer, _, {weekly, Day}}) ->
    true;
run2({true,    2,   29,  _,   ?ZERO}, {timer, _, {monthly, DoM2}})
  when DoM2 > 29 ->
    true;
run2({false,   2,   28,  _,   ?ZERO}, {timer, _, {monthly, DoM2}})
  when DoM2 > 28 ->
    true;
run2({_,       N,   30,  _,   ?ZERO}, {timer, _, {monthly, 31}})
  when N == 4 orelse N == 6 orelse N == 9 orelse N == 11 ->
    true;
run2({_,      _,   DoM,  _,   ?ZERO}, {timer, _, {monthly, DoM}}) ->
    true;
run2(_Params, _Spec) ->
    false.

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

% hourly tests
testA1([]) ->
    ?assertEqual(true, run2({false, 1, 1, 1, 0}, {timer, 1, hourly})).
testA2([]) ->
    ?assertEqual(true, run2({false, 1, 1, 1, 2}, {timer, 1, hourly})).

% daily tests
testA3([]) ->
    ?assertEqual(true,  run2({false, 1, 1, 1, 0}, {timer, 1, daily})).
testA4([]) ->
    ?assertEqual(false, run2({false, 1, 1, 1, 2}, {timer, 1, daily})).

% weekly tests
testA5([]) ->
    ?assertEqual(true,  run2({false, 1, 1, 1, 0}, {timer, 1, {weekly, 1}})).
testA6([]) ->
    ?assertEqual(false, run2({false, 1, 1, 1, 2}, {timer, 1, {weekly, 1}})).
testA7([]) ->
    ?assertEqual(false, run2({false, 1, 1, 2, 0}, {timer, 1, {weekly, 1}})).
testA8([]) ->
    ?assertEqual(false, run2({false, 1, 1, 2, 2}, {timer, 1, {weekly, 1}})).

% monthly tests
testA9([]) ->
    ?assertEqual(true,  run2({false, 1, 31, 1, 0}, {timer, 1, {monthly, 31}})).
testA10([]) ->
    ?assertEqual(false, run2({false, 1, 31, 1, 2}, {timer, 1, {monthly, 31}})).
testA11([]) ->
    ?assertEqual(false, run2({false, 1, 30, 2, 0}, {timer, 1, {monthly, 31}})).
testA12([]) ->
    ?assertEqual(false, run2({false, 1, 30, 2, 2}, {timer, 1, {monthly, 31}})).
% 30 days hath September, April, June and November
testA13([]) ->
    ?assertEqual(true, run2({false,  4, 30, 1, 0}, {timer, 1, {monthly, 31}})).
testA14([]) ->
    ?assertEqual(true, run2({false,  6, 30, 1, 0}, {timer, 1, {monthly, 31}})).
testA15([]) ->
    ?assertEqual(true, run2({false,  9, 30, 2, 0}, {timer, 1, {monthly, 31}})).
testA16([]) ->
    ?assertEqual(true, run2({false, 11, 30, 2, 0}, {timer, 1, {monthly, 31}})).
% excepting February alone which has 28
testA17([]) ->
    ?assertEqual(true, run2({false, 2, 28, 1, 0}, {timer, 1, {monthly, 31}})).
testA18([]) ->
    ?assertEqual(true, run2({false, 2, 28, 1, 0}, {timer, 1, {monthly, 30}})).
testA19([]) ->
    ?assertEqual(true, run2({false, 2, 28, 2, 0}, {timer, 1, {monthly, 29}})).
% or 29 in a leap year
testA20([]) ->
    ?assertEqual(true,  run2({true, 2, 29, 1, 0}, {timer, 1, {monthly, 31}})).
testA21([]) ->
    ?assertEqual(true,  run2({true, 2, 29, 1, 0}, {timer, 1, {monthly, 30}})).
testA22([]) ->
    ?assertEqual(true,  run2({true, 2, 29, 1, 0}, {timer, 1, {monthly, 29}})).
testA23([]) ->
    ?assertEqual(true,  run2({false, 2, 28, 1, 0}, {timer, 1, {monthly, 31}})).
testA24([]) ->
    ?assertEqual(true,  run2({false, 2, 28, 1, 0}, {timer, 1, {monthly, 30}})).
testA25([]) ->
    ?assertEqual(true,  run2({false, 2, 28, 1, 0}, {timer, 1, {monthly, 29}})).
testA26([]) ->
    ?assertEqual(true,  run2({false, 2, 28, 1, 0}, {timer, 1, {monthly, 28}})).


unit_test_() ->

    Setup = fun() -> ok end,

    SeriesA = [
               fun testA1/1,
               fun testA2/1,
               fun testA3/1,
               fun testA4/1,
               fun testA5/1,
               fun testA6/1,
               fun testA7/1,
               fun testA8/1,
               fun testA9/1,
               fun testA10/1,
               fun testA11/1,
               fun testA12/1,
               fun testA13/1,
               fun testA14/1,
               fun testA15/1,
               fun testA16/1,
               fun testA17/1,
               fun testA18/1,
               fun testA19/1,
               fun testA20/1,
               fun testA21/1,
               fun testA22/1,
               fun testA23/1,
               fun testA24/1,
               fun testA25/1,
               fun testA26/1
              ],

     {setup, Setup, [{with, [], SeriesA}]}.
