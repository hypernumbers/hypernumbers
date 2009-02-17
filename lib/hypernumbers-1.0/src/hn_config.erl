%%% @author    Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc       Stores user configurations for rest of the 
%%%            application
%%% @private
-module(hn_config).

-behaviour(gen_server).
-record(state, {conf}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-spec(start_link/0 :: () -> {ok,pid()} | ignore | {error,any()}).
             
%% @doc Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(init/1 :: (Args::any()) -> {ok,any()} | {ok,any(),any()} | ignore | {stop,any()}).
%% @doc Initiates the server
init([]) ->
    {ok, #state{}}.

-spec(handle_call/3 :: (Request::any(), From::any(), State::any()) -> {reply, any(), any()} | {noreply, any()}).
%% @doc Handling call messages
%% Set the configuration on startup
handle_call({set_conf,Conf}, _From, State) ->
    {reply, ok, State#state{conf = Conf}};
%% Get config by key
handle_call({get,Key}, _From, State) ->
    {value,{Key,Values}} = lists:keysearch(Key,1,State#state.conf),
    {reply, Values, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
