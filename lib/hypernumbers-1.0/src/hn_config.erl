%%% @author    Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc       Stores user configurations for rest of the 
%%%            application
%%% @private
-module(hn_config).

-behaviour(gen_server).
-record(state, {conf, path}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-export([ get/1, read_conf/1, hup/0 ]).

-spec(start_link/0 :: () -> {ok,pid()} | ignore | {error,any()}).
             
%% @doc Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%-spec( init/1).
%% @doc Initiates the server
init([]) ->
    {ok, #state{}}.

% -spec handle_call/3 -> {reply, any(), any()} | {noreply, any()}).
%% @doc Handling call messages
%% Set the configuration on startup
handle_call({set_conf, Path, Conf}, _From, State) ->
    {reply, ok, State#state{conf=Conf, path=Path}};
handle_call(hup, _From, State) ->
    {ok,Config} = file:consult(State#state.path), 
    {reply, ok, State#state{conf=Config}};
%% Get config by key
handle_call({get, Key}, _From, State) ->
    {value, {Key, Values}} = lists:keysearch(Key, 1, State#state.conf),
    {reply, Values, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

read_conf(Path) ->
    {ok,Config} = file:consult(Path), 
    gen_server:call(?MODULE, {set_conf, Path, Config}).

hup() ->
    gen_server:call(?MODULE, hup).

handle_cast(_Msg, State)      -> {noreply, State}.
handle_info(_Info, State)     -> {noreply, State}.
terminate(_Reason, _State)    -> ok.
code_change(_Old, State, _Ex) -> {ok, State}.
