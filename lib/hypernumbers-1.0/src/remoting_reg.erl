%%%-----------------------------------------------------------------------------
%%% File        remoting_reg.erl
%%% @author     Gordon Guthrie 
%%% @doc        remoting_reg handles registration from Flex remoting
%%% @copyright  Hypernumbers Ltd
%%%
%%% Created     : 6 Feb 2007 by gordonguthrie@backawinner.gg
%%% @private
%%%-----------------------------------------------------------------------------
-module(remoting_reg).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("spriki.hrl").
-include("hypernumbers.hrl").

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %gen_server:cast(remoting_reg, start_timer),
    {ok, {[], []}}.

handle_call({msg, _Site, Path, Msg}, _From, State) ->
    {reply,ok,State};

%% Invalid Message
handle_call(_Request,_From,State) ->
    ?INFO("Invalid Call in remoting_reg ~p ~p",[_Request,_From]),
    {reply,invalid_message, State}.

handle_cast({fetch, Site, Path, Time, Pid}, {Updates, Waiting}) ->
    ?INFO("~p",[{fetch, Site, Path, Time}]), 
    {noreply, {Updates, [{Site, Path, Time, Pid} | Waiting]}};

handle_cast(start_timer, State) ->
    ?INFO("Starting timer",[]),
    flush(5000),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?INFO("Invalid Cast in remoting_reg ~p ",[_Msg]),
    {noreply, State}.
    
handle_info(_Info, State) ->    {noreply, State}.
terminate(_Reason, _State) ->   ?INFO("terminating",[]),ok.
code_change(_Old, State, _E) -> {ok, State}.

%% Timer to flush the updates queue and timeout old clients
flush(Time) -> 
    receive 
        terminate  -> 
            ok 
    after  
        Time -> 
            gen_server:call(remoting_reg, flush), 
            flush(Time) 
    end.


