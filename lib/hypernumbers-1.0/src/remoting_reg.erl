%%%-----------------------------------------------------------------------------
%%% File        : remoting_reg.erl
%%% Author      : Gordon Guthrie 
%%% Description : remoting_reg handles registration from Flex remoting 
%%%
%%% Created     : 6 Feb 2007 by gordonguthrie@backawinner.gg
%%%-----------------------------------------------------------------------------
-module(remoting_reg).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("spriki.hrl").

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

%% Register a new Range, From will now receive updates
%% when a number in Page -> Range changes
handle_call({register,Page},{Pid,_},State) ->
    #ref{site=Site,path=Path} = Page,
    NewState = lists:append(State,[{Site,Path,Pid}]),
    {reply, {msg,"range registered"},NewState};

%% Unregisters a range, From will no longer
%% Receive updates
handle_call({unregister},From,State) ->
    N = lists:filter(
        fun(X) ->
            case X of 
            {_,_,_,From} -> false;
            _ -> true
            end 
        end,State),
    {reply,{msg,"range unregistered"},N};

%% Change Message, find everyone listening to 
%% that page then send them a change message
handle_call({change,Site,Path,Msg},_From,State) ->
    lists:foreach(
        fun(Z) ->
            case Z of
            {Site,Path,Pid} -> Pid ! {msg,Msg};
            _ -> ok
            end
        end,State),
    {reply,ok,State};

%% Invalid Message
handle_call(_Request,_From,State) ->
    {reply,invalid_message, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->    {noreply, State}.
terminate(_Reason, _State) ->    ok.
code_change(_Old, State, _E) -> {ok, State}.
