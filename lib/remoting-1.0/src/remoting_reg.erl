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

%% Register a new Range, From will now recieve updates
%% when a number in Page -> Range changes
handle_call({register,#page{site=Site,path=Path,ref=Ref}},{Pid,_},State) ->
    {reply,{msg,"range registered"},lists:append(State,[{Site,Path,Ref,Pid}])};

%% Unregisters a range, From will no longer
%% Recieve updates
handle_call({unregister},From,State) ->
    N = lists:filter(
        fun(X) ->
            case X of 
            {_,_,_,From} -> true;
            _ -> false
            end 
        end,State),
    {reply,{msg,"range unregistered"}, N};

%% Change Message, find everyone listening to 
%% that range then send them a change message
handle_call({change,#page{site=Site,path=Path,
        ref={cell,{X,Y}}},Value},_From,State) ->
    lists:foreach(
        fun(Z) ->
            case Z of
            %% Matches site path and check the cell
            %% is within range
            {Site,Path,{range,{X1,Y1,X2,Y2}},Pid} when 
                    X >= X1,Y >= Y1, X =< X2,Y =< Y2 -> 
                Msg = lists:append(["change ",Site,Path,util2:make_b26(X),
                    hn_util:text(Y)," ",hn_util:text(Value)]),
                Pid ! {msg,Msg};
            _ -> ok
            end
        end,State),
    {reply,ok,State};

%% Invalid Message
handle_call(_Request,_From,State) ->
    {reply,invalid_message, State}.

handle_cast(_Msg, State) ->     {noreply, State}.
handle_info(_Info, State) ->    {noreply, State}.
terminate(_Reason, _State) ->   ok.
code_change(_Old, State, _E) -> {ok, State}.