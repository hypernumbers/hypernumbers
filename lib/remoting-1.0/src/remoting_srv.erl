%%%-------------------------------------------------------------------
%%% File    : remoting_soc.erl
%%% Author  : Dale Harvey
%%% Description : 
%%%
%%% Created :  6 Dec 2007
%%%-------------------------------------------------------------------
-module(remoting_srv).

-behaviour(gen_server).

-include("spriki.hrl").

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2,code_change/3]).

%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, remoting}, ?MODULE,[], []).

init([]) ->

    Opts = [list, {reuseaddr, true},{packet, 0}],
    case gen_tcp:listen(?PORTNO,Opts) of

    {ok, Socket} ->
        spawn_link(fun() -> remoting_soc:accept(Socket) end),
        {ok, []};

    {error, Reason} -> {stop, Reason}
    end.

handle_call(_Req, _F, State) -> {reply, ok, State}.
handle_cast(_Msg, State)     -> {noreply, State}.
handle_info(_Info, State)    -> {noreply, State}.
terminate(_Reason, _State)   -> ok.
code_change(_Old, State, _E) -> {ok, State}.