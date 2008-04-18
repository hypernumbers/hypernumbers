%%%-------------------------------------------------------------------
%%% File    : remoting_soc.erl
%%% Author  : Dale Harvey
%%% Description : 
%%%
%%% Created :  6 Dec 2007
%%%-------------------------------------------------------------------
-module(remoting_soc).

-include("spriki.hrl").

-export([accept/1]).

accept(Listen) ->

    {ok, Soc} = gen_tcp:accept(Listen),

    %% TODO : Fix possible? race condition, receives message
    %% before controlling process activates
    gen_tcp:controlling_process(Soc,
        spawn(fun() -> loop(Soc) end)),
    
    %% Start Listening for another connection
    remoting_soc:accept(Listen).

loop(Socket)->
    receive

    {tcp, Socket,"register"++Rest} ->   
        Page = hn_util:parse_url(hn_util:trim(Rest)),  
        self() ! gen_server:call(remoting_reg,{register,Page}),
        loop(Socket);

    {tcp, Socket,"unregister"++_} ->
        self() ! gen_server:call(remoting_reg,{unregister}),
        loop(Socket);

    {tcp, Socket,"<policy-file-request/>"++_} ->
        Root = production_boot:root(),
        {ok,Msg} = hn_util:read(Root++"include/docroot/crossdomain.xml"),
        self() ! {msg,Msg++"\0"},
        loop(Socket);

    {tcp, Socket, _Msg} ->     
        self() ! {msg,"invalid message"},
        loop(Socket);

    {msg,Msg} ->
        gen_tcp:send(Socket, Msg++"\n"),
        loop(Socket);

    {tcp_closed, _Port} ->
        gen_server:call(remoting_reg,{unregister}),
        exit(normal)

    end.
    
