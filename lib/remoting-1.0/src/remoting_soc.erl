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

    %% TODO : Fix possible? race condition, recieves message
    %% before controlling process activates
    gen_tcp:controlling_process(Soc,
        spawn(fun() -> loop(Soc) end)),
    
    %% Start Listening for another connection
    remoting_soc:accept(Listen).

loop(Socket)->
    receive

    %% Format the register message, send to remoting_reg, then
    %% send the return back to self to process
    {tcp, Socket, [?REGISTER|Rest]} ->
        Page = hn_util:parse_url(string:strip(Rest)),
        self() ! gen_server:call(remoting_reg,{register,Page}),
        loop(Socket);

    {tcp, Socket, [?UNREGISTER]} ->
        self() ! gen_server:call(remoting_reg,{unregister}),
        loop(Socket);

    {tcp, Socket, [$<,$p,$o,$l,$i,$c,$y|Rest]} ->
        {ok,Msg} = hn_util:read("../include/docroot/crossdomain.xml"),
        self() ! {msg,Msg},
        loop(Socket);

    {tcp, Socket, Msg} ->
        ?F("Got Message ~s ~n",[Msg]),
        loop(Socket);

    %% Recieved a Message to send back to client
    {msg,Msg} ->
        gen_tcp:send(Socket, Msg++"\n"),
        loop(Socket);

    {tcp_closed, _Port} ->
        gen_server:call(remoting_reg,{unregister}),
        exit(normal)
    
    end.
    