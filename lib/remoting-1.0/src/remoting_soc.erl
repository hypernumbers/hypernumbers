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
    ?F("tcp open ~n"),
    remoting_soc:accept(Listen).

loop(Socket)->
    receive

    %% Format the register message, send to remoting_reg, then
    %% send the return back to self to process
    {tcp, Socket, [?REGISTER|Rest]} ->
        ?F("got register message~n"),
        Page = hn_util:parse_url(string:strip(Rest)),
        self() ! gen_server:call(remoting_reg,{register,Page}),
        loop(Socket);

    {tcp, Socket, [?UNREGISTER]} ->
        ?F("got unregister message~n"),
        self() ! gen_server:call(remoting_reg,{unregister}),
        loop(Socket);

    {tcp, Socket, [$<,$p,$o,$l,$i,$c,$y|_Rest]} ->
        ?F("got policy request message~n"),
        {ok,Msg} = hn_util:read("../include/docroot/crossdomain.xml"),
        self() ! {msg,Msg++"\0"},
        loop(Socket);

    {tcp, Socket, _Msg} ->
        ?F("got some unhandlable message~n"),
        loop(Socket);

    %% Received a Message to send back to client
    {msg,Msg} ->
        ?F("got message to send back - Msg~n",[Msg]),
        gen_tcp:send(Socket, Msg++"\n"),
        loop(Socket);

    {tcp_closed, _Port} ->
        ?F("tcp closed ~n"),
        gen_server:call(remoting_reg,{unregister}),
        exit(normal)
    
    end.
    