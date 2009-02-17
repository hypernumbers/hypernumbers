%%%-------------------------------------------------------------------
%%% File        remoting_soc.erl
%%% @author     Dale Harvey
%%% @doc
%%% @copyright  Hypernumbers Ltd
%%% @private
%%%
%%% Created :  6 Dec 2007
%%%-------------------------------------------------------------------
-module(remoting_soc).

-include("spriki.hrl").
-include("hypernumbers.hrl").

-export([accept/1]).

accept(Listen) ->
    
    {ok, Soc} = gen_tcp:accept(Listen),
    Pid = spawn(fun() -> loop(Soc) end),
    gen_tcp:controlling_process(Soc,Pid),
    %% Start Listening for another connection
    remoting_soc:accept(Listen).

loop(Socket)->

    receive
        
        {tcp, Socket,"register"++Rest} ->
            {ok,Page} = hn_util:parse_url(hn_util:trim(Rest)),  
            self() ! gen_server:call(remoting_reg,{register,Page}),
            loop(Socket);
        
        {tcp, Socket,"unregister"++_} ->
            self() ! gen_server:call(remoting_reg,{unregister}),
            loop(Socket);
        
        {tcp, Socket,"<policy-file-request/>"++_} ->
            Path = code:priv_dir("hypernumbers")++"/crossdomain.xml",
            {ok,Bin} = file:read_file(Path),
            self() ! {msg,binary_to_list(Bin)++"\0"},
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

