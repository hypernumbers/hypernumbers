%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(socket_test_SUITE).

-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

-define(str(Str,List),lists:flatten(io_lib:format(Str,List))).
-define(POST,"<create><formula>~s</formula></create>").
-define(URL,"~s/~s?attr").
-define(OPTS,[list, {reuseaddr, true},{packet, 0}]).

%% Test server callback functions
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) -> hn_loaddb:clear_db(),Config.
end_per_testcase(_TestCase, _Config) -> ok.

all() ->
    [test_socket1,test_socket2,test_socket3].

%% Test cases starts here.
%%------------------------------------------------------------------------------
test_socket1(_Config) ->
    Sock = socket_connect("127.0.0.1",?PORTNO),
    hn_util:post(?str(?URL,[?HN_URL1,"a1"]),?str(?POST,["99"]),"text/xml"),
    Msg = "change <ref type=\"cell\" ref=\"a1\"><formula>99</formula>"
        ++"</ref>\nchange <ref type=\"cell\" ref=\"a1\"><value><integer>"
        ++"99</integer></value></ref>\n",
    socket_receive(Msg),
    gen_tcp:close(Sock).
    
test_socket2(_Config) ->
    Sock1 = socket_connect("127.0.0.1",?PORTNO),
    Sock2 = socket_connect("127.0.0.1",?PORTNO),
    hn_util:post(?str(?URL,[?HN_URL1,"a1"]),?str(?POST,["99"]),"text/xml"),
    
    Msg = "change <ref type=\"cell\" ref=\"a1\"><formula>99</formula>"
        ++"</ref>\nchange <ref type=\"cell\" ref=\"a1\"><value><integer>"
        ++"99</integer></value></ref>\n",
    
    socket_receive(Msg),
    socket_receive(Msg),
        
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock1).
        
test_socket3(_Config) ->
    Sock1 = socket_connect("127.0.0.1",?PORTNO),
    Sock2 = socket_connect("127.0.0.1",?PORTNO),
    Sock3 = socket_connect("127.0.0.1",?PORTNO),
    
    hn_util:post(?str(?URL,[?HN_URL1,"a1"]),?str(?POST,["99"]),"text/xml"),
    
    Msg = "change <ref type=\"cell\" ref=\"a1\"><formula>99</formula>"
        ++"</ref>\nchange <ref type=\"cell\" ref=\"a1\"><value><integer>"
        ++"99</integer></value></ref>\n",
    
    socket_receive(Msg),
    socket_receive(Msg),
    socket_receive(Msg),
    
    gen_tcp:close(Sock3),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock1).    

socket_receive(Msg) ->
    receive 
        {tcp,_Port,Msg} -> ok 
        after 5000      -> exit("not updated") 
    end,
    ok.
    
socket_connect(Domain,Port) ->
    {ok, Sock} = gen_tcp:connect(Domain,Port,?OPTS),
    ok = gen_tcp:send(Sock, "register "++?HN_URL1),
    receive 
        {tcp,Sock,"range registered\n"} -> ok
    end,
    Sock.   

