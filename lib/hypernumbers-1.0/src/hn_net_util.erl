%%% @copyright (C) 2010, Hypernumber, Ltd.

-module(hn_net_util).

-export([cookie/3,
         email/4, email/5,
         post/3]).

-include("date.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cookies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -spec expire_days_from_now(integer()) -> integer().
%% expire_days_from_now(Days) ->
%%     NowS = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
%%     ExpS = NowS + Days * 86400.

-spec cookie(string(), string(), integer() | session) -> string(). 
cookie(Name, Value, Age) ->
    Opts = [{path, "/"}] ++ if Age == session -> [];
                               true -> [{max_age, Age}]
                            end,
    mochiweb_cookies:cookie(Name, Value, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Email 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

email(To, From, Subject, Msg) ->
    {ok, Server}   = application:get_env(hypernumbers, mailserver),
    {ok, Password} = application:get_env(hypernumbers, mailpassword),
    {ok, User}     = application:get_env(hypernumbers, mailuser),
    email([{server, Server}, {user, User}, {password, Password}],
          To, From, Subject, Msg).
        
email(Details, To, From, Subject, Msg) ->

    Server = proplists:get_value(server, Details),    
    User   = proplists:get_value(user, Details),
    Pass   = proplists:get_value(password, Details),
    
    {ok, Socket} = ssl:connect(Server, 465, [{active, false}], 1000),
    
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode(User))),
    send(Socket, binary_to_list(base64:encode(Pass))),
    send(Socket, "MAIL FROM:<"++parse_email_address(From)++">"),
    send(Socket, "RCPT TO:<"++parse_email_address(To)++">"),
    send(Socket, "DATA"),
    send_no_receive(Socket, "From: "++From),
    send_no_receive(Socket, "To: "++To),
    send_no_receive(Socket, "Date: "++dh_date:format("r")),
    send_no_receive(Socket, "Subject: "++Subject),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, Msg),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

parse_email_address(Address) ->
    lists:last(string:tokens(Address, "<>")).

send_no_receive(Socket, Data) ->
    io:format("SEND: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n").

send(Socket, Data) ->
    io:format("SEND: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) ->
    case ssl:recv(Socket, 0, 100000) of
        {ok, Return}    -> io:format("RECV: ~p~n", [Return]);
        {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTTP 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post(Url, Data, Format) ->
    {ok, {{_V, _Status,_R},_H,Body}} =
        httpc:request(post,{Url,[],Format,Data},[],[]),
    Body.
