%%% @copyright (C) 2010, Hypernumbers, Ltd.

-module(sendgrid).

-export([
         email/5,
         email/6
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Email
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

email(To, CC, From, Subject, Msg) ->
    {ok, Server}   = application:get_env(hypernumbers, sendgridserver),
    {ok, Password} = application:get_env(hypernumbers, sendgridpassword),
    {ok, User}     = application:get_env(hypernumbers, sendgriduser),
    email([{server, Server}, {user, User}, {password, Password}],
          To, CC, From, Subject, Msg).

email(Details, To, CC, From, Subject, Msg) ->

    Tos = string:tokens(To, ";"),

    Server = proplists:get_value(server, Details),
    User   = proplists:get_value(user, Details),
    Pass   = proplists:get_value(password, Details),

    {ok, Socket} = ssl:connect(Server, 465, [{active, false}], 20000),

    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode(User))),
    send(Socket, binary_to_list(base64:encode(Pass))),
    send(Socket, "MAIL FROM:<"++parse_email_address(From)++">"),
    %% now an RCPT for each recipient
    [send(Socket, "RCPT TO:<"++parse_email_address(X)++">") || X <- Tos],
    send(Socket, "DATA"),
    send_no_receive(Socket, "From: "++From),
    send_no_receive(Socket, "To: "++To),
    if (CC =/= "") -> send_no_receive(Socket, "Cc: "++CC);
       (CC == "")  -> ok
    end,
    %% debugging for emails
    send_no_receive(Socket, "Bcc: debug@hypernumbers.com"),
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
