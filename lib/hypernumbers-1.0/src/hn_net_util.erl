%%% @copyright (C) 2010-2014, Hypernumbers, Ltd.

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(hn_net_util).

-export([cookie/3,
         kill_cookie/1,
         email/5,
         email/6,
         post/3]).

%% Two years is forever on the internet.
-define(TWO_YEARS, 63113852).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cookies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cookie(string(), string(), integer() | string()) -> string().
cookie(N, V, "session")                -> c_(N, V, []);
cookie(N, V, "never")                  -> c_(N, V, [{max_age, ?TWO_YEARS}]);
cookie(N, V, Age) when is_integer(Age) -> c_(N, V, [{max_age, Age}]).

c_(Name, Value, Opts) ->
    mochiweb_cookies:cookie(Name, Value, [{path, "/"}] ++ Opts).

-spec kill_cookie(string()) -> string().
kill_cookie(N) -> cookie(N, "killitwithfire", 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Email
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

email(To, CC, From, Subject, Msg) ->
    {ok, Server}   = application:get_env(hypernumbers, mailserver),
    {ok, Password} = application:get_env(hypernumbers, mailpassword),
    {ok, User}     = application:get_env(hypernumbers, mailuser),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post(Url, Data, Format) ->
    {ok, {{_V, _Status,_R},_H,Body}} =
        httpc:request(post,{Url,[],Format,Data},[],[]),
    Body.
