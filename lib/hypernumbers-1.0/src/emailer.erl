%%% copyright 2010 Hypernumbers Ltd
%%% written by Gordon Guthrie gordon@hypernumbers.com
%%% 15th August 2010

-module(emailer).

-define(FROM, "\"Gordon Guthrie\" <gordon@hypernumbers.com>").
-define(SIG, "Cheers\n\nGordon Guthrie\nCEO hypernumbers.com\n"
        ++"+44 7776 251669\n@hypernumbers\n").

-export([send/5]).

send(Type, To, CC, Site, Args) ->
    Name = hn_util:extract_name_from_email(To),
    {ok, URL} =  application:get_env(hypernumbers, reset_url),
    send1(Type, To, CC, Name, Site, URL, Args).

send1(invalid_invite, To, CC, Name, Site, _URL, Args) ->
    io:format("Args is ~p~n", [Args]),
    Person  = kfind(person,  Args),
    Details = kfind(details, Args),
    Reason  = kfind(reason,  Args),
    Subject = "Invalid Invite for "++Person++" on "++Site,
    EmailBody = "Dear "++Name++"\n\n"
        ++"You tried to invite " ++Person++" to "++Details
        ++" on "++Site++" but the invitation failed because"
        ++Reason++".\n\nPlease return to "++Site++" and try again.\n\n"
        ++?SIG,
    ok = send_email(To, CC, ?FROM, Subject, EmailBody);

send1(invite_new, To, CC, Name, _Site, _URL, Args) ->
    io:format("Args is ~p~n", [Args]),
    Invitee  = kfind(invitee, Args),
    Msg      = kfind(msg, Args),
    Hypertag = kfind(hypertag, Args),
    InvName  = hn_util:extract_name_from_email(Invitee),
    Subject  = "Message from "++Invitee,
    EmailBody = "Dear "++Name++"\n\n"
        ++InvName++" has invited you to "
        ++"go to the following webpage:\n"++Hypertag++"\n\n"
        ++"The link will validate your user account but you"
        ++"will have to set a password as well.\n\n"
        ++InvName++" has also sent you the following message:\n"
        ++"----------------------------------------\n\n"
        ++Msg++"\n\n"
        ++"----------------------------------------\n\n"
        ++?SIG,
    ok = send_email(To, CC, ?FROM, Subject, EmailBody);

send1(invite_existing, Email, CC, Name, Site, _URL, Args) ->
    Invitee = kfind(invitee, Args),
    Path    = kfind(path, Args),
    Msg     = kfind(msg, Args),
    InvName = hn_util:extract_name_from_email(Invitee),
    Subject = "Message from "++Invitee,
    EmailBody = "Dear "++Name++"\n\n"
        ++InvName++" has invited you to "
        ++"go to the following webpage:\n"++Site++Path++"\n\n"
        ++InvName++" has also sent you the following message:\n"
        ++"----------------------------------------\n\n"
        ++Msg++"\n\n"
        ++"----------------------------------------\n\n"
        ++"You can use your existing hypernumbers account to login.\n\n"
        ++?SIG,
    ok = send_email(Email, CC, ?FROM, Subject, EmailBody);

send1(reset, To, CC, Name, Site, URL, Hash) ->
    Subject = "Password Reset Request",
    EmailBody = "Dear " ++ Name ++"\n\n"
        ++ "Somebody has requested an password reset for this account\n"
        ++ "If it wasn't you please ignore this email.\n\n"
        ++ "To reset your password please go to this URL:\n"
        ++ URL ++ "?reset=" ++ Hash
        ++ "\n\n"
        ++ "After resetting your password you will get a link back to "
        ++ Site ++ "\n\n"
        ++ ?SIG,
    ok = send_email(To, CC, ?FROM, Subject, EmailBody).

send_email(To, CC, From, Subject, EmailBody) ->
    case application:get_env(hypernumbers, environment) of
        {ok, development} ->
            io:format("Spoofing Emails~n~nTo: ~p~nCC: ~p~nFrom ~p~n"
                      ++"Subject: ~p~n~s~nEND EMAIL--~n",
                      [To, CC, From, Subject, EmailBody]);
        {ok, production}  ->
            spawn(hn_net_util, email, 
                  [To, CC, From, Subject, EmailBody]),
            ok
    end.    

-spec kfind(string(), list()) -> any().
kfind(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.
