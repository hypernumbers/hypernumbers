%%% copyright 2010 Hypernumbers Ltd
%%% written by Gordon Guthrie gordon@hypernumbers.com
%%% 15th August 2010

-module(emailer).

-define(FROM, "\"Gordon Guthrie\" <gordon@hypernumbers.com>").
-define(SIG, "Cheers\n\nGordon Guthrie\nCEO hypernumbers.com\n+44 7776 251669\n\n").

-export([send/4]).

send(reset, Email, Hash, Site) ->
    Subject = "Password Reset Request",
    Name = hn_util:extract_name_from_email(Email),
    {ok, URL} =  application:get_env(hypernumbers, reset_url),
    EmailBody = "Dear " ++ Name ++"\n\n"
        ++ "Somebody has requested an password reset for this account\n"
        ++ "If it wasnt you please ignore this email.\n\n"
        ++ "To reset your password please go to this URL:\n"
        ++ URL ++ "?reset=" ++ Hash
        ++ "\n\n"
        ++ "After resetting your password you will get a link back to "
        ++ Site ++ "\n\n"
        ++ ?SIG,
    ok = send_email(Email, ?FROM, Subject, EmailBody).

send_email(To, From, Subject, EmailBody) ->
    case application:get_env(hypernumbers, environment) of
        {ok, development} ->
            io:format("Spoofing Emails~n~nTo: ~p~nFrom ~p~n"
                      ++"Subject: ~p~n~s~nEND EMAIL--~n",
                      [To, From, Subject, EmailBody]);
        {ok, production}  ->
            spawn(hn_net_util, email, 
                  [To, From, Subject, EmailBody]),
            ok
    end.    
