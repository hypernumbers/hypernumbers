%%% copyright 2010 Hypernumbers Ltd
%%% written by Gordon Guthrie gordon@hypernumbers.com
%%% 15th August 2010

-module(emailer).

-include("spriki.hrl").
-include("defaults.hrl").

-export([
         get_details/1,
         get_default_details/0,
         send/6,
         send_email/5
        ]).

get_default_details() -> {?MASTER_EMAIL, ?MASTER_SIG}.

get_details(Site) ->
    V = new_db_api:read_kv(Site, site_email),
    DefFrom = ?MASTER_EMAIL,
    DefSig = ?MASTER_SIG,
    case V of
        [] ->
            {DefFrom, DefSig};
        [{kvstore, site_email, {_, Rec}}] ->
            case Rec#site_email.email_validated of
                true  -> {Rec#site_email.email, Rec#site_email.signature};
                false -> {DefFrom, DefSig}
            end
    end.

send(Type, To, CC, From, Site, Args) ->
    Name = hn_util:extract_name_from_email(To),
    {ok, URL} =  application:get_env(hypernumbers, reset_url),
    send1(Type, To, CC, From, Name, Site, URL, Args).

send1(invalid_invite, To, CC, From, Name, Site, _URL, Args) ->
    Person  = kfind(person,  Args),
    Details = kfind(details, Args),
    Reason  = kfind(reason,  Args),
    Sig     = kfind(sig,  Args),
    Subject = "Invalid Invite for " ++ Person ++ " on " ++ Site,
    EmailBody = "Dear " ++ Name ++ "\n\n"
        ++ "You tried to invite " ++ Person ++ " to " ++ Details
        ++ " on " ++ Site ++ " but the invitation failed because"
        ++ Reason ++ ".\n\nPlease return to " ++ Site ++ " and try again.\n\n"
        ++ Sig,
    ok = send_email(To, CC, From, Subject, EmailBody);

send1(invite_new, To, CC, From, Name, _Site, _URL, Args) ->
    Invitee  = kfind(invitee, Args),
    Msg      = kfind(msg, Args),
    Hypertag = kfind(hypertag, Args),
    Sig      = kfind(sig, Args),
    InvName  = hn_util:extract_name_from_email(Invitee),
    Subject  = "Message from "++Invitee,
    EmailBody = "Dear "++Name++"\n\n"
        ++ InvName ++ " has invited you to "
        ++ "go to the following webpage:\n" ++ Hypertag ++ "\n\n"
        ++ "The link will validate your user account but you "
        ++ "will have to set a password as well.\n\n"
        ++ InvName ++ " has also sent you the following message:\n"
        ++"----------------------------------------\n\n"
        ++ Msg ++ "\n\n"
        ++"----------------------------------------\n\n"
        ++ Sig,
    ok = send_email(To, CC, From, Subject, EmailBody);

send1(new_site_existing, Email, CC, From, Name, Site, _URL, Args) ->
    Subject = "Your new site is ready",
    Sig     = kfind(sig, Args),
    EmailBody = lists:flatten(
                  ["Hi ", Name, ",\n\n",
                   "Cool, we're glad you want another site! "
                   "We've built it for you, and it's located here:\n\n ",
                   hn_util:strip80(Site), "\n\n",
                   "Just use your existing account to login.\n\n",
                   Sig
                  ]),
    ok = send_email(Email, CC, From, Subject, EmailBody);

send1(new_site_validate, Email, CC, From, Name, Site, _URL, Args) ->
    Hypertag = kfind(hypertag, Args),
    Sig      = kfind(sig, Args),
    SiteURL = hn_util:strip80(Site),
    Subject = "Your new site is ready",
    EmailBody = lists:flatten(
      ["Hi ", Name, ",\n\n",
       "We hope you're having a fun time "
       "building your new site:\n\n ",
       SiteURL, "\n\n"
       "\n\nThis is your e-mail verification URL. "
       "Click or paste the following into your browser:\n\n",
       Hypertag,"\n\n",
       "Once you have verified your e-mail you should ",
       "set your password ", SiteURL ++ "#settings\n\n"
       "If you have any problems just tell us "
       "and we will do our best to help.",
       Sig
      ]),
    ok = send_email(Email, CC, From, Subject, EmailBody);

send1(invite_existing, Email, CC, From, Name, Site, _URL, Args) ->
    Invitee = kfind(invitee, Args),
    Path    = kfind(path, Args),
    Msg     = kfind(msg, Args),
    Sig     = kfind(sig, Args),
    InvName = hn_util:extract_name_from_email(Invitee),
    Subject = "Message from " ++ Invitee,
    EmailBody = "Dear " ++ Name ++ "\n\n"
        ++ InvName ++ " has invited you to "
        ++ "go to the following webpage:\n"++ Site ++ Path ++ "\n\n"
        ++ InvName ++ " has also sent you the following message:\n"
        ++ "----------------------------------------\n\n"
        ++ Msg ++ "\n\n"
        ++ "----------------------------------------\n\n"
        ++ "You can use your existing vixo account to login.\n\n"
        ++ Sig,
    ok = send_email(Email, CC, From, Subject, EmailBody);

send1(reset, To, CC, From, Name, Site, URL, Args) ->
    Subject = "Password Reset Request",
    Hash    = kfind(hash, Args),
    Sig     = kfind(sig, Args),
    EmailBody = "Dear " ++ Name ++"\n\n"
        ++ "Somebody has requested an password reset for this account\n"
        ++ "If it wasn't you please ignore this email.\n\n"
        ++ "To reset your password please go to this URL:\n"
        ++ URL ++ "?reset=" ++ Hash
        ++ "\n\n"
        ++ "After resetting your password you will get a link back to "
        ++ Site ++ "\n\n"
        ++ Sig,
    ok = send_email(To, CC, From, Subject, EmailBody).

send_email(To, CC, From, Subject, EmailBody) ->
    case application:get_env(hypernumbers, environment) of
        {ok, production}  ->
            spawn(sendgrid, email,
                  [To, CC, From, Subject, EmailBody]),
            ok;
        {ok, _} ->
            io:format("Spoofing Emails~n~nTo: ~p~nCC: ~p~nFrom ~p~n"
                      ++"Subject: ~p~n~s~nEND EMAIL--~n",
                      [To, CC, From, Subject, EmailBody])
    end.

-spec kfind(string() | atom(), list()) -> any().
kfind(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.
