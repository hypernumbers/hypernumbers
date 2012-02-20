%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Some utilities for doing phone
%%%            and email stuff
%%% @end
%%% Created : 13 Feb 2012 by gordon@hypernumbers.com

-module(contact_utils).

-export([
         get_recording_details/2,
         robocall/3,
         post_sms/3,
         get_twilio_account/1
         ]).

-include("keyvalues.hrl").
-include("errvals.hrl").
-include("spriki.hrl").

robocall(AC, Number, Msg) when is_integer(Number) ->
    robocall(AC, integer_to_list(Number), Msg);
robocall(AC, Number, Msg) ->
    Path = "/Accounts/" ++ AC#twilio_account.account_sid
        ++ "/SMS/Messages.Xml",
    Params = [{"From", AC#twilio_account.site_phone_no},
              {"To", Number},
              {"Body", Msg}],
    twilio:request(AC#twilio_account.account_sid,
                   AC#twilio_account.auth_token,
                   post, Path, Params).

get_recording_details(AC, Call_Sid) ->
    Path = "/Accounts/" ++ AC#twilio_account.account_sid
        ++ "/Calls/" ++ Call_Sid ++ "/Recordings.json",
    twilio:request(AC#twilio_account.account_sid,
                  AC#twilio_account.auth_token,
                  get, Path, []).

post_sms(AC, "+" ++ Number, Msg) ->
    post_sms(AC, Number, Msg);
post_sms(AC, Number, Msg) when is_integer(Number) ->
    post_sms(AC, integer_to_list(Number), Msg);
post_sms(AC, Number, Msg) ->
    Path = "/Accounts/" ++ AC#twilio_account.account_sid
        ++ "/SMS/Messages.Xml",
    Params = [{"From", AC#twilio_account.site_phone_no},
              {"To", Number},
              {"Body", Msg}],
    twilio:request(AC#twilio_account.account_sid,
                   AC#twilio_account.auth_token,
                   post, Path, Params).

get_twilio_account(Site) ->
    case new_db_api:read_kv(Site, ?twilio) of
        []                        -> ?ERRVAL_PAYONLY;
        [{kvstore, ?twilio, Rec}] -> Rec
    end.
