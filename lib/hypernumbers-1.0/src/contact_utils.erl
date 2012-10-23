%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Some utilities for doing phone
%%%            and email stuff
%%% @end
%%% Created : 13 Feb 2012 by gordon@hypernumbers.com

-module(contact_utils).

% normal utilities
-export([
         rightsize/2,
         get_site_phone_no/1,
         get_phone_menu/1,
         check_if_paid/3,
         get_recording_details/2,
         robocall/3,
         post_sms/3,
         get_twilio_account/1
         ]).

% typechecking style utilities
-export([
         get_voice/1,
         get_lang/1
        ]).

-include("keyvalues.hrl").
-include("errvals.hrl").
-include("spriki.hrl").
-include("twilio.hrl").

get_voice(0) -> "woman";
get_voice(1) -> "man";
get_voice(_) -> ?ERR_VAL.

get_lang(0) -> "en-gb";
get_lang(1) -> "en";
get_lang(2) -> "es";
get_lang(3) -> "fr";
get_lang(4) -> "de";
get_lang(_) -> ?ERR_VAL.

get_phone_menu(S) ->
    io:format("S is ~p~n", [S]),
    case new_db_api:read_kv(S, site_phone_menu) of
        [] ->
            [_Proto, "//" ++ Domain, Port] = string:tokens(S, ":"),
            Site = say_site(Domain, Port),
            [#say{text = Site ++ " is not configured for incoming calls",
                  voice = "woman", language = "en-gb"}];
        [{kvstore, site_phone_menu, {Idx, null}}] ->
            [Phone] = new_db_api:get_phone(S, Idx),
            #phone{twiml = TwiML} = Phone,
            TwiML
    end.

say_site(Domain, "80") ->
    say_subbies(Domain);
say_site(Domain, Port) ->
    say_subbies(Domain) ++ " port " ++ Port.

say_subbies(Domain) ->
    Subbies = string:tokens(Domain, "."),
    string:join(Subbies, " dot ").

check_if_paid(Fun, Args, Type) ->
    Site = get(site),
    case get_twilio_account(Site) of
        ?ERRVAL_PAYONLY ->
            ?ERRVAL_PAYONLY;
        AC ->
            case Type of
                Ty when Ty == outbound orelse Ty == none ->
                    Fun(Args, AC);
                inbound ->
                    case AC#twilio_account.type of
                        full ->
                            Fun(Args, AC);
                        outbound ->
                            ?ERRVAL_PAYONLY
                    end
            end
    end.

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

get_site_phone_no(Site) ->
    case get_twilio_account(Site) of
        ?ERRVAL_PAYONLY -> ?ERRVAL_PAYONLY;
        Rec             -> io:format("Rec is ~p~n", [Rec]),
                           Rec#twilio_account.site_phone_no
    end.

rightsize(Msg, Size) ->
    [Msg2] = typechecks:std_strs([Msg]),
    Length = length(Msg2),
    if
        Length > Size  -> {Msg3, _} = lists:split(Size, Msg2),
                          Msg3;
        Length =< Size -> Msg2
    end.
