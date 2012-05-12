%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       functions for handling phones
%%%            These fns are mirroed in phone_twilm.erl
%%% @end
%%% Created : 11 Feb 2012 by gordon@hypernumbers.com

-module(hnfuns_contacts).

-export([
         'display.phone.nos'/1,
         'text.answer.phone'/1,
         'manual.email'/1,
         'auto.email'/1,
         'manual.sms.out'/1,
         'auto.sms.out'/1,
         'auto.robocall'/1,
         'phone.out'/1,
         'phone.in'/1
         ]).

-include("spriki.hrl").
-include("errvals.hrl").
-include("keyvalues.hrl").
-include("twilio.hrl").

'display.phone.nos'([]) ->
    Site = get(site),
        case contact_utils:get_twilio_account(Site) of
            ?ERRVAL_PAYONLY ->
                "No phone numbers have been purchased for this website";
            AC ->
                #twilio_account{site_phone_no = Site_Phone, type = Type} = AC,
                Site_Phone ++ " (" ++ Type ++ ")"
        end.

'text.answer.phone'([Msg]) ->
    'text.answer.phone'([Msg, false]);
'text.answer.phone'([Msg, IsMan]) ->
    'text.answer.phone'([Msg, IsMan, "en-gb"]);
'text.answer.phone'([Msg, IsMan, Language]) ->
    [Msg2, L2] = typechecks:std_strs([Msg, Language]),
    L3 = string:to_lower(L2),
    [IsMan2] = typechecks:std_bools([IsMan]),
    Voice = case IsMan2 of
                true  -> "man";
                false -> "woman"
            end,
    case lists:member(L3, ?SAYLanguages) of
        true  -> ok;
        false -> ?ERR_VAL
    end,
    Msg3 = rightsize(Msg2, ?SAYLength),
    Say = #say{voice = Voice, language = L3, text = Msg3},
    io:format("Say is ~p~n", [Say]),
    "banjo".

'manual.email'([To, Subject, Contents]) ->
    'manual.email'([To, Subject, Contents, [], []]);
'manual.email'([To, Subject, Contents, CC]) ->
    'manual.email'([To, Subject, Contents, CC, []]);
'manual.email'([To, Su, Cn, CC, Reply]) ->
    [To2, Su2, Cn2, CC2, Fr] = check(To, Su, Cn, CC, Reply),
    Log = #contact_log{idx = get(idx),
                       type = "manual email",
                       to = To2,
                       reply_to = Fr,
                       cc = CC2,
                       subject = Su2,
                       contents = Cn2},
    TwiML = "",
    Capability = [{manual_email, To2, Fr, CC2, Su2, Cn2}],
    Type ={"softphone_type", "manual email"},
    Config = [{"button_txt", "Send"},
              {"headline_txt", "Send Email"},
              {"to", To2},
              {"reply_to", Fr},
              {"cc", CC2},
              {"subject", Su2},
              {"contents", Cn2}],
    Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                   softphone_type = Type, softphone_config = Config},
    Headline = "Send Email",
    ButtonTxt = "<small>" ++ To2 ++ "</small>",
    phone(Phone, "Manual Email: ", Headline, ButtonTxt).

'auto.email'([Condition, To, Subject, Contents]) ->
    'auto.email'([Condition, To, Subject, Contents, [], []]);
'auto.email'([Condition, To, Subject, Contents, CC]) ->
    'auto.email'([Condition, To, Subject, Contents, CC, []]);
'auto.email'([Condition, To, Su, Cn, CC, Reply]) ->
    [To2, Su2, Cn2, CC2, Fr] = check(To, Su, Cn, CC, Reply),
    [Cond2] = typechecks:std_bools([Condition]),
    case Cond2 of
        false -> "Email not sent";
        true  -> hn_net_util:email(To2, CC2, Fr, Su2, Cn2),
                 S = get(site),
                 Log = #contact_log{idx = get(idx),
                                    type = "email out",
                                    to = To2,
                                    cc = CC2,
                                    subject = Su2,
                                    contents = Cn2,
                                    reply_to = Fr},
                 spawn(hn_twilio_mochi, log, [S, Log]),
                 "<a href='./_contacts/'>email sent</a>"
    end.

check(To, Su, Cn, CC, Reply) ->
    Domain = get_domain(get(site)),
    Vals = typechecks:std_strs([To, Su, Cn, CC, Reply]),
    [To2, Su2, Cn2, CC2, R2] = Vals,
    Fr = case R2 of
             [] -> "no-reply@" ++ Domain;
             _  -> R2 ++ "@" ++ Domain
         end,
    To3 = hn_util:split_emails(To2),
    CC3 = hn_util:split_emails(CC2),
    Emails = lists:merge([To3, CC3, [Fr]]),
    case is_valid_email(Emails) of
        false -> ?ERR_VAL;
        true  -> [string:join(To3, ";"), Su2, Cn2, string:join(CC3, ";"), Fr]
    end.

'auto.robocall'([Condition, PhoneNo, Msg, Prefix]) ->
    check_if_paid(fun 'auto.robocall2'/2,
                  [Condition, PhoneNo, Msg, normalise(Prefix)], outbound);
'auto.robocall'([Condition, PhoneNo, Msg]) ->
    check_if_paid(fun 'auto.robocall2'/2, [Condition, PhoneNo, Msg, ""],
                  outbound).

'manual.sms.out'([PhoneNo, Msg, Prefix]) ->
    check_if_paid(fun 'manual.sms.out2'/2, [PhoneNo, Msg, normalise(Prefix)],
                 outbound);
'manual.sms.out'([PhoneNo, Msg]) ->
    check_if_paid(fun 'manual.sms.out2'/2, [PhoneNo, Msg, ""], outbound).

'manual.sms.out2'([PhoneNo, Msg, Prefix], _AC) ->
    [PhoneNo2] = typechecks:std_strs([PhoneNo]),
    Msg2 = rightsize(Msg, ?SMSLength),
    PhoneNo3 = compress(PhoneNo2),
    Log = #contact_log{idx = get(idx), type = "manual sms",
                       to = "+" ++ Prefix ++ PhoneNo3, contents = Msg2},
    TwiML = "",
    Capability = [{manual_sms, "+" ++ Prefix ++ PhoneNo3, Msg2}],
    Type ={"softphone_type", "manual sms"},
    Config = [{"button_txt", "(+" ++ Prefix ++ ") " ++ PhoneNo3},
              {"headline_txt", "Send SMS"}],
    Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                   softphone_type = Type, softphone_config = Config},
    Headline = "Send SMS",
    ButtonTxt = "(+" ++ Prefix ++ ") " ++ PhoneNo3,
    phone(Phone, "Manual SMS: ",  Headline, ButtonTxt).

'auto.sms.out'([Condition, PhoneNo, Msg, Prefix]) ->
    check_if_paid(fun 'auto.sms.out2'/2,
                  [Condition, PhoneNo, Msg, normalise(Prefix)], outbound);
'auto.sms.out'([Condition, PhoneNo, Msg]) ->
    check_if_paid(fun 'auto.sms.out2'/2,
                  [Condition, PhoneNo, Msg, ""], outbound).

check_if_paid(Fun, Args, Type) ->
    Site = get(site),
    case contact_utils:get_twilio_account(Site) of
        ?ERRVAL_PAYONLY ->
            ?ERRVAL_PAYONLY;
        AC ->
            case Type of
                outbound ->
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

'auto.robocall2'([Condition, PhoneNo, Msg, Prefix], AC) ->
    [Condition] = typechecks:std_bools([Condition]),
    [PhoneNo2] = typechecks:std_strs([PhoneNo]),
    PhoneNo3 = compress(PhoneNo2),
    [Msg2] = typechecks:std_strs([Msg]),
    case Condition of
        true ->
            case contact_utils:robocall(AC, "+" ++ Prefix ++ PhoneNo2, Msg2) of
                {ok, ok} -> "Robocal: " ++ Msg2 ++ " made to phone " ++ PhoneNo3;
                _        -> ?ERRVAL_ERR
            end;
        false ->
            "Robocall: " ++ Msg2 ++ " to be sent to phone " ++ PhoneNo3
    end.

'auto.sms.out2'([Condition, PhoneNo, Msg, Prefix], AC) ->
    [Condition] = typechecks:std_bools([Condition]),
    [PhoneNo2] = typechecks:std_strs([PhoneNo]),
    PhoneNo3 = "+" ++ Prefix ++ compress(PhoneNo2),
    Msg2 = rightsize(Msg, ?SMSLength),
    Log = #contact_log{idx = get(idx), type = "automatic sms",
                       to = PhoneNo3, contents = Msg2},
    S = get(site),
    case Condition of
        true ->
            case contact_utils:post_sms(AC, PhoneNo3, Msg2) of
                {ok, ok} ->
                    spawn(hn_twilio_mochi, log,
                          [S, Log#contact_log{status = "ok"}]),
                    "<a href='./_contacts/'>sms sent</a>";
                _        ->
                    spawn(hn_twilio_mochi, log,
                          [S, Log#contact_log{status = "failed"}]),
                    ?ERRVAL_ERR
            end;
        false ->
            "SMS: " ++ Msg2 ++ " to be sent to phone " ++ PhoneNo3
    end.

'phone.out'([PhoneNo, Prefix]) ->
    'phone.out2'(PhoneNo, normalise(Prefix));
'phone.out'([PhoneNo]) ->
    'phone.out2'(PhoneNo, "").

'phone.out2'(PhoneNo, Prefix) ->
    Site = get(site),
    case contact_utils:get_twilio_account(Site) of
        ?ERRVAL_PAYONLY ->
            ?ERRVAL_PAYONLY;
        AC              ->
            #twilio_account{application_sid = AppSID,
                            site_phone_no = Site_Phone} = AC,
            [PhoneNo2] = typechecks:std_strs([PhoneNo]),
            PhoneNo3 = compress(PhoneNo2),
            Log = #contact_log{idx = get(idx), type = "outbound call",
                               to = "+" ++ Prefix ++ PhoneNo3},
            TwiML = [
                     #dial{callerId = Site_Phone, record = true,
                           body = [#number{number = "+" ++ Prefix ++ PhoneNo3}]}
                    ],
            Capability = [{client_outgoing, AppSID, []}],
            Type = {"softphone_type", "outbound call"},
            Config = [{"button_txt", "(+" ++ Prefix ++ ") " ++ PhoneNo3},
                      {"headline_txt", "Make Phone Call"}],
            Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                           softphone_type = Type, softphone_config = Config},
            Headline = "Make Phone Call",
            ButtonTxt = "(+" ++ Prefix ++ ") " ++ PhoneNo3,
            phone(Phone, "Phone Out: ", Headline, ButtonTxt)
    end.

'phone.in'([]) ->
    phone(#phone{}, "Phone In: ", "yerk", "berk").

phone(Payload, Preview, Headline, ButtonTxt) ->
    Site = get(site),
    Path = get(path),
    X = get(mx),
    Y = get(my),
    URL = hn_util:refX_to_url(#refX{site = Site, path = Path,
                                    obj = {cell, {X, Y}}}),
    HTML = "<div class='hn_softphone'>"
        ++ "<div class'hn_softphone_hd'>" ++ Headline ++ "</div>"
        ++ "<div class='hn_softphone_link'>"
        ++ "<a href='" ++ URL ++ "?view=phone' target='hnsoftphone'>"
        ++ ButtonTxt ++ "</a>"
        ++ "</div>"
        ++ "<div class='small'>(opens in new window) "
        ++ "<a href='./_contacts/'>logs</a></div>"
        ++ "</div>",
    PreV = #preview{title = Preview ++ ButtonTxt, width = 2, height = 4},
    #spec_val{val = HTML, preview = PreV, sp_phone = Payload}.

compress(List) ->
    L2 = re:replace(List, " ", "", [{return, list}, global]),
    L3 = re:replace(L2, "-", "", [{return, list}, global]),
    case L3 of
        "00"++Rest1 -> Rest1;
        "0"++Rest2  -> Rest2;
        Other       -> Other
    end.

normalise("+"++Prefix)  -> Prefix;
normalise("00"++Prefix) -> Prefix;
normalise(Prefix) ->
    P2 = case tconv:to_num(Prefix) of
             {error, nan} -> twilio_web_util:country_code_to_prefix(Prefix);
             Num          -> Num
         end,
    integer_to_list(P2).

rightsize(Msg, Size) ->
    [Msg2] = typechecks:std_strs([Msg]),
    Length = length(Msg2),
    if
        Length > Size  -> {Msg3, _} = lists:split(Size, Msg2),
                         Msg3;
        Length =< Size -> Msg2
    end.

is_valid_email([])       -> true;
is_valid_email(["" | T]) -> is_valid_email(T);
is_valid_email([H | T])  -> case hn_util:valid_email(H) of
                                true   -> is_valid_email(T);
                                false  -> false
                            end.

get_domain("http://" ++ Domain) ->
    [D, _] = string:tokens(Domain, ":"),
    D.
