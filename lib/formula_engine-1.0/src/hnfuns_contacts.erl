%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       functions for handling phones
%%%            These fns are mirroed in phone_twilm.erl
%%% @end
%%% Created : 11 Feb 2012 by gordon@hypernumbers.com

-module(hnfuns_contacts).

-export([
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

'manual.email'([To, Subject, Contents]) ->
    'manual.email'([To, Subject, Contents, [], []]);
'manual.email'([To, Subject, Contents, CC]) ->
    'manual.email'([To, Subject, Contents, CC, []]);
'manual.email'([To, Su, Cn, CC, Reply]) ->
    [To2, Su2, Cn2, CC2, Fr] = check(To, Su, Cn, CC, Reply),
    Log = #contact_log{idx = get(idx),
                       type = "manual email",
                       to = To2,
                       from = Fr,
                       cc = CC2,
                       subject = Su2,
                       contents = Cn2},
    TwiML = "",
    Capability = [{manual_email, To2, Fr, CC2}],
    Type ={"softphone_type", "manual email"},
    Config = [{"button_txt", To},
              {"headline_txt", "Send Email"},
              {"to_txt", To2},
              {"from_txt", Fr},
              {"cc_txt", CC2},
              {"subject_txt", Su2},
              {"email_msg", Cn2}],
    Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                   softphone_type = Type, softphone_config = Config},
    Headline = "Send Email",
    ButtonTxt = To2,
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
                 "<a href='./contacts/'>email sent</a>"
    end.

check(To, Su, Cn, CC, Reply) ->
    Domain = get_domain(get(site)),
    Vals = typechecks:std_strs([To, Su, Cn, CC, Reply]),
    [To2, Su2, Cn2, CC2, R2] = Vals,
    Fr = case R2 of
             [] -> "no-reply@" ++ Domain;
             _  -> R2 ++ Domain
         end,
    case is_valid([To2, CC2, Fr]) of
        false -> ?ERR_VAL;
        true  -> [To2, Su2, Cn2, CC2, Fr]
    end.

'auto.robocall'([Condition, PhoneNo, Msg, Prefix]) ->
    check_if_paid(fun 'auto.robocall2'/2,
                  [Condition, PhoneNo, Msg, normalise(Prefix)]);
'auto.robocall'([Condition, PhoneNo, Msg]) ->
    check_if_paid(fun 'auto.robocall2'/2, [Condition, PhoneNo, Msg, ""]).

'manual.sms.out'([PhoneNo, Msg, Prefix]) ->
    check_if_paid(fun 'manual.sms.out2'/2, [PhoneNo, Msg, normalise(Prefix)]);
'manual.sms.out'([PhoneNo, Msg]) ->
    check_if_paid(fun 'manual.sms.out2'/2, [PhoneNo, Msg, ""]).

'manual.sms.out2'([PhoneNo, Msg, Prefix], _AC) ->
    [PhoneNo2] = typechecks:std_strs([PhoneNo]),
    Msg2 = rightsize(Msg),
    PhoneNo3 = compress(PhoneNo2),
    Log = #contact_log{idx = get(idx), type = "manual sms",
                       to = "+" ++ Prefix ++ PhoneNo3, contents = Msg2},
    TwiML = "",
    Capability = [{manual_sms, "+" ++ Prefix ++ PhoneNo3, Msg2}],
    Type ={"softphone_type", "manual sms"},
    Config = [{"button_txt", "(+" ++ Prefix ++ ") " ++ PhoneNo3},
              {"headline_txt", "Send SMS"},
              {"sms_msg", Msg2}],
    Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                   softphone_type = Type, softphone_config = Config},
    Headline = "Send SMS",
    ButtonTxt = "(+" ++ Prefix ++ ") " ++ PhoneNo3,
    phone(Phone, "Manual SMS: ",  Headline, ButtonTxt).

'auto.sms.out'([Condition, PhoneNo, Msg, Prefix]) ->
    check_if_paid(fun 'auto.sms.out2'/2,
                  [Condition, PhoneNo, Msg, normalise(Prefix)]);
'auto.sms.out'([Condition, PhoneNo, Msg]) ->
    check_if_paid(fun 'auto.sms.out2'/2, [Condition, PhoneNo, Msg, ""]).

check_if_paid(Fun, Args) ->
    Site = get(site),
    case contact_utils:get_twilio_account(Site) of
        ?ERRVAL_PAYONLY ->
            ?ERRVAL_PAYONLY;
        AC              ->
            Fun(Args, AC)
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
    Msg2 = rightsize(Msg),
    Log = #contact_log{idx = get(idx), type = "automatic sms",
                       to = PhoneNo3, contents = Msg2},
    S = get(site),
    case Condition of
        true ->
            case contact_utils:post_sms(AC, PhoneNo3, Msg2) of
                {ok, ok} ->
                    spawn(hn_twilio_mochi, log,
                          [S, Log#contact_log{status = "ok"}]),
                    "<a href='./contacts/'>sms sent</a>";
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
            TwiML = "<Response>" ++
                "<Dial callerId='" ++ Site_Phone ++ "' record='true'>" ++
                "<Number>" ++ Prefix ++ PhoneNo3 ++ "</Number>"
                "</Dial>" ++
                "</Response>",
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
        ++ "<a href='./contacts/'>logs</a></div>"
        ++ "</div>",
    {phone, {Preview ++ ButtonTxt, 2, 4, Payload}, HTML}.

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

rightsize(Msg) ->
    [Msg2] = typechecks:std_strs([Msg]),
    Length = length(Msg2),
    if
        Length > 160  -> {Msg3, _} = lists:split(160, Msg2),
                         Msg3;
        Length =< 160 -> Msg2
    end.

is_valid([])       -> true;
is_valid([[] | T]) -> is_valid(T);
is_valid([H | T])  -> case hn_util:valid_email(H) of
                          true   -> is_valid(T);
                          false  -> false
                      end.
get_domain("http://" ++ Domain) ->
    [D, _] = string:tokens(Domain, ":"),
    D.
