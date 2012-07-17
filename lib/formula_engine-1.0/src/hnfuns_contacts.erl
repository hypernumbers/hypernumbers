%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       functions for handling phones
%%%            These fns are mirroed in phone_twilm.erl
%%% @end
%%% Created : 11 Feb 2012 by gordon@hypernumbers.com

-module(hnfuns_contacts).

-define(check_paid, contact_utils:check_if_paid).
-define(en_gb, 0).

-export([
         'display.phone.nos'/1,
         'text.answerphone'/1,
         'manual.email'/1,
         'auto.email'/1,
         'manual.sms.out'/1,
         'auto.sms.out'/1,
         'auto.robocall'/1,
         'phone.out'/1,
         'phone.in'/1,
         'create.phone'/1
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
            Site_Phone ++ " (" ++ atom_to_list(Type) ++ ")"
    end.

'text.answerphone'([]) ->
    'text.answerphone'(["Please leave a message after the beep. Press Star to finish"]);
'text.answerphone'([Msg]) ->
    'text.answerphone'([Msg, false]);
'text.answerphone'([Msg, IsMan]) ->
    'text.answerphone'([Msg, IsMan, ?en_gb]);
'text.answerphone'([Msg, IsMan, Language]) ->
    ?check_paid(fun 'text.answerphone2'/2, [Msg, IsMan, Language], inbound).

'text.answerphone2'([Msg, IsMan, Language], _AC) ->
    [Msg2] = typechecks:std_strs([Msg]),
    [IsMan2, L2] = typechecks:std_ints([IsMan, Language]),
    Voice = contact_utils:get_voice(IsMan2),
    L3 = contact_utils:get_lang(L2),
    Msg3 = contact_utils:rightsize(Msg2, ?SAYLength),
    SAY = #say{voice = Voice, language = L3, text = Msg3},
    Title = contact_utils:rightsize(Msg2, 40),
    Preview = "ANSWERPHONE: " ++ Title,
    Resize = #resize{width = 2, height = 2},
    RECORD = #record{method = "GET", finishOnKey = "*"},
    #spec_val{val = "", preview = Preview, resize = Resize,
              sp_phone = #phone{twiml = [SAY, RECORD]}}.

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
    case is_valid_email(Emails, Domain) of
        false -> ?ERR_VAL;
        true  -> [string:join(To3, ";"), Su2, Cn2, string:join(CC3, ";"), Fr]
    end.

'auto.robocall'([Cond, PhNo, Msg, Prefix]) ->
    ?check_paid(fun 'auto.robocall2'/2, [Cond, PhNo, Msg, Prefix], outbound);
'auto.robocall'([Cond, PhNo, Msg]) ->
    ?check_paid(fun 'auto.robocall2'/2, [Cond, PhNo, Msg, ""], outbound).

'manual.sms.out'([PhNo, Msg, Prefix]) ->
    ?check_paid(fun 'manual.sms.out2'/2, [PhNo, Msg, Prefix], outbound);
'manual.sms.out'([PhNo, Msg]) ->
    ?check_paid(fun 'manual.sms.out2'/2, [PhNo, Msg, ""], outbound).

'manual.sms.out2'([PhNo, Msg, Prefix], _AC) ->
    [PhNo2] = typechecks:std_strs([PhNo]),
    Msg2 = contact_utils:rightsize(Msg, ?SMSLength),
    {Prefix2, PhNo3} = typechecks:std_phone_no(Prefix, PhNo2),
    Log = #contact_log{idx = get(idx), type = "manual sms",
                       to = "+" ++ Prefix2 ++ PhNo3, contents = Msg2},
    TwiML = "",
    Capability = [{manual_sms, "+" ++ Prefix2 ++ PhNo3, Msg2}],
    Type ={"softphone_type", "manual sms"},
    Config = [{"button_txt", "(+" ++ Prefix2 ++ ") " ++ PhNo3},
              {"headline_txt", "Send SMS"}],
    Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                   softphone_type = Type, softphone_config = Config},
    Headline = "Send SMS",
    ButtonTxt = "(+" ++ Prefix2 ++ ") " ++ PhNo3,
    phone(Phone, "Manual SMS: ",  Headline, ButtonTxt).

'auto.sms.out'([Cond, PhNo, Msg, Prefix]) ->
    ?check_paid(fun 'auto.sms.out2'/2, [Cond, PhNo, Msg, Prefix], outbound);
'auto.sms.out'([Cond, PhNo, Msg]) ->
    ?check_paid(fun 'auto.sms.out2'/2, [Cond, PhNo, Msg, ""], outbound).

'auto.robocall2'([Cond, PhNo, Msg, Prefix], AC) ->
    [Cond] = typechecks:std_bools([Cond]),
    [PhNo2] = typechecks:std_strs([PhNo]),
    {Prefix2, PhNo3} = typechecks:std_phone_no(Prefix, PhNo2),
    [Msg2] = typechecks:std_strs([Msg]),
    case Cond of
        true ->
            case contact_utils:robocall(AC, "+" ++ Prefix2 ++ PhNo2, Msg2) of
                {ok, ok} -> "Robocal: " ++ Msg2 ++ " made to phone (+"
                                ++ Prefix ++ ") " ++ PhNo3;
                _        -> ?ERRVAL_ERR
            end;
        false ->
            "Robocall: " ++ Msg2 ++ " to be sent to phone (+" ++ Prefix2
                ++ ") " ++ PhNo3
    end.

'auto.sms.out2'([Cond, PhNo, Msg, Prefix], AC) ->
    [Cond] = typechecks:std_bools([Cond]),
    [PhNo2] = typechecks:std_strs([PhNo]),
    {Prefix2, PhNo3} = typechecks:std_phone_no(Prefix, PhNo2),
    PhNo3 = "+" ++ Prefix2 ++ PhNo2,
    Msg2 = contact_utils:rightsize(Msg, ?SMSLength),
    Log = #contact_log{idx = get(idx), type = "automatic sms",
                       to = PhNo3, contents = Msg2},
    S = get(site),
    case Cond of
        true ->
            case contact_utils:post_sms(AC, PhNo3, Msg2) of
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
            "SMS: " ++ Msg2 ++ " to be sent to phone (+" ++ Prefix ++ ")"
                ++ PhNo3
    end.

'create.phone'([]) ->
    'create.phone'([0]);
'create.phone'([Type]) ->
    create_p2(Type).

create_p2(Type) ->
    % this is a self referencing formula that needs to rewrite if the
    % cell moves
    put(selfreference, true),
    [Type2] = typechecks:std_ints([Type]),
    Type3 = make_type(Type2),
    Site = get(site),
    case contact_utils:get_twilio_account(Site) of
        ?ERRVAL_PAYONLY ->
            ?ERRVAL_PAYONLY;
        AC ->
            #twilio_account{application_sid = AppSID} = AC,
            Log = #contact_log{idx = get(idx), type = "outbound call",
                               to = ""},
            TwiML = [#function_EXT{title = "make phone call",
                                   module = "softphone_srv",
                                   fn = "make_free_dial_call"}],
            Capability = [{client_outgoing, AppSID, []}],
            Config = make_config(Type2),
            Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                           softphone_type = Type3, softphone_config = Config},
            Headline = "Make Phone Call",
            ButtonTxt = "Create Phone",
            phone(Phone, "Phone Out: ", Headline, ButtonTxt)
    end.

make_type(0) -> {"capabilities", {struct, [{"phone_in",  "true"},
                                           {"phone_out", "true"}]}};
make_type(1) -> {"capabilities", {struct, [{"phone_in",  "true"},
                                           {"phone_out", "false"}]}};
make_type(2) -> {"capabilities", {struct, [{"phone_in",  "false"},
                                           {"phone_out", "true"}]}};
make_type(3) -> {"capabilities", {struct, [{"phone_in",  "false"},
                                           {"phone_out", "false"}]}};
make_type(_) -> {"capabilities", {struct, [{"phone_in",  "false"},
                                           {"phone_out", "false"}]}}.

make_config(_) ->
    {"config", {struct, [
                         {"phone_out_permissions",     "free dial"},
                         {"sms_out_permissions",       "free all"},
                         {"email_permissions",         "free all"},
                         {"default_dialling_code",     false},
                         {"extension",                 "1234"},
                         {"groups",                    {array, [
                                                                "Sales",
                                                                "Marketing"
                                                               ]}},
                         {"phone_no",                  "+12345"},
                         {"sms_msg",                   "yowza"},
                         {"email_to",                  "gordon@vixo.com"},
                         {"email_cc",                  "debug@vixo.com"},
                         {"email_from",                "root@vixo.com"},
                         {"email_subject",             "Hasta Victoria Sempre"},
                         {"email_body",                "erk..."},
                         {"email_signature",           "Respec'"}
                        ]}
    }.

% TODO make it handle errors better
'phone.out'([PhNo, Prefix]) ->
    'phone.out2'(PhNo, Prefix);
'phone.out'([PhNo]) ->
    'phone.out2'(PhNo, "").

'phone.out2'(PhNo, Prefix) ->
    % this is a self referencing formula that needs to rewrite if the
    % cell moves
    put(selfreference, true),
    Site = get(site),
    case contact_utils:get_twilio_account(Site) of
        ?ERRVAL_PAYONLY ->
            ?ERRVAL_PAYONLY;
        AC ->
            #twilio_account{application_sid = AppSID,
                            site_phone_no = Site_Phone} = AC,
            {Prefix2, PhNo2} = typechecks:std_phone_no(Prefix, PhNo),
            Log = #contact_log{idx = get(idx), type = "outbound call",
                               to = "+" ++ Prefix2 ++ PhNo2},
            TwiML = [
                     #dial{callerId = Site_Phone, record = true,
                           body = [#number{number = "+" ++ Prefix2 ++ PhNo2}]}
                    ],
            Capability = [{client_outgoing, AppSID, []}],
            Type = {"softphone_type", "outbound call"},
            Config = [{"button_txt", "(+" ++ Prefix2 ++ ") " ++ PhNo2},
                      {"headline_txt", "Make Phone Call"}],
            Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                           softphone_type = Type, softphone_config = Config},
            Headline = "Make Phone Call",
            ButtonTxt = "(+" ++ Prefix2 ++ ") " ++ PhNo2,
            phone(Phone, "Phone Out: ", Headline, ButtonTxt)
    end.

'phone.in'([]) -> 'phone.in'(["Default"]);
'phone.in'([Name]) -> ?check_paid(fun 'phone.in2'/2, [Name], inbound).

'phone.in2'([Name], _AC) ->
    % this is a self referencing formula that needs to rewrite if the
    % cell moves
    put(selfreference, true),
    [Name2] = typechecks:std_strs([Name]),
    Log = #contact_log{idx = get(idx), type = "inbound call", to = "Name2"},
    TwiML = [],
    Capability = [{client_incoming, Name}],
    Type = {"softphone_type", "inbound call"},
    ButtonTxt = "User:" ++ Name,
    Config = [{"button_txt", ButtonTxt},
              {"headline_txt", "Receive Phone Call"}],
    Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                   softphone_type = Type, softphone_config = Config},
    Headline = "Inbound Phone: " ++ Name2,
    phone(Phone, "Phone In: ", Headline, ButtonTxt).

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
    PreV = Preview ++ ButtonTxt,
    Resize = #resize{width = 2, height = 4},
    #spec_val{val = HTML, preview = PreV, resize = Resize,
              sp_phone = Payload}.

% exemption for testing domain
is_valid_email(_List, "hypernumbers.dev") ->
    true;
is_valid_email([], _) ->
    true;
is_valid_email(["" | T], Domain) ->
    is_valid_email(T, Domain);
is_valid_email([H | T], Domain) ->
    case hn_util:valid_email(H) of
        true   -> is_valid_email(T, Domain);
        false  -> false
    end.

get_domain("http://" ++ Domain) ->
    [D, _] = string:tokens(Domain, ":"),
    D.
