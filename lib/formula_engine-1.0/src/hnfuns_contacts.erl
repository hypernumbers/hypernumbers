%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       functions for handling phones
%%%
%%% @end
%%% Created : 11 Feb 2012 by gordon@hypernumbers.com

-module(hnfuns_contacts).

-define(check_paid, contact_utils:check_if_paid).
-define(en_gb, 0).
-define(t, typechecks).

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
-include("muin_proc_dict.hrl").

'display.phone.nos'([]) ->
    Site = get(site),
    case contact_utils:get_twilio_account(Site) of
        ?ERRVAL_PAYONLY ->
            "No phone numbers have been purchased for this website";
        AC ->
            #twilio_account{site_phone_no = SitePhone, type = Type} = AC,
            SitePhone ++ " (" ++ atom_to_list(Type) ++ ")"
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
    [Msg2] = ?t:std_strs([Msg]),
    [IsMan2, L2] = ?t:std_ints([IsMan, Language]),
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
    [Cond2] = ?t:std_bools([Condition]),
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
    Vals = ?t:std_strs([To, Su, Cn, CC, Reply]),
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
    [PhNo2] = ?t:std_strs([PhNo]),
    Msg2 = contact_utils:rightsize(Msg, ?SMSLength),
    {Prefix2, PhNo3} = ?t:std_phone_no(Prefix, PhNo2),
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
    [Cond] = ?t:std_bools([Cond]),
    [PhNo2] = ?t:std_strs([PhNo]),
    {Prefix2, PhNo3} = ?t:std_phone_no(Prefix, PhNo2),
    [Msg2] = ?t:std_strs([Msg]),
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
    [Cond] = ?t:std_bools([Cond]),
    [PhNo2] = ?t:std_strs([PhNo]),
    {Prefix2, PhNo3} = ?t:std_phone_no(Prefix, PhNo2),
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
    'create.phone'(["Open Phone"]);
'create.phone'([Title]) ->
    'create.phone'([Title, 1]);
'create.phone'([Title, Type]) ->
    'create.phone'([Title, Type, ""]);
'create.phone'([Title, Type, Groups]) ->
    'create.phone'([Title, Type, Groups, false]);
'create.phone'([Title, Type, Groups, Extension]) ->
    'create.phone'([Title, Type, Groups, Extension, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                SMSMsg]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                    SMSMsg, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
                EmailTo]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                    SMSMsg, EmailTo, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
                EmailTo, EmailCC]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                    SMSMsg, EmailTo, EmailCC, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
                EmailTo, EmailCC, EmailFrom]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                    SMSMsg, EmailTo, EmailCC, EmailFrom, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
                EmailTo, EmailCC, EmailFrom, EmailSubject]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                    SMSMsg, EmailTo, EmailCC, EmailFrom, EmailSubject, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
                EmailTo, EmailCC, EmailFrom, EmailSubject, EmailBody]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                    SMSMsg, EmailTo, EmailCC, EmailFrom, EmailSubject,
                    EmailBody, ""]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
                EmailTo, EmailCC, EmailFrom, EmailSubject, EmailBody,
                EmailSig]) ->
    'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo,
                    SMSMsg, EmailTo, EmailCC, EmailFrom, EmailSubject, EmailBody,
                    EmailSig, 0]);
'create.phone'([Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
                EmailTo, EmailCC, EmailFrom, EmailSubject, EmailBody,
                EmailSig, Colour]) ->
    create_p2(Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
              EmailTo, EmailCC, EmailFrom, EmailSubject, EmailBody, EmailSig,
              Colour).

create_p2(Title, Type, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
          ETo, ECC, EFrom, ESubject, EBody, ESig, Colour) ->
    [Title2] = ?t:std_strs([Title]),
    [Type2] = ?t:std_ints([Type]),
    Check = case type(Type2) of
                'in/out' -> outbound;
                in       -> inbound;
                out      -> outbound;
                none     -> none
            end,
    ?check_paid(fun create_p3/2,
                [Title2, Type2, Groups, Extension, DiallingCode, PhoneNo,
                 SMSMsg, ETo, ECC, EFrom, ESubject, EBody, ESig, Colour],
                Check).
create_p3([Title, Type2, Groups, Extension, DiallingCode, PhoneNo, SMSMsg,
          ETo, ECC, EFrom, ESubject, EBody, ESig, Colour], AC) ->
    % this is a self referencing formula that needs to rewrite if the
    % cell moves
    put(selfreference, true),
    [G2] = ?t:throw_std_strs([Groups]),
    [Ext2] = case Extension of
                 false -> [false];
                 []    -> [false];
                 _     -> ?t:throw_std_strs([Groups, Extension])
             end,
    [D2] = ?t:throw_std_diallingcode(DiallingCode),
    [D3, P2] = ?t:throw_std_phone_no(D2, PhoneNo),
    [SMS2, To2, CC2] = ?t:throw_std_strs([SMSMsg, ETo, ECC]),
    From2 = valid_from_email(EFrom),
    [Sbj2, B2, Sg2] = ?t:throw_std_strs([ESubject, EBody, ESig]),
    [C2] = typechecks:throw_std_ints([Colour]),
    C3 = bootstrap_utils:get_colour(C2),
    Type3 = make_type(type(Type2)),
    % Got all our parameters - need to validate them for the phone type
    case is_valid(Type2, P2, SMS2, To2, Sbj2, B2) of
        false ->
            ?ERR_VAL;
        true  ->
            #twilio_account{application_sid = AppSID,
                            site_phone_no = SitePhone} = AC,
            Capability = case Ext2 of
                             false -> [{client_outgoing, AppSID, []}];
                             _     -> [{client_outgoing, AppSID, []},
                                       {client_incoming, Ext2}]
                         end,
            Config = make_config(Type2, G2, Ext2, D3, P2, SMS2, To2,
                                 CC2, From2, Sbj2, B2, Sg2),
            {TwiML, Log} = make_twiml(Type2, D3, P2, SitePhone),
            Phone = #phone{twiml = TwiML, capability = Capability,
                           log = Log, softphone_type = Type3,
                           softphone_config = Config},
            phone2(Phone, "Phone Out: ", Title, C3)
    end.

make_twiml(none, _, _, _) ->
    {none, #contact_log{idx = get(idx)}};
make_twiml(Type2, DiallingCode, PhoneNo, SitePhone) ->
    Type3 = type(Type2),
    Type4 = case Type3 of
                in       -> "incoming call";
                out      -> "outbound call";
                'in/out' -> "outbound call";
                none     -> "none"

            end,
    Log = #contact_log{idx = get(idx), type = Type4, to = ""},
    {PhonePerms, _SMSPerms, _EmailPerms} = get_config(Type2),
    TwiML = case PhonePerms of
                "free dial"  -> [
                                 #function_EXT{title = "make phone call",
                                               module = "softphone_srv",
                                               fn = "make_free_dial_call"}
                                ];
                "fixed dial" -> Number = "+" ++ DiallingCode ++ PhoneNo,
                                [
                                 #dial{callerId = SitePhone, record = true,
                                       body = [
                                               #number{number = Number}
                                              ]}
                                ];
                "none"       -> []
            end,
    {TwiML, Log}.

type(1)  -> 'in/out';
type(2)  -> in;
type(3)  -> out;
type(4)  -> none;
type(5)  -> none;
type(6)  -> out;
type(7)  -> 'in/out';
type(8)  -> out;
type(9)  -> none;
type(10) -> none;
type(11) -> none;
type(12) -> none;
type(13) -> 'in/out';
type(14) -> 'in/out';
type(15) -> 'in/out';
type(16) -> out;
type(17) -> out;
type(18) -> out;
type(_)  -> ?ERR_VAL.

make_type('in/out') -> {"capabilities", {struct, [{"phone_in",  "true"},
                                                  {"phone_out", "true"}]}};
make_type(in)       -> {"capabilities", {struct, [{"phone_in",  "true"},
                                                  {"phone_out", "false"}]}};
make_type(out)      -> {"capabilities", {struct, [{"phone_in",  "false"},
                                                  {"phone_out", "true"}]}};
make_type(none)     -> {"capabilities", {struct, [{"phone_in",  "false"},
                                                  {"phone_out", "false"}]}}.

make_config(Type, Groups, Extension, DiallingCode,
            PhoneNo, SMSMsg, EmailTo, EmailCC, EmailFrom, EmailSubject,
            EmailBody, EmailSig) ->
    {PhonePerms, SMSPerms, EmailPerms} = get_config(Type),
    {"config", {struct, [
                         {"phone_out_permissions",     PhonePerms},
                         {"sms_out_permissions",       SMSPerms},
                         {"email_permissions",         EmailPerms},
                         {"default_dialling_code",     DiallingCode},
                         {"extension",                 Extension},
                         {"groups",                    {array, Groups}},
                         {"phone_no",                  PhoneNo},
                         {"sms_msg",                   SMSMsg},
                         {"email_to",                  EmailTo},
                         {"email_cc",                  EmailCC},
                         {"email_from",                EmailFrom},
                         {"email_subject",             EmailSubject},
                         {"email_body",                EmailBody},
                         {"email_signature",           EmailSig}
                        ]}
    }.

% phone out, sms, email
get_config(1)  -> {"free dial",  "free all",     "none"};
get_config(2)  -> {"none",       "none",         "none"};
get_config(3)  -> {"free dial",  "none",         "none"};
get_config(4)  -> {"none",       "free all",     "none"};
get_config(5)  -> {"none",       "none",         "free all"};
get_config(6)  -> {"free dial",  "free all",     "none"};
get_config(7)  -> {"free dial",  "free all",     "free all"};
get_config(8)  -> {"fixed dial", "none",         "none"};
get_config(9)  -> {"none",       "free message", "none"};
get_config(10) -> {"none",       "fixed all",    "none"};
get_config(11) -> {"none",       "none",         "fixed all"};
get_config(12) -> {"none",       "none",         "free body"};
get_config(13) -> {"fixed dial", "fixed all",    "none"};
get_config(14) -> {"fixed dial", "fixed all",    "fixed all"};
get_config(15) -> {"fixed dial", "free message", "none"};
get_config(16) -> {"fixed dial", "fixed all",    "none"};
get_config(17) -> {"fixed dial", "free message", "none"};
get_config(18) -> {"fixed dial", "free message", "free body"};
get_config(_)  -> ?ERR_VAL.

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
                            site_phone_no = SitePhone} = AC,
            {Prefix2, PhNo2} = ?t:std_phone_no(Prefix, PhNo),
            Log = #contact_log{idx = get(idx), type = "outbound call",
                               to = "+" ++ Prefix2 ++ PhNo2},
            TwiML = [
                     #dial{callerId = SitePhone, record = true,
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
    [Name2] = ?t:std_strs([Name]),
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

phone2(Payload, Preview, ButtonTxt, Colour) ->
    Site = get(site),
    Path = get(path),
    X = get(mx),
    Y = get(my),
    URL = hn_util:refX_to_url(#refX{site = Site, path = Path,
                                    obj = {cell, {X, Y}}}),
    HTML = "<a class='btn btn-large hn-btn-large "  ++ Colour ++ "' href='"
        ++ URL ++ "?view=phone' target='hnsoftphone'>"
        ++ ButtonTxt ++ "</a>",
    PreV = Preview ++ ButtonTxt,
    Resize = #resize{width = 2, height = 2},
    JS = ["/bootstrap/js/bootstrap.js", "/bootstrap/js/helper.js"],
    Reload = ["HN.BootstrapHelper.reload();"],
    CSS = ["/bootstrap/css/bootstrap.css", "/bootstrap/css/helper.css"],
    Incs = #incs{js = JS, js_reload = Reload, css = CSS},
    #spec_val{val = HTML, preview = PreV, resize = Resize, sp_phone = Payload,
             sp_incs = Incs}.

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

% this function throws a #VAL! error if the parameters dont work at the most
% basic level - so if you have a fixed dial phone out then the phone number
% can't be blank, etc, etc
% free phones
is_valid(N, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody)
  when N =< 7 ->
    true;
% fixed dial out
is_valid(8, [], _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    false;
is_valid(8, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% fixed dial SMS
is_valid(9, [], _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    false;
is_valid(9, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% full fixed SMS
is_valid(10, Phone, SMS, _EmailTo, _EmailSubj, _EmailBody)
  when Phone == [] orelse SMS == [] ->
    false;
is_valid(10, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% fixed email
is_valid(11, _Phone, _SMS, EmailTo, EmailSubj, EmailBody)
  when EmailTo == [] orelse EmailSubj == [] orelse EmailBody == [] ->
    false;
is_valid(11, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% free body email
is_valid(12, _Phone, _SMS, EmailTo, _EmailSubj, _EmailBody)
  when EmailTo == [] ->
    false;
is_valid(12, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% fixed out bound phone and SMS
is_valid(13, Phone, SMS, _EmailTo, _EmailSubj, _EmailBody)
  when Phone == [] orelse SMS == [] ->
    false;
is_valid(13, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% fixed out bound phone, SMS and email
is_valid(14, Phone, SMS, EmailTo, EmailSubj, EmailBody)
  when Phone == [] orelse SMS == [] orelse EmailTo == [] orelse EmailTo == []
       orelse EmailSubj == [] orelse EmailBody == []->
    true;
is_valid(14, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% inbound with free message fixed dial outbound and SMS
is_valid(15, [], _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    false;
is_valid(15, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% all fixed out bound phone and SMS
is_valid(16, Phone, SMS, _EmailTo, _EmailSubj, _EmailBody)
  when Phone == [] orelse SMS == [] ->
    false;
is_valid(16, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% free message fixed dial outbound and SMS
is_valid(17, [], _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    false;
is_valid(17, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
% free message fixed dial outbound and SMS with fixed email/free email body
is_valid(18, Phone, _SMS, EmailTo, EmailSubj, _EmailBody)
  when Phone == [] orelse EmailTo == [] orelse EmailSubj == [] ->
    true;
is_valid(18, _Phone, _SMS, _EmailTo, _EmailSubj, _EmailBody) ->
    true;
is_valid(_, _, _, _, _, _) ->
    false.

valid_from_email("") ->
    [_Proto, "//" ++ Domain, _Port] = string:tokens(?msite, ":"),
    "no-reply@" ++ Domain;
valid_from_email(Email) ->
    case hn_util:valid_email(Email) of
        false -> ?ERR_VAL;
        true  -> [_Name, Domain] = string:tokens(Email, "@"),
                 case ?msite of
                     Domain -> Email;
                     _      -> ?ERR_VAL
                 end
    end.
