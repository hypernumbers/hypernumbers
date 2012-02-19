%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       functions for handling phones
%%%            These fns are mirroed in phone_twilm.erl
%%% @end
%%% Created : 11 Feb 2012 by gordon@hypernumbers.com

-module(hnfuns_phone).

-export([
         'manual.sms.out'/1,
         'auto.sms.out'/1,
         'auto.robocall'/1,
         'phone.out'/1,
         'phone.in'/1
         ]).

-include("spriki.hrl").
-include("errvals.hrl").
-include("keyvalues.hrl").

'auto.robocall'([Condition, PhoneNo, Msg, Prefix]) ->
    check_if_paid(fun 'auto.robocall2'/2,
                  [Condition, PhoneNo, Msg, normalise(Prefix)]);
'auto.robocall'([Condition, PhoneNo, Msg]) ->
    check_if_paid(fun 'auto.robocall2'/2, [Condition, PhoneNo, Msg, ""]).

'manual.sms.out'([PhoneNo, Prefix]) ->
    check_if_paid(fun 'manual.sms.out2'/2, [PhoneNo, normalise(Prefix)]);
'manual.sms.out'([PhoneNo]) ->
    check_if_paid(fun 'manual.sms.out2'/2, [PhoneNo, ""]).

'manual.sms.out2'([PhoneNo, Prefix], AC) ->
    [PhoneNo2] = typechecks:std_strs([PhoneNo]),
    PhoneNo3 = compress(PhoneNo2),
    erk.

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
    PhoneNo3 = compress(PhoneNo2),
    [Msg2] = typechecks:std_strs([Msg]),
    Length = length(Msg2),
    Msg3 = if
               Length > 160  -> {Msg2, _} = lists:split(160, Msg2),
                                Msg2;
               Length =< 160 -> Msg2
           end,
    case Condition of
        true ->
            case contact_utils:post_sms(AC, "+" ++ Prefix ++ PhoneNo3, Msg3) of
                {ok, ok} -> "SMS: " ++ Msg3 ++ " sent to phone " ++ PhoneNo3;
                _        -> ?ERRVAL_ERR
            end;
        false ->
            "SMS: " ++ Msg3 ++ " to be sent to phone " ++ PhoneNo3
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
            Log = #contact_log{to = PhoneNo3},
            TwiML = "<Response>" ++
                "<Dial callerId='" ++ Site_Phone ++ "' record='true'>" ++
                "<Number>" ++ Prefix ++ PhoneNo3 ++ "</Number>"
                "</Dial>" ++
                "</Response>",
            Capability = [{client_outgoing, AppSID, []}],
            Type = {"softphone_type", "outbound call"},
            Config = {"button_txt", "(+" ++ Prefix ++ ") " ++ PhoneNo3},
            Phone = #phone{twiml = TwiML, capability = Capability, log = Log,
                           softphone_type = Type, softphone_config = Config},
            Headline = "Make Phone Call",
            ButtonTxt = "(+" ++ Prefix ++ ") " ++ PhoneNo3,
            phone(Phone, Headline, ButtonTxt)
    end.

'phone.in'([]) ->
    phone(#phone{}, "yerk", "berk").

phone(Payload, Headline, ButtonTxt) ->
    Site = get(site),
    Path = get(path),
    X = get(mx),
    Y = get(my),
    URL = hn_util:refX_to_url(#refX{site = Site, path = Path,
                                    obj = {cell, {X, Y}}}),
    HTML= "<div class='hn_softphone'>"
        ++ "<div class'hn_softphone_hd'>" ++ Headline ++ "</div>"
        ++ "<div class='hn_softphone_link'>"
        ++ "<a href='" ++ URL ++ "?view=phone' target='hnsoftphone'>"
        ++ ButtonTxt ++ "</a>"
        ++ "</div>"
        ++ "<div class='small'>(opens in new window)</div>"
        ++ "</div>",
    {phone, {"Phone: " ++ ButtonTxt, 2, 4, Payload}, HTML}.

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
