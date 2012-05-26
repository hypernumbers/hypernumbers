%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd (Vixo)
%%% @doc       Functions for administering the site
%%%
%%% @end
%%% Created : 21 Apr 2012 by gordon@vixo.com

-module(hnfuns_site).

-include("spriki.hrl").
-include("errvals.hrl").
-include("twilio.hrl").
-include("muin_records.hrl").
-include("muin_proc_dict.hrl").
-include("defaults.hrl").

-define(check_paid, contact_utils:check_if_paid).
-define(RecordingLen, 2).

% WARNING!
% when adding more fns of the type phone.menu.xxx you need to stop
% muin swallowing them by looking for phone.menu.WxH
-export([
         'users.and.groups.'/1,
         'configure.email'/1,
         'phone.menu.'/1,
         'phone.menu.say'/1,
         'phone.menu.play'/1,
         'phone.menu.input'/1,
         'phone.menu.record'/1,
         'phone.menu.phoneno'/1
         ]).

'users.and.groups.'([W, H]) ->
    [W2, H2] = typechecks:throw_std_ints([W, H]),
    Site = get(site),
    Groups = lists:sort(hn_groups:get_groups(Site)),
    HTML = "<div class='hn_site_admin'>"
        ++ "<div class='hn_site_admin_top'>Groups And Users</div>"
        ++"<table>"
        ++ "<tr><td><em>Group</em></td><td><em>Users</em></td></td>"
        ++ ["<tr><td>" ++ G ++ "</td><td>" ++ string:join(M, "<br />")
            ++ "</td></tr>" || {G, M} <- Groups]
        ++ "</table></div>",
    Resize = #resize{width = W2, height = H2},
    #spec_val{val = lists:flatten(HTML), resize = Resize, sp_site = true}.

'phone.menu.'([W, H | List]) ->
    ?check_paid(fun 'phone.menu2'/2, [W, H, List], inbound).

'phone.menu2'([W, H, List], _Acnt) ->
    Site = get(site),
    Idx = get(idx),
    V = new_db_wu:read_kvD(Site, site_phone_menu),
    OrigIdx = case V of
                  []                                           -> null;
                  [{kvstore, site_phone_menu, {OldIdx, null}}] -> OldIdx
              end,
    case OrigIdx of
        I when I == Idx orelse I == null orelse I == deleted ->
            [W2, H2] = typechecks:throw_std_ints([W, H]),
            TwiML = gather(List),
            Val = case twiml:is_valid(TwiML) of
                      false -> ?ERR_VAL;
                      true  -> twiml:compile(TwiML, html)
                  end,
            HTML = "<div class='hn_site_admin'>"
                ++"<div class='hn_site_admin_top'>Site Phone Menu</div>"
                ++ Val ++ "</div>",
            % Unique records are two part - the idx and the payload
            % for phone menus the payload is null
            ok = new_db_wu:write_kvD(Site, site_phone_menu, {Idx, null}),
            RSz = #resize{width = W2, height = H2},
            #spec_val{val = HTML, resize = RSz,
                      sp_phone = #phone{twiml = TwiML},
                      unique = site_phone_menu};
        OldI ->
            Uniq = new_db_wu:idx_to_xrefXD(Site, OldI),
            #xrefX{path = P, obj = {cell, _} = O} = Uniq,
            "Error: Phone Menu already set in "
                ++ hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O)
    end.

'phone.menu.phoneno'([Number]) ->
    'phone.menu.phoneno'([Number, ""]);
'phone.menu.phoneno'([Number, Prefix]) ->
    {Prefix2, PhNo2} = typechecks:std_phone_no(Prefix, Number),
    Twiml = [#number{number = "+" ++ Prefix ++ PhNo2}],
    % this Twiml isn't valid on its loneo!
    Preview = "PHONE NUMBER: (+" ++ Prefix2 ++ ") " ++ PhNo2,
    Resize = #resize{width = 2, height = 2},
    #spec_val{val = "", preview = Preview, resize = Resize,
              sp_phone = #phone{twiml = Twiml}}.

'phone.menu.record'([]) ->
    'phone.menu.record'(["Please leave a message after the tone"]);
'phone.menu.record'([Text]) ->
    'phone.menu.record'([Text, true]);
'phone.menu.record'([Text, PlayBeep]) ->
    'phone.menu.record'([Text, PlayBeep, ?RecordingLen]);
'phone.menu.record'([Text, PlayBeep, RecordingLen]) ->
    'phone.menu.record'([Text, PlayBeep, RecordingLen, false]);
'phone.menu.record'([Text, PlayBeep, RecordingLen, Transcribe]) ->
    'phone.menu.record'([Text, PlayBeep, RecordingLen, Transcribe, 0]);
'phone.menu.record'([Text, PlayBeep, RecordingLen, Transcribe, Voice]) ->
    'phone.menu.record'([Text, PlayBeep, RecordingLen, Transcribe, Voice, 0]);
'phone.menu.record'([Text, PlayBeep, RecordingLen, Transcribe, Voice, Lang]) ->
    [Text2] = typechecks:std_strs([Text]),
    Title = get_title(Text2),
    [PlayBeep2] = typechecks:std_bools([PlayBeep]),
    [RLen2] = typechecks:std_ints([RecordingLen]),
    [Trans2] = typechecks:std_bools([Transcribe]),
    [Voice2, Lang2] = typechecks:std_ints([Voice, Lang]),
    V3 = get_voice(Voice2),
    L3 = get_lang(Lang2),
    SAY = #say{text = Text2, voice = V3, language = L3},
    REC = #record{playBeep = PlayBeep2, transcribe = Trans2, maxLength = RLen2},
    Twiml = [SAY, REC],
    case twiml:is_valid(Twiml) of
        false ->
            ?ERRVAL_VAL;
        true  ->
            Preview = "RECORD: (" ++ Title ++ ")",
            Resize = #resize{width = 2, height = 2},
            #spec_val{val = "", preview = Preview, resize = Resize,
                      sp_phone = #phone{twiml = Twiml}}
    end.

'phone.menu.input'([List]) ->
    io:format("List is ~p~n", [List]),
    ok.

'phone.menu.play'([Url]) ->
    'phone.menu.play'([Url, 1]);
'phone.menu.play'([Url, Loop]) ->
    [U2] = typechecks:std_strs([Url]),
    [Lp2] = typechecks:std_pos_ints([Loop]),
    Preview = "PLAY: " ++ U2,
    Resize = #resize{width = 2, height = 2},
    SAY = #play{url = U2, loop = Lp2},
    #spec_val{val = "", preview = Preview, resize = Resize,
              sp_phone = #phone{twiml = [SAY]}}.

'phone.menu.say'([Text]) ->
    'phone.menu.say'([Text, 0]);
'phone.menu.say'([Text, Voice]) ->
    'phone.menu.say'([Text, Voice, 0]);
'phone.menu.say'([Text, Voice, Language]) ->
    'phone.menu.say'([Text, Voice, Language, 1]);
'phone.menu.say'([Text, Voice, Language, Loop]) ->
    phsay(Text, Voice, Language, Loop).

phsay(Text, Voice, Language, Loop) ->
    [Text2] = typechecks:std_strs([Text]),
    [V2, L2, Lp2] = typechecks:std_pos_ints([Voice, Language, Loop]),
    Len = length(Text2),
    ok = typechecks:in_range(Len, 1, 4000),
    V3 = get_voice(V2),
    L3 = get_lang(L2),
    Title = get_title(Text2),
    Preview = "SAY: " ++ Title,
    Resize = #resize{width = 2, height = 2},
    SAY = #say{text = Text2, voice = V3, language = L3, loop = Lp2},
    #spec_val{val = "", preview = Preview, resize = Resize,
              sp_phone = #phone{twiml = [SAY]}}.

'configure.email'([FromEmail]) ->
    'configure.email'([FromEmail, ""]);
'configure.email'([FromEmail, Signature]) ->
    [FromE2, Sig2] = typechecks:std_strs([FromEmail, Signature]),
    Valid = hn_util:valid_email(FromE2),
    if
        Valid == false -> ?ERR_VAL;
        Valid == true  -> ok
    end,
    Site = get(site),
    Idx = get(idx),
    V = new_db_wu:read_kvD(Site, site_email),
    % got some validation to do
    % a new email has to be validated
    R2 = case V of
             [] ->
                 {null, #site_email{email = FromE2,
                                    email_validated = false,
                                    signature = Sig2}};
             [{kvstore, site_email, {OldIdx, Rec}}] ->
                 case Rec of
                     #site_email{email = FromE2} ->
                         {OldIdx, Rec#site_email{signature = Sig2}};
                     #site_email{} ->
                         {OldIdx, #site_email{email = FromE2,
                                              email_validated = false,
                                              signature = Sig2}}
                 end
         end,
    % there can only be one configure_email panel at one time
    {OrigIdx, SE} = R2,
    case OrigIdx of
        I when I == Idx orelse I == deleted orelse I == null ->
            HTML = "<div class='hn_site_admin'>"
                ++ "<div class='hn_site_admin_top'>Email Configuration</div>"
                ++ "<div>" ++ SE#site_email.email ++ "<br />"
                ++ "<em>Has the email been validated?</em> "
                ++ atom_to_list(SE#site_email.email_validated) ++ "<br />"
                ++ "<em>Signature is:</em> " ++ SE#site_email.signature
                ++ "</div></div>",
            ok = new_db_wu:write_kvD(Site, site_email, {Idx, SE}),
            case SE#site_email.email_validated of
                false -> send_invite(FromE2, Idx);
                true  -> ok
            end,
            RSz = #resize{width = 4, height = 9},
            #spec_val{val = HTML, resize = RSz, unique = site_email};
        OldI ->
            Uniq = new_db_wu:idx_to_xrefXD(Site, OldI),
            #xrefX{path = P, obj = {cell, _} = O} = Uniq,
            "Error: Site Email already set in "
                ++ hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O)
    end.

gather(List) -> gath(List, []).

gath([], Acc) -> lists:flatten(lists:reverse(Acc));
gath([#cellref{col = {_, C}, row = {_, R}, path = Path} = H | T], Acc) ->
    Site = ?msite,
    OrigPath = ?mpath,
    Ref = hn_util:obj_to_ref({cell, {?mx + C, ?my + R}}),
    P2 = hn_util:list_to_path(muin_util:walk_path(OrigPath, Path)),
    URL = Site ++ P2 ++ Ref,
    RefX = hn_util:url_to_refX(URL),
    Phone = new_db_wu:get_phone(RefX),
    NewAcc = case Phone of
                 []  -> Acc;
                 [P] -> [P#phone.twiml | Acc]
             end,
    % now just fetch the value and chuck it away
    % need to set up the recalc links properly
    % this is the easiest/best way to do it
    _Val = muin:fetch(H, "value"),
    gath(T, NewAcc);
gath([#rangeref{tl = {{_, X1}, {_, Y1}},
                br = {{_, X2}, {_, Y2}},
                path = Path} = H | T], Acc) ->
    Site = ?msite,
    OrigPath = ?mpath,
    Ref1 = hn_util:obj_to_ref({cell, {?mx + X1, ?my + Y1}}),
    Ref2 = hn_util:obj_to_ref({cell, {?mx + X2, ?my + Y2}}),
    P2 = hn_util:list_to_path(muin_util:walk_path(OrigPath, Path)),
    URL = Site ++ P2 ++ Ref1 ++ ":" ++ Ref2,
    RefX = hn_util:url_to_refX(URL),
    Phone = new_db_wu:get_phone(RefX),
    NewAcc = case Phone of
                 [] -> Acc;
                 P  -> TwiMLs = [X#phone.twiml || X <- P],
                       [TwiMLs | Acc]
             end,
    % now just fetch the value and chuck it away
    % need to set up the recalc links properly
    % this is the easiest/best way to do it
    _Val = muin:fetch(H, "value"),
    gath(T, NewAcc);
% its an unevaluated function so eval it
gath([H | T], Acc) ->
    Val = muin:external_eval_formula(H),
    NewAcc = case Val of
                 #spec_val{} ->
                     SP_P = Val#spec_val.sp_phone,
                     case SP_P of
                         null ->
                             Acc;
                         _    ->
                             [SP_P#phone.twiml | Acc]
                     end;
                 _           -> Acc
             end,
    gath(T, NewAcc).

send_invite(From, Idx) ->
   case get(auth_req) of
       nil -> ok;
       Uid -> {ok, Email} = passport:uid_to_email(Uid),
              Name = hn_util:extract_name_from_email(From),
              CC = Email,
              Site = ?msite,
              Data = [{emailed, true}, {idx, Idx}],
              Path = ["_authorize", Name],
              Link = passport:create_hypertag_url(Site, Path, Uid, From,
                                                  Data, "never"),
              Subject = "Administrative Email for " ++ Site,
              EmailBody = "Dear " ++ Name
                  ++ ",\n\n"
                  ++ CC ++ " wants to make you the administrative e-mail for "
                  ++ "the website " ++ Site ++ "."
                  ++ "\n\n"
                  ++ "If you are happy to do this please follow this link:\n"
                  ++ Link
                  ++ "\n\n"
                  ++ ?MASTER_SIG,
              % yeah - send it to the From email 'cos after
              % this that's the email the emails is gonnae be from
              % init...
              emailer:send_email(From, CC, ?MASTER_EMAIL, Subject, EmailBody)
   end.

get_voice(0) -> "woman";
get_voice(1) -> "man";
get_voice(_) -> ?ERR_VAL.

get_lang(0) -> "en-gb";
get_lang(1) -> "en";
get_lang(2) -> "es";
get_lang(3) -> "fr";
get_lang(4) -> "de";
get_lang(_) -> ?ERR_VAL.

get_title(Text) when is_list(Text) ->
    Len = length(Text),
    if
        Len >  40 -> {Tit, _} = lists:split(40, Text),
                     Tit;
        Len =< 40 -> Text
    end.
