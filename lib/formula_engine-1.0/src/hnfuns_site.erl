%%% @author    Gordon Guthrie
%%% @copyright (C) 2012-2014, Hypernumbers Ltd (Vixo)
%%% @doc       Functions for administering the site
%%%
%%% @end
%%% Created : 21 Apr 2012 by gordon@vixo.com

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

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
         %% 'manage.api.keys.'/1,
         'invite.users'/1,
         'users.and.groups.'/1,
         'configure.email'/1,
         'phone.menu.'/1,
         'phone.menu.say'/1,
         'phone.menu.play'/1,
         'phone.menu.input'/1,
         'phone.menu.record'/1,
         'phone.menu.transcribe'/1,
         'phone.menu.phoneno'/1,
         'phone.menu.extension'/1,
         'phone.menu.conference'/1,
         'phone.menu.dial'/1,
         'phone.menu.sms'/1,
         'google.analytics'/1
        ]).

'manage.api.keys.'([H]) ->
    'manage.api.keys.'([H, 0]);
'manage.api.keys.'([H, Type]) ->
    [H2, Type2] = typechecks:throw_std_ints([H, Type]),
    ok = typechecks:in_range(Type2, 0, 1),
    m_a_k(H2, Type2).

m_a_k(H, Type) ->
    APIJson = get_api_json(),
    {Ctrl, Attr}
        = case Type of
              1 -> {"", "false"};
              0 -> {#form{id = {'api-keys', "Edit"}, kind = "api-keys"}, "true"}
          end,
    HTML = "<div class='hn_user_admin'>"
        ++ "<div class='hn_overflow2'>"
        ++ "<div class='hn_site_admin_top'>Manage API Keys</div>"
        ++ "<div class='hn_api_management' "
        ++ "data-json='" ++ APIJson ++ "' "
        ++ "data-api-enabled=" ++ Attr ++ " ></div>"
        ++ "</div>"
        ++ "</div>",
    Resize = #resize{width = 6, height = H},
    Preview = get_m_a_k_preview(Type),
    JS = ["/webcomponents/hn.apikeys.js"],
    Reload = ["HN.APIKeys.reload_apikeys();"],
    Incs = #incs{js= JS, js_reload = Reload},
    #spec_val{val = lists:flatten(HTML), resize = Resize,
              sp_webcontrol = Ctrl, preview = Preview,
              sp_incs = Incs, sp_site = true}.

get_m_a_k_preview(0) -> "API Key Management";
get_m_a_k_preview(1) -> "Read-Only API Key".

get_api_json() ->
    Site = muin:pd_retrieve(site),
    APIKeys = new_db_api:get_api_keys(Site),
    Len = length(APIKeys),
    Indices = lists:seq(1, Len),
    List = lists:zip(APIKeys, Indices),
    Struct = {array, [make_j(X, N) || {X, N} <- List]},
    JSON = (mochijson:encoder([{input_encoding, utf8}]))(Struct),
    lists:flatten(JSON).

make_j(#api{publickey = PubK, privatekey = PrivK,
            notes = Nts, urls = URLs}, N) ->
    {struct, [
              {"number", N},
              {"publickey", PubK},
              {"privatekey", PrivK},
              {"notes", Nts},
              {"urls", {array, make_j2(URLs, [])}}
             ]}.

make_j2([], Acc) ->
    lists:reverse(Acc);
make_j2([H | T], Acc) ->
    #api_url{path = P, admin = Ad, include_subs = IncS, append_only = AppO} = H,
    NewA = {struct, [
                     {"path", P},
                     {"admin", Ad},
                     {"include_subs", IncS},
                     {"append_only", AppO}
                     ]},
    make_j2(T, [NewA | Acc]).

'google.analytics'([Code]) ->
    [C] = typechecks:std_strs([Code]),
    Site = muin:pd_retrieve(site),
    Idx = muin:pd_retrieve(idx),
    ok = new_db_wu:write_kvD(Site, google_analytics, {Idx, C}),
    Preview = "Google Analytics Code: " ++ C,
    #spec_val{val = "", preview = Preview, unique = google_analytics}.

'invite.users'([]) ->
    Id     = "id_" ++ muin_util:create_name(),
    HTML = "<div class='hn_invite_admin' id='" ++ Id ++ "'>"
        ++ "<div class='hn_site_admin_top'>Invite Users</div>"
        ++ "<p>Choose some groups for your new user. An invitation e-mail will "
        ++ "be sent out.</p>"
        ++ "<input class='hn_emailforgroup' name='emailforgroup' type='text' "
        ++ "value='workmate@example.com' />"
        ++ "<div class='floatleft'>"
        ++ "<div class='hn_advuserstable'><select class='hn_sel_groups' "
        ++ "name='groups' "
        ++ "multiple='multiple'>"
        ++ lists:flatten(["<option>" ++ X ++ "</option>"
                          || X <- lists:sort(new_db_wu:all_groupsD(muin:pd_retrieve(site))),
                             X /= "admin"])
        ++ "</select></div>"
        ++ "</div>"
        ++ "<textarea class='hn_newusermsg'>Dear workmate, "
        ++ "please take a look at this page...</textarea>"
        ++ "<div>Where do you want the user to go?</div>"
        ++ "<input class='hn_invite_path' value='"
        ++ hn_util:list_to_path(muin:pd_retrieve(path)) ++ "' />"
        ++ "<input class='hn_addnewuserbutton button' "
        ++ "value='Create User' type='submit' data-id='" ++ Id ++ "'>"
        ++ "<p class='hn_newuserfeedback'>&nbsp;</p>"
        ++ "</div>",
    JS = ["/webcomponents/hn.usersandgroups.js"],
    Reload = ["HN.UsersAndGroups.reload_invite_user();"],
    Incs = #incs{js= JS, js_reload = Reload},
    Resize = #resize{width = 4, height = 20},
    Preview = "Invite Users",
    Control = #form{id = {'invite-user', "Edit"},
                    kind = "invite-user",
                    callback = {actions_hnfuns, action}},
    #spec_val{val = HTML, resize = Resize, sp_webcontrol = Control,
              sp_incs = Incs, preview = Preview, sp_site = true}.

% Options
% * 0 is read-only arranged by groups
% * 1 is read-only arranged by users
% * 2 is add and delete users from existing groups
'users.and.groups.'([W, H]) ->
    'users.and.groups.'([W, H, 0]);
'users.and.groups.'([W, H, Type]) ->
    [W2, H2, Type2] = typechecks:throw_std_ints([W, H, Type]),
    ok = typechecks:in_range(Type2, 0, 2),
    u_and_g(W2, H2, Type2).

u_and_g(W, H, Type) ->
    {Gs, Hd} = get_u_and_g_options(Type),
    HTML = "<div class='hn_user_admin'>"
        ++ "<div class='hn_site_admin_top'>Users And Groups</div>"
        ++ "<div class='hn_overflow2'>"
        ++ "<table>"
        ++ "<tr>"
        ++ Hd
        ++ "</tr>"
        ++ get_row(Gs, Type)
        ++ "</table>"
        ++ "</div>"
        ++ "</div>",
    Resize = #resize{width = W, height = H},
    Preview = get_u_and_g_preview(Type),
    case Type of
        N when N == 0 orelse N == 1 ->
            #spec_val{val = lists:flatten(HTML), resize = Resize,
                      preview = Preview, sp_site = true};
        N when N == 2 ->
            JS = ["/webcomponents/hn.usersandgroups.js"],
            Reload = ["HN.UsersAndGroups.reload_u_and_g();"],
            Incs = #incs{js= JS, js_reload = Reload},
            Control = #form{id = {'users-and-groups', "Edit"},
                            kind = "users-and-groups",
                            callback = {actions_hnfuns, run_actions}},
            #spec_val{val = lists:flatten(HTML), resize = Resize,
                      sp_webcontrol = Control, preview = Preview,
                      sp_incs = Incs, sp_site = true}
    end.

'phone.menu.'([W, H | List]) ->
    ?check_paid(fun 'phone.menu2'/2, [W, H, List], inbound).

'phone.menu2'([W, H, List], _Acnt) ->
    Site = muin:pd_retrieve(site),
    Idx = muin:pd_retrieve(idx),
    V = new_db_wu:read_kvD(Site, site_phone_menu),
    OrigIdx = case V of
                  []                                           -> null;
                  [{kvstore, site_phone_menu, {OldIdx, null}}] -> OldIdx
              end,
    case OrigIdx of
        I when I == Idx orelse I == null orelse I == deleted ->
            [W2, H2] = typechecks:throw_std_ints([W, H]),
            TwiML = collect(List),
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

'phone.menu.sms'([Text, Number]) ->
    'phone.menu.sms'([Text, Number, ""]);
'phone.menu.sms'([Text, Number, Prefix]) ->
    {Prefix2, PhNo2} = typechecks:std_phone_no(Prefix, Number),
    [Text2] = typechecks:std_strs([Text]),
    Text3 = contact_utils:rightsize(Text2, ?SMSLength),
    From = contact_utils:get_site_phone_no(?msite),
    Twiml = [#sms{from = From, to = "+" ++ Prefix2 ++ PhNo2, text = Text3}],
    Resize = #resize{width = 2, height = 2},
    Preview = "Send SMS to (+" ++ Prefix2 ++ ") " ++ PhNo2 ++ " "
        ++ contact_utils:rightsize(Text3, 10),
    #spec_val{val = "", preview = Preview, resize = Resize,
              sp_phone = #phone{twiml = Twiml}}.

'phone.menu.dial'(List) ->
    Dial = collect(List),
    Twiml = [#dial{body = Dial, record = true}],
    case twiml:is_valid(Twiml) of
        false -> ?ERRVAL_VAL;
        true  -> Preview = "Dial",
                 Resize = #resize{width = 2, height = 2},
                 #spec_val{val = "", preview = Preview, resize = Resize,
                           sp_phone = #phone{twiml = Twiml}}
    end.

'phone.menu.extension'([]) ->
    'phone.menu.extension'(["Default"]);
'phone.menu.extension'([Name]) ->
    [Name2] = typechecks:std_strs([Name]),
    Twiml = [#client{client = Name2}],
    % this Twiml isn't valid on its loneo!
    Preview = "EXTENSION: " ++ Name2,
    Resize = #resize{width = 2, height = 2},
    #spec_val{val = "", preview = Preview, resize = Resize,
              sp_phone = #phone{twiml = Twiml}}.

'phone.menu.conference'([]) ->
    'phone.menu.conference'(["Default"]);
'phone.menu.conference'([Name]) ->
    'phone.menu.conference'([Name, true]);
'phone.menu.conference'([Name, Beep]) ->
    'phone.menu.conference'([Name, Beep, 40]);
'phone.menu.conference'([Name, Beep, MaxUsers]) ->
    'phone.menu.conference'([Name, Beep, MaxUsers, false]);
'phone.menu.conference'([Name, Beep, MaxUsers, StartOnEnter]) ->
    'phone.menu.conference'([Name, Beep, MaxUsers, StartOnEnter, false]);
'phone.menu.conference'([Name, Beep, MaxUsers, StartOnEnter, EndOnExit]) ->
    'phone.menu.conference'([Name, Beep, MaxUsers, StartOnEnter,
                             EndOnExit, false]);
'phone.menu.conference'([Name, Beep, MaxUsers, StartOnEnter,
                         EndOnExit, Muted]) ->
    'phone.menu.conference'([Name, Beep, MaxUsers, StartOnEnter,
                         EndOnExit, Muted, none]);
'phone.menu.conference'([Name, Beep, MaxUsers, StartOnEnter,
                         EndOnExit, Muted, URL]) ->
    [Name2] = typechecks:std_strs([Name]),
    [Beep2] = typechecks:std_bools([Beep]),
    [Max2]  = typechecks:std_ints([MaxUsers]),
    ok = typechecks:in_range(Max2, 1, 40),
    [Start2, End2, Muted2] = typechecks:std_bools([StartOnEnter,
                                                   EndOnExit, Muted]),
    Twiml = case URL of
                none -> [#conference{conference = Name, beep = Beep2,
                                     maxParticipants = Max2,
                                     startConferenceOnEnter = Start2,
                                     endConferenceOnExit = End2,
                                     waitMethod = "GET",
                                     muted = Muted2, waitUrl = ""}];
                _    -> [#conference{conference = Name, beep = Beep2,
                                     maxParticipants = Max2,
                                     startConferenceOnEnter = Start2,
                                     endConferenceOnExit = End2,
                                     waitMethod = "GET",
                                     muted = Muted2, waitUrl = URL}]
            end,
    % this Twiml isn't valid on its loneo!
    Preview = "CONFERENCE: " ++ Name2,
    Resize = #resize{width = 2, height = 2},
    #spec_val{val = "", preview = Preview, resize = Resize,
              sp_phone = #phone{twiml = Twiml}}.

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

'phone.menu.transcribe'([]) ->
    'phone.menu.transcribe'(["Please leave a message after the tone"]);
'phone.menu.transcribe'([Text]) ->
    'phone.menu.transcribe'([Text, true]);
'phone.menu.transcribe'([Text, PlayBeep]) ->
    'phone.menu.transcribe'([Text, PlayBeep, ?RecordingLen]);
'phone.menu.transcribe'([Text, PlayBeep, RecordingLen]) ->
    'phone.menu.transcribe'([Text, PlayBeep, RecordingLen, 0]);
'phone.menu.transcribe'([Text, PlayBeep, RecordingLen, Voice]) ->
    'phone.menu.transcribe'([Text, PlayBeep, RecordingLen, Voice, 0]);
'phone.menu.transcribe'([Text, PlayBeep, RecordingLen, Voice, Lang]) ->
    'phone.menu.record'([Text, PlayBeep, RecordingLen, true, Voice, Lang]).

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
    Title = contact_utils:rightsize(Text2, 40),
    [PlayBeep2] = typechecks:std_bools([PlayBeep]),
    [RLen2] = typechecks:std_ints([RecordingLen]),
    [Trans2] = typechecks:std_bools([Transcribe]),
    [Voice2, Lang2] = typechecks:std_ints([Voice, Lang]),
    V3 = contact_utils:get_voice(Voice2),
    L3 = contact_utils:get_lang(Lang2),
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
    V3 = contact_utils:get_voice(V2),
    L3 = contact_utils:get_lang(L2),
    Title = contact_utils:rightsize(Text2, 40),
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
    Site = muin:pd_retrieve(site),
    Idx = muin:pd_retrieve(idx),
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

collect(List) -> col(List, []).

col([], Acc) -> lists:flatten(lists:reverse(Acc));
col([#cellref{col = {_, C}, row = {_, R}, path = Path} = H | T], Acc) ->
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
    col(T, NewAcc);
col([#rangeref{tl = {{_, X1}, {_, Y1}},
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
    col(T, NewAcc);
% its an unevaluated function so eval it
col([H | T], Acc) ->
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
    col(T, NewAcc).

send_invite(From, Idx) ->
   case muin:pd_retrieve(auth_req) of
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

invert([], Acc) -> lists:sort(Acc);
invert([{Group, Users} | T], Acc) ->
    NewAcc = inv2(Users, Group, Acc),
    invert(T, NewAcc).

inv2([], _Group, Acc) -> Acc;
inv2([User | T], Group, Acc) ->
    NewAcc = case lists:keyfind(User, 1, Acc) of
                 false ->
                     [{User, [Group]} | Acc];
                  {User, List} ->
                     lists:keyreplace(User, 1, Acc, {User, [Group | List]})
             end,
    inv2(T, Group, NewAcc).

get_row(Groups, N) when N == 0 orelse N == 1 ->
    ["<tr class='hn_user_table'><td>" ++ G ++ "</td><td>"
     ++ string:join(M, "<br />") ++ "</td></tr>" || {G, M} <- Groups];
get_row(Groups, 2) ->
    All = lists:sort(new_db_wu:all_groupsD(muin:pd_retrieve(site))),
    get_r2(Groups, All, []).

get_r2([], _All, Acc) ->
    lists:reverse(Acc);
get_r2([{M, G} | T], All, Acc) ->
    NewAcc = "<tr class='hn_user_table'><td>" ++ M ++ "</td>"
        ++ get_r2a(All, M, G, []),
    get_r2(T, All, [NewAcc | Acc]).

get_r2a([], _M, _G, Acc) ->
    "<td>" ++ lists:reverse(Acc) ++ "</td></tr>";
get_r2a([H | T], M, G, Acc) ->
    Start = "<input type='checkbox' class='hn_user_rem' data-group='"
        ++ H ++ "' " ++ " data-user='" ++ M ++ "'",
    Middle = case {lists:member(H, G), H} of
                 {true,  "admin"} -> " checked='checked' disabled='disabled'";
                 {false, "admin"} -> " disabled='disabled'";
                 {true, _}        -> " checked='checked'";
                 {false, _}       -> ""
             end,
    End = case H of
              "admin" -> "><label><em>" ++ H ++ "</em></label>";
              _       -> "><label>" ++ H ++ "</label>"
                  end,
    get_r2a(T, M, G, [Start ++ Middle ++ End | Acc]).

get_u_and_g_preview(0) -> "Read-Only Users And Groups: ordered by group";
get_u_and_g_preview(1) -> "Read-Only Users And Groups: ordered by user";
get_u_and_g_preview(2) -> "Editable Users And Groups".

get_u_and_g_options(Type) ->
    Site = muin:pd_retrieve(site),
    case Type of
        _N when _N == 0 ->
            {lists:sort(new_db_wu:groupsD(Site)),
             "<td><em>Group</em></td>"
             ++ "<td><em>Users</em></td></td>"};
        _N when _N == 1 orelse _N == 2 ->
            {invert(hn_groups:get_groups(Site), []),
             "<td><em>Users</em></td>"
             ++ "<td><em>Groups</em></td></td>"}
    end.
