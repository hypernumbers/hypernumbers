%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       This module handles Twilio input for hn_mochi
%%%
%%% @end
%%% Created : 10 Feb 2012 by gordon@hypernumbers.com

-module(hn_twilio_mochi).

-include("hn_mochi.hrl").
-include("spriki.hrl").
-include("keyvalues.hrl").
-include("twilio_web.hrl").

% call handling
-export([
         redir/1,
         handle_call/2,
         handle_phone_post/3
        ]).

% setup the softphone
-export([
         get_phone/3
        ]).

% display the recordings in the brower
-export([
         view_recording/2
        ]).

% utilities
-export([
         log/2
        ]).

% debug
-export([
         handle_c2_DEBUG/2
        ]).

get_phone(#refX{site = _S},
          #phone{capability = [{manual_email, _To, _Fr, _CC, _Su, _Cn}]} = P,
          _Uid) ->
    {struct, lists:flatten([P#phone.softphone_config,
                            P#phone.softphone_type])};
get_phone(#refX{site = _S},
          #phone{capability = [{manual_sms, PhoneNo, Msg}]} = P,
          _Uid) ->
    {struct, lists:flatten([{"phoneno", PhoneNo},
                            {"msg", Msg},
                            P#phone.softphone_config,
                            P#phone.softphone_type])};
get_phone(#refX{site = S},
          #phone{capability = [{client_outgoing, _, _}] = C} = P, Uid) ->
    HyperTag = get_hypertag(S, P, Uid),
    Token = get_phonetoken(S, C),
    {struct, lists:flatten([{"phonetoken", Token},
                            {"hypertag", HyperTag},
                            {"site", S},
                            P#phone.softphone_config,
                            P#phone.softphone_type])};
get_phone(#refX{site = S},
          #phone{capability = [{client_incoming, Ext}] = C} = P, Uid) ->
    HyperTag = get_hypertag(S, P, Uid),
    Token = get_phonetoken(S, C),
    {struct, lists:flatten([{"phonetoken", Token},
                            {"hypertag", HyperTag},
                            {"ext", Ext},
                            P#phone.softphone_config,
                            P#phone.softphone_type])}.

view_recording(#refX{site = S, path = P}, HyperTag) ->
    HT = passport:open_hypertag(S, P, HyperTag),
    {ok, _, _, Call_SID, _, _} = HT,
    AC = contact_utils:get_twilio_account(S),
    {ok, Details} = contact_utils:get_recording_details(AC, Call_SID),
    {struct, D2} = mochijson:decode(Details),
    case proplists:lookup("recordings", D2) of
        {"recordings", {array, [{struct, D3}]}} ->
            {"uri", URI} = proplists:lookup("uri", D3),
            URI2 = re:replace(URI, ".json$", ".mp3", [global, {return, list}]), %"
                              {redir, "https://api.twilio.com" ++ URI2};
                              {"recordings", {array, []}} ->
                                     no_recording
                             end.

handle_phone_post(#refX{site = S} = Ref, Phone, #env{body = Body, uid = Uid}) ->
    case Body of
        [{"manual_sms", {struct, Args}}] ->
            Log = Phone#phone.log,
            {ok, Email} = passport:uid_to_email(Uid),
            case handle_sms(Ref, Phone, Args) of
                {ok, ok}   ->
                    log(S, Log#contact_log{from = Email, status = "ok"}),
                    {ok, 200};
                {error, N} ->
                    log(S, Log#contact_log{from = Email, status = "failed"}),
                    {error, N}
            end;
        [{"manual_email", {struct, Args}}] ->
            ok = handle_email(Phone, Args),
            {ok, Email} = passport:uid_to_email(Uid),
            Log = Phone#phone.log,
            log(S, Log#contact_log{from = Email}),
            {ok, 200};
        _  -> {error, 401}
    end.

handle_sms(Ref, Phone, Args) ->
    #phone{capability = [{manual_sms, P, Msg}]} = Phone,
    % check that the phone no and msg in the request matches those
    % in the capability
    case validate([{"msg", Msg}, {"phoneno", P}], Args) of
        false -> {error, 401};
        true  -> #refX{site = S} = Ref,
                 AC = contact_utils:get_twilio_account(S),
                 contact_utils:post_sms(AC, P, Msg)
    end.

handle_email(Phone, Args) ->
    #phone{capability = Capability} = Phone,
    [{_, To, Fr, CC, Su, Cn}] = Capability,
    Caps = [{"to", To},
            {"reply_to", Fr},
            {"cc", CC},
            {"subject", Su},
            {"contents", Cn}],
    % check that the phone no and msg in the request matches those
    % in the capability
    case validate(Caps, Args) of
        false -> {error, 401};
        true  -> ok = hn_net_util:email(To, CC, Fr, Su, Cn)
    end.

redir(#env{} = Env) ->
    Body = process_env(Env),
    #twilio{call_sid = Sid, custom_params = CP} = Body,
    Type = twilio_web_util:get_type(Body),
    Redir = case Type of
                "start outbound" ->
                    {"site", Site} = proplists:lookup("site", CP),
                    Re = mochiweb_util:unquote(Site) ++ "/_services/phone/",
                    ok = phoneredir_srv:add_redir(Sid, Re),
                    Re;
                "call completed" ->
                    phoneredir_srv:last_call(Sid);
                _ ->
                    phoneredir_srv:get_redir(Sid)
            end,
    NewRedir = full_redir(Redir, Env),
    {"Location", NewRedir}.

handle_call(#refX{} = Ref, #env{} = Env) ->
    Body = process_env(Env),
    Type = twilio_web_util:get_type(Body),
    twilio_ext:log_terms(Body, "twilio.params.log"),
    handle_c2(Ref, Type, Body).

% this function head is for an outbound call
% twilio is calling back to get the details that the user has enabled
handle_c2(#refX{site = S, path = P}, "start outbound" = Type, Body) ->
    io:format("starting outbound call~n"),
    AC = contact_utils:get_twilio_account(S),
    #twilio_account{application_sid = LocalAppSID} = AC,
    #twilio{application_sid = AppSID, custom_params = CP} = Body,
    {"hypertag", HyperTag} = proplists:lookup("hypertag", CP),
    HT = passport:open_hypertag(S, P, HyperTag),
    {ok, Uid, EMail, [Idx, OrigEmail], _, _} = HT,
    XRefX = new_db_api:idx_to_xrefX(S, Idx),
    #xrefX{path = OrigP} = XRefX,
    OrigRef = hn_util:xrefX_to_refX(XRefX),
    [Phone] = new_db_api:get_phone(OrigRef),
    Log = Phone#phone.log,
    Data = Body#twilio.call_sid,
    % gonnae build the hypertag with the original refX so we can check it
    % for security
    OrigP2 = lists:append(OrigP, ["_contacts"]),
    RecHyperTag = passport:create_hypertag(S, OrigP2, Uid, EMail,
                                           Data, "never"),
    Url = hn_util:refX_to_url(OrigRef#refX{path = OrigP2, obj = {page, "/"}}),
    Link = "<a href='" ++ Url ++ "?view=recording&play="
        ++ RecHyperTag ++ "' target='recording'>Recording</a>",
    Log2 = Log#contact_log{reference = Link, from = OrigEmail},
    case AppSID of
        LocalAppSID ->
            log(S, Log2),
            phonecall_sup:init_call(S, Type, Body, Phone#phone.twiml, []);
        _Other ->
            error
    end;

% handle inbound calls coming in from cold
handle_c2(#refX{site = S}, "start inbound" = Type, Recs) ->
    io:format("> CALL STATUS: phone ringing...~n"),
    Callbacks = get_callbacks(S, Type),
    TwiML_ext = contact_utils:get_phone_menu(S),
    phonecall_sup:init_call(S, Type, Recs, TwiML_ext, Callbacks);
% handle the recording message being sent prior to hangup
handle_c2(#refX{site = S}, Type, Recs) when Type == "recording notification"
orelse Type == "in-progress recording notification" ->
    io:format("> CALL STATUS: being notified of recording...~n"),
    % twilio_web_util:pretty_print(Tw),
    ok = phonecall_sup:recording_notification(S, Recs),
    {ok, 200};
% handle another (?) inprogress bit of a call
handle_c2(Ref, "in progress", Recs) ->
    io:format("> CALL STATUS: Call back on ~p~n", [Ref#refX.path]),
    % twilio_web_util:pretty_print(Recs),
    Path = "dindy",
    exit("fix me up in handle_c2 (2)"),
    case Path of
        [] ->
            io:format("response to gather...~n"),
            phonecall_sup:gather_response(Recs);
        [State | _] ->
            io:format("return to state ~p~n", [State]),
            phonecall_sup:goto_state(Recs, State)
    end;
% handle call complete message when a call terminates
handle_c2(#refX{site = S, path = Path}, "call completed", Recs) ->
    io:format("> CALL STATUS: call completed...~n"),
    % twilio_web_util:pretty_print(Recs),
    case Path of
        ["_services", "phone"] ->
            ok = phonecall_sup:call_complete(S, Recs),
            {ok, 200};
        ["_services", "phone", _Sub] ->
            % do nothing here
            {ok, 200}
    end;
handle_c2(Ref, Type, Recs) ->
    io:format("in unhandled twilio callback ~p for ~p...~n",
              [Type, Ref#refX.path]),
    twilio_web_util:pretty_print(Recs),
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response>"
        ++ "<Say>Debugging message. Something has gone wrong, folks."
        ++ "Unhandled Twilio callback...</Say></Response>".

validate([], _Args) ->
    true;
validate([{K, V} | T], Args) ->
    case proplists:lookup(K, Args) of
        none     -> false;
        {K, V}   -> validate(T, Args);
        % somebody is spoofing client-side, erk!
        {K, _V2} -> false
    end.

get_callbacks(Site, Type) ->
    Now = now(),
    YY = dh_date:format("Y", Now),
    MM = dh_date:format("m", Now),
    DD = dh_date:format("d", Now),
    P = case Type of
            "start inbound"  -> ["_audit", "calls", "inbound", YY, MM, DD];
            "start outbound" -> ["_audit", "calls", "outbound", YY, MM, DD]
        end,
    RefX = #refX{site = Site, path = P, obj = {page, "/"}},
    C = fun(_Rec, State) ->
                io:format("Log on completion~n"),
                Log = phonecall_srv:get_log(State),
                Log2 = Log#contact_log{status = "completed"},
                write_log(RefX, Log2)
        end,
    R = fun(Rec, State) ->
                io:format("Log on recording~n"),
                Log = phonecall_srv:get_log(State),
                R = twilio_web_util:get_recording(Rec),
                Log2 = Log#contact_log{status = "recording", reference = R},
                write_log(RefX, Log2)
        end,
    [{completion, C}, {recording, R}].

log(Site, #contact_log{} = Log) ->
    #contact_log{idx = Idx} = Log,
    XRefX = new_db_api:idx_to_xrefX(Site, Idx),
    RefX = hn_util:xrefX_to_refX(XRefX),
    write_log(RefX, Log).

write_log(#refX{path = P} = RefX, Log) ->
    io:format("Logging ~p~n- to ~p~n", [Log, RefX]),
    P2 = lists:append(P, ["_contacts"]),
    RefX2 = RefX#refX{type = gurl, path = P2, obj = {row, {1, 1}}},
    Array = [
             {struct, [{"label", "type"},
                       {"formula", Log#contact_log.type}]},
             {struct, [{"label", "call_sid"},
                       {"formula", Log#contact_log.call_sid}]},
             {struct, [{"label", "from"},
                       {"formula", Log#contact_log.from}]},
             {struct, [{"label", "reply_to"},
                       {"formula", Log#contact_log.reply_to}]},
             {struct, [{"label", "to"},
                       {"formula", Log#contact_log.to}]},
             {struct, [{"label", "cc"},
                       {"formula", Log#contact_log.cc}]},
             {struct, [{"label", "subject"},
                       {"formula", Log#contact_log.subject}]},
             {struct, [{"label", "contents"},
                       {"formula", Log#contact_log.contents}]},
             {struct, [{"label", "reference"},
                       {"formula", Log#contact_log.reference}]},
             {struct, [{"label", "status"},
                       {"formula", Log#contact_log.status}]}
            ],
    new_db_api:handle_form_post(RefX2, Array, nil).

% debugging interface
handle_c2_DEBUG(Body, Site) ->
    Type = twilio_web_util:get_type(Body),
    Ref = #refX{site = Site, path = ["_services", "phone"]},
    handle_c2(Ref, Type, Body).

process_env(Env) ->
    #env{mochi = Mochi} = Env,
    Qs = Mochi:parse_qs(),
    twilio_web_util:process_query(Qs).

full_redir(Redir, Env) ->
    #env{mochi = Mochi} = Env,
    Old = Mochi:get(raw_path),
    [_, Path] = string:tokens(Old, "?"),
    Redir ++ "?" ++ Path.

get_phonetoken(Site, Capability) ->
    AC = contact_utils:get_twilio_account(Site),
    #twilio_account{account_sid = AccSID, auth_token = AuthToken} = AC,
    Tok = twilio_capabilities:generate(AccSID, AuthToken, Capability,
                                 [{expires_after, 7200}]),
    binary_to_list(Tok).

get_hypertag(Site, Phone, Uid) ->
    {ok, Email} = passport:uid_to_email(Uid),
    Age = 7200, % in seconds
    % uses the path as an encryption key - when this hypertag is returned
    % by twilio, twilio needs to get the originating URL from the data
    % so we sign the hypertag with the URL that the request will come in on
    IncomingPath = ["_services", "phone"],
    % now add the current user into the phone log
    {ok, Email} = passport:uid_to_email(Uid),
    #phone{idx = Idx} = Phone,
    P2 = [Idx, Email],
    passport:create_hypertag(Site, IncomingPath, Uid, Email, P2, Age).

