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
         handle_c2_DEBUG/1
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
          #phone{capability = [{client_outgoing, _, _}] = C} = P,
          Uid) ->
    AC = contact_utils:get_twilio_account(S),
    #twilio_account{account_sid = AccSID, auth_token = AuthToken} = AC,
    Token = twilio_capabilities:generate(AccSID, AuthToken, C,
                                         [{expires_after, 7200}]),
    {ok, Email} = passport:uid_to_email(Uid),
    Age = 7200, % in seconds
    % uses the path as an encryption key - when this hypertag is returned
    % by twilio, twilio needs to get the originating URL from the data
    % so we sign the hypertag with the URL that the request will come in on
    IncomingPath = ["_services", "phone"],
    % now add the current user into the phone log
    {ok, Email} = passport:uid_to_email(Uid),
    #phone{idx = Idx} = P,
    P2 = [Idx, Email],
    HyperTag = passport:create_hypertag(S, IncomingPath, Uid, Email, P2, Age),
    {struct, lists:flatten([{"phonetoken", binary_to_list(Token)},
                            {hypertag, HyperTag},
                            {site, S},
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
    Body = twilio_web_util:process_body(Env#env.body),
    #twilio{custom_params = CP} = Body,
    {"site", Site} = proplists:lookup("site", CP),
    Redir = mochiweb_util:unquote(Site) ++ "/_services/phone/",
    {"Location", Redir}.

handle_call(#refX{} = Ref, #env{} = Env) ->
    Body = twilio_web_util:process_body(Env#env.body),
    twilio_ext:log_terms(Body, "twilio.params.log"),
    handle_c2(Ref, Body).

% this function head is for an outbound call
% twilio is calling back to get the details that the user has enabled
handle_c2(#refX{site = S, path = P},
          #twilio{called = null,
                  caller = null,
                  from = null,
                  to = null,
                  call_duration = null} = Body) ->
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
        LocalAppSID -> log(S, Log2),
                       phonecall_sup:init_call(S, Body, Phone#phone.twiml);
        _Other      -> error
    end;

% handle inbound calls coming in from cold
handle_c2(#refX{site = S}, #twilio{direction = "inbound",
                                   call_status = "ringing"} = Recs) ->
    io:format("phone ringing...~n"),
    TwiML_ext = contact_utils:get_phone_menu(S),
    phonecall_sup:init_call(S, Recs, TwiML_ext);

% handle the recording message being sent prior to hangup
handle_c2(_Ref, #twilio{call_status = "completed",
                        recording = #twilio_recording{}} = Recs) ->
    % twilio_web_util:pretty_print(Tw),
    Path = "balderdash",
    exit("fix me in c2 (3)"),
    ok = phonecall_sup:recording_notification(Recs, Path),
    {ok, 200};

% handle another (?) inprogress bit of a call
handle_c2(Ref, #twilio{direction = "inbound",
                       call_status = "in-progress"} = Recs) ->
    io:format("Call back on ~p~n", [Ref#refX.path]),
    twilio_web_util:pretty_print(Recs),
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
handle_c2(#refX{site = S, path = Path},
          #twilio{call_status = "completed", recording = null} = Recs) ->
    twilio_web_util:pretty_print(Recs),
    case Path of
        [] ->
            ok = phonecall_sup:call_complete(S, Recs),
            {ok, 200};
        _Sub ->
            % do nothing here
            {ok, 200}
    end;
handle_c2(Ref, Recs) ->
    io:format("in unhandled twilio callback for ~p...~n", [Ref#refX.path]),
    twilio_web_util:pretty_print(Recs),
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response>"
        ++ "<Say>something has gone wrong, folks.</Say></Response>".

validate([], _Args) ->
    true;
validate([{K, V} | T], Args) ->
    case proplists:lookup(K, Args) of
        none     -> false;
        {K, V}   -> validate(T, Args);
        % somebody is spoofing client-side, erk!
        {K, _V2} -> false
    end.

log(Site, #contact_log{} = Log) ->
    #contact_log{idx = Idx} = Log,
    XRefX = new_db_api:idx_to_xrefX(Site, Idx),
    #xrefX{path = P} = XRefX,
    P2 = lists:append(P, ["_contacts"]),
    RefX = hn_util:xrefX_to_refX(XRefX),
    RefX2 = RefX#refX{type = gurl, path = P2, obj = {row, {1, 1}}},
    Array = [
             {struct, [{"label", "type"},
                       {"formula", Log#contact_log.type}]},
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
handle_c2_DEBUG(Body) ->
    Ref = #refX{site = "http://hypernumbers.dev:9000", path = []},
    handle_c2(Ref, Body).
