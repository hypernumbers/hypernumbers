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

-export([
         handle_recording/2,
         log/2,
         get_phone/3,
         handle_call/2,
         handle_phone_post/3
        ]).

get_phone(#refX{site = _S}, #phone{capability = [{manual_sms, PhoneNo, Msg}]} = P,
          _Uid) ->
    {struct, lists:flatten([{"phoneno", PhoneNo},
                            {"msg", Msg},
                            P#phone.softphone_config,
                            P#phone.softphone_type])};
get_phone(#refX{site = S}, #phone{capability = [{client_outgoing, _, _}] = C} = P,
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
    #phone{log = Log} = P,
    Log2 = Log#contact_log{from = Email},
    P2 = P#phone{log = Log2},
    HyperTag = passport:create_hypertag(S, IncomingPath, Uid, Email, P2, Age),
    {struct, lists:flatten([{"phonetoken", binary_to_list(Token)},
                            {hypertag, HyperTag},
                            P#phone.softphone_config,
                            P#phone.softphone_type])}.

handle_recording(#refX{site = S, path = P}, HyperTag) ->
    HT = passport:open_hypertag(S, P, HyperTag),
    {ok, _, _, Call_SID, _, _} = HT,
    AC = contact_utils:get_twilio_account(S),
    {ok, Details} = contact_utils:get_recording_details(AC, Call_SID),
    {struct, D2} = mochijson:decode(Details),
    case proplists:lookup("recordings", D2) of
        {"recordings", {array, [{struct, D3}]}} ->
            {"uri", URI} = proplists:lookup("uri", D3),
            URI2 = re:replace(URI, ".json$", ".mp3", [global, {return, list}]),
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
        _  -> {error, 401}
    end.

handle_sms(Ref, Phone, Args) ->
    #phone{capability = [{manual_sms, P, Msg}]} = Phone,
    % check that the phone no and msg in the request matches those
    % in the capability
    case validate_sms([{"msg", Msg}, {"phoneno", P}], Args) of
        false -> {error, 401};
        true  -> #refX{site = S} = Ref,
                 AC = contact_utils:get_twilio_account(S),
                 contact_utils:post_sms(AC, P, Msg)
    end.

handle_call(#refX{} = Ref, #env{} = Env) ->
    Body = twilio_web_util:process_body(Env#env.body),
    handle_c2(Ref, Body).

handle_c2(#refX{site = S, path = P},
          #twilio{called = null,
                  caller = null,
                  from = null,
                  to = null,
                  call_duration = null} = Body) ->
    AC = contact_utils:get_twilio_account(S),
    #twilio_account{application_sid = LocalAppSID} = AC,
    #twilio{application_sid = AppSID, custom_params = CP} = Body,
    {"hypertag", HyperTag} = proplists:lookup("hypertag", CP),
    HT = passport:open_hypertag(S, P, HyperTag),
    {ok, Uid, EMail, Phone, _, _} = HT,
    Log = Phone#phone.log,
    Data = Body#twilio.call_sid,
    % gonnae build the hypertag with the original refX so we can check it
    % for security
    XRefX = new_db_api:idx_to_xrefX(S, Phone#phone.idx),
    #xrefX{path = OrigP} = XRefX,
    OrigRef = hn_util:xrefX_to_refX(XRefX),
    OrigP2 = lists:merge(OrigP, ["contacts"]),
    RecHyperTag = passport:create_hypertag(S, OrigP2, Uid, EMail, Data, "never"),
    Url = hn_util:refX_to_url(OrigRef#refX{path = OrigP2, obj = {page, "/"}}),
    Link = "<a href='" ++ Url ++ "?view=recording&play="
        ++ RecHyperTag ++ "' target='recording'>Recording</a>",
    Log2 = Log#contact_log{reference = Link},
    case AppSID of
        LocalAppSID -> log(S, Log2),
                       Phone#phone.twiml;
        _Other      -> error
    end;
handle_c2(_Ref, #twilio{called = null,
                        caller = null,
                        from = null,
                        to = null,
                  call_duration = #twilio_duration{}} = Tw) ->
    io:format("Tw is ~p~n", [Tw]),
    {ok, 200}.

validate_sms([], _Args) ->
    true;
validate_sms([{K, V} | T], Args) ->
    case proplists:lookup(K, Args) of
        none     -> false;
        {K, V}   -> validate_sms(T, Args);
        % somebody is spoofing client-side, erk!
        {K, _V2} -> false
    end.

log(Site, #contact_log{} = Log) ->
    #contact_log{idx = Idx} = Log,
    XRefX = new_db_api:idx_to_xrefX(Site, Idx),
    #xrefX{path = P} = XRefX,
    P2 = lists:merge(P, ["contacts"]),
    RefX = hn_util:xrefX_to_refX(XRefX),
    RefX2 = RefX#refX{type = gurl, path = P2, obj = {row, {1, 1}}},
    Array = [
             {struct, [{"label", "type"},
                       {"formula", Log#contact_log.type}]},
             {struct, [{"label", "from"},
                       {"formula", Log#contact_log.from}]},
             {struct, [{"label", "to"},
                       {"formula", Log#contact_log.to}]},
             {struct, [{"label", "cc"},
                       {"formula", Log#contact_log.cc}]},
             {struct, [{"label", "bcc"},
                       {"formula", Log#contact_log.bcc}]},
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

