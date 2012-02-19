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
         get_phone/3,
         handle_call/2,
         handle_phone_post/3
        ]).

get_phone(#refX{site = _S}, #phone{capability = [{manual_sms, PhoneNo}]} = P, _Uid) ->
    {struct, lists:flatten([{"phoneno", PhoneNo},
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
    % so we sign the hypertab with the URL that the request will come in on
    IncomingPath = ["_services", "phone"],
    HyperTag = passport:create_hypertag(S, IncomingPath, Uid, Email, P, Age),
    {struct, lists:flatten([{"phonetoken", binary_to_list(Token)},
                            {hypertag, HyperTag},
                            P#phone.softphone_config,
                            P#phone.softphone_type])}.

handle_phone_post(#refX{} = Ref, Phone, #env{body = Body}) ->
    case Body of
        [{"manual_sms", {struct, Args}}] ->
            case handle_sms(Ref, Phone, Args) of
                {ok, ok}   -> {ok, 200};
                {error, N} -> {error, N}
            end;
        _  -> {error, 401}
    end.

handle_sms(Ref, Phone, Args) ->
    #phone{capability = [{manual_sms, P}]} = Phone,
    % check that the phone no in the request matches that
    % in the capability
    case proplists:lookup("phoneno", Args) of
        none             -> {error, 401};
        {"phoneno", P}   -> {"msg", Msg} = proplists:lookup("msg", Args),
                           #refX{site = S} = Ref,
                           AC = contact_utils:get_twilio_account(S),
                           contact_utils:post_sms(AC, P, Msg);
        % somebody is spoofing the phone no, erk!
        {"phoneno", _P2} -> {error, 401}
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
    {ok, _Uid, _EMail, Phone, _, _} = HT,
    case AppSID of
        LocalAppSID -> Phone#phone.twiml;
        _Other      -> error
    end;
handle_c2(_Ref, #twilio{called = null,
                        caller = null,
                        from = null,
                        to = null,
                  call_duration = #twilio_duration{} = CD}) ->
    io:format("Call Duration is ~p~n", [CD]),
    {ok, 200}.
