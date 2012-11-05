%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       A module for running webcontrol actions
%%%
%%% @end
%%% Created : 16 Oct 2012 by gordon@vixo.com

-module(hn_actions).

-export([
         run_actions/4
        ]).

-include("spriki.hrl").
-include("hn_mochi.hrl").

-define(E, error_logger:error_msg).
-define(S, status_srv).
-define(LOAD, hn_templates:load_template_if_no_page).

run_actions(#refX{site = S} = RefX, _Env, [{Act, {struct, L}}], UID)
  when Act == "invite_user" ->
    [Expected] = new_db_api:matching_forms(RefX, 'users-and-groups'),
    case Expected of
        {form, _, {_, 'invite-user', _}, _, _, _} ->
            {"groups", {array, Groups}} = lists:keyfind("groups", 1, L),
            {"user", Email}    = lists:keyfind("user", 1, L),
            {"msg" , Msg}      = lists:keyfind("msg", 1, L),
            {"path", GotoPath} = lists:keyfind("path", 1, L),
            case lists:member("admin", Groups) of
                true ->
                    404;
                false ->
                    Args = [{"path", GotoPath},
                            {"email", Email},
                            {"msg", Msg},
                            {"groups", {array, Groups}},
                            {"view", none}],
                    case hn_web_admin:rpc(UID, S, "invite_user", Args) of
                        ok ->
                            "success";
                        {error, Reason} ->
                            {"failure", Reason}
                    end
            end;
        _Other ->
            404
    end;
run_actions(#refX{site = S} = RefX, _Env, [{Act, {struct, L}}], _Uid)
  when Act == "add_user" orelse
       Act == "remove_user" ->
    [Expected] = new_db_api:matching_forms(RefX, 'users-and-groups'),
    case Expected of
        {form, _, {_, 'users-and-groups', _}, _, _, _} ->
            {"user", Email}  = lists:keyfind("user", 1, L),
            {"group", Group} = lists:keyfind("group", 1, L),
            case Group of
                "admin" ->
                    404;
                _ ->
                    {ok, U} = passport:email_to_uid(Email),
                    case Act of
                        "add_user" ->
                            new_db_api:add_userD(S, U, Group);
                        "remove_user" ->
                            new_db_api:rem_userD(S, U, Group)
                    end,
                    "success"
            end;
        _ ->
            404
    end;
run_actions(#refX{} = RefX, _Env, [{"phone", {struct, [{"dial", St}]}}], Uid) ->
    case  new_db_api:get_phone(RefX) of
        [] ->
            {"error", "no phone at this url"};
        [Phone] ->
            % check if the user has permission
            case can_phone_outbound(Phone, St) of
                true ->
                    {struct, [{"numbers", {array, Numbers}}]} = St,
                    ok = ?S:update_status(Uid, RefX, "made a phone call"),
                    _Ret = softphone_srv:reg_dial(RefX, Uid, Numbers),
                    "success";
                false ->
                    401
            end
    end;
run_actions(#refX{} = RefX, _Env, [{"phone", Action}], Uid)
  when Action == "hangup" orelse Action == "away" orelse Action == "back" ->
    case new_db_api:get_phone(RefX) of
        [] ->
            {"error", "no phone at this url"};
        [Phone] ->
            % check permissions
            case can_phone_inbound(Phone) of
                true ->
                    case Action of
                        "hangup" -> ok = softphone_srv:idle(RefX, Uid);
                        "away"   -> ok = softphone_srv:away(RefX, Uid);
                        "back"   -> ok = softphone_srv:back(RefX, Uid)
                    end,
                    "success";
                false ->
                    401
            end
    end;
run_actions(#refX{} = RefX, #env{mochi = Mochi} = _Env,
            [{"phone", {struct, [{"register", PhoneId},
                                 {"groups", {array, G}}]}}], Uid) ->
    % make sure the user isn't being spoofed when registering the phone
    case new_db_api:get_phone(RefX) of
        [] ->
            {"error", "no phone at this url"};
        [_Phone] ->
            Socket = Mochi:get(socket),
            % TODO this code doesn't work - it don't
            % detect when a socket goes away
            inet:setopts(Socket, [{active, once}]),
            case softphone_srv:reg_phone(RefX, self(), PhoneId, G, Uid) of
                {phoneid, _PhoneId2} = PhId2 ->
                    % now keep the socket alive
                    receive
                        {tcp_closed, Socket} ->
                            io:format("TCP closed on socket~n"),
                            ok = softphone_srv:unreg_phone(RefX, Uid),
                            {ok, died};
                        {error, timeout}->
                            io:format("First timeout of phone~n"),
                            ok = softphone_srv:unreg_phone(RefX, Uid),
                            {ok, died};
                        {msg, is_available} ->
                            io:format("Phone available~n"),
                            {"phone available", PhId2}
                    after
                        2000 ->
                            ok = softphone_srv:break_phone(RefX, Uid),
                            {"phone available", PhId2}
                    end;
                user_already_registered ->
                    {"error", "user_already_registered"}
            end
    end;
run_actions(#refX{} = RefX, _Env, [{Act, {struct, _L}} = Payload], Uid)
  when Act == "send_sms" orelse Act == "send_email" ->
    case  new_db_api:get_phone(RefX) of
        [] ->
            {"error", "no phone at this url"};
        [Phone] ->
            % GOT TO HERE
            io:format("need to check permissions for sms/email~n"),
            case hn_twilio_mochi:handle_webcontrol_post(RefX, Phone,
                                                        Payload, Uid) of
                {ok, 200}    -> "success";
                {error, 401} -> 401;
                Msg          -> io:format("Twilio Erlang sends odd error "
                                          ++ "messages ~p~n", [Msg]),
                                500
            end
    end;
run_actions(#refX{site = S, path = P} = RefX, Env, [{_, {array, Json}}], Uid) ->
    Fun1 = fun({struct, [{N, {array, Exprs}}]}) ->
                   N2 = list_to_integer(N),
                   {N2, lists:flatten([json_recs:json_to_rec(X)
                                       || X <- Exprs])}
           end,
    Commands = [Fun1(X) || X <- Json],
    Expected = new_db_api:matching_forms(RefX, 'create-button'),
    % now check that the commands coming in match those stored
    case hn_security:validate_create_pages(Expected, Commands) of
        false ->
            ?E("invalid submission~n""on:       ~p~n"
               ++ "Expected: ~p~nGot:      ~p~n",
               [RefX, Expected, Commands]),
            403;
        true ->
            % check that all the templates exists here!
            {Templates, Perms, Dest, Actions}
                = hn_webcontrols:make_actions(S, P, Commands),
            case templates_exist(S, Templates) of
                {error, Err} ->
                    ?E("Templates errors in postcreatepages: ~p~n", [Err]),
                    [{"status", "err"}, {"response", Err}];
                true ->
                    % create the pages
                    Fun2 = fun({Template, Path}) ->
                                   RefX2 = #refX{site = S, path = Path,
                                                 obj = {page, "/"}},
                                   ok = ?LOAD(RefX2, Template)
                           end,
                    [Fun2(X) || X <- Actions],
                    % now run the permissions
                    Fun3 = fun({Path, Ps}) ->
                                   [ok = process_perms(S, Path, View,
                                                       Groups, Uid)
                                    || {View, Groups} <- Ps]
                           end,
                    [Fun3(X) || X <- Perms],
                    {redirect, [{"status", "ok"}, {"redirect", Dest}]}
            end
    end.

%
% Internal fns
%
can_phone_inbound(Phone) ->
    case get_type(Phone, "phone_out") of
        "true"  -> true;
        "false" -> false
    end.

can_phone_outbound(Phone, Dial) ->
    case get_config(Phone, "phone_out_permissions") of
        "none"       -> false;
        "free dial"  -> true;
        "fixed dial" -> case numbers_match(Phone, Dial) of
                            true  -> true;
                            false -> false
                        end
    end.

templates_exist(Site, Templates) ->
    ExistingTemplates = hn_util:get_templates(Site),
    templates_e2(lists:sort(ExistingTemplates), lists:sort(Templates)).

templates_e2(_List, []) ->
    true;
templates_e2([H | T], Templates) ->
    templates_e2(T, lists:delete(H, Templates));
templates_e2([], List) ->
    N = length(List),
    Formats = lists:duplicate(N, " ~s"),
    Fmt = string:join(Formats, ","),
    Msg = lists:flatten(io_lib:format("The following templates "
                                      ++ "do not exist:~n"
                                      ++ Fmt ++ "~n", List)),
    {error, Msg}.

process_perms(Site, Path, V, Gs, Uid) ->
    {ok, Email} = passport:uid_to_email(Uid),
    Gs2 = replace_user(Gs, Email, []),
    auth_srv:add_view(Site, Path, Gs2, V).

replace_user([], _EM, Groups)           -> Groups;
replace_user(["$user" | T], EM, Groups) -> replace_user(T, EM, [EM|  Groups]);
replace_user([H | T], EM, Groups)       -> replace_user(T, EM, [H | Groups]).

numbers_match(Phone, {struct, [{"numbers", {array, L}}]}) ->
    N = get_config(Phone, "phone_no"),
    D = get_config(Phone, "default_dialling_code"),
    % hmm, sometimes I might get multiple numbers in here and
    % might need to check them. Tad hooky, auld fella
    case length(L) of
        1 -> case D ++ N of
                 L -> true;
                 _ -> false
             end;
        _ -> false
    end.

get_config(#phone{softphone_config = {"config", {struct, L}}}, Key) ->
    {Key, P} = lists:keyfind(Key, 1, L),
    P.

get_type(#phone{softphone_type = {"capabilities", {struct, L}}}, Key) ->
    {Key, P} = lists:keyfind(Key, 1, L),
    P.

