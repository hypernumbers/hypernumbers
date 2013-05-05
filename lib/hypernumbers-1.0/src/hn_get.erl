%%% @author    Gordon Guthrie <gordon@hypernumbers.dev>
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Broken out get fns for hn_mochi
%%%
%%% @end
%%% Created : 28 Mar 2013 by Gordon Guthrie <gordon@hypernumbers.dev>

-module(hn_get).

-include("hn_mochi.hrl").
-include("spriki.hrl").

-export([
         get/4
        ]).

-spec get(#refX{},
           page | cell | row | column | range,
           #qry{},
           #env{})
-> any().

% this is the path that the twilio phone redirect is wired to
get(#refX{path = ["_services", "phoneredirect" | []], obj = {page, "/"}},
     page, _Qry, Env = #env{accept = html}) ->
    Redir = case application:get_env(hypernumbers, environment) of
                {ok, development} ->
                    true;
                {ok, _Other} ->
                    case application:get_env(hypernumbers, phoneredirect) of
                        {ok, false} -> false;
                        {ok, true}  -> true
                    end
            end,
    case Redir of
        false -> hn_mochi:respond(401, Env);
        true  -> Redirect = hn_twilio_mochi:redir(Env),
                 hn_mochi:respond(302, Env#env{headers = [Redirect | Env#env.headers]})
    end;

% this path is hardwired into the module hn_twilio_mochi.erl
get(#refX{path = ["_services", "phone" | []], obj = {page, "/"}} = Ref,
     page, _Qry, Env = #env{accept = html}) ->
    case hn_twilio_mochi:handle_call(Ref, Env) of
        error     -> hn_mochi:'500'(Env);
        {ok, 200} -> hn_mochi:xml(Env, twiml:encode([])); % empty response
        TwiML     -> hn_mochi:xml(Env, TwiML)
    end;

% if a filename has got to here, there is one of 2 reasons:
% * the file don't exist - 404
% * the punter has missed a trailing slash - 303 redirect
get(#refX{path = P, obj = {filename, FileName}} = Ref, filename, Qry, Env) ->
    case length(string:tokens(FileName, ".")) of
        1 -> NewP = hn_util:list_to_path(P) ++ FileName ++ "/",
             E2 = Env#env{headers = [{"location", NewP}|Env#env.headers]},
             hn_mochi:respond(303, E2);
        _ -> error_logger:error_msg("404~n-~p~n-~p~n", [Ref, Qry]),
             hn_mochi:'404'(Ref, Env)
    end;

get(#refX{site = "http://usability.hypernumbers.com:8080" = Site,
           path = ["_reprovision"], obj = {page, "/"}}, page, _Qry, Env) ->
    ok = hn_setup:delete_site(Site),
    {initial_view, []} = hn_setup:site(Site, usability, []),
    {ok, _, Uid1} = passport:get_or_create_user("usability@hypernumbers.com"),
    passport:validate_uid(Uid1),
    passport:set_password(Uid1, "i!am!usable"),
    hn_groups:add_user(Site, "admin", Uid1),
    hn_db_upgrade:write_twilio_use_kvs(),
    hn_mochi:serve_html(200, Env, [hn_util:viewroot(Site), "/reprovision.html"]);

get(#refX{site = S, path = ["_site"]}, page, #qry{map = Name}, Env) when Name =/= undefined ->
    Maps = {struct, [hn_import:read_map(S, Name)]},
    hn_mochi:json(Env, Maps);

get(#refX{site = S, path = ["_site"]}, page, _Qry, Env) ->
    Groups    = {"groups", {array, hn_groups:get_all_groups(S)}},
    Templates = {"templates", {array, hn_util:get_templates(S)}},
    Maps      = {"maps", {array, hn_util:get_maps(S)}},
    Admin     = {"is_admin", hn_groups:is_member(Env#env.uid, S, ["admin"])},
    Lang      = {"lang", hn_mochi:get_lang(Env#env.uid)},
    Return    = {struct, [Groups, Templates, Maps, Admin, Lang]},
    hn_mochi:json(Env, Return);

get(#refX{path = ["_pages"]} = Ref, page, _Qry, Env) ->
    Pages       = {"pages", pages(Ref#refX{path = []})},
    Return      = {struct, [Pages]},
    hn_mochi:json(Env, Return);

% allready checked you are an admin
get(#refX{site = S, path = ["_statistics"]}, page, _Qry, Env) ->
    hn_mochi:text_html(Env, syslib:make_stats_page(S));

get(#refX{site = S, path = ["_logout"]}, page,
     #qry{return = QReturn}, Env) when QReturn /= undefined ->
    Return = mochiweb_util:unquote(QReturn),
    {303, Env2} = hn_mochi:cleanup(S, Return, Env),
    hn_mochi:respond(303, Env2),
    throw(ok);

get(#refX{site = Site, path = [X, _| Rest] = Path}, page, #qry{hypertag = HT}, Env)
  when X == "_mynewsite" ->
    case passport:open_hypertag(Site, Path, HT) of
        {ok, Uid, _Email, Data, Stamp, Age} ->
            Param = case lists:keyfind(param, 1, Data) of
                        false      -> "";
                        {param, P} -> P
                    end,

            Return = hn_util:strip80(Site)
                ++ hn_util:list_to_path(Rest)
                ++ Param,

            {Env2, Redir} = hn_mochi:post_login(Site, Uid, Stamp, Age,
                                                Env, Return),
            Env3 = Env2#env{headers = [{"location",Redir}|Env2#env.headers]},
            hn_mochi:respond(303, Env3),
            throw(ok);
        {error, E} ->
            % fu@#ity-bye!
            throw(E)
    end;

get(#refX{site = Site, path = [X | _Vanity]}, page, _Qry, Env)
  when X == "_forgotten_password" ->
    hn_mochi:serve_html(200, Env, [hn_util:viewroot(Site), "/forgotten_password.html"]);

get(#refX{site = Site, path = [X, _Vanity] = Path}, page,
     #qry{hypertag = HT}, Env) when X == "_authorize" ->
    case passport:open_hypertag(Site, Path, HT) of
        {ok, _Uid, _Email, Data, _Stamp, _Age} ->
            case proplists:get_value(emailed, Data) of
                true  ->
                    [{kvstore, site_email, {OldIdx, SE}}]
                        = new_db_api:read_kv(Site, site_email),
                    Idx = proplists:get_value(idx, Data),
                    case OldIdx of
                        Idx ->
                            Rec = {Idx, SE#site_email{email_validated = true}},
                            ok = new_db_api:write_kv(Site, site_email, Rec),
                            ok = new_db_api:mark_idx_dirty(Site, Idx);
                        _   ->
                            'do nothing'
                    end,
                    hn_mochi:serve_html(200, Env, [hn_util:viewroot(Site),
                                          "/thankyou.html"]);
                _Else ->
                    throw(bad_validation)
            end
    end;

get(#refX{site = Site, path = [X, _Vanity] = Path}, page,
     #qry{hypertag = HT}, Env)
  when X == "_invite"; X == "_validate" ->
    case passport:open_hypertag(Site, Path, HT) of
        {ok, Uid, _Email, Data, Stamp, Age} ->
            case proplists:get_value(emailed, Data) of
                true ->
                    ok = passport:validate_uid(Uid),
                    Redirect = case lists:keyfind(redirect, 1, Data) of
                                   false      -> "";
                                   {redirect, P} -> P
                               end,
                    Return = hn_util:strip80(Site)++Redirect,
                    {Env2, Redir} =
                        hn_mochi:post_login(Site, Uid, Stamp, Age, Env, Return),
                    Headers = [{"location",Redir}|Env2#env.headers],
                    hn_mochi:respond(303, Env2#env{headers = Headers}),
                    throw(ok);
                _Else ->
                    throw(bad_validation)
            end
    end;

get(#refX{site = Site} = RefX, _Type, #qry{view = ?RECALC}, Env) ->
    ok = new_db_api:recalc(RefX),
    Html = hn_util:viewroot(Site) ++ "/recalc.html",
    hn_mochi:serve_html(Env, Html);

get(#refX{site = Site} = Ref, cell, #qry{view = ?PHONE},
     #env{accept = html} = Env) ->
    File = case application:get_env(hypernumbers, phone_debug) of
               {ok, true} -> "softphone2.html";
               _          -> "softphone.html"
           end,
%% make sure there is a phone
    case new_db_api:get_phone(Ref) of
        []       -> hn_mochi:'404'(Ref, Env);
        [_Phone] -> Dir = hn_util:viewroot(Site) ++ "/",
                    hn_mochi:serve_html(200, Env, [Dir, File])
    end;

get(Ref, cell,  #qry{view = ?PHONE}, #env{accept = json, uid = Uid} = Env) ->
    case new_db_api:get_phone(Ref) of
        []      -> hn_mochi:json(Env, {struct, [{"error", "no phone at this url"}]});
        [Phone] -> JSON = hn_twilio_mochi:get_phone(Ref, Phone, Uid),
                   hn_mochi:json(Env, JSON)
    end;

get(#refX{} = Ref, cell, #qry{view = ?DEBUG}, #env{accept = html} = Env) ->
    Url = hn_util:refX_to_url(Ref),
    hn_mochi:text_html(Env, wrap(new_db_DEBUG:url(Url, verbose)));

% in html you can see cell and page logs
get(Ref, Obj, #qry{view = ?LOGVIEW}, #env{accept = html} = Env)
  when Obj == page orelse Obj == cell ->
    hn_mochi:text_html(Env, hn_logs:get_logs(Ref));

% in json you only get the cell logs
get(Ref, cell, #qry{view = ?LOGVIEW}, Env = #env{accept = json}) ->
    hn_mochi:json(Env, hn_logs:get_json_logs(Ref));

% 401 it otherwise for json logs
get(Ref, _Obj, #qry{view = ?LOGVIEW}, Env = #env{accept = json}) ->
    hn_mochi:'401'(Ref, Env);

% negotiate a recording
get(#refX{site = S} = Ref, page, #qry{view = ?RECORDING, play = Play},
     #env{accept = html} = Env) ->
    case hn_twilio_mochi:view_recording(Ref, Play) of
        {redir, Redir} ->
            E2 = Env#env{headers = [{"location", Redir} | Env#env.headers]},
            hn_mochi:respond(303, E2),
            throw(ok);
        no_recording ->
            Dir = hn_util:viewroot(S) ++ "/",
            File = "no_recording.html",
            hn_mochi:serve_html(200, Env, [Dir, File])
    end;

get(#refX{path = P} = Ref, Obj, #qry{css = CSS, view = ?WIKI},
     Env=#env{accept = html, uid = Uid})
  when Obj == page orelse Obj == range ->
    ok = status_srv:update_status(Uid, Ref, "view wiki page"),
    IncCSS = case CSS of
                 "none" -> "none";
                 _      -> "all"
             end,
    {{Html, W, H}, Addons} = hn_render:content(Ref, wikipage),
    Page = hn_render:wrap_page(Html, P, W, H, Addons, "wikipage", IncCSS),
    hn_mochi:text_html_nocache(Env, Page);

get(#refX{path = P} = Ref, Obj, #qry{css = CSS, view = ?WEBPAGE},
     Env = #env{accept = html, uid = Uid})
  when Obj == page orelse Obj == range ->
    ok = status_srv:update_status(Uid, Ref, "view webpage"),
    IncCSS = case CSS of
                 "none" -> "none";
                 _      -> "all"
             end,
    {{Html, W, H}, Addons} = hn_render:content(Ref, webpage),
    Page = hn_render:wrap_page(Html, P, W, H, Addons, "webpage", IncCSS),
    hn_mochi:text_html_nocache(Env, Page);

get(Ref = #refX{site = S}, page, #qry{view = FName},
     Env = #env{accept = html, uid = Uid})
  when FName /= undefined ->
    ok = status_srv:update_status(Uid, Ref, "viewed webpage"),
    Html = [hn_util:viewroot(S), "/", FName, ".html"],
    case filelib:is_file(Html) of
        true  -> hn_mochi:serve_html(Env, Html);
        false -> hn_mochi:'404'(Ref, Env)
    end;

get(#refX{site = Site, path = Path}, page,
     #qry{updates = Time, paths = More},
     Env = #env{accept = json})
  when Time /= undefined, More /= undefined ->
    Paths = [Path | [ string:tokens(X, "/") || X <- string:tokens(More, ",")]],
    remoting_request(Env, Site, Paths, Time);

%% get(#refX{site = S}, page, #qry{status = []}, Env) ->
%%     hn_mochi:json(Env, status_srv:get_status(S));

get(Ref, column, _Qry, Env = #env{accept = json}) ->
    hn_mochi:json(Env, hn_mochi:page_attributes(Ref, Env));

get(Ref, row, _Qry, Env = #env{accept = json}) ->
    hn_mochi:json(Env, hn_mochi:page_attributes(Ref, Env));

get(Ref, range, _Qry, Env = #env{accept = json}) ->
    hn_mochi:json(Env, hn_mochi:page_attributes(Ref, Env));

get(Ref, page, _Qry, Env = #env{accept = json}) ->
    hn_mochi:json(Env, hn_mochi:page_attributes(Ref, Env));

get(Ref, cell, _Qry, Env = #env{accept = json}) ->
    V = case new_db_api:read_attribute(Ref, "value") of
            [{_Ref, Val}] when is_atom(Val) ->
                atom_to_list(Val);
            [{_Ref, {datetime, D, T}}] ->
                dh_date:format("Y/m/d H:i:s",{D,T});
            [{_Ref, {errval, Val}}] ->
                atom_to_list(Val);
            [{_Ref, Val}] ->
                Val;
            [] ->
                [];
            _Else ->
                error_logger:error_msg("unmatched ~p~n", [_Else]),
                exit(unmatched_error)
        end,
    hn_mochi:json(Env, V);

get(Ref, _Type, Qry, Env) ->
    error_logger:error_msg("404~n-~p~n-~p~n", [Ref, Qry]),
    hn_mochi:'404'(Ref, Env).

%%------------------------------------------------------------------------------
%%
%% Internal Fns
%%
%%------------------------------------------------------------------------------
wrap(Text) ->
    "<html><head></head>"
        ++ "<body style='font: 14px Courier, monospace;'>"
        ++ Text
        ++ "</body></html>".

remoting_request(Env = #env{mochi = Mochi}, Site, Paths, Time) ->
    Socket = Mochi:get(socket),
    % TODO this code doesn't work - it doesn't detect the socket
    % going away - doesn't matter with updates - does with softphone
    inet:setopts(Socket, [{active, once}]),
    remoting_reg:request_update(Site, Paths, list_to_integer(Time), self()),
    receive
        {tcp_closed, Socket} -> ok;
        {error, timeout}     -> hn_mochi:json(Env, <<"timeout">>);
        {msg, Data}          -> Data2 = expand_binaries(Data),
                                hn_mochi:json(Env, Data2)
    after
        % TODO : Fix, should be controlled by remoting_reg
        30000 ->
            hn_mochi:json(Env, {struct, [{"time", remoting_reg:timestamp()},
                                {"timeout", "true"}]})
    end.
pages(#refX{} = RefX) ->
    Dict = new_db_api:read_page_structure(RefX),
    Tmp  = pages_to_json(dh_tree:add(RefX#refX.path, Dict)),
    {struct, [{"name", "home"}, {"children", {array, Tmp}}]}.

expand_binaries({struct, [{"time", Time}, {"msgs", {array, List}}]}) ->
    List2 = [binary_to_term(X) || X <- List],
    {struct, [{"time", Time}, {"msgs", {array, List2}}]}.

pages_to_json(Dict) ->
    F = fun(X) -> pages_to_j2(X, dict:fetch(X, Dict)) end,
    case hn_mochi:is_dict(Dict) of
        true  -> lists:sort(lists:map(F, dict:fetch_keys(Dict)));
        false -> Dict
    end.

pages_to_j2(X, Dict) ->
    case hn_mochi:is_dict(Dict) of
        true  ->
            case pages_to_json(Dict) of
                [] -> {struct, [{"name", X}]};
                Ch -> {struct, [{"name", X}, {"children", {array, Ch}}]}
            end;
        false -> {struct, [{"name", X}]}
    end.
