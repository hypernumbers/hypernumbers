%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_mochi).

-include("hypernumbers.hrl").

-include_lib("kernel/include/file.hrl").
-include("gettext.hrl").
-include("hn_mochi.hrl").
-include("spriki.hrl").
-include("syslib.hrl").

-define(E,        error_logger:error_msg).
-define(LOAD,     hn_templates:load_template_if_no_page).
-define(SORT,     lists:sort).
-define(NO_STAMP, undefined).
-define(DAY_S,    86400). % a day's worth of seconds
-define(check_pt_vw(A, B, C, D), auth_srv:check_particular_view(A, B, C, D)).

-export([
         start/0
        ]).

-export([
         handle/1,
         extract_styles/1,
         style_to_css/1,
         page_attrs_for_export/2,
         page_attributes/2,
         get_json_post/1    % Used for mochilog replay rewrites
        ]).

-define(SHEETVIEW,   "spreadsheet").
-define(WEBPAGE,     "webpage").
-define(WIKI,        "wikipage").
-define(DEMO,        "demopage").
-define(WEBPREVIEW,  "webpreview").
-define(WIKIPREVIEW, "wikipreview").
-define(LOGVIEW,     "logs").
-define(RECALC,      "recalc").

-spec start() -> {ok, pid()}.
start() ->
    case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting mochi~n");
       _Other     -> ok
    end,
    {ok, {Ip, Port}} = application:get_env(hypernumbers, mochi_bind),
    StrIp = inet_parse:ntoa(Ip),
    Opts = [{port, Port},
            {ip, StrIp},
            {name, ?MODULE},
            {loop, {hn_mochi, handle}}],
    mochiweb_http:start(Opts).

-spec handle(any()) -> ok.
handle(MochiReq) ->
    Site = get_site(MochiReq),
    try
        Ref = hn_util:url_to_refX(get_real_uri(MochiReq)),
        put(now, util2:get_timestamp()),
        Env = process_environment(MochiReq),
        Qry = process_query(Env),
        handle_(Ref, Env, Qry)
    catch
        ok          -> ok;
        exit:normal -> exit(normal);
        Type:What   ->
            Format = "web request failed~npath:  ~p~ntype:  ~p~nwhat:  ~p~n"
                ++"trace:~p~n",
            Path   = MochiReq:get(path),
            Msg    = [{path, Path},
                      {type, Type},
                      {what, What},
                      {trace, erlang:get_stacktrace()}],
            case What of
                {badmatch, {error, enoent}} ->
                    F1 = "dumping script kiddie (should be 404)~n~p~n",
                    ?E(F1, [process_environment(MochiReq)]),
                    log_path_errors(Path, Format, Msg),
                    '500'(process_environment(MochiReq));
                invalid_url ->
                    Dir = hn_util:viewroot(Site) ++ "/",
                    File = "invalidurl.html",
                    serve_html(404, process_environment(MochiReq), [Dir, File]);
                _ ->
                    ?E(Format, Msg),
                    '500'(process_environment(MochiReq))
            end
    end.

-spec handle_(#refX{}, #env{}, #qry{}) -> ok.

handle_(#refX{site="http://www."++Site}, E=#env{mochi=Mochi}, _Qry) ->
    Redir = "http://" ++ hn_util:strip80(Site) ++ Mochi:get(raw_path),
    Redirect = {"Location", Redir},
    respond(301, E#env{headers = [Redirect | E#env.headers]});

handle_(#refX{site = _S, path=["_sync" | Cmd]}, Env,
        #qry{return=QReturn, stamp=QStamp})
  when QReturn /= undefined ->
    %Msg = io_lib:format("~p in _sync1 for Cmd of ~p QReturn of ~p"
    %                    ++ "QStamp of ~p",
    %                    [S, Cmd, QReturn, QStamp]),
    %syslib:log(Msg, ?auth),
    Env2 = process_sync(Cmd, Env, QReturn, QStamp),
    respond(303, Env2),
    throw(ok);

handle_(Ref, Env, Qry) ->
    case hn_setup:site_exists(Ref#refX.site) of
        true  -> case filename:extension((Env#env.mochi):get(path)) of
                     []  -> authorize_resource(Env, Ref, Qry);
                     Ext -> handle_static(Ext, Ref#refX.site, Env)
                 end;
        false -> text_html(Env,
                           "The web site you seek<br/>"
                           "cannot be located, but<br/>"
                           "countless more exist.")
    end.

-spec authorize_resource(#env{}, #refX{}, #qry{}) -> no_return().
authorize_resource(Env, Ref, Qry) ->
    case cluster_up() of
        false -> text_html(Env, "There appears to be a network problem. "++
                           "Please try later");
        true  -> case Env#env.auth of
                     []   -> authorize_r2(Env, Ref, Qry);
                     Auth -> authorize_api(Auth, Env, Ref, Qry)
                 end
    end.

% this function will kinda be kooky in dev if you deregister your globals
cluster_up() ->
    Gs = global:registered_names(),
    case {lists:member(passport, Gs), lists:member(hns, Gs)} of
        {true, true} -> true;
        _            ->
            F   = "Trying to reconnect for passport/hns ~p~n",
            Msg = dh_date:format("Y/m/d G:i:s"),
            error_logger:info_msg(F, [Msg]),
            case net_adm:ping('hnlive@hypernumbers.com') of
                pong -> ok = hn_net_util:email("gordon@hypernumbers.com", "",
                                               atom_to_list(node()),
                                               "Disconnected Nodes",
                                               "...reconnecting sucessfully!"),
                        true; % reconnected, yay!
                pang -> ok = hn_net_util:email("gordon@hypernumbers.com", "",
                                               atom_to_list(node()),
                                               "Disconnected Nodes",
                                               "...reconnection unsucessful"),
                        false % not reconnected, boo!
            end
    end.

authorize_api(_Auth, _Env, _Ref, _Qry) ->
    denied.

authorize_r2(Env, Ref, Qry) ->
    Env2 = process_user(Ref#refX.site, Env),
    #env{method = Method, body = Body} = Env,
    AuthRet = case {Method, Body} of
                  {Req, _} when Req == 'GET'; Req == 'HEAD'  ->
                      authorize_get(Ref, Qry, Env2);
                  {'POST', multipart} ->
                      authorize_upload(Ref, Qry, Env2);
                  {'POST', _} ->
                      authorize_post(Ref, Qry, Env2)
              end,
    case {AuthRet, Env2#env.accept} of
        {allowed, _} ->
            handle_resource(Ref, Qry, Env2);
        {{view, View}, _} ->
            handle_resource(Ref, Qry#qry{view = View}, Env2);
        {not_found, html} ->
            serve_html(404, Env2,
                       [hn_util:viewroot(Ref#refX.site), "/404.html"]);
        {not_found, json} ->
            respond(404, Env2);
        {denied, html} ->
            serve_html(401, Env2,
                       [hn_util:viewroot(Ref#refX.site), "/401.html"]);
        {denied, json} ->
            respond(401, Env2);
        {{upload, denied}, html} ->
            Ret = {struct, [{error, "Permission denied: 401"}]},
            #env{mochi = Mochi} = Env,
            Mochi:ok({"text/html",
                      (mochijson:encoder([{input_encoding,
                                           utf8}]))(Ret)})
    end.

handle_resource(Ref, Qry, Env=#env{method = 'GET'}) ->
    mochilog:log(Env, Ref),
    ObjType = element(1, Ref#refX.obj),
    iget(Ref, ObjType, Qry, Env);

handle_resource(Ref, _Qry,
                Env=#env{method='POST', body=multipart,
                         mochi=Mochi, uid=Uid}) ->
    {ok, UserName} = passport:uid_to_email(Uid),
    {ok, File, Name, Data} = hn_file_upload:handle_upload(Mochi, Ref,
                                                          UserName),
    {_St, {Ret, _}} = load_file(Ref, Data, File, Name, UserName, Uid),
    Env2 = Env#env{raw_body = {upload, Name}},
    mochilog:log(Env2, Ref),

    Mochi:ok({"text/html",
              (mochijson:encoder([{input_encoding, utf8}]))(Ret)});

handle_resource(Ref, Qry, Env=#env{method = 'POST'}) ->
    mochilog:log(Env, Ref),
    ipost(Ref, Qry, Env).

-spec handle_static(string(), iolist(), any()) -> any().
handle_static(X, Site, Env)
  when X == ".png"; X == ".jpg"; X == ".css"; X == ".js"; X == ".txt";
X == ".ico"; X == ".json"; X == ".gif"; X == ".html"; X == ".htm"; X == ".pdf" ->
    Mochi = Env#env.mochi,
    "/"++RelPath = Mochi:get(path),
    Root = hn_util:docroot(Site),
    Mochi:serve_file(RelPath, Root),
    ok;
handle_static(_X, Site, Env) ->
    serve_html(404, Env, [hn_util:viewroot(Site), "/404.html"]).

-spec authorize_get(#refX{}, #qry{}, #env{})
-> {view, string()} | allowed | denied | not_found.

%% Specifically allow access to the json permissions. Only the permissions,
%% query may be present.
%% TODO: Only admins should be able to do this...
authorize_get(_Ref,
              #qry{permissions = [], _ = undefined},
              #env{accept = html}) ->
    allowed;

%% Authorize access to 'special' commands.
%% TODO put permissions access on _invite and _logout
authorize_get(#refX{path = [X | _]}, _Qry, #env{accept = html})
  when X == "_invite";
       X == "_mynewsite";
       X == "_validate";
       X == "_hooks";
       X == "_logout" ->
    allowed;

% deal with the reserved part of the namespace
% shows sites and pages
authorize_get(#refX{path = ["_" ++ X | _]}, _Qry, #env{accept = json})
  when X == "site";
       X == "pages" ->
    allowed;
% enable the sites pager
authorize_get(#refX{path = ["_sites" | []]}, _Qry, #env{accept = Accept})
  when Accept == html;
       Accept == json ->
    allowed;
% Feature Flag them out
%authorize_get(#refX{path = ["_services", "phone" | _], obj = {cell, _}},
%              _Qry, #env{accept = json}) ->
%    allowed;

%% Only some sites have a forgotten password box
authorize_get(#refX{path = [X | _Vanity]}, _Qry, #env{accept = html})
  when X == "_forgotten_password" ->
    case passport_running() of
        true  -> allowed;
        false -> denied
    end;

authorize_get(#refX{path = ["_" ++ _X | _]}, _Qry, #env{accept = Accept})
  when Accept == json;
       Accept == html ->
    denied;

%% Authorize update requests when the update is targeted towards a
%% spreadsheet. Since we have no closed security object, we rely on
%% 'run-time' checks.
authorize_get(#refX{site = Site, path = Path},
              #qry{updates = U, view = ?SHEETVIEW, paths = More},
              #env{accept = json, uid = Uid})
  when U /= undefined ->
    case auth_srv:check_particular_view(Site, Path, Uid, ?SHEETVIEW) of
        {view, ?SHEETVIEW} ->
            MoreViews = [auth_srv:get_any_view(Site, string:tokens(P, "/"), Uid)
                         || P <- string:tokens(More, ",")],
            case lists:all(fun({view, _}) -> true;
                              (_) -> false end,
                           MoreViews) of
                true  -> allowed;
                _Else -> denied
            end;
        _Else ->
            denied
    end;

%% Authorize access to the DEFAULT page. Notice that no query
%% parameters have been set.
authorize_get(#refX{site = Site, path = Path},
              #qry{_ = undefined},
              #env{accept = html, uid = Uid}) ->
    auth_srv:check_get_view(Site, Path, Uid);

%% Authorize access to the challenger view.
authorize_get(#refX{site = Site, path = Path},
              #qry{challenger=[]},
              #env{accept = html, uid = Uid}) ->
    auth_srv:check_get_challenger(Site, Path, Uid);

%% Always allows the demopage and preview pages views
authorize_get(_Ref, #qry{view = V}, _Env)
  when V == ?DEMO orelse V == ?WEBPREVIEW orelse V == ?WIKIPREVIEW ->
    allowed;

% you can only see the logs if you have the spreadsheet view
authorize_get(R, #qry{view = ?LOGVIEW} = Q, E) ->
    case authorize_get(R, Q#qry{view = ?SHEETVIEW}, E) of
        {view, ?SHEETVIEW} -> {view, ?LOGVIEW};
        Other              -> Other
    end;

% you can only force a recalc if you have the spreadsheet view
authorize_get(R, #qry{view = ?RECALC} = Q, E) ->
    case authorize_get(R, Q#qry{view = ?SHEETVIEW}, E) of
        {view, ?SHEETVIEW} -> {view, ?RECALC};
        Other              -> Other
    end;

%% Authorize access to one particular view.
authorize_get(#refX{site = Site, path = Path},
              #qry{view = View},
              #env{uid = Uid})
  when View /= undefined ->
    auth_srv:check_particular_view(Site, Path, Uid, View);

%% As a last resort, we will authorize a GET request to a location
%% from which we have a view.
authorize_get(#refX{site = Site, path = Path}, _Qry, Env) ->
    case auth_srv:get_any_view(Site, Path, Env#env.uid) of
        {view, _} -> allowed;
        _Else     -> denied
    end.

-spec authorize_post(#refX{}, #qry{}, #env{}) -> allowed | denied | not_found.

%% Allow special posts to occur
authorize_post(#refX{path = [X]}, _Qry, #env{accept = json})
  when X == "_login";
       X == "_hooks";
       X == "_forgotten_password";
       X == "_parse_expression" ->
    allowed;

authorize_post(#refX{site = Site, path = ["_admin"]}, _Qry,
               #env{accept = json, uid = Uid} = Env) ->
    case hn_groups:is_member(Uid, Site, ["admin"]) of
        true  -> allowed;
        false -> authorize_admin(Site, Env#env.body, Uid)
    end;

%% Allow a post to occur, if the user has access to a spreadsheet on
%% the target. But it might be a post from a form or an inline
%% update so you need to check for them too before allowing
%% the post to continue...
authorize_post(#refX{site = Site, path = Path}, _Qry, Env) ->
    case ?check_pt_vw(Site, Path, Env#env.uid, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> allowed;
        not_found          -> not_found;
        denied             -> authorize_p2(Site, Path, Env)
    end.

% WIKI's can take both 'postform' and 'postinline'
authorize_p2(Site, Path, Env) ->
    case ?check_pt_vw(Site, Path, Env#env.uid, ?WIKI) of
        not_found        -> not_found;
        {view, ?WIKI} ->
            case Env#env.body of
                [{"postform",   _}]      -> allowed;
                [{"postinline", _}]      -> allowed;
                [{"postwebcontrols", _}] -> allowed;
                _                        -> denied
            end;
        denied           -> authorize_p3(Site, Path, Env)
    end.

% WEBPAGE's can only do 'postform'
authorize_p3(Site, Path, Env) ->
    case ?check_pt_vw(Site, Path, Env#env.uid, ?WEBPAGE) of
        not_found        -> not_found;
        {view, ?WEBPAGE} ->
            case Env#env.body of
                [{"postform",   _}]      -> allowed;
                [{"postwebcontrols", _}] -> allowed;
                _                        -> denied
            end;
        % jakub's clause
        denied           ->
            case Env#env.body of
                [{"read_user_fn", _Args}]		-> allowed;
                [{"delete_user_fn", _Args}]	-> allowed;
                [{"write_user_fn", _Args}]	-> allowed;
                _						        				-> denied
            end
    end.

authorize_upload(#refX{site = S, path = P}, _Qry,  #env{uid = Uid}) ->
    Views = auth_srv:get_views(S, P, Uid),
    case has_appropriate_view(Views) of
        false -> {upload, denied};
        true  -> allowed
    end.

authorize_upload_again(#refX{site = S, path = P}, file, Uid) ->
    Views = auth_srv:get_views(S, P, Uid),
    lists:member(?SHEETVIEW, Views);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {load_templates, Template}, _Uid) ->
    Expected = new_db_api:matching_forms(RefX, 'load-template-button'),
    has_load_templates(Expected, Template);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {row, Map}, _Uid) ->
    Expected = new_db_api:matching_forms(RefX, 'map-rows-button'),
    has_map_row(Expected, Map);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {sheet, Map, Page}, _Uid) ->
    Expected = new_db_api:matching_forms(RefX, 'map-sheet-button'),
    has_map_sheet(Expected, Map, Page);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {custom, Map}, _Uid) ->
    Expected = new_db_api:matching_forms(RefX, 'map-custom-button'),
    has_map_custom(Expected, Map).

has_load_templates([], _Template) -> false;
has_load_templates([H | T], Template) ->
    case H of
        {form, _, {_, 'load-template-button', _}, _, _,
         {struct, [{"load_templates", Template}]}} ->
            true;
        _ ->
            has_load_templates(T, Template)
    end.

has_map_sheet([], _Map, _Page) -> false;
has_map_sheet([H | T], Map, Page) ->
    case H of
        {form, _, {_, 'map-sheet-button', _}, _, _,
         {struct, [{"map", Map}, {"page", Page}]}} ->
            true;
        _ ->
            has_map_sheet(T, Map, Page)
    end.

has_map_row([], _Map) -> false;
has_map_row([H | T], Map) ->
    case H of
        {form, _, {_, 'map-rows-button', _}, _, _,
         {struct, [{"map", Map}]}} ->
            true;
        _ ->
            has_map_row(T, Map)
    end.

has_map_custom([], _Map) -> false;
has_map_custom([H | T], Map) ->
    case H of
        {form, _, {_, 'map-custom-button', _}, _, _,
         {struct, [{"map", Map}]}} ->
            true;
        _ ->
            has_map_custom(T, Map)
    end.

has_appropriate_view([])                -> false;
has_appropriate_view([?SHEETVIEW | _T]) -> true;
has_appropriate_view([?WEBPAGE | _T])   -> true;
has_appropriate_view([?WIKI | _T])      -> true;
has_appropriate_view([_H | T])          -> has_appropriate_view(T).

authorize_admin(_Site, [{"admin", {_, [{"set_password", _}]}}], Uid) ->
    case passport:uid_to_email(Uid) of
        {ok, "anonymous"} -> denied;
        _                 -> allowed
    end;

authorize_admin(Site, [{"admin", {_, [{Request, {_, List}}]}}], Uid)
  when (Request == "set_view")
       orelse (Request == "set_champion")
       orelse (Request == "invite_user") ->
    case passport:uid_to_email(Uid) of
        {ok, "anonymous"} -> denied;
        _                 ->
            case lists:keyfind("path", 1, List) of
                false       -> denied;
                {"path", P} -> P2 = string:tokens(P, "/"),
                               case ?check_pt_vw(Site, P2, Uid, ?SHEETVIEW) of
                                   {view, ?SHEETVIEW} -> allowed;
                                   denied             -> denied
                               end
            end
    end.

-spec iget(#refX{},
           page | cell | row | column | range,
           #qry{},
           #env{})
-> any().

% if a filename has got to here, there is one of 2 reasons:
% * the file don't exist - 404
% * the punter has missed a trailing slash - 303 redirect
iget(#refX{path = P, obj = {filename, FileName}} = Ref, filename, Qry, Env) ->
    case length(string:tokens(FileName, ".")) of
        1 -> NewP = hn_util:list_to_path(P) ++ FileName ++ "/",
             E2 = Env#env{headers = [{"location", NewP}|Env#env.headers]},
             respond(303, E2);
        _ -> ?E("404~n-~p~n-~p~n", [Ref, Qry]),
             '404'(Ref, Env)
    end;

iget(#refX{site = S, path = ["_site"]}, page, #qry{map = Name}, Env) when Name =/= undefined ->
    Maps = {struct, [hn_import:read_map(S, Name)]},
    json(Env, Maps);

iget(#refX{site=S, path=["_site"]}, page, _Qry, Env) ->
    Groups    = {"groups", {array, hn_groups:get_all_groups(S)}},
    Templates = {"templates", {array, hn_util:get_templates(S)}},
    Maps      = {"maps", {array, hn_util:get_maps(S)}},
    Admin     = {"is_admin", hn_groups:is_member(Env#env.uid, S, ["admin"])},
    Lang      = {"lang", get_lang(Env#env.uid)},
    Return    = {struct, [Groups, Templates, Maps, Admin, Lang]},
    json(Env, Return);

iget(#refX{path=["_pages"]} = Ref, page, _Qry, Env) ->
    Pages     = {"pages", pages(Ref#refX{path=[]})},
    Return    = {struct, [Pages]},
    json(Env, Return);

iget(#refX{site=S, path=["_statistics"]}, page, _Qury, Env) ->
    case hn_groups:is_member(Env#env.uid, S, ["admin"]) of
        true  -> text_html(Env, syslib:make_stats_page(S));
        false -> serve_html(401, Env, [hn_util:viewroot(S), "/401.html"])
    end;

iget(#refX{site=S, path=["_logout"]}, page,
     #qry{return=QReturn}, Env) when QReturn /= undefined ->
    Return = mochiweb_util:unquote(QReturn),
    cleanup(S, Return, Env);

iget(#refX{site=Site, path=[X, _| Rest]=Path}, page, #qry{hypertag=HT}, Env)
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

            {Env2, Redir} = post_login(Site, Uid, Stamp, Age, Env, Return),
            Env3 = Env2#env{headers=[{"location",Redir}|Env2#env.headers]},
            respond(303, Env3),
            throw(ok);
        {error, E} ->
            % fu@#ity-bye!
            throw(E)
    end;

iget(#refX{site=Site, path=[X | _Vanity]}, page, _Qry, Env)
  when X == "_forgotten_password" ->
    serve_html(404, Env, [hn_util:viewroot(Site), "/forgotten_password.html"]);
iget(#refX{site=Site, path=[X, _Vanity] = Path}, page,
     #qry{hypertag=HT},
     Env) when X == "_invite"; X == "_validate" ->
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
                        post_login(Site, Uid, Stamp, Age, Env, Return),
                    Headers = [{"location",Redir}|Env2#env.headers],
                    respond(303, Env2#env{headers = Headers}),
                    throw(ok);
                _Else ->
                    throw(bad_validation)
            end
    end;

iget(#refX{site=Site, path=Path}, page, #qry{view=?DEMO}, Env) ->
    text_html(Env, make_demo(Site, Path));

iget(#refX{site=Site, path=Path}, page, #qry{view=?RECALC}, Env) ->
    ok = new_db_api:recalc_page(#refX{site = Site, type = url,
                                      path = Path, obj = {page, "/"}}),
    Html = hn_util:viewroot(Site) ++ "/recalc.html",
    serve_html(Env, Html);

iget(#refX{site=Site, path=Path}, page, #qry{view=?WEBPREVIEW}, Env) ->
    text_html(Env, make_preview("web", Site, Path));

iget(#refX{site=Site, path=Path}, page, #qry{view=?WIKIPREVIEW}, Env) ->
    text_html(Env, make_preview("wiki", Site, Path));

iget(Ref, page, #qry{view = ?LOGVIEW}, Env) ->
    text_html(Env, hn_logs:get_logs(Ref));

iget(Ref, Obj, #qry{view = ?WIKI}, Env=#env{accept = html,uid = Uid})
  when Obj == page orelse Obj == range ->
    ok = status_srv:update_status(Uid, Ref, "view wiki page"),
    {{Html, Width, Height}, Addons} = hn_render:content(Ref, wikipage),
    Page = hn_render:wrap_page(Html, Width, Height, Addons, "wikipage"),
    text_html_nocache(Env, Page);

iget(Ref, Obj, #qry{view=?WEBPAGE}, Env=#env{accept=html,uid=Uid})
  when Obj == page orelse Obj == range ->
    ok = status_srv:update_status(Uid, Ref, "view webpage"),
    {{Html, Width, Height}, Addons} = hn_render:content(Ref, webpage),
    Page = hn_render:wrap_page(Html, Width, Height, Addons, "webpage"),
    text_html_nocache(Env, Page);

iget(Ref=#refX{site=S}, page, #qry{view=FName},
     Env=#env{accept=html, uid=Uid})
  when FName /= undefined ->
    ok = status_srv:update_status(Uid, Ref, "viewed webpage"),
    Html = [hn_util:viewroot(S), "/", FName, ".html"],
    case filelib:is_file(Html) of
        true  -> serve_html(Env, Html);
        false -> '404'(Ref, Env)
    end;

iget(#refX{site = Site, path = Path}, page,
     #qry{updates = Time, paths = More}, Env=#env{accept = json})
  when Time /= undefined, More /= undefined ->
    Paths = [Path | [ string:tokens(X, "/") || X<-string:tokens(More, ",")]],
    remoting_request(Env, Site, Paths, Time);

iget(#refX{site = S}, page, #qry{status = []}, Env) ->
    json(Env, status_srv:get_status(S));

iget(#refX{site = S, path  = P}, page, #qry{permissions = []}, Env) ->
    json(Env, auth_srv:get_as_json(S, P));

iget(Ref, page, #qry{pages = []}, Env=#env{accept = json}) ->
    json(Env, pages(Ref));

iget(#refX{path = ["_services", "phone" | Path]} = Ref, cell, _Qry,
     Env = #env{accept = json}) ->
    AccountSID = "AC7a076e30da6d49119b335d3a6de43844",
    AuthToken  = "9248c9a2a25f6914fad9c9fb5b30e69c",
    AppSid     = "APabe7650f654fc34655fc81ae71caa3ff",
    Token = twilio_capabilities:generate(AccountSID, AuthToken,
                                         [{client_outgoing, AppSid, []}],
                                         [{expires_after, 7200}]),
    json(Env, {struct, [{"phonetoken", binary_to_list(Token)}]});

iget(Ref, page, _Qry, Env=#env{accept = json}) ->
    json(Env, page_attributes(Ref, Env));

iget(Ref, cell, #qry{view = ?LOGVIEW}, Env=#env{accept = html}) ->
    text_html(Env, hn_logs:get_logs(Ref));

iget(Ref, cell, _Qry, Env=#env{accept = json}) ->
    V = case new_db_api:read_attribute(Ref,"value") of
            [{_Ref, Val}] when is_atom(Val) ->
                atom_to_list(Val);
            [{_Ref, {datetime, D, T}}] ->
                dh_date:format("Y/m/d H:i:s",{D,T});
            [{_Ref, {errval, Val}}] ->
                atom_to_list(Val);
            [{_Ref, Val}] ->
                Val;
            _Else ->
                ?E("unmatched ~p~n", [_Else]),
                ""
        end,
    json(Env, V);

iget(Ref, _Type, Qry, Env) ->
    ?E("404~n-~p~n-~p~n", [Ref, Qry]),
    '404'(Ref, Env).

-spec ipost(#refX{}, #qry{}, #env{}) -> any().

ipost(Ref=#refX{path=["_parse_expression"]}=Ref, _Qry, Env) ->
    [{"expression", Expr}] = Env#env.body,
    % this expr is not going to be run, but you
    % need to compile it in a cell context
    % just firing in A1 - ie {cell, {1, 1}}
    Expr2 = muin:parse_expr_for_gui(Expr),
    json(Env, {struct, [{"expression", Expr2}]});

ipost(Ref=#refX{path=["_forgotten_password"]}=Ref, _Qry,
      Env=#env{uid=Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "forgot password"),
    case passport_running() of
        false -> '404'(Ref, Env);
        true  ->
            {S, R} = case ?SORT(Env#env.body) of
                         [{"email", E1}] ->
                             {ok, S1} = application:get_env(hypernumbers,
                                                            norefer_url),
                             request_pwd_reset(E1, S1);
                         [{"email", E1}, {"site", S1}] ->
                             request_pwd_reset(E1, S1);
                         [{"email", E1}, {"hash", Hash}, {"newpwd", NPwd}] ->
                             reset_password(E1, NPwd, Hash)
                     end,
            json(Env,{struct,[{"status",S}, {"response",R}]})
    end;

ipost(#refX{site=S, path=["_login"]}, Qry, E) ->
    [{"email", Email0},{"pass", Pass}, {"remember", _R}] = ?SORT(E#env.body),
    Email = string:to_lower(Email0),
    case passport:authenticate(Email, Pass, true) of
        {error, authentication_failed} ->
            json(E, {struct, [{"response", "error"}]});
        {ok, Uid, Stamp, Age} ->
            Return = case Qry#qry.return of
                         R when R /= undefined -> mochiweb_util:unquote(R);
                         _Else -> hn_util:strip80(S)
                     end,
            {E2, Redir} = post_login(S, Uid, Stamp, Age, E, Return),
            json(E2, {struct, [{"redirect", Redir}]})
    end;

%% the purpose of this message is to mark the mochilog so we don't
%% need to do nothing with anything...
ipost(_Ref, #qry{mark = []},
      Env=#env{body = [{"set",{struct, [{"mark", _Msg}]}}]}) ->
    json(Env, "success");

%% the purpose of this message is to log javascript errors into mochilog
%% so we don't need to do anything with anything
ipost(_Ref, #qry{jserr = []},
      Env=#env{body = [{"set",{struct, [{"jserr", _Msg}]}}]}) ->
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"load_template", {_, [{"name", Name}]}}],
                          uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "created page from template "++Name),
    ok = hn_templates:load_template(Ref, Name, Uid),
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"drag", {_, [{"range", Rng}]}}],
                          uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:drag_n_drop(Ref,
                                Ref#refX{obj = hn_util:parse_attr(range,Rng)},
                                Uid),
    json(Env, "success");

ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"insert", "before"}], uid = Uid})
  when O == row orelse O == column ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:insert(Ref, Uid),
    json(Env, "success");

ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"insert", "after"}], uid = Uid})
  when O == row orelse O == column ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:insert(make_after(Ref), Uid),
    json(Env, "success");

%% by default cells and ranges displace vertically
ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"insert", "before"}], uid = Uid})
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:insert(Ref, vertical, Uid),
    json(Env, "success");

%% by default cells and ranges displace vertically
ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"insert", "after"}], uid = Uid})
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:insert(Ref, vertical, Uid),
    json(Env, "success");

%% but you can specify the displacement explicitly
ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"insert", "before"}, {"displacement", D}],
               uid = Uid})
  when (O == cell orelse O == range),
       (D == "horizontal" orelse D == "vertical") ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:insert(Ref, list_to_existing_atom(D), Uid),
    json(Env, "success");

ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"insert", "after"}, {"displacement", D}],
               uid = Uid})
  when (O == cell orelse O == range),
       (D == "horizontal" orelse D == "vertical") ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    RefX2 = make_after(Ref),
    ok = new_db_api:insert(RefX2, list_to_existing_atom(D), Uid),
    json(Env, "success");

ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"delete", "all"}], uid = Uid})
  when O == page ->
    ok = status_srv:update_status(Uid, Ref, "deleted page"),
    ok = new_db_api:delete(Ref, Uid),
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body=[{"delete", "all"}],uid=Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "deleted page"),
    ok = new_db_api:delete(Ref, Uid),
    json(Env, "success");

ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"delete", Direction}],
               uid = Uid})
  when (O == cell orelse O == range),
       (Direction == "horizontal" orelse Direction == "vertical") ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:delete(Ref, list_to_atom(Direction), Uid),
    json(Env, "success");

ipost(Ref=#refX{obj = {O, _}}, _Qry,
      Env=#env{body=[{"insert", Direction}],
               uid = Uid})
  when (O == cell orelse O == range),
       (Direction == "horizontal" orelse Direction == "vertical") ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:insert(Ref, list_to_atom(Direction), Uid),
    json(Env, "success");

%% These three cases could be collapsed into one...
ipost(Ref,
      _Qry,
      Env=#env{body=[{"copy", {struct, [{"src", Src}]}}],
               uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:copy_n_paste(hn_util:url_to_refX(Src), Ref, all, Uid),
    json(Env, "success");
ipost(Ref,
      _Qry,
      Env=#env{body=[{"copystyle", {struct, [{"src", Src}]}}],
               uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:copy_n_paste(hn_util:url_to_refX(Src), Ref, style, Uid),
    json(Env, "success");
ipost(Ref,
      _Qry,
      Env=#env{body=[{"copyvalue", {struct, [{"src", Src}]}}],
               uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:copy_n_paste(hn_util:url_to_refX(Src), Ref, value, Uid),
    json(Env, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry,
      Env=#env{body=[{"borders", {struct, Attrs}}], uid=Uid})
  when O == cell orelse O == range ->
    Where = from("where", Attrs),
    Border = from("border", Attrs),
    Border_Style = from("border_style", Attrs),
    Border_Color = from("border_color", Attrs),
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:set_borders(Ref, Where, Border, Border_Style, Border_Color),
    json(Env, "success");

%% ipost(_Ref, _Qry,
%%       Env=#env{body = [{"set", {struct, [{"language", _Lang}]}}],
%%                uid = "anonymous"}) ->
%%     S = {struct, [{"error", "cant set language for anonymous users"}]},
%%     json(Env, S);

ipost(Ref=#refX{path=["_user"]}, _Qry,
      _Env=#env{body = [{"set", {struct, [{"language", _Lang}]}}],
                uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "changed language"),
    throw("can't set language right now");
%% ok = hn_users:update(Site, Uid, "language", Lang),
%% json(Env, "success");

%% ipost for inline editable cells
ipost(Ref=#refX{obj = {cell, _}} = Ref, _Qry,
      Env=#env{body = [{"postinline", {struct, [{"formula", Val}]}}],
               uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    % escape the attributes to prevent script injection, etc
    Attrs = [{"formula", hn_util:esc(Val)}],
    case new_db_api:read_attribute(Ref, "input") of
        [{#xrefX{}, "inline"}] ->
            ok = new_db_api:write_attributes([{Ref, Attrs}], Uid, Uid),
            json(Env, "success");
        [{#xrefX{}, {"select", Vs}}] ->
            [{"formula", V}] = Attrs,
            case lists:member(V, Vs) of
                true  -> ok = new_db_api:write_attributes([{Ref, Attrs}], Uid, Uid),
                         json(Env, "success");
                false -> respond(403, Env)
            end;
        [{#xrefX{}, {"dynamic_select", _Src, Vs}}] ->
            [{"formula", V}] = Attrs,
            case lists:member(V, Vs) of
                true  -> ok = new_db_api:write_attributes([{Ref, Attrs}], Uid, Uid),
                         json(Env, "success");
                false -> respond(403, Env)
            end;
        _ ->
            respond(403, Env)
    end;

ipost(Ref=#refX{obj = {cell, _}}, _Qry,
      Env=#env{body = [{"postinline", {struct, [{"clear","contents"}]}}],
               uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:clear(Ref, contents, Uid),
    json(Env, "success");

ipost(Ref=#refX{site = S, path = P} = Ref, _Qry,
      Env=#env{body = [{"postform", {struct, Vals}}], uid = PosterUid}) ->
    [{"results", ResPath}, {"values", {array, Array}}] = Vals,
    ok = status_srv:update_status(PosterUid, Ref, "edited page"),
    Transaction = common,
    Expected = new_db_api:matching_forms(Ref, Transaction),
    case hn_security:validate_form(Expected, Array) of
        false ->
            ?E("invalid form submission~n""on:       ~p~n"
               ++ "Expected: ~p~nGot:      ~p~n",
               [Ref, Expected, Array]),
            respond(403, Env);
        true  ->
            Path = string:tokens(hn_util:abs_path(P, ResPath), "/"),
            Res = Ref#refX{site = S, type = gurl, path = Path,
                           obj = {row, {1, 1}}},
            ok = new_db_api:handle_form_post(Res, Array, PosterUid),
            json(Env, "success")
    end;

% run a web control
ipost(Ref=#refX{obj = {page, _}}, _Qry,
      Env=#env{body = [{"postwebcontrols", Actions}],
               uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "created some pages"),
    run_actions(Ref, Env, mochijson:decode(Actions), Uid);

ipost(Ref, _Qry, Env=#env{body = [{"set", {struct, Attr}}], uid = Uid})
  when Attr =/= [] ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    case Attr of
        % TODO : Get Rid of this (for pasting a range of values)
        [{"formula",{array, Vals}}] ->
            post_range_values(Ref, Vals, Uid, Uid);
        [{"input", {struct, [{"select", {array, Array}}]}}] ->
            NewAttr = [{"input", {"select", Array}}],
            ok = new_db_api:write_attributes([{Ref, NewAttr}], Uid, Uid);
        [{"input", {struct, [{"dynamic_select", Expression}]}}] ->
            NewAttr = [{"input", {"dynamic_select", Expression}}],
            ok = new_db_api:write_attributes([{Ref, NewAttr}], Uid, Uid);
        [{"width", _}] ->
            ok = expand_width(Ref, Attr, Uid, Uid);
        [{"height", _}] ->
            ok = expand_height(Ref, Attr, Uid, Uid);
        [{"fixedHeight", _}] ->
                  ok = expand_height(Ref, Attr, Uid, Uid);
        _Else ->
            ok = new_db_api:write_attributes([{Ref, Attr}], Uid, Uid)
    end,
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"set", {array, Array}}], uid = Uid})
  when Array =/= [] ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    List = [X || {struct, [X]} <- Array],
    ok = new_db_api:write_attributes([{Ref, List}], Uid, Uid),
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"clear", What}], uid = Uid})
  when What == "contents"; What == "style"; What == "all" ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:clear(Ref, list_to_atom(What), Uid),
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"clear", What}], uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = new_db_api:clear(Ref, {attributes, [What]}, Uid),
    json(Env, "success");

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back_create handler                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ipost(Ref, _Qry,
%%       Env=#env{body = [{"action", "notify_back_create"}|T]}) ->

%%     %% WARNING this assumes that the list is provided in strict order - should
%%     %% really sort the list before testing for "action"
%%     Biccie   = from("biccie",     T),
%%     Proxy    = from("proxy",      T),
%%     ChildUrl = from("child_url",  T),
%%     PVsJson  = from("parent_vsn", T),
%%     CVsJson  = from("child_vsn",  T),
%%     Stamp    = from("stamp",      T),

%%     #refX{site = Site} = Ref,
%%     ParentX = Ref,
%%     _ParentUrl = hn_util:refX_to_url(ParentX),
%%     ChildX = hn_util:url_to_refX(ChildUrl),

%%     %% there is only 1 parent and 1 child for this action
%%     PVsn = json_util:unjsonify(PVsJson),
%%     CVsn = json_util:unjsonify(CVsJson),
%%     %% #version{page = PP, version = PV} = PVsn,
%%     %% #version{page = CP, version = CV} = CVsn,
%%     Sync1 = new_db_api:check_page_vsn(Site, PVsn),
%%     Sync2 =  new_db_api:check_page_vsn(Site, CVsn),
%%     case Sync1 of
%%         synched         -> ok;
%%         unsynched       -> new_db_api:resync(Site, PVsn);
%%         not_yet_synched -> ok % the child gets the version in this call...
%%     end,
%%     case Sync2 of
%%         synched         -> ok;
%%         unsynched       -> new_db_api:resync(Site, CVsn);
%%         not_yet_synched -> sync_exit()
%%     end,
%%     {struct, Return} = new_db_api:register_hn_from_web(ParentX, ChildX,
%%                                                       Proxy, Biccie),
%%     Return2 = lists:append([Return, [{"stamp", Stamp}]]),
%% json(Env, {struct, Return2});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handler                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ipost(Ref, _Qry,
%%       Env=#env{body = [{"action", "notify_back"} |T] = _Json}) ->
%%     Biccie    = from("biccie",     T),
%%     ChildUrl  = from("child_url",  T),
%%     ParentUrl = from("parent_url", T),
%%     Type      = from("type",       T),
%%     PVsJson   = from("parent_vsn", T),
%%     CVsJson   = from("child_vsn",  T),
%%     Stamp     = from("stamp",      T),

%%     %% there is only 1 parent and 1 child here
%%     PVsn = json_util:unjsonify(PVsJson),
%%     CVsn = json_util:unjsonify(CVsJson),
%%     %% #version{page = PP, version = PV} = PVsn,
%%     %% #version{page = CP, version = CV} = CVsn,
%%     ChildX = hn_util:url_to_refX(ChildUrl),
%%     ParentX = hn_util:url_to_refX(ParentUrl),
%%     #refX{site = Site} = Ref,
%%     Sync1 = new_db_api:check_page_vsn(Site, CVsn),
%%     Sync2 = new_db_api:check_page_vsn(Site, PVsn),
%%     case Sync1 of
%%         synched ->
%%             ok = new_db_api:notify_back_from_web(ParentX, ChildX,
%%                                                 Biccie, Type);
%%         unsynched ->
%%             new_db_api:resync(Site, PVsn);
%%         not_yet_synched ->
%%             ok = new_db_api:initialise_remote_page_vsn(Site, PVsn)
%%     end,
%%     case Sync2 of
%%         synched -> ok;
%%         unsynched ->
%%             ok = new_db_api:resync(Site, CVsn);
%%         not_yet_synched ->
%%             ok = new_db_api:initialise_remote_page_vsn(Site, CVsn)
%%     end,
%%     S = {struct, [{"result", "success"}, {"stamp", Stamp}]},
%%     json(Env, S);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify handler                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ipost(Ref, _Qry,
%%       Env=#env{body = [{"action", "notify"} | T] = _Json}) ->
%%     Biccie    = from("biccie",     T),
%%     ParentUrl = from("parent_url", T),
%%     Type      = from("type",       T),
%%     Payload   = from("payload",    T),
%%     PVsJson   = from("parent_vsn", T),
%%     CVsJson   = from("child_vsn",  T),
%%     Stamp     = from("stamp",      T),

%%     ParentX = hn_util:url_to_refX(ParentUrl),
%%     ChildX = Ref,
%%     _ChildUrl = hn_util:refX_to_url(ChildX),

%%     #refX{site = Site} = ChildX,
%%     PVsn = json_util:unjsonify(PVsJson),
%%     CVsn = json_util:unjsonify(CVsJson),
%%     %%#version{page = PP, version = PV} = PVsn,

%%     Sync1 = case Type of
%%                 "insert"    -> new_db_api:incr_remote_page_vsn(Site, PVsn, Payload);
%%                 "delete"    -> new_db_api:incr_remote_page_vsn(Site, PVsn, Payload);
%%                 "new_value" -> new_db_api:check_page_vsn(Site, PVsn)
%%             end,
%%     %% there is one parent and it if is out of synch, then don't process it, ask for a
%%     %% resynch
%%     case Sync1 of
%%         synched ->
%%             ok = new_db_api:notify_from_web(ParentX, Ref, Type,
%%                                            Payload, Biccie);
%%         unsynched ->
%%             ok = new_db_api:resync(Site, PVsn);
%%         not_yet_synched ->
%%             sync_exit()
%%     end,
%%     %% there are 1 to many children and if they are out of synch ask for
%%     %% a resynch for each of them
%%     Fun =
%%         fun(X) ->
%%                 Sync2 = new_db_api:check_page_vsn(Site, X),
%%                 %% #version{page = CP, version = CV} = X,
%%                 case Sync2 of
%%                     synched         -> ok;
%%                     unsynched       -> ok = new_db_api:resync(Site, X);
%%                     not_yet_synched -> sync_exit()
%%                 end
%%         end,
%%     [Fun(X) || X <- CVsn],
%%     S = {struct, [{"result", "success"}, {"stamp", Stamp}]},
%%     json(Env, S);

ipost(#refX{site = Site, path = _P}, _Qry,
      Env=#env{body = [{"admin", Json}], uid = Uid}) ->
    {struct,[{Fun, {struct, Args}}]} = Json,
    case hn_web_admin:rpc(Uid, Site, Fun, Args) of
        ok              -> json(Env, {struct, [{"result", "success"}]});
        {error, Reason} -> ?E("invalid _admin request ~p~n", [Reason]),
                           json(Env, {struct, [{"failure", Reason}]})
    end;

ipost(#refX{site=RootSite, path=["_hooks"]},
      _Qry, Env=#env{body=Body, uid=PrevUid}) ->
    [{"signup",{struct,[{"email",Email0} , {"sitetype", SiteType}]}}] = Body,
    SType = site_type_exists(SiteType),
    Email = string:to_lower(Email0),
    Zone = case application:get_env(hypernumbers, environment) of
               {ok, development} -> "hypernumbers.dev";
               {ok, server_dev} -> "dev.hypernumbers.com";
               {ok, production}  -> "tiny.hn"
           end,
    case factory:provision_site(Zone, Email, SType, PrevUid) of
        {ok, new, Site, Node, Uid, Name, InitialView} ->
            log_signup(RootSite, Site, Node, Uid, Email),
            Opaque = [{param, InitialView}],
            Expiry = "never",
            Url = passport:create_hypertag(Site, ["_mynewsite", Name],
                                           Uid, Email, Opaque, Expiry),
            json(Env, {struct, [{"result", "success"}, {"url", Url}]});
        {ok, existing, Site, Node, Uid, _Name, InitialView} ->
            log_signup(RootSite, Site, Node, Uid, Email),
            json(Env, {struct, [{"result", "success"},
                                {"url", Site ++ InitialView}]});
        {error, invalid_email} ->
            Str = "Sorry, the email provided was invalid, please try again.",
            json(Env, {struct, [{"result", "error"}, {"reason", Str}]})
    end;

ipost(#refX{site = Site, path = _P}, _Qry,
      Env=#env{body = [{"read_user_fn", Entry}], uid = Uid}) ->
    {struct, Args} = Entry,
    case hn_web_admin:rpc(Uid, Site, "read_user_fn", Args) of
        {ok, Return}	-> json(Env, Return);
        {error, Reason} -> ?E("invalid curie request ~p~n", [Reason]),
                           json(Env, {struct, [{"failure", Reason}]})
    end;


ipost(#refX{site = Site, path = _P}, _Qry,
      Env=#env{body = [{"delete_user_fn", Entry}], uid = Uid}) ->
    {struct, Args} = Entry,
    case hn_web_admin:rpc(Uid, Site, "delete_user_fn", Args) of
        {ok, Return}	-> json(Env, Return);
        {error, Reason} -> ?E("invalid curie request ~p~n", [Reason]),
                           json(Env, {struct, [{"failure", Reason}]})
    end;


ipost(#refX{site = Site, path = _P}, _Qry,
      Env=#env{body = [{"write_user_fn", Entry}], uid = Uid}) ->
    {struct, Args} = Entry,
    case hn_web_admin:rpc(Uid, Site, "write_user_fn", Args) of
        {ok, Return}	-> json(Env, Return);
        {error, Reason} -> ?E("invalid curie request ~p~n", [Reason]),
                           json(Env, {struct, [{"failure", Reason}]})
    end;

%~ ipost(_Ref, _Qry, Env=#env{body= [{"type", "user_defined_read"} | _T] = Json_Entry}) ->
%~ Return = curie:read_user_fn(Json_Entry),
%~ json(Env, Return);
%~
%~
%~ ipost(_Ref, _Qry, Env=#env{body= [{"type", "user_defined_delete"} | _T] = Json_Entry}) ->
%~ Return = curie:delete_user_fn(Json_Entry),
%~ json(Env, Return);

ipost(Ref, Qry, Env) ->
    ?E("404~n-~p~n-~p~n",[Ref, Qry]),
    '404'(Ref, Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec log_signup(string(), string(), atom(), auth_srv:uid(), string()) -> ok.
log_signup(Site, NewSite, Node, Uid, Email) ->
    Row = [ {hn_util:url_to_refX(Site ++ "/_sites/" ++ Ref), Val}
            || {Ref, Val} <- [{"A:A", Email},
                              {"B:B", "<a href='"++NewSite++
                               "'>"++NewSite++"</a>"},
                              {"C:C", Uid},
                              {"D:D", dh_date:format("Y/m/d G:i:s")},
                              {"E:E", atom_to_list(Node)} ] ],
    new_db_api:append_row(Row, nil, nil).

get_site(Env) ->
    Host = get_host(Env),
    Port = get_port(Env),
    lists:concat(["http://", string:to_lower(Host), ":", Port]).

%% Some clients dont send ip in the host header
get_real_uri(Env) ->
    Host = get_host(Env),
    Port = get_port(Env),
    lists:concat(["http://", string:to_lower(Host), ":", Port,
                  Env:get(path)]).

get_port(Env) ->
    case Env:get_header_value("HN-Port") of
        undefined ->
            {ok, P} = inet:port(Env:get(socket)),
            integer_to_list(P);
        ProxiedPort ->
            ProxiedPort
    end.

get_host(Env) ->
    case Env:get_header_value("HN-Host") of
        undefined ->
            lists:takewhile(fun(X) -> X /= $: end,
                            Env:get_header_value("host"));
        ProxiedHost ->
            ProxiedHost
    end.

get_json_post(undefined) ->
    {ok, undefined};
get_json_post(Json) ->
    {struct, Attr} = mochijson:decode(Json),
    {ok, lists:map(fun hn_util:js_to_utf8/1, Attr)}.

add_styles([], Tree) ->
    Tree;
add_styles([ {Name, CSS} | T], Tree) ->
    add_styles(T, dh_tree:set(["styles", Name], CSS, Tree)).

to_dict([], JSON) ->
    JSON;
to_dict([{Ref, KVs} | T], JSON) ->
    JSON2 = add_ref(Ref, KVs, JSON),
    to_dict(T, JSON2).

add_ref(_Ref, [], JSON) ->
    JSON;
add_ref(Ref, [{"__"++_Hidden, _}|Tail], JSON) ->
    add_ref(Ref, Tail, JSON);
add_ref(Ref, [KV | Tail], JSON) ->
    JSON2 = add_ref1(Ref, hn_util:jsonify_val(KV), JSON),
    add_ref(Ref, Tail, JSON2).

add_ref1(#xrefX{obj = {page, "/"}}, {Name, Val}, JSON) ->
    dh_tree:set(["page", Name], Val, JSON);
add_ref1(#xrefX{obj = {Ref, {X, Y}}}, Data, JSON) ->
    {Name, Val} = hn_util:jsonify_val(Data),
    dh_tree:set([atom_to_list(Ref), itol(Y), itol(X), Name], Val, JSON).

tmpdir() ->
    code:lib_dir(hypernumbers) ++ "/../../var/tmp/".

itol(X) -> integer_to_list(X).
ltoi(X) -> list_to_integer(X).

is_dict(Dict) when is_tuple(Dict) -> dict == element(1,Dict);
is_dict(_Else)                    -> false.

dict_to_struct(Dict) ->
    F = fun(X) -> dict_to_struct(X, dict:fetch(X, Dict)) end,
    case is_dict(Dict) of
        true  -> lists:map(F, dict:fetch_keys(Dict));
        false -> Dict
    end.

dict_to_struct(X, Dict) ->
    case is_dict(Dict) of
        true  -> {X, {struct, dict_to_struct(Dict)}};
        false -> {X, Dict}
    end.

-spec extract_styles(string()) -> [#style{}].
extract_styles(Site) ->
    [style_to_css(S) ||
        S <- new_db_api:read_styles_IMPORT(#refX{site=Site}) ].

style_to_css(#style{magic_style = Style, idx = I}) ->
    Num = ms_util2:no_of_fields(magic_style),
    {I, style_att(Num + 1, Style, [])}.

style_att(1, _Rec, Acc) ->
    lists:flatten(Acc);
style_att(X, Rec, Acc) ->
    case element(X, Rec) of
        [] ->
            style_att(X - 1, Rec, Acc);
        _Else ->
            Name =  ms_util2:name_by_index(magic_style, X-1),
            A = io_lib:format("~s:~s;",[Name, element(X,Rec)]),
            style_att(X - 1, Rec, [A | Acc])
    end.

from(Key, List) ->
    {value, {Key, Value}} = lists:keysearch(Key, 1, List),
    Value.

post_range_values(Ref, Values, PAr, VAr) ->
    F = fun({array, Vals}, Acc) ->
                post_column_values(Ref, Vals, PAr, VAr, Acc), Acc+1
        end,
    lists:foldl(F, 0, Values).

post_column_values(Ref, Values, PAr, VAr, Offset) ->
    #refX{obj={range,{X1, Y1, _X2, _Y2}}} = Ref,
    F = fun(Val, Acc) ->
                % if you paste in a range with blank cells from excel you
                % don't want values of "" stuck in because they count as not blank
                % with countblank() so we skip them
                % we don't do this in cell formulae though...
                NRef = Ref#refX{type = url, obj = {cell,
                                                   {X1 + Acc, Y1 + Offset}}},
                case Val of
                    "" -> ok = new_db_api:clear(NRef, contents, PAr),
                          Acc + 1;
                    _  -> ok = new_db_api:write_attributes([{NRef,
                                                             [{"formula", Val}]}],
                                                           PAr, VAr),
                          Acc + 1
                end
        end,
    lists:foldl(F, 0, Values).

remoting_request(Env=#env{mochi=Mochi}, Site, Paths, Time) ->
    Socket = Mochi:get(socket),
    inet:setopts(Socket, [{active, once}]),
    remoting_reg:request_update(Site, Paths, ltoi(Time), self()),
    receive
        {tcp_closed, Socket} -> ok;
        {error, timeout}     -> json(Env, <<"timeout">>);
        {msg, Data}          -> Data2 = expand_binaries(Data),
                                json(Env, Data2)
    after
%% TODO : Fix, should be controlled by remoting_reg
        600000 ->
            json(Env, {struct, [{"time", remoting_reg:timestamp()},
                                {"timeout", "true"}]})
    end.

% need to pay attention how you use this
% dynamic input attributes need to be cleaned up with
% hn_util:clean_up_page_attrs in certain circumstances
-spec page_attrs_for_export(#refX{}, #env{}) -> {struct, list()}.
page_attrs_for_export(#refX{site = S, path = P} = Ref, Env) ->
    page_a2(Ref, Env, export).

-spec page_attributes(#refX{}, #env{}) -> {struct, list()}.
page_attributes(#refX{site = S, path = P} = Ref, Env) ->
    page_a2(Ref, Env, full).

page_a2(#refX{site = S, path = P} = Ref, Env, Type) ->
    #env{uid=UID} = Env,
    Content = new_db_api:read_intersect_ref(Ref),
    Content2 = case Type of
                   full -> Content;
                   export -> clean_up_dyn_sel(Content, [])
               end,
    Init    = [["cell"], ["column"], ["row"], ["page"], ["styles"]],
    Tree    = dh_tree:create(Init),
    Styles  = extract_styles(S),
    NTree   = add_styles(Styles, Tree),
    Dict    = to_dict(Content2, NTree),
    Time    = {"time", remoting_reg:timestamp()},
    Usr     = {"user", Env#env.email},
    Host    = {"host", S},
    Views   = {"views", {array, auth_srv:get_views(S, P, UID)}},
    Perms   = {"permissions", auth_srv:get_as_json(S, P)},
    {struct, [Time, Usr, Host, Perms, Views | dict_to_struct(Dict)]}.

clean_up_dyn_sel([], Acc) -> Acc;
clean_up_dyn_sel([{XRefX, Attrs} | T], Acc) ->
    Attrs2 = clean2(Attrs, []),
    clean_up_dyn_sel(T, [{XRefX, Attrs2} | Acc]).

clean2([], Acc) -> Acc;
clean2([{"input", {"dynamic_select", S, Vals}} | T], Acc) ->
    clean2(T, [{"input", {"dynamic_select", S}} | Acc]);
clean2([H | T], Acc) ->
    clean2(T, [H | Acc]).

make_after(#refX{obj = {cell, {X, Y}}} = RefX) ->
    RefX#refX{obj = {cell, {X - 1, Y - 1}}};
make_after(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    DiffX = X2 - X1 - 1,
    DiffY = Y2 - Y1 - 1,
    RefX#refX{obj = {range, {X1 - DiffX, Y1 - DiffY, X2 - DiffX, Y2 - DiffY}}};
make_after(#refX{obj = {column, {X1, X2}}} = RefX) ->
    DiffX = X2 - X1 + 1,
    RefX#refX{obj = {column, {X1 + DiffX, X2 + DiffX}}};
make_after(#refX{obj = {row, {Y1, Y2}}} = RefX) ->
    DiffY = Y2 - Y1 + 1,
    RefX#refX{obj = {row, {Y1 + DiffY, Y2 + DiffY}}}. %

pages(#refX{} = RefX) ->
    Dict = new_db_api:read_page_structure(RefX),
    Tmp  = pages_to_json(dh_tree:add(RefX#refX.path, Dict)),
    {struct, [{"name", "home"}, {"children", {array, Tmp}}]}.

accept_type(Env) ->
    case Env:get_header_value('Accept') of
        undefined -> html; %cheapskate googlebots don't set accept header
        Accept    ->
            case re:run(Accept, "application/json") of
                {match, _} -> json;
                nomatch -> html
            end
    end.

pages_to_json(Dict) ->
    F = fun(X) -> pages_to_json(X, dict:fetch(X, Dict)) end,
    case is_dict(Dict) of
        true  -> lists:sort(lists:map(F, dict:fetch_keys(Dict)));
        false -> Dict
    end.

pages_to_json(X, Dict) ->
    case is_dict(Dict) of
        true  ->
            case pages_to_json(Dict) of
                [] -> {struct, [{"name", X}]};
                Ch -> {struct, [{"name", X}, {"children", {array, Ch}}]}
            end;
        false -> {struct, [{"name", X}]}
    end.

%% sync_exit() ->
%%     exit("exit from hn_mochi:handle_req impossible page versions").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Input Processors
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_query(#env{}) -> #qry{}.
process_query(#env{mochi = Mochi}) ->
    List = Mochi:parse_qs(),
    process_query_(List, #qry{}).

process_query_([], Qry) -> Qry;
process_query_([{Param, Value} | Rest], Qry) ->
    Qry2 = case catch list_to_existing_atom(Param) of
               P when is_atom(P) ->
                   case ms_util2:is_in_record(qry, P) of
                       true ->
                           Idx = ms_util2:get_index(qry, P) + 1,
                           setelement(Idx, Qry, Value);
                       false ->
                           Qry
                   end;
               _Else -> Qry
           end,
    process_query_(Rest, Qry2).

-spec process_environment(any()) -> #env{}.
process_environment(Mochi) ->
    {RawBody, Body} =
        case Mochi:get(method) of
            'GET'  -> {undefined, undefined};
            'HEAD' -> {undefined, undefined};
            'POST' -> Headers = mochiweb_headers:lookup('Content-Type',
                                                        Mochi:get(headers)),
                      case Headers of
                          none -> RB = Mochi:recv_body(),
                                  {ok, B} = get_json_post(RB),
                                  {RB, B};
                          {_,{_,T}} ->
                              case lists:prefix("multipart/form-data", T) of
                                  true  -> {undefined, multipart};
                                  false -> RB = Mochi:recv_body(),
                                           {ok, B} = get_json_post(RB),
                                           {RB, B}
                              end
                      end
        end,
    Hdrs = Mochi:get(headers),
    Auth = case mochiweb_headers:lookup("authorization", Hdrs) of
               none            -> [];
               {value, {_, A}} -> A
           end,
    #env{mochi    = Mochi,
         accept   = accept_type(Mochi),
         method   = Mochi:get(method),
         raw_body = RawBody,
         body     = Body,
         auth     = Auth}.

-spec process_user(string(), #env{}) -> #env{} | no_return().
process_user(Site, E=#env{mochi = Mochi}) ->
    Auth = Mochi:get_cookie_value("auth"),
    %syslib:log(io_lib:format("~p in process_user (a) for ~p", [Site, Auth]),
    %           ?auth),
    try passport:inspect_stamp(Auth) of
        {ok, Uid, Email} ->
            %Msg1 = io_lib:format("~p in process_user (b) for ~p ~p",
            %                     [Site, Uid, Email]),
            %syslib:log(Msg1, ?auth),
            E#env{uid = Uid, email = Email};
        {error, no_stamp} ->
            Return = cur_url(Site, E),
            %Msg2 = io_lib:format("~p in process_user (c) Return is ~p",
            %                     [Site, Return]),
            %syslib:log(Msg2, ?auth),
            case try_sync(["seek"], Site, Return, ?NO_STAMP) of
                on_sync ->
                    Stamp = passport:temp_stamp(),
                    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
                    %Msg3 = io_lib:format("~p in process_user (d) Stamp is ~p "
                    %                     ++" Cookie is ~p~n",
                    %                     [Site, Stamp, Cookie]),
                    %syslib:log(Msg3, ?auth),
                    E#env{headers = [Cookie | E#env.headers]};
                {redir, Redir} ->
                    %Msg4 = io_lib:format("~p in process_user (e) Redir is ~p~n",
                    %                     [Site, Redir]),
                    %syslib:log(Msg4, ?auth),
                    E2 = E#env{headers = [{"location",Redir}|E#env.headers]},
                    respond(303, E2),
                    throw(ok)
            end;
        {error, _Reason} ->
            %Msg5 = io_lib:format("~p in process_user (f) Redir is ~p~n",
            %                     [Site, Reason]),
            %syslib:log(Msg5, ?auth),
            cleanup(Site, cur_url(Site, E), E)
    catch error:
                _Other -> %Msg6 = io_lib:format("~p in process_user (g) "
                          %                     ++ "cleanup",[Site]),
                          %syslib:log(Msg6, ?auth),
                          cleanup(Site, cur_url(Site, E), E)
                        end.

%% Clears out auth cookie on current and main server.
-spec cleanup(string(), string(), #env{}) -> no_return().
cleanup(Site, Return, E) ->
    Cookie = hn_net_util:kill_cookie("auth"),
    E2 = E#env{headers = [Cookie | E#env.headers]},
    Redir = case try_sync(["reset"], Site, Return, ?NO_STAMP) of
                on_sync    ->
                    %Msg1 = io_lib:format("~p in cleanup Return with ~p",
                    %                     [Site, Return]),
                    %syslib:log(Msg1, ?auth),
                    Return;
                {redir, R} ->
                    %Msg2 = io_lib:format("~p in cleanup Redir with ~p",
                    %                     [Site, R]),
                    %syslib:log(Msg2, ?auth),
                    R
            end,
    E3 = E2#env{headers = [{"location",Redir}|E2#env.headers]},
    respond(303, E3),
    throw(ok).

%% Returns the url representing the current location.
-spec cur_url(string(), #env{}) -> string().
cur_url(Site, #env{mochi=Mochi}) ->
    hn_util:strip80(Site) ++ Mochi:get(raw_path).

-spec try_sync([string()], string(), string(), string())
-> {redir, string()} | on_sync.
try_sync(Cmd0, Site, Return, Stamp) ->
    %Msg = io_lib:format("~p in try_sync for Site of SUrl of ~p~n~n",
    %                    [Site, application:get_env(hypernumbers, sync_url)]),
    %syslib:log(Msg, ?auth),
    case application:get_env(hypernumbers, sync_url) of
        {ok, SUrl} when SUrl /= Site ->
            Cmd = string:join(Cmd0, "/"),
            QReturn = mochiweb_util:quote_plus(Return),
            Attrs = case Stamp of
                        ?NO_STAMP -> "?return="++QReturn;
                        _Other    -> QStamp = mochiweb_util:quote_plus(Stamp),
                                     "?return="++QReturn++"&stamp="++QStamp
                    end,
            Redir = SUrl++"/_sync/"++Cmd++"/"++Attrs,
            {redir, Redir};
        _Else ->
            on_sync
    end.

-spec post_login(string(), auth_srv:uid(), string(), integer() | string(),
                 #env{}, string())
-> {#env{}, string()}.
post_login(Site, Uid, Stamp, Age, Env, Return) ->
    Cookie = hn_net_util:cookie("auth", Stamp, Age),
    Env2 = Env#env{uid = Uid, headers = [Cookie | Env#env.headers]},
    Redir = case try_sync(["tell"], Site, Return, Stamp) of
                on_sync    -> Return;
                {redir, R} -> R end,
    {Env2, Redir}.

process_sync(["tell"], E, QReturn, undefined) ->
    process_sync(["tell"], E, QReturn, []);
process_sync(["tell"], E, QReturn, QStamp) ->
    Stamp = case mochiweb_util:unquote(QStamp) of
                []    -> passport:temp_stamp();
                Other -> Other
            end,
    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
    Return = mochiweb_util:unquote(QReturn),
    Redirect = {"Location", Return},
    %Msg = io_lib:format("in tell (1) QReturn is ~p QStamp is ~p~n "
    %                    ++ " - Cookie is ~p Return is ~p~n",
    %                    [QReturn, QStamp, Cookie, Return]),
    %syslib:log(Msg, ?auth),
    E#env{headers = [Cookie, Redirect | E#env.headers]};
process_sync(["seek"], E=#env{mochi=Mochi}, QReturn, undefined) ->
    Stamp = case Mochi:get_cookie_value("auth") of
                undefined -> passport:temp_stamp();
                S         -> S
            end,
    %Msg = io_lib:format("in seek (1) Stamp is ~p and Cookie is ~p~n~n",
    %                    [Stamp, Mochi:get_cookie_value("auth")]),
    %syslib:log(Msg, ?auth),
    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
    Return = mochiweb_util:unquote(QReturn),
    #refX{site = OrigSite} = hn_util:url_to_refX(Return),
    QStamp = mochiweb_util:quote_plus(Stamp),
    Redir = hn_util:strip80(OrigSite) ++
        "/_sync/tell/?return="++QReturn++"&stamp="++QStamp,
    Redirect = {"Location", Redir},
    %Msg2 = io_lib:format("leaving seek (2) with Redir of ~p QStamp of ~p "
    %                     ++ "~n - and Cookie of ~p~n",
    %                    [Redir, QStamp, Cookie]),
    %syslib:log(Msg2, ?auth),
    E#env{headers = [Cookie, Redirect | E#env.headers]};
process_sync(["reset"], E, QReturn, undefined) ->
    Cookie = hn_net_util:kill_cookie("auth"),
    Return = mochiweb_util:unquote(QReturn),
    Redirect = {"Location", Return},
    E#env{headers = [Cookie, Redirect | E#env.headers]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Output Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'404'(#refX{site = Site}, Env) ->
    serve_html(404, Env, [hn_util:viewroot(Site), "/404.html"]).

'500'(Env) ->
    respond(500, Env).

respond(Code, #env{mochi = Mochi, headers = Headers}) ->
    Mochi:respond({Code, Headers, []}),
    ok.

text_html_nocache(#env{mochi = Mochi, headers = Headers}, Text) ->
    Mochi:ok({"text/html", Headers ++ nocache(), Text}),
    ok.

text_html(#env{mochi = Mochi, headers = Headers}, Text) ->
    Mochi:ok({"text/html", Headers, Text}),
    ok.

-spec json(#env{}, any()) -> any().
json(#env{mochi = Mochi, headers = Headers}, Data) ->
    Mochi:ok({"application/json",
              Headers ++ nocache(),
              (mochijson:encoder([{input_encoding, utf8}]))(Data)
             }),
    ok.

%% -spec json2(#env{}, any()) -> any().
%% json2(#env{mochi = Mochi, headers = Headers}, Data) ->
%%     Mochi:ok({"application/json",
%%               Headers ++ nocache(),
%%               (mochijson2:encoder([{utf8, true}]))(Data)
%%              }),
%%     ok.

-spec serve_html(#env{}, iolist()) -> any().
serve_html(Env, File) ->
    serve_html(200, Env, File).

-spec serve_html(integer(), #env{}, iolist()) -> any().
serve_html(Status, Env=#env{uid = Uid}, File) ->
    F = fun() -> hn_util:compile_html(File, get_lang(Uid)) end,
    Response = cache(File, File++"."++get_lang(Uid), F),
    serve_file(Status, Env, Response),
    ok.

serve_file(Status, #env{mochi = Mochi, headers = Headers}, File) ->
    case file:open(File, [raw, binary]) of
        {ok, IoDevice} ->
            Mochi:respond({Status,
                           Headers ++ nocache(),
                           {file, IoDevice}}),
            file:close(IoDevice);
        _ ->
            Mochi:not_found(Headers)
    end.

get_lang("anonymous") ->
    "en_gb";
get_lang(_User) ->
    "en_gb".
%% case hn_users:get(User, "language") of
%%     {ok, Lang} -> Lang;
%%     undefined  -> "en_gb"
%% end.

cache(Source, CachedNm, Generator) ->
    Cached = tmpdir() ++ "/" ++ mochihex:to_hex(erlang:md5(CachedNm)),
    ok = filelib:ensure_dir(Cached),
    case isnt_cached(Cached, Source) of
        true -> ok = file:write_file(Cached, Generator());
        _    -> ok
    end,
    Cached.

isnt_cached(Cached, Source) ->
    not( filelib:is_file(Cached) )
        orelse hn_util:is_older(Cached, Source).

nocache() ->
    [{"Cache-Control","no-store, no-cache, must-revalidate"},
     {"Expires",      "Thu, 01 Jan 1970 00:00:00 GMT"},
     {"Pragma",       "no-cache"}].

%% function to prevent doing an uncheck list_to_atom...
%% only converts lists if they are in the file system
site_type_exists(Type) ->
    Root = code:lib_dir(hypernumbers) ++ "/priv/site_types/*/",
    Paths = filelib:wildcard(Root),
    Rev = [lists:reverse(string:tokens(X, "/")) || X <- Paths],
    Types = [X || [X | _Rest] <- Rev],
    case lists:member(Type, Types) of
        true  -> list_to_atom(Type);
        false -> exit("invalid type being commissioned....")
    end.

passport_running() ->
    {ok, Services} = application:get_env(hypernumbers, services),
    case lists:keysearch(passport, 1, Services) of
        {value, {passport, false}} -> false;
        {value, {passport, true}}  -> true
    end.

request_pwd_reset(Email, Site) ->
    case hn_util:valid_email(Email) of
        false ->
            ?E("invalid e-mail in forgotten password ~p~n",
               [Email]),
            {"failure", "you supplied an invalid e-mail"};
        true ->
            case passport:is_user(Email) of
                false -> {"failure", Email ++ " is not a user"};
                true  -> ok = passport:issue_pwd_reset(Email, Site),
                         {"success", "Password reset sent. "
                          ++ "Please check your e-mail"}
            end
    end.

reset_password(Email, Password, Hash) ->
    case hn_util:valid_email(Email) of
        false ->
            ?E("invalid e-mail in forgotten password ~p~n",
               [Email]),
            {"failure", "you supplied an invalid e-mail"};
        true ->
            case passport:reset_pwd(Email, Password, Hash) of
                {success, Success} ->
                    HRef = "Password reset - <a href=\""++Success++"\">continue</a>",
                    {"success", HRef};
                {error, weak_password} ->
                    {"failure", "that password was too weak, try again."};
                {error, invalid_email} ->
                    {"failure", "invalid e-mail address"};
                {error, reset_not_issued} -> % this is the one
                    {"failure", "reset not issued"};
                {error, invalid_reset} ->
                    ?E("invalid reset attempt~nemail: ~p~nhash:~p~n",
                       [{email, Email}, {hash, Hash}]),
                    {"failure", "invalid attempt at resetting"};
                {error, expired_reset} ->
                    {ok, URL} = application:get_env(hypernumbers, reset_url),
                    Msg = "<a href=\"" ++ URL ++"\">try again</a>",
                    {"failure", "that reset has expired, "
                     ++ Msg}
            end
    end.


run_actions(#refX{site = S, path = P} = RefX, Env,
            {struct, [{_, {array, Json}}]}, Uid) ->
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
            ?E("invalid form submission~n""on:       ~p~n"
               ++ "Expected: ~p~nGot:      ~p~n",
               [RefX, Expected, Commands]),
            respond(403, Env);
        true ->
            % check that all the templates exists here!
            {Templates, Perms, Dest, Actions}
                = hn_webcontrols:make_actions(S, P, Commands),
            case templates_exist(S, Templates) of
                {error, Err} ->
                    ?E("Templates errors in postcreatepages: ~p~n", [Err]),
                    json(Env, {struct, [{"status", "err"}, {"response", Err}]});
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
                    json(Env, {struct, [{"status", "ok"}, {"redirect", Dest}]})
            end
    end.

process_perms(Site, Path, V, Gs, Uid) ->
    {ok, Email} = passport:uid_to_email(Uid),
    Gs2 = replace_user(Gs, Email, []),
    auth_srv:add_view(Site, Path, Gs2, V).

replace_user([], _EM, Groups)           -> Groups;
replace_user(["$user" | T], EM, Groups) -> replace_user(T, EM, [EM|  Groups]);
replace_user([H | T], EM, Groups)       -> replace_user(T, EM, [H | Groups]).

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

load_file(Ref, Data, File, Name, UserName, Uid) ->
    Type = get_type(Data),
    Ext = filename:extension(Name),
    % need to reauthorize
    case authorize_upload_again(Ref, Type, Uid) of
        true  -> load_file2(Ref, File, Name, UserName, Uid, Type, Ext);
        false -> Msg = "Permission denied: 401",
                 {rejected, {{struct, [{error, Msg}]}, nothing}}
    end.

load_file2(Ref, File, Name, UserName, Uid, Type, Ext) ->
    #refX{site = S, path = P} = Ref,
    NRef = Ref#refX{path = P ++ [make_name(Name, Ext)]},
    Loc = hn_util:list_to_path(P),

    try
        case {Type, Ext} of
            {file, ".xls"} ->
                ok = hn_file_upload:import(File, UserName, NRef,
                                           Name, Uid),
                {ok, { {struct, [{"location", Loc}]}, File}};
            {file, ".csv"} ->
                Url = NRef#refX.site ++ Loc,
                ok = hn_import:csv_file(Url, File),
                {ok, { {struct, [{"location", Loc}]}, File}};
            {{load_templates, Template}, _} ->
                case hn_import:load_template_file(File, Template, S) of
                    {error, Msg} ->
                        {ok, { {struct, [{error, Msg}]}, File}};
                    ok ->
                        {ok, { {struct, [{"location", Loc}]}, File}}
                end;
            {{row, Map}, _} ->
                Dir = hn_util:etlroot(S),
                MapFile = Dir ++ "/" ++ Map ++ ".map",
                case hn_import:etl_to_row(File, S, MapFile) of
                    {error, Msg} ->
                        {ok, { {struct, [{error, Msg}]}, File}};
                    {not_valid, Msg} ->
                        {ok, { {struct, [{error, Msg}]}, File}};
                    ok ->
                        {ok, { {struct, [{"location", Loc}]}, File}}
                end;
            {{custom, Map}, _} ->
                Dir = hn_util:etlroot(S),
                MapFile = Dir ++ "/" ++ Map ++ ".map",
                case hn_import:etl_to_custom(File, S, MapFile) of
                    {error, Msg} ->
                        {ok, { {struct, [{error, Msg}]}, File}};
                    {not_valid, Msg} ->
                        {ok, { {struct, [{error, Msg}]}, File}};
                    ok ->
                        {ok, { {struct, [{"location", Loc}]}, File}}
                end;
            {{sheet, Map, Page}, _} ->
                Dir = hn_util:etlroot(S),
                MapFile = Dir ++ "/" ++ Map ++ ".map",
                Page2 = case muin_util:walk_path(Ref#refX.path, Page) of
                            [] -> [];
                            Pg -> Pg
                        end,
                Page3 = Ref#refX.site ++ hn_util:list_to_path(Page2),
                case hn_import:etl_to_sheet(File, Page3, MapFile) of
                    {not_valid, Msg} ->
                        {ok, { {struct, [{error, Msg}]}, File}};
                    ok ->
                        {ok, { {struct, [{"location", Loc}]}, File}}
                end
        end
    catch
        _Type:Reason ->
            ?ERROR("Error Importing ~p ~n User:~p~n "
                   ++ "Reason:~p~n Stack:~p~n",
                   [File, UserName, Reason,
                    erlang:get_stacktrace()]),
            Error = case Reason of
                        {badmatch, {error, Err}} -> Err;
                        _                        -> Reason
                    end,
            {ok, { {struct, [{"error", Error}]},
                   undefined}}
    end.

get_type(Data) -> get_t2(lists:sort(Data)).

get_t2([]) ->
    file;
get_t2([{"load_templates", Template}]) ->
    {load_templates, Template};
get_t2([{"map", Map}, {"type", "row"}]) ->
    {row, Map};
get_t2([{"map", Map}, {"type", "custom"}]) ->
    {custom, Map};
get_t2([{"map", Map}, {"page", Page}, {"type", "sheet"}]) ->
    {sheet, Map, Page}.

expand_height(#refX{obj = {row, {Y1, Y1}}} = Ref, Attr, PAr, VAr) ->
    ok = new_db_api:write_attributes([{Ref, Attr}], PAr, VAr);
expand_height(#refX{obj = {row, {Y1, Y2}}} = Ref, Attr, PAr, VAr) ->
    NewRef = Ref#refX{obj = {row, {Y1, Y1}}},
    ok = new_db_api:write_attributes([{NewRef, Attr}], PAr, VAr),
    NewRef2 = Ref#refX{obj = {row, {Y1 + 1, Y2}}},
    expand_height(NewRef2, Attr, PAr, VAr).

expand_width(#refX{obj = {column, {X1, X1}}} = Ref, Attr, PAr, VAr) ->
    ok = new_db_api:write_attributes([{Ref, Attr}], PAr, VAr);
expand_width(#refX{obj = {column, {X1, X2}}} = Ref, Attr, PAr, VAr) ->
    NewRef = Ref#refX{obj = {column, {X1, X1}}},
    ok = new_db_api:write_attributes([{NewRef, Attr}], PAr, VAr),
    NewRef2 = Ref#refX{obj = {column, {X1 + 1, X2}}},
    expand_width(NewRef2, Attr, PAr, VAr).

make_name(Name, Ext) ->
    Basename = string:to_lower(filename:basename(Name, Ext)),
    re:replace(Basename,"\s","_",[{return,list}, global]).

expand_binaries({struct, [{"time", Time}, {"msgs", {array, List}}]}) ->
    List2 = [binary_to_term(X) || X <- List],
    {struct, [{"time", Time}, {"msgs", {array, List2}}]}.

make_demo(Site, Path) ->
    URL = Site ++ hn_util:list_to_path(Path),
    "<!DOCTYPE html>
<html lang='en'>
        <head>
        <title>Hypernumbers Demo</title>
        <meta charset='utf-8' >
        <link rel='stylesheet' href='/hypernumbers/hn.style.css' />
        </head>
        <body class='hn_demopage' height='100%'>
        <div class='hn_demointro'>This demo shows how you can have different views of the same spreadsheet page. This makes it possible to build robust multiuser systems - by using different views for different people on different pages. Enter data into the spreadsheet or wiki view to see these views in action. Views are managed by the <em>Views</em> menu at the right-hand side of the spreadsheet menu bar.<br /><span id='hn_demobreakout'>To breakout of this demo and start your one month free trial <a href='./#tour'>click here</a></span></div>
<div class='hn_demopadding'>
<iframe id='hn_spreadsheet' src='"++URL++"?view=spreadsheet'></iframe>
</div>
<div class='hn_demopadding'>
<table width='100%'>
<tr>
<td>
<div class='hn_demoblurb'>This is the wiki view - it is used to provide locked down data entry - one page for each person or department.</div>
<iframe id='hn_wikipage' width='95%' src='"++URL++"?view=wikipage' /></iframe>
</td>
<td>
<div class='hn_demoblurb'>This is the webpage view - it is used to give people read-only access to analysis or results that your spreadsheet provides.</div>
<iframe id='hn_webpage' width='95%' src='"++URL++"?view=webpage' /></iframe>
</td>
</tr>
</table>
</div>
</body>
<!--<script src='http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js'></script>-->
<script src='/hypernumbers/jquery-1.4.2.min.js'></script>
<script src='/hypernumbers/hn.demopage.js'></script>
</html>".

make_preview(Type, Site, Path) ->
    URL = Site ++ hn_util:list_to_path(Path),
    "<!DOCTYPE html>
<html lang='en'>
        <head>
        <title>Hypernumbers " ++hn_util:capitalize_name(Type)++" Preview</title>
        <link rel='stylesheet' href='/hypernumbers/hn.style.css' />
        <meta charset='utf-8' >
        </head>
        <body class='hn_demopage'>
        <div class='hn_demointro'>To breakout of this preview <a href='./?view=spreadsheet'>click here</a></div>
        <div class='hn_demopadding'>
        <iframe class='hn_preview' src='"++URL++"?view=spreadsheet'></iframe>
        <br />
        <iframe class='hn_preview' src='"++URL++"?view="++Type++"page' /></iframe>
        </div>
        </body>
        <!--<script src='http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js'></script>-->
        <script src='/hypernumbers/jquery-1.4.2.min.js'></script>
        <script src='/hypernumbers/hn.demopage.js'></script>
        </html>".

%% catch script kiddie attempts and write them as info not error logs
%% makes rb usable
log_path_errors({path, Path}, Format, Msg) when
                                               Path == "/phpMyAdmin-2.6.0-pl1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.0-pl2/scripts/setup.php";
Path == "/phpMyAdmin-2.6.0-pl3/scripts/setup.php";
Path == "/phpMyAdmin-2.6.0-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.0-rc2/scripts/setup.php";
Path == "/phpMyAdmin-2.6.1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.1-pl1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.1-pl2/scripts/setup.php";
Path == "/phpMyAdmin-2.6.1-pl3/scripts/setup.php";
Path == "/phpMyAdmin-2.6.2-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.2-beta1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.2-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.2/scripts/setup.php";
Path == "/phpMyAdmin-2.6.2-pl1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.3/scripts/setup.php";
Path == "/phpMyAdmin-2.6.3-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.3-pl1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.4-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.4-pl1/scripts/setup.php";
Path == "/phpMyAdmin-2.6.4-pl2/scripts/setup.php";
Path == "/phpMyAdmin-2.6.4-pl3/scripts/setup.php";
Path == "/phpMyAdmin-2.6.4-pl4/scripts/setup.php";
Path == "/phpMyAdmin-2.6.4/scripts/setup.php";
Path == "/phpMyAdmin-2.7.0/scripts/setup.php";
Path == "/phpMyAdmin-2.7.0-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.7.0-pl1/scripts/setup.php";
Path == "/phpMyAdmin-2.7.0-pl2/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0-beta1/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0-rc2/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0.1/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0.2/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0.3/scripts/setup.php";
Path == "/phpMyAdmin-2.8.0.4/scripts/setup.php";
Path == "/phpMyAdmin-2.8.1-rc1/scripts/setup.php";
Path == "/phpMyAdmin-2.8.1/scripts/setup.php";
Path == "/phpMyAdmin-2.8.2/scripts/setup.php";
Path == "/sqlmanager/scripts/setup.php";
Path == "/mysqlmanager/scripts/setup.php";
Path == "/p/m/a/scripts/setup.php";
Path == "/PMA2005/scripts/setup.php";
Path == "/pma2005/scripts/setup.php";
Path == "/phpmanager/scripts/setup.php";
Path == "/php-myadmin/scripts/setup.php";
Path == "/phpmy-admin/scripts/setup.php";
Path == "/webadmin/scripts/setup.php";
Path == "/sqlweb/scripts/setup.php";
Path == "/websql/scripts/setup.php"
->
    error_logger:info_msg(Format, Msg);
log_path_errors(_Path, Format, Msg) ->
    error_logger:error_msg(Format, Msg).
