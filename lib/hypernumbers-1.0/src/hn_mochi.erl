%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_mochi).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-include_lib("kernel/include/file.hrl").
-include("gettext.hrl").
-include("auth.hrl").
-include("hn_mochi.hrl").

-export([ handle/1,
          styles_to_css/2,
          style_to_css/2,
          docroot/1,
          page_attributes/2,
          get_json_post/1, % Used for mochilog replay rewrites
          save_view/3
         ]).

-define(SHEETVIEW, "_g/core/spreadsheet").

-include("util.hrl").

-spec handle(any()) -> ok.
handle(MochiReq) ->
    try 
        Ref = hn_util:parse_url(get_real_uri(MochiReq)),
        Env = process_environment(MochiReq),
        Qry = process_query(Env),
        handle_(Ref, Env, Qry)
    catch
        ok          -> ok;
        exit:normal -> exit(normal); 
        Type:What   ->
            Report = ["web request failed",
                      {path, MochiReq:get(path)},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            '500'(MochiReq) 
    end.

-spec handle_(#refX{}, #env{}, #qry{}) -> ok. 

handle_(#refX{site="http://www."++Site}, E=#env{mochi=Mochi}, _Qry) ->
    Redir = "http://" ++ hn_util:strip80(Site) ++ Mochi:get(raw_path),
    Redirect = {"Location", Redir},
    respond(301, E#env{headers = [Redirect | E#env.headers]});

handle_(#refX{path=["_sync" | Cmd]}, Env, #qry{return=Return}) 
  when Return /= undefined ->
    Env2 = process_sync(Cmd, Env, Return),
    respond(302, Env2);

handle_(Ref, Env, Qry) ->
    case filename:extension((Env#env.mochi):get(path)) of
        []  -> check_resource_exists(Env, Ref, Qry);
        Ext -> Root = docroot(Ref#refX.site),
               handle_static(Ext, Root, Env#env.mochi)
    end.

-spec check_resource_exists(#env{}, #refX{}, #qry{}) -> no_return(). 
check_resource_exists(Env, Ref, Qry) ->
    case hn_setup:site_exists(Ref#refX.site) of
        true -> authorize_resource(Env, Ref, Qry);
        false -> text_html(Env, 
                           "The web site you seek<br/>"
                           "cannot be located, but<br/>"
                           "countless more exist.")
    end.
            
-spec authorize_resource(#env{}, #refX{}, #qry{}) -> no_return(). 
authorize_resource(Env, Ref, Qry) -> 
    Env2 = process_user(Ref#refX.site, Env),
    AuthRet = case Env2#env.method of
                  'GET'  -> authorize_get(Ref, Qry, Env2);
                  'POST' -> authorize_post(Ref, Qry, Env2)
              end,
    
    case {AuthRet, Env2#env.accept} of
        {allowed, _} ->
            handle_resource(Ref, Qry, Env2);
        {{view, View}, _} ->
            handle_resource(Ref, Qry#qry{view = View}, Env2);
        {not_found, html} ->
            serve_html(404, Env2, 
                       [viewroot(Ref#refX.site), "/_g/core/404.html"]);
        {denied, html} ->
            serve_html(401, Env2,
                       [viewroot(Ref#refX.site), "/_g/core/login.html"]);
        {not_found, json} ->
            respond(404, Env2);
        _NoPermission ->
            respond(401, Env2)
    end.

handle_resource(Ref, Qry, Env=#env{method = 'GET'}) ->
    mochilog:log(Env, Ref),
    ObjType = element(1, Ref#refX.obj),
    iget(Ref, ObjType, Qry, Env);

handle_resource(Ref, _Qry, 
                Env=#env{method='POST', body=multipart,
                         mochi=Mochi, uid=Uid}) ->
    {Data, File} = hn_file_upload:handle_upload(Mochi, Ref, Uid),
    Name = filename:basename(File),
    Env2 = Env#env{raw_body = {upload, Name}},
    mochilog:log(Env2, Ref),
    Mochi:ok({"text/html", 
              (mochijson:encoder([{input_encoding, utf8}]))(Data)});

handle_resource(Ref, Qry, Env=#env{method = 'POST'}) ->
    mochilog:log(Env, Ref),
    ipost(Ref, Qry, Env).


-spec handle_static(string(), iolist(), any()) -> any(). 
handle_static(".tpl", Root, Mochi) ->
    %% Don't cache templates
    "/"++RelPath = Mochi:get(path),
    Mochi:serve_file(RelPath, Root, nocache()),
    ok;
handle_static(X, Root, Mochi)
  when X == ".png"; X == ".jpg"; X == ".css"; X == ".js"; X == ".txt";
       X == ".ico"; X == ".json"; X == ".gif"; X == ".html"; X == ".pdf" ->
    "/"++RelPath = Mochi:get(path),
    Mochi:serve_file(RelPath, Root),
    ok.

-spec authorize_get(#refX{}, #qry{}, #env{}) 
                   -> {view, string()} | allowed | denied | not_found.

%% Specifically allow access to the json permissions. Only the permissions,
%% query may be present.
%% TODO: Only admins should be able to do this...
authorize_get(_Ref, 
              #qry{permissions = [], _ = undefined}, 
              #env{accept = html}) ->
    allowed;

authorize_get(#refX{path = [X | _]}, _Qry, #env{accept = html}) 
  when X == "_invite"; 
       X == "_mynewsite"; 
       X == "_validate";
       X == "_hooks" ->
    allowed;

%% Authorize update requests, when the update is targeted towards a
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
                true -> allowed;
                _Else -> denied
            end;       
        _Else ->
            denied
    end;

%% TODO : Broken
authorize_get(#refX{site = Site, path = Path}, 
              #qry{updates = U, view = "_g/core/webpage", paths = More}, 
              #env{accept = json, uid = Uid})
  when U /= undefined ->
    case auth_srv:check_particular_view(Site, Path, Uid, "_g/core/webpage") of
        {view, "_g/core/webpage"} ->
            MoreViews = [auth_srv:get_any_view(Site, string:tokens(P, "/"), Uid) 
                         || P <- string:tokens(More, ",")],
            case lists:all(fun({view, _}) -> true; 
                              (_) -> false end, 
                           MoreViews) of
                true -> allowed;
                _Else -> denied
            end;       
        _Else ->
            denied
    end;


%% Update requets targeted towards a non-spreadsheet view. Validation
%% for additional sources is made against the security object created
%% at a 'view-save-time'. 
authorize_get(#refX{site = Site, path = Path}, 
              #qry{updates = U, view = View, paths = More},
              #env{accept = json, uid = Uid}) 
  when U /= undefined, View /= undefined -> 
    case auth_srv:check_particular_view(Site, Path, Uid, View) of
        {view, View} ->
            {ok, [Sec]} = file:consult([viewroot(Site), "/", View, ".sec"]),
            Results = [hn_security:validate_get(Sec, Path, P) 
                       || P <- string:tokens(More, ",")],
            case lists:all(fun(X) -> X end, Results) of 
                true -> allowed;
                false -> denied
            end;
        _Else ->
            denied
    end;

%% Access to secondary data sources, described by some initial view
%% declared herein as 'via'.
authorize_get(#refX{site = Site, path = Path}, 
              #qry{view = View, via = Via}, 
              #env{accept = json, uid = Uid})
  when View /= undefined, View /= ?SHEETVIEW, Via /= undefined ->
    Base = string:tokens(Via, "/"),
    case auth_srv:check_particular_view(Site, Base, Uid, View) of
        {view, View} ->
            Target = hn_util:list_to_path(Path),
            {ok, [Sec]} = file:consult([viewroot(Site), "/", View, ".sec"]),
            case hn_security:validate_get(Sec, Base, Target) of
                true -> allowed; 
                false -> denied
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

-spec authorize_post(#refX{}, #qry{}, #env{}) -> allowed | denied.

%% Allow logins to occur.
authorize_post(#refX{path = ["_user", "login"]}, _Qry, #env{accept = json}) ->
    allowed;

authorize_post(#refX{site=_, path=["_hooks"]}, _Qry, #env{accept=json}) ->
    allowed;

authorize_post(#refX{site = Site, path = ["_admin"]}, _Qry, 
               #env{accept = json, uid = Uid}) ->
    case hn_groups:is_member(Uid, Site, ["admin"]) of
        true  -> allowed;
        false -> denied
    end;

%% Authorize posts against non spreadsheet views. The transaction
%% attempted is validated against the view's security model.
authorize_post(Ref=#refX{site = Site, path = Path}, #qry{view = View}, Env)
  when View /= undefined ->
    case auth_srv:check_particular_view(Site, Path, Env#env.uid, View) of
        {view, View} ->
            {ok, [Sec]} = file:consult([viewroot(Site), "/", View, ".sec"]),
            case hn_security:validate_trans(Sec, Ref, Env#env.body) of
                true -> allowed;
                false -> denied
            end;
        _ -> denied
    end;

%% Allow a post to occur, if the user has access to a spreadsheet on
%% the target.  the actual operation may need further validation, so
%% flag as 'allowed_pending'.
authorize_post(#refX{site = Site, path = Path}, _Qry, Env) ->
    case auth_srv:check_particular_view(
           Site, Path, Env#env.uid, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> allowed;
        _ -> denied
    end.

-spec iget(#refX{}, 
           page | cell | row | column | range,
           #qry{},
           #env{}) 
          -> any(). 

iget(Ref=#refX{path=["_user", "login"]}, page, _Qry, Env) ->
    iget(Ref, page, #qry{view = "_g/core/login"}, Env);

iget(#refX{site=Site, path=[X, _Vanity | Rest]=Path}, page, 
     #qry{hypertag=HT}, 
     Env) when X == "_invite"; X == "_mynewsite" ->
    case passport:open_hypertag(Site, Path, HT) of
        {ok, Uid, _Email, _Data, Stamp, Age} ->
            Cookie = hn_net_util:cookie("auth", Stamp, Age),
            Target = hn_util:strip80(Site) ++ hn_util:list_to_path(Rest),
            Redirect = {"Location", Target},
            Headers = [Cookie, Redirect | Env#env.headers],
            respond(302, Env#env{uid = Uid, headers = Headers}),
            throw(ok);
        {error, E} ->
            %% handle graceufully, what about time outs?
            throw(E)
    end;

iget(#refX{site=Site, path=[X, _Vanity | Rest]=Path}, page, 
     #qry{hypertag=HT}, 
     Env) when X == "_validate" ->
    case passport:open_hypertag(Site, Path, HT) of
        {ok, Uid, _Email, Data, Stamp, Age} ->
            case proplists:get_value(emailed, Data) of
                true -> 
                    ok = passport:validate_uid(Uid),
                    Cookie = hn_net_util:cookie("auth", Stamp, Age),
                    Target = hn_util:strip80(Site) ++ hn_util:list_to_path(Rest),
                    Redirect = {"Location", Target},
                    Headers = [Cookie, Redirect | Env#env.headers],
                    respond(302, Env#env{uid = Uid, headers = Headers}),
                    throw(ok);                    
                _Else -> 
                    throw(bad_validation)
            end
    end;

iget(#refX{site = Site}, page, #qry{view = FName, template = []}, Env) 
  when FName /= undefined -> 
    serve_html(Env, [viewroot(Site), "/", FName, ".tpl"]);    

iget(Ref=#refX{site = Site}, page, #qry{view = FName}, Env=#env{accept = html})
  when FName /= undefined ->
    Tpl  = [viewroot(Site), "/", FName, ".tpl"],
    Html = [viewroot(Site), "/", FName, ".html"],
    ok = case should_regen(Tpl, Html) of
             true -> ok = build_tpl(Site, FName);
             _    -> ok
         end,
    case filelib:is_file(Html) of
        true  -> serve_html(Env, Html);
        false -> '404'(Ref, Env)
    end;

iget(#refX{site = Site, path = Path}, page, 
     #qry{updates = Time, paths = More}, Env=#env{accept = json})
  when Time /= undefined, More /= undefined ->
    Paths = [Path | [ string:tokens(X, "/") || X<-string:tokens(More, ",")]],
    remoting_request(Env, Site, Paths, Time);

iget(Ref, page, #qry{renderer=[]}, Env) ->
    Html = hn_render:page(Ref),
    text_html(Env, Html);

iget(#refX{site = S}, page, #qry{status = []}, Env) -> 
    json(Env, status_srv:get_status(S));

iget(#refX{site = S}=Ref, page, #qry{rawview = View}, Env)
  when is_list(View) ->

    case is_view(View) of
        true ->
            Tpl = [viewroot(S), "/" , View, ".tpl"],
            serve_file(200, Env, Tpl);
        false -> 
            '404'(Ref, Env)
    end,
    ok;

iget(#refX{site = S, path  = P}, page, #qry{permissions = []}, Env) ->
    json(Env, auth_srv:get_as_json(S, P));

iget(Ref, page, #qry{pages = []}, Env=#env{accept = json}) ->
    json(Env, pages(Ref));

iget(Ref, page, #qry{views = []}, Env=#env{accept = json}) ->

    AllViews = filelib:wildcard(viewroot(Ref#refX.site)++"/*/*/*.tpl"),

    Views = [ begin
                  [Name, U, G | _Rest ] = lists:reverse(string:tokens(X, "/")),
                  NewName = filename:basename(Name, ".tpl"),
                  G ++ "/" ++ U ++ "/" ++ NewName
              end || X <- AllViews ],
    json(Env, {array, [ X || X<-Views, X=/="_g/core/built" ] });

%% List of template pages
iget(Ref, page, #qry{templates = []}, Env) ->
    Fun = fun(X) ->
                  [F | _T] = lists:reverse(string:tokens(X, "/")),
                  case F of
                      [$. | _T1] -> true;
                      _          -> false
                  end
          end,
    Files = lists:dropwhile(
              Fun,
              filelib:wildcard(docroot(Ref#refX.site)++"/templates/*")),
    File = [filename:basename(X) || X <- Files], 
    json(Env, {array, File});

iget(Ref, page, _Qry, Env=#env{accept = json, uid = Uid}) ->
    json(Env, page_attributes(Ref, Uid));

iget(Ref, cell, _Qry, Env=#env{accept = json}) ->
    V = case hn_db_api:read_attributes(Ref,["value"]) of
            [{_Ref, {"value", Val}}] when is_atom(Val) ->
                atom_to_list(Val);
            [{_Ref, {"value", {datetime, D, T}}}] ->
                dh_date:format("Y/m/d H:i:s",{D,T});
            [{_Ref, {"value", {errval, Val}}}] ->
                atom_to_list(Val);
            [{_Ref, {"value", Val}}] ->
                Val;
            _Else ->
                error_logger:error_msg("unmatched ~p~n", [_Else]),
                ""
        end,
    json(Env, V);

iget(Ref, Type, _Qry, Env=#env{accept=json})
  when Type == range;
       Type == column;
       Type == row ->
    Init = [["cell"], ["column"], ["row"], ["page"]],
    Tree = dh_tree:create(Init),
    Dict = to_dict(hn_db_api:read_attributes(Ref,[]), Tree),
    json(Env, {struct, dict_to_struct(Dict)});

iget(#refX{site = Site}, cell, _Qry, Env=#env{accept=html}) ->
    HTML = [viewroot(Site), "/", "_g/core/cell.html"],
    serve_html(Env, HTML);

iget(Ref, _Type, Qry, Env) ->
    error_logger:error_msg("404~n-~p~n-~p~n", [Ref, Qry]),
    '404'(Ref, Env).


-spec ipost(#refX{}, #qry{}, #env{}) -> any().

ipost(Ref=#refX{site = S, path = P}, _Qry, 
      Env=#env{body = [{"drag", {_, [{"range", Rng}]}}],
               uid = Uid}) ->
    ok = status_srv:update_status(Uid, S, P, "edited page"),
    hn_db_api:drag_n_drop(Ref, 
                          Ref#refX{obj = hn_util:parse_attr(range,Rng)},
                          Uid),
    json(Env, "success");

ipost(#refX{path=["_user","login"]}, _Qry, E) ->
    [{"email", Email0},{"pass", Pass},{"remember", Rem}] = E#env.body,
    Email = string:to_lower(Email0),
    {E2, Resp} = case passport:authenticate(Email, Pass, Rem=="true") of
                     {error, authentication_failed} -> 
                         {E, "error"};
                     {ok, Uid, Stamp, Age} ->
                         Cookie = hn_net_util:cookie("auth", Stamp, Age),
                         {E#env{uid = Uid,
                                headers = [Cookie | E#env.headers]},
                          "success"}
                 end,
    json(E2, {struct, [{"response", Resp}]});

%% the purpose of this message is to mark the mochilog so we don't 
%% need to do nothing with anything...
ipost(_Ref, #qry{mark = []}, 
      Env=#env{body = [{"set",{struct, [{"mark", _Msg}]}}]}) ->
    json(Env, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"insert", "before"}], uid = Uid})
  when O == row orelse O == column ->
    ok = hn_db_api:insert(Ref, Uid),
    json(Env, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"insert", "after"}], uid = Uid})
  when O == row orelse O == column ->
    ok = hn_db_api:insert(make_after(Ref), Uid),
    json(Env, "success");

%% by default cells and ranges displace vertically
ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"insert", "before"}], uid = Uid})
  when O == cell orelse O == range ->
    ok = hn_db_api:insert(Ref, vertical, Uid),
    json(Env, "success");

%% by default cells and ranges displace vertically
ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"insert", "after"}], uid = Uid})
  when O == cell orelse O == range ->
    ok = hn_db_api:insert(make_after(Ref), Uid),
    json(Env, "success");

%% but you can specify the displacement explicitly
ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"insert", "before"}, {"displacement", D}],
               uid = Uid})
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    ok = hn_db_api:insert(Ref, list_to_existing_atom(D), Uid),
    json(Env, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"insert", "after"}, {"displacement", D}],
               uid = Uid})
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    RefX2 = make_after(Ref),
    ok = hn_db_api:insert(RefX2, list_to_existing_atom(D), Uid),
    json(Env, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"delete", "all"}], uid = Uid}) 
  when O == page ->
    ok = hn_db_api:delete(Ref, Uid),
    json(Env, "success");

ipost(Ref, _Qry, 
      Env=#env{body=[{"delete", "all"}],
               uid = Uid}) ->
    ok = hn_db_api:delete(Ref, Uid),
    json(Env, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"delete", Direction}],
               uid = Uid})
  when O == cell orelse O == range,
       Direction == "horizontal" orelse Direction == "vertical" ->
    ok = hn_db_api:delete(Ref, list_to_atom(Direction), Uid),
    json(Env, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      Env=#env{body=[{"insert", Direction}],
               uid = Uid})
  when O == cell orelse O == range,
       Direction == "horizontal" orelse Direction == "vertical" ->
    ok = hn_db_api:insert(Ref, list_to_atom(Direction), Uid),
    json(Env, "success");


ipost(Ref, 
      _Qry,
      Env=#env{body=[{"copy", {struct, [{"src", Src}]}}],
               uid = Uid}) ->
    ok = hn_db_api:copy_n_paste(hn_util:parse_url(Src), Ref, Uid),
    json(Env, "success");

ipost(#refX{obj = {range, _}} = Ref, _Qry, 
      Env=#env{body=[{"borders", {struct, Attrs}}]}) ->
    Where = from("where", Attrs),
    Border = from("border", Attrs),
    Border_Style = from("border_style", Attrs),
    Border_Color = from("border_color", Attrs),
    ok = hn_db_api:set_borders(Ref, Where, Border, Border_Style, Border_Color),
    json(Env, "success");

ipost(_Ref, _Qry,
      Env=#env{body = [{"set", {struct, [{"language", _Lang}]}}], 
               uid = anonymous}) ->
    S = {struct, [{"error", "cant set language for anonymous users"}]},
    json(Env, S);

ipost(#refX{site = _Site, path=["_user"]}, _Qry, 
      _Env=#env{body = [{"set", {struct, [{"language", _Lang}]}}], 
               uid = _Uid}) ->
    throw("can't set language right now");
    %% ok = hn_users:update(Site, Uid, "language", Lang),
    %% json(Env, "success");

ipost(#refX{site = S, path = P}, Qry, 
      Env=#env{body = [{"set", {struct, [{"list", {array, Array}}]}}], 
               uid = PosterUid}) ->
    ViewUid = view_creator_uid(Qry#qry.view, S, PosterUid),
    ok = status_srv:update_status(PosterUid, S, P, "edited page"),
    {Lasts, Refs} = fix_up(Array, S, P),
    ok = hn_db_api:write_last(Lasts, PosterUid, ViewUid),
    ok = hn_db_api:write_attributes(Refs, PosterUid, ViewUid),
    json(Env, "success");

ipost(#refX{site = S, path = P, obj = O} = Ref, Qry, 
      Env=#env{body = [{"set", {struct, Attr}}], 
               uid = PosterUid}) ->
    Type = element(1, O),
    ViewUid = view_creator_uid(Qry#qry.view, S, PosterUid),
    ok = status_srv:update_status(PosterUid, S, P, "edited page"),
    case Attr of
        %% TODO : Get Rid of this (for pasting a range of values)
        [{"formula",{array, Vals}}] ->
            post_range_values(Ref, Vals, PosterUid, ViewUid);

        %% if posting a formula to a row or column, append
        [{"formula", Val}] when Type == column; Type == row ->
            ok = hn_db_api:write_last([{Ref, Val}], PosterUid, ViewUid);

        _Else ->
            ok = hn_db_api:write_attributes([{Ref, Attr}], PosterUid, ViewUid)
    end,
    json(Env, "success");

ipost(Ref, _Qry, 
      Env=#env{body = [{"clear", What}],
               uid = Uid}) 
  when What == "contents"; What == "style"; What == "all" ->
    ok = hn_db_api:clear(Ref, list_to_atom(What), Uid),
    json(Env, "success");

ipost(#refX{site=Site, path=Path} = Ref, _Qry,
      Env=#env{body = [{"saveview", {struct, [{"name", Name}, {"tpl", Form},
                                              {"overwrite", OverWrite}]}}], 
               uid = Uid}) ->
    TplPath = [viewroot(Site), "/" , Name, ".tpl"],
    ok      = filelib:ensure_dir([viewroot(Site), "/" , Name]),

    case (OverWrite == false) andalso filelib:is_file(TplPath) of
        true ->
            json(Env, "error");
        false ->
            AuthSpec = ["user"],
            ok       = save_view(Site, Name, Form, Uid, Ref),
            ok       = auth_srv:add_view(Site, Path, AuthSpec, Name),
            json(Env, "success")
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back_create handler                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Qry,
      Env=#env{body = [{"action", "notify_back_create"}|T]}) ->

    Biccie   = from("biccie",     T),
    Proxy    = from("proxy",      T),
    ChildUrl = from("child_url",  T),
    PVsJson  = from("parent_vsn", T),
    CVsJson  = from("child_vsn",  T),
    Stamp    = from("stamp",      T),

    #refX{site = Site} = Ref,
    ParentX = Ref,
    _ParentUrl = hn_util:refX_to_url(ParentX),    
    ChildX = hn_util:url_to_refX(ChildUrl),

    %% there is only 1 parent and 1 child for this action
    PVsn = json_util:unjsonify(PVsJson),
    CVsn = json_util:unjsonify(CVsJson),
    %% #version{page = PP, version = PV} = PVsn,
    %% #version{page = CP, version = CV} = CVsn,
    Sync1 = hn_db_api:check_page_vsn(Site, PVsn),
    Sync2 = hn_db_api:check_page_vsn(Site, CVsn),
    case Sync1 of
        synched         -> ok;
        unsynched       -> hn_db_api:resync(Site, PVsn);
        not_yet_synched -> ok % the child gets the version in this call...
    end,
    case Sync2 of
        synched         -> ok;
        unsynched       -> hn_db_api:resync(Site, CVsn);
        not_yet_synched -> sync_exit()
    end,
    {struct, Return} = hn_db_api:register_hn_from_web(ParentX, ChildX, 
                                                      Proxy, Biccie),
    Return2 = lists:append([Return, [{"stamp", Stamp}]]),
    json(Env, {struct, Return2});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handler                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Qry,
      Env=#env{body = [{"action", "notify_back"} |T] = _Json}) ->
    Biccie    = from("biccie",     T),
    ChildUrl  = from("child_url",  T),
    ParentUrl = from("parent_url", T),
    Type      = from("type",       T),
    PVsJson   = from("parent_vsn", T),
    CVsJson   = from("child_vsn",  T),
    Stamp     = from("stamp",      T),

    %% there is only 1 parent and 1 child here
    PVsn = json_util:unjsonify(PVsJson),
    CVsn = json_util:unjsonify(CVsJson),
    %% #version{page = PP, version = PV} = PVsn,
    %% #version{page = CP, version = CV} = CVsn,
    ChildX = hn_util:url_to_refX(ChildUrl),
    ParentX = hn_util:url_to_refX(ParentUrl),
    #refX{site = Site} = Ref,
    Sync1 = hn_db_api:check_page_vsn(Site, CVsn),
    Sync2 = hn_db_api:check_page_vsn(Site, PVsn),
    case Sync1 of
        synched -> 
            ok = hn_db_api:notify_back_from_web(ParentX, ChildX,
                                                Biccie, Type);
        unsynched -> 
            hn_db_api:resync(Site, PVsn);
        not_yet_synched -> 
            ok = hn_db_api:initialise_remote_page_vsn(Site, PVsn)
    end,
    case Sync2 of
        synched -> ok;
        unsynched -> 
            ok = hn_db_api:resync(Site, CVsn);
        not_yet_synched -> 
            ok = hn_db_api:initialise_remote_page_vsn(Site, CVsn)
    end,
    S = {struct, [{"result", "success"}, {"stamp", Stamp}]},
    json(Env, S);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify handler                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Qry, 
      Env=#env{body = [{"action", "notify"} | T] = _Json}) ->
    Biccie    = from("biccie",     T),
    ParentUrl = from("parent_url", T),
    Type      = from("type",       T),
    Payload   = from("payload",    T),
    PVsJson   = from("parent_vsn", T),
    CVsJson   = from("child_vsn",  T),
    Stamp     = from("stamp",      T),

    ParentX = hn_util:url_to_refX(ParentUrl),
    ChildX = Ref,
    _ChildUrl = hn_util:refX_to_url(ChildX),

    #refX{site = Site} = ChildX,
    PVsn = json_util:unjsonify(PVsJson),
    CVsn = json_util:unjsonify(CVsJson),
    %%#version{page = PP, version = PV} = PVsn,

    Sync1 = case Type of
                "insert"    -> hn_db_api:incr_remote_page_vsn(Site, PVsn, Payload);
                "delete"    -> hn_db_api:incr_remote_page_vsn(Site, PVsn, Payload);
                "new_value" -> hn_db_api:check_page_vsn(Site, PVsn)
            end,
    %% there is one parent and it if is out of synch, then don't process it, ask for a
    %% resynch
    case Sync1 of
        synched -> 
            ok = hn_db_api:notify_from_web(ParentX, Ref, Type,
                                           Payload, Biccie);
        unsynched -> 
            ok = hn_db_api:resync(Site, PVsn);
        not_yet_synched -> 
            sync_exit()
    end,
    %% there are 1 to many children and if they are out of synch ask for 
    %% a resynch for each of them
    Fun =
        fun(X) ->
                Sync2 = hn_db_api:check_page_vsn(Site, X),
                %% #version{page = CP, version = CV} = X,
                case Sync2 of
                    synched         -> ok;
                    unsynched       -> ok = hn_db_api:resync(Site, X);
                    not_yet_synched -> sync_exit()
                end
        end,
    [Fun(X) || X <- CVsn],
    S = {struct, [{"result", "success"}, {"stamp", Stamp}]},
    json(Env, S);

ipost(#refX{site = Site, path = _P}, _Qry,
      Env=#env{body = [{"admin", Json}], uid = Uid}) ->
    {struct,[{Fun, {struct, Args}}]} = Json,

    case hn_web_admin:rpc(Uid, Site, Fun, Args) of
        ok ->
            json(Env, {struct, [{"result", "success"}]});
        {error, Reason} ->
            json(Env, {struct, [{"result", "error"}, {"reason", Reason}]})
    end; 
    
ipost(#refX{site=_Site, path=["_hooks"]}, _Qry, Env=#env{body=Body}) ->
    [{"signup",{struct,[{"email",Email0}]}}] = Body,
    Email = string:to_lower(Email0),
    Zone = case application:get_env(hypernumbers, environment) of
               {ok, development} -> "hypernumbers.dev";
               {ok, production}  -> "tiny.hn"
           end,
    Type = demo,
    case factory:provision_site(Zone, Email, Type) of
        {ok, new, Site, Uid, Name} ->
            Opaque = [],
            Expiry = "never",
            Url = passport:create_hypertag(Site, ["_mynewsite", Name], 
                                           Uid, Email, Opaque, Expiry),
            json(Env, {struct, [{"result", "success"}, {"url", Url}]});
        {ok, existing, Site, _Uid, _Name} ->
            json(Env, {struct, [{"result", "success"}, {"url", Site}]});
        {error, Reason} ->
            Str = case Reason of
                      %bad_email ->
                      invalid_email ->
                          "Sorry, the email provided was invalid, "
                              "please try again.";
                      bad_provision ->
                          "Sorry there was an unknown error, please "
                              "try again."
                  end,
            json(Env, {struct, [{"result", "error"}, {"reason", Str}]})
    end;

ipost(Ref, Qry, Env) ->
    error_logger:error_msg("404~n-~p~n-~p~n",[Ref, Qry]),
    '404'(Ref, Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Helpers
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

view_creator_uid(undefined, _Site, Poster) -> Poster; 
view_creator_uid(?SHEETVIEW, _Site, Poster) -> Poster;
view_creator_uid(View, Site, _Poster) ->
    {ok, [Meta]} = file:consult([viewroot(Site), "/", View, ".meta"]),
    proplists:get_value(authreq, Meta).

%% Some clients dont send ip in the host header
get_real_uri(Env) ->
    Host = case Env:get_header_value("HN-Host") of
               undefined ->
                   lists:takewhile(fun(X) -> X /= $: end,
                                   Env:get_header_value("host")); 
               ProxiedHost ->
                   ProxiedHost
           end,
    Port = case Env:get_header_value("HN-Port") of
               undefined -> 
                   {ok, P} = inet:port(Env:get(socket)),
                   integer_to_list(P);
               ProxiedPort ->
                   ProxiedPort
           end,
    lists:concat(["http://", string:to_lower(Host), ":", Port, 
                  Env:get(path)]).

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
to_dict([ {Ref, Val} | T], JSON) ->
    to_dict(T, add_ref(Ref, hn_util:jsonify_val(Val), JSON)).

add_ref(#refX{ obj = {page,"/"}}, {Name, Val}, JSON) ->
    dh_tree:set(["page", Name], Val, JSON);
add_ref(#refX{ obj = {Ref, {X,Y}}}, Data, JSON) ->
    {Name, Val} = hn_util:jsonify_val(Data),
    dh_tree:set([atom_to_list(Ref), itol(Y), itol(X), Name], Val, JSON).

docroot(Site) ->
    code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:site_to_fs(Site)++"/docroot".
viewroot(Site) ->
    code:lib_dir(hypernumbers) ++ "/../../var/sites/"
        ++ hn_util:site_to_fs(Site)++"/views".
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

styles_to_css([], Acc) ->
    Acc;
styles_to_css([H | T], Acc) ->
    styles_to_css(T, [style_to_css(H) | Acc]).

style_to_css({styles, _Ref, X, Rec}) ->
    style_to_css(X, Rec).

style_to_css(X, Rec) ->
    Num = ms_util2:no_of_fields(magic_style),
    {X, style_att(Num + 1, Rec, [])}.

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
                NRef = Ref#refX{obj = {cell, {X1 + Acc, Y1+Offset}}},
                ok = hn_db_api:write_attributes([{NRef, [{"formula", Val}]}], 
                                                PAr, VAr),
                Acc+1 
        end,
    lists:foldl(F, 0, Values).

remoting_request(Env=#env{mochi=Mochi}, Site, Paths, Time) ->
    Socket = Mochi:get(socket),
    inet:setopts(Socket, [{active, once}]),
    remoting_reg:request_update(Site, Paths, ltoi(Time), self()),
    receive 
        {tcp_closed, Socket} -> ok;
        {error, timeout}     -> json(Env, <<"timeout">>);
        {msg, Data}          -> json(Env, Data)
    after
        %% TODO : Fix, should be controlled by remoting_reg
        600000 ->
            json(Env, {struct, [{"time", remoting_reg:timestamp()},
                                {"timeout", "true"}]})
    end.

page_attributes(#refX{site = S, path = P} = Ref, Uid) ->
    {ok,Name} = passport:uid_to_email(Uid),
    %% now build the struct
    Init   = [["cell"], ["column"], ["row"], ["page"], ["styles"]],
    Tree   = dh_tree:create(Init),
    Styles = styles_to_css(hn_db_api:read_styles(Ref), []),
    NTree  = add_styles(Styles, Tree),
    Dict   = to_dict(hn_db_api:read_whole_page(Ref), NTree),
    Time   = {"time", remoting_reg:timestamp()},
    Usr    = {"user", Name},
    Host   = {"host", S},
    Perms  = {"permissions", auth_srv:get_as_json(S, P)},
    Grps   = {"groups", {array, []}},
    Lang   = {"lang", get_lang(Uid)},
    {struct, [Time, Usr, Host, Lang, Grps, Perms
              | dict_to_struct(Dict)]}.

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
    Dict = hn_db_api:read_page_structure(RefX),
    Tmp  = pages_to_json(dh_tree:add(RefX#refX.path, Dict)),    
    {struct, [{"name", "home"}, {"children", {array, Tmp}}]}.

fix_up(List, S, P) ->
    f_up1(List, S, P, [], []).

f_up1([], _S, _P, A1, A2) ->
    {A1, lists:flatten(A2)};
f_up1([{struct, [{"ref", R}, {"formula", {array, L}}]} | T], S, P, A1, A2) ->
    [Attr | RPath] = lists:reverse(string:tokens(R, "/")),
    Obj            = hn_util:parse_attr(Attr),
    Path           = lists:reverse(RPath),
    RefX           = #refX{site = S, path = Path, obj = Obj},
    L2             = [[{"formula", X}] || X <- L],
    NewAcc         = lists:zip(hn_util:range_to_list(RefX), lists:reverse(L2)),
    f_up1(T, S, P, A1, [NewAcc | A2]);
f_up1([{struct, [{"ref", Ref}, {"formula", F}]} | T], S, P, A1, A2) ->
    [Attr | RPath] = lists:reverse(string:tokens(Ref, "/")),
    Obj            = hn_util:parse_attr(Attr),
    Path           = lists:reverse(RPath),
    RefX           = #refX{site = S, path = Path, obj = Obj},
    case Obj of
        {column, _} -> f_up1(T, S, P, [{RefX, F} | A1], A2);
        {row, _}    -> f_up1(T, S, P, [{RefX, F} | A1], A2);
        {cell, _}   -> f_up1(T, S, P, A1, [{RefX, [{"formula", F}]} | A2])
    end.

accept_type(Env) ->
    case Env:get_header_value('Accept') of
        undefined -> html; %cheapskate googlebots don't set accept header
        Accept    ->
            case re:run(Accept, "application/json") of
                {match, _} -> json;
                nomatch -> html
            end
    end.

build_tpl(Site, Tpl) ->
    
    {ok, Master} = file:read_file([viewroot(Site), "/_g/core/built.tpl"]),
    {ok, Gen}    = file:read_file([viewroot(Site), "/", Tpl, ".tpl"]),
    
    New1 = re:replace(Master, "%BODY%", hn_util:esc_regex(Gen),
                     [{return, list}]),
    New2 = re:replace(New1, "%VIEW%", Tpl, [{return, list}]),

    file:write_file([viewroot(Site), "/", Tpl, ".html"], New2).
    
pages_to_json(Dict) ->
    F = fun(X) -> pages_to_json(X, dict:fetch(X, Dict)) end,
    case is_dict(Dict) of 
        true  -> lists:map(F, dict:fetch_keys(Dict));
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

-spec save_view(string(), string(), string(), uid(), #refX{}) -> ok.
%%
save_view(Site, ViewName, ViewContent, Uid, Ref) ->
    Data = [{ref, Ref}, {authreq, Uid}, {content, ViewContent}],
    Path = [viewroot(Site), "/" , ViewName, ".meta"],
    ok = file:write_file(Path ,io_lib:fwrite("~p.\n",[Data])),
    ok = save_view(Site, ViewName, Data).

-spec save_view(string(), string(), list()) -> ok.
%%
save_view(Site, ViewName,
          [{ref, Ref}, {authreq, Uid}, {content, Content}]) ->    
    Sec  = hn_security:make(Content, Ref, Uid),
    Path = [viewroot(Site), "/" , ViewName],
    ok   = file:write_file([Path , ".tpl"], Content),
    ok   = file:write_file([Path , ".sec"], io_lib:fwrite("~p.\n",[Sec])).

is_view(View) ->
    [Pre, _User, _Name] = string:tokens(View, "/"),
    (Pre == "_u" orelse Pre == "_g").
% andalso hn_util:is_alpha(User)
% andalso hn_util:is_alpha(Name).

sync_exit() ->
    exit("exit from hn_mochi:handle_req impossible page versions").
    
should_regen(Tpl, Html) ->
    filelib:is_file(Tpl)
        andalso ( not(filelib:is_file(Html)) orelse
                  hn_util:is_older(Html, Tpl) ). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Input Processors
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_query(#env{}) -> #qry{}. 
process_query(#env{mochi = Mochi}) ->
    Lst = Mochi:parse_qs(),
    process_query_(Lst, #qry{}).
    
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
            'POST' -> {_,{_,T}} = mochiweb_headers:lookup('Content-Type', 
                                                          Mochi:get(headers)),
                      case lists:prefix("multipart/form-data", T) of
                          true  -> {undefined, multipart};
                          false -> RB = Mochi:recv_body(),
                                   {ok, B} = get_json_post(RB),
                                   {RB, B}
                      end
        end,
    #env{mochi = Mochi, 
         accept = accept_type(Mochi),
         method = Mochi:get(method),
         raw_body = RawBody,
         body = Body}.

-spec process_user(string(), #env{}) -> #env{}. 
process_user(Site, E=#env{mochi = Mochi}) ->
    Auth = Mochi:get_cookie_value("auth"),
    case passport:inspect_stamp(Auth) of
        {ok, Uid} -> E#env{uid = Uid};
        {error, no_stamp} -> 
            send_sync("ping", Site, E),
            E#env{uid = anonymous};
        {error, _Reason} ->
            Cookie = hn_net_util:kill_cookie("auth"),
            E2 = E#env{uid = anything, 
                       headers = [Cookie | E#env.headers]},
            send_sync("reset", Site, E2),
            E2
    end.

-spec send_sync(string(), string(), #env{}) -> no_return() | ok.
send_sync(Cmd, Site, E=#env{mochi = Mochi}) ->
    case application:get_env(hypernumbers, sync_url) of
        {ok, SUrl} when SUrl /= Site ->
            CurUrl = 
                hn_util:strip80(Site) ++ 
                mochiweb_util:quote_plus(Mochi:get(raw_path)),
            Redir = SUrl++"/_sync/"++Cmd++"/?return="++CurUrl,
            Redirect = {"Location", Redir},
            E2 = E#env{headers = [Redirect | E#env.headers]},
            respond(302, E2),
            throw(ok);
        _Else ->
            ok
    end.

process_sync(["reset"], E, Return) ->
    Cookie = hn_net_util:kill_cookie("auth"),
    Original = mochiweb_util:unquote(Return),
    Redirect = {"Location", Original},
    E#env{headers = [Cookie, Redirect | E#env.headers]};

process_sync(["ping"], E=#env{mochi=Mochi}, Return) ->
    Stamp = case Mochi:get_cookie_value("auth") of
        undefined -> passport:temp_stamp();
        S         -> S end,
    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
    Original = mochiweb_util:unquote(Return),
    #refX{site = OrigSite} = hn_util:parse_url(Original),
    Redir = hn_util:strip80(OrigSite) ++ 
        "/_sync/pong/"++Stamp++"/?return="++Return,
    Redirect = {"Location", Redir},
    E#env{headers = [Cookie, Redirect | E#env.headers]};

process_sync(["pong", Stamp], E, Return) ->
    Cookie   = hn_net_util:cookie("auth", Stamp, "never"),
    Original = mochiweb_util:unquote(Return),
    Redirect = {"Location", Original},
    E#env{headers = [Cookie, Redirect | E#env.headers]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Output Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'404'(#refX{site = Site}, Env) ->
    serve_html(404, Env, [viewroot(Site), "/_g/core/login.html"]).

'500'(Env) ->
    respond(500, Env).

respond(Code, #env{mochi = Mochi, headers = Headers}) ->
    Mochi:respond({Code, Headers, []}),
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

get_lang(anonymous) ->
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
