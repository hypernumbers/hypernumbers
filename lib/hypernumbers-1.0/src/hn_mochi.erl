%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_mochi).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-include_lib("kernel/include/file.hrl").
-include("gettext.hrl").
-include("hn_mochi.hrl").

-export([ start/0 ]).

-export([ handle/1,
          extract_styles/1,
          style_to_css/1,
          docroot/1,
          page_attributes/2,
          get_json_post/1 % Used for mochilog replay rewrites
         ]).

-define(SHEETVIEW, "spreadsheet").

-spec start() -> {ok, pid()}. 
start() ->
    {ok, {Ip, Port}} = application:get_env(hypernumbers, mochi_bind),
    StrIp = inet_parse:ntoa(Ip),
    Opts = [{port, Port}, 
            {ip, StrIp},
            {name, ?MODULE},
            {loop, {hn_mochi, handle}}],
    mochiweb_http:start(Opts).


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
            '500'(process_environment(MochiReq)) 
    end.

-spec handle_(#refX{}, #env{}, #qry{}) -> ok. 

handle_(#refX{site="http://www."++Site}, E=#env{mochi=Mochi}, _Qry) ->
    Redir = "http://" ++ hn_util:strip80(Site) ++ Mochi:get(raw_path),
    Redirect = {"Location", Redir},
    respond(301, E#env{headers = [Redirect | E#env.headers]});

handle_(#refX{path=["_sync" | Cmd]}, Env, #qry{return=QReturn}) 
  when QReturn /= undefined ->
    Env2 = process_sync(Cmd, Env, QReturn),
    respond(303, Env2),
    throw(ok);

handle_(Ref, Env, Qry) ->
    case filename:extension((Env#env.mochi):get(path)) of
        []  -> check_resource_exists(Env, Ref, Qry);
        Ext -> handle_static(Ext, Ref#refX.site, Env)
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
                  Req when Req == 'GET'; Req == 'HEAD'  ->
                      authorize_get(Ref, Qry, Env2);
                  'POST' -> authorize_post(Ref, Qry, Env2)
              end,

    case {AuthRet, Env2#env.accept} of
        {allowed, _} ->
            handle_resource(Ref, Qry, Env2);
        {{view, View}, _} ->
            handle_resource(Ref, Qry#qry{view = View}, Env2);
        {not_found, html} ->
            serve_html(404, Env2, 
                       [viewroot(Ref#refX.site), "/404.html"]);
        {denied, html} ->
            serve_html(401, Env2,
                       [viewroot(Ref#refX.site), "/401.html"]);
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
handle_static(X, Site, Env)
  when X == ".png"; X == ".jpg"; X == ".css"; X == ".js"; X == ".txt";
       X == ".ico"; X == ".json"; X == ".gif"; X == ".html"; X == ".pdf" ->
    Mochi = Env#env.mochi,
    "/"++RelPath = Mochi:get(path),
    Root = docroot(Site),
    Mochi:serve_file(RelPath, Root),
    ok;
handle_static(_X, Site, Env) ->
    serve_html(404, Env, [viewroot(Site), "/404.html"]).

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
authorize_get(#refX{path = [X | _]}, _Qry, #env{accept = html}) 
  when X == "_invite"; 
       X == "_mynewsite"; 
       X == "_validate";
       X == "_hooks";
       X == "_logout" ->
    allowed;

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
                true -> allowed;
                _Else -> denied
            end;       
        _Else ->
            denied
    end;

%% %% TODO : Broken
%% authorize_get(#refX{site = Site, path = Path}, 
%%               #qry{updates = U, view = "_g/core/webpage", paths = More}, 
%%               #env{accept = json, uid = Uid})
%%   when U /= undefined ->
%%     case auth_srv:check_particular_view(Site, Path, Uid, "_g/core/webpage") of
%%         {view, "_g/core/webpage"} ->
%%             MoreViews = [auth_srv:get_any_view(Site, string:tokens(P, "/"), Uid) 
%%                          || P <- string:tokens(More, ",")],
%%             case lists:all(fun({view, _}) -> true; 
%%                               (_) -> false end, 
%%                            MoreViews) of
%%                 true -> allowed;
%%                 _Else -> denied
%%             end;       
%%         _Else ->
%%             denied
%%     end;


%% %% Update requets targeted towards a non-spreadsheet view. Validation
%% %% for additional sources is made against the security object created
%% %% at a 'view-save-time'. 
%% authorize_get(#refX{site = Site, path = Path}, 
%%               #qry{updates = U, view = View, paths = More},
%%               #env{accept = json, uid = Uid}) 
%%   when U /= undefined, View /= undefined -> 
%%     case auth_srv:check_particular_view(Site, Path, Uid, View) of
%%         {view, View} ->
%%             {ok, [Sec]} = file:consult([viewroot(Site), "/", View, ".sec"]),
%%             Results = [hn_security:validate_get(Sec, Path, P) 
%%                        || P <- string:tokens(More, ",")],
%%             case lists:all(fun(X) -> X end, Results) of 
%%                 true -> allowed;
%%                 false -> denied
%%             end;
%%         _Else ->
%%             denied
%%     end;

%% %% Access to secondary data sources, described by some initial view
%% %% declared herein as 'via'.
%% authorize_get(#refX{site = Site, path = Path}, 
%%               #qry{view = View, via = Via}, 
%%               #env{accept = json, uid = Uid})
%%   when View /= undefined, View /= ?SHEETVIEW, Via /= undefined ->
%%     Base = string:tokens(Via, "/"),
%%     case auth_srv:check_particular_view(Site, Base, Uid, View) of
%%         {view, View} ->
%%             Target = hn_util:list_to_path(Path),
%%             {ok, [Sec]} = file:consult([viewroot(Site), "/", View, ".sec"]),
%%             case hn_security:validate_get(Sec, Base, Target) of
%%                 true -> allowed; 
%%                 false -> denied
%%             end;
%%         _Else ->
%%             denied
%%     end;

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

%% Allow special posts to occur
authorize_post(#refX{path = [X]}, _Qry, #env{accept = json}) 
when X == "_login";
     X == "_hooks" -> 
    allowed;

authorize_post(#refX{site = Site, path = ["_admin"]}, _Qry, 
               #env{accept = json, uid = Uid}) ->
    case hn_groups:is_member(Uid, Site, ["admin"]) of
        true  -> allowed;
        false -> denied
    end;

%% Authorize posts against non spreadsheet views. The transaction
%% attempted is validated against the view's security model.
%% authorize_post(Ref=#refX{site = Site, path = Path}, #qry{view = View}, Env)
%%   when View /= undefined ->
%%     case auth_srv:check_particular_view(Site, Path, Env#env.uid, View) of
%%         {view, View} ->
%%             {ok, [Sec]} = file:consult([viewroot(Site), "/", View, ".sec"]),
%%             case hn_security:validate_trans(Sec, Ref, Env#env.body) of
%%                 true -> allowed;
%%                 false -> denied
%%             end;
%%         _ -> denied
%%     end;

%% Allow a post to occur, if the user has access to a spreadsheet on
%% the target.  the actual operation may need further validation, so
%% flag as 'allowed_pending'.
authorize_post(#refX{site = Site, path = Path}, _Qry, Env) ->
    case auth_srv:check_particular_view(
           Site, Path, Env#env.uid, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> allowed;
        _ -> allowed %denied
    end.

-spec iget(#refX{}, 
           page | cell | row | column | range,
           #qry{},
           #env{}) 
          -> any(). 

iget(#refX{site=Site, path=["_logout"]}, page, 
     #qry{return=QReturn}, 
     Env) when QReturn /= undefined ->
    Return = mochiweb_util:unquote(QReturn),
    cleanup(Site, Return, Env);
    
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
            %% fu@#ity-bye!
            throw(E)
    end;

iget(#refX{site=Site, path=[X, _Vanity | Rest]=Path}, page, 
     #qry{hypertag=HT}, 
     Env) when X == "_invite"; X == "_validate" ->
    case passport:open_hypertag(Site, Path, HT) of
        {ok, Uid, _Email, Data, Stamp, Age} ->
            case proplists:get_value(emailed, Data) of
                true -> 
                    ok = passport:validate_uid(Uid),
                    Param = case lists:keyfind(param, 1, Data) of
                                false      -> "";
                                {param, P} -> P
                            end,
                    
                    Return = hn_util:strip80(Site) 
                        ++ hn_util:list_to_path(Rest)
                        ++ Param,
                                        
                    {Env2, Redir} = 
                        post_login(Site, Uid, Stamp, Age, Env, Return),
                    Headers = [{"location",Redir}|Env2#env.headers],
                    respond(303, Env2#env{headers = Headers}),
                    throw(ok);
                _Else -> 
                    throw(bad_validation)
            end
    end;

iget(Ref, page, #qry{view = "webpage"}, Env=#env{accept = html}) ->
    {Html, Width, Height} = hn_render:content(Ref),
    Page = hn_render:wrap_page(Html, Width, Height),
    text_html(Env, Page);

iget(Ref=#refX{site = Site}, page, #qry{view = FName}, Env=#env{accept = html})
  when FName /= undefined ->
    Html = [viewroot(Site), "/", FName, ".html"],
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

iget(Ref, page, _Qry, Env=#env{accept = json}) ->
    json(Env, page_attributes(Ref, Env));

iget(Ref, cell, _Qry, Env=#env{accept = json}) ->
    V = case hn_db_api:read_attribute(Ref,"value") of
            [{_Ref, Val}] when is_atom(Val) ->
                atom_to_list(Val);
            [{_Ref, {datetime, D, T}}] ->
                dh_date:format("Y/m/d H:i:s",{D,T});
            [{_Ref, {errval, Val}}] ->
                atom_to_list(Val);
            [{_Ref, Val}] ->
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

%% Format requests without trailing slash
iget(_Ref, Type, _Qry, Env=#env{accept=html, mochi=Mochi})
  when Type =:= cell; Type =:= name ->
    NPath = Mochi:get(path) ++ "/", 
    E2 = Env#env{headers = [{"location", NPath}|Env#env.headers]},
    respond(303, E2);
    
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

ipost(#refX{site=Site, path=["_login"]}, Qry, E) ->
    [{"email", Email0},{"pass", Pass},{"remember", Rem}] = E#env.body,
    Email = string:to_lower(Email0),
    case passport:authenticate(Email, Pass, Rem=="true") of
        {error, authentication_failed} -> 
            json(E, {struct, [{"response", "error"}]});
        {ok, Uid, Stamp, Age} ->
            Return = case Qry#qry.return of
                         R when R /= undefined -> mochiweb_util:unquote(R);
                         _Else -> hn_util:strip80(Site)
                     end,
            {E2, Redir} = post_login(Site, Uid, Stamp, Age, E, Return),
            json(E2, {struct, [{"redirect", Redir}]})
    end;

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

%% These three cases could be collapsed into one...
ipost(Ref, 
      _Qry,
      Env=#env{body=[{"copy", {struct, [{"src", Src}]}}],
               uid = Uid}) ->
    ok = hn_db_api:copy_n_paste(hn_util:parse_url(Src), Ref, all, Uid),
    json(Env, "success");
ipost(Ref, 
      _Qry,
      Env=#env{body=[{"copystyle", {struct, [{"src", Src}]}}],
               uid = Uid}) ->
    ok = hn_db_api:copy_n_paste(hn_util:parse_url(Src), Ref, style, Uid),
    json(Env, "success");
ipost(Ref, 
      _Qry,
      Env=#env{body=[{"copyvalue", {struct, [{"src", Src}]}}],
               uid = Uid}) ->
    ok = hn_db_api:copy_n_paste(hn_util:parse_url(Src), Ref, value, Uid),
    json(Env, "success");

ipost(#refX{obj = {range, _}} = Ref, _Qry, 
      Env=#env{body=[{"borders", {struct, Attrs}}]}) ->
    Where = from("where", Attrs),
    Border = from("border", Attrs),
    Border_Style = from("border_style", Attrs),
    Border_Color = from("border_color", Attrs),
    ok = hn_db_api:set_borders(Ref, Where, Border, Border_Style, Border_Color),
    json(Env, "success");

%% ipost(_Ref, _Qry,
%%       Env=#env{body = [{"set", {struct, [{"language", _Lang}]}}], 
%%                uid = "anonymous"}) ->
%%     S = {struct, [{"error", "cant set language for anonymous users"}]},
%%     json(Env, S);

ipost(#refX{site = _Site, path=["_user"]}, _Qry, 
      _Env=#env{body = [{"set", {struct, [{"language", _Lang}]}}], 
               uid = _Uid}) ->
    throw("can't set language right now");
    %% ok = hn_users:update(Site, Uid, "language", Lang),
    %% json(Env, "success");

ipost(Ref=#refX{path = P} = Ref, _Qry,
      Env=#env{body = [{"postform", {struct, Vals}}], uid = PosterUid}) ->

    [{"results", ResultsPath}, {"values", {array, Array}}] = Vals,

    %Transaction = common,
    %Expected = hn_db_api:matching_forms(Ref, Transaction),
    %% io:format("~nForms: ~p~n", [hn_db_api:matching_forms(Ref, Transaction)]),
    %% io:format("Vals: ~p~n", [Array]),
    %% io:format("Valid?: ~p~n", [hn_security:validate(Expected, Array)]),
    %true = hn_security:validate(Expected, Array),
   
    Results = Ref#refX{
                path = string:tokens(hn_util:abs_path(P, ResultsPath), "/")
               },

    % Labels from the results page
    OldLabels = hn_db_api:read_attribute(Results#refX{obj={row, {1,1}}},
                                         "__rawvalue"),
    
    Values = [ {"submitted", dh_date:format("Y/m/d h:i:s")} |
               lists:reverse(lists:foldl(fun generate_labels/2, [], Array)) ],
    
    {NewLabels, NVals} =
        allocate_values(Values, OldLabels, Results, get_last_col(OldLabels)),
    
    NLbls = [ {Lref, [{"formula", Val}]} || {Lref, Val} <- NewLabels ],
    ok = hn_db_api:write_attributes(NLbls, PosterUid, PosterUid),
    ok = hn_db_api:append_row(NVals, PosterUid, PosterUid),
    
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"set", {struct, Attr}}], uid = Uid}) ->
    case Attr of
        %% TODO : Get Rid of this (for pasting a range of values)
        [{"formula",{array, Vals}}] ->
            post_range_values(Ref, Vals, Uid, Uid);
        _Else ->
            ok = hn_db_api:write_attributes([{Ref, Attr}], Uid, Uid)
    end,
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"clear", What}], uid = Uid}) 
  when What == "contents"; What == "style"; What == "all" ->
    ok = hn_db_api:clear(Ref, list_to_atom(What), Uid),
    json(Env, "success");

ipost(Ref, _Qry, Env=#env{body = [{"clear", What}], uid = Uid}) ->
    ok = hn_db_api:clear(Ref, {attributes, [What]}, Uid),
    json(Env, "success");

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

ipost(#refX{site=RootSite, path=["_hooks"]}, 
      _Qry, Env=#env{body=Body, uid=PrevUid}) ->
    [{"signup",{struct,[{"email",Email0} , {"sitetype", SiteType}]}}] = Body,
    SType = site_type_exists(SiteType),
    Email = string:to_lower(Email0),
    Zone = case application:get_env(hypernumbers, environment) of
               {ok, development} -> "hypernumbers.dev";
               {ok, production}  -> "tiny.hn"
           end,
    case factory:provision_site(Zone, Email, SType, PrevUid) of
        {ok, new, Site, Node, Uid, Name} ->
            log_signup(RootSite, Site, Node, Uid, Email),
            Opaque = [{param, "?view=spreadsheet"}],
            Expiry = "never",
            Url = passport:create_hypertag(Site, ["_mynewsite", Name], 
                                           Uid, Email, Opaque, Expiry),
            json(Env, {struct, [{"result", "success"}, {"url", Url}]});
        {ok, existing, Site, Node, Uid, _Name} ->
            log_signup(RootSite, Site, Node, Uid, Email),
            json(Env, {struct, [{"result", "success"},
                                {"url", Site ++ "?view=spreadsheet"}]});
        {error, Reason} ->
            Str = case Reason of
                      %bad_email ->
                      invalid_email ->
                          "Sorry, the email provided was invalid, "
                              "please try again.";
                      _ ->
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
get_last_col(Labels) ->
    case [X || {#refX{obj = {cell, {X, _}}}, _Val} <- Labels] of
        []   -> 0;
        List -> lists:max(List)
    end.

allocate({Label, Value}, {Labels, Index, Ref, NLabels, Refs}) ->
    case lists:keyfind(Label, 2, Labels) of
        {#refX{obj = {cell, {X, _Y}}} = RefX, Label} ->
            % Label already exists
            {Labels, Index, Ref, NLabels,
             [{RefX#refX{obj = {column, {X, X}}}, Value} | Refs]};
        false  ->
            % Write new label
            X = Index + 1,
            {Labels, X, Ref,
             [{Ref#refX{obj = {cell, {X, 1}}}, Label} | NLabels],
             [{Ref#refX{obj = {column,  {X, X}}}, Value} | Refs]}
    end.

allocate_values(Values, Labels, Ref, Index) ->
    {_Labels, _Index, _Ref, NLabels, Refs} =
        lists:foldl(fun allocate/2, {Labels, Index, Ref, [], []}, Values),
    {NLabels, Refs}.

generate_labels({struct,[{"label", []}, {"formula", Formula}]}, List) ->
    [{uniqify("default", List), Formula} | List];
generate_labels({struct,[{"label", Label}, {"formula", Formula}]}, List) ->
    [{uniqify(Label, List), Formula} | List].

-spec uniqify(string(), list()) -> string().
uniqify(Label, List) ->
    case lists:keyfind(Label, 1, List) of
        {Label, _Value} -> uniqify(Label, List, 2);
        false           -> Label
    end.

-spec uniqify(string(), list(), integer()) -> string().
uniqify(Label, List, Index) ->
    NLabel = Label ++ " " ++ integer_to_list(Index),
    case lists:keyfind(NLabel, 1, List) of
        {NLabel, _Value} -> uniqify(Label, List, Index+1);
        false            -> NLabel
    end.                                               



-spec log_signup(string(), string(), atom(), auth_srv:uid(), string()) -> ok.
log_signup(Site, NewSite, Node, Uid, Email) ->
    Row = [ {hn_util:url_to_refX(Site ++ "/_sites/" ++ Ref), Val}
              || {Ref, Val} <- [{"A:A", Email},
                                {"B:B", NewSite},
                                {"C:C", Uid},
                                {"D:D", dh_date:format("Y/m/d G:i:s")},
                                {"E:E", atom_to_list(Node)} ] ],
    hn_db_api:append_row(Row, nil, nil).

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
to_dict([{Ref, KVs} | T], JSON) ->
    JSON2 = add_ref(Ref, KVs, JSON),
    to_dict(T, JSON2).

add_ref(_Ref, [], JSON) ->
    JSON;
add_ref(Ref, [{"__"++_Hidden, _}|Tail], JSON) ->
    add_ref(Ref, Tail, JSON);
add_ref(Ref, [KV|Tail], JSON) ->
    JSON2 = add_ref1(Ref, hn_util:jsonify_val(KV), JSON),
    add_ref(Ref, Tail, JSON2).

add_ref1(#refX{ obj = {page,"/"}}, {Name, Val}, JSON) ->
    dh_tree:set(["page", Name], Val, JSON);
add_ref1(#refX{ obj = {Ref, {X,Y}}}, Data, JSON) ->
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

-spec extract_styles(string()) -> #style{}. 
extract_styles(Site) ->
    [style_to_css(S) ||
        S <- hn_db_api:read_styles_IMPORT(#refX{site=Site}) ].
        
%% -spec extract_styles([{#refX{}, [tuple()]}]) -> #style{}. 
%% extract_styles([]) -> [];
%% extract_styles(Data) ->
%%     {Ref, _} = hd(Data),
%%     Idxs = [I || {_, Attrs} <- Data,
%%                  I <- [proplists:get_value("style", Attrs)],
%%                  I /= undefined],
%%     [style_to_css(S) || S <- hn_db_api:read_styles(Ref, Idxs)].

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

-spec page_attributes(#refX{}, #env{}) -> {struct, list()}.
page_attributes(#refX{site = S, path = P} = Ref, Env) ->
    Content = hn_db_api:read_intersect_ref(Ref),
    Init   = [["cell"], ["column"], ["row"], ["page"], ["styles"]],
    Tree   = dh_tree:create(Init),
    Styles = extract_styles(S),
    NTree  = add_styles(Styles, Tree),
    Dict   = to_dict(Content, NTree),
    Time   = {"time", remoting_reg:timestamp()},
    Usr    = {"user", Env#env.email},
    Host   = {"host", S},
    Perms  = {"permissions", auth_srv:get_as_json(S, P)},
    Grps   = {"groups", {array, []}},
    Lang   = {"lang", get_lang(Env#env.uid)},
    {struct, [Time, Usr, Host, Lang, Grps, Perms | dict_to_struct(Dict)]}.

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

sync_exit() ->
    exit("exit from hn_mochi:handle_req impossible page versions").
    
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
            'HEAD' -> {undefined, undefined};
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

-spec process_user(string(), #env{}) -> #env{} | no_return(). 
process_user(Site, E=#env{mochi = Mochi}) ->
    Auth = Mochi:get_cookie_value("auth"),
    try passport:inspect_stamp(Auth) of
        {ok, Uid, Email} ->
            E#env{uid = Uid, email = Email};
        {error, no_stamp} -> 
            Return = cur_url(Site, E),
            case try_sync(["seek"], Site, Return) of
                on_sync -> 
                    Stamp = passport:temp_stamp(),
                    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
                    E#env{headers = [Cookie | E#env.headers]};
                {redir, Redir} -> 
                    E2 = E#env{headers = [{"location",Redir}|E#env.headers]},
                    respond(303, E2), 
                    throw(ok)
            end;
        {error, _Reason} -> 
            cleanup(Site, cur_url(Site, E), E)
    catch error:_ -> 
            cleanup(Site, cur_url(Site, E), E)
    end.

%% Clears out auth cookie on currend and main server.
-spec cleanup(string(), string(), #env{}) -> no_return().
cleanup(Site, Return, E) ->
    Cookie = hn_net_util:kill_cookie("auth"),
    E2 = E#env{headers = [Cookie | E#env.headers]},
    Redir = case try_sync(["reset"], Site, Return) of
                on_sync -> Return;
                {redir, R} -> R
            end,
    E3 = E2#env{headers = [{"location",Redir}|E2#env.headers]},
    respond(303, E3), 
    throw(ok).

%% Returns teh url representing the current location.
-spec cur_url(string(), #env{}) -> string(). 
cur_url(Site, #env{mochi=Mochi}) ->
    hn_util:strip80(Site) ++ Mochi:get(raw_path).

-spec try_sync([string()], string(), string()) 
               -> {redir, string()} | on_sync.
try_sync(Cmd0, Site, Return) ->
    case application:get_env(hypernumbers, sync_url) of
        {ok, SUrl} when SUrl /= Site ->
            Cmd = string:join(Cmd0, "/"),
            QReturn = mochiweb_util:quote_plus(Return),
            Redir = SUrl++"/_sync/"++Cmd++"/?return="++QReturn,
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
    QStamp = mochiweb_util:quote_plus(Stamp),
    Redir = case try_sync(["tell", QStamp], Site, Return) of
               on_sync -> Return;
               {redir, R} -> R end,
    {Env2, Redir}.

process_sync(["tell", QStamp], E, QReturn) ->
    Stamp = mochiweb_util:unquote(QStamp),
    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
    Return = mochiweb_util:unquote(QReturn),
    Redirect = {"Location", Return},
    E#env{headers = [Cookie, Redirect | E#env.headers]};
process_sync(["seek"], E=#env{mochi=Mochi}, QReturn) ->
    Stamp = case Mochi:get_cookie_value("auth") of
        undefined -> passport:temp_stamp();
        S         -> S end,
    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
    Return = mochiweb_util:unquote(QReturn),
    #refX{site = OrigSite} = hn_util:parse_url(Return),
    QStamp = mochiweb_util:quote_plus(Stamp),
    Redir = hn_util:strip80(OrigSite) ++ 
        "/_sync/tell/"++QStamp++"/?return="++QReturn,
    Redirect = {"Location", Redir},
    E#env{headers = [Cookie, Redirect | E#env.headers]};
process_sync(["reset"], E, QReturn) ->
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
    serve_html(404, Env, [viewroot(Site), "/login.html"]).

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
