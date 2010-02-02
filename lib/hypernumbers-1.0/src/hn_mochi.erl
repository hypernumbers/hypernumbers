%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_mochi).

-include("spriki.hrl").

-include_lib("kernel/include/file.hrl").
-include("gettext.hrl").
-include("auth2.hrl").

-export([ handle/1,
          style_to_css/2,
          docroot/1,
          page_attributes/2,
          get_json_post/1 % Used for mochilog replay rewrites
         ]).

-define(SHEETVIEW, "_g/core/spreadsheet").
-define(TWO_YEARS, 63113852).

-record(req, {mochi,
              headers = [],
              user,
              uid,
              accept}).

-spec handle(any()) -> ok.
handle(MochiReq) ->
    Ref = hn_util:parse_url(get_real_uri(MochiReq)),
    Req = #req{mochi = MochiReq, 
               accept = accept_type(MochiReq)},
    Qry = parse_query(Req),
    case catch handle_(Ref, Req, Qry) of
        ok -> ok; 
        Else -> '500'(Req, Else)
    end.

-spec handle_(#refX{}, #req{}, #qry{}) -> ok. 

handle_(#refX{path=["ping"]}, Req, #qry{uid=Uid, return=Return}) 
  when Return /= undefined, uid /= undefined ->
    handle_ping(Req, Uid, Return);
handle_(Ref, Req, Qry) ->
    case filename:extension((Req#req.mochi):get(path)) of
        [] -> authorize_resource(Req, Ref, Qry);
        Ext -> Root = docroot(Ref#refX.site),
               handle_static(Ext, Root, Req#req.mochi)
    end.

-spec authorize_resource(#req{}, #refX{}, #qry{}) -> no_return(). 
authorize_resource(Req, Ref, Qry) -> 
    Req2 = process_request(Ref#refX.site, Req),
    #req{mochi=Mochi, user=User, accept=AType} = Req2,
    Ar = {hn_users:name(User), hn_users:groups(User)},
    Method = Mochi:get(method),
    AuthRet = case Method of
                  'GET' -> authorize_get(Ref, Qry, AType, Ar);
                  'POST' -> authorize_post(Ref, Qry, AType, Ar)
              end,
    case {AuthRet, AType} of
        {allowed, _} ->
            handle_resource(Method, Ref, Qry, Req2);
        {{view, View}, html} ->
            handle_resource(Method, Ref, Qry#qry{view = View}, Req2);
        {not_found, html} ->
            serve_html(404, Req2, 
                       [viewroot(Ref#refX.site), "/_g/core/404.html"]);
        {denied, html} ->
            serve_html(401, Req2,
                       [viewroot(Ref#refX.site), "/_g/core/login.html"]);
        {not_found, json} ->
            respond(404, Req2);
        _NoPermission ->
            respond(401, Req2)
    end.

handle_resource('GET', Ref, Qry, Req=#req{mochi = Mochi, user = User}) ->
    mochilog:log(Mochi, Ref, hn_users:name(User), undefined),
    ObjType = element(1, Ref#refX.obj),
    iget(Ref, ObjType, Qry, Req);

handle_resource('POST', Ref, Qry, Req=#req{mochi = Mochi, user = User}) ->
    {value, {'Content-Type', Ct}} =
        mochiweb_headers:lookup('Content-Type', Mochi:get(headers)),
    case Ct of
        %% Uploads
        "multipart/form-data" ++ _Rest ->
            {Data, File} = hn_file_upload:handle_upload(Mochi, Ref, User),
            Name = filename:basename(File),
            mochilog:log(Mochi, Ref, hn_users:name(User), {upload, Name}),
            json(Req, Data);

        %% Normal Post Requests
        _Else ->
            Body = Mochi:recv_body(),
            {ok, Post} = get_json_post(Body),
            mochilog:log(Mochi, Ref, hn_users:name(User), Body),
            ipost(Ref, Qry, Post, Req)
    end.

-spec handle_static(string(), iolist(), any()) -> any(). 
handle_static(".tpl", Root, Mochi) ->
    %% Don't cache templates
    "/"++RelPath = Mochi:get(path),
    Mochi:serve_file(RelPath, Root, nocache()),
    ok;
handle_static(E, Root, Mochi)
  when E == ".png"; E == ".jpg"; E == ".css"; E == ".js"; 
       E == ".ico"; E == ".json"; E == ".gif"; E == ".html" ->
    %% todo: do a better job with caching, etags etc.
    "/"++RelPath = Mochi:get(path),
    Mochi:serve_file(RelPath, Root),
    ok.

-spec handle_ping(#req{}, string(), string()) -> ok. 
handle_ping(R, Uid, Return) ->
    Opts = [{path, "/"}, {max_age, ?TWO_YEARS}],
    Cookie = mochiweb_cookies:cookie("uid", Uid, Opts),
    Redirect = {"Location", mochiweb_util:unquote(Return)},
    R2 = R#req{headers = [Redirect, Cookie | R#req.headers]},
    respond(302, R2).
    
-spec authorize_get(#refX{}, #qry{}, json | html, auth_req()) 
                   -> {view, string()} | allowed | denied | not_found.
authorize_get(_Ref, #qry{permissions = [], _ = undefined}, html, _Ar) ->
    allowed;
authorize_get(#refX{site = Site, path = Path}, #qry{view = View}, html, Ar) 
  when View /= undefined -> 
    auth_srv2:check_particular_view(Site, Path, Ar, View);
authorize_get(#refX{site = Site, path = Path}, #qry{challenger=[]}, html, Ar) ->
    auth_srv2:check_get_challenger(Site, Path, Ar);
%% authorize_get(#refX{site=Site}, [{"updates",_},{"path",Paths}|_], json, Ar) ->
%%     Views = [auth_srv2:get_any_view(Site, string:tokens(P, "/"), Ar) 
%%              || P <- string:tokens(Paths, ",")],
%%     case lists:all(fun({view, _}) -> true; (_) -> false end, Views) of 
%%         true -> allowed;
%%         _Else -> denied
%%     end;
authorize_get(#refX{site = Site, path = Path}, #qry{_ = undefined}, html, Ar) ->
    auth_srv2:check_get_view(Site, Path, Ar);
authorize_get(#refX{site = Site, path = Path}, _Qry, _Any, Ar) ->
    case auth_srv2:get_any_view(Site, Path, Ar) of
        {view, _} -> allowed;
        _Else -> denied
    end.

-spec authorize_post(#refX{}, #qry{}, json | html, auth_req()) 
                    -> {view, string()} | allowed | denied | not_found.
authorize_post(#refX{path = ["_user", "login"]}, _Qry, json, _Ar) ->
    allowed;
authorize_post(#refX{site = Site, path = Path}, _Qry, json, Ar) ->
    case auth_srv2:check_particular_view(Site, Path, Ar, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> 
            allowed;
        _ -> 
            case auth_srv2:get_any_view(Site, Path, Ar) of
                {view, _V} ->
                    %% {ok, Bin} = file:open([viewroot(Site), "/", V, ".sec"], [read]),
                    %% _Sec = binary_to_term(Bin),
                    %% get the security object.
                    allowed;
                _Other ->
                    denied
            end
    end;
authorize_post(_Ref, _Qry, _Type, _Ar) ->
    denied.

-spec iget(#refX{}, 
           page | cell | row | column | range,
           #qry{},
           #req{}) 
          -> any(). 

iget(Ref=#refX{path=["_user", "login"]}, page, _Qry, Req) ->
    iget(Ref, page, #qry{view = "_g/core/login"}, Req);

iget(#refX{site = Site}, page, #qry{view = FName, template = []}, Req) 
  when FName /= undefined -> 
    serve_html(Req, [viewroot(Site), "/", FName, ".tpl"]);    

iget(Ref=#refX{site = Site}, page, #qry{view = FName}, Req)
    when FName /= undefined ->
    Tpl  = [viewroot(Site), "/", FName, ".tpl"],
    Html = [viewroot(Site), "/", FName, ".html"],
    ok = case should_regen(Tpl, Html) of
             true -> ok = build_tpl(Site, FName);
             _    -> ok
         end,
    case filelib:is_file(Html) of
        true  -> serve_html(Req, Html);
        false -> '404'(Ref, Req)
    end;

iget(Ref, page, #qry{updates = Time, path = Path}, Req)
  when Time /= undefined, Path /= undefined ->
    Paths = [ string:tokens(X, "/") || X<-string:tokens(Path, ",")],
    remoting_request(Req, Ref#refX.site, Paths, Time);

iget(#refX{site = S}, page, #qry{status = []}, Req) -> 
    json(Req, status_srv:get_status(S));

iget(#refX{site = S, path  = P}, page, #qry{permissions = []}, Req) ->
    json2(Req, auth_srv2:get_as_json(S, P));

iget(#refX{site = S}, page, #qry{users= []}, Req) ->
    text_html(Req, hn_users:prettyprint_DEBUG(S));

% List of template pages
iget(Ref, page, #qry{templates = []}, Req) ->
    Fun = fun(X) ->
                  [F | _T] = lists:reverse(string:tokens(X, "/")),
                  case F of
                      [$. | _T1] -> true;
                      _          -> false
                  end
          end,
    Files = lists:dropwhile(Fun,
              filelib:wildcard(docroot(Ref#refX.site)++"/templates/*")),
    File = [filename:basename(X) || X <- Files], 
    json(Req, {array, File});

iget(Ref, page, #qry{pages = []}, Req=#req{accept = json}) ->
    json(Req, pages(Ref));

iget(Ref, page, _Qry, Req=#req{accept = json, user = User}) ->
    json(Req, page_attributes(Ref, User));

iget(Ref, cell, _Qry, Req=#req{accept = json}) ->
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
    json(Req, V);

iget(Ref, Type, _Qry, Req=#req{accept=json})
  when Type == range;
       Type == column;
       Type == row ->
    Init = [["cell"], ["column"], ["row"], ["page"]],
    Tree = dh_tree:create(Init),
    Dict = to_dict(hn_db_api:read_attributes(Ref,[]), Tree),
    json(Req, {struct, dict_to_struct(Dict)});

iget(#refX{site = Site}, cell, _Qry, Req=#req{accept=html}) ->
    serve_html(Req, [docroot(Site),"/_g/core/cell.html"]);

iget(Ref, _Type, Qry, Req) ->
    error_logger:error_msg("404~n-~p~n-~p~n", [Ref, Qry]),
    '404'(Ref, Req).


-spec ipost(#refX{}, #qry{}, any(), #req{}) -> any().

ipost(#refX{site = S, path = P} = Ref, _Qry, 
      [{"drag", {_, [{"range", Rng}]}}], Req=#req{user=User}) ->
    ok = status_srv:update_status(User, S, P, "edited page"),
    hn_db_api:drag_n_drop(Ref, Ref#refX{obj = hn_util:parse_attr(range,Rng)}),
    json(Req, "success");

ipost(#refX{site = Site, path=["_user","login"]}, _Qry, Data, Req) ->
    [{"email", Email},{"pass", Pass},{"remember", Rem}] = Data,
    Resp = case hn_users:login(Site, Email, Pass, Rem) of
               {error, invalid_user} -> 
                   [{"response","error"}];
               {ok, Token} ->
                   [{"response","success"},{"token",Token}]
           end,
    json(Req, {struct, Resp});

%% the purpose of this message is to mark the mochilog so we don't 
%% need to do nothing with anything...
ipost(_Ref, #qry{mark = []}, [{"set",{struct, [{"mark", _Msg}]}}], Req) ->
    json(Req, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, [{"insert", "before"}], Req)
  when O == row orelse O == column ->
    ok = hn_db_api:insert(Ref),
    json(Req, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, [{"insert", "after"}], Req)
  when O == row orelse O == column ->
    ok = hn_db_api:insert(make_after(Ref)),
    json(Req, "success");

%% by default cells and ranges displace vertically
ipost(#refX{obj = {O, _}} = Ref, _Qry, [{"insert", "before"}], Req)
  when O == cell orelse O == range ->
    ok = hn_db_api:insert(Ref, vertical),
    json(Req, "success");

%% by default cells and ranges displace vertically
ipost(#refX{obj = {O, _}} = Ref, _Qry, [{"insert", "after"}], Req)
  when O == cell orelse O == range ->
    ok = hn_db_api:insert(make_after(Ref)),
    json(Req, "success");

%% but you can specify the displacement explicitly
ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      [{"insert", "before"}, {"displacement", D}], Req)
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    ok = hn_db_api:insert(Ref, list_to_existing_atom(D)),
    json(Req, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, 
      [{"insert", "after"}, {"displacement", D}], Req)
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    RefX2 = make_after(Ref),
    ok = hn_db_api:insert(RefX2, list_to_existing_atom(D)),
    json(Req, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, [{"delete", "all"}], Req) 
  when O == page ->
    ok = hn_db_api:delete(Ref),
    json(Req, "success");

ipost(Ref, _Qry, [{"delete", "all"}], Req) ->
    ok = hn_db_api:delete(Ref),
    json(Req, "success");

ipost(#refX{obj = {O, _}} = Ref, _Qry, [{"delete", Direction}], Req)
  when O == cell orelse O == range,
       Direction == "horizontal" orelse Direction == "vertical" ->
    ok = hn_db_api:delete(Ref, Direction),
    json(Req, "success");

ipost(Ref=#refX{obj = {range, _}}, _Qry, [{"copy", {struct, [{"src", Src}]}}], Req) ->
    ok = hn_db_api:copy_n_paste(hn_util:parse_url(Src), Ref),
    json(Req, "success");

ipost(#refX{obj = {range, _}} = Ref, _Qry, 
      [{"borders", {struct, Attrs}}], Req) ->
    Where = from("where", Attrs),
    Border = from("border", Attrs),
    Border_Style = from("border_style", Attrs),
    Border_Color = from("border_color", Attrs),
    ok = hn_db_api:set_borders(Ref, Where, Border, Border_Style, Border_Color),
    json(Req, "success");

ipost(_Ref, _Qry,
      [{"set", {struct, [{"language", _Lang}]}}], Req=#req{user=anonymous}) ->
    S = {struct, [{"error", "cant set language for anonymous users"}]},
    json(Req, S);

ipost(#refX{site = Site, path=["_user"]}, _Qry, 
      [{"set", {struct, [{"language", Lang}]}}], Req=#req{user=User}) ->
    ok = hn_users:update(Site, User, "language", Lang),
    json(Req, "success");

ipost(#refX{site = S, path = P}, _Qry, 
      [{"set", {struct, [{"list", {array, Array}}]}}], Req=#req{user=User}) ->
    ok = status_srv:update_status(User, S, P, "edited page"),
    {Lasts, Refs} = fix_up(Array, S, P),
    ok = hn_db_api:write_last(Lasts),
    ok = hn_db_api:write_attributes(Refs),
    json(Req, "success");

ipost(#refX{site = S, path = P, obj = O} = Ref, _Qry, 
      [{"set", {struct, Attr}}], Req=#req{user=User}) ->
    Type = element(1, O),
    ok = status_srv:update_status(User, S, P, "edited page"),
    case Attr of
        %% TODO : Get Rid of this (for pasting a range of values)
        [{"formula",{array, Vals}}] ->
            post_range_values(Ref, Vals);

        %% if posting a formula to a row or column, append
        [{"formula", Val}] when Type == column; Type == row ->
            ok = hn_db_api:write_last([{Ref, Val}]);

        _Else ->
            ok = hn_db_api:write_attributes([{Ref, Attr}])
    end,
    json(Req, "success");

ipost(Ref, _Qry, [{"clear", What}], Req) 
  when What == "contents"; What == "style"; What == "all" ->
    ok = hn_db_api:clear(Ref, list_to_atom(What)),
    json(Req, "success");

ipost(#refX{site=Site, path=Path} = _Ref, _Qry,
      [{"saveview", {struct, [{"name", Name}, {"tpl", Form}]}}], 
      Req=#req{user=User}) ->
    AuthSpec = [{user, hn_users:name(User)}, {group, "dev"}],
    _AuthReq = {hn_users:name(User), hn_users:groups(User)},
    Output = [viewroot(Site), "/" , Name],
    % true = can_save_view(User, Name),
    ok = auth_srv2:add_view(Site, Path, AuthSpec, Name),
    % Sec = hn_security:make(Form, Ref, AuthReq),
    ok = filelib:ensure_dir(Output),
    ok = file:write_file([Output, ".tpl"], Form),
    {ok, F} = file:open([Output, ".sec"], [write]),
    %ok = io:format(F, "~p.", [Sec]),
    ok = file:close(F),
    json(Req, "success");

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back_create handler                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Qry,
      [{"action", "notify_back_create"}|T], Req) ->

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
    json(Req, {struct, Return2});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handler                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Qry,
      [{"action", "notify_back"} |T] = _Json, Req) ->
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
    json(Req, S);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify handler                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Qry, [{"action", "notify"} | T] = _Json, Req) ->
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
    json(Req, S);

ipost(Ref, Qry, Post, Req) ->
    error_logger:error_msg("404~n-~p~n-~p~n-~p",[Ref, Qry, Post]),
    '404'(Ref, Req).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Helpers
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

can_save_view(User, "_u/"++FName) ->
    [Name | _] = string:tokens(FName, "/"),
    Name == hn_users:name(User);

can_save_view(User, "_g/"++FName) ->
    [Group | _] = string:tokens(FName, "/"),
    lists:member(Group, hn_users:groups(User)).

-spec process_request(string(), #req{}) -> #req{}. 
process_request(Site, R) ->
    R2 = get_user(Site, R),
    get_uid(Site, R2).

-spec get_user(string(), #req{}) -> #req{}. 
get_user(Site, R=#req{mochi = Mochi}) ->
    Auth = Mochi:get_cookie_value("auth"),
    case hn_users:verify_token(Site, Auth) of
        {ok, User}        -> R#req{user = User};
        {error, no_token} -> R#req{user = anonymous};
        {error, _Reason}  ->
            %% authtoken was invalid (probably did a clean_start() while
            %% logged in, kill the cookie
            Opts = [{path, "/"}, {max_age, 0}],
            Cookie = mochiweb_cookies:cookie("auth", "expired", Opts),
            R#req{user = anonymous, 
                  headers = [Cookie | R#req.headers]}
    end.

-spec get_uid(string(), #req{}) -> #req{}.
get_uid(Site, R=#req{mochi = Mochi}) ->
    case Mochi:get_cookie_value("uid") of
        undefined ->
            Uid = mochihex:to_hex(crypto:rand_bytes(8)),
            Opts = [{path, "/"}, {max_age, ?TWO_YEARS}],
            Cookie = mochiweb_cookies:cookie("uid", Uid, Opts),
            R2 = R#req{uid = Uid, headers = [Cookie | R#req.headers]},
            case application:get_env(hypernumbers, pingto) of
                {ok, PingTo} ->
                    Current = 
                        Site ++ 
                        mochiweb_util:quote_plus(Mochi:get(raw_path)),
                    Redir = PingTo++"/ping/?uid="++Uid++"&return="++Current,
                    Redirect = {"Location", Redir},
                    respond(302, R2#req{headers = [Redirect | R2#req.headers]}),
                    throw(ok);
                _Else ->
                    R2
            end;
        Uid ->
            R#req{uid = Uid}
    end.
    
%% Some clients dont send ip in the host header
get_real_uri(Req) ->
    Host = case Req:get_header_value("HN-Host") of
               undefined ->
                   lists:takewhile(fun(X) -> X /= $: end,
                                   Req:get_header_value("host")); 
               ProxiedHost ->
                   ProxiedHost
           end,
    Port = case Req:get_header_value("HN-Port") of
               undefined -> 
                   {ok, P} = inet:port(Req:get(socket)),
                   integer_to_list(P);
               ProxiedPort ->
                   ProxiedPort
           end,
    lists:concat(["http://", string:to_lower(Host), ":", Port, 
                  Req:get(path)]).



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
        ++ hn_util:parse_site(Site)++"/docroot".
viewroot(Site) ->
    docroot(Site) ++ "/views".
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
    {itol(X), style_att(Num + 1, Rec, [])}.

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

post_range_values(Ref, Values) ->
    F = fun({array, Vals}, Acc) -> 
                post_column_values(Ref, Vals, Acc), Acc+1 
        end,
    lists:foldl(F, 0, Values).

post_column_values(Ref, Values, Offset) ->
    #refX{obj={range,{X1, Y1, _X2, _Y2}}} = Ref,
    F =  fun("", Acc)  -> Acc+1;
            (Val, Acc) -> 
                 NRef = Ref#refX{obj = {cell, {X1 + Acc, Y1+Offset}}},
                 ok = hn_db_api:write_attributes([{NRef, [{"formula", Val}]}]),
                 Acc+1 
         end,
    lists:foldl(F, 0, Values).

remoting_request(Req=#req{mochi=Mochi}, Site, Paths, Time) ->
    Socket = Mochi:get(socket),
    inet:setopts(Socket, [{active, once}]),
    remoting_reg:request_update(Site, Paths, ltoi(Time), self()),
    receive 
        {tcp_closed, Socket} -> ok;
        {error, timeout}     -> json(Req, <<"timeout">>);
        {msg, Data}          -> json(Req, Data)
    after
        %% TODO : Fix, should be controlled by remoting_reg
        600000 ->
            json(Req, {struct, [{"time", remoting_reg:timestamp()},
                                {"timeout", "true"}]})
    end.

page_attributes(#refX{site = S, path = P} = Ref, User) ->
    Name = hn_users:name(User),
    Groups = hn_users:groups(User),
    %% now build the struct
    Init   = [["cell"], ["column"], ["row"], ["page"], ["styles"]],
    Tree   = dh_tree:create(Init),
    Styles = styles_to_css(hn_db_api:read_styles(Ref), []),
    NTree  = add_styles(Styles, Tree),
    Dict   = to_dict(hn_db_api:read_whole_page(Ref), NTree),
    Time   = {"time", remoting_reg:timestamp()},
    Usr    = {"user", Name},
    Host   = {"host", S},
    Grps   = {"groups", {array, Groups}},
    Lang   = {"lang", get_lang(User)},
    Views = {views, {array, auth_srv2:get_views(S, P, {Name, Groups})}},
    
    {struct, [Time, Usr, Host, Lang, Grps, Views
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

accept_type(Req) ->
    {value, {'Accept', Accept}} =
        mochiweb_headers:lookup('Accept', Req:get(headers)),
    case re:run(Accept, "application/json") of
        {match, _} -> json;
        nomatch -> html
    end.

build_tpl(Site, Tpl) ->
    {ok, Master} = file:read_file([viewroot(Site), "/_g/core/built.tpl"]),
    {ok, Gen}    = file:read_file([viewroot(Site), "/", Tpl, ".tpl"]),

    New = re:replace(Master, "%BODY%", hn_util:esc_regex(Gen),
                     [{return, list}]),
    file:write_file([viewroot(Site), "/", Tpl, ".html"], New).
    
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
    
should_regen(Tpl, Html) ->
    filelib:is_file(Tpl)
        andalso ( not(filelib:is_file(Html)) orelse
                  hn_util:is_older(Html, Tpl) ). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parse Attributes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_query(#req{}) -> #qry{}. 
parse_query(#req{mochi = Mochi}) ->
    Lst = Mochi:parse_qs(),
    parse_query_(Lst, #qry{}).
    
parse_query_([], Qry) -> Qry;
parse_query_([{Param, Value} | Rest], Qry) ->
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
    parse_query_(Rest, Qry2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Output Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'404'(#refX{site = Site}, Req) ->
    serve_html(404, Req, [viewroot(Site), "/_g/core/login.html"]).

'500'(Req, Error) ->
    error_logger:error_msg("~p~n~p~n", [Error, erlang:get_stacktrace()]),
    respond(500, Req).

respond(Code, #req{mochi = Mochi, headers = Headers}) ->
    Mochi:respond({Code, Headers, []}),
    ok.

text_html(#req{mochi = Mochi, headers = Headers}, Text) ->
    Mochi:ok({"text/html", Headers, Text}),
    ok.
    
-spec json(#req{}, any()) -> any(). 
json(#req{mochi = Mochi, headers = Headers}, Data) ->
    Mochi:ok({"application/json",
            Headers ++ nocache(),
            (mochijson:encoder([{input_encoding, utf8}]))(Data)
           }),
    ok.

-spec json2(#req{}, any()) -> any(). 
json2(#req{mochi = Mochi, headers = Headers}, Data) ->
    Mochi:ok({"application/json",
              Headers ++ nocache(),
              (mochijson2:encoder([{utf8, true}]))(Data)
             }),
    ok.

-spec serve_html(#req{}, iolist()) -> any().
serve_html(Req, File) ->
    serve_html(200, Req, File).

-spec serve_html(integer(), #req{}, iolist()) -> any(). 
serve_html(Status, Req=#req{user = User}, File) ->
    F = fun() -> hn_util:compile_html(File, get_lang(User)) end,
    Response = cache(File, File++"."++get_lang(User), F),
    serve_html_file(Status, Req, Response),
    ok.

serve_html_file(Status, #req{mochi = Mochi, headers = Headers}, File) ->
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
get_lang(User) ->
    case hn_users:get(User, "language") of
        {ok, Lang} -> Lang;
        undefined  -> "en_gb"
    end.

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
