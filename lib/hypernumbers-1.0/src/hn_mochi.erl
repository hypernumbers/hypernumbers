%%% @author Dale Harvey
%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_mochi).

-include("regexp.hrl").
-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-include_lib("kernel/include/file.hrl").
-include("gettext.hrl").

-export([ req/1, style_to_css/2 ]).

-export([page_attributes_CHEATING/1]). % for use in gui_generator

-export([get_json_post/1]). % Used for mochilog replay rewrites


-define(hdr,[{"Cache-Control","no-store, no-cache, must-revalidate"},
             {"Expires",      "Thu, 01 Jan 1970 00:00:00 GMT"},
             {"Pragma",       "no-cache"}]).

-define(html, [{"Content-Type", "text/html"} | ?hdr]).

-define(exit, 
        exit("exit from hn_mochi:handle_req impossible page versions")).

req(Req) ->

    #refX{site = Site} = hn_util:parse_url(get_host(Req)),

    case filename:extension(Req:get(path)) of
        
        % Dont Cache templates
        X when X == ".tpl" ->
            "/"++RelPath = Req:get(path),
            Req:serve_file(RelPath, docroot(Site), ?hdr);            
        
        % Serve Static Files
        X when X == ".png"; X == ".jpg"; X == ".css"; X == ".js"; 
        X == ".ico"; X == ".json"; X == ".gif" ->
            "/"++RelPath = Req:get(path),
            Req:serve_file(RelPath, docroot(Site));
        
        [] ->
            case catch do_req(Req) of 
                ok   -> ok;
                Else -> '500'(Req, Else)
            end
    end.
    
do_req(Req) ->
    
    #refX{site = Site} = Ref = hn_util:parse_url(get_host(Req)),

    Vars   = Req:parse_qs(),
    Method = Req:get(method),

    {ok, Auth} = get_var_or_cookie("auth", Vars, Req),

    User = case hn_users:verify_token(Site, Auth) of
               
               {error, no_token} -> anonymous;
               {ok, Usr}         -> Usr;

               % authtoken was invalid (probably did a clean_start() while
               % logged in, kill the cookie
               {error, _Reason}  ->

                   Opts   = [{path, "/"}, {max_age, 0}],
                   Cookie = mochiweb_cookies:cookie("auth", "expired", Opts),

                   Req:respond(
                     {302, [{"Location", hn_util:list_to_path(Ref#refX.path)},
                            {"Content-Type", "text/html; charset=UTF-8"},
                            Cookie], ""}),

                   % heh probably not the best idea
                   throw(ok)
           end,

    Name    = hn_users:name(User),
    Groups  = hn_users:groups(User),    
    AuthRet = get_auth(Name, Groups, Method, Ref, Vars),

    case AuthRet of
        %% these are the returns for the GET's
        {return, '404'} ->
            serve_html(404, Req, [docroot(Site), "/_global/login.html"], User);
        {return, '401'} ->
            serve_html(401, Req, [docroot(Site), "/_global/login.html"], User);
        {html, File}    ->
            case Vars of
                [] -> handle_req(Method, Req, Ref, [{"view", File}], User);
                _  -> handle_req(Method, Req, Ref, Vars, User)
            end;
        %% these are the returns for the POST's
        true  -> handle_req(Method, Req, Ref, Vars, User);
        false -> Req:respond({401, [], []})
    end,
    ok.


handle_req(Method, Req, Ref, Vars, User) ->

    Type = element(1, Ref#refX.obj), 
    case Method of
         
        'GET'  ->
            CType = content_type(Req),
            mochilog:log(Req, Ref, hn_users:name(User), undefined),
            iget(Req, Ref, Type, Vars, User, CType);

        'POST' ->

            {value, {'Content-Type', Ct}} =
                mochiweb_headers:lookup('Content-Type', Req:get(headers)),

            case Ct of
                % Uploads
                "multipart/form-data" ++ _Rest ->

                    {Data, File} = hn_file_upload:handle_upload(Req, Ref, User),
                    Name = filename:basename(File),
                    
                    mochilog:log(Req, Ref, hn_users:name(User), {upload, Name}),
                    Json = (mochijson:encoder([{input_encoding, utf8}]))(Data),
                    Req:ok({"text/html", ?hdr, Json});
                
                % Normal Post Requests
                _Else ->
                    Body = Req:recv_body(),
                    {ok, Post} = get_json_post(Body),
                    mochilog:log(Req, Ref, hn_users:name(User), Body),
                    case ipost(Ref, Type, Vars, Post, User) of
                        ok            -> json(Req, "success");
                        {struct, _}=S -> json(Req, S);
                        err           -> Req:not_found()
                    end
            end
    end.    


%%---------------
%  GET REQUESTS
%%---------------
iget(Req, Ref=#refX{path=["_user", "login"]}, page, [], User, html) ->
    iget(Req, Ref, page, [{"view", "_global/login"}], User, html);

iget(Req, #refX{site = Site} = _Ref, page, [{"view", FName}, {"template", []}],
     User, html) ->
    serve_html(Req, [docroot(Site), "/", FName, ".tpl"], User);
    
iget(Req, #refX{site = Site} = _Ref, page, [{"view", FName}], User, html) ->

    %% If there is a template, generate html
    Tpl  = [docroot(Site), "/", FName, ".tpl"],
    Html = [docroot(Site), "/", FName, ".html"],
    
    ok = case filelib:is_file(Tpl) andalso
             ( not(filelib:is_file(Html))
               orelse hn_util:is_older(Html, Tpl) ) of
             true  -> ok = build_tpl(Site, FName); _ -> ok
         end,
        
    case filelib:is_file(Html) of
        true  -> serve_html(Req, Html, User);
        false -> '404'(Req, User)
    end;

iget(Req, Ref, page, [{"updates", Time}, {"path", Path}], _User, _CType) ->
    Paths = [ string:tokens(X, "/") || X<-string:tokens(Path, ",")],
    remoting_request(Req, Ref#refX.site, Paths, Time);

iget(Req, #refX{site = S}, page, [{"status", []}], _User, _CType) -> 
    json(Req, status_srv:get_status(S));

iget(Req, #refX{site = S}, page, [{"permissions", []}], _User, _CType) ->
    Req:ok({"text/html", auth_srv:pretty_print(S, [], html)});

iget(Req, #refX{site = S}, page, [{"users", []}], _User, _CType) ->
    Req:ok({"text/html", hn_users:prettyprint_DEBUG(S)});

iget(Req, #refX{site = S, path = P}, page, [{"permissions_debug", []}], User, _CType) ->
    Name    = hn_users:name(User),
    Groups  = hn_users:groups(User),
    Req:ok({"text/html", auth_srv:permissions_DEBUG(S, {Name, Groups}, P)});

% List of template pages
iget(Req, Ref, page, [{"templates", []}], _User, _CType) ->
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

% List of views available to edit
iget(Req, #refX{site = Site} = _Ref, page, [{"views", []}], User, _CType) ->

    Dirs = [ "/_u/"++hn_users:name(User)++"/"
             | [ "/_g/"++Group++"/" || Group <- hn_users:groups(User)] ],
    
    Strip = fun(FileName) ->
                    [File, Name, Pre | _ ]
                        = lists:reverse(string:tokens(FileName, "/")),
                    Pre++"/"++Name++"/"++File
            end,
    
    F = fun(Dir, Acc) ->
                Files = filelib:wildcard(docroot(Site)++Dir++"*.tpl"),
                Acc ++ [ Strip(X) || X <- Files ]
        end,
    
    json(Req, {array, lists:foldl(F, [], Dirs)});

iget(Req, Ref, page, [{"pages", []}], _User, json) ->
    json(Req, pages(Ref));

iget(Req, Ref, page, _Attr, User, json) ->
    json(Req, page_attributes(Ref, User));

iget(Req, Ref, cell, _Attr, _User, json) ->
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
    Req:ok({"text/html", V});


iget(Req, Ref, _Type, Attr, User, _CType) ->
    error_logger:error_msg("404~n-~p~n-~p~n", [Ref, Attr]),
    '404'(Req, User).

%%---------------
%  POST REQUESTS
%%---------------
ipost(#refX{site = S, path = P} = Ref, _Type, _Attr, 
      [{"drag", {_, [{"range", Rng}]}}], User) ->
    ok = status_srv:update_status(User, S, P, "edited page"),
    hn_db_api:drag_n_drop(Ref, Ref#refX{obj = hn_util:parse_attr(range,Rng)}),
    ok;

ipost(#refX{site = Site, path=["_user","login"]}, _T, _At, Data, _User) ->
    [{"email", Email},{"pass", Pass},{"remember", Rem}] = Data,
    Resp = case hn_users:login(Site, Email, Pass, Rem) of
               {error, invalid_user} -> 
                   [{"response","error"}];
               {ok, Token} ->
                   [{"response","success"},{"token",Token}]
           end,
    {struct, Resp};

%% the purpose of this message is to mark the mochilog so we don't 
%% need to do nothing with anything...
ipost(_Ref, _Type, [{"mark", []}], 
      [{"set",{struct, [{"mark", _Msg}]}}], _User) ->
    ok;

ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"}], _User)
  when O == row orelse O == column ->
    hn_db_api:insert(Ref);

ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"}], _User)
  when O == row orelse O == column ->
    hn_db_api:insert(make_after(Ref));

%% by default cells and ranges displace vertically
ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"}], _User)
  when O == cell orelse O == range ->
    hn_db_api:insert(Ref, vertical);

%% by default cells and ranges displace vertically
ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"}], _User)
  when O == cell orelse O == range ->
    hn_db_api:insert(make_after(Ref));

%% but you can specify the displacement explicitly
ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, 
      [{"insert", "before"}, {"displacement", D}], _User)
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    hn_db_api:insert(Ref, list_to_existing_atom(D));

ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, 
      [{"insert", "after"}, {"displacement", D}], _User)
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    RefX2 = make_after(Ref),
    hn_db_api:insert(RefX2, list_to_existing_atom(D));

ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, [{"delete", "all"}], _User) 
  when O == page ->
    hn_db_api:delete(Ref);

ipost(Ref, _Type, _Attr, [{"delete", "all"}], _User) ->
    hn_db_api:delete(Ref);

ipost(#refX{obj = {O, _}} = Ref, _Type, _Attr, [{"delete", Direction}], _User)
  when O == cell orelse O == range,
       Direction == "horizontal" orelse Direction == "vertical" ->
    hn_db_api:delete(Ref, Direction);

ipost(Ref, range, _Attr, [{"copy", {struct, [{"src", Src}]}}], _User) ->
    hn_db_api:copy_n_paste(hn_util:parse_url(Src), Ref);

ipost(#refX{obj = {range, _}} = Ref, _Type, _Attr, 
      [{"borders", {struct, Attrs}}], _User) ->
    Where = from("where", Attrs),
    Border = from("border", Attrs),
    Border_Style = from("border_style", Attrs),
    Border_Color = from("border_color", Attrs),
    ok = hn_db_api:set_borders(Ref, Where, Border, Border_Style, Border_Color);

ipost(_Ref, _Type, _Attr,
      [{"set", {struct, [{"language", _Lang}]}}], anonymous) ->
    {struct, [{"error", "cant set language for anonymous users"}]};

ipost(#refX{site = Site, path=["_user"]}, _Type, _Attr, 
      [{"set", {struct, [{"language", Lang}]}}], User) ->
    hn_users:update(Site, User, "language", Lang);

ipost(#refX{site = S, path = P}, _Type, _Attr, 
      [{"set", {struct, [{"list", {array, Array}}]}}], User) ->
    ok = status_srv:update_status(User, S, P, "edited page"),
    {Lasts, Refs} = fix_up(Array, S, P),
    ok = hn_db_api:write_last(Lasts),
    ok = hn_db_api:write_attributes(Refs);

ipost(#refX{site = S, path = P} = Ref, Type, _Attr, 
      [{"set", {struct, Attr}}], User) ->
    ok = status_srv:update_status(User, S, P, "edited page"),
    case Attr of
        % TODO : Get Rid of this (for pasting a range of values)
        [{"formula",{array, Vals}}] ->
            post_range_values(Ref, Vals),
            ok;

        % if posting a formula to a row or column, append
        [{"formula", Val}] when Type == column; Type == row ->
            hn_db_api:write_last([{Ref, Val}]);
        
        _Else ->
            hn_db_api:write_attributes([{Ref, Attr}])
    end;

ipost(Ref, _Type, _Attr, [{"clear", What}], _User) 
  when What == "contents"; What == "style"; What == "all" ->
    hn_db_api:clear(Ref, list_to_atom(What));

ipost(#refX{site = Site} = _Ref, _Type, _Attr, 
      [{"saveview", {struct, [{"name", Name}, {"tpl", Form}]}}], User) ->

    case can_save_view(User, Name) of
        true ->
            TplFile = [docroot(Site), "/" , Name ++ ".tpl"],
            TransFile = [docroot(Site), "/" , Name ++ ".trans"],
            ok = filelib:ensure_dir(TplFile),
            ok = file:write_file(TplFile, Form),
            Transactions = io_lib:fwrite("~p.~n", [hn_security:make_security(Form)]),
            ok = file:write_file(TransFile, Transactions);
        
        false -> 
            err
    end;
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back_create handler                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Type, _Attr,
      [{"action", "notify_back_create"}|T], _User) ->

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
        not_yet_synched -> ?exit
    end,
    {struct, Return} = hn_db_api:register_hn_from_web(ParentX, ChildX, 
                                                      Proxy, Biccie),
    Return2 = lists:append([Return, [{"stamp", Stamp}]]),
    {struct, Return2};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handler                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Type, _Attr,
      [{"action", "notify_back"} |T] = _Json, _User) ->
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
    {struct, [{"result", "success"}, {"stamp", Stamp}]};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify handler                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Ref, _Type, _Attr, [{"action", "notify"} | T] = _Json, _User) ->
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
            ?exit
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
                    not_yet_synched -> ?exit
                end
        end,
    [Fun(X) || X <- CVsn],
    {struct, [{"result", "success"}, {"stamp", Stamp}]};

ipost(_Ref, _Type, _Attr, _Post, _User) ->
    ?ERROR("404~n-~p~n-~p~n-~p",[_Ref, _Attr, _Post]),
    error.

can_save_view(User, "_u/"++FName) ->
    [Name | _] = string:tokens(FName, "/"),
    Name == hn_users:name(User);

can_save_view(User, "_g/"++FName) ->
    [Group | _] = string:tokens(FName, "/"),
    lists:member(Group, hn_users:groups(User)).

%% Some clients dont send ip in the host header
get_host(Req) ->
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
    lists:concat(["http://", Host, ":", Port, Req:get(path)]).


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
    code:lib_dir(hypernumbers) ++ "/../../var/docroot/"
        ++ hn_util:parse_site(Site).
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

remoting_request(Req, Site, Paths, Time) ->
    Socket = Req:get(socket),
    inet:setopts(Socket, [{active, once}]),
    remoting_reg:request_update(Site, Paths, ltoi(Time), self()),
    receive 
        {tcp_closed, Socket} -> ok;
        {error, timeout}     -> Req:ok({"text/html",?hdr, <<"timeout">>});
        {msg, Data}          -> json(Req, Data)
    after
        % TODO : Fix, should be controlled by remoting_reg
        600000 ->
            json(Req, {struct, [{"time", remoting_reg:timestamp()},
                                {"timeout", "true"}]})
    end.

get_var_or_cookie(Key, Vars, Req) ->

    case lists:keysearch(Key, 1, Vars) of
        false ->
            {ok, Req:get_cookie_value(Key)};
        {value, {"auth", Auth}} ->
            {ok, Auth}
    end.

page_attributes_CHEATING(Ref) ->
    Data = page_attributes(Ref, anonymous),
    (mochijson:encoder([{input_encoding, utf8}]))(Data).

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
    Perms = case auth_srv:can_write(S, {Name, Groups}, P) of
                true  -> {"permissions", {array, ["read", "write"]}};
                false -> {"permissions", {array, ["read"]}}
            end,

    Views = {views, {array, auth_srv:get_views(S, {Name, Groups}, P)}},
    
    {struct, [Time, Usr, Host, Lang, Perms, Grps, Views
              | dict_to_struct(Dict)]}.

make_after(#refX{obj = {cell, {X, Y}}} = RefX) ->
    RefX#refX{obj = {cell, {X - 1, Y - 1}}};
make_after(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    DiffX = X2 - X1 - 1,
    DiffY = Y2 - Y1 - 1,
    RefX#refX{obj = {range, {X1 - DiffX, Y1 - DiffY, X2 - DiffX, Y2 - DiffY}}};
make_after(#refX{obj = {column, {X1, X2}}} = RefX) ->
    DiffX = X2 - X1 - 1,
    RefX#refX{obj = {column, {X1 - DiffX, X2 - DiffX}}};
make_after(#refX{obj = {row, {Y1, Y2}}} = RefX) ->
    DiffY = Y2 - Y1 - 1,
    RefX#refX{obj = {row, {Y1 - DiffY, Y2 - DiffY}}}. %

pages(#refX{} = RefX) ->
    Dict = hn_db_api:read_page_structure(RefX),
    Tmp = pages_to_json(dh_tree:add(RefX#refX.path, Dict)),    
    {struct, [{"name", "home"}, {"children", {array, Tmp}}]}.

get_lang(anonymous) ->
    "en_gb";
get_lang(User) ->
    case hn_users:get(User, "language") of
        {ok, Lang} -> Lang;
        undefined  -> "en_gb"
    end.

json(Req, Data) ->
    Json = (mochijson:encoder([{input_encoding, utf8}]))(Data),
    Req:ok({"application/json", ?hdr, Json}).

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

serve_html(Req, File, User) ->
    serve_html(200, Req, File, User).
serve_html(Status, Req, File, User) ->
    F = fun() ->
                hn_util:compile_html(File, get_lang(User))
        end,    
    serve_file(Status, Req, cache(File, File++"."++get_lang(User), F)).

serve_file(Status, Req, File) ->
    case file:open(File, [raw, binary]) of
        {ok, IoDevice} ->
            Req:respond({Status, ?hdr, {file, IoDevice}}),
            file:close(IoDevice);
        _ ->
            Req:not_found()
    end.

cache(Source, CachedNm, Generator) ->
    Cached = tmpdir() ++ "/" ++ hn_util:bin_to_hexstr(erlang:md5(CachedNm)),
    ok = filelib:ensure_dir(Cached),
    case not( filelib:is_file(Cached) )
        orelse hn_util:is_older(Cached, Source) of
        true -> ok = file:write_file(Cached, Generator());
        _    -> ok
    end,
    Cached.

content_type(Req) ->
    {value, {'Accept', Accept}} =
        mochiweb_headers:lookup('Accept', Req:get(headers)),
    case re:run(Accept, "application/json") of
        {match, _} -> json;
        nomatch    ->
            case re:run(Accept, "\\*/\\*") of
                {match, _} -> html;
                nomatch    -> throw(unmatched_type)
            end
    end.

get_auth(_User, _Groups, 'GET', _Ref, [{"updates", _Time}, {"path", _P}]) ->
    % TODO: apply permissions to updates
    true;
get_auth(User, Groups, 'GET', #refX{site = Site, path = Path}, []) ->
    auth_srv:check_get_page(Site, {User, Groups}, Path);
get_auth(User, Groups, 'GET', #refX{site = Site, path = Path},
         [{"view", View}]) ->
    auth_srv:check_get_page(Site, {User, Groups}, Path, View);
get_auth(User, Groups, 'GET', #refX{site = Site, path = Path}, Vars) ->
    case lists:keyfind("path", 1, Vars) of
        {"path", P2} ->
            P3 = case P2 of
                     "/" -> [];
                     _   -> util2:chop(P2)
                 end,
            auth_srv:check_get_page(Site, {User, Groups}, P3);
        false ->
            auth_srv:check_get_page(Site, {User, Groups}, Path)
    end;
get_auth(User, Groups, 'POST', #refX{site = Site, path = Path}, _Vars) ->
    auth_srv:can_write(Site, {User, Groups}, Path).

'500'(Req, Error) ->
    error_logger:error_msg("~p~n~p~n", [Error, erlang:get_stacktrace()]),
    Req:respond({500,[],[]}).

'404'(Req, User) ->
    #refX{site = Site} = hn_util:parse_url(get_host(Req)),
    serve_html(404, Req, docroot(Site)++"/_global/login.html", User).

build_tpl(Site, Tpl) ->
    {ok, Master} = file:read_file([docroot(Site), "/_global/built.tpl"]),
    {ok, Gen}    = file:read_file([docroot(Site), "/", Tpl, ".tpl"]),

    New = re:replace(Master, "%BODY%", hn_util:esc_regex(Gen),
                     [{return, list}]),
    file:write_file([docroot(Site), "/", Tpl, ".html"], New).
    
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
