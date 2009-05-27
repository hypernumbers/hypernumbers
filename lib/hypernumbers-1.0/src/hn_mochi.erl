%%% @author Dale Harvey
%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests

-module(hn_mochi).

-include("regexp.hrl").
-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-include_lib("gettext/include/gettext.hrl").

-export([ req/1, style_to_css/2 ]).

-define(hdr,[{"Cache-Control","no-store, no-cache, must-revalidate"},
             {"Expires",      "Thu, 01 Jan 1970 00:00:00 GMT"},
             {"Pragma",       "no-cache"},
             {"Status",       200}]).

-define(html, [{"Content-Type", "text/html"} | ?hdr]).

-define(json(Req, Data),    
        Json = (mochijson:encoder([{input_encoding, utf8}]))(Data),
        Req:ok({"application/json", ?hdr, Json})).

-define(exit, 
        exit("exit from hn_mochi:handle_req impossible page versions")).

req(Req) ->

    case filename:extension(Req:get(path)) of 
        
        %% Serve Static Files
        X when X == ".png"; X == ".css"; X == ".js"; 
               X == ".ico"; X == ".json" ->
            "/"++RelPath = Req:get(path),
            Req:serve_file(RelPath, docroot());
        
        [] ->
            case catch do_req(Req) of 
                ok -> ok;
                invalid_reference ->
                    Req:respond({500,[],[]});
                Else ->
                    ?ERROR("~p~n~p",[Else, erlang:get_stacktrace()]),
                    Req:respond({500,[],[]})
            end
    end.

do_req(Req) ->

    Ref    = hn_util:parse_url(get_host(Req)),
    #refX{site = Site} = Ref,
    Vars   = Req:parse_qs(),
    Method = Req:get(method),
    
    {ok, Auth} = get_var_or_cookie("auth", Vars, Req),
    User = case hn_users:verify_token(Site, Auth) of
               {ok, Usr}        -> Usr;
               {error, _Reason} -> anonymous
           end,
   
    {ok, Access} = hn_users:get_access_level(User, Ref),

    case check_auth(Access, Ref#refX.path, Method)  of 
        login -> Req:serve_file("hypernumbers/login.html", docroot(), ?hdr);
        ok    -> handle_req(Method, Req, Ref, Vars, User)
    end,
    ok.

check_auth(no_access, ["_user"|_], _) -> ok;
check_auth(no_access, _, _)           -> login;
check_auth(read,      _, 'GET')       -> ok;
check_auth(write,     _, _)           -> ok;
check_auth(admin,     _, _)           -> ok.

handle_req(Method, Req, Ref, Vars, User) ->
    
    Type = element(1, Ref#refX.obj),
    case Method of
        'GET'  -> 
            mochilog:log(Req, Ref, hn_users:name(User), undefined),
            iget(Req, Ref, Type, Vars, User);

        'POST' ->
            
            {value, {'Content-Type', Ct}} =
                mochiweb_headers:lookup('Content-Type', Req:get(headers)),
            
            %% TODO: Log file uploads.
            case string:substr(Ct, 1, 19) of

                "multipart/form-data" ->
                    Data = hn_file_upload:handle_upload(Req, Ref, User),
                    Json = (mochijson:encoder([{input_encoding, utf8}]))(Data),
                    Req:ok({"text/html", ?hdr, Json});

                _Else ->
                    Body = Req:recv_body(),
                    {ok, Post} = get_json_post(Body),

                    mochilog:log(Req, Ref, hn_users:name(User), Body),
                    case ipost(Req, Ref, Type, Vars, Post, User) of
                        ok  -> ?json(Req, "success");
                        ret -> ok
                    end
            end
    end.    

serve_html(Req, File, anonymous) ->
    serve_file(Req, ensure(File, "en_gb"));

serve_html(Req, File, User) ->
    Ext  = case hn_users:get(User, "language") of
               {ok, Lang} -> Lang;
               undefined  -> "en_gb"
          end,
    serve_file(Req, ensure(File, Ext)).

serve_file(Req, File) ->
    case file:open(docroot() ++ "/" ++ File, [raw, binary]) of
        {ok, IoDevice} ->
            Req:ok({"text/html",?hdr,{file, IoDevice}}),
            file:close(IoDevice);
        _ ->
            Req:not_found()
    end.


ensure(File, Lang) ->
    Path = docroot() ++ "/" ++ File,
    case filelib:is_file(Path++"."++Lang) of 
        true  -> ok;
        false -> hn_util:compile_html(Path, Lang)
    end,
    File++"."++Lang.

iget(Req, #refX{path=["_user", "login"]}, page, [], User) ->
    serve_html(Req, "hypernumbers/login.html", User);
iget(Req, _Ref, page, [], User) ->
    serve_html(Req, "hypernumbers/index.html", User);
iget(Req, Ref, page, [{"updates", Time}], _User) ->
    remoting_request(Req, Ref, Time);
iget(Req, Ref, page, [{"pages", []}], _User) -> 
    ?json(Req, pages(Ref));
iget(Req, Ref, page, [{"attr", []}], User) -> 
    ?json(Req, page_attributes(Ref, User));
iget(Req, Ref, cell, [{"attr", []}], _User) ->
    % ok = fprof:trace(start),
    Dict = to_dict(hn_db_api:read_whole_page(Ref), dh_tree:new()),
    JS = case dict_to_struct(Dict) of
             [] -> {struct, []};
             [{_Cells, {struct, [{_Y, {struct, [{_X, JSON}]}}]}}] ->
                 JSON
         end, 
    Ret = ?json(Req, JS),
    % ok = fprof:trace(stop),
    Ret;
iget(Req, Ref, cell, [], _User) ->
    V = case hn_db_api:read_attributes(Ref,["value"]) of
            [{_Ref, {"value", Val}}]           -> Val; 
            _Else                              -> "" 
        end,
    Req:ok({"text/html",V});
iget(Req, Ref, _Type,  Attr, _User) ->
    ?ERROR("404~n-~p~n-~p",[Ref, Attr]),
    Req:not_found().

ipost(_Req, Ref, _Type, _Attr, [{"drag", {_, [{"range", Rng}]}}], _User) ->
    hn_db_api:drag_n_drop(Ref, Ref#refX{obj = hn_util:parse_attr(range,Rng)}),
    ok;

ipost(Req, #refX{site = Site, path=["_user","login"]}, _T, _At, Data, _User) ->
    [{"email", Email},{"pass", Pass},{"remember", Rem}] = Data,
    Resp = case hn_users:login(Site, Email, Pass, Rem) of
               {error, invalid_user} -> 
                   [{"response","error"}];
               {ok, Token} ->
                   [{"response","success"},{"token",Token}]
           end,
    ?json(Req, {struct, Resp}),
    ret;

%% the purpose of this message is to mark the mochilog so we don't need to do nothing
%% with anything...
ipost(_Req, _Ref, _Type, [{"mark", []}], [{"set",{struct, [{"mark", Msg}]}}], _User) ->
    io:format("marking ~p~n", [Msg]),
    ok;

%% the purpose of this message is to write a GUI trail in the mochilog so we 
%% don't need to do nothingwith anything...
ipost(_Req, _Ref, _Type, [{"trail", []}], [{"set",{struct, [{"trail", Msg}]}}], _User) ->
    io:format("trailing ~p~n", [Msg]),
    ok;

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"}], _User)
  when O == row orelse O == column ->
    hn_db_api:insert(Ref);

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"}], _User)
  when O == row orelse O == column ->
    RefX2 = make_after(Ref), 
    hn_db_api:insert(RefX2);

% by default cells and ranges displace vertically
ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"}], _User)
  when O == cell orelse O == range ->
    hn_db_api:insert(Ref, vertical);

% by default cells and ranges displace vertically
ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"}], _User)
  when O == cell orelse O == range ->
    RefX2 = make_after(Ref),
    hn_db_api:insert(RefX2);

% but you can specify the displacement explicitly
ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"},
                                                      {"displacement", D}], _User)
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    hn_db_api:insert(Ref, list_to_existing_atom(D));

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"},
                                                      {"displacement", D}], _User)
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    RefX2 = make_after(Ref),
    hn_db_api:insert(RefX2, list_to_existing_atom(D));

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"delete", "all"}], _User)
  when O == row orelse O == column orelse O == page->
    hn_db_api:delete(Ref);

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"delete", Direction}], _User)
  when O == cell orelse O == range,
       Direction == "horizontal" orelse Direction == "vertical" ->
    hn_db_api:delete(Ref, Direction);

ipost(_Req, Ref, range, _Attr, [{"copy", {struct, [{"range", Range}]}}], _User) ->
    hn_db_api:copy_n_paste(Ref#refX{obj = hn_util:parse_attr(range, Range)}, Ref);

ipost(Req, _Ref, _Type, _Attr,
      [{"set", {struct, [{"language", _Lang}]}}], anonymous) ->
    ?json(Req, {struct, [{"error", "cant set language for anonymous users"}]}),
    ret;

ipost(_Req, #refX{site = Site, path=["_user"]}, _Type, _Attr, 
      [{"set", {struct, [{"language", Lang}]}}], User) ->
    hn_users:update(Site, User, "language", Lang);

ipost(_Req, Ref, _Type, _Attr, [{"set", {struct, Attr}}], _User) ->
    case Attr of 
        [{"formula",{array, Vals}}] ->
            %% TODO : Get Rid of this
            post_range_values(Ref, Vals),
            ok;
        _Else ->
            hn_db_api:write_attributes(Ref, Attr)
    end;

ipost(_Req, Ref, _Type, _Attr, [{"clear", What}], _User) 
  when What == "contents"; What == "style"; What == "all" ->
    hn_db_api:clear(Ref, list_to_atom(What));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back_create handler                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Req, Ref, _Type, _Attr,
      [{"action", "notify_back_create"}|T] = Json, _User) ->
    Biccie   = from("biccie",     T),
    Proxy    = from("proxy",      T),
    ChildUrl = from("child_url",  T),
    PVsJson  = from("parent_vsn", T),
    CVsJson  = from("child_vsn",  T),

    #refX{site = Site} = Ref,
    ParentX = Ref,
    _ParentUrl = hn_util:refX_to_url(ParentX),    
    ChildX = hn_util:url_to_refX(ChildUrl),

    % bits:log("RECEIVED£" ++ pid_to_list(self()) ++ "£" ++ ParentUrl ++
    %         "£ from £" ++ ChildUrl ++ json_util:to_str(Json)),
    
    % there is only 1 parent and 1 child for this action
    PVsn = json_util:unjsonify(PVsJson),
    CVsn = json_util:unjsonify(CVsJson),
    #version{page = PP, version = PV} = PVsn,
    #version{page = CP, version = CV} = CVsn,
    Sync1 = hn_db_api:check_page_vsn(Site, PVsn),
    Sync2 = hn_db_api:check_page_vsn(Site, CVsn),
    case Sync1 of
        synched        -> ok;
        unsynched      -> log_unsynched("notify_back_create", Site, PP, PV),
                          hn_db_api:resync(Site, PVsn);
        not_yet_synched -> ok % the child gets the version in this call...
    end,
    case Sync2 of
        synched         -> ok;
        unsynched       -> log_unsynched("notify_back_create", Site, CP, CV),
                           hn_db_api:resync(Site, CVsn);
        not_yet_synched -> log_not_yet_synched("FATAL", "notify_back_create",
                                               Site, CP, CV),
                           ?exit
    end,
    Return = hn_db_api:register_hn_from_web(ParentX, ChildX, Proxy, Biccie),
    ?json(Req, Return),
    ret;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handler                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Req, Ref, _Type, _Attr,
      [{"action", "notify_back"} |T] = _Json, _User) ->
    Biccie    = from("biccie",     T),
    ChildUrl  = from("child_url",  T),
    ParentUrl = from("parent_url", T),
    Type      = from("type",       T),
    PVsJson   = from("parent_vsn", T),
    CVsJson   = from("child_vsn",  T),

    % bits:log("RECEIVED£" ++ pid_to_list(self()) ++ "£" ++ ParentUrl ++
    %         "£ from £" ++ ChildUrl ++ json_util:to_str(Json)),

    % there is only 1 parent and 1 child here
    PVsn = json_util:unjsonify(PVsJson),
    CVsn = json_util:unjsonify(CVsJson),
    #version{page = PP, version = PV} = PVsn,
    #version{page = CP, version = CV} = CVsn,
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
            log_unsynched("notify_back", Site, PP, PV),
                           hn_db_api:resync(Site, PVsn);
        not_yet_synched -> 
            log_not_yet_synched("NOT FATAL", "notify_back",
                                Site, CP, CV),
            ok = hn_db_api:initialise_remote_page_vsn(Site, PVsn)
    end,
    case Sync2 of
        synched -> ok;
        unsynched -> 
            log_unsynched("notify_back", Site, PP, PV),
            ok = hn_db_api:resync(Site, CVsn);
        not_yet_synched -> 
            log_not_yet_synched("NOT FATAL", "notify_back",
                                Site, CP, CV),
            ok = hn_db_api:initialise_remote_page_vsn(Site, CVsn)
    end,
    Req:ok({"application/json", "success"}),
    ok;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify handler                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Req, Ref, _Type, _Attr, [{"action", "notify"} | T] = _Json, _User) ->
    Biccie    = from("biccie",     T),
    ParentUrl = from("parent_url", T),
    Type      = from("type",       T),
    Payload   = from("payload",    T),
    PVsJson   = from("parent_vsn", T),
    CVsJson   = from("child_vsn",  T),
    
    ParentX = hn_util:url_to_refX(ParentUrl),
    ChildX = Ref,
    _ChildUrl = hn_util:refX_to_url(ChildX),

    % bits:log("RECEIVED£" ++ pid_to_list(self()) ++ "£" ++ ChildUrl ++
    %         "£ from £" ++ ParentUrl ++ json_util:to_str(Json)),
    
    #refX{site = Site} = ChildX,
    PVsn = json_util:unjsonify(PVsJson),
    CVsn = json_util:unjsonify(CVsJson),
    #version{page = PP, version = PV} = PVsn,
    
    Sync1 = case Type of
                "insert"    -> hn_db_api:incr_remote_page_vsn(Site, PVsn, Payload);
                "delete"    -> hn_db_api:incr_remote_page_vsn(Site, PVsn, Payload);
                "new_value" -> hn_db_api:check_page_vsn(Site, PVsn)
            end,
    % there is one parent and it if is out of synch, then don't process it, ask for a
    % resynch
    case Sync1 of
        synched -> 
            ok = hn_db_api:notify_from_web(ParentX, Ref, Type,
                                                 Payload, Biccie);
        unsynched -> 
            log_unsynched("notify", Site, PP, PV),
            ok = hn_db_api:resync(Site, PVsn);
        not_yet_synched -> 
            log_not_yet_synched("FATAL", "notify", Site, PP, PV),
            ?exit
    end,
    % there are 1 to many children and if they are out of synch ask for 
    % a resynch for each of them
    Fun =
        fun(X) ->
                Sync2 = hn_db_api:check_page_vsn(Site, X),
                #version{page = CP, version = CV} = X,
                case Sync2 of
                    synched         -> ok;
                    unsynched       -> log_unsynched("notify", Site, CP, CV),
                                       ok = hn_db_api:resync(Site, X);
                    not_yet_synched -> log_not_yet_synched("FATAL", "notify",
                                                           Site, CP, CV),
                                       ?exit
                end
        end,
    [Fun(X) || X <- CVsn],
    Req:ok({"application/json", "success"});

ipost(Req, _Ref, _Type, _Attr, _Post, _User) ->
    ?ERROR("404~n-~p~n-~p~n-~p",[_Ref, _Attr, _Post]),
    Req:not_found(),
    ok.

%% Some clients dont send ip in the host header
get_host(Req) ->
    {ok, Port} = inet:port(Req:get(socket)),
    [Domain | _] = string:tokens(Req:get_header_value("host"), ":"), 
    lists:concat(["http://", Domain, ":", itol(Port), Req:get(path)]).

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

docroot() ->
    code:priv_dir(hypernumbers) ++ "/docroot".

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
    F = fun(Val, Acc) -> 
                NRef = Ref#refX{obj = {cell, {X1 + Acc, Y1+Offset}}},
                ok = hn_db_api:write_attributes(NRef, [{"formula", Val}]),
                Acc+1 
        end,
    lists:foldl(F, 0, Values).

log_unsynched(_Location, _Site, _Page, _Vsn) ->
    % bits:log("UNSYNCHED for "++ Location ++"£" ++ pid_to_list(self()) ++
    %         "£" ++ Site ++ "£ Page £" ++ Page ++ "£ Version £" ++
    %         tconv:to_s(Vsn)),
    ok.

log_not_yet_synched(_Severity, _Location, _Site, _Page, _Vsn) ->
    % Msg = Severity ++ " NOT_YET_SYNCHED for " ++ Location ++ "£",
    % bits:log(Msg ++ pid_to_list(self()) ++ "£" ++ Site ++
    %         "£ Page £" ++ Page ++"£ Version £" ++
    %         tconv:to_s(Vsn)).
    ok.
    
remoting_request(Req, #refX{site=Site, path=Path}, Time) ->
    Socket = Req:get(socket),
    inet:setopts(Socket, [{active, once}]),
    remoting_reg:request_update(Site, Path, ltoi(Time), self()),
    receive 
        {tcp_closed, Socket} -> ok;
        {error, timeout}     -> Req:ok({"text/html",?hdr, <<"timeout">>});
        {msg, Data}          -> ?json(Req, Data)
    end.
                 
get_var_or_cookie(Key, Vars, Req) ->
    case lists:keysearch(Key, 1, Vars) of
        false ->
            {ok, Req:get_cookie_value(Key)};
        {value, {"auth", Auth}} ->
            {ok, Auth}
    end.

page_attributes(Ref, User) ->
    Init   = [["cell"], ["column"], ["row"], ["page"], ["styles"]],
    Tree   = dh_tree:create(Init),
    Tree2   = dh_tree:set(["user"], hn_users:name(User), Tree),
    Styles = styles_to_css(hn_db_api:read_styles(Ref), []),
    NTree  = add_styles(Styles, Tree2),
    Dict   = to_dict(hn_db_api:read_whole_page(Ref), NTree),
    Time   = {"time", remoting_reg:timestamp()},
    {struct, [Time | dict_to_struct(Dict)]}.

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

pages(#refX{path = [], obj = {page, "/"}} = RefX) ->
    {struct, dict_to_struct(hn_db_api:read_page_structure(RefX))};
pages(#refX{path = [H | _T], obj = {page, "/"}} = RefX) ->
    NewRefX = RefX#refX{path = [H]},
    {struct, dict_to_struct(hn_db_api:read_page_structure(NewRefX))}.


    
