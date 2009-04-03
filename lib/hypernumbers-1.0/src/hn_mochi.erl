%%% @author Dale Harvey
%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests

-module(hn_mochi).

-include("regexp.hrl").
-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-export([ req/1, style_to_css/2, parse_ref/1 ]).

-define(json(Req, Data), 
        Req:ok({"application/json", mochijson:encode(Data)})).

-define(exit, 
        exit("exit from hn_mochi:handle_req impossible page versions")).

req(Req) ->

    %% Get the domain:port
    {ok, Port} = inet:port(Req:get(socket)),
    [Domain | _] = string:tokens(Req:get_header_value("host"), ":"), 
    Url = lists:concat(["http://",Domain,":",itol(Port),Req:get(path)]),
    
    case filename:extension(Req:get(path)) of 
        
        %% Serve Static Files
        X when X == ".png"; X == ".css"; X == ".js"; X == ".ico" ->
            "/"++RelPath = Req:get(path),
            Req:serve_file(RelPath, docroot());
        
        [] ->
            case catch do_req(Req, parse_ref(Url)) of 
                ok -> ok;
                Else ->   
                    ?ERROR("~p~n",[Else]),
                    Req:respond({500,[],[]})
            end
    end.

do_req(Req, Ref) ->
    Vars   = Req:parse_qs(),
    Method = Req:get(method),
    
    {ok, Auth} = get_var_or_cookie("auth", Vars, Req),
    User = case hn_users:verify_token(Auth) of
               {ok, X} -> X;
               invalid -> anonymous
           end,
    
    {ok, Access} = hn_users:get_access_level(User, Ref),

    case check_auth(Access, Ref#refX.path, Method)  of 
        login -> Req:serve_file("hypernumbers/login.html", docroot());
        ok    -> handle_req(Method, Req, Ref, Vars)
    end,
    ok.

check_auth(no_access, ["_auth"|_], _) -> ok;
check_auth(no_access, _, _)           -> login;
check_auth(read,      _, 'GET')       -> ok;
check_auth(write,     _, _)           -> ok;
check_auth(admin,     _, _)           -> ok.

handle_req(Method, Req, Ref, Vars) ->
    Type = element(1, Ref#refX.obj),
    case Method of
        'GET'  -> iget(Req, Ref, Type, Vars);
        'POST' -> 
            {ok, Post} = get_json_post(Req:recv_body()),
            case ipost(Req, Ref, Type, Vars, Post) of
                ok  -> ?json(Req, "success");
                ret -> ok
            end
    end.    

iget(Req, _Ref, page, []) ->
    Req:serve_file("hypernumbers/index.html", docroot());

iget(Req, Ref, page, [{"updates", Time}]) ->
    remoting_request(Req, Ref, Time);

iget(Req, Ref, page, [{"attr", []}]) -> 
    ?json(Req, page_attributes(Ref));

iget(Req, Ref, cell, [{"attr", []}]) ->
    Dict = to_dict(hn_db_api:read(Ref), dh_tree:new()),
    JS = case dict_to_struct(Dict) of
             [] -> {struct, []};
             [{_Cells, {struct, [{_Y, {struct, [{_X, JSON}]}}]}}] ->
                 JSON
         end, 
    ?json(Req, JS);

iget(Req, Ref, cell, []) ->
    V = case hn_db_api:read_attributes(Ref,["value"]) of
            [{_Ref, {"value", Val}}] -> Val;
            _Else -> ""
        end,
    Req:ok({"text/html",V});

iget(Req, Ref, _Type,  Attr) ->
    ?ERROR("404~n-~p~n-~p",[Ref, Attr]),
    Req:not_found().

ipost(_Req, Ref, _Type, _Attr, [{"drag", {_, [{"range", Rng}]}}]) ->
    hn_db_api:drag_n_drop(Ref, Ref#refX{obj = parse_attr(range,Rng)}),
    ok;

ipost(Req, #refX{path=["_auth","login"]}, _Type, _Attr, Data) ->
    [{"email", Email},{"pass", Pass},{"remember", Rem}] = Data,
    Resp = case hn_users:login(Email, Pass, Rem) of
               {error, invalid_user} -> 
                   [{"response","error"}];
               {ok, Token} ->
                   [{"response","success"},{"token",Token}]
           end,
    ?json(Req, {struct, Resp}),
    ret;

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"}])
  when O == row orelse O == column ->
    RefX2 = make_before(Ref),
    hn_db_api:insert(RefX2);

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"}])
  when O == row orelse O == column ->
    hn_db_api:insert(Ref);

% by default cells and ranges displace vertically
ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"}])
  when O == cell orelse O == range ->
    RefX2 = make_before(Ref),
    hn_db_api:insert(RefX2, vertical);

% by default cells and ranges displace vertically
ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"}])
  when O == cell orelse O == range ->
    hn_db_api:insert(Ref);

% but you can specify the displacement explicitly
ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "before"},
                                                      {"displacement", D}])
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    RefX2 = make_before(Ref),
    hn_db_api:insert(RefX2, list_to_existing_atom(D));

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"insert", "after"},
                                                      {"displacement", D}])
  when O == cell orelse O == range,
       D == "horizontal" orelse D == "vertical" ->
    hn_db_api:insert(Ref, list_to_existing_atom(D));

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"delete", "all"}])
  when O == row orelse O == column ->
    hn_db_api:delete(Ref);

ipost(_Req, #refX{obj = {O, _}} = Ref, _Type, _Attr, [{"delete", Direction}])
  when O == cell orelse O == range,
       Direction == "horizontal" orelse Direction == "vertical" ->
    hn_db_api:delete(Ref, Direction);

ipost(_Req, Ref, range, _Attr, [{"copy", {struct, [{"range", Range}]}}]) ->
    hn_db_api:copy_n_paste(Ref#refX{obj = parse_attr(range, Range)}, Ref);

ipost(_Req, Ref, _Type, _Attr, [{"set", {struct, Attr}}]) ->
    case Attr of 
        [{"formula",{array,Vals}}] ->
            %% TODO : Get Rid of this
            post_range_values(Ref, Vals),
            ok;
        _Else ->
            hn_db_api:write_attributes(Ref, Attr)
    end;

ipost(_Req, Ref, _Type, _Attr, [{"clear", What}]) 
  when What == "contents"; What == "style"; What == "all" ->
    hn_db_api:clear(Ref, list_to_atom(What));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back_create handler                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Req, Ref, _Type, _Attr,
      [{"action", "notify_back_create"}|T] = Json) ->
    Biccie   = from("biccie",     T),
    Proxy    = from("proxy",      T),
    ChildUrl = from("child_url",  T),
    PVsJson  = from("parent_vsn", T),
    CVsJson  = from("child_vsn",  T),

    #refX{site = Site} = Ref,
    ParentX = Ref,
    ParentUrl = hn_util:refX_to_url(ParentX),    
    ChildX = hn_util:url_to_refX(ChildUrl),

    bits:log("RECEIVED£" ++ pid_to_list(self()) ++ "£" ++ ParentUrl ++
             "£ from £" ++ ChildUrl ++ json_util:to_str(Json)),
    
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
    Req:ok({"application/json", mochijson:encode(Return)}),
    ret;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handler                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipost(Req, Ref, _Type, _Attr,
      [{"action", "notify_back"} |T] = Json) ->
    Biccie    = from("biccie",     T),
    ChildUrl  = from("child_url",  T),
    ParentUrl = from("parent_url", T),
    Type      = from("type",       T),
    PVsJson   = from("parent_vsn", T),
    CVsJson   = from("child_vsn",  T),

    bits:log("RECEIVED£" ++ pid_to_list(self()) ++ "£" ++ ParentUrl ++
             "£ from £" ++ ChildUrl ++ json_util:to_str(Json)),

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
ipost(Req, Ref, _Type, _Attr, [{"action", "notify"} | T] = Json) ->
    Biccie    = from("biccie",     T),
    ParentUrl = from("parent_url", T),
    Type      = from("type",       T),
    Payload   = from("payload",    T),
    PVsJson   = from("parent_vsn", T),
    CVsJson   = from("child_vsn",  T),
    
    ParentX = hn_util:url_to_refX(ParentUrl),
    ChildX = Ref,
    ChildUrl = hn_util:refX_to_url(ChildX),

    bits:log("RECEIVED£" ++ pid_to_list(self()) ++ "£" ++ ChildUrl ++
             "£ from £" ++ ParentUrl ++ json_util:to_str(Json)),
    
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

ipost(Req, _Ref, _Type, _Attr, _Post) ->
    ?ERROR("404~n-~p~n-~p~n-~p",[_Ref, _Attr, _Post]),
    Req:not_found(),
    ok.

get_json_post(undefined) ->
    {ok, undefined};
get_json_post(Json) ->
    {struct, Attr} = mochijson:decode(Json),
    {ok, Attr}.

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
add_ref(#refX{ obj = {Ref, {X,Y}}}, {Name, Val}, JSON) ->
    dh_tree:set([atom_to_list(Ref), itol(Y), itol(X), Name], Val, JSON).

docroot() ->
    code:priv_dir(hypernumbers) ++ "/docroot".

itol(X) ->
    integer_to_list(X).
ltoi(X) ->
    list_to_integer(X).

is_dict(Dict) when is_tuple(Dict) ->
    dict == element(1,Dict);
is_dict(_Else) -> 
    false.

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

parse_ref("http://"++Url) ->
    [Host | Path] = string:tokens(Url, "/"), 
    case lists:last(Url) of
        $/ -> #refX{site="http://"++Host, path=Path, obj={page, "/"}};
        _  -> 
            [Addr | P] = lists:reverse(Path),
            Obj = parse_attr(cell, Addr),
            #refX{site="http://"++Host, path=lists:reverse(P), obj = Obj}
    end.

parse_attr(cell, Addr) ->
    case regexp:match(Addr,?RG_cell) of
        {match,_,_} -> {cell, util2:strip_ref(Addr)};
        _           -> parse_attr(range, Addr)
    end;

parse_attr(range, Addr) ->
    case regexp:match(Addr,?RG_range) of
        {match,_,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {X1, Y1} = util2:strip_ref(Cell1),
            {X2, Y2} = util2:strip_ref(Cell2),
            {XX1, YY1, XX2, YY2} = hn_util:rectify_range(X1, Y1, X2, Y2),
            {range, {XX1, YY1, XX2, YY2}};
        _ -> 
            parse_attr(column, Addr)
    end;

parse_attr(column, Addr) ->
    case regexp:match(Addr,?RG_col_range) of
        {match,_,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {column, {tconv:b26_to_i(Cell1), tconv:b26_to_i(Cell2)}};
        _ -> 
            parse_attr(row, Addr)
    end;

parse_attr(row, Addr) ->
    case regexp:match(Addr,?RG_row_range) of
        {match,_,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {row, {ltoi(Cell1), ltoi(Cell2)}};
        _ -> 
            throw(invalid_reference)
    end.

styles_to_css([], Acc) ->
    Acc;
styles_to_css([H | T], Acc) ->
    styles_to_css(T, [style_to_css(H) | Acc]).

style_to_css({styles, _Ref, X, Rec}) ->
    style_to_css(X, Rec).

style_to_css(X, Rec) ->
    Num = ms_util2:no_of_fields(magic_style),
    {itol(X), style_att(Num+1, Rec, [])}.

style_att(1, _Rec, Acc) ->
    lists:flatten(Acc);
style_att(X, Rec, Acc) ->
    case element(X,Rec) of
        [] ->
            style_att(X-1, Rec, Acc);
        _Else -> 
            Name =  ms_util2:name_by_index(magic_style, X-1),
            A = io_lib:format("~s:~s;",[Name, element(X,Rec)]),
            style_att(X-1, Rec, [A | Acc])
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
                ok = hn_db_api:write_attributes(NRef,
                                                      [{"formula", Val}]),
                Acc+1 
        end,
    lists:foldl(F, 0, Values).


log_unsynched(Location, Site, Page, Vsn) ->
    bits:log("UNSYNCHED for "++ Location ++"£" ++ pid_to_list(self()) ++
             "£" ++ Site ++ "£ Page £" ++ Page ++ "£ Version £" ++
             tconv:to_s(Vsn)).

log_not_yet_synched(Severity, Location, Site, Page, Vsn) ->
    Msg = Severity ++ " NOT_YET_SYNCHED for " ++ Location ++ "£",
    bits:log(Msg ++ pid_to_list(self()) ++ "£" ++ Site ++
             "£ Page £" ++ Page ++"£ Version £" ++
             tconv:to_s(Vsn)).
    
remoting_request(Req, #refX{site=Site, path=Path}, Time) ->
    Socket = Req:get(socket),
    inet:setopts(Socket, [{active, once}]),
    remoting_reg:request_update(Site, Path, ltoi(Time), self()),
    receive 
        {tcp_closed, Socket} -> 
            ok;
        {error, timeout} -> 
            Req:ok({"text/html",<<"timeout">>});
        {msg, Data} ->
            Req:ok({"application/json", mochijson:encode(Data)})
    end.
                 
get_var_or_cookie(Key, Vars, Req) ->
    case lists:keysearch(Key, 1, Vars) of
        false ->
            {ok, Req:get_cookie_value(Key)};
        {value, {"auth", Auth}} ->
            {ok, Auth}
    end.

page_attributes(Ref) ->
    Init   = [["cell"], ["column"], ["row"], ["page"], ["styles"]],
    Tree   = dh_tree:create(Init),
    Styles = styles_to_css(hn_db_api:read_styles(Ref), []),
    NTree  = add_styles(Styles, Tree),
    Dict   = to_dict(hn_db_api:read(Ref), NTree),
    Time   = {"time", remoting_reg:timestamp()},
    {struct, [Time | dict_to_struct(Dict)]}.

make_before(#refX{obj = {cell, {X, Y}}} = RefX) ->
    RefX#refX{obj = {cell, {X - 1, Y - 1}}};
make_before(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    DiffX = X2 - X1 + 1,
    DiffY = Y2 - Y1 + 1,
    RefX#refX{obj = {range, {X1 - DiffX, Y1 - DiffY, X2 - DiffX, Y2 - DiffY}}};
make_before(#refX{obj = {column, {X1, X2}}} = RefX) ->
    DiffX = X2 - X1,
    RefX#refX{obj = {column, {X1 - DiffX, X2 - DiffX}}};
make_before(#refX{obj = {row, {Y1, Y2}}} = RefX) ->
    DiffY = Y2 - Y1,
    RefX#refX{obj = {row, {Y1 - DiffY, Y2 - DiffY}}}.
