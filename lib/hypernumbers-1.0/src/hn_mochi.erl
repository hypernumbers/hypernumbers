%%% @copyright 2008-2014 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(hn_mochi).

-include("hypernumbers.hrl").

-include_lib("kernel/include/file.hrl").
-include("gettext.hrl").
-include("hn_mochi.hrl").
-include("spriki.hrl").
-include("keyvalues.hrl").

-define(E, error_logger:error_msg).

-export([
         start/0
        ]).

-export([
         handle/1,
         extract_styles/1,
         style_to_css/1,
         page_attrs_for_export/2,
         page_attributes/2,
         get_json_post/1,    % Used for mochilog replay rewrites
         get_real_uri/1,
         get_real_params/1,
         cleanup/3,
         respond/2,
         '500'/1,
         '404'/2,
         '401'/2,
         serve_html/2,
         serve_html/3,
         json/2,
         xml/2,
         text_html/2,
         text_html_nocache/2,
         get_lang/1,
         try_sync/4,
         is_dict/1,
         post_login/6
        ]).

% exports for spawning
-export([
         provision_site/6
        ]).

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
    %% first try and process the request to see if it is all in order
    try
        Ref = hn_util:url_to_refX(get_real_uri(MochiReq)),
        Env = process_environment(MochiReq),
        Qry = process_query(Env),
        % URI = io_lib:format("~s", [get_real_uri(MochiReq)]),
        % Log = io_lib:format("~s", [Env#env.raw_body]),
        % hn_util:plain_log(URI, "/tmp/wordpress1.log"),
        % hn_util:plain_log(Log, "/tmp/wordpress1.log"),
        % hn_util:plain_log(Log, "/tmp/wordpress2.log"),
        handle2(Ref, Env, Qry)
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
            ?E(Format, Msg),
            Headers = mochiweb_headers:lookup('Content-Type',
                                              MochiReq:get(headers)),
            Env2 = #env{mochi = MochiReq, headers = Headers},
            '500'(Env2)
    end.

%% now try to actually do the request
handle2(#refX{site = S, path = P} = Ref, Env, Qry) ->
    try
        handle_(Ref, Env, Qry)
    catch
        ok          -> ok;
        exit:normal -> exit(normal);
        Type:What   ->
            Format = "web request failed~npath:  ~p~ntype:  ~p~nwhat:  ~p~n"
                ++"trace:~p~n",
            Msg    = [{path, P},
                      {type, Type},
                      {what, What},
                      {trace, erlang:get_stacktrace()}],
            case What of
                {badmatch, {error, enoent}} ->
                    F1 = "dumping script kiddie (should be 404)~n~p~n",
                    ?E(F1, [Env]),
                    log_path_errors(P, Format, Msg),
                    '500'(Env);
                invalid_url ->
                    Dir = hn_util:viewroot(S) ++ "/",
                    File = "invalidurl.html",
                    serve_html(404, Env, [Dir, File]);
                _ ->
                    ?E(Format, Msg),
                    '500'(Env)
            end
    end.

-spec handle_(#refX{}, #env{}, #qry{}) -> ok.

handle_(#refX{site = "http://www."++Site}, E = #env{mochi = Mochi}, _Qry) ->
    Redir = "http://" ++ hn_util:strip80(Site) ++ Mochi:get(raw_path),
    Redirect = {"Location", Redir},
    respond(301, E#env{headers = [Redirect | E#env.headers]});

% single signon stuff
handle_(#refX{path = [], type = url, obj = {filename, "vixo.commoncookie.js"}},
        #env{mochi = Mochi} = Env, _Qry) ->
    Auth = Mochi:get_cookie_value("auth"),
    {_, Cookie} = case Auth of
                      undefined -> Stamp = passport:temp_stamp(),
                                   hn_net_util:cookie("auth", Stamp, "never");
                      _         -> hn_net_util:cookie("auth", Auth,  "never")
             end,
    JS = io_lib:format("document.cookie=~p", [Cookie]),
    Headers = [{"Set-Cookie", Cookie} | Env#env.headers],
    javascript_nocache(Env#env{headers = Headers}, JS);

% this function does single sign on for WordPress
handle_(#refX{site = S, path = ["_sync", "wordpress", "logon" | _Rest]},
        Env, #qry{hypertag = Ht, ivector = IV}) ->
    case is_wordpress(S) of
        {true, Params} ->
            {Stamp, URL} = hn_wordpress:do_logon(S, Ht, IV, Params),
            Cookie = hn_net_util:cookie("auth", Stamp, "never"),
            Headers = Env#env.headers,
            Hs2 = [{"location", URL}, Cookie | Headers],
            Env2 = Env#env{headers = Hs2},
            respond(302, Env2);
        false ->
            exit("invalid attempt to do wordpress single sigon")
    end;

handle_(#refX{site = S, path = ["_sync" | Cmd]}, Env,
        #qry{return = QReturn, stamp = QStamp})
  when QReturn /= undefined ->
    Env2 = process_sync(Cmd, Env, QReturn, QStamp, S),
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
    {Auth, Env2} = case cluster_up() of
        false -> {disconnect, Env};
        true  -> hn_authorize:authorize(Env, Ref, Qry)
    end,
    case {Auth, Env2#env.accept} of
        {disconnect, _} ->
            text_html(Env, "There appears to be a network problem. "
                      ++ "Please try later");
        {allowed, _} ->
            handle_resource(Ref, Qry, Env2);
        {{view, View}, _} ->
            handle_resource(Ref, Qry#qry{view = View}, Env2);
        {not_found, html} ->
            ViewRoot = hn_util:viewroot(Ref#refX.site),
            serve_html(404, Env2, [ViewRoot, "/404.html"]);
        {not_found, json} ->
            respond(404, Env2);
        {303, _} ->
            respond(303, Env2),
            throw(ok);
        {denied, html} ->
            ViewRoot = hn_util:viewroot(Ref#refX.site),
            serve_html(401, Env2, [ViewRoot, "/401.html"]);
        {denied, json} ->
            respond(401, Env2);
        {{upload, denied}, html} ->
            Ret = {struct, [{error, "Permission denied: 401"}]},
            #env{mochi = Mochi} = Env,
            Json = (mochijson:encoder([{input_encoding, utf8}]))(Ret),
            Mochi:ok({"text/html", Json})
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

handle_resource(Ref, Qry, Env = #env{method = 'GET'}) ->
    mochilog:log(Env, Ref),
    ObjType = element(1, Ref#refX.obj),
    hn_get:get(Ref, ObjType, Qry, Env);

handle_resource(Ref, _Qry, Env = #env{method = 'POST', body = multipart,
                                      mochi = Mochi, uid = Uid}) ->
    {ok, UserName} = passport:uid_to_email(Uid),
    {ok, File, Name, Data} = hn_file_upload:handle_upload(Mochi, Ref,
                                                          UserName),
    {_St, {Ret, _}} = load_file(Ref, Data, File, Name, UserName, Uid),
    Env2 = Env#env{raw_body = {upload, Name}},
    mochilog:log(Env2, Ref),

    Mochi:ok({"text/html",
              (mochijson:encoder([{input_encoding, utf8}]))(Ret)});

handle_resource(Ref, Qry, Env = #env{method = 'POST'}) ->
    mochilog:log(Env, Ref),
    hn_post:post(Ref, Qry, Env).

-spec handle_static(string(), iolist(), any()) -> any().
handle_static(X, Site, Env)
  when X == ".png"; X == ".jpg"; X == ".css"; X == ".js"; X == ".txt";
X == ".ico"; X == ".json"; X == ".gif"; X == ".html"; X == ".htm";
X == ".pdf"; X == ".eot"; X == ".ttf"; X == ".svg" ->
    Mochi = Env#env.mochi,
    "/"++RelPath = Mochi:get(path),
    Root = hn_util:docroot(Site),
    Mochi:serve_file(RelPath, Root),
    ok;
handle_static(_X, Site, Env) ->
    serve_html(404, Env, [hn_util:viewroot(Site), "/404.html"]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec log_signup(#refX{}, string(), string(), atom(), auth_srv:uid(),
                 string(), string(), string()) -> ok.
log_signup(RefX, NewSite, SiteType, Node, Uid, Email, Zone, LoginURL) ->
    % write the signup to the new commision table
    case application:get_env(hypernumbers, featureflag) of
        {ok, on} ->
            io:format("logging the commission...~n"),
            #refX{site = S, path = P, obj = O} = RefX,
            Commission = #commission{uid = Uid, email = Email, site = NewSite,
                                     sitetype = SiteType, zone = Zone,
                                     link = LoginURL, comm_site = S,
                                     comm_path = P, comm_cell = O,
                                     synched = false},
            ok = new_db_api:write_commission_logD(Commission);
        {ok, off} ->
            io:format("not logging the commission...~n"),
            ok
    end,

    LogS = case application:get_env(hypernumbers, environment) of
               {ok, development} -> "http://hypernumbers.dev:9000";
               {ok, server_dev}  -> "http://dev.hypernumbers.com:8080";
               {ok, production}  -> "http://crm.vixo.com:80"
           end,
    LogP = make_log_path(Email),
    Paths = [LogP ++ "_sites/", "/_sites/"],
    Fun = fun(X) ->
                  Row = [{hn_util:url_to_refX(LogS ++ X ++ Ref), Val}
                         || {Ref, Val} <- [
                                           {"A:A", Email},
                                           {"B:B", "<a href='"++NewSite++
                                            "'>"++NewSite++"</a>"},
                                           {"C:C", Uid},
                                           {"D:D", dh_date:format("Y/m/d G:i:s")},
                                           {"E:E", atom_to_list(Node)}
                                          ]],
                  new_db_api:append_row(Row, nil, nil)
          end,
    [Fun(X) || X <- Paths],
    ST2 = atom_to_list(SiteType),
    Body = Email ++ " commissioned a " ++ ST2 ++ " site. Yip!",
    emailer:send_email("gordon@vixo.com;stephen@vixo.com", "", "robot@vixo.com",
                       "New " ++ ST2 ++ " site commissioned", Body).

make_log_path(Email) ->
    Email2 = re:replace(Email, "\\.", "-", [{return, list}, global]),
    [U, Domain] = string:tokens(Email2, "@"),
    "/users/" ++ Domain ++ "/" ++ U ++ "/".

%% Some clients dont send ip in the host header
get_real_params(Mochi) ->
    List = Mochi:parse_qs(),
    "?" ++ string:join([X ++ "=" ++ Y || {X, Y} <- List], ",").

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

get_json_post(Json) ->
    get_json_post(Json, json).

get_json_post(undefined, _) ->
    {ok, undefined};
get_json_post(HTML, html) ->
    {ok, HTML};
get_json_post(Json, json) ->
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
%% this is a range object which we don't want to send to the front
%% end, so we supress it
add_ref(Ref, [{range, _} | Tail], JSON) ->
    add_ref(Ref, Tail, JSON);
add_ref(Ref, [{"__rawvalue", _} = KV | Tail], JSON) ->
    JSON2 = add_ref1(Ref, hn_util:jsonify_val(KV), JSON),
    add_ref(Ref, Tail, JSON2);
add_ref(Ref, [{"__"++_Hidden, _} | Tail], JSON) ->
    add_ref(Ref, Tail, JSON);
add_ref(Ref, [KV | Tail], JSON) ->
    JSON2 = add_ref1(Ref, hn_util:jsonify_val(KV), JSON),
    add_ref(Ref, Tail, JSON2).

add_ref1(#xrefX{obj = {page, "/"}}, {Name, Val}, JSON) ->
    dh_tree:set(["page", Name], Val, JSON);
add_ref1(#xrefX{obj = {Ref, {X, Y}}}, Data, JSON) ->
    {Name, Val} = hn_util:jsonify_val(Data),
    dh_tree:set([atom_to_list(Ref), integer_to_list(Y),
                 integer_to_list(X), Name], Val, JSON).

tmpdir() ->
    code:lib_dir(hypernumbers) ++ "/../../var/tmp/".

is_dict(Dict) when is_tuple(Dict) -> dict == element(1, Dict);
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
        S <- new_db_api:read_styles_IMPORT(#refX{site = Site}) ].

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

% need to pay attention how you use this
% dynamic input attributes need to be cleaned up with
% hn_util:clean_up_page_attrs in certain circumstances
-spec page_attrs_for_export(#refX{}, #env{}) -> {struct, list()}.
page_attrs_for_export(Ref, Env) ->
    page_a2(Ref, Env, export).

-spec page_attributes(#refX{}, #env{}) -> {struct, list()}.
page_attributes(Ref, Env) ->
    page_a2(Ref, Env, full).

page_a2(#refX{site = S, path = P, obj = O} = Ref, Env, Type) ->
    #env{uid = UID} = Env,
    Content = new_db_api:read_intersect_ref(Ref),
    Content2 = case Type of
                   full   -> Content;
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
    Ob = case O of
             {page, "/"} -> "page";
             _           -> hn_util:obj_to_ref(O)
         end,
    Object  = {"object", Ob},
    Path    = {"path", hn_util:list_to_path(P)},
    Views   = {"views", {array, auth_srv:get_views(S, P, UID)}},
    Perms   = {"permissions", auth_srv:get_as_json(S, P)},
    {struct, [Time, Usr, Host, Path, Object, Perms, Views | dict_to_struct(Dict)]}.

clean_up_dyn_sel([], Acc) -> Acc;
clean_up_dyn_sel([{XRefX, Attrs} | T], Acc) ->
    Attrs2 = clean2(Attrs, []),
    clean_up_dyn_sel(T, [{XRefX, Attrs2} | Acc]).

clean2([], Acc) -> Acc;
clean2([{"input", {"dynamic_select", S, _Vals}} | T], Acc) ->
    clean2(T, [{"input", {"dynamic_select", S}} | Acc]);
clean2([H | T], Acc) ->
    clean2(T, [H | Acc]).

accept_type(Env) ->
    case Env:get_header_value('Accept') of
        undefined -> html; %cheapskate googlebots don't set accept header
        Accept    ->
            case re:run(Accept, "application/json") of
                {match, _} -> json;
                nomatch    -> html
            end
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
    Accept = accept_type(Mochi),
    {RawBody, Body} =
        case Mochi:get(method) of
            'GET'  -> {undefined, undefined};
            'HEAD' -> {undefined, undefined};
            'POST' -> Headers = mochiweb_headers:lookup('Content-Type',
                                                        Mochi:get(headers)),
                      case Headers of
                          none ->
                              RB = Mochi:recv_body(),
                              {ok, B} = get_json_post(RB, Accept),
                              {RB, B};
                          {_, {_,T} } ->
                              case lists:prefix("multipart/form-data", T) of
                                  true  -> {undefined, multipart};
                                  false -> RB = Mochi:recv_body(),
                                           {ok, B} = get_json_post(RB, Accept),
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
         accept   = Accept,
         method   = Mochi:get(method),
         raw_body = RawBody,
         body     = Body,
         auth     = Auth}.

%% Clears out auth cookie on current and main server.
-spec cleanup(string(), string(), #env{}) -> no_return().
cleanup(Site, Return, E) ->
    Cookie = hn_net_util:kill_cookie("auth"),
    E2 = E#env{headers = [Cookie | E#env.headers]},
    Redir = case try_sync(["reset"], Site, Return, ?NO_STAMP) of
                on_sync ->
                    Return;
                {redir, R} ->
                    R
            end,
    E3 = E2#env{headers = [{"location", Redir} | E2#env.headers]},
    {303, E3}.

-spec try_sync([string()], string(), string(), string())
-> {redir, string()} | on_sync.
try_sync(Cmd0, Site, Return, Stamp) ->
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

process_sync(["tell"], E, QReturn, undefined, Site) ->
    process_sync(["tell"], E, QReturn, [], Site);
process_sync(["tell"], E, QReturn, QStamp, _Site) ->
    Stamp = case mochiweb_util:unquote(QStamp) of
                []    -> passport:temp_stamp();
                Other -> Other
            end,
    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
    Return = mochiweb_util:unquote(QReturn),
    Redirect = {"Location", Return},
    E#env{headers = [Cookie, Redirect | E#env.headers]};
process_sync(["seek"], E = #env{mochi = Mochi}, QReturn, undefined, _Site) ->
    Stamp = case Mochi:get_cookie_value("auth") of
                undefined -> passport:temp_stamp();
                S         -> S
            end,
    Cookie = hn_net_util:cookie("auth", Stamp, "never"),
    Return = mochiweb_util:unquote(QReturn),
    #refX{site = OrigSite} = hn_util:url_to_refX(Return),
    QStamp = mochiweb_util:quote_plus(Stamp),
    Redir = hn_util:strip80(OrigSite) ++
        "/_sync/tell/?return="++QReturn++"&stamp="++QStamp,
    Redirect = {"Location", Redir},
    E#env{headers = [Cookie, Redirect | E#env.headers]};
process_sync(["reset"], E, QReturn, undefined, _Site) ->
    Cookie = hn_net_util:kill_cookie("auth"),
    Return = mochiweb_util:unquote(QReturn),
    Redirect = {"Location", Return},
    E#env{headers = [Cookie, Redirect | E#env.headers]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Output Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'401'(#refX{site = Site}, Env) ->
    serve_html(401, Env, [hn_util:viewroot(Site), "/401.html"]).

'404'(#refX{site = Site}, Env) ->
    serve_html(404, Env, [hn_util:viewroot(Site), "/404.html"]).

'500'(Env) ->
    respond(500, Env).

respond(Code, #env{mochi = Mochi, headers = Headers}) ->
    Mochi:respond({Code, Headers, []}),
    ok.

javascript_nocache(#env{mochi = Mochi, headers = Headers}, Text) ->
    Mochi:ok({"text/html", Headers ++ nocache(), Text}),
    ok.

text_html_nocache(#env{mochi = Mochi, headers = Headers}, Text) ->
     Mochi:ok({"text/html", Headers ++ nocache(), Text}),
     ok.

text_html(#env{mochi = Mochi, headers = Headers}, Text) ->
    Mochi:ok({"text/html", Headers, Text}),
    ok.

-spec xml(#env{}, any()) -> any().
xml(#env{mochi = Mochi, headers = Headers}, Data) ->
    Mochi:ok({"text/xml",
              Headers ++ nocache(),
              Data}),
    ok.

%% -spec jsonp(#env{}, any(), any()) -> any().
%% jsonp(#env{mochi = Mochi, headers = Headers}, Data, CallbackFn) ->
%%     JsonP = CallbackFn ++ "(" ++
%%         (mochijson:encoder([{input_encoding, utf8}]))(Data) ++ ");",
%%     Mochi:ok({"application/json", Headers ++ nocache(), JsonP}).

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
serve_html(Status, Env = #env{uid = Uid}, File) ->
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

load_file(Ref, Data, File, Name, UserName, Uid) ->
    Type = get_type(Data),
    Ext = filename:extension(Name),
    % need to reauthorize
    case hn_authorize:authorize_upload_again(Ref, Type, Uid) of
        true  -> load_file2(Ref, File, Name, UserName, Uid, Type, Ext);
        false -> Msg = "Permission denied: 401",
                 {rejected, {{struct, [{error, Msg}]}, nothing}}
    end.

load_file2(Ref, File, Name, UserName, Uid, Type, Ext) ->
    #refX{site = S, path = P} = Ref,
    NRef = Ref#refX{path = P ++ [make_name(Name, Ext)], obj = {page, "/"}},
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

make_name(Name, Ext) ->
    Basename = string:to_lower(filename:basename(Name, Ext)),
    re:replace(Basename,"\s","_",[{return,list}, global]).

provision_site(#refX{site = RootSite} = RefX, PrevUID, SiteType,
               Email, Data, Env) ->
    Zone = case application:get_env(hypernumbers, environment) of
               {ok, development} -> "hypernumbers.dev";
               {ok, server_dev}  -> "dev.hypernumbers.com";
               {ok, production}  -> "tryvixo.com"
           end,
    {From, Sig} = emailer:get_details(RootSite),
    case factory:provision_site(Zone, Email, From, Sig, SiteType,
                                PrevUID, Data) of
        {ok, new, Site, Node, Uid, Name, InitialView} ->
            Opaque = [{param, InitialView}],
            Expiry = "never",
            LoginUrl = passport:create_hypertag_url(Site, ["_mynewsite", Name],
                                                    Uid, Email, Opaque, Expiry),
            log_signup(RefX, Site, SiteType, Node, Uid, Email, Zone, LoginUrl),
            json(Env, {struct, [{"result", "success"}, {"url", LoginUrl}]});
        {ok, existing, Site, Node, Uid, Name, InitialView} ->
            Opaque = [{param, InitialView}],
            Expiry = "never",
            LoginUrl = passport:create_hypertag_url(Site, ["_mynewsite", Name],
                                                    Uid, Email, Opaque, Expiry),
            log_signup(RefX, Site, SiteType, Node, Uid, Email, Zone, LoginUrl),
            json(Env, {struct, [{"result", "success"},
                                {"url", Site ++ InitialView}]});
        {error, invalid_email} ->
            Str = "Sorry, the email provided was invalid, please try again.",
            json(Env, {struct, [{"result", "error"}, {"reason", Str}]})
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

is_wordpress(Site) ->
    case new_db_api:read_kv(Site, ?wordpress) of
        []                          -> false;
        [{kvstore, wordpress, List}] -> {true, List}
    end.

%% pretty_print(#env{} = E) ->
%%     PP = io_lib:format("Env is:\~n"
%%                        ++ "-------\~n"
%%                        ++ "   Accept:   ~p\~n"
%%                        ++ "   Body:     ~p\~n"
%%                        ++ "   Raw Body: ~p\~n"
%%                        ++ "   Headers:  ~p\~n"
%%                        ++ "   Method:   ~p\~n"
%%                        ++ "   Mochi:    ~p\~n"
%%                        ++ "   UID:      ~p\~n"
%%                        ++ "   Email:    ~p\~n"
%%                        ++ "   Auth:     ~p\~n",
%%                        [E#env.accept,  E#env.body,   E#env.raw_body,
%%                         E#env.headers, E#env.method, E#env.mochi,
%%                         E#env.uid,     E#env.email,  E#env.auth]),
%%     lists:flatten(PP).

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
