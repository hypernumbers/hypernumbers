%%% @author Dale Harvey
%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests

-module(hn_mochi).

-include("regexp.hrl").
-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-export([ req/1, style_to_css/2 ]).

-define(XHR_TIMEOUT, 10000).

req(Req) ->

    {ok, Port} = inet:port(Req:get(socket)),
    [Domain | _] = string:tokens(Req:get_header_value("host"), ":"), 
    Url = lists:concat(["http://",Domain,":",itol(Port),Req:get(path)]),

    case filename:extension(Req:get(path)) of 
        
        %% Serve Static Files
        X when X == ".png"; X == ".css"; X == ".js"; X == ".ico" ->
            "/"++RelPath = Req:get(path),
            Req:serve_file(RelPath, docroot());
        
        [] ->
            case catch stuff(Url, Req) of 
                ok   -> ok;
                Else ->                                
                    ?ERROR("~p~n~p",[Else, erlang:get_stacktrace()]),
                    Req:ok({"text/html",<<"screwed">>})
            end
    end.

stuff(Url, Req) ->
    Ref = parse_ref(Url),

    {Type, _Val} = Ref#refX.obj,

    Post = case Req:recv_body() of
               undefined -> undefined;
               Json -> 
                   {struct, Attr} = mochijson:decode(Json),
                   Attr
           end,

    handle_req(Req:get(method), Req, Ref, Type, Req:parse_qs(), Post), 
    ok.

handle_req('GET', Req, Ref, page, [{"updates", Time}], _Post) ->
    Socket = Req:get(socket),
    inet:setopts(Socket, [{active, once}]),
    Msg = {fetch, Ref#refX.site, Ref#refX.path, list_to_integer(Time), self()},
    gen_server:cast(remoting_reg, Msg),
    receive 
        {tcp_closed, Socket} -> ok;
        {error, timeout}     -> Req:ok({"text/html",<<"timeout">>});
        {msg, Data}          -> 
            Req:ok({"application/json", mochijson:encode(Data)})
    end;

handle_req('GET', Req, Ref, page, [{"attr", []}], _Post) -> 
    Init  = [["cells"], ["cols"], ["rows"], ["page"], ["styles"]],
    Tree  = dh_tree:create(Init),
    Styles = styles_to_css(hn_db_api:read_styles(Ref), []),
    NTree = add_styles(Styles, Tree),
    Dict  = to_dict(hn_db_api:read(Ref), NTree),
    Time  = {"time", remoting_reg:timestamp()},
    JSON  = {struct, [Time | dict_to_struct(Dict)]},
    Req:ok({"application/json", mochijson:encode(JSON)});

handle_req('GET', Req, _Ref, page, _Attr, _Post) ->
    Req:serve_file("hypernumbers/index.html", docroot());

handle_req('GET', Req, Ref, cell, _Attr, _Post) ->
    Dict = to_dict(hn_db_api:read(Ref), dh_tree:new()),
    JS = case dict_to_struct(Dict) of
             [] -> {struct, []};
             [{_Cells, {struct, [{_Y, {struct, [{_X, JSON}]}}]}}] ->
                 JSON
         end, 
    Req:ok({"application/json", mochijson:encode(JS)});

handle_req('POST', Req, Ref, _Type, _Attr, [{"set", {struct, Attr}}]) ->
    hn_db_api:write_attributes(Ref, Attr),
    Req:ok({"application/json", "success"});

handle_req('POST', Req, Ref, _Type, _Attr, [{"clear", "all"}]) ->
    hn_db_api:clear(Ref, all),
    Req:ok({"application/json", "success"});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handlers                                     %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_req('POST', Req, Ref, _Type, _Attr, [{"action", "notify_back_create"}|T]) ->
    Biccie   = from("biccie",    T),
    Proxy    = from("proxy",     T),
    ChildUrl = from("child_url", T),
    {ok, ChildRef} = hn_util:parse_url(ChildUrl),
    {ChildX, _} = hn_util:ref_to_refX(ChildRef, "dont care"),
    ParentX = Ref,
    Return = hn_db_api:register_hypernumber(ParentX, ChildX, Proxy, Biccie),
    Req:ok({"application/json", mochijson:encode({struct, Return})});

handle_req('POST', Req, _Ref, _Type, _Attr, [{"action", "notify_back"}|T] = _Json) ->
    Biccie    = from("biccie",     T),
    ChildUrl  = from("child_url",  T),
    ParentUrl = from("parent_url", T),
    Type      = from("type",       T),
    {ok, ChildRef} = hn_util:parse_url(ChildUrl),
    {ChildX, _} = hn_util:ref_to_refX(ChildRef, "dont care"),
    {ok, ParentRef} = hn_util:parse_url(ParentUrl),
    {ParentX, _} = hn_util:ref_to_refX(ParentRef, "dont care"),
    {ok, ok}  = hn_db_api:handle_notify_back(ParentX, ChildX, Biccie, Type),
    Req:ok({"application/json", "success"});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify handlers                                          %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_req('POST', Req, Ref, _Type, _Attr, [{"action", "notify"}|T] = _Json) ->
    Biccie     = from("biccie",          T),
    ParentUrl  = from("parent_url",      T),
    Type       = from("type",            T),
    Value      = from("value",           T),
    DepTree    = from("dependency-tree", T),
    Version    = from("version",         T),
    {ok, ParentRef} = hn_util:parse_url(ParentUrl),
    {ParentX, _} = hn_util:ref_to_refX(ParentRef, "dont care"),
    {array, DepTree2} = DepTree,
    Return = hn_db_api:handle_notify(ParentX, Ref, Type, Value,
                                     DepTree2, Biccie, Version),
    Req:ok({"application/json", "success"});


handle_req(_Method, Req, _Ref, _Type,  _Attr, _Post) ->
    ?INFO("404~n-~p~n-~p~n-~p",[_Ref, _Attr, _Post]),
    Req:ok({"text/html",<<"bleh">>}).

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
add_ref(#refX{ obj = {cell, {X,Y}}}, {Name, Val}, JSON) ->
    dh_tree:set(["cells", itol(Y), itol(X), Name], Val, JSON);
add_ref(#refX{ obj = {column, X}}, {Name, Val}, JSON) ->
    dh_tree:set(["cols", itol(X), Name], Val, JSON);
add_ref(#refX{ obj = {row, Y}}, {Name, Val}, JSON) ->
    dh_tree:set(["rows", itol(Y), Name], Val, JSON).

docroot() ->
    code:priv_dir(hypernumbers) ++ "/docroot".

itol(X) ->
    integer_to_list(X).

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
    [Cell1, Cell2] = string:tokens(Addr, ":"),
    {X1, Y1} = util2:strip_ref(Cell1),
    {X2, Y2} = util2:strip_ref(Cell2),
    {range, {X1, Y1, X2, Y2}}.

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
    
from(Key, List) -> {value, {Key, Value}} = lists:keysearch(Key, 1, List),
                   Value.

