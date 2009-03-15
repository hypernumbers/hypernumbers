%%% @author Dale Harvey
%%% @copyright 2008 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests

-module(hn_mochi).

-include("regexp.hrl").
-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-export([ req/1 ]).

-define(XHR_TIMEOUT, 10000).

req(Req) ->

    {ok, Port} = inet:port(Req:get(socket)),
    [Domain | Rest] = string:tokens(Req:get_header_value("host"), ":"), 
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
    handle_req(Req:get(method), Req, Ref, Type, Req:parse_qs()), 
    ok.

handle_req('GET', Req, Ref, page, [{"updates", Time}]) ->
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

handle_req('GET', Req, Ref, page, [{"attr", []}]) ->
    %?INFO("~p",[hn_db_api:read_styles(Ref)]),
    Tree = dh_tree:create([["cells"], ["cols"], ["rows"], ["page"]]),
    Dict = to_dict(hn_db_api:read(Ref), Tree),
    Time = {"time", remoting_reg:timestamp()},
    JSON = {struct, [Time | dict_to_struct(Dict)]},
    Req:ok({"application/json", mochijson:encode(JSON)});

handle_req('GET', Req, _Ref, page, _Attr) ->
    Req:serve_file("hypernumbers/index.html", docroot());

handle_req('GET', Req, Ref, cell, _Attr) ->
    Dict = to_dict(hn_db_api:read(Ref), dh_tree:new()),
    JS = case dict_to_struct(Dict) of
             [] -> {struct, []};
             [{_Cells, {struct, [{Y, {struct, [{X, JSON}]}}]}}] ->
                 JSON
         end, 
    Req:ok({"application/json", mochijson:encode(JS)});

handle_req('POST', Req, Ref, cell, _Attr) ->
    {struct, POST} = mochijson:decode(Req:recv_body()),
    case POST of
        [{"set", {struct, Attr}}] ->
            hn_db_api:write_attributes(Ref, Attr);
        [{"clear", "all"}] ->
            hn_db_api:clear(Ref, all)
    end,
    Req:ok({"application/json", "success"});

handle_req('POST', Req, Ref, range, _Attr) ->
    {struct, POST} = mochijson:decode(Req:recv_body()),
    case POST of
        [{"clear", "all"}] ->
            hn_db_api:clear(Ref, all)
    end,
    Req:ok({"application/json", "success"});


handle_req(_Method, Req, _Ref, _Type,  _Attr) ->
    ?INFO("404 ~p ~p",[_Ref, _Attr]),
    Req:ok({"text/html",<<"bleh">>}).

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
