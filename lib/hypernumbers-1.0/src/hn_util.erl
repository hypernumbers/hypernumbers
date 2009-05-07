%-----------------------------------------------------------------------------
%%% File        hn_util.erl
%%% @author     Dale Harvey
%%% @doc        Utilities for hypernumbers application
%%% @private
%%% @copyright Hypernumbers Ltd
%%%-----------------------------------------------------------------------------
-module(hn_util).

-include("spriki.hrl").
-include("regexp.hrl").
-include("hypernumbers.hrl").
-include("yaws.hrl").
-include("yaws_api.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").

-define(rfc1123, muin_date:to_rfc1123_string).

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([
         email/1,
         email_test_results/1,
         email_build_fail/1,
         % HyperNumbers Utils
         compile_html/2,
         delete_gen_html/0,
         generate_po/1,
         jsonify_val/1,

         refX_to_url/1,
         index_to_url/1,
         obj_to_str/1,
         xml_to_val/1,
         % item_to_xml/1,
         in_range/2,
         to_xml/1,
         % ref_to_index/1,
         refX_to_index/1,
         range_to_list/1,
         rectify_range/4,
         rectify_row_or_col/2,

         % HTTP Utils
         req/1,
         post/2,
         post/3,
         parse_url/1,
         parse_vars/1,
         parse_ref/1,
         
         % List Utils
         add_uniq/2,
         is_alpha/1,
         is_numeric/1,
         text/1,
         trim/1,
         random_string/1,
         intersection/2,
         bin_to_hexstr/1,
         hexstr_to_bin/1,
         get_req_type/1,
         list_to_path/1,

         % Just some record conversion utilities
         % refX_to_ref/2,
         % ref_to_refX/2,
         % from_hn_item/1,
         refX_from_index/1,
         index_from_refX/1,
         % index_from_ref/1,
         url_to_refX/1,

         % general utilities
         get_hosts/1,
         get_offset/3,
         js_to_utf8/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% API functions                                                            %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_offset(insert, D, {cell,     _})              -> g_o1(D, 1, 1);
get_offset(insert, D, {row,    {Y1, Y2}})         -> g_o1(D, 0, Y2 - Y1 + 1); 
get_offset(insert, D, {column, {X1, X2}})         -> g_o1(D, X2 - X1 + 1, 0); 
get_offset(insert, D, {range,  {X1, Y1, X2, Y2}}) -> g_o1(D, X2 - X1 + 1,
                                                          Y2 - Y1 + 1);
get_offset(delete, D, {cell,    _})               -> g_o1(D, -1, -1);
get_offset(delete, D, {row,    {Y1, Y2}})         -> g_o1(D, 0, -(Y2 - Y1 + 1)); 
get_offset(delete, D, {column, {X1, X2}})         -> g_o1(D, -(X2 - X1 + 1), 0); 
get_offset(delete, D, {range,  {X1, Y1, X2, Y2}}) -> g_o1(D, -(X2 - X1 + 1),
                                                          -(Y2 - Y1 + 1)). 
g_o1(vertical, _X, Y)   -> {0, Y};
g_o1(horizontal, X, _Y) -> {X, 0}.


get_hosts(List) when is_list(List) -> get_hosts1(List, []).

get_hosts1([], Acc) -> Acc;
get_hosts1([{_IP, Port, [Host]} | T], Acc) ->
    NewAcc = "http://" ++ Host ++ ":" ++ integer_to_list(Port),
    get_hosts1(T, [NewAcc | Acc]).

compile_html(Html, Lang) ->
    {ok, Bin} = file:read_file(code:lib_dir(hypernumbers)++"/po/"++Lang++".po"),
    gettext:store_pofile(Lang, Bin),
    {ok, C} = sgte:compile_file(Html),
    NHtml = sgte:render(C, [{options, [{gettext_lc, Lang}]}]),
    file:write_file(Html++"."++Lang, NHtml),   
    ok.

generate_po(Url) ->
    delete_gen_html(),
    {ok,{{_V,_Status,_R},_H,Body}} = http:request(get,{Url++"?attr",[]},[],[]),
    {struct, Json} = mochijson2:decode(Body),
    {struct, Cells} = ?pget(<<"cell">>, Json),
    {struct, Pos} = ?pget(<<"2">>, Cells),
    Files = lists:map(fun po_files/1, Pos),
    lists:map(fun(X) -> po_row(Files, X) end, Cells),
    lists:map(fun({_I, F}) -> file:close(F) end, Files),
    ok.

po_files({Index, {struct, List}}) ->
    Lang = binary_to_list(?pget(<<"value">>, List)),
    Path = code:lib_dir(hypernumbers)++"/po/"++Lang++".po",
    {ok, File} = file:open(Path, [write]),
    {Index, File}.

po_row(_File, {<<"1">>, _Children}) -> ok;
po_row(_File, {<<"2">>, _Children}) -> ok;
po_row(File, {_Row, {struct, Children}}) ->
    {struct, Attr} = ?pget(<<"1">>, Children),
    Id = ?pget(<<"value">>, Attr),
    lists:map(fun(X) -> po_val(File, Id, X) end, Children),
    ok.
   
po_val(Files, Id, {Col, {struct, Cell}}) ->
    Str = "msgid \"~s\"\nmsgstr \"~s\"\n\n",
    io:format(?pget(Col, Files), Str, [Id, ?pget(<<"value">>, Cell)]).

delete_gen_html() ->
    Dir = code:lib_dir(hypernumbers)++"/priv/docroot/hypernumbers/",
    [file:delete(X) || X <- filelib:wildcard(Dir++"*.html.*")].

jsonify_val({"__permissions", _})           -> {"__permissions", "bleh"};
jsonify_val({"__groups", _})                -> {"__groups", "bleh"};
jsonify_val({"__dependency-tree", _})       -> {"__dependency-tree", "bleh"};
jsonify_val({"parents", _})                 -> {"parents", "bleh"};    
jsonify_val({Name, {errval, Error}})        -> {Name, atom_to_list(Error)};
jsonify_val({Name, {datetime, Date, Time}}) -> {Name, ?rfc1123({datetime, Date, Time})};
jsonify_val({"value", true})                -> {"value", "true"};
jsonify_val({"value", false})               -> {"value", "false"};
%% TODO: fix names
jsonify_val({Name, {namedexpr, _Path, Nm}}) -> {Name, Nm};
jsonify_val(Else)                           -> Else.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions convert to and from #refX, #ref, #hn_item                %%%
%%% and #index records                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
url_to_refX(Url) ->
    {ok, RefX} = parse_url(Url),
    RefX.

%refX_to_ref(RefX, Name) ->
%    #refX{site = S, path = P, obj = R, auth = A} = RefX,
%    #ref{site = S, path = P, ref = R, name = Name, auth = A}.

%ref_to_refX(Ref, Val) ->
%    #ref{site = S, path = P, ref = R, name = Key, auth = A} = Ref,
%    RefX = #refX{site = S, path = P, obj = R, auth = A},
%    {RefX, {Key, Val}}.

%from_hn_item(List) -> from_hn_item(List, []).

%from_hn_item([], Acc)      -> Acc;
%from_hn_item([H | T], Acc) -> #hn_item{addr = Ref, val = V} = H, 
%                              NewAcc = ref_to_refX(Ref, V),
%                              from_hn_item(T, [NewAcc | Acc]).

refX_from_index(#index{site = S, path = P, column = X, row = Y}) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}}.

index_from_refX(#refX{site = S, path = P, obj = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

%index_from_ref(#ref{site = S, path = P, ref = {cell, {X, Y}}}) ->
%    #index{site = S, path = P, column = X, row = Y}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% HyperNumbers Utils                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rectify_range(X1, Y1, X2, Y2) ->
    % in case the range is passed in arsey-backwards
    {X1a, X2a} = rectify_row_or_col(X1, X2),
    {Y1a, Y2a} = rectify_row_or_col(Y1, Y2),
    {X1a, Y1a, X2a, Y2a}.

rectify_row_or_col(Z1, Z2) ->
    if
        (Z1 >  Z2) -> {Z2, Z1};
        (Z1 =< Z2) -> {Z1, Z2}
    end.    

range_to_list(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    {X1a, Y1a, X2a, Y2a} = rectify_range(X1, Y1, X2, Y2),
    range_to_list1(RefX, X1a, X1a, Y1a, X2a, Y2a, []).

range_to_list1(RefX, _Reset, X, Y, X, Y, Acc) ->
    [RefX#refX{obj = {cell, {X, Y}}} | Acc];
range_to_list1(RefX, Reset, X2, Y1, X2, Y2, Acc) ->
    range_to_list1(RefX, Reset, Reset, Y1 + 1, X2, Y2,
                   [RefX#refX{obj = {cell, {X2, Y1}}} | Acc]);
range_to_list1(RefX, Reset, X1, Y1, X2, Y2, Acc) ->
    range_to_list1(RefX, Reset, X1 + 1, Y1, X2, Y2,
                   [RefX#refX{obj = {cell, {X1, Y1}}} | Acc]).

refX_to_url(#refX{site = Site, path = Path, obj = {cell, {X, Y}}}) ->
    lists:append([Site, list_to_path(Path), tconv:to_b26(X), text(Y)]);
refX_to_url(#refX{site = Site, path = Path, obj = {column, {X1, X2}}}) ->
    lists:append([Site, list_to_path(Path), tconv:to_b26(X1), ":",
                  tconv:to_b26(X2)]);
refX_to_url(#refX{site = Site, path = Path, obj = {row, {Y1, Y2}}}) ->
    lists:append([Site, list_to_path(Path), text(Y1), text(Y2)]);
refX_to_url(#refX{site = Site, path = Path, obj ={range, {X1, Y1, X2, Y2}}}) ->
    lists:append([Site, list_to_path(Path), tconv:to_b26(X1), text(Y1), ":",
                  tconv:to_b26(X2), text(Y2)]);
refX_to_url(#refX{site = Site, path = Path, obj = {page, "/"}}) ->
    lists:append([Site, list_to_path(Path)]).
            
index_to_url(#index{site=Site,path=Path,column=X,row=Y}) ->
    lists:append([Site, list_to_path(Path),tconv:to_b26(X), text(Y)]).

list_to_path([])   -> "/";
list_to_path(Path) -> "/" ++ string:join(Path, "/") ++ "/".

%ref_to_index(#ref{site = S, path = P, ref= {cell, {X, Y}}}) ->
%    #index{site = S, path = P, column = X, row = Y}.

refX_to_index(#refX{site = S, path = P, obj = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

obj_to_str({page,Path})           -> Path;   
obj_to_str({cell,{X,Y}})          -> tconv:to_b26(X)++text(Y);
obj_to_str({row,{Y,Y}})           -> text(Y);
% obj_to_str({row,{Y}})             -> text(Y); % old compatibility - delete!
obj_to_str({row,{Y1,Y2}})         -> text(Y1)++":"++text(Y2);
% obj_to_str({column,X})            -> tconv:to_b26(X); % old compatibility - delete!
obj_to_str({column,{X,X}})        -> tconv:to_b26(X);
obj_to_str({column,{X1,X2}})      -> tconv:to_b26(X1)++":"++tconv:to_b26(X2);
obj_to_str({range,{X1,Y1,X2,Y2}}) -> tconv:to_b26(X1)++text(Y1)++":"++
                                         tconv:to_b26(X2)++text(Y2).

xml_to_val({bool,[],[true]})      -> true;
xml_to_val({bool,[],[false]})     -> false;
xml_to_val({errval,[],[Ref]})     -> Ref;
xml_to_val({float,[],[Ref]})      -> list_to_float(Ref);
xml_to_val({int,[],[Ref]})        -> list_to_integer(Ref);
xml_to_val({string,[],[Ref]})     -> Ref;
xml_to_val({datetime, [], [Ref]}) -> Ref;
xml_to_val(Else)                  -> Else.

%%% Turn a hn_item record into its xml <ref> display
%item_to_xml(#hn_item{addr = A, val = V}) ->

%    Type = atom_to_list(element(1, A#ref.ref)),
%    Str  = hn_util:obj_to_str(A#ref.ref),

%    Value = case A#ref.name of
%                value    -> to_xml(V);
%                rawvalue -> to_xml(V);
%                style    -> to_xml(V);
%                _Else    -> to_val(V)
%            end,    
%    {ref, [{type, Type}, {ref, Str}], [{A#ref.name, [], Value}]}.

in_range({range,{X1,Y1,X2,Y2}}, {cell,{X,Y}}) ->
    Y >= Y1 andalso Y =< Y2 andalso X >= X1 andalso X =< X2.

%to_val({xml,Xml}) -> Xml;
%to_val(Else) ->
%    case io_lib:char_list(Else) of
%        true  -> [Else];
%        false -> throw({unmatched_type,Else})
%    end.

to_xml(true)                     -> [{bool,[],  ["true"]}];
to_xml(false)                    -> [{bool,[],  ["false"]}];    
to_xml(Val) when is_integer(Val) -> [{int,[],   [integer_to_list(Val)]}];
to_xml(Val) when is_float(Val)   -> [{float,[], [float_to_list(Val)]}];
to_xml({errval, Errval})         -> [{error,[], [atom_to_list(Errval)]}];
to_xml({error, Errval})          -> [{error,[], [atom_to_list(Errval)]}];
to_xml({blank, [], []})          -> [{blank,[], []}];
to_xml(Dt) when is_record(Dt, datetime) ->
    [{string, [], [muin_date:to_rfc1123_string(Dt)]}];
to_xml({X,Values}) when X == range; X == array ->
    ?INFO("range ~p",[Values]),
    F = fun(Z) when is_list(Z) -> {row,[],lists:map(fun to_xml/1,Z)};
           (_)                 -> throw({not_range,{X,Values}})
        end,
    [{X,[],lists:map(F,Values)}];
to_xml(Else) ->
    case io_lib:char_list(Else) of
        true  -> [{string,[],[Else]}];
        false -> throw({unmatched_type,Else})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Http Utils                                                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
req(Url) ->
    {ok,{{_V,_Status,_R},_H,Body}} = http:request(get,{Url,[]},[],[]),
    Body.

post(Url,Data) ->
    {ok, {{_V,_Status,_R},_H,Body}} = 
        http:request(post,{Url,[],"text/plain",Data},[],[]),
    Body.

post(Url,Data,Format) ->
    {ok, {{_V, _Status,_R},_H,Body}} =
        http:request(post,{Url,[],Format,Data},[],[]),
    Body.

parse_url(Url) when is_list(Url) ->
    parse_url(yaws_api:parse_url(Url));

parse_url(Url) when is_record(Url, url) ->

    Port = case {Url#url.port,Url#url.scheme} of
               {undefined,http}  -> "80";
               {undefined,https} -> "443";
               {Else,_} -> integer_to_list(Else)
           end,

    Site = lists:concat([Url#url.scheme,"://",Url#url.host,":",Port]),

    {Ref,Path} = case lists:last(Url#url.path) of
                     $/ -> 
                         {"/",string:tokens(Url#url.path,"/")};
                     _  ->
                         TP = lists:flatten(Url#url.path),
                         Tokens = string:tokens(TP,"/"),
                         [TmpRef|T] = lists:reverse(Tokens),
                         {TmpRef,lists:reverse(T)}
                 end,

    Obj = parse_ref(Ref),
    {ok,#refX{site = Site, path = Path, obj = Obj}}.

parse_ref(Ref) ->
    RefType = type_reference(Ref),    
    RefVal  = case RefType of
                  page ->   "/";
                  cell ->   util2:strip_ref(undollar(Ref));
                  range ->  util2:parse_range(undollar(Ref));
                  column -> element(1,util2:strip_ref(undollar(Ref)++"1"));
                  row ->    element(2,util2:strip_ref("a"++undollar(Ref)))
              end,
    {RefType, RefVal}.

undollar(A) -> {ok, NewA, _} = regexp:gsub(A, "\\$", ""), %")
                   NewA.

get_req_type([{"format","json"}|_]) -> {ok,json};
get_req_type([{"format","xml"}|_])  -> {ok,xml};
get_req_type([])                    -> {ok,xml};
get_req_type([_N|Tail])             -> get_req_type(Tail).

parse_vars([]) -> {ok,[]};
parse_vars(Query) ->

    Split = fun(X) -> 
                    case string:chr(X,$=) of
                        0 -> 
                            X;
                        _ -> 
                            [H,T] = string:tokens(X,"="),
                            {H,T}
                    end
            end,

    Pairs = lists:map(Split,string:tokens(Query,"&")),

    {ok,Pairs}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% List Utils                                                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
intersection(ListA,ListB) ->
    intersection(ListA,ListB,[]).

intersection([],_List,Acc) ->
    Acc;
intersection([H|T],List,Acc) ->
    NAcc = case lists:member(H,List) of
               true  -> [H|Acc];
               false -> Acc
           end,
    intersection(T,List,NAcc).

%%--------------------------------------------------------------------
%% Function:    add_uniq/2
%%
%% Description: 
%%--------------------------------------------------------------------
add_uniq(List,Item) -> 

    [Item] ++ lists:filter(
                fun(X) -> ?COND(X == Item,false,true) end,
                List).

%%--------------------------------------------------------------------
%% Function:    trim/2
%%--------------------------------------------------------------------
trim(String) -> 
    strip_ws(lists:reverse(strip_ws(lists:reverse(String)))).

strip_ws([H|Rest]) when H == 10; H == 13; H == 32 -> 
    strip_ws(Rest);
strip_ws(String) -> 
    String.

%%--------------------------------------------------------------------
%% Function:    is_alpha/1
%% Description: Returns true if a string is a list of a-z
%%--------------------------------------------------------------------
is_alpha(Str) ->
    Fun = fun(XX) ->         
                  if XX < 97  -> false;  
                     XX > 122 -> false;
                     true     -> true      
                  end                  
          end,
    case is_list(Str) of
        false -> false;
        true  -> lists:all(Fun, Str)
    end.

%%--------------------------------------------------------------------
%% Function:    is_numeric/1
%% Description: Returns true if a string is a list of digits
%%--------------------------------------------------------------------
is_numeric([]) -> false;
is_numeric(Str) ->
    Fun = fun(XX) ->         
                  if XX < 48 -> false;  
                     XX > 57 -> false;
                     true    -> true      
                  end                  
          end,
    case is_list(Str) of
        false -> false;
        true -> lists:all(Fun, Str)
    end.

random_string(Len) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random_string("",Len).

random_string(Str,0) ->
    Str;

random_string(Str,Len) ->
    case random:uniform(3) of
        1 -> Asc = 96 + random:uniform(26);
        2 -> Asc = 47 + random:uniform(9);
        3 -> Asc = 64 + random:uniform(26)
    end,
    random_string([Asc|Str],Len-1).

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

list_to_hexstr([]) -> 
    [];
list_to_hexstr([H|T]) ->
    to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

%%--------------------------------------------------------------------
%% Function:    text/1
%% Description: Returns a string representation of the parameter
%%--------------------------------------------------------------------
text(X) when is_integer(X) -> integer_to_list(X);
text(X) when is_float(X)   -> float_to_list(X);
text(X) when is_list(X)    -> lists:flatten(X);
text({errval, Errval})     -> atom_to_list(Errval);
text(X) when is_boolean(X) -> atom_to_list(X);
text(Dt) when is_record(Dt, datetime) -> muin_date:to_rfc1123_string(Dt);
text(_X) -> "". %% quick fix for the "plain" api

js_to_utf8({struct, Val}) -> {struct, lists:map(fun js_to_utf8/1, Val)};
js_to_utf8({array, Val})  -> {array,  lists:map(fun js_to_utf8/1, Val)};
js_to_utf8({Key, Val})    -> {xmerl_ucs:to_utf8(Key), js_to_utf8(Val)};
js_to_utf8(X) when is_integer(X); is_float(X); is_atom(X) -> X;
js_to_utf8(X)             -> xmerl_ucs:to_utf8(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal Functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_reference("/") -> page;
type_reference(Cell) ->
    case string:chr(Cell, $:) of
        0 ->
            case hn_util:is_numeric(Cell) of
                true  -> row;
                false ->
                    case hn_util:is_alpha(Cell) of
                        true  -> column;
                        false -> 
                            case regexp:match(Cell, ?RG_cell) of
                                {match, _, _} -> cell;
                                _  ->
                                    throw({invalid_reference, Cell})
                            end
                    end
            end;
        _ ->
            range
    end.



email_build_fail(Rev) ->
    email(?FORMAT(build_failed_tpl(), [Rev])).

email_test_results(Rev) ->
    TestRoot = code:lib_dir(hypernumbers)++"/../../logs/",
    Runs     = filelib:wildcard(TestRoot++"ct_run.*"),
    LastRun  = lists:last(lists:sort(Runs)),
    
    {ok, Bin} = file:read_file(LastRun++"/last_name"),
    Summary = string:strip(binary_to_list(Bin), right, $\n)++"/suite.summary",
    
    {ok, [{summary, {Pass, Fail, Skip}}]} = file:consult(Summary),

    case {Fail > 0, Skip > 0} of
        {false, false} -> ok;
        _Else ->
            email(?FORMAT(tests_failed_tpl(), [Rev, Pass, Fail, Skip]))
    end.

email(Msg) ->    
    Conf = hn_config:get(mail),
    User = ?pget(user, Conf),
    
    {ok, IP}  = inet:getaddr(?pget(server, Conf), inet),
    {ok, Pid} = smtp_fsm:start(inet_parse:ntoa(IP)),
    
    {ok, _}   = smtp_fsm:ehlo(Pid),
    {ok, _}   = smtp_fsm:plain_login(Pid, User, ?pget(pass, Conf)),

    ok = smtp_fsm:sendemail(Pid, User, ?pget(address, Conf), Msg),
    smtp_fsm:close(Pid).
    
tests_failed_tpl() ->
    "Systems tests failed on Revision ~p\nPassed :\t~p\n"
        "Failed :  \t~p\nSkipped :\t~p\n".

build_failed_tpl() ->
    "Hypernumbers failed to build after Revision ~p".
