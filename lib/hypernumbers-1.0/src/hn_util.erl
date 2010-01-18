%%%-----------------------------------------------------------------------------
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
-include("handy_macros.hrl").
-include("muin_records.hrl").

-include_lib("kernel/include/file.hrl").

-define(rfc1123, muin_date:to_rfc1123_string).
-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([
         email/4, email/5,

         % HyperNumbers Utils
         delete_directory/1,
         compile_html/2,
         delete_gen_html/0,
         generate_po/1,
         generate_po_CHEATING/1,
         jsonify_val/1,
         is_older/2,

         refX_to_url/1,
         index_to_url/1,
         obj_to_str/1,
         xml_to_val/1,
         in_range/2,
         to_xml/1,
         refX_to_index/1,
         range_to_list/1,
         rectify_range/4,
         rectify_row_or_col/2,
         path_to_json_path/1,

         % HTTP Utils
         req/1,
         post/2,
         post/3,
         url_encode/1,
         parse_url/1,
         parse_ref/1,
         parse_attr/1,
         parse_attr/2,
         parse_vars/1,
         parse_site/1,
         
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
         refX_from_index/1,
         index_from_refX/1,
         url_to_refX/1,

         % general utilities
         get_offset/3,
         js_to_utf8/1,
         diff/2,
         lists_diff/2,
         esc_regex/1,
         
         % file copy utils
         recursive_copy/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% API functions                                                            %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Escape the replacement part of the regular expression
%% so no spurious replacements
-spec esc_regex(list() | binary()) -> list().
esc_regex(List) when is_binary(List) ->
    esc_regex(binary_to_list(List));
esc_regex(List) ->
    esc_regex(List, []).

esc_regex([], Acc) ->
    lists:flatten(lists:reverse(Acc));

esc_regex([$&   | Rest], Acc) -> esc_regex(Rest, ["\\&" | Acc]);
%esc_regex([$\   | Rest], Acc) -> esc_regex(Rest, ["\\" | Acc]);
esc_regex([Else | Rest], Acc) -> esc_regex(Rest, [Else | Acc]).

recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = rec_copy1(From, To, X) || X <- Files],
    ok.


% ignore hidden
rec_copy1(_From, _To, [$. | _T]) ->
    ok; 
rec_copy1(From, To, File) ->

    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),

    case filelib:is_dir(NewFrom) of

        true  ->
            ok = filelib:ensure_dir(NewTo),
            recursive_copy(NewFrom, NewTo);
        
        false ->
            case filelib:is_file(NewFrom) of                
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {ok, _} = file:copy(NewFrom, NewTo),
                    ok;
                false ->
                    ok            
            end
    end.


%% Delete a directory (and all its children)
-spec delete_directory(string()) -> ok.
delete_directory(From) ->
    {ok, Files} = file:list_dir(From),
    [ok = delete_dir(filename:join(From, File)) || File <- Files],
    ok = file:del_dir(From).

delete_dir(File) ->
    case filelib:is_dir(File) of
        true  -> delete_directory(File);
        false -> file:delete(File)
    end.

diff(Time2, Time1) ->
    {Mega2, Sec2, Micro2} = Time2,
    {Mega1, Sec1, Micro1} = Time1,
    (1000000000000 * Mega2 + 1000000 * Sec2 + Micro2) - 
        (1000000000000 * Mega1 + 1000000 * Sec1 + Micro1).
   

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


compile_html(Html, Lang) ->
    io:format("~p~n",[Html]),
    {ok, Bin} = file:read_file(code:lib_dir(hypernumbers)++"/po/"++Lang++".po"),
    gettext:store_pofile(Lang, Bin),
    {ok, C} = sgte:compile_file(Html),
    sgte:render(C, [{options, [{gettext_lc, Lang}]}]).

is_older(File1, File2) ->
    {ok, Info1} = file:read_file_info(File1),
    {ok, Info2} = file:read_file_info(File2),
    Info2#file_info.mtime > Info1#file_info.mtime.

generate_po_CHEATING(Ref) ->
    Body = hn_mochi:page_attributes(parse_url(Ref)),
    generate_po1(Body).

generate_po(Url) ->
    delete_gen_html(),
    {ok,{{_V,_Status,_R},_H,Body}} = http:request(get,{Url++"?attr",[]},[],[]),
    generate_po1(Body).

generate_po1(Body) ->
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
jsonify_val({Name, {datetime, {1,1,1}=Date, Time}}) ->
    {Name, dh_date:format("g:i A", {Date, Time})};
jsonify_val({Name, {datetime, Date, Time}}) ->
    {Name, muin_date:to_rfc1123_string({datetime, Date, Time})};
jsonify_val({"value", true})                -> {"value", "true"};
jsonify_val({"value", false})               -> {"value", "false"};
%% TODO: fix names
jsonify_val({Name, {namedexpr, _Path, Nm}}) -> {Name, Nm};
jsonify_val(Else)                           -> Else.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions convert to and from #refX and  #index records and Urls   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
url_to_refX(Url) ->
    parse_url(Url).

refX_from_index(#index{site = S, path = P, column = X, row = Y}) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}}.

index_from_refX(#refX{site = S, path = P, obj = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

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

refX_to_index(#refX{site = S, path = P, obj = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

path_to_json_path([])                -> "path.json";
path_to_json_path(P) when is_list(P) -> "path." ++ string:join(P, ".")
                                            ++ ".json".

obj_to_str({page,Path})           -> Path;   
obj_to_str({cell,{X,Y}})          -> tconv:to_b26(X)++text(Y);
obj_to_str({row,{Y,Y}})           -> text(Y);
obj_to_str({row,{Y1,Y2}})         -> text(Y1)++":"++text(Y2);
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

in_range({range,{X1,Y1,X2,Y2}}, {cell,{X,Y}}) ->
    Y >= Y1 andalso Y =< Y2 andalso X >= X1 andalso X =< X2.

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

parse_site("http://"++Site) ->
    [case S of $: -> $&; S  -> S end 
     || S <- Site].

parse_url("http://"++Url) ->
    {Host, Path, NUrl} = prs(Url),
    case lists:last(NUrl) of
        $/ -> #refX{site="http://"++Host, path=Path, obj={page, "/"}};
        _  -> 
            [Addr | P] = lists:reverse(Path),
            Obj = parse_attr(cell, Addr),
            #refX{site="http://"++Host, path=lists:reverse(P), obj = Obj}
    end.

prs(Url) ->
    case string:tokens(Url, "/") of
        [Host]        -> {Host, [], "/"};
        [Host | Path] -> {Host, Path, Url}
    end.

parse_attr(Addr) ->
    parse_attr(cell, Addr).

parse_attr(cell, Addr) ->
    case re:run(Addr,?RG_cell) of
        {match,_} -> {cell, util2:strip_ref(Addr)};
        _         -> parse_attr(range, Addr)
    end;

parse_attr(range, Addr) ->
    case re:run(Addr,?RG_range) of
        {match,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {X1, Y1} = util2:strip_ref(Cell1),
            {X2, Y2} = util2:strip_ref(Cell2),
            {XX1, YY1, XX2, YY2} = hn_util:rectify_range(X1, Y1, X2, Y2),
            {range, {XX1, YY1, XX2, YY2}};
        _ -> 
            parse_attr(column, Addr)
    end;

parse_attr(column, Addr) ->
    case re:run(Addr,?RG_col_range) of
        {match,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {column, {tconv:b26_to_i(Cell1), tconv:b26_to_i(Cell2)}};
        _ -> 
            parse_attr(row, Addr)
    end;

parse_attr(row, Addr) ->
    case re:run(Addr,?RG_row_range) of
        {match,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {row, {ltoi(Cell1), ltoi(Cell2)}};
        _ ->
            % this is where references will be typed as names
            % (when we get them in)
            {name, Addr}
            %throw(invalid_reference)
    end.

ltoi(X) ->
    list_to_integer(X).

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

undollar(A) ->
    re:replace(A, "\\$", "", [{return, list}, global]). %"

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

url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case erlang:integer_to_list(H, 16) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

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

lists_diff(List1, List2) ->
    lists_diff(lists:sort(List1), lists:sort(List2), []).

lists_diff([], [], Acc) ->
    Acc;
lists_diff([], L2, Acc) ->
    [{extra, L2} | Acc];
lists_diff(L1, [], Acc) ->
    [{missing, L1} | Acc];
lists_diff([{K, V} | Tl1], [{K, V} | Tl2], Acc) ->
    lists_diff(Tl1, Tl2, Acc);
lists_diff([{K1, V1} | Tl1], [{K2, _V2} | _Tl2] = L2, Acc) when K1 < K2 ->
    lists_diff(Tl1, L2, [{missing, {K1, V1}} | Acc]);
lists_diff([{K1, _V1} | _Tl1] = L1, [{K2, V2} | Tl2], Acc) when K2 < K1 ->
    lists_diff(L1, Tl2, [{extra, {K2, V2}} | Acc]);
lists_diff([{K, V1} | Tl1], [{K, V2} | Tl2], Acc) ->
    lists_diff(Tl1, Tl2, [{K, V1, V2} | Acc]).

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
                            case re:run(Cell, ?RG_cell) of
                                {match, _} -> cell;
                                _  ->
                                    throw({invalid_reference, Cell})
                            end
                    end
            end;
        _ ->
            range
    end.

%% email_build_fail(Rev) ->
%%     email(?FORMAT(build_failed_tpl(), [Rev])).

%% email_test_results(Rev) ->
%%     TestRoot = code:lib_dir(hypernumbers)++"/../../logs/",
%%     Runs     = filelib:wildcard(TestRoot++"ct_run.*"),
%%     LastRun  = lists:last(lists:sort(Runs)),
    
%%     {ok, Bin} = file:read_file(LastRun++"/last_name"),
%%     Summary = string:strip(binary_to_list(Bin), right, $\n)++"/suite.summary",
    
%%     {ok, [{summary, {Pass, Fail, Skip}}]} = file:consult(Summary),

%%     case {Fail > 0, Skip > 0} of
%%         {false, false} -> ok;
%%         _Else ->
%%             email(?FORMAT(tests_failed_tpl(), [Rev, Pass, Fail, Skip]))
%%     end.    

%% tests_failed_tpl() ->
%%     "Systems tests failed on Revision ~p\nPassed :\t~p\n"
%%         "Failed :  \t~p\nSkipped :\t~p\n".

%% build_failed_tpl() ->
%%     "Hypernumbers failed to build after Revision ~p".

email(To, From, Subject, Msg) ->
    {ok, Server}   = application:get_env(hypernumbers, mailserver),
    {ok, Password} = application:get_env(hypernumbers, mailpassword),
    {ok, User}     = application:get_env(hypernumbers, mailuser),
    email([{server, Server}, {user, User}, {password, Password}],
          To, From, Subject, Msg).
    
    
email(Details, To, From, Subject, Msg) ->

    Server = proplists:get_value(server, Details),    
    User   = proplists:get_value(user, Details),
    Pass   = proplists:get_value(password, Details),
    
    {ok, Socket} = ssl:connect(Server, 465, [{active, false}], 1000),
    
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode(User))),
    send(Socket, binary_to_list(base64:encode(Pass))),
    send(Socket, "MAIL FROM:<"++parse_email_address(From)++">"),
    send(Socket, "RCPT TO:<"++parse_email_address(To)++">"),
    send(Socket, "DATA"),
    send_no_receive(Socket, "From: "++From),
    send_no_receive(Socket, "To: "++To),
    send_no_receive(Socket, "Date: "++dh_date:format("r")),
    send_no_receive(Socket, "Subject: "++Subject),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, Msg),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

parse_email_address(Address) ->
    lists:last(string:tokens(Address, "<>")).

send_no_receive(Socket, Data) ->
    io:format("SEND: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n").

send(Socket, Data) ->
    io:format("SEND: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) ->
    case ssl:recv(Socket, 0, 5000) of
        {ok, Return}    -> io:format("RECV: ~p~n", [Return]);
        {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
    end.
