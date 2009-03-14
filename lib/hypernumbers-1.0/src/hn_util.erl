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

-export([
         % HyperNumbers Utils
         jsonify_val/1,

         index_to_url/1,
         ref_to_str/1,
         xml_to_val/1,
         item_to_xml/1,
         in_range/2,
         to_xml/1,
         ref_to_index/1,
         refX_to_index/1,
         range_to_list/1,
         rectify_range/4,

         % HTTP Utils
         req/1,
         post/2,
         post/3,
         parse_url/1,
         parse_vars/1,
         
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
         refX_to_ref/2,
         ref_to_refX/2,
         from_hn_item/1,
         refX_from_index/1,
         index_from_refX/1,
         index_from_ref/1,
         url_to_refX/1
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions convert to and from #refX, #ref, #hn_item                %%%
%%% and #indexrecords                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jsonify_val({"__permissions", _}) ->
    {"__permissions", "bleh"};
jsonify_val({"__groups", _}) ->
    {"__groups", "bleh"};
jsonify_val({"dependency-tree", _}) ->
    {"dependency-tree", "bleh"};
jsonify_val({"parents", _}) ->
    {"parents", "bleh"};
jsonify_val({Name, {errval, Error}}) ->
    {Name, atom_to_list(Error)};
jsonify_val({Name, {datetime, Date, Time}}) ->
    {Name, muin_date:to_rfc1123_string({datetime, Date, Time})};
jsonify_val(Else) ->
    Else.

url_to_refX({url, [{type, _}], [Url]}) ->
    {ok, Ref} = parse_url(Url),
    {RefX, _} = ref_to_refX(Ref, "to be chucked away"),
    RefX.
    
refX_to_ref(RefX, Name) ->
    #refX{site = S, path = P, obj = R, auth = A} = RefX,
    #ref{site = S, path = P, ref = R, name = Name, auth = A}.

ref_to_refX(Ref, Val) ->
    #ref{site = S, path = P, ref = R, name = Key, auth = A} = Ref,
    RefX = #refX{site = S, path = P, obj = R, auth = A},
    {RefX, {Key, Val}}.

from_hn_item(List) -> from_hn_item(List, []).

from_hn_item([], Acc)      -> Acc;
from_hn_item([H | T], Acc) -> #hn_item{addr = Ref, val = V} = H, 
                              NewAcc = ref_to_refX(Ref, V),
                              from_hn_item(T, [NewAcc | Acc]).

refX_from_index(#index{site = S, path = P, column = X, row = Y}) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}}.

index_from_refX(#refX{site = S, path = P, obj = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

index_from_ref(#ref{site = S, path = P, ref = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% HyperNumbers Utils                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rectify_range(X1, Y1, X2, Y2) ->
    % in case the range is passed in arsey-backwards
    {X1a, X2a} = if
                     X1 < X2 -> {X1, X2};
                     true    -> {X2, X1}
                 end,
    {Y1a, Y2a} = if
                     Y1 < Y2 -> {Y1, Y2};
                     true    -> {Y2, Y1}
                 end,
    {X1a, Y1a, X2a, Y2a}.

%% This is a bit of a mess - range_to_list1 works for #refs{},
%% and range_to_list2 works for #refX{}'s
range_to_list(#ref{ref = {range, {X1, Y1, X2, Y2}}} = Ref) ->
    {X1a, Y1a, X2a, Y2a} = rectify_range(X1, Y1, X2, Y2),
    range_to_list1(Ref, X1a, X1a, Y1a, X2a, Y2a, []);
range_to_list(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    {X1a, Y1a, X2a, Y2a} = rectify_range(X1, Y1, X2, Y2),
    range_to_list2(RefX, X1a, X1a, Y1a, X2a, Y2a, []).

range_to_list1(Ref, _Reset, X, Y, X, Y, Acc) -> [Ref#ref{ref = {cell, {X, Y}}} | Acc];
range_to_list1(Ref, Reset, X2, Y1, X2, Y2, Acc) ->
    range_to_list1(Ref, Reset, Reset, Y1 + 1, X2, Y2,
                  [Ref#ref{ref = {cell, {X2, Y1}}} | Acc]);
range_to_list1(Ref, Reset, X1, Y1, X2, Y2, Acc) ->
    range_to_list1(Ref, Reset, X1 + 1, Y1, X2, Y2,
                  [Ref#ref{ref = {cell, {X1, Y1}}} | Acc]).

range_to_list2(RefX, _Reset, X, Y, X, Y, Acc) -> [RefX#refX{obj = {cell, {X, Y}}} | Acc];
range_to_list2(RefX, Reset, X2, Y1, X2, Y2, Acc) ->
    range_to_list2(RefX, Reset, Reset, Y1 + 1, X2, Y2,
                  [RefX#refX{obj = {cell, {X2, Y1}}} | Acc]);
range_to_list2(RefX, Reset, X1, Y1, X2, Y2, Acc) ->
    range_to_list2(RefX, Reset, X1 + 1, Y1, X2, Y2,
                  [RefX#refX{obj = {cell, {X1, Y1}}} | Acc]).

index_to_url(#index{site=Site,path=Path,column=X,row=Y}) ->
    lists:append([Site, list_to_path(Path),tconv:to_b26(X), text(Y)]).

list_to_path([])   -> "/";
list_to_path(Path) -> "/" ++ string:join(Path, "/") ++ "/".

ref_to_index(#ref{site = S, path = P, ref= {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

refX_to_index(#refX{site = S, path = P, obj = {cell, {X, Y}}}) ->
    #index{site = S, path = P, column = X, row = Y}.

%% @todo remove these old compatibility clauses when the new GUI is in
%% Gordon Guthrie 2009/01/21
ref_to_str({page,Path})           -> Path;   
ref_to_str({cell,{X,Y}})          -> tconv:to_b26(X)++text(Y);
ref_to_str({row,{Y,Y}})           -> text(Y);
ref_to_str({row,{Y}})             -> text(Y); % old compatibility - delete!
ref_to_str({row,{Y1,Y2}})         -> text(Y1)++":"++text(Y2);
ref_to_str({column,X})          -> tconv:to_b26(X); % old compatibility - delete!
ref_to_str({column,{X,X}})        -> tconv:to_b26(X);
ref_to_str({column,{X1,X2}})      -> tconv:to_b26(X1)++":"++tconv:to_b26(X2);
ref_to_str({range,{X1,Y1,X2,Y2}}) -> tconv:to_b26(X1)++text(Y1)++":"++
                                         tconv:to_b26(X2)++text(Y2).

xml_to_val({bool,[],[true]})      -> true;
xml_to_val({bool,[],[false]})     -> false;
xml_to_val({errval,[],[Ref]})     -> Ref;
xml_to_val({float,[],[Ref]})      -> list_to_float(Ref);
xml_to_val({int,[],[Ref]})        -> list_to_integer(Ref);
xml_to_val({string,[],[Ref]})     -> Ref;
xml_to_val({datetime, [], [Ref]}) -> Ref;
xml_to_val(Else)                  -> Else.

%% Turn a hn_item record into its xml <ref> display
item_to_xml(#hn_item{addr = A, val = V}) ->

    Type = atom_to_list(element(1, A#ref.ref)),
    Str  = hn_util:ref_to_str(A#ref.ref),
    
    Value = case A#ref.name of
                value    -> to_xml(V);
                rawvalue -> to_xml(V);
                style    -> to_xml(V);
                _Else    -> to_val(V)
    end,    
    {ref, [{type, Type}, {ref, Str}], [{A#ref.name, [], Value}]}.
    
in_range({range,{X1,Y1,X2,Y2}},{cell,{X,Y}}) ->
    Y >= Y1 andalso Y =< Y2 andalso X >= X1 andalso X =< X2.

to_val({xml,Xml}) -> Xml;
to_val(Else) ->
    case io_lib:char_list(Else) of
    true  -> [Else];
    false -> throw({unmatched_type,Else})
    end.

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

parse_url(Url) when is_record(Url,url) ->

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
    
    RefType = parse_reference(Ref),    
    RefVal  = case RefType of
                  page ->   "/";
                  cell ->   util2:strip_ref(Ref);
                  range ->  util2:parse_range(Ref);
                  column -> element(1,util2:strip_ref(Ref++"1"));
                  row ->    element(2,util2:strip_ref("a"++Ref))
              end,
    
    {ok,#ref{site=Site,
             path=Path,
             ref={RefType,RefVal}}}.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal Functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_reference("/") -> page;
parse_reference(Cell) ->
    case string:chr(Cell,$:) of
        0 ->
            case hn_util:is_numeric(Cell) of
                true  -> row;
                false ->
                    case hn_util:is_alpha(Cell) of
                        true  -> column;
                        false -> 
                            case regexp:match(Cell,?RG_cell) of
                                {match,_,_} -> cell;
                                _  ->
                                    throw({invalid_reference,Cell})
                            end
                    end
            end;
        _ ->
            range
    end.

