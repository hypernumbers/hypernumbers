%%%-----------------------------------------------------------------------------
%%% File        : util2.erl
%%% Author      : Gordon Guthrie <gordonguthrie@vixo.com>
%%% Description :
%%%
%%% Created     : 22 Nov 2006 by Gordon Guthrie <gordonguthrie@vixo.com>
%%%-----------------------------------------------------------------------------
-module(util2).

-include("spriki.hrl").

-export([
	 get_timestamp/0,
	 get_biccie/0,
	 remove_auth/1,
	 chop/1,
	 rev_chop/1,
	 invalid/0,
	 make_num/1,
	 parse_range/1,
	 strip_ref/1,
	 get_page_path/1,
	 make_text/1,
	 get_ref/2,
	 get_cell_ref/4,
	 repath/1,
	 pad_list/1,
	 unpack_text/1,
	 mk_str_frm_list/1,
	 strip_brackets/1,
	 print_as_hex/2,
	 make_paths/1,
	 make_b26/1,
	 mk_int_frm_b26/1,
	 relative_error/2,
	 sloppy_equals/2]).

%%%-----------------------------------------------------------------------------
%%%
%%% Worker functions for the util2
%%% There are two util files cos you can't load one with try/catch into the
%%% debugger (ie that one)
%%%
%%%-----------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions are all utility functions                                %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_timestamp()->
    {Mega,Sec,Micro}=now(),
    1000000*Mega+1000*Sec+Micro.

%%--------------------------------------------------------------------
%% Function:    get_biccie/0
%% Description: a biccie is like a cookie but we don't store the
%%              session the same way as a cookie
%%--------------------------------------------------------------------
get_biccie() ->
    %% This is the code that generates a cookie in Yaws-1.70
    %% nicked from the module yaws_session_server.erl
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    atom_to_list(node()) ++ [$-|integer_to_list(N)].

remove_auth(Site)->
    [Protocol,Rest]=string:tokens(Site,"//"),
    Site3=case string:tokens(Rest,"@") of
	      [_Auth,Site2] -> Site2;
	      [Site2]      -> Site2
	  end,
    Protocol++"//"++Site3.

chop(Path)->
    string:tokens(Path, "/").

rev_chop(Path)->
    lists:reverse(chop(Path)).

invalid()->{1,[],[{"invalid code"}],[]}.

make_num(List)->
    case regexp:match(List,"^-?[0-9]*$") of
	{match,_,_} -> list_to_integer(List);
	_Other       -> list_to_float(List)
    end.

parse_range(Range)->
    [Cell1,Cell2]=string:tokens(Range,":"),
    Cell1L=string:to_lower(Cell1),
    Cell2L=string:to_lower(Cell2),
    {X1,Y1}=strip_ref(Cell1L),
    {X2,Y2}=strip_ref(Cell2L),
    if (X1 >= X2) ->
	    X1new = X2,
	    X2new = X1;
       true ->
	    X1new = X1,
	    X2new = X2
    end,
    if (Y1 >= Y2) ->
	    Y1new = Y2,
	    Y2new = Y1;
       true ->
	    Y1new = Y1,
	    Y2new = Y2
    end,
    {X1new,Y1new,X2new,Y2new}.

strip_ref(Ref) ->
    {X,Y} = lists:partition(
        fun(XX) ->         
            if XX < 97  -> false;  
	       XX > 122 -> false;
	       true     -> true      
            end
        end,string:to_lower(Ref)),
    
    {mk_int_frm_b26(X),list_to_integer(Y)}.

get_page_path(Path) ->
    case util2:rev_chop(Path) of
	[_ValRef,"?flextoolbar"|Rest] -> repath(Rest)
    end.


make_text(Item) ->
    case Item of
	Item when is_integer(Item)-> integer_to_list(Item);
	Item when is_float(Item)  -> float_to_list(Item);
	Item when is_list(Item)   -> lists:flatten(Item);
	Item when is_atom(Item)   -> atom_to_list(Item);
	false -> "False";
	true -> "True";
	_ -> Item
    end.

get_ref(Site,Formula)->
    [Ref|Rest]=util2:rev_chop(Formula),
    Path=repath(Rest),
    case string:tokens(Ref,":") of
 	[[$x|X],[$y|Y]] ->
 	    make_text(spriki:get_value(Site,Path,X,Y));
 	_ -> "UNDEFINED"
    end.

get_cell_ref(Site,Path,X,Y)->
    make_text(spriki:get_value(Site,Path,X,Y)).

%% this will fuck up the spriki - final slash added to repath!
repath("/")->             "/";
repath([])->              "/";
repath(Rest)->            repath(Rest,[]).
repath([],Residuum)->     lists:concat([Residuum,"/"]);
repath([H|T],Residuum) -> repath(T,lists:concat(["/",H,Residuum])).

pad_list(List)->
    pad_list(List,[]).

pad_list([],[])->
    [];
pad_list([],Residuum)->
    List=lists:reverse(Residuum),
    lists:concat(List);
pad_list([H|T],Residuum) ->
    Fun=fun(X)->lists:append(make_text(X)," ") end,
    List=lists:map(Fun,tuple_to_list(H)),
    pad_list(T,[lists:concat(List)|Residuum]).

unpack_text(Text)->
    {ok,Tok,_}   = erl_scan:string("Return="++lists:flatten(Text)++"."),
    {ok,Abs}     = erl_parse:parse_exprs(Tok),
    {_, List, _} = erl_eval:exprs(Abs,[]),

    lists:map(
        fun(XX)-> 
            #page{site=S,path=P,ref={cell,{X,Y}}} = hn_util:parse_url(XX),
            {S,P,X,Y} 
        end,List).

mk_str_frm_list(List) when is_list(List) ->
    mk_str_frm_list(List,[]).

mk_str_frm_list([H|T],Residuum)->
    mk_str_frm_list(T,[",","\"",H,"\""|Residuum]);
mk_str_frm_list([],[_H|T]) ->
    lists:flatten(["[",lists:reverse(T),"]"]).

strip_brackets(A1) when is_list(A1) ->
    {ok,A2,1}=regexp:sub(A1,"^\\(",""),
    {ok,A3,1}=regexp:sub(A2,"\\)$",""),
    A3.

print_as_hex(Text,Binary) -> 
    io:format("In Binary ~p: ",[Text]),
	print_as_hex(Binary).

print_as_hex(<<>>) -> 
    io:format("~n",[]);
print_as_hex(<<H:8/integer,Rest/binary>>) -> 
    io:format("~.16B ",[H]),
	print_as_hex(Rest).

make_paths(List) when is_list(List) ->
    Fun=fun({S,P,X,Y})->lists:concat(["\"",S,P,"/","x",X,":","y",Y,"\","]) end,
    NewList=lists:map(Fun,List),
    NewList2=lists:flatten(NewList),
    %% Now strip the last ',' off
    string:strip(NewList2,right,$,).

%% TODO: Remove and fix all code that uses this function to use tconv:to_b26().
make_b26(Int) when is_integer(Int) ->
    tconv:to_b26(Int). 

%% this function takes an alpha string and thinks
%% it is a base 26 integer
%% TODO: Remove and fix all code that uses this function to use tconv:to_i/2.
mk_int_frm_b26(X) when is_list(X)->
    tconv:to_i(X, b26).

%% This function computes relative error, which provides a way
%% to compare two floats regardless of their range.
relative_error(0, 0.00000) -> 0.00000000001;
relative_error(0.00000, 0) -> 0.00000000001;
relative_error(0, 0) -> 0.0000000001;
relative_error(Result, ExpectedResult) -> erlang:abs((Result - ExpectedResult) / ExpectedResult).

sloppy_equals(N1, N2) -> relative_error(N1, N2) =< 0.0001.
