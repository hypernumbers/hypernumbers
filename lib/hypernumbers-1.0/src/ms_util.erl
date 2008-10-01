%%%-------------------------------------------------------------------
%%% File        : ms_util.erl
%%% Author      : Gordon Guthrie gordon@hypernumbers.com
%%% Description : this is the match spec utilities module
%%%               it works closely with ms_util2.erl which is
%%%               the generated module that 'introspects'
%%%               the record structures of myheader.hrl
%%%
%%% Created     :  3 Sep 2008 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(ms_util).

-export([make_ms/2]).
-import(ms_util2,[get_index/2,no_of_fields/1]).

%%%
%%% External Functions (API)
%%%

make_ms(Rec,List) when is_atom(Rec), is_list(List) ->
    NoFields=no_of_fields(Rec),
    NewList=proc_list(Rec,List),
    Return=list_to_tuple([Rec|build(NewList,NoFields)]),
    Return.

%%%
%%% Internal Functions
%%%

proc_list(Rec,List) -> proc_list(Rec,List,[]).

%% bit funky - return the list sorted in reverse order
proc_list(Rec,[],Acc)            -> lists:reverse(lists:keysort(1,Acc));
proc_list(Rec,[{Field,B}|T],Acc) -> proc_list(Rec,T,[{get_index(Rec,Field),
                                                      B}|Acc]).

build(List,NoFields) -> build(List,NoFields,[]).

build([],0,Acc)           -> Acc;
build([{N,Bits}|T],N,Acc) -> build(T,N-1,[Bits|Acc]);
build([H|T],N,Acc)        -> build([H|T],N-1,['_'|Acc]);%don't drop H - matchs later
build([],N,Acc)           -> build([],N-1,['_'|Acc]).
