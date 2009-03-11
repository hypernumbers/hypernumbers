%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @TODO write a module descriptor for this module
-module(dh_tree).

-include("hypernumbers.hrl").
-export([new/0, create/1, add/2, set/3, erase/2, flatlist/1]).

%-spec create(List::list(list(string()))) -> any().
%% doc take a list of keys and generate a tree
new() ->
    create([]). 

create(List) ->
    create(List,dict:new()).
create([],Dict) ->
    Dict;
create([H|T],Dict) ->
    create(T,add(H,Dict)).

%-spec add(Key::list(string()),Dict::any()) -> any().
%% @doc Add a key in the form ["key1","key2"] 
%%      to the tree
add([],Dict) -> 
    Dict;
add([H],Dict) ->
    dict:store(H,dict:new(),Dict);
add([H|T],Dict) ->
    NDict = case dict:is_key(H,Dict) of
                true  -> dict:fetch(H,Dict);
                false -> dict:new()
            end,
    dict:store(H,add(T,NDict),Dict).

%-spec add(Key::list(string()),Dict::any()) -> any().
%% @doc Add a key in the form ["key1","key2"] 
%%      to the tree
set([H], Val, Dict) ->
    dict:store(H, Val, Dict);
set([H|T], Val, Dict) ->
    NDict = case dict:is_key(H,Dict) of
                true  -> dict:fetch(H,Dict);
                false -> dict:new()
            end,
    dict:store(H,set(T, Val, NDict),Dict).

%-spec erase(Key::list(string()),Dict::any()) -> any().
%% @doc erase a node from the tree 
%%      (including children)
erase([H],Dict) ->
    dict:erase(H,Dict);
erase([H|T],Dict) ->
    dict:store(H,erase(T,dict:fetch(H,Dict)),Dict).
             
%-spec flatlist(Dict::any()) -> {ok,list()}.
%% @doc Generate a flat list of all the nodes
%%      represented by the nested dict
flatlist(Dict) ->
    {ok,flatlist([],[],Dict)}.

%-spec flatlist(List::list(), Acc::list(), Dict::any()) ->
%    list(list(string())).
%% @doc Generate a flat list of all the nodes
%%      represented by the nested dict
flatlist(List,Acc,Dict) ->
    case dict:fetch_keys(Dict) of
        [] ->
            [Acc|List];
        Keys ->
            F = fun(X,NAcc) ->
                        flatlist(NAcc,Acc++[X],dict:fetch(X,Dict))
                end,
            lists:foldl(F,[Acc|List],Keys)
    end.

