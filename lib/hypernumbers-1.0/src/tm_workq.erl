%%%-------------------------------------------------------------------
%%% File    : tm_workqueue.erl
%%%
%%% Description : Bootstraped implementation of a priority queue.
%%%
%%%-------------------------------------------------------------------
-module(tm_workq).

-export([new/0,
         add/3,
         next/1,
         merge/2]).

-export([test/0]).


-type work_queue() :: {dict(), nil | [{pos_integer(), any()}]}.

-spec new() -> work_queue().
new() -> {dict:new(), nil}.

-spec add(any(), pos_integer(), work_queue()) -> work_queue(). 
add(Elem, Priority, {Map, _}) ->
    Item = {Priority, Elem},
    Map2 = dict:store(Elem, Item, Map),
    {Map2, nil}.

-spec next(work_queue()) -> {empty | any(), work_queue()}.
next({Map, nil}) ->
    Vs = [V || {_K, V} <- dict:to_list(Map)],
    next({Map, lists:usort(Vs)});
next({_Map, []}=WQ) ->
    {empty, WQ};
next({Map, [{_Priority, Elem} | Rest]}) ->
    Map2 = dict:erase(Elem, Map),
    {Elem, {Map2, Rest}}.

%% Merge the right heap into the left heap. When duplicate items with
%% different priorities are encountered, the highest priority is
%% retained (higher priorities are calculated later).
-spec merge(work_queue(), work_queue()) -> work_queue().
merge({LMap, _}, {RMap, _}) ->
    Map2 = dict:merge(fun merge_fun/3, LMap, RMap),
    {Map2, nil}.

merge_fun(_Key, {P1, _}=V1, {P2, _}) when P1 >= P2 ->
    V1;
merge_fun(_Key, _V1, V2) ->
    V2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    ok = test_add(),
    ok = test_merge().

test_add() ->
    Q0 = ?MODULE:new(),
    {empty, Q1} = ?MODULE:next(Q0),

    Q2 = ?MODULE:add(alpha, 1, Q1),
    Q3 = ?MODULE:add(beta, 10, Q2),
    Q4 = ?MODULE:add(gamma, 5, Q3),
    
    {alpha, Q5} = ?MODULE:next(Q4),
    {gamma, Q6} = ?MODULE:next(Q5),
    {beta,  Q7} = ?MODULE:next(Q6),
    {empty, _Q} = ?MODULE:next(Q7),
    ok.

test_merge() ->
    LQ0 = ?MODULE:new(),
    LQ1 = ?MODULE:add(alpha, 1, LQ0),
    LQ2 = ?MODULE:add(beta, 4, LQ1),

    RQ0 = ?MODULE:new(),
    RQ1 = ?MODULE:add(alpha, 2, RQ0),
    RQ2 = ?MODULE:add(delta, 1, RQ1),
    
    Q0 = ?MODULE:merge(LQ2, RQ2),
    {delta, Q1} = ?MODULE:next(Q0),
    {alpha, Q2} = ?MODULE:next(Q1),
    {beta, _}   = ?MODULE:next(Q2),
    ok.
    
