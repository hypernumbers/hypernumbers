%%%-------------------------------------------------------------------
%%% File    : hn_workq.erl
%%%
%%% Description : Bootstraped implementation of mergeable priority
%%% queues.
%%%
%%%-------------------------------------------------------------------
-module(hn_workq).

-export([new/1, new/2,
         add/3,
         needs_elem/3,
         next/1,
         id/1,
         is_empty/1,
         merge/2]).

-include_lib("eunit/include/eunit.hrl").
-include("auth.hrl").

-type now() :: {integer(),integer(),integer()}.
-type work_queue() :: {dict(), 
                       nil | [{pos_integer(), any()}], 
                       integer() | now(),
                       nil | auth_req()}.

-spec new(nil | auth_req()) -> work_queue().
new(AuthReq) -> {dict:new(), nil, erlang:now(), AuthReq}.

-spec new(nil | auth_req(), integer()) -> work_queue().
new(AuthReq, Ts) -> {dict:new(), nil, Ts, AuthReq}. 

-spec id(work_queue()) -> integer() | now().
id({_, _, Ts, _}) -> Ts.

-spec is_empty(work_queue()) -> boolean().
is_empty({Map, _, _, _}) -> 
   dict:size(Map) == 0.
   
-spec add(any(), integer(), work_queue()) -> work_queue(). 
add(Elem, Priority, {Map, _, Ts, Ar}) ->
    Item = {Priority, Elem},
    Map2 = dict:store(Elem, Item, Map),
    {Map2, nil, Ts, Ar}.

%% Needs element determines if the proposed candidate and
%% priority needs to be added to the dictionary:
%% If elem is not in dictionary, then yes;
%% Otherwise, only if the new priority is higher.
-spec needs_elem(any(), integer(), work_queue()) -> boolean().
needs_elem(Elem, NewPri, {Map, _, _ , _}) -> 
    case dict:find(Elem, Map) of
        {ok, {P, _}} when P >= NewPri -> false;
        _ -> true
    end.

-spec next(work_queue()) 
          -> {empty, work_queue()} | {any(), nil | auth_req(), work_queue()}.
next({Map, nil, Ts, Ar}) ->
    Vs = [V || {_K, V} <- dict:to_list(Map)],
    next({Map, lists:usort(Vs), Ts, Ar});
next({_Map, [], _, _}=WQ) ->
    {empty, WQ};
next({Map, [{_Priority, Elem} | Rest], Ts, Ar}) ->
    Map2 = dict:erase(Elem, Map),
    {Elem, Ar, {Map2, Rest, Ts, Ar}}.

%% Merge the right heap into the left heap. When duplicate items with
%% different priorities are encountered, the highest priority is
%% retained (higher priorities are calculated later).
-spec merge(work_queue(), work_queue() | [work_queue()]) -> work_queue().

merge(Q, []) ->
    Q;
merge(Q, [QR|Qs]) ->
    Q2 = merge(Q, QR),
    merge(Q2, Qs);
merge({LMap, _, _, _}, {RMap, _, RTs, RAr}) ->
    Map2 = dict:merge(fun merge_fun/3, LMap, RMap),
    {Map2, nil, RTs, RAr}.

merge_fun(_Key, {P1, _}=V1, {P2, _}) when P1 >= P2 ->
    V1;
merge_fun(_Key, _V1, V2) ->
    V2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unit_test_() ->
    [fun test_simple/0,
     fun test_add/0,
     fun test_needs_elem/0,
     fun test_merge/0,
     fun test_merge_multi/0].


-spec test_simple() -> no_return().
test_simple() ->
    Q0 = ?MODULE:new(nil),
    true = ?MODULE:is_empty(Q0),
    Q1 = ?MODULE:new(nil, 123),
    true = ?MODULE:is_empty(Q1),
    123 = ?MODULE:id(Q1).
    
-spec test_add() -> no_return(). 
test_add() ->
    Q0 = ?MODULE:new(nil),
    {empty, Q1} = ?MODULE:next(Q0),

    Q2 = ?MODULE:add(alpha, 1, Q1),
    Q3 = ?MODULE:add(beta, 10, Q2),
    Q4 = ?MODULE:add(gamma, 5, Q3),
    
    {alpha, nil, Q5} = ?MODULE:next(Q4),
    {gamma, nil, Q6} = ?MODULE:next(Q5),
    {beta,  nil, Q7} = ?MODULE:next(Q6),
    {empty, _Q} = ?MODULE:next(Q7).

-spec test_needs_elem() -> no_return().
test_needs_elem() -> 
    Q0 = ?MODULE:new(nil),
    true = ?MODULE:needs_elem(alpha, 10, Q0),
    Q1 = ?MODULE:add(alpha, 10, Q0),
    false = ?MODULE:needs_elem(alpha, 9, Q1),
    false = ?MODULE:needs_elem(alpha, 10, Q1),
    true = ?MODULE:needs_elem(alpha, 11, Q1),
    true = ?MODULE:needs_elem(zeta, 20, Q1).

-spec test_merge() -> no_return().
test_merge() ->
    LQ0 = ?MODULE:new(nil),
    LQ1 = ?MODULE:add(alpha, 1, LQ0),
    LQ2 = ?MODULE:add(beta, 4, LQ1),

    RQ0 = ?MODULE:new(nil),
    RQ1 = ?MODULE:add(alpha, 2, RQ0),
    RQ2 = ?MODULE:add(delta, 1, RQ1),
    
    Q0 = ?MODULE:merge(LQ2, RQ2),
    {delta, nil, Q1} = ?MODULE:next(Q0),
    {alpha, nil, Q2} = ?MODULE:next(Q1),
    {beta, nil, _}   = ?MODULE:next(Q2).

-spec test_merge_multi() -> no_return().
test_merge_multi() ->
    Q0 = ?MODULE:new(nil),
    Q1 = ?MODULE:add(omega, 0, Q0),

    LQ0 = ?MODULE:new(nil),
    LQ1 = ?MODULE:add(alpha, 1, LQ0),
    LQ2 = ?MODULE:add(beta, 4, LQ1),

    RQ0 = ?MODULE:new(nil),
    RQ1 = ?MODULE:add(alpha, 2, RQ0),
    RQ2 = ?MODULE:add(delta, 1, RQ1),
    
    Q2 = ?MODULE:merge(Q1, [LQ2, RQ2]),
    {omega, nil, Q3} = ?MODULE:next(Q2),
    {delta, nil, Q4} = ?MODULE:next(Q3),
    {alpha, nil, Q5} = ?MODULE:next(Q4),
    {beta, nil, _}   = ?MODULE:next(Q5).
    
