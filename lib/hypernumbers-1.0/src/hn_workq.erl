%%%-------------------------------------------------------------------
%%% File    : hn_workq.erl
%%%
%%% Description : Bootstraped implementation of mergeable priority
%%% queues.
%%%
%%%-------------------------------------------------------------------
-module(hn_workq).

-export([new/0, new/1,
         add/3,
         next/1,
         id/1,
         is_empty/1,
         merge/2]).

-include_lib("eunit/include/eunit.hrl").

-type now() :: {integer(),integer(),integer()}.
-type work_queue() :: {dict(), 
                       nil | [{pos_integer(), any()}], 
                       integer() | now()}.

-spec new() -> work_queue().
new() -> {dict:new(), nil, erlang:now()}.

-spec new(integer()) -> work_queue().
new(Ts) -> {dict:new(), nil, Ts}. 

-spec id(work_queue()) -> integer() | now().
id({_, _, Ts}) -> Ts.

-spec is_empty(work_queue()) -> true | false.
is_empty({Map, _, _}) -> 
   dict:size(Map) == 0.
   
-spec add(any(), integer(), work_queue()) -> work_queue(). 
add(Elem, Priority, {Map, _, Ts}) ->
    Item = {Priority, Elem},
    Map2 = dict:store(Elem, Item, Map),
    {Map2, nil, Ts}.

-spec next(work_queue()) -> {empty | any(), work_queue()}.
next({Map, nil, Ts}) ->
    Vs = [V || {_K, V} <- dict:to_list(Map)],
    next({Map, lists:usort(Vs), Ts});
next({_Map, [], _}=WQ) ->
    {empty, WQ};
next({Map, [{_Priority, Elem} | Rest], Ts}) ->
    Map2 = dict:erase(Elem, Map),
    {Elem, {Map2, Rest, Ts}}.

%% Merge the right heap into the left heap. When duplicate items with
%% different priorities are encountered, the highest priority is
%% retained (higher priorities are calculated later).
-spec merge(work_queue(), work_queue() | [work_queue()]) -> work_queue().

merge(Q, []) ->
    Q;
merge(Q, [QR|Qs]) ->
    Q2 = merge(Q, QR),
    merge(Q2, Qs);
merge({LMap, _, _}, {RMap, _, RTs}) ->
    Map2 = dict:merge(fun merge_fun/3, LMap, RMap),
    {Map2, nil, RTs}.

merge_fun(_Key, {P1, _}=V1, {P2, _}) when P1 >= P2 ->
    V1;
merge_fun(_Key, _V1, V2) ->
    V2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unit_test_() ->
    [?_test(fun test_add/0),
     ?_test(fun test_merge/0),
     ?_test(fun test_merge_multi/0)].

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

test_merge_multi() ->
    Q0 = ?MODULE:new(),
    Q1 = ?MODULE:add(omega, 0, Q0),

    LQ0 = ?MODULE:new(),
    LQ1 = ?MODULE:add(alpha, 1, LQ0),
    LQ2 = ?MODULE:add(beta, 4, LQ1),

    RQ0 = ?MODULE:new(),
    RQ1 = ?MODULE:add(alpha, 2, RQ0),
    RQ2 = ?MODULE:add(delta, 1, RQ1),
    
    Q2 = ?MODULE:merge(Q1, [LQ2, RQ2]),
    {omega, Q3} = ?MODULE:next(Q2),
    {delta, Q4} = ?MODULE:next(Q3),
    {alpha, Q5} = ?MODULE:next(Q4),
    {beta, _}   = ?MODULE:next(Q5),
    ok.
    
