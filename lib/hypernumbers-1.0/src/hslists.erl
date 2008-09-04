%%% @doc List functions? MOAR!!
%%% @author Hasan Veldstra <hasan@12monkeys.co.uk> [http://12monkeys.co.uk]
%%% Distributed under the same license as Erlang.

%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%% NB: The main repo for hslists lives at:
%%%     http://www.github.com/hassy/hslists/tree/master/
%%% This file has to be copied here manually (no externals for Git).
%%% Last updated on: Apr 29, 2008
%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-module(hslists).
-import(lists, [all/2, append/1, duplicate/2, flatten/1, foldl/3, foreach/2,
                map/2, nth/2, nthtail/2, reverse/1, seq/2, sum/1, zip/2,
                zipwith/3]).

-export([init/1, mid/1, random/1, intersperse/2, join/2, intercalate/2,
         transpose/1, product/1, average/1, loop/2, shuffle/1, uniq/1,
         enumerate/1, drop/2]).

%% @doc Returns all the elements of a list except the last one.
init(L) ->
    reverse(tl(reverse(L))).

%% @doc Returns list without the first and the last element.
mid(L) when length(L) > 1 ->
    reverse(tl(reverse(tl(L)))).

%% @doc Takes an element and a list and intersperses that element between
%% the elements of the list.
intersperse(_Elt, []) ->
    [];
intersperse(Elt, L) ->
    tl(intersperse1(Elt, L, [])).

intersperse1(Elt, [Hd | Tl], Acc) ->
    intersperse1(Elt, Tl, Acc ++ [Elt, Hd]);
intersperse1(_Elt, [], Acc) ->
    Acc.

%% @doc intersperse/1.
join(Elt, L) ->
    intersperse(Elt, L).

%% @doc Inserts the list L in between the lists in Layers and flattens the
%% result.
intercalate(L, Layers) ->
    flatten(intersperse(L, Layers)).

%% @doc Transposes the rows and columns of its argument. For example:<br />
%% <code>transpose([[1, 2, 3], [a, b, c]]) = [[1, a], [2, b], [3, c]]</code>
%% <br />
%% All sublists must be of the same length.
transpose(L) ->
    Len = length(hd(L)),
    foreach(fun(X) -> Len = length(X) end,
            L),
    transpose1(L, duplicate(Len, [])).

transpose1([Row | Restrows], Acc) ->
    Newacc = zipwith(fun(X, Y) -> Y ++ [X] end,
                     Row, Acc),
    transpose1(Restrows, Newacc);
transpose1([], Acc) ->
    Acc.

%% @doc Computes the product of a list of numbers.
product([Hd | Tl]) ->
    foldl(fun(X, Acc) -> X * Acc end,
          Hd, Tl).

average(L) ->
    sum(L) / length(L).

%% @doc Calls fun F N times and returns the list of results.
loop(Fun, N) when is_integer(N) ->
    loop1(Fun, 1, N, []);
%% @doc Calls fun F on each element of list L, also giving that element's
%% position in list L as second parameter and returns the list of results.
loop(Fun, L) when is_list(L) ->
    loop2(Fun, L, 1, length(L), []).

loop1(_Fun, Curr, N, Acc) when Curr > N ->
    reverse(Acc);
loop1(Fun, Curr, N, Acc) ->
    loop1(Fun, Curr + 1, N, [Fun(Curr) | Acc]).

loop2(_Fun, [], _Curr, _N, Acc) ->
    reverse(Acc);
loop2(Fun, [Hd | Tl], Curr, N, Acc) ->
    loop2(Fun, Tl, Curr + 1, N, [Fun(Hd, Curr) | Acc]).

%% @doc Returns randomly picked element from the list.
random(L) ->
    nth(random:uniform(length(L)), L).

%% @doc Returns a list containing elements of L in random order.
shuffle(L) ->
    shuffle1(L, []).
shuffle1([], Acc) ->
    Acc;
shuffle1(L, Acc) ->
    RandElt = random(L),
    shuffle1(L -- [RandElt], [RandElt | Acc]).

%% @doc Return a copy of list with duplicates removed.
uniq([Hd | Tl]) ->
    [ Hd | uniq([Y || Y <- Tl, Y =/= Hd])];
uniq([]) ->
    [].

%% @doc Return list with sequence numbers attached to elements.
enumerate(L) ->
    zip(L, seq(1, length(L))).

%% @doc Return list with first N elements dropped. If N is negative, then
%% elements are dropped from the end of the list.
drop(L, N) when N > 0 ->
    nthtail(N, L);
drop(L, N) when N < 0 ->
    reverse(nthtail(N * -1, reverse(L))).
