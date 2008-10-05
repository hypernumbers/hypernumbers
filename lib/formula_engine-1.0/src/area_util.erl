%%% @doc Utility functions common to arrays and ranges.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(area_util).
-export([apply_each/2, width/1, height/1, get_at/3, make_array/2]).
-compile(export_all).
-include("handy_macros.hrl").
-include("typechecks.hrl").

%% @doc Apply a function to each value in array/range.
apply_each(Fun, A = {Tag, Rows}) when ?is_area(A) ->
    {Tag,
     reverse(foldl(fun(Row, Acc) ->
                           Newrow = map(Fun, Row),
                           [Newrow|Acc]
                   end,
                   [], Rows))}.

%% @doc Apply function to each value in array/range. The function gets
%% element's position in addition to its value. ({Val, {Col, Row}}).
apply_each_with_pos(Fun, A = {Tag, Rows}) when ?is_area(A) ->
    {Tag,
     reverse(foldl(fun({Row, RowIdx}, Acc) ->
                           ValCoords = foldl(fun({V, I}, Acc2) ->
                                                     [{V, {I, RowIdx}}|Acc2]
                                             end,
                                             [], reverse(zip(Row, seq(1, length(Row))))),
                           Newrow = map(Fun, ValCoords),
                           [Newrow|Acc]
                   end,
                   [], zip(Rows, seq(1, length(Rows)))))}.

%% @doc Get width of array/range.
width(A = {_, Rows}) when ?is_area(A) ->
    length(hd(Rows)).

%% @doc Get height of array/range.
height(A = {_, Rows}) when ?is_area(A) ->
    length(Rows).

%% O(r + c)
%% @doc Return value at position.
get_at(Col, Row, A = {_, Rows}) when ?is_area(A) ->
    get_at(Col, Row, Rows, width(A), height(A)).

get_at(Col, Row, _Rows, W, H) when Col > W orelse Row > H ->
    {error, out_of_range};
get_at(Col, Row, _Rows, _W, _H) when Col < 1 orelse Row < 1 ->
    {error, out_of_range};
get_at(Col, Row, Rows, _W, _H) ->
    {ok, nth(Col, nth(Row, Rows))}.

make_array(W, H) ->
    {array, lists:duplicate(H, lists:duplicate(W, 0))}.
