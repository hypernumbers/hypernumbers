%%% @doc Utility functions common to arrays and ranges.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(area_util).
-export([apply_each/2, width/1, height/1, at/3, make_array/2, to_list/1, col/2, row/2]).
-compile(export_all).
-include("handy_macros.hrl").
-include("typechecks.hrl").

-define(OUT_OF_RANGE, {error, out_of_range}).


%% @doc Apply a function to each value in array/range.

%% @spec apply_each(Fun = function(), A = area()) -> area().

apply_each(Fun, A = {Tag, Rows}) when ?is_area(A) ->
    {Tag,
     reverse(foldl(fun(Row, Acc) ->
                           Newrow = map(Fun, Row),
                           [Newrow|Acc]
                   end,
                   [], Rows))}.

%% @doc Apply function to each value in array/range. The function gets
%% element's position in addition to its value. ({Val, {Col, Row}}).

%% @spec apply_each_with_pos(Fun = function(), A = area()) -> area().

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

%% @doc Return area as a list with elements enumerated left-to-right top-down.

%% @spec to_list(Area :: area()) -> list()

to_list(Area = {_, _Rows = [H|T]}) when ?is_area(Area) -> to_list(H, T, []).
to_list(Row, [], Acc) -> Acc ++ Row;
to_list(Row, [NRow|Rest], Acc) -> to_list(NRow, Rest, Acc ++ Row).

%% @doc Get width of array/range.

%% @spec width(A = area()) -> pos_integer()

width(A = {_, Rows}) when ?is_area(A) ->
    length(hd(Rows)).

%% @doc Get height of array/range.

%% @spec height(A = area()) -> pos_integer()

height(A = {_, Rows}) when ?is_area(A) ->
    length(Rows).

%% @doc Return value at position.

%% @spec at(Col = pos_integer(), Row = pos_integer(), A = area()) -> term()

%% this is O(r + c)
at(Col, Row, A = {_, Rows}) when ?is_area(A) ->
    at(Col, Row, Rows, width(A), height(A)).

at(Col, Row, _Rows, W, H) when Col > W orelse Row > H ->
    ?OUT_OF_RANGE;
at(Col, Row, _Rows, _W, _H) when Col < 1 orelse Row < 1 ->
    ?OUT_OF_RANGE;
at(Col, Row, Rows, _W, _H) ->
    {ok, nth(Col, nth(Row, Rows))}.

%% @doc Create an array from a list of rows.

%% @spec make_array(Rows = list()) -> array().

make_array(Rows) when is_list(Rows) ->
    {array, Rows}. %% Check that all rows are of equal length?

%% @doc Create an array of specified width & height filled with 0s.

%% @spec make_array(W = pos_integer(), H = pos_integer()) -> array().

make_array(W, H) ->
    {array, lists:duplicate(H, lists:duplicate(W, 0))}.

%% @doc Return horizontal array.

%% @spec row(N = pos_integer(), A = area()) -> area().

row(N, A = {Tag, Rows}) when ?is_area(A) andalso is_integer(N) ->
    H = height(A),
    if H < N -> ?OUT_OF_RANGE;
       true  -> {Tag, nth(N, Rows)}
    end.

%% @doc Return vertical array.

%% @spec col(N = pos_integer(), A = area()) -> area().

col(N, A = {Tag, Rows}) when ?is_area(A) andalso is_integer(N) ->
    W = width(A),
    if W < N -> ?OUT_OF_RANGE;
       true  ->
            R = foldl(fun(X, Acc) -> [[nth(N, X)]|Acc] end, [], Rows),
            {Tag, R}
    end.
