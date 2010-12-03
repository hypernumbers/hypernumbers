%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Utility functions common to arrays and ranges.
%%%
%%% TODO: may well be worth switching to a list-of-tuples representation
%%%       (i.e. [{Y, X, Term}]) -- will allow for fast column selection,
%%%       speed up at() and apply_each()/apply_each_with_pos().
%%% TODO: may want to cache width/height as well:
%%%      {Tag, Width, Height, [ {Y, X, Term} ]}

-module(area_util).
-export([apply_each/2, width/1, height/1, at/3, make_array/2, to_list/1,
         col/2, row/2, are_congruent/2]).
-export([to_absolute/3, to_relative/3]).

%% vv -- needs to go.
-compile(export_all).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").

-define(OUT_OF_RANGE, {error, out_of_range}).

is_matrix({_Type, Rows} = Area) when ?is_area(Area) ->
    Size = length(hd(Rows)),
    lists:all(fun(X) -> length(X) == Size end, Rows)
        andalso length(Rows) == Size.

%%% @doc Check if all areas in the list have the same dimensions as A.
are_congruent(A, As) when ?is_area(A), is_list(As) ->
    {W, H} = {width(A), height(A)},
    lists:all(fun(X) -> {W, H} == {width(X), height(X)} end, As);
%%% @doc Check if two areas have the same dimensions.
are_congruent(A1, A2) when ?is_area(A1), ?is_area(A2) ->
    are_congruent(A1, [A2]).

                      
%% @spec apply_each(Fun, A) -> area()  Fun = function(), A = area()
%% @doc Apply a function to each value in array/range.
apply_each(Fun, A = {Tag, Rows}) when ?is_area(A) ->
    {Tag,
     lists:reverse(lists:foldl(fun(Row, Acc) ->
                           Newrow = lists:map(Fun, Row),
                           [Newrow|Acc]
                   end,
                   [], Rows))}.

%% @spec apply_each_with_pos(Fun, A) -> area() Fun = function(), A = area()
%% @doc Apply function to each value in array/range. The function gets
%% element's position in addition to its value. ({Val, {Col, Row}}).
apply_each_with_pos(Fun, A = {Tag, Rows}) when ?is_area(A) ->
    Fun = fun({Row, RowIdx}, Acc) ->
                  ValCoords =
                      
                      Fun2 = fun({V, I}, Acc2) ->
                                     [{V, {I, RowIdx}}|Acc2]
                             end,
                  List = lists:reverse(lists:zip(Row, lists:seq(1, length(Row)))),
                  lists:foldl(Fun2, [], List),
                  Newrow = lists:map(Fun, ValCoords),
                  [Newrow|Acc]
          end,
    {Tag, lists:reverse(lists:foldl(Fun, [], lists:zip(Rows, lists:seq(1, length(Rows)))))}.

%% @spec to_list(Area :: area()) -> list()
%% @doc Return area as a list with elements enumerated left-to-right top-down.
to_list(Area = {_, _Rows = [H|T]}) when ?is_area(Area) -> to_list(H, T, []).
to_list(Row, [], Acc) -> Acc ++ Row;
to_list(Row, [NRow|Rest], Acc) -> to_list(NRow, Rest, Acc ++ Row).

%% @spec width(A :: area()) -> pos_integer()
%% @doc Get width of array/range.
width(Ref) when ?is_rangeref(Ref) ->
    Ref#rangeref.width;
width(A = {_, Rows}) when ?is_area(A) ->
    length(hd(Rows)).

%% @spec height(A :: area()) -> pos_integer()
%% @doc Get height of array/range.
height(Ref) when ?is_rangeref(Ref) ->
    Ref#rangeref.height;
height(A = {_, Rows}) when ?is_area(A) ->
    length(Rows).

%% @spec at(Col :: pos_integer(), Row :: pos_integer(), A :: area()) -> term()
%% @doc Return value at position.
%% this is O(r + c)
at(Col, Row, A = {_, Rows}) when ?is_area(A) ->
    at(Col, Row, Rows, width(A), height(A)).

at(Col, Row, _Rows, W, H) when Col > W orelse Row > H ->
    ?OUT_OF_RANGE;
at(Col, Row, _Rows, _W, _H) when Col < 1 orelse Row < 1 ->
    ?OUT_OF_RANGE;
at(Col, Row, Rows, _W, _H) ->
    {ok, lists:nth(Col, lists:nth(Row, Rows))}.

%% @spec make_array(Rows :: list()) -> array()
%% @doc Create an array from a list of rows.
make_array(Rows) when is_list(Rows) ->
    {array, Rows}. %% Check that all rows are of equal length?

%% @spec make_array(W :: pos_integer(), H :: pos_integer()) -> array()
%% @doc Create an array of specified width and height filled with 0s.
make_array(W, H) ->
    {array, lists:duplicate(H, lists:duplicate(W, 0))}.

%% @spec row(N :: pos_integer(), A :: area()) -> area()
%% @doc Return horizontal array.
row(N, A = {Tag, Rows}) when ?is_area(A) andalso is_integer(N) ->
    H = height(A),
    if H < N -> ?OUT_OF_RANGE;
       true  -> {Tag, [lists:nth(N, Rows)]}
    end.

%% @spec col(N :: pos_integer(), A :: area()) -> area()
%% @doc Return vertical array.
col(N, A = {Tag, Rows}) when ?is_area(A) andalso is_integer(N) ->
    W = width(A),
    if W < N -> ?OUT_OF_RANGE;
       true  ->
            R = lists:foldr(fun(X, Acc) -> [[lists:nth(N, X)]|Acc] end, [], Rows),
            {Tag, R}
    end.

to_absolute(R, CellX, CellY) when ?is_rangeref(R) ->
    abs_rel_convert(R, CellX, CellY, fun to_abs/2).

to_relative(R, CellX, CellY) when ?is_rangeref(R) ->
    abs_rel_convert(R, CellX, CellY, fun to_rel/2).

to_abs({offset, O}, C) -> O + C; 
to_abs(A, _)           -> A. 

to_rel({offset, _}=O, _) -> O; 
to_rel(P, C)             -> {offset, P - C}.

abs_rel_convert(R, CellX, CellY, F) when ?is_rangeref(R) ->
    {X1, Y1} = R#rangeref.tl,
    {X2, Y2} = R#rangeref.br,
    R#rangeref{tl = {F(X1, CellX), F(Y1, CellY)},
               br = {F(X2, CellX), F(Y2, CellY)}}.

