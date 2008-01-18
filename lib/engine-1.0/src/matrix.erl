%%%-------------------------------------------------------------------
%%% File    :     matrix.erl
%%% Author  :     <bd@vixo.com>
%%%
%%% Description : Matrix library function module.
%%%
%%%               This differs from the previous way I implemented it
%%%               by instead using flat lists with keys containing
%%%               their position, seems to work as it is both clearer
%%%               and shorter.
%%%
%%% Created :     24 Jan 2007 by  <bd@vixo.com>
%%%-------------------------------------------------------------------
-module(matrix).

%% API
-export([
	 create/2, create/3, list_to_matrix/2, get/2, set/3, 
	 multiply/2, select/2, cofactor/2, determinant/1, transpose/1,
	 adjoint/1, inverse/1, are_equal/2
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:    list_to_matrix/2
%% Description: Converts flat version of list to 2D matrix.
%%--------------------------------------------------------------------
list_to_matrix(List, RowLength) ->
    Convert = fun(X, {I, J}) when I == RowLength ->
		      {{{I, J}, X}, {1, J + 1}};
		 (X, {I, J})                     ->
		      {{{I, J}, X}, {I + 1, J}}
	      end,
    {Matrix, {_Col, Rows}} = lists:mapfoldl(Convert, {1, 1}, List),
    {Rows - 1, RowLength, Matrix}.

%%--------------------------------------------------------------------
%% Function:    create/2/3
%% Description: Makes a with the rows and columns in args.
%%--------------------------------------------------------------------
create(Rows, Columns) ->
    create(fun(_) -> 0 end, Rows, Columns).

create(F, Rows, Columns) ->
    Base = lists:seq(1, Rows * Columns),
    Convert = fun(X) ->
		      I = (X - 1) rem Columns + 1,
		      J = (X - 1) div Columns + 1,
		      {{I, J}, F({I, J})}
	      end,
    {Rows, Columns, lists:map(Convert, Base)}.

%%--------------------------------------------------------------------
%% Function:    get/2
%% Description: Alters value in matrix at position IJ.
%%--------------------------------------------------------------------
get({_R, _C, Matrix}, {I, J}) ->
    {value, {_Key, Value}} = lists:keysearch({I, J}, 1, Matrix),
    Value.

%%--------------------------------------------------------------------
%% Function:    set/3
%% Description: Alters value in matrix at position IJ.
%%--------------------------------------------------------------------
set({R, C, Matrix}, {I, J}, Value) ->
    {R, C, lists:keyreplace({I, J}, 1, Matrix, {{I, J}, Value})}.

%%--------------------------------------------------------------------
%% Function:    multiply/3

%% Description: Multiplies two matrices together, new matrix has
%%              a number of columns as the first has rows and rows
%%              as the second has columns -> {n2, m1}.
%%--------------------------------------------------------------------
multiply(Matrix1, Matrix2) ->
    F = fun({I, J}) ->
		List1 = select(fun({A, _B}) -> A == I end, Matrix2),
		List2 = select(fun({_A, B}) -> B == J end, Matrix1),
		JustMatrix1 = element(3, List1),
		JustMatrix2 = element(3, List2),
		Zipped = lists:zip(JustMatrix1, JustMatrix2),
		Product = fun({{_K1, X}, {_K2, Y}}, Acc) -> 
				  Acc + (X * Y) 
			  end,
		lists:foldl(Product, 0, Zipped)
	end,
    create(F, element(1, Matrix1), element(2, Matrix2)).

%%--------------------------------------------------------------------
%% Function:    select/3
%%
%% Description: The matrix version of filter, very easy to get rows
%%              and columns of matrices by passing in fun that returns
%%              true when it is equal to the row or column.
%%--------------------------------------------------------------------
select(F, {R, C, Matrix}) ->
    F2 = fun({Key, _Value}) -> F(Key) end,
    {R, C, lists:filter(F2, Matrix)}.    

%%--------------------------------------------------------------------
%% Function:    cofactor/2
%% Description: Gets cofactor matrix of Aij, removes row I, column J.
%%--------------------------------------------------------------------
cofactor(Matrix, {I, J}) ->
    io:format("M: ~p~n", [Matrix]),
    L = select(fun({A, B}) -> not (A == I orelse B == J) end, Matrix),
    Shuffle = fun({{X, Y}, Value}) when X > I, Y > J ->
		      {{X - 1, Y - 1}, Value};
		 ({{X, Y}, Value}) when X > I        ->
		      {{X - 1, Y}, Value};
		 ({{X, Y}, Value}) when Y > J        ->
		      {{X, Y - 1}, Value};
		 (Else)                              ->
		      Else
	      end,
    {M, N} = {element(1, L) - 1, element(2, L) - 1},
    {M, N, lists:map(Shuffle, element(3, L))}.

%%--------------------------------------------------------------------
%% Function:    determinant/1
%% Description: Calculates determinant of square matrix.
%%--------------------------------------------------------------------
determinant({1, 1, [{_Key, Value}]}) ->
    Value;
determinant({N, N, Matrix})          ->
    io:format("Determinant(~p)~n", [Matrix]),
    F = fun(X) ->  
		A = matrix:get({N, N, Matrix}, {X, 1}),
		B = determinant(cofactor({N, N, Matrix}, {X, 1})),
		io:format("{A, B}: {~p, ~p}~n", [A, B]),
		A * B * math:pow(-1, X + 1)
	end,
    lists:sum(lists:map(F, lists:seq(1, N))).

%%--------------------------------------------------------------------
%% Function:    transpose/1
%%
%% Description: Gets transpose, swaps rows and columns (ij -> ji).
%%--------------------------------------------------------------------
transpose(Matrix) ->
    transform(fun({{I, J}, Value}) -> {{J, I}, Value} end, Matrix).

%%--------------------------------------------------------------------
%% Function:    divide/1
%% Description: Divides a matrix by a constant factor.
%%--------------------------------------------------------------------
divide(Matrix, X) when not (X == 0) ->
    F = fun({{I, J}, Value}) -> {{I, J}, Value / X} end,
    transform(F, Matrix).

%%--------------------------------------------------------------------
%% Function:    adjoint/1
%% Description: Gets the adjoint of a matrix.
%%--------------------------------------------------------------------
adjoint(Matrix) ->
    A = fun({I, J}) -> determinant(cofactor(Matrix, {I, J})) end,
    B = fun({{I, J}, Value}) -> 
		{{I, J}, math:pow(-1, I + J) * Value} 
	end,
    C = create(A, element(1, Matrix), element(2, Matrix)),
    transpose(transform(B, C)).

%%--------------------------------------------------------------------
%% Function:    inverse/1
%% Description: Gets the inverse of a matrix.
%%--------------------------------------------------------------------
inverse(Matrix) ->
    case round(determinant(Matrix)) of
	0 -> {error, no_inverse};
	D -> divide(adjoint(Matrix), D)
    end.

% Checks if the contents of two matrices is the same.
are_equal({C, R, _}, {C2, R2, _}) when (R /= R2) orelse (C /= C2) -> io:format("clause 1~n"), false;
are_equal(M1, M2) -> 
  Cols = lists:seq(1, element(1, M1), 1), % Lists of column and row numbers.
  Rows = lists:seq(1, element(2, M1), 1),
  IdxPairs = combine(Cols, Rows), % The column lists has to be *first* because of how matrices are implemented.
  Results = lists:map(fun(X) -> util2:sloppy_equals(matrix:get(M1, X), matrix:get(M2, X)) end, IdxPairs),
  length(lists:filter(fun(X) -> X == false end, Results)) == 0.
  
%%====================================================================
%% Internal functions
%%====================================================================
transform(F, {R, C, Matrix}) ->
    {R, C, lists:map(F, Matrix)}.

% Kinda like zip but doesn't require the lists to be of the same length.
combine(L1, L2) -> 
  F = fun(X) ->
    lists:map(fun(Y) -> {X, Y} end, L2)
  end,
  lists:flatten(lists:map(F, L1)).