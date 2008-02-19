%%% Built-in math functions.
%%% Hasan Veldstra <hasan@hypernumbers.com>

-module(stdfuns_math).

-include("handy_macros.hrl").
-include("typechecks.hrl").

%%% Excel 2004 Mac API.
-export([
         %% Basic arithmetics
         plus/1,
         minus/1,
         times/1,
         divide/1,
         negate/1,
         
         %% Arithmetic
         sum/1,
         product/1,
         average/1,
         quotient/1,
         abs/1,
         sqrt/1,
         power/1,
         sign/1,
         exp/1,
         fact/1,
         %%gcd/1,
         %%lcm/1,
         mod/1,

         %% Arrays and matrices
         mdeterm/1,
         minverse/1,
         mmult/1,
         %%multinomial

         %% Logarithms
         ln/1,
         log/1,
         log10/1,

         %% Random numbers
         rand/0,
         randbetween/1,

         
         
         %% Rounding numbers
         int/1
        ]).

%%% NOTES:
%%%
%%% INCOMPATIBILITIES:
%%%   1. Many (most? all?) Excel functions discard strings and booleans in
%%%      the input, unless those have been typed directly *into the formula*.
%%%      For example: SUM(A1) where A1 contans "10" will evaluate to 0,
%%%      but SUM("10") will evaluate to 10.
%%%      We don't make this distinction, and simply discard non-numeric
%%%      values.
%%%      Examples of such functions: SUM, PRODUCT, POWER etc.

is_error(X) when is_atom(X) ->
    ?COND(is_number(stdfuns:error_type(X)), true, false);
is_error(_) ->
    false.

%%% ----------------- %%%
%%% Basic arithmetics %%%
%%% ----------------- %%%

plus([Num1, Num2]) ->
    ?ensure_numbers([Num1, Num2]),
    Num1 + Num2.

minus([Num1, Num2]) ->
    ?ensure_numbers([Num1, Num2]),
    Num1 - Num2.

times([Num1, Num2]) ->
    ?ensure_numbers([Num1, Num2]),
    Num1 * Num2.

divide([Num, Divisor]) ->
    ?ensure_numbers([Num, Divisor]),
    ?ensure_nonzero(Divisor),
    Num / Divisor.

negate([Num]) ->
    ?ensure_number(Num),
    -Num.


%%% ---------- %%%
%%% Arithmetic %%%
%%% ---------- %%%

%% Adds all numbers in a range of cells.
sum(Vals_) ->
    Vals = flatten(Vals_),
    ?maybe_throw_up(Vals),
    lists:sum([X || X <- Vals, is_number(X)]).

%% Multiplies all the numbers given as arguments and returns the product.
product(Vals_) ->
    Vals = flatten(Vals_),
    ?maybe_throw_up(Vals),
    foldl(?funXY(X * Y), 1, [X || X <- Vals, is_number(X)]).

%% Returns the integer portion of a division, discarding the remainder.
quotient([Num, Divisor]) ->
    trunc(divide([Num, Divisor])).

%% INCOMPATIBILITY NOTE:
%% ABS(1,2) pops up "too many args" dialog in Excel. We're returning #VALUE!.
abs([Num]) ->
    ?ensure_number(Num),
    erlang:abs(Num).
    
%% Returns the arithmetic means of its arguments.
average(Vals_) ->
    Vals = flatten(Vals_),
    ?maybe_throw_up(Vals),
    Nums = [X || X <- Vals, is_number(X)],
    ?ensure_nonzero(length(Nums)),
    lists:sum(Nums) / length(Nums).

%% Returns a positive square root.
sqrt([Num]) ->
    ?ensure_number(Num),
    ?COND(Num < 0, ?ERR_NUM, math:sqrt(Num)).

power([Num, Pow]) ->
    ?ensure_numbers([Num, Pow]),
    math:pow(Num, Pow).

sign([Num]) ->
    ?ensure_number(Num),
    ?COND(Num > 0, 1,
          ?COND(Num == 0, 0,
                -1)).    

exp([Num]) ->
    ?ensure_number(Num),
    math:exp(Num).

%% NOTE: Num (artificially) limited to 8192 (Excel's limit is 170).
fact([Num]) ->
    ?ensure_number(Num),
    ?COND(Num == 0, 1,
          ?COND(Num < 0, ?ERR_NUM,
                ?COND(Num > 8192, ?ERR_NUM,
                      foldl(?funXY(X * Y), 1, seq(1, Num))))).

%% Returns the remainder after number is divided by divisor. The result
%% has the same sign as divisor.
mod([Num, Divisor]) ->
    ?ensure_numbers([Num, Divisor]),
    Num - Divisor * int(divide([Num, Divisor])).

%%% ------------------- %%%
%%% Arrays and matrices %%%
%%% ------------------- %%%

mdeterm([L]) ->
    ?IF(not(is_list(L)), ?ERR_VAL),
    ?ensure_numbers(flatten(L)),
    Mx = matrix:new(L),
    ?IF(not(matrix:is_square(Mx)), ?ERR_VAL),
    matrix:det(Mx).

minverse([L]) ->
    ?IF(not(is_list(L)), ?ERR_VAL),
    ?ensure_numbers(flatten(L)),
    Mx = matrix:new(L),
    ?IF(not(matrix:is_square(Mx)), ?ERR_VAL),
    ?IF(matrix:det(Mx) == 0, ?ERR_NUM),
    {matrix, _, _, NewL} = matrix:invert(Mx),
    NewL.

mmult([L1, L2]) ->
    ?IF(not(is_list(L1)), ?ERR_VAL),
    ?IF(not(is_list(L2)), ?ERR_VAL),
    ?ensure_numbers(flatten(L1)),
    ?ensure_numbers(flatten(L2)),
    Mx1 = matrix:new(L1),
    Mx2 = matrix:new(L2),

    case matrix:multiply(Mx1, Mx2) of
        {matrix, _, _, NewL} ->
            NewL;
        {error, ""} ->
            ?ERR_VAL
    end.


%%% ---------- %%%
%%% Logarithms %%%
%%% ---------- %%%

ln([Num]) ->
    ?ensure_positive_ex(Num, ?ERR_NUM),
    math:log(Num).

log([Num]) ->
    log([Num, 10]);
log([Num, Base]) ->
    ?ensure_numbers([Num, Base]),
    ?ensure_positive_ex(Num, ?ERR_NUM),
    ?ensure_positive_ex(Base, ?ERR_NUM),
    ?IF(Base == 1, ?ERR_DIV),
    math:log(Num) / math:log(Base).

log10([Num]) ->
    log([Num, 10]).

%%% -------------- %%%
%%% Random numbers %%%
%%% -------------- %%%

rand() ->
    {X, Y, Z} = erlang:now(),
    random:seed(X, Y, Z),
    random:uniform().

randbetween([First, Last]) ->
    ?ensure_numbers([First, Last]),
    rand() * (Last - First) + First.
    

%%% ---------------- %%%
%%% Rounding numbers %%%
%%% ---------------- %%%

%% Rounds a number down to the nearest integer.
int([Num]) ->
    ?ensure_number(Num),
    ?COND(erlang:round(Num) > Num,
          erlang:round(Num) - 1,
          erlang:round(Num)).
