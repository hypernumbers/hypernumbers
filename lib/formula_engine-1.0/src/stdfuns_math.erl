%%% @doc Built-in math functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>


%%% IMPORTANT NOTES:
%%% ================
%%%
%%%
%%% INCOMPATIBILITIES:
%%%   1. Many (most? all?) Excel functions discard strings and booleans in
%%%      the input, unless those have been typed directly *into the formula*.
%%%      For example: SUM(A1) when A1="10" will evaluate to 0,
%%%      but SUM("10") will evaluate to 10.
%%%      We don't make this distinction, and simply discard non-numeric
%%%      values.
%%%      Examples of such functions: SUM, PRODUCT, POWER etc.
%%%
%%%   2. This one applies for functions with a fixed number of arguments,
%%%      e.g. ABS. If wrong number of arguments is entered, Excel displays an
%%%      error dialog, and doesn't attempt to evaluate the formula. Most of
%%%      the time, our implementations will return #VALUE!.
%%%
%%%   3. Our SUBTOTAL does not care about hidden values. SUBTOTAL(1, A1:A5),
%%%      and SUBTOTAL(101, A1:A5) will do the same thing.

-module(stdfuns_math).

-include("handy_macros.hrl").
-include("typechecks.hrl").

-import(muin_util, [cast/2]).

-export([
         %% Basics
         '+'/1,
         '-'/1,
         '*'/1,
         '/'/1,
         negate/1,
         
         %% Arithmetic
         sum/1,
         product/1,
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
         round/1,
         rounddown/1,
         roundup/1,
         ceiling/1,
         combin/1,
         even/1,
         floor/1,
         int/1,
         mround/1,
         odd/1,
         trunc/1,

         %% Special numbers
         pi/0,
         sqrtpi/1,
         roman/1,

         %% Summation
         %%seriessum/1,
         subtotal/1,
         sumif/1,
         sumproduct/1,
         sumsq/1,
         %%sumx2my2/1,
         %%sumx2py2/1,
         %%sumxmy2/1,

         %% Trigonometry
         sin/1,
         cos/1,
         tan/1,
         asin/1,
         acos/1,
         atan/1,
         atan2/1,
         sinh/1,
         cosh/1,
         tanh/1,
         asinh/1,
         acosh/1,
         atanh/1,
         degrees/1,
         radians/1
        ]).

-compile(export_all).

-define(is_multiple(Num, Mult),
        (erlang:trunc(Num / Mult) * Mult) == (Num * 1.0)).


%% is_error(X) when is_atom(X) ->
%%     ?COND(is_number(stdfuns:error_type(X)), true, false);
%% is_error(_) ->
%%     false.

%%% ----------------- %%%
%%% Basic arithmetics %%%
%%% ----------------- %%%

'+'([blank, Num2]) ->
    '+'([0, Num2]);
'+'([Num1, blank]) ->
    '+'([Num1, 0]);
'+'([Num1, Num2]) ->
    ?ensure_numbers([Num1, Num2]),
    Num1 + Num2.

'-'([blank, Num2]) ->
    '-'([0, Num2]);
'-'([Num1, blank]) ->
    '-'([Num1, 0]);
'-'([Num1, Num2]) ->
    ?ensure_numbers([Num1, Num2]),
    Num1 - Num2.

'*'([blank, Num2]) ->
    '*'([0, Num2]);
'*'([Num1, blank]) ->
    '*'([Num1, 0]);
'*'([Num1, Num2]) ->
    ?ensure_numbers([Num1, Num2]),
    Num1 * Num2.

'/'([blank, Num2]) ->
    '/'([0, Num2]);
'/'([_, blank]) ->
    ?ERR_DIV;
'/'([Num, Divisor]) ->
    ?ensure_numbers([Num, Divisor]),
    ?ensure_nonzero(Divisor),
    Num / Divisor.

negate([Num]) ->
    ?ensure_number(Num),
    -(Num).


%%% ---------- %%%
%%% Arithmetic %%%
%%% ---------- %%%

sum(Vals) ->
    Flatvals = flatten(Vals),
    ?ensure_no_errvals(Flatvals),
    Nums = [X || X <- Flatvals, is_number(X)],
    sum1(Nums).
sum1(Nums) ->
    lists:sum(Nums).

product(Vals) ->
    Flatvals = flatten(Vals),
    ?ensure_no_errvals(Flatvals),
    Nums = [X || X <- Flatvals, is_number(X)],
    product1(Nums).
product1(Nums) ->
    foldl(?Lxacc(X * Acc), 1, Nums).

quotient([Num, Divisor]) ->
    ?MODULE:trunc('/'([Num, Divisor])).

abs([Num]) ->
    ?ensure_number(Num),
    erlang:abs(Num).
    
sqrt([Num]) ->
    ?ensure_number(Num),
    ?ensure(Num >= 0, ?ERR_NUM),
    math:sqrt(Num).

power([Num, Pow]) ->
    ?ensure_numbers([Num, Pow]),
    math:pow(Num, Pow).

sign([Num]) ->
    ?ensure_number(Num),
    sign1(Num).
sign1(0) ->
    0;
sign1(X) when X > 0 ->
    1;
sign1(X) when X < 0 ->
    -1.

exp([Num]) ->
    ?ensure_number(Num),
    math:exp(Num).

%% NOTE: Num (artificially) limited to 8192 (Excel's limit is 170).
fact([Num_]) ->
    ?ensure_number(Num_),
    Num = erlang:trunc(Num_),
    ?ensure(Num =< 8192, ?ERR_NUM),
    ?ensure_non_negative_ex(Num, ?ERR_NUM),
    fact1(Num).
fact1(0) ->
    1;
fact1(Num) ->
    foldl(?Lxacc(X * Acc),
          1,
          seq(1, Num)).

%% Returns the remainder after number is divided by divisor. The result
%% has the same sign as divisor.
mod([Num, Divisor]) ->
    ?ensure_numbers([Num, Divisor]),
    Num - Divisor * int('/'([Num, Divisor])).

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
        {error, _} ->
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
    random:uniform().

randbetween([First, Last]) ->
    ?ensure_numbers([First, Last]),
    rand() * (Last - First) + First.
    

%%% ---------------- %%%
%%% Rounding numbers %%%
%%% ---------------- %%%

round([Num, NumDigits]) ->
    ?ensure_numbers([Num, NumDigits]),
    round1(Num, NumDigits).
round1(Num, 0) ->
    erlang:round(Num);
round1(Num, NumDigits) when NumDigits < 0 ->
    Pow = math:pow(10, erlang:abs(NumDigits)),
    erlang:round(Num / Pow) * erlang:round(Pow);
round1(Num, NumDigits) ->
    Pow = math:pow(10, NumDigits),
    erlang:round(Num * Pow) / erlang:round(Pow).

rounddown([Num, NumDigits]) ->
    ?ensure_numbers([Num, NumDigits]),
    rounddown1(Num, NumDigits).
rounddown1(Num, 0) ->
    erlang:trunc(Num);
rounddown1(Num, NumDigits) when NumDigits < 0 ->
    Pow = math:pow(10, erlang:abs(NumDigits)),
    erlang:trunc(Num / Pow) * Pow;
rounddown1(Num, NumDigits) when NumDigits > 0 ->
    Pow = math:pow(10, NumDigits),
    erlang:trunc(Num * Pow) / Pow.

roundup([Num, NumDigits]) ->
    ?ensure_numbers([Num, NumDigits]),
    roundup1(Num, NumDigits).
roundup1(Num, 0) ->
    ?COND(erlang:trunc(Num) == Num,
          Num,
          erlang:trunc(Num) + sign1(Num));
roundup1(Num, NumDigits) when NumDigits < 0 ->
    Pow = math:pow(10, erlang:abs(NumDigits)),
    Rounded = erlang:trunc(erlang:trunc(Num) / Pow) * Pow,
    ?COND(erlang:abs(Num) > erlang:abs(Rounded),
          Rounded + (sign(Num) * Pow),
          Rounded);
roundup1(Num, NumDigits) ->
    Pow = math:pow(10, NumDigits),
    Rndup = fun(X) when erlang:round(X) < X, X >= 0 ->
                    erlang:round(X) + 1;
               (X) when erlang:round(X) > X, X < 0 ->
                    erlang:round(X) - 1;
               (X) ->
                    erlang:round(X)
            end,                       
    Rndup(Num * Pow) / erlang:round(Pow).

ceiling([Num, Multiple]) ->
    ?ensure_numbers([Num, Multiple]),
    ceiling1(Num, Multiple).
ceiling1(_Num, 0) ->
    0;
ceiling1(Num, Multiple) when ?is_multiple(Num, Multiple) ->
    Num;
ceiling1(Num, Multiple) ->
    erlang:trunc(Num / Multiple) * Multiple + Multiple.

combin([0, 0]) ->
    1;
combin([N, Chosen]) ->
    ?ensure_numbers([N, Chosen]),
    ?ensure_non_negative_ex(N, ?ERR_NUM),
    ?ensure_non_negative_ex(Chosen, ?ERR_NUM),
    ?ensure(N >= Chosen, ?ERR_NUM),
    fact(N) div (fact(Chosen) * fact(N - Chosen)).

even([Num]) ->
    ?ensure_number(Num),
    even1(Num).
even1(Num) when ?is_multiple(Num, 2) ->
    Num;
even1(Num) when Num > 0 ->
    ceiling1(Num, 2);
even1(Num) when Num < 0 ->
    ceiling1(Num, -2).

floor([Num, Multiple]) ->
    ?ensure_numbers([Num, Multiple]),
    ?ensure(sign1(Num) == sign1(Multiple), ?ERR_NUM),
    floor1(Num, Multiple).
floor1(_Num, 0) ->
    0;
floor1(Num, Multiple) when ?is_multiple(Num, Multiple) ->
    Num;
floor1(Num, Multiple) ->
    erlang:trunc(Num / Multiple) * Multiple.

int([Num]) ->
    ?ensure_number(Num),
    ?COND(erlang:round(Num) > Num,
          erlang:round(Num) - 1,
          erlang:round(Num)).

%%% FIXME: Should be easy, offload to floor or ceiling depending on args.
mround([Num, Multiple]) ->
    ?ensure_numbers([Num, Multiple]),
    ?ensure(sign1(Num) == sign1(Multiple), ?ERR_NUM),
    Num.

odd([Num]) ->
    ?ensure_number(Num),
    odd1(Num).
odd1(0) ->
    1;
odd1(Num) ->
    E = even1(Num),
    ?COND(erlang:abs(E) - erlang:abs(Num) < 1,
          E + sign1(Num),
          E - sign1(Num)).

trunc([Num]) ->
    erlang:trunc([Num, 0]);
trunc([Num, NumDigits]) ->
    ?ensure_numbers([Num, NumDigits]),
    rounddown1(Num, NumDigits).


%%% --------------- %%%
%%% Special numbers %%%
%%% --------------- %%%

pi() ->
    math:pi().

sqrtpi([Num]) ->
    ?ensure_number(Num),
    ?ensure_non_negative_ex(Num, ?ERR_NUM),
    math:sqrt(Num * math:pi()).

%%% TODO: Implement. Also, remember that the result is a string.
roman([Num]) ->
    roman([Num, 0]);
roman([Num, Form]) ->
    ?ensure_numbers([Num, Form]),
    ?ensure_non_negative_ex(Num, ?ERR_VAL),
    ?ensure(Num =< 3999, ?ERR_VAL),
    ?ensure(member(Form, [1, 2, 3, 4, true, false]), ?ERR_VAL),
    roman1(Num, Form).
roman1(_Num, _Form) ->
    0.

%%% --------- %%%
%%% Summation %%%
%%% --------- %%%

subtotal([1, L])  -> stdfuns_stats:average([L]);
subtotal([2, L])  -> stdfuns_stats:count([L]);
subtotal([3, L])  -> stdfuns_stats:counta([L]);
subtotal([4, L])  -> stdfuns_stats:max([L]);
subtotal([5, L])  -> stdfuns_stats:min([L]);
subtotal([6, L])  -> product([L]);
subtotal([7, L])  -> stdfuns_stats:stdev([L]);
subtotal([8, L])  -> stdfuns_stats:stdevp([L]);
subtotal([9, L])  -> sum([L]);
subtotal([10, L]) -> stdfuns_stats:var([L]);
subtotal([11, L]) -> stdfuns_stats:varp([L]);

subtotal([100, L])  -> stdfuns_stats:average([L]);
subtotal([102, L])  -> stdfuns_stats:count([L]);
subtotal([103, L])  -> stdfuns_stats:counta([L]);
subtotal([104, L])  -> stdfuns_stats:max([L]);
subtotal([105, L])  -> stdfuns_stats:min([L]);
subtotal([106, L])  -> product([L]);
subtotal([107, L])  -> stdfuns_stats:stdev([L]);
subtotal([108, L])  -> stdfuns_stats:stdevp([L]);
subtotal([109, L])  -> sum([L]);
subtotal([110, L]) -> stdfuns_stats:var([L]);
subtotal([111, L]) -> stdfuns_stats:varp([L]);

subtotal([_, _]) -> ?ERR_VAL.

sumif([L, Crit]) ->
    sumif([L, Crit, L]);
sumif([L, Crit, L2]) ->
    F = string_funs:make(Crit),
    foldl(fun(X, {Sum, Idx}) ->
                  ?COND(F(X),
                        {Sum + cast(nth(Idx, L2), num), Idx + 1},
                        {Sum, Idx + 1})
          end,
          {0, 1}, L).

sumproduct([L]) ->
    Numlists = map(fun(Xs) ->
                           [cast(X, num) ||
                               X <- ?ensure_no_errvals(?flatten(L))]
                   end,
                   L),
    Len = length(hd(Numlists)),
    Allok = all(fun(X) -> length(X) == Len end,
                tl(Numlists)),
    ?COND(Allok, sumproduct1(Numlists), ?ERR_VAL).
sumproduct1(Numlists) ->
    foldl(fun(Xs, Acc) -> Acc + product1(tuple_to_list(Xs)) end,
          0, hslists:zipn(Numlists)).

sumsq([L]) ->
    Nums = ?filter_numbers(?ensure_no_errvals(?flatten(L))),
    sumsq1(Nums).
sumsq1(Nums) ->
    foldl(fun(X, Acc) -> Acc + X * X end, 0, Nums).
          

%%% ------------ %%%
%%% Trigonometry %%%
%%% ------------ %%%

sin([Num]) ->
    ?ensure_number(Num),
    math:sin(Num).

cos([Num]) ->
    ?ensure_number(Num),
    math:cos(Num).

tan([Num]) ->
    ?ensure_number(Num),
    math:tan(Num).

asin([Num]) ->
    ?ensure_number(Num),
    ?ensure(Num >= -1 andalso Num =< 1, ?ERR_NUM),
    math:asin(Num).

acos([Num]) ->
    ?ensure_number(Num),
    ?ensure(Num >= -1 andalso Num =< 1, ?ERR_NUM),
    math:acos(Num).

atan([Num]) ->
    ?ensure_number(Num),
    ?ensure(Num >= -1 andalso Num =< 1, ?ERR_NUM),
    math:atan(Num).

atan2([X, Y]) ->
    ?ensure_numbers([X, Y]),
    ?ensure(X =/= 0 andalso Y =/= 0, ?ERR_DIV),
    math:atan2(Y, X). %% Yep, this is correct.

sinh([Num]) ->
    ?ensure_number(Num),
    math:sinh(Num).

cosh([Num]) ->
    ?ensure_number(Num),
    math:cosh(Num).

tanh([Num]) ->
    ?ensure_number(Num),
    math:tanh(Num).

asinh([Num]) ->
    ?ensure_number(Num),
    math:asinh(Num).

acosh([Num]) ->
    ?ensure_number(Num),
    math:acosh(Num).

atanh([Num]) ->
    ?ensure_number(Num),
    ?ensure(Num >= -1 andalso Num =< 1, ?ERR_NUM),
    math:atanh(Num).

degrees([Angle]) ->
    ?ensure_number(Angle),
    Angle / math:pi() * 180.

radians([Angle]) ->
    ?ensure_number(Angle),
    Angle * math:pi() / 180.
