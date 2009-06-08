%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% @doc Built-in math functions.
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
%%% @private

-module(stdfuns_math).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").

-import(muin_util, [cast/2]).

-define(GOOGOL, 1.0E100).

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
         gcd/1,
         lcm/1,
         mod/1,

         %% Arrays and matrices
         transpose/1,
         mdeterm/1,
         munit/1,
         minverse/1,
         mmult/1,
         multinomial/1,

         %% Logarithms
         ln/1,
         log/1,
         log10/1,

         %% Random numbers
         rand/1,
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
         pi/1,
         sqrtpi/1,

         %% Summation
         seriessum/1,
         subtotal/1,
         sumif/1,
         sumx2my2/1,
         sumx2py2/1,
         sumxmy2/1,

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

%% A lot of math functions simply cast everything.
-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

%%% Operators ~~~~~
'+'([{datetime, D, T}, V2]) when is_number(V2) -> '+'([V2, {datetime, D, T}]);
'+'([V1, {datetime, D, T}]) when is_number(V1) ->
    Days = erlang:trunc(V1),
    Secs = V1 - Days,
    OldDays = calendar:date_to_gregorian_days(D),
    OldSecs = calendar:time_to_seconds(T),
    {datetime, calendar:gregorian_days_to_date(OldDays + Days),
     calendar:seconds_to_time(OldSecs + Secs)};
'+'([V1, V2]) ->
    [Num1, Num2] = ?numbers([V1, V2], ?default_rules),
    case Num1 + Num2 of
        X when X > ?GOOGOL -> ?ERR_NUM;
        Result             -> Result
    end.

%% note that there is an operation =date - number
%% but no = number - date
'-'([{datetime, D, T}, V2]) when is_number(V2) -> 
    Days = erlang:trunc(V2),
    Secs = V2 - Days,
    OldDays = calendar:date_to_gregorian_days(D),
    OldSecs = calendar:time_to_seconds(T),
    #datetime{date= calendar:gregorian_days_to_date(OldDays - Days),
              time = calendar:seconds_to_time(OldSecs - Secs)};
'-'([V1, V2]) ->
    [Num1, Num2] = ?numbers([V1, V2], ?default_rules),
    Num1 - Num2.

'*'([V1, V2]) ->
    [Num1, Num2] = ?numbers([V1, V2], ?default_rules),
    case Num1 * Num2 of
        X when X > ?GOOGOL -> ?ERR_NUM;
        Result             -> Result
    end.

'/'([V1, V2]) ->
    [Num1, Num2] = ?numbers([V1, V2], ?default_rules),
    ?ensure(Num2 =/= 0,   ?ERR_DIV),
    ?ensure(Num2 =/= 0.0, ?ERR_DIV),
    Num1/Num2.

negate([V]) ->
    -(?number(V, ?default_rules)).

%%% Arithmetic ~~~~~

sum(Vs) ->
    Flatvs = ?flatten_all(Vs),
    Nums = ?numbers([0|Flatvs], [ignore_strings, ignore_bools, ignore_dates, cast_blanks]),
    sum1(Nums).

sum1(Nums) ->
    Return=lists:sum(Nums),
    Return.

product(Vals) ->
    Flatvals = ?flatten_all(Vals),
    ?ensure_no_errvals(Flatvals),
    Nums = ?numbers(Flatvals, [cast_strings, cast_bools, ignore_blanks,
                               cast_dates]),
    product1(Nums).
product1(Nums) ->
    foldl(fun(X, Acc) -> X * Acc end,
          1, Nums).

%% @todo not an Excel 97 function - no test suite
quotient([V1, V2]) ->
    [Num, Divisor] = ?numbers([V1, V2], ?default_rules),
    ?MODULE:trunc('/'([Num, Divisor])).

abs([V]) ->
    Num = ?number(V, ?default_rules),
    erlang:abs(Num).

sqrt([V1]) ->
    Num = ?number(V1, ?default_rules),
    ?ensure(Num >= 0, ?ERR_NUM),
    math:sqrt(Num).

power([V1, V2]) ->
    ?ensure({V1, V2} =/= {blank, blank}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {false, false}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {false, blank}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {blank, false}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {false, 0}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {blank, 0}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {0, false}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {0, blank}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {0, 0}, ?ERR_NUM),
    [Num] = ?numbers([V1], ?default_rules),
    [Pow] = ?numbers([V2], ?default_rules),
    math:pow(Num, Pow).

sign([V1]) ->
    Num = ?number(V1, ?default_rules),
    sign1(Num).
sign1(0)            -> 0;
sign1(X) when X > 0 -> 1;
sign1(X) when X < 0 -> -1.

exp([V1]) ->
    Num = ?number(V1, ?default_rules),
    math:exp(Num).

%% NOTE: Artificially limited to 256 (Excel's limit is 170).
fact([V1]) ->
    Num = ?int(V1, ?default_rules),
    ?ensure(Num =< 256, ?ERR_NUM),
    ?ensure(Num >= 0, ?ERR_NUM),
    fact1(Num).
fact1(0) ->
    1;
fact1(Num) ->
    foldl(fun(X, Acc) -> X * Acc end,
          1, seq(1, Num)).

%% Keep the rource of the old one until we are sure
%% the new one works :)
%%gcd([V1, V2]) ->
%%    [A, B] = ?numbers([V1, V2], ?default_rules),
%%    gcd1(A, B).
%%gcd1(A, 0) -> A;
%%gcd1(A, B) -> gcd1(B, A rem B).

%% @todo not
gcd(V) -> [A|T] = ?numbers(V, ?default_rules),
          gcd1(A,T).

gcd1(A,[])    -> A;
gcd1(A,[0|T]) -> gcd1(A,T);
gcd1(A,[B|T]) -> A2=gcd2(A,B),
                 gcd1(A2,T).
    
gcd2(A,0) -> A;
gcd2(A,B) -> gcd2(B, A rem B).


%% Keep the rource of the old one until we are sure
%% the new one works :)    
%% lcm([V1, V2]) ->
%%    [A, B] = ?numbers([V1, V2], ?default_rules),
%%    lcm1(A, B).
%% lcm1(A, B) ->
%%    A * B / gcd1(A, B).

lcm(V) -> [A|T] = ?numbers(V, ?default_rules),
          lcm1(A,T).

lcm1(A,[])    -> A;
lcm1(A,[B|T]) -> Div=gcd2(A,B),
                 % A2 should be an integer - use round to cast it to one 
                 A2=erlang:round(A*B/Div),
                 lcm1(A2,T).

%% Returns the remainder after number is divided by divisor. The result
%% has the same sign as divisor.
mod([V1, V2]) ->
    % io:format("in stdfuns_math:mod V1 is ~p V2 is ~p~n", [V1, V2]),
    [Num, Divisor] = ?numbers([V1, V2], ?default_rules),
    ?ensure(Divisor =/= 0, ?ERR_DIV),
    ?ensure(Divisor =/= 0.0, ?ERR_DIV),    
    % io:format("in stdfuns_math:mod Num is ~p Divisor is ~p~n", [Num, Divisor]),
    Num - Divisor * int([Num/Divisor]).


%%% Arrays and matrices ~~~~~

transpose([A]) when ?is_area(A) ->
    {_, Rows} = A,
    {array, hslists:transpose(Rows)}.

mdeterm([A]) when ?is_area(A) ->
    W = area_util:width(A),
    H = area_util:height(A),
    ?ensure(W == H, ?ERR_VAL),
    {_, Rows} = ?numbers(A, [ban_strings, ban_blanks, cast_bools, ban_dates]),
    mdeterm1(Rows, W);
mdeterm([V]) ->
    mdeterm([{array, [[V]]}]).
mdeterm1(Rows, 1) ->
    [[Num]] = Rows,
    Num;
mdeterm1(Rows, 2) ->
    [[A, B], [C, D]] = Rows,
    A*D - B*C;
mdeterm1(Rows, 3) ->
    [[A, B, C], [D, E, F], [G, H, I]] = Rows,
    A*E*I - A*F*H - B*D*I + B*F*G + C*D*H - C*E*G;
mdeterm1(_Rows, _W) ->
    ?ERR_NUM.

%% @todo not Excel 97 function - no test suite
munit([V]) ->
    N = ?number(V, [cast_strings, cast_bools, ban_dates, ban_blanks]),
    Empty = area_util:make_array(N, N),
    area_util:apply_each_with_pos(fun({_, {C, C}}) -> 1;
                                     ({_, _})      -> 0
                                  end,
                                  Empty).

minverse([L]) ->
    % io:format("in minverse L is ~p~n", [L]),
    %?IF(not(is_list(L)), ?ERR_VAL),
    % io:format("Got to 1~n"),
    ?ensure_numbers(flatten(L)),
    % io:format("Got to 2~n"),
    Mx = matrix:new(L),
    % io:format("Got to 3~n"),
    ?IF(not(matrix:is_square(Mx)), ?ERR_VAL),
    % io:format("Got to 4~n"),
    ?IF(matrix:det(Mx) == 0, ?ERR_NUM),
    % io:format("Got to 5~n"),
    {matrix, _, _, NewL} = matrix:invert(Mx),
    % io:format("Got to 6~n"),
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

%% @todo not Excel 97 - no test suite
multinomial(L) ->
    Nums = ?filter_numbers_with_cast(?ensure_no_errvals(?flatten(L))),
    Allok = all(fun(X) -> X >= 1 end, Nums),
    ?COND(Allok, multinomial1(Nums), ?ERR_NUM).
multinomial1(Nums) ->
    Nom = fact([sum(Nums)]),
    Div = foldl(fun(X, Acc) ->
                        Acc * fact([X])
                end,
                1, Nums),
    Nom/Div.

%%% Logarithms ~~~~~

ln([V1]) ->
    Num = ?number(V1, ?default_rules),
    ?ensure(Num > 0, ?ERR_NUM),
    math:log(Num).

log([V1]) ->
    Num = ?number(V1, ?default_rules),
    log([Num, 10]);
log([V1, V2]) ->
    [Num, Base] = ?numbers([V1, V2], ?default_rules),
    ?ensure_positive(Num),
    ?ensure_positive(Base),
    ?IF(Base == 1, ?ERR_DIV),
    math:log(Num) / math:log(Base).

log10([V1]) ->
    Num = ?number(V1, ?default_rules),
    log([Num, 10]).

%%% Random numbers ~~~~~

rand([]) ->
    Bytes = crypto:rand_bytes(15),
    io:format("in rand with ~p~n", [Bytes]),
    rand1(Bytes, 0, -1).
rand1(<<>>, F, _) ->
    io:format("In rand1 (1) with ~p~n", [F]),
    F;
rand1(<<Byte:8, Rest/binary>>, F, Exp) ->
    D = Byte rem 10,
    F2 = F + D*math:pow(10, Exp),
    io:format("In rand1 (2) with D of ~p F2 of ~p Exp of ~p and F of~p~n", 
              [D, F2, Exp, F]),
    rand1(Rest, F2, Exp-1).


randbetween([V1, V2]) ->
    [First, Last] = ?numbers([V1, V2], ?default_rules),
    rand([]) * (Last - First) + First.

%%% Rounding numbers ~~~~~

round([V1, V2]) ->
    [Num, NumDigits] = ?numbers([V1, V2], ?default_rules),
    NumDigits2 = erlang:round(NumDigits),
    round1(Num, NumDigits2).
round1(Num, 0) ->
    erlang:round(Num);
round1(Num, NumDigits) when NumDigits < 0 ->
    Pow = math:pow(10, erlang:abs(NumDigits)),
    erlang:round(Num / Pow) * erlang:round(Pow);
round1(Num, NumDigits) ->
    Pow = math:pow(10, NumDigits),
    erlang:round(Num * Pow) / erlang:round(Pow).

rounddown([V1, V2]) ->
    [Num, NumDigits] = ?numbers([V1, V2], ?default_rules),
    NumDigits2 = erlang:round(NumDigits),
    rounddown1(Num, NumDigits2).
rounddown1(Num, 0) ->
    erlang:trunc(Num);
rounddown1(Num, NumDigits) when NumDigits < 0 ->
    Pow = math:pow(10, erlang:abs(NumDigits)),
    erlang:trunc(Num / Pow) * Pow;
rounddown1(Num, NumDigits) when NumDigits > 0 ->
    Pow = math:pow(10, NumDigits),
    erlang:trunc(Num * Pow) / Pow.

roundup([V1, V2]) ->
    [Num, NumDigits] = ?numbers([V1, V2], ?default_rules),
    NumDigits2 = erlang:round(NumDigits),
    roundup1(Num, NumDigits2).
roundup1(Num, 0) ->
    ?COND(erlang:trunc(Num) == Num,
          Num,
          erlang:trunc(Num) + sign1(Num));
roundup1(Num, NumDigits) when NumDigits < 0 ->
    Pow = math:pow(10, erlang:abs(NumDigits)),
    Rounded = erlang:trunc(erlang:trunc(Num) / Pow) * Pow,
    ?COND(erlang:abs(Num) > erlang:abs(Rounded),
          Rounded + (sign1(Num) * Pow),
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

ceiling([V1, V2]) ->
    [Num, Multiple] = ?numbers([V1, V2], ?default_rules),
    ?ensure(sign1(Num) == sign1(Multiple), ?ERR_NUM),
    ceiling1(Num, Multiple).
ceiling1(_Num, 0) ->
    0;
ceiling1(Num, Multiple) when ?is_multiple(Num, Multiple) ->
    Num;
ceiling1(Num, Multiple) ->
    erlang:trunc(Num / Multiple) * Multiple + Multiple.

combin([V1, V2]) when V1 == 0 andalso V2 == 0 ->
    1;
combin([V1, V2]) ->
    [N, Chosen] = ?ints([V1, V2], ?default_rules),
    ?ensure(N >= 0, ?ERR_NUM),
    ?ensure(Chosen >= 0, ?ERR_NUM),
    ?ensure(N >= Chosen, ?ERR_NUM),
    fact1(N) div (fact1(Chosen) * fact1(N - Chosen)).

even([V1]) ->
    Num = ?number(V1, ?default_rules),
    even1(Num).
even1(Num) when ?is_multiple(Num, 2) ->
    Num;
even1(Num) when Num > 0 ->
    ceiling1(Num, 2);
even1(Num) when Num < 0 ->
    ceiling1(Num, -2).

floor([V1, V2]) ->
    [Num, Multiple] = ?numbers([V1, V2], ?default_rules),
    ?ensure(sign1(Num) == sign1(Multiple), ?ERR_NUM),
    floor1(Num, Multiple).
floor1(_Num, 0) ->
    0;
floor1(Num, Multiple) when ?is_multiple(Num, Multiple) ->
    Num;
floor1(Num, Multiple) ->
    erlang:trunc(Num / Multiple) * Multiple.

int([V1]) ->
    Num = ?number(V1, ?default_rules),
    ?COND(erlang:round(Num) > Num,
          erlang:round(Num) - 1,
          erlang:round(Num)).

%% @todo not Excel 97 - no test suite
mround([V1, V2]) ->
    [Num, Multiple] = ?numbers([V1, V2], ?default_rules),
    ?ensure(sign1(Num) == sign1(Multiple), ?ERR_NUM),
    roundup1(Num, Multiple).

odd([V1]) ->
    Num = ?number(V1, ?default_rules),
    odd1(Num).
odd1(Num) when Num == 0 ->
    1;
odd1(Num) ->
    E = even1(Num),
    ?COND(erlang:abs(E) - erlang:abs(Num) < 1,
          E + sign1(Num),
          E - sign1(Num)).

trunc([V1]) ->
    ?int(V1, ?default_rules);
trunc([V1, V2]) ->
    [Num, NumDigits] = ?numbers([V1, V2], ?default_rules),
    rounddown1(Num, erlang:round(NumDigits)).

%%% Special numbers ~~~~~

pi([]) -> math:pi().

%% @todo not Excel 97 - no test suite
sqrtpi([V1]) ->
    Num = ?number(V1, ?default_rules),
    ?ensure(Num >= 0, ?ERR_NUM),
    math:sqrt(Num * math:pi()).

%%% Summation ~~~~~

%% @todo not Excel 97 - no test suite
seriessum([K, N, M, Coeffs]) ->
    ?ensure_numbers([K, N, M]),
    ?ensure_numbers(?ensure_no_errvals(?flatten(Coeffs))),
    Nums = ?flatten(Coeffs),
    seriessum1(K, N, M, Nums).
seriessum1(K, N, M, As) ->
    {Res, _} = foldl(fun(A, {Sum, I}) ->
                             {Sum + A * math:pow(K, N + M * I),
                              I + 1}
                     end,
                     {0, 0},
                     As),
    Res.

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
subtotal([110, L])  -> stdfuns_stats:var([L]);
subtotal([111, L])  -> stdfuns_stats:varp([L]);

subtotal([_, _]) -> ?ERR_VAL.

sumif([L, Crit]) ->
    sumif([L, Crit, L]);
sumif([V1, Crit, V2]) ->
    %% V1 and V2 must be areas of same dimensions
    ?ensure(?is_area(V1), ?ERR_VAL),
    ?ensure(?is_area(V2), ?ERR_VAL),
    ?ensure(area_util:are_congruent(V1, V2), ?ERR_VAL),
    case odf_criteria:create(Crit) of
        {error, _Reason} -> 0;
        Fun              -> sumif1(area_util:to_list(V1), area_util:to_list(V2),
                                   Fun)
    end.
sumif1(L1, L2, Fun) ->
    sumif1(L1, L2, Fun, 0).
sumif1([], [], _F, Sum) ->
    Sum;
sumif1([H1|T1], [H2|T2], Fun, Acc) ->
    case Fun(H1) of
        true  -> sumif1(T1, T2, Fun, stdfuns_math:'+'([Acc, H2]));
        false -> sumif1(T1, T2, Fun, Acc)
    end.


sumx2my2([A1, A2]) ->
    Nums1 = ?filter_numbers(?ensure_no_errvals(?flatten(A1))),
    Nums2 = ?filter_numbers(?ensure_no_errvals(?flatten(A2))),
    ?ensure(length(Nums1) == length(Nums2), ?ERR_VAL),
    sumx2my2_1(Nums1, Nums2).
sumx2my2_1(Nums1, Nums2) ->
    sum(map(fun({X, Y}) ->
                    (X * X) - (Y * Y)
            end,
            zip(Nums1, Nums2))).

sumx2py2([A1, A2]) ->
    Nums1 = ?filter_numbers(?ensure_no_errvals(?flatten(A1))),
    Nums2 = ?filter_numbers(?ensure_no_errvals(?flatten(A2))),
    ?ensure(length(Nums1) == length(Nums2), ?ERR_VAL),
    sumx2py2_1(Nums1, Nums2).
sumx2py2_1(Nums1, Nums2) ->
    sum(map(fun({X, Y}) ->
                    (X * X) + (Y * Y)
            end,
            zip(Nums1, Nums2))).

sumxmy2([A1, A2]) ->
    Nums1 = ?filter_numbers(?ensure_no_errvals(?flatten(A1))),
    Nums2 = ?filter_numbers(?ensure_no_errvals(?flatten(A2))),
    ?ensure(length(Nums1) == length(Nums2), ?ERR_VAL),
    sumxmy2_1(Nums1, Nums2).
sumxmy2_1(Nums1, Nums2) ->
    sum(map(fun({X, Y}) ->
                    math:pow(X - Y, 2)
            end,
            zip(Nums1, Nums2))).

%%% Trigonometry ~~~~~

sin([V]) ->
    Num = ?number(V, ?default_rules),
    math:sin(Num).

cos([V]) ->
    Num = ?number(V, ?default_rules),
    math:cos(Num).

tan([V]) ->
    Num = ?number(V, ?default_rules),
    math:tan(Num).

asin([V]) ->
    Num = ?number(V, ?default_rules),
    ?ensure(Num >= -1 andalso Num =< 1, ?ERR_NUM),
    math:asin(Num).

acos([V]) ->
    Num = ?number(V, ?default_rules),
    ?ensure(Num >= -1 andalso Num =< 1, ?ERR_NUM),
    math:acos(Num).

atan([V]) ->
    Num = ?number(V, ?default_rules),
    math:atan(Num).

atan2([V1, V2]) ->
    X = ?number(V1, ?default_rules),
    Y = ?number(V2, ?default_rules),

    if X == 0 andalso Y == 0 -> ?ERR_DIV;
       true -> ok
    end,

    math:atan2(Y, X). % Yep, the order of args is reversed.

sinh([V]) ->
    Num = ?number(V, ?default_rules),
    math:sinh(Num).

cosh([V]) ->
    Num = ?number(V, ?default_rules),
    ?ensure(Num < 711, ?ERR_NUM),
    math:cosh(Num).

tanh([V]) ->
    Num = ?number(V, ?default_rules),
    math:tanh(Num).

asinh([V]) ->
    Num = ?number(V, ?default_rules),
    math:asinh(Num).

acosh([V]) ->
    Num = ?number(V, ?default_rules),
    ?ensure(Num >= 1, ?ERR_NUM),
    math:acosh(Num).

atanh([V]) ->
    Num = ?number(V, ?default_rules),
    ?ensure(Num > -1 andalso Num < 1, ?ERR_NUM),
    math:atanh(Num).

degrees([V]) ->
    Angle = ?number(V, ?default_rules),
    Angle / math:pi() * 180.

radians([V]) ->
    Angle = ?number(V, ?default_rules),
    Angle * math:pi() / 180.
