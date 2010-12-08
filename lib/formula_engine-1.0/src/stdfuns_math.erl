%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% @doc Built-in math functions.
%%% IMPORTANT NOTES:
%%% ================
%%%
%%% INCOMPATIBILITIES
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

-include("typechecks.hrl").
-include("muin_records.hrl").

-define(GOOGOL, 1.0E100).

-export([
%% Basics
         '+'/1,
         '-'/1,
         '*'/1,
         '/'/1,
         '^^'/1,
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
         roman/1,

%% Summation
         seriessum/1,
         subtotal/1,
         sumif/1,
         sumx2my2/1,
         sumx2py2/1,
         sumxmy2/1,
         sumproduct/1,
         sumsq/1,

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
         radians/1,

         % Easter Egg
         thelifeuniverseandeverything/1
        ]).

-define(is_multiple(Num, Mult),
        (erlang:trunc(Num / Mult) * Mult) == (Num * 1.0)).

%% A lot of math functions simply cast everything.
-define(default_rules, [first_array, cast_strings, cast_bools, cast_blanks, cast_dates]).

is_num_or_date(X) ->
    is_number(X) orelse ?is_date(X).

%%% Operators ~~~~~
'+'([V1, V2]) ->
    muin:col([V1, V2],
             [eval_funs, fetch, area_first, {cast, str, num, ?ERRVAL_VAL},
              {cast, bool, num}, {cast, blank, num}],
             [return_errors, {all, fun is_num_or_date/1}],
             fun '+_'/1).

'+_'([DT, V2]) when is_record(DT, datetime) ->
    '+_'([V2, DT]);
'+_'([V1, #datetime{date=Date, time=Time}]) when is_number(V1) ->
    NewTime = calendar:datetime_to_gregorian_seconds({Date, Time})
        + erlang:round(V1 * 86400),
    {NDate, NTime} = calendar:gregorian_seconds_to_datetime(NewTime),
    #datetime{date=NDate, time=NTime};

'+_'([Num1, Num2]) ->
    case Num1 + Num2 of
        X when X > ?GOOGOL -> ?ERRVAL_NUM;
        Result             -> Result
    end.

%% note that there is an operation =date - number
%% but no = number - date
'-'([DT, V2]) when is_record(DT, datetime), is_number(V2) -> 
    '+_'([-V2, DT]);

'-'([#datetime{date=D1, time=T1}, #datetime{date=D2, time=T2}]) ->
    S1 = calendar:datetime_to_gregorian_seconds({D1, T1}),
    S2 = calendar:datetime_to_gregorian_seconds({D2, T2}),
    {DiffDate, _} = calendar:gregorian_seconds_to_datetime(S1 - S2),
    calendar:date_to_gregorian_days(DiffDate);

'-'([V1, V2]) ->
    case muin:col([V1, V2],
                  [eval_funs, fetch, area_first, {cast, str, num, ?ERRVAL_VAL},
                   {cast, bool, num}, {cast, blank, num}],
                  [return_errors, {all, fun is_number/1}]) of
        [Num1, Num2] -> Num1 - Num2;
        Other        -> Other
    end.

'*'([V1, V2]) ->
    case muin:col([V1, V2],
                  [eval_funs, fetch, area_first, {cast, str, num, ?ERRVAL_VAL},
                   {cast, bool, num}, {cast, blank, num}],
                  [return_errors, {all, fun is_number/1}]) of
        [Num1, Num2] ->
            case Num1 * Num2 of
                X when X > ?GOOGOL -> ?ERRVAL_NUM;
                Result             -> Result
            end;
        Other -> Other
    end.

'/'([V1, V2]) ->
    case muin:col([V1, V2],
                  [eval_funs, fetch, area_first, {cast, str, num, ?ERRVAL_VAL},
                   {cast, bool, num}, {cast, blank, num}],
                  [return_errors, {all, fun is_number/1}]) of
        [_Num1, X] when X == 0 -> ?ERRVAL_DIV;
        [Num1, Num2]           -> Num1 / Num2;
        Other                  -> Other
    end.


%%-define(default_rules, [first_array, cast_strings, cast_bools, cast_blanks, cast_dates]).


'^^'([_, _]=Args) ->
    muin:col(Args, [eval_funs],
             [return_errors, {all, fun(X) -> ?is_rangeref(X) end}],
             fun '^^_'/1).

'^^_'([V1, V2]) ->

    #rangeref{ tl = {{offset,AX1},{offset,AY1}},
               br = {{offset,AX2},{offset,AY2}} } = V1,

    #rangeref{ tl = {{offset,BX1},{offset,BY1}},
               br = {{offset,BX2},{offset,BY2}} } = V2,

    case intersect({{AX1,AY1}, {AX2,AY2}},
                   {{BX1,BY1}, {BX2,BY2}}) of

        {error, no_intersect} ->
            ?ERRVAL_NULL;

        {{X1, Y1}, {X2, Y2}} ->
            Path = hn_util:list_to_path(muin:context_setting(path)),
            #rangeref{ tl     = {{offset, X1},{offset, Y1}},
                       br     = {{offset, X2},{offset, Y2}},
                       path   = Path, type=finite, text="",
                       width  = (X2-X1)+1,
                       height = (Y1-Y1)+1 }
    end.

negate([V]) ->
    muin:col([V],
             [first_array, cast_num],
             [return_errors, {all, fun is_number/1}],
             fun([X]) -> -X end).

%%% Arithmetic ~~~~~

sum(Vs) ->
    io:format("in sum with args of ~p~n", [Vs]),
    muin:col(Vs, [eval_funs, {cast, str, num, ?ERRVAL_VAL},
                  {cast, bool, num}, fetch, flatten,
                  {ignore, blank}, {ignore, str}, {ignore, bool}],
             [return_errors, {all, fun is_number/1}],
             fun sum1/1).

sum1(Nums) ->
    lists:sum(Nums).

product(Vals) ->    
    muin:col(Vals,
             [eval_funs, {cast, str, num, ?ERRVAL_VAL}, {cast, bool, num},
              fetch, flatten, area_first, {ignore, blank}, {ignore, str},
              {ignore, bool}],
             [return_errors, {all, fun is_number/1}],
             fun product1/1).

product1(Nums) ->
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, Nums).

%% @todo not an Excel 97 function - no test suite
quotient([V1, V2]) ->
    [Num, Divisor] = ?numbers([V1, V2], ?default_rules),
    ?MODULE:trunc('/'([Num, Divisor])).

abs([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun abs_/1).

abs_([Num]) ->
    erlang:abs(Num).

sqrt([V1]) ->
    Num = ?number(V1, ?default_rules),
    ?ensure(Num >= 0, ?ERR_NUM),
    math:sqrt(Num).

power([TV1, TV2]) ->
    V1 = case (TV1 == 0.0) of
             true  -> 0;
             false -> TV1
         end,
    V2 = case (TV2 == 0.0) of
             true  -> 0;
             false -> TV2
         end,
    [Num] = ?numbers([V1], ?default_rules),
    [Pow] = ?numbers([V2], ?default_rules),
    ?ensure({V1, V2} =/= {blank, blank}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {false, false}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {false, blank}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {blank, false}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {false, 0}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {blank, 0}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {0, false}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {0, blank}, ?ERR_NUM),
    ?ensure({V1, V2} =/= {0, 0}, ?ERR_NUM),

    % Dont throw formula errors when numbers are too large
    try   math:pow(Num, Pow)
    catch error:_Err -> ?ERR_NUM end.

sign([V1]) ->
    Num = ?number(V1, ?default_rules),
    sign1(Num).

sign1(0)            -> 0;
sign1(X) when X > 0 -> 1;
sign1(X) when X < 0 -> -1.

exp([V1]) ->
    Num = ?number(V1, ?default_rules),
    math:exp(Num).

fact([V1]) ->
    Num = ?int(V1, ?default_rules),
    ?ensure(Num =< 170, ?ERR_NUM),
    ?ensure(Num >= 0, ?ERR_NUM),
    fact1(Num).
fact1(0) ->
    1;
fact1(Num) ->
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, lists:seq(1, Num)).

%% @todo not
gcd(V) ->
    [A|T] = ?numbers(V, ?default_rules),
    gcd1(A,T).

gcd1(A,[])    -> A;
gcd1(A,[0|T]) -> gcd1(A,T);
gcd1(A,[B|T]) -> A2=gcd2(A,B),
                 gcd1(A2,T).

gcd2(A,0) -> A;
gcd2(A,B) -> gcd2(B, A rem B).


lcm(V) ->
    [A|T] = ?numbers(V, ?default_rules),
    lcm1(A,T).

lcm1(A,[])    ->
    A;
lcm1(A,[B|T]) ->
    Div=gcd2(A,B),
    % A2 should be an integer - use round to cast it to one 
    A2=erlang:round(A*B/Div),
    lcm1(A2,T).

%% Returns the remainder after number is divided by divisor. The result
%% has the same sign as divisor.
mod([V1, V2]) ->
    [Num, Divisor] = ?numbers([V1, V2], ?default_rules),
    ?ensure(Divisor =/= 0, ?ERR_DIV),
    ?ensure(Divisor =/= 0.0, ?ERR_DIV),    
    Num - Divisor * int([Num/Divisor]).

%%% Arrays and matrices ~~~~~

transpose([A]) when ?is_area(A) ->
    {_, Rows} = A,
    {array, hslists:transpose(Rows)}.

mdeterm([A]) when is_number(A) ->
    A;
mdeterm([A]) ->
    muin:col([A], [eval_funs, fetch, {ignore, bool}, {ignore, str}, {ignore, blank}],
             [return_errors],
             fun mdeterm_/1).

mdeterm_([]) ->
    ?ERRVAL_VAL;
mdeterm_([{_Type, Rows}=Area]) when ?is_area(Area) -> 
    case area_util:is_matrix(Area) of
        false -> ?ERRVAL_VAL;
        true  -> mdeterm1(Rows, area_util:width(Area))
    end.

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

minverse(Args) ->
    muin:col(Args, [eval_funs, fetch, {ignore, bool}, {ignore, str},
                    {ignore, blank}], [return_errors],
             fun minverse_/1).
minverse_([{_Type, _Rows}=Area]) when ?is_area(Area) -> 
    case area_util:is_matrix(Area) of
        false -> ?ERRVAL_VAL;
        true  -> 0
    end;
minverse_([A]) when is_number(A) ->
    minverse_([{array, [[A]]}]);
minverse_([]) ->
    ?ERRVAL_VAL.

mmult(Args) ->
    muin:col(Args, [eval_funs, fetch, {ignore, bool}, {ignore, str},
                    {ignore, blank}], [return_errors],
             fun mmult_/1).

mmult_([{_, R1}=L1, {_, R2}=L2]) when ?is_area(L1), ?is_area(L2) -> 
    case area_util:is_matrix(L1) andalso area_util:is_matrix(L2) of
        false -> ?ERRVAL_VAL;
        true  -> {array, dh_matrix:multiply(R1, R2)}
    end;
mmult_([A,B]) when is_number(A), is_number(B) ->
    mmult_([{array, [[A]]}, {array, [[B]]}]);
mmult_(_) ->
    ?ERRVAL_VAL.

%% @todo not Excel 97 - no test suite
multinomial(L) ->
    Nums = ?filter_numbers_with_cast(?ensure_no_errvals(?flatten(L))),
    Allok = lists:all(fun(X) -> X >= 1 end, Nums),
    case Allok of
        true  -> multinomial1(Nums);
        false -> ?ERR_NUM
    end.
multinomial1(Nums) ->
    Nom = fact([sum(Nums)]),
    Div = lists:foldl(fun(X, Acc) ->
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
    case (Base == 1) of
        true  -> ?ERR_DIV;
        false -> nothing
    end,
    math:log(Num) / math:log(Base).

log10([V1]) ->
    Num = ?number(V1, ?default_rules),
    log([Num, 10]).

%%% Random numbers ~~~~~

rand([]) ->
    Bytes = crypto:rand_bytes(15),
    rand1(Bytes, 0, -1).
rand1(<<>>, F, _) ->
    F;
rand1(<<Byte:8, Rest/binary>>, F, Exp) ->
    D = Byte rem 10,
    F2 = F + D*math:pow(10, Exp),
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
    case (erlang:trunc(Num) == Num) of
        true  -> Num;
        false -> erlang:trunc(Num) + sign1(Num)
    end;
roundup1(Num, NumDigits) when NumDigits < 0 ->
    Pow = math:pow(10, erlang:abs(NumDigits)),
    Rounded = erlang:trunc(erlang:trunc(Num) / Pow) * Pow,
    case (erlang:abs(Num) > erlang:abs(Rounded)) of
        true  -> Rounded + (sign1(Num) * Pow);
        false -> Rounded
    end;
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

ceiling([_,_]=Args) ->
    muin:col(Args, [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun ceiling_/1).

ceiling_([Num, Multiple]) ->
    case (sign1(Num) == sign1(Multiple)
          orelse Num =:= 0
          orelse Multiple =:= 0) of
        true  -> ceiling1(Num, Multiple);
        false -> ?ERRVAL_NUM
    end.
ceiling1(_Num, 0) ->
    0;
ceiling1(Num, Multiple) when ?is_multiple(Num, Multiple) ->
    Num;
ceiling1(Num, Multiple) ->
    erlang:trunc(Num / Multiple) * Multiple + Multiple.

combin([V1, V2]) when V1 == 0 andalso V2 == 0 ->
    1;
combin([_, _]=Args) ->
    muin:col(Args, [eval_funs, area_first, fetch, {cast, int}],
             [return_errors, {all, fun is_integer/1}],
             fun combin_/1).
combin_([N, Chosen]) when N < 0 orelse Chosen < 0 orelse N < Chosen ->
    ?ERRVAL_NUM;
combin_([N, Chosen]) ->
    fact1(N) div (fact1(Chosen) * fact1(N - Chosen)).

even([V1]) ->
    muin:col([V1], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun even1/1).

even1([Num]) when ?is_multiple(Num, 2) ->
    Num;
even1([Num]) when Num > 0 ->
    ceiling1(Num, 2);
even1([Num]) when Num < 0 ->
    ceiling1(Num, -2).

floor([V1, V2]) ->
    [Num, Multiple] = ?numbers([V1, V2], ?default_rules),
    ?ensure(sign1(Num) == sign1(Multiple)
            orelse Num == 0 orelse Multiple == 0, ?ERR_NUM),
    floor1(Num, Multiple).

floor1(_Num, 0) ->
    0;
floor1(Num, Multiple) when ?is_multiple(Num, Multiple) ->
    Num;
floor1(Num, Multiple) ->
    erlang:trunc(Num / Multiple) * Multiple.

int([V1]) ->
    muin:col([V1], [eval_funs, area_first, fetchdb, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun int_/1).

int_([Num]) ->
    case (erlang:round(Num) > Num) of
        true  -> erlang:round(Num) - 1;
        false -> erlang:round(Num)
    end.

%% @todo not Excel 97 - no test suite
mround([V1, V2]) ->
    [Num, Multiple] = ?numbers([V1, V2], ?default_rules),
    ?ensure(sign1(Num) == sign1(Multiple), ?ERR_NUM),
    roundup1(Num, Multiple).

odd([V1]) ->
    muin:col([V1], [eval_funs, area_first, fetchdb, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun odd1/1).

odd1([Num]) when Num == 0 ->
    1;
odd1([Num]) ->
    E = even1([Num]),
    case (erlang:abs(E) - erlang:abs(Num) < 1) of
        true  -> E + sign1(Num);
        false -> E - sign1(Num)
    end.

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


%% Constants for roman - types of return
-define(CLASSIC,0).
-define(CONCISE1,1).
-define(CONCISE2,2).
-define(CONCISE3,3).
-define(SIMPLIFIED,4).

roman([X])        ->
    roman([X, 0]);
roman([X, true])  ->
    roman([X, ?CLASSIC]);
roman([X, false]) ->
    roman([X, ?CONCISE3]);
roman([V1, V2]) ->
    X = ?int(V1, [cast_strings, cast_bools, ban_dates, cast_blanks]),
    Type = ?int(V2, [cast_strings, cast_bools, ban_dates, cast_blanks]),
%% we need to build the roman numbers right to left so we have to
%% reverse the string representation of the number
    case X of
        X when X < 0 orelse X > 3999 -> ?ERRVAL_VAL;
        _Else ->
            List = lists:map(fun(C) -> [C] end, integer_to_list(X)),
            get_roman(List, Type)
    end.

%% first deal with the single digit number
get_roman(["0"],_) -> "";
get_roman(["1"],_) -> "I";
get_roman(["2"],_) -> "II";
get_roman(["3"],_) -> "III";
get_roman(["4"],_) -> "IV";
get_roman(["5"],_) -> "V";
get_roman(["6"],_) -> "VI";
get_roman(["7"],_) -> "VII";
get_roman(["8"],_) -> "VIII";
get_roman(["9"],_) -> "IX";

%% Now deal with the 2 digit numbers
%% 45 to 49 and 95 to 99 are the problems here

%% First classic
get_roman(["4","5"],?CLASSIC) -> "XLV";
get_roman(["4","6"],?CLASSIC) -> "XLVI";
get_roman(["4","7"],?CLASSIC) -> "XLVII";
get_roman(["4","8"],?CLASSIC) -> "XLVIII";
get_roman(["4","9"],?CLASSIC) -> "XLIX";

get_roman(["9","5"],?CLASSIC) -> "XCV";
get_roman(["9","6"],?CLASSIC) -> "XCVI";
get_roman(["9","7"],?CLASSIC) -> "XCVII";
get_roman(["9","8"],?CLASSIC) -> "XCVIII";
get_roman(["9","9"],?CLASSIC) -> "XCIX";

%% Then the rest...
get_roman(["4","5"],_) -> "VL";
get_roman(["4","6"],_) -> "VLI";
get_roman(["4","7"],_) -> "VLII";
get_roman(["4","8"],_) -> "VLIII";
%% special for 49
get_roman(["4","9"],?CONCISE1) -> "VLIV";
get_roman(["4","9"],_)         -> "IL";

get_roman(["9","5"],_) -> "VC";
get_roman(["9","6"],_) -> "VCI";
get_roman(["9","7"],_) -> "VCII";
get_roman(["9","8"],_) -> "VCIII";
%% special for 99
get_roman(["9","9"],?CONCISE1) -> "VCIV";
get_roman(["9","9"],_)         -> "IC";

%% Now build the rest of the 2 digits
get_roman([Second,First],_) -> get_roman2([Second])++get_roman([First],?CLASSIC);

%% Now build the three digits

%% the issue is the digits representing 440-499 and 940-999
%% deal with 445 to 449 first
%% First classic
get_roman(["4","4","5"],?CLASSIC) -> "CDXLV";
get_roman(["4","4","6"],?CLASSIC) -> "CDXLVI";
get_roman(["4","4","7"],?CLASSIC) -> "CDXLVII";
get_roman(["4","4","8"],?CLASSIC) -> "CDXLVIII";
get_roman(["4","4","9"],?CLASSIC) -> "CDXLIX";

get_roman(["4","4","5"],_) -> "CDVL";
get_roman(["4","4","6"],_) -> "CDVLI";
get_roman(["4","4","7"],_) -> "CDVLII";
get_roman(["4","4","8"],_) -> "CDVLIII";
%% special for 449
get_roman(["4","4","9"],?CONCISE1) -> "CDVLIV";
get_roman(["4","4","9"],_)         -> "CDIL";
%% now the 450's, 460's, 470's and 480's
get_roman(["4","5",First],?CLASSIC) -> "CDL"   ++get_roman([First],?CLASSIC);
get_roman(["4","6",First],?CLASSIC) -> "CDLX"  ++get_roman([First],?CLASSIC);
get_roman(["4","7",First],?CLASSIC) -> "CDLXX" ++get_roman([First],?CLASSIC);
get_roman(["4","8",First],?CLASSIC) -> "CDLXXX" ++get_roman([First],?CLASSIC);

get_roman(["4","5",First],_) -> "LD"    ++get_roman([First],?CLASSIC);
get_roman(["4","6",First],_) -> "LDX"   ++get_roman([First],?CLASSIC);
get_roman(["4","7",First],_) -> "LDXX"  ++get_roman([First],?CLASSIC);
get_roman(["4","8",First],_) -> "LDXXX" ++get_roman([First],?CLASSIC);
%% now the 490'syes
%% Classic and Concise2 are both straightforward
get_roman(["4","9",First],?CLASSIC)  -> "CDXC" ++get_roman([First],?CLASSIC);
get_roman(["4","9",First],?CONCISE2) -> "XD" ++get_roman([First],?CONCISE2);
% Now do 495, 496, 497, 498 and 499 for Concise1, Concise2 and Simple
get_roman(["4","9","5"],?CONCISE1) -> "LDVL";
get_roman(["4","9","6"],?CONCISE1) -> "LDVLI";
get_roman(["4","9","7"],?CONCISE1) -> "LDVLII";
get_roman(["4","9","8"],?CONCISE1) -> "LDVLIII";
get_roman(["4","9","9"],?CONCISE1) -> "LDVLIV";

get_roman(["4","9","5"],?CONCISE3) -> "VD";
get_roman(["4","9","6"],?CONCISE3) -> "VDI";
get_roman(["4","9","7"],?CONCISE3) -> "VDII";
get_roman(["4","9","8"],?CONCISE3) -> "VDIII";
get_roman(["4","9","9"],?CONCISE3) -> "VDIV";

get_roman(["4","9","5"],?SIMPLIFIED) -> "VD";
get_roman(["4","9","6"],?SIMPLIFIED) -> "VDI";
get_roman(["4","9","7"],?SIMPLIFIED) -> "VDII";
get_roman(["4","9","8"],?SIMPLIFIED) -> "VDIII";
get_roman(["4","9","9"],?SIMPLIFIED) -> "ID";
%% Now do 490, 491, 492, 493 and 494 for all the types
get_roman(["4","9",First],?CLASSIC)  -> "CDXC" ++get_roman([First],?CLASSIC);
get_roman(["4","9",First],?CONCISE1) -> "LDXL" ++get_roman([First],?CONCISE1);
get_roman(["4","9",First],Type)      -> "XD" ++get_roman([First],Type);

%% Now deal with 945 to 999 first
%% First 945 to 949
%% First classic
get_roman(["9","4","5"],?CLASSIC) -> "CMXLV";
get_roman(["9","4","6"],?CLASSIC) -> "CMXLVI";
get_roman(["9","4","7"],?CLASSIC) -> "CMXLVII";
get_roman(["9","4","8"],?CLASSIC) -> "CMXLVIII";
get_roman(["9","4","9"],?CLASSIC) -> "CMXLIX";

get_roman(["9","4","5"],_) -> "CMVL";
get_roman(["9","4","6"],_) -> "CMVLI";
get_roman(["9","4","7"],_) -> "CMVLII";
get_roman(["9","4","8"],_) -> "CMVLIII";
%% special for 949
get_roman(["9","4","9"],?CONCISE1) -> "CMVLIV";
get_roman(["9","4","9"],_)         -> "CMIL";
%% now the 950's, 960's, 970's and 980's
get_roman(["9","5",First],?CLASSIC) -> "CML"   ++get_roman([First],?CLASSIC);
get_roman(["9","6",First],?CLASSIC) -> "CMLX"  ++get_roman([First],?CLASSIC);
get_roman(["9","7",First],?CLASSIC) -> "CMLXX" ++get_roman([First],?CLASSIC);
get_roman(["9","8",First],?CLASSIC) -> "CMLXXX" ++get_roman([First],?CLASSIC);

get_roman(["9","5",First],_) -> "LM"    ++get_roman([First],?CLASSIC);
get_roman(["9","6",First],_) -> "LMX"   ++get_roman([First],?CLASSIC);
get_roman(["9","7",First],_) -> "LMXX"  ++get_roman([First],?CLASSIC);
get_roman(["9","8",First],_) -> "LMXXX" ++get_roman([First],?CLASSIC);
%% now the 990's
%% Classic and Concise2 are both straightforward
get_roman(["9","9",First],?CLASSIC)  -> "CMXC" ++get_roman([First],?CLASSIC);
get_roman(["9","9",First],?CONCISE2) -> "XM"   ++get_roman([First],?CONCISE2);
% Now do 995, 996, 997, 998 and 999 for Concise1, Concise2 and Simple
get_roman(["9","9","5"],?CONCISE1) -> "LMVL";
get_roman(["9","9","6"],?CONCISE1) -> "LMVLI";
get_roman(["9","9","7"],?CONCISE1) -> "LMVLII";
get_roman(["9","9","8"],?CONCISE1) -> "LMVLIII";
get_roman(["9","9","9"],?CONCISE1) -> "LMVLIV";

get_roman(["9","9","5"],_) -> "VM";
get_roman(["9","9","6"],_) -> "VMI";
get_roman(["9","9","7"],_) -> "VMII";
get_roman(["9","9","8"],_) -> "VMIII";
%% special for 999
get_roman(["9","9","9"],?CONCISE3) -> "VMIV";
get_roman(["9","9","9"],_)         -> "IM";
%% Now do 990, 991, 992, 993 and 994 for all the types
get_roman(["9","9",First],?CLASSIC)  -> "CMXC" ++get_roman([First],?CLASSIC);
get_roman(["9","9",First],?CONCISE1) -> "LMXL" ++get_roman([First],?CONCISE1);
get_roman(["9","9",First],Type)      -> "XM"   ++get_roman([First],Type);

%% Now do all the other 3 digit numbers
get_roman([Third,Second,First],Type) -> get_roman3([Third])
                                            ++get_roman([Second,First|[]],Type);
get_roman([Fourth|Rest],Type)        -> get_roman4([Fourth])++get_roman(Rest,Type).

get_roman2(["0"]) -> "";
get_roman2(["1"]) -> "X";
get_roman2(["2"]) -> "XX";
get_roman2(["3"]) -> "XXX";
get_roman2(["4"]) -> "XL";
get_roman2(["5"]) -> "L";
get_roman2(["6"]) -> "LX";
get_roman2(["7"]) -> "LXX";
get_roman2(["8"]) -> "LXXX";
get_roman2(["9"]) -> "XC".

get_roman3(["0"]) -> "";
get_roman3(["1"]) -> "C";
get_roman3(["2"]) -> "CC";
get_roman3(["3"]) -> "CCC";
get_roman3(["4"]) -> "CD";
get_roman3(["5"]) -> "D";
get_roman3(["6"]) -> "DC";
get_roman3(["7"]) -> "DCC";
get_roman3(["8"]) -> "DCC";
get_roman3(["9"]) -> "CM".

get_roman4(["1"]) -> "M";
get_roman4(["2"]) -> "MM";
get_roman4(["3"]) -> "MMM".


%%% Summation ~~~~~

sumsq(Vs) ->
    muin:col(Vs,
             [eval_funs, {cast, str, num, ?ERRVAL_VAL},
              {cast, bool, num}, fetch, flatten,
              {ignore, blank}, {ignore, str}, {ignore, bool}],
             [return_errors, {all, fun is_number/1}],
             fun sumsq_/1).

sumsq_(Nums) ->
    sum([X * X || X <- Nums]).

sumproduct(Arrs) ->
    NArrs = [muin:col([X], [eval_funs, fetch, {convflat, str, ?ERRVAL_VAL},
                            {convflat, bool, ?ERRVAL_VAL}, flatten,
                            {conv, bool, 0}, {conv, blank, 0}, {conv, str, 0}],
                      [return_errors, {all, fun is_number/1}]) || X <- Arrs],    
    muin_util:run(NArrs, fun sumproduct_/1).

sumproduct_(Arrs) ->
    lists:sum( [ product1(X) || X <- hslists:transpose(Arrs) ]).

%% @todo not Excel 97 - no test suite
seriessum([K, N, M, Coeffs]) ->
    ?ensure_numbers([K, N, M]),
    ?ensure_numbers(?ensure_no_errvals(?flatten(Coeffs))),
    Nums = ?flatten(Coeffs),
    seriessum1(K, N, M, Nums).
seriessum1(K, N, M, As) ->
    {Res, _} = lists:foldl(fun(A, {Sum, I}) ->
                                   {Sum + A * math:pow(K, N + M * I),
                                    I + 1}
                           end,
                           {0, 0},
                           As),
    Res.

%% TODO, needs do not strictly evaluate args, need
%% to change all the callees to support refs first
subtotal([Index, Arr]) ->
    Ind = muin:col([Index], [eval_funs, fetch, {cast, int}],
                   [return_errors, {all, fun is_integer/1}]),
    muin_util:apply([Ind, Arr], fun subtotal_/2).    

subtotal_([1], L)  -> stdfuns_stats:average([L]);
subtotal_([2], L)  -> stdfuns_stats:count([L]);
subtotal_([3], L)  -> stdfuns_stats:counta([L]);
subtotal_([4], L)  -> stdfuns_stats:max([L]);
subtotal_([5], L)  -> stdfuns_stats:min([L]);
subtotal_([6], L)  -> product([L]);
subtotal_([7], L)  -> stdfuns_stats:stdev([L]);
subtotal_([8], L)  -> stdfuns_stats:stdevp([L]);
subtotal_([9], L)  -> sum([L]);
subtotal_([10], L) -> stdfuns_stats:var([L]);
subtotal_([11], L) -> stdfuns_stats:varp([L]);
subtotal_([101], L)  -> stdfuns_stats:average([L]);
subtotal_([102], L)  -> stdfuns_stats:count([L]);
subtotal_([103], L)  -> stdfuns_stats:counta([L]);
subtotal_([104], L)  -> stdfuns_stats:max([L]);
subtotal_([105], L)  -> stdfuns_stats:min([L]);
subtotal_([106], L)  -> product([L]);
subtotal_([107], L)  -> stdfuns_stats:stdev([L]);
subtotal_([108], L)  -> stdfuns_stats:stdevp([L]);
subtotal_([109], L)  -> sum([L]);
subtotal_([110], L)  -> stdfuns_stats:var([L]);
subtotal_([111], L)  -> stdfuns_stats:varp([L]);

subtotal_(_, _) -> ?ERRVAL_VAL.

sumif([L, Crit]) ->
    sumif([L, Crit, L]);
sumif([V1, Cr, V2]) ->

    Tmp = [eval_funs, fetch, flatten],
    Val1   = muin:col([V1], Tmp),
    Val2   = muin:col([V2], Tmp),
    [Crit] = muin:col([Cr], [eval_funs, fetch, flatten]),

    case length(Val1) == length(Val2) of
        false ->
            ?ERRVAL_VAL;
        true ->
            case odf_criteria:create(Crit) of
                {error, _Reason} -> 0;
                Fun              -> sumif1(Val1, Val2, Fun, 0)
            end
    end.

sumif1([], [], _F, Sum) ->
    Sum;
sumif1([H1|T1], [H2|T2], Fun, Acc) ->
    case Fun(H1) of
        true  ->
            [Val] = muin:col([H2], [{cast, num}, {conv, str, 0}]),
            sumif1(T1, T2, Fun, stdfuns_math:'+'([Acc, Val]));
        false -> sumif1(T1, T2, Fun, Acc)
    end.

sumx2my2([A1, A2]) ->
    Nums1 = muin:col([A1], [eval_funs, fetch, flatten, {ignore, blank}],
                     [return_errors, {all, fun is_number/1}]),
    Nums2 = muin:col([A2], [eval_funs, fetch, flatten, {ignore, blank}],
                     [return_errors, {all, fun is_number/1}]),
    muin_util:apply([Nums1, Nums2], fun sumx2my2_/2).
sumx2my2_(Nums1, Nums2) when Nums1 == []; Nums2 == [] ->
    ?ERRVAL_VAL;
sumx2my2_(Nums1, Nums2) when length(Nums1) =/= length(Nums2) ->
    ?ERRVAL_NA;
sumx2my2_(Nums1, Nums2) ->
    sum(lists:map(fun({X, Y}) ->
                          (X * X) - (Y * Y)
                  end,
                  lists:zip(Nums1, Nums2))).

sumx2py2([A1, A2]) ->
    Nums1 = muin:col([A1], [eval_funs, fetch, flatten, {ignore, blank}],
                     [return_errors, {all, fun is_number/1}]),
    Nums2 = muin:col([A2], [eval_funs, fetch, flatten, {ignore, blank}],
                     [return_errors, {all, fun is_number/1}]),
    muin_util:apply([Nums1, Nums2], fun sumx2py2_/2).

sumx2py2_(Nums1, Nums2) when Nums1 == []; Nums2 == [] ->
    ?ERRVAL_VAL;
sumx2py2_(Nums1, Nums2) when length(Nums1) =/= length(Nums2) ->
    ?ERRVAL_NA;
sumx2py2_(Nums1, Nums2) ->
    sum(lists:map(fun({X, Y}) ->
                          (X * X) + (Y * Y)
                  end,
                  lists:zip(Nums1, Nums2))).

sumxmy2([A1, A2]) ->
    Nums1 = muin:col([A1], [eval_funs, fetch, flatten, {ignore, blank}],
                     [return_errors, {all, fun is_number/1}]),
    Nums2 = muin:col([A2], [eval_funs, fetch, flatten, {ignore, blank}],
                     [return_errors, {all, fun is_number/1}]),
    muin_util:apply([Nums1, Nums2], fun sumxmy2_/2).

sumxmy2_(Nums1, Nums2) when Nums1 == []; Nums2 == [] ->
    ?ERRVAL_VAL;
sumxmy2_(Nums1, Nums2) when length(Nums1) =/= length(Nums2) ->
    ?ERRVAL_NA;
sumxmy2_(Nums1, Nums2) ->
    sum(lists:map(fun({X, Y}) ->
                          math:pow(X - Y, 2)
                  end,
                  lists:zip(Nums1, Nums2))).

%%% Trigonometry ~~~~~

sin([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun sin_/1).
sin_([Num]) ->
    math:sin(Num).

cos([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun cos_/1).
cos_([Num]) -> 
    math:cos(Num).

tan([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun tan_/1).
tan_([Num]) ->
    math:tan(Num).

asin([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun asin_/1).
asin_([Num]) when Num > 1 orelse Num < -1 ->
    ?ERRVAL_NUM;
asin_([Num]) ->
    math:asin(Num).

acos([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun acos_/1).
acos_([Num]) when Num < -1 orelse Num > 1 ->
    ?ERRVAL_NUM;
acos_([Num]) ->
    math:acos(Num).

acosh([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun acosh_/1).
acosh_([Num]) when Num < 1 ->
    ?ERRVAL_NUM;
acosh_([Num]) ->
    math:acosh(Num).


atan([V]) ->
    muin:col([V], [eval_funs, first_array, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun atan_/1).
atan_([Num]) ->
    math:atan(Num).

atan2([_, Y]) when ?is_rangeref(Y) ->
    ?ERRVAL_VAL;
atan2([_, _]=Args) ->
    muin:col(Args, [eval_funs, first_array, fetch, {cast, num},
                    {conv, str, ?ERRVAL_VAL}],
             [return_errors, {all, fun is_number/1}],
             fun atan2_/1).
atan2_([X, Y]) when X == 0 andalso Y == 0 ->
    ?ERRVAL_DIV;
atan2_([X, Y]) ->
    math:atan2(Y, X).

atanh([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun atanh_/1).
atanh_([Num]) when Num =< -1 orelse Num >= 1 ->
    ?ERRVAL_NUM;
atanh_([Num]) ->
    math:atanh(Num).

sinh([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun sinh_/1).
sinh_([Num]) ->
    try   math:sinh(Num)
    catch error:_Err -> ?ERRVAL_NUM end.

cosh([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun cosh_/1).
cosh_([Num]) when Num >= 711 ->
    ?ERRVAL_NUM;
cosh_([Num]) ->
    math:cosh(Num).

tanh([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun tanh_/1).
tanh_([Num]) ->
    math:tanh(Num).

asinh([V]) ->
    muin:col([V], [eval_funs, area_first, fetch, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun asinh_/1).
asinh_([Num]) ->
    math:asinh(Num).

degrees([V]) ->
    muin:col([V], [eval_funs, area_first, fetchdb, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun degrees_/1).

degrees_([Angle]) ->
    Angle / math:pi() * 180.

radians([V]) ->
    muin:col([V], [eval_funs, area_first, fetchdb, {cast, num}],
             [return_errors, {all, fun is_number/1}],
             fun radians_/1).

radians_([Angle]) ->
    Angle * math:pi() / 180.

%% Make sure first range is top left
intersect({{AX1, AY1}, _}=A, {{BX1, BY1}, _}=B)
  when BX1 < AX1; BY1 < AY1 ->
    intersect(B, A);

intersect({{_,_}, {AX2,AY2}}, {{BX1, BY1}, {_, _}})
  when AX2 < BX1; AY2 < BY1 ->
    {error, no_intersect};

intersect({{_,_}, {AX2,AY2}}, {{BX1, BY1}, {BX2,BY2}}) ->
    X1 = lists:min([AX2, BX1]),
    X2 = case (AX2 > BX2) of
             true  -> BX2;
             false -> AX2
         end,
    Y1 = lists:min([AY2, BY1]),
    Y2 = case (AY2 > BY2) of
             true  -> BY2;
             false -> AY2
         end,
    {{X1, Y1}, {X2, Y2}}.

thelifeuniverseandeverything([]) ->
    42.

%%% tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").

intersect_test_() ->
    [
     ?_assertEqual({{2,1}, {2,3}},
                   intersect({{1,1}, {2,3}}, {{2,1}, {3,3}})),
     ?_assertEqual({{2,1}, {2,3}},
                   intersect({{-1,-1}, {2,3}}, {{2,1}, {3,3}})),
     ?_assertEqual({{2,1}, {2,3}},
                   intersect({{2,1}, {3,3}}, {{1,1}, {2,3}})),
     ?_assertEqual({{2,1}, {2,3}},
                   intersect({{2,1}, {3,3}}, {{-1,-1}, {2,3}})),
     ?_assertEqual({{2,0}, {3,0}},
                   intersect({{1, 0}, {3,0}}, {{2,0}, {5,0}})),
     ?_assertEqual({{5,0}, {6,0}},
                   intersect({{5, 0}, {6,0}}, {{4,0}, {8,0}})),
     ?_assertEqual({error, no_intersect},
                   intersect({{1, 1}, {2,2}}, {{3,1}, {4,2}}))
    ].


