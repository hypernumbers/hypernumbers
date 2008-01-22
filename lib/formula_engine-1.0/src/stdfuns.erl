%%% Implementations of Excel-compatible functions.
%%% Hasan Veldstra <hasan@hypernumbers.com>
%%% Benefits Dragon <bd@vixo.com>

-module(stdfuns).

-include("spriki.hrl").
-include("handy_macros.hrl").

% Used in: floor, ceiling, even.
% Can't use arbitrary functions in guards, hence using a macro.
-define(is_multiple(Num, Mult),
        (erlang:trunc(Num / Mult) * Mult) == (Num * 1.0)).

%% Stats functions that need higher level information but should be
%% otherwise easy:
%%
%% info/1
%% address/2
%% cell/2
%% column/1
%% columns/1
%% isref/1
%% offset/3-5
%% row/1
%%
%% Quite a few functions (4-6) could be gained easily if I knew what
%% the hell the bloody difference was between deviations of a sample
%% and that of a mean.

%% Excel-compatible functions.
-export([plus/2,
         minus/2,
         times/2,
         divide/2,
         abs/1,
         acos/1,
         acosh/1,
         address/5,
         'and'/1,
         asc/1,
         areas/1,
         asin/1,
         asinh/1,
         atan/1,
         atan2/2,
         atanh/1,
         avedev/1,
         average/1,
         averagea/1,
         betadist/5,
         betainv/5,
         binomdist/4,
         calculate/2,
         ceiling/2,
         cell/2,
         char/1,
         chidist/2,
         chiinv/2,
         chitest/2,
         choose/1,
         clean/1,
         code/1,
         column/1,
         columns/1,
         combin/2,
         concatenate/1,
         confidence/3,
         correl/2,
         cos/1,
         cosh/1,
         count/1,
         counta/1,
         countblank/1,
         countif/2,
         covar/2,
         critbinom/3,
         date/3,
         datevalue/1,
         day/1,
         days360/3,
         daverage/4,
         db/4,
         db/5,
         dcount/4,
         dcounta/4,
         ddb/4,
         ddb/5,
         devsq/1,
         dget/4,
         dydx/2,
         days360/2,
         error_type/1,
         even/1,
         exact/2,
         exp/1,
         expondist/3,
         fact/1,
         false/0,
         fdist/3,
         find/2,
         find/3,
         findb/2,
         findb/3,
         finv/3,
         fisher/1,
         fisherinv/1,
         fixed/1,
         fixed/2,
         fixed/3,
         floor/2,
         forecast/3,
         frequency/2,
         ftest/2,
         fv/3,
         fv/4,
         fv/5,
         gammadist/4,
         gammainv/3,
         gammaln/1,
         geomean/1,
         gestep/1,
         gestep/2,
         growth/4,
         harmean/1,
         hour/1,
         hypgeomdist/4,
         'if'/1,
         index/4,
         int/1,
         integrate/2,
         intercept/2,
         ipmt/4,
         ipmt/5,
         ipmt/6,
         irr/1,
         irr/2,
         iserr/1,
         iserror/1,
         islogical/1,
         isna/1,
         isnontext/1,
         isnumber/1,
         ispmt/4,
         istext/1,
         jis/1,
         kurt/1,
         large/2,
         left/1,
         left/2,
         leftb/1,
         leftb/2,
         len/1,
         lenb/1,
         linest/2,
         linest/3,
         ln/1,
         log/1,
         log/2,
         log10/1,
         lower/1,
         max/1,
         maxa/1,
         mdeterm/1,
         median/1,
         mid/3,
         midb/3,
         min/1,
         mina/1,
         minute/1,
         minverse/2,
         mmult/4,
         mod/2,
         mode/1,
         month/1,
         n/1,
         na/0,
         'not'/1,
         normdist/4,
         normsdist/1,
         now/0,
         odd/1,
         'or'/1,
         permut/2,
         pi/0,
         pound/1,
         pound/2,
         power/2,
         product/1,
         radians/1,
         rand/0,
         replace/4,
         rept/2,
         right/1,
         right/2,
         rightb/1,
         rightb/2,
         round/2,
         rounddown/2,
         roundup/2,
         search/2,
         search/3,
         searchb/2,
         searchb/3,
         second/1,
         sign/1,
         sin/1,
         sinh/1,
         small/2,
         sqrt/1,
         standardise/3,
         stdev/1,
         stdevp/1,
         substitute/3,
         substitute/4,
         sum/1,
         sum/2,
         sumproduct/2,
         sumsq/1,
         tan/1,
         tanh/1,
         today/0,
         trim/1,
         true/0,
         type/1,
         upper/1,
         value/1,
         var/1,
         vara/1,
         varp/1,
         varpa/1,
         year/1,
         cumulate/1,
         percentile/2,
         proper/1,
         quartile/2,
         rank/2,
         rank/3,
         rows/2,
         pearson/2,
         moment/2,
         skew/1,
         slope/2,
         steyx/2,
         stdeva/1,
         subtotal/2,
         sumif/2,
         sumif/3,
         sumx2my2/2,
         sumx2py2/2,
         sumxmy2/2,
         trend/3,
         trend/4,
         transpose/2,
         trimmean/2,
         trunc/1,
         trunc/2,
         weibull/4,
         %% Operator functions.
         negate/1,
         eq/2,
         neq/2,
         lt/2,
         gt/2,
         lte/2,
         gte/2]).


%%-----------------------------------------------------------------------------
%%
%% Microsoft 97 API
%%
%%
%% VERY IMPORTANT NOTES ON HOW THIS DIFFERS FROM EXCEL SPEC!!!
%%        Empty cells are represented as the atom 'empty'.
%%
%%        Alterations for UK:
%%
%%        dollar/1      -> pound/1
%%        standardize/1 -> standardise/1
%%        days360/2     -> Defaults to European date format
%%
%%        Functions highlighted by one line of comments don't work.
%%
%%        pi() only goes to 5dp (can't do more).
%%
%%        Cumulative normal distribution works well but as it uses
%%        a taylor series approximation gives naff results towards
%%        ends of limits.
%%
%%        In linest/3 we don't have a 4th argument as we don't have
%%        the option of returning other relevant data, its return
%%        is of type {B1, B0} for the linear equation.
%%
%%        stdev and stdevp do the same thing at the moment (not
%%        sure of the difference). steyx does not return same
%%        value as Excel version but it is just as meaningful (from
%%        0 -> 1 instead of Excel that is boundless).
%%
%%        Where criterium are specified this module expects a
%%        function to be passed in i.e. the level above this has to
%%        convert ">99" into fun(X) -> X > 99 end.
%%
%%        Not too sure how differences of byte-representation in
%%        characters is being handled (e.g. leftb, rightb) and also
%%        asc/1 just assumes that they are single-byte characters in
%%        unicode form i.e. just strips out the spare 0-byte.
%%        FindB works the same as find (as it is assumed that unicode
%%        is split over two list elements).
%%
%%        Some functions REQUIRE knowledge of row lengths, e.g. DB
%%        functions, for this it is assumed to be rectangle and
%%        an additional argument specifying the lengths of the rows
%%        is required (needed as Gordon currently flattens 2D lists).
%%        (Others are index/3, sumproduct/2, mmult/4).
%%
%%        Matrix functions return type 'matrix':
%%
%%        matrix()  -> {Rows, Columns, [element()]}
%%        element() -> {{I, J}, Value}.
%%
%%        Assumption that trimmean trims Percent in total, i.e. 25%
%%        trims 12.5% from each side.
%%
%%====================================================================

%%%==== Operator functions

%%% Arithmetic

plus(Val1, Val2) ->
    Val1 + Val2.

minus(Val1, Val2) -> 
    Val1 - Val2.

times(Val1, Val2) ->
    Val1 * Val2.

divide(_Val1, Val2) when (Val2 == 0) ->
    exit("divide by 0");
divide(Val1, Val2) -> 
    Val1 / Val2.

negate(Val) ->
    Val * -1.

%%% Logical

eq(A, B) ->
    A == B.
    
neq(A, B) ->
    A =/= B.
    
gt(A, B) ->
    A > B.
    
gte(A, B) ->
    A >= B.
    
lt(A, B) ->
    A < B.
    
lte(A, B) ->
    A =< B.


abs(X) when X > 0 ->
    X;
abs(X) ->
    X * -1.

acos(X) ->
    math:acos(X).

acosh(X) ->
    math:acosh(X).

address(_RowNum, _ColumnNum, _AbsNum, _A1, _SheetText) ->
    {error, unsupported}.

'and'(L) ->
    Booleans = lists:map(fun value_to_boolean/1, L),
    IsTrue = fun(X) -> X end,
    lists:all(IsTrue, Booleans).

asc(Text) when hd(Text) == 0 ->
    F = fun(0, {List, true})  ->
 		{List, false};
 	   (X, {List, false}) ->
 		{[X|List], true}
 	end,
    lists:foldr(F, {[], false}, Text);
asc(Text)                    ->
    Text.

areas(X) when is_list(X) ->
    length(X);
areas(_X)                ->
    1.

asin(X) ->
    math:asin(X).

asinh(X) ->
    math:asinh(X).

atan(X) ->
    math:atan(X).

atan2(X, Y) ->
    math:atan2(Y, X). % standard function takes args in reverse order.

atanh(X) ->
    math:atanh(X).

avedev(N) ->
    Avg = average(N),
    DevFun = fun(X, Acc) -> Acc + erlang:abs(Avg - X) end,
    Deviation = lists:foldl(DevFun, 0, N),
    Deviation / length(N).

average(X) ->
    lists:sum(X) / length(X).

averagea(X) ->
    F = fun(Y) -> cast_value(Y) end,
    average(lists:map(F, X)).

betadist(_X, _Alpha, _Beta, _A, _B) ->
    {error, unsupported}.

betainv(_P, _Alpha, _Beta, _A, _B) ->
    {error, unsupported}.

binomdist(X, _, _, _) when X < 0                  ->
    0;
binomdist(X, Trials, P, false) when is_integer(X) ->
    combin(Trials, X) * power(P, X) * power((1 - P), (Trials - X));
binomdist(X, Trials, P, true) when is_integer(X)  ->
    binomdist(X, Trials, P, false) + binomdist(X - 1, Trials, P, true).

ceiling(_X, 0) -> 0;
ceiling(X, Multiple) when ?is_multiple(X, Multiple) -> X;
ceiling(X, Multiple) -> erlang:trunc(X / Multiple) * Multiple + Multiple.

cell(_InfoType, _Reference) ->
    {error, unsupported}.

char(X) ->
    [X].

%% Not working (formula from sheets is wrong I think).
chidist(X, DegreesOfFreedom) ->
    Beta = 2,
    Alpha = DegreesOfFreedom / Beta,
    Chi = fun() -> 1 / (power(2, Alpha) * gamma(Alpha)) end,
    power(Chi(), (Alpha - 1)) * exp(X * -0.5).

chiinv(_P, _DegreesOfFreedom) ->
    {error, unsupported}.

chitest(_ActualRange, _ExpectedRange) ->
    {error, unsupported}.

%% choose gets its variables in a list now and needs to break
%% out the first one
choose([H|T])-> choose(H,T).

choose(Index, Values) ->
    lists:nth(Index, Values).

clean(Text) ->
    F = fun(X) -> X > 31 end,
    lists:filter(F, Text).

code([H|_T]) ->
    H.

column(_Reference) ->
    {error, unsupported}.

columns(_Array) ->
    {error, unsupported}.

combin(0,0) -> 1; % Excel returns 1.
combin(N, Chosen) ->
    fact(N) div (fact(Chosen) * fact(N - Chosen)).

%% FIXME: This function *needs* type information.
%% See concatenate_test1 in the test suite
%% for text functions (under testroot/unit_tests).
concatenate(ListOfTexts) ->
    lists:append(ListOfTexts).

confidence(_Alpha, _StandardDeviation, _Size) ->
    {error, unsupported}.

correl(Xs, Ys) when length(Xs) == length(Ys) ->
    pearson(Xs, Ys).

cos(X) ->
    math:cos(X).

cosh(X) ->
    math:cosh(X).

count(Range) ->
    F = fun(X) -> is_number(X) end,
    length(lists:filter(F, Range)).

counta(Range) ->
    F = fun(X) -> is_boolean(X) orelse
				  (not (is_atom(X)
					orelse is_function(X))) end,
    length(lists:filter(F, Range)).

countblank(Range) ->
    F = fun(X) -> X == empty end,
    length(lists:filter(F, Range)).

countif(Range, Criteria) ->
    length(lists:filter(Criteria, Range)).

covar(_Array1, _Array2) ->
    {error, unsupported}.

critbinom(Trials, P, Alpha) when not (Alpha < 0), not (Alpha > 1) ->
    critbinom(Trials, P, Alpha, 0).

critbinom(Trials, P, Alpha, X) ->
    Value = binomdist(X, Trials, P, true),
    if
 	Value >= Alpha ->
 	    X;
 	true           ->
 	    critbinom(Trials, P, Alpha, X + 1)
    end.

date(Year, Month, Date) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Date}, {0, 0, 0}}).

datevalue([Date,"/",Month,"/",Year]) when length(Date) < 3,
					  length(Month) < 3,
					  length(Year) == 4 ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Date}, {0, 0, 0}}).

daverage(Database, RowWidth, Field, Criteria) ->
    average(db_generic(Database, RowWidth, Field, Criteria)).

day(Seconds) ->
    {{_, _, Date}, {_, _, _}} = calendar:gregorian_seconds_to_datetime(Seconds),
    Date.

days360(StartDate, EndDate) ->
    days360(StartDate, EndDate, true).

days360({Year, Date, Month}, {Year2, Date2, Month2}, false) ->
    days360({Year, Month, Date}, {Year2, Month2, Date2}, true);
days360({Year, Month, Date}, {Year2, Month2, Date2}, true) ->
    YearDiff = (Year2 - Year) * 12,
    MonthDiff = (YearDiff + Month2 - Month) * 30,
    MonthDiff + Date2 - Date.

db(Cost, Salvage, Life, Period) ->
    db(Cost, Salvage, Life, Period, 12).

db(_Cost, _Salvage, _Life, _Period, _Month) ->
    {error, unsupported}.

dcount(Database, RowWidth, Field, Criteria) ->
    count(db_generic(Database, RowWidth, Field, Criteria)).

dcounta(Database, RowWidth, Field, Criteria) ->
    counta(db_generic(Database, RowWidth, Field, Criteria)).

ddb(Cost, Salvage, Life, Period) ->
    ddb(Cost, Salvage, Life, Period, 2).

ddb(_Cost, _Salvage, _Life, _Period, _Factor) ->
    {error, unsupported}.

devsq(Values) ->
    moment(Values, 2) * length(Values).

dget(Database, RowWidth, Field, Criteria) ->
    hd(db_generic(Database, RowWidth, Field, Criteria)).

error_type({error, null})        -> 1;
error_type({error, div_by_zero}) -> 2;
error_type({error, value})       -> 3;
error_type({error, reference})   -> 4;
error_type({error, name})        -> 5;
error_type({error, number})      -> 6;
error_type({error, na})          -> 7;
error_type(_)                    -> {error, na}.

%% Rounds up to the nearest multiple of 2 *away from zero*.
even(X) when ?is_multiple(X, 2) -> X; % Also handles the case when X is 0.
even(X) when (X > 0) -> ceiling(X, 2);
even(X) when (X < 0) -> ceiling(X, -2).

exact(X, X)   ->
    true;
exact(_X, _Y) ->
    false.

exp(X) ->
    math:exp(X).

expondist(X, Lamda, false) when not (X < 0) ->
    exp(-1 * X / Lamda) / Lamda;
expondist(X, Lamda, true) when not (X < 0)  ->
    1 - exp(-1 * X / Lamda).

fact(X) when X < 0 ->
    {error, number};
fact(0)            ->
    1;
fact(X)            ->
    X * fact(X-1).

false() ->
    false.

fdist(_X, _DegFreedom1, _DegFreedom2) ->
    {error, unsupported}.

find(FindText, WithinText) ->
    find(FindText, WithinText, 1).

find(FindText,WithinText,Index)
  when Index > 0, Index =< length(WithinText) ->
    ReturnText=case string:substr(WithinText, Index, length(FindText)) of
		   FindText ->
		       Index;
		   _Else    ->
		       find(FindText, WithinText, Index + 1)
	       end,
    ReturnText;
find(_FindText,_WithinText,_Index) ->
    {error, value}.

findb(FindText,WithinText) ->
    findb(FindText,WithinText,1).

findb(FindText,WithinText,Index) ->
    find(FindText,WithinText,Index).

finv(_P, _DegFreedom1, _DegFreedom2) ->
    {error, unsupported}.

fisher(X) when X > -1; X < 1 ->
    {error, unsupported}.

fisherinv(_Y) ->
    {error, unsupported}.

%% Butt-ugly, but it works.
%% FIXME:
fixed(Number) -> fixed(Number, 2).

fixed(Number, Decimals) when is_number(Decimals) ->
  fixed(Number, Decimals, false);
fixed(Number, NoCommas) when is_boolean(NoCommas) ->
  fixed(Number, 2, NoCommas).

fixed(Number, Decimals, NoCommas) ->
  RoundedNumber = round(Number, Decimals) * 1.0,
  Str = if
    Decimals > 0 ->
      lists:nth(1, io_lib:format("~." ++ integer_to_list(Decimals) ++ "f", [RoundedNumber]));
    true -> integer_to_list(erlang:trunc(RoundedNumber))
  end,

  case NoCommas of
    true -> Str; % May need to attach type information here later.
    false ->
      IntAsStr = integer_to_list(erlang:trunc(RoundedNumber)),

      if
        Decimals > 0 ->
          commify(IntAsStr) ++ "." ++ lists:sublist(Str,
                    string:rchr(Str, $.) + 1, % Start with the symbol right after the dot...
                    length(Str) - string:rchr(Str, $.)); % ...And go all the way 'till the end.
        true -> commify(IntAsStr)
    end
  end.


floor(_X, 0) -> 0;
floor(X, Multiple) when ?is_multiple(X, Multiple) -> X;
floor(X, Multiple) -> erlang:trunc(X / Multiple) * Multiple.

forecast(X, KnownX, KnownY) ->
    {B1, B0} = linest(KnownX, KnownY),
    B1 * X + B0.

frequency(Array, Bins) ->
    GetPos = fun(X, {Value, State, Done}) ->
		     if
			 Value < X, not Done -> {Value, State - 1, true};
			 not Done            -> {Value, State + 1, false};
			 true                -> {Value, State, true}
		     end
	     end,
    F = fun(X, Acc) ->
		Index =
		    case lists:foldl(GetPos, {X, 1, false}, Bins) of
			{_X, I, true}   -> I;
			{_X, _I, false} -> length(Bins) -1
		    end,
		F2 = fun(Y) -> Y + 1 end,
		set(Acc, Index, F2)
	end,
    lists:foldl(F, lists:duplicate(length(Bins), 0), Array).

ftest(_Array1, _Array2) ->
    {error, unsupported}.

fv(Rate, Periods, Payment) ->
    fv(Rate, Periods, Payment, 0).

fv(Rate, Periods, Payment, Value) ->
    fv(Rate, Periods, Payment, Value, 0).

fv(Rate, Periods, Payment, Value, 0) when Periods > 0 ->
    fv(Rate, Periods-1, Payment, (Value*Rate)+Payment, 0);
fv(_Rate, 0, _Payment, Value, 0)                      ->
    Value;
fv(Rate, Periods, Payment, Value, 1) when Periods > 0 ->
    fv(Rate, Periods-1, Payment, (Value+Payment)*Rate, 1);
fv(_Rate, 0, _Payment, Value, 1)                      ->
    Value.

gammadist(X, Alpha, Beta, false)
  when not (X < 0), Alpha > 0, Beta > 0   ->
    Top = power(X, Alpha - 1) * exp(-1 * X / Beta),
    Top / (power(Beta, Alpha) * gamma(Alpha));
gammadist(_X, _Alpha, _Beta, false) ->
    0.

gammainv(_P, _Alpha, _Beta) ->
    {error, unsupported}.

gammaln(X) when X >= 0 ->
    {error, unsupported}.

geomean(_List) ->
    {error, unsupported}.

gestep(Number) ->
    gestep(Number, 0).

gestep(Number, Step) when Number < Step ->
    0;
gestep(_Number, _Step)                  ->
    1.

growth(_Xs, _Ys, _NewXs, _K) ->
    {error, unsupported}.

harmean(_Values) ->
    {error, unsupported}.

hour(Seconds) ->
    {{_, _, _}, {Hour, _, _}} = calendar:gregorian_seconds_to_datetime(Seconds),
    Hour.

hypgeomdist(_SampleS, _NumInSample, _PopulationS, _NumInPopulation) ->
    {error, unsupported}.

index(Array, RowWidth, Row, Column) ->
    lists:nth((Row - 1) * RowWidth + Column, Array).

'if'([Test, TrueVal, FalseVal]) ->
    case value_to_boolean(Test) of
        true ->
            TrueVal;
        false ->
            FalseVal;
        _ ->
            nothing % TODO: Throw an error.
    end.

int(X) when round(X) > X ->
    round(X) - 1;
int(X)                   ->
    round(X).

intercept(Ys, Xs) ->
    {M, C} = linest(Xs, Ys),
    C / M.

ipmt(Rate, Period, NPeriods, Value) ->
    ipmt(Rate, Period, NPeriods, Value, 0).

ipmt(Rate, Period, NPeriods, Value, FinalValue) ->
    ipmt(Rate, Period, NPeriods, Value, FinalValue, 0).

ipmt(Rate, Period, NPeriods, Value, FinalValue, 0) ->
    Diff = Value - FinalValue,
    MR = Rate * Period,
    (Diff* MR*math:pow((1+MR),NPeriods))/(math:pow((1+MR),NPeriods)-1).

irr(Values) ->
    irr(Values, 0.1).

irr(_Values, _Guess) ->
    {error, unsupported}.

iserr({error, na}) ->
    false;
iserr({error, _}) ->
    true;
iserr(_) ->
    false.

iserror({error, _}) ->
    true;
iserror(_) ->
    false.

islogical(true)  ->
    true;
islogical(false) ->
    true;
islogical(_) ->
    false.

isna({error, na}) ->
    true;
isna(_) ->
    false.

isnontext(X) when not is_list(X) ->
    true;
isnontext(X) when is_list(X)     ->
    F = fun(Z) -> Z < 32 orelse Z > 127 end,
    lists:any(F, X).

isnumber(X) when is_integer(X) ->
    true;
isnumber(X) when is_float(X)   ->
    true;
isnumber(_) ->
    false.

ispmt(Rate, Period, NPeriods, Value) ->
    (math:pow(1 + (Rate * Period), NPeriods) - 1) * Value.

istext(X) ->
    not isnontext(X).

%% not sure what this does...
jis([0|T]) ->
    [0|T];
jis(List)  ->
    F = fun(X) -> [0, X] end,
    lists:flatten(lists:map(F, List)).

kurt(Values) ->
    (moment(Values, 4) / power(moment(Values, 2), 2)) - 3.

large(Array, K) ->
    Sorted = lists:sort(Array),
    lists:nth(K, lists:reverse(Sorted)).

left(Text) ->
    left(Text, 1).

left(Text, Index) ->
    lists:sublist(Text, 1, Index).

leftb(Text) when is_list(Text) ->
    leftb(Text, 1).

leftb(Text, Index) when is_list(Text) ->
    <<A:Index/binary, _Rest/binary>> = list_to_binary(Text),
    binary_to_list(A).

len(Text) when is_list(Text) ->
    length(Text).

lenb(Text) when is_list(Text) ->
    size(list_to_binary(Text)).

linest(Xs, Ys) ->
    linest(Xs, Ys, false).

linest(Xs, Ys, true) ->
    {average(Xs) / average(Ys), 0};
linest(Xs, Ys, false) ->
    {MuX, MuY} = {average(Xs), average(Ys)},
    F = fun({X, Y}) -> {(X - MuX) * (Y - MuY), (X - MuX) * (X - MuX)} end,
    NewList = lists:map(F, lists:zip(Xs, Ys)),
    {Left, Right} = lists:unzip(NewList),
    B1 = lists:sum(Left) / lists:sum(Right),
    {B1, MuY - (B1 * MuX)}.

ln(X) ->
    math:log(X).

log(X) ->
    log(X, 10).

log(X, P) ->
    math:log(X) / math:log(P).

log10(X) ->
    math:log10(X).

lower(Text) ->
    Transform = fun(X) ->
			if
			    X >= 65, X =< 91 -> X + 32;
			    true             -> X
			end
		end,
    lists:map(Transform, Text).

max(Values) ->
    lists:max(Values).

maxa(Values) ->
    Alter = fun(X) -> cast_value(X) end,
    max(lists:map(Alter, Values)).

mdeterm(List) ->
    RowLength = round(math:sqrt(length(List))),
    Matrix = matrix:list_to_matrix(List, RowLength),
    matrix:determinant(Matrix).

median(Values) ->
    quartile(Values, 2).

mid(Text, Index, _Length) when Index > length(Text) -> "";
mid(Text, Index, Length) ->
    string:substr(Text, Index, Length).

midb(Text, Index, Length) ->
    Data = list_to_binary(Text),
    <<_Start:Index/binary, Middle:Length/binary, _End/binary>> = Data,
    binary_to_list(Middle).

min(Values) ->
    lists:min(Values).

mina(Values) ->
    Alter = fun(X) -> cast_value(X) end,
    min(lists:map(Alter, Values)).

minute(Seconds) ->
    {{_,_,_},{_,Minute,_}}=calendar:gregorian_seconds_to_datetime(Seconds),
    Minute.

%% The version of minverse in Excel 2007 only has one parameter
minverse(Matrix, RowWidth) ->
    matrix:inverse(matrix:list_to_matrix(Matrix, RowWidth)).

%% The version of minverse in Excel 2007 only has two parameters
mmult(Matrix1, RowWidth1, Matrix2, RowWidth2) ->
    M1 = matrix:list_to_matrix(Matrix1, RowWidth1),
    M2 = matrix:list_to_matrix(Matrix2, RowWidth2),
    matrix:multiply(M1, M2).

mod(_Num, Divisor) when (Divisor == 0) -> {error, div_by_zero};
mod(Num, Divisor) -> Num - Divisor * int(Num / Divisor).

mode(List) ->
    Sorted = lists:sort(List),
    AddUp = fun(X, [{Key, Value}|T]) ->
		    if
			X == Key -> [{Key, Value+1}|T];
			true     -> [{X, 1},{Key,Value}|T]
		    end
	    end,
    KeyList = lists:foldl(AddUp, [{hd(Sorted),0}], Sorted),
    SortedKeyList = lists:keysort(2, KeyList),
    {Key, _Value} = lists:last(SortedKeyList),
    Key.

month(Seconds) ->
    {{_,Month,_},{_,_,_}}=calendar:gregorian_seconds_to_datetime(Seconds),
    Month.

%% TODO: Dates.
n(Bool) when Bool == true -> 1;
n(Bool) when Bool == false -> 0;
n(Float) when is_float(Float) -> Float;
n(Int) when is_integer(Int) -> Int;
n({error, X}) -> {error, X};
n(_) -> 0.

na() ->
    {error, na}.

normdist(X, Mean, Dev, true)  ->
    0.5 * (1 + erf((X - Mean) / (Dev * math:sqrt(2))));
normdist(X, Mean, Dev, false) ->
    K = 1 / (Dev * math:sqrt(2 * pi())),
    Top = -1 * ((X - Mean) * (X - Mean)),
    K * math:exp(Top / ((2 * Dev) * (2 * Dev))).

normsdist(Z) ->
    normdist(Z, 0, 1, true).

'not'(X) ->
    not value_to_boolean(X).

now() ->
    DT = calendar:now_to_datetime(erlang:now()),
    calendar:datetime_to_gregorian_seconds(DT).

odd(0) -> 1;
odd(X) ->
    E = even(X),
    if
      erlang:abs(E) - erlang:abs(X) < 1 -> E + sign(X);
      true -> E - sign(X)
    end.

'or'(L) ->
    IsTrue = fun(X) -> X end,
    Booleans = lists:map(fun(X) -> value_to_boolean(X) end, L),
    lists:any(IsTrue, Booleans).

pearson(Xs, Ys) ->
    {MuX, DevX} = {average(Xs), stdev(Xs)},
    {MuY, DevY} = {average(Ys), stdev(Ys)},
    Zx = fun(X) -> (X - MuX) / DevX end,
    Zy = fun(Y) -> (Y - MuY) / DevY end,
    ZxZy = xy(lists:map(Zx, Xs), lists:map(Zy, Ys)),
    sum(ZxZy) / (length(Xs) - 1).

percentile(List, K) ->
    F = fun(X) -> X / lists:sum(List) end,
    L = lists:map(F, cumulate(List)),
    first_max(L, K).

permut(N, R) when N >= R ->
    fact(N) div fact(N - R).

pi() -> math:pi().

pound(X) -> pound(X, 2).

pound(X, Decimals) when Decimals < 1 ->
    PDec = Decimals * -1,
    Z = round(X / math:pow(10, PDec)) * round(math:pow(10, PDec)),
    "£" ++ integer_to_list(Z);
pound(X, Decimals)                   ->
    MadeBig = round(X * math:pow(10, Decimals)),
    Size = length(integer_to_list(MadeBig)) + 1,
    String = float2dblstr(MadeBig / math:pow(10, Decimals)),
    "£" ++ lists:sublist(String, Size).

power(N, P) -> math:pow(N, P).

product(Values) ->
    Multiple = fun(X, Acc) ->
		       Acc * X
	       end,
    lists:foldl(Multiple, 1, Values).

proper(Text) ->
    proper(Text, true).

proper([], _Space) ->
    [];
proper([$ |T], _Space) ->
    [$ |proper(T, true)];
proper([H|T], true) when H > 96, H < 118 ->
    [H-32|proper(T, false)];
proper([H|T], false) when H > 64, H < 92 ->
    [H+32|proper(T, false)];
proper([H|T], _Space) ->
    [H|proper(T, false)].

quartile(List, K) ->
    lists:nth(percentile(List, K * 0.25), List).

radians(Angle) ->
    Angle * pi() / 180.

rand() ->
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    random:uniform().

rank(Number, List) ->
    rank(Number, List, false).

rank(Number, List, false) ->
    (length(List) + 1) - rank(Number, List, true);
rank(Number, List, true)  ->
    Sorted = lists:sort(List),
    first_max(Sorted, Number).

replace([], _Index, _Length, _NewText) ->
    [];
replace(OldText, _Index, 0, _NewText) ->
    OldText;
replace([_H1|OldText], 1, Length, [H2|NewText]) ->
    [H2|replace(OldText, 1, Length-1, NewText)];
replace([H1|OldText], Index, Length, NewText) ->
    [H1|replace(OldText, Index-1, Length, NewText)].

rept(Text, Repeats) ->
    string:copies(Text, Repeats).

right(Text) ->
    right(Text, 1).

right(_Text, 0) -> "";
right(Text, NumChars) when NumChars >= length(Text) -> Text;
right(Text, NumChars) ->
    lists:sublist(Text, length(Text) + 1 - NumChars, NumChars).

rightb(Text) ->
    rightb(Text, 1).

rightb(Text, Index) when is_list(Text) ->
    Data = list_to_binary(Text),
    Size = size(Data) - Index,
    <<_A:Size/binary, Rest/binary>> = Data,
    binary_to_list(Rest).

%% Round the number to the left of the decimal point.
round(X, NumDigits) when (NumDigits < 0) ->
  Exp = erlang:abs(NumDigits),
  erlang:round(X / math:pow(10, Exp)) * erlang:round(math:pow(10, Exp));
%% Round to the nearest integer.
round(X, 0) -> erlang:round(X);
%% Round to the specified number of decimal places.
round(X, NumDigits) ->
  erlang:round(X * math:pow(10, NumDigits)) / erlang:round(math:pow(10, NumDigits)).

%% Like round(), but always rounds *towards* zero.
rounddown(X, 0) -> erlang:trunc(X);
rounddown(X, NumDigits) when (NumDigits < 0) ->
  Exp = erlang:abs(NumDigits),
  erlang:trunc(X / math:pow(10, Exp)) * math:pow(10, Exp);
rounddown(X, NumDigits) -> erlang:trunc(X * math:pow(10, NumDigits)) / math:pow(10, NumDigits).

%% Like round(), but always rounds *away* from zero.
%% When NumDigits is negative, the behavior is a bit non-intuitive.
roundup(X, 0) ->
  if
    erlang:trunc(X) == (X * 1.0) -> X; % This also handles the case when X is 0.
    true -> erlang:trunc(X) + sign(X)
  end;
roundup(X, NumDigits) when NumDigits < 0 ->
  Exp = erlang:abs(NumDigits),
  Rounded = erlang:trunc(erlang:trunc(X) / math:pow(10, Exp)) * math:pow(10, Exp),
  if
    erlang:abs(X) > erlang:abs(Rounded) -> Rounded + (sign(X) * math:pow(10, Exp));
    true -> Rounded
  end;
roundup(X, NumDigits) -> rndup( X * math:pow(10, NumDigits) ) / erlang:round(math:pow(10, NumDigits)).
%% FIXME: get rid of the call to rndup above.

rows(Table, RowLength) ->
    length(Table) div RowLength.

search(FindText, WithinText) ->
    search(FindText, WithinText, 1).

search(FindText, WithinText, Index) ->
    find(lower(FindText), lower(WithinText), Index).

%% NOT WORKING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
searchb(FindText, WithinText) ->
    searchb(FindText, WithinText, 1).

searchb(FindText, WithinText, Index) ->
    findb(lower(FindText), lower(WithinText), Index).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

second(Seconds) ->
    {{_, _, _}, {_, _, Second}} = calendar:gregorian_seconds_to_datetime(Seconds),
    Second.

sign(X) when X > 0 ->
    1;
sign(X) when X < 0 ->
    -1;
sign(0)            ->
    0.

sin(X) ->
    math:sin(X).

sinh(X) ->
    math:sinh(X).

skew(Values) ->
    moment(Values, 3) / math:pow(moment(Values, 2), 1.5).

slope(Xs, Ys) ->
    element(1, linest(Xs, Ys)).

small(Array, K) ->
    Sorted = lists:sort(Array),
    lists:nth(K, Sorted).

sqrt(X) ->
    math:sqrt(X).

standardise(X, Mean, StandardDeviation) ->
    (X - Mean) / math:sqrt(StandardDeviation).

stdev(Values) ->
    math:sqrt(devsq(Values) / (length(Values) - 1)).

stdeva(Values) ->
    NewValues = lists:map(fun(X) -> cast_value(X) end, Values),
    math:sqrt(devsq(NewValues) / (length(Values) - 1)).

stdevp(Values) ->
    math:sqrt(devsq(Values) / (length(Values) - 1)).

steyx(Xs, Ys) ->
    math:pow(pearson(Xs, Ys), 2).


substitute(Text, OldText, NewText) ->
  substitute(Text, OldText, NewText, 1).

substitute(Text, _OldText, _NewText, 0) -> Text;

substitute(Text, OldText, NewText, 1) ->
  case find(OldText, Text) of
    {error, value} -> Text;
    Index -> lists:sublist(Text, Index - 1) ++ % The part before the text to replace.
             NewText ++
             lists:sublist(Text, Index + length(OldText), length(Text)) % And the part after.
  end;

substitute(Text, OldText, NewText, InstanceNum) ->
  case find(OldText, Text) of % substitute() is case-sensitive, so using find().
	  {error, value} -> Text;
    Index ->
      PrePart = lists:sublist(Text, 1, Index), % Part we're keeping intact.
      TailPart = substitute(lists:sublist(Text, Index + 1, length(Text)), % Part where a substitution *may* take place.
                            OldText,
                            NewText,
                            InstanceNum - 1),
      PrePart ++ TailPart
  end.


subtotal(1, Refs) -> lists:map(fun(X)  -> average(X) end, Refs);
subtotal(2, Refs) -> lists:map(fun(X)  -> count(X) end, Refs);
subtotal(3, Refs) -> lists:map(fun(X)  -> counta(X) end, Refs);
subtotal(4, Refs) -> lists:map(fun(X)  -> max(X) end, Refs);
subtotal(5, Refs) -> lists:map(fun(X)  -> min(X) end, Refs);
subtotal(6, Refs) -> lists:map(fun(X)  -> product(X) end, Refs);
subtotal(7, Refs) -> lists:map(fun(X)  -> stdev(X) end, Refs);
subtotal(8, Refs) -> lists:map(fun(X)  -> stdevp(X) end, Refs);
subtotal(9, Refs) -> lists:map(fun(X)  -> sum(X) end, Refs);
subtotal(10, Refs) -> lists:map(fun(X) -> var(X) end, Refs);
subtotal(11, Refs) -> lists:map(fun(X) -> varp(X) end, Refs).

sum(X1, X2) ->
    sum([X1, X2]).

sum(Values) when is_list(Values) ->
    lists:sum([X || X <- flatten(Values), is_number(X)]);

sum(X) ->
    sum([X]).

sumif(Range, Criteria) -> sumif(Range, Criteria, Range).

sumif(Range, Criteria, Values) ->
    Zip = lists:zip(Range, Values),
    Filter = lists:filter(fun({X, _Y}) -> Criteria(X) end, Zip),
    sum(element(2, lists:unzip(Filter))).

sumproduct(Arrays, RowWidth) ->
    sumproductm(deepen(Arrays, RowWidth), 0).

sumproductm([[]|_Rest], Total) ->
    Total;
sumproductm(Arrays, Total)    ->
    MapFold = fun([H|T], Acc) ->
		      {T, Acc * H}
	      end,
    {NewList, Acc} = lists:mapfoldl(MapFold, 1, Arrays),
    sumproductm(NewList, Total + Acc).

sumsq(Values) ->
    SqFun = fun(X, Acc) -> Acc + math:pow(X, 2) end,
    lists:foldl(SqFun, 0, Values).

sumx2my2(Xs, Ys) ->
    F = fun({X, Y}) -> (X * X) - (Y * Y) end,
    sum(lists:map(F, lists:zip(Xs, Ys))).

sumx2py2(Xs, Ys) ->
    F = fun({X, Y}) -> (X * X) + (Y * Y) end,
    sum(lists:map(F, lists:zip(Xs, Ys))).

sumxmy2(Xs, Ys) ->
    F = fun({X, Y}) -> math:pow(X - Y, 2) end,
    sum(lists:map(F, lists:zip(Xs, Ys))).

tan(X) ->
    math:tan(X).

tanh(X) ->
    math:tanh(X).

today() ->
    {{A, B, C}, {_, _, _}} = calendar:now_to_datetime(erlang:now()),
    calendar:datetime_to_gregorian_seconds({{A, B, C}, {0, 0, 0}}).

transpose(Rows, RowWidth) ->
    Matrix = matrix:list_to_matrix(Rows, RowWidth),
    matrix:transpose(Matrix).

trend(Xs, Ys, NewXs) ->
    trend(Xs, Ys, NewXs, false).

trend(Xs, Ys, NewXs, Const) ->
    {M, C} = linest(Xs, Ys, Const),
    lists:map(fun(X) -> X * M + C end, NewXs).

trim(Text) ->
    string:strip(Text, both).

trimmean(Values, Percent) when Percent =< 50 ->
    Sorted = lists:sort(Values),
    N = round((Percent / 100) * length(Values)) div 2,
    average(lists:sublist(Sorted, N + 1, length(Values) - 2 * N));
trimmean(_Values, _Percent)                  ->
    {error, value}.

true() ->
    true.

trunc(X) ->
    trunc(X, 0).

trunc(X, D) ->
    rounddown(X, D).

type(X) when is_number(X) -> 1;
type(X) when is_boolean(X) -> 4;
type({error, _}) -> 16;
type(X) when is_list(X) ->
  IsText = not isnontext(X),
  if
    IsText -> 2;
    true   -> 64
  end.

upper(Text) ->
    Transform = fun(X) ->
			if
			    X >= 97, X =< 123 -> X - 32;
			    true              -> X
			end
		end,
    lists:map(Transform, Text).

value(Text) ->
    case string:to_float(Text) of
	{error, no_float} ->
	    {Value, _} = string:to_integer(Text),
	    Value;
	{Value, _}        ->
	    Value
    end.

var(Xs) ->
    math:pow(stdev(Xs), 2).

vara(Xs) ->
    var(lists:map(fun(X) -> cast_value(X) end, Xs)).

varp(Xs) ->
    math:pow(stdevp(Xs), 2).

varpa(Xs) ->
    varp(lists:map(fun(X) -> cast_value(X) end, Xs)).

weibull(X, Alpha, Beta, false) when X >= 0 ->
    (Alpha / Beta) * math:pow(X, Alpha - 1) * math:exp(-1 * math:pow(X, Alpha) / Beta);
weibull(X, Alpha, Beta, true) when X >= 0  ->
    1 - math:exp(-1 * math:pow(X, Alpha) / Beta).

year(Seconds) ->
    {{Year, _, _}, {_, _, _}} = calendar:gregorian_seconds_to_datetime(Seconds),
    Year.

%%====================================================================
%%
%% Internal functions
%%
%%====================================================================

%% Takes an integer (as list), and gives it back formatted with commas.
commify(IntAsStr) when is_list(IntAsStr) ->
  commify(lists:reverse(IntAsStr), $,, []).
commify([A, B, C, D | T], P, Acc) ->
  commify([D|T], P, [P, C, B, A|Acc]);
commify(L, _, Acc) ->
  lists:reverse(L) ++ Acc.

cast_value(X) when is_number(X)  -> X;
cast_value(true)                 -> 1;
cast_value(false)                -> 0;
cast_value(X) when is_list(X)    ->
    case {is_list_number(X), lists:member($., X)} of
	{true, true}  ->
	    list_to_float(X);
	{true, false} ->
	    list_to_integer(X);
	_Else          ->
	    0
    end.

cumulate(List) ->
    F = fun(X, [])    -> [X];
	   (X, [H|T]) -> [X+H,H|T]
	end,
    lists:reverse(lists:foldl(F, [], List)).

db_generic(Database, RowWidth, Field, Criteria) when is_function(Criteria) ->
    Column = get_ncolumn(Database, RowWidth, Field),
    lists:filter(Criteria, Column);
db_generic(Database, RowWidth, Field, _Criteria)                           ->
    db_generic(Database, RowWidth, Field, fun(_X) -> true end).

deepen([], _RowWidth)  ->
    [];
deepen(List, RowWidth) ->
    {List1, List2} = lists:split(RowWidth, List),
    [List1|deepen(List2, RowWidth)].

erf(X) ->
    EXPANSION_DETAIL = 20,
    (2 / math:sqrt(pi())) * taylor(X, taylor_list(EXPANSION_DETAIL)).

first_max(List, X) ->
    first_max(List, X, 1).

first_max([H|T], X, N) when H < X   -> first_max(T, X, N + 1);
first_max([H|_T], X, N) when X =< H -> N;
first_max([], _X, _N)               -> {error, value}.

gamma(Alpha) when is_integer(Alpha) ->
    fact(Alpha);
gamma(Alpha) when is_float(Alpha)   ->
    fact(round(Alpha)).

get_column(Database, RowWidth, Field) ->
    Index = index_of(Field, Database),
    Grid = deepen(Database, RowWidth),
    F = fun(X) -> lists:nth(Index, X) end,
    lists:map(F, Grid).

get_ncolumn(Database, RowWidth, Field) ->
    tl(get_column(Database, RowWidth, Field)).

is_list_number(List) ->
    F = fun(X) -> (X > 47 andalso X < 58) orelse (X == $.) end,
    lists:all(F, List).

moment(Values, M) ->
    Avg = average(Values),
    DevFun = fun(X, Acc) -> Acc + math:pow((Avg - X), M) end,
    lists:foldl(DevFun, 0, Values) / length(Values).

rndup(X) when round(X) < X, X >= 0 ->
    round(X) + 1;
rndup(X) when round(X) > X, X < 0  ->
    round(X) - 1;
rndup(X)                           ->
    round(X).

taylor(X, Cof) ->
    F = fun({Power, Div}) ->
		math:pow(X, Power) / Div
	end,
    Eval = lists:map(F, Cof),
    lists:sum(Eval).

taylor_list(N) ->
    List = lists:seq(1, N),
    F = fun(X) ->
		A = X - 1,
		{A * 2 + 1, math:pow(-1, A) * (2 * A + 1) * fact(A)}
	end,
    lists:map(F, List).

xy(Xs, Ys) ->
    L = lists:zip(Xs, Ys),
    lists:map(fun({X, Y}) -> X * Y end, L).

%%--------------------------------------------------------------------
%% Function: float2dblstr(Number)
%%           (copied from swarm/lib/utilities-1.0/src/util.erl)
%%
%% Description: Arguments
%%
%%              * string | float
%%
%%              Floats are converted in a not so pleasant form to
%%              strings, this is a nicer alternative i.e
%%
%%              "5.3e-01" -> "0.53"
%%--------------------------------------------------------------------
float2dblstr(Value) when is_float(Value) ->
    float2dblstr(float_to_list(Value));
float2dblstr(Value) when is_list(Value) ->
    {Number,Exponent} = split_float(Value),
    if
	Exponent > 0  -> add_zeros(digit_list(Number), Exponent - 1);
	Exponent == 0 -> add_zeros(digit_list(Number), 0);
	true          -> add_decimals(digit_list(Number), Exponent * -1)
    end.

split_float(Value) -> split_float(Value, []).

split_float([$e|T], Current) -> {Current, list_to_integer(T)};
split_float([H|T],  Current) -> split_float(T, [H|Current]).

digit_list(List) ->
    case string:strip(lists:delete($., List), both, $0) of
	[]     -> "0";
	IntStr -> lists:reverse(IntStr)
    end.

add_zeros([], N)       -> string:chars($0, N);
add_zeros([H], 0)      -> [H];
add_zeros([H|List], 0) -> [H] ++ "." ++ List;
add_zeros([H|T], N)    -> [H|add_zeros(T, N-1)].

add_decimals(List, 0) -> List;
add_decimals(List, 1) -> [$0, $.|List];
add_decimals(List, N) -> add_decimals([$0|List], N-1).

%%-------------------------------------------------------------------
%% Array Functions
%%-------------------------------------------------------------------
set([], _Index, _F) ->
    [];
set([H|T], 0, F) ->
    [F(H)|T];
set([H|T], Index, F) ->
    [H|set(T, Index-1, F)].

index_of(X, List) ->
    index_of(X, List, 1).

index_of(_X, [], _I)   ->
    {error, not_found};
index_of(X, [X|_T], I) ->
    I;
index_of(X, [_H|T], I) ->
    index_of(X, T, I + 1).

%%-------------------------------------------------------------------
%% Formula Functions
%%-------------------------------------------------------------------
integrate(By, By) ->
    {divide, {pow, By, 2}};
integrate(X, By) when is_number(X) ->
    {mult, X, By};
integrate({pow, e, X}, By) ->
    {divide, {pow, e, X}, dydx(X, By)};
integrate({pow, X, Y}, By) ->
    {divide, {pow, X, {add, dydx(Y, By), 1}}, {add, dydx(Y, By), 1}};
integrate({mult, _X, Y}, By) ->
    U = integrate(Y, By),
    io:format("U := ~p~n", [U]),
    ok.
%%    {add, {mult, U, X}, {mult, -1, integrate({mult, U, dydx(Y, By)}, By)}}.

calculate(undef, _Value) ->
    undef;
calculate({pow, X, Y}, Value) ->
    CX = calculate(X, Value),
    CY = calculate(Y, Value),
    case {CX, CY} of
	{0, _}                 -> 0;
	{_, {mult, -1, undef}} -> 0;
	{_, {mult, 1, undef}}  -> undef;
	{e, P}                 -> math:exp(P);
	{A, P}                 -> power(A, P)
    end;
calculate({mult, X, Y}, Value) ->
    CX = calculate(X, Value),
    CY = calculate(Y, Value),
    if
	CX == 0; CY == 0 -> 0;
	CX == undef      -> {mult, sign(CY), undef};
	CY == undef      -> {mult, sign(CX), undef};
	true             -> CX * CY
    end;
calculate({divide, X, Y}, Value) ->
    CX = calculate(X, Value),
    CY = calculate(Y, Value),
    if
	CY == undef -> {mult, sign(CX), undef};
	CX == 0     -> 0;
	true        -> CX / CY
    end;
calculate({add, X, Y}, Value) ->
    calculate(X, Value) + calculate(Y, Value);
calculate(e, _Value) ->
    e;
calculate(X, Value) when is_atom(X) ->
    Value;
calculate(X, _Value) when is_number(X) ->
    X.

dydx({pow, e, Y}, By)  ->
    {mult, dydx(Y, By), {pow, e, Y}};
dydx({pow, X, Y}, By) ->
    {pow, {mult, Y, {mult, dydx(X, By), X}}, Y - 1};
dydx({mult, X, Y}, By) ->
    {add, {mult, X, dydx(Y, By)}, {mult, Y, dydx(X, By)}};
dydx({add, X, Y}, By) ->
    {add, dydx(X, By), dydx(Y, By)};
dydx({divide, X, Y}, By) ->
    dydx({mult, X, {divide, 1, Y}}, By);
dydx(X, _By) when is_number(X) ->
    0;
dydx(By, By) ->
    1.

%% Excel's boolean functions work with boolean literals, strings, numbers, and
%% even dates.
%% Literal FALSE, string "FALSE" (and its variants like "fALSe"), and 0 evaluate 
%% to false. 
%% Literal true, string "TRUE" (and its variants), any other number, and any
%% date evaluate to true.
value_to_boolean(Val) when is_list(Val) ->
    case string:to_lower(Val) of
	"true" ->
	    true;
	"false" ->
	    false;
	_       ->
	    false  % FIXME: Throw an error here.
    end;
%value_to_boolean({date, Val}) ->
%    true;
value_to_boolean(0) ->
    false;
value_to_boolean(Val) when is_integer(Val) ->
    true;
value_to_boolean(Val) when is_float(Val) ->
    case ceiling(Val, 1) == 0 of
	true -> false;
	false -> true
    end;
value_to_boolean(true) ->
    true;
value_to_boolean(false) ->
    false.
