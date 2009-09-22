%%% Financial functions.
%%% <hasan@hypernumbers.com>
%%% @private
-module(stdfuns_financial).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

-export([effect/1, fv/1, ipmt/1, ispmt/1, nominal/1, npv/1, pv/1,
         sln/1, syd/1, pmt/1, rate/1, nper/1, irr/1]).

-compile(export_all).

-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

-import(muin_collect, [ col/2, col/3, col/4 ]).

db([V1, V2, V3, V4]) ->
    db([V1, V2, V3, V4, 12]);
db([_, _, _, _, _]=Args) ->
    col(Args,
        [eval_funs, first_array, fetch_name, fetch_ref, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun db_/1).

db_([Cost, _, _, _, _]) when Cost == 0; Cost =:= 0  -> 0;
db_([Cost, _Salvage, Life, Period, Month])
  when Month < 1; Month > 12; Cost < 0;
       Period > erlang:trunc(Life + (Month/12)) ->
    ?ERRVAL_NUM;

db_([Cost, Salvage, Life, Period, Month]) ->
    db1(Cost, Salvage, erlang:trunc(Life), Period, Month).
db1(Cost, Salvage, Life, Period, Month) when Period == 1 ->
    Cost - dbvalue(Cost, Salvage, Life, 1, Month);
db1(Cost, Salvage, Life, Period, Month) ->
    dbvalue(Cost, Salvage, Life, Period, Month)
        - dbvalue(Cost, Salvage, Life, Period-1, Month).

dbrate(Salvage, Cost, Life) ->
    (stdfuns_math:round([1 - math:pow( Salvage/Cost, 1/Life),3])).

dbvalue(Cost, Salvage, Life, Period, Month) when Period < 1 ->
    Cost * ( 1 - ((Month / 12) * dbrate(Salvage, Cost, Life)));
dbvalue(Cost, Salvage, Life, Period, Month) when Period =< Life ->
    dbvalue(Cost, Salvage, Life, Period-1, Month)
        * (1/dbrate(Salvage, Cost, Life));
dbvalue(_, _, _, _, _) ->
    0.

ddb([V1, V2, V3, V4]) ->
    ddb([V1, V2, V3, V4, 2]);
ddb([_, _, _, _, _]=Args) ->
    col(Args,
        [eval_funs, first_array, fetch_name, fetch_ref, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun ddb_/1).

ddb_([_Cost, _Salvage, Life, Period, Factor])
  when Life < 0; Period > Life; Factor < 0 ->
    ?ERRVAL_NUM;

ddb_([Cost, Salvage, Life, Period, Factor]) ->
    ddb1_(Cost, Salvage, Life, Period, Factor).

ddb1_(Cost, _Salvage, Life, Period, Factor) when Period =< 1 ->
    Cost * (Factor / Life);
ddb1_(Cost, Salvage, Life, Period, Factor) when Period < Life ->
    A = (Cost - ddb1_(Cost, Salvage, Life, Period-1, Factor)) * (Factor / Life),
    B = (Cost - Salvage - ddb1_(Cost, Salvage, Life, Period-1, Factor)),
    lists:min([A, B]).


irr([Range]) ->
    irr([Range, 0.1]);
irr([Range, Guess]) ->
    %% Warning, each ignore reverses the order. Odd ignores == Reverse list!!
    VS = col([Range], [eval_funs, {cast, str, num, ?ERRVAL_VAL},
                       fetch, flatten, 
                       {ignore, blank}, {ignore, str}, {ignore, bool}],
             [return_errors, {all, fun is_number/1}]),
    NGuess = col([Guess], [eval_funs, fetch, flatten, {cast, num},
                           {ignore, blank}, {ignore, str}, {ignore, bool}],
                 [return_errors]),
    case {VS, NGuess} of
        {Err_VS, _} when ?is_errval(Err_VS), 
                         ?is_rangeref(Range) -> ?ERRVAL_VAL;
        {Err_VS, _} when ?is_errval(Err_VS)  -> Err_VS;
        {_, Err_NG} when ?is_errval(Err_NG)  -> Err_NG;
        {_, [NegG]} when NegG =< -1          -> ?ERRVAL_VAL;
        {[], _    } when ?is_rangeref(Range) -> ?ERRVAL_NUM;
        {L1, L2   } when L1 == []; L2 == []  -> ?ERRVAL_VAL;
        {VS2, NG2 } -> muin_util:apply([lists:reverse(VS2), hd(NG2)], 
                                       fun irr_/2)
    end.
            

irr_(Range, Guess) ->
    %% Need both a positve and negative to compute.
    Irr = case lists:any(fun(X) -> X > 0 end, Range) and
         lists:any(fun(X) -> X < 0 end, Range) of
        false ->
            ?ERRVAL_NUM;
        true ->
            V1 = irr1(Guess, Guess * 1.1 + 0.01, Range),
            V2 = irr1(0, 1, Range),
            if is_number(V1), abs(V1 - Guess) =< abs(V2 - Guess) -> V1; 
               true                                              -> V2 end
    end,
    if is_number(Irr), Irr > 1.0e7 -> ?ERRVAL_NUM;
       true                        -> Irr end.

            
irr1(Rate0, Rate1, Range) ->
    X0 = npv1(Rate0, Range),
    X1 = npv1(Rate1, Range),
    secant(Rate1, Rate0, X1, X0, fun(N) -> npv1(N, Range) end).


effect(Args = [_, _]) ->
    [Nomrate, Npery] = ?numbers(Args, ?default_rules),
    ?ensure(Nomrate > 0, ?ERR_NUM),
    ?ensure(Npery >= 1, ?ERR_NUM),
    effect1(Nomrate, trunc(Npery)).
effect1(Nomrate, Npery) ->
    math:pow(1 + (Nomrate / Npery), Npery) - 1.

ipmt([V1, V2, V3, V4]) ->
    ipmt([V1, V2, V3, V4, 0, 0]);
ipmt([V1, V2, V3, V4, V5]) ->
    ipmt([V1, V2, V3, V4, V5, 0]);
ipmt(Args = [_, _, _, _, _, _]) ->
    [Rate, Per, Nper, Pv, Fv, Type] = ?numbers(Args, ?default_rules),
    ?ensure(Type == 0 orelse Type == 1, ?ERR_NUM),
    ipmt1(Rate, Per, Nper, Pv, Fv, Type).
ipmt1(Rate, Per, Nper, Pv, Fv, 0) ->
    Diff = Pv - Fv,
    MR = Rate * Per,
    (Diff * MR * math:pow((1 + MR), Nper)) / (math:pow((1 + MR), Nper) - 1);
ipmt1(_Rate, _Per, _Nper, _Pv, _Fv, 1) ->
    0. %% TODO:

ispmt(Args = [_, _, _, _]) ->
    [Rate, Per, Nper, Pv] = ?numbers(Args, ?default_rules),
    ispmt1(Rate, Per, Nper, Pv).
ispmt1(Rate, Per, Nper, Pv) ->
    (math:pow(1 + (Rate * Per), Nper) - 1) * Pv.

nominal(Args = [_, _]) ->
    [Effrate, Npery] = ?numbers(Args, ?default_rules),
    ?ensure(Effrate > 0, ?ERR_NUM),
    ?ensure(Npery >= 1, ?ERR_NUM),
    nominal1(Effrate, trunc(Npery)).
nominal1(Effrate, Npery) ->
    Npery * (math:pow(Effrate + 1, -Npery) - 1).

npv([Rate | Args]) ->

    NRate = col([Rate],
                [eval_funs, first_array, fetch_name, fetch_ref, cast_num],
                [return_errors, {all, fun is_number/1}]),
    
    NArgs = col(Args,
                [eval_funs, flatten_as_str, {cast, str, num, ?ERRVAL_VAL},
                 fetch_name, fetch_ref, {ignore, blank},
                 {ignore, str}, cast_num],
                [return_errors, {all, fun is_number/1}]),

    case {NRate, NArgs} of
        {Err, _} when ?is_errval(Err) -> Err;
        {_, Err} when ?is_errval(Err) -> Err;
        {[NRate2], NArgs2}            -> npv_([NRate2 | NArgs2])
    end.

npv_([Rate | Vals]) ->
    npv1(Rate, Vals).

npv1(Rate, Vals) ->
    npv1(Rate, Vals, 1, 0).
npv1(_Rate, [], _Num, Acc) ->
    Acc;
npv1(Rate, [Hd|Tl], Num, Acc) ->
    npv1(Rate, Tl, Num+1, Acc+Hd/(math:pow((1+Rate),Num))).

sln(Args = [_, _, _]) ->
    _Other = col(Args, [eval_funs, fetch, area_first, cast_num],
               [return_errors, {all, fun is_number/1}],
               fun sln1/1).
sln1([_Cost, _Salv, 0]) ->
    ?ERRVAL_DIV;
sln1([Cost, Salv, Life]) ->
    (Cost-Salv)/Life.

syd([Cost, Salv, Life, Per]) ->
    
    Life1 = col([Life], [eval_funs, fetch, area_first, {ignore, blank},
                         {cast, int}],
                [return_errors, {all, fun is_integer/1}]),
    
    Othr = col([Cost, Salv, Per],
               [eval_funs, fetch, area_first, cast_num],
               [return_errors, {all, fun is_number/1}]),

    muin_util:apply([Life1, Othr], fun syd_/2).

syd_([Life], [Cost, Salv, Per])
  when Life > 0, Per > 0, Salv >= 0, Life >= Per ->
    (Cost-Salv)*(Life-Per+1)*2/(Life*(Life+1));
syd_(_,_) ->
    ?ERRVAL_NUM.

pv([Rate, NPer, Pmt])              -> pv([Rate, NPer, Pmt, 0]);
pv([Rate, NPer, Pmt, Fv])          -> pv([Rate, NPer, Pmt, Fv, 0]);
pv([Rate, NPer, Pmt, undef, Type]) -> pv([Rate, NPer, Pmt, 0, Type]);
pv([Rate, NPer, undef, Fv, Type])  -> pv([Rate, NPer, 0, Fv, Type]);
pv(Args = [_, _, _, _, _]) ->
    col(Args,
        [first_array, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun pv_/1).

pv_([Rate, Nper, Pmt, Fv, Partype]) when Partype =/= 1, Partype =/= 0 ->
    pv_([Rate, Nper, Pmt, Fv, 1]);
pv_([Rate, _Nper, _Pmt, _Fv, _Type]) when Rate == -1 ->
    ?ERRVAL_DIV;
pv_([Rate, Nper, Pmt, Fv, _Partype]) when Rate == 0 ->
    - Fv - (Pmt * Nper);
pv_([Rate, Nper, Pmt, Fv, Type]) ->
    Tmp = math:pow((1+Rate),Nper),
    (0 - Fv - Pmt * (1+Rate*Type)* (Tmp - 1)/Rate) / (Tmp).


fv([Rate, NPer, Pmt])              -> fv([Rate, NPer, Pmt, 0]);
fv([Rate, NPer, Pmt, Pv])          -> fv([Rate, NPer, Pmt, Pv, 0]);
fv([Rate, NPer, Pmt, undef, Type]) -> fv([Rate, NPer, Pmt, 0, Type]);
fv(Args = [_, _, _, _, _]) ->
    col(Args,
        [area_first, {conv, blank, 0}, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun fv_/1).

fv_([Rate, Nper, Pmt, Pv, Type]) when Type =/= 1, Type =/= 0 ->
    fv_([Rate, Nper, Pmt, Pv, 1]);
fv_([0, Nper, Pmt, Pv, _Type]) ->
    -1 * (Pv + Pmt * Nper);
fv_([Rate, Nper, Pmt, Pv, Type]) ->
    Tmp = math:pow(1+Rate, Nper),
    (Pmt * (1+Rate*Type) * (1-Tmp)/Rate) - Pv * Tmp.

           
pmt([A, B, C, D]) -> pmt([A, B, C, D, 0]);
pmt([A, B, C])    -> pmt([A, B, C, 0, 0]);
pmt(Args = [_, _, _, _, _]) ->
    col(Args,
        [area_first, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun pmt_/1).

pmt_([Rate, Nper, Pmt, Pv, Type]) when Type =/= 1, Type =/= 0 ->
    pmt_([Rate, Nper, Pmt, Pv, 1]);
pmt_([_Rate, 0, _Pv, _Fv, _Type]) ->
    ?ERRVAL_DIV;
pmt_([0, Nper, Pv, Fv, _Type]) ->
    -1 * (Pv + Fv) / Nper;
pmt_([Rate, Nper, Pv, Fv, Type]) ->
    Tmp = math:pow(1+Rate, Nper),
    (Rate*(Fv+Pv*Tmp)) / ((1+Rate*Type) * (1 - Tmp)).


nper([Rate, Pmt, Pv])     -> nper([Rate, Pmt, Pv, 0, 0]);
nper([Rate, Pmt, Pv, Fv]) -> nper([Rate, Pmt, Pv, Fv, 0]);
nper(Args = [_, _, _, _, _]) ->
    col(Args,
        [first_array, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun nper_/1).

nper_([_Rate, 0, _Pv, _Fv, _Type]) ->
    ?ERRVAL_NUM;
nper_([0, Pmt, Pv, Fv, _Type]) ->
    -1 * (Pv + Fv) / Pmt;
nper_([Rate, Pmt, Pv, Fv, Type]) ->
    Tmp = 1+Rate*Type,
    Numer = (Pmt*Tmp -Fv*Rate)/(Pmt*Tmp+Pv*Rate),
    case (Numer =< 0) or ((1+Rate) =< 0) of
        true -> ?ERRVAL_NUM;
        false -> math:log10(Numer) / math:log10(1+Rate)
    end.

rate([NPer, Pmt, Pv])              -> rate([NPer, Pmt, Pv, 0]);
rate([NPer, Pmt, Pv, Fv])          -> rate([NPer, Pmt, Pv, Fv, 0]);
rate([NPer, Pmt, Pv, Fv, Type])    -> rate([NPer, Pmt, Pv, Fv, Type, 0.1]);

rate([NPer, Pmt, Pv, undef, Type, Guess]) ->
    rate([NPer, Pmt, Pv, 0, Type, Guess]);

rate(Args = [_, _, _, _, _, _]) ->
    col(Args,
        [area_first, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun rate_/1).

rate_([Nper, _Pmt, _Pv, _Fv, _Type, _Guess]) when Nper =< 1 ->
    ?ERRVAL_NUM;
rate_([_Nper, _Pmt, _Pv, _Fv, _Type, Guess]) when Guess =< -1 ->
    ?ERRVAL_VAL;
rate_([Nper, Pmt, Pv, Fv, Type, Guess]) ->
    Rate0 = Guess,
    Rate1 = Guess * 1.1 + 0.01,
    X0 = xn(Pmt, Rate0, Nper, Pv, Fv, Type),
    X1 = xn(Pmt, Rate1, Nper, Pv, Fv, Type),
    secant(Rate1, Rate0, X1, X0,
           fun(N) -> xn(Pmt, N, Nper, Pv, Fv, Type) end).

             
%%% helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-define(ITERATION_LIMIT, 100).
-define(EPSILON,  0.0000001).

%% Calculate next approximation of value based on two previous approximations.
secant(Pa, Ppa, Px, Ppx, Fun) ->
    secant(Pa, Ppa, Px, Ppx, Fun, 1).
secant(_, _, _, _, _, ?ITERATION_LIMIT) ->
    ?ERRVAL_NUM;
secant(Pa, Ppa, Px, Ppx, Fun, I) ->
    Divisor = (Px - Ppx),
    case Divisor of
        X when X == 0 -> 
            ?ERRVAL_NUM; % floats cast to integer...
        _ -> Ca = Pa - (Px * (Pa - Ppa))/Divisor,
             Xn = Fun(Ca),
             if abs(Xn) =< ?EPSILON -> Ca;
                true -> secant(Ca, Pa, Xn, Px, Fun, I + 1)
             end
    end.

%% Calculate ?(X) given Pmt for current iteration of one of the arguments.
xn(Pmt, Rate, Nper, Pv, Fv, _Type) when Rate == 0 ->
    Pv + Fv + (Pmt * Nper);
xn(Pmt, Rate, Nper, Pv, Fv, Partype) ->
    Tmp = math:pow(1+Rate, Nper),
    Pv * Tmp + Pmt * (1+Rate*Partype) * (Tmp-1) / Rate + Fv.


%%% tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").

cmp(F1, F2) ->
    erlang:abs(1 - F1 / F2) < 0.01.

pmt_test_() ->
    [
     ?_assert(cmp(pmt([1, 2, 3, 4, 5]), -2.67)),
     ?_assert(cmp(pmt([11.1, 2, 3, 4, 5]),-2.80)),
     ?_assert(cmp(pmt([-1.2, 2, 3, 4, 5]), 25.75))
    ].

rate_test_() ->
    [
     ?_assert(cmp(rate([24, -250, 5000]), 0.015130844)),
     ?_assert(cmp(rate([208, -700, 8000, undef, 1]), 0.09589041)),
     ?_assert(cmp(rate([10, -1000, 6500]), 0.087113756))
    ].

nper_test_() ->
    [
     ?_assert(cmp(nper([11, 234, 567, 1]), -1.35535)),
     ?_assert(cmp(nper([0, 234, 567, 1]), -2.42)),
     ?_assert(cmp(nper([0.00625, -150, 5000]), 37.4951))
    ].

fv_test_() ->
    [
     ?_assert(cmp(fv([0.075/12, 2*12, -250, -5000, 1]), 12298.46)),
     ?_assert(cmp(fv([0.06/52, 4*52, -50, -8000, 0]), 21915.09))
    ].

pv_test_() ->
    [
     ?_assert(cmp(pv([0.12, 48, 1000, 1234, 1]), -9298.18)),
     ?_assert(cmp(pv([0.00625, 24, 250, undef, 0]), -5555.61)),
     ?_assert(cmp(pv([0.06/52, 4*52, 50, undef, 1]), -9252.07))
    ].
