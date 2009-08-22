%%% Financial functions.
%%% <hasan@hypernumbers.com>
%%% @private

-module(stdfuns_financial).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

-export([effect/1, fv/1, ipmt/1, ispmt/1, nominal/1, npv/1, pv/1,
         sln/1, syd/1, pmt/1, rate/1, nper/1]).
-compile(export_all).
-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

-import(muin_collect, [ col/2, col/3, col/4 ]).

%% db([V1, V2, V3, V4]) ->
%%     "the function db has an infinite loop!";
%%     % db([V1, V2, V3, V4, 12]);
%% db([V1, V2, V3, V4, V5]) ->
%%     [Cost, Salvage, Life, Period, Month] = ?numbers([V1, V2, V3, V4, V5],
%%                                                     ?default_rules),
%%     ?ensure((Month >= 1) andalso (Month =< 12), ?ERR_NUM), 
%%     "the function db has an infinite loop!".
%%     % db1(Cost, Salvage, Life, Period, Month).
-define(dbrate,
        (stdfuns_math:round1(1 - math:pow(Salvage / Cost, 1 / Life), 3))).
db1(Cost, Salvage, Life, Life, Month) -> % Last period
    io:format("in stdfuns_financial:db1 (1)~n"),
    Prevdepr = foldl(fun(X, Acc) ->
                             Acc + db1(Cost, Salvage, Life, X, Month)
                     end,
                     0, seq(1, Life - 1)),
    ((Cost - Prevdepr) * ?dbrate * (12 - Month)) / 12;
db1(Cost, Salvage, Life, 1, Month) -> % First period
    io:format("in stdfuns_financial:db1 (2)~n"),
    Cost * ?dbrate * Month / 12;
db1(Cost, Salvage, Life, Period, Month) -> % Some other period
    io:format("in stdfuns_financial:db1 (3)~n"),
    Prevdepr = foldl(fun(X, Acc) ->
                             Acc + db1(Cost, Salvage, Life, X, Month)
                     end,
                     0, seq(1, Period - 1)),
    (Cost - Prevdepr) * ?dbrate.

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
        {[NRate2], NArgs2} ->
            npv_([NRate2 | NArgs2])
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
    [Cost, Salv, Life] = ?numbers(Args, ?default_rules),
    sln1(Cost, Salv, Life).
sln1(Cost, Salv, Life) ->
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

pv([Rate, NPer, Pmt]) ->
    pv([Rate, NPer, Pmt, 0]);
pv([Rate, NPer, Pmt, Fv]) ->
    pv([Rate, NPer, Pmt, Fv, 0]);

pv(Args = [_, _, _, _, _]) ->
    col(Args,
        [first_array, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun pv_/1).

pv_([Rate, Nper, Pmt, Fv, _Partype]) when Rate == 0 ->
    - Fv - (Pmt * Nper);
pv_([Rate, Nper, Pmt, Fv, Type]) ->
    Tmp = math:pow((1+Rate),Nper),
    (0 - Fv - Pmt * (1+Rate*Type)* (Tmp - 1)/Rate) / (Tmp).

fv(Args = [_, _, _, _, _]) ->
    [Rate, Nper, Pmt, Pv, Partype] = ?numbers(Args, ?default_rules),
    Fv0 = 0,
    Fv1 = 1000,
    X0 = xn(Pmt, Rate, Nper, Pv, Fv0, Partype),
    X1 = xn(Pmt, Rate, Nper, Pv, Fv1, Partype),
    secant(Fv1, Fv0, X1, X0,
           fun(N) -> xn(Pmt, Rate, Nper, Pv, N, Partype) end).
           
pmt([A, B, C, D]) -> pmt([A, B, C, D, 0]);
pmt([A, B, C])    -> pmt([A, B, C, 0, 0]);
pmt(Args = [_, _, _, _, _]) ->
    [Rate, Nper, Pv, Fv, Partype] = ?numbers(Args, ?default_rules),
    Pmt0 = -Pv*Rate*(Nper/12)/Nper,
    Pmt1 = Pmt0 - 50,
    X0 = xn(Pmt0, Rate, Nper, Pv, Fv, Partype),
    X1 = xn(Pmt1, Rate, Nper, Pv, Fv, Partype),
    secant(Pmt1, Pmt0, X1, X0,
           fun(N) -> xn(N, Rate, Nper, Pv, Fv, Partype) end).

rate([NPer, Pmt, PV]) ->
    rate([NPer, Pmt, PV, 0]);
rate([NPer, Pmt, PV, Fv]) ->
    rate([NPer, Pmt, PV, Fv, 0]);
rate([NPer, Pmt, PV, Fv, Type]) ->
    rate([NPer, Pmt, PV, Fv, Type, 0.1]);

rate(Args = [_, _, _, _, _, _]) ->
    col(Args,
        [first_array, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun rate_/1).

rate_([Nper, Pmt, Pv, Fv, Type, _Est]) ->
    Rate0 = 0.01,
    Rate1 = 0.09,
    X0 = xn(Pmt, Rate0, Nper, Pv, Fv, Type),
    X1 = xn(Pmt, Rate1, Nper, Pv, Fv, Type),
    secant(Rate1, Rate0, X1, X0,
           fun(N) -> xn(Pmt, N, Nper, Pv, Fv, Type) end).

nper(Args = [_, _, _, _, _]) ->
    col(Args,
        [first_array, cast_num],
        [return_errors, {all, fun is_number/1}],
        fun nper_/1).

nper_([Rate, Pmt, Pv, Fv, Partype]) ->
    Nper0 = 10,
    Nper1 = 16,
    X0 = xn(Pmt, Rate, Nper0, Pv, Fv, Partype),
    X1 = xn(Pmt, Rate, Nper1, Pv, Fv, Partype),
    secant(Nper1, Nper0, X1, X0,
           fun(N) -> xn(Pmt, Rate, N, Pv, Fv, Partype) end).

%%% helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-define(ITERATION_LIMIT, 20).
%% Calculate next approximation of value based on two previous approximations.
secant(Pa, Ppa, Px, Ppx, Fun) ->
    secant(Pa, Ppa, Px, Ppx, Fun, 1).
secant(_, _, _, _, _, ?ITERATION_LIMIT) ->
    ?ERRVAL_NUM;
secant(Pa, Ppa, Px, Ppx, Fun, I) ->
    Divisor = (Px-Ppx)*Px,
    case Divisor of
        X when X == 0 orelse X == 0.0 -> ?ERRVAL_DIV;
        _             ->
            Ca = Pa-(Pa-Ppa)/Divisor,
            Xn = Fun(Ca),
            case Xn of
                0 -> Ca;
                _ -> secant(Ca, Pa, Xn, Px, Fun, I+1)
            end
    end.

%% Calculate ?(X) given Pmt for current iteration of one of the arguments.
xn(Pmt, Rate, Nper, Pv, Fv, Partype) ->
    Tmp = math:pow(1+Rate, Nper),
    Pv * Tmp + Pmt * (1+Rate*Partype) * (Tmp-1) / Rate + Fv.

