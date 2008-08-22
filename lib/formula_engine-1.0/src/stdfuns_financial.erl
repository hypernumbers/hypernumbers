%%% Financial functions.
%%% <hasan@hypernumbers.com>

-module(stdfuns_financial).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

-export([
         db/1,
         effect/1,
         fv/1,
         ipmt/1,
         ispmt/1,
         nominal/1,
         npv/1,
         pv/1,
         sln/1,
         syd/1
        ]).
-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

db([V1, V2, V3, V4]) ->
    db([V1, V2, V3, V4, 12]);
db([V1, V2, V3, V4, V5]) ->
    [Cost, Salvage, Life, Period, Month] = ?numbers([V1, V2, V3, V4, V5],
                                                    ?default_rules),
    ?ensure((Month >= 1) andalso (Month =< 12), ?ERR_NUM), 
    db1(Cost, Salvage, Life, Period, Month).
-define(dbrate,
        (stdfuns_math:round1(1 - math:pow(Salvage / Cost, 1 / Life), 3))).
db1(Cost, Salvage, Life, Life, Month) -> % Last period
    Prevdepr = foldl(fun(X, Acc) ->
                             Acc + db1(Cost, Salvage, Life, X, Month)
                     end,
                     0, seq(1, Life - 1)),
    ((Cost - Prevdepr) * ?dbrate * (12 - Month)) / 12;
db1(Cost, Salvage, Life, 1, Month) -> % First period
    Cost * ?dbrate * Month / 12;
db1(Cost, Salvage, Life, Period, Month) -> % Some other period
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

fv([V1, V2, V3, V4]) ->
    fv([V1, V2, V3, V4, 0]);
fv(Args = [_, _, _, _, _]) ->
    [Rate, Nper, Pmt, Pv, Type] = ?numbers(Args, ?default_rules),
    ?ensure(Type == 0 orelse Type == 1, ?ERR_NUM),
    fv1(Rate, Nper, Pmt, Pv, Type).
fv1(_Rate, 0, _Pmt, Pv, 0) ->
    Pv;
fv1(Rate, Nper, Pmt, Pv, 0) ->
    fv1(Rate, Nper - 1, Pmt, (Pv * Rate) + Pmt, 0);
fv1(_Rate, 0, _Pmt, Pv, 1) ->
    Pv;
fv1(Rate, Nper, Pmt, Pv, 1) ->
    fv1(Rate, Nper - 1, Pmt, (Pv + Pmt) * Rate, 1).

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

npv([V1, V2 | Tl]) ->
    Rate = ?number(V1, ?default_rules),
    Vals = ?numbers([V2|Tl], ?default_rules), % TODO: cast_string_zero
    npv1(Rate, Vals).
npv1(Rate, Vals) ->
    npv1(Rate, Vals, 1, 0).
npv1(_Rate, [], _Num, Acc) ->
    Acc;
npv1(Rate, [Hd|Tl], Num, Acc) ->
    npv1(Rate, Tl, Num+1, Acc+Hd/(math:pow((1+Rate),Num))).
    
pv(Args = [_, _, _, _, _]) ->
    [Rate, Nper, Pmt, Fv, Type] = ?numbers(Args, ?default_rules),
    pv1(Rate, Nper, Pmt, Fv, Type).
pv1(0, Nper, Pmt, Fv, _Type) ->
    -(Pmt * Nper + Fv);
pv1(Rate, Nper, Pmt, Fv, Type) ->
    N = math:pow(Rate + 1, Nper),
    Bigass = (N - 1) / Rate,
    T = -(Pmt * (Rate * Type + 1) * Bigass + Fv),
    T / N.

sln(Args = [_, _, _]) ->
    [Cost, Salv, Life] = ?numbers(Args, ?default_rules),
    sln1(Cost, Salv, Life).
sln1(Cost, Salv, Life) ->
    (Cost-Salv)/Life.

syd(Args = [_, _, _, _]) ->
    [Cost, Salv, Life, Per] = ?numbers(Args, ?default_rules),
    syd1(Cost, Salv, Life, Per).
syd1(Cost, Salv, Life, Per) ->
    (Cost-Salv)*(Life-Per+1)*2/(Life*(Life+1)).
