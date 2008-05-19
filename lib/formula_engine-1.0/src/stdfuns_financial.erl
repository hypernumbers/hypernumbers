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
         pv/1
        ]).

db([Cost, Salvage, Life, Period]) ->
    db([Cost, Salvage, Life, Period, 12]);
db([Cost, Salvage, Life, Period, Month]) ->
    ?ensure_numbers([Cost, Salvage, Life, Period, Month]),
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

effect([Nomrate, Npery]) ->
    ?ensure_numbers([Nomrate, Npery]),
    ?ensure_positive(Nomrate),
    ?ensure(Npery >= 1, ?ERR_NUM),
    effect1(Nomrate, trunc(Npery)).
effect1(Nomrate, Npery) ->
    math:pow(1 + (Nomrate / Npery), Npery) - 1.

fv([Rate, Nper, Pmt, Pv]) ->
    fv([Rate, Nper, Pmt, Pv, 0]);
fv([Rate, Nper, Pmt, Pv, Type]) ->
    ?ensure_numbers([Rate, Nper, Pmt, Pv, Type]),
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

ipmt([Rate, Per, Nper, Pv]) ->
    ipmt([Rate, Per, Nper, Pv, 0, 0]);
ipmt([Rate, Per, Nper, Pv, Fv]) ->
    ipmt([Rate, Per, Nper, Pv, Fv, 0]);
ipmt([Rate, Per, Nper, Pv, Fv, Type]) ->
    ?ensure_numbers([Rate, Per, Nper, Pv, Fv, Type]),
    ?ensure(Type == 0 orelse Type == 1, ?ERR_NUM),
    ipmt1(Rate, Per, Nper, Pv, Fv, Type).
ipmt1(Rate, Per, Nper, Pv, Fv, 0) ->
    Diff = Pv - Fv,
    MR = Rate * Per,
    (Diff * MR * math:pow((1 + MR), Nper)) / (math:pow((1 + MR), Nper) - 1);
ipmt1(_Rate, _Per, _Nper, _Pv, _Fv, 1) ->
    0. %% TODO:

ispmt([Rate, Per, Nper, Pv]) ->
    ?ensure_numbers([Rate, Per, Nper, Pv]),
    ispmt1(Rate, Per, Nper, Pv).
ispmt1(Rate, Per, Nper, Pv) ->
    (math:pow(1 + (Rate * Per), Nper) - 1) * Pv.

nominal([Effrate, Npery]) ->
    ?ensure_numbers([Effrate, Npery]),
    ?ensure_positive(Effrate),
    ?ensure(Npery >= 1, ?ERR_NUM),
    nominal1(Effrate, trunc(Npery)).
nominal1(Effrate, Npery) ->
    Npery * (math:pow(Effrate + 1, -Npery) - 1).

pv([Rate, Nper, Pmt, Fv, Type]) ->
    ?ensure_numbers([Rate, Nper, Pmt, Fv, Type]),
    pv1(Rate, Nper, Pmt, Fv, Type).
pv1(0, Nper, Pmt, Fv, _Type) ->
    -(Pmt * Nper + Fv);
pv1(Rate, Nper, Pmt, Fv, Type) ->
    N = math:pow(Rate + 1, Nper),
    Bigass = (N - 1) / Rate,
    T = -(Pmt * (Rate * Type + 1) * Bigass + Fv),
    T / N.
    
