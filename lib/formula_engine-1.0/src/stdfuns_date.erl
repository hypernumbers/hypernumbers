%%%----------------------------------------------------------------------------
%%% @doc Date functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%----------------------------------------------------------------------------

-module(stdfuns_date).
-export([date/1,
         datedif/1,
         datevalue/1,
         day/1,
         month/1,
         year/1,
         edate/1,
         eomonth/1,
         networkdays/1,
         today/1,
         weekday/1,
         weeknum/1,
         hour/1,
         minute/1,
         second/1,
         now/1,
         timevalue/1]).

-include("typechecks.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").

-define(else, true). % silly hack for ifs

date(Arg = [_, _, _]) ->
    [Year, Month, Day] = ?numbers(Arg, [cast_strings,
                                        cast_bools,
                                        zero_blanks]),
    #datetime{date = {Year, Month, Day}}.

datedif([V1, V2, V3]) ->
    [Start, End] = ?dates([V1, V2], [cast_strings]),
    ?ensure(muin_date:gt(End, Start), ?ERR_NUM),
    ?estring(V3),
    Unit = string:to_upper(?utf8l(V3)),
    datedif1(Start, End, Unit).
datedif1(Start, End, "Y") ->
    #datetime{date = {Startyr, Startmo, Startday}} = Start,
    #datetime{date = {Endyr, Endmo, Endday}} = End,
    Maxyrs = Endyr - Startyr,
    if Endmo > Startmo ->
            Maxyrs;
       Endmo == Startmo ->
            if Endday >= Startday ->
                    Maxyrs;
               ?else ->
                    Maxyrs - 1
            end;
       ?else ->
            Maxyrs - 1
    end;
datedif1(Start, End, "M") ->
    Startmos = 12 * muin_date:year(Start) + muin_date:month(Start),
    Endmos = 12 * muin_date:year(End) + muin_date:month(End),
    Maxmos = Endmos - Startmos,
    case muin_date:day(End) >= muin_date:day(Start) of
        true -> Maxmos;
        false -> Maxmos - 1
    end;
datedif1(Start, End, "D") ->
    muin_date:dtdiff(Start, End);
datedif1(Start, End, "MD") ->
    Startday = muin_date:day(Start),
    Endday = muin_date:day(End),
    if Endday >= Startday ->
            Endday - Startday;
       ?else ->
            31 - (Startday - Endday)
    end;
datedif1(Start, End, "YM") ->
    Startmo = muin_date:month(Start),
    Endmo = muin_date:month(End),
    if Endmo >= Startmo ->
            Endmo - Startmo;
       ?else ->
            12 - (Startmo - Endmo)
    end;
datedif1(Start, End, "YD") ->
    foldl(fun(X, Acc) ->
                  Acc + calendar:last_day_of_the_month(muin_date:year(Start), X)
          end,
          0,
          seq(muin_date:month(Start), muin_date:month(End))).

datevalue([V1]) ->
    ?date(V1, [cast_strings]).

day([Dt]) ->
    ?edate(Dt),
    muin_date:day(Dt).

month([Dt]) ->
    ?edate(Dt),
    muin_date:month(Dt).

year([Dt]) ->
    ?edate(Dt),
    muin_date:year(Dt).

edate([V1, V2]) ->
    Start = ?date(V1, [cast_strings]),
    Months = ?int(V2, [cast_bools,
                       cast_strings,
                       zero_blanks]),
    edate1(Start, Months).
edate1(Start, Months) ->
    #datetime{date = {Startyr, Startmo, Startday}} = Start,
    Yrdelta = Months / 12,
    Modelta = Months - (Yrdelta * 12),
    Start#datetime{date = {Startyr + Yrdelta, Startmo + Modelta, Startday}}.

eomonth([V1, V2]) ->
    Start = ?date(V1, [cast_strings]),
    Months = ?int(V2, [cast_strings,
                       cast_bools,
                       zero_blanks]),
    %% TODO: More error checks, see Excel.
    eomonth1(Start, Months).
eomonth1(Start, Months) ->
    Tdate = edate1(Start, Months),
    #datetime{date = {Tyr, Tmo, _Tday}} = Tdate,
    Lastday = calendar:last_day_of_the_month(Tyr, Tmo),
    Tdate#datetime{date = {Tyr, Tmo, Lastday}}.

%% TODO: Can provide lists of holidays here for convenience, e.g.
%% "GB" -> British holidays, "RUS" -> Russian holidays etc.
networkdays([V1, V2]) ->
    networkdays([V1, V2, []]);
networkdays([V1, V2, V3]) ->
    [Start, End] = ?dates([V1, V2], [cast_strings]),
    Holidays = case V3 of
                   [] -> [];
                   _  -> ?dates(V3, [cast_strings])
               end,
    networkdays1(Start, End, Holidays).
networkdays1(Start, End, Holidays) ->
    muin_date:foldl(fun(X, Acc) ->
                            case calendar:day_of_the_week(X#datetime.date) of
                                6 -> Acc; % Saturday
                                7 -> Acc; % Sunday
                                _ ->      % Check if the date falls on holiday
                                    %% FIXME: Will break on #datetimes with the same date
                                    %% but different time fields.
                                    case member(X, Holidays) of
                                        true -> Acc;
                                        false -> Acc + 1
                                    end
                            end
                    end,
                    0, Start, End).

%% TODO: Potential for confusion if user & server are in different timezones, e.g.
%% a Japanese user on a Californian server...
today([]) ->
    {Date, _Time} = calendar:now_to_universal_time(erlang:now()),
    #datetime{date = Date}.

weekday([V1]) ->
    weekday([V1, 1]);
weekday([V1, V2]) ->
    Dt = ?date(V1, [cast_strings]),
    Rettype = ?int(V2, [cast_strings, cast_bools]),
    ?ensure(Rettype == 1 orelse Rettype == 2 orelse Rettype == 3,
            ?ERR_NUM),
    weekday1(Dt, Rettype).
weekday1(Dt, 1) ->
    case (calendar:day_of_the_week(Dt#datetime.date) + 1) rem 7 of
        0 -> 7;
        N -> N
    end;
weekday1(Dt, 2) ->
    calendar:day_of_the_week(Dt#datetime.date);
weekday1(Dt, 3) ->
    calendar:day_of_the_week(Dt#datetime.date) - 1.

weeknum([V1]) ->
    weeknum([V1, 1]);
weeknum([V1, V2]) ->
    Dt = ?date(V1, [cast_strings]),
    Rettype = ?int(V2, [cast_strings, cast_bools]),
    ?ensure(Rettype == 1 orelse Rettype == 2,
            ?ERR_NUM),
    weeknum1(Dt, Rettype).
weeknum1(Dt, Rettype) ->
    muin_date:foldl(fun(X, Acc) ->
                            case weekday1(X, Rettype) of
                                1 -> Acc + 1; % Sunday for Rettype = 1, Monday for 2.
                                _ -> Acc
                            end
                    end,
                    1,
                    #datetime{date = {muin_date:year(Dt), 1, 1}},
                    #datetime{date = {muin_date:year(Dt), 12, 31}}).

hour([V1]) ->
    Dt = ?date(V1, [cast_strings]),
    muin_date:hour(Dt).

minute([V1]) ->
    Dt = ?date(V1, [cast_strings]),
    muin_date:minute(Dt).

second([V1]) ->
    Dt = ?date(V1, [cast_strings]),
    muin_date:second(Dt).

now([]) ->
    {Date, Time} = calendar:now_to_universal_time(erlang:now()),
    #datetime{date = Date, time = Time}.

timevalue([V1]) ->
    ?date(V1, [cast_strings]).

    
%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").

datedif_test_() ->
    [
     %% Years
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "Y") == 1),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "Y") == 0),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "Y") == 1),

     %% Months
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "M") == 12),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "M") == 11),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "M") == 12),

     %% Days
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "D") == 365),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "D") == 364),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "D") == 366),

     %% Days only
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "MD") == 0),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "MD") == 30),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "MD") == 1),

     %% Months only
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "YM") == 0),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "YM") == 11),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "YM") == 0),
     
     %% Days -- years ignorned
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 17}},
%%                        "YD") == 0),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "YD") == 364),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 18}},
%%                        "YD") == 1),

     ?_assert(true)
    ].
