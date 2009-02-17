%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Date functions.
%%% @private

-module(stdfuns_date).
-export([date/1,
         dateh/1,
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

-define(cast_all, [cast_strings, cast_bools, cast_blanks, cast_numbers]). %% FIXME: This is NOT the right thing to do.

-define(last_day, calendar:last_day_of_the_month).

date(Arg = [_, _, _]) ->
    [Year, Month, Day] = ?ints(Arg, ?cast_all),
    io:format("Arg is ~p~nY/M/D is ~p ~p ~p~n",[Arg, Year, Month, Day]),
    if
        Year <  1900  -> dateh([Year+1900,Month,Day]);
        Year >= 1900  -> dateh([Year,Month,Day])
    end.

dateh([Y, M, D]) when (M =< 0) ->
    Diff = abs(M) rem 12,
    Offset = (abs(M) - Diff)/12,
    NewYear = erlang:trunc(Y - Offset - 1),
    NewMonth = 12 - Diff,
    io:format("in dateh (1) In  Y/M/D is ~p ~p ~p~n", [Y, M, D]),
    io:format("in dateh (1) Out Y/M/D is ~p ~p ~p~n", [NewYear, NewMonth, D]),
    dateh([NewYear, NewMonth, D]);
dateh([Y, M, D]) when (M > 12) ->
    Diff = M rem 12,
    Offset = erlang:trunc((M - Diff)/12),
    NewYear = Y + Offset,
    NewMonth = Diff,
    io:format("in dateh (2) In  Y/M/D is ~p ~p ~p~n", [Y, M, D]),
    io:format("in dateh (2) Out Y/M/D is ~p ~p ~p~n", [NewYear, NewMonth, D]),
    dateh([NewYear, NewMonth, D]);
dateh([Y, M, D]) when (D =< 0) ->
    NewMonth = M - 1, 
    LastDay2 =  ?last_day(Y, NewMonth),
    NewDay = LastDay2 + D,
    io:format("in dateh (3) In  Y/M/D is ~p ~p ~p~n", [Y, M, D]),
    io:format("in dateh (3) Out Y/M/D is ~p ~p ~p~n", [Y, NewMonth, NewDay]),
    dateh([Y, NewMonth, NewDay]);
dateh([Y, M, D]) ->
    % this is a key one - we don't know if D is too big (yet):
    % * 30 is too big if it is February
    % * 30 is small enough if it is January
    LastDay = ?last_day(Y, M),
    if
        (D > LastDay)  -> NewMonth = M + 1,
                          NewDay = D - LastDay,
                          io:format("in dateh (4a) In  Y/M/D is ~p ~p ~p~n",
                                   [Y, M, D]),
                          io:format("in dateh (4a) Out Y/M/D is ~p ~p ~p~n",
                                   [Y, NewMonth, NewDay]),
                          dateh([Y, NewMonth, NewDay]);
        (D =< LastDay) -> io:format("in dateh (4b) Exiting with  Y/M/D is ~p ~p ~p~n",
                                    [Y, M, D]),
                          #datetime{date = {Y, M, D}}
    end.

%% @TODO what is this function for? Not an Excel function - ask Hasan...
datedif([V1, V2, V3]) ->
    [Start, End] = ?dates([V1, V2], ?cast_all),
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
    ?date(V1, ?cast_all).

day([Dt]) ->
    ?edate(Dt),
    muin_date:day(Dt).

month([Dt]) ->
    ?edate(Dt),
    muin_date:month(Dt).

year([Dt]) ->
    io:format("In year Dt is ~p~n", [Dt]),
    ?edate(Dt),
    muin_date:year(Dt).

%% @todo there is not test suite for this function because it is not an 
%% Excel 97 one
edate([V1, V2]) ->
    Start = ?date(V1, ?cast_all),
    Months = ?int(V2, ?cast_all),
    edate1(Start, Months).
edate1(Start, Months) ->
    #datetime{date = {Startyr, Startmo, Startday}} = Start,
    Yrdelta = Months / 12,
    Modelta = Months - (Yrdelta * 12),
    Start#datetime{date = {Startyr + Yrdelta, Startmo + Modelta, Startday}}.

%% @todo there is not test suite for this function because it is not an 
%% Excel 97 one
eomonth([V1, V2]) ->
    Start = ?date(V1, ?cast_all),
    Months = ?int(V2, ?cast_all),
    %% TODO: More error checks, see Excel.
    eomonth1(Start, Months).
eomonth1(Start, Months) ->
    Tdate = edate1(Start, Months),
    #datetime{date = {Tyr, Tmo, _Tday}} = Tdate,
    Lastday = calendar:last_day_of_the_month(Tyr, Tmo),
    Tdate#datetime{date = {Tyr, Tmo, Lastday}}.

%% @todo there is not test suite for this function because it is not an 
%% Excel 97 one
%% 
%% Also caan provide lists of holidays here for convenience, e.g.
%% "GB" -> British holidays, "RUS" -> Russian holidays etc.
networkdays([V1, V2]) ->
    networkdays([V1, V2, []]);
networkdays([V1, V2, V3]) ->
    [Start, End] = ?dates([V1, V2], ?cast_all),
    Holidays = case V3 of
                   [] -> [];
                   _  -> ?dates(V3, ?cast_all)
               end,
    networkdays1(Start, End, Holidays).
networkdays1(Start, End, Holidays) ->
    muin_date:walk(fun(X, Acc) ->
                           case calendar:day_of_the_week(X#datetime.date) of
                               6 -> Acc; % Saturday
                               7 -> Acc; % Sunday
                               _ ->      % Check if the date falls on holiday
                                   %% FIXME: Will break on #datetimes with 
                                   %% the same date
                                   %% but different time fields.
                                   case member(X, Holidays) of
                                       true -> Acc;
                                       false -> Acc + 1
                                   end
                           end
                   end,
                   0, Start, End).

%% @TODO: Potential for confusion if user and server are in 
%% different timezones, e.g.
%% a Japanese user on a Californian server...
today([]) ->
    {Date, _Time} = calendar:now_to_universal_time(erlang:now()),
    #datetime{date = Date}.

weekday([V1]) ->
    weekday([V1, 1]);
weekday([V1, V2]) ->
    Dt = ?date(V1, ?cast_all),
    Rettype = ?int(V2, ?cast_all),
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

%% @todo there is not test suite for this function because it is not an 
%% Excel 97 one
weeknum([V1]) ->
    weeknum([V1, 1]);
weeknum([V1, V2]) ->
    Dt = ?date(V1, ?cast_all),
    Rettype = ?int(V2, ?cast_all),
    ?ensure(Rettype == 1 orelse Rettype == 2,
            ?ERR_NUM),
    weeknum1(Dt, Rettype).
weeknum1(Dt, Rettype) ->
    muin_date:walk(fun(X, Acc) ->
                           case weekday1(X, Rettype) of
                               1 -> Acc + 1; % Sunday for Rettype = 1, Monday for 2.
                               _ -> Acc
                           end
                   end,
                   1,
                   #datetime{date = {muin_date:year(Dt), 1, 1}},
                   #datetime{date = {muin_date:year(Dt), 12, 31}}).

hour([V1]) ->
    Dt = ?date(V1, ?cast_all),
    muin_date:hour(Dt).

minute([V1]) ->
    Dt = ?date(V1, ?cast_all),
    muin_date:minute(Dt).

second([V1]) ->
    Dt = ?date(V1, ?cast_all),
    muin_date:second(Dt).

now([]) ->
    {Date, Time} = calendar:now_to_universal_time(erlang:now()),
    #datetime{date = Date, time = Time}.

timevalue([V1]) ->
    ?date(V1, ?cast_all).

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
