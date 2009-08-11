%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Date functions.
%%% @private

-module(stdfuns_date).
-export([date/1,
         dateh/1,
         datedif/1,
         datevalue/1,
         day/1,
         days360/1,
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
         timevalue/1,
         time/1]).

-import(muin_collect, [ collect/3, col/3, col/4 ]).

-include("typechecks.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").

%% FIXME: This is NOT the right thing to do.
-define(cast_all, [first_array, cast_strings, cast_bools, cast_blanks,
                   cast_numbers]).

-define(last_day, calendar:last_day_of_the_month).

date(Args = [_, _, _]) ->

    % bit insane, excel rounds negative floats and truncs positive ones
    Rnd = fun(X) when X >= 0 -> erlang:trunc(X);
             (X) when X < 0  -> erlang:round(X)
          end,
    
    F = fun(Y, M, D) ->
                dateh([erlang:trunc(Y), Rnd(M), Rnd(D)])
        end,

    case col(Args, [flatten_as_str, {cast, num}],
             [return_errors, {all, fun is_number/1}]) of
        Err when ?is_errval(Err) -> Err;
        [Y, M, D] when Y < 1900  -> F(Y+1900, M, D);
        [Y, M, D]                -> F(Y, M, D)
    end.

dateh([Y, M, D]) when (M =< 0) ->
    Diff = abs(M) rem 12,
    Offset = (abs(M) - Diff)/12,
    NewYear = erlang:trunc(Y - Offset - 1),
    NewMonth = 12 - Diff,
    dateh([NewYear, NewMonth, D]);
dateh([Y, M, D]) when (M > 12) ->
    Diff = M rem 12,
    Offset = erlang:trunc((M - Diff)/12),
    NewYear = Y + Offset,
    NewMonth = Diff,
    dateh([NewYear, NewMonth, D]);
dateh([Y, M, D]) when (D =< 0) ->
    NewMonth = M - 1, 
    LastDay2 =  ?last_day(Y, NewMonth),
    NewDay = LastDay2 + D,
    dateh([Y, NewMonth, NewDay]);
dateh([Y, M, D]) ->
    % this is a key one - we don't know if D is too big (yet):
    % * 30 is too big if it is February
    % * 30 is small enough if it is January
    LastDay = ?last_day(Y, M),
    if
        (D > LastDay)  ->
            NewMonth = M + 1,
            NewDay = D - LastDay,
            dateh([Y, NewMonth, NewDay]);
        (D =< LastDay) ->             
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
    ?date(V1, [first_array, cast_strings, ban_bools, ban_blanks, ban_numbers]).


days360([Date1, Date2]) ->
    days360([Date1, Date2, true]);

days360([PreDate1, PreDate2, PreMethod]) ->
    
    [Date1, Date2] = collect([PreDate1, PreDate2], date,
                             [cast_or_err, die_on_err]),
    [Method]       = collect([PreMethod], bool,
                             [cast_or_err, die_on_err]),
    
    days360(Date1, Date2, Method).

days360(Date1, Date2, Method) when Date1 > Date2 ->
    -days360(Date2, Date1, Method);

%% Same Year
days360(#datetime{date={Y,M1,D1}},#datetime{date={Y,M2,D2}}, _Method) ->
    (calendar:last_day_of_the_month(Y, M1) - D1) + ((M2 - M1) * 30) + D2;

days360(#datetime{date={Y1,M1,D1}}, #datetime{date={Y2,M2,D2}}, _Method) ->
    Left = calendar:last_day_of_the_month(Y1, M1) - D1 + (30 * (12 - M1)),
    FromStart = D2 + (30 * (M2-1)),
    Left + ((Y2 - Y1 - 1) * 360) + FromStart.


%% TODO take out the guards, throw on selection
day([Val]) when is_number(Val) andalso Val < 0 ->
    ?ERRVAL_NUM;
day([Val]) ->
    Date = ?date(Val, [first_array, cast_strings, cast_bools,
                       cast_blanks, cast_numbers]),
    muin_date:day(Date).

month(Args) ->
    col(Args,
        [fetch_name, eval_funs, first_array, cast_blank,
         {cast, num, date, ?ERRVAL_NUM}, {cast, date}],
        [return_errors, {all, fun muin_collect:is_date/1}],
        fun month_/1).

month_([Date]) ->
    muin_date:month(Date).

year([Val]) when is_number(Val) andalso Val < 0 ->
    ?ERRVAL_NUM;
year([Val]) ->
    Date = ?date(Val, [first_array, cast_strings, cast_bools,
                       cast_blanks, cast_numbers]),
    muin_date:year(Date).


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
    
    Dat = col([V1],
              [fetch_name, eval_funs, first_array,
               {cast, num, date, ?ERRVAL_NUM},
               {cast, date}],
              [return_errors, {all, fun muin_collect:is_date/1}]),
    
    Ret = col([V2],
              [fetch_name, eval_funs, first_array, {cast, int}],
              [return_errors, {all, fun is_number/1}]),
    
    case {Dat, Ret} of
        {[Date], [Return]} -> weekday1(Date, Return);
        {[_Date], Err}     -> Err;
        {Err, _Else}       -> Err
    end.
        
%    io:format("hello?~n"),
%    Dt = ?date(V1, ?cast_all),
%    Rettype = ?int(V2, ?cast_all),    
%% io:format("Dt ~p", [Dt]),
%% weekday1(Dt, Rettype).

weekday1(Dt, 1) ->
    case (calendar:day_of_the_week(Dt#datetime.date) + 1) rem 7 of
        0 -> 7;
        N -> N
    end;
weekday1(Dt, 2) ->
    calendar:day_of_the_week(Dt#datetime.date);
weekday1(Dt, 3) ->
    calendar:day_of_the_week(Dt#datetime.date) - 1;
weekday1(_Dt, _X) ->
    ?ERRVAL_NUM.

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


hour([D]) ->
    [Val | _ ] = ?flatten_all([D]),
    Date = ?date(Val, [cast_strings, cast_bools, cast_blanks, cast_numbers]),
    muin_date:hour(Date).

minute([D]) ->
    [Val | _ ] = ?flatten_all([D]),
    Date = ?date(Val, [cast_strings, cast_bools, cast_blanks, cast_numbers]),
    muin_date:minute(Date).

second(Args) ->
    col(Args,
        [fetch_name, eval_funs, first_array,
         {cast, num, date, ?ERRVAL_NUM},
         {cast, date}],
        [return_errors, {all, fun muin_collect:is_date/1}],
        fun second_/1).

second_([Date]) ->
    muin_date:second(Date).

now([]) ->
    {Date, Time} = calendar:now_to_universal_time(erlang:now()),
    #datetime{date = Date, time = Time}.

timevalue(Args) ->
    col(Args,
        [fetch_name, eval_funs, first_array, {cast, str, date}],
        [return_errors, {all, fun muin_collect:is_date/1}],
        fun timevalue_/1).

timevalue_([#datetime{time={H,M,S}}]) ->
    Secs = (H * 3600) + (M * 60) + S,
    Secs / 86400.

time(Args) ->
    col(Args,
        [fetch_name, eval_funs, first_array, {cast, int}],
        [return_errors, {all, fun is_number/1}],
        fun time_/1).

time_([H,M,S]) ->
    X = ((H rem 24) * 3600) + ((M rem 60) * 60) + (S rem 60),
    Hours = erlang:trunc(X / 3600),
    Mins  = erlang:trunc((X - (Hours * 3600)) / 60),
    Secs  = erlang:trunc((X - (Hours * 3600) - (Mins * 60))),
    #datetime{time = {Hours, Mins, Secs}}.
      
%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% -include_lib("eunit/include/eunit.hrl").

%% datedif_test_() ->
%%     [
%%      %% Years
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 17}},
%%                        "Y") == 1),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "Y") == 0),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 18}},
%%                        "Y") == 1),

%%      %% Months
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 17}},
%%                        "M") == 12),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "M") == 11),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 18}},
%%                        "M") == 12),

%%      %% Days
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 17}},
%%                        "D") == 365),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "D") == 364),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 18}},
%%                        "D") == 366),

%%      %% Days only
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 17}},
%%                        "MD") == 0),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "MD") == 30),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 18}},
%%                        "MD") == 1),

%%      %% Months only
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 17}},
%%                        "YM") == 0),
%%      %%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%      %%                        #datetime{date = {1987, 8, 16}},
%%      %%                        "YM") == 11),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 18}},
%%                        "YM") == 0),

%%      %% Days -- years ignorned
%%      %%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%      %%                        #datetime{date = {1987, 8, 17}},
%%      %%                        "YD") == 0),
%%      %%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%      %%                        #datetime{date = {1987, 8, 16}},
%%      %%                        "YD") == 364),
%%      %%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%      %%                        #datetime{date = {1987, 8, 18}},
%%      %%                        "YD") == 1),

%%      ?_assert(true)
%%     ].
