%%%----------------------------------------------------------------------------
%%% @doc Datetime support functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%----------------------------------------------------------------------------

%%% TODO:
%%% * Proper types for #datetime's, not just tuple()'s.

-module(muin_date).

-export([excel_mac_to_gregorian/1, excel_win_to_gregorian/1,
         year/1, month/1, day/1,
         hour/1, minute/1, second/1,
         gt/2, dtdiff/2,
         next_day/1, foldl/4,
         mtest/0]).

%%-include("handy_macros.hrl").
-include("muin_records.hrl").

%% Epochs are defined in Section 5.28 of the excelfileformatV1-41.pdf
%% Windows has the Lotus-1-2-3 bug where it adds a 
%% leap year in 1900 - the OO documentation saying the Windows epoch
%% begins in 31/12/1899 (the Windows docos say it is 1/1/1900)
%% This is because Open Office took a decision to make all dates
%% between 31/12/1899 and 29/2/1900 'buggy'
%% http://blogs.msdn.com/brian_jones/archive/2006/10/25/spreadsheetml-dates.aspx
%% we need to fix this...
-define(EXCEL_MAC_EPOCH, {1904, 1, 1}).
-define(EXCEL_WIN_EPOCH, {1900, 1, 1}).
-define(NUM_SECONDS_IN_A_DAY, 86400).

%%% PUBLIC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec excel_mac_to_gregorian(int()) -> tuple()
%% @doc Convert number to date using Excel Mac's date system (1904 epoch).
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excel_mac_to_gregorian(N) ->
    excel_to_gregorian(N, macintosh).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec excel_win_to_gregorian(int()) -> tuple()
%% @doc Convert number to date using Excel Mac's date system (1900 epoch).
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excel_win_to_gregorian(N) ->
    excel_to_gregorian(N, windows).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec year(tuple()) -> int()
%% @doc Read the year field of a #datetime record.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
year(_Dt = #datetime{date = {Year, _, _}}) ->
    Year.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec month(tuple()) -> int()
%% @doc Read the month field of a #datetime record.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
month(_Dt = #datetime{date = {_, Month, _}}) ->
    Month.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec day(tuple()) -> int()
%% @doc Read the day field of a #datetime record.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
day(_Dt = #datetime{date = {_, _, Day}}) ->
    Day.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec hour(tuple()) -> int()
%% @doc Read the hour field of a #datetime record.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hour(_Dt = #datetime{time = {Hour, _, _}}) ->
    Hour.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec minute(tuple()) -> int()
%% @doc Read the minute field of a #datetime record.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
minute(_Dt = #datetime{time = {_, Minute, _}}) ->
    Minute.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec second(tuple()) -> int()
%% @doc Read the second field of a #datetime record.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
second(_Dt = #datetime{time = {_, _, Second}}) ->
    Second.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec gt(tuple(), tuple()) -> bool()
%% @doc Checks if Date1 is later than Date2.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gt(#datetime{date = {Yr1, _, _}}, #datetime{date = {Yr2, _, _}})
  when Yr1 > Yr2 ->
    true;
gt(#datetime{date = {Yr, Mo1, _}}, #datetime{date = {Yr, Mo2, _}})
  when Mo1 > Mo2 -> % same year
    true;
gt(#datetime{date = {Yr, Mo, Day1}}, #datetime{date = {Yr, Mo, Day2}})
  when Day1 > Day2 -> % same year & month
    true;
gt(_, _) ->
    false.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec dtdiff(tuple(), tuple()) -> int()
%% @doc Returns the difference between two dates in days.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dtdiff(Start, End) ->
    Startdays = calendar:date_to_gregorian_days(Start#datetime.date),
    Enddays = calendar:date_to_gregorian_days(End#datetime.date),
    Enddays - Startdays.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec next_day(tuple(), tuple()) -> tuple()
%% @doc Returns the date one day after the given date.
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
next_day(#datetime{date = {Year, 12, 31}} = Dt) ->
    Dt#datetime{date = {Year + 1, 1, 1}};
next_day(#datetime{date = {Year, Month, 31}} = Dt) ->
    Dt#datetime{date = {Year, Month + 1, 1}};
next_day(#datetime{date = {Year, Month, Day}} = Dt) ->
    case calendar:last_day_of_the_month(Year, Month) of
        Day -> Dt#datetime{date = {Year, Month + 1, 1}};
        _   -> Dt#datetime{date = {Year, Month, Day + 1}}
    end.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% @spec foldl(fun(), term(), tuple(), tuple()) -> term()
%% @doc Like lists:foldl/3, but the fun is applied to each date between
%% Start and End
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
foldl(Fun, Acc, Start, End) ->
    case gt(Start, End) of
        true ->
            Acc;
        false ->
            Nacc = Fun(Start, Acc),
            foldl(Fun, Nacc, next_day(Start), End)
    end.

%% For testing in the shell.
mtest() ->
    foldl(fun(Dt, Acc) ->
		  #datetime{date = {_, _, Day}} = Dt,
		  Acc ++ [Day]
	  end,
	  [],
	  #datetime{date = {2008, 6, 30}}, #datetime{date = {2008, 7, 2}}).

%%% PRIVATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @spec excel_to_gregorian(int(), tuple()) -> tuple()
%% @doc Convert an integer to #datetime relative to an epoch.
excel_to_gregorian(N, Epoch) when is_integer(N) -> % day only, no time
    Numdays = get_numdays(N,Epoch),
    Date = calendar:gregorian_days_to_date(Numdays),
    #datetime{date = Date};
excel_to_gregorian(F, Epoch) when is_float(F) ->
    Daysxl = trunc(F),
    io:format("in muin_date:excel_to_gregorian Daysxl is ~p~n",[Daysxl]),
    Numdays = get_numdays(Daysxl,Epoch),
    Date = calendar:gregorian_days_to_date(Numdays),
    Time = dayftotime(F - Daysxl),
    Numsecs = calendar:datetime_to_gregorian_seconds({Date, Time}),
    {Datef, Timef} = calendar:gregorian_seconds_to_datetime(Numsecs),
    #datetime{date = Datef, time = Timef}.

%% @spec dayftotime(float()) -> tuple()
%% @doc Convert fraction of a day to time.
dayftotime(F) when is_float(F) andalso F < 1 ->
    Numsecs = F * ?NUM_SECONDS_IN_A_DAY,
    Hour = trunc(Numsecs / 3600),
    Minute = trunc((Numsecs - (Hour * 3600)) / 60),
    Second = trunc(Numsecs - Hour * 3600 - Minute * 60),
    io:format("in muin_date:dayftotime F is ~p Numsecs is ~p Hour is ~p Minutes is ~p Second is ~p~n",[F,Numsecs,Hour,Minute,second]),
    {Hour, Minute, Second}.

%% @spec get_numdays(integer(),atom()) -> tuple()
%% @doc converts Excel number of days since the epoch to times.
get_numdays(N,Epoch)->
    case Epoch of
	macintosh -> Epoch2=?EXCEL_MAC_EPOCH,
		     calendar:date_to_gregorian_days(Epoch2) + N - 1;
	windows   -> Epoch2=?EXCEL_WIN_EPOCH,
		     %% If we are in Windows numbering reset day 60 (eg 29/2/1900 
		     %% which is a non-existant leap year) back to 28/2/1900
		     if
			 N >= 60 -> calendar:date_to_gregorian_days(Epoch2)+N-2;
			 true    -> calendar:date_to_gregorian_days(Epoch2)+N-1
		     end
    end.

%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").
-define(d(N, Y, M, D),
        ?_assert(excel_mac_to_gregorian(N) ==
                 #datetime{date = {Y, M, D}})).
-define(dt(F, Y, M, D, H, Min, S),
        ?_assert(excel_mac_to_gregorian(F) ==
                 #datetime{date = {Y, M, D}, time = {H, Min, S}})).

excel_mac_to_gregorian_test_() ->
    [
     %% Whole days.
     ?d(0, 1904, 1, 1),
     ?d(1, 1904, 1, 2),
     ?d(13687, 1941, 6, 22),
     ?d(15104, 1945, 5, 9),
     ?d(30179, 1986, 8, 17),
     ?d(35063, 1999, 12, 31),
     ?d(35064, 2000, 1, 1),
     ?d(48944, 2038, 1, 1),

     %% Date and time.
     %% TODO: Some dates come out off by a second - investigate.
     ?dt(38161.53911, 2008, 6,  24, 12, 56, 19),
     %%?dt(30179.00273, 1986, 8,  17, 0,  3,  56),
     ?dt(13687.16667, 1941, 6,  22, 4,  0,  0),
     ?dt(67831.98778, 2089, 9,  17, 23, 42, 24)
     %%?dt(123456.789,  2242, 1,  4,  18, 56, 10)
     %%?dt(9801.00001,  1930, 11, 1,  0,  0,  1)
    ].

next_date_test_() ->
    [
     %% Normal increments and end-of-year edge case.
     ?_assert(next_day(#datetime{date = {1986, 8, 17}}) == #datetime{date = {1986, 8, 18}}),
     ?_assert(next_day(#datetime{date = {1986, 8, 30}}) == #datetime{date = {1986, 8, 31}}),
     ?_assert(next_day(#datetime{date = {1986, 8, 31}}) == #datetime{date = {1986, 9, 1}}),
     ?_assert(next_day(#datetime{date = {2000, 12, 31}}) == #datetime{date = {2001, 1, 1}}),
     ?_assert(next_day(#datetime{date = {1999, 12, 31}}) == #datetime{date = {2000, 1, 1}}),
     ?_assert(next_day(#datetime{date = {2008, 7, 1}}) == #datetime{date = {2008, 7, 2}}),
     ?_assert(next_day(#datetime{date = {1900, 1, 1}}) == #datetime{date = {1900, 1, 2}}),
     %% Leap years.
     ?_assert(next_day(#datetime{date = {2000, 2, 28}}) == #datetime{date = {2000, 2, 29}}),
     ?_assert(next_day(#datetime{date = {2000, 2, 29}}) == #datetime{date = {2000, 3, 1}}),
     ?_assert(next_day(#datetime{date = {2000, 12, 30}}) == #datetime{date = {2000, 12, 31}}),
     ?_assert(next_day(#datetime{date = {1900, 2, 28}}) == #datetime{date = {1900, 3, 1}})
    ].

foldl_test_() ->
    [
     ?_assert(
        foldl(fun(Dt, Acc) ->
		      #datetime{date = {_, _, Day}} = Dt,
		      Acc ++ [Day]
	      end,
	      [],
	      #datetime{date = {2008, 6, 30}}, #datetime{date = {2008, 7, 2}}) == [30, 1, 2])

    ].
