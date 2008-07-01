%%%----------------------------------------------------------------------------
%%% @doc Datetime support functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%----------------------------------------------------------------------------

%%% TODO:
%%% * Proper types for #datetime's, not just tuple()'s.

-module(muin_date).

-export([excel_mac_to_gregorian/1, excel_win_to_gregorian/1,
         year/1, month/1, day/1,
         gt/2, dtdiff/2]).

-include("muin_dates.hrl").

-define(EXCEL_MAC_EPOCH, {1904, 1, 1}).
-define(EXCEL_WIN_EPOCH, {1900, 1, 1}).
-define(NUM_SECONDS_IN_A_DAY, 86400).


%%% PUBLIC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @spec excel_mac_to_gregorian(int()) -> tuple()
%% @doc Convert number to date using Excel Mac's date system (1904 epoch).
excel_mac_to_gregorian(N) ->
    excel_to_gregorian(N, ?EXCEL_MAC_EPOCH).

%% @spec excel_win_to_gregorian(int()) -> tuple()
%% @doc Convert number to date using Excel Mac's date system (1900 epoch).
excel_win_to_gregorian(N) ->
    excel_to_gregorian(N, ?EXCEL_WIN_EPOCH).

%% @spec myear(tuple()) -> int()
%% @doc Read the year field of a #datetime record.
year(_Dt = #datetime{date = {Year, _, _}}) ->
    Year.

%% @spec mmonth(tuple()) -> int()
%% @doc Read the month field of a #datetime record.
month(_Dt = #datetime{date = {_, Month, _}}) ->
    Month.

%% @spec mday(tuple()) -> int()
%% @doc Read the day field of a #datetime record.
day(_Dt = #datetime{date = {_, _, Day}}) ->
    Day.

%% @spec gt(tuple(), tuple()) -> bool()
%% @doc Checks if Date1 is later than Date2.
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

%% @spec dtdiff(tuple(), tuple()) -> int()
%% @doc Returns the difference between two dates in days.
dtdiff(Start, End) ->
    Startdays = calendar:date_to_gregorian_days(Start#datetime.date),
    Enddays = calendar:date_to_gregorian_days(End#datetime.date),
    Enddays - Startdays.


%%% PRIVATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @spec excel_to_gregorian(int(), tuple()) -> tuple()
%% @doc Convert an integer to #datetime relative to an epoch.
excel_to_gregorian(N, Epoch) when is_integer(N) -> % day only, no time
    Numdays = calendar:date_to_gregorian_days(Epoch) + N,
    Date = calendar:gregorian_days_to_date(Numdays),
    #datetime{date = Date};
excel_to_gregorian(F, Epoch) when is_float(F) ->
    Daysxl = trunc(F),
    Numdays = calendar:date_to_gregorian_days(Epoch) + Daysxl,
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
    {Hour, Minute, Second}.


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
