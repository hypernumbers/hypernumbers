%%% @doc    This module provides utilities for handling the 
%%%         number/text formatting for a cell
%%%         Created the 11th March 2008
%%%         Cut out the 31st August 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @private
-module(format_util).

%% Standard Interfaces
-export([clock_12/1,
         pad_year/1,
         pad_calendar/1,
         get_short_day/1,
         get_day/1,
         get_short_month/1,
         get_month/1,
         get_last_two/1,
         get_dayname/1,
         get_short_dayname/1
        ]).

clock_12(Hour) when is_integer(Hour), (Hour > 12), (Hour =< 24) -> Hour-12;
clock_12(Hour) when is_integer(Hour), (Hour >= 0), (Hour =< 12) -> Hour.

pad_year(A) ->
    case length(A) of
        1 -> "000"++A;
        2 -> "00"++A;
        3 -> "0"++A;
        4 -> A
    end.

pad_calendar(A) ->
    case length(A) of 
        1 -> "0"++A;
        2 -> A
    end.

get_short_day(1) -> "Mon";
get_short_day(2) -> "Tues";
get_short_day(3) -> "Wed";
get_short_day(4) -> "Thur";
get_short_day(5) -> "Fri";
get_short_day(6) -> "Sat";
get_short_day(7) -> "Sun".

get_day(1) -> "Monday";
get_day(2) -> "Tuesday";
get_day(3) -> "Wednesday";
get_day(4) -> "Thursday";
get_day(5) -> "Friday";
get_day(6) -> "Saturday";
get_day(7) -> "Sunday".

get_short_month(1) -> "Jan";
get_short_month(2) -> "Feb";
get_short_month(3) -> "Mar";
get_short_month(4) -> "Apr";
get_short_month(5) -> "May";
get_short_month(6) -> "Jun";
get_short_month(7) -> "Jul";
get_short_month(8) -> "Aug";
get_short_month(9) -> "Sept";
get_short_month(10) -> "Oct";
get_short_month(11) -> "Nov";
get_short_month(12) -> "Dec".

get_month(1) -> "January";
get_month(2) -> "February";
get_month(3) -> "March";
get_month(4) -> "April";
get_month(5) -> "May";
get_month(6) -> "June";
get_month(7) -> "July";
get_month(8) -> "August";
get_month(9) -> "September";
get_month(10) -> "October";
get_month(11) -> "November";
get_month(12) -> "December".

get_last_two(String) -> string:substr(String,length(String)-1,2).

get_dayname(1) -> "Monday";
get_dayname(2) -> "Tuesday";
get_dayname(3) -> "Wednesday";
get_dayname(4) -> "Thursday";
get_dayname(5) -> "Friday";
get_dayname(6) -> "Saturday";
get_dayname(7) -> "Sunday".

get_short_dayname(1) -> "Mon";
get_short_dayname(2) -> "Tues";
get_short_dayname(3) -> "Wed";
get_short_dayname(4) -> "Thurs";
get_short_dayname(5) -> "Fri";
get_short_dayname(6) -> "Sat";
get_short_dayname(7) -> "Sun".

