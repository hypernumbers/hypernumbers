%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @doc create date ranges from fuzzy dates
-module(fuzzy_date).

-include("hypernumbers.hrl").
 
-export([range/1, range/2, nrange/1, nrange/2]).

-define(GREGORIAN_SECONDS_1970, 62167219200).
-define(END_DAY,   {24,0,0}).
-define(START_DAY, {0,0,0}).

%% Same as range() but the return format is same as erlang:now()
nrange(When) ->
    nrange(When, calendar:local_time()).
nrange(When, Date) ->
    {Start, End} = range(When, Date),
    {datetime_to_now(Start), datetime_to_now(End)}.

range(When) ->
    range(When, calendar:local_time()).

range("today", {{Y, M, D}, _Time}) ->
    { {{Y,M,D},?START_DAY}, {{Y,M,D},?END_DAY} };

range("yesterday", {{Y, M, D}, _Time}) when D > 1 ->
    { {{Y,M,D-1},?START_DAY}, {{Y,M,D-1},?END_DAY} };
range("yesterday", {{Y, M, D}, _Time})  ->
    { {{Y,M-1,D},?START_DAY}, {{Y,M-1,D},?END_DAY} };

%% "DD/MM/YY"
%% TODO: Take the first part of the year from _Now
range([D1,D2,$/,M1,M2,$/,Y1,Y2], _Now)  ->
    {Y, M, D} = {ltoi([$2, $0, Y1,Y2]), ltoi([M1,M2]), ltoi([D1,D2])},
    { {{Y,M,D},?START_DAY}, {{Y,M,D},?END_DAY} };

%% "D/M/Y"
range([D1,$/,M1,$/,Y1], _Now)  ->
    {Y, M, D} = {ltoi(Y1), ltoi(M1), ltoi(D1)},
    { {{Y,M,D},?START_DAY}, {{Y,M,D},?END_DAY} };

range(Date, _Now) ->
    throw({date_not_recognised, Date}).

ltoi(X) ->
    list_to_integer(X).

datetime_to_now(DateTime) ->
    GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
    {ESeconds div 1000000, ESeconds rem 1000000, 0}.

