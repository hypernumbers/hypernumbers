%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Sustainable Performance Ltd
%%% @doc       implements the mapping fun for pret
%%%            Temporary until user-defined fns
%%%            are added
%%%
%%% @end
%%% Created : 18 Nov 2011 by <gordon@hypernumbers.com>

-module(usermap).

-export([
         pret/2
         ]).

% debugging
-export([
         get_pret_week/3
        ]).

-define(startdayofweek, 4).
-define(D, stdfuns_date).
-define(E, stdfuns_eng).
-define(M, stdfuns_math).

-include_lib("eunit/include/eunit.hrl").

pret("mps/" ++ MPAN, Date) ->
    pret(MPAN, Date);
pret(MPAN, Date) ->
    [Year, Month, Day] = lists:reverse(string:tokens(Date, "/")),
    {WeekNo, DayNo} = get_pret_week(Year, Month, Day),
    Path = ["stores", lookup(MPAN), "electricity", Year, tconv:to_s(WeekNo),
            tconv:to_s(DayNo), "MPAN" ++ MPAN],
    "/" ++ string:join(Path, "/") ++ "/".

lookup(MPAN) ->
    Dict = [{"turnmill-street", "1200036532750"},
            {"cannon-street-52-54", "1200027822889"},
            {"cannon-street-52-54", "1200032341571"},
            {"london-bridge", "1200042616632"},
            {"farringdon-street", "1200061025952"},
            {"manchester-spinningfields", "1630000618246"},
            {"milton-keynes", "1100050349746"},
            {"oxford-street", "1200010086239"},
            {"st-martins-lane", "1200050010052"},
            {"bakers-street-211", "1200010078567"},
            {"queensway", "1200032522075"},
            {"queensway", "1200032522084"},
            {"queensway", "1200050380582"}],
    case lists:keyfind(MPAN, 2, Dict) of
        {Seg, MPAN} -> Seg;
        _Other      -> exit("Store not found for " ++ MPAN)
    end.

get_pret_week(Year, Month, Day) ->
    BYear = 2011,
    BMonth = 1,
    BDay = 1 + (1 - ?E:gestep([?startdayofweek,
                               ?D:weekday([?D:date([BYear, BMonth, 1]), 2])])) * 7,
    BDate = ?D:date([BYear, BMonth, BDay]),
    Offset = + ?startdayofweek - ?D:weekday([?D:date([BYear, 1, 1]), 2]) - 6,
    FullBDate = ?M:'+'([BDate, Offset]),
    FullDate = {datetime, {tconv:to_i(Year), tconv:to_i(Month),
                           tconv:to_i(Day)}, {0, 0, 0}},
    Diff = ?M:'-'([FullDate, FullBDate]),
    ImpliedWeek = ?M:int([Diff/7 + 1]),
    ImpliedWeek2 = if
                       ImpliedWeek == 0         -> 52;
                       ImpliedWeek  > 52        -> ImpliedWeek - 52;
                       ImpliedWeek =< 52
                       andalso ImpliedWeek >= 0 -> ImpliedWeek
                   end,
    ImpliedDay = ?D:weekday([FullDate, 1]) + 2,
    ImpliedDay2 = if
                      ImpliedDay >  7 -> ImpliedDay - 7;
                      ImpliedDay =< 7 -> ImpliedDay
                  end,
    {ImpliedWeek2, ImpliedDay2}.

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

testA1([]) -> ?assertEqual(get_pret_week("2011", "12", "30"), {1,  1}).
testA2([]) -> ?assertEqual(get_pret_week("2010", "12", "30"), {52, 7}).
testA3([]) -> ?assertEqual(get_pret_week("2010", "12", "31"), {1,  1}).
testA4([]) -> ?assertEqual(get_pret_week("2011", "4", "12"),  {15, 5}).
testA5([]) -> ?assertEqual(get_pret_week("2011", "8", "25"),  {34, 7}).
testA6([]) -> ?assertEqual(get_pret_week("2011", "8", "26"),  {35, 1}).
testA7([]) -> ?assertEqual(get_pret_week("2011", "12", "29"), {52, 7}).
testA8([]) -> ?assertEqual(get_pret_week("2011", "12", "29"), {52, 7}).
testA9([]) -> ?assertEqual(get_pret_week("2012", "1", "1"),   {1,  3}).

unit_test_() ->

    Setup = fun() -> ok end,

    SeriesA = [
               fun testA1/1,
               fun testA2/1,
               fun testA3/1,
               fun testA4/1,
               fun testA5/1,
               fun testA6/1,
               fun testA7/1,
               fun testA8/1,
               fun testA9/1
              ],

    %{setup, Setup, Cleanup,
    {setup, Setup, [{with, [], SeriesA}]}.

