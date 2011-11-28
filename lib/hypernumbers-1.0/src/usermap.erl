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
         pret/2,
         pret_preprocess/2
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

pret_preprocess(Dir, FileName) ->
    [Header | Rows] = parse_csv:parse_file(Dir ++ FileName),
    Rows2 = filter(Rows, []),
    [_IssueDate, Postfix] = string:tokens(FileName, " "),
    ok = write(Rows2, Header, Dir, Postfix).

filter([], Acc) -> chunk(Acc);
filter([H | T], Acc) ->
    List = tuple_to_list(H),
    {Head, _Tail} = lists:split(51, List),
    case lists:member("null", Head) of
        true  -> filter(T, Acc);
        false -> filter(T, [H | Acc])
    end.

chunk(List) ->
    Fun = fun(A, B) ->
                  if
                      element(3, A) >  element(3, B) -> false;
                      element(3, A) =< element(3, B) -> true
                  end
          end,
    SortedList = lists:sort(Fun, List),
    chunk2(SortedList, []).

chunk2([], Acc) ->
    Acc;
% first one through gets their own clause
chunk2([H | T], []) ->
    Key = element(3, H),
    chunk2(T, [{Key, [H]}]);
chunk2([H1 | T1], [{K2, V2} | T2] = Acc) ->
    Date1 = element(3, H1),
    Date2 = K2,
    if
        Date1 == Date2 -> chunk2(T1, [{K2, [H1 | V2]} | T2]);
        Date1 /= Date2 -> chunk2(T1, [{element(3, H1), [H1]} | Acc])
    end.

write([], _, _, _) ->
    ok;
write([{Date, Recs} | T], Header, Dir, Postfix) ->
    ok = write2(Dir, Date, Postfix, Header, Recs),
    write(T, Header, Dir, Postfix).

write2(Dir, Date, Postfix, Header, Recs) ->
    Date2 = string:join(string:tokens(Date, "/"), "_"),
    File = "processed_" ++ Date2 ++ "_" ++ Postfix,
    case file:open(Dir ++ File, [exclusive, append]) of
        {ok, IODevice} ->
            io:fwrite(IODevice, "~s~n", [make(Header)]),
            [io:fwrite(IODevice, "~s~n", [make(X)]) || X <- Recs],
            file:close(IODevice);
        {error, eexist} ->
            io:format("You have already processed this file. "
                      ++ "Delete the output and retry if you like.~n"),
            exit({error, eexist})
    end,
    ok.

make(Tuple) ->
    List = tuple_to_list(Tuple),
    mk2(List, []).

mk2([], Acc) ->
    Ret = lists:flatten(lists:reverse(Acc)),
    Len = length(Ret),
    {Ret2, _Discard} = lists:split(Len - 2, Ret),
    Ret2;
mk2([H | T], Acc)  ->
    Str = io_lib:format("~s,", [H]),
    mk2(T, [Str | Acc]).
