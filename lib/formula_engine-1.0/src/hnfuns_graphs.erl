%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       Provides the markdown for graphs
%%%
%%% @end
%%% Created : 11 Jan 2010 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(hnfuns_graphs).

-export([
         linegraph/1,
         piechart/1,
         histogram/1
        ]).

linegraph([{range, Data}]) ->
    Data2 = make_data(get_data(Data)),
    % Max = tconv:to_s(stdfuns_stats:max(Data2)),
    % Min = tconv:to_s(stdfuns_stats:min(Data2)),
    Min = 0,
    Max = 100,
    "![graph](http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ make_data(Data2) ++ "&amp;cht=lc&amp;"
        ++ "chds="++ tconv:to_s(Min) ++ "," ++ tconv:to_s(Max) ++ "&amp;"
        ++ "chxt=y&amp;chxr=0," ++ tconv:to_s(Min) ++ "," ++ tconv:to_s(Max)
        ++ ")";
linegraph([1, {range, Data}, Colour]) ->
    Data2 = make_data(get_data(Data)),
    Colours2 = make_colours([Colour]),
    lineg1(Data2, 0, 100, Colours2);
linegraph([2, {range, Data1}, {range, Data2}, {range, Colours}]) ->
    io:format("Data1 is ~p~nData2 is ~p~nColours is ~p~n",
              [Data1, Data2, Colours]),
    Data1a = make_data(get_data(Data1)),
    Data2a = make_data(get_data(Data2)),
    Coloursa = make_colours(get_data(Colours)),
    lineg1(Data1a ++ "|" ++ Data2a, 0, 100, Coloursa);
linegraph([3, {range, Data1}, {range, Data2}, {range, Data3}, {range, Colours}]) ->
    Data1a = make_data(get_data(Data1)),
    Data2a = make_data(get_data(Data2)),
    Data3a = make_data(get_data(Data3)),
    Coloursa = make_colours(get_data(Colours)),
    lineg1(Data1a ++ "|" ++ Data2a ++ "|" ++ Data3a, 0, 100, Coloursa);
linegraph([4, {range, Data1}, {range, Data2},
           {range, Data3}, {range, Data4},
           {range, Colours}]) ->
    Data1a = make_data(get_data(Data1)),
    Data2a = make_data(get_data(Data2)),
    Data3a = make_data(get_data(Data3)),
    Data4a = make_data(get_data(Data4)),
    Coloursa = make_colours(get_data(Colours)),
    lineg1(Data1a ++ "|" ++ Data2a ++ "|" ++ Data3a ++ "|" ++ Data4a,
           0, 100, Coloursa).

lineg1(Data, Min, Max, Colours) ->
    Min1 = tconv:to_s(Min),
    Max1 = tconv:to_s(Max),
    "![graph](http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ Data
        ++ "&amp;cht=lc&amp;"
        ++ "chds="++ Min1 ++ "," ++ Max1 ++ "&amp;"
        ++ "chxt=y&amp;chxr=0," ++ Min1 ++ "," ++ Max1
        ++ "&amp;chco="
        ++ Colours
        ++ ")".

piechart([{range, Data}, {range, Titles}, {range, Colours}]) ->
    Data2 = make_data(normalise(get_data(Data))),
    Titles2 = make_titles(get_data(Titles)),
    Colours2 = make_colours(get_data(Colours)),
    "![graph](http://chart.apis.google.com/chart?cht=p3&amp;chd=t:"
        ++ Data2
        ++ "&amp;chs=450x100&amp;chl="
        ++ Titles2
        ++ "&amp;chco="
        ++ Colours2.

histogram([{range, Data}, {range, Titles}]) ->
    Data2 = make_data(get_data(Data)),
    Titles2 = make_titles(get_data(Titles)),
    "![graph](http://chart.apis.google.com/chart?cht=bvs&amp;chd=t:"
        ++ Data2
        ++ "&amp;chs=250x100&amp;chl="
        ++ Titles2.

normalise(Data) ->
    Total = lists:sum(Data),
    Fun = fun(X) -> trunc(100*X/Total) end,
    lists:map(Fun, Data).

get_data(List) -> get_d1(List, []).

get_d1([], Acc)                      -> lists:reverse(Acc);
get_d1([[H] | T], Acc)               -> get_d1(T, [H | Acc]);
get_d1([H | T], Acc) when is_list(H) ->
    get_d1(T, lists:flatten([lists:reverse(H) | Acc])).

make_data(List) -> make_d1(List, []).

make_d1([], Acc)      -> string:join(lists:reverse(Acc), ",");
make_d1([H | T], Acc) when is_integer(H) ->
    make_d1(T, [integer_to_list(H) | Acc]);
make_d1([H | T], Acc) when is_float(H) ->
    make_d1(T, [float_to_list(H) | Acc]).

make_titles(List) -> string:join(List, "|").

make_colours(List) -> make_c1(List, []).

make_c1([], Acc)              -> string:join(lists:reverse(Acc), ",");
make_c1(["red"     | T], Acc) -> make_c1(T, ["FF0000" | Acc]);
make_c1(["green"   | T], Acc) -> make_c1(T, ["00FF00" | Acc]);
make_c1(["blue"    | T], Acc) -> make_c1(T, ["0000FF" | Acc]);
make_c1(["yellow"  | T], Acc) -> make_c1(T, ["FFFF00" | Acc]);
make_c1(["cyan"    | T], Acc) -> make_c1(T, ["00FFFF" | Acc]);
make_c1(["magenta" | T], Acc) -> make_c1(T, ["FF00FF" | Acc]);
make_c1(["black"   | T], Acc) -> make_c1(T, ["000000" | Acc]);
make_c1(["white"   | T], Acc) -> make_c1(T, ["FFFFFF" | Acc]);
make_c1(["orange"  | T], Acc) -> make_c1(T, ["FF7F00" | Acc]);
make_c1([H | T], Acc)         -> make_c1(T, [H | Acc]).


