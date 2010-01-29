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
         linegraph3/1,
         piechart/1,
         histogram/1,
         test/1
        ]).

-define(ROW, 0).
-define(COLUMN, 1).

-include("typechecks.hrl").
-include("muin_records.hrl").

-import(muin_util, [cast/2]).
-import(muin_collect, [ col/2, col/3, col/4 ]).

test([Data, 0]) ->
    io:format("Data (in rows) is ~p~n", [Data]),
    "banzai!";
test([Data, 1]) ->
    io:format("Data (in columns) is ~p~n", [Data]),
    "bonzai!".

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
linegraph([2, {range, Data1}, {range, Data2}, {range, [Colours]}]) ->
    io:format("Data1 is ~p~nData2 is ~p~nColours is ~p~n",
              [Data1, Data2, Colours]),
    Data1a = make_data(get_data(Data1)),
    Data2a = make_data(get_data(Data2)),
    Coloursa = make_colours(Colours),
    lineg1(Data1a ++ "|" ++ Data2a, 0, 100, Coloursa);
linegraph([3, {range, Data1}, {range, Data2}, {range, Data3},
           {range, [Colours]}]) ->
    Data1a = make_data(get_data(Data1)),
    Data2a = make_data(get_data(Data2)),
    Data3a = make_data(get_data(Data3)),
    Coloursa = make_colours(Colours),
    lineg1(Data1a ++ "|" ++ Data2a ++ "|" ++ Data3a, 0, 100, Coloursa);
linegraph([4, {range, Data1}, {range, Data2},
           {range, Data3}, {range, Data4},
           {range, [Colours]}]) ->
    Data1a = make_data(get_data(Data1)),
    Data2a = make_data(get_data(Data2)),
    Data3a = make_data(get_data(Data3)),
    Data4a = make_data(get_data(Data4)),
    Coloursa = make_colours(Colours),
    lineg1(Data1a ++ "|" ++ Data2a ++ "|" ++ Data3a ++ "|" ++ Data4a,
           0, 100, Coloursa).

lineg1(Data, Min, Max, Colours) ->
    Min1 = tconv:to_s(Min),
    Max1 = tconv:to_s(Max),
    "![graph](http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ Data
        ++ "&amp;cht=lc&amp;"
        ++ "chds="++ Min1 ++ "," ++ Max1 ++ "&amp;"
        ++ "chxt=x&amp;chxr="
        ++ "0," ++ Min1 ++ "," ++ Max1
        ++ "&amp;chco="
        ++ Colours
        ++ ")".

%% Variable 'O' is 'Orientation'
linegraph3([Data])              -> lg3(Data, ?ROW, {{scale, auto},
                                                    noAxes, randCol});
linegraph3([Data, O])           -> lg3(Data, O, {{scale, auto},
                                                 noAxes, randCol});
linegraph3([Data, O, Min, Max]) -> lg3(Data, O, {{Min, Max}, noAxes, randCol});
linegraph3([Data, O, Min, Max, XAxis]) ->
                  lg3(Data, O, {{Min, Max}, XAxis, randCol});
linegraph3([Data, O, Min, Max, XAxis, Cols]) ->
                  lg3(Data, O, {{Min, Max}, XAxis, Cols}).
    
lg3(Data, Orientation, {Scale, Axes, Colours}) ->
    {Data2, _NoOfRows, _NoOfCols} = extract(Data, Orientation),
    Data3 = [col([X],
                 [eval_funs,
                  fetch, flatten,
                  {cast, str, num, ?ERRVAL_VAL},
                  {cast, bool, num},
                  {cast, blank, num},
                  {ignore, str},
                  {ignore, blank}
                 ],
                 [return_errors, {all, fun is_number/1}]) || X <- Data2],
    % check for errors and then fix the reversed lists problems
    case has_error(Data3) of
        {true, Error} -> Error;
        false         -> lg3a(rev(Data3), Scale, Axes, Colours)
    end.

lg3a(Data, Scale, Axes, Colours) ->
    {Min, Max} = get_scale(Scale, Data),
    XAxis2 = get_axes(Axes),
    Colours2 = get_colours(Colours),
    io:format("XAxis2 is ~p~nColours2 is ~p~n",
               [XAxis2, Colours2]),
    Ret = "![graph](http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ conv_data(Data) ++ "&amp;cht=lc&amp;"
        ++ "chds="++ tconv:to_s(Min) ++ "," ++ tconv:to_s(Max) ++ "&amp;"
        ++ "chxt="
        ++ conv_x_axis(XAxis2)
        ++ "1:|" ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ conv_colours(Colours2)
        ++ ")",
    io:format("Ret is ~p~n", [Ret]),
    Ret.

conv_colours([])      -> [];
conv_colours(Colours) -> "&amp;chco=" ++ make_colours(Colours).

conv_x_axis([])    -> "y&amp;chxl=";
conv_x_axis(XAxis) -> "x,y&amp;chxl=0:|" ++ string:join(XAxis, "|") ++ "|". 

conv_data(Data) -> conv_d1(Data, []).

conv_d1([], Acc)      -> string:join(lists:reverse(Acc), "|");
conv_d1([H | T], Acc) -> conv_d1(T, [make_data(H) | Acc]).

get_colours(randCol) -> [];
get_colours(Colours) ->
    io:format("Colours is ~p~n", [Colours]),
    col([Colours],
         [eval_funs,
          {ignore, bool},
          {ignore, num},
          {ignore,date},
          fetch, flatten],
          [return_errors]).

get_axes(noAxes) -> [];
get_axes(XAxis)  -> col([XAxis],
                        [eval_funs,
                         {cast, num, str},
                         {cast, bool, str},
                         {cast, date, str},
                         {cast, blank, str},
                         fetch, flatten],
                        [return_errors]).

get_scale({scale, auto}, Data) ->
    Min = tconv:to_s(stdfuns_stats:min(lists:merge(Data))),
    Max = tconv:to_s(stdfuns_stats:max(lists:merge(Data))),
    {Min, Max};
get_scale(Scale, _Data) -> Scale.
    
extract({range, Data}, ?ROW)    -> tartup(Data);
extract({range, Data}, ?COLUMN) -> tartup(hslists:transpose(Data)).

tartup(Data) ->
    [F | _T] = Data,
    NoOfCols = length(F), % rectangular matrix
    {chunk(Data), length(Data), NoOfCols}.


rev(List) -> rev1(List, []).

rev1([], Acc)      -> lists:reverse(Acc);
rev1([H | T], Acc) -> rev1(T, [lists:reverse(H) | Acc]).

chunk(Data) -> chk2(Data, []).

chk2([], Acc)      -> lists:reverse(Acc);
chk2([H | T], Acc) -> chk2(T, [{range, [H]} | Acc]).

has_error([])                      -> false;
has_error([{errval, Val} | _T])    -> {true, {errval, Val}};
has_error([H | T]) when is_list(H) -> case has_error(H) of
                                          {true, E} -> {true, E};
                                          false     -> has_error(T)
                                      end;
has_error([_H | T])                -> has_error(T).

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
    make_d1(T, [tconv:to_s(H) | Acc]).

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
