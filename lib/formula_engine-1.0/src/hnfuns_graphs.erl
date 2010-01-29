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

-define(ROW, 0).
-define(COLUMN, 1).

-include("typechecks.hrl").
-include("muin_records.hrl").

-import(muin_util, [cast/2]).
-import(muin_collect, [ col/2, col/3, col/4 ]).

%%
%% Exported functions
%%

%% Variable 'O' is 'Orientation'
linegraph([Data])              ->
    lg1(Data, ?ROW, {{scale, auto}, [], []});
linegraph([Data, O])           ->
    lg1(Data, O, {{scale, auto}, [], []});
linegraph([Data, O, Min, Max]) ->
    lg1(Data, O, {{Min, Max}, [], []});
linegraph([Data, O, Min, Max, XAxis]) ->
    lg1(Data, O, {{Min, Max}, XAxis, []});
linegraph([Data, O, Min, Max, XAxis, Cols]) ->
    lg1(Data, O, {{Min, Max}, XAxis, Cols}).

piechart([Data])                  -> pie1(Data, [], []);
piechart([Data, Titles])          -> pie1(Data, Titles, []);
piechart([Data, Titles, Colours]) -> pie1(Data, Titles, Colours).

histogram([D])                   -> hist1(D, {{scale, auto}, [], []});
histogram([D, Tt])               -> hist1(D, {{scale, auto}, Tt, []});
histogram([D, Tt, Cols])         -> hist1(D, {{scale, auto}, Tt, Cols});
histogram([D, Tt, Cols, Mn, Mx]) -> hist1(D, {{Mn, Mx}, Tt, Cols}).
    
%%
%% Internal Functions
%%
hist1(Data, {Scale, Titles, Colours}) ->
    Data2 = cast_data(Data),
    Titles2 = make_titles(cast_titles(Titles)),
    Colours2 = conv_colours(get_colours(Colours)),
    Data3 = lists:reverse(Data2),
    {Min, Max} = get_scale(Scale, Data2),
    "![graph](http://chart.apis.google.com/chart?cht=bvs&amp;chd=t:"
        ++ make_data(Data3)
        ++ "&amp;chxt=y&amp;chxl=0:|"
        ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ "&amp;chs=250x100&amp;chl="
        ++ Titles2
        ++ Colours2.

pie1(Data, Titles, Colours) ->
    Data2 = cast_data(Data),
    Titles2 = cast_titles(Titles),
    Colours2 = conv_colours(get_colours(Colours)),
    case has_error([Data2, Titles2, Colours2]) of
        {true, Error} -> Error;
        false         -> pie2(Data2, Titles2, Colours2)
    end.

pie2(Data, Titles, Colours) ->
    Data1 = make_data(normalise(Data)),
    Titles1 = make_titles(Titles),
    Colours1 = conv_colours(get_colours(Colours)),
    "![graph](http://chart.apis.google.com/chart?cht=p3&amp;chd=t:"
        ++ Data1
        ++ "&amp;chs=450x100&amp;chl="
        ++ Titles1
        ++ Colours1.

lg1(Data, Orientation, {Scale, Axes, Colours}) ->
    {Data2, _NoOfRows, _NoOfCols} = extract(Data, Orientation),
    Data3 = [cast_data(X) || X <- Data2],
    % check for errors and then fix the reversed lists problems
    Colours2 = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data),
    XAxis2 = get_axes(Axes),
    case has_error([Data3, Colours2, Min, Max, XAxis2]) of
        {true, Error} -> Error;
        false         -> lg2(rev(Data3), {Min, Max}, XAxis2, Colours)
    end.

lg2(Data, {Min, Max}, XAxis, Colours) ->
   "![graph](http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ conv_data(Data) ++ "&amp;cht=lc&amp;"
        ++ "chds="++ tconv:to_s(Min) ++ "," ++ tconv:to_s(Max)
        ++ "&amp;chxt="
        ++ conv_x_axis(XAxis)
        ++ "1:|" ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ conv_colours(Colours)
        ++ ")".

cast_data(Data) ->
    col([Data],
                [eval_funs,
                 fetch, flatten,
                 {cast, str, num, ?ERRVAL_VAL},
                 {cast, bool, num},
                 {cast, blank, num},
                 {ignore, str},
                 {ignore, blank}
                ],
                [return_errors, {all, fun is_number/1}]).

cast_titles(Titles) ->
    col([Titles],
                  [eval_funs,
                   fetch, flatten,
                   {cast, bool, str},
                   {cast, num, str},
                   {cast, date, str},
                   {cast, blank, str}],
                  [return_errors]).

conv_colours([])      -> [];
conv_colours(Colours) -> "&amp;chco=" ++ make_colours(Colours).

conv_x_axis([])    -> "y&amp;chxl=";
conv_x_axis(XAxis) -> "x,y&amp;chxl=0:|"
                          ++ string:join(XAxis, "|") ++ "|". 

conv_data(Data) -> conv_d1(Data, []).

conv_d1([], Acc)      -> string:join(lists:reverse(Acc), "|");
conv_d1([H | T], Acc) -> conv_d1(T, [make_data(H) | Acc]).

get_colours(Colours) ->
    col([Colours],
         [eval_funs,
          {ignore, bool},
          {ignore, num},
          {ignore,date},
          fetch, flatten],
          [return_errors]).

get_axes(XAxis)  -> col([XAxis],
                        [eval_funs,
                         fetch, flatten,
                         {cast, num, str},
                         {cast, bool, str},
                         {cast, date, str},
                         {cast, blank, str}],
                        [return_errors]).

get_scale({scale, auto}, [H | T]) when is_list(H) ->
    Min = tconv:to_s(stdfuns_stats:min(lists:merge([H | T]))),
    Max = tconv:to_s(stdfuns_stats:max(lists:merge([H | T]))),
    {Min, Max};
get_scale({scale, auto}, Data) ->
    Min = tconv:to_s(stdfuns_stats:min(Data)),
    Max = tconv:to_s(stdfuns_stats:max(Data)),
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

normalise(Data) ->
    Total = lists:sum(Data),
    Fun = fun(X) -> trunc(100*X/Total) end,
    lists:map(Fun, Data).

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
