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
         histogram/1,
         barchart/1
        ]).

-define(ROW, [true]).
-define(COLUMN, [false]).

-include("typechecks.hrl").
-include("muin_records.hrl").

-import(muin_util, [cast/2]).
-import(muin_collect, [ col/2, col/3, col/4 ]).

%%
%% Exported functions
%%

barchart([Data]) ->
    bar(Data, 0, {{scale, auto}, [], []});
barchart([Data, O])           ->
    bar(Data, O, {{scale, auto}, [], []});
barchart([Data, O, Min, Max]) ->
    bar(Data, O, {{Min, Max}, [], []});
barchart([Data, O, Min, Max, XAxis]) ->
    bar(Data, O, {{Min, Max}, XAxis, []});
barchart([Data, O, Min, Max, XAxis, Cols]) ->
    bar(Data, O, {{Min, Max}, XAxis, Cols}).


bar(Data, Orientation, {Scale, Axes, Colours}) ->
    Orientation2 = cast_orientation(Orientation),
    case has_error([Data, Orientation2, Scale, Axes, Colours]) of
        {true, Error} -> Error;
        false         -> bar2(Data, Orientation2, Scale, Axes, Colours)
    end.

bar2(Data, Orientation, Scale, Axes, Colours) ->
    {Data2, _NoOfRows, _NoOfCols} = extract(Data, Orientation),
    Data3 = [cast_data(X) || X <- Data2],
    % check for errors and then fix the reversed lists problems
    Colours2 = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data3),
    XAxis2 = get_axes(Axes),
    case has_error([Data3, Colours2, Min, Max, XAxis2]) of
        {true, Error} -> Error;
        false         -> bar3(rev(Data3), {Min, Max}, XAxis2, Colours2)
    end.

bar3(Data, {Min, Max}, XAxis, Colours) ->
    Ret = "<img src='http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ conv_data(Data) ++ "&amp;cht=bvs&amp;"
        ++ "chds="++ tconv:to_s(Min) ++ "," ++ tconv:to_s(Max)
        ++ "&amp;"
        ++ "chxt=y&amp;"
        ++ "chxt="
        ++ conv_x_axis(XAxis)
        ++ "1:|" ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ conv_colours(Colours)
        ++ "' />",
    io:format("Ret is ~p~n", [Ret]),
    Ret.

linegraph([Data]) ->
    linegraph([Data, ?ROW]);
linegraph([Data, O])           ->
    lg1(Data, O, {{scale, auto}, [], []});
linegraph([Data, O, Min, Max]) ->
    lg1(Data, O, {{Min, Max}, [], []});
linegraph([Data, O, Min, Max, XAxis]) ->
    lg1(Data, O, {{Min, Max}, XAxis, []});
linegraph([Data, O, Min, Max, XAxis, Cols]) ->
    lg1(Data, O, {{Min, Max}, XAxis, Cols}).

lg1(Data, Orientation, {Scale, Axes, Colours}) ->
    Orientation2 = cast_orientation(Orientation),
    case has_error([Data, Orientation2, Scale, Axes, Colours]) of
        {true, Error} -> Error;
        false         -> lg2(Data, Orientation2, Scale, Axes, Colours)
    end.

lg2(Data, Orientation, Scale, Axes, Colours) ->
    {Data2, _NoOfRows, _NoOfCols} = extract(Data, Orientation),
    Data3 = [cast_data(X) || X <- Data2],
    % check for errors and then fix the reversed lists problems
    Colours2 = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data3),
    XAxis2 = get_axes(Axes),
    case has_error([Data3, Colours2, Min, Max, XAxis2]) of
        {true, Error} -> Error;
        false         -> lg3(rev(Data3), {Min, Max}, XAxis2, Colours2)
    end.

lg3(Data, {Min, Max}, XAxis, Colours) ->
    "<img src='http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ conv_data(Data) ++ "&amp;cht=lc&amp;"
        ++ "chds="++ tconv:to_s(Min) ++ "," ++ tconv:to_s(Max)
        ++ "&amp;chxt="
        ++ conv_x_axis(XAxis)
        ++ "1:|" ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ conv_colours(Colours)
        ++ "' />".


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
    Data2 = lists:reverse(cast_data(Data)),
    Titles2 = cast_titles(Titles),
    Colours2 = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data2),
    case has_error([Data2, Min, Max, Titles2, Colours2]) of
        {true, Error} -> Error;
        false         -> hist2(Data2, Min, Max, Titles2, Colours2)
    end.

hist2(Data, Min, Max, Titles, Colours) ->
    Data1 = make_data(normalise(Data)),
    Titles1 = make_titles(Titles),
    Colours1 = conv_colours(Colours),
    
    "<img src='http://chart.apis.google.com/chart?cht=bvs&amp;chd=t:"
        ++ Data1
        ++ "&amp;chxt=y&amp;chxl=0:|"
        ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ "&amp;chs=250x100&amp;chl="
        ++ Titles1
        ++ Colours1
        ++"' />".

pie1(Data, Titles, Colours) ->
    Data2 = lists:reverse(cast_data(Data)),
    Titles2 = cast_titles(Titles),
    Colours2 = get_colours(Colours),
    case has_error([Data2, Titles2, Colours2]) of
        {true, Error} -> Error;
        false         -> pie2(Data2, Titles2, Colours2)
    end.

pie2(Data, Titles, Colours) ->
    Data1 = make_data(normalise(Data)),
    Titles1 = make_titles(Titles),
    Colours1 = conv_colours(Colours),
    "<img src='http://chart.apis.google.com/chart?cht=p3&amp;chd=t:"
        ++ Data1
        ++ "&amp;chs=450x100&amp;chl="
        ++ Titles1
        ++ Colours1
        ++ "' />".

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

cast_orientation(O) ->
    col([O],
        [eval_funs,
         fetch,
         {cast, num, bool, ?ERRVAL_VAL},
         {cast, str, bool, ?ERRVAL_VAL}],
        [return_errors, {all, fun is_boolean/1}]).

conv_colours([[]])    ->
    [];
conv_colours(Colours) ->
    "&amp;chco=" ++ make_colours(Colours).

conv_x_axis([]) ->
    "y&amp;chxl=";
conv_x_axis(XAxis) ->
    "x,y&amp;chxl=0:|" ++ string:join(XAxis, "|") ++ "|". 

conv_data(Data) ->
    conv_d1(Data, []).

conv_d1([], Acc) ->
    string:join(lists:reverse(Acc), "|");
conv_d1([H | T], Acc) ->
    conv_d1(T, [make_data(H) | Acc]).

get_colours(Colours) ->
    col([Colours],
         [eval_funs,
          {ignore, bool},
          {ignore, num},
          {ignore,date},
          fetch, flatten],
          [return_errors, {all, fun is_list/1}]).

get_axes(XAxis)  ->
    col([XAxis],
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
get_scale(Scale, _Data) ->
    Scale.

extract({range, Data}, ?ROW)    ->
    tartup(Data);
extract({range, Data}, ?COLUMN) ->
    tartup(hslists:transpose(Data)).

tartup(Data) ->
    [F | _T] = Data,
    NoOfCols = length(F), % rectangular matrix
    {chunk(Data), length(Data), NoOfCols}.

rev(List) ->
    rev1(List, []).

rev1([], Acc)      ->
    lists:reverse(Acc);
rev1([H | T], Acc) ->
    rev1(T, [lists:reverse(H) | Acc]).

chunk(Data) ->
    chk2(Data, []).

chk2([], Acc)      ->
    lists:reverse(Acc);
chk2([H | T], Acc) ->
    chk2(T, [{range, [H]} | Acc]).

has_error([]) ->
    false;
has_error([{errval, Val} | _T]) ->
    {true, {errval, Val}};
has_error([H | T]) when is_list(H) ->
    case has_error(H) of
        {true, E} -> {true, E};
        false     -> has_error(T)
    end;
has_error([_H | T]) ->
    has_error(T).

normalise(Data) ->
    Total = lists:sum(Data),
    Fun = fun(X) -> trunc(100*X/Total) end,
    lists:map(Fun, Data).

make_data(List) ->
    make_d1(List, []).

make_d1([], Acc) ->
    string:join(lists:reverse(Acc), ",");
make_d1([H | T], Acc) when is_integer(H) ->
    make_d1(T, [integer_to_list(H) | Acc]);
make_d1([H | T], Acc) when is_float(H) ->
    make_d1(T, [tconv:to_s(H) | Acc]).

make_titles(List) ->
    string:join(List, "|").

make_colours(List) ->
    string:join([ replace_colour(Colour) || Colour <- List ], ",").

replace_colour(Colour) ->    
    case lists:keysearch(Colour, 1, colours()) of
        {value, {Colour, Val}} -> Val;
        false                  -> Colour
    end.

colours() ->
    [{"black"   , "000000"},
     {"green"   , "008000"},
     {"silver"  , "C0C0C0"},
     {"lime"    , "00FF00"}, 
     {"gray"    , "808080"},
     {"olive"   , "808000"},
     {"white"   , "FFFFFF"},
     {"yellow"  , "FFFF00"},
     {"maroon"  , "800000"},
     {"navy"    , "000080"}, 
     {"red"     , "FF0000"},
     {"blue"    , "0000FF"},
     {"purple"  , "800080"},
     {"teal"    , "008080"}, 
     {"fuchsia" , "FF00FF"},
     {"aqua"    , "00FFFF"}].
