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

-define(ROW, [true]).
-define(COLUMN, [false]).

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
    "![graph](http://chart.apis.google.com/chart?cht=bvs&amp;chd=t:"
        ++ Data1
        ++ "&amp;chxt=y&amp;chxl=0:|"
        ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ "&amp;chs=250x100&amp;chl="
        ++ Titles1
        ++ Colours1.

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
    "![graph](http://chart.apis.google.com/chart?cht=p3&amp;chd=t:"
        ++ Data1
        ++ "&amp;chs=450x100&amp;chl="
        ++ Titles1
        ++ Colours1.

lg1(Data, Orientation, {Scale, Axes, Colours}) ->
    Orientation2 = cast_orientation(Orientation),
    case has_error([Orientation2]) of
        {true, Error} -> Error;
        false         -> lg2(Data, Orientation2, Scale, Axes, Colours)
    end.

lg2(Data, Orientation, Scale, Axes, Colours) ->
    {Data2, _NoOfRows, _NoOfCols} = extract(Data, Orientation),
    Data3 = rev([cast_data(X) || X <- Data2]),
    % check for errors and then fix the reversed lists problems
    Colours2 = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data3),
    XAxis2 = get_axes(Axes),
    case has_error([Data3, Colours2, Min, Max, XAxis2]) of
        {true, Error} -> Error;
        false         -> lg2(rev(Data3), {Min, Max}, XAxis2, Colours2)
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

cast_orientation(O) ->
    col([O],
        [eval_funs,
         fetch,
         {cast, num, bool, ?ERRVAL_VAL},
         {cast, str, bool, ?ERRVAL_VAL}],
        [return_errors, {all, fun is_boolean/1}]).

conv_colours([[]])      -> [];
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
          [return_errors, {all, fun is_list/1}]).

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

make_c1([], Acc) -> string:join(lists:reverse(Acc), ",");
make_c1(["aliceblue"            | T], Acc) -> make_c1(T, ["F0F8FF" | Acc]);
make_c1(["antiquewhite"         | T], Acc) -> make_c1(T, ["FAEBD7" | Acc]);
make_c1(["aqua"                 | T], Acc) -> make_c1(T, ["00FFFF" | Acc]);
make_c1(["aquamarine"           | T], Acc) -> make_c1(T, ["7FFFD4" | Acc]);
make_c1(["azure"                | T], Acc) -> make_c1(T, ["F0FFFF" | Acc]);
make_c1(["beige"                | T], Acc) -> make_c1(T, ["F5F5DC" | Acc]);
make_c1(["bisque"               | T], Acc) -> make_c1(T, ["FFE4C4" | Acc]);
make_c1(["black"                | T], Acc) -> make_c1(T, ["000000" | Acc]);
make_c1(["blanchedalmond"       | T], Acc) -> make_c1(T, ["FFEBCD" | Acc]);
make_c1(["blue"                 | T], Acc) -> make_c1(T, ["0000FF" | Acc]);
make_c1(["blueviolet"           | T], Acc) -> make_c1(T, ["8A2BE2" | Acc]);
make_c1(["brown"                | T], Acc) -> make_c1(T, ["A52A2A" | Acc]);
make_c1(["burlywood"            | T], Acc) -> make_c1(T, ["DEB887" | Acc]);
make_c1(["cadetblue"            | T], Acc) -> make_c1(T, ["5F9EA0" | Acc]);
make_c1(["chartreuse"           | T], Acc) -> make_c1(T, ["7FFF00" | Acc]);
make_c1(["chocolate"            | T], Acc) -> make_c1(T, ["D2691E" | Acc]);
make_c1(["coral"                | T], Acc) -> make_c1(T, ["FF7F50" | Acc]);
make_c1(["cornflowerblue"       | T], Acc) -> make_c1(T, ["6495ED" | Acc]);
make_c1(["cornsilk"             | T], Acc) -> make_c1(T, ["FFF8DC" | Acc]);
make_c1(["crimson"              | T], Acc) -> make_c1(T, ["DC143C" | Acc]);
make_c1(["cyan"                 | T], Acc) -> make_c1(T, ["00FFFF" | Acc]);
make_c1(["darkblue"             | T], Acc) -> make_c1(T, ["00008B" | Acc]);
make_c1(["darkcyan"             | T], Acc) -> make_c1(T, ["008B8B" | Acc]);
make_c1(["darkgoldenrod"        | T], Acc) -> make_c1(T, ["B8860B" | Acc]);
make_c1(["darkgray"             | T], Acc) -> make_c1(T, ["A9A9A9" | Acc]);
make_c1(["darkgreen"            | T], Acc) -> make_c1(T, ["006400" | Acc]);
make_c1(["darkkhaki"            | T], Acc) -> make_c1(T, ["BDB76B" | Acc]);
make_c1(["darkmagenta"          | T], Acc) -> make_c1(T, ["8B008B" | Acc]);
make_c1(["darkolivegreen"       | T], Acc) -> make_c1(T, ["556B2F" | Acc]);
make_c1(["darkorange"           | T], Acc) -> make_c1(T, ["FF8C00" | Acc]);
make_c1(["darkorchid"           | T], Acc) -> make_c1(T, ["9932CC" | Acc]);
make_c1(["darkred"              | T], Acc) -> make_c1(T, ["8B0000" | Acc]);
make_c1(["darksalmon"           | T], Acc) -> make_c1(T, ["E9967A" | Acc]);
make_c1(["darkseagreen"         | T], Acc) -> make_c1(T, ["8FBC8F" | Acc]);
make_c1(["darkslateblue"        | T], Acc) -> make_c1(T, ["483D8B" | Acc]);
make_c1(["darkslategray"        | T], Acc) -> make_c1(T, ["2F4F4F" | Acc]);
make_c1(["darkturquoise"        | T], Acc) -> make_c1(T, ["00CED1" | Acc]);
make_c1(["darkviolet"           | T], Acc) -> make_c1(T, ["9400D3" | Acc]);
make_c1(["deeppink"             | T], Acc) -> make_c1(T, ["FF1493" | Acc]);
make_c1(["deepskyblue"          | T], Acc) -> make_c1(T, ["00BFFF" | Acc]);
make_c1(["dimgray"              | T], Acc) -> make_c1(T, ["696969" | Acc]);
make_c1(["dodgerblue"           | T], Acc) -> make_c1(T, ["1E90FF" | Acc]);
make_c1(["firebrick"            | T], Acc) -> make_c1(T, ["B22222" | Acc]);
make_c1(["floralwhite"          | T], Acc) -> make_c1(T, ["FFFAF0" | Acc]);
make_c1(["forestgreen"          | T], Acc) -> make_c1(T, ["228B22" | Acc]);
make_c1(["fuchsia"              | T], Acc) -> make_c1(T, ["FF00FF" | Acc]);
make_c1(["gainsboro"            | T], Acc) -> make_c1(T, ["DCDCDC" | Acc]);
make_c1(["ghostwhite"           | T], Acc) -> make_c1(T, ["F8F8FF" | Acc]);
make_c1(["gold"                 | T], Acc) -> make_c1(T, ["FFD700" | Acc]);
make_c1(["goldenrod"            | T], Acc) -> make_c1(T, ["DAA520" | Acc]);
make_c1(["gray"                 | T], Acc) -> make_c1(T, ["808080" | Acc]);
make_c1(["green"                | T], Acc) -> make_c1(T, ["008000" | Acc]);
make_c1(["greenyellow"          | T], Acc) -> make_c1(T, ["ADFF2F" | Acc]);
make_c1(["honeydew"             | T], Acc) -> make_c1(T, ["F0FFF0" | Acc]);
make_c1(["hotpink"              | T], Acc) -> make_c1(T, ["FF69B4" | Acc]);
make_c1(["indianred"            | T], Acc) -> make_c1(T, ["CD5C5C" | Acc]);
make_c1(["indigo"               | T], Acc) -> make_c1(T, ["4B0082" | Acc]);
make_c1(["ivory"                | T], Acc) -> make_c1(T, ["FFFFF0" | Acc]);
make_c1(["khaki"                | T], Acc) -> make_c1(T, ["F0E68C" | Acc]);
make_c1(["lavender"             | T], Acc) -> make_c1(T, ["E6E6FA" | Acc]);
make_c1(["lavenderblush"        | T], Acc) -> make_c1(T, ["FFF0F5" | Acc]);
make_c1(["lawngreen"            | T], Acc) -> make_c1(T, ["7CFC00" | Acc]);
make_c1(["lemonchiffon"         | T], Acc) -> make_c1(T, ["FFFACD" | Acc]);
make_c1(["lightblue"            | T], Acc) -> make_c1(T, ["ADD8E6" | Acc]);
make_c1(["lightcoral"           | T], Acc) -> make_c1(T, ["F08080" | Acc]);
make_c1(["lightcyan"            | T], Acc) -> make_c1(T, ["E0FFFF" | Acc]);
make_c1(["lightgoldenrodyellow" | T], Acc) -> make_c1(T, ["FAFAD2" | Acc]);
make_c1(["lightgreen"           | T], Acc) -> make_c1(T, ["90EE90" | Acc]);
make_c1(["lightgrey"            | T], Acc) -> make_c1(T, ["D3D3D3" | Acc]);
make_c1(["lightpink"            | T], Acc) -> make_c1(T, ["FFB6C1" | Acc]);
make_c1(["lightsalmon"          | T], Acc) -> make_c1(T, ["FFA07A" | Acc]);
make_c1(["lightseagreen"        | T], Acc) -> make_c1(T, ["20B2AA" | Acc]);
make_c1(["lightskyblue"         | T], Acc) -> make_c1(T, ["87CEFA" | Acc]);
make_c1(["lightslategray"       | T], Acc) -> make_c1(T, ["778899" | Acc]);
make_c1(["lightsteelblue"       | T], Acc) -> make_c1(T, ["B0C4DE" | Acc]);
make_c1(["lightyellow"          | T], Acc) -> make_c1(T, ["FFFFE0" | Acc]);
make_c1(["lime"                 | T], Acc) -> make_c1(T, ["00FF00" | Acc]);
make_c1(["limegreen"            | T], Acc) -> make_c1(T, ["32CD32" | Acc]);
make_c1(["linen"                | T], Acc) -> make_c1(T, ["FAF0E6" | Acc]);
make_c1(["magenta"              | T], Acc) -> make_c1(T, ["FF00FF" | Acc]);
make_c1(["maroon"               | T], Acc) -> make_c1(T, ["800000" | Acc]);
make_c1(["mediumaquamarine"     | T], Acc) -> make_c1(T, ["66CDAA" | Acc]);
make_c1(["mediumblue"           | T], Acc) -> make_c1(T, ["0000CD" | Acc]);
make_c1(["mediumorchid"         | T], Acc) -> make_c1(T, ["BA55D3" | Acc]);
make_c1(["mediumpurple"         | T], Acc) -> make_c1(T, ["9370DB" | Acc]);
make_c1(["mediumseagreen"       | T], Acc) -> make_c1(T, ["3CB371" | Acc]);
make_c1(["mediumslateblue"      | T], Acc) -> make_c1(T, ["7B68EE" | Acc]);
make_c1(["mediumspringgreen"    | T], Acc) -> make_c1(T, ["00FA9A" | Acc]);
make_c1(["mediumturquoise"      | T], Acc) -> make_c1(T, ["48D1CC" | Acc]);
make_c1(["mediumvioletred"      | T], Acc) -> make_c1(T, ["C71585" | Acc]);
make_c1(["midnightblue"         | T], Acc) -> make_c1(T, ["191970" | Acc]);
make_c1(["mintcream"            | T], Acc) -> make_c1(T, ["F5FFFA" | Acc]);
make_c1(["mistyrose"            | T], Acc) -> make_c1(T, ["FFE4E1" | Acc]);
make_c1(["moccasin"             | T], Acc) -> make_c1(T, ["FFE4B5" | Acc]);
make_c1(["navajowhite"          | T], Acc) -> make_c1(T, ["FFDEAD" | Acc]);
make_c1(["navy"                 | T], Acc) -> make_c1(T, ["000080" | Acc]);
make_c1(["oldlace"              | T], Acc) -> make_c1(T, ["FDF5E6" | Acc]);
make_c1(["olive"                | T], Acc) -> make_c1(T, ["808000" | Acc]);
make_c1(["olivedrab"            | T], Acc) -> make_c1(T, ["6B8E23" | Acc]);
make_c1(["orange"               | T], Acc) -> make_c1(T, ["FFA500" | Acc]);
make_c1(["orangered"            | T], Acc) -> make_c1(T, ["FF4500" | Acc]);
make_c1(["orchid"               | T], Acc) -> make_c1(T, ["DA70D6" | Acc]);
make_c1(["palegoldenrod"        | T], Acc) -> make_c1(T, ["EEE8AA" | Acc]);
make_c1(["palegreen"            | T], Acc) -> make_c1(T, ["98FB98" | Acc]);
make_c1(["paleturquoise"        | T], Acc) -> make_c1(T, ["AFEEEE" | Acc]);
make_c1(["palevioletred"        | T], Acc) -> make_c1(T, ["DB7093" | Acc]);
make_c1(["papayawhip"           | T], Acc) -> make_c1(T, ["FFEFD5" | Acc]);
make_c1(["peachpuff"            | T], Acc) -> make_c1(T, ["FFDAB9" | Acc]);
make_c1(["peru"                 | T], Acc) -> make_c1(T, ["CD853F" | Acc]);
make_c1(["pink"                 | T], Acc) -> make_c1(T, ["FFC0CB" | Acc]);
make_c1(["plum"                 | T], Acc) -> make_c1(T, ["DDA0DD" | Acc]);
make_c1(["powderblue"           | T], Acc) -> make_c1(T, ["B0E0E6" | Acc]);
make_c1(["purple"               | T], Acc) -> make_c1(T, ["800080" | Acc]);
make_c1(["red"                  | T], Acc) -> make_c1(T, ["FF0000" | Acc]);
make_c1(["rosybrown"            | T], Acc) -> make_c1(T, ["BC8F8F" | Acc]);
make_c1(["royalblue"            | T], Acc) -> make_c1(T, ["041690" | Acc]);
make_c1(["saddlebrown"          | T], Acc) -> make_c1(T, ["8B4513" | Acc]);
make_c1(["salmon"               | T], Acc) -> make_c1(T, ["FA8072" | Acc]);
make_c1(["sandybrown"           | T], Acc) -> make_c1(T, ["F4A460" | Acc]);
make_c1(["seagreen"             | T], Acc) -> make_c1(T, ["2E8B57" | Acc]);
make_c1(["seashell"             | T], Acc) -> make_c1(T, ["FFF5EE" | Acc]);
make_c1(["sienna"               | T], Acc) -> make_c1(T, ["A0522D" | Acc]);
make_c1(["silver"               | T], Acc) -> make_c1(T, ["C0C0C0" | Acc]);
make_c1(["skyblue"              | T], Acc) -> make_c1(T, ["87CEEB" | Acc]);
make_c1(["slateblue"            | T], Acc) -> make_c1(T, ["6A5ACD" | Acc]);
make_c1(["slategray"            | T], Acc) -> make_c1(T, ["708090" | Acc]);
make_c1(["snow"                 | T], Acc) -> make_c1(T, ["FFFAFA" | Acc]);
make_c1(["springgreen"          | T], Acc) -> make_c1(T, ["00FF7F" | Acc]);
make_c1(["steelblue"            | T], Acc) -> make_c1(T, ["4682B4" | Acc]);
make_c1(["tan"                  | T], Acc) -> make_c1(T, ["D2B48C" | Acc]);
make_c1(["teal"                 | T], Acc) -> make_c1(T, ["088080" | Acc]);
make_c1(["thistle"              | T], Acc) -> make_c1(T, ["D8BFD8" | Acc]);
make_c1(["tomato"               | T], Acc) -> make_c1(T, ["FF6347" | Acc]);
make_c1(["turquoise"            | T], Acc) -> make_c1(T, ["40E0D0" | Acc]);
make_c1(["violet"               | T], Acc) -> make_c1(T, ["EE82EE" | Acc]);
make_c1(["wheat"                | T], Acc) -> make_c1(T, ["F5DEB3" | Acc]);
make_c1(["white"                | T], Acc) -> make_c1(T, ["FFFFFF" | Acc]);
make_c1(["whitesmoke"           | T], Acc) -> make_c1(T, ["F5F5F5" | Acc]);
make_c1(["yellow"               | T], Acc) -> make_c1(T, ["FFFF00" | Acc]);
make_c1(["yellowgreen"          | T], Acc) -> make_c1(T, ["9ACD32" | Acc]);
make_c1([H | T], Acc) -> make_c1(T, [H | Acc]).
