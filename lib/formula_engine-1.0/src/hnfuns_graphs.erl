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
         'sparkline.1x1'/1,
         'sparkline.2x2'/1,
         'xy.3x6'/1,
         'xy.4x8'/1,
         'xy.6x12'/1,
         'speedo.2x4'/1,
         'speedo.3x6'/1,
         'speedo.4x8'/1,
         'speedo.6x12'/1,
         'linegraph.3x6'/1,
         'linegraph.4x8'/1,
         'linegraph.6x12'/1,
         'dategraph.3x6'/1,
         'dategraph.4x8'/1,
         'dategraph.6x12'/1,
         linegraph/1,
         piechart/1,
         histogram/1,
         barchart/1
        ]).

-define(ROW,    true).
-define(COLUMN, false).

-include("typechecks.hrl").
-include("muin_records.hrl").

%% definitions of standard abbreviations
-define(apiurl,     "<img src='http://chart.apis.google.com/chart?").
-define(urlclose,   "' />").
-define(axesrange,  "chxr").  % 0=x, 1=Y, min, max (seperated by |)"
-define(axes,       "chxt").  % handles multiple axes
-define(size,       "chs").   % Width x Height
-define(colours,    "chco").  % colours of the lines
-define(data,       "chd").   % data - depends on chart time
-define(datalables, "chdl").  % separated by |
-define(type,       "cht").
-define(legendpos,  "chdlp"). % t | l | r | b
-define(linestyles, "chls").
-define(margins,    "chma").
-define(title,      "chtt").
-define(axeslables, "chxl").
-define(axeslabpos, "chxp").
-define(tickmarks,  "chxt").
-define(speedolab,  "chl").

% definition of standard stuff
-define(SIZE1x1,     "66x20").
-define(SIZE2x2,     "146x42").
-define(SIZE2x4,     "146x86").
-define(SIZE3x6,     "218x130").
-define(SIZE4x8,     "306x174").
-define(SIZE6x12,    "466x262").
-define(NORMALAXES,  "x,y").
-define(LABELAXES,   "x,x,y,y").
-define(XYLINE,      "lxy").
-define(SPARKLINE,   "ls").
-define(TOPHORIZ,    "t").
-define(TOPVERT,     "tv").
-define(RIGHTVERT,   "r").
-define(LEFTVERT,    "l").
-define(BOTHORIZ,    "b").
-define(BOTVERT,     "bv").
-define(NORMMARGINS, "5,5,5,5").
-define(BOTHAXES,    "x,y").

-define(SPCOLOURS, [
                    "444444",
                    "CCCCCC",
                    "888888",
                    "666666",
                    "CCCCCC"
                   ]).

-define(XYCOLOURS, [
                    "000000",
                    "008000",
                    "800000",
                    "000080", 
                    "FF0000",
                    "0000FF",
                    "800080",
                    "C0C0C0",
                    "00FF00", 
                    "808080",
                    "808000",
                    %"FFFFFF",
                    "008080", 
                    "FF00FF",
                    "00FFFF",
                    "FFFF00"
                   ]).

%%
%% Exported functions
%%
'sparkline.1x1'(List) ->
    {Data, Colours} = chunk_spark(List),
    {resize, 1, 1, spark1(?SIZE1x1, Data, Colours)}.

'sparkline.2x2'(List) ->
    {Data, Colours} = chunk_spark(List),
    {resize, 2, 2, spark1(?SIZE2x2, Data, Colours)}.

chunk_spark([Lines | List]) ->
    [Lines1] = typechecks:std_ints([Lines]),
    muin_checks:ensure(Lines1 > 0, ?ERRVAL_NUM),
    muin_checks:ensure(Lines1 == length(List), ?ERRVAL_NUM),
    % now make the colours
    Colours = allocate_colours(Lines, ?SPCOLOURS),
    Data1 = [lists:reverse(cast_data(X)) || X <- List],
    Min = lists:min(lists:flatten(Data1)),
    Max = lists:max(lists:flatten(Data1)),
    Data2 = [normalize_sp(X, Min, Max) || X <- Data1],
    Data3 = "t:"++conv_data(Data2),
    {Data3, Colours}.

normalize_sp(List, Min, Max) ->
    Diff = Max - Min,
    Fun = fun(blank) -> blank;
             (X)     -> (X - Min)*100/Diff
          end,
    [Fun(X) || X <- List].

spark1(Size, Data, Colours) ->
    Opts = [
            {?colours, Colours},
            {?type, ?SPARKLINE},
            {?size, Size},
            {?data, Data},
            {?linestyles, "1|1"}
           ],
    make_chart(Opts).

'dategraph.3x6'(List) ->
    Ret = chunk_dategraph(List, double),
    {Data, Scale, AxesLabPos, Colours, Rest, StartDate, EndDate} = Ret,
    {resize, 3, 6, dg1(?SIZE3x6, Data, Scale, AxesLabPos, Colours, Rest,
        StartDate, EndDate, [{?tickmarks, ?BOTHAXES}])}.

'dategraph.4x8'(List) ->
    Ret = chunk_dategraph(List, double),
    {Data, Scale, AxesLabPos, Colours, Rest, StartDate, EndDate} = Ret,
    {resize, 4, 8, dg1(?SIZE4x8, Data, Scale, AxesLabPos, Colours, Rest,
                       StartDate, EndDate, [])}.

'dategraph.6x12'(List) ->
    Ret = chunk_dategraph(List, double),
    {Data, Scale, AxesLabPos, Colours, Rest, StartDate, EndDate} = Ret,
    {resize, 6, 11, dg1(?SIZE6x12, Data, Scale, AxesLabPos, Colours, Rest,
                        StartDate, EndDate, [])}.

'linegraph.3x6'(List) ->
    % There is a bug with the Y-Axis as this size!
    {Data, Scale, AxesLabPos, Colours, Rest} = chunk_linegraph(List, double),
    {resize, 3, 6, xy1(?SIZE3x6, Data, Scale, AxesLabPos, Colours, Rest,
        [{?tickmarks, ?BOTHAXES}])}.

'linegraph.4x8'(List) ->
    {Data, Scale, AxesLabPos, Colours, Rest} = chunk_linegraph(List, double),
    {resize, 4, 8, xy1(?SIZE4x8, Data, Scale, AxesLabPos, Colours, Rest, [])}.

'linegraph.6x12'(List) ->
    {Data, Scale, AxesLabPos, Colours, Rest} = chunk_linegraph(List, double),
    {resize, 6, 11, xy1(?SIZE6x12, Data, Scale, AxesLabPos, Colours, Rest, [])}.

'xy.3x6'(List) ->
    % There is a bug with the Y-Axis as this size!
    {Data, Scale, AxesLabPos, Colours, Rest} = chunk_xy(List, single),
    {resize, 3, 6, xy1(?SIZE3x6, Data, Scale, AxesLabPos, Colours, Rest,
        [{?tickmarks, ?BOTHAXES}])}.

'xy.4x8'(List) ->
    {Data, Scale, AxesLabPos, Colours, Rest} = chunk_xy(List, double),
    {resize, 4, 8, xy1(?SIZE4x8, Data, Scale, AxesLabPos, Colours, Rest, [])}.

'xy.6x12'(List) ->
    {Data, Scale, AxesLabPos, Colours, Rest} = chunk_xy(List, double),
    {resize,6, 11, xy1(?SIZE6x12, Data, Scale, AxesLabPos, Colours, Rest, [])}.

chunk_dategraph([X, Lines | List], LabType) ->
    DataX = lists:reverse(cast_dates(X)),
    Ret = chunk_l2(DataX, Lines, List),
    {MinX, MaxX, MinY, MaxY, Data, Cols, Rest} = Ret,
    StartDate = cast_date(MinX),
    EndDate = cast_date(MaxX),
    AxesLabPos = make_axes_lab_pos_date(MinX, MaxX, MaxY),
    Scale = make_scale(LabType, auto, MinX, MaxX, MinY, MaxY),
    {Data, {?axesrange, Scale}, {?axeslabpos, AxesLabPos}, Cols, Rest,
     StartDate, EndDate}.

chunk_linegraph([X, Lines | List], LabType) ->
    DataX = cast_data(X),
    Ret = chunk_l2(DataX, Lines, List),
    {MinX, MaxX, MinY, MaxY, Data, Cols, Rest} = Ret,
    AxesLabPos = make_axes_lab_pos(MaxX, MaxY),
    Scale = make_scale(LabType, auto, MinX, MaxX, MinY, MaxY),
    {Data, {?axesrange, Scale}, {?axeslabpos, AxesLabPos}, Cols, Rest}.

chunk_l2(DataX, Lines, List) ->
    [Lines1] = typechecks:std_ints([Lines]),
    muin_checks:ensure(Lines1 > 0, ?ERRVAL_NUM),
    {Data, Rest} = lists:split(Lines1, List),
    {MinY, MaxY, DataY} = process_data_linegraph(Data),
    MinX = stdfuns_stats:min(DataX),
    MaxX = stdfuns_stats:max(DataX),
    DataX2 = normalize_sp(DataX, MinX, MaxX),
    Data2 = make_data(DataX2, DataY, []),
    % now make the colours
    Colours = allocate_colours(Lines, ?XYCOLOURS),
    {MinX, MaxX, MinY, MaxY, Data2, {?colours, Colours}, Rest}.

make_data(_X, [], Acc)     -> "t:" ++ string:join(lists:reverse(Acc), "|");
make_data(X, [H | T], Acc) -> NewAcc = make_d2(X, H, [], []),
                              make_data(X, T, [NewAcc | Acc]).

make_d2([], [], A1, A2)                   -> A1a = [tconv:to_s(X) || X <- A1],
                                             A2a = [tconv:to_s(X) || X <- A2],
                                             string:join(A1a, ",") ++ "|"
                                                 ++ string:join(A2a, ",");
make_d2([_H1 | T1], [blank | T2], A1, A2) -> make_d2(T1, T2, A1, A2);
make_d2([H1 | T1], [H2 | T2], A1, A2)     -> make_d2(T1, T2, [H1 | A1], [H2 | A2]);
make_d2([], _List, _A1, _A2) -> ?ERR_VAL; % X and Y ranges must be congruent
make_d2(_List, [], _A1, _A2) -> ?ERR_VAL. % X and Y ranges must be congruent

chunk_xy([Lines | List], LabType) ->
    [Lines1] = typechecks:std_ints([Lines]),
    muin_checks:ensure(Lines1 > 0, ?ERRVAL_NUM),
    {Data, Rest} = lists:split(Lines1, List),
    {MinX, MaxX, MinY, MaxY, Data1} = process_data_xy(Data),
    Scale = make_scale(LabType, auto, MinX, MaxX, MinY, MaxY),
    AxesLabPos = make_axes_lab_pos(MaxX, MaxY),
    % now make the colours
    Colours = allocate_colours(Lines, ?XYCOLOURS),
    {Data1, {?axesrange, Scale}, {?axeslabpos, AxesLabPos},
     {?colours, Colours}, Rest}.

dg1(Size, Data, Scale, AxesLabPos, Colours, [], StartDate, EndDate, Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos,
                             make_labs_date("", "", StartDate, EndDate),
                             {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);
dg1(Size, Data, Scale, AxesLabPos, Colours, [Tt | []], StartDate, EndDate, Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs_date("", "", StartDate, EndDate),
                            {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);
dg1(Size, Data, Scale, AxesLabPos, Colours, [Tt, Xl | []],
    StartDate, EndDate, Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs_date(Xl, "", StartDate, EndDate),
                             {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);
dg1(Size, Data, Scale, AxesLabPos, Colours, [Tt, Xl, Yl | []],
    StartDate, EndDate, Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs_date(Xl, Yl, StartDate, EndDate),
                             {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);    
dg1(Size, Data, Scale, AxesLabPos, Colours, [Tt, Xl, Yl, Srs | []],
    StartDate, EndDate, Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs_date(Xl, Yl, StartDate, EndDate),
                             {?axes, ?LABELAXES},
                             {?legendpos, ?TOPHORIZ}, make_series(Srs)], Opts]),
    xy2(Size, Data, NewOpts).

xy1(Size, Data, Scale, AxesLabPos, Colours, [], Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_labs("", ""),
                             {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);
xy1(Size, Data, Scale, AxesLabPos, Colours, [Tt | []], Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs("", ""),
                            {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);
xy1(Size, Data, Scale, AxesLabPos, Colours, [Tt, Xl | []], Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs(Xl, ""), {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);
xy1(Size, Data, Scale, AxesLabPos, Colours, [Tt, Xl, Yl | []], Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs(Xl, Yl), {?axes, ?LABELAXES}], Opts]),
    xy2(Size, Data, NewOpts);    
xy1(Size, Data, Scale, AxesLabPos, Colours, [Tt, Xl, Yl, Srs | []], Opts) ->
    NewOpts = lists:concat([[Scale, Colours, AxesLabPos, make_title(Tt),
                             make_labs(Xl, Yl), {?axes, ?LABELAXES},
                             {?legendpos, ?TOPHORIZ}, make_series(Srs)], Opts]),
    xy2(Size, Data, NewOpts).

xy2(Size, Data, Opts) ->
    case has_error([Data]) of
        {true, Error} -> Error;
        false         -> xy3(Size, Data, Opts)
    end.

xy3(Size, Data, Opts) ->
    NewOpts = [
               {?type, ?XYLINE},
               {?size, Size},
               {?data, Data},
               {?linestyles, "1|1"}
              ],
    case Opts of
        [] -> make_chart(NewOpts);
        _  -> make_chart(lists:concat([Opts, NewOpts]))
    end.

make_series(Srs) ->
    Srs2 = typechecks:flat_strs([Srs]),
    {?datalables, string:join(Srs2, "|")}.

make_labs_date(X, Y, StartDate, EndDate) ->
    [X1, Y1] = typechecks:std_strs([X, Y]),
    {?axeslables, "0:|"++StartDate++"|"++EndDate
           ++"|1:||"++X1++"|3:||"++Y1}.

make_labs(X, Y) ->
    [X1, Y1] = typechecks:std_strs([X, Y]),
    {?axeslables, "1:|"++X1++"|3:|"++Y1}.
     
make_title(Title) ->
    [T2] = typechecks:std_strs([Title]),
    {?title, T2}.

make_axes_lab_pos_date(MinX, MaxX, MaxY) ->
    "0,"++tconv:to_s(MinX)++","++tconv:to_s(MaxX)++"|1,"
        ++tconv:to_s(MaxX)++"|3,"++tconv:to_s(MaxY).

make_axes_lab_pos(MaxX, MaxY) ->
    "1,"++tconv:to_s(MaxX)++"|3,"++tconv:to_s(MaxY).

make_scale(null, _, _, _, _, _) -> "";
make_scale(Type, auto, MinX, MaxX, MinY, MaxY) ->
    make_s1(Type, MinX, MaxX, MinY, MaxY).
% make_scale(Type, [X1, X2 | []], _MinX, _MaxX, MinY, MaxY) ->
%     [X1a, X2a] = typechecks:std_nums([X1, X2]),
%     make_s1(Type, X1a, X2a, MinY, MaxY).

make_s1(single, MinX, MaxX, MinY, MaxY) ->
    "0,"++tconv:to_s(MinX)++","++tconv:to_s(MaxX)
        ++"|2,"++tconv:to_s(MinY)++","++tconv:to_s(MaxY);
make_s1(double, MinX, MaxX, MinY, MaxY) ->
    "0,"++tconv:to_s(MinX)++","++tconv:to_s(MaxX)
        ++"|1,"++tconv:to_s(MinX)++","++tconv:to_s(MaxX)
        ++"|2,"++tconv:to_s(MinY)++","++tconv:to_s(MaxY)
        ++"|3,"++tconv:to_s(MinY)++","++tconv:to_s(MaxY).
cast_date(N) ->
    {datetime, D, T} = muin_util:cast(N, num, date),
    dh_date:format("d-M-y", {D, T}).

make_chart(List) -> make_c(List, []).

make_c([], Acc)           -> lists:flatten([?apiurl | Acc]) ++ ?urlclose;
make_c([{K, V} | T], Acc) -> NewAcc = "&amp;" ++ K ++ "=" ++ V,
                             make_c(T, [NewAcc | Acc]).

'speedo.2x4'([_V] = L)               -> speedo(?SIZE2x4, L);
'speedo.2x4'([_V, _Tt] = L)          -> speedo(?SIZE2x4, L);
'speedo.2x4'([_V, _Tt, _S] =  L)     -> speedo(?SIZE2x4, L);
'speedo.2x4'([_V, _Tt, _S, _Th] = L) -> speedo(?SIZE2x4, L).

'speedo.3x6'(List)  -> {resize, 3, 6,  speedo(?SIZE3x6,  List)}.
'speedo.4x8'(List)  -> {resize, 4, 8,  speedo(?SIZE4x8,  List)}.
'speedo.6x12'(List) -> {resize, 6, 12, speedo(?SIZE6x12, List)}.

speedo(Size, [V])                     -> V2 = cast_val(V),
                                         speedo1(Size, V2, "", "", "", "", 1);
speedo(Size, [V, Tt])                 -> V2 = cast_val(V),
                                         speedo1(Size, V2, Tt, "", "", "", 1);
speedo(Size, [V, Tt, SubT])           -> V2 = cast_val(V),
                                         speedo1(Size, V2, Tt, SubT, "", "", 1);
speedo(Size, [V, Tt, SubT, Th])       -> Scale = speedo_scale(Th),
                                         V2 = cast_val(V),
                                         speedo1(Size, V2, Tt, SubT, Th, "", Scale);
speedo(Size, [V, Tt, SubT, Th, Labs]) -> Scale = speedo_scale(Th),
                                         V2 = cast_val(V),
                                         speedo1(Size, V2, Tt, SubT, Th, Labs, Scale).
cast_val(Val) ->  cast_v2(Val, 0, 100, 1).

cast_v2(Val, Min, Max, Scale) ->
    if
        Val < Min                     -> ?ERRVAL_VAL;
        Val > Max                     -> ?ERRVAL_VAL;
        Min =< Val andalso Val =< Max -> Val * Scale
    end.

speedo1(Size, Val, Title, Subtitle, Threshold, Lables, Scale) ->
    [Tt2]  = cast_titles(Title),
    [Sb2]  = cast_titles(Subtitle),
    %[Th2]  = cast_data(Threshold),
    %[Lab2] = cast_titles(Lables),
    if
        Val < 0                             -> ?ERRVAL_VAL;
        Val > 100 * Scale                   -> ?ERRVAL_VAL;
        0 =< Val andalso Val =< 100 * Scale ->
            Opts = [],
            NewOpts = [
                       {?axeslables, "0:|OK|Beware|Danger"},
                       {?tickmarks, "y"},
                       {?size, Size},
                       {?type, "gm"},
                       {?colours, "000000,008000|FFCC33|FF0000"},
                       {?data, "t:" ++ tconv:to_s(Val)},
                       {?speedolab, Sb2},
                       {?title, Tt2}
                       ],
            %        "<img src='http://chart.apis.google.com/chart" ++
            % "?chxl=0:|OK|Beware|Danger" ++
            % "&amp;chxt=y" ++
            % "&amp;chs=" ++Size++
            % "&amp;cht=gm" ++
            % "&amp;chco=000000,008000|FFCC33|FF0000" ++
            % "&amp;chd=t:" ++ tconv:to_s(Val2 * 100) ++
            % "&amp;chl=" ++ S2 ++
            % "&amp;chtt=" ++ T2 ++
            % "'>"
            case Opts of
                [] -> make_chart(NewOpts);
                _  -> make_chart(lists:concat([Opts, NewOpts]))
            end
    end.

speedo_scale(Th) ->
    [Zero, Orange, Red, Max] = cast_data(Th),
    1.

barchart([Data]) ->
    bar(Data, 0, {{scale, auto}, [], []});
barchart([Data, O])           ->
    bar(Data, O, {{scale, auto}, [], []});
barchart([Data, O, XAxis]) ->
    bar(Data, O, {{scale, auto}, XAxis, []});
barchart([Data, O, XAxis, Cols]) ->
    bar(Data, O, {{scale, auto}, XAxis, Cols});
barchart([Data, O, XAxis, Cols, Min, Max]) ->
    bar(Data, O, {{Min, Max}, XAxis, Cols}).

bar(Data, Orientation, {Scale, Axes, Colours}) ->
    Orientation2 = cast_orientation(Orientation),
    case has_error([Data, Orientation2, Scale, Axes, Colours]) of
        {true, Error} -> Error;
        false         -> bar2(Data, Orientation2, Scale, Axes, Colours)
    end.

bar2(Data, Orientation, Scale, Axes, Colours) ->
    {Data2, _NoOfRows, _NoOfCols} = extract(Data, Orientation),
    Data3      = [cast_data(X) || X <- Data2],
    Colours2   = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data3),
    XAxis2     = get_axes(Axes),
    case has_error([Data3, Colours2, Min, Max, XAxis2]) of
        {true, Error} -> Error;
        false         -> bar3(rev(Data3), {Min, Max}, XAxis2, Colours2)
    end.

bar3(Data, {Min, Max}, XAxis, Colours) ->
    "<img src='http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ conv_data(Data) ++ "&amp;cht=bvs&amp;"
        ++ "chds="++ tconv:to_s(Min) ++ "," ++ tconv:to_s(Max)
        ++ "&amp;"
        ++ "chxt=y&amp;"
        ++ "chxt="
        ++ conv_x_axis(XAxis)
        ++ "1:|" ++ tconv:to_s(Min) ++ "|" ++ tconv:to_s(Max)
        ++ conv_colours(Colours)
        ++ "' />".

linegraph([Data]) ->
    linegraph([Data, ?ROW]);
linegraph([Data, O])           ->
    lg1(Data, O, {{scale, auto}, [], []});
linegraph([Data, O, XAxis]) ->
    lg1(Data, O, {{scale, auto}, XAxis, []});
linegraph([Data, O, XAxis, Cols]) ->
    lg1(Data, O, {{scale, auto}, XAxis, Cols});
linegraph([Data, O, XAxis, Cols, Min, Max]) ->
    lg1(Data, O, {{Min, Max}, XAxis, Cols}).


lg1(Data, Orientation, {Scale, Axes, Colours}) ->
    Orientation2 = cast_orientation(Orientation),
    case has_error([Data, Orientation2, Scale, Axes, Colours]) of
        {true, Error} -> Error;
        false         -> lg2(Data, Orientation2, Scale, Axes, Colours)
    end.

lg2(Data, Orientation, Scale, Axes, Colours) ->
    {Data2, _NoOfRows, _NoOfCols} = extract(Data, Orientation),
    Data3      = [cast_data(X) || X <- Data2],
    Colours2   = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data3),
    XAxis2     = get_axes(Axes),
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
process_data_linegraph(Data) ->
    Data1 = [proc_dxy1(X) || X <- Data],
    Data2 = [X || {X, _NoR, _NoC} <- Data1],
    Data3 = [lists:flatten([lists:reverse(cast_linegraph_data(X)) || X <- X1])
             || X1 <- Data2],
    {MinY, MaxY} = get_maxes_lg(Data3),
    Data4 = normalize_linegraph(Data3, MinY, MaxY, []),
    {MinY, MaxY, Data4}.

process_data_xy(Data) ->
    Data1 = [proc_dxy1(X) || X <- Data],
    Data2 = [X || {X, _NoR, _NoC} <- Data1],
    Data3 = [[lists:reverse(cast_data(X)) || X <- X1] || X1 <- Data2],
    {MinX, MaxX, MinY, MaxY} = get_maxes(Data3),
    Data4 = normalize_xy(Data3, MinX, MaxX, MinY, MaxY, []),
    Data5 = [conv_data(X) || X <- Data4],
    {MinX, MaxX, MinY, MaxY, "t:"++string:join(Data5, "|")}.

normalize_linegraph([], _, _, Acc) -> lists:reverse(Acc);
normalize_linegraph([H | T], Min, Max, Acc) ->
    NewAcc = lists:reverse(normalize_sp(H, Min, Max)),
    normalize_linegraph(T, Min, Max, [NewAcc | Acc]).

normalize_xy([], _, _, _, _, Acc) -> lists:reverse(Acc);
normalize_xy([[H1, H2] | T], MinX, MaxX, MinY, MaxY, Acc) ->
    NewH1 = lists:reverse(normalize_sp(H1, MinX, MaxX)),
    NewH2 = lists:reverse(normalize_sp(H2, MinY, MaxY)),
    normalize_xy(T, MinX, MaxX, MinY, MaxY, [[NewH1, NewH2] | Acc]).

proc_dxy1({range, X} = R) ->
    if
        length(X) ==  2 -> extract(R, ?ROW);
        length(X) =/= 2 -> extract(R, ?COLUMN)
    end.

get_maxes_lg(List) -> get_mlg(List, none, none).

get_mlg([], Min, Max)        -> {Min, Max};
get_mlg([H | T], none, none) -> Min = stdfuns_stats:min(H),
                                Max = stdfuns_stats:max(H),
                                get_mlg(T, Min, Max);
get_mlg([H | T], Min, Max)   -> NewMin = stdfuns_stats:min([Min | H]),
                                NewMax = stdfuns_stats:max([Max | H]),
                                get_mlg(T, NewMin, NewMax).

get_maxes([[X, Y] | T]) -> get_m2(T,
                                  stdfuns_stats:min(X),
                                  stdfuns_stats:max(X),
                                  stdfuns_stats:min(Y),
                                  stdfuns_stats:max(Y)).

get_m2([], MinX, MaxX, MinY, MaxY) -> {MinX, MaxX, MinY, MaxY};
get_m2([[X, Y] | T], MinX, MaxX, MinY, MaxY) ->
    get_m2(T,
           stdfuns_stats:min([MinX | X]),
           stdfuns_stats:max([MaxX | X]),
           stdfuns_stats:min([MinY | Y]),
           stdfuns_stats:max([MaxY | Y])).

hist1(Data, {Scale, Titles, Colours}) ->
    Data2      = lists:reverse(cast_data(Data)),
    Titles2    = cast_titles(Titles),
    Colours2   = get_colours(Colours),
    {Min, Max} = get_scale(Scale, Data2),
    case has_error([Data2, Min, Max, Titles2, Colours2]) of
        {true, Error} -> Error;
        false         -> hist2(Data2, Min, Max, Titles2, Colours2)
    end.

hist2(Data, Min, Max, Titles, Colours) ->
    Data1    = make_data(normalize(Data)),
    Titles1  = make_titles(Titles),
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
    Data2    = cast_data(Data),
    Titles2  = cast_titles(Titles),
    Colours2 = get_colours(Colours),
    case has_error([Data2, Titles2, Colours2]) of
        {true, Error} -> Error;
        false         -> pie2(Data2, Titles2, Colours2)
    end.

pie2(Data, Titles, Colours) ->
    Data1    = make_data(normalize(Data)),
    Titles1  = make_titles(Titles),
    Colours1 = conv_colours(Colours),
    "<img src='http://chart.apis.google.com/chart?cht=p3&amp;chd=t:"
        ++ Data1
        ++ "&amp;chs=450x100&amp;chl="
        ++ Titles1
        ++ Colours1
        ++ "' />".

cast_linegraph_data(Data) ->
    muin_collect:col([Data],
                     [eval_funs,
                      fetch, flatten,
                      {cast, str, num, ?ERRVAL_VAL},
                      {cast, bool, num},
                      {ignore, str}
                     ],
                     [return_errors, {all, fun muin_collect:is_blank_or_number/1}]).

cast_dates(Data) ->
    muin_collect:col([Data],
                     [eval_funs,
                      fetch, flatten,
                      {cast, date, num},
                      {cast, bool, num},
                      {cast, blank, num},
                      {ignore, blank}
                     ],
                     [return_errors, {all, fun is_number/1}]).

cast_data(Data) ->
    muin_collect:col([Data],
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
    muin_collect:col([Titles],
                     [eval_funs,
                      fetch, flatten,
                      {cast, bool, str},
                      {cast, num, str},
                      {cast, date, str},
                      {cast, blank, str}],
                     [return_errors]).

cast_orientation(Or) ->
    [CastOr] = muin_collect:col([Or],
                                [eval_funs,
                                 fetch,
                                 {cast, num, bool, ?ERRVAL_VAL},
                      {cast, str, bool, ?ERRVAL_VAL}],
                                [return_errors, {all, fun is_boolean/1}]),
    CastOr.

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
    muin_collect:col([Colours],
                     [eval_funs,
                      {ignore, bool},
                      {ignore, num},
                      {ignore,date},
                      fetch, flatten],
                     [return_errors, {all, fun is_list/1}]).

get_axes(XAxis)  ->
    muin_collect:col([XAxis],
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

rev(List) -> rev1(List, []).

rev1([], Acc)      -> lists:reverse(Acc);
rev1([H | T], Acc) -> rev1(T, [lists:reverse(H) | Acc]).

chunk(Data) -> chk2(Data, []).

chk2([], Acc)      -> lists:reverse(Acc);
chk2([H | T], Acc) -> chk2(T, [{range, [H]} | Acc]).

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

normalize(Data) ->
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

allocate_colours(N, Colours) ->
    NoOfCols = length(Colours),
    NSets = trunc(N/NoOfCols),
    Rem = N rem NoOfCols,
    {Extra, _Rest} = lists:split(Rem, Colours),
    Dup = lists:concat(lists:duplicate(NSets, Colours)),
    NewList = lists:concat([Dup, Extra]),
    string:join(NewList, ",").

colours() -> [
              {"black"   , "000000"},
              {"green"   , "008000"},
              {"maroon"  , "800000"},
              {"navy"    , "000080"}, 
              {"red"     , "FF0000"},
              {"blue"    , "0000FF"},
              {"purple"  , "800080"},
              {"silver"  , "C0C0C0"},
              {"lime"    , "00FF00"}, 
              {"gray"    , "808080"},
              {"olive"   , "808000"},
              {"white"   , "FFFFFF"},
              {"teal"    , "008080"}, 
              {"fuchsia" , "FF00FF"},
              {"aqua"    , "00FFFF"},
              {"yellow"  , "FFFF00"}
             ].
