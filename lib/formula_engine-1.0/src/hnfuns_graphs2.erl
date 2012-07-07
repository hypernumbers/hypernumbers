%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       The module for producing graphs
%%%
%%% @end
%%% Created : 11 Jan 2010 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hnfuns_graphs2).

-export([
         'new.linegraph.'/1
        ]).

-define(ROW,    true).
-define(COLUMN, false).

-include("typechecks.hrl").
-include("muin_records.hrl").
-include("spriki.hrl").

%% definitions of standard abbreviations
-define(apiurl,     "<img src='http://chart.apis.google.com/chart?").
-define(urlclose,   "' />").
-define(axesrange,  "chxr").  % 0=x, 1=Y, min, max (seperated by |)"
-define(piecolours, "chxs").
-define(axes,       "chxt").  % handles multiple axes
-define(size,       "chs").   % Width x Height
-define(colours,    "chco").  % colours of the lines
-define(data,       "chd").   % data - depends on chart time
-define(datalables, "chdl").  % separated by |
-define(pielables,  "chl").
-define(customscale,"chds").
-define(type,       "cht").
-define(legendpos,  "chdlp"). % t | l | r | b
-define(linestyles, "chls").
-define(margins,    "chma").
-define(title,      "chtt").
-define(titlestyle, "chts").
-define(axeslables, "chxl").
-define(axeslabpos, "chxp").
-define(tickmarks,  "chxt").
-define(speedolab,  "chl").
-define(barwidths,  "chbh").
-define(zeroline,   "chp").
-define(linemarkers, "chm").

% definition of standard stuff
-define(SPEEDOPARAMS, {array, [[0, 33, 66, 100]]}).
-define(RED,          "FF3333").
-define(ORANGE,       "FF9900").
-define(GREEN,        "80C65A").
-define(GREY,         "999999").
-define(BLACK,        "000000").
-define(NORMALAXES,   "x,y").
-define(LABLEAXES,    "x,x,y,y").
-define(HIST_VGROUP,  "bvg").
-define(HIST_VSTACK,  "bvs").
-define(HIST_HGROUP,  "bhg").
-define(HIST_HSTACK,  "bhs").
-define(PIECHART,     "pc").
-define(XYLINE,       "lxy").
-define(EQXAXIS,      "lc").
-define(SPARKLINE,    "ls").
-define(TOPHORIZ,     "t").
-define(TOPVERT,      "tv").
-define(RIGHTVERT,    "r").
-define(LEFTVERT,     "l").
-define(BOTHORIZ,     "b").
-define(BOTVERT,      "bv").
-define(NORMMARGINS,  "5,5,5,5").
-define(BOTHAXES,     "x,y").
-define(SPKBARWIDTHS, "10,1,2").
-define(SPKBARWIDTH,  10).
-define(SPKBARSPACE,  4).
-define(SPKBARGRPSP,  5).

-define(SPCOLOURS, [
                    "737373",
                    "F15A60",
                    "7AC36A",
                    "5A9BD4",
                    "FAA75B",
                    "9E67AB",
                    "CE7058",
                    "D77FB4"
                   ]).

-define(XYMARKERS, [
                    "c",
                    "d",
                    "o",
                    "s",
                    "x"
                    ]).

-define(XYCOLOURS, [
                    "737373",
                    "F15A60",
                    "7AC36A",
                    "5A9BD4",
                    "FAA75B",
                    "9E67AB",
                    "CE7058",
                    "D77FB4"
                   ]).

'new.linegraph.'([W, H | List]) ->
    [Width] = typechecks:throw_std_ints([W]),
    [Height] = typechecks:throw_std_ints([H]),
    {DataX, Lines, Cols, Rest} = chunk_linegraph(List),
    {Title, XAxis, YAxis, Series} = process(Rest, Lines),
    Data = make_data(DataX, Cols, YAxis, Series, []),
    Payload = lists:flatten(io_lib:format("~p", [Data])),
    Options = "[\"title\", \"" ++ Title ++ "\"]",
    Resize = #resize{width = Width, height = Height},
    HTML = "<div class='hn_graphs' "
        ++ "data-array='" ++ Payload ++ "' "
        ++ "data-options='" ++ Options ++ "' "
        ++ "style='height:inherit;width:inherit;'>"
        ++ "</div>",
    JS = ["/graphs/hn.graphs.js"],
    Reload = ["HN.Graphs.reload();"],
    Incs = #incs{js = JS, js_reload = Reload},
    Preview = "Line Graph",
    #spec_val{val = HTML, preview = Preview,
              resize = Resize, sp_incs = Incs}.

chunk_linegraph([X, Lines | List]) ->
    DataX = cast_data(X),
    [Lines2] = typechecks:throw_std_ints([Lines]),
    {Cols, Rest} = chunk_l2(Lines2, List),
    {DataX, Lines2, Cols, Rest}.

chunk_l2(Lines, List) ->
    [Lines2] = typechecks:throw_std_ints([Lines]),
    muin_checks:ensure(Lines2 > 0, ?ERRVAL_NUM),
    {Data, Rest} = lists:split(Lines2, List),
    Prefetched = cast_prefetch(Data),
    Data1 = [proc_dxy1(X) || X <- Prefetched],
    Data2 = [X || {X, _NoR, _NoC} <- Data1],
    Data3 = [lists:flatten([cast_linegraph_data(X) || X <- X1])
             || X1 <- Data2],
    {Data3, Rest}.

process([], Lines)         -> p2("", "", "",    make_series(Lines));
process([Title], Lines)    -> p2(Title, "", "", make_series(Lines));
process([Tt, X], Lines)    -> p2(Tt, X, "",     make_series(Lines));
process([Tt, X, Y], Lines) -> p2(Tt, X, Y,      make_series(Lines));
process([Tt, X, Y, S], _)  -> p2(Tt, X, Y, S).

p2(Title, XAxis, YAxis, Series) ->
    [T2, X2, Y2] = typechecks:std_strs([Title, XAxis, YAxis]),
    Srs2 = lists:reverse(typechecks:throw_flat_strs([Series])),
    {T2, X2, Y2, Srs2}.

make_series(_Lines) ->
    "erk".

make_data([], _, YAxis, Series, Acc) ->
    [[YAxis | Series] | lists:reverse(Acc)];
make_data([X | T], Cols, YAxis, Series, Acc) ->
    {NewAcc, NewCols} = split(Cols, X, [], []),
    make_data(T, NewCols, YAxis, Series, [NewAcc | Acc]).

split([], X, Acc1, Acc2) ->
    {[X | lists:reverse(Acc1)], lists:reverse(Acc2)};
split([[H | Rest] | T], X, Acc1, Acc2) ->
    split(T, X, [H | Acc1], [Rest | Acc2]).

proc_dxy1({Type, X} = R) when Type == range orelse Type == array->
    if
        length(X) ==  2 -> extract(R, ?ROW);
        length(X) =/= 2 -> extract(R, ?COLUMN)
    end.

extract({Type, Data}, ?ROW) when Type == range orelse Type == array ->
    tartup(Data);
extract({Type, Data}, ?COLUMN) when Type == range
                                    orelse Type == array ->
    tartup(hslists:transpose(Data)).

tartup(Data) ->
    [F | _T] = Data,
    NoOfCols = length(F), % rectangular matrix
    {chunk(Data), length(Data), NoOfCols}.

chunk(Data) -> chk2(Data, []).

chk2([], Acc)      -> lists:reverse(Acc);
chk2([H | T], Acc) -> chk2(T, [{range, [H]} | Acc]).

%%%
%%% Casting functions
%%%

cast_data(Data) ->
    Vals = muin_collect:col([Data],
                            [eval_funs,
                             fetch, flatten,
                             {cast, str, num, ?ERRVAL_VAL},
                      {cast, bool, num},
                             {cast, blank, num},
                             {ignore, str},
                             {ignore, blank}
                            ],
                            [return_errors, {all, fun is_number/1}]),
    lists:reverse(Vals).

cast_prefetch(Data) ->
    muin_collect:col(Data, [fetch_ref], []).

cast_linegraph_data(Data) ->
    muin_collect:col([Data],
                     [eval_funs,
                      fetch, flatten,
                      {cast, bool, num},
                      empty_str_as_blank
                     ],
                     [return_errors]).
