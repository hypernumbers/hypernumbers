%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_render).

-export([page/1]).

-compile(export_all).

-include("spriki.hrl").

-define(DEFAULT_WIDTH, 80).
-define(DEFAULT_HEIGHT, 20).


page(Ref) ->
    Data = lists:sort(fun order_objs/2, hn_db_api:read_whole_page(Ref)),
    Cells = coalesce([{{X,Y},P} || {#refX{obj={cell,{X,Y}}},P} <- Data]),
    RowHs = [{R, H} || {#refX{obj={row,{R,R}}},{"height",H}} <- Data],
    ColWs = [{C, W} || {#refX{obj={column,{C,C}}},{"width",W}} <- Data],
    {CellsHtml, TotalWidth} = layout(Cells, ColWs, RowHs),
    wrap(CellsHtml, TotalWidth).

layout(Cells, CWs, RHs) ->
    Col = 1,
    Row = 1,
    PX = 0,
    PY = 0,
    {H,RHs2} = row_height(Row, RHs),
    Opaque = {CWs, 0},
    layout(Cells, Col, Row, PX, PY, H, CWs, RHs2, Opaque, []).

%% End of input
layout([], _Col, _Row, PX, _PY, _H, _CWs, _RHs, {_,MaxW}, Acc) ->
    TotalWidth = max(MaxW, PX),
    {lists:reverse(Acc), TotalWidth};

%% Output the next cell value in the current row.
layout([{{C,R}, L}|T], C, R, PX, PY, H, CWs, RHs, Opaque, Acc) ->
    Value = proplists:get_value("value", L, undefined),
    {W,CWs2} = col_width(C,CWs),
    case proplists:get_value("merge", L, undefined) of
        undefined ->
            Acc2 = [draw(Value, PX, PY, W, H) | Acc],
            PX2 = PX + W,
            layout(T, C+1, R, PX2, PY, H, CWs2, RHs, Opaque, Acc2);
        {struct, [{"right", Right}, {"down", Down}]} ->
            MW = width_across(C+1, C+Right, CWs2, W),
            MH = height_below(R+1, R+Down, RHs, H),
            Acc2 = [draw(Value, PX, PY, MW, MH) | Acc],
            PX2 = PX + MW,
            T2 = expunge(T, {C,C+Right,R,R+Down}),
            layout(T2, C+Right+1, R, PX2, PY, H, CWs2, RHs, Opaque, Acc2)
    end;

%% No cell for this column, but still haven't changed rows.
layout(Lst=[{{_,R},_}|_], C, R, PX, PY, H, CWs, RHs, Opaque, Acc) ->
    {W,CWs2} = col_width(C,CWs),
    layout(Lst, C+1, R, PX+W, PY, H, CWs2, RHs, Opaque, Acc);

%% Wind back, and advance to the next row.
layout(Lst, _Col, Row, PX, PY, H, _CWs, RHs, {CWs,MaxW}, Acc) ->
    PX2 = 0,
    PY2 = PY + H,
    Row2 = Row + 1,

    {H2,RHs2} = row_height(Row2, RHs),
    Opaque = {CWs, max(MaxW, PX)},
    layout(Lst, 1, Row2, PX2, PY2, H2, CWs, RHs2, Opaque, Acc).

expunge([], _Rng) -> 
    []; 
%% At a row past range, halt.
expunge([{{_,R},_}|_]=Lst, {_RC1,_RC2,_RR1,RR2}) when R > RR2 ->
    Lst;
%% Expunge cell.
expunge([{{C,R},_}|Tail], Rng={RC1,RC2,RR1,RR2}) when 
      RC1 =< C, C =< RC2,
      RR1 =< R, R =< RR2 ->
    expunge(Tail, Rng);
%% Keep cell, continue.
expunge([Cell | Tail], Rng) ->
    [Cell | expunge(Tail, Rng)].

width_across(C, Stop, _CWs, Acc) when C > Stop ->
    Acc;
width_across(C, Stop, CWs, Acc) ->
    {W, CWs2} = col_width(C, CWs),
    width_across(C+1, Stop, CWs2, W + Acc).

height_below(R, Stop, _RHs, Acc) when R > Stop ->
    Acc;
height_below(R, Stop, RHs, Acc) ->
    {H, RHs2} = row_height(R, RHs),
    height_below(R+1, Stop, RHs2, H + Acc).

row_height(Y, [{Y, H}|T]) -> {H, T};
row_height(_, T)          -> {?DEFAULT_HEIGHT, T}.

col_width(X, [{X, W}|T]) -> {W, T};
col_width(_, T)          -> {?DEFAULT_WIDTH, T}.

max(X,Y) when X < Y -> Y; 
max(X,_)            -> X.

draw(undefined, _X, _Y, _W, _H) ->
    "";
draw(Value, X, Y, W, H) ->
    Style = io_lib:format(
              "style='left:~bpx;top:~bpx;width:~bpx;height:~bpx'",
              [X, Y, W, H]),
    ["<div ",Style,">", Value, "</div>"].

order_objs({RA,_}, {RB,_}) ->
    {_, {XA, YA}} = RA#refX.obj,
    {_, {XB, YB}} = RB#refX.obj,
    if YA /= YB -> YA < YB;
       true     -> XA =< XB
    end.

coalesce([])             -> []; 
coalesce([{C, _}|_]=Lst) -> coalesce(Lst, C, [], []).

coalesce([], C, PropAcc, Acc) ->
    Acc2 = case PropAcc of [] -> Acc;
               _  -> [{C, PropAcc} | Acc] 
           end,
    lists:reverse(Acc2);
coalesce([{C, {K,_}=Prop}|Tail], C, PropAcc, Acc) 
  when K == "value"; 
       K == "style";
       K == "merge" ->
    coalesce(Tail, C, [Prop|PropAcc], Acc);
coalesce([{C, _Prop}|Tail], C, PropAcc, Acc) ->
    coalesce(Tail, C, PropAcc, Acc);
coalesce([{NewC, _}|_]=Lst, C, PropAcc, Acc) ->
    Acc2 = case PropAcc of [] -> Acc;
               _  -> [{C, PropAcc} | Acc] 
           end,
    coalesce(Lst, NewC, [], Acc2).

%% Temporary Function
wrap(Cells, TotalWidth) -> 
    OuterStyle = io_lib:format("style='width:~bpx'", [TotalWidth]),
    ["<!DOCTYPE html>
<html lang='en'>

         <head>
         <meta charset='utf-8' />
         <title>Hypernumbers</title>
         <link rel='stylesheet' href='/hypernumbers/hn.sheet.css' />	
         <link rel='stylesheet' href='/hypernumbers/hn.style.css' />	
         <script src='/hypernumbers/jquery-1.4.2.min.js'></script>
         </head>

         <body data-view='_g/core/webpage' style='overflow:auto;'>

         <span id='hidden_input'></span>

         <div id='outer' ", OuterStyle, ">
         <div id='inner'>", Cells, "</div>
         </div>

         </body>",

     %% <script src='/hypernumbers/json2.js'></script>
     %% <script src='/hypernumbers/hn.js'></script>
     %% <script src='/hypernumbers/hn.util.js'></script>
     %% <script src='/hypernumbers/hn.sheet.js'></script>
     %% <script src='/hypernumbers/hn.data.js'></script>
     %% <script src='/hypernumbers/hn.renderpage.js'></script>
     "</html>"].
