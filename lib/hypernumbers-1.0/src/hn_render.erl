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
    PosX = 0,
    PosY = 0,
    {Height,RHs2} = row_height(Row, RHs),
    Opaque = {CWs, 0},
    layout(Cells, Col, Row, PosX, PosY, Height, CWs, RHs2, Opaque, []).
    
%% End of input
layout([], _Col, _Row, PosX, _PosY, _Height, _CWs, _RHs, {_,MaxW}, Acc) ->
    TotalWidth = max(MaxW, PosX),
    {lists:reverse(Acc), TotalWidth};

 %% Output the next cell value in the current row.
 layout([{{X,Y}, L}|T], X, Y, PosX, PosY, Height, CWs, RHs, Opaque, Acc) ->
     {Width,CWs2} = col_width(X,CWs),
     Value = proplists:get_value("value", L, undefined),
     case proplists:get_value("merge", L, undefined) of
         undefined ->
             Acc2 = [draw(Value, PosX, PosY, Width, Height) | Acc],
            PosX2 = PosX + Width,
            layout(T, X+1, Y, PosX2, PosY, Height, CWs2, RHs, Opaque, Acc2);
        {struct, [{"right", _Right}, {"down", _Down}]} ->
            throw(err)
    end;

%% No cell for this column in the current row.
layout(Lst=[{{_,Y},_}|_], X, Y, PosX, PosY, Height, CWs, RHs, Opaque, Acc) ->
    {Width,CWs2} = col_width(X,CWs),
    layout(Lst, X+1, Y, PosX+Width, PosY, Height, CWs2, RHs, Opaque, Acc);

%% Wind back, and advance to the next row.
layout(Lst, _Col, Row, PosX, PosY, Height, _CWs, RHs, {CWs, MaxW}, Acc) ->
    PosX2 = 0,
    PosY2 = PosY + Height,
    Row2 = Row + 1,
    {Height2,RHs2} = row_height(Row2, RHs),
    Opaque = {CWs, max(MaxW, PosX)},
    layout(Lst, 1, Row2, PosX2, PosY2, Height2, CWs, RHs2, Opaque, Acc).

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
