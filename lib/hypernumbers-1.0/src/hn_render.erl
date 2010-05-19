%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_render).

-export([content/1, 
         wrap_page/2, wrap_region/2]).

-include("spriki.hrl").

-define(DEFAULT_WIDTH, 80).
-define(DEFAULT_HEIGHT, 22).

-type intpair() :: {integer(), integer()}.
-type cells() :: [{intpair(), [tuple()]}].
-type cols() :: [intpair()].
-type rows() :: [intpair()].
-type textdata() :: string() | [textdata()].

-record(rec, {maxwidth = 0,
              colwidths = [],
              palette = [],
              startcol}).
                 
%% Returns a tuple containing the rendered html for the area covered
%% by the given Ref, along with the width of said html.
-spec content(#refX{}) -> {[textdata()], integer()}.
content(Ref) ->
    Data = lists:sort(fun order_objs/2, hn_db_api:read_ref(Ref)),
    Cells = [{{X,Y},L} || {#refX{obj={cell,{X,Y}}},L} <- Data],
    RowHs = [{R, H} || {#refX{obj={row,{R,R}}},[{"height",H}]} <- Data],
    ColWs = [{C, W} || {#refX{obj={column,{C,C}}},[{"width",W}]} <- Data],
    Palette = array:from_orddict
                (lists:sort
                   (hn_mochi:styles_to_css
                      (hn_db_api:read_styles(Ref), 
                       []
                      ))),
    layout(Ref, Cells, ColWs, RowHs, Palette).

-spec layout(#refX{}, cells(), cols(), rows(), array()) 
            -> {[textdata()], integer()}.
layout(Ref, Cells, CWs, RHs, Palette) ->
    PX = 0,
    PY = 0,
    Col = startcol(Ref),
    Row = startrow(Ref),
    {H,RHs2} = row_height(Row, RHs),
    Rec = #rec{colwidths=CWs, palette=Palette, startcol=Col},
    layout(Cells, Col, Row, PX, PY, H, CWs, RHs2, Rec, []).

-spec layout(cells(), 
             integer(), integer(), integer(), integer(), integer(), 
             cols(), rows(), #rec{}, [textdata()])
            -> {[textdata()],integer()}.
                    
%% End of input
layout([], _Col, _Row, PX, _PY, _H, _CWs, _RHs, Rec, Acc) ->
    TotalWidth = max(PX, Rec#rec.maxwidth),
    {lists:reverse(Acc), TotalWidth};

%% Output the next cell value in the current row.
layout([{{C,R}, L}|T], C, R, PX, PY, H, CWs, RHs, Rec, Acc) ->
    Value = pget("value", L),
    Css = read_css(pget("style", L), Rec#rec.palette),
    {W,CWs2} = col_width(C,CWs),
    case pget("merge", L) of
        undefined ->
            Acc2 = [draw(Value, Css, PX, PY, W, H) | Acc],
            PX2 = PX + W,
            layout(T, C+1, R, PX2, PY, H, CWs2, RHs, Rec, Acc2);
        {struct, [{"right", Right}, {"down", Down}]} ->
            {MW,CWs3} = width_across(C+1, C+Right, CWs2, W),
            MH = height_below(R+1, R+Down, RHs, H),
            Acc2 = [draw(Value, Css, PX, PY, MW, MH) | Acc],
            PX2 = PX + MW,
            T2 = expunge(T, {C,C+Right,R,R+Down}),
            layout(T2, C+Right+1, R, PX2, PY, H, CWs3, RHs, Rec, Acc2)
    end;

%% No cell for this column, but still haven't changed rows.
layout(Lst=[{{_,R},_}|_], C, R, PX, PY, H, CWs, RHs, Rec, Acc) ->
    {W,CWs2} = col_width(C,CWs),
    layout(Lst, C+1, R, PX+W, PY, H, CWs2, RHs, Rec, Acc);

%% Wind back, and advance to the next row.
layout(Lst, _Col, Row, PX, PY, H, _CWs, RHs, Rec, Acc) ->
    PX2 = 0,
    PY2 = PY + H,
    Col2 = Rec#rec.startcol,
    Row2 = Row + 1,
    {H2,RHs2} = row_height(Row2, RHs),
    Rec2 = Rec#rec{maxwidth = max(Rec#rec.maxwidth, PX)},
    layout(Lst, Col2, Row2, PX2, PY2, H2, 
           Rec#rec.colwidths, RHs2, Rec2, Acc).

-spec expunge(cells(), {integer(), integer(), integer(), integer()}) 
             -> cells().
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

-spec width_across(integer(), integer(), cols(), integer()) 
                  -> {integer(), cols()}. 
width_across(C, Stop, CWs, Acc) when C > Stop ->
    {Acc, CWs};
width_across(C, Stop, CWs, Acc) ->
    {W, CWs2} = col_width(C, CWs),
    width_across(C+1, Stop, CWs2, W + Acc).

-spec height_below(integer(), integer(), rows(), integer())
                   -> integer().
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

-spec draw(undefined | string(), 
           textdata(),
           integer(), integer(), integer(), integer())
          -> textdata().
draw(undefined, "", _X, _Y, _W, _H) -> "";
draw(undefined, Css, X, Y, W, H) -> draw("", Css, X, Y, W, H);
draw(Value, Css, X, Y, W, H) ->
    % Tom wants to fix this up :(
    Val = case Value of
              {errval, ErrVal} -> atom_to_list(ErrVal);
              A when is_atom(A) -> atom_to_list(A);
              _                -> Value
          end,
    Style = io_lib:format(
              "style='left:~bpx;top:~bpx;width:~bpx;height:~bpx;~s'",
              [X, Y, W, H, Css]),
    ["<div ",Style,">", Val, "</div>"].

-spec order_objs({#refX{},any()}, {#refX{},any()}) -> boolean(). 
order_objs({RA,_}, {RB,_}) ->
    {_, {XA, YA}} = RA#refX.obj,
    {_, {XB, YB}} = RB#refX.obj,
    if YA /= YB -> YA < YB;
       true     -> XA =< XB
    end.

-spec read_css(undefined | integer(), array()) -> string(). 
read_css(undefined, _Palette) -> "";
read_css(Idx, Palette) -> array:get(Idx, Palette).

-spec startcol(#refX{}) -> integer(). 
startcol(#refX{obj={range,{X,_,_,_}}}) -> X;
startcol(_)                            -> 1.

-spec startrow(#refX{}) -> integer(). 
startrow(#refX{obj={range,{_,Y,_,_}}}) -> Y;
startrow(_)                            -> 1.
    
pget(K,L) -> proplists:get_value(K,L,undefined).

-spec wrap_page([textdata()], integer()) -> [textdata()]. 
wrap_page(Content, TotalWidth) -> 
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

         <body data-view='_g/core/webpage'>

         <span id='hidden_input'></span>

         <div id='outer' ", OuterStyle, ">
         <div id='inner' class='hn_inner'>", Content, "</div>
         </div>

    <div id='editspreadsheet'>
      <div id='powered'>
        powered by <br />
        <a href='http://hypernumbers.com'>
          <span class='hyper'>hyper</span><span class='numbers'>numbers</span>
        </a>
      </div>
      <a href='?view=_g/core/spreadsheet' id='editlogin' title='edit / login'>login</a>
    </div>

  </body>
  
  <script src='/hypernumbers/json2.js'></script>
  <script src='/hypernumbers/hn.js'></script>
  <script src='/hypernumbers/hn.util.js'></script>
  <script src='/hypernumbers/hn.sheet.js'></script>
  <script src='/hypernumbers/hn.data.js'></script>

  <script src='/hypernumbers/hn.renderpage.js'></script>
  </html>"].

-spec wrap_region([textdata()], integer()) -> [textdata()]. 
wrap_region(Content, Width) -> 
    OuterStyle = io_lib:format("style='width:~bpx'", [Width]),
    ["<div class='hn_inner' ", OuterStyle, ">",
     Content,
     "</div>"].
