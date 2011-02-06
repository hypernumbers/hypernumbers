%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Handle Hypernumbers HTTP requests
-module(hn_render).

-export([content/1,
         content/2, 
         wrap_page/4,
         wrap_region/3]).

-include("spriki.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_WIDTH, 80).
-define(DEFAULT_HEIGHT, 22).

-type intpair() :: {integer(), integer()}.
-type cells() :: [{intpair(), [tuple()]}].
-type cols() :: [intpair()].
-type rows() :: [intpair()].
-type textdata() :: string() | [textdata()].

-record(rec, {maxwidth = 0,
              maxmerge_height = 0,
              colwidths = [],
              palette,
              startcol}).

content(Ref) -> content(Ref, webpage).

%% Returns a tuple containing the rendered html for the area covered
%% by the given Ref, along with the width of said html.
-spec content(#refX{}, atom()) -> {{[textdata()], integer(), integer()}, #render{}}.
content(Ref, Type) ->
    Data = lists:sort(fun order_objs/2, read_data_without_page(Ref)),
    Cells = [{{X,Y},L} || {#refX{obj={cell,{X,Y}}},L} <- Data],
    % need to do old and new row/col types
    RowHs = [{R, pget("height", RPs, ?DEFAULT_HEIGHT)} 
             || {#refX{obj={row,{R,R}}},RPs} <- Data],
    ColWs = [{C, pget("width", CPs, ?DEFAULT_WIDTH)} 
             || {#refX{obj={column,{C,C}}},CPs} <- Data],
    Palette = gb_trees:from_orddict
                (lists:sort
                 (hn_mochi:extract_styles(Ref#refX.site))),
    CSSList = hn_db_api:read_attribute(Ref#refX{obj={page, "/"}}, "css"),
    JSList = hn_db_api:read_attribute(Ref#refX{obj={page, "/"}}, "js"),
    TitleList = hn_db_api:read_attribute(Ref#refX{obj={page, "/"}}, "title"),
    Open = "<link rel='stylesheet' href='",
    CSS = [Open ++ X ++ "' type='text/css' />" || {_, X} <- CSSList],
    JS = ["<script src='" ++ X ++ "'></script>" || {_, X} <- JSList],
    Title = ["<title>" ++ X ++ "</title>" || {_, X} <- TitleList],
    Addons = #render{css=CSS, js=JS, title=Title},
    {layout(Ref, Type, Cells, ColWs, RowHs, Palette), Addons}.

read_data_without_page(Ref) ->
    Refs = hn_db_api:read_intersect_ref(Ref),
    [ {RefX, Val} || {RefX, Val} <- Refs, element(1, RefX#refX.obj) =/= page ].

-spec layout(#refX{}, atom(), cells(), cols(), rows(), gb_tree()) 
            -> {[textdata()], integer(), integer()}.
layout(Ref, Type, Cells, CWs, RHs, Palette) ->
    PX = 0,
    PY = 0,
    Col = startcol(Ref),
    Row = startrow(Ref),
    {H,RHs2} = row_height(Row, RHs),
    Rec = #rec{colwidths=CWs, palette=Palette, startcol=Col},
    layout(Cells, Type, Col, Row, PX, PY, H, CWs, RHs2, Rec, []).

-spec layout(cells(), atom(),
             integer(), integer(), integer(), integer(), integer(), 
             cols(), rows(), #rec{}, [textdata()])
            -> {[textdata()],integer(), integer()}.
                    
%% End of input
layout([], _Type, _Col, _Row, PX, PY, H, _CWs, _RHs, Rec, Acc) ->
    TotalHeight = erlang:max(PY + H, Rec#rec.maxmerge_height),
    TotalWidth = erlang:max(PX, Rec#rec.maxwidth),
    {lists:reverse(Acc), TotalWidth, TotalHeight};

%% Output the next cell value in the current row.
layout([{{C,R}, L}|T], Type, C, R, PX, PY, H, CWs, RHs, Rec, Acc) ->
    Value = pget("value", L),
    Input = case Type of
                wikipage  -> pget("input", L);
                webpage   -> "none"
            end,
    Css = read_css(pget("style", L), Rec#rec.palette),
    {W,CWs2} = col_width(C,CWs),
    case pget("merge", L) of
        undefined ->
            Acc2 = [draw(Value, Css, Input, C, R, PX, PY, W, H) | Acc],
            layout(T, Type, C+1, R, PX+W, PY, H, CWs2, RHs, Rec, Acc2);
        {struct, [{"right", Right}, {"down", Down}]} ->
            {MW,CWs3} = width_across(C+1, C+Right, CWs2, W),
            MH = height_below(R+1, R+Down, RHs, H),
            Rec2 = Rec#rec{maxmerge_height =
                               erlang:max(Rec#rec.maxmerge_height, MH + PY)},
            Acc2 = [draw(Value, Css, Input, C, R, PX, PY, MW, MH) | Acc],
            T2 = expunge(T, {C,C+Right,R,R+Down}),
            layout(T2, Type, C+Right+1, R, PX+MW, PY, H, CWs3, RHs, Rec2, Acc2)
    end;

%% No cell for this column, but still haven't changed rows.
layout(Lst=[{{_,R},_}|_], Type, C, R, PX, PY, H, CWs, RHs, Rec, Acc) ->
    {W,CWs2} = col_width(C,CWs),
    layout(Lst, Type, C+1, R, PX+W, PY, H, CWs2, RHs, Rec, Acc);

%% Wind back, and advance to the next row.
layout(Lst, Type, _Col, Row, PX, PY, H, _CWs, RHs, Rec, Acc) ->
    PX2 = 0,
    PY2 = PY + H,
    Col2 = Rec#rec.startcol,
    Row2 = Row + 1,
    {H2,RHs2} = row_height(Row2, RHs),
    Rec2 = Rec#rec{maxwidth = erlang:max(Rec#rec.maxwidth, PX)},
    layout(Lst, Type, Col2, Row2, PX2, PY2, H2, 
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

-spec draw(undefined | string(), 
           textdata(),
           string(),
           integer(), integer(),
           integer(), integer(), integer(), integer())
          -> textdata().
draw(undefined,"",Inp,_C,_R,_X,_Y,_W,_H)  when Inp =/= "inline" -> "";
draw(undefined,Css,Inp,C,R,X,Y,W,H) -> draw("",Css, Inp,C,R,X,Y,W,H);
draw(Value,Css,Inp,C,R,X,Y,W,H) ->
    % Tom wants to fix this up :(
    Val = case Value of
              {errval, ErrVal} -> atom_to_list(ErrVal);
              A when is_atom(A) -> atom_to_list(A);
              I when is_integer(I) -> integer_to_list(I);
              F when is_float(F) -> float_to_list(F);
              _                  -> Value
          end,
    Cell = tconv:to_b26(C) ++ integer_to_list(R),
    Style = io_lib:format(
              "style='left:~bpx;top:~bpx;width:~bpx;height:~bpx;~s'",
              [X, Y, W, H, Css]),
        case Inp of
            "inline" ->
                "<div class='hn_padded' "++Style ++">"++
                    "<div class='inline' data-ref='"++Cell++"'>"++Val++
                    "</div></div>";
            _        ->
                "<div data-ref='"++Cell++"'"++Style++">"++Val++"</div>"
        end.

-spec order_objs({#refX{},any()}, {#refX{},any()}) -> boolean(). 
order_objs({RA,_}, {RB,_}) ->
    {_, {XA, YA}} = RA#refX.obj,
    {_, {XB, YB}} = RB#refX.obj,
    if YA /= YB -> YA < YB;
       true     -> XA =< XB
    end.

-spec read_css(undefined | integer(), gb_tree()) -> string(). 
read_css(undefined, _Palette) -> "";
read_css(Idx, Palette) -> case gb_trees:lookup(Idx, Palette) of
                              none -> "";
                              {value, V} -> V
                          end.            

-spec startcol(#refX{}) -> integer(). 
startcol(#refX{obj={range,{X,_,_,_}}}) -> X;
startcol(_)                            -> 1.

-spec startrow(#refX{}) -> integer(). 
startrow(#refX{obj={range,{_,Y,_,_}}}) -> Y;
startrow(_)                            -> 1.
    
pget(K,L) -> proplists:get_value(K,L,undefined).

pget(K,L,D) -> proplists:get_value(K,L,D).

-spec wrap_page([textdata()], integer(), integer(), #render{}) -> [textdata()]. 
wrap_page(Content, TotalWidth, TotalHeight, Addons) -> 
    OuterStyle = io_lib:format("style='width:~bpx;height:~bpx'", 
                               [TotalWidth, TotalHeight]),
    Title = case Addons#render.title of
                [] -> "<title>Hypernumbers - the team spreadsheet</title>";
                T  -> T
            end,

    ["<!DOCTYPE html>
<html lang='en'>
         <head>
"     ++Title++
"        <meta charset='utf-8' />
         <link rel='stylesheet' href='/hypernumbers/hn.sheet.css' />	
         <link rel='stylesheet' href='/hypernumbers/hn.style.css' />
         <link rel='stylesheet' href='/webcomponents/webcomponents.css' />
         <link rel='stylesheet' href='/webcomponents/jquery.ui.potato.menu.css'>
         <link rel='stylesheet' href='/tblsorter/style.css' />
"     ++Addons#render.css++
"         </head>

         <body data-view='webpage'>

         <span id='hidden_input'></span>

         <div id='outer' ", OuterStyle, ">
          <div id='clinput'></div>
          <div id='inner' class='hn_inner'>", Content, "</div>
         </div>

<div id='editspreadsheet' class='ctrlbox'>

 <div id='powered'>
   <span class='hyper'>hyper</span><span class='numbers'>numbers</span>
 </div>

 <div id='editmenu' class='ctrlbox'>
  <div id='editloggedin'>
   <div id='uname'></div>
  <div id='allowedviews'></div>
  <a id='hn_reset_pwd'>Change Password</a> | <a id='logout'>Logout</a>
        <form id='hn_passwordform'>
	      <input type='password' id='hn_passwordval'>
        <input type='submit' value='Set Password' class='button'>
        <p>Passwords must be more than 8 characters and should
        include punctuation and numbers</p>
        <div id='hn_pwd_feedback'></div>
	    </form>
  </div>
  <div id='editanon'>
   <form action='' method='post' id='login'>
    <div class='formrow'><label for='email'>Email Address</label>
    <input type='text' id='email' /></div>
    <div class='formrow'><label for='pass'>Password</label>
    <input type='password' id='pass' /></div>
    <div class='formrow'><input type='submit' id='submit' value='Log in' class='button' /><span id='forgotten_pwd'></span><br /></div>
   <div id='loginfeedback'></div>

    or sign up at <a href='http://hypernumbers.com'>hypernumbers.com</a>
   </form>
  </div>
 </div>
</div>  
 <script src='/hypernumbers/jquery-1.4.2.min.js'></script>
 <!--<script src='http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js'></script>-->
"  ++Addons#render.js++
"  <script src='/hypernumbers/jquery.tablesorter.min.js'></script>  
  <script src='/hypernumbers/json2.js'></script>
  <script src='/webcomponents/jquery.ui.potato.menu.js'></script>
  <script src='/hypernumbers/hn.js'></script>
  <script src='/hypernumbers/hn.util.js'></script>
  <script src='/hypernumbers/hn.sheet.js'></script>
  <script src='/hypernumbers/hn.data.js'></script>
  <script src='/hypernumbers/hn.callbacks.js'></script>
  <script src='/hypernumbers/hn.sitedata.js'></script>
  <script src='/hypernumbers/hn.renderpage.js'></script>
  </body>
  </html>"].

-spec wrap_region([textdata()], integer(), integer()) -> [textdata()]. 
wrap_region(Content, Width, Height) -> 
    OuterStyle = io_lib:format("style='width:~bpx;height:~bpx'", 
                               [Width, Height]),
    ["<div class='hn_inner' ", OuterStyle, ">",
     Content,
     "</div>"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_test() ->
    Ref = #refX{site="http://hypernumbers.dev:9000",
                path=["web"],
                obj={page,"/"}},
    Cells = [{{1,1},[{"style",1},{"value","1"}]},
             {{7,1},[{"style",1},{"value","2"}]},
             {{7,2},[{"style",1},{"value","3"}]}],
    ColWs = [],
    RowHs = [],
    Palette = gb_trees:empty(),
    {_, W, H} = layout(Ref, webpage, Cells, ColWs, RowHs, Palette),
    ?_assertEqual({560, 44}, {W, H}).

col_rows_test_() ->
    Ref = #refX{site="http://hypernumbers.dev:9000",
                path=["web"],
                obj={page,"/"}},
    Cells = [{{1,1},[{"style",1},{"value","1"}]},
             {{7,1},[{"style",1},{"value","2"}]},
             {{7,2},[{"style",1},{"value","3"}]},
             {{5,3},[{"style",1},{"value","4"}]}],
    ColWs = [{6, 30}, {7, 150}],
    RowHs = [{1, 40}, {3, 10}],
    Palette = gb_trees:empty(),
    {_, W, H} = layout(Ref, webpage, Cells, ColWs, RowHs, Palette),
    ?_assertEqual({580, 72}, {W, H}).

merged_col_test_() ->
    Ref = #refX{site="http://hypernumbers.dev:9000",
                path=["web"],
                obj={page,"/"}},
    Cells = [{{1,1}, 
              [{"merge",{struct,[{"right",3},{"down",0}]}},
               {"style",1},{"value","1"}]},
             {{7,1},
              [{"value","rightmost"},
               {"merge",{struct,[{"right",1},{"down",0}]}},
               {"style",1}]},
             {{1,2},[{"style",1},{"value","0"}]},
             {{7,2},[{"style",1},{"value","3"}]},
             {{4,3},
              [{"merge",{struct,[{"right",1},{"down",0}]}}]}],
    ColWs = [],
    RowHs = [],
    Palette = gb_trees:empty(),
    {_, W, H} = layout(Ref, webpage, Cells, ColWs, RowHs, Palette),
    ?_assertEqual({640, 66}, {W, H}).

merged_row_test_() ->
    Ref = #refX{site="http://hypernumbers.dev:9000",
                path=["web"],
                obj={page,"/"}},
    Cells = [{{1,1},[{"value","1"},{"style",1}]},
             {{6,1},
              [{"value","goes to 15"},
               {"style",1},
               {"merge",{struct,[{"right",0},{"down",14}]}}]},
             {{1,2},[{"merge",{struct,[{"right",0},{"down",3}]}}]},
             {{1,9},[{"value","last row (9)"},{"style",2}]}],
    ColWs = [],
    RowHs = [],
    Palette = gb_trees:empty(),
    {_, W, H} = layout(Ref, webpage, Cells, ColWs, RowHs, Palette),
    ?_assertEqual({480, 330}, {W, H}).
