% This module has been generated by gen_full_test.rb
% DO NOT EDIT MANUALLY.
%
% Source file: b_quadratic_equations.xls
% Generated on: Fri Feb 22 19:04:07 +0000 2008

-module(b_quadratic_equations_SUITE).
-compile(export_all).
-include("ct.hrl").

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    test_util:wait(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/b_quadratic_equations.xls"),
    Fun =fun({{{sheet,Sheet},{row_index,RowIdx},{col_index,ColIdx}},Input}) ->
      io:format("Sheet is ~p RowIdx is ~p and ColIdx is ~p~n",[Sheet,RowIdx,ColIdx]),
      Data1 = case Input of
        {_,Data2}                                -> Data2;
        {_,number,Data2} when is_float(Data2)   -> float_to_list(Data2);
        {_,number,Data2} when is_integer(Data2) -> integer_to_list(Data2);
        {_,error,Error}                          -> Error;
        {_,boolean,true}                         -> "true";
        {_,boolean,false}                        -> "false"
      end,
      Path="/"++Sheet++"/",
      Cell=util2:make_b26(ColIdx+1)++integer_to_list(RowIdx+1),
      io:format("Cell is ~p~n",[Cell]),
      hn_post("http://127.0.0.1:9000",Path,Cell,Data1) end,
    lists:map(Fun,Data),
    io:format("in init_per_suite Data is ~p~n",[Data]),
    Config.
  
end_per_suite(_Config) ->
    production_boot:stop(),
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config,{Sheet,Row,Col}) ->
  test_util:read_from_excel_data(Config,b_quadratic_equations_SUITE,{Sheet,Row,Col}).

%%% Helper functions.

hn_post(Site, Path, Cell, Data) ->
    Url=Site++Path++Cell,
    PostData = "action=create&value=" ++ yaws_api:url_encode(Data),
    Data2 = {Url, [], "text/plain", PostData},
    io:format("in hn_post Data is ~p~n",[Data2]),
    Return = http:request(post, Data2, [], []),
    io:format("in hn_post return from POST is ~p~n",[Return]),
    {ok, {{V, 200, R}, H, Body}} = Return.


hn_get(Site,Path,Cell) ->
    io:format("in hn_get Site is ~p Path is ~p and Cell is ~p~n",[Site,Path,Cell]),
    Url=Site++Path++Cell,
    io:format("in hn_get Url is ~p~n",[Url]),
    {ok, {{V, 200, R}, H, Body}} = http:request(get, {Url, []}, [], []),
    io:format("in hn_get body is ~p~n",[Body]),
    %%stdext:text2num(Body). %% Assume it's all numbers for now.
    Body.

assert_eql(X, Y) when is_integer(X) andalso is_float(Y) ->
    X * 1.0 == Y;
assert_eql(X, Y) when is_float(X) andalso is_integer(Y) ->
    X == Y * 1.0;
assert_eql(X, Y) ->
    X == Y.
    
sheet1_a5_test(doc) -> [{userdata,[{""}]}];
sheet1_a5_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","a5"),
  Expected="C:",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_a1_test(doc) -> [{userdata,[{""}]}];
sheet1_a1_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","a1"),
  Expected="Quadratic Equation Solver",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_a7_test(doc) -> [{userdata,[{""}]}];
sheet1_a7_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","a7"),
  Expected="Answers:",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_a3_test(doc) -> [{userdata,[{""}]}];
sheet1_a3_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","a3"),
  Expected="A:",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_a4_test(doc) -> [{userdata,[{""}]}];
sheet1_a4_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","a4"),
  Expected="B:",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_b5_test(doc) -> [{userdata,[{""}]}];
sheet1_b5_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","b5"),
  Expected="4.78",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_b7_test(doc) -> [{userdata,[{""}]}];
sheet1_b7_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","b7"),
  Expected="-0.353764194476439",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_b8_test(doc) -> [{userdata,[{""}]}];
sheet1_b8_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","b8"),
  Expected="-3.86052151980928",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_b3_test(doc) -> [{userdata,[{""}]}];
sheet1_b3_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","b3"),
  Expected="3.5",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_b4_test(doc) -> [{userdata,[{""}]}];
sheet1_b4_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","b4"),
  Expected="14.75",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_c5_test(doc) -> [{userdata,[{""}]}];
sheet1_c5_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","c5"),
  Expected="1.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_c7_test(doc) -> [{userdata,[{""}]}];
sheet1_c7_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","c7"),
  Expected="-0.0501256289338006",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_c8_test(doc) -> [{userdata,[{""}]}];
sheet1_c8_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","c8"),
  Expected="-19.9498743710662",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_c3_test(doc) -> [{userdata,[{""}]}];
sheet1_c3_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","c3"),
  Expected="1.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_c4_test(doc) -> [{userdata,[{""}]}];
sheet1_c4_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","c4"),
  Expected="20.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_d5_test(doc) -> [{userdata,[{""}]}];
sheet1_d5_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","d5"),
  Expected="0.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_d1_test(doc) -> [{userdata,[{""}]}];
sheet1_d1_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","d1"),
  Expected="",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_d7_test(doc) -> [{userdata,[{""}]}];
sheet1_d7_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","d7"),
  Expected="0.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_d8_test(doc) -> [{userdata,[{""}]}];
sheet1_d8_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","d8"),
  Expected="0.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_d3_test(doc) -> [{userdata,[{""}]}];
sheet1_d3_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","d3"),
  Expected="1.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_d4_test(doc) -> [{userdata,[{""}]}];
sheet1_d4_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","d4"),
  Expected="0.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_e5_test(doc) -> [{userdata,[{""}]}];
sheet1_e5_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","e5"),
  Expected="9.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_e7_test(doc) -> [{userdata,[{""}]}];
sheet1_e7_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","e7"),
  Expected="-0.0898909545909816",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_e8_test(doc) -> [{userdata,[{""}]}];
sheet1_e8_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","e8"),
  Expected="-8.34344237874235",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_e3_test(doc) -> [{userdata,[{""}]}];
sheet1_e3_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","e3"),
  Expected="12.0",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_e4_test(doc) -> [{userdata,[{""}]}];
sheet1_e4_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","e4"),
  Expected="101.2",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_f5_test(doc) -> [{userdata,[{""}]}];
sheet1_f5_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","f5"),
  Expected="123.9",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_f7_test(doc) -> [{userdata,[{""}]}];
sheet1_f7_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","f7"),
  Expected="-0.139683370975035",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_f8_test(doc) -> [{userdata,[{""}]}];
sheet1_f8_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","f8"),
  Expected="-0.995629234255631",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_f3_test(doc) -> [{userdata,[{""}]}];
sheet1_f3_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","f3"),
  Expected="890.9",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
sheet1_f4_test(doc) -> [{userdata,[{""}]}];
sheet1_f4_test(_Config) -> 
  Got=hn_get("http://127.0.0.1:9000","/Sheet1/","f4"),
  Expected="1011.45",
  io:format("Expected : ~p~nGot      : ~p~n",[Expected,Got]),
  test_util:expected2(Expected,Got).
  
all() -> 
    [sheet1_a5_test,
   sheet1_a1_test,
   sheet1_a7_test,
   sheet1_a3_test,
   sheet1_a4_test,
   sheet1_b5_test,
   sheet1_b7_test,
   sheet1_b8_test,
   sheet1_b3_test,
   sheet1_b4_test,
   sheet1_c5_test,
   sheet1_c7_test,
   sheet1_c8_test,
   sheet1_c3_test,
   sheet1_c4_test,
   sheet1_d5_test,
   sheet1_d1_test,
   sheet1_d7_test,
   sheet1_d8_test,
   sheet1_d3_test,
   sheet1_d4_test,
   sheet1_e5_test,
   sheet1_e7_test,
   sheet1_e8_test,
   sheet1_e3_test,
   sheet1_e4_test,
   sheet1_f5_test,
   sheet1_f7_test,
   sheet1_f8_test,
   sheet1_f3_test,
   sheet1_f4_test
    ].
  
