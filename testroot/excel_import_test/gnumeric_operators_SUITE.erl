% This module has been generated by generatetest.rb
% DO NOT EDIT MANUALLY.
%
% Source file: gnumeric_operators.xls
% Generated on: Wed Jan 23 21:10:00 +0000 2008

-module(gnumeric_operators_SUITE).
-compile(export_all).
-include("test_server.hrl").


init_per_suite(Config) ->
    code:add_patha("../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/gnumeric_operators.xls"),
    %%io:format("in init_per_suite Data is ~p~n",[Data]),
    lists:merge([Config, [{gnumeric_operators_SUITE, Data}]]).
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config, {Row,Col}) ->
    {value, Result} = lists:keysearch(gnumeric_operators_SUITE, 1, Config),
    Data = element(2, Result),
    Key={{row_index,Row},{col_index,Col}},
    {value, Result2} = lists:keysearch(Key, 1, Data),
    El=element(2, Result2),
    %%io:format("El is ~p~n",[El]),
    case El of
        {value, number, Number} -> {number,Number};
        {string,String}         -> {string,String};
        {formula,Formula}       -> {formula,Formula};
        {value,boolean,Boolean} -> {boolean,Boolean};
        {value,error,Error}     -> {error, Error};
        Other                   -> io:format("(in generatetest.rb - fix me Other is ~p~n",[Other])
    end.

a5_test(doc) -> [""];
a5_test(suite) -> [];
a5_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["LTE",read_from_excel_data(Config,{4,0})]),
  test_util:expected(true, test_util:excel_equal({string,"LTE"}, read_from_excel_data(Config,{4,0}))).
  
a16_test(doc) -> [""];
a16_test(suite) -> [];
a16_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Epsilon :",read_from_excel_data(Config,{15,0})]),
  test_util:expected(true, test_util:excel_equal({string,"Epsilon :"}, read_from_excel_data(Config,{15,0}))).
  
a11_test(doc) -> [""];
a11_test(suite) -> [];
a11_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["EXP",read_from_excel_data(Config,{10,0})]),
  test_util:expected(true, test_util:excel_equal({string,"EXP"}, read_from_excel_data(Config,{10,0}))).
  
a6_test(doc) -> [""];
a6_test(suite) -> [];
a6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["NOT_EQUAL",read_from_excel_data(Config,{5,0})]),
  test_util:expected(true, test_util:excel_equal({string,"NOT_EQUAL"}, read_from_excel_data(Config,{5,0}))).
  
a1_test(doc) -> [""];
a1_test(suite) -> [];
a1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["EQUAL",read_from_excel_data(Config,{0,0})]),
  test_util:expected(true, test_util:excel_equal({string,"EQUAL"}, read_from_excel_data(Config,{0,0}))).
  
a12_test(doc) -> [""];
a12_test(suite) -> [];
a12_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["CONCAT",read_from_excel_data(Config,{11,0})]),
  test_util:expected(true, test_util:excel_equal({string,"CONCAT"}, read_from_excel_data(Config,{11,0}))).
  
a7_test(doc) -> [""];
a7_test(suite) -> [];
a7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["ADD",read_from_excel_data(Config,{6,0})]),
  test_util:expected(true, test_util:excel_equal({string,"ADD"}, read_from_excel_data(Config,{6,0}))).
  
a2_test(doc) -> [""];
a2_test(suite) -> [];
a2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["GT",read_from_excel_data(Config,{1,0})]),
  test_util:expected(true, test_util:excel_equal({string,"GT"}, read_from_excel_data(Config,{1,0}))).
  
a8_test(doc) -> [""];
a8_test(suite) -> [];
a8_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["SUB",read_from_excel_data(Config,{7,0})]),
  test_util:expected(true, test_util:excel_equal({string,"SUB"}, read_from_excel_data(Config,{7,0}))).
  
a3_test(doc) -> [""];
a3_test(suite) -> [];
a3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["LT",read_from_excel_data(Config,{2,0})]),
  test_util:expected(true, test_util:excel_equal({string,"LT"}, read_from_excel_data(Config,{2,0}))).
  
a9_test(doc) -> [""];
a9_test(suite) -> [];
a9_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["MULT",read_from_excel_data(Config,{8,0})]),
  test_util:expected(true, test_util:excel_equal({string,"MULT"}, read_from_excel_data(Config,{8,0}))).
  
a4_test(doc) -> [""];
a4_test(suite) -> [];
a4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["GTE",read_from_excel_data(Config,{3,0})]),
  test_util:expected(true, test_util:excel_equal({string,"GTE"}, read_from_excel_data(Config,{3,0}))).
  
a10_test(doc) -> [""];
a10_test(suite) -> [];
a10_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["DIV",read_from_excel_data(Config,{9,0})]),
  test_util:expected(true, test_util:excel_equal({string,"DIV"}, read_from_excel_data(Config,{9,0}))).
  
b16_test(doc) -> [""];
b16_test(suite) -> [];
b16_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.0e-006,read_from_excel_data(Config,{15,1})]),
  test_util:expected(true, test_util:excel_equal({number,1.0e-006}, read_from_excel_data(Config,{15,1}))).
  
c5_test(doc) -> [""];
c5_test(suite) -> [];
c5_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=LTE!A48",read_from_excel_data(Config,{4,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=LTE!A48"}, read_from_excel_data(Config,{4,2}))).
  
c11_test(doc) -> [""];
c11_test(suite) -> [];
c11_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=EXP!A48",read_from_excel_data(Config,{10,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=EXP!A48"}, read_from_excel_data(Config,{10,2}))).
  
c6_test(doc) -> [""];
c6_test(suite) -> [];
c6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=NOT_EQUAL!A48",read_from_excel_data(Config,{5,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=NOT_EQUAL!A48"}, read_from_excel_data(Config,{5,2}))).
  
c1_test(doc) -> [""];
c1_test(suite) -> [];
c1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=EQUAL!A48",read_from_excel_data(Config,{0,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=EQUAL!A48"}, read_from_excel_data(Config,{0,2}))).
  
c12_test(doc) -> [""];
c12_test(suite) -> [];
c12_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=CONCAT!A48",read_from_excel_data(Config,{11,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=CONCAT!A48"}, read_from_excel_data(Config,{11,2}))).
  
c7_test(doc) -> [""];
c7_test(suite) -> [];
c7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=ADD!A48",read_from_excel_data(Config,{6,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=ADD!A48"}, read_from_excel_data(Config,{6,2}))).
  
c2_test(doc) -> [""];
c2_test(suite) -> [];
c2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=GT!A48",read_from_excel_data(Config,{1,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=GT!A48"}, read_from_excel_data(Config,{1,2}))).
  
c8_test(doc) -> [""];
c8_test(suite) -> [];
c8_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=SUB!A48",read_from_excel_data(Config,{7,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=SUB!A48"}, read_from_excel_data(Config,{7,2}))).
  
c3_test(doc) -> [""];
c3_test(suite) -> [];
c3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=LT!A48",read_from_excel_data(Config,{2,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=LT!A48"}, read_from_excel_data(Config,{2,2}))).
  
c9_test(doc) -> [""];
c9_test(suite) -> [];
c9_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=MULT!A48",read_from_excel_data(Config,{8,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=MULT!A48"}, read_from_excel_data(Config,{8,2}))).
  
c4_test(doc) -> [""];
c4_test(suite) -> [];
c4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=GTE!A48",read_from_excel_data(Config,{3,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=GTE!A48"}, read_from_excel_data(Config,{3,2}))).
  
c10_test(doc) -> [""];
c10_test(suite) -> [];
c10_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=DIV!A48",read_from_excel_data(Config,{9,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=DIV!A48"}, read_from_excel_data(Config,{9,2}))).
  
e1_test(doc) -> [""];
e1_test(suite) -> [];
e1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["This set of results was calculated on Excel 97 SR-2",read_from_excel_data(Config,{0,4})]),
  test_util:expected(true, test_util:excel_equal({string,"This set of results was calculated on Excel 97 SR-2"}, read_from_excel_data(Config,{0,4}))).
  
all(doc) -> [""];
all(suite) -> 
    [a5_test,
   a16_test,
   a11_test,
   a6_test,
   a1_test,
   a12_test,
   a7_test,
   a2_test,
   a8_test,
   a3_test,
   a9_test,
   a4_test,
   a10_test,
   b16_test,
   c5_test,
   c11_test,
   c6_test,
   c1_test,
   c12_test,
   c7_test,
   c2_test,
   c8_test,
   c3_test,
   c9_test,
   c4_test,
   c10_test,
   e1_test
    ].
  
