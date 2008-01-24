% This module has been generated by generatetest.rb
% DO NOT EDIT MANUALLY.
%
% Source file: just_numbers.xls
% Generated on: Wed Jan 23 21:09:48 +0000 2008

-module(just_numbers_SUITE).
-compile(export_all).
-include("test_server.hrl").


init_per_suite(Config) ->
    code:add_patha("../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/just_numbers.xls"),
    %%io:format("in init_per_suite Data is ~p~n",[Data]),
    lists:merge([Config, [{just_numbers_SUITE, Data}]]).
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config, {Row,Col}) ->
    {value, Result} = lists:keysearch(just_numbers_SUITE, 1, Config),
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
  io:format("Expected : ~p~nGot      : ~p~n",[4.0,read_from_excel_data(Config,{4,0})]),
  test_util:expected(true, test_util:excel_equal({number,4.0}, read_from_excel_data(Config,{4,0}))).
  
a11_test(doc) -> [""];
a11_test(suite) -> [];
a11_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[10.0,read_from_excel_data(Config,{10,0})]),
  test_util:expected(true, test_util:excel_equal({number,10.0}, read_from_excel_data(Config,{10,0}))).
  
a6_test(doc) -> [""];
a6_test(suite) -> [];
a6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[5.0,read_from_excel_data(Config,{5,0})]),
  test_util:expected(true, test_util:excel_equal({number,5.0}, read_from_excel_data(Config,{5,0}))).
  
a1_test(doc) -> [""];
a1_test(suite) -> [];
a1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[0.0,read_from_excel_data(Config,{0,0})]),
  test_util:expected(true, test_util:excel_equal({number,0.0}, read_from_excel_data(Config,{0,0}))).
  
a7_test(doc) -> [""];
a7_test(suite) -> [];
a7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[6.0,read_from_excel_data(Config,{6,0})]),
  test_util:expected(true, test_util:excel_equal({number,6.0}, read_from_excel_data(Config,{6,0}))).
  
a2_test(doc) -> [""];
a2_test(suite) -> [];
a2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.0,read_from_excel_data(Config,{1,0})]),
  test_util:expected(true, test_util:excel_equal({number,1.0}, read_from_excel_data(Config,{1,0}))).
  
a8_test(doc) -> [""];
a8_test(suite) -> [];
a8_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[7.0,read_from_excel_data(Config,{7,0})]),
  test_util:expected(true, test_util:excel_equal({number,7.0}, read_from_excel_data(Config,{7,0}))).
  
a3_test(doc) -> [""];
a3_test(suite) -> [];
a3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[2.0,read_from_excel_data(Config,{2,0})]),
  test_util:expected(true, test_util:excel_equal({number,2.0}, read_from_excel_data(Config,{2,0}))).
  
a9_test(doc) -> [""];
a9_test(suite) -> [];
a9_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[8.0,read_from_excel_data(Config,{8,0})]),
  test_util:expected(true, test_util:excel_equal({number,8.0}, read_from_excel_data(Config,{8,0}))).
  
a4_test(doc) -> [""];
a4_test(suite) -> [];
a4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[3.0,read_from_excel_data(Config,{3,0})]),
  test_util:expected(true, test_util:excel_equal({number,3.0}, read_from_excel_data(Config,{3,0}))).
  
a10_test(doc) -> [""];
a10_test(suite) -> [];
a10_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[9.0,read_from_excel_data(Config,{9,0})]),
  test_util:expected(true, test_util:excel_equal({number,9.0}, read_from_excel_data(Config,{9,0}))).
  
c11_test(doc) -> [""];
c11_test(suite) -> [];
c11_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[777.0,read_from_excel_data(Config,{10,2})]),
  test_util:expected(true, test_util:excel_equal({number,777.0}, read_from_excel_data(Config,{10,2}))).
  
c1_test(doc) -> [""];
c1_test(suite) -> [];
c1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.1,read_from_excel_data(Config,{0,2})]),
  test_util:expected(true, test_util:excel_equal({number,1.1}, read_from_excel_data(Config,{0,2}))).
  
c2_test(doc) -> [""];
c2_test(suite) -> [];
c2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[0.12,read_from_excel_data(Config,{1,2})]),
  test_util:expected(true, test_util:excel_equal({number,0.12}, read_from_excel_data(Config,{1,2}))).
  
c3_test(doc) -> [""];
c3_test(suite) -> [];
c3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[-0.99,read_from_excel_data(Config,{2,2})]),
  test_util:expected(true, test_util:excel_equal({number,-0.99}, read_from_excel_data(Config,{2,2}))).
  
e2_test(doc) -> [""];
e2_test(suite) -> [];
e2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[0.123,read_from_excel_data(Config,{1,4})]),
  test_util:expected(true, test_util:excel_equal({number,0.123}, read_from_excel_data(Config,{1,4}))).
  
e3_test(doc) -> [""];
e3_test(suite) -> [];
e3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[123.123456789012,read_from_excel_data(Config,{2,4})]),
  test_util:expected(true, test_util:excel_equal({number,123.123456789012}, read_from_excel_data(Config,{2,4}))).
  
g5_test(doc) -> [""];
g5_test(suite) -> [];
g5_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4294967296.0,read_from_excel_data(Config,{4,6})]),
  test_util:expected(true, test_util:excel_equal({number,4294967296.0}, read_from_excel_data(Config,{4,6}))).
  
g6_test(doc) -> [""];
g6_test(suite) -> [];
g6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4294967297.0,read_from_excel_data(Config,{5,6})]),
  test_util:expected(true, test_util:excel_equal({number,4294967297.0}, read_from_excel_data(Config,{5,6}))).
  
g1_test(doc) -> [""];
g1_test(suite) -> [];
g1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[0.0,read_from_excel_data(Config,{0,6})]),
  test_util:expected(true, test_util:excel_equal({number,0.0}, read_from_excel_data(Config,{0,6}))).
  
g7_test(doc) -> [""];
g7_test(suite) -> [];
g7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.84467440737095e+019,read_from_excel_data(Config,{6,6})]),
  test_util:expected(true, test_util:excel_equal({number,1.84467440737095e+019}, read_from_excel_data(Config,{6,6}))).
  
g2_test(doc) -> [""];
g2_test(suite) -> [];
g2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[256.0,read_from_excel_data(Config,{1,6})]),
  test_util:expected(true, test_util:excel_equal({number,256.0}, read_from_excel_data(Config,{1,6}))).
  
g3_test(doc) -> [""];
g3_test(suite) -> [];
g3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[65535.0,read_from_excel_data(Config,{2,6})]),
  test_util:expected(true, test_util:excel_equal({number,65535.0}, read_from_excel_data(Config,{2,6}))).
  
g53_test(doc) -> [""];
g53_test(suite) -> [];
g53_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.0,read_from_excel_data(Config,{52,6})]),
  test_util:expected(true, test_util:excel_equal({number,1.0}, read_from_excel_data(Config,{52,6}))).
  
g4_test(doc) -> [""];
g4_test(suite) -> [];
g4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[16777216.0,read_from_excel_data(Config,{3,6})]),
  test_util:expected(true, test_util:excel_equal({number,16777216.0}, read_from_excel_data(Config,{3,6}))).
  
all(doc) -> [""];
all(suite) -> 
    [a5_test,
   a11_test,
   a6_test,
   a1_test,
   a7_test,
   a2_test,
   a8_test,
   a3_test,
   a9_test,
   a4_test,
   a10_test,
   c11_test,
   c1_test,
   c2_test,
   c3_test,
   e2_test,
   e3_test,
   g5_test,
   g6_test,
   g1_test,
   g7_test,
   g2_test,
   g3_test,
   g53_test,
   g4_test
    ].
  
