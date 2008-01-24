% This module has been generated by generatetest.rb
% DO NOT EDIT MANUALLY.
%
% Source file: array_formulae.xls
% Generated on: Wed Jan 23 21:09:54 +0000 2008

-module(array_formulae_SUITE).
-compile(export_all).
-include("test_server.hrl").


init_per_suite(Config) ->
    code:add_patha("../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/array_formulae.xls"),
    %%io:format("in init_per_suite Data is ~p~n",[Data]),
    lists:merge([Config, [{array_formulae_SUITE, Data}]]).
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config, {Row,Col}) ->
    {value, Result} = lists:keysearch(array_formulae_SUITE, 1, Config),
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
  io:format("Expected : ~p~nGot      : ~p~n",["Functions with arrays",read_from_excel_data(Config,{4,0})]),
  test_util:expected(true, test_util:excel_equal({string,"Functions with arrays"}, read_from_excel_data(Config,{4,0}))).
  
a6_test(doc) -> [""];
a6_test(suite) -> [];
a6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["1D Sum",read_from_excel_data(Config,{5,0})]),
  test_util:expected(true, test_util:excel_equal({string,"1D Sum"}, read_from_excel_data(Config,{5,0}))).
  
a1_test(doc) -> [""];
a1_test(suite) -> [];
a1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Array Formulae",read_from_excel_data(Config,{0,0})]),
  test_util:expected(true, test_util:excel_equal({string,"Array Formulae"}, read_from_excel_data(Config,{0,0}))).
  
a7_test(doc) -> [""];
a7_test(suite) -> [];
a7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["2D Sum",read_from_excel_data(Config,{6,0})]),
  test_util:expected(true, test_util:excel_equal({string,"2D Sum"}, read_from_excel_data(Config,{6,0}))).
  
a2_test(doc) -> [""];
a2_test(suite) -> [];
a2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.0,read_from_excel_data(Config,{1,0})]),
  test_util:expected(true, test_util:excel_equal({number,1.0}, read_from_excel_data(Config,{1,0}))).
  
a3_test(doc) -> [""];
a3_test(suite) -> [];
a3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[3.0,read_from_excel_data(Config,{2,0})]),
  test_util:expected(true, test_util:excel_equal({number,3.0}, read_from_excel_data(Config,{2,0}))).
  
b6_test(doc) -> [""];
b6_test(suite) -> [];
b6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=SUM({1,2,3})",read_from_excel_data(Config,{5,1})]),
  test_util:expected(true, test_util:excel_equal({formula,"=SUM({1,2,3})"}, read_from_excel_data(Config,{5,1}))).
  
b7_test(doc) -> [""];
b7_test(suite) -> [];
b7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=SUM({1,2,3;4,5,6;7,8,9})",read_from_excel_data(Config,{6,1})]),
  test_util:expected(true, test_util:excel_equal({formula,"=SUM({1,2,3;4,5,6;7,8,9})"}, read_from_excel_data(Config,{6,1}))).
  
b2_test(doc) -> [""];
b2_test(suite) -> [];
b2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[2.0,read_from_excel_data(Config,{1,1})]),
  test_util:expected(true, test_util:excel_equal({number,2.0}, read_from_excel_data(Config,{1,1}))).
  
b3_test(doc) -> [""];
b3_test(suite) -> [];
b3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4.0,read_from_excel_data(Config,{2,1})]),
  test_util:expected(true, test_util:excel_equal({number,4.0}, read_from_excel_data(Config,{2,1}))).
  
c2_test(doc) -> [""];
c2_test(suite) -> [];
c2_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=A2:A3*B2:B3",read_from_excel_data(Config,{1,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=A2:A3*B2:B3"}, read_from_excel_data(Config,{1,2}))).
  
c3_test(doc) -> [""];
c3_test(suite) -> [];
c3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=A2:A3*B2:B3",read_from_excel_data(Config,{2,2})]),
  test_util:expected(true, test_util:excel_equal({formula,"=A2:A3*B2:B3"}, read_from_excel_data(Config,{2,2}))).
  
all(doc) -> [""];
all(suite) -> 
    [a5_test,
   a6_test,
   a1_test,
   a7_test,
   a2_test,
   a3_test,
   b6_test,
   b7_test,
   b2_test,
   b3_test,
   c2_test,
   c3_test
    ].
  
