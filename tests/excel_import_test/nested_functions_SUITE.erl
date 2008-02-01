% This module has been generated by generatetest.rb
% DO NOT EDIT MANUALLY.
%
% Source file: nested_functions.xls
% Generated on: Sat Jan 26 01:08:44 +0000 2008

-module(nested_functions_SUITE).
-compile(export_all).
-include("ct.hrl").


init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/nested_functions.xls"),
    %% io:format("in init_per_suite Data is ~p~n",[Data]),
    lists:merge([Config, [{nested_functions_SUITE, Data}]]).
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config, {Row,Col}) ->
    {value, Result} = lists:keysearch(nested_functions_SUITE, 1, Config),
    Data = element(2, Result),
    Key={{row_index,Row},{col_index,Col}},
    {value, Result2} = lists:keysearch(Key, 1, Data),
    El=element(2, Result2),
    %% io:format("El is ~p~n",[El]),
    case El of
        {value, number, Number} -> {number,Number};
        {string,String}         -> {string,String};
        {formula,Formula}       -> {formula,Formula};
        {value,boolean,Boolean} -> {boolean,Boolean};
        {value,error,Error}     -> {error, Error};
        Other                   -> io:format("(in generatetest.rb - fix me Other is ~p~n",[Other])
    end.

a5_test(doc) -> [{userdata,[{""}]}];
a5_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Single nested functions"},read_from_excel_data(Config,{4,0})]),
  test_util:expected2(read_from_excel_data(Config,{4,0}), {string,"Single nested functions"}).
  
a11_test(doc) -> [{userdata,[{""}]}];
a11_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Multiply nested functions"},read_from_excel_data(Config,{10,0})]),
  test_util:expected2(read_from_excel_data(Config,{10,0}), {string,"Multiply nested functions"}).
  
a6_test(doc) -> [{userdata,[{""}]}];
a6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Multiply nested functions"},read_from_excel_data(Config,{5,0})]),
  test_util:expected2(read_from_excel_data(Config,{5,0}), {string,"Multiply nested functions"}).
  
a1_test(doc) -> [{userdata,[{""}]}];
a1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"This spreadsheet test nested function calls"},read_from_excel_data(Config,{0,0})]),
  test_util:expected2(read_from_excel_data(Config,{0,0}), {string,"This spreadsheet test nested function calls"}).
  
a7_test(doc) -> [{userdata,[{""}]}];
a7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Multiply nested functions"},read_from_excel_data(Config,{6,0})]),
  test_util:expected2(read_from_excel_data(Config,{6,0}), {string,"Multiply nested functions"}).
  
a8_test(doc) -> [{userdata,[{""}]}];
a8_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Multiply nested functions"},read_from_excel_data(Config,{7,0})]),
  test_util:expected2(read_from_excel_data(Config,{7,0}), {string,"Multiply nested functions"}).
  
a3_test(doc) -> [{userdata,[{""}]}];
a3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Nested funs except SUM with 1 arg or CHOOSE"},read_from_excel_data(Config,{2,0})]),
  test_util:expected2(read_from_excel_data(Config,{2,0}), {string,"Nested funs except SUM with 1 arg or CHOOSE"}).
  
a9_test(doc) -> [{userdata,[{""}]}];
a9_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Multiply nested functions"},read_from_excel_data(Config,{8,0})]),
  test_util:expected2(read_from_excel_data(Config,{8,0}), {string,"Multiply nested functions"}).
  
a4_test(doc) -> [{userdata,[{""}]}];
a4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Argument with operators"},read_from_excel_data(Config,{3,0})]),
  test_util:expected2(read_from_excel_data(Config,{3,0}), {string,"Argument with operators"}).
  
a10_test(doc) -> [{userdata,[{""}]}];
a10_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{string,"Multiply nested functions"},read_from_excel_data(Config,{9,0})]),
  test_util:expected2(read_from_excel_data(Config,{9,0}), {string,"Multiply nested functions"}).
  
b5_test(doc) -> [{userdata,[{""}]}];
b5_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=SQRT(AVERAGE(33,4)-AVERAGE(4,5))"},read_from_excel_data(Config,{4,1})]),
  test_util:expected2(read_from_excel_data(Config,{4,1}), {formula,"=SQRT(AVERAGE(33,4)-AVERAGE(4,5))"}).
  
b11_test(doc) -> [{userdata,[{""}]}];
b11_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=SQRT(2+SQRT(1))"},read_from_excel_data(Config,{10,1})]),
  test_util:expected2(read_from_excel_data(Config,{10,1}), {formula,"=SQRT(2+SQRT(1))"}).
  
b6_test(doc) -> [{userdata,[{""}]}];
b6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=SQRT(AVERAGE(AVERAGE(5,6,7)+5-SQRT(3),4)-AVERAGE(4,5))"},read_from_excel_data(Config,{5,1})]),
  test_util:expected2(read_from_excel_data(Config,{5,1}), {formula,"=SQRT(AVERAGE(AVERAGE(5,6,7)+5-SQRT(3),4)-AVERAGE(4,5))"}).
  
b7_test(doc) -> [{userdata,[{""}]}];
b7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=AVERAGE(AVERAGE(5,6,7)+8-9,4)"},read_from_excel_data(Config,{6,1})]),
  test_util:expected2(read_from_excel_data(Config,{6,1}), {formula,"=AVERAGE(AVERAGE(5,6,7)+8-9,4)"}).
  
b8_test(doc) -> [{userdata,[{""}]}];
b8_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=AVERAGE(AVERAGE(5,6,7)+SQRT(12)-9,4)"},read_from_excel_data(Config,{7,1})]),
  test_util:expected2(read_from_excel_data(Config,{7,1}), {formula,"=AVERAGE(AVERAGE(5,6,7)+SQRT(12)-9,4)"}).
  
b9_test(doc) -> [{userdata,[{""}]}];
b9_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=AVERAGE(AVERAGE(5,6,7)+SQRT(1),4)"},read_from_excel_data(Config,{8,1})]),
  test_util:expected2(read_from_excel_data(Config,{8,1}), {formula,"=AVERAGE(AVERAGE(5,6,7)+SQRT(1),4)"}).
  
b4_test(doc) -> [{userdata,[{""}]}];
b4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=AVERAGE(1+2,(3*5+4))"},read_from_excel_data(Config,{3,1})]),
  test_util:expected2(read_from_excel_data(Config,{3,1}), {formula,"=AVERAGE(1+2,(3*5+4))"}).
  
b10_test(doc) -> [{userdata,[{""}]}];
b10_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[{formula,"=SQRT(AVERAGE(5,6,7)+SQRT(1))"},read_from_excel_data(Config,{9,1})]),
  test_util:expected2(read_from_excel_data(Config,{9,1}), {formula,"=SQRT(AVERAGE(5,6,7)+SQRT(1))"}).
  
all() -> 
    [a5_test,
   a11_test,
   a6_test,
   a1_test,
   a7_test,
   a8_test,
   a3_test,
   a9_test,
   a4_test,
   a10_test,
   b5_test,
   b11_test,
   b6_test,
   b7_test,
   b8_test,
   b9_test,
   b4_test,
   b10_test
    ].
  
