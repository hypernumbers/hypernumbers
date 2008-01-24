% This module has been generated by generatetest.rb
% DO NOT EDIT MANUALLY.
%
% Source file: arithmetic_and_precedence.xls
% Generated on: Wed Jan 23 21:09:49 +0000 2008

-module(arithmetic_and_precedence_SUITE).
-compile(export_all).
-include("test_server.hrl").

init_per_suite(Config) ->
    code:add_patha("../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/arithmetic_and_precedence.xls"),
    io:format("in init_per_suite Data is ~p~n",[Data]),
    Table=[{arithmetic_and_precedence,ets:new(cell,[ordered_set,public,named_table])}],
    excel_util:write(Table,arithmetic_and_precedence,Data),
    Config.
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data({Row,Col}) ->
    Key={{row_index,Row},{col_index,Col}},
    Data=read(arithmetic_and_precedence,Key),
    io:format("in read_from_excel_data Data is ~p~n",[Data]),
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

read(Name,Key)->
  Return=ets:lookup(Name,Key).

a5_test(doc) -> [""];
a5_test(suite) -> [];
a5_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1+2",read_from_excel_data({4,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1+2"}, read_from_excel_data({4,0}))).
  
a16_test(doc) -> [""];
a16_test(suite) -> [];
a16_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(2+3)-(4*5)/(6^7)",read_from_excel_data({15,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(2+3)-(4*5)/(6^7)"}, read_from_excel_data({15,0}))).
  
a27_test(doc) -> [""];
a27_test(suite) -> [];
a27_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1+(2)",read_from_excel_data({26,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1+(2)"}, read_from_excel_data({26,0}))).
  
a22_test(doc) -> [""];
a22_test(suite) -> [];
a22_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1+(2+(3+(4)))",read_from_excel_data({21,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1+(2+(3+(4)))"}, read_from_excel_data({21,0}))).
  
a33_test(doc) -> [""];
a33_test(suite) -> [];
a33_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=111+-2--3*-4/-5",read_from_excel_data({32,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=111+-2--3*-4/-5"}, read_from_excel_data({32,0}))).
  
a6_test(doc) -> [""];
a6_test(suite) -> [];
a6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1-2",read_from_excel_data({5,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1-2"}, read_from_excel_data({5,0}))).
  
a17_test(doc) -> [""];
a17_test(suite) -> [];
a17_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(2^3)/(4*5)-(6+7)",read_from_excel_data({16,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(2^3)/(4*5)-(6+7)"}, read_from_excel_data({16,0}))).
  
a28_test(doc) -> [""];
a28_test(suite) -> [];
a28_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(2222+3)-(4*5)/(6^7)",read_from_excel_data({27,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(2222+3)-(4*5)/(6^7)"}, read_from_excel_data({27,0}))).
  
a1_test(doc) -> [""];
a1_test(suite) -> [];
a1_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["This spreadsheet tests basic arithmetic operations",read_from_excel_data({0,0})]),
  test_util:expected(true, test_util:excel_equal({string,"This spreadsheet tests basic arithmetic operations"}, read_from_excel_data({0,0}))).
  
a12_test(doc) -> [""];
a12_test(suite) -> [];
a12_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Precedence and brackets",read_from_excel_data({11,0})]),
  test_util:expected(true, test_util:excel_equal({string,"Precedence and brackets"}, read_from_excel_data({11,0}))).
  
a23_test(doc) -> [""];
a23_test(suite) -> [];
a23_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1+(2+(3+(4)))-(22-(33-(44-(55))))",read_from_excel_data({22,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1+(2+(3+(4)))-(22-(33-(44-(55))))"}, read_from_excel_data({22,0}))).
  
a34_test(doc) -> [""];
a34_test(suite) -> [];
a34_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=111+-2--3*-4/-5^-6",read_from_excel_data({33,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=111+-2--3*-4/-5^-6"}, read_from_excel_data({33,0}))).
  
a7_test(doc) -> [""];
a7_test(suite) -> [];
a7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1*2",read_from_excel_data({6,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1*2"}, read_from_excel_data({6,0}))).
  
a18_test(doc) -> [""];
a18_test(suite) -> [];
a18_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=((((2+3)-4)*5)^6)",read_from_excel_data({17,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=((((2+3)-4)*5)^6)"}, read_from_excel_data({17,0}))).
  
a29_test(doc) -> [""];
a29_test(suite) -> [];
a29_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=3",read_from_excel_data({28,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=3"}, read_from_excel_data({28,0}))).
  
a13_test(doc) -> [""];
a13_test(suite) -> [];
a13_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Formula",read_from_excel_data({12,0})]),
  test_util:expected(true, test_util:excel_equal({string,"Formula"}, read_from_excel_data({12,0}))).
  
a24_test(doc) -> [""];
a24_test(suite) -> [];
a24_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(1)",read_from_excel_data({23,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(1)"}, read_from_excel_data({23,0}))).
  
a35_test(doc) -> [""];
a35_test(suite) -> [];
a35_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(3333)*(4/5/6)^8-0.4+4*-0.5+(((3/4)^(4---6)*5)/7^2/3)+(3-4*5+7^((6)))",read_from_excel_data({34,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(3333)*(4/5/6)^8-0.4+4*-0.5+(((3/4)^(4---6)*5)/7^2/3)+(3-4*5+7^((6)))"}, read_from_excel_data({34,0}))).
  
a8_test(doc) -> [""];
a8_test(suite) -> [];
a8_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1/2",read_from_excel_data({7,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1/2"}, read_from_excel_data({7,0}))).
  
a19_test(doc) -> [""];
a19_test(suite) -> [];
a19_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=((1*2+3))",read_from_excel_data({18,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=((1*2+3))"}, read_from_excel_data({18,0}))).
  
a30_test(doc) -> [""];
a30_test(suite) -> [];
a30_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=111+-2",read_from_excel_data({29,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=111+-2"}, read_from_excel_data({29,0}))).
  
a3_test(doc) -> [""];
a3_test(suite) -> [];
a3_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Simple Arithmetic",read_from_excel_data({2,0})]),
  test_util:expected(true, test_util:excel_equal({string,"Simple Arithmetic"}, read_from_excel_data({2,0}))).
  
a14_test(doc) -> [""];
a14_test(suite) -> [];
a14_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=2+3-4*5/6^7",read_from_excel_data({13,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=2+3-4*5/6^7"}, read_from_excel_data({13,0}))).
  
a25_test(doc) -> [""];
a25_test(suite) -> [];
a25_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1+(2*3)",read_from_excel_data({24,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1+(2*3)"}, read_from_excel_data({24,0}))).
  
a36_test(doc) -> [""];
a36_test(suite) -> [];
a36_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=3%",read_from_excel_data({35,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=3%"}, read_from_excel_data({35,0}))).
  
a9_test(doc) -> [""];
a9_test(suite) -> [];
a9_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=1/0",read_from_excel_data({8,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=1/0"}, read_from_excel_data({8,0}))).
  
a20_test(doc) -> [""];
a20_test(suite) -> [];
a20_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(((((((4)))))))",read_from_excel_data({19,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(((((((4)))))))"}, read_from_excel_data({19,0}))).
  
a31_test(doc) -> [""];
a31_test(suite) -> [];
a31_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=111+-2--3",read_from_excel_data({30,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=111+-2--3"}, read_from_excel_data({30,0}))).
  
a4_test(doc) -> [""];
a4_test(suite) -> [];
a4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Formula",read_from_excel_data({3,0})]),
  test_util:expected(true, test_util:excel_equal({string,"Formula"}, read_from_excel_data({3,0}))).
  
a15_test(doc) -> [""];
a15_test(suite) -> [];
a15_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=2^3/4*5-6+7",read_from_excel_data({14,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=2^3/4*5-6+7"}, read_from_excel_data({14,0}))).
  
a26_test(doc) -> [""];
a26_test(suite) -> [];
a26_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(1+2)*3",read_from_excel_data({25,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(1+2)*3"}, read_from_excel_data({25,0}))).
  
a10_test(doc) -> [""];
a10_test(suite) -> [];
a10_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=2^2",read_from_excel_data({9,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=2^2"}, read_from_excel_data({9,0}))).
  
a21_test(doc) -> [""];
a21_test(suite) -> [];
a21_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=(1+(2+(3+(4))))",read_from_excel_data({20,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=(1+(2+(3+(4))))"}, read_from_excel_data({20,0}))).
  
a32_test(doc) -> [""];
a32_test(suite) -> [];
a32_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["=111+-2--3*-4",read_from_excel_data({31,0})]),
  test_util:expected(true, test_util:excel_equal({formula,"=111+-2--3*-4"}, read_from_excel_data({31,0}))).
  
b5_test(doc) -> [""];
b5_test(suite) -> [];
b5_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[3.0,read_from_excel_data({4,1})]),
  test_util:expected(true, test_util:excel_equal({number,3.0}, read_from_excel_data({4,1}))).
  
b16_test(doc) -> [""];
b16_test(suite) -> [];
b16_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4.99992855509831,read_from_excel_data({15,1})]),
  test_util:expected(true, test_util:excel_equal({number,4.99992855509831}, read_from_excel_data({15,1}))).
  
b27_test(doc) -> [""];
b27_test(suite) -> [];
b27_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[3.0,read_from_excel_data({26,1})]),
  test_util:expected(true, test_util:excel_equal({number,3.0}, read_from_excel_data({26,1}))).
  
b22_test(doc) -> [""];
b22_test(suite) -> [];
b22_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[10.0,read_from_excel_data({21,1})]),
  test_util:expected(true, test_util:excel_equal({number,10.0}, read_from_excel_data({21,1}))).
  
b33_test(doc) -> [""];
b33_test(suite) -> [];
b33_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.4,read_from_excel_data({32,1})]),
  test_util:expected(true, test_util:excel_equal({number,1.4}, read_from_excel_data({32,1}))).
  
b6_test(doc) -> [""];
b6_test(suite) -> [];
b6_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[-1.0,read_from_excel_data({5,1})]),
  test_util:expected(true, test_util:excel_equal({number,-1.0}, read_from_excel_data({5,1}))).
  
b17_test(doc) -> [""];
b17_test(suite) -> [];
b17_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[-12.6,read_from_excel_data({16,1})]),
  test_util:expected(true, test_util:excel_equal({number,-12.6}, read_from_excel_data({16,1}))).
  
b28_test(doc) -> [""];
b28_test(suite) -> [];
b28_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4.99992855509831,read_from_excel_data({27,1})]),
  test_util:expected(true, test_util:excel_equal({number,4.99992855509831}, read_from_excel_data({27,1}))).
  
b23_test(doc) -> [""];
b23_test(suite) -> [];
b23_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[32.0,read_from_excel_data({22,1})]),
  test_util:expected(true, test_util:excel_equal({number,32.0}, read_from_excel_data({22,1}))).
  
b34_test(doc) -> [""];
b34_test(suite) -> [];
b34_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[-187501.0,read_from_excel_data({33,1})]),
  test_util:expected(true, test_util:excel_equal({number,-187501.0}, read_from_excel_data({33,1}))).
  
b7_test(doc) -> [""];
b7_test(suite) -> [];
b7_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[2.0,read_from_excel_data({6,1})]),
  test_util:expected(true, test_util:excel_equal({number,2.0}, read_from_excel_data({6,1}))).
  
b18_test(doc) -> [""];
b18_test(suite) -> [];
b18_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[15625.0,read_from_excel_data({17,1})]),
  test_util:expected(true, test_util:excel_equal({number,15625.0}, read_from_excel_data({17,1}))).
  
b29_test(doc) -> [""];
b29_test(suite) -> [];
b29_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[3.0,read_from_excel_data({28,1})]),
  test_util:expected(true, test_util:excel_equal({number,3.0}, read_from_excel_data({28,1}))).
  
b13_test(doc) -> [""];
b13_test(suite) -> [];
b13_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Answer",read_from_excel_data({12,1})]),
  test_util:expected(true, test_util:excel_equal({string,"Answer"}, read_from_excel_data({12,1}))).
  
b24_test(doc) -> [""];
b24_test(suite) -> [];
b24_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[1.0,read_from_excel_data({23,1})]),
  test_util:expected(true, test_util:excel_equal({number,1.0}, read_from_excel_data({23,1}))).
  
b35_test(doc) -> [""];
b35_test(suite) -> [];
b35_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[117629.660468932,read_from_excel_data({34,1})]),
  test_util:expected(true, test_util:excel_equal({number,117629.660468932}, read_from_excel_data({34,1}))).
  
b8_test(doc) -> [""];
b8_test(suite) -> [];
b8_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[0.5,read_from_excel_data({7,1})]),
  test_util:expected(true, test_util:excel_equal({number,0.5}, read_from_excel_data({7,1}))).
  
b19_test(doc) -> [""];
b19_test(suite) -> [];
b19_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[5.0,read_from_excel_data({18,1})]),
  test_util:expected(true, test_util:excel_equal({number,5.0}, read_from_excel_data({18,1}))).
  
b30_test(doc) -> [""];
b30_test(suite) -> [];
b30_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[-1.0,read_from_excel_data({29,1})]),
  test_util:expected(true, test_util:excel_equal({number,-1.0}, read_from_excel_data({29,1}))).
  
b14_test(doc) -> [""];
b14_test(suite) -> [];
b14_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4.99992855509831,read_from_excel_data({13,1})]),
  test_util:expected(true, test_util:excel_equal({number,4.99992855509831}, read_from_excel_data({13,1}))).
  
b25_test(doc) -> [""];
b25_test(suite) -> [];
b25_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[7.0,read_from_excel_data({24,1})]),
  test_util:expected(true, test_util:excel_equal({number,7.0}, read_from_excel_data({24,1}))).
  
b36_test(doc) -> [""];
b36_test(suite) -> [];
b36_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[0.03,read_from_excel_data({35,1})]),
  test_util:expected(true, test_util:excel_equal({number,0.03}, read_from_excel_data({35,1}))).
  
b9_test(doc) -> [""];
b9_test(suite) -> [];
b9_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[-2146826281,read_from_excel_data({8,1})]),
  test_util:expected(true, test_util:excel_equal({number,-2146826281}, read_from_excel_data({8,1}))).
  
b20_test(doc) -> [""];
b20_test(suite) -> [];
b20_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4.0,read_from_excel_data({19,1})]),
  test_util:expected(true, test_util:excel_equal({number,4.0}, read_from_excel_data({19,1}))).
  
b31_test(doc) -> [""];
b31_test(suite) -> [];
b31_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[2.0,read_from_excel_data({30,1})]),
  test_util:expected(true, test_util:excel_equal({number,2.0}, read_from_excel_data({30,1}))).
  
b4_test(doc) -> [""];
b4_test(suite) -> [];
b4_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",["Answer",read_from_excel_data({3,1})]),
  test_util:expected(true, test_util:excel_equal({string,"Answer"}, read_from_excel_data({3,1}))).
  
b15_test(doc) -> [""];
b15_test(suite) -> [];
b15_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[11.0,read_from_excel_data({14,1})]),
  test_util:expected(true, test_util:excel_equal({number,11.0}, read_from_excel_data({14,1}))).
  
b26_test(doc) -> [""];
b26_test(suite) -> [];
b26_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[9.0,read_from_excel_data({25,1})]),
  test_util:expected(true, test_util:excel_equal({number,9.0}, read_from_excel_data({25,1}))).
  
b10_test(doc) -> [""];
b10_test(suite) -> [];
b10_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[4.0,read_from_excel_data({9,1})]),
  test_util:expected(true, test_util:excel_equal({number,4.0}, read_from_excel_data({9,1}))).
  
b21_test(doc) -> [""];
b21_test(suite) -> [];
b21_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[10.0,read_from_excel_data({20,1})]),
  test_util:expected(true, test_util:excel_equal({number,10.0}, read_from_excel_data({20,1}))).
  
b32_test(doc) -> [""];
b32_test(suite) -> [];
b32_test(Config) -> 
  io:format("Expected : ~p~nGot      : ~p~n",[-13.0,read_from_excel_data({31,1})]),
  test_util:expected(true, test_util:excel_equal({number,-13.0}, read_from_excel_data({31,1}))).
  
all(doc) -> [""];
all(suite) -> 
    [a5_test,
   a16_test,
   a27_test,
   a22_test,
   a33_test,
   a6_test,
   a17_test,
   a28_test,
   a1_test,
   a12_test,
   a23_test,
   a34_test,
   a7_test,
   a18_test,
   a29_test,
   a13_test,
   a24_test,
   a35_test,
   a8_test,
   a19_test,
   a30_test,
   a3_test,
   a14_test,
   a25_test,
   a36_test,
   a9_test,
   a20_test,
   a31_test,
   a4_test,
   a15_test,
   a26_test,
   a10_test,
   a21_test,
   a32_test,
   b5_test,
   b16_test,
   b27_test,
   b22_test,
   b33_test,
   b6_test,
   b17_test,
   b28_test,
   b23_test,
   b34_test,
   b7_test,
   b18_test,
   b29_test,
   b13_test,
   b24_test,
   b35_test,
   b8_test,
   b19_test,
   b30_test,
   b14_test,
   b25_test,
   b36_test,
   b9_test,
   b20_test,
   b31_test,
   b4_test,
   b15_test,
   b26_test,
   b10_test,
   b21_test,
   b32_test
    ].
  
