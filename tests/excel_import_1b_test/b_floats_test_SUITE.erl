% This module has been generated by gen_rev_comp_test.rb
% DO NOT EDIT MANUALLY.
%
% Source file: b_floats.xls
% Generated on: Fri Feb 22 19:03:30 +0000 2008

-module(b_floats_test_SUITE).
-compile(export_all).
-include("ct.hrl").


init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/b_floats.xls"),
    %% io:format("in init_per_suite Data is ~p~n",[Data]),
    Pid=spawn(test_util,test_state,[Data]),
    io:format("in init_per_suite Pid is ~p~n",[Pid]),
    [{?MODULE,Pid}|Config].
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config,{Sheet,Row,Col}) ->
  test_util:read_from_excel_data(Config,b_floats_test_SUITE,{Sheet,Row,Col}).

sheet1_a5_test(doc) -> [{userdata,[{""}]}];
sheet1_a5_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",4,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",4,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,0.03}]),
      test_util:expected2(Msg, {number,0.03})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a11_test(doc) -> [{userdata,[{""}]}];
sheet1_a11_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",10,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",10,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0e-009}]),
      test_util:expected2(Msg, {number,3.0e-009})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a6_test(doc) -> [{userdata,[{""}]}];
sheet1_a6_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",5,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",5,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,0.0003}]),
      test_util:expected2(Msg, {number,0.0003})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a12_test(doc) -> [{userdata,[{""}]}];
sheet1_a12_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",11,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",11,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0e-010}]),
      test_util:expected2(Msg, {number,3.0e-010})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a7_test(doc) -> [{userdata,[{""}]}];
sheet1_a7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0e-005}]),
      test_util:expected2(Msg, {number,3.0e-005})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a2_test(doc) -> [{userdata,[{""}]}];
sheet1_a2_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",1,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",1,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0}]),
      test_util:expected2(Msg, {number,3.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a13_test(doc) -> [{userdata,[{""}]}];
sheet1_a13_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",12,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",12,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=0.2+0.3"}]),
      test_util:expected2(Msg, {formula,"=0.2+0.3"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a8_test(doc) -> [{userdata,[{""}]}];
sheet1_a8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0e-006}]),
      test_util:expected2(Msg, {number,3.0e-006})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a3_test(doc) -> [{userdata,[{""}]}];
sheet1_a3_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",2,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",2,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.3}]),
      test_util:expected2(Msg, {number,3.3})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a14_test(doc) -> [{userdata,[{""}]}];
sheet1_a14_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",13,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",13,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=0.02+0.03"}]),
      test_util:expected2(Msg, {formula,"=0.02+0.03"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a9_test(doc) -> [{userdata,[{""}]}];
sheet1_a9_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",8,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",8,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0e-007}]),
      test_util:expected2(Msg, {number,3.0e-007})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a4_test(doc) -> [{userdata,[{""}]}];
sheet1_a4_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",3,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",3,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,0.3}]),
      test_util:expected2(Msg, {number,0.3})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_a10_test(doc) -> [{userdata,[{""}]}];
sheet1_a10_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",9,0}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",9,0}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0e-008}]),
      test_util:expected2(Msg, {number,3.0e-008})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
all() -> 
    [sheet1_a5_test,
   sheet1_a11_test,
   sheet1_a6_test,
   sheet1_a12_test,
   sheet1_a7_test,
   sheet1_a2_test,
   sheet1_a13_test,
   sheet1_a8_test,
   sheet1_a3_test,
   sheet1_a14_test,
   sheet1_a9_test,
   sheet1_a4_test,
   sheet1_a10_test
    ].
  
