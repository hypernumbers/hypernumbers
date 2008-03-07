% This module has been generated by gen_rev_comp_test.rb
% DO NOT EDIT MANUALLY.
%
% Source file: b_shared_formulae.xls
% Generated on: Mon Mar 03 13:54:35 +0000 2008

-module(b_shared_formulae_test_SUITE).
-compile(export_all).
-include("ct.hrl").


init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/b_shared_formulae.xls"),
    %% io:format("in init_per_suite Data is ~p~n",[Data]),
    Pid=spawn(test_util,test_state,[Data]),
    io:format("in init_per_suite Pid is ~p~n",[Pid]),
    [{?MODULE,Pid}|Config].
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config,{Sheet,Row,Col}) ->
  test_util:read_from_excel_data(Config,b_shared_formulae_test_SUITE,{Sheet,Row,Col}).

sheet1_k7_test(doc) -> [{userdata,[{""}]}];
sheet1_k7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,10}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,10}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,1.0}]),
      test_util:expected2(Msg, {number,1.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_k8_test(doc) -> [{userdata,[{""}]}];
sheet1_k8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,10}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,10}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=K7*99"}]),
      test_util:expected2(Msg, {formula,"=K7*99"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"Numbers->"}]),
      test_util:expected2(Msg, {string,"Numbers->"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,1.0}]),
      test_util:expected2(Msg, {number,1.0})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"Formulae ->"}]),
      test_util:expected2(Msg, {string,"Formulae ->"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,2.0}]),
      test_util:expected2(Msg, {number,2.0})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0}]),
      test_util:expected2(Msg, {number,3.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b16_test(doc) -> [{userdata,[{""}]}];
sheet1_b16_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",15,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",15,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,50.0}]),
      test_util:expected2(Msg, {number,50.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b11_test(doc) -> [{userdata,[{""}]}];
sheet1_b11_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",10,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",10,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,100.0}]),
      test_util:expected2(Msg, {number,100.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b17_test(doc) -> [{userdata,[{""}]}];
sheet1_b17_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",16,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",16,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,40.0}]),
      test_util:expected2(Msg, {number,40.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b12_test(doc) -> [{userdata,[{""}]}];
sheet1_b12_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",11,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",11,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,90.0}]),
      test_util:expected2(Msg, {number,90.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b7_test(doc) -> [{userdata,[{""}]}];
sheet1_b7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,10.0}]),
      test_util:expected2(Msg, {number,10.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b18_test(doc) -> [{userdata,[{""}]}];
sheet1_b18_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",17,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",17,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,30.0}]),
      test_util:expected2(Msg, {number,30.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b2_test(doc) -> [{userdata,[{""}]}];
sheet1_b2_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",1,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",1,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,2.0}]),
      test_util:expected2(Msg, {number,2.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b13_test(doc) -> [{userdata,[{""}]}];
sheet1_b13_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",12,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",12,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,80.0}]),
      test_util:expected2(Msg, {number,80.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b8_test(doc) -> [{userdata,[{""}]}];
sheet1_b8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B7*99"}]),
      test_util:expected2(Msg, {formula,"=B7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b19_test(doc) -> [{userdata,[{""}]}];
sheet1_b19_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",18,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",18,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,20.0}]),
      test_util:expected2(Msg, {number,20.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b3_test(doc) -> [{userdata,[{""}]}];
sheet1_b3_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",2,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",2,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0}]),
      test_util:expected2(Msg, {number,3.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b14_test(doc) -> [{userdata,[{""}]}];
sheet1_b14_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",13,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",13,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,70.0}]),
      test_util:expected2(Msg, {number,70.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b20_test(doc) -> [{userdata,[{""}]}];
sheet1_b20_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",19,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",19,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,10.0}]),
      test_util:expected2(Msg, {number,10.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b4_test(doc) -> [{userdata,[{""}]}];
sheet1_b4_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",3,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",3,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,4.0}]),
      test_util:expected2(Msg, {number,4.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b15_test(doc) -> [{userdata,[{""}]}];
sheet1_b15_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",14,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",14,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,60.0}]),
      test_util:expected2(Msg, {number,60.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b10_test(doc) -> [{userdata,[{""}]}];
sheet1_b10_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",9,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",9,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"Numbers"}]),
      test_util:expected2(Msg, {string,"Numbers"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c16_test(doc) -> [{userdata,[{""}]}];
sheet1_c16_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",15,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",15,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B16*999"}]),
      test_util:expected2(Msg, {formula,"=B16*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c11_test(doc) -> [{userdata,[{""}]}];
sheet1_c11_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",10,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",10,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B11*999"}]),
      test_util:expected2(Msg, {formula,"=B11*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c17_test(doc) -> [{userdata,[{""}]}];
sheet1_c17_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",16,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",16,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B17*999"}]),
      test_util:expected2(Msg, {formula,"=B17*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c12_test(doc) -> [{userdata,[{""}]}];
sheet1_c12_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",11,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",11,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B12*999"}]),
      test_util:expected2(Msg, {formula,"=B12*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c7_test(doc) -> [{userdata,[{""}]}];
sheet1_c7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,9.0}]),
      test_util:expected2(Msg, {number,9.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c18_test(doc) -> [{userdata,[{""}]}];
sheet1_c18_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",17,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",17,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B18*999"}]),
      test_util:expected2(Msg, {formula,"=B18*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c2_test(doc) -> [{userdata,[{""}]}];
sheet1_c2_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",1,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",1,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(A2:B2)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(A2:B2)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c13_test(doc) -> [{userdata,[{""}]}];
sheet1_c13_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",12,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",12,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B13*999"}]),
      test_util:expected2(Msg, {formula,"=B13*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c8_test(doc) -> [{userdata,[{""}]}];
sheet1_c8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=C7*99"}]),
      test_util:expected2(Msg, {formula,"=C7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c19_test(doc) -> [{userdata,[{""}]}];
sheet1_c19_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",18,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",18,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B19*999"}]),
      test_util:expected2(Msg, {formula,"=B19*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c3_test(doc) -> [{userdata,[{""}]}];
sheet1_c3_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",2,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",2,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(A3:B3)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(A3:B3)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c14_test(doc) -> [{userdata,[{""}]}];
sheet1_c14_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",13,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",13,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B14*999"}]),
      test_util:expected2(Msg, {formula,"=B14*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c20_test(doc) -> [{userdata,[{""}]}];
sheet1_c20_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",19,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",19,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B20*999"}]),
      test_util:expected2(Msg, {formula,"=B20*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c4_test(doc) -> [{userdata,[{""}]}];
sheet1_c4_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",3,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",3,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(A4:B4)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(A4:B4)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c15_test(doc) -> [{userdata,[{""}]}];
sheet1_c15_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",14,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",14,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=B15*999"}]),
      test_util:expected2(Msg, {formula,"=B15*999"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_c10_test(doc) -> [{userdata,[{""}]}];
sheet1_c10_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",9,2}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",9,2}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"Formulae"}]),
      test_util:expected2(Msg, {string,"Formulae"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_d7_test(doc) -> [{userdata,[{""}]}];
sheet1_d7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,3}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,3}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,8.0}]),
      test_util:expected2(Msg, {number,8.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_d2_test(doc) -> [{userdata,[{""}]}];
sheet1_d2_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",1,3}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",1,3}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(B2:C2)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(B2:C2)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_d8_test(doc) -> [{userdata,[{""}]}];
sheet1_d8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,3}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,3}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=D7*99"}]),
      test_util:expected2(Msg, {formula,"=D7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_d3_test(doc) -> [{userdata,[{""}]}];
sheet1_d3_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",2,3}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",2,3}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(B3:C3)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(B3:C3)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_d4_test(doc) -> [{userdata,[{""}]}];
sheet1_d4_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",3,3}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",3,3}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(B4:C4)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(B4:C4)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_e7_test(doc) -> [{userdata,[{""}]}];
sheet1_e7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,4}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,4}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,7.0}]),
      test_util:expected2(Msg, {number,7.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_e2_test(doc) -> [{userdata,[{""}]}];
sheet1_e2_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",1,4}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",1,4}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(C2:D2)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(C2:D2)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_e8_test(doc) -> [{userdata,[{""}]}];
sheet1_e8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,4}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,4}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=E7*99"}]),
      test_util:expected2(Msg, {formula,"=E7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_e3_test(doc) -> [{userdata,[{""}]}];
sheet1_e3_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",2,4}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",2,4}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(C3:D3)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(C3:D3)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_e4_test(doc) -> [{userdata,[{""}]}];
sheet1_e4_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",3,4}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",3,4}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=AVERAGE(C4:D4)"}]),
      test_util:expected2(Msg, {formula,"=AVERAGE(C4:D4)"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_f7_test(doc) -> [{userdata,[{""}]}];
sheet1_f7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,5}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,5}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,6.0}]),
      test_util:expected2(Msg, {number,6.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_f8_test(doc) -> [{userdata,[{""}]}];
sheet1_f8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,5}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,5}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=F7*99"}]),
      test_util:expected2(Msg, {formula,"=F7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_g7_test(doc) -> [{userdata,[{""}]}];
sheet1_g7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,6}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,6}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,5.0}]),
      test_util:expected2(Msg, {number,5.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_g8_test(doc) -> [{userdata,[{""}]}];
sheet1_g8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,6}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,6}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=G7*99"}]),
      test_util:expected2(Msg, {formula,"=G7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_h7_test(doc) -> [{userdata,[{""}]}];
sheet1_h7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,7}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,7}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,4.0}]),
      test_util:expected2(Msg, {number,4.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_h8_test(doc) -> [{userdata,[{""}]}];
sheet1_h8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,7}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,7}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=H7*99"}]),
      test_util:expected2(Msg, {formula,"=H7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_i7_test(doc) -> [{userdata,[{""}]}];
sheet1_i7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,8}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,8}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,3.0}]),
      test_util:expected2(Msg, {number,3.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_i8_test(doc) -> [{userdata,[{""}]}];
sheet1_i8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,8}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,8}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=I7*99"}]),
      test_util:expected2(Msg, {formula,"=I7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_j7_test(doc) -> [{userdata,[{""}]}];
sheet1_j7_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",6,9}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",6,9}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{number,2.0}]),
      test_util:expected2(Msg, {number,2.0})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_j8_test(doc) -> [{userdata,[{""}]}];
sheet1_j8_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",7,9}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",7,9}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=J7*99"}]),
      test_util:expected2(Msg, {formula,"=J7*99"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
all() -> 
    [sheet1_k7_test,
   sheet1_k8_test,
   sheet1_a7_test,
   sheet1_a2_test,
   sheet1_a8_test,
   sheet1_a3_test,
   sheet1_a4_test,
   sheet1_b16_test,
   sheet1_b11_test,
   sheet1_b17_test,
   sheet1_b12_test,
   sheet1_b7_test,
   sheet1_b18_test,
   sheet1_b2_test,
   sheet1_b13_test,
   sheet1_b8_test,
   sheet1_b19_test,
   sheet1_b3_test,
   sheet1_b14_test,
   sheet1_b20_test,
   sheet1_b4_test,
   sheet1_b15_test,
   sheet1_b10_test,
   sheet1_c16_test,
   sheet1_c11_test,
   sheet1_c17_test,
   sheet1_c12_test,
   sheet1_c7_test,
   sheet1_c18_test,
   sheet1_c2_test,
   sheet1_c13_test,
   sheet1_c8_test,
   sheet1_c19_test,
   sheet1_c3_test,
   sheet1_c14_test,
   sheet1_c20_test,
   sheet1_c4_test,
   sheet1_c15_test,
   sheet1_c10_test,
   sheet1_d7_test,
   sheet1_d2_test,
   sheet1_d8_test,
   sheet1_d3_test,
   sheet1_d4_test,
   sheet1_e7_test,
   sheet1_e2_test,
   sheet1_e8_test,
   sheet1_e3_test,
   sheet1_e4_test,
   sheet1_f7_test,
   sheet1_f8_test,
   sheet1_g7_test,
   sheet1_g8_test,
   sheet1_h7_test,
   sheet1_h8_test,
   sheet1_i7_test,
   sheet1_i8_test,
   sheet1_j7_test,
   sheet1_j8_test
    ].
  
