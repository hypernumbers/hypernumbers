% This module has been generated by gen_rev_comp_test.rb
% DO NOT EDIT MANUALLY.
%
% Source file: b_basic_unicode_strings.xls
% Generated on: Fri Feb 22 19:03:29 +0000 2008

-module(b_basic_unicode_strings_test_SUITE).
-compile(export_all).
-include("ct.hrl").


init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:setup_paths(),
    Data = test_util:read_excel_file("/Win Excel 2007 (as 97)/b_basic_unicode_strings.xls"),
    %% io:format("in init_per_suite Data is ~p~n",[Data]),
    Pid=spawn(test_util,test_state,[Data]),
    io:format("in init_per_suite Pid is ~p~n",[Pid]),
    [{?MODULE,Pid}|Config].
  
end_per_suite(_Config) ->
    ok.
  
init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

read_from_excel_data(Config,{Sheet,Row,Col}) ->
  test_util:read_from_excel_data(Config,b_basic_unicode_strings_test_SUITE,{Sheet,Row,Col}).

sheet1_b16_test(doc) -> [{userdata,[{""}]}];
sheet1_b16_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",15,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",15,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"�"}]),
      test_util:expected2(Msg, {string,"�"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b27_test(doc) -> [{userdata,[{""}]}];
sheet1_b27_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",26,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",26,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"������\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"������\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b38_test(doc) -> [{userdata,[{""}]}];
sheet1_b38_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",37,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",37,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"l???�?\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"l???�?\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"??;???????G?"}]),
      test_util:expected2(Msg, {string,"??;???????G?"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b22_test(doc) -> [{userdata,[{""}]}];
sheet1_b22_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",21,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",21,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"?????"}]),
      test_util:expected2(Msg, {string,"?????"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b33_test(doc) -> [{userdata,[{""}]}];
sheet1_b33_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",32,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",32,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"???????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"???????\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b6_test(doc) -> [{userdata,[{""}]}];
sheet1_b6_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",5,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",5,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"������"}]),
      test_util:expected2(Msg, {string,"������"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"l???�?"}]),
      test_util:expected2(Msg, {string,"l???�?"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b28_test(doc) -> [{userdata,[{""}]}];
sheet1_b28_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",27,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",27,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"AaAaAa\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"AaAaAa\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b39_test(doc) -> [{userdata,[{""}]}];
sheet1_b39_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",38,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",38,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"??????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"??????\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"???????"}]),
      test_util:expected2(Msg, {string,"???????"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b34_test(doc) -> [{userdata,[{""}]}];
sheet1_b34_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",33,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",33,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"????????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"????????\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"AaAaAa"}]),
      test_util:expected2(Msg, {string,"AaAaAa"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"??????"}]),
      test_util:expected2(Msg, {string,"??????"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b29_test(doc) -> [{userdata,[{""}]}];
sheet1_b29_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",28,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",28,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"�?????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"�?????\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b40_test(doc) -> [{userdata,[{""}]}];
sheet1_b40_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",39,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",39,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"??????????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"??????????\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"�"}]),
      test_util:expected2(Msg, {string,"�"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"????????"}]),
      test_util:expected2(Msg, {string,"????????"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b35_test(doc) -> [{userdata,[{""}]}];
sheet1_b35_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",34,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",34,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"-��?���\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"-��?���\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"�?????"}]),
      test_util:expected2(Msg, {string,"�?????"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"??????????"}]),
      test_util:expected2(Msg, {string,"??????????"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b30_test(doc) -> [{userdata,[{""}]}];
sheet1_b30_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",29,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",29,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"a�b?c?d�e�f?g?h?i?\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"a�b?c?d�e�f?g?h?i?\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b41_test(doc) -> [{userdata,[{""}]}];
sheet1_b41_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",40,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",40,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"????\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"kfdks45678dk�sfjk"}]),
      test_util:expected2(Msg, {string,"kfdks45678dk�sfjk"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"-��?���"}]),
      test_util:expected2(Msg, {string,"-��?���"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b25_test(doc) -> [{userdata,[{""}]}];
sheet1_b25_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",24,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",24,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"�\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"�\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b36_test(doc) -> [{userdata,[{""}]}];
sheet1_b36_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",35,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",35,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"�456789?????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"�456789?????\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b9_test(doc) -> [{userdata,[{""}]}];
sheet1_b9_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",8,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",8,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"a�b?c?d�e�f?g?h?i?"}]),
      test_util:expected2(Msg, {string,"a�b?c?d�e�f?g?h?i?"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"????"}]),
      test_util:expected2(Msg, {string,"????"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b31_test(doc) -> [{userdata,[{""}]}];
sheet1_b31_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",30,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",30,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"a`b�c^d~�e?f?��g?h?\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"a`b�c^d~�e?f?��g?h?\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b42_test(doc) -> [{userdata,[{""}]}];
sheet1_b42_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",41,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",41,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"?\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"?\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"�456789?????"}]),
      test_util:expected2(Msg, {string,"�456789?????"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b26_test(doc) -> [{userdata,[{""}]}];
sheet1_b26_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",25,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",25,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"kfdks45678dk�sfjk\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"kfdks45678dk�sfjk\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b37_test(doc) -> [{userdata,[{""}]}];
sheet1_b37_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",36,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",36,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"�\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"�\")"})
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
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"a`b�c^d~�e?f?��g?h?"}]),
      test_util:expected2(Msg, {string,"a`b�c^d~�e?f?��g?h?"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b21_test(doc) -> [{userdata,[{""}]}];
sheet1_b21_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",20,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",20,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{string,"?"}]),
      test_util:expected2(Msg, {string,"?"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b32_test(doc) -> [{userdata,[{""}]}];
sheet1_b32_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",31,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",31,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"??;???????G?\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"??;???????G?\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
sheet1_b43_test(doc) -> [{userdata,[{""}]}];
sheet1_b43_test(Config) -> 
  {value,{_,Pid}}=lists:keysearch(?MODULE,1,Config),
  io:format("in test case Pid is ~p MODULE is ~p~n Key is ~p",[Pid,?MODULE,{"Sheet1",42,1}]),
  Pid ! {msg,self(),?MODULE,{"Sheet1",42,1}},
  receive
    Msg -> 
      io:format("Expected is :~p~nGot is      :~p~n",[Msg,{formula,"=CLEAN(\"?????\")"}]),
      test_util:expected2(Msg, {formula,"=CLEAN(\"?????\")"})
  after
    500 -> io:format("timed out in test case!~n"),
            exit("die in flames!")
  end.
  
all() -> 
    [sheet1_b16_test,
   sheet1_b27_test,
   sheet1_b38_test,
   sheet1_b11_test,
   sheet1_b22_test,
   sheet1_b33_test,
   sheet1_b6_test,
   sheet1_b17_test,
   sheet1_b28_test,
   sheet1_b39_test,
   sheet1_b12_test,
   sheet1_b34_test,
   sheet1_b7_test,
   sheet1_b18_test,
   sheet1_b29_test,
   sheet1_b40_test,
   sheet1_b2_test,
   sheet1_b13_test,
   sheet1_b35_test,
   sheet1_b8_test,
   sheet1_b19_test,
   sheet1_b30_test,
   sheet1_b41_test,
   sheet1_b3_test,
   sheet1_b14_test,
   sheet1_b25_test,
   sheet1_b36_test,
   sheet1_b9_test,
   sheet1_b20_test,
   sheet1_b31_test,
   sheet1_b42_test,
   sheet1_b15_test,
   sheet1_b26_test,
   sheet1_b37_test,
   sheet1_b10_test,
   sheet1_b21_test,
   sheet1_b32_test,
   sheet1_b43_test
    ].
  
