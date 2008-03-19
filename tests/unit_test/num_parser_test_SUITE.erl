%%%-----------------------------------------------------------------------------
%%% File        : num_parser_test_SUITE.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests the parser for the numerical format
%%%
%%%
%%% Created     : 10 Mar 2008 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(num_parser_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

%% Tests 1 to 40 all come from the default custom formats in Excel
%% all tests furth of 40 are bespoke tests
%% all tests are generated from the Excel spreadsheet num_parser_master.xls
%% which is in Subversion alongside this test suite

%% Tests 1
-define(FORMAT1,"0").

-define(VALUE1a,33).
-define(OUTPUT1a,{black,"33"}).

-define(VALUE1b,-33).
-define(OUTPUT1b,{black,"-33"}).

-define(VALUE1c,33.333).
-define(OUTPUT1c,{black,"33"}).

-define(VALUE1d,-33.33).
-define(OUTPUT1d,{black,"-33"}).

-define(VALUE1e,0).
-define(OUTPUT1e,{black,"0"}).

-define(VALUE1f,0.0).
-define(OUTPUT1f,{black,"0"}).

-define(VALUE1g,"bob").
-define(OUTPUT1g,{black,"bob"}).

-define(VALUE1h,0.0003333).
-define(OUTPUT1h,{black,"0"}).

-define(VALUE1i,-0.000033).
-define(OUTPUT1i,{black,"0"}).

-define(VALUE1j,0.000000000000000000000000000003333).
-define(OUTPUT1j,{black,"0"}).

-define(VALUE1k,-0.00000000000000000000000000000033).
-define(OUTPUT1k,{black,"0"}).

-define(VALUE1l,33330000000000000000000000000000000000000000000).
-define(OUTPUT1l,{black,"33330000000000000000000000000000000000000000000"}).

-define(VALUE1m,-33330000000000000000000000000000000000000000000).
-define(OUTPUT1m,{black,"-33330000000000000000000000000000000000000000000"}).

%% Test 2
-define(FORMAT2,"0.00").
-define(VALUE2a,33).
-define(OUTPUT2a,{black,"33.00"}).

-define(VALUE2b,-33).
-define(OUTPUT2b,{black,"-33.00"}).

-define(VALUE2c,33.333).
-define(OUTPUT2c,{black,"33.33"}).

-define(VALUE2d,-33.33).
-define(OUTPUT2d,{black,"-33.33"}).

-define(VALUE2e,0).
-define(OUTPUT2e,{black,"0.00"}).

-define(VALUE2f,0.0).
-define(OUTPUT2f,{black,"0.00"}).

-define(VALUE2g,"bob").
-define(OUTPUT2g,{black,"bob"}).

-define(VALUE2h,0.0003333).
-define(OUTPUT2h,{black,"0.00"}).

-define(VALUE2i,-0.000033).
-define(OUTPUT2i,{black,"0.00"}).

-define(VALUE2j,0.000000000000000000000000000003333).
-define(OUTPUT2j,{black,"0.00"}).

-define(VALUE2k,-0.00000000000000000000000000000033).
-define(OUTPUT2k,{black,"0.00"}).

-define(VALUE2l,33330000000000000000000000000000000000000000000).
-define(OUTPUT2l,{black,"33330000000000000000000000000000000000000000000.00"}).

-define(VALUE2m,-33330000000000000000000000000000000000000000000).
-define(OUTPUT2m,{black,"-33330000000000000000000000000000000000000000000.00"}).

%% Test 3
-define(FORMAT3,"#,##0").
-define(FORMAT4,"#,##0.00").
-define(FORMAT5,"#,##0;-#,##0").
-define(FORMAT6,"#,##0;[Red]-#,##0").
-define(FORMAT7,"#,##0.00;-#,##0.00").
-define(FORMAT8,"#,##0.00;[Red]-#,##0.00").
-define(FORMAT9,"£#,##0;-£#,##0").
-define(FORMAT10,"£#,##0;[Red]-£#,##0").
-define(FORMAT11,"£#,##0.00;-£#,##0.00").
-define(FORMAT12,"£#,##0.00;[Red]-£#,##0.00").
-define(FORMAT13,"0%").
-define(FORMAT14,"0.00%").
-define(FORMAT15,"0.00E+00").
-define(FORMAT16,"0.00E+00").
-define(FORMAT17,"# ?/?").
-define(FORMAT18,"# ??/??").
-define(FORMAT19,"dd/mm/yyyy").
-define(FORMAT20,"dd-mmm-yy").
-define(FORMAT21,"dd-mmm").
-define(FORMAT22,"mmm-yy").
-define(FORMAT23,"h:mm AM/PM").
-define(FORMAT24,"h:mm:ss AM/PM").
-define(FORMAT25,"hh:mm").
-define(FORMAT26,"hh:mm:ss").
-define(FORMAT27,"dd/mm/yyyy hh:mm").
-define(FORMAT28,"mm:ss").
-define(FORMAT29,"mm:ss.0").
-define(FORMAT30,"@").
-define(FORMAT31,"[h]:mm:ss").
-define(FORMAT32,"_-£* #,##0_-;-£* #,##0_-;_-£* \"-\"_-;_-@_-").
-define(FORMAT33,"_-* #,##0_-;-* #,##0_-;_-* \"-\"_-;_-@_-").
-define(FORMAT34,"_-£* #,##0.00_-;-£* #,##0.00_-;_-£* \"-\"??_-;_-@_-").
-define(FORMAT35,"_-* #,##0.00_-;-* #,##0.00_-;_-* \"-\"??_-;_-@_-").
-define(FORMAT36,"[>100][ReD]_-£* #,##0_-;[>=100][BLue]-£* #,##0_-;[>=100][GreEn]_-£* \"-\"_-;_-@_-").
-define(FORMAT37,"[=<100]_-* #,##0_-;[=<100]-* #,##0_-;[=100]_-* \"-\"_-;_-@_-").
-define(FORMAT38,"[>100.0][ReD]_-£* #,##0_-;[>=100.0][BLue]-£* #,##0_-;[>=100.0][GreEn]_-£* \"-\"_-;_-@_-").
-define(FORMAT39,"[=<100.0]_-* #,##0_-;[=<100.0]-* #,##0_-;[=100.0]_-* \"-\"_-;_-@_-").
-define(FORMAT40,"0;;").

%% Now some more tests
-define(FORMAT41,"0000").

-define(VALUE41a,33).
-define(OUTPUT41a,{black,"0033"}).

-define(VALUE41b,33.00).
-define(OUTPUT41b,{black,"0033"}).

-define(VALUE41c,-33).
-define(OUTPUT41c,{black,"-0033"}).

-define(VALUE41d,-33.00).
-define(OUTPUT41d,{black,"-0033"}).

%% Test server callback functions
%%------------------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:setup_paths(),
    Config.

%%------------------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initiation before each test case
%%------------------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%------------------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%------------------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%------------------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%------------------------------------------------------------------------------
all() -> 
    [
     num_parser_test1a,
     num_parser_test1b,
     num_parser_test1c,
     num_parser_test1d,
     num_parser_test1e,
     num_parser_test1f,
     num_parser_test1g,
     num_parser_test1h,
     num_parser_test1i,
     num_parser_test1j,
     num_parser_test1k,
     num_parser_test1l,
     num_parser_test1m,
     num_parser_test2a,
     num_parser_test2b,
     num_parser_test2c,
     num_parser_test2d,
     num_parser_test2e,
     num_parser_test2f,
     num_parser_test2g,
     num_parser_test2h,
     num_parser_test2i,
     num_parser_test2j,
     num_parser_test2k,
     num_parser_test2l,
     num_parser_test2m,
     num_parser_test3,
     num_parser_test4,
     num_parser_test5,
     num_parser_test6,
     num_parser_test7,
     num_parser_test8,
     num_parser_test9,
     num_parser_test10,
     num_parser_test11,
     num_parser_test12,
     num_parser_test13,
     num_parser_test14,
     num_parser_test15,
     num_parser_test16,
     num_parser_test17,
     num_parser_test18,
     num_parser_test19,
     num_parser_test20,
     num_parser_test21,
     num_parser_test22,
     num_parser_test23,
     num_parser_test24,
     num_parser_test25,
     num_parser_test26,
     num_parser_test27,
     num_parser_test28,
     num_parser_test29,
     num_parser_test30,
     num_parser_test31,
     num_parser_test32,
     num_parser_test33,
     num_parser_test34,
     num_parser_test35,
     num_parser_test36,
     num_parser_test37,
     num_parser_test38,
     num_parser_test39,
     num_parser_test40,
     num_parser_test41a,
     num_parser_test41b,
     num_parser_test41c,
     num_parser_test41d
    ].
    
%% Case executor
executor(Format,Value,Expected)->
    {erlang,Output}=format:compile_format(Format),
    Got=format:run_format(Value,Output),
    test_util:expected(Expected,Got).

%% Test cases starts here.
%%------------------------------------------------------------------------------
num_parser_test1a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1a(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1a,?OUTPUT1a).

num_parser_test1b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1b(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1b,?OUTPUT1b).

num_parser_test1c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1c(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1c,?OUTPUT1c).

num_parser_test1d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1d(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1d,?OUTPUT1d).

num_parser_test1e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1e(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1e,?OUTPUT1e).

num_parser_test1f() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1f(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1f,?OUTPUT1f).

num_parser_test1g() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1g(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1g,?OUTPUT1g).

num_parser_test1h() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1h(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1h,?OUTPUT1h).

num_parser_test1i() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1i(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1i,?OUTPUT1i).

num_parser_test1j() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1j(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1j,?OUTPUT1j).

num_parser_test1k() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1k(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1k,?OUTPUT1k).

num_parser_test1l() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1l(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1l,?OUTPUT1l).

num_parser_test1m() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1m(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1m,?OUTPUT1m).

num_parser_test2a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2a(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2a,?OUTPUT2a).

num_parser_test2b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2b(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2b,?OUTPUT2b).

num_parser_test2c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2c(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2c,?OUTPUT2c).

num_parser_test2d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2d(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2d,?OUTPUT2d).

num_parser_test2e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2e(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2e,?OUTPUT2e).

num_parser_test2f() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2f(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2f,?OUTPUT2f).

num_parser_test2g() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2g(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2g,?OUTPUT2g).

num_parser_test2h() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2h(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2h,?OUTPUT2h).

num_parser_test2i() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2i(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2i,?OUTPUT2i).

num_parser_test2j() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2j(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2j,?OUTPUT2j).

num_parser_test2k() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2k(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2k,?OUTPUT2k).

num_parser_test2l() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2l(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2l,?OUTPUT2l).

num_parser_test2m() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2m(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2m,?OUTPUT2m).

num_parser_test3() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT3).

num_parser_test4() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test4(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT4).

num_parser_test5() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test5(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT5).

num_parser_test6() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test6(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT6).

num_parser_test7() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test7(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT7).

num_parser_test8() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test8(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT8).

num_parser_test9() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test9(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT9).

num_parser_test10() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test10(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT10).

num_parser_test11() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test11(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT11).

num_parser_test12() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test12(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT12).

num_parser_test13() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test13(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT13).

num_parser_test14() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test14(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT14).

num_parser_test15() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test15(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT15).

num_parser_test16() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test16(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT16).

num_parser_test17() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test17(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT17).

num_parser_test18() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test18(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT18).

num_parser_test19() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test19(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT19).

num_parser_test20() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test20(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT20).

num_parser_test21() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test21(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT21).

num_parser_test22() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test22(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT22).

num_parser_test23() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT23).

num_parser_test24() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT24).

num_parser_test25() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT25).

num_parser_test26() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT26).

num_parser_test27() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT27).

num_parser_test28() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT28).

num_parser_test29() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT29).

num_parser_test30() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test30(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT30).

num_parser_test31() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT31).

num_parser_test32() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT32).

num_parser_test33() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT33).

num_parser_test34() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test34(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT34).

num_parser_test35() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test35(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT35).

num_parser_test36() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test36(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT36).

num_parser_test37() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test37(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT37).

num_parser_test38() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test38(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT38).

num_parser_test39() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test39(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT39).

num_parser_test40(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?FORMAT40).

num_parser_test41a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41a(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41a,?OUTPUT41a).

num_parser_test41b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41b(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41b,?OUTPUT41b).

num_parser_test41c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41c(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41c,?OUTPUT41c).

num_parser_test41d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41d(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41d,?OUTPUT41d).

