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

%% The first 35 functions should all pass the formatting
%% Tests 1
-define(INPUT1,"0").

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
-define(INPUT2,"0.00").
-define(INPUT3,"#,##0").
-define(INPUT4,"#,##0.00").
-define(INPUT5,"#,##0;-#,##0").
-define(INPUT6,"#,##0;[Red]-#,##0").
-define(INPUT7,"#,##0.00;-#,##0.00").
-define(INPUT8,"#,##0.00;[Red]-#,##0.00").
-define(INPUT9,"£#,##0;-£#,##0").
-define(INPUT10,"£#,##0;[Red]-£#,##0").
-define(INPUT11,"£#,##0.00;-£#,##0.00").
-define(INPUT12,"£#,##0.00;[Red]-£#,##0.00").
-define(INPUT13,"0%").
-define(INPUT14,"0.00%").
-define(INPUT15,"0.00E+00").
-define(INPUT16,"0.00E+00").
-define(INPUT17,"# ?/?").
-define(INPUT18,"# ??/??").
-define(INPUT19,"dd/mm/yyyy").
-define(INPUT20,"dd-mmm-yy").
-define(INPUT21,"dd-mmm").
-define(INPUT22,"mmm-yy").
-define(INPUT23,"h:mm AM/PM").
-define(INPUT24,"h:mm:ss AM/PM").
-define(INPUT25,"hh:mm").
-define(INPUT26,"hh:mm:ss").
-define(INPUT27,"dd/mm/yyyy hh:mm").
-define(INPUT28,"mm:ss").
-define(INPUT29,"mm:ss.0").
-define(INPUT30,"@").
-define(INPUT31,"[h]:mm:ss").
-define(INPUT32,"_-£* #,##0_-;-£* #,##0_-;_-£* \"-\"_-;_-@_-").
-define(INPUT33,"_-* #,##0_-;-* #,##0_-;_-* \"-\"_-;_-@_-").
-define(INPUT34,"_-£* #,##0.00_-;-£* #,##0.00_-;_-£* \"-\"??_-;_-@_-").
-define(INPUT35,"_-* #,##0.00_-;-* #,##0.00_-;_-* \"-\"??_-;_-@_-").


-define(INPUT36,"[>100][ReD]_-£* #,##0_-;[>=100][BLue]-£* #,##0_-;[>=100][GreEn]_-£* \"-\"_-;_-@_-").
-define(INPUT37,"[=<100]_-* #,##0_-;[=<100]-* #,##0_-;[=100]_-* \"-\"_-;_-@_-").
-define(INPUT38,"[>100.0][ReD]_-£* #,##0_-;[>=100.0][BLue]-£* #,##0_-;[>=100.0][GreEn]_-£* \"-\"_-;_-@_-").
-define(INPUT39,"[=<100.0]_-* #,##0_-;[=<100.0]-* #,##0_-;[=100.0]_-* \"-\"_-;_-@_-").
-define(INPUT40,"0;;").

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
     num_parser_test2,
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
     num_parser_test40
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
num_parser_test1a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1a(suite) -> 
    [];

num_parser_test1a(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1a,Output),
    test_util:expected(?OUTPUT1a,Got).

num_parser_test1b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1b(suite) -> 
    [];

num_parser_test1b(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1b,Output),
    test_util:expected(?OUTPUT1b,Got).

num_parser_test1c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1c(suite) -> 
    [];

num_parser_test1c(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1c,Output),
    test_util:expected(?OUTPUT1c,Got).

num_parser_test1d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1d(suite) -> 
    [];

num_parser_test1d(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1d,Output),
    test_util:expected(?OUTPUT1d,Got).

num_parser_test1e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1e(suite) -> 
    [];

num_parser_test1e(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1e,Output),
    test_util:expected(?OUTPUT1e,Got).

num_parser_test1f() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1f(suite) -> 
    [];

num_parser_test1f(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1f,Output),
    test_util:expected(?OUTPUT1f,Got).

num_parser_test1g() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1g(suite) -> 
    [];

num_parser_test1g(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1g,Output),
    test_util:expected(?OUTPUT1g,Got).

num_parser_test1h() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1h(suite) -> 
    [];

num_parser_test1h(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1h,Output),
    test_util:expected(?OUTPUT1h,Got).

num_parser_test1i() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1i(suite) -> 
    [];

num_parser_test1i(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1i,Output),
    test_util:expected(?OUTPUT1i,Got).

num_parser_test1j() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1j(suite) -> 
    [];

num_parser_test1j(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1j,Output),
    test_util:expected(?OUTPUT1j,Got).

num_parser_test1k() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1k(suite) -> 
    [];

num_parser_test1k(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1k,Output),
    test_util:expected(?OUTPUT1k,Got).

num_parser_test1l() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1l(suite) -> 
    [];

num_parser_test1l(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1l,Output),
    test_util:expected(?OUTPUT1l,Got).

num_parser_test1m() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1m(suite) -> 
    [];

num_parser_test1m(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT1),
    Got=format:run_format(?VALUE1m,Output),
    test_util:expected(?OUTPUT1m,Got).

num_parser_test2() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2(suite) -> 
    [];

num_parser_test2(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT2).

num_parser_test3() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3(suite) -> 
    [];

num_parser_test3(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT3).

num_parser_test4() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test4(suite) -> 
    [];

num_parser_test4(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT4).

num_parser_test5() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test5(suite) -> 
    [];

num_parser_test5(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT5).

num_parser_test6() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test6(suite) -> 
    [];

num_parser_test6(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT6).

num_parser_test7() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test7(suite) -> 
    [];

num_parser_test7(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT7).

num_parser_test8() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test8(suite) -> 
    [];

num_parser_test8(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT8).

num_parser_test9() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test9(suite) -> 
    [];

num_parser_test9(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT9).

num_parser_test10() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test10(suite) -> 
    [];

num_parser_test10(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT10).

num_parser_test11() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test11(suite) -> 
    [];

num_parser_test11(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT11).

num_parser_test12() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test12(suite) -> 
    [];

num_parser_test12(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT12).

num_parser_test13() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test13(suite) -> 
    [];

num_parser_test13(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT13).

num_parser_test14() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test14(suite) -> 
    [];

num_parser_test14(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT14).

num_parser_test15() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test15(suite) -> 
    [];

num_parser_test15(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT15).

num_parser_test16() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test16(suite) -> 
    [];

num_parser_test16(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT16).

num_parser_test17() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test17(suite) -> 
    [];

num_parser_test17(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT17).

num_parser_test18() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test18(suite) -> 
    [];

num_parser_test18(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT18).

num_parser_test19() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test19(suite) -> 
    [];

num_parser_test19(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT19).

num_parser_test20() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test20(suite) -> 
    [];

num_parser_test20(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT20).

num_parser_test21() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test21(suite) -> 
    [];

num_parser_test21(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT21).

num_parser_test22() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test22(suite) -> 
    [];

num_parser_test22(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT22).

num_parser_test23() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23(suite) -> 
    [];

num_parser_test23(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT23).

num_parser_test24() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24(suite) -> 
    [];

num_parser_test24(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT24).

num_parser_test25() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25(suite) -> 
    [];

num_parser_test25(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT25).

num_parser_test26() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26(suite) -> 
    [];

num_parser_test26(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT26).

num_parser_test27() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27(suite) -> 
    [];

num_parser_test27(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT27).

num_parser_test28() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28(suite) -> 
    [];

num_parser_test28(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT28).

num_parser_test29() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29(suite) -> 
    [];

num_parser_test29(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT29).

num_parser_test30() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test30(suite) -> 
    [];

num_parser_test30(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT30).

num_parser_test31() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31(suite) -> 
    [];

num_parser_test31(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT31).

num_parser_test32() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32(suite) -> 
    [];

num_parser_test32(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT32).

num_parser_test33() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33(suite) -> 
    [];

num_parser_test33(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT33).

num_parser_test34() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test34(suite) -> 
    [];

num_parser_test34(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT34).

num_parser_test35() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test35(suite) -> 
    [];

num_parser_test35(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT35).

num_parser_test36() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test36(suite) -> 
    [];

num_parser_test36(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT36).

num_parser_test37() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test37(suite) -> 
    [];

num_parser_test37(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT37).

num_parser_test38() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test38(suite) -> 
    [];

num_parser_test38(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT38).

num_parser_test39() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test39(suite) -> 
    [];

num_parser_test39(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT39).

num_parser_test40(suite) -> 
    [];

num_parser_test40(Config) when is_list(Config) -> 
    {erlang,Output}=format:compile_format(?INPUT40).
