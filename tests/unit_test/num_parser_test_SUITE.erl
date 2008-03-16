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
-define(INPUT1,33,"0").
-define(INPUT2,33,"0.00").
-define(INPUT3,33,"#,##0").
-define(INPUT4,33,"#,##0.00").
-define(INPUT5,33,"#,##0;-#,##0").
-define(INPUT6,33,"#,##0;[Red]-#,##0").
-define(INPUT7,33,"#,##0.00;-#,##0.00").
-define(INPUT8,33,"#,##0.00;[Red]-#,##0.00").
-define(INPUT9,33,"£#,##0;-£#,##0").
-define(INPUT10,33,"£#,##0;[Red]-£#,##0").
-define(INPUT11,33,"£#,##0.00;-£#,##0.00").
-define(INPUT12,33,"£#,##0.00;[Red]-£#,##0.00").
-define(INPUT13,33,"0%").
-define(INPUT14,33,"0.00%").
-define(INPUT15,33,"0.00E+00").
-define(INPUT16,33,"0.00E+00").
-define(INPUT17,33,"# ?/?").
-define(INPUT18,33,"# ??/??").
-define(INPUT19,33,"dd/mm/yyyy").
-define(INPUT20,33,"dd-mmm-yy").
-define(INPUT21,33,"dd-mmm").
-define(INPUT22,33,"mmm-yy").
-define(INPUT23,33,"h:mm AM/PM").
-define(INPUT24,33,"h:mm:ss AM/PM").
-define(INPUT25,33,"hh:mm").
-define(INPUT26,33,"hh:mm:ss").
-define(INPUT27,33,"dd/mm/yyyy hh:mm").
-define(INPUT28,33,"mm:ss").
-define(INPUT29,33,"mm:ss.0").
-define(INPUT30,33,"@").
-define(INPUT31,33,"[h]:mm:ss").
-define(INPUT32,33,"_-£* #,##0_-;-£* #,##0_-;_-£* \"-\"_-;_-@_-").
-define(INPUT33,33,"_-* #,##0_-;-* #,##0_-;_-* \"-\"_-;_-@_-").
-define(INPUT34,33,"_-£* #,##0.00_-;-£* #,##0.00_-;_-£* \"-\"??_-;_-@_-").
-define(INPUT35,33,"_-* #,##0.00_-;-* #,##0.00_-;_-* \"-\"??_-;_-@_-").


-define(INPUT36,36,"[>100][ReD]_-£* #,##0_-;[>=100][BLue]-£* #,##0_-;[=>100][[GreEn]_-£* \"-\"_-;_-@_-").
-define(INPUT37,37,"[<=100]_-* #,##0_-;[=<100]-* #,##0_-;[=100]_-* \"-\"_-;_-@_-").
-define(INPUT38,38,"[>100.0][ReD]_-£* #,##0_-;[>=100.0][BLue]-£* #,##0_-;[=>100.0][[GreEn]_-£* \"-\"_-;_-@_-").
-define(INPUT39,39,"[<=100.0]_-* #,##0_-;[=<100.0]-* #,##0_-;[=100.0]_-* \"-\"_-;_-@_-").

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
     num_parser_test1,
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
     num_parser_test39
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
num_parser_test1() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1(suite) -> 
    [];

num_parser_test1(Config) when is_list(Config) -> 
    Output=format:format(?INPUT1).

num_parser_test2() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2(suite) -> 
    [];

num_parser_test2(Config) when is_list(Config) -> 
    Output=format:format(?INPUT2).

num_parser_test3() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3(suite) -> 
    [];

num_parser_test3(Config) when is_list(Config) -> 
    Output=format:format(?INPUT3).

num_parser_test4() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test4(suite) -> 
    [];

num_parser_test4(Config) when is_list(Config) -> 
    Output=format:format(?INPUT4).

num_parser_test5() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test5(suite) -> 
    [];

num_parser_test5(Config) when is_list(Config) -> 
    Output=format:format(?INPUT5).

num_parser_test6() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test6(suite) -> 
    [];

num_parser_test6(Config) when is_list(Config) -> 
    Output=format:format(?INPUT6).

num_parser_test7() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test7(suite) -> 
    [];

num_parser_test7(Config) when is_list(Config) -> 
    Output=format:format(?INPUT7).

num_parser_test8() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test8(suite) -> 
    [];

num_parser_test8(Config) when is_list(Config) -> 
    Output=format:format(?INPUT8).

num_parser_test9() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test9(suite) -> 
    [];

num_parser_test9(Config) when is_list(Config) -> 
    Output=format:format(?INPUT9).

num_parser_test10() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test10(suite) -> 
    [];

num_parser_test10(Config) when is_list(Config) -> 
    Output=format:format(?INPUT10).

num_parser_test11() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test11(suite) -> 
    [];

num_parser_test11(Config) when is_list(Config) -> 
    Output=format:format(?INPUT11).

num_parser_test12() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test12(suite) -> 
    [];

num_parser_test12(Config) when is_list(Config) -> 
    Output=format:format(?INPUT12).

num_parser_test13() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test13(suite) -> 
    [];

num_parser_test13(Config) when is_list(Config) -> 
    Output=format:format(?INPUT13).

num_parser_test14() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test14(suite) -> 
    [];

num_parser_test14(Config) when is_list(Config) -> 
    Output=format:format(?INPUT14).

num_parser_test15() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test15(suite) -> 
    [];

num_parser_test15(Config) when is_list(Config) -> 
    Output=format:format(?INPUT15).

num_parser_test16() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test16(suite) -> 
    [];

num_parser_test16(Config) when is_list(Config) -> 
    Output=format:format(?INPUT16).

num_parser_test17() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test17(suite) -> 
    [];

num_parser_test17(Config) when is_list(Config) -> 
    Output=format:format(?INPUT17).

num_parser_test18() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test18(suite) -> 
    [];

num_parser_test18(Config) when is_list(Config) -> 
    Output=format:format(?INPUT18).

num_parser_test19() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test19(suite) -> 
    [];

num_parser_test19(Config) when is_list(Config) -> 
    Output=format:format(?INPUT19).

num_parser_test20() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test20(suite) -> 
    [];

num_parser_test20(Config) when is_list(Config) -> 
    Output=format:format(?INPUT20).

num_parser_test21() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test21(suite) -> 
    [];

num_parser_test21(Config) when is_list(Config) -> 
    Output=format:format(?INPUT21).

num_parser_test22() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test22(suite) -> 
    [];

num_parser_test22(Config) when is_list(Config) -> 
    Output=format:format(?INPUT22).

num_parser_test23() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23(suite) -> 
    [];

num_parser_test23(Config) when is_list(Config) -> 
    Output=format:format(?INPUT23).

num_parser_test24() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24(suite) -> 
    [];

num_parser_test24(Config) when is_list(Config) -> 
    Output=format:format(?INPUT24).

num_parser_test25() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25(suite) -> 
    [];

num_parser_test25(Config) when is_list(Config) -> 
    Output=format:format(?INPUT25).

num_parser_test26() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26(suite) -> 
    [];

num_parser_test26(Config) when is_list(Config) -> 
    Output=format:format(?INPUT26).

num_parser_test27() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27(suite) -> 
    [];

num_parser_test27(Config) when is_list(Config) -> 
    Output=format:format(?INPUT27).

num_parser_test28() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28(suite) -> 
    [];

num_parser_test28(Config) when is_list(Config) -> 
    Output=format:format(?INPUT28).

num_parser_test29() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29(suite) -> 
    [];

num_parser_test29(Config) when is_list(Config) -> 
    Output=format:format(?INPUT29).

num_parser_test30() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test30(suite) -> 
    [];

num_parser_test30(Config) when is_list(Config) -> 
    Output=format:format(?INPUT30).

num_parser_test31() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31(suite) -> 
    [];

num_parser_test31(Config) when is_list(Config) -> 
    Output=format:format(?INPUT31).

num_parser_test32() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32(suite) -> 
    [];

num_parser_test32(Config) when is_list(Config) -> 
    Output=format:format(?INPUT32).

num_parser_test33() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33(suite) -> 
    [];

num_parser_test33(Config) when is_list(Config) -> 
    Output=format:format(?INPUT33).

num_parser_test34() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test34(suite) -> 
    [];

num_parser_test34(Config) when is_list(Config) -> 
    Output=format:format(?INPUT34).

num_parser_test35() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test35(suite) -> 
    [];

num_parser_test35(Config) when is_list(Config) -> 
    Output=format:format(?INPUT35).

num_parser_test36() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test36(suite) -> 
    [];

num_parser_test36(Config) when is_list(Config) -> 
    Output=format:format(?INPUT36).

num_parser_test37() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test37(suite) -> 
    [];

num_parser_test37(Config) when is_list(Config) -> 
    Output=format:format(?INPUT37).

num_parser_test38() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test38(suite) -> 
    [];

num_parser_test38(Config) when is_list(Config) -> 
    Output=format:format(?INPUT38).

num_parser_test39() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test39(suite) -> 
    [];

num_parser_test39(Config) when is_list(Config) -> 
    Output=format:format(?INPUT39).

