%%%-----------------------------------------------------------------------------
%%% File        : num_parser_test_SUITE.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests the parser for the numerical format
%%%
%%% Created     : 13 Oct 2007 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(num_lexer_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

%% Test 1
-define(INPUT1, "[BLACK]").
-define(OUTPUT1,[{colour,black}]).

%% Test 2
-define(INPUT2, "[BLUE]").
-define(OUTPUT2,[{colour,blue}]).

%% Test 3
-define(INPUT3, "[CYAN]").
-define(OUTPUT3,[{colour,cyan}]).

%% Test 4
-define(INPUT4, "[GREEN]").
-define(OUTPUT4,[{colour,green}]).

%% Test 5
-define(INPUT5, "[MAGENTA]").
-define(OUTPUT5,[{colour,magenta}]).

%% Test 6
-define(INPUT6, "[RED]").
-define(OUTPUT6,[{colour,red}]).

%% Test 7
-define(INPUT7, "[WHITE]").
-define(OUTPUT7,[{colour,white}]).

%% Test 8
-define(INPUT8, "[YELLOW]").
-define(OUTPUT8,[{colour,yellow}]).

%% Test 9
-define(INPUT9, "yy;yyyy").
-define(OUTPUT9,[{year,two_digit},{duff},{year,four_digit}]).

%% Test 10
-define(INPUT10, "m;mm").
-define(OUTPUT10,[{mon_min,no_zero},{duff},{mon_min,zero}]).

%% Test 11
-define(INPUT11, "mmm;mmmm").
-define(OUTPUT11,[{mon,abbr},{duff},{mon,full}]).

%% Test 12
-define(INPUT12, "d;dd").
-define(OUTPUT12,[{day,no_zero},{duff},{day,zero}]).

%% Test 13
-define(INPUT13, "ddd;dddd").
-define(OUTPUT13,[{day,abbr},{duff},{day,full}]).

%% Test 14
-define(INPUT14, "h;hh").
-define(OUTPUT14,[{hour,no_zero},{duff},{hour,zero}]).

%% Test 15
-define(INPUT15, "s;ss").
-define(OUTPUT15,[{sec,no_zero},{duff},{sec,zero}]).

%% Test 16
-define(INPUT16, "AM/PM").
-define(OUTPUT16,[{ampm,full_caps}]).

%% Test 17
-define(INPUT17, "am/pm").
-define(OUTPUT17,[{ampm,full_lowercase}]).

%% Test 18
-define(INPUT18, "A/P").
-define(OUTPUT18,[{ampm,abbr_caps}]).

%% Test 19
-define(INPUT19, "a/p").
-define(OUTPUT19,[{ampm,abbr_lowercase}]).

%% Test20
-define(INPUT20, " ").
-define(OUTPUT20,[{space}]).

%% Test21
-define(INPUT21, "#?.%,ee-_$/(): _@").
-define(OUTPUT21,[{hash},
		  {question},
		  {dot},
		  {percent},
		  {comma},
		  {exponent},
		  {exponent},
		  {minus},
		  {underscore},
		  {dollar},
		  {forwardslash},
		  {open_bra},
		  {close_ket},
		  {colon},
		  {space},
		  {underscore},
		  {at}]).

%% Test22
-define(INPUT22, "[<100]").
-define(OUTPUT22,[{open_sq_brackets},
		  {lt},
		  {integer,"100"},
                  {close_sq_brackets}]).

%% Test23
-define(INPUT23, "[<100.0]").
-define(OUTPUT23,[{open_sq_brackets},
		  {lt},
		  {float,"100.0"},
                  {close_sq_brackets}]).

%% Test24
-define(INPUT24, "[<100.0e-10]").
-define(OUTPUT24,[{open_sq_brackets},
		  {lt},
		  {float,"100.0e-10"},
                  {close_sq_brackets}]).

%% Test25
-define(INPUT25, "[<100.0e+10]").
-define(OUTPUT25,[{open_sq_brackets},
		  {lt},
		  {float,"100.0e+10"},
                  {close_sq_brackets}]).

%% Test26
-define(INPUT26, "[<=100]").
-define(OUTPUT26,[{open_sq_brackets},
		  {lt},
                  {eq},
		  {integer,"100"},
                  {close_sq_brackets}]).

%% Test27
-define(INPUT27, "[>100]").
-define(OUTPUT27,[{open_sq_brackets},
		  {gt},
		  {integer,"100"},
                  {close_sq_brackets}]).


%% Test28
-define(INPUT28, "[>=100]").
-define(OUTPUT28,[{open_sq_brackets},
		  {gt},
                  {eq},
		  {integer,"100"},
                  {close_sq_brackets}]).

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
    code:add_patha("/opt/SVN/spriki/trunk/ebin"),
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
    [num_lexer_test1,
     num_lexer_test2,
     num_lexer_test3,
     num_lexer_test4,
     num_lexer_test5,
     num_lexer_test6,
     num_lexer_test7,
     num_lexer_test8,
     num_lexer_test9,
     num_lexer_test10,
     num_lexer_test11,
     num_lexer_test12,
     num_lexer_test13,
     num_lexer_test14,
     num_lexer_test15,
     num_lexer_test16,
     num_lexer_test17,
     num_lexer_test18,
     num_lexer_test19,
     num_lexer_test20,
     num_lexer_test21,
     num_lexer_test22,
     num_lexer_test23,
     num_lexer_test24,
     num_lexer_test25,
     num_lexer_test26,
     num_lexer_test27,
     num_lexer_test28].

%% Test cases starts here.
%%------------------------------------------------------------------------------
num_lexer_test1() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test1(suite) -> 
    [];

num_lexer_test1(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT1),
    test_util:expected(?OUTPUT1,Output).

num_lexer_test2() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test2(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT2),
    test_util:expected(?OUTPUT2,Output).


num_lexer_test3() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test3(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT3),
    test_util:expected(?OUTPUT3,Output).


num_lexer_test4() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test4(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT4),
    test_util:expected(?OUTPUT4,Output).


num_lexer_test5() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test5(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT5),
    test_util:expected(?OUTPUT5,Output).


num_lexer_test6() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test6(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT6),
    test_util:expected(?OUTPUT6,Output).


num_lexer_test7() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test7(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT7),
    test_util:expected(?OUTPUT7,Output).


num_lexer_test8() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test8(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT8),
    test_util:expected(?OUTPUT8,Output).


num_lexer_test9() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test9(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT9),
    test_util:expected(?OUTPUT9,Output).


num_lexer_test10() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test10(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT10),
    test_util:expected(?OUTPUT10,Output).


num_lexer_test11() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test11(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT12),
    test_util:expected(?OUTPUT12,Output).


num_lexer_test12() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test12(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT12),
    test_util:expected(?OUTPUT12,Output).


num_lexer_test13() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test13(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT13),
    test_util:expected(?OUTPUT13,Output).


num_lexer_test14() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test14(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT14),
    test_util:expected(?OUTPUT14,Output).


num_lexer_test15() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test15(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT15),
    test_util:expected(?OUTPUT15,Output).


num_lexer_test16() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test16(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT16),
    test_util:expected(?OUTPUT16,Output).


num_lexer_test17() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test17(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT17),
    test_util:expected(?OUTPUT17,Output).


num_lexer_test18() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test18(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT18),
    test_util:expected(?OUTPUT18,Output).


num_lexer_test19() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test19(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT19),
    test_util:expected(?OUTPUT19,Output).


num_lexer_test20() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test20(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT20),
    test_util:expected(?OUTPUT20,Output).


num_lexer_test21() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test21(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT21),
    test_util:expected(?OUTPUT21,Output).


num_lexer_test22() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test22(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT22),
    test_util:expected(?OUTPUT22,Output).


num_lexer_test23() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test23(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT23),
    test_util:expected(?OUTPUT23,Output).


num_lexer_test24() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test24(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT24),
    test_util:expected(?OUTPUT24,Output).


num_lexer_test25() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test25(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT25),
    test_util:expected(?OUTPUT25,Output).


num_lexer_test26() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test26(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT26),
    test_util:expected(?OUTPUT26,Output).


num_lexer_test27() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test27(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT27),
    test_util:expected(?OUTPUT27,Output).


num_lexer_test28() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_lexer_test28(Config) when is_list(Config) -> 
    {ok,Output,_}=num_format_lexer:string(?INPUT28),
    test_util:expected(?OUTPUT28,Output).
