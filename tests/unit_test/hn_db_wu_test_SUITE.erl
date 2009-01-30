%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       A test suite for writing cell attributes
%%%
%%% @end
%%% Created : 26 Jan 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hn_db_wu_test_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../lib/hypernumbers-1.0/include/spriki.hrl").

-define(SITE1, "http://hypernumbers.dev").

%%
%% Test A
%%
%% This test just tests that when a formula is written it
%% can be read back
%%

-define(PATH_A, ["a"]).

% Ref 1
-define(Ref1,{cell, {1, 1}}).
-define(Val1, "1").
-define(Expected1A, "1").

% Ref 2
-define(Ref2,{cell, {1, 2}}).
-define(Val2, "two").
-define(Expected2A, "two").

% Ref 3
-define(Ref3,{cell, {1, 3}}).
-define(Val3, "'3").
-define(Expected3A, "'3").

% Ref 4
-define(Ref4,{cell, {1, 4}}).
-define(Val4, "=2+2").
-define(Expected4A, "=2+2").

%%
%% Test B
%%  
%% This tests checks that the complete set of attributes
%% are returned when a formula is written
%% 

-define(PATH_B, ["b"]).

-define(Expected1B, [formula, value, rawvalue, 'overwrite-color', style]).
-define(Expected2B, [formula, value, rawvalue, 'overwrite-color', style]).
-define(Expected3B, [formula, value, rawvalue, 'overwrite-color', style]).
-define(Expected4B, [formula, value, rawvalue, 'overwrite-color', style]).

%%
%% Test C
%% 
%% This tests columns and rows stuff
%% 

-define(PATH_C, ["c"]).

-define(InitialiseC,[{{cell, {1, 1}}, "1"},
                     {{cell, {1, 2}}, "two"},
                     {{cell, {2, 1}}, "'3"},
                     {{cell, {2, 2}}, "=2+2"}]).

% Ref1
-define(Ref1C, {column, 1}).
-define(Expected1C, [1, "two"]).

% Ref 2
-define(Ref2C, {row, 2}).
-define(Expected2C, [4, "two"]).

% Ref 3
-define(Ref3C, {range, {1, 1, 2, 2}}).
-define(Expected3C, [1, "two", "3", 4]).

% Ref 4
-define(Ref4C, {page, "/"}).
-define(Expected4C, [1, "two", "3", 4]).

%%
%% Test D
%% 
%% This tests reading lists of attributes
%% 
-define(PATH_D, ["D"]).

-define(InitialiseD,[{{cell, {1, 1}}, "1"},
                     {{cell, {1, 2}}, "two"},
                     {{cell, {2, 1}}, "'3"},
                     {{cell, {2, 2}}, "=2+2"}]).

-define(Ref1D, [{column, 1}, {row, 2}]).
-define(Expected1D, [1, "two", "two", 4]).

%%
%% Test D1
%% 
%% This tests reading lists of attributes
%%
%% like Test D but with a different page and a different read 
-define(Ref1D1, [{page, "/"}]).
-define(Expected1D1, [1, "two", "3", 4]).

%%
%% Test E
%% 
%% This tests writing ranges
%% 
-define(PATH_E, ["E"]).

-define(InitialiseE,{formula, "1"}).
-define(Ref1E, {range, {1, 1, 2, 3}}).
-define(Expected1E, [1, 1, 1, 1, 1, 1]).

%%
%% Test F
%% 
%% This tests deletes
%% 
-define(PATH_F, ["F"]).

-define(InitialiseF,[{{cell, {1, 1}}, "1"},
                     {{cell, {1, 2}}, "two"},
                     {{cell, {1, 3}}, "'3"},
                     {{cell, {1, 4}}, "=2+2"},
                     {{cell, {2, 1}}, "5"},
                     {{cell, {2, 2}}, "6"},
                     {{cell, {2, 3}}, "=14/2"},
                     {{cell, {2, 4}}, "=16/2"},
                     {{cell, {3, 1}}, "9"},
                     {{cell, {3, 2}}, "10"},
                     {{cell, {3, 3}}, "11"},
                     {{cell, {3, 4}}, "12"}
                    ]).
-define(Ref1F, [{cell, {1,1}}, {row, 3}, {column, 3}, {range, {1, 4, 4, 4}}]).
-define(Expected1F, ["two", 5, 6]).

%%
%% Test G
%% 
%% This tests deletes
%% 
-define(PATH_G, ["G"]).

-define(InitialiseG,[{{cell, {1, 1}}, "1"},
                     {{cell, {1, 2}}, "2"},
                     {{cell, {2, 1}}, "5"},
                     {{cell, {2, 2}}, "6"}
                    ]).
-define(Ref1G, [{range, {1, 1, 4, 4}}]).
-define(Expected1G, ['overwrite-color',
                     'overwrite-color',
                     'overwrite-color',
                     'overwrite-color',
                     'style',
                     'style',
                     'style',
                     'style']).

%% Test server callback functions
%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    % First create a new database
    ok = hn_db:create(),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Initiation before each test case
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%%
%% Returns a list of all test cases in this test suite
%%
%% @spec all() -> TestCases
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     test_case1A,
     test_case2A,
     test_case3A,
     test_case4A,
     test_case1B,
     test_case2B,
     test_case3B,
     test_case4B,
     test_case1C,
     test_case2C,
     test_case3C,
     test_case4C,
     test_case1D,
     test_case1D1, % interstitial!
     test_case1E,
     test_case1F,
     test_case1G
    ].

%%
%% Test A
%% 

testA(Ref, Val, Expected) ->
    Ref2 = #refX{site = ?SITE1, path = ?PATH_A, obj = Ref},
    Fun1 = fun() -> hn_db_wu:write_attr(Ref2, {formula, Val}) end,
    mnesia:activity(transaction, Fun1),
    Fun2 = fun() -> hn_db_wu:read_attrs(Ref2, [formula]) end,
    [Got] = mnesia:activity(transaction, Fun2),
    #hn_item{val = GotValue} = Got,
    test2(Expected, GotValue).

%%
%% Test B
%% 

testB(Ref, Val, Expected) ->
    Ref2 = #refX{site = ?SITE1, path = ?PATH_B, obj = Ref},
    Fun1 = fun() -> hn_db_wu:write_attr(Ref2, {formula, Val}) end,
    mnesia:activity(transaction, Fun1),
    Fun2 = fun() -> hn_db_wu:read_cells(Ref2) end,
    Got = mnesia:activity(transaction, Fun2),
    Get2 = extract_names(Got),
    test2(lists:sort(Expected), Get2).

%%
%% Test C
%% 

testC(Initialise, Ref, Expected) ->
    write(Initialise, ?PATH_C),
    Ref2 = #refX{site = ?SITE1, path = ?PATH_C, obj = Ref},
    Fun2 = fun() -> hn_db_wu:read_attrs(Ref2) end,
    Got = mnesia:activity(transaction, Fun2),
    io:format("in testC Got is ~p~n", [Got]),
    Get2 = extract_values(Got),
    test2(lists:sort(Expected), Get2).

%%
%% Test D
%% 

testD(Initialise, Ref, Expected) ->
    write(Initialise, ?PATH_D),
    RefList = make_list(Ref, ?PATH_D),
    Fun2 = fun() ->
                   [hn_db_wu:read_attrs(X) || X <- RefList]
           end,
    Got = lists:flatten(mnesia:activity(transaction, Fun2)),
    io:format("in testD Got is ~p~n", [Got]),
    Get2 = extract_values(Got),
    test2(lists:sort(Expected), Get2).

%%
%% Test E
%% 

testE(Initialise, Ref, Expected) ->
    Ref2= #refX{site = ?SITE1, path = ?PATH_E, obj = Ref},
    Fun1 = fun() -> hn_db_wu:write_attr(Ref2, Initialise) end,
    mnesia:activity(transaction, Fun1),          
    Fun2 = fun() -> hn_db_wu:read_attrs(Ref2) end,
    Got = mnesia:activity(transaction, Fun2),
    io:format("in testD Got is ~p~n", [Got]),
    Get2 = extract_values(Got),
    test2(lists:sort(Expected), Get2).

%%
%% Test F
%% 

testF(Initialise, RefList, Expected) ->
    write(Initialise, ?PATH_F),
    Fun1 = fun(X) -> io:format("In fun for X ~p~n", [X]),
                     RefX = #refX{site = ?SITE1, path = ?PATH_F, obj = X},
                     Fun2 = fun() -> hn_db_wu:delete_cell(RefX) end,
                     mnesia:activity(transaction, Fun2)
           end,
    [Fun1(X) || X <- RefList],
    Ref2 = #refX{site = ?SITE1, path = ?PATH_F, obj = {page, "/"}},
    Fun2 = fun() -> hn_db_wu:read_attrs(Ref2) end,
    Got = mnesia:activity(transaction, Fun2),
    io:format("in testE Got is ~p~n", [Got]),
    Get2 = extract_values(Got),
    test2(lists:sort(Expected), Get2).

%%
%% Test G
%% 

testG(Initialise, RefList, Expected) ->
    write(Initialise, ?PATH_G),
    Fun1 = fun(X) -> io:format("In fun for X ~p~n", [X]),
                     RefX = #refX{site = ?SITE1, path = ?PATH_G, obj = X},
                     Fun2 = fun() -> hn_db_wu:delete_cell(RefX) end,
                     mnesia:activity(transaction, Fun2)
           end,
    [Fun1(X) || X <- RefList],
    Ref2 = #refX{site = ?SITE1, path = ?PATH_G, obj = {page, "/"}},
    Fun2 = fun() -> hn_db_wu:read_attrs(Ref2) end,
    Got = mnesia:activity(transaction, Fun2),
    io:format("in testE Got is ~p~n", [Got]),
    Get2 = extract_names(Got),
    test2(lists:sort(Expected), Get2).

%%
%% Various comparison and helper functions
%% 

make_list(List, Path) -> make_list(List, Path, []).

make_list([], Path, Acc) -> Acc;
make_list([H | T], Path, Acc) ->
    Ref = #refX{site = ?SITE1, path = Path, obj = H},
    make_list(T, Path, [Ref | Acc]).

write([], _Path) -> ok;
write([{Ref, Val} | T], Path) ->
    Ref2 = #refX{site = ?SITE1, path = Path, obj = Ref},
    Fun1 = fun() -> hn_db_wu:write_attr(Ref2, {formula, Val}) end,
    mnesia:activity(transaction, Fun1),
    write(T, Path).

extract_values(List) -> extract_values2(List, []).

extract_values2([], Acc) -> lists:sort(Acc);
extract_values2([H| T], Acc) ->
    #hn_item{addr = Addr, val = Val} = H,
    #ref{name = Name} = Addr,
    case Name of
        rawvalue -> extract_values2(T, [Val | Acc]);
         _       -> extract_values2(T, Acc)
    end.

extract_names(List) -> extract_names2(List, []).

extract_names2([], Acc) -> lists:sort(Acc);
extract_names2([#hn_item{addr = Ref} | T], Acc) -> #ref{name = N} = Ref,
                                                  extract_names2(T, [N | Acc]).


test2(Expected, Expected) -> io:format("Success: ~p~n",[Expected]);
test2(Expected, Got)      -> io:format("Failure: Expected ~p~n"++
                                       "Got ~p~n",[Expected, Got]),
                             exit('endless fail').

%% Test cases starts here.
%%--------------------------------------------------------------------

%%
%% Test A
%% 
test_case1A() ->
    [{doc, "Simple test of attributes"}].

test_case1A(Config) when is_list(Config) ->
    testA(?Ref1, ?Val1, ?Expected1A),
    ok.

test_case2A() ->
    [{doc, "Simple test of attributes"}].

test_case2A(Config) when is_list(Config) ->
    testA(?Ref2, ?Val2, ?Expected2A),
    ok.

test_case3A() ->
    [{doc, "Simple test of attributes"}].

test_case3A(Config) when is_list(Config) ->
    testA(?Ref3, ?Val3, ?Expected3A),
    ok.

test_case4A() ->
    [{doc, "Simple test of attributes"}].

test_case4A(Config) when is_list(Config) ->
    testA(?Ref4, ?Val4, ?Expected4A),
    ok.

%%
%% Test B
%% 
test_case1B() ->
    [{doc, "Simple test of attributes"}].

test_case1B(Config) when is_list(Config) ->
    testB(?Ref1, ?Val1, ?Expected1B),
    ok.

test_case2B() ->
    [{doc, "Simple test of attributes"}].

test_case2B(Config) when is_list(Config) ->
    testB(?Ref2, ?Val2, ?Expected2B),
    ok.

test_case3B() ->
    [{doc, "Simple test of attributes"}].

test_case3B(Config) when is_list(Config) ->
    testB(?Ref3, ?Val3, ?Expected3B),
    ok.

test_case4B() ->
    [{doc, "Simple test of attributes"}].

test_case4B(Config) when is_list(Config) ->
    testB(?Ref4, ?Val4, ?Expected4B),
    ok.

%%
%% Test C
%% 

test_case1C() ->
    [{doc, "Simple test of attributes"}].

test_case1C(Config) when is_list(Config) ->
    testC(?InitialiseC, ?Ref1C, ?Expected1C),
    ok.

test_case2C() ->
    [{doc, "Simple test of attributes"}].

test_case2C(Config) when is_list(Config) ->
    testC(?InitialiseC, ?Ref2C, ?Expected2C),
    ok.

test_case3C() ->
    [{doc, "Simple test of attributes"}].

test_case3C(Config) when is_list(Config) ->
    testC(?InitialiseC, ?Ref3C, ?Expected3C),
    ok.

test_case4C() ->
    [{doc, "Simple test of attributes"}].

test_case4C(Config) when is_list(Config) ->
    testC(?InitialiseC, ?Ref4C, ?Expected4C),
    ok.

%%
%% Test D
%% 
test_case1D() ->
    [{doc, "Simple test of attributes"}].

test_case1D(Config) when is_list(Config) ->
    testD(?InitialiseD, ?Ref1D, ?Expected1D),
    ok.

%%
%% Test D1
%% 
%% Note most of the macros are 'D' ones non 'D1' ones...
test_case1D1() ->
    [{doc, "Simple test of attributes"}].

test_case1D1(Config) when is_list(Config) ->
    testD(?InitialiseD, ?Ref1D1, ?Expected1D1),
    ok.

%%
%% Test E
%% 
test_case1E() ->
    [{doc, "Simple test of attributes"}].

test_case1E(Config) when is_list(Config) ->
    testE(?InitialiseE, ?Ref1E, ?Expected1E),
    ok.
    
%%
%% Test F
%% 
test_case1F() ->
    [{doc, "Simple test of attributes"}].

test_case1F(Config) when is_list(Config) ->
    testF(?InitialiseF, ?Ref1F, ?Expected1F),
    ok.

%%
%% Test G
%% 
test_case1G() ->
    [{doc, "Simple test of attributes"}].

test_case1G(Config) when is_list(Config) ->
    testG(?InitialiseG, ?Ref1G, ?Expected1G),
    ok.
    
