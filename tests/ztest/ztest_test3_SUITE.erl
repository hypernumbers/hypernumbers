%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Test suite for z-tests
%%%
%%% @end
%%% Created :  24 Mar 2012 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(ztest_test3_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(SITE, "http://tests.hypernumbers.dev:9000").
-define(SLEEP, 1000).

-include_lib("hypernumbers/include/spriki.hrl").
-include("ct.hrl").

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
    case hn_setup:site_exists(?SITE) of
        true  -> ok = hn_setup:delete_site(?SITE);
        false -> ok
    end,
    hn_setup:site(?SITE, blank, []),
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
    ok = hn_setup:delete_site(?SITE).

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
     test1,
     test2,
     test3,
     test4
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
test1() ->
    [{doc, "Add some vals and then a z-fn"}].

test1(Config) when is_list(Config) ->
    Path = "test1",
    Paths = ["bob", "jim", "jon", "alice", "wendy"],
    [load_cell([Path, X], 5, 5, "999") || X <- Paths],
    [load_cell([Path, X, "extra"], 5, 5, "1") || X <- Paths],
    Formula = "=sum(/" ++ Path ++ "/[e5 > 1]/[true]/e5)",
    load_cell([Path], 1, 1, Formula),
    Val = read_raw([Path], 1, 1),
    pass(5, Val).

test2() ->
    [{doc, "Add some vals and then a z-fn and the some more vals"}].

test2(Config) when is_list(Config) ->
    Path = "test2",
    Paths = ["bob", "jim", "jon", "alice", "wendy"],
    [load_cell([Path, X], 5, 5, "999") || X <- Paths],
    [load_cell([Path, X, "extra"], 5, 5, "1") || X <- Paths],
    Formula = "=sum(/" ++ Path ++ "/[e5 > 1]/[true]/e5)",
    load_cell([Path], 1, 1, Formula),
    load_cell([Path, "alchemy", "bimbo"], 5, 5, "1"),
    load_cell([Path, "alchemy"], 5, 5, "789"),
    Val = read_raw([Path], 1, 1),
    pass(6, Val).

test3() ->
    [{doc, "Add some vals and then a z-fn and the some more vals"}].

test3(Config) when is_list(Config) ->
    Path = "test3",
    Paths = ["bob", "jim", "jon", "alice", "wendy"],
    [load_cell([Path, X], 5, 5, "999") || X <- Paths],
    [load_cell([Path, X, "extra"], 5, 5, "1") || X <- Paths],
    load_cell([Path, "alchemy", "bimbo"], 5, 5, "1"),
    Formula = "=sum(/" ++ Path ++ "/[e5 > 1]/[true]/e5)",
    load_cell([Path], 1, 1, Formula),
    load_cell([Path, "alchemy"], 5, 5, "789"),
    Val = read_raw([Path], 1, 1),
    pass(6, Val).

test4() ->
    [{doc, "Add some vals and then a z-fn and the some more vals"}].

test4(Config) when is_list(Config) ->
    Path = "test4",
    Paths = ["bob", "jim", "jon", "alice", "wendy"],
    [load_cell([Path, X], 5, 5, "999") || X <- Paths],
    [load_cell([Path, X, "extra"], 5, 5, "1") || X <- Paths],
    load_cell([Path, "alchemy"], 5, 5, "789"),
    Formula = "=sum(/" ++ Path ++ "/[e5 > 1]/[true]/e5)",
    load_cell([Path], 1, 1, Formula),
    load_cell([Path, "alchemy", "bimbo"], 5, 5, "1"),
    Val = read_raw([Path], 1, 1),
    pass(6, Val).

% new internal fs
load_cell(Path, X, Y, Formula) ->
    ZCell = #refX{site = ?SITE, path = Path, type = url,
                  obj = {cell, {X, Y}}},
    io:format("Loading ~p into ~p~n", [Formula, ZCell]),
    ok = new_db_api:write_attributes([{ZCell, [{"formula", Formula}]}]).

read_raw(Path, X, Y) ->
    ZCell = #refX{site = ?SITE, path = Path, type = url,
                  obj = {cell, {X, Y}}},
    io:format("Reading ~p~n", [ZCell]),
    timer:sleep(?SLEEP),
    case new_db_api:read_attribute(ZCell, "__rawvalue") of
        [{_, ZCellVal}] -> ZCellVal;
        Other           -> Other
    end.



%%% Internal fns
deletecol(PageRoot, N) when is_integer(N) andalso N > 0 ->
    ZCol = #refX{site = ?SITE, type = url,
                 path = [PageRoot, integer_to_list(N)],
                 obj = {column, {1, 1}}},
    io:format("deleting col in ~p~n", [ZCol]),
    ok = new_db_api:delete(ZCol, nil).

insertcol(PageRoot, N) when is_integer(N) andalso N > 0 ->
    ZCol = #refX{site = ?SITE, type = url,
                 path = [PageRoot, integer_to_list(N)],
                 obj = {column, {1, 1}}},
    io:format("inserting col in ~p~n", [ZCol]),
    ok = new_db_api:insert(ZCol, nil).

deleterow(PageRoot, N) when is_integer(N) andalso N > 0 ->
    ZRow = #refX{site = ?SITE, type = url,
                 path = [PageRoot, integer_to_list(N)],
                 obj = {row, {1, 1}}},
    io:format("deleting row in ~p~n", [ZRow]),
    ok = new_db_api:delete(ZRow, nil).

insertrow(PageRoot, N) when is_integer(N) andalso N > 0 ->
    ZRow = #refX{site = ?SITE, type = url,
                 path = [PageRoot, integer_to_list(N)],
                 obj = {row, {1, 1}}},
    io:format("inserting row in ~p~n", [ZRow]),
    ok = new_db_api:insert(ZRow, nil).

pass(Expected, Expected) -> io:format("Pass: Expected ~p Got ~p~n",
                                      [Expected, Expected]),
                            'test passed';
pass(Expected, Got)      -> io:format("Fail: Expected ~p Got ~p~n",
                                      [Expected, Got]),
                            exit("test failed").

load_vanilla_zformula(PageRoot) ->
    io:format("loading vanilla zformula for ~p~n", [PageRoot]),
    Formula = "=sum(/" ++ PageRoot ++ "/[true]/a1)",
    ZCell = #refX{site = ?SITE, path = [PageRoot], type = url,
                  obj = {cell, {5, 5}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", Formula}]}]).

load_complex_zformula(PageRoot) ->
    io:format("loading complex zformula for ~p~n", [PageRoot]),
    Formula = "=sum(/" ++ PageRoot ++ "/[e6>1]/e5)",
    ZCell = #refX{site = ?SITE, path = [PageRoot], type = url,
                  obj = {cell, {5, 5}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", Formula}]}]).

dump(PageRoot) ->
    ZCell = #refX{site = ?SITE, path = [PageRoot], type = url,
                  obj = {cell, {1, 1}}},
    [{_, Cell}] = new_db_api:read_ref(ZCell),
    io:format("Cell on ~p is ~p~n", [PageRoot, Cell]).

add_page(PageRoot) ->
    io:format("adding page on ~p~n", [PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, "added page"], type = url,
                  obj = {cell, {5, 5}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "1"}]}]).

add_complex_page(PageRoot) ->
    io:format("adding complex page on ~p~n", [PageRoot]),
    ZCell1 = #refX{site = ?SITE, path = [PageRoot, "added page"], type = url,
                   obj = {cell, {5, 5}}},
    ZCell2 = #refX{site = ?SITE, path = [PageRoot, "added page"], type = url,
                   obj = {cell, {5, 6}}},
   ok = new_db_api:write_attributes([{ZCell1, [{"formula", "1"}]}]),
   ok = new_db_api:write_attributes([{ZCell2, [{"formula", "999"}]}]).


load_values(_PageRoot, 0) -> ok;
load_values(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("loading value for page ~p on ~p~n", [N, PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {5, 5}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "1"}]}]),
    load_values(PageRoot, N - 1).

load_complex_values(_PageRoot, 0) -> ok;
load_complex_values(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("loading complex value for page ~p on ~p~n", [N, PageRoot]),
    ZCell1 = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {5, 5}}},
    ZCell2 = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {5, 6}}},
    ok = new_db_api:write_attributes([{ZCell1, [{"formula", "1"}]}]),
    ok = new_db_api:write_attributes([{ZCell2, [{"formula", "999"}]}]),
    load_complex_values(PageRoot, N - 1).

delete(PageRoot, N, Y) when is_integer(N) andalso N > 0 ->
    io:format("Deleting the cell for ~p on ~p~n", [N, PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {5, Y}}},
    io:format("deleting ~p~n", [ZCell]),
    ok = new_db_api:delete(ZCell, nil).

poke10(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("Poking the value 10 into ~p on ~p~n", [N, PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {5, 5}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "10"}]}]).

dropcomplex(PageRoot, N) when is_integer(N) andalso N > 0 ->
    io:format("Dropping the complex z-query into ~p on ~p~n", [N, PageRoot]),
    ZCell = #refX{site = ?SITE, path = [PageRoot, integer_to_list(N)], type = url,
                  obj = {cell, {5, 6}}},
    ok = new_db_api:write_attributes([{ZCell, [{"formula", "-1"}]}]).

debug() ->
    init_per_suite([]),
    {ok, U} = passport:email_to_uid("test@hypernumbers.com"),
    ok = hn_groups:add_user(?SITE, "admin", U),
    test1([]).
