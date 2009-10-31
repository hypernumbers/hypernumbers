-module(test).

-export([all/0, 
         sys/0, sys/1, 
         excel/0, excel/1, excel/2 ]).

-define(LOG_DIR,     "logs/").
-define(TEST_DIR,    "tests/").
-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").


all() -> excel(), sys().


sys() ->
    sys([]).
sys(Suites) ->
    SOpt = if Suites == [] -> [];
              true         -> [{suite, Suites}] end,
    Opts = [ {dir, [filename:absname(?SYSTEST_DIR)]} ],
    do_test(Opts ++ SOpt).


excel() ->
    excel("1"),
    excel("2").
excel(TName) ->
    WC = filename:absname(?TEST_DIR)++"/excel_import_"++TName++"*",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    do_test(Opts).
excel(T, S) ->
    Test = filename:absname(?TEST_DIR)++"/excel_import_"++T++"_test",
    Suite = S ++ "_SUITE",
    Opts = [ {dir, [Test]},
             {suite, [Suite]} ],
    do_test(Opts).


do_test(Opts) ->
    DefaultOps = [{logdir, filename:absname(?LOG_DIR)}],
    ct:run_test(
      lists:ukeymerge(
        1, lists:keysort(1, Opts), lists:keysort(1, DefaultOps))).
