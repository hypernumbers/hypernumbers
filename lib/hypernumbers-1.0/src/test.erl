-module(test).

-export([all/0, 
         sys/0, sys/1, 
         excel/0, excel/1 ]).

-define(LOG_DIR,     "logs/").
-define(TEST_DIR,    "tests/").
-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").


all() -> excel(), sys().


sys() ->
    sys([]).
sys(Suites) ->
    SOpt = if Suites == [] -> [];
              true         -> [{suites, Suites}] end,
    Opts = [ {logdir, filename:absname(?LOG_DIR)},
             {dir, [filename:absname(?SYSTEST_DIR)]} ],
    ct:run_test(Opts ++ SOpt).


excel() ->
    excel("1"),
    excel("2").
excel(TName) ->
    WC = filename:absname(?TEST_DIR)++"/excel_import_"++TName++"*",
    Tests = filelib:wildcard(WC),
    Opts = [ {logdir, filename:absname(?LOG_DIR)},
             {dir, Tests} ],
    ct:run_test(Opts).
    
    
    
