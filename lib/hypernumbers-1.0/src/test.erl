-module(test).

-export([all/0, 
         sys/0, sys/1, 
         excel/0, excel/1, excel/2 ]).

-define(LOG_DIR,     "var/tests/").
-define(TEST_DIR,    "tests/").
-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").

-define(COVER_FILE, "tests/cover.spec").
-define(COVER_DATA, "tests/cover.data").


all() -> excel(), sys().


sys() ->
    sys([]).
sys(Suites) ->
    %% Copy source files
    SrcDir = code:lib_dir(hypernumbers)++"/src/",
    EbinDir = code:lib_dir(hypernumbers)++"/ebin/",
    file:write_file(?COVER_DATA, []),
    [file:copy(S, EbinDir++filename:basename(S)) 
     || S <- filelib:wildcard(SrcDir++"*.erl")],
    
    %% Setup Options
    SOpt = if Suites == [] -> [];
              true         -> [{suite, [S++"_SUITE" || S <- Suites]}] end,
    Opts = [ {dir, [filename:absname(?SYSTEST_DIR)]}
             ],

    do_test(SOpt ++ Opts),
    
    %% Cleanup
    [file:delete(S) || S <- filelib:wildcard(EbinDir++"*.erl")],
    ok.


excel() ->
    excel("1"),
    excel("2").
excel(TName) ->
    WC = filename:absname(?TEST_DIR)++"/excel_import_"++TName++"*_test",
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
    application:unset_env(hypernumbers, pingto),
    filelib:ensure_dir(filename:absname(?LOG_DIR)++"/"),
    DefaultOps = [{logdir, filename:absname(?LOG_DIR)}],
    ct:run_test(
      lists:ukeymerge(1, 
                      lists:keysort(1, Opts), 
                      lists:keysort(1, DefaultOps))),
    ok.
