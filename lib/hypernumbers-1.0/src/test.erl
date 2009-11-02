-module(test).

-export([all/0, 
         sys/0, sys/1, 
         excel/0, excel/1, excel/2 ]).

-define(LOG_DIR,     "logs/").
-define(TEST_DIR,    "tests/").
-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").

-define(COVER_FILE, "tests/cover.spec").
-define(COVER_DATA, "tests/cover.data").


all() -> excel(), sys().


sys() ->
    sys([]).
sys(Suites) ->
    SOpt = if Suites == [] -> [];
              true         -> [{suite, [S++"_SUITE" || S <- Suites]}] end,
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
    %% Copy source files
    SrcDir = code:lib_dir(hypernumbers)++"/src/",
    EbinDir = code:lib_dir(hypernumbers)++"/ebin/",

    [file:copy(S, EbinDir++filename:basename(S)) 
     || S <- filelib:wildcard(SrcDir++"*.erl")],

    file:write_file(?COVER_DATA, []),
    DefaultOps = [{logdir, filename:absname(?LOG_DIR)},
                  {cover, filename:absname(?COVER_FILE)}],

    ct:run_test(
      lists:ukeymerge(
        1, lists:keysort(1, Opts), lists:keysort(1, DefaultOps))),

    [file:delete(S) || S <- filelib:wildcard(EbinDir++"*.erl")],
    ok.
   

