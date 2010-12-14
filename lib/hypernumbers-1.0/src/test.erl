-module(test).

-export([all/0, 
         sys/0,
         sys/1, 
         excel/0,
         excel/1,
         excel/2 ,
         security/0,
         security/1,
         post_logged_out_TEST/2,
         post_login_TEST/4,
         post_TEST_DEBUG/0
         ]).

-define(LOG_DIR,     "var/tests/").
-define(TEST_DIR,    "tests/").
-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").

-define(COVER_FILE, "tests/cover.spec").
-define(COVER_DATA, "tests/cover.data").

-define(SITE, "http://tests.hypernumbers.dev:9000").

init() ->
    catch hn_setup:site(?SITE, blank, []).
        
init_sec() ->
    catch hn_setup:site(?SITE, security_test, []).

all() -> excel(), sys(), security().

security() -> 
    init_sec(),
    WC = filename:absname(?TEST_DIR)++"/security_test",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    do_test(Opts).

security(S) ->
    init_sec(),
    WC = filename:absname(?TEST_DIR)++"/security_test",
    Tests = filelib:wildcard(WC),
    [compile(X) || X <- Tests],
    Suite = S ++ "_SUITE",
    Opts = [ {dir, [Tests]},
             {suite, [Suite]} ],
    do_test(Opts).

compile(File) ->
    case compile:file(File, []) of
        {ok, FileName} ->
            io:fwrite("OK: ~s~n", [File]),
            code:delete(FileName),
            code:purge(FileName),
            code:load_file(FileName),
            ok;
        _Error ->
              exit("can't compile...")
    end.

sys() ->
    sys([]).
sys(Suites) ->
    init(),
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
    init(),
    WC = filename:absname(?TEST_DIR)++"/excel_import_"++TName++"*_test",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    do_test(Opts).
excel(T, S) ->
    init(),
    Test = filename:absname(?TEST_DIR)++"/excel_import_"++T++"_test",
    Suite = S ++ "_SUITE",
    Opts = [ {dir, [Test]},
             {suite, [Suite]} ],
    do_test(Opts).

post_TEST_DEBUG() ->
    % User = "test@hypernumbers.com",
    % Password = "i!am!secure",
    URL  = "http://tests.hypernumbers.dev:9000/test4/a1",
    Data = "{\"postinline\":{\"formula\":\"bleh\"}}",
    post_logged_out_TEST(URL, Data).

post_login_TEST(User, Password, URL, Data) ->
    [Proto, Root | _Rest] = string:tokens(URL, "/"),
    URL2=Proto ++"//"++Root++"/_login/?return="++URL,
    Type = "application/json",
    Accept = [{"Accept", "application/json"}],
    Login = "{\"email\":\""++User++"\", \"pass\":\""++Password
        ++"\",\"remember\":\"false\"}",
    R = httpc:request(post,{URL2, Accept, Type, Login}, [], []),
    Ret = case R of
              {ok, {{_, 200, _}, Headers, _}} ->
                  CookieList = proplists:get_all_values("set-cookie",
                                                                  Headers),
                  Cookie = get_right_cookie(CookieList),
                  A2 = [{"cookie", Cookie} | Accept],
                  io:format("logged in, going in with ~p~n~p~n", [URL, Data]),
                  httpc:request(post,{URL, A2, Type, Data}, [], []);
              {ok, {{_, _Other, _}, _, _}} ->
                  exit("invalid return from login - wig out!")
          end,
    {ok, {{_, Code, _}, _, _}} = Ret,
    Code.    

get_right_cookie(["auth=test!hypernumbers.com"++_R = H | _T]) -> H;
get_right_cookie([_H | T]) -> get_right_cookie(T).

post_logged_out_TEST(URL, Data) ->
    Type = "application/json",
    Accept = [{"Accept", "application/json"}],
    R = httpc:request(post,{URL, Accept, Type, Data}, [], []),
    R2 = case R of
             {ok, {{_, 303, _}, Headers, _}}   ->
                 {"location", Loc} = proplists:lookup("location", Headers),
                 Pong = httpc:request(post,{Loc, Accept, Type, Data}, [], []),
                 {ok, {{_, 303, _}, Headers2, _}} = Pong,
                 {"location", Loc2} = proplists:lookup("location", Headers2),
                 {"set-cookie", C2} = proplists:lookup("set-cookie", Headers2),
                 A2 = [{"cookie", C2} | Accept],
                 Pung = httpc:request(post,{Loc2, A2, Type, Data}, [], []),
                 {ok, {{_, 303, _}, Headers3, _}} = Pung,
                 {"location", Loc3} = proplists:lookup("location", Headers3),
                 {"set-cookie", C3} = proplists:lookup("set-cookie", Headers3),
                 A3 = [{"cookie", C3} | Accept],
                 httpc:request(post,{Loc3, A3, Type, Data}, [], []);
             {ok, {{_, _Other, _}, _, _}} ->
                 R
         end,
    {ok, {{_, Code, _}, _, _}} = R2,
    Code.

%%% Internal functions

do_test(Opts) ->
    application:unset_env(hypernumbers, sync_url),
    filelib:ensure_dir(filename:absname(?LOG_DIR)++"/"),
    DefaultOps = [{logdir, filename:absname(?LOG_DIR)}],
    ct:run_test(
      lists:ukeymerge(1, 
                      lists:keysort(1, Opts), 
                      lists:keysort(1, DefaultOps))),
    ok.
