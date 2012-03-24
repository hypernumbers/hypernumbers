-module(test).

-export([all/0,
         fuzz/0,
         sys/0,
         sys/1,
         auth/0,
         ztest/0,
         excel/0,
         excel/1,
         excel/2 ,
         security/0,
         security/1,
         post_logged_out_TEST/2,
         post_login_TEST/4,
         post_TEST_DEBUG/0
          ]).

% debugging
-export([
         generate_fuzz_tests/0
        ]).

-define(LOG_DIR,     "var/tests/").
-define(TEST_DIR,    "tests/").
-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").

-define(COVER_FILE, "tests/cover.spec").
-define(COVER_DATA, "tests/cover.data").

-define(SITE, "http://tests.hypernumbers.dev:9000").

init() -> setup(blank, []).

init_fuzz() ->
    {ok, _, Uid} = passport:get_or_create_user("test@hypernumbers.com"),
    passport:validate_uid(Uid),
    passport:set_password(Uid, "i!am!secure"),
    setup:site(blank, [{creator, Uid}]).

init_sec() ->
    {ok, _, Uid} = passport:get_or_create_user("test@hypernumbers.com"),
    passport:validate_uid(Uid),
    passport:set_password(Uid, "i!am!secure"),
    setup(security_test, [{creator, Uid}]).

setup(Type, Opts) when is_list(Opts) ->
    case hn_setup:site_exists(?SITE) of
        true  -> hn_setup:delete_site(?SITE);
        false -> ok
    end,
    catch hn_setup:site(?SITE, Type, Opts).

all() -> excel(), sys(), security(), fuzz(), auth(), ztest().

fuzz() ->
    init_fuzz(),
    ok = generate_fuzz_tests(),
    WC = filename:absname(?TEST_DIR)++"/funs_fuzz_test",
    io:format("WC is ~p~n", [WC]),
    Tests = filelib:wildcard(WC),
    io:format("Tests is ~p~n", [Tests]),
    Opts = [ {dir, Tests} ],
    do_test(Opts).

security() ->
    init_sec(),
    WC = filename:absname(?TEST_DIR)++"/security_test",
    io:format("WC is ~p~n", [WC]),
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    io:format("Tests is ~p~n", [Tests]),
    do_test(Opts).

security(S) ->
    io:format("Security running with ~p~n", [S]),
    init_sec(),
    WC = filename:absname(?TEST_DIR)++"/security_test",
    Tests = filelib:wildcard(WC),
    %[compile(X) || X <- Tests],
    Suite = S ++ "_SUITE",
    Opts = [ {dir, Tests},
             {suite, [Suite]} ],
    do_test(Opts).

compile(File) ->
    io:format("Compiling ~p~n", [File]),
    case compile:file(File, [return_errors]) of
        {ok, FileName} ->
            io:fwrite("OK: ~s~n", [File]),
            code:delete(FileName),
            code:purge(FileName),
            code:load_file(FileName),
            ok;
        Error ->
            io:format("Error is ~p~n", [Error]),
            exit(lists:flatten(io_lib:format("can't compile ~p...", [File])))
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

ztest() ->
    WC = filename:absname(?TEST_DIR)++"/ztest",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    io:format("Tests is ~p~n", [Tests]),
    do_test(Opts).

auth() ->
    WC = filename:absname(?TEST_DIR)++"/auth_test",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    io:format("Tests is ~p~n", [Tests]),
    do_test(Opts).

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
    io:format("Opts is ~p~n", [Opts]),
    application:unset_env(hypernumbers, sync_url),
    filelib:ensure_dir(filename:absname(?LOG_DIR)++"/"),
    DefaultOps = [{logdir, filename:absname(?LOG_DIR)}],
    ct:run_test(
      lists:ukeymerge(1,
                      lists:keysort(1, Opts),
                      lists:keysort(1, DefaultOps))),
    ok.

generate_fuzz_tests() ->
    File = code:lib_dir(hypernumbers)
        ++ "/priv/core_install/docroot/hypernumbers/"
        ++ "fns_en-GB.json",
    {ok, Fns} = file:read_file(File),
    {array, List} = mochijson:decode(Fns),
    ok = gen2(List).

gen2([]) -> ok;
gen2([{struct, H} | T]) ->
    {"fn", Name} = lists:keyfind("fn", 1, H),
    {"args", {array, Args}} = lists:keyfind("args", 1, H),
    {Name2, Min} = case lists:keyfind("resize", 1, H) of
                       {"resize", false}    -> {Name, 0};
                       {"resize", "row"}    -> {Name ++ ".", 1};
                       {"resize", "column"} -> {Name ++ ".", 1};
                       {"resize", "range"}  -> {Name ++ ".", 2}
            end,
    Module = find_module(Name2),
    Bounds = get_bounds(Args, Min),
    ok = gen_fuzz_tests(Module, Name2, Bounds),
    gen2(T).

get_bounds([], Min) -> {Min, Min};
get_bounds([{struct, List} | T], Min) ->
    case lists:keyfind("type", 1, List) of
        {"type", "finite"}   -> get_bounds(T, Min + 1);
        {"type", "variable"} -> get_bounds(T, Min + 2);
        {"type", "infinite"} -> case Min of
                                    0 -> {1, 3};
                                    _ -> {Min, Min + 2}
                                end;
        _Other               -> {Min, Min + length(T) + 1}
    end.

gen_fuzz_tests(Module, Name, {Min, Max}) ->
    if
        Min =< Max -> write_test(Module, Name, Min),
                      gen_fuzz_tests(Module, Name,
                                     {Min + 1, Max});
         Min > Max -> ok
    end.

write_test(Module, Name, Min) ->
    NewName = "fuzz_" ++ Name ++ "_args_"
        ++ integer_to_list(Min) ++ "_SUITE",
    NewName2 = [case X of $. -> $_; X -> X end || X <- NewName],
    NewName3 = case NewName2 of
               "and" -> "special_and";
               "if" -> "special_if";
               "or" -> "special_or";
               _     -> NewName2
           end,
    File = "-module(" ++ NewName3 ++ ").\n\n"
        ++ "-define(MODULENAME, '" ++ atom_to_list(Module) ++ "')." ++ "\n"
        ++ "-define(FN, '" ++ Name ++ "').\n"
        ++ "-define(NOOFPARAMS, " ++ integer_to_list(Min) ++ ").\n\n"
        ++ "-include(\"fuzz_include.irl\")." ++ "\n",
    Dir = code:lib_dir(hypernumbers)++"/../../tests/funs_fuzz_test/",
    file:write_file(Dir ++ NewName3 ++ ".erl", File),
    ok.

find_module(Name) ->
    Modules = muin:get_modules(),
    find_m(Modules, Name).

find_m([], Name) -> Msg = "function " ++ Name ++ " is not found",
                    exit(Msg);
find_m([H | T], Name) ->
    Info = erlang:apply(H, 'module_info', []),
    {exports, List} = lists:keyfind(exports, 1, Info),
    case lists:keyfind(list_to_atom(Name), 1, List) of
        false -> find_m(T, Name);
        _     -> H
    end.
