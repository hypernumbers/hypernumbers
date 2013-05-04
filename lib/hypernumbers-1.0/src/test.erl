-module(test).

-export([
         all/0,
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
         authorization/0,
         authorization/1,
         get_logged_in_json_TEST/1,
         get_logged_in_json_notadmin_TEST/1,
         get_logged_out_json_TEST/1,
         get_logged_in_html_TEST/1,
         get_logged_in_html_notadmin_TEST/1,
         get_logged_out_html_TEST/1,
         get_api_TEST/3,
         post_logged_in_TEST/2,
         post_logged_in_notadmin_TEST/2,
         post_logged_out_TEST/2,
         post_login_TEST/4,
         post_TEST_DEBUG/0,
         post_api_TEST/3
        ]).

% debugging
-export([
         generate_fuzz_tests/0
        ]).

-include("spriki.hrl").

-define(LOG_DIR,     "var/tests/").
-define(TEST_DIR,    "tests/").
-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").

-define(COVER_FILE, "tests/cover.spec").
-define(COVER_DATA, "tests/cover.data").

-define(SITE, "http://tests.hypernumbers.dev:9000").
-define(SYSSITE, "http://sys.hypernumbers.dev:9000").
-define(SECSITE, "http://security.hypernumbers.dev:9000").

-define(PL, proplists:lookup).

-define(PUBLIC,            "12345678123456781234567812345678").
-define(PRIVATE,           "ABCDEFGHABCDEFGHABCDEFGHABCDEFGH").
-define(PUBLICADMIN,       "X2345678123456781234567812345678").
-define(PRIVATEADMIN,      "XBCDEFGHABCDEFGHABCDEFGHABCDEFGH").
-define(PUBLICSUBDIRS,     "Y2345678123456781234567812345678").
-define(PRIVATESUBDIRS,    "YBCDEFGHABCDEFGHABCDEFGHABCDEFGH").
-define(PUBLICAPPENDONLY,  "Z2345678123456781234567812345678").
-define(PRIVATEAPPENDONLY, "ZBCDEFGHABCDEFGHABCDEFGHABCDEFGH").

init() -> setup(blank, []).

init_sys() ->
    {ok, _, Uid} = passport:get_or_create_user("test@hypernumbers.com"),
    passport:validate_uid(Uid),
    passport:set_password(Uid, "i!am!secure"),
    setup(?SYSSITE, blank, [{creator, Uid}]).

init_fuzz() ->
    {ok, _, Uid} = passport:get_or_create_user("test@hypernumbers.com"),
    passport:validate_uid(Uid),
    passport:set_password(Uid, "i!am!secure"),
    setup(blank, [{creator, Uid}]).

init_sec() ->
    {ok, _, Uid} = passport:get_or_create_user("test@hypernumbers.com"),
    passport:validate_uid(Uid),
    passport:set_password(Uid, "i!am!secure"),
    setup(?SECSITE, security_test, [{creator, Uid}]).

init_auth() ->
    Email = "test@hypernumbers.com",
    {ok, _, Uid} = passport:get_or_create_user(Email),
    passport:validate_uid(Uid),
    passport:set_password(Uid, "i!am!secure"),
    setup(authorization_test, [{creator, Uid}]),
    {ok, _, Uid2} = passport:get_or_create_user("nonadmin@hypernumbers.com"),
    passport:validate_uid(Uid2),
    passport:set_password(Uid2, "i!am!secure"),
    ok = hn_groups:add_user(?SITE, "user", Uid2),
    API_URLs1 = [#api_url{path = "/", admin = false, include_subs = false,
                         append_only = false}],
    API1 = #api{publickey = ?PUBLIC, privatekey = ?PRIVATE, urls = API_URLs1},
    new_db_api:write_api(?SITE, API1),
    API_URLs2 = [#api_url{path = "/", admin = true, include_subs = true,
                         append_only = false}],
    API2 = #api{publickey = ?PUBLICADMIN, privatekey = ?PRIVATEADMIN, urls = API_URLs2},
    new_db_api:write_api(?SITE, API2),
    API_URLs3 = [#api_url{path = "/", admin = false, include_subs = true,
                         append_only = false}],
    API3 = #api{publickey = ?PUBLICSUBDIRS, privatekey = ?PRIVATESUBDIRS, urls = API_URLs3},
    new_db_api:write_api(?SITE, API3),
    API_URLs4 = [#api_url{path = "/", admin = false, include_subs = true,
                         append_only = true}],
    API4 = #api{publickey = ?PUBLICAPPENDONLY, privatekey = ?PRIVATEAPPENDONLY, urls = API_URLs4},
    new_db_api:write_api(?SITE, API4).

setup(Type, Opts) when is_list(Opts) ->
    setup(?SITE, Type, Opts).

setup(Site, Type, Opts) ->
    case hn_setup:site_exists(Site) of
        true  -> hn_setup:delete_site(Site);
        false -> ok
    end,
    hn_setup:site(Site, Type, Opts).

all() -> excel(), sys(), security(), fuzz(), auth(), ztest(), authorization().

fuzz() ->
    init_fuzz(),
    ok = generate_fuzz_tests(),
    WC = filename:absname(?TEST_DIR)++"/funs_fuzz_test",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    do_test(Opts).

security() ->
    init_sec(),
    WC = filename:absname(?TEST_DIR) ++ "/security_test",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    do_test(Opts).

authorization() ->
    init_auth(),
    WC = filename:absname(?TEST_DIR) ++ "/authorization_test",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
    do_test(Opts).

security(S) ->
    init_sec(),
    WC = filename:absname(?TEST_DIR) ++ "/security_test",
    Tests = filelib:wildcard(WC),
    [compile(X) || X <- Tests],
    Suite = S ++ "_SUITE",
    Opts = [ {dir, Tests},
             {suite, [Suite]} ],
    do_test(Opts).

authorization(S) ->
    init_auth(),
    WC = filename:absname(?TEST_DIR) ++ "/authorization_test",
    Tests = filelib:wildcard(WC),
    [compile(X) || X <- Tests],
    Suite = S ++ "_SUITE",
    Opts = [ {dir, Tests},
             {suite, [Suite]} ],
    do_test(Opts).

compile(File) ->
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
    init_sys(),
    % Copy source files
    SrcDir = code:lib_dir(hypernumbers)++"/src/",
    EbinDir = code:lib_dir(hypernumbers)++"/ebin/",
    file:write_file(?COVER_DATA, []),
    [file:copy(S, EbinDir++filename:basename(S))
     || S <- filelib:wildcard(SrcDir++"*.erl")],

    % Setup Options
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
    do_test(Opts).

auth() ->
    WC = filename:absname(?TEST_DIR)++"/auth_test",
    Tests = filelib:wildcard(WC),
    Opts = [ {dir, Tests} ],
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
    URL  = "http://security.hypernumbers.dev:9000/test1/a1",
    Data = "{\"delete\":\"all\"}",
    post_logged_in_TEST(URL, Data).

post_logged_in_TEST(URL, Data) ->
    post_logged_in2(URL, Data, admin).

post_logged_in_notadmin_TEST(URL, Data) ->
    post_logged_in2(URL, Data, notadmin).

post_logged_in2(URL, Data, UserType) ->
    User = case UserType of
               admin    -> "test@hypernumbers.com";
               notadmin -> "nonadmin@hypernumbers.com"
           end,
    Password = "i!am!secure",
    post_login_TEST(User, Password, URL, Data).

post_login_TEST(User, Password, URL, Data) ->
    [Proto, Root | _Rest] = string:tokens(URL, "/"),
    URL2=Proto ++ "//" ++ Root ++ "/_login/?return=" ++ URL,
    Type = "application/json",
    Accept = [{"Accept", "application/json"}],
    Login = "{\"email\":\"" ++ User ++ "\", \"pass\":\"" ++ Password
        ++ "\",\"remember\":false}",
    ok = httpc:reset_cookies(),
    R = httpc:request(post, {URL2, Accept, Type, Login}, [], []),
    Ret = case R of
              {ok, {{_, 200, _Body}, Headers, _}} ->
                  CookieList = proplists:get_all_values("set-cookie",
                                                        Headers),
                  Cookie = get_right_cookie(CookieList),
                  A2 = [{"cookie", Cookie} | Accept],
                  io:format("logged in, going in with ~p~n~p~n", [URL, Data]),
                  httpc:request(post, {URL, A2, Type, Data}, [], []);
              {ok, {{_, Other, _}, _, _}} ->
                  Msg = io_lib:format("in test.erl: invalid return "
                                      ++ " from login ~p - wig out!",
                                      [Other]),
                  exit(lists:flatten(Msg))
          end,
    {ok, {{_, Code, _}, _, _}} = Ret,
    Code.

get_right_cookie(["auth=nonadmin!hypernumbers.com"++_R = H | _T]) -> H;
get_right_cookie(["auth=test!hypernumbers.com"++_R = H | _T]) -> H;
get_right_cookie([_H | T]) -> get_right_cookie(T).

post_api_TEST(URL, Body, Type) ->
    {PubK, PrivK} = case Type of
                        api ->            {?PUBLIC,           ?PRIVATE};
                        api_admin ->      {?PUBLICADMIN,      ?PRIVATEADMIN};
                        api_subdirs ->    {?PUBLICSUBDIRS,    ?PRIVATESUBDIRS};
                        api_appendonly -> {?PUBLICAPPENDONLY, ?PRIVATEAPPENDONLY}
                    end,
    MD5 = binary_to_list(crypto:md5(Body)),
    ContentMD5 = {"content-md5", MD5},
    Accept = {"Accept", "application/json"},
    Headers = [Accept, ContentMD5],
    Signature = hmac_api_lib:sign(PrivK, PubK, post, URL, Headers, "application/json"),
    Headers2 = [Signature | Headers],
    ok = httpc:reset_cookies(),
    R = httpc:request(post, {URL, Headers2, "application/json", Body}, [], []),
    {ok, {{_, Code, _}, _, _ReturnedBody}} = R,
    Code.

get_api_TEST(URL, Type, Accept) ->
    {PubK, PrivK} = case Type of
                        api ->            {?PUBLIC,           ?PRIVATE};
                        api_admin ->      {?PUBLICADMIN,      ?PRIVATEADMIN};
                        api_subdirs ->    {?PUBLICSUBDIRS,    ?PRIVATESUBDIRS};
                        api_appendonly -> {?PUBLICAPPENDONLY, ?PRIVATEAPPENDONLY}
                    end,
    Accept2 = case Accept of
                 html -> [{"Accept", "html"}];
                 json -> [{"Accept", "application/json"}]
             end,
    Signature = hmac_api_lib:sign(PrivK, PubK, get, URL, Accept2, []),
    Headers = [Signature | Accept2],
    ok = httpc:reset_cookies(),
    R = httpc:request(get, {URL, Headers}, [], []),
    {ok, {{_, Code, _}, _, _Body}} = R,
    %% Dir = code:priv_dir(hypernumbers) ++ "../../../../var/api-logs/",
    %% #refX{path = P, obj = O} = hn_util:url_to_refX(URL),
    %% Ob = case O of
    %%         {page, "/"} -> "page";
    %%         _           -> hn_util:obj_to_ref(O)
    %%       end,
    %% File = Dir ++ hn_util:path_to_json_path(lists:append([P ,["type-" ++ Ob]])),
    %% log(Body, File),
    Code.

get_logged_out_json_TEST(URL) ->
    get_logged_out(URL, "application/json").

get_logged_out_html_TEST(URL) ->
    get_logged_out(URL, "html").

get_logged_out(URL, Accept) ->
    AcceptHdr = [{"Accept", Accept}],
    ok = httpc:reset_cookies(),
    R = httpc:request(get, {URL, AcceptHdr}, [], []),
    R2 = case R of
             {ok, {{_, 303, _}, Headers, _}}   ->
                 {"location", Loc} = proplists:lookup("location", Headers),
                 Pong = httpc:request(get, {Loc, AcceptHdr}, [], []),
                 {ok, {{_, 303, _}, Headers2, _}} = Pong,
                 {"location", Loc2} = proplists:lookup("location", Headers2),
                 {"set-cookie", C2} = proplists:lookup("set-cookie", Headers2),
                 A2 = [{"cookie", C2} | AcceptHdr],
                 Pung = httpc:request(get, {Loc2, A2}, [], []),
                 {ok, {{_, 303, _}, Headers3, _}} = Pung,
                 {"location", Loc3} = proplists:lookup("location", Headers3),
                 {"set-cookie", C3} = proplists:lookup("set-cookie", Headers3),
                 A3 = [{"cookie", C3} | AcceptHdr],
                 httpc:request(get, {Loc3, A3}, [], []);
             {ok, {{_, _Other, _}, _, _}} ->
                 R
         end,
    {ok, {{_, Code, _}, _, _}} = R2,
    Code.

get_logged_in_json_TEST(URL) ->
    get_logged_in(URL, "application/json", admin).

get_logged_in_json_notadmin_TEST(URL) ->
    get_logged_in(URL, "application/json", notadmin).

get_logged_in_html_TEST(URL) ->
    get_logged_in(URL, "html", admin).

get_logged_in_html_notadmin_TEST(URL) ->
    get_logged_in(URL, "html", notadmin).

get_logged_in(URL, Accept, UserType) ->
    Cookie = login(URL, UserType),
    Accept2 = [{"Accept", Accept}],
    Hdrs = [{"cookie", Cookie} | Accept2],
    ok = httpc:reset_cookies(),
    R2 = httpc:request(get, {URL, Hdrs}, [], []),
    {ok, {{_, Code, _}, _, _}} = R2,
    Code.

post_logged_out_TEST(URL, Data) ->
    Type = "application/json",
    Accept = [{"Accept", "application/json"}],
    ok = httpc:reset_cookies(),
    R = httpc:request(post,{URL, Accept, Type, Data}, [], []),
    R2 = case R of
             {ok, {{_, 303, _}, Headers, _}} ->
                 {"location", Loc} = proplists:lookup("location", Headers),
                 Pong = httpc:request(post, {Loc, Accept, Type, Data}, [], []),
                 {ok, {{_, 303, _}, Headers2, _}} = Pong,
                 {"location", Loc2} = proplists:lookup("location", Headers2),
                 {"set-cookie", C2} = proplists:lookup("set-cookie", Headers2),
                 A2 = [{"cookie", C2} | Accept],
                 Pung = httpc:request(post, {Loc2, A2, Type, Data}, [], []),
                 {ok, {{_, 303, _}, Headers3, _}} = Pung,
                 {"location", Loc3} = proplists:lookup("location", Headers3),
                 {"set-cookie", C3} = proplists:lookup("set-cookie", Headers3),
                 A3 = [{"cookie", C3} | Accept],
                 httpc:request(post, {Loc3, A3, Type, Data}, [], []);
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
    Tests = lists:ukeymerge(1,lists:keysort(1, Opts), lists:keysort(1, DefaultOps)),
    ct:run_test(Tests),
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

login(URL, UserType) ->
    User = case UserType of
               admin    -> "test@hypernumbers.com";
               notadmin -> "nonadmin@hypernumbers.com"
           end,
    Password = "i!am!secure",
    [Proto, Root | _Rest] = string:tokens(URL, "/"),
    URL2 = Proto ++ "//" ++ Root ++ "/_login/?return=" ++ Proto ++ "//" ++ Root,
    Type = "application/json",
    Hdr = [{"Accept", "application/json"}],
    Login = "{\"email\":\"" ++ User ++ "\", \"pass\":\"" ++ Password
        ++ "\",\"remember\":false}",
    ok = httpc:reset_cookies(),
    R = httpc:request(post, {URL2, Hdr, Type, Login}, [], []),
    Cookie = case R of
                 {ok, {{_, 303, _}, Headers, _}} ->
                     {"location", Lc} = ?PL("location", Headers),
                     Pong = httpc:request(post, {Lc, Hdr, Type, Login}, [], []),
                     {ok, {{_, 303, _}, Headers2, _}} = Pong,
                     {"set-cookie", C2} = ?PL("set-cookie", Headers2),
                     C2;
                 {ok, {{_, _Code, _}, Headers4, _}} ->
                     CookieList = proplists:get_all_values("set-cookie",
                                                           Headers4),
                     C4 = get_right_cookie(CookieList),
                     C4
             end,
    Cookie.

%% log(String, File) ->
%%     _Return = filelib:ensure_dir(File),
%%     case file:open(File, [write]) of
%%         {ok, Id} ->
%%             io:fwrite(Id, "~s~n", [String]),
%%             file:close(Id);
%%         _ ->
%%             error
%%     end.
