-module(compile_code).

-export([
         start/0,
         quick/0,
         make_release_boot_scripts/0
        ]).

% exports for compiling
-export([
         build/1,
         build_lexer_parser/1,
         build_quick/1,
         clean/1
        ]).

% for upgrading ssl
-export([
         get_ssl_rel_file/0
        ]).

%% Debugging
-export([
         jslint_DEBUG/0,
         jslint_DEBUG/1
        ]).

%% Compile will generate warnings for all files
%% unless included here
-define(NO_WARNINGS,
        [
         "xfl_lexer.erl",
         "russian_lexer.erl",
         "french_lexer.erl",
         "german_lexer.erl",
         "italian_lexer.erl",
         "spanish_lexer.erl",
         "portuguese_lexer.erl",
         "superlex.erl",
         "num_format_lexer.erl",
         "cond_lexer.erl",
         "url_query_lexer.erl"
        ]).

%% Directories containing source files
-define(DIRS,
        [
         "/lib/sgte/",
         "/lib/hypernumbers-1.0/",
         "/lib/formula_engine-1.0/",
         "/lib/read_excel-1.0/",
         "/lib/sysmon-1.0/",
         "/lib/erlsha2/",
         "/lib/bert/",
         "/lib/gettext/",
         "/lib/mochiweb/",
         "/lib/sitemods/",
         "/lib/starling/",
         "/lib/twilio/",
         "/lib/load_testing/"
        ]).

%% location of the jslint.js file relative to root
-define(JSLINT, "priv/jslint/jslint.js").

%% Directories contain javascript to be linted
-define(JSDIRS, [
                 "lib/hypernumbers-1.0/priv/core_install/docroot/hypernumbers",
                 "lib/hypernumbers-1.0/priv/core_install/docroot/bootstrap/js/",
                 "lib/hypernumbers-1.0/priv/core_install/docroot/contacts/",
                 "lib/hypernumbers-1.0/priv/core_install/docroot/graphs/",
                 "lib/hypernumbers-1.0/priv/core_install/docroot/postmessage/",
                 "lib/hypernumbers-1.0/priv/core_install/docroot/webcomponents"
                ]).

%% Javascript files to be ignored
-define(JSIGNORE, [
                   "2.0.0-crypto-md5.js",
                   "2.0.0-crypto-sha256.js",
                   "ajaxfileupload.js",
                   "jquery-1.4.2.js",
                   "jquery-1.4.2.min.js",
                   "jquery-1.4.4.js",
                   "jquery-1.4.4.min.js",
                   "jquery-1.5.js",
                   "jquery-1.5.min.js",
                   "jquery-1.7.1.js",
                   "jquery-1.7.1.min.js",
                   "jquery.columnmanager.js",
                   "jquery.columnmanager.min.js",
                   "jquery.cookie.js",
                   "jquery.cookie.min.js",
                   "jquery-ext.js",
                   "jquery.filemenu.js",
                   "jquery.jstree.js",
                   "jquery.jstree.min.js",
                   "jquery.scrollbarWidth.js",
                   "jquery.scrollbarWidth.min.js",
                   "jquery.tablesorter.jfilterselect.js",
                   "jquery.tablesorter.jfilterselect.min.js",
                   "jquery.tablesorter.js",
                   "jquery.tablesorter.min.js",
                   "json2.js",
                   "json2.min.js",
                   "bootstrap.js",
                   "bootstrap.min.js",
                   "jquery.cleditor.js",
                   "jscolor.js",
                   "jquery.tooltip.js",
                   "jquery.tooltip.min.js",
                   "jquery.ui.potato.menu.js",
                   "jquery.tabs.js",
                   "fb_lang.js",
                   "finder.js",
                   "ba-debug.js",
                   "ba-postmessage.js",
                   "froogaloop.js"
                  ]).

jslint_DEBUG(File) ->
    jslint3([get_root() ++ "lib/hypernumbers-1.0/priv/core_install"
             ++ "/docroot/"
             ++ File],
            get_root() ++ ?JSLINT).

jslint_DEBUG() -> jslint().

jslint() ->
    [ok = jslint2(get_root() ++ X) || X <- ?JSDIRS],
    ok.

jslint2(Dir) ->
    Files = lists:sort(filelib:wildcard(Dir ++ "/*.js")),
    Fun = fun(X) ->
                  X2 = filename:basename(X),
                  case lists:member(X2, ?JSIGNORE) of
                       true  -> false;
                       false -> true
                   end
          end,
    Files2 = lists:filter(Fun, Files),
    ok = jslint3(Files2, get_root() ++ ?JSLINT).

jslint3([], _JSLint)     -> ok;
jslint3([H | T], JSLint) ->
    io:format("~n~n~nAbout to lint ~p~n", [H]),
    Cmd = "rhino " ++ JSLint ++ " " ++ H,
    Ret = os:cmd(Cmd),
    io:format("*************************************************~n"
              ++"JSLint for:~p~n~s~n",
              [filename:basename(H), Ret]),
    jslint3(T, JSLint).

start() ->
    io:format("about to build standard~n"),
    build_standard(),
    io:format("about to build release~n"),
    build_release().

quick() ->
    build_standard().

build_standard() ->
    Dir = get_root(),

    add_libs_to_path(),

    % First set up the include file
    Inc_list = [
                {i, Dir ++ "lib/gettext/include"},
                {i, Dir ++ "lib/load_testing/include"},
                {i, Dir ++ "lib/read_excel-1.0/include"},
                {i, Dir ++ "lib/hypernumbers-1.0/include"},
                {i, Dir ++ "lib/sysmon-1.0/include"},
                {i, Dir ++ "lib/twilio/include"},
                {i, code:lib_dir(xmerl)++"/include"}
               ],


    % List of {ErlangFile, OutputDirectory} tuples.
    Fun = fun(X) ->
                  Fun2 = fun(Y) -> {Y, Dir ++ X ++ "ebin"} end,
                  Src = filelib:wildcard(Dir ++ X ++ "src/*.erl"),
                  Dot = filelib:wildcard(Dir ++ X ++ "src/.*.erl"),
                  lists:map(Fun2, Src -- Dot)
          end,

    Dirs = lists:flatten(lists:map(Fun, ?DIRS)),

    ok = compile_funcs(Dirs, Inc_list).

add_libs_to_path() ->
    Dir = get_root(),
    % Add ebins for everything in /lib/ (eugh)
    [code:add_pathz(X ++ "/ebin")
      || X <- filelib:wildcard(Dir++"/lib/*")],
    ok.

make_release_boot_scripts() ->
    add_libs_to_path(),
    build_release().

build_release() ->
    get_rel_file(),
    get_debug_rel_file(),
    get_no_mnesia_debug_rel_file(),
    get_ssl_rel_file().

get_root() ->
    [_File, _Ebin | Rest] =
        lists:reverse(string:tokens(code:which(compile_code), "/")),

    Pre = case os:type() of
              {win32,_} -> "";
              _         -> "/"
          end,

    Pre++string:join(lists:reverse(Rest),"/")++"/".

compile_funcs(List, Inc_list) ->
    [ ok = compile({X, [debug_info, {outdir, Y} | Inc_list]})
      || {X, Y} <- List ],
    ok.

compile({File, Opt}) ->
    Append = case lists:member(filename:basename(File), ?NO_WARNINGS) of
                 true  -> [report_errors];
                 false -> [report_errors, report_warnings]
             end,
    Options = lists:append(Opt,Append),

    % Ensure output directory exists.
    [debug_info, {outdir, Dir} | _] = Options,
    filelib:ensure_dir(Dir ++ "/"),

    case uptodate(File, Dir) of
        false -> compile(File, Options);
        _     -> ok
    end.

compile(File, Options) ->
    case compile:file(File, Options) of
        {ok, FileName} ->
            io:fwrite("OK: ~s~n", [File]),
            _Del = code:delete(FileName),
            _Purge = code:purge(FileName),
            _Load = code:load_file(FileName),
            ok;
        _Error ->
            erlang:halt(0)
    end.

%% Is the beam older than the erl file? check the date of
%% any included .hrl files
uptodate(File, Dir) ->

    % Find the beam corresponding to this erl file.
    Beam = Dir ++"/"++ filename:basename(File,".erl") ++ ".beam",

    case beam_lib:chunks(Beam, [abstract_code]) of
        {error,_,_} -> % beam doesn't exist, recompile
            io:format("Beam for ~p doesn't exist - recompile~n", [File]),
            false;

        {ok, {_, [{abstract_code,{_, AC}}]}} ->
            F = fun({attribute, _Num, file, {Path, _}}) ->
                        case filename:extension(Path) of
                            ".hrl" -> true;
                            _Else  -> false
                        end;
                   (_Else) ->
                        false
                end,
            G = fun({attribute, _Num, file, {Path, _}}) -> Path end,
            H = fun(Path) -> filelib:last_modified(Path) end,

            Includes = lists:map(G, lists:filter(F, AC)),
            SrcFiles = [File | Includes],
            Latest = lists:max(lists:map(H, SrcFiles)),

            % if the beam is newer than the last change to any
            % of the source files, don't need to compile
            case (filelib:last_modified(Beam) > Latest) of
                false -> io:format("~p needs to be recompiled~n", [File]),
                         false;
                true  -> true
            end;

        Other -> io:format("Recompile ~p~n- because ~p~n", [File, Other]),
                 false
    end.

%% given an application name, return its version (based on reading its .app)
-spec get_vsn(atom()) -> string().
get_vsn(App) ->
    load(App),
    {ok, Vsn} = application:get_key(App, vsn),
    Vsn.

load(App) ->
    case application:load(App) of
        ok                           -> ok;
        {error, {already_loaded, _}} -> ok;
        E                            -> erlang:exit(E)
    end.

%% build the release description
-spec make_rel_file(string(), string(), list()) -> tuple().
make_rel_file(App, Version, Deps) ->
    {release,
     {App, Version}, {erts, erlang:system_info(version)},
     [ {X, get_vsn(X)} || X <- Deps ]
    }.

get_rel_file() ->
    Apps = [kernel, stdlib, inets, crypto, sasl, mnesia, ssl, public_key,
            gettext, sgte, read_excel, sysmon, starling, formula_engine,
            erlsha2, twilio, mochiweb, bert, hypernumbers, load_testing],
    Rel  = make_rel_file("hypernumbers", "1.0", Apps),
    ok   = file:write_file("hypernumbers.rel", fmt("~p.", [Rel])),
    ok   = systools:make_script("hypernumbers",
                                [local,{path,["../lib/*/ebin","."]}]).

get_no_mnesia_debug_rel_file() ->
    Rel = make_rel_file("NO_MNESIA", "1.0", [kernel, stdlib, inets, ssl,
                                             crypto, public_key]),
    ok  = file:write_file("no_mnesia.rel", fmt("~p.", [Rel])),
    ok  = systools:make_script("no_mnesia", [local]).

get_debug_rel_file() ->
    Rel = make_rel_file("DEBUG", "1.0", [kernel, stdlib, inets, ssl,
                                             crypto, public_key, mnesia]),
    ok  = file:write_file("debug.rel", fmt("~p.", [Rel])),
    ok  = systools:make_script("debug", [local]).

get_ssl_rel_file() ->
    Rel = make_rel_file("START SSL", "1.0", [kernel, stdlib, inets, ssl,
                                             crypto, public_key]),
    ok  = file:write_file("start_ssl.rel", fmt("~p.", [Rel])),
    ok  = systools:make_script("start_ssl", [local]).

%% short hand to format and flatten
-spec fmt(string(), list()) -> string().
fmt(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).

clean(Root) ->
    Files = filelib:wildcard(Root ++ "lib/*/ebin/*.beam") ++
        filelib:wildcard("ebin/*.beam"),
    [file:delete(F) || F <- Files],
    ok.

build(Root) ->
    clean(Root),
    io:format("Compiling Dependencies ...~n"),
    filelib:ensure_dir("lib/mochiweb/ebin/"),
    filelib:ensure_dir("lib/hypernumbers-1.0/ebin/"),
    filelib:ensure_dir("ebin/"),
    io:format("...building mochiweb~n"),
    file:set_cwd("lib/mochiweb"),
    os:cmd("make"),
    file:set_cwd(Root),
    file:set_cwd("lib/gettext"),
    io:format("...building gettext~n"),
    os:cmd("make"),
    file:set_cwd(Root),
    file:set_cwd("lib/starling"),
    io:format("...building starling~n"),
    io:format(os:cmd("rake")),
    file:set_cwd(Root),
    file:set_cwd("lib/bert"),
    io:format("...building bert~n"),
    io:format(os:cmd("rake")),
    file:set_cwd(Root),
    file:set_cwd("lib/twilio"),
    io:format("...building twilio~n"),
    io:format(os:cmd("./rebar compile")),
    file:set_cwd(Root),
    file:set_cwd("lib/erlsha2"),
    io:format("...building erlsha2~n"),
    io:format(os:cmd("./rebar compile")),
    file:set_cwd(Root),

    % build the lexer-parsers
    io:format("...building lexer-parser~n"),
    make_lexer_parser(Root),
    minify(Root),
    make_ms_util(Root),
    file:set_cwd(Root++"/ebin"),
    io:format("...now compile all the actual code~n"),
    compile_code:start(),
    check_console_log(Root).

build_lexer_parser(Root) ->
    make_ms_util(Root),
    make_lexer_parser(Root),
    minify(Root),
    make_quick(Root),
    check_console_log(Root).

build_quick(Root) ->
    make_ms_util(Root),
    minify(Root),
    make_quick(Root),
    check_console_log(Root).

make_lexer_parser(Root) ->
    file:set_cwd("lib/formula_engine-1.0/priv/"),
    os:cmd("./generate.escript"),
    file:set_cwd(Root).

make_quick(Root) ->
    file:set_cwd(Root++"/ebin"),
    compile_code:quick(),
    file:set_cwd(Root).

minify(Root) ->
    Dir = Root ++ "/lib/hypernumbers-1.0/priv/core_install/minify.specs/",
    Prefix = Root ++ "/lib/hypernumbers-1.0/priv/core_install/docroot",
    Files = filelib:wildcard(Dir ++ "*.specs"),
    min2(Files, Prefix),
    ok.

min2([], _Prefix) ->
    ok;
min2([H | T], Prefix) ->
    File = filename:basename(H),
    [View, "minify", "specs"] = string:tokens(File, "."),
    {ok, [Specs]} = file:consult(H),
    min3(Specs, View, Prefix),
    min2(T, Prefix).

min3([], _View, _Prefix) ->
    ok;
min3([{Where, What} | T], View, Prefix) ->
    min4(What, View, Where, Prefix),
    min3(T, View, Prefix).

min4([], _View, _Where, _Prefix) ->
    ok;
min4([{Type, Files} | T], View, Where, Prefix) ->
    min5(Files, View, Type, Where, Prefix, []),
    min4(T, View, Where, Prefix).

% fake ending - if the Acc is [] just delete the old version of the file
% if it exists
min5([], View, Type, Where, Prefix, []) ->
    File = string:join([View, atom_to_list(Where), atom_to_list(Type)], "."),
    % we don't care if the file exists when we delete it so don't check
    % the return value
    _ = file:delete(Prefix ++ "/" ++ File);
min5([], View, Type, Where, Prefix, Acc) ->
    Contents = lists:reverse(Acc),
    File = string:join([View, atom_to_list(Where), atom_to_list(Type)], "."),
    ok = file:write_file(Prefix ++ "/" ++ File, lists:flatten(Contents));
min5([{Size, File} | T], View, Type, Where, Prefix, Acc) ->
    {ok, Contents} = file:read_file(Prefix ++ File),
    NewAcc = min6(File, Contents, Size),
    min5(T, View, Type, Where, Prefix, [NewAcc | Acc]).

min6(File, Contents, max) ->
    io_lib:format("/*~n~s~n*/~n~n~s~n", [File, Contents]);
min6(_File, _Contents, min) ->
    exit("yeah, not minifying yet...").

make_ms_util(Root) ->
    file:set_cwd(Root),
    compile:file("priv/ms_util/make_ms_util.erl",
                 [{outdir, "lib/hypernumbers-1.0/ebin"}, debug_info,
                  {i, "lib/hypernumbers-1.0/include"}]),
    code:add_path("lib/hypernumbers-1.0/ebin"),
    make_ms_util:make(),
    {ok, _Bytes} =
        file:copy("ms_util2.erl", "lib/hypernumbers-1.0/src/ms_util2.erl"),
    ok = file:delete("ms_util2.erl").

check_console_log(Root) ->
    file:set_cwd(Root++"/lib/hypernumbers-1.0/priv/core_install/docroot/"),
    print_msgs(string:tokens(os:cmd("grep -R -l console.log *"), "\n")).

print_msgs(List) ->
    io:format("Console.log in:~n"),
    [io:format("~p~n", [X]) || X <- List].
