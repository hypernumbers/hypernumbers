-module(compile_code).
-export([start/0, quick/0]).

%% Debuggin
-export([jslint_DEBUG/0,
         jslint_DEBUG/1]).

%% Compile will generate warnings for all files
%% unless included here
-define(NO_WARNINGS,
        ["xfl_lexer.erl", "russian_lexer.erl",
         "french_lexer.erl", "german_lexer.erl",
         "italian_lexer.erl", "spanish_lexer.erl",
         "portuguese_lexer.erl", "superlex.erl", 
         "num_format_lexer.erl","cond_lexer.erl",
         "url_query_lexer.erl"]).

%% Directories containing source files
-define(DIRS,
        ["/lib/sgte/",
         "/lib/hypernumbers-1.0/",
         "/lib/formula_engine-1.0/",
         "/lib/read_excel-1.0/"
        ]).

%% location of the jslint.js file relative to root
-define(JSLINT, "priv/jslint/jslint.js").

%% Directories contain javascript to be linted
-define(JSDIRS, [
                 "lib/hypernumbers-1.0/priv/core_install/docroot/hypernumbers"
                ]).

%% Javascript files to be ignored
-define(JSIGNORE, [
                   "ajaxfileupload.js",
                   "jquery-1.4.2.min.js",
                   "jquery.columnmanager.min.js",
                   "jquery-ext.js",
                   "jquery.filemenu.js",
                   "jquery.tablesorter.min.js",
                   "json2.js"
                   ]).

jslint_DEBUG(File) ->
    jslint3([get_root() ++ "lib/hypernumbers-1.0/priv/core_install/docroot/hypernumbers/"
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
    io:format("*************************************************~nJSLint for:~p~n~s~n",
              [filename:basename(H), Ret]),
    jslint3(T, JSLint).

start() ->
    build_standard(),
    build_release().

quick() ->
    build_standard().

build_standard() ->
    Dir = get_root(),
    
    % Add ebins for everything in /lib/ (eugh)
    [ code:add_pathz(X ++ "/ebin")
      || X <- filelib:wildcard(Dir++"/lib/*") ],
        
    % First set up the include file
    Inc_list = [{i, Dir ++ "lib/gettext/include"},
                {i, Dir ++ "lib/read_excel-1.0/include"},
                {i, Dir ++ "lib/hypernumbers-1.0/include"},
                {i, Dir ++ "lib/hypernumbers-1.0/src"},
                {i, code:lib_dir(xmerl)++"/include"}],

    % List of {ErlangFile, OutputDirectory} tuples.
    Fun = fun(X) ->
                  Fun2 = fun(Y) -> {Y, Dir ++ X ++ "ebin"} end,
                  Src = filelib:wildcard(Dir ++ X ++ "src/*.erl"),
                  Dot = filelib:wildcard(Dir ++ X ++ "src/.*.erl"),
                  lists:map(Fun2, Src -- Dot)
          end,
    
    Dirs = lists:flatten(lists:map(Fun, ?DIRS)),

    ok = compile_funcs(Dirs, Inc_list).

build_release() ->
    get_rel_file(),
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

        {ok,{_,[{abstract_code,{_,AC}}]}} ->
            F = fun({attribute,_Num,file,{Path,_}}) -> 
                        case filename:extension(Path) of
                            ".hrl" -> true;
                            _Else  -> false
                        end;
                   (_Else) -> 
                        false
                end,
            G = fun({attribute,_Num,file,{Path,_}}) -> Path end,
            H = fun(Path) -> filelib:last_modified(Path) end,

            Includes = lists:map(G,lists:filter(F,AC)),
            SrcFiles = [File|Includes],  
            Latest = lists:max(lists:map(H,SrcFiles)),

            % if the beam is newer than the last change to any
            % of the source files, don't need to compile
            case (filelib:last_modified(Beam) > Latest) of
                false -> io:format("~p needs to be recompiled~n", [File]),
                         false;
                true  -> true
            end;
        
        _Blah -> io:format("Recompile ~p because ~p~n", [File, _Blah]),
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
    Apps = [kernel, stdlib, inets, crypto, sasl, mnesia, ssl, public_key, gettext,
            sgte, read_excel, starling, formula_engine, mochiweb,
            hypernumbers],
    Rel  = make_rel_file("hypernumbers", "1.0", Apps),
    ok   = file:write_file("hypernumbers.rel", fmt("~p.", [Rel])),
    ok   = systools:make_script("hypernumbers",
                                [local,{path,["../lib/*/ebin","."]}]).

get_ssl_rel_file() ->
    Rel = make_rel_file("START SSL", "1.0", [kernel, stdlib, inets, ssl, crypto, public_key]),
    ok  = file:write_file("start_ssl.rel", fmt("~p.", [Rel])),
    ok  = systools:make_script("start_ssl", [local]).

%% short hand to format and flatten
-spec fmt(string(), list()) -> string().
fmt(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).

