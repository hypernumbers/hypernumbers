
-module(compile_code).
-export([start/0]).

%% Compile wil generate warnings for all files
%% unless included here
-define(NO_WARNINGS,
        ["xfl_lexer.erl", "russian_lexer.erl",
         "french_lexer.erl", "german_lexer.erl",
          "italian_lexer.erl", "spanish_lexer.erl",
          "portuguese_lexer.erl", "superlex.erl", 
          "num_format_lexer.erl","cond_lexer.erl",
         "url_query_lexer.erl"]).

%% Directories containg source files
-define(DIRS,
        ["/lib/sgte/",
         "/lib/hypernumbers-1.0/",
         "/lib/formula_engine-1.0/",
         "/lib/introspection-1.0/",
         "/lib/mnesia_logger-1.0/",
         "/lib/read_excel-1.0/",
         "/lib/gui_generator-1.0/"]).

-define(EXTRA_ERL_FILES, ["src/generate_tests.erl"]).

start() ->
    
    Dir = get_root(),
    
    io:fwrite("~nStarting the compilation~n~n", []),
    code:add_pathz(Dir ++ "/lib/eunit/ebin"),
    code:add_pathz(Dir ++ "/lib/gettext/ebin"),

    % First set up the include file
    Inc_list = [{i, Dir ++ "include"},
                {i, Dir ++ "lib/eunit/include"},
                {i, Dir ++ "lib/gettext/include"},
                {i, Dir ++ "lib/read_excel-1.0/include"},
                {i, Dir ++ "lib/hypernumbers-1.0/include"},
                {i, code:lib_dir(xmerl)++"/include"}],

    % List of {ErlangFile, OutputDirectory} tuples.
    Fun = fun(X) ->
                  Fun2 = fun(Y) -> {Y, Dir ++ X ++ "ebin"} end,
                  Src = filelib:wildcard(Dir ++ X ++ "src/*.erl"),
                  Dot = filelib:wildcard(Dir ++ X ++ "src/.*.erl"),
                  lists:map(Fun2, Src -- Dot)
          end,
    Dirs = lists:flatten(lists:map(Fun, ?DIRS)),

    Fun3 = fun(X) -> {Dir++X,Dir++"ebin"} end,
    
    Extra = lists:map(Fun3, ?EXTRA_ERL_FILES),

    compile_funcs(Dirs++Extra, Inc_list),
    get_rel_file(),                
    delete_gen_html().

delete_gen_html() ->
    Dir = get_root()++"lib/hypernumbers-1.0/priv/docroot/hypernumbers/",
    [file:delete(X) || X <- filelib:wildcard(Dir++"*.html.*")].

get_root() ->

    [_File, _Ebin | Rest] =
        lists:reverse(string:tokens(code:which(compile_code), "/")),

    Pre = case os:type() of
              {win32,_} -> "";
              _         -> "/"
          end,

    Pre++string:join(lists:reverse(Rest),"/")++"/".

compile_funcs(List, Inc_list) ->
    New_list = [{X, [debug_info, {outdir, Y} | Inc_list]} || {X, Y} <- List],
    comp_lists(New_list,ok).

comp_lists([{File, Opt}|T], OldStatus) ->
    
    Append = case lists:member(filename:basename(File), ?NO_WARNINGS) of
                 true  -> [report_errors];
                 false -> [report_errors,report_warnings]
             end,
    Options = lists:append(Opt,Append),
    
    % Ensure output directory exists.
    [debug_info, {outdir, Dir} | _] = Options,
    filelib:ensure_dir(Dir ++ "/"),
    
    Comp = fun() -> NewStatus = compile:file(File, Options),
                    case NewStatus of
                        {ok, FileName} ->
                            io:fwrite("OK: ~s~n", [File]),
                            code:delete(FileName),
                            code:purge(FileName),
                            code:load_file(FileName),
                            comp_lists(T, OldStatus);
                        _Error ->
                            comp_lists(T, error)
                    end
           end,

    case uptodate(File, Dir) of
        false -> Comp();
        _     -> comp_lists(T, OldStatus)
    end;
comp_lists([], Status) ->
    io:fwrite("   Termination Status: ~p~n", [Status]),
    Status.

%% Is the beam older than the erl file? check the date of 
%% any included .hrl files
uptodate(File, Dir) ->

    % Find the beam corresponding to this erl file.
    Beam = Dir ++"/"++ filename:basename(File,".erl") ++ ".beam",
    
    case beam_lib:chunks(Beam,[abstract_code]) of
        {error,_,_} -> % beam doesn't exist, recompile
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
            filelib:last_modified(Beam) > Latest;
        
        _ -> false
    end.

get_vsn(Module) ->
    AppFile = code:lib_dir(Module)++"/ebin/"++atom_to_list(Module)++".app",
    {ok,[{application,_App,Attrs}]} = file:consult(AppFile),
    {value,{vsn,Vsn}} = lists:keysearch(vsn,1,Attrs),
    Vsn.

get_rel_file() ->

    F = lists:append(["{release, {\"hypernumbers\",\"1.0\"}, ",
                      "{erts,\"",erlang:system_info(version),"\"},"
                      "[{kernel,\"",get_vsn(kernel),"\"},",
                      "{stdlib,\"",get_vsn(stdlib),"\"},",
                      "{inets,\"",get_vsn(inets),"\"},",
                      "{crypto,\"",get_vsn(crypto),"\"},",
                      "{sasl,\"",get_vsn(sasl),"\"},",
                      "{mnesia,\"",get_vsn(mnesia),"\"},",
                      "{gettext,\"1.3.0\"},",
                      "{sgte,\"0.7.1\"},",
                      "{read_excel,\"1.0\"},",
                      "{starling_app,\"0.0.1\"},",
                      "{formula_engine,\"1.0\"},",
                      "{mochiweb,\"0.01\"},",
                      "{hypernumbers,\"1.0\"}]}."]),
    
    ok = file:write_file("hypernumbers.rel",F),
    ok = systools:make_script("hypernumbers",
                         [local,{path,["../lib/*/ebin","."]}]).



