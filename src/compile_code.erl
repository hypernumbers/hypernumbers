
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
        ["/lib/hypernumbers-1.0/",
         "/lib/formula_engine-1.0/",
         "/lib/read_excel-1.0/",
         "/lib/mochi-1.0/"]).

-define(EXTRA_ERL_FILES, []).

start() ->
    
    get_rel_file(),
    
    [_File, _Ebin | Rest] =
        lists:reverse(string:tokens(code:which(compile_code), "/")),

    Pre = case os:type() of
              {win32,_} -> "";
              _         -> "/"
          end,
    
    App_rt_dir = Pre++string:join(lists:reverse(Rest),"/")++"/",

    io:fwrite("~nStarting the compilation~n~n", []),
    code:add_pathz(App_rt_dir ++ "/lib/eunit/ebin"),

    %% First set up the include file
    Inc_list = [{i, App_rt_dir ++ "/include"},
                {i, App_rt_dir ++ "/lib/eunit/include"},
                {i, App_rt_dir ++ "/lib/read_excel-1.0/include"},
                {i, App_rt_dir ++ "/lib/hypernumbers-1.0/include"},
                {i, App_rt_dir ++ "/lib/yaws-1.76/include"},
                {i, code:lib_dir(xmerl)++"/include"}],

    %% List of {ErlangFile, OutputDirectory} tuples.
    Dirs = lists:flatten(lists:map(fun(X) ->
                                           lists:map(fun(Y) -> {Y, App_rt_dir ++ X ++ "ebin"} end,
                                                     filelib:wildcard(App_rt_dir ++ X ++ "src/*.erl"))
                                   end,
                                   ?DIRS)),
    
    Extra = lists:map(
              fun(X) ->
                      {App_rt_dir++X,App_rt_dir++"ebin"}
              end,
              ?EXTRA_ERL_FILES),
    
    compile_funcs(Dirs++Extra, Inc_list).

compile_funcs(List, Inc_list) ->
    New_list = [{X, [debug_info, {outdir, Y} | Inc_list]} || {X, Y} <- List],
    comp_lists(New_list,ok).

comp_lists([{File, Opt}|T], OldStatus) ->

    Append = case lists:member(filename:basename(File), ?NO_WARNINGS) of
                 true  -> [report_errors];
                 false -> [report_errors,report_warnings]
             end,
    Options = lists:append(Opt,Append),

    %% Ensure output directory exists.
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

    %% Find the beam corresponding to this erl file.
    Beam = Dir ++"/"++ filename:basename(File,".erl") ++ ".beam",
    
    case beam_lib:chunks(Beam,[abstract_code]) of
        {error,_,_} -> %% beam doesnt exist, recompile
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

            %% if the beam is newer than the last change to any
            %% of the source files, dont need to compile
            filelib:last_modified(Beam) > Latest
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
                      "{yaws, \"1.76\", load},",
                      "{read_excel,\"1.0\"},",
                      "{starling_app,\"0.0.1\"},",
                      "{formula_engine,\"1.0\"},",
                      "{mochi,\"1.0\"},",
                      "{hypernumbers,\"1.0\"}]}."]),
    
    file:write_file("hypernumbers.rel",F),
    systools:make_tar("hypernumbers",[{erts,"/usr/local/lib/erlang"},{path,["../lib/*/ebin"]}]),
    systools:make_script("hypernumbers",[local,{path,["../lib/*/ebin"]}]).
