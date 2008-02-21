%%%-----------------------------------------------------------------------------
%%% File        : compile_code.erl
%%% Author      : Gordon Guthrie <gordon@bulbasaur.politicaldiscussion.org>
%%% Description :
%%%
%%% Created     : 18 Jan 2004 by Gordon Guthrie
%%%               <gordon@bulbasaur.politicaldiscussion.org>
%%%
%%% Rewritten in a recursive functional style (as opposed to mock imperative)
%%% by GG 18th July 2005
%%%
%%% Rewritten again by Gordon Guthrie, cos it was still rubbish
%%%
%%%-----------------------------------------------------------------------------
-module(compile_code).

-export([start/0]).

start() ->
    compile().

compile() ->
    [_File,_Ebin|Rest] = lists:reverse(string:tokens(
                                         code:which(compile_code),"/")),

	Pre = case os:type() of
        {win32,_} -> ""; 
        _ ->         "/" 
    end,

    App_rt_dir = Pre++implode(lists:reverse(Rest),"/")++"/",

    io:fwrite("~nStarting the compilation~n~n", []),

    code:add_pathz(App_rt_dir ++ "/lib/eunit/ebin"),
    %% First set up the include file
    Inc_list = [{i, App_rt_dir ++ "/include"},
                {i, App_rt_dir ++ "/lib/read_excel-1.0/include"},
                {i, code:lib_dir(xmerl)++"/include"}],

    Dirs = 
        lists:flatten(lists:map(
                        fun(X) ->
                                lists:map(
                                  fun(Y) -> {Y,X++"ebin"} end,
                                  filelib:wildcard(X++"src/*.erl"))
                        end,
                        [App_rt_dir++"/lib/db_access-1.0/",
                         App_rt_dir++"/lib/engine-1.0/",
                         App_rt_dir++"/lib/hypernumbers.com-1.0/",
                         App_rt_dir++"/lib/formula_engine-1.0/",
                         App_rt_dir++"/lib/read_excel-1.0/",
                         App_rt_dir++"/lib/remoting-1.0/",
                         App_rt_dir++"/lib/utilities-1.0/",
                         App_rt_dir++"/lib/mochi-1.0/"])),
    
    Extra = [
             {App_rt_dir++"priv/muin/leex.erl",App_rt_dir++"ebin"},
             {App_rt_dir++"src/bits.erl",App_rt_dir++"ebin"},
             {App_rt_dir++"src/production_boot.erl",App_rt_dir++"ebin"},
             {App_rt_dir++"src/load_db.erl",App_rt_dir++"ebin"},
             {App_rt_dir++"src/test_util.erl",App_rt_dir++"ebin"}],
    
    compile_funcs(Dirs++Extra, Inc_list).

compile_funcs(List, Inc_list) ->
    New_list = [{X, [debug_info, {outdir, Y} | Inc_list]} || {X, Y} <- List],
    comp_lists(New_list).

comp_lists(List) -> comp_lists(List, ok).

comp_lists([{File, Opt}|T], OldStatus) ->

    %% leex generated files that generate a lot
    %% of warnings, disable report_warnings for them
    Gen = ["ru_fe.erl","muin_supd_lexer.erl","muin_lexer.erl"],    
    Append = case lists:member(filename:basename(File),Gen) of
        true -> [return_errors];
        false -> [return_errors,report_warnings]
    end,
    Options = lists:append(Opt,Append),

    ensure_outdir(Options),    
    NewStatus = compile:file(File, Options),
    case OldStatus of
        ok ->
            case NewStatus of
                {ok, FileName} ->
                    io:fwrite("OK: ~p~n", [File]),
                    _Purge_results1 = code:purge(FileName),
                    _Delete_results = code:delete(FileName),
                    _Purge_results2 = code:purge(FileName),
                    _Load_results = code:load_file(FileName),
                    comp_lists(T, ok);
                Error ->
                    io:fwrite("   Compile failure:    ~p~n", [File]),
                    io:fwrite("   Error is       :    ~p~n~n", [Error]),
                    comp_lists(T, error)
            end;
        Other ->
            case NewStatus of
                {ok, FileName2} ->
                    io:fwrite("OK: ~p~n", [File]),
                    _Delete_results = code:delete(FileName2),
                    _Purge_results = code:purge(FileName2),
                    _Load_results = code:load_file(FileName2),
                    %% This is the key - pass an error message on once
                    %% you get a single failure to compile
                    comp_lists(T, Other);
                Error ->
                    io:fwrite("   Compile failure:    ~p~n", [File]),
                    io:fwrite("   Error is       :    ~p~n~n", [Error]),
                    comp_lists(T, Other)
            end
    end;

comp_lists([], Status) ->
    io:fwrite("   Termination Status: ~p~n", [Status]),
	Status.

implode(Data,Seperator) ->
   lists:foldr(fun(X,[]) -> X; (X,Acc) -> X++Seperator++Acc end, "", Data).

%% Ensures that the output directory exists before compiling
ensure_outdir([debug_info, {outdir, Dir} | _]) ->
    filelib:ensure_dir(Dir++"/").
