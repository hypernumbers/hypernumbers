%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(generate_tests).

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([run/0]).
-include("hypernumbers.hrl").
-include("spriki.hrl").

run() ->    
    Root = string:tokens(code:lib_dir(hypernumbers),"/"),
    [_Lib,_Dot,_Ext,_Ebin|Rest] = lists:reverse(Root),
    Tests = "/"++string:join(lists:reverse(Rest),"/")
        ++"/tests/",

    Files       = filelib:wildcard(Tests++"hn_files/*.json"),
    {ok,TmpTpl} = file:read_file(Tests++"system_test/test_SUITE.tpl"),
    Tpl         = binary_to_list(TmpTpl),

    lists:map(fun(X) -> gen_test(Tests,Tpl,X) end,Files),
    ok.

gen_test(Path,Tpl,Src) ->
    
    Name   = filename:basename(Src,".json"),
    Suite  = Name++"_SUITE",
    SysDir = Path++"system_test/",
    ActDir = SysDir++"actions/",
    ActionFile = ActDir++Name++"_actions.erl",
    
    A = case filelib:is_file(ActionFile) of
            true ->
                compile:file(ActionFile,[{outdir,ActDir}]),
                Name++"_actions:run(),";
            false -> 
                ""
        end,
    
    {ok, JsonTxt}   = file:read_file(Src),
    {struct, Json}  = hn_util:js_to_utf8(mochijson:decode(JsonTxt)),
    {struct, Cells} = ?pget("cell", Json),
    Count = length(Cells),

    Ref   = #refX{site="http://127.0.0.1:9000", path=[Name]},
    Root  = Ref#refX.site ++ hn_util:list_to_path(Ref#refX.path),
    
    Cases = gen_test_cases(Name, Name, Count),

    Names   = [ Name++"_A"++itol(X) || X <- lists:seq(2, Count) ],
    NameStr = string:join(Names, ","),

    Test  = ?FORMAT(Tpl,[Suite, ActDir, Root, Src,A,Ref#refX.site,
                        NameStr, Cases]),

    file:write_file(SysDir++Suite++".erl",Test).

gen_test_cases(Name, Path, N) ->
    Str = "~s(_Conf) -> ~n \"Success\" = get_val(#refX{path=[~p]"
        ++",obj = {cell,{1,~p}}}).~n",    
  
    [ ?FORMAT(Str,[Name++"_A"++itol(X), Path, X]) 
      || X <- lists:seq(2, N) ].

itol(X) ->
    integer_to_list(X).
