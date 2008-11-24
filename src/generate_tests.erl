%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(generate_tests).

-export([run/0]).
-include("hypernumbers.hrl").
-include("spriki.hrl").

run() ->    
    Root = string:tokens(code:lib_dir(hypernumbers),"/"),
    [_Lib,_Dot,_Ext,_Ebin|Rest] = lists:reverse(Root),
    Tests = "/"++string:join(lists:reverse(Rest),"/")
        ++"/tests/",

    Files       = filelib:wildcard(Tests++"xml_files/*.xml"),
    {ok,TmpTpl} = file:read_file(Tests++"system_test/test_SUITE.tpl"),
    Tpl         = binary_to_list(TmpTpl),

    lists:map(fun(X) -> gen_test(Tests,Tpl,X) end,Files),
    ok.

gen_test(Path,Tpl,Src) ->
    
    Name   = filename:basename(Src,".xml"),
    Suite  = Name++"_SUITE",
    SysDir = Path++"system_test/",
    ActDir = SysDir++"actions/",
    ActionFile = ActDir++Name++"_actions.erl",
    
    A = case filelib:is_file(ActionFile) of
            true ->
                compile:file(ActionFile,[{outdir,ActDir}]),
                "basic_test_actions:run(),";
            false -> 
                ""
        end,
    
    {ok,TmpFile} = file:read_file(Src),
    {root,[{domain,Domain}],Pages} 
        = simplexml:from_xml_string(binary_to_list(TmpFile)),

    {ok,Ref} = hn_util:parse_url(Domain),
    Root  = hn_util:list_to_path(Ref#ref.path),
    List  = gen_test_list(Root,Pages,[]),
    Cases = lists:flatten(gen_test_cases(Name,List,[])),    
    Test  = ?FORMAT(Tpl,[Suite,ActDir,Src,A,Ref#ref.site,
                        gen_test_names(Name,List),Cases]),

    file:write_file(SysDir++Suite++".erl",Test).

gen_test_name(Name,Path,Cell) ->
    lists:concat([Name,"_",string:join(Path,"_"),"_",Cell]).

gen_test_names(Name,List) ->
    F = fun({P,V}) -> gen_test_name(Name,P,V) end,
    string:join(lists:map(F,List),",").

gen_test_cases(_Name,[],Acc) ->
    Acc;
gen_test_cases(Name,[{P,V}|List],Acc) ->
    Str = "~s(_Conf) -> ~n \"Success\" = get_val(#ref{path=~p"
                  ++",ref = {cell,{1,~p}}}).~n",    
    {_X,Y} = util2:strip_ref(V),
    Fun = ?FORMAT(Str,[gen_test_name(Name,P,V),P,Y]),
    gen_test_cases(Name,List,[Fun|Acc]).

gen_test_list(_Root,[],Acc) ->
    Acc;
gen_test_list(Root,[{page,[{path,Path}],Attr}|T],Acc) ->
    [{attr,[],Refs}] =  Attr,
    P = string:tokens(Root++Path,"/"),
    gen_test_list(Root,T,from_refs(P,Refs,Acc)).


from_refs(_Path,[],Acc) ->
    Acc;
from_refs(Path,[{ref,[Type,Ref],[{value,[],_Val}]}|T],Acc) ->
    case {Type,Ref} of
        {{type,"cell"},{ref,R}} ->
            case util2:strip_ref(R) of
                {1,_X} -> from_refs(Path,T,[{Path,R}|Acc]);
                _      -> from_refs(Path,T,Acc)
            end;
        _ -> from_refs(Path,T,Acc)
    end;
from_refs(Path,[_H|T],Acc) ->
    from_refs(Path,T,Acc).

