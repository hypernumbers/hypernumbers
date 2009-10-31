%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(generate_tests).

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).

-export([run/0]).
-include("hypernumbers.hrl").
-include("spriki.hrl").

run() ->    
    Root         = get_root(),
    Tests        = Root ++ "/tests/",
    Files        = filelib:wildcard(Tests++"hn_files/*.json"),
    {ok, TmpTpl} = file:read_file(Tests++"system_test/test_SUITE.tpl"),
    Tpl          = binary_to_list(TmpTpl),
    [ gen_test(Tests,Tpl,X) || X <- Files ],
    ok.

get_root() ->
    [_File, _Ebin | Rest] =
        lists:reverse(string:tokens(code:which(generate_tests), "/")),
    Pre = case os:type() of
              {win32,_} -> "";
              _         -> "/"
          end,
    Pre++string:join(lists:reverse(Rest),"/")++"/".

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
    
    {ok, JsonTxt}     = file:read_file(Src),
    {struct, Json}    = hn_util:js_to_utf8(mochijson:decode(JsonTxt)),
    {struct, Cells}   = ?pget("cell", Json),
    {struct, HeadRow} = ?pget("1", Cells),
    {struct, A1}      = ?pget("1", HeadRow),
    
    Count = case ?pget("value", A1) of
                "NOTESTS" -> 0;
                Range ->                    
                    {range, {_,X1,_,X2}} = hn_util:parse_attr(Range),
                     X2-X1+2
            end,
    
    Ref   = #refX{site="http://127.0.0.1:9000", path=[Name]},
    Root  = Ref#refX.site ++ hn_util:list_to_path(Ref#refX.path),
    
    Cases = gen_test_cases(Name, Name, Count),
    Names = gen_names(Name, Count),

    Test  = ?FORMAT(Tpl, [Suite, ActDir, Root, Src, A, Ref#refX.site,
                          Names, Cases]),

    file:write_file(SysDir++Suite++".erl",Test).

gen_names(_Name, 0) ->
    [];
gen_names(Name, Count) ->
    Str = [ Name++"_A"++itol(X) || X <- lists:seq(2, Count) ],
    string:join(Str, ",").

gen_test_cases(_Name, _Path, 0) ->
    [];
gen_test_cases(Name, Path, N) ->
    Str = "~s(_Conf) -> ~n \"Success\" = get_val(#refX{path=[~p],obj ="
            "{cell,{1,~p}}}).~n",
  
    [ ?FORMAT(Str,[Name++"_A"++itol(X), Path, X]) 
      || X <- lists:seq(2, N) ].

itol(X) ->
    integer_to_list(X).
