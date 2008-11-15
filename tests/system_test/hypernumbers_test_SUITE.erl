-module(hypernumbers_test_SUITE).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").
-include("ct.hrl").

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

all() ->
    [test_all].

test_page(Site,Path) ->
    ?INFO("Testing ~p ~p",[Site,Path]),
    Ref = #ref{site=Site,path=Path,ref={column,1}},
    {attr,[],Refs} = hn_yaws:get_page_attributes(Ref),
    F = fun({ref,Ref,[{value,[],[{string,[],[Val]}]}]}) ->
                case Val of
                    "Success" -> ok;
                    _Else     -> throw({failed_test,Ref})
                end;
           (_) -> ok
        end,
    lists:map(F,Refs),
    ok.

test_all(_Config) ->
    Path = string:tokens(code:lib_dir(hypernumbers),"/"),
    [_Lib,_Dot,_Ext,_Ebin|Rest] = lists:reverse(Path),
    Xml = "/"++string:join(lists:reverse(Rest),"/")++"/tests/xml_files/*.xml",
    Files = filelib:wildcard(Xml),
    F = fun(X) ->
                ?INFO("Importing ~p",[X]),
                {ok,Domain}= hn_import:hn_xml(X),
                NPage = hn_main:get_pages_under([]),
                lists:map(fun(Y) -> test_page(Domain,Y) end,NPage)
        end,
    lists:map(F,Files),
    ok.
