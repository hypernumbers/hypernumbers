%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(cell_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

%% Test server callback functions
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    code:add_path("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    Config.

end_per_suite(_Config) ->
    production_boot:stop(),
    ok.

init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

all() ->
    [cell1,cell2,cell3,cell4,cell5,cell6,cell7,cell8,cell9].

%% Test cases starts here.
%%------------------------------------------------------------------------------
cell1() -> [{userdata,[{doc,"Set /private to be private"}]}].
cell1(Config) when is_list(Config) ->
    test_util:expected(["abc","abc","abc"],post_data("a1",{"post",
        [{"action","create"},{"value","abc"}]})).

cell2() ->  [{userdata,[{doc,"Set /private to be private"}]}].
cell2(Config) when is_list(Config) ->
    test_util:expected(["5","5","5"],post_data("a1",{"post",
        [{"action","create"},{"value","5"}]})).

cell3() -> [{userdata,[{doc,"Set /private to be private"}]}].
cell3(Config) when is_list(Config) ->
    test_util:expected(["24","24","24"],post_data("a1",{"post",
        [{"action","create"},{"value","=12+12"}]})).

cell4() -> [{userdata,[{doc,"Set /private to be private"}]}].
cell4(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/mypage/a1?format=xml",
        "<post><action>create</action><value>10</value></post>","text/xml"),
    test_util:expected(["10","10","10"],post_data("a1",{"post",
        [{"action","create"},{"value","=/mypage/a1bas"}]})).

cell5() ->  [{userdata,[{doc,"Set /private to be private"}]}].
cell5(Config) when is_list(Config) ->
    test_util:expected(["24","24","24"],post_data("a1",{"post",
        [{"action","create"},{"value","=12 + 12"}]})).

cell6() -> [{userdata,[{doc,"Set /private to be private"}]}].
cell6(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/a1?format=xml",
        "<post><action>create</action><value>99</value></post>","text/xml"),
    test_util:expected(["99","99","99"],post_data("a1",{"post",
        [{"action","create"},{"value","=/a1"}]})).

cell7() -> [{userdata,[{doc,"Set /private to be private"}]}].
cell7(Config) when is_list(Config) ->
    test_util:expected(["divide by 0","divide by 0","divide by 0"],
        post_data("/testing/a1",{"post",[{"action","create"},{"value","=5/0"}]})).

cell8() -> [{userdata,[{doc,"Set /private to be private"}]}].
cell8(Config) when is_list(Config) ->
    Url = "http://127.0.0.1:9000/a1?format=xml",
    XML = "<post> <action>create</action> <value>5</value> </post>",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
         http:request(post,{Url,[],"text/xml",XML},[],[]),
    test_util:expected("5",util2:flatten_data({xml},Body)).

cell9() ->  [{userdata,[{doc,"Set /private to be private"}]}].
cell9(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/test/a1","action=create&value=99"),
    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=%3D/test/a1"),
    hn_util:post("http://127.0.0.1:9000/test/a1","action=create&value=50"),
    test_util:expected("50",hn_util:req("http://127.0.0.1:9000/a1")).

%% Utilities
%%------------------------------------------------------------------------------
post_data(Cell,Data) ->
    [post_data(Cell,Data,{xml}),post_data(Cell,Data,{list}),
     post_data(Cell,Data,{json})].

post_data(Cell,Data,Format) ->

    {PData,Type,FormatStr} = case Format of
        {list} -> {spriki:to_post(Data),"text/plain","list"};
        {xml}  -> {spriki:to_xml(Data), "text/xml",  "xml"};
        {json} -> {spriki:to_json(Data),"text/plain","json"}
    end,

    Url = "http://127.0.0.1:9000/"++Cell++"?format="++FormatStr,

    D = hn_util:post(Url,PData,Type),

    ?F("test output ~p~n",[D]),

    util2:flatten_data(Format,D).

