%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(column_test_SUITE).

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

init_per_testcase(_TestCase, Config) -> bits:clear_db(),Config.
end_per_testcase(_TestCase, _Config) -> ok.

all() ->
    [column1,column2,columnlast,columnlast2].

%% Test cases starts here.
%%------------------------------------------------------------------------------
column1() ->  [{userdata,[{doc,"Description here"}]}].
column1(Config) when is_list(Config) ->
   test_util:expected(lists:duplicate(3,"a\nb\n1\n2"),post_data("1",{"post",
        [{"action","create"},{"value",[{"cell","a"},{"cell","b"},{"cell","1"},{"cell","2"}]}]},"")).

column2() -> [{userdata,[{doc,"Description here"}]}].
column2(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/test/a1?format=xml",
        "<post><action>create</action><value>99</value></post>","text/xml"),
    test_util:expected(lists:duplicate(3,"10\n99\n104\nError: divide by 0"),post_data("f",{"post",
        [{"action","create"},{"value",[{"cell","=5+5"},{"cell","=/test/a1"},
        {"cell","=/test/a1+5"},{"cell","=4/0"}]}]},"")).


columnlast() -> [{userdata,[{doc,"Description here"}]}].
columnlast(Config) when is_list(Config) ->
    post_data("a",{"post",[{"action","create"},{"value",[
        {"cell","1"},{"cell","2"},{"cell","3"},{"cell","4"}]}]},"&last"),

    D = util2:flatten_data({list},hn_util:req("http://127.0.0.1:9000/a1:g10")),    
    test_util:expected("1\n1\n1\n2\n2\n2\n3\n3\n3\n4\n4\n4",D).

columnlast2() -> [{userdata,[{doc,"Description here"}]}].
columnlast2(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/a1?format=xml",
        "<post><action>create</action><value>99</value></post>","text/xml"),
    post_data("a",{"post",[{"action","create"},{"value",[{"cell","1"}]}]},"&last"),

    D = util2:flatten_data({list},hn_util:req("http://127.0.0.1:9000/a")),    
    test_util:expected("99\n1\n1\n1",D).

%% Utilities
%%------------------------------------------------------------------------------
post_data(Cell,Data,Last) ->
    [post_data(Cell,Data,{list},Last),post_data(Cell,Data,{xml},Last),
     post_data(Cell,Data,{json},Last)].

post_data(Cell,Data,Format,Last) ->

    {PData,Type,FormatStr} = case Format of
        {list} -> {spriki:to_post(Data),"text/plain","list"};
        {xml}  -> {spriki:to_xml(Data), "text/xml",  "xml"};
        {json} -> {spriki:to_json(Data),"text/plain","json"}
    end,

    Url = "http://127.0.0.1:9000/"++Cell++"?format="++FormatStr++Last,
    D = hn_util:post(Url,PData,Type),

    ?F("Return : ~p~n",[D]),

    util2:flatten_data(Format,D).