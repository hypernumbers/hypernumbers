%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(range_test_SUITE).

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

all() -> [range1,range2].

%% Test cases starts here.
%%------------------------------------------------------------------------------
range1() ->      [{userdata,[{doc,"Description here"}]}].
range1(Config) when is_list(Config) ->
    test_util:expected(lists:duplicate(3,"a\nb\n1\n2"),post_data("a1:b2",{"post",
        [{"action","create"},{"value",[{"cell","a"},{"cell","b"},{"cell","1"},{"cell","2"}]}]})).

range2() ->      [{userdata,[{doc,"Description here"}]}].
range2(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/test/a1?format=xml",
        "<post><action>create</action><value>99</value></post>","text/xml"),
    test_util:expected(lists:duplicate(3,"10\n99\n104\nError: divide by 0"),post_data("a1:b2",{"post",
        [{"action","create"},{"value",[{"cell","=5+5"},{"cell","=/test/a1"},
        {"cell","=/test/a1+5"},{"cell","=4/0"}]}]})).

%% Utilities
%%------------------------------------------------------------------------------
post_data(Cell,Data) ->
    [post_data(Cell,Data,{list}),post_data(Cell,Data,{xml}),
     post_data(Cell,Data,{json})].

post_data(Cell,Data,Format) ->

    {PData,Type,FormatStr} = case Format of
        {list} -> {spriki:to_post(Data),"text/plain","list"};
        {xml}  -> {spriki:to_xml(Data), "text/xml",  "xml"};
        {json} -> {spriki:to_json(Data),"text/plain","json"}
    end,

    Url = "http://127.0.0.1:9000/"++Cell++"?format="++FormatStr,
    D = hn_util:post(Url,PData,Type),
    
    util2:flatten_data(Format,D).

