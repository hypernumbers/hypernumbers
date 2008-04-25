%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(row_test_SUITE).

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

all() -> [
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
row_xml() -> [{userdata,[{doc,"Test Basic Post of a string, xml encoding"}]}].
row_xml(Config) when is_list(Config) -> 

    V1 = "abc",V2 = "99",V3 = "=12 + 12",V4 = "=/test/a1",
    Data = "<create><value>"++V1++"</value><value>"++V2++"</value>"
        ++"<value>"++V3++"</value><value>"++V4++"</value></create>",
    Expected = {row,[{label,"1"}],[
        {column,[{label,"a"}],[V1]},
        {column,[{label,"b"}],[V2]},
        {column,[{label,"c"}],["24"]},
        {column,[{label,"d"}],["0"]}]},
    Post   = hn_util:post(?HN_URL1++"/1?format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

row_json() -> [{userdata,[{doc,"Test Basic Post of a string, xml encoding"}]}].
row_json(Config) when is_list(Config) -> 

    V1 = "abc",V2 = "99",V3 = "=12 + 12",V4 = "=/test/a1",
    Data = "[\"create\",[[\"value\",[\""++V1++"\"]],[\"value\",[\""++V2++"\"]],"
        ++"[\"value\",[\""++V3++"\"]],[\"value\",[\""++V4++"\"]]]]",

    Expected = {row,[{label,"1"}],[
        {column,[{label,"a"}],[V1]},
        {column,[{label,"b"}],[V2]},
        {column,[{label,"c"}],["24"]},
        {column,[{label,"d"}],["0"]}]},

    Post   = hn_util:post(?HN_URL1++"/1?format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),  
    test_util:expected(Expected,Result).

last_xml() -> [{userdata,[{doc,"Test Basic Post of a string, xml encoding"}]}].
last_xml(Config) when is_list(Config) -> 

    V1 = "abc",V2 = "99",V3 = "=12 + 12",

    hn_util:post(?HN_URL1++"/1?format=xml&last",
        "<create><value>"++V1++"</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/1?format=xml&last",
        "<create><value>"++V2++"</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/1?format=xml&last",
        "<create><value>"++V3++"</value></create>","text/xml"),

    Expected = {row,[{label,"1"}],[
        {column,[{label,"a"}],[V1]},
        {column,[{label,"b"}],[V2]},
        {column,[{label,"c"}],["24"]}]},

    GET = hn_util:req(?HN_URL1++"/1?format=xml"),
    Result = simplexml:from_xml_string(GET),  
    test_util:expected(Expected,Result).

last2_xml() -> [{userdata,[{doc,"Test Basic Post of a string, xml encoding"}]}].
last2_xml(Config) when is_list(Config) -> 

    V1 = "abc",V2 = "99",V3 = "=12 + 12",

    hn_util:post(?HN_URL1++"/1?format=xml&last",
        "<create><value>"++V1++"</value><value>"++V3++"</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/1?format=xml&last",
        "<create><value>"++V2++"</value><value>"++V2++"</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/1?format=xml&last",
        "<create><value>"++V3++"</value><value>"++V1++"</value></create>","text/xml"),

    Expected = {row,[{label,"3"}],[
        {column,[{label,"a"}],["24"]},
        {column,[{label,"b"}],["abc"]}]},

    GET = hn_util:req(?HN_URL1++"/3?format=xml"),
    Result = simplexml:from_xml_string(GET),  
    test_util:expected(Expected,Result).
