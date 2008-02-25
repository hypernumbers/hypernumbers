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

all() -> [range_xml,range_json].

%% Test cases starts here.
%%------------------------------------------------------------------------------
range_xml() ->  [{userdata,[{doc,"Description here"}]}].
range_xml(Config) when is_list(Config) ->

    Data = "<create><row><value>abc</value><value>=12+12</value><value>"
        ++"99</value><value>99</value></row><row><value>abc</value><value>"
        ++"=12+12</value><value>99</value><value>99</value></row></create>",

    Expected = {range,[],
        [{row,[{label,"2"}],[
            {column,[{label,"b"}],["abc"]},{column,[{label,"c"}],["24"]},
            {column,[{label,"d"}],["99"]},{column,[{label,"e"}],["99"]}]},
        {row,[{label,"3"}],[
            {column,[{label,"b"}],["abc"]},{column,[{label,"c"}],["24"]},
            {column,[{label,"d"}],["99"]},{column,[{label,"e"}],["99"]}]}]},

    Post   = hn_util:post(?HN_URL1++"/b2:e3?format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

range_json() ->  [{userdata,[{doc,"Description here"}]}].
range_json(Config) when is_list(Config) ->

    Data = "[\"create\",[[\"row\",[[\"value\",[\"abc\"]],[\"value\",[\"=12+12\"]]"
        ++",[\"value\",[\"99\"]],[\"value\",[\"99\"]]]],[\"row\",[[\"value\",[\"abc"
        ++"\"]],[\"value\",[\"=12+12\"]],[\"value\",[\"99\"]],[\"value\",[\"99\"]]]]]]",

    Expected = {range,[],
        [{row,[{label,"2"}],[
            {column,[{label,"b"}],["abc"]},{column,[{label,"c"}],["24"]},
            {column,[{label,"d"}],["99"]},{column,[{label,"e"}],["99"]}]},
        {row,[{label,"3"}],[
            {column,[{label,"b"}],["abc"]},{column,[{label,"c"}],["24"]},
            {column,[{label,"d"}],["99"]},{column,[{label,"e"}],["99"]}]}]},

    Post   = hn_util:post(?HN_URL1++"/b2:e3?format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),  
    test_util:expected(Expected,Result).
