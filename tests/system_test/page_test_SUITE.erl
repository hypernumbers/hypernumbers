%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(page_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").


%% Test server callback functions
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    code:add_path("../../../../../ebin"),
    production_boot:start(),
    Config.

end_per_suite(_Config) ->
    production_boot:stop(),
    ok.

init_per_testcase(_TestCase, Config) -> bits:clear_db(),Config.
end_per_testcase(_TestCase, _Config) -> ok.

all() -> [].
%%    [empty_pages_xml,empty_pages_json,
%%    pages_xml, pages_json].

%% Test cases starts here.
%%------------------------------------------------------------------------------
empty_pages_xml() -> [{userdata,[{doc,"Empty Call to ?pages"}]}].
empty_pages_xml(Config) when is_list(Config) ->
    test_util:expected("<dir path=\"/\"/>",
        hn_util:req(?HN_URL1++"/?pages&format=xml")).

empty_pages_json() -> [{userdata,[{doc,"Empty Call to ?pages"}]}].
empty_pages_json(Config) when is_list(Config) ->
    test_util:expected("[\"dir\",[{\"path\":\"/\"}]]",
        hn_util:req(?HN_URL1++"/?pages&format=json")).

pages_xml() -> [{userdata,[{doc,"Empty Call to ?pages"}]}].
pages_xml(Config) when is_list(Config) ->

    hn_util:post(?HN_URL1++"/test/random/a1?format=xml",
        "<create><value>234</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/another/long/test/a1?format=xml",
        "<create><value>534</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/test/random/again/a1?format=xml",
        "<create><value>456</value></create>","text/xml"),

    Expected = "<dir path=\"/\"><dir path=\"test\"><dir path=\"random\">"
        ++"<dir path=\"again\"></dir></dir></dir><dir path=\"another\">"
        ++"<dir path=\"long\"><dir path=\"test\"></dir></dir></dir></dir>",

    test_util:expected(Expected,
        hn_util:req(?HN_URL1++"/?pages&format=xml")).

pages_json() -> [{userdata,[{doc,"Empty Call to ?pages"}]}].
pages_json(Config) when is_list(Config) ->

    hn_util:post(?HN_URL1++"/test/random/a1?format=xml",
        "<create><value>234</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/another/long/test/a1?format=xml",
        "<create><value>534</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/test/random/again/a1?format=xml",
        "<create><value>456</value></create>","text/xml"),

    Expected = "[\"dir\",[{\"path\":\"/\"},[\"dir\",[{\"path\":\"test\"}"
        ++",[\"dir\",[{\"path\":\"random\"},[\"dir\",[{\"path\":\"again\"}"
        ++",\"\"]]]]]],[\"dir\",[{\"path\":\"another\"},[\"dir\",["
        ++"{\"path\":\"long\"},[\"dir\",[{\"path\":\"test\"},\"\"]]]]]]]]",

    test_util:expected(Expected,
        hn_util:req(?HN_URL1++"/?pages&format=json")).
