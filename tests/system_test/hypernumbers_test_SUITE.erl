%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(hypernumbers_test_SUITE).

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

all() ->
    [hn1,hn2].

%% Test cases starts here.
%%------------------------------------------------------------------------------
hn1() -> [{userdata,[{doc,"Description here"}]}].
hn1(Config) when is_list(Config) ->
    Hn = "=hypernumber(\""++?HN_URL1++"/a1?hypernumber\")",
    hn_util:post(?HN_URL1++"/a1","<create><value>99</value></create>","text/xml"),
    hn_util:post(?HN_URL2++"/a1","<create><value>"++Hn++"</value></create>","text/xml"),
    Expected = "<cell><value>99</value></cell>",
    test_util:expected(Expected,hn_util:req(?HN_URL2++"/a1")).

hn2() -> [{userdata,[{doc,"Description here"}]}].
hn2(Config) when is_list(Config) ->
    Hn = "=hypernumber(\""++?HN_URL1++"/a1?hypernumber\")",
    hn_util:post(?HN_URL1++"/a1","<create><value>99</value></create>","text/xml"),
    hn_util:post(?HN_URL2++"/a1","<create><value>"++Hn++"</value></create>","text/xml"),
    hn_util:post(?HN_URL1++"/a1","<create><value>66</value></create>","text/xml"),
    Expected = "<cell><value>66</value></cell>",
    test_util:expected(Expected,hn_util:req(?HN_URL2++"/a1")).