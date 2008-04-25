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
    [hn1,hn2,hn3,hn4].

%% Test cases starts here.
%%------------------------------------------------------------------------------
hn1() -> [{userdata,[{doc,"Test Simple Hypernumber"}]}].
hn1(Config) when is_list(Config) ->
    Hn = "=hypernumber(\""++?HN_URL1++"/a1?hypernumber\")",
    hn_util:post(?HN_URL1++"/a1?attr","<create><formula>99</formula></create>","text/xml"),
    hn_util:post(?HN_URL2++"/a1?attr","<create><formula>"++Hn++"</formula></create>","text/xml"),
    "99" = hn_util:req(?HN_URL2++"/a1").

hn2() -> [{userdata,[{doc,"Test Hypernumber Change"}]}].
hn2(Config) when is_list(Config) ->
    Hn = "=hypernumber(\""++?HN_URL1++"/a1?hypernumber\")",
    hn_util:post(?HN_URL1++"/a1?attr","<create><formula>99</formula></create>","text/xml"),
    hn_util:post(?HN_URL2++"/a1?attr","<create><formula>"++Hn++"</formula></create>","text/xml"),
    hn_util:post(?HN_URL1++"/a1?attr","<create><formula>66</formula></create>","text/xml"),
    timer:sleep(500),
    "66" = hn_util:req(?HN_URL2++"/a1").
    
hn3() -> [{userdata,[{doc,"Test setting empty hynumber"}]}].
hn3(Config) when is_list(Config) ->
    Hn = "=hypernumber(\""++?HN_URL1++"/a1?hypernumber\")",
    hn_util:post(?HN_URL2++"/a1?attr","<create><formula>"++Hn++"</formula></create>","text/xml"),
    "0" = hn_util:req(?HN_URL2++"/a1").
    
hn4() -> [{userdata,[{doc,"Test setting then changing empty hynumber"}]}].
hn4(Config) when is_list(Config) ->
    Hn = "=hypernumber(\""++?HN_URL1++"/a1?hypernumber\")",
    hn_util:post(?HN_URL2++"/a1?attr","<create><formula>"++Hn++"</formula></create>","text/xml"),
    hn_util:post(?HN_URL1++"/a1?attr","<create><formula>66</formula></create>","text/xml"),
    timer:sleep(1000),
    "66" = hn_util:req(?HN_URL2++"/a1").
