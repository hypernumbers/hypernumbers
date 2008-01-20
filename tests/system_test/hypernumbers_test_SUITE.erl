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
    [hn1,hn2,hn3,hn4,hn5].

%% Test cases starts here.
%%------------------------------------------------------------------------------
hn1() -> [{userdata,[{doc,"Description here"}]}].
hn1(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=99"),
    hn_util:post("http://gordonguthrie.org:9000/a1",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://127.0.0.1:9000/a1?hypernumber\")")),
    test_util:expected("99",hn_util:req("http://gordonguthrie.org:9000/a1")).

hn2() ->[{userdata,[{doc,"Description here"}]}].
hn2(Config) when is_list(Config) ->
    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=99"),
    hn_util:post("http://gordonguthrie.org:9000/a1",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://127.0.0.1:9000/a1?hypernumber\")")),
    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=543"),
    timer:sleep(1000),
    test_util:expected("543",hn_util:req("http://gordonguthrie.org:9000/a1")).

hn3() -> [{userdata,[{doc,"Description here"}]}].
hn3(Config) when is_list(Config) ->

    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=99"),
    hn_util:post("http://127.0.0.1:9000/a2",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://127.0.0.1:9000/a1?hypernumber\")")),
    hn_util:post("http://gordonguthrie.org:9000/a1",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://127.0.0.1:9000/a1?hypernumber\")")),
    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=543"),

    timer:sleep(1000),

    test_util:expected(["543","543"],
        [hn_util:req("http://gordonguthrie.org:9000/a1"),
        hn_util:req("http://127.0.0.1:9000/a2")]).

hn4() -> [{userdata,[{doc,"Description here"}]}].
hn4(Config) when is_list(Config) ->

    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=99"),
    hn_util:post("http://gordonguthrie.org:9000/a1",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://127.0.0.1:9000/a1?hypernumber\")")),
    hn_util:post("http://127.0.0.1:9000/a2",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://127.0.0.1:9000/a1?hypernumber\")")),
    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=543"),

    timer:sleep(1000),

    test_util:expected(["543","543"],
        [hn_util:req("http://gordonguthrie.org:9000/a1"),
        hn_util:req("http://127.0.0.1:9000/a2")]).

hn5() ->  [{userdata,[{doc,"Description here"}]}].
hn5(Config) when is_list(Config) ->

    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=99"),
    hn_util:post("http://gordonguthrie.org:9000/a1",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://127.0.0.1:9000/a1?hypernumber\")")),
    hn_util:post("http://127.0.0.1:9000/a2",
        "action=create&value="++yaws_api:url_encode(
        "=hypernumber(\"http://gordonguthrie.org:9000/a1?hypernumber\")")),
    hn_util:post("http://127.0.0.1:9000/a1","action=create&value=543"),

    timer:sleep(1000),

    test_util:expected(["543","543"],
        [hn_util:req("http://gordonguthrie.org:9000/a1"),
        hn_util:req("http://127.0.0.1:9000/a2")]).
