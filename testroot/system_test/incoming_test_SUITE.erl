%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(incoming_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

init_per_suite(Config) ->
    code:add_path("../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    Url = "http://127.0.0.1:9000",
    hn_util:post(Url++"/mypage/a1?format=xml",
        "<post><action>create</action><value>=/from1/a1</value></post>","text/xml"),
    hn_util:post(Url++"/mypage/a2?format=xml",
        "<post><action>create</action><value>=/from2/a1+/from3/a1</value></post>","text/xml"),

    Config.

end_per_suite(_Config) ->
    production_boot:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.


all() ->
    [
        test_incoming1,
        test_incoming2,
        test_incoming3,
        test_incoming4
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
test_incoming1() ->  [{userdata,[{doc,"Set /private to be private"}]}].
test_incoming1(Config) when is_list(Config) ->

    Rtn = "http://127.0.0.1:9000\n/mypage/\n1\n1\nhttp://127.0.0.1:"
        ++"9000\n/from1/\n1\n1",

    test_util:expected(lists:duplicate(3,Rtn),
        [get_data("mypage/a1",{xml}),get_data("mypage/a1",{list}),
         get_data("mypage/a1",{json})]).


test_incoming2() -> [{userdata,[{doc,"Set /private to be private"}]}].
test_incoming2(Config) when is_list(Config) ->

    Rtn = "http://127.0.0.1:9000\n/mypage/\n2\n1\nhttp://127.0.0."
        ++"1:9000\n/from3/\n1\n1\nhttp://127.0.0.1:9000\n/mypage/\n2\n1\nhttp://"
        ++"127.0.0.1:9000\n/from2/\n1\n1",

    test_util:expected(lists:duplicate(3,Rtn),
        [get_data("mypage/a2",{xml}),get_data("mypage/a2",{list}),
         get_data("mypage/a2",{json})]).

test_incoming3() -> [{userdata,[{doc,"Set /private to be private"}]}].
test_incoming3(Config) when is_list(Config) ->

    Rtn = "http://127.0.0.1:9000\n/mypage/\n2\n1\nhttp://127.0.0.1:9000"
        ++"\n/from2/\n1\n1\nhttp://127.0.0.1:9000\n/mypage/\n2\n1\nhttp://127.0.0.1:"
        ++"9000\n/from3/\n1\n1\nhttp://127.0.0.1:9000\n/mypage/\n1\n1\nhttp://127.0."
        ++"0.1:9000\n/from1/\n1\n1",

    test_util:expected(lists:duplicate(3,Rtn),
        [get_data("mypage/",{xml}),get_data("mypage/",{list}),
         get_data("mypage/",{json})]).

test_incoming4() -> [{userdata,[{doc,"Set /private to be private"}]}].
test_incoming4(Config) when is_list(Config) ->
    test_util:expected(lists:duplicate(3,"empty"),
        [get_data("mypage/a3",{xml}),get_data("mypage/a3",{list}),
         get_data("mypage/a3",{json})]).

get_data(Cell,Format) ->
    FormatStr = case Format of
        {list} -> "list"; {xml} -> "xml";
        {json} -> "json"; {json,nocallback} -> "json&nocollback" end,
    Url = "http://127.0.0.1:9000/"++Cell++"?format="++FormatStr++"&incoming",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        http:request(get,{Url, []}, [], []),

    util2:flatten_data(Format,Body).

