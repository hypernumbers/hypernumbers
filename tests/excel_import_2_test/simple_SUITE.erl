-module(simple_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

-define(POST(Site, Path, Cell, Data), hn_post(Site, Path, Cell, Data)).
-define(GET(Site, Path, Cell), hn_get(Site, Path, Cell)).
-define(ASSERT_EQL(X, Y), assert_eql(X, Y)).

init_per_suite(Config) ->
    code:add_path("../../../../../ebin"),
    inets:start(),
    production_boot:start(),
    bits:clear_db(),
    Config.

end_per_suite(_Config) ->
    inets:stop(),
    production_boot:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [sheet1_b1,
sheet1_c2,
sheet1_b2,
sheet1_c3,
sheet1_b3].

    sheet1_b1() ->
        [{userdata, [{doc, ""}]}].

    sheet1_b1(Config) when is_list(Config) ->
        ?POST("http://127.0.0.1:9000", 
              "/simple/Sheet1/", 
              b1, 
              "This is a simple introductory test"),

        ?ASSERT_EQL("This is a simple introductory test", 
                    ?GET("http://127.0.0.1:9000", 
                         "/simple/Sheet1/", 
                         b1)).
    sheet1_c2() ->
        [{userdata, [{doc, ""}]}].

    sheet1_c2(Config) when is_list(Config) ->
        ?POST("http://127.0.0.1:9000", 
              "/simple/Sheet1/", 
              c2, 
              "1"),

        ?ASSERT_EQL("1.0", 
                    ?GET("http://127.0.0.1:9000", 
                         "/simple/Sheet1/", 
                         c2)).
    sheet1_b2() ->
        [{userdata, [{doc, ""}]}].

    sheet1_b2(Config) when is_list(Config) ->
        ?POST("http://127.0.0.1:9000", 
              "/simple/Sheet1/", 
              b2, 
              "Integer"),

        ?ASSERT_EQL("Integer", 
                    ?GET("http://127.0.0.1:9000", 
                         "/simple/Sheet1/", 
                         b2)).
    sheet1_c3() ->
        [{userdata, [{doc, ""}]}].

    sheet1_c3(Config) when is_list(Config) ->
        ?POST("http://127.0.0.1:9000", 
              "/simple/Sheet1/", 
              c3, 
              "1.2"),

        ?ASSERT_EQL("1.2", 
                    ?GET("http://127.0.0.1:9000", 
                         "/simple/Sheet1/", 
                         c3)).
    sheet1_b3() ->
        [{userdata, [{doc, ""}]}].

    sheet1_b3(Config) when is_list(Config) ->
        ?POST("http://127.0.0.1:9000", 
              "/simple/Sheet1/", 
              b3, 
              "Float"),

        ?ASSERT_EQL("Float", 
                    ?GET("http://127.0.0.1:9000", 
                         "/simple/Sheet1/", 
                         b3)).

%% Helper functions.

hn_post(Site, Path, Cell, Data__) ->
    Url = Site ++ Path ++ atom_to_list(Cell),
    PostData = "action=create&value=" ++ tconv:to_s(Data__), % FIXME: CGI escape?
    Data = {Url, [], "text/plain", PostData},
    {ok, {{V, 200, R}, H, Body}} = http:request(post, Data, [], []).


hn_get(Site, Path, Cell) ->
    Url = Site ++ Path ++ atom_to_list(Cell),
    {ok, {{V, 200, R}, H, Body}} = http:request(get, {Url, []}, [], []),
    stdext:text2num(Body). %% Assume it's all numbers for now.


assert_eql(X, Y) when is_integer(X) andalso is_float(Y) ->
    X * 1.0 == Y;
assert_eql(X, Y) when is_float(X) andalso is_integer(Y) ->
    X == Y * 1.0;
assert_eql(X, Y) ->
    X == Y.
    
