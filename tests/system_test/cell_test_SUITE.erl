%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(cell_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

-define(FXML,"?format=xml").
-define(FJSON,"?format=json").

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

all() -> [
    string_xml,  string_json,
    formula_xml, formula_json, 
    formula2_xml,formula2_json,
    ref_xml,     ref_json,
    ref2_xml,    ref2_json,
    ref3_xml,    ref3_json,
    ref4_xml,    ref4_json,
    xml_parse ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
string_xml() -> [{userdata,[{doc,"Test Basic Post of a string, xml encoding"}]}].
string_xml(Config) when is_list(Config) -> 
    Value    = "abc",
    Data     = "<create><value>"++Value++"</value></create>",
    Expected = {attr,[],[{ref,[{type,"cell"},{ref,"a1"}],[{value,[],[Value]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

string_json() -> [{userdata,[{doc,"Test Basic Post of a string, json encoding"}]}].
string_json(Config) when is_list(Config) -> 
    Value    = "abc",
    Data     = "[\"create\",[[\"value\",[\""++Value++"\"]]]]",
    Expected = {attr,[],[{ref,[{type,"cell"},{ref,"a1"}],[{value,[],[Value]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=json",Data,"text/plain"),
    ?F("test ~p~n",[Post]),
    Result = simplexml:from_json_string(Post),
    test_util:expected(Expected,Result).

formula_xml() -> [{userdata,[{doc,"Test Basic Post of a formula, xml encoding"}]}].
formula_xml(Config) when is_list(Config) -> 
    Value    = "=12+12",
    Data     = "<create><value>"++Value++"</value></create>",
    Expected = {attr,[],
        [{ref,[{type,"cell"},{ref,"a1"}],[{value,[],["24"]}]},
         {ref,[{type,"cell"},{ref,"a1"}],[{formula,[],["=12+12"]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

formula_json() -> [{userdata,[{doc,"Test Basic Post of a formula, json encoding"}]}].
formula_json(Config) when is_list(Config) -> 
    Value    = "=12+12",
    Data     = "[\"create\",[[\"value\",[\""++Value++"\"]]]]",
    Expected = {attr,[],
        [{ref,[{type,"cell"},{ref,"a1"}],[{value,[],["24"]}]},
         {ref,[{type,"cell"},{ref,"a1"}],[{formula,[],["=12+12"]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),
    test_util:expected(Expected,Result).

formula2_xml() -> [{userdata,[{doc,"Test Basic Post of a formula, xml encoding"}]}].
formula2_xml(Config) when is_list(Config) -> 
    Value    = "=12 + 12",
    Data     = "<create><value>"++Value++"</value></create>",
    Expected = {attr,[],
        [{ref,[{type,"cell"},{ref,"a1"}],[{value,[],["24"]}]},
         {ref,[{type,"cell"},{ref,"a1"}],[{formula,[],["=12+12"]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

formula2_json() -> [{userdata,[{doc,"Test Basic Post of a formula, json encoding"}]}].
formula2_json(Config) when is_list(Config) -> 
    Value    = "=12 + 12",
    Data     = "[\"create\",[[\"value\",[\""++Value++"\"]]]]",
    Expected = {attr,[],
        [{ref,[{type,"cell"},{ref,"a1"}],[{value,[],["24"]}]},
         {ref,[{type,"cell"},{ref,"a1"}],[{formula,[],["=12+12"]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),
    test_util:expected(Expected,Result).

ref_xml() -> [{userdata,[{doc,"Test Basic Post of a formula, xml encoding"}]}].
ref_xml(Config) when is_list(Config) -> 
    Value    = "=a1",
    Value2   = "99",
    hn_util:post(?HN_URL1++"/a1?attr&format=xml","<create><value>"
        ++Value2++"</value></create>","text/xml"),
    Data     = "<create><value>"++Value++"</value></create>",
    Expected = {attr,[],
              [{ref,[{type,"cell"},{ref,"a2"}],[{formula,[],["=A1"]}]},
               {ref,[{type,"cell"},{ref,"a2"}],[{value,[],["99"]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a2?attr&format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

ref_json() -> [{userdata,[{doc,"Test Basic Post of a formula, json encoding"}]}].
ref_json(Config) when is_list(Config) -> 
    Value    = "=a1",
    Value2   = "99",
    hn_util:post(?HN_URL1++"/a1?attr&format=xml",
        "<create><value>"++Value2++"</value></create>","text/xml"),
    Data     = "[\"create\",[[\"value\",[\""++Value++"\"]]]]",
    Expected = {attr,[],
              [{ref,[{type,"cell"},{ref,"a2"}],[{formula,[],["=A1"]}]},
               {ref,[{type,"cell"},{ref,"a2"}],[{value,[],["99"]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a2?attr&format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),
    test_util:expected(Expected,Result).

ref2_xml() -> [{userdata,[{doc,"Test Basic Post of a formula, xml encoding"}]}].
ref2_xml(Config) when is_list(Config) -> 
    Value    = "=/test/a1",
    Value2   = "99",
    hn_util:post(?HN_URL1++"/test/a1?attr&format=xml","<create><value>"
        ++Value2++"</value></create>","text/xml"),
    Data     = "<create><value>"++Value++"</value></create>",
    Expected =  {attr,[],
        [{ref,[{type,"cell"},{ref,"a2"}],[{formula,[],["=/TEST/A1"]}]},
         {ref,[{type,"cell"},{ref,"a2"}],[{value,[],["99"]}]}]},
    Post   = hn_util:post(?HN_URL1++"/a2?attr&format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

ref2_json() -> [{userdata,[{doc,"Test Basic Post of a formula, json encoding"}]}].
ref2_json(Config) when is_list(Config) -> 
    Value    = "=/test/a1",
    Value2   = "99",
    hn_util:post(?HN_URL1++"/test/a1?attr&format=xml",
        "<create><value>"++Value2++"</value></create>","text/xml"),
    Data     = "[\"create\",[[\"value\",[\""++Value++"\"]]]]",
    Expected = {cell,[],[{value,[],[Value2]}]},
    Post   = hn_util:post(?HN_URL1++"/a2?attr&format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),
    test_util:expected(Expected,Result).

ref3_json() -> [{userdata,[{doc,"Test Basic Post of a formula, json encoding"}]}].
ref3_json(Config) when is_list(Config) -> 
    Value    = "=a1",
    Data     = "[\"create\",[[\"value\",[\""++Value++"\"]]]]",
    Expected = {cell,[],[{value,[],["24"]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),
    test_util:expected(Expected,Result).

ref3_xml() -> [{userdata,[{doc,"Test Basic Post of a formula, xml encoding"}]}].
ref3_xml(Config) when is_list(Config) -> 
    Value    = "=a1",
    Data     = "<create><value>"++Value++"</value></create>",
    Expected = {cell,[],[{value,[],["24"]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

ref4_json() -> [{userdata,[{doc,"Test Basic Post of a formula, json encoding"}]}].
ref4_json(Config) when is_list(Config) -> 
    Value    = "=5/0",
    Data     = "[\"create\",[[\"value\",[\""++Value++"\"]]]]",
    Expected = {cell,[],[{value,[],["24"]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=json",Data,"text/plain"),
    Result = simplexml:from_json_string(Post),
    test_util:expected(Expected,Result).

ref4_xml() -> [{userdata,[{doc,"Test Basic Post of a formula, xml encoding"}]}].
ref4_xml(Config) when is_list(Config) -> 
    Value    = "=5/0",
    Data     = "<create><value>"++Value++"</value></create>",
    Expected = {cell,[],[{value,[],["24"]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

xml_parse() -> [{userdata,[{doc,"Test Basic Post of a formula, xml encoding"}]}].
xml_parse(Config) when is_list(Config) -> 
    Value    = "abc",
    Data     = "<create> <value>"++Value++"</value> </create>",
    Expected = {cell,[],[{value,[],[Value]}]},
    Post   = hn_util:post(?HN_URL1++"/a1?attr&format=xml",Data,"text/xml"),
    Result = simplexml:from_xml_string(Post),  
    test_util:expected(Expected,Result).

