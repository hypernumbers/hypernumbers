%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(types_test_SUITE).

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

all() -> [
    from_json1, json1, xml1,
    from_json2, json2, xml2,
    from_json3, json3, xml3,
    from_json4, json4, xml4,
    from_json5, json5, xml5,
    from_json6, json6, xml6,
    from_json7 ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
json1() -> [{userdata,[{doc,"Testing data type converter"}]}].
json1(Config) when is_list(Config) -> 
    Data = {root,[],[]},
    Json = "[\"root\",[]]",
    test_util:expected(Json,simplexml:to_json_string(Data)).

json2() -> [{userdata,[{doc,"Testing data type converter"}]}].
json2(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[]},
    Json = "[\"root\",[{\"attribute\":\"value\"}]]",
    test_util:expected(Json,simplexml:to_json_string(Data)).

json3() -> [{userdata,[{doc,"Testing data type converter"}]}].
json3(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"},{another,"attribute"}],[]},
    Json = "[\"root\",[{\"attribute\":\"value\"},{\"another\":\"attribute\"}]]",
    test_util:expected(Json,simplexml:to_json_string(Data)).

json4() -> [{userdata,[{doc,"Testing data type converter"}]}].
json4(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[{firstchild,[],[]},{secondchild,[],[]}]},
    Json = "[\"root\",[{\"attribute\":\"value\"},[\"firstchild\",[]],[\"secondchild\",[]]]]",
    test_util:expected(Json,simplexml:to_json_string(Data)).

json5() -> [{userdata,[{doc,"Testing data type converter"}]}].
json5(Config) when is_list(Config) -> 
    Data = {root,[],["test"]},
    Json = "[\"root\",[\"test\"]]",
    test_util:expected(Json,simplexml:to_json_string(Data)).

json6() -> [{userdata,[{doc,"Testing data type converter"}]}].
json6(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[{child,[],["3.1459"]},{child,[],
        [{anotherchild,[],["3.1459"]}]}]},
    Json = "[\"root\",[{\"attribute\":\"value\"},[\"child\",[\"3.1459\"]],"
        ++"[\"child\",[[\"anotherchild\",[\"3.1459\"]]]]]]",
    test_util:expected(Json,simplexml:to_json_string(Data)).

from_json1() -> [{userdata,[{doc,"Testing data type converter"}]}].
from_json1(Config) when is_list(Config) -> 
    Data = {root,[],[]},
    Json = "[\"root\",[]]",
    test_util:expected(Data,simplexml:from_json_string(Json)).

from_json2() -> [{userdata,[{doc,"Testing data type converter"}]}].
from_json2(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[]},
    Json = "[\"root\",[{\"attribute\":\"value\"}]]",
    test_util:expected(Data,simplexml:from_json_string(Json)).

from_json3() -> [{userdata,[{doc,"Testing data type converter"}]}].
from_json3(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"},{another,"attribute"}],[]},
    Json = "[\"root\",[{\"attribute\":\"value\"},{\"another\":\"attribute\"}]]",
    test_util:expected(Data,simplexml:from_json_string(Json)).

from_json4() -> [{userdata,[{doc,"Testing data type converter"}]}].
from_json4(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[{firstchild,[],[]},{secondchild,[],[]}]},
    Json = "[\"root\",[{\"attribute\":\"value\"},[\"firstchild\",[]],[\"secondchild\",[]]]]",
    test_util:expected(Data,simplexml:from_json_string(Json)).

from_json5() -> [{userdata,[{doc,"Testing data type converter"}]}].
from_json5(Config) when is_list(Config) -> 
    Data = {root,[],["test"]},
    Json = "[\"root\",[\"test\"]]",
    test_util:expected(Data,simplexml:from_json_string(Json)).

from_json6() -> [{userdata,[{doc,"Testing data type converter"}]}].
from_json6(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[{child,[],["3.1459"]},{child,[],
        [{anotherchild,[],["3.1459"]}]}]},
    Json = "[\"root\",[{\"attribute\":\"value\"},[\"child\",[\"3.1459\"]],"
        ++"[\"child\",[[\"anotherchild\",[\"3.1459\"]]]]]]",
    test_util:expected(Data,simplexml:from_json_string(Json)).

from_json7() -> [{userdata,[{doc,"Testing data type converter"}]}].
from_json7(Config) when is_list(Config) -> 
    Data = {create,[],[{value,[],["value"]}]},
    Json = "[\"create\",[[\"value\",[\"value\"]]]]",
    test_util:expected(Data,simplexml:from_json_string(Json)).

xml1() -> [{userdata,[{doc,"Testing data type converter"}]}].
xml1(Config) when is_list(Config) -> 
    Data = {root,[],[]},
    XML  = "<root/>",
    test_util:expected(XML,simplexml:to_xml_string(Data)).

xml2() -> [{userdata,[{doc,"Testing data type converter"}]}].
xml2(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[]},
    XML  = "<root attribute=\"value\"/>",
    test_util:expected(XML,simplexml:to_xml_string(Data)).

xml3() -> [{userdata,[{doc,"Testing data type converter"}]}].
xml3(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"},{another,"attribute"}],[]},
    XML  = "<root attribute=\"value\" another=\"attribute\"/>",
    test_util:expected(XML,simplexml:to_xml_string(Data)).

xml4() -> [{userdata,[{doc,"Testing data type converter"}]}].
xml4(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[{firstchild,[],[]},{secondchild,[],[]}]},
    XML  = "<root attribute=\"value\"><firstchild/><secondchild/></root>",
    test_util:expected(XML,simplexml:to_xml_string(Data)).

xml5() -> [{userdata,[{doc,"Testing data type converter"}]}].
xml5(Config) when is_list(Config) -> 
    Data = {root,[],["test"]},
    XML  = "<root>test</root>",
    test_util:expected(XML,simplexml:to_xml_string(Data)).

xml6() -> [{userdata,[{doc,"Testing data type converter"}]}].
xml6(Config) when is_list(Config) -> 
    Data = {root,[{attribute,"value"}],[{child,[],["3.1459"]},{child,[],
        [{anotherchild,[],["3.1459"]}]}]},
    XML  = "<root attribute=\"value\"><child>3.1459</child><child><anotherchild>3.1459"
        ++"</anotherchild></child></root>",
    test_util:expected(XML,simplexml:to_xml_string(Data)).