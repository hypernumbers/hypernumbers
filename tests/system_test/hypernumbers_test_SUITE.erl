%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(hypernumbers_test_SUITE).

-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

-define(str(Str,List),lists:flatten(io_lib:format(Str,List))).
-define(POST,"<create><formula>~s</formula></create>").
-define(URL,"~s/~s?attr").

%% Test server callback functions
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) -> hn_loaddb:create_db(),Config.
end_per_testcase(_TestCase, _Config) -> ok.

all() ->
    [test_post1,test_post2,test_post3,test_post4,test_post5,
     test_update1,test_update2,test_update3,
     test_update4,test_update5,test_update6,
     test_blank1].

%% Test cases starts here.
%%------------------------------------------------------------------------------
-define(testpost(Name, Ref, Value, Answer),
    Name(_Config) ->
        HNum = ?str("=hypernumber(\"~s/~s?hypernumber\")",[?HN_URL1,Ref]),
        hn_util:post(?str(?URL,[?HN_URL1,Ref]),?str(?POST,[Value]),"text/xml"),
        hn_util:post(?str(?URL,[?HN_URL2,Ref]),?str(?POST,[HNum]),"text/xml"),
        timer:sleep(500),
        Xml = simplexml:from_xml_string(hn_util:req(?str(?URL,[?HN_URL2,Ref]))),
        Answer = simplexml:search(Xml,value)
    ).
    
-define(testupdate(Name, Ref, Value, NewValue, Answer),
    Name(_Config) ->
        HNum = ?str("=hypernumber(\"~s/~s?hypernumber\")",[?HN_URL1,Ref]),
        hn_util:post(?str(?URL,[?HN_URL1,Ref]),?str(?POST,[Value]),"text/xml"),
        hn_util:post(?str(?URL,[?HN_URL2,Ref]),?str(?POST,[HNum]),"text/xml"),
        hn_util:post(?str(?URL,[?HN_URL1,Ref]),?str(?POST,[NewValue]),"text/xml"),
        timer:sleep(500),
        Xml = simplexml:from_xml_string(hn_util:req(?str(?URL,[?HN_URL2,Ref]))),
        Answer = simplexml:search(Xml,value)
    ).
    
-define(testblank(Name, Ref, Answer),
    Name(_Config) ->
        HNum = ?str("=hypernumber(\"~s/~s?hypernumber\")",[?HN_URL1,Ref]),
        hn_util:post(?str(?URL,[?HN_URL2,Ref]),?str(?POST,[HNum]),"text/xml"),
        timer:sleep(500),
        Xml = simplexml:from_xml_string(hn_util:req(?str(?URL,[?HN_URL2,Ref]))),
        Answer = simplexml:search(Xml,value)
    ).
    

?testpost(test_post1,"a1","99",[{value,[],[{integer,[],["99"]}]}]).
?testpost(test_post2,"a1","2.0",[{value,[],[{float,[],["2.00"]}]}]).
?testpost(test_post3,"a1","abc",[{value,[],[{string,[],["abc"]}]}]).
?testpost(test_post4,"a1","=AVERAGE(2,4)",[{value,[],[{float,[],["3.0"]}]}]).
?testpost(test_post5,"a1","={1,2,3}",[{value,[],[{matrix,[],[{row,[],[{integer,[],["1"]},{integer,[],["2"]},{integer,[],["3"]}]}]}]}]).

?testupdate(test_update1,"a1","99","66",[{value,[],[{integer,[],["66"]}]}]).
?testupdate(test_update2,"a1","99","abc",[{value,[],[{string,[],["abc"]}]}]).
?testupdate(test_update3,"a1","99","2.0",[{value,[],[{float,[],["2.0"]}]}]).
?testupdate(test_update4,"a1","abc","66",[{value,[],[{integer,[],["66"]}]}]).
?testupdate(test_update5,"a1","2.0","abc",[{value,[],[{string,[],["abc"]}]}]).
?testupdate(test_update6,"a1","66","=AVERAGE(2,4)",[{value,[],[{float,[],["99"]}]}]).

?testblank(test_blank1,"a1",[{value,[],[{integer,[],["0"]}]}]).
