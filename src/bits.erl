%%%-----------------------------------------------------------------------------
%%% File        : bits.erl
%%% Author      : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : helpful bits and bobs
%%%
%%% Created     : 16 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(bits).

%% API
-export([clear_db/0,fire_register/0,fire_post/0,setup_demo/0,setup_test/0]).

%%==============================================================================
%% API
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function:
%% Description:
%%------------------------------------------------------------------------------

%%==============================================================================
%% Internal functions
%%==============================================================================

clear_db()->
    application:stop(mnesia),
    application:stop(engine),
    load_db:create_db(persistent),
    application:start(engine),
    application:start(remoting),
    application:start(mnemosyne),
    {ok, "db cleared down"}.

fire_post()->
    Url="http://gordon.bt_athome.com:80/myholidays/c1",
    Action1="action=notify",
    Action2="registered_URL=http://backawinner.gg:1234/page1/a1",
    Action3="cookie=secret",
    Action4="proxy_URL=http://backawinner.gg:1234/page1/a1",
    Actions=Action1++"&"++Action2++"&"++Action3++"&"++Action4,
    case http:request(post,{Url,[],"text/text",Actions},[],[]) of
	{ok,{{_,200,_},_,Body}} ->
	    io:format("in bits:fire_register Body is ~p~n",[Body]),
	    ok;
	Other -> io:format("in bits:fire_register remote site "++
			   "error is ~p~n",[Other]),
		 ok
    end.


fire_register()->
    Url="http://127.0.0.1:9000/zzz/a1",
    Action1="action=register",
    Action2="registered_URL=http://backawinner.gg:1234/page1/a1",
    Action3="cookie=secret",
    Action4="proxy_URL=http://backawinner.gg:1234/page1/a1",
    Actions=Action1++"&"++Action2++"&"++Action3++"&"++Action4,
    case http:request(post,{Url,[],"text/text",Actions},[],[]) of
	{ok,{{_,200,_},_,Body}} ->
	    io:format("in bits:fire_register Body is ~p~n",[Body]),
	    ok;
	Other -> io:format("in bits:fire_register remote site "++
			   "error is ~p~n",[Other]),
		 ok
    end.

setup_test() ->

    Url = "http://127.0.0.1:9000",

    hn_util:post(Url++"/test1/a1?format=xml",
        "<post><action>create</action><value>=/test2/a1</value></post>"),
    hn_util:post(Url++"/test1/a1?format=xml",
        "<post><action>create</action><value>=/test2/a1+/test3/a1</value></post>"),

    ok.



setup_demo() ->
    io:format("in bits:setup_demo - kicking off!~n"),
    JetEasy  = "http://jeteasy.com:9000/from/edinburgh/to/nice/2007/",
    BrianAir = "http://bryanair.com:9000/from/stansted/to/nice/2007/",
    French   = "http://myfrenchholiday.fr:9000/",

    Xml = "<post><action>create</action><value><cell>{C1}</cell>"
        ++"<cell>{C2}</cell><cell>{C3}</cell><cell>{C4}</cell>"
        ++"<cell>{C5}</cell></value></post>",

    FrXml = "<post><action>create</action><value><cell>{C1}</cell>"
        ++"<cell>{C2}</cell><cell>{C3}</cell><cell>{C4}</cell>"
        ++"<cell>{C5}</cell><cell>{C6}</cell></value></post>",

    AvXml = "<post><action>create</action><value><cell>{C1}</cell>"
        ++"<cell>{C2}</cell><cell>{C3}</cell></value></post>",

    io:format("in bits:setup_demo - Setting up graphics!~n"),
    %% Setup Graphics
    hn_util:post("http://jeteasy.com:9000/?format=xml&admin","<post><action>create"
        ++"</action><gui>jeteasyindex.swf</gui><name>Hello</name><public>true</public></post>","text/xml"),
    hn_util:post("http://jeteasy.com:9000/from/?format=xml&admin","<post><action>create"
        ++"</action><gui>jeteasy.swf</gui><name>Hello</name><public>true</public></post>","text/xml"),
    hn_util:post("http://bryanair.com:9000/?format=xml&admin","<post><action>create"
        ++"</action><gui>bryanairindex.swf</gui><name>Hello</name><public>true</public></post>","text/xml"),
    hn_util:post("http://bryanair.com:9000/from/?format=xml&admin","<post><action>create"
        ++"</action><gui>bryanair.swf</gui><name>Hello</name><public>true</public></post>","text/xml"),
    hn_util:post("http://myfrenchholiday.fr:9000/?format=xml&admin","<post><action>create"
        ++"</action><gui>frindex.swf</gui><name>Hello</name><public>true</public></post>","text/xml"),
    hn_util:post("http://myfrenchholiday.fr:9000/villa/?format=xml&admin","<post><action>create"
        ++"</action><gui>fr.swf</gui><name>Hello</name><public>true</public></post>","text/xml"),

    io:format("in bits:setup_demo - setting up cells!~n"),

    CellHeaders = hn_util:str_replace(Xml,[{"C1","Flight No"},{"C2","Seats"},
        {"C3","Price"},{"C4","Depart"},{"C5","Arrive"}]),
    PropertyHeaders = hn_util:str_replace(FrXml,[{"C1","Name"},{"C2","Image"},
        {"C3","Sleeps"},{"C4","Description"},{"C5","Location"},{"C6","Features"}]),
    AvailabilityHeaders = hn_util:str_replace(AvXml,[{"C1","Date"},{"C2","Price"},
        {"C3","Availability"}]),

    hn_util:post(French++"villa/sleeps3/villadetitrit/B2:B7?format=xml",PropertyHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E2:G2?format=xml",AvailabilityHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/B2:B7?format=xml",PropertyHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E2:G2?format=xml",AvailabilityHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/B2:B7?format=xml",PropertyHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E2:G2?format=xml",AvailabilityHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/B2:B7?format=xml",PropertyHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E2:G2?format=xml",AvailabilityHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/B2:B7?format=xml",PropertyHeaders,"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E2:G2?format=xml",AvailabilityHeaders,"text/xml"),

    hn_util:post(French++"villa/sleeps3/villadetitrit/C2:C7?format=xml",
        hn_util:str_replace(FrXml,[{"C1","Villa de Titrit"},
            {"C2","http://myfrenchholiday.fr:9000/images/villa.jpg"},{"C3","3"},{"C4","This stone built granary and barn date from about 1675, sympathetically converted into two attractive, detached cottages, with many interesting features including spiral staircases, original timbers, oak floors and gallery bedrooms"},
            {"C5","2 Miles north of Nice"},{"C6","All year short breaks, Bed linen included, Cot Available, Detached"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E3:G3?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 01"},{"C2","230"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E4:G4?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 08"},{"C2","240"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E5:G5?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 15"},{"C2","250"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E6:G6?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 22"},{"C2","260"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E7:G7?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 29"},{"C2","29000"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E8:G8?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 06"},{"C2","290"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E9:G9?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 13"},{"C2","350"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E10:G10?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 20"},{"C2","350"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/villadetitrit/E11:G11?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 27"},{"C2","310"},{"C3","no"}]),"text/xml"),

    hn_util:post(French++"villa/sleeps5/skyvilla/C2:C7?format=xml",
        hn_util:str_replace(FrXml,[{"C1","Sky Villa"},
            {"C2","http://myfrenchholiday.fr:9000/images/villa1.jpg"},{"C3","5"},{"C4","this comfortable, detached stone cottage has its own secure garden and enjoys a beautifully rural setting. Dating from the early 190000s,its exposed stone walls and beams help to retain its traditional atmosphere"},
            {"C5","1 Mile south of Nice"},{"C6","Enclosed garden, Garden, Rural, Shower"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E3:G3?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 01"},{"C2","230"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E4:G4?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 08"},{"C2","240"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E5:G5?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 15"},{"C2","250"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E6:G6?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 22"},{"C2","260"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E7:G7?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 29"},{"C2","29000"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E8:G8?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 06"},{"C2","290"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E9:G9?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 13"},{"C2","350"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E10:G10?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 20"},{"C2","350"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps5/skyvilla/E11:G11?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 27"},{"C2","310"},{"C3","no"}]),"text/xml"),

    hn_util:post(French++"villa/sleeps8/oceanview/C2:C7?format=xml",
        hn_util:str_replace(FrXml,[{"C1","Ocean View"},
            {"C2","http://myfrenchholiday.fr:9000/images/villa2.jpg"},{"C3","8"},{"C4","Situated in the heart of the small and picturesque medieval border town of Knighton, this pretty half-timbered cotage, built in 1678 retains many original features and has oak beams throughout"},
            {"C5","2 Miles east of Nice"},{"C6","Television, Washing machine, Woodburner"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E3:G3?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 01"},{"C2","230"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E4:G4?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 08"},{"C2","240"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E5:G5?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 15"},{"C2","250"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E6:G6?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 22"},{"C2","260"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E7:G7?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 29"},{"C2","29000"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E8:G8?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 06"},{"C2","290"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E9:G9?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 13"},{"C2","350"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E10:G10?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 20"},{"C2","350"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps8/oceanview/E11:G11?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 27"},{"C2","310"},{"C3","no"}]),"text/xml"),

    hn_util:post(French++"villa/sleeps3/thecove/C2:C7?format=xml",
        hn_util:str_replace(FrXml,[{"C1","The Cove"},
            {"C2","http://myfrenchholiday.fr:9000/images/villa3.jpg"},{"C3","3"},{"C4","The estate also owns 20 miles of private footpaths as well as large tracts of the Black Mountains. Rods are available for salmon and trout on the River Usk for a modest daily fee."},
            {"C5","3 Miles west of Nice"},{"C6","1 pet welcome, 2 night short breaks"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E3:G3?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 01"},{"C2","230"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E4:G4?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 08"},{"C2","240"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E5:G5?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 15"},{"C2","250"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E6:G6?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 22"},{"C2","260"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E7:G7?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 29"},{"C2","29000"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E8:G8?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 06"},{"C2","290"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E9:G9?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 13"},{"C2","350"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E10:G10?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 20"},{"C2","350"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps3/thecove/E11:G11?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 27"},{"C2","310"},{"C3","no"}]),"text/xml"),

    hn_util:post(French++"villa/sleeps4/stonehead/C2:C7?format=xml",
        hn_util:str_replace(FrXml,[{"C1","Stone Head"},
            {"C2","http://myfrenchholiday.fr:9000/images/villa4.jpg"},{"C3","4"},{"C4","Garden Cottage:  Single storey: the cottage was totally renovated and refurbished in 2004. Entrance  porch, fully equipped kitchen and dining area, including dishwasher and washing machine, separate sitting room, bathroom with over the bath shower attachment, twin bedroom and single bedroom. "},
            {"C5","France duh"},{"C6","Television, Washing machine, Woodburner"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E3:G3?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 01"},{"C2","230"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E4:G4?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 08"},{"C2","240"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E5:G5?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 15"},{"C2","250"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E6:G6?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 22"},{"C2","260"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E7:G7?format=xml",
        hn_util:str_replace(AvXml,[{"C1","June 29"},{"C2","29000"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E8:G8?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 06"},{"C2","290"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E9:G9?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 13"},{"C2","350"},{"C3","yes"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E10:G10?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 20"},{"C2","350"},{"C3","no"}]),"text/xml"),
    hn_util:post(French++"villa/sleeps4/stonehead/E11:G11?format=xml",
        hn_util:str_replace(AvXml,[{"C1","July 27"},{"C2","310"},{"C3","no"}]),"text/xml"),


    hn_util:post(JetEasy++"june/01/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"june/08/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"june/15/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"june/22/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"june/29/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"july/06/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"july/13/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"july/20/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(JetEasy++"july/27/b2:f2?format=xml",CellHeaders,"text/xml"),

    hn_util:post(JetEasy++"june/01/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/01/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/01/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/01/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/01/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/01/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"june/08/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/08/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/08/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/08/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/08/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/08/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"june/15/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/15/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/15/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/15/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/15/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/15/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"june/22/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/22/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/22/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/22/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/22/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/22/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"june/29/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/29/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/29/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/29/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/29/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"june/29/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"july/06/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/06/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/06/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/06/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/06/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/06/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"july/13/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/13/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/13/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/13/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/13/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/13/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"july/20/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/20/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/20/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/20/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/20/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/20/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(JetEasy++"july/27/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/27/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/27/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/27/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/27/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(JetEasy++"july/27/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"june/01/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"june/08/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"june/15/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"june/22/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"june/29/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"july/06/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"july/13/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"july/20/b2:f2?format=xml",CellHeaders,"text/xml"),
    hn_util:post(BrianAir++"july/27/b2:f2?format=xml",CellHeaders,"text/xml"),

    hn_util:post(BrianAir++"june/01/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/01/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/01/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/01/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/01/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/01/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"june/08/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/08/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/08/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/08/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/08/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/08/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"june/15/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/15/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/15/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/15/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/15/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/15/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"june/22/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/22/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/22/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/22/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/22/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/22/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"june/29/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/29/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/29/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/29/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/29/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"june/29/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"july/06/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/06/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/06/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/06/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/06/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/06/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"july/13/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/13/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/13/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/13/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/13/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/13/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"july/20/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/20/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/20/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/20/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/20/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/20/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),

    hn_util:post(BrianAir++"july/27/b3:f3?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/27/b4:f4?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/27/b5:f5?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/27/b6:f6?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/27/b7:f7?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","58"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),
    hn_util:post(BrianAir++"july/27/b8:f8?format=xml",hn_util:str_replace(Xml,[
        {"C1","AB123"},{"C2","124"},{"C3","50"},{"C4","11:50"},{"C5","12:40"}]),"text/xml"),


    io:format("in bits:setup_demo - setting up master page!~n"),
    %% Now add the master page
    Site="http://gordonguthrie.org:9000",
    Path="/myholidays/",
    spriki:process_input(Site,Path,1,2,"Week Starting"),
    spriki:process_input(Site,Path,2,1,"Our Flights"),
    spriki:process_input(Site,Path,4,1,"Dale's Flights"),

    spriki:process_input(Site,Path,2,2,"Price"),
    spriki:process_input(Site,Path,3,2,"Seats Left"),
    spriki:process_input(Site,Path,4,2,"Price"),
    spriki:process_input(Site,Path,5,2,"Seats Left"),

    spriki:process_input(Site,Path,1,3,"'6th July"),
    spriki:process_input(Site,Path,2,3,
			 "=hypernumber(\"http://bryanair.com:9000/from/stansted/to/"++
			 "nice/2007/july/06/d3?hypernumber\")*4"),
    spriki:process_input(Site,Path,3,3,
			 "=hypernumber(\"http://bryanair.com:9000/from/stansted/to/"++
			 "nice/2007/july/06/c3?hypernumber\")"),
    spriki:process_input(Site,Path,4,3,
			 "=hypernumber(\"http://jeteasy.com:9000/from/edinburgh/to/"++
			 "nice/2007/july/06/d3?hypernumber\")*2"),
    spriki:process_input(Site,Path,5,3,
			 "=hypernumber(\"http://jeteasy.com:9000/from/edinburgh/to/"++
			 "nice/2007/july/06/c3?hypernumber\")"),

    spriki:process_input(Site,Path,1,4,"'13th July"),
    spriki:process_input(Site,Path,2,4,
			 "=hypernumber(\"http://bryanair.com:9000/from/stansted/to/"++
			 "nice/2007/july/06/d4?hypernumber\")*4"),
    spriki:process_input(Site,Path,3,4,
			 "=hypernumber(\"http://bryanair.com:9000/from/stansted/to/"++
			 "nice/2007/july/06/c4?hypernumber\")"),
    spriki:process_input(Site,Path,4,4,
			 "=hypernumber(\"http://jeteasy.com:9000/from/edinburgh/to/"++
			 "nice/2007/july/06/d4?hypernumber\")*2"),
    spriki:process_input(Site,Path,5,4,
			 "=hypernumber(\"http://jeteasy.com:9000/from/edinburgh/to/"++
			 "nice/2007/july/06/c4?hypernumber\")"),

    spriki:process_input(Site,Path,1,5,"'20th July"),
    spriki:process_input(Site,Path,2,5,
			 "=hypernumber(\"http://bryanair.com:9000/from/stansted/to/"++
			 "nice/2007/july/06/d5?hypernumber\")*4"),
    spriki:process_input(Site,Path,3,5,
			 "=hypernumber(\"http://bryanair.com:9000/from/stansted/to/"++
			 "nice/2007/july/06/c5?hypernumber\")"),
    spriki:process_input(Site,Path,4,5,
			 "=hypernumber(\"http://jeteasy.com:9000/from/edinburgh/to/"++
			 "nice/2007/july/06/d5?hypernumber\")*2"),
    spriki:process_input(Site,Path,5,5,
			 "=hypernumber(\"http://jeteasy.com:9000/from/edinburgh/to/"++
			 "nice/2007/july/06/c5?hypernumber\")"),

    spriki:process_input(Site,Path,6,1,"The Cove"),
    spriki:process_input(Site,Path,7,2,"Price"),
    spriki:process_input(Site,Path,7,2,"Availability"),

    spriki:process_input(Site,Path,6,3,
			 "=hypernumber(\"http://myfrenchholiday.fr:9000/villa/"++
			 "sleeps3/thecove/f8?hypernumber\")"),
    spriki:process_input(Site,Path,7,3,
			 "=hypernumber(\"http://myfrenchholiday.fr:9000/villa/"++
			 "sleeps3/thecove/g8?hypernumber\")"),

    spriki:process_input(Site,Path,6,4,
			 "=hypernumber(\"http://myfrenchholiday.fr:9000/villa/"++
			 "sleeps3/thecove/f9?hypernumber\")"),
    spriki:process_input(Site,Path,7,4,
			 "=hypernumber(\"http://myfrenchholiday.fr:9000/villa/"++
			 "sleeps3/thecove/g9?hypernumber\")"),

    spriki:process_input(Site,Path,6,5,
			 "=hypernumber(\"http://myfrenchholiday.fr:9000/villa/"++
			 "sleeps3/thecove/f10?hypernumber\")"),
    spriki:process_input(Site,Path,7,5,
			 "=hypernumber(\"http://myfrenchholiday.fr:9000/villa/"++
			 "sleeps3/thecove/g10?hypernumber\")"),

    ok.
