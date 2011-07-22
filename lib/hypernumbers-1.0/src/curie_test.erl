%%% @author    Jakub Chlanda
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%		Test suite for curie.erl
%%%		Use test_me/8 to simulate subbmition of user_defined_fn wizard.
%%% @end
%%% Created :  21 Jul 2011

-module(curie_test).

-export([
         test_me/8
        ]).

%%TODO
	%~ Make sure that cookie is valid, it lives only 7 days, find the way of getting new cookie every time the function is called!
%%TODO

%~ curie_test:test_me("http://hypernumbers.dev:9000", ["page1"],"b1", "b2", "b8", ["b5", "b6"], ["c5", "c6"], ["f5", "f6"]).
test_me(Site, Page, Name, Description, OutputValue, ListOfParameterNames, ListOfParameterDescriptions, ListOfParameterValues)	->
    
    String_Page = get_page(Page, ""),
    Entry = 	{struct,[	{"type","user_defined"},
							{"properties",
				{struct,[	{"site", Site},
							{"page", String_Page},
							{"fn_name",Name},
                            {"fn_description", Description},
                            {"fn_output_value", OutputValue},
                            {"fn_parameters", {array, get_parameters(Site, Page, ListOfParameterNames, ListOfParameterDescriptions, ListOfParameterValues, [])}}
                           ]}}
                       ]},
                       
    Json_Entry = lists:flatten(mochijson:encode(Entry)),
	%~ Cookie lives only 7 days!
    Auth = "test!hypernumbers.com|90a9b1042a97f45c008d1949b3cf25a2|63478978880|e50fa7683a5852dbd57fc4b62406c3b4",
    httpc:request(post, {"http://hypernumbers.dev:9000/page1/",
                         [{"host","hypernumbers.dev:9000"},
                          {"accept", "application/json"},
                          {"cookie", Auth}],
                         "", Json_Entry }, [], []).
	
	
get_parameters(_Site, _Page, [], [], [], Parameters)	->
	io:format("Parameters are: ~p~n", [Parameters]),
	lists:reverse(Parameters);

get_parameters(Site, Page, [H | T], [H2 | T2], [H3 | T3], Parameters)	->
	Parameter = {struct, [{"name", H}, {"desc", H2}, {"value", H3}]},
	get_parameters(Site, Page, T, T2, T3, [Parameter | Parameters]).


get_page([], String_Page)	->
	"/" ++ String_Page;
	
get_page([H | T], String_Page)	->
	New_String = String_Page ++ H ++ "/",
	get_page(T, New_String).
