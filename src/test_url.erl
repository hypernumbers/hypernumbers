%%%-------------------------------------------------------------------
%%% @author U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%% @copyright (C) 2008, U-psytoo\gordonguthrie
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2008 by U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%%-------------------------------------------------------------------
-module(test_url).

-export([test/0]).

-include("../lib/hypernumbers-1.0/include/spriki.hrl").


%% define a couple of items that define what site we are on
-define(SITE,"site").
-define(NAME,"name").
-define(USERNAME,"gordonguthrie").

-define(INPUT3,"test3","/test3/{auto,incr}/","index","null").
-define(NAME3,"test3").
-define(PAGES3,["test3","99"]).
-define(OUTPUT3,"/test3/100/").

-define(INPUT17,"test17","/test17/{username}/{auto,incr}/","index","null").
-define(NAME17,"test17").
-define(PAGES17,["test17","gordonguthrie","2"]).
-define(OUTPUT17,"/test17/gordonguthrie/3/").

-define(INPUT18,"test18","/test18/{auto,incr}/{username}/","index","null").
-define(NAME18,"test18").
-define(PAGES18,["test18","2","gordonguthrie"]).
-define(OUTPUT18,"/test18/3/gordonguthrie/").

-define(INPUT19,"test19","/test19//{username}/{year,long}/{auto,incr}/","index","null").
-define(NAME19,"test19").
-define(PAGES19,["test19","gordonguthrie","2008","999"]).
-define(OUTPUT19,"/test19/gordonguthrie/2008/1000/").




test()->
    Paths=[?PAGES3,?PAGES17,?PAGES18,?PAGES19],
    Fun = fun(Path) ->
                  Addr=#ref{site=?SITE,path=Path,name=?NAME},
                  hn_db:write_item(Addr,dummy)
          end,
    lists:map(Fun,Paths),

    io:format("in test_url:test TEST 3~n"),
    {ok,ok}=hn_templates:write_def(?INPUT3),
    Got3=hn_templates:get_next(#ref{site=?SITE},?NAME3,?USERNAME),
    test_util:expected3(?OUTPUT3,Got3),
    
    io:format("in test_url:test TEST 17~n"),
    {ok,ok}=hn_templates:write_def(?INPUT17),
    Got17=hn_templates:get_next(#ref{site=?SITE},?NAME17,?USERNAME),
    test_util:expected3(?OUTPUT17,Got17),

%% expected to fail 
    io:format("in test_url:test TEST 18~n"),
    {ok,ok}=hn_templates:write_def(?INPUT18),
    Got18=hn_templates:get_next(#ref{site=?SITE},?NAME18,?USERNAME),
    test_util:expected3(?OUTPUT18,Got18),

    io:format("in test_url:test TEST 19~n"),
    {ok,ok}=hn_templates:write_def(?INPUT19),
    Got19=hn_templates:get_next(#ref{site=?SITE},?NAME19,?USERNAME),
    test_util:expected3(?OUTPUT19,Got19).
