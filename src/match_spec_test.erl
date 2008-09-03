%%%-------------------------------------------------------------------
%%% File        : match_spec_test.erl
%%% Author      : Gordon Guthrie <Gordon Guthrie@caturday>
%%% Description : 
%%%
%%% Created     :  3 Sep 2008 by Gordon Guthrie <Gordon Guthrie@caturday>
%%%-------------------------------------------------------------------
-module(match_spec_test).

-export([test/0]).

-include("../include/spriki.hrl").

test()->
    %% First set up a clean DB for testing
    Paths=[{["test3"],"bob"},
	   {["test3"],"jim"},
	   {["test3"],"jane"},
	   {["test3","99"],"billy"},
	   {["test3","99"],"joel"},
	   {["test3","99","some_more"],"henry"}
	  ],
    Fun = fun({Path,Name}) ->
		  Addr=#ref{site=site,path=Path,name=Name},
		  io:format("In init_per_suite Addr is ~p~n",[Addr]),
		  hn_db:write_item(Addr,dummy)
	  end,
    lists:map(Fun,Paths),
    {atomic,ok}=hn_templates:write_def("test1",false,"/1/","index"),
    {atomic,ok}=hn_templates:write_def("test2",false,"/2/","index"),
    {atomic,ok}=hn_templates:write_def("test3",false,"/3/","index"),
    {atomic,ok}=hn_templates:write_def("test4",false,"/4/","index"),
    {atomic,ok}=hn_templates:write_def("test5",false,"/5/","index"),

    %% Now do some work
    get_pages_under(["test3","99"]).

get_pages_under(PathComps)->		  
    Bits=make_bits(lists:reverse(PathComps),'$1'),
    io:format("in muin:test PathComps are ~p Bits are ~p~n",[PathComps,Bits]),
    Ref=ms_util:make_ms(ref,[{path,Bits}]),
    io:format("in muin:test Ref is ~p~n",[Ref]),
    Head=ms_util:make_ms(hn_item,[{addr,Ref}]),
    io:format("in muin:test Head is ~p~n",[Head]),
    Cond=[],
    Body=[Bits],
    MatchSpec=[{Head,Cond,Body}],
    io:format("in muin:test MatchSpec is ~p~n",[MatchSpec]),
    Match = fun() ->
		     mnesia:select(hn_item,MatchSpec)
            end,
    {atomic, Res} = mnesia:transaction(Match),
    List=hslists:uniq(Res),
    io:format("in muin:test~n-Res is ~p~n-List is ~p~n",[Res,List]),
    %% now remove the PathComp from itself
    lists:delete(PathComps,List).

%% makes an improper list as required by match specification
make_bits([],Acc)    -> Acc;
make_bits([H|T],Acc) -> make_bits(T,[H|Acc]).
