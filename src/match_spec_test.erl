%%%-------------------------------------------------------------------
%%% File        : match_spec_test.erl
%%% Author      : Gordon Guthrie <Gordon Guthrie@caturday>
%%% Description :
%%%
%%% Created     :  3 Sep 2008 by Gordon Guthrie <Gordon Guthrie@caturday>
%%%-------------------------------------------------------------------
-module(match_spec_test).

-export([test/0]).

-include("../lib/hypernumbers-1.0/include/spriki.hrl").
-include("../lib/hypernumbers-1.0/include/handy_macros.hrl").

test()->
    setup_db(),
%% Now do some work
    %%get_pages_under(["test3","99"]),
    Path=["test3","[>13]"],
    {CompedPath,Cond,N}=comp_url(Path,1),
    io:format("in muin:test Path is ~p~n-CompedPath is ~p~n"++
              "Cond is ~p~n",[Path,CompedPath,Cond]),
    Selector=get(matchspec),
    put(matchspec,Selector+1),
    Bits=make_bits(lists:reverse(CompedPath),?MS(Selector)),
    io:format("in muin:test Bits are ~p~n",[Bits]),
    Ref=ms_util:make_ms(ref,[{path,Bits},{site,'$5'},{name,rawvalue}]),
    io:format("in muin:test Ref is ~p~n",[Ref]),
    Head=ms_util:make_ms(hn_item,[{addr,Ref},{val,'$7'}]),
    io:format("in muin:test Head is ~p~n",[Head]),
    %%Body=make_body(N),
    %%Body=[Bits],
    %% the problem is that the condition > 13 works on integers but
    %% the path is stored as a string in the database...
    Cond2=[],
    Body=[{{'$1','$2'}}],
    MatchSpec=[{Head,Cond2,Body}],
    io:format("in muin:test MatchSpec is ~p~n",[MatchSpec]),
    Match = fun() ->
                    mnesia:select(hn_item,MatchSpec)
            end,
    {atomic, Res} = mnesia:transaction(Match),
    List=hslists:uniq(Res),
    io:format("in muin:test~n-Res is ~p~n-List is ~p~n",[Res,List]),
%% now remove the PathComp from itself
    List.

make_body(N) -> make_body(N,[]).

make_body(0,Acc) -> lists:reverse(Acc);
make_body(N,Acc) -> make_body(N-1,[?MS(N)|Acc]).
    

comp_url(Url,N) ->
    comp_url(Url,N,[],[]).

comp_url([],N,Acc1,Acc2) ->
    % Reset the value of N in the process dictionary
    put(matchspec,N),
    io:format("in match_spec_test:comp_url Acc2 is ~p~n",[Acc2]),
    {lists:reverse(Acc1),expand_cond(Acc2),N};
comp_url([H|T],N,Acc1,Acc2) ->
    % Eek need to put N into the process dictionary here so I can use it
    % in the parser - not nice!
    put(matchspec,N),
    % Increment N if you use the Match but not if you don't
    {M,C,Nn}=case H of
              [$[|_T] -> {?MS(N),comp_seg(H,N),N+1};
              _       -> {H,[],N}
          end,
    comp_url(T,Nn,[M|Acc1],[C|Acc2]).

comp_seg(Query,N)->
    {Ok,Toks,_}=url_query_lexer:string(Query),
    io:format("in muin:comp_seg Toks are ~p N is ~p~n",[Toks,N]),
    {ok,Seg}=url_query_parser:parse(Toks),
    io:format("in muin:comp_seg Seg is ~p~n",[Seg]),
    Seg.

expand_cond([H,[]]) -> [H];
expand_cond(Other)  -> expand_cond2(Other,[]).

expand_cond2([],Acc)     -> io:format("got to 1~n"),
                            ["{'and',"++Acc++"}"];
expand_cond2([H|[]],Acc) -> io:format("got to 2~n"),
                            [Acc++","++H];
expand_cond2([H|T],[])   -> io:format("got to 3~n"),
                            expand_cond2(T,H);
expand_cond2([H|T],Acc)  -> io:format("got to 4~n"),
                            expand_cond2(T,Acc++","++H).

%% makes an improper list as required by match specification
make_bits(List) -> make_bits(List, []).

make_bits([],Acc)    -> Acc;
make_bits([H|T],Acc) -> make_bits(T,[H|Acc]).

setup_db() ->
    bits:clear_db(),
%% First set up a clean DB for testing
    List=[{["test3"],{cell,{1,1}},rawvalue,"bob"},
          {["test3"],{cell,{1,1}},rawvalue,"jim"},
          {["test3"],{cell,{1,2}},rawvalue,"jane"},
          {["test3","9"],{cell,{1,1}},rawvalue,"billy"},
          {["test3","9"],{cell,{1,2}},rawvalue,"jaimie"},
          {["test3","9"],{cell,{1,3}},rawvalue,"joe"},
          {["test3","99"],{cell,{1,1}},rawvalue,"andy"},
          {["test3","99"],{cell,{1,2}},rawvalue,"billy"},
          {["test3","99"],{cell,{1,3}},rawvalue,"joel"},
          {["test3","99","some_more"],{cell,{1,1}},rawvalue,"henry"}
         ],
    Fun = fun({Path,Ref,Name,Val}) ->
                  Addr=#ref{site=site,ref=Ref,path=Path,name=Name},
                  hn_db:write_item(Addr,Val)
          end,
    lists:map(Fun,List),
%%    {atomic,ok}=hn_templates:write_def("test1",false,"/1/","index"),
%%    {atomic,ok}=hn_templates:write_def("test2",false,"/2/","index"),
%%    {atomic,ok}=hn_templates:write_def("test3",false,"/3/","index"),
%%    {atomic,ok}=hn_templates:write_def("test4",false,"/4/","index"),
%%    {atomic,ok}=hn_templates:write_def("test5",false,"/5/","index"),
    {ok,ok}.


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
