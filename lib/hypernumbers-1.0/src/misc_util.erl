-module(misc_util).

-export([stdfuns_export_xml/0,
         import_xml_attributes/2,
         do_import/2,
         profile/2,
         demo/1,
         demo/2,
         cheat/1,
         cheat/2,
         cheat/0]).

-include("builtins.hrl").
-include("spriki.hrl").

-define(CATEGORIES, [
                     {stdfuns_math,      "Math"},
                     {stdfuns_text,      "Text"},
                     {stdfuns_logical,   "Logical"},
                     {stdfuns_stats,     "Statistical"},
                     {stdfuns_info,      "Informational"}
                    ]).

demo(Site) -> demo(Site,code:priv_dir(hypernumbers)++"/demo_pages/").

demo(Site,Dir) ->
    io:format("in misc_util:demo Site is ~p Dir is ~p~n",[Site,Dir]),
    Files = filelib:wildcard(Dir++"*.xml"),
    io:format("in misc_util:demo Site is ~p Files is ~p~n",[Site,Files]),
    F = fun(X) ->
                Name = filename:basename(X,".xml"),
                {ok,Xml} = file:read_file(X),
                Url = Site ++ string:join(string:tokens(Name,"-"),"/") ++ "/",
                Data = simplexml:from_xml_string(binary_to_list(Xml)),
                do_import(Url,Data),
                ok
        end,
    lists:map(F,Files),
    ok.

cheat() ->
    Site="http://il_ballo.dev:9000",
    RootPath=["stock","item"], 
    cheat(Site++"/","il_ballo").
%% N=10,
%% load_page_defaults(Site,RootPath,"deliveries",N),
%% load_page_defaults(Site,RootPath,"sales",N).

%% load_page_defaults(_,_,_,0) -> ok;
%% load_page_defaults(Site,RootPath,SubPage,N) ->
%%    Path=lists:reverse([SubPage,integer_to_list(N)|lists:reverse(RootPath)]),
%%    Ref1=#ref{site=Site,path=Path,ref={page,"/"},name=form,auth=[]},
%%    Val1="/forms/"++SubPage++"/",
%%    Ref2=#ref{site=Site,path=Path,ref={page,"/"},name=gui,auth=[]},
%%    Val2="il_ballo2",
%%    Ref3=#ref{site=Site,path=Path,ref={page,"/"},name=template,auth=[]},
%%    Val3="@"++SubPage,
%%    hn_db:write_item(Ref1,Val1),
%%    hn_db:write_item(Ref2,Val2),
%%    hn_db:write_item(Ref3,Val3),
%%    load_page_defaults(Site,RootPath,SubPage,N-1).

cheat(Example) ->
    hypernumbers_app:clean_start(),
    demo("http://127.0.0.1:9000/",code:priv_dir(hypernumbers)++"/demo_pages/"++
         Example++"/").

cheat(Site,Example) ->
    hypernumbers_app:clean_start(),
    demo(Site,code:priv_dir(hypernumbers)++"/demo_pages/"++Example++"/").

%% Read std_funs and produce a workable
%% xml document for the functions list
stdfuns_export_xml() ->

    Filter = fun
                 ({_Fun,Ident},Ident) -> true;
                 (_,_Ident) -> false
             end,
    
    Function = fun({Fun,_AtomIdent}) ->
                       {function,[{label,atom_to_list(Fun)}],[]}
               end,
    
    Category = fun({AtomIdent,Desc}) ->
                       
                       Funs = lists:filter(fun
                                               (X) -> Filter(X,AtomIdent)
                                           end, ?STDFUNS),
                       
                       {category,[{name,Desc}],
                        lists:map(fun(X) -> Function(X) end,Funs)
                       }
               end,
    
    Cats = lists:map(
             fun(X) -> Category(X) end,
             ?CATEGORIES),
    
    {function,[],Cats}.

import_xml_attributes(File,Url) ->
    {ok,Bin} = file:read_file(File),
    do_import(Url,simplexml:from_xml_string(binary_to_list(Bin))).

do_import(Url,{attr,[],Refs}) ->
    lists:map
      (
      fun(X) ->
              case X of
                  %% Dont sent value attributes
                  {ref,_,[{value,_,_}]} -> ok;
                  {ref,_,[{dynamic,_,_}]} -> ok;
                  {ref,_,[{'dependancy-tree',_,_}]} -> ok;
                  {ref,_,[{'parents',_,_}]} -> ok;
                  {ref,[_,{ref,Ref}],[{Name,_,[{_Type,[],Children}]}]} ->
                      Xml = io_lib:format("<create><~s><![CDATA[~s]]></~s></create>",
                                          [Name,simplexml:to_xml_string(Children),
                                           Name]),
                      hn_util:post(Url++string:to_lower(Ref)++"?attr",
                                   lists:flatten(Xml),"text/xml");
                  {ref,[_,{ref,Ref}],[{Name,_,Children}]} ->
                      V = case io_lib:deep_char_list(Children) of
                              true ->
                                  lists:flatten(Children);
                              false ->
                                  simplexml:to_xml_string(Children)
                          end,
                      Xml = io_lib:format("<create><~s><![CDATA[~s]]></~s></create>",
                                          [Name,V,Name]),
                      hn_util:post(Url++string:to_lower(Ref)++"?attr",
                                   lists:flatten(Xml),
                                   "text/xml")
              end
      end,Refs
     ).

profile(N,F) ->
    statistics(runtime),
    statistics(wall_clock),
    for(1,N,F),
    {_,Time1} = statistics(runtime),
    {_,Time2} = statistics(wall_clock),
    {Time1,Time2}.

for(N,N,F) -> [F()];
for(I,N,F) -> [F()|for(I+1,N,F)].
