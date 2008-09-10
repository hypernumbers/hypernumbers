-module(misc_util).
-export([stdfuns_export_xml/0,
	 import_xml_attributes/2,
	 do_import/2,
	 profile/2,
	 testa/0,
	 demo/1,
	 cheat/1]).
-include("builtins.hrl").
-include("spriki.hrl").

-define(CATEGORIES, [
    {stdfuns_math,      "Math"},
    {stdfuns_text,      "Text"},
    {stdfuns_logical,   "Logical"},
    {stdfuns_stats,     "Statistical"},
    {stdfuns_info,      "Informational"}
    ]).

demo(Site) ->
    Files = filelib:wildcard(code:priv_dir(hypernumbers)++"/demo_pages/*.xml"),
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

cheat(1) -> import_xml_attributes("c:\\opt\\code\\trunk\\priv\\dale\\data.xml","http://127.0.0.1:9000/data/");
cheat(2) -> import_xml_attributes("/cygdeive/c/opt/code/trunk/priv/dale/data.xml","http://127.0.0.1:9000/data/").

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

testa() -> testb(foo).
testb(bar) -> "foo".

import_xml_attributes(File,Url) ->
    {ok,Bin} = file:read(File),
    do_import(Url,simplexml:from_xml_string(binary_to_list(Bin))).

do_import(Url,{attr,[],Refs}) ->
    lists:map
      (
      fun(X) ->
	      case X of
		  %% Dont sent value attributes
		  {ref,_,[{value,_,_}]} -> ok;
		  {ref,_,[{'dependancy-tree',_,_}]} -> ok;
		  {ref,_,[{'parents',_,_}]} -> ok;
		  {ref,[_,{ref,Ref}],[{Name,_,[{_Type,[],Children}]}]} ->
		      Xml = io_lib:format("<create><~s><![CDATA[~s]]></~s></create>",
					  [Name,simplexml:to_xml_string(Children),Name]),
		      hn_util:post(Url++string:to_lower(Ref)++"?attr",lists:flatten(Xml),"text/xml");
		  {ref,[_,{ref,Ref}],[{Name,_,Children}]} ->
                      V = case io_lib:deep_char_list(Children) of
                              true -> 
                                  lists:flatten(Children);
                              false ->
                                  simplexml:to_xml_string(Children)
                          end,
		      Xml = io_lib:format("<create><~s><![CDATA[~s]]></~s></create>",
					  [Name,V,Name]),
		      hn_util:post(Url++string:to_lower(Ref)++"?attr",lists:flatten(Xml),"text/xml")
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
