%%%-------------------------------------------------------------------
%%% File        : hn_templates.erl
%%% Author      : Gordon Guthrie <gordon@hypernumbers.com>
%%% Description : This module deals with templates 
%%%
%%% Created     : 30 Aug 2008 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hn_templates).

-export([write_def/4,
	 get_next/2,
	 make_path/1]).

-import(format_util,
	[clock_12/1,
	 pad_year/1,
	 pad_calendar/1,
	 get_short_day/1,
	 get_day/1,
	 get_short_month/1,
	 get_month/1,
	 get_last_two/1,
	 get_dayname/1,
	 get_short_dayname/1
	]).

-include("spriki.hrl").
-include("handy_macros.hrl").

%%
%% External API
%%

%% @spec write_def(TemplateName,IsDynamic,RootURL,GUI) -> {ok,ok} | {error,Error}
%% @doc takes an incoming template and writes it to the database
write_def(TemplateName,IsDynamic,RootURL,GUI) ->
    hn_db:write_template(TemplateName,IsDynamic,RootURL,GUI).

%% @spec get_next(TemplateName) -> NewName
%% @doc gets the next valid page name
get_next(Ref,TemplateName) ->
    io:format("in hn_templates:get_next TemplateName is ~p~n",[TemplateName]),
    {ok,Template}=hn_db:read_template(TemplateName),
    io:format("in hn_templates:get_next got to 0~n"),
    {template,TemplateName,IsDynamic,TemplatePath,Gui}=Template,
    io:format("in hn_templates:get_next got to 1~n"),
    io:format("in hn_templates:get_next got to 2a~n"),
    Return=new_path(Ref,TemplatePath),
    io:format("in hn_templates:get_next Return is ~p~n",[Return]),
    Return.

%%
%% compiles a path down to a form that can be processed
%%
make_path(List) when is_list(List) ->
    NewList=string:tokens(List,"/"),
    make_path(NewList,[]).

make_path([],Acc)   -> lists:reverse(Acc);
make_path([H|T],Acc) -> make_path(T,[parse(H)|Acc]).

%%
%% Internal Functions
%%
parse(String) ->
    [H|_Discard]=String,
    T=last(String),
    case {H,T} of
	{${,$}} ->
	    String2=lists:sublist(String,2,string:len(String)-2),
	    List=string:tokens(String2,","),
	    case List of
		[A,B]   -> {A,B};
		[A,B,C] -> {A,B,C}
	    end;
	Other -> String
    end.
		    
new_path(Ref,TemplatePath) -> new_path2(Ref,TemplatePath,[]).

new_path2(Ref,[],Acc)                  -> Acc;
new_path2(Ref,[{"auto","incr"}|T],Acc) -> new_path2(Ref,T,Acc++"/"++
						    get_incr(Ref,Acc)++"/");
new_path2(Ref,[{"year",F}|T],Acc)      -> new_path2(Ref,T,Acc++"/"++year(F)++"/");
new_path2(Ref,[{"month",F}|T],Acc)     -> new_path2(Ref,T,Acc++"/"++month(F)++"/");
new_path2(Ref,[{"day",F}|T],Acc)       -> new_path2(Ref,T,Acc++"/"++day(F)++"/");
new_path2(Ref,[{"hour",F}|T],Acc)      -> new_path2(Ref,T,Acc++"/"++hour(F)++"/");
new_path2(Ref,[{"minute",F}|T],Acc)    -> new_path2(Ref,T,Acc++"/"++minute(F)++"/");
new_path2(Ref,[{"second",F}|T],Acc)    -> new_path2(Ref,T,Acc++"/"++second(F)++"/");
new_path2(Ref,[{"weekday",F}|T],Acc)   -> new_path2(Ref,T,Acc++"/"++weekday(F)++"/");
new_path2(Ref,[H|T],Acc)               -> new_path2(Ref,T,Acc++"/"++H).

new_path(A,B,C) -> io:format("in hn_templates:new_path/3 not written yet!~n").

get_incr(Ref,Path) ->
    Path2=string:tokens(Path,"/"),
    io:format("in hn_templates:get_incr Ref is ~p Path is ~p Path2 is ~p~n",
	      [Ref,Path,Path2]),
    Ref2=Ref#ref{path=lists:append(Path2,'_')},
    io:format("in hn_templates:get_next Ref2 are ~p~n",[Ref2]),
    Items = hn_db:get_item(Ref2),
    io:format("in hn_templates:get_next Items are ~p~n",[Items]),
    F = fun(#hn_item{addr=R}) ->
		Last = lists:last(R#ref.path),
		io:format("in hn_templates:get_next Last is ~p~n",[Last]),
		?COND(hn_util:is_numeric(Last),
		      list_to_integer(Last),0)
	end,
    Ind = case Items of
	      [] -> 1;
	      _Else -> lists:max(lists:map(F,Items))+1
	  end,
    io:format("in hn_templates:get_next Ind is ~p~n",[Ind]),
    integer_to_list(Ind).

year(F) -> {{Year,_,_},_}=erlang:localtime(),
	   io:format("in hn_templates:year F is ~p Year is ~p~n",[F,Year]),
	   case F of
	       "yyyy" -> integer_to_list(Year);
	       "yy"   -> get_last_two(integer_to_list(Year))
	   end.

month(F) -> {{_,Month,_},_}=erlang:localtime(),
	    case F of
		"mm"    -> pad_calendar(integer_to_list(Month));
		"m"     -> integer_to_list(Month);
		"long"  -> get_month(Month);
		"short" -> get_short_month(Month)
	    end.

day(F) -> {{_,_,Day},_}=erlang:localtime(),
	  case F of
	      "dd"    -> pad_calendar(integer_to_list(Day));
	      "d"     -> integer_to_list(Day);
	      "long"  -> get_day(Day);
	      "short" -> get_short_day(Day)
	  end.

hour(F) -> {_,{Hour,_,_}}=erlang:localtime(),
	   case F of
	       "hh" -> pad_calendar(integer_to_list(Hour));
	       "h"  -> integer_to_list(Hour)
	   end.

minute(F) -> {_,{_,Minute,_}}=erlang:localtime(),
	     case F of
		 "mm" -> pad_calendar(integer_to_list(Minute));
		 "m"  -> integer_to_list(Minute)
	     end.

second(F) -> {_,{_,_,Second}}=erlang:localtime(),
	     case F of
		 "ss" -> pad_calendar(integer_to_list(Second));
		 "s"  -> integer_to_list(Second)
	     end.

weekday(F) -> {Date,_}=erlang:localtime(),
	  case F of
	      "dd"    -> pad_calendar(integer_to_list(calendar:day_of_the_week(Date)));
	      "d"     -> integer_to_list(calendar:day_of_the_week(Date));
	      "long"  -> get_dayname(calendar:day_of_the_week(Date));
	      "short" -> get_short_dayname(calendar:day_of_the_week(Date))
	  end.
