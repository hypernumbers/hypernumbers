%%%-------------------------------------------------------------------
%%% File        : hn_templates.erl
%%% Author      : Gordon Guthrie <gordon@hypernumbers.com>
%%% Description : This module deals with templates
%%%
%%% Created     : 30 Aug 2008 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hn_templates).

-export([write_def/4,
         get_next/3,
         get_next2/3,
         make_path/1,
         get_templates/0]).

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
-include("hypernumbers.hrl").

%%
%% External API
%%

%% @spec write_def(TemplateName,IsDynamic,RootURL,GUI) -> {ok,ok} | {error,Error}
%% @doc takes an incoming template and writes it to the database
write_def(TemplateName,RootURL,GUI,Form) ->
    hn_db:write_template(TemplateName,RootURL,GUI,Form).

%% @spec get_next(Ref,TemplateName, UserName) -> NewName
%% @doc gets the next valid page name
get_next(Ref,TemplateName,UserName) ->
    {ok,Template}=hn_db:read_template(TemplateName),
    {template,TemplateName,TemplatePath,_Gui,_Form}=Template,
    Return=new_path(Ref,TemplatePath,UserName),
    Return.

%% @spec get_next2(Ref,Path,User) -> NewName
%% @doc gets the next valid page name
get_next2(Ref,Path,User) ->
    new_path(Ref,Path,User).

%% @spec make_path(List) -> NewList
%% @doc compiles a path down to a form that can be processed
make_path(List) when is_list(List) ->
    NewList=string:tokens(List,"/"),
    make_path(NewList,[]).

make_path([],Acc)    -> lists:reverse(Acc);
make_path([H|T],Acc) -> make_path(T,[parse(H)|Acc]).

%% @spec get_templates() -> Templates
%% @doc returns all the templates on the system as a list
get_templates() ->
    {ok,Templates}=hn_db:get_templates(),
    Fun=fun(X) ->
                #template{name=Name,temp_path=Path,gui=Gui,form=Form}=X,
                {template,[{name,[],[Name]},{path,[],[repath(Path)]},
                           {gui,[],[Gui]},{form,[],[Form]}]}
        end,
    XML=lists:map(Fun,Templates),
    {ok,{templates,[],XML}}.

%%
%% Internal Functions
%%

repath(Path) -> repath(Path,[]).

repath([],[])           -> "/";
repath([],Acc)          -> lists:concat([lists:reverse(Acc)|["/"]]);
repath([{A,B,C}|T],Acc) -> repath(T,["{"++A++","++B++","++C++"}","/"|Acc]);
repath([{A,B}|T],Acc)   -> repath(T,["{"++A++","++B++"}","/"|Acc]);
repath([{A}|T],Acc)     -> repath(T,["{"++A++"}","/"|Acc]);
repath([H|T],Acc)       -> repath(T,[H,"/"|Acc]).

parse(String) ->
    [H|_Discard]=String,
    T=last(String),
    case {H,T} of
        {${,$}} ->
            String2=lists:sublist(String,2,string:len(String)-2),
            List=string:tokens(String2,","),
            case List of
                [A]     -> {A};
                [A,B]   -> {A,B};
                [A,B,C] -> {A,B,C}
            end;
        _Other -> String
    end.

new_path(Ref,TemplatePath,UserName) -> new_p2(Ref,TemplatePath,UserName,[]).

new_p2(_Ref,[],_UNm,Acc)                -> Acc++"/";
new_p2(Ref,[{"auto","incr"}|T],UNm,Acc) -> new_p2(Ref,T,UNm,Acc++
                                                  "/"++get_incr(Ref,Acc));
new_p2(Ref,[{"year"}|T],UNm,Acc)        -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++year("yyyy"));
new_p2(Ref,[{"year",F}|T],UNm,Acc)      -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++year(F));
new_p2(Ref,[{"month"}|T],UNm,Acc)       -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++month("short"));
new_p2(Ref,[{"month",F}|T],UNm,Acc)     -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++month(F));
new_p2(Ref,[{"day"}|T],UNm,Acc)         -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++day("dd"));
new_p2(Ref,[{"day",F}|T],UNm,Acc)       -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++day(F));
new_p2(Ref,[{"hour"}|T],UNm,Acc)        -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++hour("hh"));
new_p2(Ref,[{"hour",F}|T],UNm,Acc)      -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++hour(F));
new_p2(Ref,[{"minute"}|T],UNm,Acc)      -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++minute("mm"));
new_p2(Ref,[{"minute",F}|T],UNm,Acc)    -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++minute(F));
new_p2(Ref,[{"second"}|T],UNm,Acc)      -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++second("ss"));
new_p2(Ref,[{"second",F}|T],UNm,Acc)    -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++second(F));
new_p2(Ref,[{"weekday"}|T],UNm,Acc)     -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++weekday("short"));
new_p2(Ref,[{"weekday",F}|T],UNm,Acc)   -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++weekday(F));
new_p2(Ref,[{"username"}|T],UNm,Acc)    -> new_p2(Ref,T,UNm,
                                                  Acc++"/"++UNm);
new_p2(Ref,[H|T],UNm,Acc)               -> new_p2(Ref,T,UNm,Acc++"/"++H).

get_incr(_Ref,Path) ->
    Path2=string:tokens(Path,"/"),
    Ref2=ms_util:make_ms(ref,[{path,lists:append(Path2,'_')},
                              {ref,{page,"/"}}]),
    Items = hn_db:get_item(Ref2),
    F = fun(#hn_item{addr=R}) ->
                %% Items seemed to include root pages that 
                %% dont have enough path elements to pull Offset out of
                Offset = length(Path2)+1,
                case Offset > length(R#ref.path) of
                    true  -> 0;
                    false ->
                        Last = lists:nth(Offset,R#ref.path),
                        ?COND(hn_util:is_numeric(Last),
                              list_to_integer(Last),0)
                end
        end,

    Ind = case Items of
              [] -> 1;
              _Else -> lists:max(lists:map(F,Items))+1
          end,
    integer_to_list(Ind).

year(F) -> {{Year,_,_},_}=erlang:localtime(),
           case F of
               "long"  -> integer_to_list(Year);
               "short" -> get_last_two(integer_to_list(Year));
               "yyyy"  -> integer_to_list(Year);
               "yy"    -> get_last_two(integer_to_list(Year))
           end.

month(F) -> {{_,Month,_},_}=erlang:localtime(),
            case F of
                "long"  -> get_month(Month);
                "short" -> get_short_month(Month);
                "mm"    -> pad_calendar(integer_to_list(Month));
                "m"     -> integer_to_list(Month)
            end.

day(F) -> {{_,_,Day},_}=erlang:localtime(),
          case F of
              "long"  -> pad_calendar(integer_to_list(Day));
              "short" -> integer_to_list(Day);
              "dd"    -> pad_calendar(integer_to_list(Day));
              "d"     -> integer_to_list(Day)
          end.

hour(F) -> {_,{Hour,_,_}}=erlang:localtime(),
           case F of
               "long"  -> pad_calendar(integer_to_list(Hour));
               "short" -> integer_to_list(Hour);
               "hh"    -> pad_calendar(integer_to_list(Hour));
               "h"     -> integer_to_list(Hour)
           end.

minute(F) -> {_,{_,Minute,_}}=erlang:localtime(),
             case F of
                 "long"  -> pad_calendar(integer_to_list(Minute));
                 "short" -> integer_to_list(Minute);
                 "mm"    -> pad_calendar(integer_to_list(Minute));
                 "m"     -> integer_to_list(Minute)
             end.

second(F) -> {_,{_,_,Second}}=erlang:localtime(),
             case F of
                 "long"  -> pad_calendar(integer_to_list(Second));
                 "short" -> integer_to_list(Second);
                 "ss"    -> pad_calendar(integer_to_list(Second));
                 "s"     -> integer_to_list(Second)
             end.

weekday(F) -> {Date,_}=erlang:localtime(),
              case F of
                  "long"  -> get_dayname(calendar:day_of_the_week(Date));
                  "short" -> get_short_dayname(calendar:day_of_the_week(Date))
              end.
