%%% @doc    This module handles the number/text formatting for a cell
%%%         Created the 11th March 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
-module(format).

-export([
          compile_format/1,
          run_format/2,
          format/2
        ]).

%%% @doc takes a value and a format and returns the tagged value as per 
%%%      the format
%%% @spec format(Value ,Format) -> {tag,FormattedValue} | {error, InvMsg}
%%% @type Value = [string|number|error|boolean|date]
%%% @type InvMsg = string
compile_format(Format)->
    {ok,Tokens,_}=num_format_lexer:string(Format),
    {ok,Output}=num_format_parser:parse(Tokens),
    {erlang,Output}.
    
run_format(X,Src)->
  {ok,ErlTokens,_}=erl_scan:string(Src),
  {ok,ErlAbsForm}=erl_parse:parse_exprs(ErlTokens),
  {value,Fun,_}=erl_eval:exprs(ErlAbsForm,[]),
  Fun(X).

format(X,Format)->
  format(X,Format,[]).
  
format(X,[],Acc)    -> lists:flatten(lists:reverse(Acc));
format(X,[H|T],Acc) -> format(X,T,[execute(X,H)|Acc]).

execute(X,{format,F}) -> X;
execute(X,{year, two_digit}) -> X;
execute(X,{year, four_digit}) -> X;
execute(X,{mon_min, no_zero}) -> X;
execute(X,{mon_min, zero}) -> X;
execute(X,{mon, abbr}) -> X;
execute(X,{mon, full}) -> X;
execute(X,{day, no_zero}) -> X;
execute(X,{day, zero}) -> X;
execute(X,{day, abbr}) -> X;
execute(X,{day, full}) -> X;
execute(X,{hour, no_zero}) -> X;
execute(X,{hour, zero}) -> X;
execute(X,{sec, no_zero}) -> X;
execute(X,{sec, zero}) -> X;
execute(X,{ampm, full_caps}) -> X;
execute(X,{ampm, full_lowercase}) -> X;
execute(X,{ampm, abbr_caps}) -> X;
execute(X,{ampm, abbr_lowercase}) -> X;
execute(X,{at,_}) -> X.
