%%% @doc    This module handles the number/text formatting for a cell
%%%         Created the 11th March 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
-module(format).

-export([compile_format/1]).

%%% @doc takes a value and a format and returns the tagged value as per 
%%%      the format
%%% @spec format(Value ,Format) -> {tag,FormattedValue} | {error, InvMsg}
%%% @type Value = [string|number|error|boolean|date]
%%% @type InvMsg = string
compile_format(Format)->
    {ok,Tokens,_}=num_format_lexer:string(Format),
    io:format("in format:compile_format Tokens are ~p~n",[Tokens]),
    Output=num_format_parser:parse(Tokens),
    {erlang,Output}.

