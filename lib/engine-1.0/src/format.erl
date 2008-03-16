%%% @doc    This module handles the number/text formatting for a cell
%%%         Created the 11th March 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
-module(format).

-export([format/2]).

%%% @doc takes a value and a format and returns the tagged value as per 
%%%      the format
%%% @spec format(Value ,Format) -> {tag,FormattedValue} | {error, InvMsg}
%%% @type Value = [string|number|error|boolean|date]
%%% @type InvMsg = string
format(Value,Format)->
    {ok,Tokens,_}=num_format_lexer:string(Format),
    io:format("in format:format4 Tokens are ~p~n",[Tokens]),
    Output=num_format_parser:parse(Tokens),
    {Value,Output}.

choose(Value,Positive,Negative,Zero) when is_number(Value) -> 
    choose2(Value,Positive,Negative,Zero);
choose(Value,Positive,Negative,Zero) -> Positive.

choose2(Value,Positive,Negative,Zero) ->
    case Value of
        0            -> Zero;
        0.0          -> Zero;
        X when X < 0 -> Negative;
        _            -> Positive
    end.

