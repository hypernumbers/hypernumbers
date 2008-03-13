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
    %% io:format("Got to format~n"),
    Formats=string:tokens(Format,";"),
    format2(Value,Formats).

format2(Value,[One])->
    %% io:format("Got to format2 (1)~n"),
    Positive=One,
    Negative=One,
    Zero=One,
    Text="",
    format3(Value,Positive,Negative,Zero,Text);
format2(Value,[One,Two,Three]) -> 
    %% io:format("Got to format2 (2)~n"),
    Positive=One,
    Negative=Two,
    Zero=Three,
    Text="",
    format3(Value,Positive,Negative,Zero,Text);
format2(Value,[One,Two,Three,Four]) -> 
    %% io:format("Got to format2 (3)~n"),
    Positive=One,
    Negative=Two,
    Zero=Three,
    Text=Four,
    format3(Value,Positive,Negative,Zero,Text);
format2(_Value,Other) -> 
    %% io:format("Got to format2 (4)~n"),
    {error,"wrong number of formats"}.

format3(Value,Positive,Negative,Zero,Text)->
    %% io:format("Got to format3~n"),
    Format=choose(Value,Positive,Negative,Zero),
    format4(Value,Format).

format4(Value,Format)->
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

