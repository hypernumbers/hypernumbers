%% Type conversion functions.
%% <hasan@hypernumbers.com>
%% <gordon@hypernumbers.com>

-module(tconv).
-export([to_b26/1,
         to_i/1,
         to_i/2,
         to_f/1,
         to_s/1]).

%% String -> integer.
to_i(Str) when is_list(Str) ->
    {ok, [Val], []} = io_lib:fread("~d", Str),
    Val.

%% Base 26 number as string -> integer.
to_i(Str, b26) when is_list(Str) ->
    b26_to_i(string:to_lower(lists:reverse(Str)), 0, 0).


%% String -> float.
to_f(Str) when is_list(Str) ->
    {ok, [Val], []} = io_lib:fread("~f", Str),
    Val.


%% Integer -> string.
to_s(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_s(Flt) when is_float(Flt) ->
    float_to_list(Flt);
to_s(Str) when is_list(Str) ->
    Str.

%% Integer -> base 26 number as string.
to_b26(Int) when is_integer(Int) ->
    to_b26(Int, []).

to_b26(0, Value) ->
    Value;

to_b26(Int, Value) ->
    Div = Int/26,
    DivInt = trunc(Div),
    Alpha = Int - 26 * DivInt + 96,
    to_b26(DivInt, [Alpha | Value]).


%%% ----------------- %%%
%%% Private functions %%%
%%% ----------------- %%%

b26_to_i([], _Power, Value) ->
    Value;

b26_to_i([H|T],Power,Value)->
    NewValue = case (H > 96) andalso (H < 123) of
                   true ->
                       round((H - 96) * math:pow(26, Power));
                   _    ->
                       exit([H | T] ++ " is not a valid base 26 number")
               end,
    b26_to_i(T, Power + 1, NewValue + Value).
