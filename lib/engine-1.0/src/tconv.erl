%% Type conversion functions.
%% <hasan@hypernumbers.com>
%% <gordon@hypernumbers.com>

-module(tconv).
-export([to_b26/1,
         to_i/1,
         to_i/2,
         to_l/1,
         to_f/1,
         to_s/1,
         to_num/1]).

%% String -> integer.
to_i(Str) when is_list(Str) ->
    list_to_integer(Str);
%% Number -> integer.
to_i(Num) when is_number(Num) ->
    trunc(Num).
%% Base 26 number as string -> integer.
%% TODO: Can detect when string is base-26.
to_i(Str, b26) when is_list(Str) ->
    b26_to_i(string:to_lower(lists:reverse(Str)), 0, 0).


%% String -> float.
to_f(Str) when is_list(Str) ->
    {ok, [Val], []} = io_lib:fread("~f", Str),
    Val.

%% String -> number.
to_num(Str) when is_list(Str) ->
    try to_i(Str)
    catch
        error:_ ->
            try to_f(Str)
            catch
                error:_ ->
                    {error, nan}
            end
    end;
to_num(Num) when is_number(Num) ->
    Num.

to_s(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_s(Flt) when is_float(Flt) ->
    float_to_list(Flt);
to_s(Str) when is_list(Str) ->
    Str;
to_s(A) when is_atom(A) ->
    atom_to_list(A).

to_l(L) when is_list(L) ->
    L;
to_l(B) when is_binary(B) ->
    binary_to_list(B);
to_l(Num) when is_number(Num) ->
    to_s(Num);
to_l(T) when is_tuple(T) ->
    tuple_to_list(T).

%% @doc Convert an integer to an A1-style column name.
%% @spec to_b26(integer()) -> string()
to_b26(N) when is_integer(N) ->
    to_b26(N - 1, "").

to_b26(N, Res) when N >= 26 ->
    to_b26(N div 26 - 1,
           [(65 + N rem 26) | Res]);
to_b26(N, Res) when N < 26 ->
    [(N + 65) | Res].


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
