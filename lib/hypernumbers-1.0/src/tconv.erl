%%% Type conversion functions.
%%% <hasan@hypernumbers.com>
%%% <gordon@hypernumbers.com>
%%% @private

-module(tconv).
-export([to_b26/1,
         to_i/1,
         to_l/1,
         to_f/1,
         to_s/1,
         to_num/1,
         b26_to_i/1]).

%% String -> integer.
to_i(Str) when is_list(Str) ->
    case to_num(Str) of
        {error, nan} ->
            b26_to_i(string:to_lower(lists:reverse(Str)), 0, 0);
        Num ->
            trunc(Num)
    end;
%% Number -> integer.
to_i(Num) when is_number(Num) -> trunc(Num).

%% @doc Convert value to float.
to_f(Str) when is_list(Str) ->
    %
    NStr = re:replace(Str, "^((?:-|\\+)?[0-9]+)(E(?:-|\\+)?[0-9]+)$", %"
                      "\\1.0\\2", [{return, list}]),
    {ok, [Val], []} = io_lib:fread("~f", NStr),
    Val;
to_f(F) when is_float(F) -> F.

%% String -> number.
to_num(Str) when is_list(Str)   ->
    Str2 = string:strip(Str),
    try conv_to_int(Str2)
    catch
        exit : _ ->
            try to_f(Str2)
            catch
                error:
                _ -> {error, nan};
                exit:
                _ -> {error, nan}
            end
    end;

to_num(Num) when is_number(Num) ->   Num.

to_s(DateTime = {datetime, _D, _T}) -> muin_date:to_rfc1123_string(DateTime);
to_s(Int) when is_integer(Int)      -> integer_to_list(Int);
to_s(Flt) when is_float(Flt)        ->
    %% definetaly a better way to test this (3.0 = "3")
    case erlang:trunc(Flt) == Flt andalso Flt < 99999 of
        true  -> integer_to_list(erlang:trunc(Flt));
        false -> string:to_upper(mochinum:digits(Flt))
    end;
to_s(Str) when is_list(Str)         -> Str;
to_s(A) when is_atom(A)             -> atom_to_list(A).

to_l(L) when is_list(L)       -> L;
to_l(B) when is_binary(B)     -> binary_to_list(B);
to_l(Num) when is_number(Num) -> to_s(Num);
to_l(T) when is_tuple(T)      -> tuple_to_list(T).

%% @doc Convert an integer to an A1-style column name.
%% @spec to_b26(integer()) -> string()
to_b26(N) when is_integer(N) -> to_b26(N - 1, "").
to_b26(N, Res) when N >= 26  -> to_b26(N div 26 - 1,[(65 + N rem 26) | Res]);
to_b26(N, Res) when N < 26   -> [(N + 65) | Res].

%% @doc Convert an string to a decimal integer
%% @spec b26_to_i(string()) -> integer()

b26_to_i(List) when is_list(List) ->
    b26_to_i(string:to_lower(lists:reverse(List)),0,0).

%% private functions
b26_to_i([], _Power, Value) -> 
    Value;

b26_to_i([H|T],Power,Value)->
    NewValue = case (H > 96) andalso (H < 123) of
                   true ->
                       round((H - 96) * math:pow(26, Power));
                   _    ->
                       erlang:error([H | T] ++ " is not a valid base 26 number")
               end,
    b26_to_i(T, Power + 1, NewValue + Value).

conv_to_int(Str) when is_list(Str) ->
    try list_to_integer(Str)
    catch
        error:_ ->
            case string:tokens(Str, "e+") of
                [Int, Exp] ->
                    I2 = to_num(Int),
                    E2 = to_num(Exp),
                    case {I2, E2} of
                        {{error, nan}, _} -> exit("not integer");
                        {_, {error, nan}} -> exit("not integer");
                        {I3, E3}          -> I3 * math:pow(10, E3)
                    end;
                _          -> exit("not integer")
            end
    end.

