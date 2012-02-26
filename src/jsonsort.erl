-module(jsonsort).

-export([sort/1]).

sort(File) ->
    io:format("File is ~p~n", [File]),
    {ok, Bin} = file:read_file(File),
    {array, Term} = mochijson:decode(Bin),
    Fun = fun({struct, Args1}, {struct, Args2}) ->
                  {"fn", A} = proplists:lookup("fn", Args1),
                  {"fn", B} = proplists:lookup("fn", Args2),
                  if A >  B -> false;
                     A <  B -> true
                  end
          end,
    Struct = lists:sort(Fun, Term),
    Json = lists:flatten(mochijson:encode({array, Struct})),
    file:write_file("fns2.json", Json).
