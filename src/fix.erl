-module(fix).

-export([fix/0, clean/0]).

-record(item,
        {
          idx,
          key,
          val
         }).

fix() ->
    Table = '127.0.0.1&9001&item',
    % Table = '192.168.56.101&9000&item',
    Fun = fun() ->
                  Pattern = {'_', '_', "value", '_'},
                  mnesia:match_object(Table, Pattern, read)
          end,
    {atomic, Recs} = mnesia:transaction(Fun),
    dump(Recs).

dump([]) -> ok;
dump([H | T]) ->
    #item{idx = I, key = K, val = V} = H,                
    io:format("I is ~p K is ~p V is ~p~n", [I, K, V]),
    case V of
        {xml, _}         -> io:format("bit of XML here...~n");
        {errval, _}      -> io:format("error here...~n");
        {datetime, _, _} -> io:format("datetime here...~n");
        _                -> (mochijson:encoder([{input_encoding, utf8}]))(V)
    end,
    dump(T).


clean() ->
    Keys = [1258641103278839],
    Table = '127.0.0.1&9001&item',
    [clean1(K, Table) || K <- Keys].

clean1(K, Table) ->
    Fun =
        fun() ->
                Pattern = {'_', K, "value", '_'},
                Record = mnesia:match_object(Table, Pattern, read),
                io:format("Record is ~p~n", [Record]),
                case Record of
                    []   -> ok;
                    [R] -> Result1 = mnesia:delete(Table, R, write),
                           io:format("Result1 is ~p~n", [Result1])
                end,
                R2 = #item{idx = K, key = "value", val = "swapped out by GG"},
                Result = mnesia:write(Table, R2, write),
                io:format("Result is ~p~n", [Result])
        end,
    Ret = mnesia:transaction(Fun),
    io:format("Ret is ~p~n", [Ret]).

