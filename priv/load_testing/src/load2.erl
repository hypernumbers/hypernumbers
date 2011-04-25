-module(load2).

-export([test/0]).

-record(local_obj,
        {
          idx,
          type,
          path,
          obj,
          revidx
         }).

test() -> test_repeat(100).

test_repeat(0) -> ok;
test_repeat(N) -> ok = create_tables(),
                  test1(5000),
                  ok = delete_tables(),
                  test_repeat(N - 1).

test1(0) -> ok;
test1(N) -> test2(N, 50),
            test1(N - 1).

test2(_, 0) -> ok;
test2(N, M) -> test3(N, M   ),
               test2(N, M - 1).

test3(N, M) ->
    Idx = util2:get_timestamp(),
    Path = N,
    Obj = M,
    RevIdx = Idx,
    R = #local_obj{idx = Idx, path = Path, obj = Obj, revidx = RevIdx},
    % now set up the write fns
    Fun1 = fun() ->
                   mnesia:write(local_obj1, R, write)
           end,
    Fun2 = fun() ->
                   mnesia:write(local_obj2, R, write)
           end,
    Fun3 = fun() ->
                   mnesia:write(local_obj3, R, write)
           end,
    N1 = util2:get_timestamp(),
    mnesia:activity(transaction, Fun1),
    N2 = util2:get_timestamp(),
    mnesia:activity(transaction, Fun2),
    N3 = util2:get_timestamp(),
    mnesia:activity(transaction, Fun3),
    N4 = util2:get_timestamp(),
    Rem = trunc(N/50) - (N/50),
    case {M, Rem} of
        {1, 0.0} -> io:format("logging ~p ~p~n", [N, M]),
                    log(io_lib:format("~p\t~p\t~p\t~p\t~p",
                                      [N, M, (N2 - N1)/100000,
                                       (N3 - N2)/100000,
                                       (N4 - N3)/100000]));
        _        -> ok
    end,
    ok.

tables() -> [{local_obj1, []}, {local_obj2, [path]}, {local_obj3, [path, revidx]}].

create_tables() ->

    Tables = tables(),
    [mnesia:create_table(Table, [{record_name, local_obj},
                                 {attributes, record_info(fields, local_obj)},
                                 {disc_copies, [node()]},
                                 {type, set},
                                 {local_content, true},
                                 {index, I}]) || {Table, I} <- Tables],
    ok.

delete_tables() ->
    Tables = tables(),
    [mnesia:delete_table(Table) || {Table, _} <- Tables],
    ok.

log(String) ->
    log(String, "../logs/indices_test.txt").

log(String, File) ->
    _Return=filelib:ensure_dir(File),

    case file:open(File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [String]),
	    file:close(Id);
	_ ->
	    error
    end.
