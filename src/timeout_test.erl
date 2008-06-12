-module(timeout_test).

-import(lists, [foreach/2, map/2]).
-import(test_util, [conv_for_post/1, conv_from_get/1, cmp/2, hnpost/3, hnget/2, readxls/1]).

-export([run_test/0]).

run_test() ->
    %%bits:clear_db(),
    %%test_util:wait(),
    %%io:format("Current path:~n"),
    %%c:pwd(),
    
    gen_server:cast(dirty_cell,{setstate,passive}),
    Celldata = readxls("../../tests/excel_files/Win_Excel07_As_97/timeout_tests.xls"),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "timeout_test" ++ "/" ++ Sheetname ++ "/",
                Ref = tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1),
                hnpost(Path, Ref, Postdata)
        end,
    foreach(Postcell, Celldata),
    io:format("finish posting ~n",[]),
    gen_server:cast(dirty_cell,{setstate,active}),
    gen_server:call(dirty_cell,flush,infinity),
    {ok,ok}.

