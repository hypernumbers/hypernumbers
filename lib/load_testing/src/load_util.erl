%%% @author        gordon <gordon@hypernumbers.com>
%%% @copyright     (C) 2011-2014, gordon
%%% @doc           Utilities for load testing
%%%
%%% @end
%%% Created : 20 Aug 2011 by gordon <gordon@gordon.dev>

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(load_util).


-export([
         make_rolling/2
         ]).

make_rolling(File, ChunkSize) when is_integer(ChunkSize) ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../priv/load_testing/logs/",
    {ok, Handle} = file:open(Dir ++ File, read),
    Average = make_r2(Handle, ChunkSize, 0, 0, []),
    Root = filename:rootname(File),
    Export = Root ++ ".rolling_average."
        ++ integer_to_list(ChunkSize) ++ ".csv",
    io:format("Exporting to file ~p~n", [Dir ++ Export]),
    {ok, Handle2} = file:open(Dir ++ Export, [append]),
    write(Handle2, Average).

write(Handle2, []) -> file:close(Handle2);
write(Handle2, [H | T]) ->
    file:write(Handle2, io_lib:format("~p~n", [H])),
    write(Handle2, T).

make_r2(Handle, ChunkSize, 0, Av, Acc) ->
    io:format("~n"),
    make_r2(Handle, ChunkSize, ChunkSize, 0, [Av/ChunkSize | Acc]);
make_r2(Handle, ChunkSize, N, Av, Acc) ->
    io:format("."),
    case file:read_line(Handle) of
        {ok, Line} ->
            [_, Num] = string:tokens(Line, ","),
            Num2 = string:strip(Num, right, 10), % 10 is \n
            make_r2(Handle, ChunkSize, N - 1, Av + list_to_float(Num2), Acc);
        eof -> file:close(Handle),
               lists:reverse(Acc)
    end.
