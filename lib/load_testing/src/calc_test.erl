%%% @author    Gordon Guthrie
%%% @copyright (C) 2012-2014 Hypernumbers Ltd
%%% @doc       A module for testing intensive calcs
%%%
%%% @end
%%% Created : 24 Jun 2012 by gordon@vixo.com

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

-module(calc_test).

-export([
         run/0,
         process/1
         ]).

run() ->
    Site = "http://x.hypernumbers.dev:9000",
    case hn_setup:site_exists(Site) of
        true  -> hn_setup:delete_site(Site);
        false -> ok
    end,
    % cprof:start(),
    % last param can be false | mem | disc_and_mem
    hn_setup:load_TEST(Site, 'vixo-business-model', false),
    io:format("Site created - wait for dirties to clear...~n"),
    ok = new_db_api:wait_for_dirty(Site),
    % cprof:pause(),
    % {_Total, Log} = cprof:analyse(),
    % Stamp = "." ++ dh_date:format("Y_M_d_H_i_s"),
    % File = "cprof" ++ Stamp ++ ".output",
    % Msg = io_lib:format("~w", [Log]),
    % hn_util:log(Msg, File),
    ok.

process(File) ->
    {ok, Terms} = file:consult(File),
    Result = process2(Terms, []),
    io:format("~p~n", [Result]),
    ok.

process2([], Acc) ->
    process3(Acc, []);
process2([[{K, V}] | T], Acc) ->
    NewAcc = case lists:keyfind(K, 1, Acc) of
                 false     -> [{K, V} | Acc];
                 {K, OldV} -> NewKV = {K, lists:flatten([V | OldV])},
                              lists:keyreplace(K, 1, Acc, NewKV)
             end,
    process2(T, NewAcc).

process3([], Acc) ->
    process5(Acc, []);
process3([{K, List} | T], Acc) ->
    NewAcc = {K, process4(List, [])},
    process3(T, [NewAcc | Acc]).

process4([], Acc) ->
    Acc;
process4([{ts, {_, Fn, Arity}, Calls, Time} | T], Acc) ->
    NewAcc = case lists:keyfind({Fn, Arity}, 1, Acc) of
                 false ->
                     [{{Fn, Arity}, {Calls, Time}} | Acc];
                 {{Fn, Arity}, {OldCalls, OldTime}} ->
                     Val = {Calls + OldCalls, Time + OldTime},
                     NewKV = {{Fn, Arity}, Val},
                     lists:keyreplace({Fn, Arity}, 1, Acc, NewKV)
             end,
    process4(T, NewAcc).

process5([], Acc) ->
    Fun = fun({_, _, _, _, A}, {_, _, _, _, B}) ->
                  if A >  B -> true;
                     A =< B -> false
                  end
          end,
    lists:sort(Fun, lists:flatten(Acc));
process5([{Mod, List} | T], Acc) ->
    NewAcc = process6(List, Mod, []),
    process5(T, [NewAcc | Acc]).

process6([], _Mod, Acc) ->
    Acc;
process6([{{Fn, Arity}, {Calls, Time}} | T], Mod, Acc) ->
    NewAcc = {Mod, Fn, Arity, Calls, Time},
    process6(T, Mod, [NewAcc | Acc]).
