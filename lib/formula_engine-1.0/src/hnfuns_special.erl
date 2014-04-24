%%% @author    Gordon Guthrie
%%% @copyright (C) 2011-2014, Hypernumbers Ltd
%%% @doc       This module implements special
%%%            hypernumbers functions that are
%%%            sui generis
%%% @end
%%% Created :  2 May 2011 by gordon@hypernumbers.com

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

-module(hnfuns_special).

-export([
         'debug.array.'/1,
         array1d/1,
         timestamp/1,
         tick/1,
         snapshot/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

'debug.array.'([W, H | Rest]) ->
    [W2, H2] = typechecks:std_ints([W, H]),
    case Rest of
        [{array, Contents}] ->
            Text = lists:flatten(print_array(Contents, [])),
            Txt2 = lists:flatten(io_lib:format("~p", [Text])),
            Len = length(Txt2),
            % strip of the first and last quotes and then add curly brackets
            Txt3 = [[123], lists:sublist(Txt2, 2, Len - 2),[125]],
            Resize = #resize{width = W2, height = H2},
            #spec_val{val = lists:flatten(Txt3), resize = Resize};
         Other ->
            io:format("Other is ~p~n", [Other]),
            ?ERRVAL_VAL
    end.

print_array([], Acc)      -> Body = lists:reverse(Acc),
                             string:join(Body, ";");
print_array([H | T], Acc) -> NewAcc = print_elems(H, []),
                             print_array(T, [NewAcc | Acc]).

print_elems([], Acc)      -> lists:flatten(string:join(lists:reverse(Acc), ","));
print_elems([H | T], Acc) -> NewAcc = io_lib:format("~p", [H]),
                             print_elems(T, [NewAcc | Acc]).

array1d(Args) ->
    Array = array(Args, []),
    Arr2 = muin_collect:col(Array, [fetch, flatten]),
    % yeah, oh, why don't we just merge the two collections, no?
    % bad idea....
    Arr3 = muin_collect:col(Arr2, [blank_as_str]),
    {array, [Arr3]}.

array([], Acc)                  -> lists:reverse(Acc);
array([{array, List} | T], Acc) -> array(T, flatten2d(List, Acc));
array([{range, List} | T], Acc) -> array(T, flatten2d(List, Acc));
array([H | T], Acc)             -> array(T, [H | Acc]).

% arrays and ranges are 2D
% flattens the list by sticking the elements individually onto
% the accumulator of the parent function
flatten2d([], Acc)      -> Acc;
flatten2d([H | T], Acc) -> flatten2d(T, flattenon(H, Acc)).

flattenon([], List)     -> List;
flattenon([H | T], Acc) -> flattenon(T, [H | Acc]).

snapshot([Arg]) ->
    Rules = [first_array, fetch_name, fetch_ref, eval_funs, blank_as_str],
    Passes = [],
    [Ret] = muin_collect:col([Arg], Rules, Passes),
    % snapshot works by killng the parents so the fn never relcalcs
    muin:pd_store(finite_refs, []),
    muin:pd_store(infinite_refs, []),
    Ret.

timestamp(_List) -> stdfuns_date:now([]).

tick([]) -> tick([1]);
tick(Options) ->
    Opts = typechecks:std_ints(Options),
    Spec = case Opts of
               [0]    -> everyminute;
               [1]    -> hourly;
               [2]    -> daily;
               [3]    -> {weekly, 1}; % monday
               [3, 1] -> {weekly, 1}; % monday
               [3, 2] -> {weekly, 2}; % tuesday
               [3, 3] -> {weekly, 3}; % wednesday
               [3, 4] -> {weekly, 4}; % thursday
               [3, 5] -> {weekly, 5}; % friday
               [3, 6] -> {weekly, 6}; % saturday
               [3, 7] -> {weekly, 7}; % sunday
               [4]    -> {monthly, 1};
               [4, N] when N > 0 andalso N < 31 -> {monthly, N};
               _      -> ?ERR_VAL
           end,
    #spec_val{val = stdfuns_date:now([]), sp_timer = #sp_timer{spec = Spec}}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

