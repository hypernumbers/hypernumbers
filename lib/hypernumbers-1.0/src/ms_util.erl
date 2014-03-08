%%%-------------------------------------------------------------------
%%% file        ms_util.erl
%%% @copyright (C) 2011 - 2014, Hypernumbers.com
%%% @author     Gordon Guthrie gordon@hypernumbers.com
%%% @doc        this is the match spec utilities module
%%%             it works closely with ms_util2.erl which is
%%%             the generated module that 'introspects'
%%%             the record structures of myheader.hrl
%%% @end
%%% Created     :  3 Sep 2008 by Gordon Guthrie
%%% @private
%%%-------------------------------------------------------------------

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

-module(ms_util).

-export([make_ms/2, make_record/2]).
-import(ms_util2, [get_index/2, no_of_fields/1]).

%%%
%%% External Functions (API)
%%%

make_record(Rec, List) when is_atom(Rec), is_list(List) ->
    make(Rec, List, record).

make_ms(Rec, List) when is_atom(Rec), is_list(List) ->
    make(Rec, List, match_spec).

make(Rec, List, Type) ->
    NoFields = no_of_fields(Rec),
    NewList = proc_list(Rec, List),
    list_to_tuple([Rec | build(NewList, NoFields, Type)]).

%%%
%%% Internal Functions
%%%

proc_list(Rec,List) -> proc_list(Rec,List,[]).

%% bit funky - return the list sorted in reverse order
proc_list(_Rec, [], Acc  )           -> lists:reverse(lists:keysort(1, Acc));
proc_list(Rec, [{Field,B} | T], Acc) ->
    proc_list(Rec, T,[{get_index(Rec, Field), B} | Acc]).

build(List, NoFields, Type) -> bld(List, NoFields, Type,[]).

bld([], 0, _Ty, A)             -> A;
bld([{N, Bits} | T], N, Ty, A) -> bld(T, N-1, Ty, [Bits | A]);
%% don't drop H in the next clause because it matchs later
bld([H | T], N, record, A)     -> bld([H | T], N - 1, record, [[] | A]);
%% don't drop H in the next clause because it matchs later
bld([H | T], N, match_spec, A) -> bld([H | T], N-1, match_spec, ['_' | A]);
bld([], N, record, A)          -> bld([], N - 1, record, [[]  | A]);
bld([], N, match_spec, A)      -> bld([], N - 1, match_spec, ['_' | A]).
