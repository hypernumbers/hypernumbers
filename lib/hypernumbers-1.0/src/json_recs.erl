%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       This module converts Erlang records into json
%%%            and vice-versa
%%%
%%% @end
%%% Created :  1 Feb 2011 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(json_recs).

%% API
-export([
         rec_to_json/1,
         json_to_rec/1
        ]).

-include("spriki.hrl").

%%%===================================================================
%%% API
%%%===================================================================
rec_to_json(Tuple) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    [Rec | T] = List,
    NoOfFields = ms_util2:no_of_fields(Rec),
    Zip = make_zip(Rec, NoOfFields, []),
    {struct, [{Rec, {array, make_struct(lists:zip(Zip,rec_to_json2(T, [])))}}]};
% if the thang aint a tuple, it aint a record
rec_to_json(A) -> A.

json_to_rec({struct, [List]}) -> json_to_r(List);
json_to_rec(A)              -> A.

%%%===================================================================
%%% Internal functions
%%%===================================================================
rec_to_json2([], Acc)      -> lists:reverse(Acc);
rec_to_json2([H | T], Acc) -> rec_to_json2(T, [rec_to_json(H) | Acc]).

json_to_r({Rec, {array, Fields}}) ->
    Rec2 = convert(Rec),
    F2 = [{ms_util2:get_index(Rec2, X), Val} || {_, [{X, Val}]} <- Fields],
    {_, F3} = lists:unzip(lists:sort(F2)),
    list_to_tuple([Rec2 | json_to_r2(F3, [])]).

json_to_r2([], Acc)      -> lists:reverse(Acc);
json_to_r2([H | T], Acc) -> json_to_r2(T, [json_to_rec(H) | Acc]).

make_zip(_Rec, 0, Acc) -> Acc;
make_zip(Rec,  N, Acc) -> NewAcc = ms_util2:name_by_index(Rec, N),
                          make_zip(Rec, N - 1, [NewAcc | Acc]).

% when you round trip via a web brower records come back as lists
% if you round trip via the test suite they don't so you need to
% put this convert step in...
convert(X) when is_atom(X) -> X;
convert(X) when is_list(X) -> list_to_existing_atom(X).

make_struct(List) when is_list(List) -> [make_s(X) || X <- List].

make_s({_A, _B} = C) when is_tuple(C) -> {struct, [C]}.

%%% Tests:
-include_lib("eunit/include/eunit.hrl").

json_test_() ->
    A = #local_obj{idx = 1, type = 2, path = 3, obj = 4, revidx = 5},
    B = #local_obj{idx = 1, type = 2, path = 3},
    C = #local_obj{idx = 1, type = 2, path = 3, obj = A, revidx = B},
    [
     ?_assert(A == json_to_rec(rec_to_json(A))),
     ?_assert(B == json_to_rec(rec_to_json(B))),
     ?_assert(C == json_to_rec(rec_to_json(C)))
    ].
