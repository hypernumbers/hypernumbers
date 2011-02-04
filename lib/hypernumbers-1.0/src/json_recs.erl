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
         banjo/0,
         rec_to_json/1,
         json_to_rec/1
        ]).

-include("spriki.hrl").

%%%===================================================================
%%% API
%%%===================================================================
banjo() -> rec_to_json(#wcpagename{template = "dandy", name="randy"}).

rec_to_json(Tuple) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    [Rec | T] = List,
    NoOfFields = ms_util2:no_of_fields(Rec),
    Zip = make_zip(Rec, NoOfFields, []),
    {struct, [{Rec, {array, make_struct(lists:zip(Zip,T))}}]}.

json_to_rec({struct, List}) ->
    json_to_r(List, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================
json_to_r([], Acc) -> lists:reverse(Acc);
json_to_r([{Rec, {array, Fields}} | T], Acc) ->
    Rec2 = list_to_existing_atom(Rec),
    F2 = [{ms_util2:get_index(Rec2, X), Val} || {_, [{X, Val}]} <- Fields],
    {_, F3} = lists:unzip(lists:sort(F2)),
    NewAcc = list_to_tuple([Rec2 | F3]),
    json_to_r(T, [NewAcc | Acc]).
    
make_zip(_Rec, 0, Acc) -> Acc;
make_zip(Rec,  N, Acc) -> NewAcc = ms_util2:name_by_index(Rec, N),
                          make_zip(Rec, N - 1, [NewAcc | Acc]).

make_struct(List) when is_list(List) -> [make_s(X) || X <- List].

make_s({_A, _B} = C) when is_tuple(C) -> {struct, [C]}.

%%%===================================================================
%%% Tests:
%%%===================================================================
-include_lib("eunit/include/eunit.hrl").

rec_test_() ->
    [
     ?_assert(rec_to_json(#refX{}) == "dandy!"),
     ?_assert(rec_to_json(#wcpagename{template = "dandy", name="randy"}) == "dandy!")
    ].
