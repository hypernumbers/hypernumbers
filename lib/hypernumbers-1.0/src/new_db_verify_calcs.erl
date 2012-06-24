%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       This modules checks if the database has recalced
%%%            correctly. Simply put it checks if all children
%%%            have recalculated more recently than their parents
%%%
%%% @end
%%% Created : 22 Jun 2012 by gordon@vixo.com

-module(new_db_verify_calcs).

-include("spriki.hrl").

-export([
         verify/1,
         fix/1
         ]).

verify(Site) -> verify2(Site, false).

fix(Site) -> verify2(Site, true).

verify2(Site, ShouldFix) ->
    Tbl1 = new_db_wu:trans(Site, item),
    Tbl2 = new_db_wu:trans(Site, relation),
    Fun1 = fun(Item, Count) ->
                   Idx = Item#item.idx,
                   Attrs = binary_to_term(Item#item.attrs),
                   C2 = case lists:keyfind("__lastcalced", 1, Attrs) of
                            false ->
                                Count;
                            {"__lastcalced", Time} ->
                                case mnesia:read(Tbl2, Idx) of
                                    [] ->
                                        Count;
                                    Recs ->
                                        Kids = [X#relation.children || X <- Recs],
                                        Kids2 = lists:flatten(Kids),
                                        check_valid(Kids2, Idx, Site, Time,
                                                    ShouldFix, Count)
                           end
                   end,
                   C2
           end,
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, 1, Tbl1)
           end,
    mnesia:activity(async_dirty, Fun2),
    ok.

check_valid([], _Idx, _Site, _Time, _ShouldFix, Count) ->
    Count;
check_valid([H | T], Idx, Site, Time, ShouldFix, Count) ->
    Tbl1 = new_db_wu:trans(Site, item),
    [C] = mnesia:read(Tbl1, H),
    A = binary_to_term(C#item.attrs),
    {"__lastcalced", Time2} = lists:keyfind("__lastcalced", 1, A),
    NewCount
        = if
              Time2 > Time ->
                  Count;
              Time2 < Time ->
                  Tbl3 = new_db_wu:trans(Site, local_obj),
                  [Par] = mnesia:read(Tbl3, H),
                  [Ch] = mnesia:read(Tbl3, Idx),
                  io:format("Recalc problem: ~p~n"
                            ++ "Young: ~p ~p (~p)~n"
                            ++ "Old:  ~p ~p (~p)~n",
                            [Count,
                             binary_to_term(Par#local_obj.path),
                             hn_util:obj_to_ref(Par#local_obj.obj),
                             H,
                             binary_to_term(Ch#local_obj.path),
                             hn_util:obj_to_ref(Ch#local_obj.obj),
                             Idx]),
                  Count + 1
          end,
    check_valid(T, Idx, Site, Time, ShouldFix, NewCount).
