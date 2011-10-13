%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumers Ltd
%%% @doc       A module to verify the status
%%%            of a database
%%%
%%% @end
%%% Created : 12 Oct 2011 by gordon@hypernumbers.com

-module(new_db_verify).

-include("spriki.hrl").

% main api
-export([
         check/0,
         check/3
        ]).

% local_objs is the major table
% it defines the idx's that are used across the whole estate...
-export([
         check_local_obj/3
         ]).

% these are lesser tables that need checking
-export([
         check_form/3,
         check_include/3,
         check_item/3,
         check_logging/3,
         check_relation/3,
         check_timer/3
        ]).

% main api
check() ->
    Sites = hn_setup:get_sites(),
    [check(X, verbose, dontfix) || X <- Sites].

check(Site, Verbose, Fix) ->
    io:format("Checking ~p with ~p ~p~n", [Site, Verbose, Fix]),
    case check_local_obj(Site, Verbose, Fix) of
        []   -> check2(Site, Verbose, Fix);
        List -> io:format("local objs borked~n~p~n", [List])
    end.

check2(Site, Verbose, Fix) ->
    BrokenForms       = check_form(Site, Verbose, Fix),
    BrokenIncs        = check_include(Site, Verbose, Fix),
    BrokenItems       = check_item(Site, Verbose, Fix),
    BrokenLogs        = check_logging(Site, Verbose, Fix),
    {BrokenRels, Num} = check_relation(Site, Verbose, Fix),
    BrokenTimers      = check_timer(Site, Verbose, Fix),
    case Verbose of
        verbose ->
            io:format("BrokenForms  is ~p~n"
                      ++ "BrokenIncs   is ~p~n"
                      ++ "BrokenItems  is ~p~n"
                      ++ "BrokenLogs   is ~p~n"
                      ++ "BrokenRels   is ~p~n"
                      ++ "BrokenTimers is ~p~n",
                      [BrokenForms, BrokenIncs, BrokenItems,
                       BrokenLogs, BrokenRels, BrokenTimers]);
        _ ->
            io:format("No of Broken: Forms: ~p "
                      ++ "Incs: ~p "
                      ++ "Items: ~p "
                      ++ "Logs: ~p "
                      ++ "Rels: ~p "
                      ++ "Timers: ~p~n",
                      [length(BrokenForms), length(BrokenIncs),
                       length(BrokenItems), length(BrokenLogs),
                       Num, length(BrokenTimers)])
    end.

% master table check
check_local_obj(Site, V, _Fix) ->
    Tbl1 = new_db_wu:trans(Site, relation),
    Fun1 = fun(X, Acc) ->
                   #local_obj{idx = I, path = P, obj = O, revidx = R} = X,
                   % check that the revidx is the correct
                   P2 = hn_util:list_to_path(binary_to_term(P)),
                   O2 = hn_util:obj_to_ref(O),
                   R2 = binary_to_term(R),
                   NA = case P2 ++ O2 of
                            R2 -> [];
                            _  ->
                                write(V,"Revidx is borked ~p ~p~n",
                                      [R2, P2 ++ O2]),
                                I
                        end,
                   % If the local_obj is a cell is MUST have one and
                   % only relation record - if it is not a cell it MUST NOT
                   % have a relation record
                   NA2 = case {mnesia:read(Tbl1, I, read), O} of
                             {[], {cell, _}} ->
                                 write(V, "no rel for ~p~n", [I]),
                                 I;
                             {[_Rec], {cell, _}} ->
                                 [];
                             {[Rec], _} ->
                                 write(V, "shouldn't have a rel ~p for ~p~n",
                                       [Rec, I]),
                                 I;
                             {[], _} ->
                                 [];
                             List ->
                                 write(V, "many rels ~p for ~p~n",
                                       [List, I]),
                                 I
                         end,
                   NA3 = hslists:uniq(lists:flatten([NA, NA2])),
                   lists:merge(NA3, Acc)
           end,
    io:format("Checking site ~p~n", [Site]),
    Tbl2 = new_db_wu:trans(Site, local_obj),
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, [], Tbl2)
           end,
    mnesia:activity(transaction, Fun2).

%% simple checks for duff idx's
check_form(Site, _V, _Fix) ->
    io:format("fix index of form~n"),
    _Borked = check_table(Site, form).

check_include(Site, _V, _Fix) ->
    _Borked = check_table(Site, include).

check_item(Site, _V, _Fix) ->
    _Borked = check_table(Site, item).

check_logging(Site, _V, _Fix) ->
    _Borked = check_table(Site, logging).

check_relation(Site, V, _Fix) ->
    Tbl = new_db_wu:trans(Site, relation),
    Fun1 = fun(Rel, {IAcc, N}) ->
                   #relation{cellidx = Idx, children = C, parents = P,
                             infparents = Inf, z_parents = Z} = Rel,
                   Borked = check_rel2([{children, C}, {parents, P},
                                    {inf, Inf}, {zs, Z}], Tbl, Idx, V, []),
                   case summarise(Borked, 0) of
                       0 -> {IAcc, N};
                       I -> io:format("~p borked rels~n", [I]),
                            {[Idx | IAcc], N + I}
                   end
           end,
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, {[], 0}, Tbl)
           end,
    mnesia:activity(transaction, Fun2).

summarise([], N) -> N;
summarise([{_, List} | T], N) -> summarise(T, length(List) + N).

check_rel2([], _Tbl, _Idx, _V, [{zs, []}, {inf, []}, {parents, []},
                                {children, []}]) ->
    [{zs, []}, {inf, []}, {parents, []}, {children, []}];
check_rel2([], _Tbl, Idx, V, Acc) ->
    write(V, "The following idxs are borked in the "
          ++ "relations record for ~p~n~p~n", [Idx, Acc]),
    Acc;
check_rel2([{Type, I} | T], Tbl, Idx, V, Acc) ->
    NewAcc = [check_rel3(I, Type, Idx, Tbl, V, []) | Acc],
    check_rel2(T, Tbl, Idx, V, NewAcc).

check_rel3([], Type, _Idx, _Tbl, _V, Acc) -> {Type, Acc};
check_rel3([H | T], Type, Idx, Tbl, V, Acc) ->
    NewAcc = case mnesia:read(Tbl, H, read) of
                 []    -> write(V, "~p: ~p of ~p doesn't exist~n",
                                [Type, H, Idx]),
                          [H | Acc];
                 _List -> Acc
             end,
    NewAcc2 = case check_rel4(Type, H, Idx, Tbl, V) of
                  ok    -> NewAcc;
                  error -> [H | Acc]
              end,
    check_rel3(T, Type, Idx, Tbl, V, NewAcc2).

% the relation record for each child MUST have the Idx as a parent
check_rel4(children, C, Idx, Tbl, V) ->
    case mnesia:read(Tbl, C, read) of
        []  -> write(V, "The child ~p of ~p doesn't exist~n", [C, Idx]),
               error;
        [R] -> #relation{parents = P, infparents = I, z_parents = Z} = R,
               Ps = lists:flatten([P, I, Z]),
               case lists:member(Idx, Ps) of
                   true  -> ok;
                   false -> write(V, "The relation of the child ~p of ~p "
                                  ++ "doesn't have it as a parent~n",
                                  [C, Idx]),
                            error
               end
    end;
% the relation record for each parent MUST have the idx as a child
check_rel4(Type, P, Idx, Tbl, V) ->
    case mnesia:read(Tbl, P, read) of
        []  -> write(V, "The parent ~p: ~p of ~p doesn't exist~n",
                     [Type, P, Idx]),
               error;
        [R] -> #relation{children = C} = R,
               case lists:member(Idx, C) of
                   true  -> ok;
                   false -> write(V, "The relation of the parent ~p: ~p of ~p"
                                  ++ "doenst have it as a child~n",
                                  [Type, P, Idx]),
                            error
               end
    end.

check_timer(Site, _V, _Fix) ->
    _Borked = check_table(Site, timer).

%%%
%%% Internal Functions
%%%
check_table(Site, Table) ->
    Tbl = new_db_wu:trans(Site, Table),
    Tbl2 = new_db_wu:trans(Site, local_obj),
    Index = case Table of
                form -> ms_util2:get_index(Table, key);
                _    -> ms_util2:get_index(Table, idx)
            end,
    Fun1 = fun(X, Acc) ->
                   Idx = erlang:element(Index + 1, X),
                   NewAcc = case mnesia:read(Tbl2, Idx, read) of
                       []   -> [Idx | Acc];
                       [_R] -> Acc
                   end,
                  NewAcc
          end,
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, [], Tbl)
           end,
    mnesia:activity(transaction, Fun2).

write(verbose, Msg, Data) -> io:format(Msg, Data);
write(_, _, _)            -> ok.
