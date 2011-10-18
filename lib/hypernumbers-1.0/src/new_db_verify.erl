%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumers Ltd
%%% @doc       A module to verify the status
%%%            of a database
%%%
%%% @end
%%% Created : 12 Oct 2011 by gordon@hypernumbers.com

-module(new_db_verify).

-define(m, mnesia:table_info).

-include("spriki.hrl").

% main api
-export([
         check/0,
         check/3
        ]).

% zinfs need their own checking
-export([
         check_zinfs/3
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
         check_relation/3,
         check_timer/3
        ]).

% the verbose writer needs to be used elsewhere
-export([
         write/3
        ]).


% main api
check() ->
    Sites = hn_setup:get_sites(),
    io:format("about to check ~p sites...~n", [length(Sites)]),
    Fun = fun(X) ->
                  io:format("."),
                  check(X, quiet, dontfix)
          end,
    [Fun(X) || X <- Sites],
    io:format("Over and out...~n"),
    ok.

check(Site, Verbose, Fix) ->
    write(Verbose, "Checking ~p with ~p ~p~n", [Site, Verbose, Fix]),
    case check_local_obj(Site, Verbose, Fix) of
        []   -> check2(Site, Verbose, Fix);
        List -> write(Verbose, "local objs borked~n~p~n", [List]),
                Tbl = new_db_wu:trans(Site, local_obj),
                Size = mnesia:table_info(Tbl, size),
                io:format("in ~p ~p out of ~p local_objs are borked~n",
                          [Site, length(List), Size])
    end.

check2(Site, Verbose, Fix) ->
    Borked = check_local_objs2(Site, Verbose),
    BrokenZinfs       = check_zinfs(Site,    Verbose, Fix),
    BrokenForms       = check_form(Site,     Verbose, Fix),
    BrokenIncs        = check_include(Site,  Verbose, Fix),
    BrokenItems       = check_item(Site,     Verbose, Fix),
    {BrokenRels, Num} = check_relation(Site, Verbose, Fix),
    BrokenTimers      = check_timer(Site,    Verbose, Fix),
    case Verbose of
        verbose ->
            io:format("Borked local_objs are ~p~n", [Borked]),
            io:format("BrokenZinfs  is ~p~n"
                      ++ "BrokenForms  is ~p~n"
                      ++ "BrokenIncs   is ~p~n"
                      ++ "BrokenItems  is ~p~n"
                      ++ "BrokenRels   is ~p~n"
                      ++ "BrokenTimers is ~p~n",
                      [BrokenZinfs, BrokenForms, BrokenIncs,
                       BrokenItems, BrokenRels, BrokenTimers]);
        _ ->
            SizeLObjs  = ?m(new_db_wu:trans(Site, local_obj), size),
            SizeForms  = ?m(new_db_wu:trans(Site, form),       size),
            SizeIncs   = ?m(new_db_wu:trans(Site, include),    size),
            SizeItems  = ?m(new_db_wu:trans(Site, item),       size),
            SizeRels   = ?m(new_db_wu:trans(Site, relation),   size),
            SizeTimers = ?m(new_db_wu:trans(Site, timer),      size),
            Errors = length(Borked)
                + length(BrokenZinfs)
                + length(BrokenForms)
                + length(BrokenIncs)
                + length(BrokenItems)
                + Num
                + length(BrokenTimers),
            case Errors of
                0 ->
                    write(Verbose, "No errors~n", []);
                _N ->
                    io:format("~nSite: ~p~nNo of borked local_objs:~p "
                              ++ "out of ~p~n",
                              [Site, length(Borked), SizeLObjs]),
                    io:format("No of Broken:~n"
                              ++ "Zinfs:  ~p~n"
                              ++ "Forms:  ~p out of ~p~n"
                              ++ "Incs:   ~p out of ~p~n"
                              ++ "Items:  ~p out of ~p~n"
                              ++ "Rels:   ~p out of ~p~n"
                              ++ "Timers: ~p out of ~p~n",
                              [length(BrokenZinfs),
                               length(BrokenForms), SizeForms,
                               length(BrokenIncs),  SizeIncs,
                               length(BrokenItems), SizeItems,
                               Num, SizeRels,
                               length(BrokenTimers), SizeTimers])
            end
    end.

% second pass check of local_objs
check_local_objs2(Site, Verbose) ->
    Tbl = new_db_wu:trans(Site, local_obj),
    F2 = fun(LO, {N, Acc}) ->
                 #local_obj{path = P, obj = O, revidx = R} = LO,
                 P2 = binary_to_term(P),
                 Pattern = {local_obj, '_', '_', '_', '_', R},
                 case mnesia:index_match_object(Tbl, Pattern, 6, read) of
                     [_I] ->
                         {N, Acc};
                     List ->
                         write(Verbose, "~p LO ~p ~p ~p borked ~p~n",
                               [N, Site, P2, O, length(List)]),
                         NewAcc = add(P, O, List, Acc),
                         {N + 1, NewAcc}
                 end
         end,
    F3 = fun() ->
                 mnesia:foldl(F2, {0, []}, Tbl)
         end,
    {NBorked, Borked} = mnesia:activity(ets, F3),
    write(Verbose, "~p borked local_objs for ~p~n",
          [NBorked, Site]),
    transform(Borked, []).

transform([], Acc) -> Acc;
transform([H | T], Acc) ->
    {_, List} = H,
    Fun = fun(#local_obj{idx = I1}, #local_obj{idx = I2}) ->
                  if
                      I1 >  I2 -> true;
                      I1 =< I2 -> false
                  end
          end,
    L2 = lists:sort(Fun, List),
    [LO | Extras] = L2,
    NewAcc = lists:merge(zip(Extras, LO, []), Acc),
    transform(T, NewAcc).

zip([], _, Acc)       -> Acc;
zip([H | T], LO, Acc) ->
    #local_obj{idx = BorkedIdx} = H,
    #local_obj{idx = MasterIdx} = LO,
    zip(T, LO, [{BorkedIdx, MasterIdx} | Acc]).

add(Path, Obj, List, Acc) ->
    case lists:keymember({Path, Obj}, 1, Acc) of
        true  -> Acc;
        false -> [{{Path, Obj}, List} | Acc]
    end.

% check zinfs
check_zinfs(Site, Verbose, Fix) ->
    zinf_srv:verify(Site, Verbose, Fix).

%% BorkedZ = zinf_srv:check_borked(Site, Verbose, Fix, List),
%% case BorkedZ of
%%     [] -> ok;
%%     _  -> case Verbose of
%%               verbose ->
%%                   io:format("borked zinfs are ~p~n",
%%                             [BorkedZ]);
%%               _ ->
%%                   io:format("There are ~p borked zinfs~n",
%%                             [length(BorkedZ)])
%%           end
%% end,

% master table check
check_local_obj(Site, V, _Fix) ->
    Tbl1 = new_db_wu:trans(Site, relation),
    Fun1 = fun(X, Acc) ->
                   #local_obj{idx = I, type = Ty, path = P,
                              obj = O, revidx = R} = X,
                   % check that the revidx is the correct
                   P2 = hn_util:list_to_path(binary_to_term(P)),
                   O2 = hn_util:obj_to_ref(O),
                   R2 = binary_to_term(R),
                   NA0 = case P2 ++ O2 of
                            R2 -> [];
                            _  ->
                                write(V,"Revidx ~p is borked ~p ~p~n",
                                      [I, R2, P2 ++ O2]),
                                I
                        end,
                   % check that things dont have negative indices
                   NA = case O of
                             {cell, {X, Y}} when X < 1 orelse Y < 1 ->
                                 write(V, "local_obj ~p ~p ~p is duff~n",
                                       [I, P2, O2]),
                                 I;
                             {_, {N1, N2}} when N1 < 1 orelse N2 < 1 ->
                                 write(V, "local_obj ~p ~p ~p is duff~n",
                                       [I, P2, O2]),
                                 I;
                             _ ->
                                 []
                         end,
                   % check that the type is not 'undefined'
                   NA1 = case Ty of
                             undefined ->
                                 write(V, "local_obj ~p is undefined~n",
                                       [I]),
                                 I;
                             "url" ->
                                 write(V, "local_obj ~p is quoted url~n",
                                       [I]),
                                 I;
                             url ->
                                 [];
                             gurl ->
                                 []
                         end,
                   % If the local_obj is a cell is MUST have one and
                   % only one relation record - if it is not a cell it MUST NOT
                   % have a relation record
                   NA2 = case {mnesia:read(Tbl1, I, read), Ty, O} of
                             {[], url, {cell, _}} ->
                                 probe_item(V, Site, I);
                             {[_Rec], url, {cell, _}} ->
                                 [];
                             % cols, rows and pages should have no relations
                             {[], url, _} ->
                                 [];
                             % cols, rows and pages don't have rels
                             {[Rec], url, _} ->
                                 write(V, "shouldn't have a rel ~p for "
                                       ++ "~p ~p ~p~n",
                                       [Rec, P2, O, I]),
                                 I;
                             % gurls don't have rels
                             {[], gurl, _} ->
                                 [];
                             {List, Type, Obj} ->
                                 write(V, "many rels ~p for ~p ~p ~p ~p ~p~n",
                                       [List, Type, Obj, P2, O, I]),
                                 I
                         end,
                   NA3 = hslists:uniq(lists:flatten([NA, NA0, NA1, NA2])),
                   lists:merge(NA3, Acc)
           end,
    Tbl2 = new_db_wu:trans(Site, local_obj),
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, [], Tbl2)
           end,
    mnesia:activity(ets, Fun2).

probe_item(V, Site, I) ->
    Tbl = new_db_wu:trans(Site, item),
    case mnesia:read(Tbl, I, read) of
        [] ->
            write(V, "item ~p has no attrs as well~n", [I]),
            I;
        [#item{attrs = A}] ->
            A2 = binary_to_term(A),
            case lists:keyfind("formula", 1, A2) of
                false -> [];
                F     -> write(V, "relationless item ~p has formula ~p~n",
                               [I, F]),
                         I
            end
    end.

%% simple checks for duff idx's
check_form(Site, _V, _Fix) ->
    _Borked = check_table(Site, form).

check_include(Site, _V, _Fix) ->
    _Borked = check_table(Site, include).

check_item(Site, _V, _Fix) ->
    _Borked = check_table(Site, item).

check_relation(Site, V, _Fix) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, relation),
    Fun1 = fun(Rel, {IAcc, N}) ->
                   #relation{cellidx = Idx, children = C, parents = P,
                             infparents = Inf, z_parents = Z} = Rel,
                   Borked = check_rel2([{children, C}, {parents, P},
                                    {inf, Inf}, {zs, Z}], Tbl1, Tbl2,
                                       Idx, V, []),
                   case summarise(Borked, 0) of
                       0 -> {IAcc, N};
                       I -> io:format("~p borked rels~n", [I]),
                            {[Idx | IAcc], N + I}
                   end
           end,
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, {[], 0}, Tbl2)
           end,
    mnesia:activity(ets, Fun2).

summarise([], N) -> N;
summarise([{_, List} | T], N) -> summarise(T, length(List) + N).

check_rel2([], _Tbl1, _Tbl2, _Idx, _V, [{zs, []}, {inf, []}, {parents, []},
                                        {children, []}]) ->
    [{zs, []}, {inf, []}, {parents, []}, {children, []}];
check_rel2([], _Tbl1, _Tbl2, Idx, V, Acc) ->
    write(V, "The following idxs are borked in the "
          ++ "relations record for ~p~n~p~n", [Idx, Acc]),
    Acc;
check_rel2([{Type, I} | T], Tbl1, Tbl2, Idx, V, Acc) ->
    NewAcc = [check_rel3(I, Type, Idx, Tbl1, Tbl2, V, []) | Acc],
    check_rel2(T, Tbl1, Tbl2, Idx, V, NewAcc).

check_rel3([], Type, _Idx, _Tbl1, _Tbl2, _V, Acc) -> {Type, Acc};
check_rel3([H | T], Type, Idx, Tbl1, Tbl2, V, Acc) ->
    NewAcc = case mnesia:read(Tbl1, H, read) of
                 []    -> write(V, "~p: ~p of ~p doesn't exist~n",
                                [Type, H, Idx]),
                          [H | Acc];
                 _List -> Acc
             end,
    NewAcc2 = case check_rel4(Type, H, Idx, Tbl1, Tbl2, V) of
                  ok    -> NewAcc;
                  error -> [H | Acc]
              end,
    check_rel3(T, Type, Idx, Tbl1, Tbl2, V, NewAcc2).

% the relation record for each child MUST have the Idx as a parent
check_rel4(children, C, Idx, _Tbl1, Tbl2, V) ->
    case mnesia:read(Tbl2, C, read) of
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
check_rel4(parents, P, Idx, _Tbl1, Tbl2, V) ->
    case mnesia:read(Tbl2, P, read) of
        []  -> write(V, "The parent: ~p of ~p doesn't exist~n",
                     [P, Idx]),
               error;
        [R] -> #relation{children = C} = R,
               case lists:member(Idx, C) of
                   true  -> ok;
                   false -> write(V, "The relation of the parent: ~p of ~p"
                                  ++ "doenst have it as a child~n",
                                  [P, Idx]),
                            error
               end
    end;
% an infinite or z_parent MUST have a local_obj of type 'gurl'
% associated with
check_rel4(Type, P, Idx, Tbl1, _Tbl2, V)
  when Type == inf orelse Type == z_parents ->
  case mnesia:read(Tbl1, P, read) of
      []                        -> write(V, "The parent ~p: ~p of ~p "
                                          ++" doesn't exist~n",
                                          [Type, P, Idx]),
                                    error;
      [#local_obj{type = gurl}] -> ok;
      [#local_obj{type = url}]  -> write(V, "The parent ~p: ~p of ~p "
                                          ++" is a 'url' not 'gurl'~n",
                                         [Type, P, Idx]),
                                   error
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
    mnesia:activity(ets, Fun2).

write(verbose, Msg, Data) -> io:format(Msg, Data);
write(_, _, _)            -> ok.
