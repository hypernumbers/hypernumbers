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
         check_logging/3,
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
    [check(X, quiet, dontfix) || X <- Sites],
    ok.

check(Site, Verbose, Fix) ->
    io:format("Checking ~p with ~p ~p~n", [Site, Verbose, Fix]),
    case check_local_obj(Site, Verbose, Fix) of
        []   -> check2(Site, Verbose, Fix);
        List -> write(Verbose, "local objs borked~n~p~n", [List]),
                BorkedZ = zinf_srv:check_borked(Site, Verbose, Fix, List),
                io:format("BorkedZ is ~p~n", [BorkedZ]),
                Tbl = new_db_wu:trans(Site, local_obj),
                Size = mnesia:table_info(Tbl, size),
                io:format("~p out of ~p local_objs are borked~n",
                          [length(List), Size])
    end.

check2(Site, Verbose, Fix) ->
    BrokenZinfs       = check_zinfs(Site,    Verbose, Fix),
    BrokenForms       = check_form(Site,     Verbose, Fix),
    BrokenIncs        = check_include(Site,  Verbose, Fix),
    BrokenItems       = check_item(Site,     Verbose, Fix),
    BrokenLogs        = check_logging(Site,  Verbose, Fix),
    {BrokenRels, Num} = check_relation(Site, Verbose, Fix),
    BrokenTimers      = check_timer(Site,    Verbose, Fix),
    case Verbose of
        verbose ->
            io:format("BrokenZinfs  is ~p~n"
                      ++ "BrokenForms  is ~p~n"
                      ++ "BrokenIncs   is ~p~n"
                      ++ "BrokenItems  is ~p~n"
                      ++ "BrokenLogs   is ~p~n"
                      ++ "BrokenRels   is ~p~n"
                      ++ "BrokenTimers is ~p~n",
                      [BrokenZinfs, BrokenForms, BrokenIncs,
                       BrokenItems, BrokenLogs, BrokenRels,
                       BrokenTimers]);
        _ ->
            SizeForms  = ?m(new_db_wu:trans(Site, form),     size),
            SizeIncs   = ?m(new_db_wu:trans(Site, include),  size),
            SizeItems  = ?m(new_db_wu:trans(Site, item),     size),
            SizeLogs   = ?m(new_db_wu:trans(Site, logging),  size),
            SizeRels   = ?m(new_db_wu:trans(Site, relation), size),
            SizeTimers = ?m(new_db_wu:trans(Site, timer),    size),
            Errors = length(BrokenZinfs)
                + length(BrokenForms)
                + length(BrokenIncs)
                + length(BrokenItems)
                + length(BrokenLogs)
                + Num
                + length(BrokenTimers),
            case Errors of
                0 ->
                    io:format("No errors~n");
                _N ->
                    io:format("No of Broken:~n"
                              ++ "Zinfs:  ~p~n"
                              ++ "Forms:  ~p out of ~p~n"
                              ++ "Incs:   ~p out of ~p~n"
                              ++ "Items:  ~p out of ~p~n"
                              ++ "Logs:   ~p out of ~p~n"
                              ++ "Rels:   ~p out of ~p~n"
                              ++ "Timers: ~p out of ~p~n",
                              [length(BrokenZinfs),
                               length(BrokenForms), SizeForms,
                               length(BrokenIncs),  SizeIncs,
                               length(BrokenItems), SizeItems,
                               length(BrokenLogs),  SizeLogs,
                               Num, SizeRels,
                               length(BrokenTimers), SizeTimers])
                    end
    end.

% check zinfs
check_zinfs(Site, Verbose, Fix) ->
    zinf_srv:verify(Site, Verbose, Fix).

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
                   NA = case P2 ++ O2 of
                            R2 -> [];
                            _  ->
                                write(V,"Revidx is borked ~p ~p~n",
                                      [R2, P2 ++ O2]),
                                I
                        end,
                   % check that the type is not 'undefined'
                   NA1 = case Ty of
                             undefined -> write(V, "local_obj ~p is undefined~n",
                                                [I]),
                                          I;
                             "url"     -> write(V, "local_obj ~p is quoted url~n",
                                                [I]),
                                          I;
                             url       -> [];
                             gurl      -> []
                         end,
                   % If the local_obj is a cell is MUST have one and
                   % only relation record - if it is not a cell it MUST NOT
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
                   NA3 = hslists:uniq(lists:flatten([NA, NA1, NA2])),
                   lists:merge(NA3, Acc)
           end,
    Tbl2 = new_db_wu:trans(Site, local_obj),
    Fun2 = fun() ->
                   mnesia:foldl(Fun1, [], Tbl2)
           end,
    mnesia:activity(transaction, Fun2).

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

check_logging(Site, _V, _Fix) ->
    _Borked = check_table(Site, logging).

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
    mnesia:activity(transaction, Fun2).

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
    mnesia:activity(transaction, Fun2).

write(verbose, Msg, Data) -> io:format(Msg, Data);
write(_, _, _)            -> ok.
