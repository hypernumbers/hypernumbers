%%% @author     Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       A module to fix up problems
%%%            identified by new_db_verify
%%%
%%% @end
%%% Created : 24 Oct 2011 by <gordon@hypernumbers.com>

-module(new_db_fix).

% main api
-export([
         fix/2,
         uncouple_dups/2, % not sure if this is tested!
         fix_dups/2
        ]).

% clean up dirty zinfs
-export([
         clean_up_dirty_queues/0,
         clean_up_dirty_queues/1,
         clean_up_dirty_zinfs/0,
         clean_up_dirty_zinfs/1,
         clean_up_dirty_for_zinfs/0,
         clean_up_dirty_for_zinfs/1
        ]).

% debugging exports
-export([
         fix/0,
         uncouple_dups/0,
         fix_DEBUG/2
        ]).

% spawning api
-export([
         fix_SPAWN/2,
         uncouple_dups_SPAWN/2,
         fix_dups_SPAWN/2
        ]).

-include("spriki.hrl").

fix() ->
    Dir = "/home/gordon/hypernumbers/priv/verification/",
    File = "fixable_errors.20_Oct_11_13_27_19.terms",
    fix(Dir, File).

uncouple_dups() ->
    Dir = "/home/gordon/hypernumbers/priv/verification/",
    File = "fixable_errors.20_Oct_11_13_27_19.terms",
    uncouple_dups(Dir, File).

fix_dups(Dir, File) ->
    fix_dups_SPAWN(Dir, File).

fix_DEBUG(Dir, File) -> fix_SPAWN(Dir, File).

fix(Dir, File) ->
    spawn(new_db_fix, fix_SPAWN, [Dir, File]).

uncouple_dups(Dir, File) ->
    spawn(new_db_fix, uncouple_dups_SPAWN, [Dir, File]).

fix_SPAWN(Dir, File) ->
    {ok, [{Data1, _Data2}]} = file:consult(Dir ++ File),
    io:format("Problems loaded - fixing errors~n"),
    [fix2(X, Site) || {Site, X} <- Data1],
    ok.

uncouple_dups_SPAWN(Dir, File) ->
    {ok, [{_Data1, Data2}]} = file:consult(Dir ++ File),
    io:format("Problems loaded - fixing dups~n"),
    [uncouple_dups2(Dups, Site) || {Site, Dups} <- Data2],
    ok.

fix_dups_SPAWN(Dir, File) ->
    {ok, [{_Data1, Data2}]} = file:consult(Dir ++ File),
    io:format("Problems loaded - fixing dups~n"),
    [fix_dups2(Dups, Site) || {Site, Dups} <- Data2],
    ok.

uncouple_dups2([], _Site)     -> ok;
uncouple_dups2([H | T], Site) -> ok = uncouple_dups3(H, "http://" ++ Site),
                                 uncouple_dups2(T, Site).

uncouple_dups3({RevIdx, _List}, Site) ->
    % io:format("Fix up ~p ~p ~p~n", [Site, RevIdx, length(List)]),
    F = fun() ->
                Tbl1 = new_db_wu:trans(Site, local_obj),
                Pattern = {local_obj, '_', '_', '_', '_', term_to_binary(RevIdx)},
                Ret = mnesia:index_match_object(Tbl1, Pattern, 6, read),
                [Master | Rest] = lists:reverse(lists:sort(Ret)),
                RX = [X || #local_obj{idx = X} <- Rest],
                % io:format("Master is ~p~nList is ~p~n", [Master#local_obj.idx,
                %                                         RX]),
                uncouple_dups4(RX, Master, Site)
        end,
    mnesia:activity(transaction, F),
    ok.

uncouple_dups4([], _Master, _Site)     -> ok;
uncouple_dups4([Idx| T], Master, Site) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, relation),
    [Rec] = mnesia:read(Tbl1, Idx, write),
    #local_obj{path = P, obj = O} = Rec,
    case O of
        {cell, _} ->
            XRefX = #xrefX{idx = Idx, site = Site, path = binary_to_term(P),
                           obj = O},
            %io:format("XRefX is ~p~n", [XRefX]),
            new_db_wu:write_attrs(XRefX, [{"formula", ""}]),
            [Rel] = mnesia:read(Tbl2, Idx, write),
            C = Rel#relation.children,
            [new_db_api:mark_idx_dirty(Site, X) || X <- C];
        _ -> ok
    end,
    uncouple_dups4(T, Master, Site).

fix_dups2([], _Site)     -> ok;
fix_dups2([H | T], Site) -> ok = fix_dups3(H, "http://" ++ Site),
                                 fix_dups2(T, Site).

fix_dups3({RevIdx, _List}, Site) ->
    % io:format("Fix up ~p ~p ~p~n", [Site, RevIdx, length(List)]),
    F = fun() ->
                Tbl1 = new_db_wu:trans(Site, local_obj),
                Pattern = {local_obj, '_', '_', '_', '_', term_to_binary(RevIdx)},
                Ret = mnesia:index_match_object(Tbl1, Pattern, 6, read),
                [Master | Rest] = lists:reverse(lists:sort(Ret)),
                % case Master#local_obj.obj of
                %    {cell, _} ->
                %        io:format("Master is ~p~n", [Master]),
                %        new_db_DEBUG:raw_idx(Site, Master#local_obj.idx);
                %    _Other ->
                %        ok
                %end,
                RX = [X || #local_obj{idx = X} <- Rest],
                fix_dups4(RX, Master, Site)
        end,
    mnesia:activity(transaction, F),
    ok.

fix_dups4([], _Master, _Site)     -> ok;
fix_dups4([Idx| T], Master, Site) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, relation),
    Tbl3 = new_db_wu:trans(Site, item),
    [Rec] = mnesia:read(Tbl1, Idx, write),
    #local_obj{obj = O} = Rec,
    case O of
        {cell, _} ->
            %io:format("Should be deleting cell ~p ~p~n", [Site, Rec]),
            %new_db_DEBUG:raw_idx(Site, Idx);
            %case mnesia:read(Tbl2, idx, write) of
            %    [Rec2] -> io:format("Relations is ~p~n", [Rec2]);
            %    []     -> io:format("No relations~n")
            %end;
            ok = mnesia:delete(Tbl1, Idx, write),
            ok = mnesia:delete(Tbl2, Idx, write),
            ok = mnesia:delete(Tbl3, Idx, write);
        _ ->
            ok = mnesia:delete(Tbl1, Idx, write),
            case mnesia:read(Tbl3, Idx, write) of
                []   -> ok;
                [_I] -> ok = mnesia:delete(Tbl3, Idx, write)
            end
    end,
    fix_dups4(T, Master, Site).

fix2([], _)                   -> ok;
fix2([{Idx, Type} | T], Site) -> fix3("http://" ++ Site, Type, Idx),
                                 fix2(T, Site).

fix3(Site, {"delete zinf", ZIdx}, Idx) ->
    io:format("delete zinf_parent ~p from ~p on ~p~n",
              [ZIdx, Idx, Site]),
    Tbl1 = new_db_wu:trans(Site, relation),
    Fun = fun() ->
                  [Rec] = mnesia:read(Tbl1, Idx),
                  NewZ_Parents = lists:delete(ZIdx, Rec#relation.z_parents),
                  Rec2 = Rec#relation{z_parents = NewZ_Parents},
                  ok = mnesia:write(Tbl1, Rec2, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid tables (type 1)", Idx) ->
    io:format("Deleting ~p ~p Invalid tables (type 1)~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, relation),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write),
                  ok = mnesia:delete(Tbl2, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid tables (type 2) (don't fix)", _Idx) -> ok;
fix3(_Site, "Invalid tables (type 3) (old adding in css/js)", _Idx) ->
    ok;
% fixing invalid tables type 4 is handled by Invalid Object (cell) (type 2)
fix3(_Site, "Invalid tables (type 4)", _Idx) -> ok;
fix3(Site, "Invalid tables (type 5)", Idx) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid relations (type 1a)", Idx) ->
    io:format("recalcing Invalid relations (type 1a) for ~p ~p~n",
              [Site, Idx]),
     Tbl1 = new_db_wu:trans(Site, relation),
     Fun = fun() ->
                 case  mnesia:read(Tbl1, Idx) of
                     [Rel] ->
                         #relation{infparents = Dirties} = Rel,
                         [new_db_api:mark_idx_dirty(Site, X) || X <- Dirties];
                     [] ->
                         ok
                 end
           end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid relations (type 1b)", _Idx) -> ok;
% fixed by delete zinf
fix3(_Site, "Invalid relations (type 1c)", _Idx) -> ok;
fix3(_Site, "Invalid relations (type 2)", _Idx) -> ok;
fix3(_Site, "Invalid relations (type 3)", _Idx) -> ok;
fix3(_Site, "Invalid include (type 1)", _Idx) -> ok;
fix3(Site, "Invalid include (type 2)", Idx) ->
    io:format("Marking dirty ~p ~p Invalid include (type 2)~n",
              [Site, Idx]),
    new_db_api:mark_idx_dirty(Site, Idx);
fix3(Site, "Invalid timer", Idx) ->
    io:format("Deleting ~p ~p Invalid timer~n", [Site, Idx]),
    Tbl = new_db_wu:trans(Site, timer),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid form", _Idx) -> ok;
fix3(_Site, "Invalid formula (type 1)", _Idx) -> ok;
fix3(Site, "Invalid formula (type 2)", Idx) ->
    io:format("Deleting ~p ~p Invalid formula (type 2)~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, item),
    Tbl2 = new_db_wu:trans(Site, local_obj),
    Tbl3 = new_db_wu:trans(Site, relation),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write),
                  ok = mnesia:delete(Tbl2, Idx, write),
                  ok = mnesia:delete(Tbl3, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid Object (cell) (type 1)", Idx) ->
    io:format("Deleting ~p ~p Invalid Object (cell) (type 1)~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, relation),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write),
                  ok = mnesia:delete(Tbl2, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid Object (cell) (type 2)", Idx) ->
    io:format("deleting Invalid Object (cell) (type 2) ~p ~p~n",
              [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid Object (row) (type 3)", _Idx) -> ok;
fix3(_Site, "Invalid Object (column) (type 4)", _Idx) -> ok;
fix3(_Site, "Invalid Object (page) (type 5) (old adding in css/js)",
     _Idx) -> ok;
fix3(_Site, "Invalid Object (type 6)", _Idx) -> ok;
fix3(_Site, "Invalid Type 1 (zinf)", _Idx) -> ok;
fix3(Site, "Invalid types", Idx) ->
    io:format("fixing Invalid types ~p ~p~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  case mnesia:read(Tbl1, Idx, write) of
                      [Rec] ->
                          Rec2 = Rec#local_obj{type = gurl},
                          ok = mnesia:write(Tbl1, Rec2, write);
                      [] ->
                          ok
                  end
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 1)", Idx) ->
    io:format("deleting Invalid grid (type 1) ~p ~p~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, item),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write),
                  ok = mnesia:delete(Tbl2, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 2)", Idx) ->
    io:format("deleting Invalid grid (type 2) ~p ~p~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 3)", Idx) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, relation),
    Fun = fun() ->
                  ok = mnesia:delete(Tbl1, Idx, write),
                  ok = mnesia:delete(Tbl2, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid grid (type 4)", _Idx) -> ok;
fix3(_Site, "Invalid reverse index", _Idx) -> ok;
fix3(_Site, "Invalid zinf (type 1)", _Idx) -> ok;
    %% Tbl1 = new_db_wu:trans(Site, local_obj),
    %% Fun = fun() ->
    %%              ok = mnesia:delete(Tbl1, Idx, write)
    %%       end,
    %% mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid zinf (type 2)", _Idx) -> ok.

clean_up_dirty_zinfs() ->
    Sites = hn_setup:get_sites(),
    [clean_up_dirty_zinfs(X) || X <- Sites],
    ok.

clean_up_dirty_zinfs(Site) ->
    Tbl = new_db_wu:trans(Site, dirty_zinf),
    Fun1 = fun() ->
                   Fun2 = fun(X, Acc) ->
                                  New = X#dirty_zinf.new,
                                  Old = X#dirty_zinf.old,
                                  NewN = clean_up(New, []),
                                  NewO = clean_up(Old, []),
                                  Rec2 = X#dirty_zinf{new = NewN, old = NewO},
                                  mnesia:write(Tbl, Rec2, write),
                                  Acc
                          end,
                   mnesia:foldl(Fun2, [], Tbl)
           end,
    mnesia:activity(transaction, Fun1).

clean_up([], Acc) -> Acc;
clean_up([{error, id_not_found, _} = H | T], Acc) ->
    io:format("Duff ~p~n", [H]),
    clean_up(T, Acc);
clean_up([H | T], Acc) ->
    clean_up(T, [H | Acc]).

clean_up_dirty_for_zinfs() ->
    Sites = hn_setup:get_sites(),
    [clean_up_dirty_for_zinfs(X) || X <- Sites],
    ok.

clean_up_dirty_for_zinfs(Site) ->
    Tbl = new_db_wu:trans(Site, dirty_for_zinf),
    Tbl2 = new_db_wu:trans(Site, local_obj),
    Fun1 = fun() ->
                   Fun2 = fun(X, Acc) ->
                                  #dirty_for_zinf{dirty = Dirty} = X,
                                  case Dirty of
                                      false -> [X | Acc];
                                      _     -> #xrefX{idx = Idx} = Dirty,
                                               case exists(Tbl2, Idx) of
                                                   true  -> Acc;
                                                   false -> [X | Acc]
                                               end
                                  end
                          end,
                   Duffs = mnesia:foldl(Fun2, [], Tbl),
                   [del(Tbl, X) || X <- Duffs]
           end,
    mnesia:activity(transaction, Fun1).

exists(Tbl, Idx) ->
    case mnesia:read(Tbl, Idx, write) of
        [] ->
            io:format("No local obj for ~p ~p~n", [Tbl, Idx]),
            false;
        _ ->
            true
    end.

del(Tbl, X) ->
    io:format("deleting ~p ~p~n", [Tbl, X]),
    ok = mnesia:delete_object(Tbl, X, write).

clean_up_dirty_queues() ->
    Sites = hn_setup:get_sites(),
    [clean_up_dirty_queues(X) || X <- Sites],
    ok.

clean_up_dirty_queues(Site) ->
    Tbl = new_db_wu:trans(Site, dirty_queue),
    Fun1 = fun() ->
                   Fun2 = fun(X, Acc) ->
                                  io:format("X is ~p~n", [X]),
                                  #dirty_queue{dirty = Dirty} = X,
                                  NewDirty = clean_up_queue(Dirty, Site, []),
                                  Acc
                          end,
                   mnesia:foldl(Fun2, [], Tbl)
           end,
    mnesia:activity(transaction, Fun1).

clean_up_queue([], Site, Acc) ->
    Acc;
clean_up_queue([H | T], Site, Acc) ->
    io:format("H is ~p~n", [H]),
    Tbl = new_db_wu:trans(Site, local_obj),
    NewAcc = case mnesia:read(Tbl, H, write) of
                 [] -> io:format("~p doens't exists...~n", [H]),
                       Acc;
                 _  -> [H | Acc]
             end,
    clean_up_queue(T, Site, NewAcc).


