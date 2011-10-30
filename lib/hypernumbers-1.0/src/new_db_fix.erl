%%% @author     Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       A module to fix up problems
%%%            identified by new_db_verify
%%%
%%% @end
%%% Created : 24 Oct 2011 by <gordon@hypernumbers.com>

-module(new_db_fix).

-export([
         fix/0,
         fix_DEBUG/2,
         fix/2
        ]).

% spawning api
-export([
         fix_SPAWN/2
        ]).

-include("spriki.hrl").

fix() ->
    Dir = "/home/gordon/hypernumbers/priv/verification/",
    File = "fixable_errors.20_Oct_11_13_27_19.terms",
    fix(Dir, File).

fix_DEBUG(Dir, File) ->
    fix_SPAWN(Dir, File).

fix(Dir, File) ->
    spawn(new_db_fix, fix_SPAWN, [Dir, File]).

fix_SPAWN(Dir, File) ->
    {ok, [{Data1, Data2}]} = file:consult(Dir ++ File),
    io:format("Problems loaded~n"),
    [fix2(X, Site) || {Site, X} <- Data1],
    [fix_dups2(Dups, Site) || {Site, Dups} <- Data2],
    ok.

fix_dups2([], _Site)     -> ok;
fix_dups2([H | T], Site) -> ok = fix_dups3(H, "http://" ++ Site),
                            fix_dups2(T, Site).

fix_dups3({RevIdx, List}, Site) ->
    io:format("Fix up ~p ~p ~p~n", [Site, RevIdx, length(List)]),
    F = fun() ->
                Tbl1 = new_db_wu:trans(Site, local_obj),
                Pattern = {local_obj, '_', '_', '_', '_', term_to_binary(RevIdx)},
                Ret = mnesia:index_match_object(Tbl1, Pattern, 6, read),
                [_Master | Rest] = lists:reverse(Ret),
                ok = dump(Rest, Site)
        end,
    mnesia:activity(transaction, F),
    ok.

dump([], _Site)     -> ok;
dump([#local_obj{idx = Idx, path = P, obj = O} | T], Site) ->
    Tbl2 = new_db_wu:trans(Site, item),
    Tbl3 = new_db_wu:trans(Site, relation),
    case mnesia:read(Tbl2, Idx, write) of
        []      -> ok;
        [_Item] -> ok %io:format("Item is ~p~n", [Item])
    end,
    case mnesia:read(Tbl3, Idx, write) of
        []    -> ok;
        [Rel] -> #relation{parents = List} = Rel,
                 %io:format("Relation is ~p~n", [Rel]),
                 case List of
                     [] -> ok;
                     _L -> P2 = binary_to_term(P),
                           io:format("forcing recalcs ~p ~p~n", [P2, O]),
                           ok = new_db_wu:mark_these_idxs_dirtyD(List, Site, nil)
                 end
    end,
    dump(T, Site).

fix2([], _)                   -> ok;
fix2([{Idx, Type} | T], Site) -> ok = fix3("http://" ++ Site, Type, Idx),
                                 fix2(T, Site).

fix3(Site, "Invalid tables (type 1)", Idx) ->
    io:format("Deleting ~p ~p Invalid tables (type 1)~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, relation),
    Fun = fun() ->
                  mnesia:delete(Tbl1, Idx, write),
                  mnesia:delete(Tbl2, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid tables (type 2)", _Idx) -> ok;
fix3(_Site, "Invalid tables (type 3) (old adding in css/js)", _Idx) -> ok;
% fixing invalid tables type 4 is handled by Invalid Object (cell) (type 2)
fix3(_Site, "Invalid tables (type 4)", _Idx) -> ok;
fix3(Site, "Invalid relations (type 1)", Idx) ->
    Tbl1 = new_db_wu:trans(Site, relation),
    Fun = fun() ->
                  [Rel] = mnesia:read(Tbl1, Idx),
                  #relation{infparents = Dirties} = Rel,
                  [new_db_api:mark_idx_dirty(Site, X) || X <- Dirties]
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid relations (type 2)", _Idx) -> ok;
fix3(_Site, "Invalid relations (type 3)", _Idx) -> ok;
fix3(_Site, "Invalid include (type 1)", _Idx) -> ok;
fix3(Site, "Invalid include (type 2)", Idx) ->
    io:format("Marking dirty ~p ~p Invalid include (type 2)~n", [Site, Idx]),
    new_db_api:mark_idx_dirty(Site, Idx);
fix3(Site, "Invalid timer", Idx) ->
    io:format("Deleting ~p ~p Invalid timer)~n", [Site, Idx]),
    Tbl = new_db_wu:trans(Site, timer),
    Fun = fun() ->
                  mnesia:delete(Tbl, Idx, write)
                      end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid form", _Idx) -> ok;
fix3(_Site, "Invalid formula", _Idx) -> ok;
fix3(_Site, "Invalid Object (cell) (type 1)", _Idx) -> ok;
fix3(Site, "Invalid Object (cell) (type 2)", Idx) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  mnesia:delete(Tbl1, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid Object (row) (type 3)", _Idx) -> ok;
fix3(_Site, "Invalid Object (column) (type 4)", _Idx) -> ok;
fix3(_Site, "Invalid Object (page) (type 5) (old adding in css/js)",
     _Idx) -> ok;
fix3(_Site, "Invalid Object (type 6)", _Idx) -> ok;
fix3(Site, "Invalid types", Idx) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  [Rec] = mnesia:read(Tbl1, Idx, write),
                  Rec2 = Rec#local_obj{type = gurl},
                  mnesia:write(Tbl1, Rec2, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 1)", Idx) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, item),
    Fun = fun() ->
                  mnesia:delete(Tbl1, Idx, write),
                  mnesia:delete(Tbl2, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 2)", Idx) ->
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  mnesia:delete(Tbl1, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid grid (type 3)", _Idx) -> ok;
fix3(_Site, "Invalid reverse index", _Idx) -> ok;
fix3(_Site, "Invalid zinf (type 1)", _Idx) -> ok;
fix3(_Site, "Invalid zinf (type 2)", _Idx) -> ok.
