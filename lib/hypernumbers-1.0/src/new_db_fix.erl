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
         uncouple_dups/2
        ]).

% debugging exports
-export([
         fix/0,
         uncouple_dups/0,
         fix_DEBUG/2,
         uncouple_dups_DEBUG/2,
         fix_dups_DEBUG/2
        ]).

% spawning api
-export([
         fix_SPAWN/2,
         uncouple_dups_SPAWN/2
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

fix_DEBUG(Dir, File) ->
    fix_SPAWN(Dir, File).

uncouple_dups_DEBUG(Dir, File) ->
    uncouple_dups_SPAWN(Dir, File).

fix_dups_DEBUG(Dir, File) ->
    fix_dups_SPAWN(Dir, File).


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

uncouple_dups3({RevIdx, List}, Site) ->
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

fix_dups3({RevIdx, List}, Site) ->
    % io:format("Fix up ~p ~p ~p~n", [Site, RevIdx, length(List)]),
    F = fun() ->
                Tbl1 = new_db_wu:trans(Site, local_obj),
                Pattern = {local_obj, '_', '_', '_', '_', term_to_binary(RevIdx)},
                Ret = mnesia:index_match_object(Tbl1, Pattern, 6, read),
                [Master | Rest] = lists:reverse(lists:sort(Ret)),
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
    #local_obj{path = P, obj = O} = Rec,
    case O of
        {cell, _} ->
            %io:format("Cell to delete is ~p ~p ~p~n",
            %          [Site, binary_to_term(P), O]),
            %[Rel] = mnesia:read(Tbl2, Idx, write),
            %io:format("Rel to delete is ~p~n", [Rel]),
            %[Item] = mnesia:read(Tbl3, Idx, write),
            %A = binary_to_term(Item#item.attrs),
            %io:format("Item to delete is ~p~n", [A]);
            mnesia:delete(Tbl1, Idx, write),
            mnesia:delete(Tbl2, Idx, write),
            mnesia:delete(Tbl3, Idx, write);
        _ ->
            %io:format("Non-cell to delete is ~p~n", [Rec])
            mnesia:delete(Tbl1, Idx, write),
            case mnesia:read(Tbl3, Idx, write) of
                []   -> ok;
                [_I] -> mnesia:delete(Tbl3, Idx, write)
            end
    end,
    fix_dups4(T, Master, Site).

fix2([], _)                   -> ok;
fix2([{Idx, Type} | T], Site) -> fix3("http://" ++ Site, Type, Idx),
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
fix3(_Site, "Invalid tables (type 2) (don't fix)", _Idx) -> ok;
fix3(_Site, "Invalid tables (type 3) (old adding in css/js)", _Idx) ->
    ok;
% fixing invalid tables type 4 is handled by Invalid Object (cell) (type 2)
fix3(_Site, "Invalid tables (type 4)", _Idx) -> ok;
fix3(Site, "Invalid relations (type 1)", Idx) ->
%% io:format("(SHOULD) recalcing Invalid relations (type 1) for ~p ~p~n",
%%           [Site, Idx]),
%% Tbl1 = new_db_wu:trans(Site, relation),
%% Fun = fun() ->
%%               [Rel] = mnesia:read(Tbl1, Idx),
%%               #relation{infparents = Dirties} = Rel,
%%               [new_db_api:mark_idx_dirty(Site, X) || X <- Dirties]
%%       end,
%% mnesia:activity(transaction, Fun);
    ok;
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
                  mnesia:delete(Tbl, Idx, write)
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
                  mnesia:delete(Tbl1, Idx, write),
                  mnesia:delete(Tbl2, Idx, write),
                  mnesia:delete(Tbl3, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid Object (cell) (type 1)", _Idx) -> ok;
fix3(Site, "Invalid Object (cell) (type 2)", Idx) ->
    io:format("deleteing Invalid Object (cell) (type 2) ~p ~p~n",
              [Site, Idx]),
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
    io:format("fixing Invalid types ~p ~p~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  [Rec] = mnesia:read(Tbl1, Idx, write),
                  Rec2 = Rec#local_obj{type = gurl},
                  mnesia:write(Tbl1, Rec2, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 1)", Idx) ->
    io:format("deleting Invalid grid (type 1) ~p ~p~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Tbl2 = new_db_wu:trans(Site, item),
    Fun = fun() ->
                  mnesia:delete(Tbl1, Idx, write),
                  mnesia:delete(Tbl2, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 2)", Idx) ->
    io:format("deleting Invalid grid (type 2) ~p ~p~n", [Site, Idx]),
    Tbl1 = new_db_wu:trans(Site, local_obj),
    Fun = fun() ->
                  mnesia:delete(Tbl1, Idx, write)
          end,
    mnesia:activity(transaction, Fun);
fix3(Site, "Invalid grid (type 3)", Idx) ->
    io:format("Fixing invalid grid problems~n"),
    Tbl1 = new_db_wu:trans(Site, item),
    Fun = fun() ->
                  mnesia:delete(Tbl1, Idx, write),
                  [Rec] = mnesia:read(Tbl1, Idx, write),
                  Attrs = binary_to_term(Rec#item.attrs),
                  io:format("Attrs is ~p~n", [Attrs])
          end,
    mnesia:activity(transaction, Fun);
fix3(_Site, "Invalid reverse index", _Idx) -> ok;
fix3(_Site, "Invalid zinf (type 1)", _Idx) -> ok;
fix3(_Site, "Invalid zinf (type 2)", _Idx) -> ok.
