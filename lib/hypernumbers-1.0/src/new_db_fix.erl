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
         fix/2
        ]).

fix() ->
    Dir = "/home/gordon/hypernumbers/priv/verification/",
    File = "fixable_errors.20_Oct_11_13_27_19.terms",
    fix(Dir, File).

fix(Dir, File) ->
    {ok, [{Data1, _Data2}]} = file:consult(Dir ++ File),
    io:format("Problems loaded~n"),
    [fix2(X, Site) || {Site, X} <- Data1],
    ok.

fix2([], _) ->
    ok;
fix2([{Idx, Type} | T], Site) ->
    fix3("http://" ++ Site, Type, Idx),
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
fix3(_Site, "Invalid relations (type 1)", _Idx) -> ok;
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
fix3(_Site, "Invalid Object (cell) (type 2)", _Idx) -> ok;
fix3(_Site, "Invalid Object (row) (type 3)", _Idx) -> ok;
fix3(_Site, "Invalid Object (column) (type 4)", _Idx) -> ok;
fix3(_Site, "Invalid Object (page) (type 5)", _Idx) -> ok;
fix3(_Site, "Invalid Object (type 6)", _Idx) -> ok;
fix3(_Site, "Invalid types", _Idx) -> ok;
fix3(_Site, "Invalid grid", _Idx) -> ok;
fix3(_Site, "Invalid reverse index", _Idx) -> ok;
fix3(_Site, "Invalid zinf", _Idx) -> ok.
