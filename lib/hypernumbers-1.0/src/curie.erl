%%% @author    Gordon guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  3 Jun 2011 by gordon@hypernumbers.com

-module(curie).

-include("spriki.hrl").

-export([
         build_fun/4
        ]).

build_fun(Site, Path, ReturnRef, ParamList) ->
    RetRef = #refX{site = Site, path = Path,
                   obj = hn_util:parse_ref(ReturnRef)},
    [{_, AST}] = new_db_api:read_attribute(RetRef, "__ast"),
    io:format("AST is ~p~n", [AST]),
    case check_off_page(AST, Path) of
        invalid -> {error, offpageref};
        valid   -> build_fun2(RetRef, AST, ParamList)
    end.

build_fun2(RetRef, AST, ParamList) ->
    io:format("ReturnRef is ~p~n AST is ~p~nParamList is ~p~n",
              [RetRef, AST, ParamList]),
    NewAST = walk(AST, RetRef,ParamList, []),
    io:format("NewAST is ~p~n", [NewAST]),
    ok.

walk([], _, _, Acc) -> lists:reverse(Acc);
walk([{cellref, {offset, X}, {offset, Y}, _, _} = H | T], Ref, Params, Acc) ->
    #refX{obj = {cell, {OldX, OldY}}} = Ref,
    NewX = OldX + X,
    NewY = OldY + Y,
    NewObj = hn_util:obj_to_ref({cell, {NewX, NewY}}),
    io:format("NewObj is ~p~n", [NewObj]),
    walk(T, Ref, Params, [H | Acc]);
walk([H | T], Ref, Params, Acc) ->
    walk(T, Ref, Params, [H | Acc]).

check_off_page([], _Path) -> valid;
check_off_page([{cellref, _, _, P, _} | T], Path) ->
    case muin_util:walk_path(Path, P) of
        Path -> check_off_page(T, Path);
        _    -> invalid
    end;
check_off_page([_H | T], Path) ->
    check_off_page(T, Path).
