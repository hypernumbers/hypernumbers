%%%-------------------------------------------------------------------
%%% File        : record_util.erl
%%% Author      : Gordon Guthrie gordon@hypernumbers.com
%%% Description : utilities for manipulating records
%%%
%%% Created     :  2 Sep 2008 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(make_ms_util).

-export([make/0]).

-define(MODULENAME,"ms_util2").

make() ->
    {ok,Tree}=epp:parse_file("lib/hypernumbers-1.0/include/spriki.hrl",[],[]),
    Src=make_src(Tree),
    ok=file:write_file(?MODULENAME++".erl",list_to_binary(Src)).

make_src(Tree) -> make_src(Tree,[]).

make_src([],Acc)                              -> make_src2(Acc,[],[]);
make_src([{attribute,_,record,Record}|T],Acc) -> make_src(T,[Record|Acc]);
make_src([_H|T],Acc)                          -> make_src(T,Acc).

make_src2([],Acc1,Acc2)    -> top_and_tail(Acc1,Acc2);
make_src2([H|T],Acc1,Acc2) -> {NewAcc1,NewAcc2}=expand_rec(H),
			      make_src2(T,[NewAcc1|Acc1],[NewAcc2|Acc2]).

expand_rec({Name,Def}) -> expand_fields(Name,Def,1,[]).

expand_fields(Name,[],N,Acc) -> {mk2(Name,N-1),lists:reverse([mk(Name)|Acc])};
expand_fields(Name,[{record_field,_,{atom,_,F},_}|T],N,Acc) -> 
    expand_fields(Name,T,N+1,[mk(Name,F,N)|Acc]);
expand_fields(Name,[{record_field,_,{atom,_,F}}|T],N,Acc) -> 
    expand_fields(Name,T,N+1,[mk(Name,F,N)|Acc]);
expand_fields(Name,[_H|T],N,Acc) -> expand_fields(Name,T,N+1,Acc).

%% mk2/1 builds the no of fields fns
mk2(Name,N) -> "no_of_fields("++atom_to_list(Name)++") -> "++
		   integer_to_list(N)++";\n".

%% mk/1 builds an error line
mk(Name) -> "get_index("++atom_to_list(Name)++",F) -> "++
		"exit({error, \"Record: "++atom_to_list(Name)++
		" has no field called \"++atom_to_list(F)});\n".

mk(Name,Field,N) -> 
    "get_index("++atom_to_list(Name)++", '"++
	atom_to_list(Field)++"')-> "++integer_to_list(N)++";\n".

top_and_tail(Acc1,Acc2)->
    Top="%% This module automatically generated - do not edit\n"++
        "\n"++
        "%%% This module provides utilities for use in building\n"++
        "%%% match specifications from records\n"++
        "%%% @private"++
        "\n"++
        "-module("++?MODULENAME++").\n"++
        "\n"++
        "-export([get_index/2, no_of_fields/1, is_in_record/2]).\n"++
        "\n"++
        "is_in_record(Record, Field) ->\n"++
        "    Return = try get_index(Record, Field)\n"++
        "             catch\n"++
        "                exit : _ -> false\n"++
        "                end,\n"++
        "    case Return of\n"++
        "      false -> false;\n"++
        "      _     -> true\n"++
        "    end.\n\n",
    Tail1="no_of_fields(Other) -> exit({error, \"Invalid Record Name: \""++
	"++Other}).\n\n\n",
    Tail2="get_index(Record,_Field) -> exit({error, \""++
	"Invalid Record Name: \"++Record}).\n",
    Top++lists:flatten(Acc1)++Tail1++lists:flatten(Acc2)++Tail2.

