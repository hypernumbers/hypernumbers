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

make_src([],A)                              -> make_src2(A,[],[],[]);
make_src([{attribute,_,record,Record}|T],A) -> make_src(T,[Record|A]);
make_src([_H|T],A)                          -> make_src(T,A).

make_src2([],A1,A2,A3)    -> top_and_tail(A1,A2,A3);
make_src2([H|T],A1,A2,A3) -> {NewA1,NewA2,NewA3}=exp_rec(H),
			      make_src2(T,[NewA1|A1],[NewA2|A2],[NewA3|A3]).

exp_rec({Name,Def}) -> exp_fields(Name,Def,1,[],[]).

%% expand the fields
exp_fields(Name,[],N,A1,A2) ->
    {mk3(Name,N-1),lists:reverse([mk(Name)|A1]),lists:reverse(A2)};
exp_fields(Name,[{record_field,_,{atom,_,F},_}|T],N,A1,A2) -> 
    exp_fields(Name,T,N+1,[mk(Name,F,N)|A1],[mk2(Name,F,N)|A2]);
exp_fields(Name,[{record_field,_,{atom,_,F}}|T],N,A1,A2) -> 
    exp_fields(Name,T,N+1,[mk(Name,F,N)|A1],[mk2(Name,F, N)|A2]);
exp_fields(Name,[_H|T],N,A1,A2) ->
    exp_fields(Name,T,N+1,A1,A2).

%% mk3/2 builds the no of fields fns
mk3(Name,N) -> "no_of_fields("++atom_to_list(Name)++") -> "++
                   integer_to_list(N)++";\n".

%% mk2/2 builds the index-to-field lookup
mk2(Name,Field,N) -> "name_by_index('"++atom_to_list(Name)++"', "++
                         integer_to_list(N)++") -> \""++
                         atom_to_list(Field)++"\";\n".

%% mk/1 builds an error line
mk(Name) -> "get_index2("++atom_to_list(Name)++",F) -> "++
                "exit({error, "++atom_to_list(Name)++", no_exists, F});\n".

mk(Name,Field,N) -> 
    "get_index2("++atom_to_list(Name)++", '"++
        atom_to_list(Field)++"')-> "++integer_to_list(N)++";\n".

top_and_tail(A1,A2,A3)->
    io:format("In top_and_tail~n-A3 is ~p~n",[A3]),
    Top="%% This module automatically generated - do not edit\n"++
        "\n"++
        "%%% This module provides utilities for use in building\n"++
        "%%% match specifications from records\n"++
        "%%% @private"++
        "\n"++
        "-module("++?MODULENAME++").\n"++
        "\n"++
        "-export([get_index/2,\n"++
        "         no_of_fields/1,\n"++
        "         is_in_record/2,\n"++
        "         name_by_index/2]).\n"++
        "\n"++
        "is_in_record(Record, Field) ->\n"++
        "    Return = try get_index(Record, Field)\n"++
        "             catch\n"++
        "                exit : _ -> false\n"++
        "                end,\n"++
        "    case Return of\n"++
        "      false -> false;\n"++
        "      _     -> true\n"++
        "    end.\n\n"++
        "get_index(Record, Field) when is_atom(Field)-> \n"++
        "   get_index2(Record, Field);\n"++
        "get_index(Record, Field) ->\n"++
        "   Field2 = try erlang:list_to_existing_atom(Field)\n"++
        "      catch\n"++
        "          _Error : _ -> exit({error, Record, no_exists, Field})\n"++
        "      end,\n"++
        "     get_index2(Record, Field2).\n",

    Tail1="no_of_fields(Other) -> exit({error, \"Invalid Record Name: \""++
	"++Other}).\n\n\n",
    Tail2="get_index2(Record,_Field) -> exit({error, \""++
	"Invalid Record Name: \"++Record}).\n",
    Tail3="name_by_index(Record,N) -> exit({error, Record, no_field_at_index, N}).\n\n",
    Top++lists:flatten(A1)++Tail1++lists:flatten(A2)++Tail2++
        lists:flatten(A3)++Tail3.

