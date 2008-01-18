%%%-------------------------------------------------------------------
%%% File        : userdefined.erl
%%% Author      : gordonguthrie@backawinner.gg
%%% Description :
%%%
%%% Created     : 8th October 2007
%%%-------------------------------------------------------------------
-module(userdef).


-export([hello_world/1,
         bool/3,
    bob/1,
	 board/3,
	 ectoplasm/3,
	 dog/1,
	 magnus/3,
	 frog/2]).

frog(_A,_B)-> "croak, croak".

bob(_List) -> "yo".

magnus(A,B,C)->
    A/B+C.

dog(A) ->
     5*A.

ectoplasm(A,B,C)->
    A*B+C.

board(A,B,C)->
    A+B*C.

hello_world([])   -> "Hello World no parameters";
hello_world(List) -> hello_world(List,0).

hello_world([],No)->
    "Hello World with "++integer_to_list(No)++" parameters";
hello_world([_,T],No)->
    hello_world(T,No+1).

bool(A,"=",B) ->
    case A == B of
	true -> true;
	_    -> false
    end;
bool(A,">",B) ->
    case A > B of
	true -> true;
	_    -> false
    end;
bool(A,"<",B) ->
    case A < B of
	true -> true;
	_    -> false
    end;
bool(A,">=",B) ->
    case A >= B of
	true -> true;
	_    -> false
    end;
bool(A,"=<",B) ->
    case A =< B of
	true -> true;
	_    -> false
    end;
bool(A,"/=",B) ->
    case A /= B of
	true -> true;
	_    -> false
    end;
bool(A,"=:=",B) ->
    case A =:= B of
	true -> true;
	_    -> false
    end;
bool(A,"=/=",B) ->
    case A =/= B of
	true -> true;
	_    -> false
    end.
