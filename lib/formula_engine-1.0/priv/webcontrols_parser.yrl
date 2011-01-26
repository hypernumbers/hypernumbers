%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr
Conds
Cond
Clause

.

Terminals

open
close
comma
slash
path

.

Rootsymbol Expr.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators

%% ----- Grammar definition.

Expr -> Conds       : lit('$1').
Expr -> Conds slash : lit('$1').

Conds -> Cond Cond : join('$1', '$2').
Conds -> Cond : '$1'.

Cond -> Clause : '$1'.

Cond -> slash path : '$2'.

Clause -> slash open path comma path close :  op('$3', '$5').

Erlang code.
%% Erlang code follows here
op({path, A}, {path, B}) -> io:format("In op ~p ~p~n", [A, B]),
                                  {{tempate, A}, {generator, B}}.

join(A, B) -> io:format("In join ~p ~p~n", [A, B]),
              [A, B].

lit(A) -> io:format("in lit ~p~n", [A]),
          A.
