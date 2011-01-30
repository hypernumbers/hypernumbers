%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr
Segs
Seg
Clause
SubClause
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

Expr -> Seg  slash : lit(['$1']).
Expr -> Segs slash : lit('$1').

Segs -> Seg Seg : join('$1', '$2').

Seg -> Clause : '$1'.

Seg -> slash path : '$2'.

Clause -> SubClause close : clause('$1').

SubClause -> SubClause comma path : join('$1', '$3').
SubClause -> slash open path      : '$3'.

Erlang code.

-export([
         p_TEST/1
        ]).

%% Erlang code follows here

clause([{path, A}, {path, B}]) -> {{template, A}, {name, B}}.

join(A, B) -> io:format("In join ~p ~p~n", [A, B]),
              [A, B].

lit(A) -> io:format("in lit ~p~n", [A]),
          A.

%%% Tests:
-include_lib("eunit/include/eunit.hrl").

p_TEST(String) ->
    io:format("String is ~p~n", [String]),
    {ok, Toks, 1} = webcontrols_lexer:lex(String),
    io:format("Toks is ~p~n", [Toks]),
    {ok, Ret} = parse(Toks),
    io:format("Ret is ~p~n", [Ret]),
    Ret.

seg_test_() ->
    [
     ?_assert(p_TEST("/blah/") == [
                                   {path, "blah"}
                                  ]),

     ?_assert(p_TEST("/[Template, Name]/") == [
                                              {{template, "Template"},
                                               {name, "Name"}}
                                             ]),
     
     ?_assert(p_TEST("/blah/[Template, Name]/") == [
                                                    {path, "blah"},
                                                    {{template, "Template"},
                                                    {name, "Name"}}
                                                   ]),
     
     ?_assert(p_TEST("/blah/[Template, Name]/") == [
                                                    {path, "blah"},
                                                    {{template, "Template"},
                                                    {name, "Name"}}
                                                   ])
    ].
