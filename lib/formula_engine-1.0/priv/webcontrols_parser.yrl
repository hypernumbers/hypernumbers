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

Expr -> Seg  slash : lists:flatten(['$1']).
Expr -> Segs slash : lists:flatten('$1').

Segs -> Segs Seg : join('$1', '$2').
Segs -> Seg  Seg : join('$1', '$2').

Seg -> Clause : '$1'.

Seg -> slash path : '$2'.

Clause -> SubClause close : clause(lists:flatten('$1')).

SubClause -> SubClause comma path : join('$1', '$3').
SubClause -> slash open path      : '$3'.

Erlang code.

-export([
         p_TEST/1
        ]).

%% Erlang code follows here

% 2 args
clause([{path, A}, {path, B}]) ->
    {{template, A}, {name, B}};
% 3 args
clause([{path, A}, {path, "auto"}, {path, "incr"}]) -> 
    {{template, A}, {incr, ""}};
clause([{path, A}, {path, "auto"}, {path, "random"}]) -> 
    {{template, A}, {random, ""}};
clause([{path, A}, {path, "date"}, {path, "yy"}]) -> 
    {{template, A}, {year, two_digit}};
clause([{path, A}, {path, "date"}, {path, "yyyy"}]) -> 
    {{template, A}, {year, four_digit}};
clause([{path, A}, {path, "date"}, {path, "m"}]) -> 
    {{template, A}, {month, no_zero}};
clause([{path, A}, {path, "date"}, {path, "mm"}]) -> 
    {{template, A}, {month, zero}};
clause([{path, A}, {path, "date"}, {path, "mmm"}]) -> 
    {{template, A}, {month, abbr}};
clause([{path, A}, {path, "date"}, {path, "mmmm"}]) -> 
    {{template, A}, {month, full}};
clause([{path, A}, {path, "date"}, {path, "d"}]) -> 
    {{template, A}, {day, no_zero}};
clause([{path, A}, {path, "date"}, {path, "dd"}]) -> 
    {{template, A}, {day, zero}};
clause([{path, A}, {path, "date"}, {path, "ddd"}]) -> 
    {{template, A}, {day, abbr}};
clause([{path, A}, {path, "date"}, {path, "dddd"}]) -> 
    {{template, A}, {day, full}};
% 4 args
clause([{path, A}, {path, "auto"}, {path, "incr"}, {path, B}]) -> 
    {{template, A}, {incr, B}};
clause([{path, A}, {path, "auto"}, {path, "random"}, {path, B}]) -> 
    {{template, A}, {random, B}}.

join(A, B) -> [A, B].

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

     ?_assert(p_TEST("/[Template, Name]/") ==
              [
               {{template, "template"},
                {name, "name"}}
              ]),

     ?_assert(p_TEST("/[Template, auto, incr]/") ==
              [
               {{template, "template"},
                {incr, ""}}
              ]),

     ?_assert(p_TEST("/[Template, auto, incr, Yeah]/") ==
              [
               {{template, "template"},
                {incr, "yeah"}}
              ]),

     ?_assert(p_TEST("/[Template, auto, random]/") ==
              [
               {{template, "template"},
                {random, ""}}
              ]),

     ?_assert(p_TEST("/[Template, auto, random, Yeah]/") ==
              [
               {{template, "template"},
                {random, "yeah"}}
              ]),

     ?_assert(p_TEST("/[Template, date, yy]/") ==
              [
               {{template, "template"},
                {year, two_digit}}
              ]),

     ?_assert(p_TEST("/[Template, date, yyyy]/") ==
              [
               {{template, "template"},
                {year, four_digit}}
              ]),

     ?_assert(p_TEST("/[Template, date, m]/") ==
              [
               {{template, "template"},
                {month, no_zero}}
              ]),

     ?_assert(p_TEST("/[Template, date, mm]/") ==
              [
               {{template, "template"},
                {month, zero}}
              ]),

     ?_assert(p_TEST("/[Template, date, mmm]/") ==
              [
               {{template, "template"},
                {month, abbr}}
              ]),

     ?_assert(p_TEST("/[Template, date, mmmm]/") ==
              [
               {{template, "template"},
                {month, full}}
              ]),

     ?_assert(p_TEST("/[Template, date, d]/") ==
              [
               {{template, "template"},
                {day, no_zero}}
              ]),

     ?_assert(p_TEST("/[Template, date, dd]/") ==
              [
               {{template, "template"},
                {day, zero}}
              ]),

     ?_assert(p_TEST("/[Template, date, ddd]/") ==
              [
               {{template, "template"},
                {day, abbr}}
              ]),

     ?_assert(p_TEST("/[Template, date, dddd]/") ==
              [
               {{template, "template"},
                {day, full}}
              ]),

     ?_assert(p_TEST("/blah/[Template, Name]/") ==
              [
               {path, "blah"},
               {{template, "template"},
                {name, "name"}}
              ]),

     ?_assert(p_TEST("/blah/bleh/[Template, Name]/bloh/") ==
              [
               {path, "blah"},
               {path, "bleh"},
               {{template, "template"},
                {name, "name"}},
               {path, "bloh"}
              ])

    ].
