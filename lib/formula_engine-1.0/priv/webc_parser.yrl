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

Expr -> Seg  slash : ['$1'].
Expr -> Segs slash :  lists:flatten('$1').

Segs -> Segs Seg : join('$1', '$2').
Segs -> Seg  Seg : join('$1', '$2').

Seg -> Clause : '$1'.

Seg -> slash path : '$2'.

Clause -> SubClause close : clause(lists:flatten('$1')).

SubClause -> SubClause comma path : join('$1', '$3').
SubClause -> slash open path      : '$3'.

Erlang code.

-export([
         run/1,
         compile/1,
         p_TEST/1
        ]).

-include("webcontrols.hrl").

%%% Api
compile(String) ->
    io:format("String is ~p~n", [String]),
    {ok, Toks, 1} = webc_lexer:lex(String),
    {ok, AST} = parse(Toks),
    io:format("AST is ~p~n", [AST]),
    AST.
    
run(AST) ->
    io:format("AST is ~p~n", [AST]),
    42.

%%%% Parser code

% 2 args
clause([{path, A}, {path, B}]) ->
    #wcpagename{template = A, name = B};
% 3 args
clause([{path, A}, {path, "auto"}, {path, "incr"}]) ->
    #wcpagenumber{template = A, type = incr, prefix = ""};
clause([{path, A}, {path, "auto"}, {path, "random"}]) ->
    #wcpagenumber{template = A, type = random, prefix = ""};
clause([{path, A}, {path, "date"}, {path, "yy"}]) ->
    #wcpagedate{template = A, format = "yy"};
clause([{path, A}, {path, "date"}, {path, "yyyy"}]) -> 
    #wcpagedate{template = A, format = "yyyy"};
clause([{path, A}, {path, "date"}, {path, "m"}]) -> 
    #wcpagedate{template = A, format = "m"};
clause([{path, A}, {path, "date"}, {path, "mm"}]) -> 
    #wcpagedate{template = A, format = "mm"};
clause([{path, A}, {path, "date"}, {path, "mmm"}]) -> 
    #wcpagedate{template = A, format = "mmm"};
clause([{path, A}, {path, "date"}, {path, "mmmm"}]) -> 
    #wcpagedate{template = A, format = "mmmm"};
clause([{path, A}, {path, "date"}, {path, "d"}]) -> 
    #wcpagedate{template = A, format = "d"};
clause([{path, A}, {path, "date"}, {path, "dd"}]) -> 
    #wcpagedate{template = A, format = "dd"};
clause([{path, A}, {path, "date"}, {path, "ddd"}]) -> 
    #wcpagedate{template = A, format = "ddd"};
clause([{path, A}, {path, "date"}, {path, "dddd"}]) -> 
    #wcpagedate{template = A, format = "dddd"};
% 4 args
clause([{path, A}, {path, "auto"}, {path, "incr"}, {path, B}]) -> 
    #wcpagenumber{template = A, type = incr, prefix = B};
clause([{path, A}, {path, "auto"}, {path, "random"}, {path, B}]) -> 
    #wcpagenumber{template = A, type = random, prefix = B}.

join(A, B) -> [A, B].

%%% Tests:
-include_lib("eunit/include/eunit.hrl").

p_TEST(String) ->
    io:format("String is ~p~n", [String]),
    {ok, Toks, 1} = webc_lexer:lex(String),
    io:format("Toks is ~p~n", [Toks]),
    {ok, AST} = parse(Toks),
    io:format("AST is ~p~n", [AST]),
    AST.

seg_test_() ->
    [
     ?_assert(p_TEST("/blah/") == [
                                   {path, "blah"}
                                  ]),

     ?_assert(p_TEST("/[Template, Name]/") ==
              [
               {wcpagename, "template", "name"}
              ]),

     ?_assert(p_TEST("/[Template, auto, incr]/") ==
              [
               {wcpagenumber, "template", incr, ""}
              ]),

     ?_assert(p_TEST("/[Template, auto, incr, Yeah]/") ==
              [
               {wcpagenumber, "template", incr, "yeah"}
              ]),

     ?_assert(p_TEST("/[Template, auto, random]/") ==
              [
               {wcpagenumber, "template", random, ""}
              ]),

     ?_assert(p_TEST("/[Template, auto, random, Yeah]/") ==
              [
               {wcpagenumber, "template", random, "yeah"}
              ]),

     ?_assert(p_TEST("/[Template, date, yy]/") ==
              [
               {wcpagedate, "template", "yy"}
              ]),

     ?_assert(p_TEST("/[Template, date, yyyy]/") ==
              [
               {wcpagedate, "template", "yyyy"}
              ]),

     ?_assert(p_TEST("/[Template, date, m]/") ==
              [
               {wcpagedate, "template", "m"}
              ]),

     ?_assert(p_TEST("/[Template, date, mm]/") ==
              [
               {wcpagedate, "template", "mm"}
              ]),

     ?_assert(p_TEST("/[Template, date, mmm]/") ==
              [
               {wcpagedate, "template", "mmm"}
              ]),

     ?_assert(p_TEST("/[Template, date, mmmm]/") ==
              [
               {wcpagedate, "template", "mmmm"}
              ]),

     ?_assert(p_TEST("/[Template, date, d]/") ==
              [
               {wcpagedate, "template", "d"}
              ]),

     ?_assert(p_TEST("/[Template, date, dd]/") ==
              [
               {wcpagedate, "template", "dd"}
              ]),

     ?_assert(p_TEST("/[Template, date, ddd]/") ==
              [
               {wcpagedate, "template", "ddd"}
              ]),

     ?_assert(p_TEST("/[Template, date, dddd]/") ==
              [
               {wcpagedate, "template", "dddd"}
              ]),

     ?_assert(p_TEST("/blah/[Template, Name]/") ==
              [
               {path, "blah"},
               {wcpagename, "template", "name"}
              ]),

     ?_assert(p_TEST("/blah/bleh/[Template, Name]/bloh/") ==
              [
               {path, "blah"},
               {path, "bleh"},
               {wcpagename, "template", "name"},
               {path, "bloh"}
              ])

    ].

prod_test_() ->
    [

     ?_assert(p_TEST("/[jingo, bobbie]/") ==
              [
               {wcpagename, "jingo", "bobbie"}
              ])
    ].
