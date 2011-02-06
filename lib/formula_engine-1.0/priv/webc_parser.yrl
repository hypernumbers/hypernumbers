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
plainpath
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

Seg -> slash plainpath : '$2'.

Clause -> SubClause close : clause(lists:flatten('$1')).

SubClause -> SubClause comma plainpath : join('$1', '$3').
SubClause -> slash open plainpath      : '$3'.

Erlang code.

-export([
         compile/1,
         p_TEST/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

%%% Api
compile(String) ->
    {ok, Toks, 1} = webc_lexer:lex(String),
    {ok, AST} = parse(Toks),
    AST.
    
%%%% Parser code

% 2 args
clause([{plainpath, A}, {plainpath, B}]) ->
    #namedpage{template = A, name = B};
% 3 args
clause([{plainpath, A}, {plainpath, "auto"}, {plainpath, "incr"}]) ->
    #numberedpage{template = A, type = "increment", prefix = ""};
clause([{plainpath, A}, {plainpath, "auto"}, {plainpath, "increment"}]) ->
    #numberedpage{template = A, type = "increment", prefix = ""};
clause([{plainpath, A}, {plainpath, "auto"}, {plainpath, "random"}]) ->
    #numberedpage{template = A, type = "random", prefix = ""};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "yy"}]) ->
    #datedpage{template = A, format = "yy"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "yyyy"}]) -> 
    #datedpage{template = A, format = "yyyy"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "m"}]) -> 
    #datedpage{template = A, format = "m"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "mm"}]) -> 
    #datedpage{template = A, format = "mm"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "mmm"}]) -> 
    #datedpage{template = A, format = "mmm"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "mmmm"}]) -> 
    #datedpage{template = A, format = "mmmm"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "d"}]) -> 
    #datedpage{template = A, format = "d"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "dd"}]) -> 
    #datedpage{template = A, format = "dd"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "ddd"}]) -> 
    #datedpage{template = A, format = "ddd"};
clause([{plainpath, A}, {plainpath, "date"}, {plainpath, "dddd"}]) -> 
    #datedpage{template = A, format = "dddd"};
% 4 args
clause([{plainpath, A}, {plainpath, "auto"}, {plainpath, "incr"},
        {plainpath, B}]) -> 
    ok = check_not_num_prefix(B),
    #numberedpage{template = A, type = increment, prefix = B};
clause([{plainpath, A}, {plainpath, "auto"}, {plainpath, "increment"},
        {plainpath, B}]) -> 
    ok = check_not_num_prefix(B),
    #numberedpage{template = A, type = "increment", prefix = B};
clause([{plainpath, A}, {plainpath, "auto"}, {plainpath, "random"},
        {plainpath, B}]) -> 
    ok = check_not_num_prefix(B),
    #numberedpage{template = A, type = "random", prefix = B}.

join(A, B) -> [A, B].

check_not_num_prefix(A) ->
    case tconv:to_num(A) of
        {error, nan} -> ok;
        _            -> ?ERR_VAL
    end.    

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
                                   {plainpath, "blah"}
                                  ]),

     ?_assert(p_TEST("/[Template, Name]/") ==
              [
               {namedpage, "template", "name"}
              ]),

     ?_assert(p_TEST("/[Template, auto, increment]/") ==
              [
               {numberedpage, "template", "increment", ""}
              ]),

     ?_assert(p_TEST("/[Template, auto, increment, Yeah]/") ==
              [
               {numberedpage, "template", "increment", "yeah"}
              ]),

     ?_assert(p_TEST("/[Template, auto, random]/") ==
              [
               {numberedpage, "template", "random", ""}
              ]),

     ?_assert(p_TEST("/[Template, auto, random, Yeah]/") ==
              [
               {numberedpage, "template", "random", "yeah"}
              ]),

     ?_assert(p_TEST("/[Template, date, yy]/") ==
              [
               {datedpage, "template", "yy"}
              ]),

     ?_assert(p_TEST("/[Template, date, yyyy]/") ==
              [
               {datedpage, "template", "yyyy"}
              ]),

     ?_assert(p_TEST("/[Template, date, m]/") ==
              [
               {datedpage, "template", "m"}
              ]),

     ?_assert(p_TEST("/[Template, date, mm]/") ==
              [
               {datedpage, "template", "mm"}
              ]),

     ?_assert(p_TEST("/[Template, date, mmm]/") ==
              [
               {datedpage, "template", "mmm"}
              ]),

     ?_assert(p_TEST("/[Template, date, mmmm]/") ==
              [
               {datedpage, "template", "mmmm"}
              ]),

     ?_assert(p_TEST("/[Template, date, d]/") ==
              [
               {datedpage, "template", "d"}
              ]),

     ?_assert(p_TEST("/[Template, date, dd]/") ==
              [
               {datedpage, "template", "dd"}
              ]),

     ?_assert(p_TEST("/[Template, date, ddd]/") ==
              [
               {datedpage, "template", "ddd"}
              ]),

     ?_assert(p_TEST("/[Template, date, dddd]/") ==
              [
               {datedpage, "template", "dddd"}
              ]),

     ?_assert(p_TEST("/blah/[Template, Name]/") ==
              [
               {plainpath, "blah"},
               {namedpage, "template", "name"}
              ]),

     ?_assert(p_TEST("/blah/bleh/[Template, Name]/bloh/") ==
              [
               {plainpath, "blah"},
               {plainpath, "bleh"},
               {namedpage, "template", "name"},
               {plainpath, "bloh"}
              ])

    ].

prod_test_() ->
    [

     ?_assert(p_TEST("/[jingo, bobbie]/") ==
              [
               {namedpage, "jingo", "bobbie"}
              ])
    ].
